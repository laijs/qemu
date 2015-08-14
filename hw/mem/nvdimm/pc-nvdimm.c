/*
 * NVDIMM (A Non-Volatile Dual In-line Memory Module) Virtualization Implement
 *
 * Copyright(C) 2015 Intel Corporation.
 *
 * Author:
 *  Xiao Guangrong <guangrong.xiao@linux.intel.com>
 *
 * Currently, it only supports PMEM Virtualization.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>
 */

#include <sys/mman.h>
#include <sys/ioctl.h>
#include <linux/fs.h>

#include "exec/address-spaces.h"
#include "hw/mem/pc-nvdimm.h"

#include "internal.h"

#define MIN_CONFIG_DATA_SIZE    (128 << 10)

static struct nvdimms_info {
    ram_addr_t current_addr;
    int device_index;
} nvdimms_info;

/* the address range [offset, ~0ULL) is reserved for NVDIMM. */
void pc_nvdimm_reserve_range(ram_addr_t offset)
{
    offset = ROUND_UP(offset, PAGE_SIZE);
    nvdimms_info.current_addr = offset;
}

static ram_addr_t reserved_range_push(uint64_t size)
{
    uint64_t current;

    current = ROUND_UP(nvdimms_info.current_addr, PAGE_SIZE);

    /* do not have enough space? */
    if (current + size < current) {
        return 0;
    }

    nvdimms_info.current_addr = current + size;
    return current;
}

static uint32_t new_device_index(void)
{
    return nvdimms_info.device_index++;
}

static int pc_nvdimm_built_list(Object *obj, void *opaque)
{
    GSList **list = opaque;

    if (object_dynamic_cast(obj, TYPE_PC_NVDIMM)) {
        PCNVDIMMDevice *nvdimm = PC_NVDIMM(obj);

        /* only realized NVDIMMs matter */
        if (memory_region_size(&nvdimm->mr)) {
            *list = g_slist_append(*list, nvdimm);
        }
    }

    object_child_foreach(obj, pc_nvdimm_built_list, opaque);
    return 0;
}

GSList *get_nvdimm_built_list(void)
{
    GSList *list = NULL;

    object_child_foreach(qdev_get_machine(), pc_nvdimm_built_list, &list);
    return list;
}

static char *get_file(Object *obj, Error **errp)
{
    PCNVDIMMDevice *nvdimm = PC_NVDIMM(obj);

    return g_strdup(nvdimm->file);
}

static void set_file(Object *obj, const char *str, Error **errp)
{
    PCNVDIMMDevice *nvdimm = PC_NVDIMM(obj);

    if (memory_region_size(&nvdimm->mr)) {
        error_setg(errp, "cannot change property value");
        return;
    }

    if (nvdimm->file) {
        g_free(nvdimm->file);
    }

    nvdimm->file = g_strdup(str);
}

static bool has_configdata(Object *obj, Error **errp)
{
    PCNVDIMMDevice *nvdimm = PC_NVDIMM(obj);

    return nvdimm->configdata;
}

static void set_configdata(Object *obj, bool value, Error **errp)
{
    PCNVDIMMDevice *nvdimm = PC_NVDIMM(obj);

    nvdimm->configdata = value;
}

static void pc_nvdimm_init(Object *obj)
{
    object_property_add_str(obj, "file", get_file, set_file, NULL);
    object_property_add_bool(obj, "configdata", has_configdata,
                             set_configdata, NULL);
}

static uint64_t get_file_size(int fd)
{
    struct stat stat_buf;
    uint64_t size;

    if (fstat(fd, &stat_buf) < 0) {
        return 0;
    }

    if (S_ISREG(stat_buf.st_mode)) {
        return stat_buf.st_size;
    }

    if (S_ISBLK(stat_buf.st_mode) && !ioctl(fd, BLKGETSIZE64, &size)) {
        return size;
    }

    return 0;
}

static void pc_nvdimm_realize(DeviceState *dev, Error **errp)
{
    PCNVDIMMDevice *nvdimm = PC_NVDIMM(dev);
    char name[512];
    void *buf;
    ram_addr_t addr;
    uint64_t size, nvdimm_size, config_size = MIN_CONFIG_DATA_SIZE;
    int fd;

    if (!nvdimm->file) {
        error_setg(errp, "file property is not set");
    }

    fd = open(nvdimm->file, O_RDWR);
    if (fd < 0) {
        error_setg(errp, "can not open %s", nvdimm->file);
        return;
    }

    size = get_file_size(fd);
    buf = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (buf == MAP_FAILED) {
        error_setg(errp, "can not do mmap on %s", nvdimm->file);
        goto do_close;
    }

    nvdimm->config_data_size = config_size;
    if (nvdimm->configdata) {
        /* reserve MIN_CONFIGDATA_AREA_SIZE for configue data. */
        nvdimm_size = size - config_size;
        nvdimm->config_data_addr = buf + nvdimm_size;
    } else {
        nvdimm_size = size;
        nvdimm->config_data_addr = NULL;
    }

    if ((int64_t)nvdimm_size <= 0) {
        error_setg(errp, "file size is too small to store NVDIMM"
                         " configure data");
        goto do_unmap;
    }

    addr = reserved_range_push(nvdimm_size);
    if (!addr) {
        error_setg(errp, "do not have enough space for size %#lx.\n", size);
        goto do_unmap;
    }

    nvdimm->device_index = new_device_index();
    sprintf(name, "NVDIMM-%d", nvdimm->device_index);
    memory_region_init_ram_ptr(&nvdimm->mr, OBJECT(dev), name, nvdimm_size,
                               buf);
    vmstate_register_ram(&nvdimm->mr, DEVICE(dev));
    memory_region_add_subregion(get_system_memory(), addr, &nvdimm->mr);

    return;

do_unmap:
    munmap(buf, size);
do_close:
    close(fd);
}

static void pc_nvdimm_class_init(ObjectClass *oc, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);

    /* nvdimm hotplug has not been supported yet. */
    dc->hotpluggable = false;

    dc->realize = pc_nvdimm_realize;
    dc->desc = "NVDIMM memory module";
}

static TypeInfo pc_nvdimm_info = {
    .name          = TYPE_PC_NVDIMM,
    .parent        = TYPE_DEVICE,
    .instance_size = sizeof(PCNVDIMMDevice),
    .instance_init = pc_nvdimm_init,
    .class_init    = pc_nvdimm_class_init,
};

static void pc_nvdimm_register_types(void)
{
    type_register_static(&pc_nvdimm_info);
}

type_init(pc_nvdimm_register_types)
