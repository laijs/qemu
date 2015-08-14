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

#include "hw/mem/pc-nvdimm.h"

#define PAGE_SIZE      (1UL << 12)

static struct nvdimms_info {
    ram_addr_t current_addr;
} nvdimms_info;

/* the address range [offset, ~0ULL) is reserved for NVDIMM. */
void pc_nvdimm_reserve_range(ram_addr_t offset)
{
    offset = ROUND_UP(offset, PAGE_SIZE);
    nvdimms_info.current_addr = offset;
}

static char *get_file(Object *obj, Error **errp)
{
    PCNVDIMMDevice *nvdimm = PC_NVDIMM(obj);

    return g_strdup(nvdimm->file);
}

static void set_file(Object *obj, const char *str, Error **errp)
{
    PCNVDIMMDevice *nvdimm = PC_NVDIMM(obj);

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

static void pc_nvdimm_realize(DeviceState *dev, Error **errp)
{
    PCNVDIMMDevice *nvdimm = PC_NVDIMM(dev);

    if (!nvdimm->file) {
        error_setg(errp, "file property is not set");
    }
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
