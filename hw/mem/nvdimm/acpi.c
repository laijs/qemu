/*
 * NVDIMM (A Non-Volatile Dual In-line Memory Module) NFIT Implement
 *
 * Copyright(C) 2015 Intel Corporation.
 *
 * Author:
 *  Xiao Guangrong <guangrong.xiao@linux.intel.com>
 *
 * NFIT is defined in ACPI 6.0: 5.2.25 NVDIMM Firmware Interface Table (NFIT)
 * and the DSM specfication can be found at:
 *       http://pmem.io/documents/NVDIMM_DSM_Interface_Example.pdf
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

#include "qemu-common.h"

#include "exec/address-spaces.h"
#include "hw/acpi/aml-build.h"
#include "hw/mem/pc-nvdimm.h"

#include "internal.h"

static void nfit_spa_uuid_pm(void *uuid)
{
    uuid_le uuid_pm = UUID_LE(0x66f0d379, 0xb4f3, 0x4074, 0xac, 0x43, 0x0d,
                              0x33, 0x18, 0xb7, 0x8c, 0xdb);
    memcpy(uuid, &uuid_pm, sizeof(uuid_pm));
}

enum {
    NFIT_TABLE_SPA = 0,
    NFIT_TABLE_MEM = 1,
    NFIT_TABLE_IDT = 2,
    NFIT_TABLE_SMBIOS = 3,
    NFIT_TABLE_DCR = 4,
    NFIT_TABLE_BDW = 5,
    NFIT_TABLE_FLUSH = 6,
};

enum {
    EFI_MEMORY_UC = 0x1ULL,
    EFI_MEMORY_WC = 0x2ULL,
    EFI_MEMORY_WT = 0x4ULL,
    EFI_MEMORY_WB = 0x8ULL,
    EFI_MEMORY_UCE = 0x10ULL,
    EFI_MEMORY_WP = 0x1000ULL,
    EFI_MEMORY_RP = 0x2000ULL,
    EFI_MEMORY_XP = 0x4000ULL,
    EFI_MEMORY_NV = 0x8000ULL,
    EFI_MEMORY_MORE_RELIABLE = 0x10000ULL,
};

/*
 * struct nfit - Nvdimm Firmware Interface Table
 * @signature: "NFIT"
 */
struct nfit {
    ACPI_TABLE_HEADER_DEF
    uint32_t reserved;
} QEMU_PACKED;

/*
 * struct nfit_spa - System Physical Address Range Structure
 */
struct nfit_spa {
    uint16_t type;
    uint16_t length;
    uint16_t spa_index;
    uint16_t flags;
    uint32_t reserved;
    uint32_t proximity_domain;
    uint8_t type_uuid[16];
    uint64_t spa_base;
    uint64_t spa_length;
    uint64_t mem_attr;
} QEMU_PACKED;

/*
 * struct nfit_memdev - Memory Device to SPA Map Structure
 */
struct nfit_memdev {
    uint16_t type;
    uint16_t length;
    uint32_t nfit_handle;
    uint16_t phys_id;
    uint16_t region_id;
    uint16_t spa_index;
    uint16_t dcr_index;
    uint64_t region_len;
    uint64_t region_spa_offset;
    uint64_t region_dpa;
    uint16_t idt_index;
    uint16_t interleave_ways;
    uint16_t flags;
    uint16_t reserved;
} QEMU_PACKED;

/*
 * struct nfit_dcr - NVDIMM Control Region Structure
 */
struct nfit_dcr {
    uint16_t type;
    uint16_t length;
    uint16_t dcr_index;
    uint16_t vendor_id;
    uint16_t device_id;
    uint16_t revision_id;
    uint16_t sub_vendor_id;
    uint16_t sub_device_id;
    uint16_t sub_revision_id;
    uint8_t reserved[6];
    uint32_t serial_number;
    uint16_t fic;
    uint16_t num_bcw;
    uint64_t bcw_size;
    uint64_t cmd_offset;
    uint64_t cmd_size;
    uint64_t status_offset;
    uint64_t status_size;
    uint16_t flags;
    uint8_t reserved2[6];
} QEMU_PACKED;

#define REVSISON_ID             1
#define NFIT_FIC1               0x201

#define MAX_NVDIMM_NUMBER       10
#define NOTIFY_VALUE            0x99

static int get_nvdimm_device_number(GSList *list)
{
    int nr = 0;

    for (; list; list = list->next) {
        nr++;
    }

    return nr;
}

static uint32_t nvdimm_index_to_sn(int index)
{
    return 0x123456 + index;
}

static uint32_t nvdimm_index_to_handle(int index)
{
    return index + 1;
}

static size_t get_nfit_total_size(int nr)
{
    /* each nvdimm has 3 tables. */
    return sizeof(struct nfit) + nr * (sizeof(struct nfit_spa) +
                  sizeof(struct nfit_memdev) + sizeof(struct nfit_dcr));
}

static int build_spa_table(void *buf, PCNVDIMMDevice *nvdimm, int spa_index)
{
    struct nfit_spa *nfit_spa;
    uint64_t addr = object_property_get_int(OBJECT(&nvdimm->mr), "addr", NULL);

    nfit_spa = (struct nfit_spa *)buf;

    /*
     * nfit_spa->flags is set to zero so that proximity_domain
     * info is ignored.
     */
    nfit_spa->type = cpu_to_le16(NFIT_TABLE_SPA);
    nfit_spa->length = cpu_to_le16(sizeof(*nfit_spa));
    nfit_spa_uuid_pm(&nfit_spa->type_uuid);
    nfit_spa->spa_index = cpu_to_le16(spa_index);
    nfit_spa->spa_base = cpu_to_le64(addr);
    nfit_spa->spa_length = cpu_to_le64(memory_region_size(&nvdimm->mr));
    nfit_spa->mem_attr = cpu_to_le64(EFI_MEMORY_WB | EFI_MEMORY_NV);

    return sizeof(*nfit_spa);
}

static int build_memdev_table(void *buf, PCNVDIMMDevice *nvdimm,
                              int spa_index, int dcr_index)
{
    struct nfit_memdev *nfit_memdev;
    uint64_t addr = object_property_get_int(OBJECT(&nvdimm->mr), "addr", NULL);
    uint32_t handle = nvdimm_index_to_handle(nvdimm->device_index);

    nfit_memdev = (struct nfit_memdev *)buf;
    nfit_memdev->type = cpu_to_le16(NFIT_TABLE_MEM);
    nfit_memdev->length = cpu_to_le16(sizeof(*nfit_memdev));
    nfit_memdev->nfit_handle = cpu_to_le32(handle);
    /* point to nfit_spa. */
    nfit_memdev->spa_index = cpu_to_le16(spa_index);
    /* point to nfit_dcr. */
    nfit_memdev->dcr_index = cpu_to_le16(dcr_index);
    nfit_memdev->region_len = cpu_to_le64(memory_region_size(&nvdimm->mr));
    nfit_memdev->region_dpa = cpu_to_le64(addr);
    /* Only one interleave for pmem. */
    nfit_memdev->interleave_ways = cpu_to_le16(1);

    return sizeof(*nfit_memdev);
}

static int build_dcr_table(void *buf, PCNVDIMMDevice *nvdimm, int dcr_index)
{
    struct nfit_dcr *nfit_dcr;
    uint32_t sn = nvdimm_index_to_sn(nvdimm->device_index);

    nfit_dcr = (struct nfit_dcr *)buf;
    nfit_dcr->type = cpu_to_le16(NFIT_TABLE_DCR);
    nfit_dcr->length = cpu_to_le16(sizeof(*nfit_dcr));
    nfit_dcr->dcr_index = cpu_to_le16(dcr_index);
    nfit_dcr->vendor_id = cpu_to_le16(0x8086);
    nfit_dcr->device_id = cpu_to_le16(1);
    nfit_dcr->revision_id = cpu_to_le16(REVSISON_ID);
    nfit_dcr->serial_number = cpu_to_le32(sn);
    nfit_dcr->fic = cpu_to_le16(NFIT_FIC1);

    return sizeof(*nfit_dcr);
}

static void build_nfit_table(GSList *device_list, char *buf)
{
    int index = 0;

    buf += sizeof(struct nfit);

    for (; device_list; device_list = device_list->next) {
        PCNVDIMMDevice *nvdimm = device_list->data;
        struct nfit_memdev *nfit_memdev;
        struct nfit_dcr *nfit_dcr;
        int spa_index, dcr_index;

        spa_index = ++index;
        dcr_index = ++index;

        /* build System Physical Address Range Description Table. */
        buf += build_spa_table(buf, nvdimm, spa_index);

        /*
         * build Memory Device to System Physical Address Range Mapping
         * Table.
         */
        nfit_memdev = (struct nfit_memdev *)buf;
        buf += build_memdev_table(buf, nvdimm, spa_index, dcr_index);

        /* build Control Region Descriptor Table. */
        nfit_dcr = (struct nfit_dcr *)buf;
        buf += build_dcr_table(buf, nvdimm, dcr_index);

        calculate_nvdimm_isetcookie(nvdimm, nfit_memdev->region_spa_offset,
                                    nfit_dcr->serial_number);
    }
}

enum {
    NFIT_CMD_IMPLEMENTED = 0,

    /* bus commands */
    NFIT_CMD_ARS_CAP = 1,
    NFIT_CMD_ARS_START = 2,
    NFIT_CMD_ARS_QUERY = 3,

    /* per-dimm commands */
    NFIT_CMD_SMART = 1,
    NFIT_CMD_SMART_THRESHOLD = 2,
    NFIT_CMD_DIMM_FLAGS = 3,
    NFIT_CMD_GET_CONFIG_SIZE = 4,
    NFIT_CMD_GET_CONFIG_DATA = 5,
    NFIT_CMD_SET_CONFIG_DATA = 6,
    NFIT_CMD_VENDOR_EFFECT_LOG_SIZE = 7,
    NFIT_CMD_VENDOR_EFFECT_LOG = 8,
    NFIT_CMD_VENDOR = 9,
};

struct dsm_buffer {
    /* RAM page. */
    uint32_t handle;
    uint8_t arg0[16];
    uint32_t arg1;
    uint32_t arg2;
    union {
        char arg3[PAGE_SIZE - 3 * sizeof(uint32_t) - 16 * sizeof(uint8_t)];
    };

    /* MMIO page. */
    union {
        uint32_t notify;
        char pedding[PAGE_SIZE];
    };
};

static ram_addr_t dsm_addr;
static size_t dsm_size;

static uint64_t dsm_read(void *opaque, hwaddr addr,
                         unsigned size)
{
    fprintf(stderr, "BUG: we never read DSM notification MMIO.\n");
    assert(0);
    return 0;
}

static void dsm_write(void *opaque, hwaddr addr,
                      uint64_t val, unsigned size)
{
    assert(val == NOTIFY_VALUE);
}

static const MemoryRegionOps dsm_ops = {
    .read = dsm_read,
    .write = dsm_write,
    .endianness = DEVICE_LITTLE_ENDIAN,
};

static int build_dsm_buffer(void)
{
    MemoryRegion *dsm_ram_mr, *dsm_mmio_mr;
    ram_addr_t addr;;

    QEMU_BUILD_BUG_ON(PAGE_SIZE * 2 != sizeof(struct dsm_buffer));

    /* DSM buffer has already been built. */
    if (dsm_addr) {
        return 0;
    }

    addr = reserved_range_push(2 * PAGE_SIZE);
    if (!addr) {
        return -1;
    }

    dsm_addr = addr;
    dsm_size = PAGE_SIZE * 2;

    dsm_ram_mr = g_new(MemoryRegion, 1);
    memory_region_init_ram(dsm_ram_mr, NULL, "dsm_ram", PAGE_SIZE,
                           &error_abort);
    vmstate_register_ram_global(dsm_ram_mr);
    memory_region_add_subregion(get_system_memory(), addr, dsm_ram_mr);

    dsm_mmio_mr = g_new(MemoryRegion, 1);
    memory_region_init_io(dsm_mmio_mr, NULL, &dsm_ops, dsm_ram_mr,
                          "dsm_mmio", PAGE_SIZE);
    memory_region_add_subregion(get_system_memory(), addr + PAGE_SIZE,
                                dsm_mmio_mr);
    return 0;
}

void pc_nvdimm_build_nfit_table(GArray *table_offsets, GArray *table_data,
                                GArray *linker)
{
    GSList *list;
    size_t total;
    char *buf;
    int nfit_start, nr;

    if (build_dsm_buffer()) {
        fprintf(stderr, "do not have enough space for DSM buffer.\n");
        return;
    }

    list = get_nvdimm_built_list();
    nr = get_nvdimm_device_number(list);
    total = get_nfit_total_size(nr);

    if (nr <= 0 || nr > MAX_NVDIMM_NUMBER) {
        goto exit;
    }

    nfit_start = table_data->len;
    acpi_add_table(table_offsets, table_data);

    buf = acpi_data_push(table_data, total);
    build_nfit_table(list, buf);

    build_header(linker, table_data, (void *)(table_data->data + nfit_start),
                 "NFIT", table_data->len - nfit_start, 1);

    build_nvdimm_configdata(list);

exit:
    g_slist_free(list);
}

static bool device_cmd_has_arg3[] = {
    false,      /* NFIT_CMD_IMPLEMENTED */
    false,      /* NFIT_CMD_SMART */
    false,      /* NFIT_CMD_SMART_THRESHOLD */
    false,      /* NFIT_CMD_DIMM_FLAGS */
    false,      /* NFIT_CMD_GET_CONFIG_SIZE */
    true,       /* NFIT_CMD_GET_CONFIG_DATA */
    true,       /* NFIT_CMD_SET_CONFIG_DATA */
    false,      /* NFIT_CMD_VENDOR_EFFECT_LOG_SIZE */
    false,      /* NFIT_CMD_VENDOR_EFFECT_LOG */
    false,      /* NFIT_CMD_VENDOR */
};

#define BUILD_STA_METHOD(_dev_, _method_)                                  \
    do {                                                                   \
        _method_ = aml_method("_STA", 0);                                  \
        aml_append(_method_, aml_return(aml_int(0x0f)));                   \
        aml_append(_dev_, _method_);                                       \
    } while (0)

#define SAVE_ARG012_HANDLE(_method_, _handle_)                             \
    do {                                                                   \
        aml_append(_method_, aml_store(_handle_, aml_name("HDLE")));       \
        aml_append(_method_, aml_store(aml_arg(0), aml_name("ARG0")));     \
        aml_append(_method_, aml_store(aml_arg(1), aml_name("ARG1")));     \
        aml_append(_method_, aml_store(aml_arg(2), aml_name("ARG2")));     \
    } while (0)

#define NOTIFY_AND_RETURN(_method_)                                        \
    do {                                                                   \
        aml_append(_method_, aml_store(aml_int(NOTIFY_VALUE),              \
                   aml_name("NOTI")));                                     \
        aml_append(_method_, aml_return(aml_name("ODAT")));                \
    } while (0)

static void build_nvdimm_devices(Aml *root_dev, GSList *list)
{
    Aml *has_arg3;
    int i, cmd_nr;

    cmd_nr = ARRAY_SIZE(device_cmd_has_arg3);
    has_arg3 = aml_package(cmd_nr);
    for (i = 0; i < cmd_nr; i++) {
        aml_append(has_arg3, aml_int(device_cmd_has_arg3[i]));
    }
    aml_append(root_dev, aml_name_decl("CAG3", has_arg3));

    for (; list; list = list->next) {
        PCNVDIMMDevice *nvdimm = list->data;
        uint32_t handle = nvdimm_index_to_handle(nvdimm->device_index);
        Aml *dev, *method, *ifctx;

        dev = aml_device("NVD%d", nvdimm->device_index);
        aml_append(dev, aml_name_decl("_ADR", aml_int(handle)));

        BUILD_STA_METHOD(dev, method);

        method = aml_method("_DSM", 4);
        {
            SAVE_ARG012_HANDLE(method, aml_int(handle));

            /* Local5 = DeRefOf(Index(CAG3, Arg2)) */
            aml_append(method,
                       aml_store(aml_derefof(aml_index(aml_name("CAG3"),
                       aml_arg(2))), aml_local(5)));
            /* if 0 < local5 */
            ifctx = aml_if(aml_lless(aml_int(0), aml_local(5)));
            {
                /* Local0 = Index(Arg3, 0) */
                aml_append(ifctx, aml_store(aml_index(aml_arg(3), aml_int(0)),
                           aml_local(0)));
                /* Local1 = sizeof(Local0) */
                aml_append(ifctx, aml_store(aml_sizeof(aml_local(0)),
                           aml_local(1)));
                /* Local2 = Local1 << 3 */
                aml_append(ifctx, aml_store(aml_shiftleft(aml_local(1),
                           aml_int(3)), aml_local(2)));
                /* Local3 = DeRefOf(Local0) */
                aml_append(ifctx, aml_store(aml_derefof(aml_local(0)),
                           aml_local(3)));
                /* CreateField(Local3, 0, local2, IBUF) */
                aml_append(ifctx, aml_create_field(aml_local(3),
                           aml_int(0), aml_local(2), "IBUF"));
                /* ARG3 = IBUF */
                aml_append(ifctx, aml_store(aml_name("IBUF"),
                           aml_name("ARG3")));
            }
            aml_append(method, ifctx);
            NOTIFY_AND_RETURN(method);
        }
        aml_append(dev, method);

        aml_append(root_dev, dev);
    }
}

void pc_nvdimm_build_acpi_devices(Aml *sb_scope)
{
    Aml *dev, *method, *field;
    struct dsm_buffer *dsm_buf;
    GSList *list = get_nvdimm_built_list();
    int nr = get_nvdimm_device_number(list);

    if (nr <= 0 || nr > MAX_NVDIMM_NUMBER) {
        g_slist_free(list);
        return;
    }

    dev = aml_device("NVDR");
    aml_append(dev, aml_name_decl("_HID", aml_string("ACPI0012")));

    /* map DSM buffer into ACPI namespace. */
    aml_append(dev, aml_operation_region("DSMR", AML_SYSTEM_MEMORY,
               dsm_addr, dsm_size));

    /*
     * DSM input:
     * @HDLE: store device's handle, it's zero if the _DSM call happens
     *        on ROOT.
     * @ARG0 ~ @ARG3: store the parameters of _DSM call.
     *
     * They are ram mapping on host so that these access never cause VM-EXIT.
     */
    field = aml_field("DSMR", AML_DWORD_ACC, AML_PRESERVE);
    aml_append(field, aml_named_field("HDLE",
                   sizeof(dsm_buf->handle) * BITS_PER_BYTE));
    aml_append(field, aml_named_field("ARG0",
                   sizeof(dsm_buf->arg0) * BITS_PER_BYTE));
    aml_append(field, aml_named_field("ARG1",
                   sizeof(dsm_buf->arg1) * BITS_PER_BYTE));
    aml_append(field, aml_named_field("ARG2",
                   sizeof(dsm_buf->arg2) * BITS_PER_BYTE));
    aml_append(field, aml_named_field("ARG3",
                   sizeof(dsm_buf->arg3) * BITS_PER_BYTE));
    /*
     * DSM input:
     * @NOTI: write value to it will notify QEMU that _DSM method is being
     *        called and the parameters can be found in dsm_buf.
     *
     * It is MMIO mapping on host so that it will cause VM-exit and QEMU
     * gets control.
     */
    aml_append(field, aml_named_field("NOTI",
                   sizeof(dsm_buf->notify) * BITS_PER_BYTE));
    aml_append(dev, field);

    /*
     * DSM output:
     * @ODAT: it resues the first page of dsm buffer and QEMU uses it to
     *        stores the result
     *
     * Since the first page is reused by both input and out, the input data
     * will be lost after storing new result into @ODAT
     */
    field = aml_field("DSMR", AML_DWORD_ACC, AML_PRESERVE);
    aml_append(field, aml_named_field("ODAT", PAGE_SIZE * BITS_PER_BYTE));
    aml_append(dev, field);

    BUILD_STA_METHOD(dev, method);

    method = aml_method("_DSM", 4);
    {
        SAVE_ARG012_HANDLE(method, aml_int(0));
        NOTIFY_AND_RETURN(method);
    }
    aml_append(dev, method);

    build_nvdimm_devices(dev, list);

    aml_append(sb_scope, dev);
    g_slist_free(list);
}
