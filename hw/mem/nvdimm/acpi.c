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

#define REVSISON_ID    1
#define NFIT_FIC1      0x201

#define MAX_NVDIMM_NUMBER       10

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
        int spa_index, dcr_index;

        spa_index = ++index;
        dcr_index = ++index;

        /* build System Physical Address Range Description Table. */
        buf += build_spa_table(buf, nvdimm, spa_index);

        /*
         * build Memory Device to System Physical Address Range Mapping
         * Table.
         */
        buf += build_memdev_table(buf, nvdimm, spa_index, dcr_index);

        /* build Control Region Descriptor Table. */
        buf += build_dcr_table(buf, nvdimm, dcr_index);
    }
}

void pc_nvdimm_build_nfit_table(GArray *table_offsets, GArray *table_data,
                                GArray *linker)
{
    GSList *list = get_nvdimm_built_list();
    size_t total;
    char *buf;
    int nfit_start, nr;

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
exit:
    g_slist_free(list);
}
