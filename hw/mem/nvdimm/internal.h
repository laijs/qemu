/*
 * NVDIMM (A Non-Volatile Dual In-line Memory Module) Virtualization Implement
 *
 * Copyright(C) 2015 Intel Corporation.
 *
 * Author:
 *  Xiao Guangrong <guangrong.xiao@linux.intel.com>
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 */

#ifndef __NVDIMM_INTERNAL_H
#define __NVDIMM_INTERNAL_H

/* #define NVDIMM_DEBUG */

#ifdef NVDIMM_DEBUG
#define nvdebug(fmt, ...) fprintf(stderr, "nvdimm: " fmt, ## __VA_ARGS__)
#else
#define nvdebug(...)
#endif

#define PAGE_SIZE               (1UL << 12)

typedef struct {
    uint8_t b[16];
} uuid_le;

#define UUID_LE(a, b, c, d0, d1, d2, d3, d4, d5, d6, d7)                   \
((uuid_le)                                                                 \
{ { (a) & 0xff, ((a) >> 8) & 0xff, ((a) >> 16) & 0xff, ((a) >> 24) & 0xff, \
    (b) & 0xff, ((b) >> 8) & 0xff, (c) & 0xff, ((c) >> 8) & 0xff,          \
    (d0), (d1), (d2), (d3), (d4), (d5), (d6), (d7) } })

GSList *get_nvdimm_built_list(void);
ram_addr_t reserved_range_push(uint64_t size);

void calculate_nvdimm_isetcookie(PCNVDIMMDevice *nvdimm, uint64_t spa,
                                 uint32_t sn);
void build_nvdimm_configdata(GSList *device_list);
#endif
