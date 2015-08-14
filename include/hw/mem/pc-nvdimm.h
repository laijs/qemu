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

#ifndef __PC_NVDIMM_H
#define __PC_NVDIMM_H

#include "hw/qdev.h"

typedef struct PCNVDIMMDevice {
    /* private */
    DeviceState parent_obj;

    char *file;
    bool configdata;
} PCNVDIMMDevice;

#define TYPE_PC_NVDIMM "pc-nvdimm"

#define PC_NVDIMM(obj) \
    OBJECT_CHECK(PCNVDIMMDevice, (obj), TYPE_PC_NVDIMM)

#endif
