/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2024                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_WCHAR_T
#define __FC_DEFINE_WCHAR_T
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
#include "__fc_machdep.h"
#if !defined(__cplusplus)
/* wchar_t is a keyword in C++ and shall not be a typedef. */
typedef __WCHAR_T wchar_t;
#else
typedef __WCHAR_T fc_wchar_t;
#endif
__END_DECLS
__POP_FC_STDLIB
#endif
