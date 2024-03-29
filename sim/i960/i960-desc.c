/* CPU data for i960.

THIS FILE IS MACHINE GENERATED WITH CGEN.

Copyright (C) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.

This file is part of the GNU Binutils and/or GDB, the GNU debugger.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

#include "sysdep.h"
#include <ctype.h>
#include <stdio.h>
#include "ansidecl.h"
#include "bfd.h"
#include "symcat.h"
#include "i960-desc.h"
#include "i960-opc.h"
#include "opintl.h"

/* Attributes.  */

static const CGEN_ATTR_ENTRY bool_attr[] =
{
  { "#f", 0 },
  { "#t", 1 },
  { 0, 0 }
};

static const CGEN_ATTR_ENTRY MACH_attr[] =
{
  { "base", MACH_BASE },
  { "i960_ka_sa", MACH_I960_KA_SA },
  { "i960_ca", MACH_I960_CA },
  { "max", MACH_MAX },
  { 0, 0 }
};

const CGEN_ATTR_TABLE i960_cgen_ifield_attr_table[] =
{
  { "MACH", & MACH_attr[0] },
  { "VIRTUAL", &bool_attr[0], &bool_attr[0] },
  { "UNSIGNED", &bool_attr[0], &bool_attr[0] },
  { "PCREL-ADDR", &bool_attr[0], &bool_attr[0] },
  { "ABS-ADDR", &bool_attr[0], &bool_attr[0] },
  { "RESERVED", &bool_attr[0], &bool_attr[0] },
  { "SIGN-OPT", &bool_attr[0], &bool_attr[0] },
  { 0, 0, 0 }
};

const CGEN_ATTR_TABLE i960_cgen_hardware_attr_table[] =
{
  { "MACH", & MACH_attr[0] },
  { "VIRTUAL", &bool_attr[0], &bool_attr[0] },
  { "UNSIGNED", &bool_attr[0], &bool_attr[0] },
  { "SIGNED", &bool_attr[0], &bool_attr[0] },
  { "CACHE-ADDR", &bool_attr[0], &bool_attr[0] },
  { "FUN-ACCESS", &bool_attr[0], &bool_attr[0] },
  { "PC", &bool_attr[0], &bool_attr[0] },
  { "PROFILE", &bool_attr[0], &bool_attr[0] },
  { 0, 0, 0 }
};

const CGEN_ATTR_TABLE i960_cgen_operand_attr_table[] =
{
  { "MACH", & MACH_attr[0] },
  { "VIRTUAL", &bool_attr[0], &bool_attr[0] },
  { "UNSIGNED", &bool_attr[0], &bool_attr[0] },
  { "PCREL-ADDR", &bool_attr[0], &bool_attr[0] },
  { "ABS-ADDR", &bool_attr[0], &bool_attr[0] },
  { "SIGN-OPT", &bool_attr[0], &bool_attr[0] },
  { "NEGATIVE", &bool_attr[0], &bool_attr[0] },
  { "RELAX", &bool_attr[0], &bool_attr[0] },
  { "SEM-ONLY", &bool_attr[0], &bool_attr[0] },
  { 0, 0, 0 }
};

const CGEN_ATTR_TABLE i960_cgen_insn_attr_table[] =
{
  { "MACH", & MACH_attr[0] },
  { "ALIAS", &bool_attr[0], &bool_attr[0] },
  { "VIRTUAL", &bool_attr[0], &bool_attr[0] },
  { "UNCOND-CTI", &bool_attr[0], &bool_attr[0] },
  { "COND-CTI", &bool_attr[0], &bool_attr[0] },
  { "SKIP-CTI", &bool_attr[0], &bool_attr[0] },
  { "DELAY-SLOT", &bool_attr[0], &bool_attr[0] },
  { "RELAXABLE", &bool_attr[0], &bool_attr[0] },
  { "RELAX", &bool_attr[0], &bool_attr[0] },
  { "NO-DIS", &bool_attr[0], &bool_attr[0] },
  { "PBB", &bool_attr[0], &bool_attr[0] },
  { 0, 0, 0 }
};

CGEN_KEYWORD_ENTRY i960_cgen_opval_h_gr_entries[] = 
{
  { "fp", 31 },
  { "sp", 1 },
  { "r0", 0 },
  { "r1", 1 },
  { "r2", 2 },
  { "r3", 3 },
  { "r4", 4 },
  { "r5", 5 },
  { "r6", 6 },
  { "r7", 7 },
  { "r8", 8 },
  { "r9", 9 },
  { "r10", 10 },
  { "r11", 11 },
  { "r12", 12 },
  { "r13", 13 },
  { "r14", 14 },
  { "r15", 15 },
  { "g0", 16 },
  { "g1", 17 },
  { "g2", 18 },
  { "g3", 19 },
  { "g4", 20 },
  { "g5", 21 },
  { "g6", 22 },
  { "g7", 23 },
  { "g8", 24 },
  { "g9", 25 },
  { "g10", 26 },
  { "g11", 27 },
  { "g12", 28 },
  { "g13", 29 },
  { "g14", 30 },
  { "g15", 31 }
};

CGEN_KEYWORD i960_cgen_opval_h_gr = 
{
  & i960_cgen_opval_h_gr_entries[0],
  34
};

CGEN_KEYWORD_ENTRY i960_cgen_opval_h_cc_entries[] = 
{
  { "cc", 0 }
};

CGEN_KEYWORD i960_cgen_opval_h_cc = 
{
  & i960_cgen_opval_h_cc_entries[0],
  1
};



/* The hardware table.  */

#define A(a) (1 << (CONCAT2 (CGEN_HW_,a) - CGEN_ATTR_BOOL_OFFSET))
#define HW_ENT(n) i960_cgen_hw_table[n]

const CGEN_HW_ENTRY i960_cgen_hw_table[] =
{
  { HW_H_PC, & HW_ENT (HW_H_PC + 1), "h-pc", CGEN_ASM_KEYWORD, (PTR) 0, { CGEN_HW_NBOOL_ATTRS, 0|A(PROFILE)|A(PC), { (1<<MACH_BASE) } } },
  { HW_H_MEMORY, & HW_ENT (HW_H_MEMORY + 1), "h-memory", CGEN_ASM_KEYWORD, (PTR) 0, { CGEN_HW_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } } },
  { HW_H_SINT, & HW_ENT (HW_H_SINT + 1), "h-sint", CGEN_ASM_KEYWORD, (PTR) 0, { CGEN_HW_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } } },
  { HW_H_UINT, & HW_ENT (HW_H_UINT + 1), "h-uint", CGEN_ASM_KEYWORD, (PTR) 0, { CGEN_HW_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } } },
  { HW_H_ADDR, & HW_ENT (HW_H_ADDR + 1), "h-addr", CGEN_ASM_KEYWORD, (PTR) 0, { CGEN_HW_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } } },
  { HW_H_IADDR, & HW_ENT (HW_H_IADDR + 1), "h-iaddr", CGEN_ASM_KEYWORD, (PTR) 0, { CGEN_HW_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } } },
  { HW_H_GR, & HW_ENT (HW_H_GR + 1), "h-gr", CGEN_ASM_KEYWORD, (PTR) & i960_cgen_opval_h_gr, { CGEN_HW_NBOOL_ATTRS, 0|A(CACHE_ADDR)|A(PROFILE), { (1<<MACH_BASE) } } },
  { HW_H_CC, & HW_ENT (HW_H_CC + 1), "h-cc", CGEN_ASM_KEYWORD, (PTR) & i960_cgen_opval_h_cc, { CGEN_HW_NBOOL_ATTRS, 0|A(CACHE_ADDR)|A(PROFILE), { (1<<MACH_BASE) } } },
  { 0 }
};

/* don't undef HW_ENT, used later */
#undef A

/* The instruction field table.  */

#define A(a) (1 << (CONCAT2 (CGEN_IFLD_,a) - CGEN_ATTR_BOOL_OFFSET))

const CGEN_IFLD i960_cgen_ifld_table[] =
{
  { I960_F_NIL, "f-nil", 0, 0, 0, 0, { CGEN_IFLD_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }  },
  { I960_F_OPCODE, "f-opcode", 0, 32, 0, 8, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_SRCDST, "f-srcdst", 0, 32, 8, 5, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_SRC2, "f-src2", 0, 32, 13, 5, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_M3, "f-m3", 0, 32, 18, 1, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_M2, "f-m2", 0, 32, 19, 1, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_M1, "f-m1", 0, 32, 20, 1, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_OPCODE2, "f-opcode2", 0, 32, 21, 4, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_ZERO, "f-zero", 0, 32, 25, 2, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_SRC1, "f-src1", 0, 32, 27, 5, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_ABASE, "f-abase", 0, 32, 13, 5, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_MODEA, "f-modea", 0, 32, 18, 1, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_ZEROA, "f-zeroa", 0, 32, 19, 1, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_OFFSET, "f-offset", 0, 32, 20, 12, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_MODEB, "f-modeb", 0, 32, 18, 4, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_SCALE, "f-scale", 0, 32, 22, 3, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_ZEROB, "f-zerob", 0, 32, 25, 2, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_INDEX, "f-index", 0, 32, 27, 5, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_OPTDISP, "f-optdisp", 32, 32, 0, 32, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_BR_SRC1, "f-br-src1", 0, 32, 8, 5, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_BR_SRC2, "f-br-src2", 0, 32, 13, 5, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_BR_M1, "f-br-m1", 0, 32, 18, 1, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_BR_DISP, "f-br-disp", 0, 32, 19, 11, { CGEN_IFLD_NBOOL_ATTRS, 0|A(PCREL_ADDR), { (1<<MACH_BASE) } }  },
  { I960_F_BR_ZERO, "f-br-zero", 0, 32, 30, 2, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { I960_F_CTRL_DISP, "f-ctrl-disp", 0, 32, 8, 22, { CGEN_IFLD_NBOOL_ATTRS, 0|A(PCREL_ADDR), { (1<<MACH_BASE) } }  },
  { I960_F_CTRL_ZERO, "f-ctrl-zero", 0, 32, 30, 2, { CGEN_IFLD_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
  { 0 }
};

#undef A

/* The operand table.  */

#define A(a) (1 << (CONCAT2 (CGEN_OPERAND_,a) - CGEN_ATTR_BOOL_OFFSET))
#define OPERAND(op) CONCAT2 (I960_OPERAND_,op)

const CGEN_OPERAND i960_cgen_operand_table[MAX_OPERANDS] =
{
/* pc: program counter */
  { "pc", & HW_ENT (HW_H_PC), 0, 0,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(SEM_ONLY), { (1<<MACH_BASE) } }  },
/* src1: source register 1 */
  { "src1", & HW_ENT (HW_H_GR), 27, 5,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
/* src2: source register 2 */
  { "src2", & HW_ENT (HW_H_GR), 13, 5,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
/* dst: source/dest register */
  { "dst", & HW_ENT (HW_H_GR), 8, 5,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
/* lit1: literal 1 */
  { "lit1", & HW_ENT (HW_H_UINT), 27, 5,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
/* lit2: literal 2 */
  { "lit2", & HW_ENT (HW_H_UINT), 13, 5,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
/* st_src: store src */
  { "st_src", & HW_ENT (HW_H_GR), 8, 5,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
/* abase: abase */
  { "abase", & HW_ENT (HW_H_GR), 13, 5,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
/* offset: offset */
  { "offset", & HW_ENT (HW_H_UINT), 20, 12,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
/* scale: scale */
  { "scale", & HW_ENT (HW_H_UINT), 22, 3,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
/* index: index */
  { "index", & HW_ENT (HW_H_GR), 27, 5,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
/* optdisp: optional displacement */
  { "optdisp", & HW_ENT (HW_H_UINT), 0, 32,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
/* br_src1: branch src1 */
  { "br_src1", & HW_ENT (HW_H_GR), 8, 5,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
/* br_src2: branch src2 */
  { "br_src2", & HW_ENT (HW_H_GR), 13, 5,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
/* br_disp: branch displacement */
  { "br_disp", & HW_ENT (HW_H_IADDR), 19, 11,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(PCREL_ADDR), { (1<<MACH_BASE) } }  },
/* br_lit1: branch literal 1 */
  { "br_lit1", & HW_ENT (HW_H_UINT), 8, 5,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(UNSIGNED), { (1<<MACH_BASE) } }  },
/* ctrl_disp: ctrl branch disp */
  { "ctrl_disp", & HW_ENT (HW_H_IADDR), 8, 22,
    { CGEN_OPERAND_NBOOL_ATTRS, 0|A(PCREL_ADDR), { (1<<MACH_BASE) } }  },
};

#undef A

#define A(a) (1 << (CONCAT2 (CGEN_INSN_,a) - CGEN_ATTR_BOOL_OFFSET))
#define OP(field) CGEN_SYNTAX_MAKE_FIELD (OPERAND (field))

/* The instruction table.  */

static const CGEN_IBASE i960_cgen_insn_table[MAX_INSNS] =
{
  /* Special null first entry.
     A `num' value of zero is thus invalid.
     Also, the special `invalid' insn resides here.  */
  { 0, 0, 0 },
/* mulo $src1, $src2, $dst */
  {
    I960_INSN_MULO, "mulo", "mulo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* mulo $lit1, $src2, $dst */
  {
    I960_INSN_MULO1, "mulo1", "mulo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* mulo $src1, $lit2, $dst */
  {
    I960_INSN_MULO2, "mulo2", "mulo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* mulo $lit1, $lit2, $dst */
  {
    I960_INSN_MULO3, "mulo3", "mulo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* remo $src1, $src2, $dst */
  {
    I960_INSN_REMO, "remo", "remo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* remo $lit1, $src2, $dst */
  {
    I960_INSN_REMO1, "remo1", "remo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* remo $src1, $lit2, $dst */
  {
    I960_INSN_REMO2, "remo2", "remo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* remo $lit1, $lit2, $dst */
  {
    I960_INSN_REMO3, "remo3", "remo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* divo $src1, $src2, $dst */
  {
    I960_INSN_DIVO, "divo", "divo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* divo $lit1, $src2, $dst */
  {
    I960_INSN_DIVO1, "divo1", "divo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* divo $src1, $lit2, $dst */
  {
    I960_INSN_DIVO2, "divo2", "divo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* divo $lit1, $lit2, $dst */
  {
    I960_INSN_DIVO3, "divo3", "divo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* remi $src1, $src2, $dst */
  {
    I960_INSN_REMI, "remi", "remi", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* remi $lit1, $src2, $dst */
  {
    I960_INSN_REMI1, "remi1", "remi", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* remi $src1, $lit2, $dst */
  {
    I960_INSN_REMI2, "remi2", "remi", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* remi $lit1, $lit2, $dst */
  {
    I960_INSN_REMI3, "remi3", "remi", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* divi $src1, $src2, $dst */
  {
    I960_INSN_DIVI, "divi", "divi", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* divi $lit1, $src2, $dst */
  {
    I960_INSN_DIVI1, "divi1", "divi", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* divi $src1, $lit2, $dst */
  {
    I960_INSN_DIVI2, "divi2", "divi", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* divi $lit1, $lit2, $dst */
  {
    I960_INSN_DIVI3, "divi3", "divi", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* addo $src1, $src2, $dst */
  {
    I960_INSN_ADDO, "addo", "addo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* addo $lit1, $src2, $dst */
  {
    I960_INSN_ADDO1, "addo1", "addo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* addo $src1, $lit2, $dst */
  {
    I960_INSN_ADDO2, "addo2", "addo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* addo $lit1, $lit2, $dst */
  {
    I960_INSN_ADDO3, "addo3", "addo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* subo $src1, $src2, $dst */
  {
    I960_INSN_SUBO, "subo", "subo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* subo $lit1, $src2, $dst */
  {
    I960_INSN_SUBO1, "subo1", "subo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* subo $src1, $lit2, $dst */
  {
    I960_INSN_SUBO2, "subo2", "subo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* subo $lit1, $lit2, $dst */
  {
    I960_INSN_SUBO3, "subo3", "subo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* notbit $src1, $src2, $dst */
  {
    I960_INSN_NOTBIT, "notbit", "notbit", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* notbit $lit1, $src2, $dst */
  {
    I960_INSN_NOTBIT1, "notbit1", "notbit", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* notbit $src1, $lit2, $dst */
  {
    I960_INSN_NOTBIT2, "notbit2", "notbit", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* notbit $lit1, $lit2, $dst */
  {
    I960_INSN_NOTBIT3, "notbit3", "notbit", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* and $src1, $src2, $dst */
  {
    I960_INSN_AND, "and", "and", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* and $lit1, $src2, $dst */
  {
    I960_INSN_AND1, "and1", "and", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* and $src1, $lit2, $dst */
  {
    I960_INSN_AND2, "and2", "and", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* and $lit1, $lit2, $dst */
  {
    I960_INSN_AND3, "and3", "and", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* andnot $src1, $src2, $dst */
  {
    I960_INSN_ANDNOT, "andnot", "andnot", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* andnot $lit1, $src2, $dst */
  {
    I960_INSN_ANDNOT1, "andnot1", "andnot", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* andnot $src1, $lit2, $dst */
  {
    I960_INSN_ANDNOT2, "andnot2", "andnot", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* andnot $lit1, $lit2, $dst */
  {
    I960_INSN_ANDNOT3, "andnot3", "andnot", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* setbit $src1, $src2, $dst */
  {
    I960_INSN_SETBIT, "setbit", "setbit", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* setbit $lit1, $src2, $dst */
  {
    I960_INSN_SETBIT1, "setbit1", "setbit", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* setbit $src1, $lit2, $dst */
  {
    I960_INSN_SETBIT2, "setbit2", "setbit", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* setbit $lit1, $lit2, $dst */
  {
    I960_INSN_SETBIT3, "setbit3", "setbit", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* notand $src1, $src2, $dst */
  {
    I960_INSN_NOTAND, "notand", "notand", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* notand $lit1, $src2, $dst */
  {
    I960_INSN_NOTAND1, "notand1", "notand", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* notand $src1, $lit2, $dst */
  {
    I960_INSN_NOTAND2, "notand2", "notand", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* notand $lit1, $lit2, $dst */
  {
    I960_INSN_NOTAND3, "notand3", "notand", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* xor $src1, $src2, $dst */
  {
    I960_INSN_XOR, "xor", "xor", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* xor $lit1, $src2, $dst */
  {
    I960_INSN_XOR1, "xor1", "xor", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* xor $src1, $lit2, $dst */
  {
    I960_INSN_XOR2, "xor2", "xor", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* xor $lit1, $lit2, $dst */
  {
    I960_INSN_XOR3, "xor3", "xor", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* or $src1, $src2, $dst */
  {
    I960_INSN_OR, "or", "or", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* or $lit1, $src2, $dst */
  {
    I960_INSN_OR1, "or1", "or", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* or $src1, $lit2, $dst */
  {
    I960_INSN_OR2, "or2", "or", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* or $lit1, $lit2, $dst */
  {
    I960_INSN_OR3, "or3", "or", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* nor $src1, $src2, $dst */
  {
    I960_INSN_NOR, "nor", "nor", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* nor $lit1, $src2, $dst */
  {
    I960_INSN_NOR1, "nor1", "nor", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* nor $src1, $lit2, $dst */
  {
    I960_INSN_NOR2, "nor2", "nor", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* nor $lit1, $lit2, $dst */
  {
    I960_INSN_NOR3, "nor3", "nor", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* not $src1, $src2, $dst */
  {
    I960_INSN_NOT, "not", "not", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* not $lit1, $src2, $dst */
  {
    I960_INSN_NOT1, "not1", "not", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* not $src1, $lit2, $dst */
  {
    I960_INSN_NOT2, "not2", "not", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* not $lit1, $lit2, $dst */
  {
    I960_INSN_NOT3, "not3", "not", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* clrbit $src1, $src2, $dst */
  {
    I960_INSN_CLRBIT, "clrbit", "clrbit", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* clrbit $lit1, $src2, $dst */
  {
    I960_INSN_CLRBIT1, "clrbit1", "clrbit", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* clrbit $src1, $lit2, $dst */
  {
    I960_INSN_CLRBIT2, "clrbit2", "clrbit", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* clrbit $lit1, $lit2, $dst */
  {
    I960_INSN_CLRBIT3, "clrbit3", "clrbit", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shlo $src1, $src2, $dst */
  {
    I960_INSN_SHLO, "shlo", "shlo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shlo $lit1, $src2, $dst */
  {
    I960_INSN_SHLO1, "shlo1", "shlo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shlo $src1, $lit2, $dst */
  {
    I960_INSN_SHLO2, "shlo2", "shlo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shlo $lit1, $lit2, $dst */
  {
    I960_INSN_SHLO3, "shlo3", "shlo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shro $src1, $src2, $dst */
  {
    I960_INSN_SHRO, "shro", "shro", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shro $lit1, $src2, $dst */
  {
    I960_INSN_SHRO1, "shro1", "shro", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shro $src1, $lit2, $dst */
  {
    I960_INSN_SHRO2, "shro2", "shro", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shro $lit1, $lit2, $dst */
  {
    I960_INSN_SHRO3, "shro3", "shro", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shli $src1, $src2, $dst */
  {
    I960_INSN_SHLI, "shli", "shli", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shli $lit1, $src2, $dst */
  {
    I960_INSN_SHLI1, "shli1", "shli", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shli $src1, $lit2, $dst */
  {
    I960_INSN_SHLI2, "shli2", "shli", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shli $lit1, $lit2, $dst */
  {
    I960_INSN_SHLI3, "shli3", "shli", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shri $src1, $src2, $dst */
  {
    I960_INSN_SHRI, "shri", "shri", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shri $lit1, $src2, $dst */
  {
    I960_INSN_SHRI1, "shri1", "shri", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shri $src1, $lit2, $dst */
  {
    I960_INSN_SHRI2, "shri2", "shri", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* shri $lit1, $lit2, $dst */
  {
    I960_INSN_SHRI3, "shri3", "shri", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* emul $src1, $src2, $dst */
  {
    I960_INSN_EMUL, "emul", "emul", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* emul $lit1, $src2, $dst */
  {
    I960_INSN_EMUL1, "emul1", "emul", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* emul $src1, $lit2, $dst */
  {
    I960_INSN_EMUL2, "emul2", "emul", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* emul $lit1, $lit2, $dst */
  {
    I960_INSN_EMUL3, "emul3", "emul", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* mov $src1, $dst */
  {
    I960_INSN_MOV, "mov", "mov", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* mov $lit1, $dst */
  {
    I960_INSN_MOV1, "mov1", "mov", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* movl $src1, $dst */
  {
    I960_INSN_MOVL, "movl", "movl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* movl $lit1, $dst */
  {
    I960_INSN_MOVL1, "movl1", "movl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* movt $src1, $dst */
  {
    I960_INSN_MOVT, "movt", "movt", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* movt $lit1, $dst */
  {
    I960_INSN_MOVT1, "movt1", "movt", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* movq $src1, $dst */
  {
    I960_INSN_MOVQ, "movq", "movq", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* movq $lit1, $dst */
  {
    I960_INSN_MOVQ1, "movq1", "movq", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* modpc $src1, $src2, $dst */
  {
    I960_INSN_MODPC, "modpc", "modpc", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* modac $src1, $src2, $dst */
  {
    I960_INSN_MODAC, "modac", "modac", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* lda $offset, $dst */
  {
    I960_INSN_LDA_OFFSET, "lda-offset", "lda", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* lda $offset($abase), $dst */
  {
    I960_INSN_LDA_INDIRECT_OFFSET, "lda-indirect-offset", "lda", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* lda ($abase), $dst */
  {
    I960_INSN_LDA_INDIRECT, "lda-indirect", "lda", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* lda ($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDA_INDIRECT_INDEX, "lda-indirect-index", "lda", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* lda $optdisp, $dst */
  {
    I960_INSN_LDA_DISP, "lda-disp", "lda", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* lda $optdisp($abase), $dst */
  {
    I960_INSN_LDA_INDIRECT_DISP, "lda-indirect-disp", "lda", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* lda $optdisp[$index*S$scale], $dst */
  {
    I960_INSN_LDA_INDEX_DISP, "lda-index-disp", "lda", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* lda $optdisp($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDA_INDIRECT_INDEX_DISP, "lda-indirect-index-disp", "lda", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ld $offset, $dst */
  {
    I960_INSN_LD_OFFSET, "ld-offset", "ld", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ld $offset($abase), $dst */
  {
    I960_INSN_LD_INDIRECT_OFFSET, "ld-indirect-offset", "ld", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ld ($abase), $dst */
  {
    I960_INSN_LD_INDIRECT, "ld-indirect", "ld", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ld ($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LD_INDIRECT_INDEX, "ld-indirect-index", "ld", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ld $optdisp, $dst */
  {
    I960_INSN_LD_DISP, "ld-disp", "ld", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ld $optdisp($abase), $dst */
  {
    I960_INSN_LD_INDIRECT_DISP, "ld-indirect-disp", "ld", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ld $optdisp[$index*S$scale], $dst */
  {
    I960_INSN_LD_INDEX_DISP, "ld-index-disp", "ld", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ld $optdisp($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LD_INDIRECT_INDEX_DISP, "ld-indirect-index-disp", "ld", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldob $offset, $dst */
  {
    I960_INSN_LDOB_OFFSET, "ldob-offset", "ldob", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldob $offset($abase), $dst */
  {
    I960_INSN_LDOB_INDIRECT_OFFSET, "ldob-indirect-offset", "ldob", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldob ($abase), $dst */
  {
    I960_INSN_LDOB_INDIRECT, "ldob-indirect", "ldob", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldob ($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDOB_INDIRECT_INDEX, "ldob-indirect-index", "ldob", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldob $optdisp, $dst */
  {
    I960_INSN_LDOB_DISP, "ldob-disp", "ldob", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldob $optdisp($abase), $dst */
  {
    I960_INSN_LDOB_INDIRECT_DISP, "ldob-indirect-disp", "ldob", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldob $optdisp[$index*S$scale], $dst */
  {
    I960_INSN_LDOB_INDEX_DISP, "ldob-index-disp", "ldob", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldob $optdisp($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDOB_INDIRECT_INDEX_DISP, "ldob-indirect-index-disp", "ldob", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldos $offset, $dst */
  {
    I960_INSN_LDOS_OFFSET, "ldos-offset", "ldos", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldos $offset($abase), $dst */
  {
    I960_INSN_LDOS_INDIRECT_OFFSET, "ldos-indirect-offset", "ldos", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldos ($abase), $dst */
  {
    I960_INSN_LDOS_INDIRECT, "ldos-indirect", "ldos", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldos ($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDOS_INDIRECT_INDEX, "ldos-indirect-index", "ldos", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldos $optdisp, $dst */
  {
    I960_INSN_LDOS_DISP, "ldos-disp", "ldos", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldos $optdisp($abase), $dst */
  {
    I960_INSN_LDOS_INDIRECT_DISP, "ldos-indirect-disp", "ldos", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldos $optdisp[$index*S$scale], $dst */
  {
    I960_INSN_LDOS_INDEX_DISP, "ldos-index-disp", "ldos", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldos $optdisp($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDOS_INDIRECT_INDEX_DISP, "ldos-indirect-index-disp", "ldos", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldib $offset, $dst */
  {
    I960_INSN_LDIB_OFFSET, "ldib-offset", "ldib", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldib $offset($abase), $dst */
  {
    I960_INSN_LDIB_INDIRECT_OFFSET, "ldib-indirect-offset", "ldib", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldib ($abase), $dst */
  {
    I960_INSN_LDIB_INDIRECT, "ldib-indirect", "ldib", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldib ($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDIB_INDIRECT_INDEX, "ldib-indirect-index", "ldib", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldib $optdisp, $dst */
  {
    I960_INSN_LDIB_DISP, "ldib-disp", "ldib", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldib $optdisp($abase), $dst */
  {
    I960_INSN_LDIB_INDIRECT_DISP, "ldib-indirect-disp", "ldib", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldib $optdisp[$index*S$scale], $dst */
  {
    I960_INSN_LDIB_INDEX_DISP, "ldib-index-disp", "ldib", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldib $optdisp($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDIB_INDIRECT_INDEX_DISP, "ldib-indirect-index-disp", "ldib", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldis $offset, $dst */
  {
    I960_INSN_LDIS_OFFSET, "ldis-offset", "ldis", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldis $offset($abase), $dst */
  {
    I960_INSN_LDIS_INDIRECT_OFFSET, "ldis-indirect-offset", "ldis", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldis ($abase), $dst */
  {
    I960_INSN_LDIS_INDIRECT, "ldis-indirect", "ldis", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldis ($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDIS_INDIRECT_INDEX, "ldis-indirect-index", "ldis", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldis $optdisp, $dst */
  {
    I960_INSN_LDIS_DISP, "ldis-disp", "ldis", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldis $optdisp($abase), $dst */
  {
    I960_INSN_LDIS_INDIRECT_DISP, "ldis-indirect-disp", "ldis", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldis $optdisp[$index*S$scale], $dst */
  {
    I960_INSN_LDIS_INDEX_DISP, "ldis-index-disp", "ldis", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldis $optdisp($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDIS_INDIRECT_INDEX_DISP, "ldis-indirect-index-disp", "ldis", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldl $offset, $dst */
  {
    I960_INSN_LDL_OFFSET, "ldl-offset", "ldl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldl $offset($abase), $dst */
  {
    I960_INSN_LDL_INDIRECT_OFFSET, "ldl-indirect-offset", "ldl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldl ($abase), $dst */
  {
    I960_INSN_LDL_INDIRECT, "ldl-indirect", "ldl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldl ($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDL_INDIRECT_INDEX, "ldl-indirect-index", "ldl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldl $optdisp, $dst */
  {
    I960_INSN_LDL_DISP, "ldl-disp", "ldl", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldl $optdisp($abase), $dst */
  {
    I960_INSN_LDL_INDIRECT_DISP, "ldl-indirect-disp", "ldl", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldl $optdisp[$index*S$scale], $dst */
  {
    I960_INSN_LDL_INDEX_DISP, "ldl-index-disp", "ldl", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldl $optdisp($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDL_INDIRECT_INDEX_DISP, "ldl-indirect-index-disp", "ldl", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldt $offset, $dst */
  {
    I960_INSN_LDT_OFFSET, "ldt-offset", "ldt", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldt $offset($abase), $dst */
  {
    I960_INSN_LDT_INDIRECT_OFFSET, "ldt-indirect-offset", "ldt", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldt ($abase), $dst */
  {
    I960_INSN_LDT_INDIRECT, "ldt-indirect", "ldt", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldt ($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDT_INDIRECT_INDEX, "ldt-indirect-index", "ldt", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldt $optdisp, $dst */
  {
    I960_INSN_LDT_DISP, "ldt-disp", "ldt", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldt $optdisp($abase), $dst */
  {
    I960_INSN_LDT_INDIRECT_DISP, "ldt-indirect-disp", "ldt", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldt $optdisp[$index*S$scale], $dst */
  {
    I960_INSN_LDT_INDEX_DISP, "ldt-index-disp", "ldt", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldt $optdisp($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDT_INDIRECT_INDEX_DISP, "ldt-indirect-index-disp", "ldt", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldq $offset, $dst */
  {
    I960_INSN_LDQ_OFFSET, "ldq-offset", "ldq", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldq $offset($abase), $dst */
  {
    I960_INSN_LDQ_INDIRECT_OFFSET, "ldq-indirect-offset", "ldq", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldq ($abase), $dst */
  {
    I960_INSN_LDQ_INDIRECT, "ldq-indirect", "ldq", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldq ($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDQ_INDIRECT_INDEX, "ldq-indirect-index", "ldq", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldq $optdisp, $dst */
  {
    I960_INSN_LDQ_DISP, "ldq-disp", "ldq", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldq $optdisp($abase), $dst */
  {
    I960_INSN_LDQ_INDIRECT_DISP, "ldq-indirect-disp", "ldq", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldq $optdisp[$index*S$scale], $dst */
  {
    I960_INSN_LDQ_INDEX_DISP, "ldq-index-disp", "ldq", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* ldq $optdisp($abase)[$index*S$scale], $dst */
  {
    I960_INSN_LDQ_INDIRECT_INDEX_DISP, "ldq-indirect-index-disp", "ldq", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* st $st_src, $offset */
  {
    I960_INSN_ST_OFFSET, "st-offset", "st", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* st $st_src, $offset($abase) */
  {
    I960_INSN_ST_INDIRECT_OFFSET, "st-indirect-offset", "st", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* st $st_src, ($abase) */
  {
    I960_INSN_ST_INDIRECT, "st-indirect", "st", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* st $st_src, ($abase)[$index*S$scale] */
  {
    I960_INSN_ST_INDIRECT_INDEX, "st-indirect-index", "st", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* st $st_src, $optdisp */
  {
    I960_INSN_ST_DISP, "st-disp", "st", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* st $st_src, $optdisp($abase) */
  {
    I960_INSN_ST_INDIRECT_DISP, "st-indirect-disp", "st", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* st $st_src, $optdisp[$index*S$scale */
  {
    I960_INSN_ST_INDEX_DISP, "st-index-disp", "st", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* st $st_src, $optdisp($abase)[$index*S$scale] */
  {
    I960_INSN_ST_INDIRECT_INDEX_DISP, "st-indirect-index-disp", "st", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stob $st_src, $offset */
  {
    I960_INSN_STOB_OFFSET, "stob-offset", "stob", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stob $st_src, $offset($abase) */
  {
    I960_INSN_STOB_INDIRECT_OFFSET, "stob-indirect-offset", "stob", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stob $st_src, ($abase) */
  {
    I960_INSN_STOB_INDIRECT, "stob-indirect", "stob", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stob $st_src, ($abase)[$index*S$scale] */
  {
    I960_INSN_STOB_INDIRECT_INDEX, "stob-indirect-index", "stob", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stob $st_src, $optdisp */
  {
    I960_INSN_STOB_DISP, "stob-disp", "stob", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stob $st_src, $optdisp($abase) */
  {
    I960_INSN_STOB_INDIRECT_DISP, "stob-indirect-disp", "stob", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stob $st_src, $optdisp[$index*S$scale */
  {
    I960_INSN_STOB_INDEX_DISP, "stob-index-disp", "stob", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stob $st_src, $optdisp($abase)[$index*S$scale] */
  {
    I960_INSN_STOB_INDIRECT_INDEX_DISP, "stob-indirect-index-disp", "stob", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stos $st_src, $offset */
  {
    I960_INSN_STOS_OFFSET, "stos-offset", "stos", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stos $st_src, $offset($abase) */
  {
    I960_INSN_STOS_INDIRECT_OFFSET, "stos-indirect-offset", "stos", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stos $st_src, ($abase) */
  {
    I960_INSN_STOS_INDIRECT, "stos-indirect", "stos", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stos $st_src, ($abase)[$index*S$scale] */
  {
    I960_INSN_STOS_INDIRECT_INDEX, "stos-indirect-index", "stos", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stos $st_src, $optdisp */
  {
    I960_INSN_STOS_DISP, "stos-disp", "stos", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stos $st_src, $optdisp($abase) */
  {
    I960_INSN_STOS_INDIRECT_DISP, "stos-indirect-disp", "stos", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stos $st_src, $optdisp[$index*S$scale */
  {
    I960_INSN_STOS_INDEX_DISP, "stos-index-disp", "stos", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stos $st_src, $optdisp($abase)[$index*S$scale] */
  {
    I960_INSN_STOS_INDIRECT_INDEX_DISP, "stos-indirect-index-disp", "stos", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stl $st_src, $offset */
  {
    I960_INSN_STL_OFFSET, "stl-offset", "stl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stl $st_src, $offset($abase) */
  {
    I960_INSN_STL_INDIRECT_OFFSET, "stl-indirect-offset", "stl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stl $st_src, ($abase) */
  {
    I960_INSN_STL_INDIRECT, "stl-indirect", "stl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stl $st_src, ($abase)[$index*S$scale] */
  {
    I960_INSN_STL_INDIRECT_INDEX, "stl-indirect-index", "stl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stl $st_src, $optdisp */
  {
    I960_INSN_STL_DISP, "stl-disp", "stl", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stl $st_src, $optdisp($abase) */
  {
    I960_INSN_STL_INDIRECT_DISP, "stl-indirect-disp", "stl", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stl $st_src, $optdisp[$index*S$scale */
  {
    I960_INSN_STL_INDEX_DISP, "stl-index-disp", "stl", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stl $st_src, $optdisp($abase)[$index*S$scale] */
  {
    I960_INSN_STL_INDIRECT_INDEX_DISP, "stl-indirect-index-disp", "stl", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stt $st_src, $offset */
  {
    I960_INSN_STT_OFFSET, "stt-offset", "stt", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stt $st_src, $offset($abase) */
  {
    I960_INSN_STT_INDIRECT_OFFSET, "stt-indirect-offset", "stt", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stt $st_src, ($abase) */
  {
    I960_INSN_STT_INDIRECT, "stt-indirect", "stt", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stt $st_src, ($abase)[$index*S$scale] */
  {
    I960_INSN_STT_INDIRECT_INDEX, "stt-indirect-index", "stt", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stt $st_src, $optdisp */
  {
    I960_INSN_STT_DISP, "stt-disp", "stt", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stt $st_src, $optdisp($abase) */
  {
    I960_INSN_STT_INDIRECT_DISP, "stt-indirect-disp", "stt", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stt $st_src, $optdisp[$index*S$scale */
  {
    I960_INSN_STT_INDEX_DISP, "stt-index-disp", "stt", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stt $st_src, $optdisp($abase)[$index*S$scale] */
  {
    I960_INSN_STT_INDIRECT_INDEX_DISP, "stt-indirect-index-disp", "stt", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stq $st_src, $offset */
  {
    I960_INSN_STQ_OFFSET, "stq-offset", "stq", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stq $st_src, $offset($abase) */
  {
    I960_INSN_STQ_INDIRECT_OFFSET, "stq-indirect-offset", "stq", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stq $st_src, ($abase) */
  {
    I960_INSN_STQ_INDIRECT, "stq-indirect", "stq", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stq $st_src, ($abase)[$index*S$scale] */
  {
    I960_INSN_STQ_INDIRECT_INDEX, "stq-indirect-index", "stq", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stq $st_src, $optdisp */
  {
    I960_INSN_STQ_DISP, "stq-disp", "stq", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stq $st_src, $optdisp($abase) */
  {
    I960_INSN_STQ_INDIRECT_DISP, "stq-indirect-disp", "stq", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stq $st_src, $optdisp[$index*S$scale */
  {
    I960_INSN_STQ_INDEX_DISP, "stq-index-disp", "stq", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* stq $st_src, $optdisp($abase)[$index*S$scale] */
  {
    I960_INSN_STQ_INDIRECT_INDEX_DISP, "stq-indirect-index-disp", "stq", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* cmpobe $br_src1, $br_src2, $br_disp */
  {
    I960_INSN_CMPOBE_REG, "cmpobe-reg", "cmpobe", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpobe $br_lit1, $br_src2, $br_disp */
  {
    I960_INSN_CMPOBE_LIT, "cmpobe-lit", "cmpobe", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpobne $br_src1, $br_src2, $br_disp */
  {
    I960_INSN_CMPOBNE_REG, "cmpobne-reg", "cmpobne", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpobne $br_lit1, $br_src2, $br_disp */
  {
    I960_INSN_CMPOBNE_LIT, "cmpobne-lit", "cmpobne", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpobl $br_src1, $br_src2, $br_disp */
  {
    I960_INSN_CMPOBL_REG, "cmpobl-reg", "cmpobl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpobl $br_lit1, $br_src2, $br_disp */
  {
    I960_INSN_CMPOBL_LIT, "cmpobl-lit", "cmpobl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpoble $br_src1, $br_src2, $br_disp */
  {
    I960_INSN_CMPOBLE_REG, "cmpoble-reg", "cmpoble", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpoble $br_lit1, $br_src2, $br_disp */
  {
    I960_INSN_CMPOBLE_LIT, "cmpoble-lit", "cmpoble", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpobg $br_src1, $br_src2, $br_disp */
  {
    I960_INSN_CMPOBG_REG, "cmpobg-reg", "cmpobg", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpobg $br_lit1, $br_src2, $br_disp */
  {
    I960_INSN_CMPOBG_LIT, "cmpobg-lit", "cmpobg", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpobge $br_src1, $br_src2, $br_disp */
  {
    I960_INSN_CMPOBGE_REG, "cmpobge-reg", "cmpobge", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpobge $br_lit1, $br_src2, $br_disp */
  {
    I960_INSN_CMPOBGE_LIT, "cmpobge-lit", "cmpobge", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpibe $br_src1, $br_src2, $br_disp */
  {
    I960_INSN_CMPIBE_REG, "cmpibe-reg", "cmpibe", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpibe $br_lit1, $br_src2, $br_disp */
  {
    I960_INSN_CMPIBE_LIT, "cmpibe-lit", "cmpibe", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpibne $br_src1, $br_src2, $br_disp */
  {
    I960_INSN_CMPIBNE_REG, "cmpibne-reg", "cmpibne", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpibne $br_lit1, $br_src2, $br_disp */
  {
    I960_INSN_CMPIBNE_LIT, "cmpibne-lit", "cmpibne", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpibl $br_src1, $br_src2, $br_disp */
  {
    I960_INSN_CMPIBL_REG, "cmpibl-reg", "cmpibl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpibl $br_lit1, $br_src2, $br_disp */
  {
    I960_INSN_CMPIBL_LIT, "cmpibl-lit", "cmpibl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpible $br_src1, $br_src2, $br_disp */
  {
    I960_INSN_CMPIBLE_REG, "cmpible-reg", "cmpible", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpible $br_lit1, $br_src2, $br_disp */
  {
    I960_INSN_CMPIBLE_LIT, "cmpible-lit", "cmpible", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpibg $br_src1, $br_src2, $br_disp */
  {
    I960_INSN_CMPIBG_REG, "cmpibg-reg", "cmpibg", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpibg $br_lit1, $br_src2, $br_disp */
  {
    I960_INSN_CMPIBG_LIT, "cmpibg-lit", "cmpibg", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpibge $br_src1, $br_src2, $br_disp */
  {
    I960_INSN_CMPIBGE_REG, "cmpibge-reg", "cmpibge", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpibge $br_lit1, $br_src2, $br_disp */
  {
    I960_INSN_CMPIBGE_LIT, "cmpibge-lit", "cmpibge", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* bbc $br_src1, $br_src2, $br_disp */
  {
    I960_INSN_BBC_REG, "bbc-reg", "bbc", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* bbc $br_lit1, $br_src2, $br_disp */
  {
    I960_INSN_BBC_LIT, "bbc-lit", "bbc", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* bbs $br_src1, $br_src2, $br_disp */
  {
    I960_INSN_BBS_REG, "bbs-reg", "bbs", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* bbs $br_lit1, $br_src2, $br_disp */
  {
    I960_INSN_BBS_LIT, "bbs-lit", "bbs", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* cmpi $src1, $src2 */
  {
    I960_INSN_CMPI, "cmpi", "cmpi", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* cmpi $lit1, $src2 */
  {
    I960_INSN_CMPI1, "cmpi1", "cmpi", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* cmpi $src1, $lit2 */
  {
    I960_INSN_CMPI2, "cmpi2", "cmpi", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* cmpi $lit1, $lit2 */
  {
    I960_INSN_CMPI3, "cmpi3", "cmpi", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* cmpo $src1, $src2 */
  {
    I960_INSN_CMPO, "cmpo", "cmpo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* cmpo $lit1, $src2 */
  {
    I960_INSN_CMPO1, "cmpo1", "cmpo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* cmpo $src1, $lit2 */
  {
    I960_INSN_CMPO2, "cmpo2", "cmpo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* cmpo $lit1, $lit2 */
  {
    I960_INSN_CMPO3, "cmpo3", "cmpo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* testno $br_src1 */
  {
    I960_INSN_TESTNO_REG, "testno-reg", "testno", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* testg $br_src1 */
  {
    I960_INSN_TESTG_REG, "testg-reg", "testg", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* teste $br_src1 */
  {
    I960_INSN_TESTE_REG, "teste-reg", "teste", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* testge $br_src1 */
  {
    I960_INSN_TESTGE_REG, "testge-reg", "testge", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* testl $br_src1 */
  {
    I960_INSN_TESTL_REG, "testl-reg", "testl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* testne $br_src1 */
  {
    I960_INSN_TESTNE_REG, "testne-reg", "testne", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* testle $br_src1 */
  {
    I960_INSN_TESTLE_REG, "testle-reg", "testle", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* testo $br_src1 */
  {
    I960_INSN_TESTO_REG, "testo-reg", "testo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
/* bno $ctrl_disp */
  {
    I960_INSN_BNO, "bno", "bno", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* bg $ctrl_disp */
  {
    I960_INSN_BG, "bg", "bg", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* be $ctrl_disp */
  {
    I960_INSN_BE, "be", "be", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* bge $ctrl_disp */
  {
    I960_INSN_BGE, "bge", "bge", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* bl $ctrl_disp */
  {
    I960_INSN_BL, "bl", "bl", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* bne $ctrl_disp */
  {
    I960_INSN_BNE, "bne", "bne", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* ble $ctrl_disp */
  {
    I960_INSN_BLE, "ble", "ble", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* bo $ctrl_disp */
  {
    I960_INSN_BO, "bo", "bo", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(COND_CTI), { (1<<MACH_BASE) } }
  },
/* b $ctrl_disp */
  {
    I960_INSN_B, "b", "b", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(UNCOND_CTI), { (1<<MACH_BASE) } }
  },
/* bx $offset($abase) */
  {
    I960_INSN_BX_INDIRECT_OFFSET, "bx-indirect-offset", "bx", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(UNCOND_CTI), { (1<<MACH_BASE) } }
  },
/* bx ($abase) */
  {
    I960_INSN_BX_INDIRECT, "bx-indirect", "bx", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(UNCOND_CTI), { (1<<MACH_BASE) } }
  },
/* bx ($abase)[$index*S$scale] */
  {
    I960_INSN_BX_INDIRECT_INDEX, "bx-indirect-index", "bx", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(UNCOND_CTI), { (1<<MACH_BASE) } }
  },
/* bx $optdisp */
  {
    I960_INSN_BX_DISP, "bx-disp", "bx", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(UNCOND_CTI), { (1<<MACH_BASE) } }
  },
/* bx $optdisp($abase) */
  {
    I960_INSN_BX_INDIRECT_DISP, "bx-indirect-disp", "bx", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(UNCOND_CTI), { (1<<MACH_BASE) } }
  },
/* callx $optdisp */
  {
    I960_INSN_CALLX_DISP, "callx-disp", "callx", 64,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(UNCOND_CTI), { (1<<MACH_BASE) } }
  },
/* callx ($abase) */
  {
    I960_INSN_CALLX_INDIRECT, "callx-indirect", "callx", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(UNCOND_CTI), { (1<<MACH_BASE) } }
  },
/* callx $offset($abase) */
  {
    I960_INSN_CALLX_INDIRECT_OFFSET, "callx-indirect-offset", "callx", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(UNCOND_CTI), { (1<<MACH_BASE) } }
  },
/* ret */
  {
    I960_INSN_RET, "ret", "ret", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(UNCOND_CTI), { (1<<MACH_BASE) } }
  },
/* calls $src1 */
  {
    I960_INSN_CALLS, "calls", "calls", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(UNCOND_CTI), { (1<<MACH_BASE) } }
  },
/* fmark */
  {
    I960_INSN_FMARK, "fmark", "fmark", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0|A(UNCOND_CTI), { (1<<MACH_BASE) } }
  },
/* flushreg */
  {
    I960_INSN_FLUSHREG, "flushreg", "flushreg", 32,
    { CGEN_INSN_NBOOL_ATTRS, 0, { (1<<MACH_BASE) } }
  },
};

#undef A
#undef MNEM
#undef OP

static void
init_tables ()
{
}

/* Initialize a cpu table and return a descriptor.
   It's much like opening a file, and must be the first function called.  */

CGEN_CPU_DESC
i960_cgen_cpu_open (mach, endian)
     int mach;
     enum cgen_endian endian;
{
  CGEN_CPU_TABLE *cd = (CGEN_CPU_TABLE *) xmalloc (sizeof (CGEN_CPU_TABLE));
  static int init_p;

  if (! init_p)
    {
      init_tables ();
      init_p = 1;
    }

  memset (cd, 0, sizeof (*cd));

  cd->mach = mach;
  cd->endian = endian;
  /* FIXME: for the sparc case we can determine insn-endianness statically.
     The worry here is where both data and insn endian can be independently
     chosen, in which case this function will need another argument.
     Actually, will want to allow for more arguments in the future anyway.  */
  cd->insn_endian = endian;

  cd->int_insn_p = CGEN_INT_INSN_P;

  cd->max_insn_size = CGEN_MAX_INSN_SIZE;

  cd->hw_list = & i960_cgen_hw_table[0];

  cd->ifld_table = & i960_cgen_ifld_table[0];

  cd->operand_table = & i960_cgen_operand_table[0];

  {
    int i;
    const CGEN_IBASE *ib = & i960_cgen_insn_table[0];
    CGEN_INSN *insns = (CGEN_INSN *) xmalloc (MAX_INSNS * sizeof (CGEN_INSN));
    memset (insns, 0, MAX_INSNS * sizeof (CGEN_INSN));
    for (i = 0; i < MAX_INSNS; ++i)
      insns[i].base = &ib[i];
    cd->insn_table.init_entries = insns;
  }
  cd->insn_table.entry_size = sizeof (CGEN_IBASE);
  cd->insn_table.num_init_entries = MAX_INSNS;

  return (CGEN_CPU_DESC) cd;
}

/* Close a cpu table.  */

void
i960_cgen_cpu_close (cd)
     CGEN_CPU_DESC cd;
{
  if (cd->insn_table.init_entries)
    free ((CGEN_INSN *) cd->insn_table.init_entries);
  free (cd);
}

