/* CPU data header for fr30.

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

#ifndef FR30_CPU_H
#define FR30_CPU_H

#define CGEN_ARCH fr30

/* Given symbol S, return fr30_cgen_<S>.  */
#define CGEN_SYM(s) CONCAT3 (fr30,_cgen_,s)

/* Selected cpu families.  */
#define HAVE_CPU_FR30BF

#define CGEN_INSN_LSB0_P 0
#define CGEN_WORD_BITSIZE 32
#define CGEN_DEFAULT_INSN_BITSIZE 16
#define CGEN_BASE_INSN_BITSIZE 16
#define CGEN_MIN_INSN_BITSIZE 16
#define CGEN_MAX_INSN_BITSIZE 48
#define CGEN_DEFAULT_INSN_SIZE (CGEN_DEFAULT_INSN_BITSIZE / 8)
#define CGEN_BASE_INSN_SIZE (CGEN_BASE_INSN_BITSIZE / 8)
#define CGEN_MIN_INSN_SIZE (CGEN_MIN_INSN_BITSIZE / 8)
#define CGEN_MAX_INSN_SIZE (CGEN_MAX_INSN_BITSIZE / 8)
#define CGEN_INT_INSN_P 0

/* FIXME: Need to compute CGEN_MAX_SYNTAX_BYTES.  */

/* CGEN_MNEMONIC_OPERANDS is defined if mnemonics have operands.
   e.g. In "b,a foo" the ",a" is an operand.  If mnemonics have operands
   we can't hash on everything up to the space.  */
#define CGEN_MNEMONIC_OPERANDS
/* Maximum number of operands any insn or macro-insn has.  */
#define CGEN_MAX_INSN_OPERANDS 16

/* Maximum number of fields in an instruction.  */
#define CGEN_MAX_IFMT_OPERANDS 7

/* Enums.  */

/* Enum declaration for insn op1 enums.  */
typedef enum insn_op1 {
  OP1_0, OP1_1, OP1_2, OP1_3
 , OP1_4, OP1_5, OP1_6, OP1_7
 , OP1_8, OP1_9, OP1_A, OP1_B
 , OP1_C, OP1_D, OP1_E, OP1_F
} INSN_OP1;

/* Enum declaration for insn op2 enums.  */
typedef enum insn_op2 {
  OP2_0, OP2_1, OP2_2, OP2_3
 , OP2_4, OP2_5, OP2_6, OP2_7
 , OP2_8, OP2_9, OP2_A, OP2_B
 , OP2_C, OP2_D, OP2_E, OP2_F
} INSN_OP2;

/* Enum declaration for insn op3 enums.  */
typedef enum insn_op3 {
  OP3_0, OP3_1, OP3_2, OP3_3
 , OP3_4, OP3_5, OP3_6, OP3_7
 , OP3_8, OP3_9, OP3_A, OP3_B
 , OP3_C, OP3_D, OP3_E, OP3_F
} INSN_OP3;

/* Enum declaration for insn op4 enums.  */
typedef enum insn_op4 {
  OP4_0
} INSN_OP4;

/* Enum declaration for insn op5 enums.  */
typedef enum insn_op5 {
  OP5_0, OP5_1
} INSN_OP5;

/* Enum declaration for insn cc enums.  */
typedef enum insn_cc {
  CC_RA, CC_NO, CC_EQ, CC_NE
 , CC_C, CC_NC, CC_N, CC_P
 , CC_V, CC_NV, CC_LT, CC_GE
 , CC_LE, CC_GT, CC_LS, CC_HI
} INSN_CC;

/* Enum declaration for general registers.  */
typedef enum h_gr {
  H_GR_R0 = 0, H_GR_R1 = 1, H_GR_R2 = 2, H_GR_R3 = 3
 , H_GR_R4 = 4, H_GR_R5 = 5, H_GR_R6 = 6, H_GR_R7 = 7
 , H_GR_R8 = 8, H_GR_R9 = 9, H_GR_R10 = 10, H_GR_R11 = 11
 , H_GR_R12 = 12, H_GR_R13 = 13, H_GR_R14 = 14, H_GR_R15 = 15
 , H_GR_AC = 13, H_GR_FP = 14, H_GR_SP = 15
} H_GR;

/* Enum declaration for coprocessor registers.  */
typedef enum h_cr {
  H_CR_CR0, H_CR_CR1, H_CR_CR2, H_CR_CR3
 , H_CR_CR4, H_CR_CR5, H_CR_CR6, H_CR_CR7
 , H_CR_CR8, H_CR_CR9, H_CR_CR10, H_CR_CR11
 , H_CR_CR12, H_CR_CR13, H_CR_CR14, H_CR_CR15
} H_CR;

/* Enum declaration for dedicated registers.  */
typedef enum h_dr {
  H_DR_TBR, H_DR_RP, H_DR_SSP, H_DR_USP
 , H_DR_MDH, H_DR_MDL
} H_DR;

/* Enum declaration for program status.  */
typedef enum h_ps {
  H_PS_PS
} H_PS;

/* Enum declaration for General Register 13 explicitely required.  */
typedef enum h_r13 {
  H_R13_R13
} H_R13;

/* Enum declaration for General Register 14 explicitely required.  */
typedef enum h_r14 {
  H_R14_R14
} H_R14;

/* Enum declaration for General Register 15 explicitely required.  */
typedef enum h_r15 {
  H_R15_R15
} H_R15;

/* Attributes.  */

/* Enum declaration for machine type selection.  */
typedef enum mach_attr {
  MACH_BASE, MACH_FR30, MACH_MAX
} MACH_ATTR;

/* Number of architecture variants.  */
#define MAX_MACHS ((int) MACH_MAX)

/* Ifield support.  */

extern const struct cgen_ifld fr30_cgen_ifld_table[];

/* Ifield attribute indices.  */

/* Enum declaration for cgen_ifld attrs.  */
typedef enum cgen_ifld_attr {
  CGEN_IFLD_MACH, CGEN_IFLD_NBOOLS, CGEN_IFLD_START_BOOL = 31, CGEN_IFLD_VIRTUAL
 , CGEN_IFLD_UNSIGNED, CGEN_IFLD_PCREL_ADDR, CGEN_IFLD_ABS_ADDR, CGEN_IFLD_RESERVED
 , CGEN_IFLD_SIGN_OPT
} CGEN_IFLD_ATTR;

/* Number of non-boolean elements in cgen_ifld.  */
#define CGEN_IFLD_NBOOL_ATTRS ((int) CGEN_IFLD_NBOOLS)

/* Enum declaration for fr30 ifield types.  */
typedef enum ifield_type {
  FR30_F_NIL, FR30_F_OP1, FR30_F_OP2, FR30_F_OP3
 , FR30_F_OP4, FR30_F_OP5, FR30_F_CC, FR30_F_CCC
 , FR30_F_RJ, FR30_F_RI, FR30_F_RS1, FR30_F_RS2
 , FR30_F_RJC, FR30_F_RIC, FR30_F_CRJ, FR30_F_CRI
 , FR30_F_U4, FR30_F_U4C, FR30_F_I4, FR30_F_M4
 , FR30_F_U8, FR30_F_I8, FR30_F_I20_4, FR30_F_I20_16
 , FR30_F_I20, FR30_F_I32, FR30_F_UDISP6, FR30_F_DISP8
 , FR30_F_DISP9, FR30_F_DISP10, FR30_F_S10, FR30_F_U10
 , FR30_F_REL9, FR30_F_DIR8, FR30_F_DIR9, FR30_F_DIR10
 , FR30_F_REL12, FR30_F_REGLIST_HI_ST, FR30_F_REGLIST_LOW_ST, FR30_F_REGLIST_HI_LD
 , FR30_F_REGLIST_LOW_LD, FR30_F_MAX
} IFIELD_TYPE;

#define MAX_IFLD ((int) FR30_F_MAX)

/* Hardware attribute indices.  */

/* Enum declaration for cgen_hw attrs.  */
typedef enum cgen_hw_attr {
  CGEN_HW_MACH, CGEN_HW_NBOOLS, CGEN_HW_START_BOOL = 31, CGEN_HW_VIRTUAL
 , CGEN_HW_UNSIGNED, CGEN_HW_SIGNED, CGEN_HW_CACHE_ADDR, CGEN_HW_FUN_ACCESS
 , CGEN_HW_PC, CGEN_HW_PROFILE
} CGEN_HW_ATTR;

/* Number of non-boolean elements in cgen_hw.  */
#define CGEN_HW_NBOOL_ATTRS ((int) CGEN_HW_NBOOLS)

/* Enum declaration for fr30 hardware types.  */
typedef enum hw_type {
  HW_H_PC, HW_H_MEMORY, HW_H_SINT, HW_H_UINT
 , HW_H_ADDR, HW_H_IADDR, HW_H_GR, HW_H_CR
 , HW_H_DR, HW_H_PS, HW_H_R13, HW_H_R14
 , HW_H_R15, HW_H_NBIT, HW_H_ZBIT, HW_H_VBIT
 , HW_H_CBIT, HW_H_IBIT, HW_H_SBIT, HW_H_TBIT
 , HW_H_D0BIT, HW_H_D1BIT, HW_H_CCR, HW_H_SCR
 , HW_H_ILM, HW_MAX
} HW_TYPE;

#define MAX_HW ((int) HW_MAX)

/* Operand attribute indices.  */

/* Enum declaration for cgen_operand attrs.  */
typedef enum cgen_operand_attr {
  CGEN_OPERAND_MACH, CGEN_OPERAND_NBOOLS, CGEN_OPERAND_START_BOOL = 31, CGEN_OPERAND_VIRTUAL
 , CGEN_OPERAND_UNSIGNED, CGEN_OPERAND_PCREL_ADDR, CGEN_OPERAND_ABS_ADDR, CGEN_OPERAND_SIGN_OPT
 , CGEN_OPERAND_NEGATIVE, CGEN_OPERAND_RELAX, CGEN_OPERAND_SEM_ONLY, CGEN_OPERAND_HASH_PREFIX
} CGEN_OPERAND_ATTR;

/* Number of non-boolean elements in cgen_operand.  */
#define CGEN_OPERAND_NBOOL_ATTRS ((int) CGEN_OPERAND_NBOOLS)

/* Enum declaration for fr30 operand types.  */
typedef enum cgen_operand_type {
  FR30_OPERAND_PC, FR30_OPERAND_RI, FR30_OPERAND_RJ, FR30_OPERAND_RIC
 , FR30_OPERAND_RJC, FR30_OPERAND_CRI, FR30_OPERAND_CRJ, FR30_OPERAND_RS1
 , FR30_OPERAND_RS2, FR30_OPERAND_R13, FR30_OPERAND_R14, FR30_OPERAND_R15
 , FR30_OPERAND_PS, FR30_OPERAND_U4, FR30_OPERAND_U4C, FR30_OPERAND_U8
 , FR30_OPERAND_I8, FR30_OPERAND_UDISP6, FR30_OPERAND_DISP8, FR30_OPERAND_DISP9
 , FR30_OPERAND_DISP10, FR30_OPERAND_S10, FR30_OPERAND_U10, FR30_OPERAND_I32
 , FR30_OPERAND_M4, FR30_OPERAND_I20, FR30_OPERAND_DIR8, FR30_OPERAND_DIR9
 , FR30_OPERAND_DIR10, FR30_OPERAND_LABEL9, FR30_OPERAND_LABEL12, FR30_OPERAND_REGLIST_LOW_LD
 , FR30_OPERAND_REGLIST_HI_LD, FR30_OPERAND_REGLIST_LOW_ST, FR30_OPERAND_REGLIST_HI_ST, FR30_OPERAND_CC
 , FR30_OPERAND_CCC, FR30_OPERAND_NBIT, FR30_OPERAND_VBIT, FR30_OPERAND_ZBIT
 , FR30_OPERAND_CBIT, FR30_OPERAND_IBIT, FR30_OPERAND_SBIT, FR30_OPERAND_TBIT
 , FR30_OPERAND_D0BIT, FR30_OPERAND_D1BIT, FR30_OPERAND_CCR, FR30_OPERAND_SCR
 , FR30_OPERAND_ILM, FR30_OPERAND_MAX
} CGEN_OPERAND_TYPE;

/* Number of operands types.  */
#define MAX_OPERANDS ((int) FR30_OPERAND_MAX)

/* Maximum number of operands referenced by any insn.  */
#define MAX_OPERAND_INSTANCES 8

/* Insn attribute indices.  */

/* Enum declaration for cgen_insn attrs.  */
typedef enum cgen_insn_attr {
  CGEN_INSN_MACH, CGEN_INSN_NBOOLS, CGEN_INSN_START_BOOL = 31, CGEN_INSN_ALIAS
 , CGEN_INSN_VIRTUAL, CGEN_INSN_UNCOND_CTI, CGEN_INSN_COND_CTI, CGEN_INSN_SKIP_CTI
 , CGEN_INSN_DELAY_SLOT, CGEN_INSN_RELAXABLE, CGEN_INSN_RELAX, CGEN_INSN_NO_DIS
 , CGEN_INSN_PBB, CGEN_INSN_NOT_IN_DELAY_SLOT
} CGEN_INSN_ATTR;

/* Number of non-boolean elements in cgen_insn.  */
#define CGEN_INSN_NBOOL_ATTRS ((int) CGEN_INSN_NBOOLS)

/* cgen.h uses things we just defined.  */
#include "opcode/cgen.h"

/* Attributes.  */
extern const CGEN_ATTR_TABLE fr30_cgen_hw_attr_table[];
extern const CGEN_ATTR_TABLE fr30_cgen_operand_attr_table[];
extern const CGEN_ATTR_TABLE fr30_cgen_insn_attr_table[];

/* Hardware decls.  */

extern CGEN_KEYWORD fr30_cgen_opval_h_gr;
extern CGEN_KEYWORD fr30_cgen_opval_h_cr;
extern CGEN_KEYWORD fr30_cgen_opval_h_dr;
extern CGEN_KEYWORD fr30_cgen_opval_h_ps;
extern CGEN_KEYWORD fr30_cgen_opval_h_r13;
extern CGEN_KEYWORD fr30_cgen_opval_h_r14;
extern CGEN_KEYWORD fr30_cgen_opval_h_r15;

#define CGEN_INIT_PARSE(od) \
{\
}
#define CGEN_INIT_INSERT(od) \
{\
}
#define CGEN_INIT_EXTRACT(od) \
{\
}
#define CGEN_INIT_PRINT(od) \
{\
}



#endif /* FR30_CPU_H */
