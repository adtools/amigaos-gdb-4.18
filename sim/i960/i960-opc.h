/* Instruction opcode header for i960.

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

#ifndef I960_OPC_H
#define I960_OPC_H

/* -- opc.h */

#undef CGEN_DIS_HASH_SIZE
#define CGEN_DIS_HASH_SIZE 256
#undef CGEN_DIS_HASH
#define CGEN_DIS_HASH(buffer, value) ((unsigned char *) (buffer))[3]

/* ??? Until cgen disassembler complete and functioning well, redirect back
   to old disassembler.  */
#define CGEN_PRINT_INSN(od, pc, info) print_insn_i960_orig (pc, info)

/* -- */
/* Enum declaration for i960 instruction types.  */
typedef enum cgen_insn_type {
  I960_INSN_INVALID, I960_INSN_MULO, I960_INSN_MULO1, I960_INSN_MULO2
 , I960_INSN_MULO3, I960_INSN_REMO, I960_INSN_REMO1, I960_INSN_REMO2
 , I960_INSN_REMO3, I960_INSN_DIVO, I960_INSN_DIVO1, I960_INSN_DIVO2
 , I960_INSN_DIVO3, I960_INSN_REMI, I960_INSN_REMI1, I960_INSN_REMI2
 , I960_INSN_REMI3, I960_INSN_DIVI, I960_INSN_DIVI1, I960_INSN_DIVI2
 , I960_INSN_DIVI3, I960_INSN_ADDO, I960_INSN_ADDO1, I960_INSN_ADDO2
 , I960_INSN_ADDO3, I960_INSN_SUBO, I960_INSN_SUBO1, I960_INSN_SUBO2
 , I960_INSN_SUBO3, I960_INSN_NOTBIT, I960_INSN_NOTBIT1, I960_INSN_NOTBIT2
 , I960_INSN_NOTBIT3, I960_INSN_AND, I960_INSN_AND1, I960_INSN_AND2
 , I960_INSN_AND3, I960_INSN_ANDNOT, I960_INSN_ANDNOT1, I960_INSN_ANDNOT2
 , I960_INSN_ANDNOT3, I960_INSN_SETBIT, I960_INSN_SETBIT1, I960_INSN_SETBIT2
 , I960_INSN_SETBIT3, I960_INSN_NOTAND, I960_INSN_NOTAND1, I960_INSN_NOTAND2
 , I960_INSN_NOTAND3, I960_INSN_XOR, I960_INSN_XOR1, I960_INSN_XOR2
 , I960_INSN_XOR3, I960_INSN_OR, I960_INSN_OR1, I960_INSN_OR2
 , I960_INSN_OR3, I960_INSN_NOR, I960_INSN_NOR1, I960_INSN_NOR2
 , I960_INSN_NOR3, I960_INSN_NOT, I960_INSN_NOT1, I960_INSN_NOT2
 , I960_INSN_NOT3, I960_INSN_CLRBIT, I960_INSN_CLRBIT1, I960_INSN_CLRBIT2
 , I960_INSN_CLRBIT3, I960_INSN_SHLO, I960_INSN_SHLO1, I960_INSN_SHLO2
 , I960_INSN_SHLO3, I960_INSN_SHRO, I960_INSN_SHRO1, I960_INSN_SHRO2
 , I960_INSN_SHRO3, I960_INSN_SHLI, I960_INSN_SHLI1, I960_INSN_SHLI2
 , I960_INSN_SHLI3, I960_INSN_SHRI, I960_INSN_SHRI1, I960_INSN_SHRI2
 , I960_INSN_SHRI3, I960_INSN_EMUL, I960_INSN_EMUL1, I960_INSN_EMUL2
 , I960_INSN_EMUL3, I960_INSN_MOV, I960_INSN_MOV1, I960_INSN_MOVL
 , I960_INSN_MOVL1, I960_INSN_MOVT, I960_INSN_MOVT1, I960_INSN_MOVQ
 , I960_INSN_MOVQ1, I960_INSN_MODPC, I960_INSN_MODAC, I960_INSN_LDA_OFFSET
 , I960_INSN_LDA_INDIRECT_OFFSET, I960_INSN_LDA_INDIRECT, I960_INSN_LDA_INDIRECT_INDEX, I960_INSN_LDA_DISP
 , I960_INSN_LDA_INDIRECT_DISP, I960_INSN_LDA_INDEX_DISP, I960_INSN_LDA_INDIRECT_INDEX_DISP, I960_INSN_LD_OFFSET
 , I960_INSN_LD_INDIRECT_OFFSET, I960_INSN_LD_INDIRECT, I960_INSN_LD_INDIRECT_INDEX, I960_INSN_LD_DISP
 , I960_INSN_LD_INDIRECT_DISP, I960_INSN_LD_INDEX_DISP, I960_INSN_LD_INDIRECT_INDEX_DISP, I960_INSN_LDOB_OFFSET
 , I960_INSN_LDOB_INDIRECT_OFFSET, I960_INSN_LDOB_INDIRECT, I960_INSN_LDOB_INDIRECT_INDEX, I960_INSN_LDOB_DISP
 , I960_INSN_LDOB_INDIRECT_DISP, I960_INSN_LDOB_INDEX_DISP, I960_INSN_LDOB_INDIRECT_INDEX_DISP, I960_INSN_LDOS_OFFSET
 , I960_INSN_LDOS_INDIRECT_OFFSET, I960_INSN_LDOS_INDIRECT, I960_INSN_LDOS_INDIRECT_INDEX, I960_INSN_LDOS_DISP
 , I960_INSN_LDOS_INDIRECT_DISP, I960_INSN_LDOS_INDEX_DISP, I960_INSN_LDOS_INDIRECT_INDEX_DISP, I960_INSN_LDIB_OFFSET
 , I960_INSN_LDIB_INDIRECT_OFFSET, I960_INSN_LDIB_INDIRECT, I960_INSN_LDIB_INDIRECT_INDEX, I960_INSN_LDIB_DISP
 , I960_INSN_LDIB_INDIRECT_DISP, I960_INSN_LDIB_INDEX_DISP, I960_INSN_LDIB_INDIRECT_INDEX_DISP, I960_INSN_LDIS_OFFSET
 , I960_INSN_LDIS_INDIRECT_OFFSET, I960_INSN_LDIS_INDIRECT, I960_INSN_LDIS_INDIRECT_INDEX, I960_INSN_LDIS_DISP
 , I960_INSN_LDIS_INDIRECT_DISP, I960_INSN_LDIS_INDEX_DISP, I960_INSN_LDIS_INDIRECT_INDEX_DISP, I960_INSN_LDL_OFFSET
 , I960_INSN_LDL_INDIRECT_OFFSET, I960_INSN_LDL_INDIRECT, I960_INSN_LDL_INDIRECT_INDEX, I960_INSN_LDL_DISP
 , I960_INSN_LDL_INDIRECT_DISP, I960_INSN_LDL_INDEX_DISP, I960_INSN_LDL_INDIRECT_INDEX_DISP, I960_INSN_LDT_OFFSET
 , I960_INSN_LDT_INDIRECT_OFFSET, I960_INSN_LDT_INDIRECT, I960_INSN_LDT_INDIRECT_INDEX, I960_INSN_LDT_DISP
 , I960_INSN_LDT_INDIRECT_DISP, I960_INSN_LDT_INDEX_DISP, I960_INSN_LDT_INDIRECT_INDEX_DISP, I960_INSN_LDQ_OFFSET
 , I960_INSN_LDQ_INDIRECT_OFFSET, I960_INSN_LDQ_INDIRECT, I960_INSN_LDQ_INDIRECT_INDEX, I960_INSN_LDQ_DISP
 , I960_INSN_LDQ_INDIRECT_DISP, I960_INSN_LDQ_INDEX_DISP, I960_INSN_LDQ_INDIRECT_INDEX_DISP, I960_INSN_ST_OFFSET
 , I960_INSN_ST_INDIRECT_OFFSET, I960_INSN_ST_INDIRECT, I960_INSN_ST_INDIRECT_INDEX, I960_INSN_ST_DISP
 , I960_INSN_ST_INDIRECT_DISP, I960_INSN_ST_INDEX_DISP, I960_INSN_ST_INDIRECT_INDEX_DISP, I960_INSN_STOB_OFFSET
 , I960_INSN_STOB_INDIRECT_OFFSET, I960_INSN_STOB_INDIRECT, I960_INSN_STOB_INDIRECT_INDEX, I960_INSN_STOB_DISP
 , I960_INSN_STOB_INDIRECT_DISP, I960_INSN_STOB_INDEX_DISP, I960_INSN_STOB_INDIRECT_INDEX_DISP, I960_INSN_STOS_OFFSET
 , I960_INSN_STOS_INDIRECT_OFFSET, I960_INSN_STOS_INDIRECT, I960_INSN_STOS_INDIRECT_INDEX, I960_INSN_STOS_DISP
 , I960_INSN_STOS_INDIRECT_DISP, I960_INSN_STOS_INDEX_DISP, I960_INSN_STOS_INDIRECT_INDEX_DISP, I960_INSN_STL_OFFSET
 , I960_INSN_STL_INDIRECT_OFFSET, I960_INSN_STL_INDIRECT, I960_INSN_STL_INDIRECT_INDEX, I960_INSN_STL_DISP
 , I960_INSN_STL_INDIRECT_DISP, I960_INSN_STL_INDEX_DISP, I960_INSN_STL_INDIRECT_INDEX_DISP, I960_INSN_STT_OFFSET
 , I960_INSN_STT_INDIRECT_OFFSET, I960_INSN_STT_INDIRECT, I960_INSN_STT_INDIRECT_INDEX, I960_INSN_STT_DISP
 , I960_INSN_STT_INDIRECT_DISP, I960_INSN_STT_INDEX_DISP, I960_INSN_STT_INDIRECT_INDEX_DISP, I960_INSN_STQ_OFFSET
 , I960_INSN_STQ_INDIRECT_OFFSET, I960_INSN_STQ_INDIRECT, I960_INSN_STQ_INDIRECT_INDEX, I960_INSN_STQ_DISP
 , I960_INSN_STQ_INDIRECT_DISP, I960_INSN_STQ_INDEX_DISP, I960_INSN_STQ_INDIRECT_INDEX_DISP, I960_INSN_CMPOBE_REG
 , I960_INSN_CMPOBE_LIT, I960_INSN_CMPOBNE_REG, I960_INSN_CMPOBNE_LIT, I960_INSN_CMPOBL_REG
 , I960_INSN_CMPOBL_LIT, I960_INSN_CMPOBLE_REG, I960_INSN_CMPOBLE_LIT, I960_INSN_CMPOBG_REG
 , I960_INSN_CMPOBG_LIT, I960_INSN_CMPOBGE_REG, I960_INSN_CMPOBGE_LIT, I960_INSN_CMPIBE_REG
 , I960_INSN_CMPIBE_LIT, I960_INSN_CMPIBNE_REG, I960_INSN_CMPIBNE_LIT, I960_INSN_CMPIBL_REG
 , I960_INSN_CMPIBL_LIT, I960_INSN_CMPIBLE_REG, I960_INSN_CMPIBLE_LIT, I960_INSN_CMPIBG_REG
 , I960_INSN_CMPIBG_LIT, I960_INSN_CMPIBGE_REG, I960_INSN_CMPIBGE_LIT, I960_INSN_BBC_REG
 , I960_INSN_BBC_LIT, I960_INSN_BBS_REG, I960_INSN_BBS_LIT, I960_INSN_CMPI
 , I960_INSN_CMPI1, I960_INSN_CMPI2, I960_INSN_CMPI3, I960_INSN_CMPO
 , I960_INSN_CMPO1, I960_INSN_CMPO2, I960_INSN_CMPO3, I960_INSN_TESTNO_REG
 , I960_INSN_TESTG_REG, I960_INSN_TESTE_REG, I960_INSN_TESTGE_REG, I960_INSN_TESTL_REG
 , I960_INSN_TESTNE_REG, I960_INSN_TESTLE_REG, I960_INSN_TESTO_REG, I960_INSN_BNO
 , I960_INSN_BG, I960_INSN_BE, I960_INSN_BGE, I960_INSN_BL
 , I960_INSN_BNE, I960_INSN_BLE, I960_INSN_BO, I960_INSN_B
 , I960_INSN_BX_INDIRECT_OFFSET, I960_INSN_BX_INDIRECT, I960_INSN_BX_INDIRECT_INDEX, I960_INSN_BX_DISP
 , I960_INSN_BX_INDIRECT_DISP, I960_INSN_CALLX_DISP, I960_INSN_CALLX_INDIRECT, I960_INSN_CALLX_INDIRECT_OFFSET
 , I960_INSN_RET, I960_INSN_CALLS, I960_INSN_FMARK, I960_INSN_FLUSHREG
 , I960_INSN_MAX
} CGEN_INSN_TYPE;

/* Index of `invalid' insn place holder.  */
#define CGEN_INSN_INVALID I960_INSN_INVALID

/* Total number of insns in table.  */
#define MAX_INSNS ((int) I960_INSN_MAX)

/* This struct records data prior to insertion or after extraction.  */
struct cgen_fields
{
  int length;
  long f_nil;
  long f_opcode;
  long f_srcdst;
  long f_src2;
  long f_m3;
  long f_m2;
  long f_m1;
  long f_opcode2;
  long f_zero;
  long f_src1;
  long f_abase;
  long f_modea;
  long f_zeroa;
  long f_offset;
  long f_modeb;
  long f_scale;
  long f_zerob;
  long f_index;
  long f_optdisp;
  long f_br_src1;
  long f_br_src2;
  long f_br_m1;
  long f_br_disp;
  long f_br_zero;
  long f_ctrl_disp;
  long f_ctrl_zero;
};



#endif /* I960_OPC_H */
