/*  armemu.c -- Main instruction emulation:  ARM7 Instruction Emulator.
    Copyright (C) 1994 Advanced RISC Machines Ltd.
    Modifications to add arch. v4 support by <jsmith@cygnus.com>.
 
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
 
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
 
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

#include "armdefs.h"
#include "armemu.h"

static ARMword GetDPRegRHS(ARMul_State *state, ARMword instr) ;
static ARMword GetDPSRegRHS(ARMul_State *state, ARMword instr) ;
static void WriteR15(ARMul_State *state, ARMword src) ;
static void WriteSR15(ARMul_State *state, ARMword src) ;
static ARMword GetLSRegRHS(ARMul_State *state, ARMword instr) ;
static ARMword GetLS7RHS(ARMul_State *state, ARMword instr) ;
static unsigned LoadWord(ARMul_State *state, ARMword instr, ARMword address) ;
static unsigned LoadHalfWord(ARMul_State *state, ARMword instr, ARMword address,int signextend) ;
static unsigned LoadByte(ARMul_State *state, ARMword instr, ARMword address,int signextend) ;
static unsigned StoreWord(ARMul_State *state, ARMword instr, ARMword address) ;
static unsigned StoreHalfWord(ARMul_State *state, ARMword instr, ARMword address) ;
static unsigned StoreByte(ARMul_State *state, ARMword instr, ARMword address) ;
static void LoadMult(ARMul_State *state, ARMword address, ARMword instr, ARMword WBBase) ;
static void StoreMult(ARMul_State *state, ARMword address, ARMword instr, ARMword WBBase) ;
static void LoadSMult(ARMul_State *state, ARMword address, ARMword instr, ARMword WBBase) ;
static void StoreSMult(ARMul_State *state, ARMword address, ARMword instr, ARMword WBBase) ;
static unsigned Multiply64(ARMul_State *state, ARMword instr,int signextend,int scc) ;
static unsigned MultiplyAdd64(ARMul_State *state, ARMword instr,int signextend,int scc) ;

#define LUNSIGNED (0)   /* unsigned operation */
#define LSIGNED   (1)   /* signed operation */
#define LDEFAULT  (0)   /* default : do nothing */
#define LSCC      (1)   /* set condition codes on result */

/***************************************************************************\
*               short-hand macros for LDR/STR                               *
\***************************************************************************/

/* store post decrement writeback */
#define SHDOWNWB()                                      \
  lhs = LHS ;                                           \
  if (StoreHalfWord(state, instr, lhs))                 \
     LSBase = lhs - GetLS7RHS(state, instr) ;

/* store post increment writeback */
#define SHUPWB()                                        \
  lhs = LHS ;                                           \
  if (StoreHalfWord(state, instr, lhs))                 \
     LSBase = lhs + GetLS7RHS(state, instr) ;

/* store pre decrement */
#define SHPREDOWN()                                     \
  (void)StoreHalfWord(state, instr, LHS - GetLS7RHS(state, instr)) ;

/* store pre decrement writeback */
#define SHPREDOWNWB()                                   \
  temp = LHS - GetLS7RHS(state, instr) ;                \
  if (StoreHalfWord(state, instr, temp))                \
     LSBase = temp ;

/* store pre increment */
#define SHPREUP()                                       \
  (void)StoreHalfWord(state, instr, LHS + GetLS7RHS(state, instr)) ;  

/* store pre increment writeback */
#define SHPREUPWB()                                     \
  temp = LHS + GetLS7RHS(state, instr) ;                \
  if (StoreHalfWord(state, instr, temp))                \
     LSBase = temp ;

/* load post decrement writeback */
#define LHPOSTDOWN()                                    \
{                                                       \
  int done = 1 ;                                        \
  lhs = LHS ;                                           \
  switch (BITS(5,6)) {                                  \
    case 1: /* H */                                     \
      if (LoadHalfWord(state,instr,lhs,LUNSIGNED))      \
         LSBase = lhs - GetLS7RHS(state,instr) ;        \
      break ;                                           \
    case 2: /* SB */                                    \
      if (LoadByte(state,instr,lhs,LSIGNED))            \
         LSBase = lhs - GetLS7RHS(state,instr) ;        \
      break ;                                           \
    case 3: /* SH */                                    \
      if (LoadHalfWord(state,instr,lhs,LSIGNED))        \
         LSBase = lhs - GetLS7RHS(state,instr) ;        \
      break ;                                           \
    case 0: /* SWP handled elsewhere */                 \
    default:                                            \
      done = 0 ;                                        \
      break ;                                           \
    }                                                   \
  if (done)                                             \
     break ;                                            \
}

/* load post increment writeback */
#define LHPOSTUP()                                      \
{                                                       \
  int done = 1 ;                                        \
  lhs = LHS ;                                           \
  switch (BITS(5,6)) {                                  \
    case 1: /* H */                                     \
      if (LoadHalfWord(state,instr,lhs,LUNSIGNED))      \
         LSBase = lhs + GetLS7RHS(state,instr) ;        \
      break ;                                           \
    case 2: /* SB */                                    \
      if (LoadByte(state,instr,lhs,LSIGNED))            \
         LSBase = lhs + GetLS7RHS(state,instr) ;        \
      break ;                                           \
    case 3: /* SH */                                    \
      if (LoadHalfWord(state,instr,lhs,LSIGNED))        \
         LSBase = lhs + GetLS7RHS(state,instr) ;        \
      break ;                                           \
    case 0: /* SWP handled elsewhere */                 \
    default:                                            \
      done = 0 ;                                        \
      break ;                                           \
    }                                                   \
  if (done)                                             \
     break ;                                            \
}

/* load pre decrement */
#define LHPREDOWN()                                     \
{                                                       \
  int done = 1 ;                                        \
  temp = LHS - GetLS7RHS(state,instr) ;                 \
  switch (BITS(5,6)) {                                  \
    case 1: /* H */                                     \
      (void)LoadHalfWord(state,instr,temp,LUNSIGNED) ;  \
      break ;                                           \
    case 2: /* SB */                                    \
      (void)LoadByte(state,instr,temp,LSIGNED) ;        \
      break ;                                           \
    case 3: /* SH */                                    \
      (void)LoadHalfWord(state,instr,temp,LSIGNED) ;    \
      break ;                                           \
    case 0: /* SWP handled elsewhere */                 \
    default:                                            \
      done = 0 ;                                        \
      break ;                                           \
    }                                                   \
  if (done)                                             \
     break ;                                            \
}

/* load pre decrement writeback */
#define LHPREDOWNWB()                                   \
{                                                       \
  int done = 1 ;                                        \
  temp = LHS - GetLS7RHS(state, instr) ;                \
  switch (BITS(5,6)) {                                  \
    case 1: /* H */                                     \
      if (LoadHalfWord(state,instr,temp,LUNSIGNED))     \
         LSBase = temp ;                                \
      break ;                                           \
    case 2: /* SB */                                    \
      if (LoadByte(state,instr,temp,LSIGNED))           \
         LSBase = temp ;                                \
      break ;                                           \
    case 3: /* SH */                                    \
      if (LoadHalfWord(state,instr,temp,LSIGNED))       \
         LSBase = temp ;                                \
      break ;                                           \
    case 0: /* SWP handled elsewhere */                 \
    default:                                            \
      done = 0 ;                                        \
      break ;                                           \
    }                                                   \
  if (done)                                             \
     break ;                                            \
}

/* load pre increment */
#define LHPREUP()                                       \
{                                                       \
  int done = 1 ;                                        \
  temp = LHS + GetLS7RHS(state,instr) ;                 \
  switch (BITS(5,6)) {                                  \
    case 1: /* H */                                     \
      (void)LoadHalfWord(state,instr,temp,LUNSIGNED) ;  \
      break ;                                           \
    case 2: /* SB */                                    \
      (void)LoadByte(state,instr,temp,LSIGNED) ;        \
      break ;                                           \
    case 3: /* SH */                                    \
      (void)LoadHalfWord(state,instr,temp,LSIGNED) ;    \
      break ;                                           \
    case 0: /* SWP handled elsewhere */                 \
    default:                                            \
      done = 0 ;                                        \
      break ;                                           \
    }                                                   \
  if (done)                                             \
     break ;                                            \
}

/* load pre increment writeback */
#define LHPREUPWB()                                     \
{                                                       \
  int done = 1 ;                                        \
  temp = LHS + GetLS7RHS(state, instr) ;                \
  switch (BITS(5,6)) {                                  \
    case 1: /* H */                                     \
      if (LoadHalfWord(state,instr,temp,LUNSIGNED))     \
         LSBase = temp ;                                \
      break ;                                           \
    case 2: /* SB */                                    \
      if (LoadByte(state,instr,temp,LSIGNED))           \
         LSBase = temp ;                                \
      break ;                                           \
    case 3: /* SH */                                    \
      if (LoadHalfWord(state,instr,temp,LSIGNED))       \
         LSBase = temp ;                                \
      break ;                                           \
    case 0: /* SWP handled elsewhere */                 \
    default:                                            \
      done = 0 ;                                        \
      break ;                                           \
    }                                                   \
  if (done)                                             \
     break ;                                            \
}

/***************************************************************************\
*                             EMULATION of ARM6                             *
\***************************************************************************/

/* The PC pipeline value depends on whether ARM or Thumb instructions
   are being executed: */
ARMword isize;

#ifdef MODE32
ARMword ARMul_Emulate32(register ARMul_State *state)
{
#else
ARMword ARMul_Emulate26(register ARMul_State *state)
{
#endif
 register ARMword instr, /* the current instruction */
                 dest, /* almost the DestBus */
                 temp, /* ubiquitous third hand */
                 pc ; /* the address of the current instruction */
 ARMword lhs, rhs ; /* almost the ABus and BBus */
 ARMword decoded, loaded ; /* instruction pipeline */

/***************************************************************************\
*                        Execute the next instruction                       *
\***************************************************************************/

 if (state->NextInstr < PRIMEPIPE) {
    decoded = state->decoded ;
    loaded = state->loaded ;
    pc = state->pc ;
    }

 do { /* just keep going */
#ifdef MODET
    if (TFLAG) {
     isize = 2;
    } else
#endif
     isize = 4;
    switch (state->NextInstr) {
       case SEQ :
          state->Reg[15] += isize ; /* Advance the pipeline, and an S cycle */
          pc += isize ;
          instr = decoded ;
          decoded = loaded ;
          loaded = ARMul_LoadInstrS(state,pc+(isize * 2),isize) ;
          break ;

       case NONSEQ :
          state->Reg[15] += isize ; /* Advance the pipeline, and an N cycle */
          pc += isize ;
          instr = decoded ;
          decoded = loaded ;
          loaded = ARMul_LoadInstrN(state,pc+(isize * 2),isize) ;
          NORMALCYCLE ;
          break ;

       case PCINCEDSEQ :
          pc += isize ; /* Program counter advanced, and an S cycle */
          instr = decoded ;
          decoded = loaded ;
          loaded = ARMul_LoadInstrS(state,pc+(isize * 2),isize) ;
          NORMALCYCLE ;
          break ;

       case PCINCEDNONSEQ :
          pc += isize ; /* Program counter advanced, and an N cycle */
          instr = decoded ;
          decoded = loaded ;
          loaded = ARMul_LoadInstrN(state,pc+(isize * 2),isize) ;
          NORMALCYCLE ;
          break ;

       case RESUME : /* The program counter has been changed */
          pc = state->Reg[15] ;
#ifndef MODE32
          pc = pc & R15PCBITS ;
#endif
          state->Reg[15] = pc + (isize * 2) ;
          state->Aborted = 0 ;
          instr = ARMul_ReLoadInstr(state,pc,isize) ;
          decoded = ARMul_ReLoadInstr(state,pc + isize,isize) ;
          loaded = ARMul_ReLoadInstr(state,pc + isize * 2,isize) ;
          NORMALCYCLE ;
          break ;

       default : /* The program counter has been changed */
          pc = state->Reg[15] ;
#ifndef MODE32
          pc = pc & R15PCBITS ;
#endif
          state->Reg[15] = pc + (isize * 2) ;
          state->Aborted = 0 ;
          instr = ARMul_LoadInstrN(state,pc,isize) ;
          decoded = ARMul_LoadInstrS(state,pc + (isize),isize) ;
          loaded = ARMul_LoadInstrS(state,pc + (isize * 2),isize) ;
          NORMALCYCLE ;
          break ;
       }
    if (state->EventSet)
       ARMul_EnvokeEvent(state) ;
    
#if 0
    /* Enable this for a helpful bit of debugging when tracing is needed.  */
    fprintf (stderr, "pc: %x, instr: %x\n", pc & ~1, instr);
    if (instr == 0) abort ();
#endif

    if (state->Exception) { /* Any exceptions */
       if (state->NresetSig == LOW) {
           ARMul_Abort(state,ARMul_ResetV) ;
           break ;
           }
       else if (!state->NfiqSig && !FFLAG) {
           ARMul_Abort(state,ARMul_FIQV) ;
           break ;
           }
       else if (!state->NirqSig && !IFLAG) {
          ARMul_Abort(state,ARMul_IRQV) ;
          break ;
          }
       }

    if (state->CallDebug > 0) {
       instr = ARMul_Debug(state,pc,instr) ;
       if (state->Emulate < ONCE) {
          state->NextInstr = RESUME ;
          break ;
          }
       if (state->Debug) {
          fprintf(stderr,"At %08lx Instr %08lx Mode %02lx\n",pc,instr,state->Mode) ;
          (void)fgetc(stdin) ;
          }
       }
    else
       if (state->Emulate < ONCE) {
          state->NextInstr = RESUME ;
          break ;
          }

    state->NumInstrs++ ;

#ifdef MODET
 /* Provide Thumb instruction decoding. If the processor is in Thumb
    mode, then we can simply decode the Thumb instruction, and map it
    to the corresponding ARM instruction (by directly loading the
    instr variable, and letting the normal ARM simulator
    execute). There are some caveats to ensure that the correct
    pipelined PC value is used when executing Thumb code, and also for
    dealing with the BL instruction. */
    if (TFLAG) { /* check if in Thumb mode */
      ARMword new;
      switch (ARMul_ThumbDecode(state,pc,instr,&new)) {
        case t_undefined:
          ARMul_UndefInstr(state,instr); /* This is a Thumb instruction */
          break;

        case t_branch: /* already processed */
          goto donext;

        case t_decoded: /* ARM instruction available */
          instr = new; /* so continue instruction decoding */
          break;
      }
    }
#endif

/***************************************************************************\
*                       Check the condition codes                           *
\***************************************************************************/
    if ((temp = TOPBITS(28)) == AL)
       goto mainswitch ; /* vile deed in the need for speed */

    switch ((int)TOPBITS(28)) { /* check the condition code */
       case AL : temp=TRUE ;
                 break ;
       case NV : temp=FALSE ;
                 break ;
       case EQ : temp=ZFLAG ;
                 break ;
       case NE : temp=!ZFLAG ;
                 break ;
       case VS : temp=VFLAG ;
                 break ;
       case VC : temp=!VFLAG ;
                 break ;
       case MI : temp=NFLAG ;
                 break ;
       case PL : temp=!NFLAG ;
                 break ;
       case CS : temp=CFLAG ;
                 break ;
       case CC : temp=!CFLAG ;
                 break ;
       case HI : temp=(CFLAG && !ZFLAG) ;
                 break ;
       case LS : temp=(!CFLAG || ZFLAG) ;
                 break ;
       case GE : temp=((!NFLAG && !VFLAG) || (NFLAG && VFLAG)) ;
                 break ;
       case LT : temp=((NFLAG && !VFLAG) || (!NFLAG && VFLAG)) ;
                 break ;
       case GT : temp=((!NFLAG && !VFLAG && !ZFLAG) || (NFLAG && VFLAG && !ZFLAG)) ;
                 break ;
       case LE : temp=((NFLAG && !VFLAG) || (!NFLAG && VFLAG)) || ZFLAG ;
                 break ;
       } /* cc check */

/***************************************************************************\
*               Actual execution of instructions begins here                *
\***************************************************************************/

    if (temp) { /* if the condition codes don't match, stop here */
mainswitch:

       switch ((int)BITS(20,27)) {

/***************************************************************************\
*                 Data Processing Register RHS Instructions                 *
\***************************************************************************/

          case 0x00 : /* AND reg and MUL */
#ifdef MODET
             if (BITS(4,11) == 0xB) {
               /* STRH register offset, no write-back, down, post indexed */
               SHDOWNWB() ;
               break ;
               }
             /* TODO: CHECK: should 0xD and 0xF generate undefined intruction aborts? */
#endif
             if (BITS(4,7) == 9) { /* MUL */
                rhs = state->Reg[MULRHSReg] ;
                if (MULLHSReg == MULDESTReg) {
                   UNDEF_MULDestEQOp1 ;
                   state->Reg[MULDESTReg] = 0 ;
                   }
                else if (MULDESTReg != 15)
                   state->Reg[MULDESTReg] = state->Reg[MULLHSReg] * rhs ;
                else {
                   UNDEF_MULPCDest ;
                   }
                for (dest = 0, temp = 0 ; dest < 32 ; dest++)
                   if (rhs & (1L << dest))
                      temp = dest ; /* mult takes this many/2 I cycles */
                ARMul_Icycles(state,ARMul_MultTable[temp],0L) ;
                }
             else { /* AND reg */
                rhs = DPRegRHS ;
                dest = LHS & rhs ;
                WRITEDEST(dest) ;
                }
             break ;

          case 0x01 : /* ANDS reg and MULS */
#ifdef MODET
             if ((BITS(4,11) & 0xF9) == 0x9) {
               /* LDR register offset, no write-back, down, post indexed */
               LHPOSTDOWN() ;
               /* fall through to rest of decoding */
               }
#endif
             if (BITS(4,7) == 9) { /* MULS */
                rhs = state->Reg[MULRHSReg] ;
                if (MULLHSReg == MULDESTReg) {
                   UNDEF_MULDestEQOp1 ;
                   state->Reg[MULDESTReg] = 0 ;
                   CLEARN ;
                   SETZ ;
                   }
                else if (MULDESTReg != 15) {
                   dest = state->Reg[MULLHSReg] * rhs ;
                   ARMul_NegZero(state,dest) ;
                   state->Reg[MULDESTReg] = dest ;
                   }
                else {
                   UNDEF_MULPCDest ;
                   }
                for (dest = 0, temp = 0 ; dest < 32 ; dest++)
                   if (rhs & (1L << dest))
                      temp = dest ; /* mult takes this many/2 I cycles */
                ARMul_Icycles(state,ARMul_MultTable[temp],0L) ;
                }
             else { /* ANDS reg */
                rhs = DPSRegRHS ;
                dest = LHS & rhs ;
                WRITESDEST(dest) ;
                }
             break ;

          case 0x02 : /* EOR reg and MLA */
#ifdef MODET
             if (BITS(4,11) == 0xB) {
               /* STRH register offset, write-back, down, post indexed */
               SHDOWNWB() ;
               break ;
               }
#endif
             if (BITS(4,7) == 9) { /* MLA */
                rhs = state->Reg[MULRHSReg] ;
                if (MULLHSReg == MULDESTReg) {
                   UNDEF_MULDestEQOp1 ;
                   state->Reg[MULDESTReg] = state->Reg[MULACCReg] ;
                   }
                else if (MULDESTReg != 15)
                   state->Reg[MULDESTReg] = state->Reg[MULLHSReg] * rhs + state->Reg[MULACCReg] ;
                else {
                   UNDEF_MULPCDest ;
                   }
                for (dest = 0, temp = 0 ; dest < 32 ; dest++)
                   if (rhs & (1L << dest))
                      temp = dest ; /* mult takes this many/2 I cycles */
                ARMul_Icycles(state,ARMul_MultTable[temp],0L) ;
                }
             else {
                rhs = DPRegRHS ;
                dest = LHS ^ rhs ;
                WRITEDEST(dest) ;
                }
             break ;

          case 0x03 : /* EORS reg and MLAS */
#ifdef MODET
             if ((BITS(4,11) & 0xF9) == 0x9) {
               /* LDR register offset, write-back, down, post-indexed */
               LHPOSTDOWN() ;
               /* fall through to rest of the decoding */
               }
#endif
             if (BITS(4,7) == 9) { /* MLAS */
                rhs = state->Reg[MULRHSReg] ;
                if (MULLHSReg == MULDESTReg) {
                   UNDEF_MULDestEQOp1 ;
                   dest = state->Reg[MULACCReg] ;
                   ARMul_NegZero(state,dest) ;
                   state->Reg[MULDESTReg] = dest ;
                   }
                else if (MULDESTReg != 15) {
                   dest = state->Reg[MULLHSReg] * rhs + state->Reg[MULACCReg] ;
                   ARMul_NegZero(state,dest) ;
                   state->Reg[MULDESTReg] = dest ;
                   }
                else {
                   UNDEF_MULPCDest ;
                   }
                for (dest = 0, temp = 0 ; dest < 32 ; dest++)
                   if (rhs & (1L << dest))
                      temp = dest ; /* mult takes this many/2 I cycles */
                ARMul_Icycles(state,ARMul_MultTable[temp],0L) ;
                }
             else { /* EORS Reg */
                rhs = DPSRegRHS ;
                dest = LHS ^ rhs ;
                WRITESDEST(dest) ;
                }
             break ;

          case 0x04 : /* SUB reg */
#ifdef MODET
             if (BITS(4,7) == 0xB) {
               /* STRH immediate offset, no write-back, down, post indexed */
               SHDOWNWB() ;
               break ;
               }
#endif
             rhs = DPRegRHS;
             dest = LHS - rhs ;
             WRITEDEST(dest) ;
             break ;

          case 0x05 : /* SUBS reg */
#ifdef MODET
             if ((BITS(4,7) & 0x9) == 0x9) {
               /* LDR immediate offset, no write-back, down, post indexed */
               LHPOSTDOWN() ;
               /* fall through to the rest of the instruction decoding */
               }
#endif
             lhs = LHS ;
             rhs = DPRegRHS ;
             dest = lhs - rhs ;
             if ((lhs >= rhs) || ((rhs | lhs) >> 31)) {
                ARMul_SubCarry(state,lhs,rhs,dest) ;
                ARMul_SubOverflow(state,lhs,rhs,dest) ;
                }
             else {
                CLEARC ;
                CLEARV ;
                }
             WRITESDEST(dest) ;
             break ;

          case 0x06 : /* RSB reg */
#ifdef MODET
             if (BITS(4,7) == 0xB) {
               /* STRH immediate offset, write-back, down, post indexed */
               SHDOWNWB() ;
               break ;
               }
#endif
             rhs = DPRegRHS ;
             dest = rhs - LHS ;
             WRITEDEST(dest) ;
             break ;

          case 0x07 : /* RSBS reg */
#ifdef MODET
             if ((BITS(4,7) & 0x9) == 0x9) {
               /* LDR immediate offset, write-back, down, post indexed */
               LHPOSTDOWN() ;
               /* fall through to remainder of instruction decoding */
               }
#endif
             lhs = LHS ;
             rhs = DPRegRHS ;
             dest = rhs - lhs ;
             if ((rhs >= lhs) || ((rhs | lhs) >> 31)) {
                ARMul_SubCarry(state,rhs,lhs,dest) ;
                ARMul_SubOverflow(state,rhs,lhs,dest) ;
                }
             else {
                CLEARC ;
                CLEARV ;
                }
             WRITESDEST(dest) ;
             break ;

          case 0x08 : /* ADD reg */
#ifdef MODET
             if (BITS(4,11) == 0xB) {
               /* STRH register offset, no write-back, up, post indexed */
               SHUPWB() ;
               break ;
               }
#endif
#ifdef MODET
             if (BITS(4,7) == 0x9) { /* MULL */
               /* 32x32 = 64 */
               ARMul_Icycles(state,Multiply64(state,instr,LUNSIGNED,LDEFAULT),0L) ;
               break ;
               }
#endif
             rhs = DPRegRHS ;
             dest = LHS + rhs ;
             WRITEDEST(dest) ;
             break ;

          case 0x09 : /* ADDS reg */
#ifdef MODET
             if ((BITS(4,11) & 0xF9) == 0x9) {
               /* LDR register offset, no write-back, up, post indexed */
               LHPOSTUP() ;
               /* fall through to remaining instruction decoding */
               }
#endif
#ifdef MODET
             if (BITS(4,7) == 0x9) { /* MULL */
               /* 32x32=64 */
               ARMul_Icycles(state,Multiply64(state,instr,LUNSIGNED,LSCC),0L) ;
               break ;
               }
#endif
             lhs = LHS ;
             rhs = DPRegRHS ;
             dest = lhs + rhs ;
             ASSIGNZ(dest==0) ;
             if ((lhs | rhs) >> 30) { /* possible C,V,N to set */
                ASSIGNN(NEG(dest)) ;
                ARMul_AddCarry(state,lhs,rhs,dest) ;
                ARMul_AddOverflow(state,lhs,rhs,dest) ;
                }
             else {
                CLEARN ;
                CLEARC ;
                CLEARV ;
                }
             WRITESDEST(dest) ;
             break ;

          case 0x0a : /* ADC reg */
#ifdef MODET
             if (BITS(4,11) == 0xB) {
               /* STRH register offset, write-back, up, post-indexed */
               SHUPWB() ;
               break ;
               }
#endif
#ifdef MODET
             if (BITS(4,7) == 0x9) { /* MULL */
               /* 32x32=64 */
               ARMul_Icycles(state,MultiplyAdd64(state,instr,LUNSIGNED,LDEFAULT),0L) ;
               break ;
               }
#endif
             rhs = DPRegRHS ;
             dest = LHS + rhs + CFLAG ;
             WRITEDEST(dest) ;
             break ;

          case 0x0b : /* ADCS reg */
#ifdef MODET
             if ((BITS(4,11) & 0xF9) == 0x9) {
               /* LDR register offset, write-back, up, post indexed */
               LHPOSTUP() ;
               /* fall through to remaining instruction decoding */
               }
#endif
#ifdef MODET
             if (BITS(4,7) == 0x9) { /* MULL */
               /* 32x32=64 */
               ARMul_Icycles(state,MultiplyAdd64(state,instr,LUNSIGNED,LSCC),0L) ;
               break ;
               }
#endif
             lhs = LHS ;
             rhs = DPRegRHS ;
             dest = lhs + rhs + CFLAG ;
             ASSIGNZ(dest==0) ;
             if ((lhs | rhs) >> 30) { /* possible C,V,N to set */
                ASSIGNN(NEG(dest)) ;
                ARMul_AddCarry(state,lhs,rhs,dest) ;
                ARMul_AddOverflow(state,lhs,rhs,dest) ;
                }
             else {
                CLEARN ;
                CLEARC ;
                CLEARV ;
                }
             WRITESDEST(dest) ;
             break ;

          case 0x0c : /* SBC reg */
#ifdef MODET
             if (BITS(4,7) == 0xB) {
               /* STRH immediate offset, no write-back, up post indexed */
               SHUPWB() ;
               break ;
               }
#endif
#ifdef MODET
             if (BITS(4,7) == 0x9) { /* MULL */
               /* 32x32=64 */
               ARMul_Icycles(state,Multiply64(state,instr,LSIGNED,LDEFAULT),0L) ;
               break ;
               }
#endif
             rhs = DPRegRHS ;
             dest = LHS - rhs - !CFLAG ;
             WRITEDEST(dest) ;
             break ;

          case 0x0d : /* SBCS reg */
#ifdef MODET
             if ((BITS(4,7) & 0x9) == 0x9) {
               /* LDR immediate offset, no write-back, up, post indexed */
               LHPOSTUP() ;
               }
#endif
#ifdef MODET
             if (BITS(4,7) == 0x9) { /* MULL */
               /* 32x32=64 */
               ARMul_Icycles(state,Multiply64(state,instr,LSIGNED,LSCC),0L) ;
               break ;
               }
#endif
             lhs = LHS ;
             rhs = DPRegRHS ;
             dest = lhs - rhs - !CFLAG ;
             if ((lhs >= rhs) || ((rhs | lhs) >> 31)) {
                ARMul_SubCarry(state,lhs,rhs,dest) ;
                ARMul_SubOverflow(state,lhs,rhs,dest) ;
                }
             else {
                CLEARC ;
                CLEARV ;
                }
             WRITESDEST(dest) ;
             break ;

          case 0x0e : /* RSC reg */
#ifdef MODET
             if (BITS(4,7) == 0xB) {
               /* STRH immediate offset, write-back, up, post indexed */
               SHUPWB() ;
               break ;
               }
#endif
#ifdef MODET
             if (BITS(4,7) == 0x9) { /* MULL */
               /* 32x32=64 */
               ARMul_Icycles(state,MultiplyAdd64(state,instr,LSIGNED,LDEFAULT),0L) ;
               break ;
               }
#endif
             rhs = DPRegRHS ;
             dest = rhs - LHS - !CFLAG ;
             WRITEDEST(dest) ;
             break ;

          case 0x0f : /* RSCS reg */
#ifdef MODET
             if ((BITS(4,7) & 0x9) == 0x9) {
               /* LDR immediate offset, write-back, up, post indexed */
               LHPOSTUP() ;
               /* fall through to remaining instruction decoding */
               }
#endif
#ifdef MODET
             if (BITS(4,7) == 0x9) { /* MULL */
               /* 32x32=64 */
               ARMul_Icycles(state,MultiplyAdd64(state,instr,LSIGNED,LSCC),0L) ;
               break ;
               }
#endif
             lhs = LHS ;
             rhs = DPRegRHS ;
             dest = rhs - lhs - !CFLAG ;
             if ((rhs >= lhs) || ((rhs | lhs) >> 31)) {
                ARMul_SubCarry(state,rhs,lhs,dest) ;
                ARMul_SubOverflow(state,rhs,lhs,dest) ;
                }
             else {
                CLEARC ;
                CLEARV ;
                }
             WRITESDEST(dest) ;
             break ;

          case 0x10 : /* TST reg and MRS CPSR and SWP word */
#ifdef MODET
             if (BITS(4,11) == 0xB) {
               /* STRH register offset, no write-back, down, pre indexed */
               SHPREDOWN() ;
               break ;
               }
#endif
             if (BITS(4,11) == 9) { /* SWP */
                UNDEF_SWPPC ;
                temp = LHS ;
                BUSUSEDINCPCS ;
#ifndef MODE32
                if (VECTORACCESS(temp) || ADDREXCEPT(temp)) {
                   INTERNALABORT(temp) ;
                   (void)ARMul_LoadWordN(state,temp) ;
                   (void)ARMul_LoadWordN(state,temp) ;
                   }
                else
#endif
                dest = ARMul_SwapWord(state,temp,state->Reg[RHSReg]) ;
                if (temp & 3)
                    DEST = ARMul_Align(state,temp,dest) ;
                else
                    DEST = dest ;
                if (state->abortSig || state->Aborted) {
                   TAKEABORT ;
                   }
                }
             else if ((BITS(0,11)==0) && (LHSReg==15)) { /* MRS CPSR */
                UNDEF_MRSPC ;
                DEST = ECC | EINT | EMODE ;
                }
             else {
                UNDEF_Test ;
                }
             break ;

          case 0x11 : /* TSTP reg */
#ifdef MODET
             if ((BITS(4,11) & 0xF9) == 0x9) {
               /* LDR register offset, no write-back, down, pre indexed */
               LHPREDOWN() ;
               /* continue with remaining instruction decode */
               }
#endif
             if (DESTReg == 15) { /* TSTP reg */
#ifdef MODE32
                state->Cpsr = GETSPSR(state->Bank) ;
                ARMul_CPSRAltered(state) ;
#else
                rhs = DPRegRHS ;
                temp = LHS & rhs ;
                SETR15PSR(temp) ;
#endif
                }
             else { /* TST reg */
                rhs = DPSRegRHS ;
                dest = LHS & rhs ;
                ARMul_NegZero(state,dest) ;
                }
             break ;

          case 0x12 : /* TEQ reg and MSR reg to CPSR (ARM6) */
#ifdef MODET
             if (BITS(4,11) == 0xB) {
               /* STRH register offset, write-back, down, pre indexed */
               SHPREDOWNWB() ;
               break ;
               }
#endif
#ifdef MODET
             if (BITS(4,27)==0x12FFF1) { /* BX */
               /* Branch to the address in RHSReg. If bit0 of
                  destination address is 1 then switch to Thumb mode: */
               ARMword addr = state->Reg[RHSReg];
	       
	       /* If we read the PC then the bottom bit is clear */
	       if (RHSReg == 15) addr &= ~1;
	       
	       /* Enable this for a helpful bit of debugging when
		  GDB is not yet fully working... 
	       fprintf (stderr, "BX at %x to %x (go %s)\n",
			state->Reg[15], addr, (addr & 1) ? "thumb": "arm" ); */

               if (addr & (1 << 0)) { /* Thumb bit */
                 SETT;
                 state->Reg[15] = addr & 0xfffffffe;
                 /* NOTE: The other CPSR flag setting blocks do not
                    seem to update the state->Cpsr state, but just do
                    the explicit flag. The copy from the seperate
                    flags to the register must happen later. */
                 FLUSHPIPE;
                 } else {
                 CLEART;
                 state->Reg[15] = addr & 0xfffffffc;
                 FLUSHPIPE;
                 }
               }
#endif
             if (DESTReg==15 && BITS(17,18)==0) { /* MSR reg to CPSR */
                UNDEF_MSRPC ;
                temp = DPRegRHS ;
                   ARMul_FixCPSR(state,instr,temp) ;
                }
             else {
                UNDEF_Test ;
                }
             break ;

          case 0x13 : /* TEQP reg */
#ifdef MODET
             if ((BITS(4,11) & 0xF9) == 0x9) {
               /* LDR register offset, write-back, down, pre indexed */
               LHPREDOWNWB() ;
               /* continue with remaining instruction decode */
               }
#endif
             if (DESTReg == 15) { /* TEQP reg */
#ifdef MODE32
                state->Cpsr = GETSPSR(state->Bank) ;
                ARMul_CPSRAltered(state) ;
#else
                rhs = DPRegRHS ;
                temp = LHS ^ rhs ;
                SETR15PSR(temp) ;
#endif
                }
             else { /* TEQ Reg */
                rhs = DPSRegRHS ;
                dest = LHS ^ rhs ;
                ARMul_NegZero(state,dest) ;
                }
             break ;

          case 0x14 : /* CMP reg and MRS SPSR and SWP byte */
#ifdef MODET
             if (BITS(4,7) == 0xB) {
               /* STRH immediate offset, no write-back, down, pre indexed */
               SHPREDOWN() ;
               break ;
               }
#endif
             if (BITS(4,11) == 9) { /* SWP */
                UNDEF_SWPPC ;
                temp = LHS ;
                BUSUSEDINCPCS ;
#ifndef MODE32
                if (VECTORACCESS(temp) || ADDREXCEPT(temp)) {
                   INTERNALABORT(temp) ;
                   (void)ARMul_LoadByte(state,temp) ;
                   (void)ARMul_LoadByte(state,temp) ;
                   }
                else
#endif
                DEST = ARMul_SwapByte(state,temp,state->Reg[RHSReg]) ;
                if (state->abortSig || state->Aborted) {
                   TAKEABORT ;
                   }
                }
             else if ((BITS(0,11)==0) && (LHSReg==15)) { /* MRS SPSR */
                UNDEF_MRSPC ;
                DEST = GETSPSR(state->Bank) ;
                }
             else {
                UNDEF_Test ;
                }
             break ;

          case 0x15 : /* CMPP reg */
#ifdef MODET
             if ((BITS(4,7) & 0x9) == 0x9) {
               /* LDR immediate offset, no write-back, down, pre indexed */
               LHPREDOWN() ;
               /* continue with remaining instruction decode */
               }
#endif
             if (DESTReg == 15) { /* CMPP reg */
#ifdef MODE32
                state->Cpsr = GETSPSR(state->Bank) ;
                ARMul_CPSRAltered(state) ;
#else
                rhs = DPRegRHS ;
                temp = LHS - rhs ;
                SETR15PSR(temp) ;
#endif
                }
             else { /* CMP reg */
                lhs = LHS ;
                rhs = DPRegRHS ;
                dest = lhs - rhs ;
                ARMul_NegZero(state,dest) ;
                if ((lhs >= rhs) || ((rhs | lhs) >> 31)) {
                   ARMul_SubCarry(state,lhs,rhs,dest) ;
                   ARMul_SubOverflow(state,lhs,rhs,dest) ;
                   }
                else {
                   CLEARC ;
                   CLEARV ;
                   }
                }
             break ;

          case 0x16 : /* CMN reg and MSR reg to SPSR */
#ifdef MODET
             if (BITS(4,7) == 0xB) {
               /* STRH immediate offset, write-back, down, pre indexed */
               SHPREDOWNWB() ;
               break ;
               }
#endif
             if (DESTReg==15 && BITS(17,18)==0) { /* MSR */
                UNDEF_MSRPC ;
                ARMul_FixSPSR(state,instr,DPRegRHS);
                }
             else {
                UNDEF_Test ;
                }
             break ;

          case 0x17 : /* CMNP reg */
#ifdef MODET
             if ((BITS(4,7) & 0x9) == 0x9) {
               /* LDR immediate offset, write-back, down, pre indexed */
               LHPREDOWNWB() ;
               /* continue with remaining instruction decoding */
               }
#endif
             if (DESTReg == 15) {
#ifdef MODE32
                state->Cpsr = GETSPSR(state->Bank) ;
                ARMul_CPSRAltered(state) ;
#else
                rhs = DPRegRHS ;
                temp = LHS + rhs ;
                SETR15PSR(temp) ;
#endif
                break ;
                }
             else { /* CMN reg */
                lhs = LHS ;
                rhs = DPRegRHS ;
                dest = lhs + rhs ;
                ASSIGNZ(dest==0) ;
                if ((lhs | rhs) >> 30) { /* possible C,V,N to set */
                   ASSIGNN(NEG(dest)) ;
                   ARMul_AddCarry(state,lhs,rhs,dest) ;
                   ARMul_AddOverflow(state,lhs,rhs,dest) ;
                   }
                else {
                   CLEARN ;
                   CLEARC ;
                   CLEARV ;
                   }
                }
             break ;

          case 0x18 : /* ORR reg */
#ifdef MODET
             if (BITS(4,11) == 0xB) {
               /* STRH register offset, no write-back, up, pre indexed */
               SHPREUP() ;
               break ;
               }
#endif
             rhs = DPRegRHS ;
             dest = LHS | rhs ;
             WRITEDEST(dest) ;
             break ;

          case 0x19 : /* ORRS reg */
#ifdef MODET
             if ((BITS(4,11) & 0xF9) == 0x9) {
               /* LDR register offset, no write-back, up, pre indexed */
               LHPREUP() ;
               /* continue with remaining instruction decoding */
               }
#endif
             rhs = DPSRegRHS ;
             dest = LHS | rhs ;
             WRITESDEST(dest) ;
             break ;

          case 0x1a : /* MOV reg */
#ifdef MODET
             if (BITS(4,11) == 0xB) {
               /* STRH register offset, write-back, up, pre indexed */
               SHPREUPWB() ;
               break ;
               }
#endif
             dest = DPRegRHS ;
             WRITEDEST(dest) ;
             break ;

          case 0x1b : /* MOVS reg */
#ifdef MODET
             if ((BITS(4,11) & 0xF9) == 0x9) {
               /* LDR register offset, write-back, up, pre indexed */
               LHPREUPWB() ;
               /* continue with remaining instruction decoding */
               }
#endif
             dest = DPSRegRHS ;
             WRITESDEST(dest) ;
             break ;

          case 0x1c : /* BIC reg */
#ifdef MODET
             if (BITS(4,7) == 0xB) {
               /* STRH immediate offset, no write-back, up, pre indexed */
               SHPREUP() ;
               break ;
               }
#endif
             rhs = DPRegRHS ;
             dest = LHS & ~rhs ;
             WRITEDEST(dest) ;
             break ;

          case 0x1d : /* BICS reg */
#ifdef MODET
             if ((BITS(4,7) & 0x9) == 0x9) {
               /* LDR immediate offset, no write-back, up, pre indexed */
               LHPREUP() ;
               /* continue with instruction decoding */
               }
#endif
             rhs = DPSRegRHS ;
             dest = LHS & ~rhs ;
             WRITESDEST(dest) ;
             break ;

          case 0x1e : /* MVN reg */
#ifdef MODET
             if (BITS(4,7) == 0xB) {
               /* STRH immediate offset, write-back, up, pre indexed */
               SHPREUPWB() ;
               break ;
               }
#endif
             dest = ~DPRegRHS ;
             WRITEDEST(dest) ;
             break ;

          case 0x1f : /* MVNS reg */
#ifdef MODET
             if ((BITS(4,7) & 0x9) == 0x9) {
               /* LDR immediate offset, write-back, up, pre indexed */
               LHPREUPWB() ;
               /* continue instruction decoding */
               }
#endif
             dest = ~DPSRegRHS ;
             WRITESDEST(dest) ;
             break ;

/***************************************************************************\
*                Data Processing Immediate RHS Instructions                 *
\***************************************************************************/

          case 0x20 : /* AND immed */
             dest = LHS & DPImmRHS ;
             WRITEDEST(dest) ;
             break ;

          case 0x21 : /* ANDS immed */
             DPSImmRHS ;
             dest = LHS & rhs ;
             WRITESDEST(dest) ;
             break ;

          case 0x22 : /* EOR immed */
             dest = LHS ^ DPImmRHS ;
             WRITEDEST(dest) ;
             break ;

          case 0x23 : /* EORS immed */
             DPSImmRHS ;
             dest = LHS ^ rhs ;
             WRITESDEST(dest) ;
             break ;

          case 0x24 : /* SUB immed */
             dest = LHS - DPImmRHS ;
             WRITEDEST(dest) ;
             break ;

          case 0x25 : /* SUBS immed */
             lhs = LHS ;
             rhs = DPImmRHS ;
             dest = lhs - rhs ;
             if ((lhs >= rhs) || ((rhs | lhs) >> 31)) {
                ARMul_SubCarry(state,lhs,rhs,dest) ;
                ARMul_SubOverflow(state,lhs,rhs,dest) ;
                }
             else {
                CLEARC ;
                CLEARV ;
                }
             WRITESDEST(dest) ;
             break ;

          case 0x26 : /* RSB immed */
             dest = DPImmRHS - LHS ;
             WRITEDEST(dest) ;
             break ;

          case 0x27 : /* RSBS immed */
             lhs = LHS ;
             rhs = DPImmRHS ;
             dest = rhs - lhs ;
             if ((rhs >= lhs) || ((rhs | lhs) >> 31)) {
                ARMul_SubCarry(state,rhs,lhs,dest) ;
                ARMul_SubOverflow(state,rhs,lhs,dest) ;
                }
             else {
                CLEARC ;
                CLEARV ;
                }
             WRITESDEST(dest) ;
             break ;

          case 0x28 : /* ADD immed */
             dest = LHS + DPImmRHS ;
             WRITEDEST(dest) ;
             break ;

          case 0x29 : /* ADDS immed */
             lhs = LHS ;
             rhs = DPImmRHS ;
             dest = lhs + rhs ;
             ASSIGNZ(dest==0) ;
             if ((lhs | rhs) >> 30) { /* possible C,V,N to set */
                ASSIGNN(NEG(dest)) ;
                ARMul_AddCarry(state,lhs,rhs,dest) ;
                ARMul_AddOverflow(state,lhs,rhs,dest) ;
                }
             else {
                CLEARN ;
                CLEARC ;
                CLEARV ;
                }
             WRITESDEST(dest) ;
             break ;

          case 0x2a : /* ADC immed */
             dest = LHS + DPImmRHS + CFLAG ;
             WRITEDEST(dest) ;
             break ;

          case 0x2b : /* ADCS immed */
             lhs = LHS ;
             rhs = DPImmRHS ;
             dest = lhs + rhs + CFLAG ;
             ASSIGNZ(dest==0) ;
             if ((lhs | rhs) >> 30) { /* possible C,V,N to set */
                ASSIGNN(NEG(dest)) ;
                ARMul_AddCarry(state,lhs,rhs,dest) ;
                ARMul_AddOverflow(state,lhs,rhs,dest) ;
                }
             else {
                CLEARN ;
                CLEARC ;
                CLEARV ;
                }
             WRITESDEST(dest) ;
             break ;

          case 0x2c : /* SBC immed */
             dest = LHS - DPImmRHS - !CFLAG ;
             WRITEDEST(dest) ;
             break ;

          case 0x2d : /* SBCS immed */
             lhs = LHS ;
             rhs = DPImmRHS ;
             dest = lhs - rhs - !CFLAG ;
             if ((lhs >= rhs) || ((rhs | lhs) >> 31)) {
                ARMul_SubCarry(state,lhs,rhs,dest) ;
                ARMul_SubOverflow(state,lhs,rhs,dest) ;
                }
             else {
                CLEARC ;
                CLEARV ;
                }
             WRITESDEST(dest) ;
             break ;

          case 0x2e : /* RSC immed */
             dest = DPImmRHS - LHS - !CFLAG ;
             WRITEDEST(dest) ;
             break ;

          case 0x2f : /* RSCS immed */
             lhs = LHS ;
             rhs = DPImmRHS ;
             dest = rhs - lhs - !CFLAG ;
             if ((rhs >= lhs) || ((rhs | lhs) >> 31)) {
                ARMul_SubCarry(state,rhs,lhs,dest) ;
                ARMul_SubOverflow(state,rhs,lhs,dest) ;
                }
             else {
                CLEARC ;
                CLEARV ;
                }
             WRITESDEST(dest) ;
             break ;

          case 0x30 : /* TST immed */
             UNDEF_Test ;
             break ;

          case 0x31 : /* TSTP immed */
             if (DESTReg == 15) { /* TSTP immed */
#ifdef MODE32
                state->Cpsr = GETSPSR(state->Bank) ;
                ARMul_CPSRAltered(state) ;
#else
                temp = LHS & DPImmRHS ;
                SETR15PSR(temp) ;
#endif
                }
             else {
                DPSImmRHS ; /* TST immed */
                dest = LHS & rhs ;
                ARMul_NegZero(state,dest) ;
                }
             break ;

          case 0x32 : /* TEQ immed and MSR immed to CPSR */
             if (DESTReg==15 && BITS(17,18)==0) { /* MSR immed to CPSR */
                ARMul_FixCPSR(state,instr,DPImmRHS) ;
                }
             else {
                UNDEF_Test ;
                }
             break ;

          case 0x33 : /* TEQP immed */
             if (DESTReg == 15) { /* TEQP immed */
#ifdef MODE32
                state->Cpsr = GETSPSR(state->Bank) ;
                ARMul_CPSRAltered(state) ;
#else
                temp = LHS ^ DPImmRHS ;
                SETR15PSR(temp) ;
#endif
                }
             else {
                DPSImmRHS ; /* TEQ immed */
                dest = LHS ^ rhs ;
                ARMul_NegZero(state,dest) ;
                }
             break ;

          case 0x34 : /* CMP immed */
             UNDEF_Test ;
             break ;

          case 0x35 : /* CMPP immed */
             if (DESTReg == 15) { /* CMPP immed */
#ifdef MODE32
                state->Cpsr = GETSPSR(state->Bank) ;
                ARMul_CPSRAltered(state) ;
#else
                temp = LHS - DPImmRHS ;
                SETR15PSR(temp) ;
#endif
                break ;
                }
             else {
                lhs = LHS ; /* CMP immed */
                rhs = DPImmRHS ;
                dest = lhs - rhs ;
                ARMul_NegZero(state,dest) ;
                if ((lhs >= rhs) || ((rhs | lhs) >> 31)) {
                   ARMul_SubCarry(state,lhs,rhs,dest) ;
                   ARMul_SubOverflow(state,lhs,rhs,dest) ;
                   }
                else {
                   CLEARC ;
                   CLEARV ;
                   }
                }
             break ;

          case 0x36 : /* CMN immed and MSR immed to SPSR */
             if (DESTReg==15 && BITS(17,18)==0) /* MSR */
                ARMul_FixSPSR(state, instr, DPImmRHS) ;
             else {
                UNDEF_Test ;
                }
             break ;

          case 0x37 : /* CMNP immed */
             if (DESTReg == 15) { /* CMNP immed */
#ifdef MODE32
                state->Cpsr = GETSPSR(state->Bank) ;
                ARMul_CPSRAltered(state) ;
#else
                temp = LHS + DPImmRHS ;
                SETR15PSR(temp) ;
#endif
                break ;
                }
             else {
                lhs = LHS ; /* CMN immed */
                rhs = DPImmRHS ;
                dest = lhs + rhs ;
                ASSIGNZ(dest==0) ;
                if ((lhs | rhs) >> 30) { /* possible C,V,N to set */
                   ASSIGNN(NEG(dest)) ;
                   ARMul_AddCarry(state,lhs,rhs,dest) ;
                   ARMul_AddOverflow(state,lhs,rhs,dest) ;
                   }
                else {
                   CLEARN ;
                   CLEARC ;
                   CLEARV ;
                   }
                }
             break ;

          case 0x38 : /* ORR immed */
             dest = LHS | DPImmRHS ;
             WRITEDEST(dest) ;
             break ;

          case 0x39 : /* ORRS immed */
             DPSImmRHS ;
             dest = LHS | rhs ;
             WRITESDEST(dest) ;
             break ;

          case 0x3a : /* MOV immed */
             dest = DPImmRHS ;
             WRITEDEST(dest) ;
             break ;

          case 0x3b : /* MOVS immed */
             DPSImmRHS ;
             WRITESDEST(rhs) ;
             break ;

          case 0x3c : /* BIC immed */
             dest = LHS & ~DPImmRHS ;
             WRITEDEST(dest) ;
             break ;

          case 0x3d : /* BICS immed */
             DPSImmRHS ;
             dest = LHS & ~rhs ;
             WRITESDEST(dest) ;
             break ;

          case 0x3e : /* MVN immed */
             dest = ~DPImmRHS ;
             WRITEDEST(dest) ;
             break ;

          case 0x3f : /* MVNS immed */
             DPSImmRHS ;
             WRITESDEST(~rhs) ;
             break ;

/***************************************************************************\
*              Single Data Transfer Immediate RHS Instructions              *
\***************************************************************************/

          case 0x40 : /* Store Word, No WriteBack, Post Dec, Immed */
             lhs = LHS ;
             if (StoreWord(state,instr,lhs))
                LSBase = lhs - LSImmRHS ;
             break ;

          case 0x41 : /* Load Word, No WriteBack, Post Dec, Immed */
             lhs = LHS ;
             if (LoadWord(state,instr,lhs))
                LSBase = lhs - LSImmRHS ;
             break ;

          case 0x42 : /* Store Word, WriteBack, Post Dec, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             lhs = LHS ;
             temp = lhs - LSImmRHS ;
             state->NtransSig = LOW ;
             if (StoreWord(state,instr,lhs))
                LSBase = temp ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;

          case 0x43 : /* Load Word, WriteBack, Post Dec, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             lhs = LHS ;
             state->NtransSig = LOW ;
             if (LoadWord(state,instr,lhs))
                LSBase = lhs - LSImmRHS ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;

          case 0x44 : /* Store Byte, No WriteBack, Post Dec, Immed */
             lhs = LHS ;
             if (StoreByte(state,instr,lhs))
                LSBase = lhs - LSImmRHS ;
             break ;

          case 0x45 : /* Load Byte, No WriteBack, Post Dec, Immed */
             lhs = LHS ;
             if (LoadByte(state,instr,lhs,LUNSIGNED))
                LSBase = lhs - LSImmRHS ;
             break ;

          case 0x46 : /* Store Byte, WriteBack, Post Dec, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             lhs = LHS ;
             state->NtransSig = LOW ;
             if (StoreByte(state,instr,lhs))
                LSBase = lhs - LSImmRHS ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;

          case 0x47 : /* Load Byte, WriteBack, Post Dec, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             lhs = LHS ;
             state->NtransSig = LOW ;
             if (LoadByte(state,instr,lhs,LUNSIGNED))
                LSBase = lhs - LSImmRHS ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;

          case 0x48 : /* Store Word, No WriteBack, Post Inc, Immed */
             lhs = LHS ;
             if (StoreWord(state,instr,lhs))
                LSBase = lhs + LSImmRHS ;
             break ;

          case 0x49 : /* Load Word, No WriteBack, Post Inc, Immed */
             lhs = LHS ;
             if (LoadWord(state,instr,lhs))
                LSBase = lhs + LSImmRHS ;
             break ;

          case 0x4a : /* Store Word, WriteBack, Post Inc, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             lhs = LHS ;
             state->NtransSig = LOW ;
             if (StoreWord(state,instr,lhs))
                LSBase = lhs + LSImmRHS ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;

          case 0x4b : /* Load Word, WriteBack, Post Inc, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             lhs = LHS ;
             state->NtransSig = LOW ;
             if (LoadWord(state,instr,lhs))
                LSBase = lhs + LSImmRHS ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;

          case 0x4c : /* Store Byte, No WriteBack, Post Inc, Immed */
             lhs = LHS ;
             if (StoreByte(state,instr,lhs))
                LSBase = lhs + LSImmRHS ;
             break ;

          case 0x4d : /* Load Byte, No WriteBack, Post Inc, Immed */
             lhs = LHS ;
             if (LoadByte(state,instr,lhs,LUNSIGNED))
                LSBase = lhs + LSImmRHS ;
             break ;

          case 0x4e : /* Store Byte, WriteBack, Post Inc, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             lhs = LHS ;
             state->NtransSig = LOW ;
             if (StoreByte(state,instr,lhs))
                LSBase = lhs + LSImmRHS ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;

          case 0x4f : /* Load Byte, WriteBack, Post Inc, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             lhs = LHS ;
             state->NtransSig = LOW ;
             if (LoadByte(state,instr,lhs,LUNSIGNED))
                LSBase = lhs + LSImmRHS ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;


          case 0x50 : /* Store Word, No WriteBack, Pre Dec, Immed */
             (void)StoreWord(state,instr,LHS - LSImmRHS) ;
             break ;

          case 0x51 : /* Load Word, No WriteBack, Pre Dec, Immed */
             (void)LoadWord(state,instr,LHS - LSImmRHS) ;
             break ;

          case 0x52 : /* Store Word, WriteBack, Pre Dec, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             temp = LHS - LSImmRHS ;
             if (StoreWord(state,instr,temp))
                LSBase = temp ;
             break ;

          case 0x53 : /* Load Word, WriteBack, Pre Dec, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             temp = LHS - LSImmRHS ;
             if (LoadWord(state,instr,temp))
                LSBase = temp ;
             break ;

          case 0x54 : /* Store Byte, No WriteBack, Pre Dec, Immed */
             (void)StoreByte(state,instr,LHS - LSImmRHS) ;
             break ;

          case 0x55 : /* Load Byte, No WriteBack, Pre Dec, Immed */
             (void)LoadByte(state,instr,LHS - LSImmRHS,LUNSIGNED) ;
             break ;

          case 0x56 : /* Store Byte, WriteBack, Pre Dec, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             temp = LHS - LSImmRHS ;
             if (StoreByte(state,instr,temp))
                LSBase = temp ;
             break ;

          case 0x57 : /* Load Byte, WriteBack, Pre Dec, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             temp = LHS - LSImmRHS ;
             if (LoadByte(state,instr,temp,LUNSIGNED))
                LSBase = temp ;
             break ;

          case 0x58 : /* Store Word, No WriteBack, Pre Inc, Immed */
             (void)StoreWord(state,instr,LHS + LSImmRHS) ;
             break ;

          case 0x59 : /* Load Word, No WriteBack, Pre Inc, Immed */
             (void)LoadWord(state,instr,LHS + LSImmRHS) ;
             break ;

          case 0x5a : /* Store Word, WriteBack, Pre Inc, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             temp = LHS + LSImmRHS ;
             if (StoreWord(state,instr,temp))
                LSBase = temp ;
             break ;

          case 0x5b : /* Load Word, WriteBack, Pre Inc, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             temp = LHS + LSImmRHS ;
             if (LoadWord(state,instr,temp))
                LSBase = temp ;
             break ;

          case 0x5c : /* Store Byte, No WriteBack, Pre Inc, Immed */
             (void)StoreByte(state,instr,LHS + LSImmRHS) ;
             break ;

          case 0x5d : /* Load Byte, No WriteBack, Pre Inc, Immed */
             (void)LoadByte(state,instr,LHS + LSImmRHS,LUNSIGNED) ;
             break ;

          case 0x5e : /* Store Byte, WriteBack, Pre Inc, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             temp = LHS + LSImmRHS ;
             if (StoreByte(state,instr,temp))
                LSBase = temp ;
             break ;

          case 0x5f : /* Load Byte, WriteBack, Pre Inc, Immed */
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             temp = LHS + LSImmRHS ;
             if (LoadByte(state,instr,temp,LUNSIGNED))
                LSBase = temp ;
             break ;

/***************************************************************************\
*              Single Data Transfer Register RHS Instructions               *
\***************************************************************************/

          case 0x60 : /* Store Word, No WriteBack, Post Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             if (StoreWord(state,instr,lhs))
                LSBase = lhs - LSRegRHS ;
             break ;

          case 0x61 : /* Load Word, No WriteBack, Post Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             if (LoadWord(state,instr,lhs))
                LSBase = lhs - LSRegRHS ;
             break ;

          case 0x62 : /* Store Word, WriteBack, Post Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             state->NtransSig = LOW ;
             if (StoreWord(state,instr,lhs))
                LSBase = lhs - LSRegRHS ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;

          case 0x63 : /* Load Word, WriteBack, Post Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             state->NtransSig = LOW ;
             if (LoadWord(state,instr,lhs))
                LSBase = lhs - LSRegRHS ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;

          case 0x64 : /* Store Byte, No WriteBack, Post Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             if (StoreByte(state,instr,lhs))
                LSBase = lhs - LSRegRHS ;
             break ;

          case 0x65 : /* Load Byte, No WriteBack, Post Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             if (LoadByte(state,instr,lhs,LUNSIGNED))
                LSBase = lhs - LSRegRHS ;
             break ;

          case 0x66 : /* Store Byte, WriteBack, Post Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             state->NtransSig = LOW ;
             if (StoreByte(state,instr,lhs))
                LSBase = lhs - LSRegRHS ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;

          case 0x67 : /* Load Byte, WriteBack, Post Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             state->NtransSig = LOW ;
             if (LoadByte(state,instr,lhs,LUNSIGNED))
                LSBase = lhs - LSRegRHS ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;

          case 0x68 : /* Store Word, No WriteBack, Post Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             if (StoreWord(state,instr,lhs))
                LSBase = lhs + LSRegRHS ;
             break ;

          case 0x69 : /* Load Word, No WriteBack, Post Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             if (LoadWord(state,instr,lhs))
                LSBase = lhs + LSRegRHS ;
             break ;

          case 0x6a : /* Store Word, WriteBack, Post Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             state->NtransSig = LOW ;
             if (StoreWord(state,instr,lhs))
                LSBase = lhs + LSRegRHS ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;

          case 0x6b : /* Load Word, WriteBack, Post Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             state->NtransSig = LOW ;
             if (LoadWord(state,instr,lhs))
                LSBase = lhs + LSRegRHS ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;

          case 0x6c : /* Store Byte, No WriteBack, Post Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             if (StoreByte(state,instr,lhs))
                LSBase = lhs + LSRegRHS ;
             break ;

          case 0x6d : /* Load Byte, No WriteBack, Post Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             if (LoadByte(state,instr,lhs,LUNSIGNED))
                LSBase = lhs + LSRegRHS ;
             break ;

          case 0x6e : /* Store Byte, WriteBack, Post Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             state->NtransSig = LOW ;
             if (StoreByte(state,instr,lhs))
                LSBase = lhs + LSRegRHS ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;

          case 0x6f : /* Load Byte, WriteBack, Post Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             lhs = LHS ;
             state->NtransSig = LOW ;
             if (LoadByte(state,instr,lhs,LUNSIGNED))
                LSBase = lhs + LSRegRHS ;
             state->NtransSig = (state->Mode & 3)?HIGH:LOW ;
             break ;


          case 0x70 : /* Store Word, No WriteBack, Pre Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             (void)StoreWord(state,instr,LHS - LSRegRHS) ;
             break ;

          case 0x71 : /* Load Word, No WriteBack, Pre Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             (void)LoadWord(state,instr,LHS - LSRegRHS) ;
             break ;

          case 0x72 : /* Store Word, WriteBack, Pre Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             temp = LHS - LSRegRHS ;
             if (StoreWord(state,instr,temp))
                LSBase = temp ;
             break ;

          case 0x73 : /* Load Word, WriteBack, Pre Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             temp = LHS - LSRegRHS ;
             if (LoadWord(state,instr,temp))
                LSBase = temp ;
             break ;

          case 0x74 : /* Store Byte, No WriteBack, Pre Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             (void)StoreByte(state,instr,LHS - LSRegRHS) ;
             break ;

          case 0x75 : /* Load Byte, No WriteBack, Pre Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             (void)LoadByte(state,instr,LHS - LSRegRHS,LUNSIGNED) ;
             break ;

          case 0x76 : /* Store Byte, WriteBack, Pre Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             temp = LHS - LSRegRHS ;
             if (StoreByte(state,instr,temp))
                LSBase = temp ;
             break ;

          case 0x77 : /* Load Byte, WriteBack, Pre Dec, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             temp = LHS - LSRegRHS ;
             if (LoadByte(state,instr,temp,LUNSIGNED))
                LSBase = temp ;
             break ;

          case 0x78 : /* Store Word, No WriteBack, Pre Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             (void)StoreWord(state,instr,LHS + LSRegRHS) ;
             break ;

          case 0x79 : /* Load Word, No WriteBack, Pre Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             (void)LoadWord(state,instr,LHS + LSRegRHS) ;
             break ;

          case 0x7a : /* Store Word, WriteBack, Pre Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             temp = LHS + LSRegRHS ;
             if (StoreWord(state,instr,temp))
                LSBase = temp ;
             break ;

          case 0x7b : /* Load Word, WriteBack, Pre Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             temp = LHS + LSRegRHS ;
             if (LoadWord(state,instr,temp))
                LSBase = temp ;
             break ;

          case 0x7c : /* Store Byte, No WriteBack, Pre Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             (void)StoreByte(state,instr,LHS + LSRegRHS) ;
             break ;

          case 0x7d : /* Load Byte, No WriteBack, Pre Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             (void)LoadByte(state,instr,LHS + LSRegRHS,LUNSIGNED) ;
             break ;

          case 0x7e : /* Store Byte, WriteBack, Pre Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             temp = LHS + LSRegRHS ;
             if (StoreByte(state,instr,temp))
                LSBase = temp ;
             break ;

          case 0x7f : /* Load Byte, WriteBack, Pre Inc, Reg */
             if (BIT(4)) {
                ARMul_UndefInstr(state,instr) ;
                break ;
                }
             UNDEF_LSRBaseEQOffWb ;
             UNDEF_LSRBaseEQDestWb ;
             UNDEF_LSRPCBaseWb ;
             UNDEF_LSRPCOffWb ;
             temp = LHS + LSRegRHS ;
             if (LoadByte(state,instr,temp,LUNSIGNED))
                LSBase = temp ;
             break ;

/***************************************************************************\
*                   Multiple Data Transfer Instructions                     *
\***************************************************************************/

          case 0x80 : /* Store, No WriteBack, Post Dec */
             STOREMULT(instr,LSBase - LSMNumRegs + 4L,0L) ;
             break ;

          case 0x81 : /* Load, No WriteBack, Post Dec */
             LOADMULT(instr,LSBase - LSMNumRegs + 4L,0L) ;
             break ;

          case 0x82 : /* Store, WriteBack, Post Dec */
             temp = LSBase - LSMNumRegs ;
             STOREMULT(instr,temp + 4L,temp) ;
             break ;

          case 0x83 : /* Load, WriteBack, Post Dec */
             temp = LSBase - LSMNumRegs ;
             LOADMULT(instr,temp + 4L,temp) ;
             break ;

          case 0x84 : /* Store, Flags, No WriteBack, Post Dec */
             STORESMULT(instr,LSBase - LSMNumRegs + 4L,0L) ;
             break ;

          case 0x85 : /* Load, Flags, No WriteBack, Post Dec */
             LOADSMULT(instr,LSBase - LSMNumRegs + 4L,0L) ;
             break ;

          case 0x86 : /* Store, Flags, WriteBack, Post Dec */
             temp = LSBase - LSMNumRegs ;
             STORESMULT(instr,temp + 4L,temp) ;
             break ;

          case 0x87 : /* Load, Flags, WriteBack, Post Dec */
             temp = LSBase - LSMNumRegs ;
             LOADSMULT(instr,temp + 4L,temp) ;
             break ;


          case 0x88 : /* Store, No WriteBack, Post Inc */
             STOREMULT(instr,LSBase,0L) ;
             break ;

          case 0x89 : /* Load, No WriteBack, Post Inc */
             LOADMULT(instr,LSBase,0L) ;
             break ;

          case 0x8a : /* Store, WriteBack, Post Inc */
             temp = LSBase ;
             STOREMULT(instr,temp,temp + LSMNumRegs) ;
             break ;

          case 0x8b : /* Load, WriteBack, Post Inc */
             temp = LSBase ;
             LOADMULT(instr,temp,temp + LSMNumRegs) ;
             break ;

          case 0x8c : /* Store, Flags, No WriteBack, Post Inc */
             STORESMULT(instr,LSBase,0L) ;
             break ;

          case 0x8d : /* Load, Flags, No WriteBack, Post Inc */
             LOADSMULT(instr,LSBase,0L) ;
             break ;

          case 0x8e : /* Store, Flags, WriteBack, Post Inc */
             temp = LSBase ;
             STORESMULT(instr,temp,temp + LSMNumRegs) ;
             break ;

          case 0x8f : /* Load, Flags, WriteBack, Post Inc */
             temp = LSBase ;
             LOADSMULT(instr,temp,temp + LSMNumRegs) ;
             break ;


          case 0x90 : /* Store, No WriteBack, Pre Dec */
             STOREMULT(instr,LSBase - LSMNumRegs,0L) ;
             break ;

          case 0x91 : /* Load, No WriteBack, Pre Dec */
             LOADMULT(instr,LSBase - LSMNumRegs,0L) ;
             break ;

          case 0x92 : /* Store, WriteBack, Pre Dec */
             temp = LSBase - LSMNumRegs ;
             STOREMULT(instr,temp,temp) ;
             break ;

          case 0x93 : /* Load, WriteBack, Pre Dec */
             temp = LSBase - LSMNumRegs ;
             LOADMULT(instr,temp,temp) ;
             break ;

          case 0x94 : /* Store, Flags, No WriteBack, Pre Dec */
             STORESMULT(instr,LSBase - LSMNumRegs,0L) ;
             break ;

          case 0x95 : /* Load, Flags, No WriteBack, Pre Dec */
             LOADSMULT(instr,LSBase - LSMNumRegs,0L) ;
             break ;

          case 0x96 : /* Store, Flags, WriteBack, Pre Dec */
             temp = LSBase - LSMNumRegs ;
             STORESMULT(instr,temp,temp) ;
             break ;

          case 0x97 : /* Load, Flags, WriteBack, Pre Dec */
             temp = LSBase - LSMNumRegs ;
             LOADSMULT(instr,temp,temp) ;
             break ;


          case 0x98 : /* Store, No WriteBack, Pre Inc */
             STOREMULT(instr,LSBase + 4L,0L) ;
             break ;

          case 0x99 : /* Load, No WriteBack, Pre Inc */
             LOADMULT(instr,LSBase + 4L,0L) ;
             break ;

          case 0x9a : /* Store, WriteBack, Pre Inc */
             temp = LSBase ;
             STOREMULT(instr,temp + 4L,temp + LSMNumRegs) ;
             break ;

          case 0x9b : /* Load, WriteBack, Pre Inc */
             temp = LSBase ;
             LOADMULT(instr,temp + 4L,temp + LSMNumRegs) ;
             break ;

          case 0x9c : /* Store, Flags, No WriteBack, Pre Inc */
             STORESMULT(instr,LSBase + 4L,0L) ;
             break ;

          case 0x9d : /* Load, Flags, No WriteBack, Pre Inc */
             LOADSMULT(instr,LSBase + 4L,0L) ;
             break ;

          case 0x9e : /* Store, Flags, WriteBack, Pre Inc */
             temp = LSBase ;
             STORESMULT(instr,temp + 4L,temp + LSMNumRegs) ;
             break ;

          case 0x9f : /* Load, Flags, WriteBack, Pre Inc */
             temp = LSBase ;
             LOADSMULT(instr,temp + 4L,temp + LSMNumRegs) ;
             break ;

/***************************************************************************\
*                            Branch forward                                 *
\***************************************************************************/

          case 0xa0 : case 0xa1 : case 0xa2 : case 0xa3 :
          case 0xa4 : case 0xa5 : case 0xa6 : case 0xa7 :
             state->Reg[15] = pc + 8 + POSBRANCH ;
             FLUSHPIPE ;
             break ;

/***************************************************************************\
*                           Branch backward                                 *
\***************************************************************************/

          case 0xa8 : case 0xa9 : case 0xaa : case 0xab :
          case 0xac : case 0xad : case 0xae : case 0xaf :
             state->Reg[15] = pc + 8 + NEGBRANCH ;
             FLUSHPIPE ;
             break ;

/***************************************************************************\
*                       Branch and Link forward                             *
\***************************************************************************/

          case 0xb0 : case 0xb1 : case 0xb2 : case 0xb3 :
          case 0xb4 : case 0xb5 : case 0xb6 : case 0xb7 :
#ifdef MODE32
             state->Reg[14] = pc + 4 ; /* put PC into Link */
#else
             state->Reg[14] = pc + 4 | ECC | ER15INT | EMODE ; /* put PC into Link */
#endif
             state->Reg[15] = pc + 8 + POSBRANCH ;
             FLUSHPIPE ;
             break ;

/***************************************************************************\
*                       Branch and Link backward                            *
\***************************************************************************/

          case 0xb8 : case 0xb9 : case 0xba : case 0xbb :
          case 0xbc : case 0xbd : case 0xbe : case 0xbf :
#ifdef MODE32
             state->Reg[14] = pc + 4 ; /* put PC into Link */
#else
             state->Reg[14] = (pc + 4) | ECC | ER15INT | EMODE ; /* put PC into Link */
#endif
             state->Reg[15] = pc + 8 + NEGBRANCH ;
             FLUSHPIPE ;
             break ;

/***************************************************************************\
*                        Co-Processor Data Transfers                        *
\***************************************************************************/

          case 0xc0 :
          case 0xc4 : /* Store , No WriteBack , Post Dec */
             ARMul_STC(state,instr,LHS) ;
             break ;

          case 0xc1 :
          case 0xc5 : /* Load , No WriteBack , Post Dec */
             ARMul_LDC(state,instr,LHS) ;
             break ;

          case 0xc2 :
          case 0xc6 : /* Store , WriteBack , Post Dec */
             lhs = LHS ;
             state->Base = lhs - LSCOff ;
             ARMul_STC(state,instr,lhs) ;
             break ;

          case 0xc3 :
          case 0xc7 : /* Load , WriteBack , Post Dec */
             lhs = LHS ;
             state->Base = lhs - LSCOff ;
             ARMul_LDC(state,instr,lhs) ;
             break ;

          case 0xc8 :
          case 0xcc : /* Store , No WriteBack , Post Inc */
             ARMul_STC(state,instr,LHS) ;
             break ;

          case 0xc9 :
          case 0xcd : /* Load , No WriteBack , Post Inc */
             ARMul_LDC(state,instr,LHS) ;
             break ;

          case 0xca :
          case 0xce : /* Store , WriteBack , Post Inc */
             lhs = LHS ;
             state->Base = lhs + LSCOff ;
             ARMul_STC(state,instr,LHS) ;
             break ;

          case 0xcb :
          case 0xcf : /* Load , WriteBack , Post Inc */
             lhs = LHS ;
             state->Base = lhs + LSCOff ;
             ARMul_LDC(state,instr,LHS) ;
             break ;


          case 0xd0 :
          case 0xd4 : /* Store , No WriteBack , Pre Dec */
             ARMul_STC(state,instr,LHS - LSCOff) ;
             break ;

          case 0xd1 :
          case 0xd5 : /* Load , No WriteBack , Pre Dec */
             ARMul_LDC(state,instr,LHS - LSCOff) ;
             break ;

          case 0xd2 :
          case 0xd6 : /* Store , WriteBack , Pre Dec */
             lhs = LHS - LSCOff ;
             state->Base = lhs ;
             ARMul_STC(state,instr,lhs) ;
             break ;

          case 0xd3 :
          case 0xd7 : /* Load , WriteBack , Pre Dec */
             lhs = LHS - LSCOff ;
             state->Base = lhs ;
             ARMul_LDC(state,instr,lhs) ;
             break ;

          case 0xd8 :
          case 0xdc : /* Store , No WriteBack , Pre Inc */
             ARMul_STC(state,instr,LHS + LSCOff) ;
             break ;

          case 0xd9 :
          case 0xdd : /* Load , No WriteBack , Pre Inc */
             ARMul_LDC(state,instr,LHS + LSCOff) ;
             break ;

          case 0xda :
          case 0xde : /* Store , WriteBack , Pre Inc */
             lhs = LHS + LSCOff ;
             state->Base = lhs ;
             ARMul_STC(state,instr,lhs) ;
             break ;

          case 0xdb :
          case 0xdf : /* Load , WriteBack , Pre Inc */
             lhs = LHS + LSCOff ;
             state->Base = lhs ;
             ARMul_LDC(state,instr,lhs) ;
             break ;

/***************************************************************************\
*            Co-Processor Register Transfers (MCR) and Data Ops             *
\***************************************************************************/

          case 0xe0 : case 0xe2 : case 0xe4 : case 0xe6 :
          case 0xe8 : case 0xea : case 0xec : case 0xee :
             if (BIT(4)) { /* MCR */
                if (DESTReg == 15) {
                   UNDEF_MCRPC ;
#ifdef MODE32
                   ARMul_MCR(state,instr,state->Reg[15] + isize) ;
#else
                   ARMul_MCR(state,instr,ECC | ER15INT | EMODE |
                                          ((state->Reg[15] + isize) & R15PCBITS) ) ;
#endif
                   }
                else
                   ARMul_MCR(state,instr,DEST) ;
                }
             else /* CDP Part 1 */
                ARMul_CDP(state,instr) ;
             break ;

/***************************************************************************\
*            Co-Processor Register Transfers (MRC) and Data Ops             *
\***************************************************************************/

          case 0xe1 : case 0xe3 : case 0xe5 : case 0xe7 :
          case 0xe9 : case 0xeb : case 0xed : case 0xef :
             if (BIT(4)) { /* MRC */
                temp = ARMul_MRC(state,instr) ;
                if (DESTReg == 15) {
                   ASSIGNN((temp & NBIT) != 0) ;
                   ASSIGNZ((temp & ZBIT) != 0) ;
                   ASSIGNC((temp & CBIT) != 0) ;
                   ASSIGNV((temp & VBIT) != 0) ;
                   }
                else
                   DEST = temp ;
                }
             else /* CDP Part 2 */
                ARMul_CDP(state,instr) ;
             break ;

/***************************************************************************\
*                             SWI instruction                               *
\***************************************************************************/

          case 0xf0 : case 0xf1 : case 0xf2 : case 0xf3 :
          case 0xf4 : case 0xf5 : case 0xf6 : case 0xf7 :
          case 0xf8 : case 0xf9 : case 0xfa : case 0xfb :
          case 0xfc : case 0xfd : case 0xfe : case 0xff :
             if (instr == ARMul_ABORTWORD && state->AbortAddr == pc) { /* a prefetch abort */
                ARMul_Abort(state,ARMul_PrefetchAbortV) ;
                break ;
                }
    
             if (!ARMul_OSHandleSWI(state,BITS(0,23))) {
                ARMul_Abort(state,ARMul_SWIV) ;
                }
             break ;
          } /* 256 way main switch */
       } /* if temp */

#ifdef MODET
donext:
#endif

    if (state->Emulate == ONCE)
        state->Emulate = STOP;
    else if (state->Emulate != RUN)
        break;
    } while (1) ; /* do loop */

 state->decoded = decoded ;
 state->loaded = loaded ;
 state->pc = pc ;
 return(pc) ;
 } /* Emulate 26/32 in instruction based mode */


/***************************************************************************\
* This routine evaluates most Data Processing register RHS's with the S     *
* bit clear.  It is intended to be called from the macro DPRegRHS, which    *
* filters the common case of an unshifted register with in line code        *
\***************************************************************************/

static ARMword GetDPRegRHS(ARMul_State *state, ARMword instr)
{ARMword shamt , base ;

 base = RHSReg ;
 if (BIT(4)) { /* shift amount in a register */
    UNDEF_Shift ;
    INCPC ;
#ifndef MODE32
    if (base == 15)
       base = ECC | ER15INT | R15PC | EMODE ;
    else
#endif
       base = state->Reg[base] ;
    ARMul_Icycles(state,1,0L) ;
    shamt = state->Reg[BITS(8,11)] & 0xff ;
    switch ((int)BITS(5,6)) {
       case LSL : if (shamt == 0)
                     return(base) ;
                  else if (shamt >= 32)
                     return(0) ;
                  else
                     return(base << shamt) ;
       case LSR : if (shamt == 0)
                     return(base) ;
                  else if (shamt >= 32)
                     return(0) ;
                  else
                     return(base >> shamt) ;
       case ASR : if (shamt == 0)
                     return(base) ;
                  else if (shamt >= 32)
                     return((ARMword)((long int)base >> 31L)) ;
                  else
                     return((ARMword)((long int)base >> (int)shamt)) ;
       case ROR : shamt &= 0x1f ;
                  if (shamt == 0)
                     return(base) ;
                  else
                     return((base << (32 - shamt)) | (base >> shamt)) ;
       }
    }
 else { /* shift amount is a constant */
#ifndef MODE32
    if (base == 15)
       base = ECC | ER15INT | R15PC | EMODE ;
    else
#endif
       base = state->Reg[base] ;
    shamt = BITS(7,11) ;
    switch ((int)BITS(5,6)) {
       case LSL : return(base<<shamt) ;
       case LSR : if (shamt == 0)
                     return(0) ;
                  else
                     return(base >> shamt) ;
       case ASR : if (shamt == 0)
                     return((ARMword)((long int)base >> 31L)) ;
                  else
                     return((ARMword)((long int)base >> (int)shamt)) ;
       case ROR : if (shamt==0) /* its an RRX */
                     return((base >> 1) | (CFLAG << 31)) ;
                  else
                     return((base << (32 - shamt)) | (base >> shamt)) ;
       }
    }
 return(0) ; /* just to shut up lint */
 }
/***************************************************************************\
* This routine evaluates most Logical Data Processing register RHS's        *
* with the S bit set.  It is intended to be called from the macro           *
* DPSRegRHS, which filters the common case of an unshifted register         *
* with in line code                                                         *
\***************************************************************************/

static ARMword GetDPSRegRHS(ARMul_State *state, ARMword instr)
{ARMword shamt , base ;

 base = RHSReg ;
 if (BIT(4)) { /* shift amount in a register */
    UNDEF_Shift ;
    INCPC ;
#ifndef MODE32
    if (base == 15)
       base = ECC | ER15INT | R15PC | EMODE ;
    else
#endif
       base = state->Reg[base] ;
    ARMul_Icycles(state,1,0L) ;
    shamt = state->Reg[BITS(8,11)] & 0xff ;
    switch ((int)BITS(5,6)) {
       case LSL : if (shamt == 0)
                     return(base) ;
                  else if (shamt == 32) {
                     ASSIGNC(base & 1) ;
                     return(0) ;
                     }
                  else if (shamt > 32) {
                     CLEARC ;
                     return(0) ;
                     }
                  else {
                     ASSIGNC((base >> (32-shamt)) & 1) ;
                     return(base << shamt) ;
                     }
       case LSR : if (shamt == 0)
                     return(base) ;
                  else if (shamt == 32) {
                     ASSIGNC(base >> 31) ;
                     return(0) ;
                     }
                  else if (shamt > 32) {
                     CLEARC ;
                     return(0) ;
                     }
                  else {
                     ASSIGNC((base >> (shamt - 1)) & 1) ;
                     return(base >> shamt) ;
                     }
       case ASR : if (shamt == 0)
                     return(base) ;
                  else if (shamt >= 32) {
                     ASSIGNC(base >> 31L) ;
                     return((ARMword)((long int)base >> 31L)) ;
                     }
                  else {
                     ASSIGNC((ARMword)((long int)base >> (int)(shamt-1)) & 1) ;
                     return((ARMword)((long int)base >> (int)shamt)) ;
                     }
       case ROR : if (shamt == 0)
                     return(base) ;
                  shamt &= 0x1f ;
                  if (shamt == 0) {
                     ASSIGNC(base >> 31) ;
                     return(base) ;
                     }
                  else {
                     ASSIGNC((base >> (shamt-1)) & 1) ;
                     return((base << (32-shamt)) | (base >> shamt)) ;
                     }
       }
    }
 else { /* shift amount is a constant */
#ifndef MODE32
    if (base == 15)
       base = ECC | ER15INT | R15PC | EMODE ;
    else
#endif
       base = state->Reg[base] ;
    shamt = BITS(7,11) ;
    switch ((int)BITS(5,6)) {
       case LSL : ASSIGNC((base >> (32-shamt)) & 1) ;
                  return(base << shamt) ;
       case LSR : if (shamt == 0) {
                     ASSIGNC(base >> 31) ;
                     return(0) ;
                     }
                  else {
                     ASSIGNC((base >> (shamt - 1)) & 1) ;
                     return(base >> shamt) ;
                     }
       case ASR : if (shamt == 0) {
                     ASSIGNC(base >> 31L) ;
                     return((ARMword)((long int)base >> 31L)) ;
                     }
                  else {
                     ASSIGNC((ARMword)((long int)base >> (int)(shamt-1)) & 1) ;
                     return((ARMword)((long int)base >> (int)shamt)) ;
                     }
       case ROR : if (shamt == 0) { /* its an RRX */
                     shamt = CFLAG ;
                     ASSIGNC(base & 1) ;
                     return((base >> 1) | (shamt << 31)) ;
                     }
                  else {
                     ASSIGNC((base >> (shamt - 1)) & 1) ;
                     return((base << (32-shamt)) | (base >> shamt)) ;
                     }
       }
    }
 return(0) ; /* just to shut up lint */
 }

/***************************************************************************\
* This routine handles writes to register 15 when the S bit is not set.     *
\***************************************************************************/

static void WriteR15(ARMul_State *state, ARMword src)
{
  /* The ARM documentation implies (but doe snot state) that the bottom bit of the PC is never set */
#ifdef MODE32
 state->Reg[15] = src & PCBITS & ~ 0x1 ;
#else
 state->Reg[15] = (src & R15PCBITS & ~ 0x1) | ECC | ER15INT | EMODE ;
 ARMul_R15Altered(state) ;
#endif
 FLUSHPIPE ;
 }

/***************************************************************************\
* This routine handles writes to register 15 when the S bit is set.         *
\***************************************************************************/

static void WriteSR15(ARMul_State *state, ARMword src)
{
#ifdef MODE32
 state->Reg[15] = src & PCBITS ;
 if (state->Bank > 0) {
    state->Cpsr = state->Spsr[state->Bank] ;
    ARMul_CPSRAltered(state) ;
    }
#else
 if (state->Bank == USERBANK)
    state->Reg[15] = (src & (CCBITS | R15PCBITS)) | ER15INT | EMODE ;
 else
    state->Reg[15] = src ;
 ARMul_R15Altered(state) ;
#endif
 FLUSHPIPE ;
 }

/***************************************************************************\
* This routine evaluates most Load and Store register RHS's.  It is         *
* intended to be called from the macro LSRegRHS, which filters the          *
* common case of an unshifted register with in line code                    *
\***************************************************************************/

static ARMword GetLSRegRHS(ARMul_State *state, ARMword instr)
{ARMword shamt, base ;

 base = RHSReg ;
#ifndef MODE32
 if (base == 15)
    base = ECC | ER15INT | R15PC | EMODE ; /* Now forbidden, but .... */
 else
#endif
    base = state->Reg[base] ;

 shamt = BITS(7,11) ;
 switch ((int)BITS(5,6)) {
    case LSL : return(base << shamt) ;
    case LSR : if (shamt == 0)
                  return(0) ;
               else
                  return(base >> shamt) ;
    case ASR : if (shamt == 0)
                  return((ARMword)((long int)base >> 31L)) ;
               else
                  return((ARMword)((long int)base >> (int)shamt)) ;
    case ROR : if (shamt==0) /* its an RRX */
                  return((base >> 1) | (CFLAG << 31)) ;
               else
                  return((base << (32-shamt)) | (base >> shamt)) ;
    }
 return(0) ; /* just to shut up lint */
 }

/***************************************************************************\
* This routine evaluates the ARM7T halfword and signed transfer RHS's.      *
\***************************************************************************/

static ARMword GetLS7RHS(ARMul_State *state, ARMword instr)
{
 if (BIT(22) == 0) { /* register */
#ifndef MODE32
    if (RHSReg == 15)
      return ECC | ER15INT | R15PC | EMODE ; /* Now forbidden, but ... */
#endif
    return state->Reg[RHSReg] ;
    }

 /* else immediate */
 return BITS(0,3) | (BITS(8,11) << 4) ;
 }

/***************************************************************************\
* This function does the work of loading a word for a LDR instruction.      *
\***************************************************************************/

static unsigned LoadWord(ARMul_State *state, ARMword instr, ARMword address)
{
 ARMword dest ;

 BUSUSEDINCPCS ;
#ifndef MODE32
 if (ADDREXCEPT(address)) {
    INTERNALABORT(address) ;
    }
#endif
 dest = ARMul_LoadWordN(state,address) ;
 if (state->Aborted) {
    TAKEABORT ;
    return(state->lateabtSig) ;
    }
 if (address & 3)
    dest = ARMul_Align(state,address,dest) ;
 WRITEDEST(dest) ;
 ARMul_Icycles(state,1,0L) ;

 return(DESTReg != LHSReg) ;
}

#ifdef MODET
/***************************************************************************\
* This function does the work of loading a halfword.                        *
\***************************************************************************/

static unsigned LoadHalfWord(ARMul_State *state, ARMword instr, ARMword address,int signextend)
{
 ARMword dest ;

 BUSUSEDINCPCS ;
#ifndef MODE32
 if (ADDREXCEPT(address)) {
    INTERNALABORT(address) ;
    }
#endif
 dest = ARMul_LoadHalfWord(state,address) ;
 if (state->Aborted) {
    TAKEABORT ;
    return(state->lateabtSig) ;
    }
 UNDEF_LSRBPC ;
 if (signextend)
   {
     if (dest & 1 << (16 - 1))
         dest = (dest & ((1 << 16) - 1)) - (1 << 16) ;
   }
 WRITEDEST(dest) ;
 ARMul_Icycles(state,1,0L) ;
 return(DESTReg != LHSReg) ;
}
#endif /* MODET */

/***************************************************************************\
* This function does the work of loading a byte for a LDRB instruction.     *
\***************************************************************************/

static unsigned LoadByte(ARMul_State *state, ARMword instr, ARMword address,int signextend)
{
 ARMword dest ;

 BUSUSEDINCPCS ;
#ifndef MODE32
 if (ADDREXCEPT(address)) {
    INTERNALABORT(address) ;
    }
#endif
 dest = ARMul_LoadByte(state,address) ;
 if (state->Aborted) {
    TAKEABORT ;
    return(state->lateabtSig) ;
    }
 UNDEF_LSRBPC ;
 if (signextend)
   {
     if (dest & 1 << (8 - 1))
         dest = (dest & ((1 << 8) - 1)) - (1 << 8) ;
   }
 WRITEDEST(dest) ;
 ARMul_Icycles(state,1,0L) ;
 return(DESTReg != LHSReg) ;
}

/***************************************************************************\
* This function does the work of storing a word from a STR instruction.     *
\***************************************************************************/

static unsigned StoreWord(ARMul_State *state, ARMword instr, ARMword address)
{BUSUSEDINCPCN ;
#ifndef MODE32
 if (DESTReg == 15)
    state->Reg[15] = ECC | ER15INT | R15PC | EMODE ;
#endif
#ifdef MODE32
 ARMul_StoreWordN(state,address,DEST) ;
#else
 if (VECTORACCESS(address) || ADDREXCEPT(address)) {
    INTERNALABORT(address) ;
    (void)ARMul_LoadWordN(state,address) ;
    }
 else
    ARMul_StoreWordN(state,address,DEST) ;
#endif
 if (state->Aborted) {
    TAKEABORT ;
    return(state->lateabtSig) ;
    }
 return(TRUE) ;
}

#ifdef MODET
/***************************************************************************\
* This function does the work of storing a byte for a STRH instruction.     *
\***************************************************************************/

static unsigned StoreHalfWord(ARMul_State *state, ARMword instr, ARMword address)
{BUSUSEDINCPCN ;

#ifndef MODE32
 if (DESTReg == 15)
    state->Reg[15] = ECC | ER15INT | R15PC | EMODE ;
#endif

#ifdef MODE32
 ARMul_StoreHalfWord(state,address,DEST);
#else
 if (VECTORACCESS(address) || ADDREXCEPT(address)) {
    INTERNALABORT(address) ;
    (void)ARMul_LoadHalfWord(state,address) ;
    }
 else
    ARMul_StoreHalfWord(state,address,DEST) ;
#endif

 if (state->Aborted) {
    TAKEABORT ;
    return(state->lateabtSig) ;
    }

 return(TRUE) ;
}
#endif /* MODET */

/***************************************************************************\
* This function does the work of storing a byte for a STRB instruction.     *
\***************************************************************************/

static unsigned StoreByte(ARMul_State *state, ARMword instr, ARMword address)
{BUSUSEDINCPCN ;
#ifndef MODE32
 if (DESTReg == 15)
    state->Reg[15] = ECC | ER15INT | R15PC | EMODE ;
#endif
#ifdef MODE32
 ARMul_StoreByte(state,address,DEST) ;
#else
 if (VECTORACCESS(address) || ADDREXCEPT(address)) {
    INTERNALABORT(address) ;
    (void)ARMul_LoadByte(state,address) ;
    }
 else
    ARMul_StoreByte(state,address,DEST) ;
#endif
 if (state->Aborted) {
    TAKEABORT ;
    return(state->lateabtSig) ;
    }
 UNDEF_LSRBPC ;
 return(TRUE) ;
}

/***************************************************************************\
* This function does the work of loading the registers listed in an LDM     *
* instruction, when the S bit is clear.  The code here is always increment  *
* after, it's up to the caller to get the input address correct and to      *
* handle base register modification.                                        *
\***************************************************************************/

static void LoadMult(ARMul_State *state, ARMword instr,
                     ARMword address, ARMword WBBase)
{ARMword dest, temp ;

 UNDEF_LSMNoRegs ;
 UNDEF_LSMPCBase ;
 UNDEF_LSMBaseInListWb ;
 BUSUSEDINCPCS ;
#ifndef MODE32
 if (ADDREXCEPT(address)) {
    INTERNALABORT(address) ;
    }
#endif
 if (BIT(21) && LHSReg != 15)
    LSBase = WBBase ;

    for (temp = 0 ; !BIT(temp) ; temp++) ; /* N cycle first */
    dest = ARMul_LoadWordN(state,address) ;
    if (!state->abortSig && !state->Aborted)
       state->Reg[temp++] = dest ;
    else
       if (!state->Aborted)
          state->Aborted = ARMul_DataAbortV ;

    for (; temp < 16 ; temp++) /* S cycles from here on */
       if (BIT(temp)) { /* load this register */
          address += 4 ;
          dest = ARMul_LoadWordS(state,address) ;
          if (!state->abortSig && !state->Aborted)
             state->Reg[temp] = dest ;
          else
             if (!state->Aborted)
                state->Aborted = ARMul_DataAbortV ;
          }

 if (BIT(15)) { /* PC is in the reg list */
#ifdef MODE32
    state->Reg[15] = PC ;
#endif
    FLUSHPIPE ;
    }

 ARMul_Icycles(state,1,0L) ; /* to write back the final register */

 if (state->Aborted) {
    if (BIT(21) && LHSReg != 15)
       LSBase = WBBase ;
    TAKEABORT ;
    }
 }

/***************************************************************************\
* This function does the work of loading the registers listed in an LDM     *
* instruction, when the S bit is set. The code here is always increment     *
* after, it's up to the caller to get the input address correct and to      *
* handle base register modification.                                        *
\***************************************************************************/

static void LoadSMult(ARMul_State *state, ARMword instr,
                      ARMword address, ARMword WBBase)
{ARMword dest, temp ;

 UNDEF_LSMNoRegs ;
 UNDEF_LSMPCBase ;
 UNDEF_LSMBaseInListWb ;
 BUSUSEDINCPCS ;
#ifndef MODE32
 if (ADDREXCEPT(address)) {
    INTERNALABORT(address) ;
    }
#endif

 if (!BIT(15) && state->Bank != USERBANK) {
    (void)ARMul_SwitchMode(state,state->Mode,USER26MODE) ; /* temporary reg bank switch */
    UNDEF_LSMUserBankWb ;
    }

 if (BIT(21) && LHSReg != 15)
    LSBase = WBBase ;

    for (temp = 0 ; !BIT(temp) ; temp++) ; /* N cycle first */
    dest = ARMul_LoadWordN(state,address) ;
    if (!state->abortSig)
       state->Reg[temp++] = dest ;
    else
       if (!state->Aborted)
          state->Aborted = ARMul_DataAbortV ;

    for (; temp < 16 ; temp++) /* S cycles from here on */
       if (BIT(temp)) { /* load this register */
          address += 4 ;
          dest = ARMul_LoadWordS(state,address) ;
          if (!state->abortSig || state->Aborted)
             state->Reg[temp] = dest ;
          else
             if (!state->Aborted)
                state->Aborted = ARMul_DataAbortV ;
          }

 if (BIT(15)) { /* PC is in the reg list */
#ifdef MODE32
    if (state->Mode != USER26MODE && state->Mode != USER32MODE) {
       state->Cpsr = GETSPSR(state->Bank) ;
       ARMul_CPSRAltered(state) ;
       }
    state->Reg[15] = PC ;
#else
    if (state->Mode == USER26MODE || state->Mode == USER32MODE) { /* protect bits in user mode */
       ASSIGNN((state->Reg[15] & NBIT) != 0) ;
       ASSIGNZ((state->Reg[15] & ZBIT) != 0) ;
       ASSIGNC((state->Reg[15] & CBIT) != 0) ;
       ASSIGNV((state->Reg[15] & VBIT) != 0) ;
       }
    else
       ARMul_R15Altered(state) ;
#endif
    FLUSHPIPE ;
    }

 if (!BIT(15) && state->Mode != USER26MODE && state->Mode != USER32MODE)
    (void)ARMul_SwitchMode(state,USER26MODE,state->Mode) ; /* restore the correct bank */

 ARMul_Icycles(state,1,0L) ; /* to write back the final register */

 if (state->Aborted) {
    if (BIT(21) && LHSReg != 15)
       LSBase = WBBase ;
    TAKEABORT ;
    }

}

/***************************************************************************\
* This function does the work of storing the registers listed in an STM     *
* instruction, when the S bit is clear.  The code here is always increment  *
* after, it's up to the caller to get the input address correct and to      *
* handle base register modification.                                        *
\***************************************************************************/

static void StoreMult(ARMul_State *state, ARMword instr,
                      ARMword address, ARMword WBBase)
{ARMword temp ;

 UNDEF_LSMNoRegs ;
 UNDEF_LSMPCBase ;
 UNDEF_LSMBaseInListWb ;
 if (!TFLAG) {
   BUSUSEDINCPCN ; /* N-cycle, increment the PC and update the NextInstr state */
 }

#ifndef MODE32
 if (VECTORACCESS(address) || ADDREXCEPT(address)) {
    INTERNALABORT(address) ;
    }
 if (BIT(15))
    PATCHR15 ;
#endif

 for (temp = 0 ; !BIT(temp) ; temp++) ; /* N cycle first */
#ifdef MODE32
 ARMul_StoreWordN(state,address,state->Reg[temp++]) ;
#else
 if (state->Aborted) {
    (void)ARMul_LoadWordN(state,address) ;
    for ( ; temp < 16 ; temp++) /* Fake the Stores as Loads */
       if (BIT(temp)) { /* save this register */
          address += 4 ;
          (void)ARMul_LoadWordS(state,address) ;
          }
    if (BIT(21) && LHSReg != 15)
       LSBase = WBBase ;
    TAKEABORT ;
    return ;
    }
 else
    ARMul_StoreWordN(state,address,state->Reg[temp++]) ;
#endif
 if (state->abortSig && !state->Aborted)
    state->Aborted = ARMul_DataAbortV ;

 if (BIT(21) && LHSReg != 15)
    LSBase = WBBase ;

 for ( ; temp < 16 ; temp++) /* S cycles from here on */
    if (BIT(temp)) { /* save this register */
       address += 4 ;
       ARMul_StoreWordS(state,address,state->Reg[temp]) ;
       if (state->abortSig && !state->Aborted)
             state->Aborted = ARMul_DataAbortV ;
       }
    if (state->Aborted) {
       TAKEABORT ;
       }
 }

/***************************************************************************\
* This function does the work of storing the registers listed in an STM     *
* instruction when the S bit is set.  The code here is always increment     *
* after, it's up to the caller to get the input address correct and to      *
* handle base register modification.                                        *
\***************************************************************************/

static void StoreSMult(ARMul_State *state, ARMword instr,
                       ARMword address, ARMword WBBase)
{ARMword temp ;

 UNDEF_LSMNoRegs ;
 UNDEF_LSMPCBase ;
 UNDEF_LSMBaseInListWb ;
 BUSUSEDINCPCN ;
#ifndef MODE32
 if (VECTORACCESS(address) || ADDREXCEPT(address)) {
    INTERNALABORT(address) ;
    }
 if (BIT(15))
    PATCHR15 ;
#endif

 if (state->Bank != USERBANK) {
    (void)ARMul_SwitchMode(state,state->Mode,USER26MODE) ; /* Force User Bank */
    UNDEF_LSMUserBankWb ;
    }

 for (temp = 0 ; !BIT(temp) ; temp++) ; /* N cycle first */
#ifdef MODE32
 ARMul_StoreWordN(state,address,state->Reg[temp++]) ;
#else
 if (state->Aborted) {
    (void)ARMul_LoadWordN(state,address) ;
    for ( ; temp < 16 ; temp++) /* Fake the Stores as Loads */
       if (BIT(temp)) { /* save this register */
          address += 4 ;
          (void)ARMul_LoadWordS(state,address) ;
          }
    if (BIT(21) && LHSReg != 15)
       LSBase = WBBase ;
    TAKEABORT ;
    return ;
    }
 else
    ARMul_StoreWordN(state,address,state->Reg[temp++]) ;
#endif
 if (state->abortSig && !state->Aborted)
    state->Aborted = ARMul_DataAbortV ;

 if (BIT(21) && LHSReg != 15)
    LSBase = WBBase ;

 for (; temp < 16 ; temp++) /* S cycles from here on */
    if (BIT(temp)) { /* save this register */
       address += 4 ;
       ARMul_StoreWordS(state,address,state->Reg[temp]) ;
       if (state->abortSig && !state->Aborted)
             state->Aborted = ARMul_DataAbortV ;
       }

 if (state->Mode != USER26MODE && state->Mode != USER32MODE)
    (void)ARMul_SwitchMode(state,USER26MODE,state->Mode) ; /* restore the correct bank */

 if (state->Aborted) {
    TAKEABORT ;
    }
}

/***************************************************************************\
* This function does the work of adding two 32bit values together, and      *
* calculating if a carry has occurred.                                      *
\***************************************************************************/

static ARMword Add32(ARMword a1,ARMword a2,int *carry)
{
  ARMword result = (a1 + a2);
  unsigned int uresult = (unsigned int)result;
  unsigned int ua1 = (unsigned int)a1;

  /* If (result == RdLo) and (state->Reg[nRdLo] == 0),
     or (result > RdLo) then we have no carry: */
  if ((uresult == ua1) ? (a2 != 0) : (uresult < ua1))
   *carry = 1;
  else
   *carry = 0;

  return(result);
}

/***************************************************************************\
* This function does the work of multiplying two 32bit values to give a     *
* 64bit result.                                                             *
\***************************************************************************/

static unsigned Multiply64(ARMul_State *state,ARMword instr,int msigned,int scc)
{
  int nRdHi, nRdLo, nRs, nRm; /* operand register numbers */
  ARMword RdHi, RdLo, Rm;
  int scount; /* cycle count */

  nRdHi = BITS(16,19);
  nRdLo = BITS(12,15);
  nRs = BITS(8,11);
  nRm = BITS(0,3);

  /* Needed to calculate the cycle count: */
  Rm = state->Reg[nRm];

  /* Check for illegal operand combinations first: */
  if (   nRdHi != 15
      && nRdLo != 15
      && nRs   != 15
      && nRm   != 15
      && nRdHi != nRdLo
      && nRdHi != nRm
      && nRdLo != nRm)
    {
      ARMword lo, mid1, mid2, hi; /* intermediate results */
      int carry;
      ARMword Rs = state->Reg[ nRs ];
      int sign = 0;

      if (msigned)
	{
	  /* Compute sign of result and adjust operands if necessary.  */
	  
	  sign = (Rm ^ Rs) & 0x80000000;
	  
	  if (((signed long)Rm) < 0)
	    Rm = -Rm;
	  
	  if (((signed long)Rs) < 0)
	    Rs = -Rs;
	}
      
      /* We can split the 32x32 into four 16x16 operations. This ensures
	 that we do not lose precision on 32bit only hosts: */
      lo =   ((Rs & 0xFFFF) * (Rm & 0xFFFF));
      mid1 = ((Rs & 0xFFFF) * ((Rm >> 16) & 0xFFFF));
      mid2 = (((Rs >> 16) & 0xFFFF) * (Rm & 0xFFFF));
      hi =   (((Rs >> 16) & 0xFFFF) * ((Rm >> 16) & 0xFFFF));
      
      /* We now need to add all of these results together, taking care
	 to propogate the carries from the additions: */
      RdLo = Add32(lo,(mid1 << 16),&carry);
      RdHi = carry;
      RdLo = Add32(RdLo,(mid2 << 16),&carry);
      RdHi += (carry + ((mid1 >> 16) & 0xFFFF) + ((mid2 >> 16) & 0xFFFF) + hi);

      if (sign)
	{
	  /* Negate result if necessary.  */
	  
	  RdLo = ~ RdLo;
	  RdHi = ~ RdHi;
	  if (RdLo == 0xFFFFFFFF)
	    {
	      RdLo = 0;
	      RdHi += 1;
	    }
	  else
	    RdLo += 1;
	}
      
      state->Reg[nRdLo] = RdLo;
      state->Reg[nRdHi] = RdHi;
      
    } /* else undefined result */
  else fprintf (stderr, "MULTIPLY64 - INVALID ARGUMENTS\n");
  
  if (scc)
    {
      if ((RdHi == 0) && (RdLo == 0))
	ARMul_NegZero(state,RdHi); /* zero value */
      else
	ARMul_NegZero(state,scc); /* non-zero value */
    }
  
  /* The cycle count depends on whether the instruction is a signed or
     unsigned multiply, and what bits are clear in the multiplier: */
  if (msigned && (Rm & ((unsigned)1 << 31)))
    Rm = ~Rm; /* invert the bits to make the check against zero */
  
  if ((Rm & 0xFFFFFF00) == 0)
    scount = 1 ;
  else if ((Rm & 0xFFFF0000) == 0)
    scount = 2 ;
  else if ((Rm & 0xFF000000) == 0)
    scount = 3 ;
  else
    scount = 4 ;
  
  return 2 + scount ;
}

/***************************************************************************\
* This function does the work of multiplying two 32bit values and adding    *
* a 64bit value to give a 64bit result.                                     *
\***************************************************************************/

static unsigned MultiplyAdd64(ARMul_State *state,ARMword instr,int msigned,int scc)
{
  unsigned scount;
  ARMword RdLo, RdHi;
  int nRdHi, nRdLo;
  int carry = 0;

  nRdHi = BITS(16,19);
  nRdLo = BITS(12,15);

  RdHi = state->Reg[nRdHi] ;
  RdLo = state->Reg[nRdLo] ;

  scount = Multiply64(state,instr,msigned,LDEFAULT);

  RdLo = Add32(RdLo,state->Reg[nRdLo],&carry);
  RdHi = (RdHi + state->Reg[nRdHi]) + carry;

  state->Reg[nRdLo] = RdLo;
  state->Reg[nRdHi] = RdHi;

  if (scc) {
    if ((RdHi == 0) && (RdLo == 0))
     ARMul_NegZero(state,RdHi); /* zero value */
    else
     ARMul_NegZero(state,scc); /* non-zero value */
  }

  return scount + 1; /* extra cycle for addition */
}
