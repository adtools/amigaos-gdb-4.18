/* Host-dependent code for AmigaOS for GDB, the GNU debugger. */

/* Target-vector operations for controlling Unix child processes, for GDB.
   Copyright 1990, 1991, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.
   Contributed by Cygnus Support.

This file is part of GDB.

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "defs.h"
#include "frame.h"  /* required by inferior.h */
#include "inferior.h"
#include "target.h"
#include "wait.h"
#include "gdbcore.h"
#include "command.h"
#include "xcoffsolib.h"
#include "symfile.h"
#include "objfiles.h"
#include "libbfd.h"  /* For bfd_cache_lookup (FIXME) */
#include "bfd.h"
#include <signal.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/ptrace.h>
#include <machine/reg.h>

#undef BITSPERBYTE
#undef LONGBITS
#undef MAXINT
#undef MININT
#include <proto/exec.h>
#include <proto/dos.h>

static void
child_prepare_to_store PARAMS ((void));

#ifndef CHILD_WAIT
static int child_wait PARAMS ((int, struct target_waitstatus *));
#endif /* CHILD_WAIT */

static void child_open PARAMS ((char *, int));

static void
child_files_info PARAMS ((struct target_ops *));

static void
child_detach PARAMS ((char *, int));

static void
child_attach PARAMS ((char *, int));

static void
ptrace_me PARAMS ((void));

static void
ptrace_him PARAMS ((int));

static void child_create_inferior PARAMS ((char *, char *, char **));

static void
child_mourn_inferior PARAMS ((void));

static int
child_can_run PARAMS ((void));

extern struct vmap * map_vmap PARAMS ((bfd *bf, bfd *arch));
extern struct target_ops exec_ops;

extern void
fixup_breakpoints PARAMS ((CORE_ADDR low, CORE_ADDR high, CORE_ADDR delta));

extern char **environ;

int amigaos_enforcer = 0;

/* Forward declaration */
extern struct target_ops child_ops;

#ifndef CHILD_WAIT

/* Wait for child to do something.  Return pid of child, or -1 in case
   of error; store status through argument pointer OURSTATUS.  */

static int
child_wait (pid, ourstatus)
     int pid;
     struct target_waitstatus *ourstatus;
{
  int save_errno;
  int status;

  do {
    set_sigint_trap();	/* Causes SIGINT to be passed on to the
			   attached process. */
    set_sigio_trap ();

    pid = wait (&status);
    save_errno = errno;

    clear_sigio_trap ();

    clear_sigint_trap();

    if (pid == -1)
      {
	if (save_errno == EINTR)
	  continue;
	fprintf_unfiltered (gdb_stderr, "Child process unexpectedly missing: %s.\n",
		 safe_strerror (save_errno));
	/* Claim it exited with unknown signal.  */
	ourstatus->kind = TARGET_WAITKIND_SIGNALLED;
	ourstatus->value.sig = TARGET_SIGNAL_UNKNOWN;
        return -1;
      }
  } while (pid != inferior_pid); /* Some other child died or stopped */
  store_waitstatus (ourstatus, status);
  return pid;
}
#endif /* CHILD_WAIT */

/* Attach to process PID, then initialize for debugging it.  */

static void
child_attach (args, from_tty)
     char *args;
     int from_tty;
{
  if (!args)
    error_no_arg ("process-id to attach");

  {
    char *exec_file;
    int pid, cli_pid;

    pid = atoi (args);
    Forbid();
    if (cli_pid = (int)FindCliProc(pid))
      pid = cli_pid;
    Permit();

    if (pid == getpid())		/* Trying to masturbate? */
      error ("I refuse to debug myself!");

    if (from_tty)
      {
	exec_file = (char *) get_exec_file (0);

	if (exec_file)
	  printf_unfiltered ("Attaching to program `%s', %s\n", exec_file,
		  target_pid_to_str (pid));
	else
	  printf_unfiltered ("Attaching to %s\n", target_pid_to_str (pid));

	gdb_flush (gdb_stdout);
      }

    attach (pid);
    inferior_pid = pid;
    push_target (&child_ops);
  }
}


/* Take a program previously attached to and detaches it.
   The program resumes execution and will no longer stop
   on signals, etc.  We'd better not have left any breakpoints
   in the program or it'll die when it hits one.  For this
   to work, it may be necessary for the process to have been
   previously attached.  It *might* work if the program was
   started via the normal ptrace (PTRACE_TRACEME).  */

static void
child_detach (args, from_tty)
     char *args;
     int from_tty;
{
  {
    int siggnal = 0;

    if (from_tty)
      {
	char *exec_file = get_exec_file (0);
	if (exec_file == 0)
	  exec_file = "";
	printf_unfiltered ("Detaching from program: %s %s\n", exec_file,
		target_pid_to_str (inferior_pid));
	gdb_flush (gdb_stdout);
      }
    if (args)
      siggnal = atoi (args);

    detach (siggnal);
    inferior_pid = 0;
    unpush_target (&child_ops);
  }
}

/* Get ready to modify the registers array.  On machines which store
   individual registers, this doesn't need to do anything.  On machines
   which store all the registers in one fell swoop, this makes sure
   that registers contains all the registers from the program being
   debugged.  */

static void
child_prepare_to_store ()
{
#ifdef CHILD_PREPARE_TO_STORE
  CHILD_PREPARE_TO_STORE ();
#endif
}

/* Print status information about what we're accessing.  */

static void
child_files_info (ignore)
     struct target_ops *ignore;
{
  printf_unfiltered ("\tUsing the running image of %s %s.\n",
	  attach_flag? "attached": "child", target_pid_to_str (inferior_pid));
}

/* ARGSUSED */
static void
child_open (arg, from_tty)
     char *arg;
     int from_tty;
{
  error ("Use the \"run\" command to start a Unix child process.");
}

/* Stub function which causes the inferior that runs it, to be ptrace-able
   by its parent process.  */

static void
ptrace_me ()
{
  /* "Trace me, Dr. Memory!" */
  call_ptrace (0, 0, (PTRACE_ARG3_TYPE) 0, 0);
}

/* Stub function which causes the GDB that runs it, to start ptrace-ing
   the child process.  */

static void
ptrace_him (pid)
     int pid;
{
  push_target (&child_ops);

#ifdef START_INFERIOR_TRAPS_EXPECTED
  startup_inferior (START_INFERIOR_TRAPS_EXPECTED);
#else
  /* One trap to exec the shell, one to exec the program being debugged.  */
  startup_inferior (2);
#endif
}

/* Start an inferior Unix child process and sets inferior_pid to its pid.
   EXEC_FILE is the file to run.
   ALLARGS is a string containing the arguments to the program.
   ENV is the environment vector to pass.  Errors reported with error().  */

static void
child_create_inferior (exec_file, allargs, env)
     char *exec_file;
     char *allargs;
     char **env;
{
  fork_inferior (exec_file, allargs, env, ptrace_me, ptrace_him, NULL, NULL);
  /* We are at the first instruction we care about.  */
  /* Pedal to the metal... */
  proceed ((CORE_ADDR) -1, TARGET_SIGNAL_0, 0);
}

static void
child_mourn_inferior ()
{
  unpush_target (&child_ops);
  proc_remove_foreign (inferior_pid);
  generic_mourn_inferior ();
}

static int
child_can_run ()
{
  return(1);
}

/* Send a SIGINT to the process group.  This acts just like the user typed a
   ^C on the controlling terminal.

   XXX - This may not be correct for all systems.  Some may want to use
   killpg() instead of kill (-pgrp). */

void
child_stop ()
{
  extern pid_t inferior_process_group;

  kill (-inferior_process_group, SIGINT);
}

struct target_ops child_ops = {
  "child",			/* to_shortname */
  "Unix child process",		/* to_longname */
  "Unix child process (started by the \"run\" command).",	/* to_doc */
  child_open,			/* to_open */
  0,				/* to_close */
  child_attach,			/* to_attach */
  child_detach, 		/* to_detach */
  child_resume,			/* to_resume */
  child_wait,			/* to_wait */
  fetch_inferior_registers,	/* to_fetch_registers */
  store_inferior_registers,	/* to_store_registers */
  child_prepare_to_store,	/* to_prepare_to_store */
  child_xfer_memory,		/* to_xfer_memory */
  child_files_info,		/* to_files_info */
  memory_insert_breakpoint,	/* to_insert_breakpoint */
  memory_remove_breakpoint,	/* to_remove_breakpoint */
  terminal_init_inferior,	/* to_terminal_init */
  terminal_inferior, 		/* to_terminal_inferior */
  terminal_ours_for_output,	/* to_terminal_ours_for_output */
  terminal_ours,		/* to_terminal_ours */
  child_terminal_info,		/* to_terminal_info */
  kill_inferior,		/* to_kill */
  0,				/* to_load */
  0,				/* to_lookup_symbol */
  child_create_inferior,	/* to_create_inferior */
  child_mourn_inferior,		/* to_mourn_inferior */
  child_can_run,		/* to_can_run */
  0, 				/* to_notice_signals */
  0,				/* to_thread_alive */
  child_stop,			/* to_stop */
  process_stratum,		/* to_stratum */
  0,				/* to_next */
  1,				/* to_has_all_memory */
  1,				/* to_has_memory */
  1,				/* to_has_stack */
  1,				/* to_has_registers */
  1,				/* to_has_execution */
  0,				/* to_sections */
  0,				/* to_sections_end */
  OPS_MAGIC			/* to_magic */
};

void
_initialize_inftarg ()
{
  add_target (&child_ops);
}

void
fetch_inferior_registers (int regno)
{
  struct reg inferior_registers;
  struct fpreg inferior_fp_registers;

  extern char registers[];

  registers_fetched ();
  
  ptrace (PT_GETREGS, inferior_pid,
	  (PTRACE_ARG3_TYPE) &inferior_registers, 0);
  ptrace (PT_GETFPREGS, inferior_pid,
	  (PTRACE_ARG3_TYPE) &inferior_fp_registers, 0);
  
  memcpy (registers, &inferior_registers, 16 * 4);
  memcpy (&registers[REGISTER_BYTE (FP0_REGNUM)], &inferior_fp_registers,
	 sizeof inferior_fp_registers.r_fpregs);
  *(int *)&registers[REGISTER_BYTE (PS_REGNUM)] = inferior_registers.r_sr;
  *(caddr_t *)&registers[REGISTER_BYTE (PC_REGNUM)] = inferior_registers.r_pc;
  memcpy (&registers[REGISTER_BYTE (FPC_REGNUM)],
	 &inferior_fp_registers.r_fpcontrol,
	 sizeof inferior_fp_registers - sizeof inferior_fp_registers.r_fpregs);
}

/* Store our register values back into the inferior.
   If REGNO is -1, do this for all registers.
   Otherwise, REGNO specifies which register (so we can save time).  */

void
store_inferior_registers (int regno)
{
  struct reg inferior_registers;
  struct fpreg inferior_fp_registers;

  extern char registers[];

  memcpy (&inferior_registers, registers, 16 * 4);
  memcpy (&inferior_fp_registers, &registers[REGISTER_BYTE (FP0_REGNUM)],
	 sizeof inferior_fp_registers.r_fpregs);
  inferior_registers.r_sr = *(int *)&registers[REGISTER_BYTE (PS_REGNUM)];
  inferior_registers.r_pc = *(caddr_t *)&registers[REGISTER_BYTE (PC_REGNUM)];

  memcpy (&inferior_fp_registers.r_fpcontrol,
	 &registers[REGISTER_BYTE (FPC_REGNUM)],
	 sizeof inferior_fp_registers - sizeof inferior_fp_registers.r_fpregs);

  ptrace (PT_SETREGS, inferior_pid,
	  (PTRACE_ARG3_TYPE) &inferior_registers, 0);
  ptrace (PT_SETFPREGS, inferior_pid,
	  (PTRACE_ARG3_TYPE) &inferior_fp_registers, 0);
}

/* Add symbols for an objfile.  */

static int
objfile_symbol_add (arg)
     char *arg;
{
  struct objfile *obj = (struct objfile *) arg;

  syms_from_objfile (obj, 0, 0, 0);
  new_symfile_objfile (obj, 0, 0);
  return 1;
}

/* handle symbol translation on vmapping */

static void
vmap_symtab (vp)
     register struct vmap *vp;
{
  register struct objfile *objfile;
  asection *textsec;
  asection *datasec;
  asection *bsssec;
  CORE_ADDR text_delta;
  CORE_ADDR data_delta;
  CORE_ADDR bss_delta;
  struct section_offsets *new_offsets;
  int i;
  
  objfile = vp->objfile;
  if (objfile == NULL)
    {
      /* OK, it's not an objfile we opened ourselves.
	 Currently, that can only happen with the exec file, so
	 relocate the symbols for the symfile.  */
      if (symfile_objfile == NULL)
	return;
      objfile = symfile_objfile;
    }

  new_offsets = alloca
    (sizeof (struct section_offsets)
     + sizeof (new_offsets->offsets) * objfile->num_sections);

  for (i = 0; i < objfile->num_sections; ++i)
    ANOFFSET (new_offsets, i) = ANOFFSET (objfile->section_offsets, i);
  
  textsec = bfd_get_section_by_name (vp->bfd, ".text");
  text_delta =
    vp->tstart - ANOFFSET (objfile->section_offsets, textsec->target_index);
  ANOFFSET (new_offsets, textsec->target_index) = vp->tstart;

  datasec = bfd_get_section_by_name (vp->bfd, ".data");
  if (datasec)
  {
    data_delta =
      vp->dstart - ANOFFSET (objfile->section_offsets, datasec->target_index);
    ANOFFSET (new_offsets, datasec->target_index) = vp->dstart;
  }
  
  bsssec = bfd_get_section_by_name (vp->bfd, ".bss");
  if (bsssec)
  {
    bss_delta =
      vp->bstart - ANOFFSET (objfile->section_offsets, bsssec->target_index);
    ANOFFSET (new_offsets, bsssec->target_index) = vp->bstart;
  }

  objfile_relocate (objfile, new_offsets);

  {
    struct obj_section *s;
    for (s = objfile->sections; s < objfile->sections_end; ++s)
      {
	if (s->the_bfd_section->target_index == textsec->target_index)
	  {
	    s->addr += text_delta;
	    s->endaddr += text_delta;
	  }
	else if (datasec && s->the_bfd_section->target_index == datasec->target_index)
	  {
	    s->addr += data_delta;
	    s->endaddr += data_delta;
	  }
	else if (bsssec && s->the_bfd_section->target_index == bsssec->target_index)
	  {
	    s->addr += bss_delta;
	    s->endaddr += bss_delta;
	  }
      }
  }
  
  breakpoint_re_set ();
}

/* Add a new vmap entry for a shared library.

   Return the vmap new entry.  */

static struct vmap *
add_vmap (char *name)
{
  bfd *abfd, *last;
  register char *mem, *objname;
  struct objfile *obj;
  struct vmap *vp;

  for (vp = vmap; vp; vp = vp->nxt)
  {
    if (!stricmp(vp->name, name))
      return vp;
  }
  /* This ldi structure was allocated using alloca() in 
     xcoff_relocate_symtab(). Now we need to have persistent object 
     and member names, so we should save them. */

  mem = name + strlen (name) + 1;
  mem = savestring (mem, strlen (mem));
  objname = savestring (name, strlen (name));

  abfd = bfd_openr (objname, gnutarget);
  if (!abfd)
  {
    if (stricmp("/libs/ixemul.debug", name) == 0)
      return NULL;
    error ("Could not open `%s' as an executable file: %s",
	   objname, bfd_errmsg (bfd_get_error ()));
  }

  /* make sure we have an object file */

  if (bfd_check_format (abfd, bfd_object))
    vp = map_vmap (abfd, 0);
  else
    {
    obj_err:
      bfd_close (abfd);
      error ("\"%s\": not in executable format: %s.",
	     objname, bfd_errmsg (bfd_get_error ()));
      /*NOTREACHED*/
    }
  obj = allocate_objfile (vp->bfd, 0, 0, 0);
  vp->objfile = obj;

#ifndef SOLIB_SYMBOLS_MANUAL
  if (catch_errors (objfile_symbol_add, (char *)obj,
		    "Error while reading shared library symbols:\n",
		    RETURN_MASK_ALL))
    {
      /* Note this is only done if symbol reading was successful.  */
      vmap_symtab (vp); /* why do this if amigaos_relocate_symtab also calls this? */
      vp->loaded = 1;
    }
#endif
  return vp;
}

void amigaos_relocate_symtab(unsigned int pid)
{
  long segs = 1, a4 = 0;
  unsigned long newstart[3];
  int i, n, offset;
  register struct vmap *vp;
  struct ixinfo *info;

  if (segs)
  {
    for (vp = vmap; vp; vp = vp->nxt)
      {
        if (vp->objfile == NULL)  /* is it the executable? */
        {
	  segs = ptrace(PT_GETSEGS, pid, 0, 0);
	  a4 = ptrace(PT_GETA4, pid, 0, 0);
	  if (a4 == -1)
	    a4 = 0;
	}
        else
        {
	  info = (struct ixinfo *)ptrace(PT_GETIXINFO, 0, 0, 0);
	  segs = info->ixemul_seglist;
	}
	i = 0;
	while (segs && i < 3)
	{
	  segs <<= 2;
	  n = *(long *)segs;
	  newstart[i++] = segs + 4;
	  segs = n;
	}
	if (a4)
	  newstart[1] = a4 - 0x7ffe; /* baserelative executable: start of
	                                data section is at a4 - 0x7ffe */
	vp->tend = (CORE_ADDR) newstart[0] + vp->tend - vp->tstart;
	vp->dend = (CORE_ADDR) newstart[1] + vp->dend - vp->dstart;
	vp->bend = (CORE_ADDR) newstart[2] + vp->bend - vp->bstart;
	vp->tstart = (CORE_ADDR) newstart[0];
	vp->dstart = (CORE_ADDR) newstart[1];
	vp->bstart = (CORE_ADDR) newstart[2];

	/* relocate symbol table(s). */
	vmap_symtab (vp);
        if (vp->objfile == NULL)  /* is it the executable? */
	  add_vmap("/libs/ixemul.debug");
      }
    if (!exec_ops.to_sections)
      error ("vmap_exec: exec_ops.to_sections == 0\n");

    for (i=0; &exec_ops.to_sections[i] < exec_ops.to_sections_end; i++)
      {
        if (STREQ(".text", exec_ops.to_sections[i].the_bfd_section->name))
  	{
  	  offset = newstart[0] - exec_ops.to_sections[i].addr;
  	  exec_ops.to_sections[i].addr += offset;
  	  exec_ops.to_sections[i].endaddr += offset;
  	}
        else if (STREQ(".data", exec_ops.to_sections[i].the_bfd_section->name))
  	{
  	  offset = newstart[1] - exec_ops.to_sections[i].addr;
  	  exec_ops.to_sections[i].addr += offset;
  	  exec_ops.to_sections[i].endaddr += offset;
  	}
        else if (STREQ(".bss", exec_ops.to_sections[i].the_bfd_section->name))
  	{
  	  offset = newstart[2] - exec_ops.to_sections[i].addr;
  	  exec_ops.to_sections[i].addr += offset;
  	  exec_ops.to_sections[i].endaddr += offset;
  	}
      }
  }
}

/* Given an ip value corresponding to the start of a function,
   return the ip of the first instruction after the function 
   prologue.  This is the generic m68k support.  Machines which
   require something different can override the SKIP_PROLOGUE
   macro to point elsewhere.

   Some instructions which typically may appear in a function
   prologue include:

   A link instruction, word form:

	link.w	%a5,&0			4e55  XXXX

   A link instruction, long form:

	link.l  %fp,&F%1		480d  XXXX  XXXX

   A movm instruction to preserve integer regs:

	movm.l  &M%1,(4,%sp)		48ef  XXXX  XXXX

   A fmovm instruction to preserve float regs:

	fmovm   &FPM%1,(FPO%1,%sp)	f237  XXXX  XXXX  XXXX  XXXX

   Some profiling setup code (FIXME, not recognized yet):

	lea.l   (.L3,%pc),%a1		43fb  XXXX  XXXX  XXXX
	bsr     _mcount			61ff  XXXX  XXXX

  */

#define P_LINK_L	0x480d
#define P_LINK_W	0x4e55
#define P_MOV_L		0x207c
#define P_JSR		0x4eb9
#define P_BSR		0x61ff
#define P_LEA_L		0x43fb
#define P_MOVM_L	0x48ef
#define P_FMOVM		0xf237
#define P_TRAP		0x4e40

CORE_ADDR
amigaos_skip_prologue (ip)
CORE_ADDR ip;
{
  register CORE_ADDR limit;
  struct symtab_and_line sal;
  register int op;

  /* Find out if there is a known limit for the extent of the prologue.
     If so, ensure we don't go past it.  If not, assume "infinity". */

  sal = find_pc_line (ip, 0);
  limit = (sal.end) ? sal.end : (CORE_ADDR) ~0;

  while (ip < limit)
    {
      op = read_memory_integer (ip, 2);
      op &= 0xFFFF;
      
      if (op == P_LINK_W)
	{
	  ip += 4;	/* Skip link.w */
	}
      else if (op == 0x4855)
	ip += 2; /* Skip pea %fp */
      else if (op == 0x2b4f)
	ip += 2; /* Skip move.l %sp, %fp */
      else if (op == P_LINK_L)
	{
	  ip += 6;	/* Skip link.l */
	}
      else if (op == P_MOVM_L)
	{
	  ip += 6;	/* Skip movm.l */
	}
      else if (op == P_FMOVM)
	{
	  ip += 10;	/* Skip fmovm */
	}
      else
	{
	  break;	/* Found unknown code, bail out. */
	}
    }
  return (ip);
}

void
amigaos_find_saved_regs (frame_info, saved_regs)
     struct frame_info *frame_info;
     struct frame_saved_regs *saved_regs;
{
  register int regnum;							
  register int regmask;							
  register CORE_ADDR next_addr;						
  register CORE_ADDR pc;

  /* First possible address for a pc in a call dummy for this frame.  */
  CORE_ADDR possible_call_dummy_start =
    (frame_info)->frame - CALL_DUMMY_LENGTH - FP_REGNUM*4 - 4 - 4 - 8*12;

  int nextinsn;
  memset (saved_regs, 0, sizeof (*saved_regs));
  if ((frame_info)->pc >= possible_call_dummy_start
      && (frame_info)->pc <= (frame_info)->frame)
    {

      /* It is a call dummy.  We could just stop now, since we know
	 what the call dummy saves and where.  But this code proceeds
	 to parse the "prologue" which is part of the call dummy.
	 This is needlessly complex and confusing.  FIXME.  */

      next_addr = (frame_info)->frame;
      pc = possible_call_dummy_start;
    }
  else   								
    {
      pc = get_pc_function_start ((frame_info)->pc); 			

      if (0x4855 == read_memory_integer (pc, 2)
	  && 0x2b4f == read_memory_integer (pc + 2, 2))
	{
	  /*
	    pea %fp
            move.l %sp, %fp */

	  pc += 4;
	  next_addr = frame_info->frame;
	}
      else if (0x480d == read_memory_integer (pc, 2))
	/* link.l %fp */
	/* Find the address above the saved   
	   regs using the amount of storage from the link instruction.  */
	next_addr = (frame_info)->frame + read_memory_integer (pc += 2, 4), pc+=4; 
      else if (0x4e55 == read_memory_integer (pc, 2))			
	/* link.w %fp */
	/* Find the address above the saved   
	   regs using the amount of storage from the link instruction.  */
	next_addr = (frame_info)->frame + read_memory_integer (pc += 2, 2), pc+=2; 
      else goto lose;

      /* If have an addal #-n, sp next, adjust next_addr.  */		
      if ((0177777 & read_memory_integer (pc, 2)) == 0157774)		
	next_addr += read_memory_integer (pc += 2, 4), pc += 4;		
    }									
  regmask = read_memory_integer (pc + 2, 2);				

  /* Here can come an fmovem.  Check for it.  */		
  nextinsn = 0xffff & read_memory_integer (pc, 2);			
  if (0xf227 == nextinsn						
      && (regmask & 0xff00) == 0xe000)					
    { pc += 4; /* Regmask's low bit is for register fp7, the first pushed */ 
      for (regnum = FP0_REGNUM + 7; regnum >= FP0_REGNUM; regnum--, regmask >>= 1)		
	if (regmask & 1)						
          saved_regs->regs[regnum] = (next_addr -= 12);		
      regmask = read_memory_integer (pc + 2, 2); }

  /* next should be a moveml to (sp) or -(sp) or a movl r,-(sp) */	
  if (0044327 == read_memory_integer (pc, 2))				
    { pc += 4; /* Regmask's low bit is for register 0, the first written */ 
      for (regnum = 0; regnum < 16; regnum++, regmask >>= 1)		
	if (regmask & 1)						
          saved_regs->regs[regnum] = (next_addr += 4) - 4; }	
  else if (0044347 == read_memory_integer (pc, 2))			
    {
      pc += 4; /* Regmask's low bit is for register 15, the first pushed */ 
      for (regnum = 15; regnum >= 0; regnum--, regmask >>= 1)		
	if (regmask & 1)						
          saved_regs->regs[regnum] = (next_addr -= 4);
    }
  else if (0x2f00 == (0xfff0 & read_memory_integer (pc, 2)))		
    {
      regnum = 0xf & read_memory_integer (pc, 2); pc += 2;		
      saved_regs->regs[regnum] = (next_addr -= 4);
      /* gcc, at least, may use a pair of movel instructions when saving
	 exactly 2 registers.  */
      if (0x2f00 == (0xfff0 & read_memory_integer (pc, 2)))
	{
	  regnum = 0xf & read_memory_integer (pc, 2);
	  pc += 2;
	  saved_regs->regs[regnum] = (next_addr -= 4);
	}
    }

  /* fmovemx to index of sp may follow.  */				
  regmask = read_memory_integer (pc + 2, 2);				
  nextinsn = 0xffff & read_memory_integer (pc, 2);			
  if (0xf236 == nextinsn						
      && (regmask & 0xff00) == 0xf000)					
    { pc += 10; /* Regmask's low bit is for register fp0, the first written */ 
      for (regnum = FP0_REGNUM + 7; regnum >= FP0_REGNUM; regnum--, regmask >>= 1)		
	if (regmask & 1)						
          saved_regs->regs[regnum] = (next_addr += 12) - 12;	
      regmask = read_memory_integer (pc + 2, 2); }			

  /* clrw -(sp); movw ccr,-(sp) may follow.  */				
  if (0x426742e7 == read_memory_integer (pc, 4))			
    saved_regs->regs[PS_REGNUM] = (next_addr -= 4);		
  lose: ;								
  saved_regs->regs[SP_REGNUM] = (frame_info)->frame + 8;		
  saved_regs->regs[FP_REGNUM] = (frame_info)->frame;		
  saved_regs->regs[PC_REGNUM] = (frame_info)->frame + 4;		
#ifdef SIG_SP_FP_OFFSET
  /* Adjust saved SP_REGNUM for fake _sigtramp frames.  */
  if (frame_info->signal_handler_caller && frame_info->next)
    saved_regs->regs[SP_REGNUM] = frame_info->next->frame + SIG_SP_FP_OFFSET;
#endif
}

/* Push an empty stack frame, to record the current PC, etc.  */

void
amigaos_push_dummy_frame ()
{
  register CORE_ADDR sp = read_register (SP_REGNUM) - STACK_SPACE_FOR_DUMMY_FRAME;
  register int regnum;
  char raw_buffer[12];

  sp = push_word (sp, read_register (PC_REGNUM));
  sp = push_word (sp, read_register (FP_REGNUM));
  write_register (FP_REGNUM, sp);

  /* Always save the floating-point registers, whether they exist on
     this target or not.  */
  for (regnum = FP0_REGNUM + 7; regnum >= FP0_REGNUM; regnum--)
    {
      read_register_bytes (REGISTER_BYTE (regnum), raw_buffer, 12);
      sp = push_bytes (sp, raw_buffer, 12);
    }

  for (regnum = FP_REGNUM + 1; regnum >= 0; regnum--)
    {
      if (regnum != FP_REGNUM)
        sp = push_word (sp, read_register (regnum));
    }
  sp = push_word (sp, read_register (PS_REGNUM));
  write_register (SP_REGNUM, sp);
}

/* Discard from the stack the innermost frame,
   restoring all saved registers.  */

void
amigaos_pop_frame ()
{
  register struct frame_info *frame = get_current_frame ();
  register CORE_ADDR fp;
  register int regnum;
  struct frame_saved_regs fsr;
  struct frame_info *fi;
  char raw_buffer[12];
  long stack_offset = 0;

  fp = FRAME_FP (frame);
  if (PC_IN_CALL_DUMMY(read_pc(), read_sp(), fp))
    stack_offset = STACK_SPACE_FOR_DUMMY_FRAME;
  get_frame_saved_regs (frame, &fsr);
  for (regnum = FP0_REGNUM + 7 ; regnum >= FP0_REGNUM ; regnum--)
    {
      if (fsr.regs[regnum])
	{
	  read_memory (fsr.regs[regnum], raw_buffer, 12);
	  write_register_bytes (REGISTER_BYTE (regnum), raw_buffer, 12);
	}
    }
  for (regnum = FP_REGNUM + 1 ; regnum >= 0 ; regnum--)
    {
      if (regnum != FP_REGNUM && fsr.regs[regnum])
	{
	  write_register (regnum, read_memory_integer (fsr.regs[regnum], 4));
	}
    }
  if (fsr.regs[PS_REGNUM])
    {
      write_register (PS_REGNUM, read_memory_integer (fsr.regs[PS_REGNUM], 4));
    }
  write_register (FP_REGNUM, read_memory_integer (fp, 4));
  write_register (PC_REGNUM, read_memory_integer (fp + 4, 4));
  write_register (SP_REGNUM, fp + 8 + stack_offset);
  flush_cached_frames ();
}

int amigaos_pc_in_call_dummy(CORE_ADDR pc, CORE_ADDR sp, CORE_ADDR frame_address)
{
  return (INNER_THAN (sp-STACK_SPACE_FOR_DUMMY_FRAME,(pc)) &&
	 (frame_address != 0) && INNER_THAN ((pc), frame_address));
}

CORE_ADDR amigaos_skip_trampoline_code(CORE_ADDR pc)
{
  return NULL;
}

int amigaos_in_sigtramp(CORE_ADDR pc)
{
  struct ixinfo *info;
  
  info = (struct ixinfo *)ptrace(PT_GETIXINFO, 0, 0, 0);
  return (pc >= info->sigtramp_start && pc < info->sigtramp_end);
}

#include <proto/exec.h>

static void cleanup_trap(void)
{
  struct ixinfo *info;
  
  info = (struct ixinfo *)ptrace(PT_GETIXINFO, 0, 0, 0);
  Supervisor((void *)info->restore_vector);
}

void amigaos_install_enforcer_handler(void)
{
  struct ixinfo *info;
  
  if (!amigaos_enforcer)
    return;
  info = (struct ixinfo *)ptrace(PT_GETIXINFO, 0, 0, 0);
  if (info->install_vector)
    {
      Supervisor((void *)info->install_vector);
      atexit(cleanup_trap);
    }
  else
    {
      fprintf_unfiltered(gdb_stderr, "Cannot install the bus error handler: you probably\n\
don't have a 68020 or higher CPU.\n");
    }
}
