/* Native-dependent definitions for Commodore Amiga running AmigaOS.
   Copyright 1994 Free Software Foundation, Inc.
   Written by Fred Fish (fnf@cygnus.com)

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

#ifndef NM_AMIGAOS_H
#define NM_AMIGAOS_H

/* Override copies of {fetch,store}_inferior_registers in infptrace.c.  */

#define FETCH_INFERIOR_REGISTERS

/* Do implement the attach and detach commands.  */

#define ATTACH_DETACH

/* We use the same relocation hack as this target */
#define IBM6000_TARGET

#define PTRACE_ARG3_TYPE caddr_t

/* When a child process is just starting, we sneak in and relocate
   the symbol table (and other stuff) after the dynamic linker has
   figured out where they go.  */

#define SOLIB_CREATE_INFERIOR_HOOK(PID)	\
  do {					\
    amigaos_relocate_symtab (PID);	\
  } while (0)

/* When a target process or core-file has been attached, we sneak in
   and figure out where the shared libraries have got to.  */

#define	SOLIB_ADD(a, b, c)	\
  if (inferior_pid)	\
    /* Attach to process.  */  \
    amigaos_relocate_symtab (inferior_pid); \

extern void amigaos_relocate_symtab PARAMS ((unsigned int));

#endif /* NM_AMIGAOS_H */
