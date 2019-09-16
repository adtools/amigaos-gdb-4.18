/* Read AmigaOS object files for GDB.
   Copyright 1991, 1992, 1994 Free Software Foundation, Inc.
   Converted to AmigaOS format files by Leonard Norrgard.  Original
   was paread.c by Fred Fish.

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
#include "bfd.h"
#include <time.h> /* For time_t in libbfd.h.  */
#include <sys/types.h> /* For time_t, if not in time.h.  */
#include "libbfd.h"
#include "libamiga.h"
#include "symtab.h"
#include "symfile.h"
#include "objfiles.h"
#include "buildsym.h"
#include "stabsread.h"
#include "gdb-stabs.h"
#include "complaints.h"
#include <string.h>
#include "demangle.h"
#include <sys/file.h>

#include "aout/aout64.h"

/* Various things we might complain about... */

static void
amiga_symfile_init PARAMS ((struct objfile *));

static void
amiga_new_init PARAMS ((struct objfile *));

extern void
amiga_symfile_read PARAMS ((struct objfile *, struct section_offsets *, int));

static void
amiga_symfile_finish PARAMS ((struct objfile *));

static void
free_amigainfo PARAMS ((PTR));

static struct section_offsets *
amiga_symfile_offsets PARAMS ((struct objfile *, CORE_ADDR));

static void
record_minimal_symbol PARAMS ((char *, CORE_ADDR,
			       enum minimal_symbol_type,
			       struct objfile *));

static void
record_minimal_symbol (name, address, ms_type, objfile)
     char *name;
     CORE_ADDR address;
     enum minimal_symbol_type ms_type;
     struct objfile *objfile;
{
  name = obsavestring (name, strlen (name), &objfile -> symbol_obstack);
  prim_record_minimal_symbol (name, address, ms_type, objfile);
}

/* This cleans up the objfile's sym_stab_info pointer, and the chain of
   stab_section_info's, that might be dangling from it.  */

static void
free_amigainfo (objp)
     PTR objp;
{
  struct objfile *objfile = (struct objfile *)objp;
  struct dbx_symfile_info *dbxinfo = (struct dbx_symfile_info *)
				     objfile->sym_stab_info;
  struct stab_section_info *ssi, *nssi;

  ssi = dbxinfo->stab_section_info;
  while (ssi)
    {
      nssi = ssi->next;
      mfree (objfile->md, ssi);
      ssi = nssi;
    }

  dbxinfo->stab_section_info = 0;	/* Just say No mo info about this.  */
}

/* Initialize anything that needs initializing when a completely new symbol
   file is specified (not just adding some symbols from another file, e.g. a
   shared library).

   We reinitialize buildsym, since we may be reading stabs from an amigaos file.  */

static void
amiga_new_init (ignore)
     struct objfile *ignore;
{
  stabsread_new_init ();
  buildsym_new_init ();
}

/* Perform any local cleanups required when we are done with a particular
   objfile.  I.E, we are in the process of discarding all symbol information
   for an objfile, freeing up all memory held for it, and unlinking the
   objfile struct from the global list of known objfiles. */

static void
amiga_symfile_finish (objfile)
     struct objfile *objfile;
{
  if (objfile -> sym_stab_info != NULL)
    {
      mfree (objfile -> md, objfile -> sym_stab_info);
    }
}

/* AmigaOS specific initialization routine for reading symbols.

   It is passed a pointer to a struct sym_fns which contains, among other
   things, the BFD for the file whose symbols are being read, and a slot for
   a pointer to "private data" which we can fill with goodies.

   This routine is almost a complete ripoff of dbx_symfile_init.  The
   common parts of these routines should be extracted and used instead of
   duplicating this code.  FIXME. */

static void
amiga_symfile_init (objfile)
     struct objfile *objfile;
{
  int val;
  bfd *sym_bfd = objfile->obfd;
  char *name = bfd_get_filename (sym_bfd);
  asection *text_sect;

  /* Allocate struct to keep track of the symfile */
  objfile->sym_stab_info = (PTR)
    xmmalloc (objfile -> md, sizeof (struct dbx_symfile_info));
  memset ((PTR) objfile->sym_stab_info, 0, sizeof (struct dbx_symfile_info));

  /* FIXME POKING INSIDE BFD DATA STRUCTURES */
#define	STRING_TABLE_OFFSET	adata(sym_bfd).str_filepos
#define	SYMBOL_TABLE_OFFSET	adata(sym_bfd).sym_filepos

  /* FIXME POKING INSIDE BFD DATA STRUCTURES */

  DBX_SYMFILE_INFO (objfile)->stab_section_info = NULL;
  text_sect = bfd_get_section_by_name (sym_bfd, ".text");
  if (!text_sect)
    error ("Can't find .text section in symbol file");
  DBX_TEXT_ADDR (objfile) = bfd_section_vma (sym_bfd, text_sect);
  DBX_TEXT_SIZE (objfile) = bfd_section_size (sym_bfd, text_sect);

  /* FIXME: I suspect this should be external_nlist.  The size of host
     types like long and bfd_vma should not affect how we read the
     file.  */
  DBX_SYMBOL_SIZE (objfile) = sizeof (struct internal_nlist);
  DBX_SYMCOUNT (objfile) = AMIGA_DATA(sym_bfd)->symtab_size
    / DBX_SYMBOL_SIZE (objfile);
  DBX_SYMTAB_OFFSET (objfile) = SYMBOL_TABLE_OFFSET;

  /* Read the string table and stash it away in the psymbol_obstack.  It is
     only needed as long as we need to expand psymbols into full symbols,
     so when we blow away the psymbol the string table goes away as well.
     Note that gdb used to use the results of attempting to malloc the
     string table, based on the size it read, as a form of sanity check
     for botched byte swapping, on the theory that a byte swapped string
     table size would be so totally bogus that the malloc would fail.  Now
     that we put in on the psymbol_obstack, we can't do this since gdb gets
     a fatal error (out of virtual memory) if the size is bogus.  We can
     however at least check to see if the size is zero or some negative
     value. */

  DBX_STRINGTAB_SIZE (objfile) = AMIGA_DATA (sym_bfd)->stringtab_size;

  if (DBX_SYMCOUNT (objfile) == 0
      || DBX_STRINGTAB_SIZE (objfile) == 0)
    return;

  if (DBX_STRINGTAB_SIZE (objfile) <= 0
      || DBX_STRINGTAB_SIZE (objfile) > bfd_get_size (sym_bfd))
    error ("ridiculous string table size (%d bytes).",
	   DBX_STRINGTAB_SIZE (objfile));

  DBX_STRINGTAB (objfile) =
    (char *) obstack_alloc (&objfile -> psymbol_obstack,
			    DBX_STRINGTAB_SIZE (objfile));
  OBJSTAT (objfile, sz_strtab += DBX_STRINGTAB_SIZE (objfile));

  /* Now read in the string table in one big gulp.  */

  val = bfd_seek (sym_bfd, STRING_TABLE_OFFSET, L_SET);
  if (val < 0)
    perror_with_name (name);
  val = bfd_read (DBX_STRINGTAB (objfile), DBX_STRINGTAB_SIZE (objfile), 1,
		  sym_bfd);
  if (val == 0)
    error ("End of file reading string table");
  else if (val < 0)
    /* It's possible bfd_read should be setting bfd_error, and we should be
       checking that.  But currently it doesn't set bfd_error.  */
    perror_with_name (name);
  else if (val != DBX_STRINGTAB_SIZE (objfile))
    error ("Short read reading string table");
}

/* Amiga specific parsing routine for section offsets.

   Plain and simple for now.  */

static struct section_offsets *
amiga_symfile_offsets (objfile, addr)
     struct objfile *objfile;
     CORE_ADDR addr;
{
  struct section_offsets *section_offsets;
  int i;

  objfile->num_sections = SECT_OFF_MAX;
  section_offsets = (struct section_offsets *)
    obstack_alloc (&objfile -> psymbol_obstack,
		   sizeof (struct section_offsets)
		   + sizeof (section_offsets->offsets) * (SECT_OFF_MAX-1));

  for (i = 0; i < SECT_OFF_MAX; i++)
    ANOFFSET (section_offsets, i) = addr;

  return section_offsets;
}

/* Register that we are able to handle AmigaOS object file formats.  */

static struct sym_fns amiga_sym_fns =
{
  bfd_target_amiga_flavour,
  amiga_new_init,	/* sym_new_init: init anything gbl to entire symtab */
  amiga_symfile_init,	/* sym_init: read initial info, setup for sym_read() */
  amiga_symfile_read,	/* sym_read: read a symbol file into symtab */
  amiga_symfile_finish,	/* sym_finish: finished with file, cleanup */
  amiga_symfile_offsets,/* sym_offsets:  Translate ext. to int. relocation */
  NULL			/* next: pointer to next struct sym_fns */
};

void
_initialize_amigaread ()
{
  add_symtab_fns (&amiga_sym_fns);
}
