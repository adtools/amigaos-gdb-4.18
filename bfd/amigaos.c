/* BFD back-end for Commodore-Amiga AmigaOS binaries.
   Copyright (C) 1990-1994 Free Software Foundation, Inc.
   Contributed by Leonard Norrgard.  Partially based on the bout
   and ieee BFD backends and Markus Wild's tool hunk2gcc.
   Revised and updated by Stephan Thesing 

This file is part of BFD, the Binary File Descriptor library.

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

/*
SECTION
	amiga back end

This section describes the overall structure of the Amiga BFD back end.
The linker stuff can be found in @xref{amigalink}.
@menu
@* implementation::
@* amigalink::
@end menu

INODE
implementation, amigalink, amiga, amiga


SECTION
	implementation


The need for a port of the bfd library for Amiga style object (hunk) files
arose by the desire to port the GNU debugger gdb to the Amiga.
Also, the linker ld should be updated to the current version (2.5.2).
@@*
This port bases on the work done by Leonard Norrgard, who started porting
gdb. Raphael Luebbert, who supports the ixemul.library, has also worked on
implementing the needed @code{ptrace()} system call and gas2.5.

@menu
@* not supported::
@* Does it work ?::
@* TODO::
@end menu

INODE
not supported, Does it work ?,implementation,implementation
SUBSECTION
	not supported

Currently, the implementation does not support Amiga link library files, like
e.g. amiga.lib. This may be added in a later version, if anyone starts work
on it, or I find some time for it.

The handling of the symbols in hunk files is a little bit broken:
	  o The symbols in a load file are totally ignored at the moment, so gdb and gprof
            do not work.
	  o The symbols of a object module (Hunk file, starting with HUNK_UNIT) are read in
            correctly, but HUNK_SYMBOL hunks are also ignored.

The reason for this is the following:
Amiga symbol hunks do not allow for much information. Only a name and a value are allowed.
On the other hand, a.out format carries along much more information (see, e.g. the
entry on set symbols in the ld manual). The old linker copied this information into
a HUNK_DEBUG hunk. Now there is the choice:
	o ignoring the debug hunk, read in only HUNK_SYMBOL definitions => extra info is lost.
	o read in the debug hunk and use the information therein => How can clashs between the
          information in the debug hunk and HUNK_SYMBOL or HUNK_EXT hunks be avoided ?
I haven't decided yet, what to do about this.


Although bfd allows to link together object modules of different flavours, 
producing a.out style executables does not work on Amiga :-)
It should, however, be possible to create a.out files with the -r option of ld
(incremental link).

INODE 
Does it work ?,TODO ,not supported , implementation
SUBSECTION
	Does it work ?

Currently, the following utillities work:
	o objdump
	o objcopy
	o strip
	o nm
	o ar
	o gas

	
INODE
TODO, , Does it work ?, implementation
SUBSECTION
	TODO

	o fix fixme:s

@*
BFD:
	o add flag to say if the format allows multiple sections with the
          same name.  Fix bfd_get_section_by_name() and bfd_make_section()
          accordingly.
       
	o dumpobj.c: the disassembler: use relocation record data to find symbolic
           names of addresses, when available.  Needs new routine where one can
          specify the source section of the symbol to be printed as well as some
          rewrite of the disassemble functions.
       
*/

#include "bfd.h"
#include "bfdlink.h"
#include "sysdep.h"
#include "libbfd.h"
#include "libamiga.h"
#include <a.out.h>

#define GL(x) bfd_get_32 (abfd, (bfd_byte *) (x))

#define DEBUG_AMIGA 10000

#if DEBUG_AMIGA
#include <varargs.h>
static void error_print(va_alist)
va_dcl
{
va_list args;
char *fmt;

va_start(args);
fmt=va_arg(args,char *);

(void)vfprintf(stderr,fmt,args);
va_end(args);
}

#define DPRINT(L,x) if (L>=DEBUG_AMIGA) error_print x
#else
#define DPRINT(L,x) 
#endif

static boolean amiga_digest_file ();
static boolean amiga_mkobject ();

reloc_howto_type howto_hunk_reloc8 =
{
  HUNK_RELOC8, /* type */
  0,           /* rightshift */
  0,           /* size */
  8,           /* bitsize */
  true,        /* pc_relative */
  0,           /* bitpos */
  complain_overflow_bitfield,  /* complain_on_overflow */
  0,           /* special_function */
  "reloc8",    /* textual name */
  false,       /* partial_inplace? */
  0x000000ff,  /* src_mask */
  0x000000ff,  /* dst_mask */
  true         /* pcrel_offset */
};

reloc_howto_type howto_hunk_reloc16 =
{HUNK_RELOC16,0,1,16,true,0,complain_overflow_bitfield,0,"reloc16",false,0x0000ffff,0x0000ffff,true};

reloc_howto_type howto_hunk_reloc32 =
{HUNK_RELOC32,0,2,32,true,0,complain_overflow_bitfield,0,"reloc32",false,0xffffffff,0xffffffff,true};

reloc_howto_type howto_hunk_drel8 =
{HUNK_DREL8,0,0,8,false,0,complain_overflow_bitfield,0,"drel8",false,0x000000ff,0x000000ff,false};

reloc_howto_type howto_hunk_drel16 =
{HUNK_DREL16,0,1,16,false,0,complain_overflow_bitfield,0,"drel16",false,0x0000ffff,0x0000ffff,false};

reloc_howto_type howto_hunk_drel32 =
{HUNK_DREL32,0,2,32,false,0,complain_overflow_bitfield,0,"drel32",false,0xffffffff,0xffffffff,false};

reloc_howto_type *amiga_howto_array[2][3] =
{
  { &howto_hunk_reloc8, &howto_hunk_reloc16, &howto_hunk_reloc32 },
  { &howto_hunk_drel8, &howto_hunk_drel16, &howto_hunk_drel32 }
};


/* This one is used by the linker and tells us, if a debug hunk should be written out*/
int write_debug_hunk=0;
/* This is also used by the linker to set the attribute of sections */
int amiga_attribute=0;

static const struct bfd_target *
amiga_object_p (abfd)
     bfd *abfd;
{
  char buf[8];
  unsigned int x;
  struct stat stat_buffer;

  /* An Amiga object file must be at least 8 bytes long.  */
  if (bfd_read ((PTR) buf, 1, 8, abfd) != 8)
    {
      bfd_set_error(bfd_error_wrong_format);
      return 0;
    }

  /* Does it look like an Amiga object file?  */
  x = GL(buf);
  if ((x != HUNK_UNIT) && (x != HUNK_HEADER))
    {
      /* Not an Amiga file.  */
      bfd_set_error(bfd_error_wrong_format);
      return 0;
    }

  /* So far it seems to be an Amiga file.  Now slurp it
     in and examine it closer.  */
  stat_buffer.st_size=0;
  if ((-1 == fstat (fileno ((FILE *) abfd->iostream), &stat_buffer))||(stat_buffer.st_size%4!=0))
    { /* Error during system call or file length is not multiple of longword size */
      bfd_set_error((stat_buffer.st_size%4==0)?bfd_error_system_call:bfd_error_wrong_format);
      return 0;
    }

  /* Can't fail and return (but must be declared boolean to suit
     other bfd requirements).  */
  (void) amiga_mkobject (abfd);


  AMIGA_DATA(abfd)->first_byte = (unsigned long *) bfd_alloc (abfd, stat_buffer.st_size);
  bfd_seek (abfd, 0, SEEK_SET);
  
  if (bfd_read (AMIGA_DATA(abfd)->first_byte, 1, stat_buffer.st_size, abfd)!=stat_buffer.st_size)
    {
      /* Error, reading file */
     return (const struct bfd_target *)0;
   }

  AMIGA_DATA(abfd)->file_pointer = AMIGA_DATA(abfd)->first_byte;
  AMIGA_DATA(abfd)->file_end = (unsigned long *)((unsigned char *)AMIGA_DATA(abfd)->first_byte + stat_buffer.st_size);

  if (!amiga_digest_file (abfd))
    {
      /* Something went wrong.  */
      return (const struct bfd_target *) 0;
    }

  /* Set default architecture to m68k:68000.  */
  /* So we can link on 68000 AMIGAs..... */
  abfd->arch_info = bfd_scan_arch ("m68k:68000");

  return (abfd->xvec);
}

/* Skip over the hunk length longword + the number of longwords given there.  */
#define next_hunk(abfd) \
  { AMIGA_DATA(abfd)->file_pointer += 1 + GL(AMIGA_DATA(abfd)->file_pointer); }

static asection *
amiga_get_section_by_hunk_number (abfd, hunk_number)
     bfd *abfd;
      long hunk_number;
{
  /* A cache, so we don't have to search the entire list every time.  */
  static asection *last_reference;
  asection *p;

  switch(hunk_number)
    {
    case -1:
      return bfd_abs_section_ptr;
      break;
    case -2:
      return bfd_und_section_ptr;
      break;
    case -3: 
      return bfd_com_section_ptr;
      break;
    default:
      if (last_reference)
	if (last_reference->target_index == hunk_number)
	  return last_reference;
      for (p = abfd->sections; p != NULL; p = p->next)
	if (p->target_index == hunk_number)
	  {
	    last_reference = p;
	    return p;
	  }
      BFD_FAIL();
      return (asection *) 0;
    }
return NULL;
}

static boolean
amiga_add_reloc (abfd, section, offset, symbol, howto, target_hunk)
     bfd *abfd;
     asection *section;
     bfd_size_type offset;
     amiga_symbol_type * symbol;
     reloc_howto_type *howto;
     long target_hunk;
{
  amiga_reloc_type *reloc;

  reloc = (amiga_reloc_type *) bfd_zalloc (abfd, sizeof (amiga_reloc_type));

  if (!reloc)
    {
      bfd_set_error(bfd_error_no_memory);
      return(false);
    }
  reloc->next = 0;


  abfd -> flags |= HAS_RELOC;
  section -> flags |= SEC_RELOC;

  if (amiga_per_section(section)->reloc_tail)
    amiga_per_section(section)->reloc_tail->next = reloc;
  else
    section->relocation = (struct reloc_cache_entry *) reloc;
  amiga_per_section(section)->reloc_tail = reloc;
  amiga_per_section(section)->reloc_tail->next = NULL;
  reloc->relent.address = offset;
  reloc->relent.addend = 0;
  reloc->relent.howto = howto;

  if (symbol==NULL) /* relative to section */
    reloc->symbol=(amiga_symbol_type *)(amiga_get_section_by_hunk_number(abfd,target_hunk)->symbol);
  else
    reloc->symbol = symbol;
  reloc->relent.sym_ptr_ptr=(asymbol **)(&(reloc->symbol));
  reloc->target_hunk = target_hunk;

  return true;
}

/* BFD doesn't currently allow multiple sections with the same
   name, so we try a little harder to get a unique name.  */
asection *
amiga_make_unique_section (abfd, name)
     bfd *abfd;
     CONST char *name;
{
  asection *section;


  section = bfd_make_section (abfd, name);
  if (!section)
    {
      int i = 1;
      char *new_name;

      new_name = bfd_alloc (abfd, strlen(name) + 3);

      /* We try to come up with an original name (since BFD
	 currently requires all sections to have different names).  */
      while (!section && (i<=99))
	{
	  sprintf (new_name, "%s_%u", name, i);
	  section = bfd_make_section (abfd, new_name);
	}

      if (!section)
	{
	  /* Complain about the given name.  */
	  bfd_set_error(bfd_error_bad_value);
	  return 0;
	}
    }


  return section;
}



#if DEBUG_AMIGA
#define DPRINTHUNK(x)   fprintf (stderr,"Processing %s hunk (0x%x)...",\
	  (x) == HUNK_UNIT ? "HUNK_UNIT" :\
	  (x) == HUNK_NAME ? "HUNK_NAME" :\
	  (x) == HUNK_DEBUG ? "HUNK_DEBUG" :\
	  (x) == HUNK_OVERLAY ? "HUNK_OVERLAY" :\
	  (x) == HUNK_BREAK ? "HUNK_BREAK" :\
	  (x) == HUNK_HEADER ? "HUNK_HEADER" :\
	  (x) == HUNK_CODE ? "HUNK_CODE" :\
	  (x) == HUNK_DATA ? "HUNK_DATA" :\
	  (x) == HUNK_BSS ? "HUNK_BSS" :\
	  (x) == HUNK_RELOC8 ? "HUNK_RELOC8" :\
	  (x) == HUNK_RELOC16 ? "HUNK_RELOC16" :\
	  (x) == HUNK_RELOC32 ? "HUNK_RELOC32" :\
	  (x) == HUNK_DREL8 ? "HUNK_DREL8" :\
	  (x) == HUNK_DREL16 ? "HUNK_DREL16" :\
	  (x) == HUNK_DREL32 ? "HUNK_DREL32" :\
	  (x) == HUNK_SYMBOL ? "HUNK_SYMBOL" :\
	  (x) == HUNK_EXT ? "HUNK_EXT" :\
	  (x) == HUNK_END ? "HUNK_END" :\
	  (x) == HUNK_LIB ? "HUNK_LIB" :\
	  (x) == HUNK_INDEX ? "HUNK_INDEX" :\
	  "*unknown*",(x))
#define DPRINTHUNKEND fprintf(stderr,"...done\n")
#else
#define DPRINTHUNK(x) 
#define DPRINTHUNKEND 
#endif

static boolean amiga_read_unit(bfd *abfd);
static boolean amiga_read_load(bfd *abfd);
static boolean amiga_handle_cdb_hunk(bfd *abfd, int ht,int hn,int ha,int hs);
static boolean amiga_handle_rest(bfd *abfd,asection *cs,boolean isload);

static boolean
amiga_digest_file (abfd)
     bfd *abfd;
{
  amiga_data_type *amiga_data = AMIGA_DATA(abfd);
  int hunk_type;


  hunk_type = HUNK_VALUE(GL(amiga_data->file_pointer++));


  switch (hunk_type)
    {
    case HUNK_UNIT:

      /* Read in the unit  */
      if (!amiga_read_unit(abfd)) /* Error */
	return(false);
      break;
      
    case HUNK_HEADER:
      /* This is a load file, well it should not be used with linking,
	 but it is possible to do so, so always allow it. */
      
      /* Read in the load file */
      if (!amiga_read_load(abfd)) /* Error */
	{
	  return(false);
	}
      break;
    } /* of switch hunk_type */



  /* If there is any trailing garbage, i.e. we are not at EOF, then
     complain and reject the file... */
  if (amiga_data->file_pointer!=amiga_data->file_end) /* ooops */
    {
      bfd_set_error(bfd_error_wrong_format);
      return(false);
    }

  /* Success, all went well... */
 return(true);

}/* of amiga_digest_file */


/* If we read over the end of file, error */
#define TOO_LARGE_P   if (amiga_data->file_pointer>=amiga_data->file_end)\
    {DPRINT(10,("Overflow of file pointer\n"));bfd_set_error(bfd_error_wrong_format);return false;}


/* Read in Unit file */
/* file pointer is located after the HUNK_UNIT LW */
static boolean amiga_read_unit(bfd *abfd)
{

  amiga_data_type *amiga_data = AMIGA_DATA(abfd);
  int hunk_type;
  int hunk_number=0;
  int hunk_attribute=0;

  /* The unit name, if given, has no meaning to us, 
     since we are not in an archive. 
     We could set the name of abfd to the name, but that 
     may cause confusion... 
     So just ignore the unit name 
     ST 121194 */
  amiga_data->file_pointer+=GL(amiga_data->file_pointer)+1;

  TOO_LARGE_P;


  while (true)
    {

      /* Now there may be CODE, DATA, BSS, SYMBOL, DEBUG, RELOC Hunks */
      
      hunk_type=HUNK_VALUE(GL(amiga_data->file_pointer)); /* Type of next hunk */
      hunk_attribute=HUNK_ATTRIBUTE(GL(amiga_data->file_pointer++));
      hunk_attribute=(hunk_attribute==HUNK_ATTR_CHIP)?
	MEMF_CHIP:(hunk_attribute==HUNK_ATTR_FAST)?MEMF_FAST:0;
      
    
      switch(hunk_type)
	{
	case HUNK_DEBUG:
	  /* We ignore debug hunks in UNITS, for now, because gnu's debug hunks
	     are only added by the linker to LOAD files.....
	     And anyhow, a.out format can keep much more information for units...
	     */
	  amiga_data->file_pointer+=1+GL(amiga_data->file_pointer);
	  TOO_LARGE_P;

	  break;

	case HUNK_NAME:
	case HUNK_CODE:
	case HUNK_DATA:
	case HUNK_BSS:
	  /* Handle this hunk, including relocs, etc.
	     The finishing HUNK_END is consumed by the routine
	     */
	  if (!amiga_handle_cdb_hunk(abfd,hunk_type,hunk_number++,hunk_attribute,-1))
	    return(false);

	  break;

	default:
	  /* Something very nasty happened:
	     Illegal Hunk occured....
	     */
	  bfd_set_error(bfd_error_wrong_format);
	  return(false);
	  break;
	  
	}/* Of switch hunk_type */


      /* Last HUNK ?*/
      if (amiga_data->file_pointer==amiga_data->file_end)
	return true;

      TOO_LARGE_P; /* Sanity check */
  
      /* Next hunk */
    }/* Of while */

}/* Of amiga_read_unit */


#define MAX_HUNKS 200 /* Max # of hunks. we can read in.. */

/* Read a load file */
static boolean amiga_read_load(bfd *abfd)
{
  amiga_data_type *amiga_data = AMIGA_DATA(abfd); 
  int hunk_sizes[MAX_HUNKS]; /* If there are more hunks than this, we are in trouble...*/
  int hunk_attributes[MAX_HUNKS];
  int hunk_number=0;
  int hunk_type;
  int max_hunk_number=0;
  int i,n;

  TOO_LARGE_P; /* Sanity */

  /* Read hunk lengths (and memory attributes...) */
  /* Read in each hunk */

  /* If there are resident libs: abort (obsolete feature) */
  if (GL(amiga_data->file_pointer++)!=0)
    {
      bfd_set_error(bfd_error_wrong_format);
      return(false);
    }

  TOO_LARGE_P;

  max_hunk_number=GL(amiga_data->file_pointer++);

  if (max_hunk_number>MAX_HUNKS) /* Ooops... too much hunks */
    {
      fprintf(stderr,"File %s has too many hunks (%d), aborting\n",abfd->filename,max_hunk_number);
      bfd_set_error(bfd_error_wrong_format);
      return(false);
    }

  /* Sanity */
  if ((max_hunk_number<1)||
      (amiga_data->file_pointer+max_hunk_number+2>=amiga_data->file_end))
    {
      bfd_set_error(bfd_error_wrong_format);
      return(false);
    }

  /* Num of root hunk must be 0 */
  if (GL(amiga_data->file_pointer++)!=0)
    {
      bfd_set_error(bfd_error_wrong_format);
      return(false);
    }

  /* Num of last hunk must be mhn-1 */
  if (GL(amiga_data->file_pointer++)!=max_hunk_number-1)
    {
      bfd_set_error(bfd_error_wrong_format);
      return(false);
    }

  /* Now, read in sizes and memory attributes */

  for (i=0;i<max_hunk_number;i++)
    {
      n=HUNK_VALUE(GL(amiga_data->file_pointer));
      hunk_sizes[i]=n<<2;
      n=HUNK_ATTRIBUTE(GL(amiga_data->file_pointer++));
      if (n==HUNK_ATTR_FOLLOWS) /* Attribute is in next long */
	hunk_attributes[i]=GL(amiga_data->file_pointer++);
      else
	hunk_attributes[i]= (n==HUNK_ATTR_CHIP)?MEMF_CHIP:
	  (n==HUNK_ATTR_FAST)?MEMF_FAST:0;
    }

  TOO_LARGE_P; /* Sanity */

  /* We can now read in all the hunks */

  for (hunk_number=0;hunk_number<max_hunk_number;hunk_number++)
    {
      hunk_type=HUNK_VALUE(GL(amiga_data->file_pointer++));


      /* This may be HUNK_NAME, CODE, BSS, DEBUG, DATA */

      switch(hunk_type)
	{
	case HUNK_NAME:
	case HUNK_CODE:
	case HUNK_DATA:
	case HUNK_BSS:
	case HUNK_DEBUG:
	  if (
	  !amiga_handle_cdb_hunk(abfd,hunk_type,hunk_number,hunk_attributes[hunk_number],
				hunk_sizes[hunk_number]))
	    {
	      bfd_set_error(bfd_error_wrong_format);
	      return(false);
	    }
	  break;

	default:
	  /* illegal hunk */
	  bfd_set_error(bfd_error_wrong_format);
	  return(false);
	  break;
	}/* Of switch */


    }

  /* dgv -- if the end is not reached at this point, there could be a
     HUNK_DEBUG here */

  if (amiga_data->file_pointer!=amiga_data->file_end) {
    hunk_type = HUNK_VALUE (GL(amiga_data->file_pointer++));
    if (hunk_type == HUNK_DEBUG) {
	  if (
	  !amiga_handle_cdb_hunk(abfd,hunk_type,0,0,0))
	    {
	      bfd_set_error(bfd_error_wrong_format);
	      return(false);
	    }
    }
  }

  if (amiga_data->file_pointer!=amiga_data->file_end)
    {
      bfd_set_error(bfd_error_wrong_format);
      return(false);
    }

  return(true);

}/* Of amiga_read_load */


/* Handle NAME, CODE, DATA, BSS, DEBUG Hunks */
static boolean 
amiga_handle_cdb_hunk(bfd *abfd, 
		      int hunk_type, 
		      int hunk_number,
		      int hunk_attribute, /* MEMF_CHIP, etc. */
		      int hunk_size)
/* If hunk_size==-1, then we are digesting a HUNK_UNIT */
{
  amiga_data_type *amiga_data = AMIGA_DATA(abfd);
  char *current_name=NULL;
  int len;
  asection *current_section=NULL;

  if (hunk_type==HUNK_NAME) /* get name */
    {
      len=HUNK_VALUE(GL(amiga_data->file_pointer))<<2; /* Length */
      /* DIRTY: We move the name 4 bytes towards the beginning of the file,
	        overwriting the length,
	        and end it with a '\0' */
      strncpy((char *)amiga_data->file_pointer,(char *)(amiga_data->file_pointer+1),len);
      *(((char *)amiga_data->file_pointer)+len)='\0';
      current_name=(char *)amiga_data->file_pointer;
      amiga_data->file_pointer+=1+(len>>2); /* Advance */

      TOO_LARGE_P;

      hunk_type=HUNK_VALUE(GL(amiga_data->file_pointer++)); /* Next hunk */
      if (hunk_size==-1) /* In a unit ==> Get attribute from file */
	{
	  hunk_attribute=HUNK_ATTRIBUTE(GL(amiga_data->file_pointer-1));
	  hunk_attribute=(hunk_attribute==HUNK_ATTR_CHIP)?MEMF_CHIP:
	             (hunk_attribute==HUNK_ATTR_FAST)?MEMF_FAST:0;
	}
    }
  else /* Set curent name to something appropriate */
    current_name=(hunk_type==HUNK_CODE)?".text":
                 (hunk_type==HUNK_BSS)?".bss":".data";
      


  /* file_pointer is now at  hunk_type */

  switch (hunk_type)
    {
    case HUNK_CODE:
      /* Handle code hunk */

      len=HUNK_VALUE(GL(amiga_data->file_pointer++))<<2; /* Length of section */
      if (len>hunk_size) /* len may be shorter than hunk_size... */ 
	if (hunk_size==-1)
	  hunk_size=len;
	else
	  {
	    bfd_set_error(bfd_error_wrong_format);
	    return false;
	  }

      /* Make new section */
      current_section=amiga_make_unique_section(abfd,current_name);
      if (!current_section)
	return false;

      current_section->filepos=(char *)(amiga_data->file_pointer)-(char *)amiga_data->first_byte;
      current_section->_raw_size=hunk_size;
      current_section->_cooked_size=hunk_size;
      current_section->target_index=hunk_number;
      bfd_set_section_flags(abfd,current_section, 
			    SEC_ALLOC | SEC_LOAD | SEC_CODE | SEC_HAS_CONTENTS);

      amiga_per_section(current_section)->real_length=len; /* real size */
      amiga_per_section(current_section)->attribute=hunk_attribute;

      amiga_data->file_pointer+=(len>>2); /* next hunk */

      if (!amiga_handle_rest(abfd,current_section,hunk_size!=-1))
	return false;
      
      break;

    case HUNK_DATA:
      /* Handle data hunk */

      len=HUNK_VALUE(GL(amiga_data->file_pointer++))<<2; /* Length of section */
      if (len>hunk_size) /* len may be shorter than hunk_size... */ 
	if (hunk_size==-1)
	  hunk_size=len;
	else
	  {
	    bfd_set_error(bfd_error_wrong_format);
	    return false;
	  }

      current_section=amiga_make_unique_section(abfd,current_name);
      if (!current_section)
	return false;

      current_section->filepos=(char *)amiga_data->file_pointer-(char *)amiga_data->first_byte;
      current_section->_raw_size=hunk_size;
      current_section->_cooked_size=hunk_size;
      current_section->target_index=hunk_number;
      bfd_set_section_flags(abfd,current_section, 
			    SEC_ALLOC | SEC_LOAD | SEC_DATA | SEC_HAS_CONTENTS);

      amiga_per_section(current_section)->real_length=len; /* real size */
      amiga_per_section(current_section)->attribute=hunk_attribute;

      amiga_data->file_pointer+=(len>>2); /* next hunk */
      if (!amiga_handle_rest(abfd,current_section,hunk_size!=-1))
	return false;
      break;

    case HUNK_BSS:
      /* Handle bss hunk */
      
      len=HUNK_VALUE(GL(amiga_data->file_pointer++))<<2; /* Length of section */
      if (len>hunk_size) /* len may be shorter than hunk_size... */ 
	if (hunk_size==-1)
	  hunk_size=len;
	else
	  {
	    bfd_set_error(bfd_error_wrong_format);
	    return false;
	  }

      current_section=amiga_make_unique_section(abfd,current_name);
      if (!current_section)
	return false;

      current_section->filepos=(file_ptr)-1;
      current_section->_raw_size=hunk_size;
      current_section->_cooked_size=hunk_size;
      current_section->target_index=hunk_number;

      bfd_set_section_flags(abfd,current_section, SEC_ALLOC );

      amiga_per_section(current_section)->real_length=len; /* real size */
      amiga_per_section(current_section)->attribute=hunk_attribute;

      if (!amiga_handle_rest(abfd,current_section,hunk_size!=-1))
	return false;
      break;

    case HUNK_DEBUG:
      if (hunk_size==-1) /* We are digesting a unit ==> ignore debug hunks */
	{
	  amiga_data->file_pointer+=1+GL(amiga_data->file_pointer);
	  return(true);
	}
      /* We are digesting a load file, so check for gnu debug hunk, else ignore it */
      /* format of gnu debug hunk is:
          HUNK_DEBUG
              N
            0413.L    Magic number
	  symtabsize
	  strtabsize
	  symtabdata  [length=symtabsize]
	  strtabdata  [length=strtabsize]
          [pad bytes]
	  */

      if (GL(amiga_data->file_pointer+1)==0413) /* GNU DEBUG HUNK */
	{
	  /*FIXME: we should add the symbols in the debug hunk to symtab... */
	  
	  amiga_data->symtab_size=GL(amiga_data->file_pointer+2);
	  amiga_data->stringtab_size=GL(amiga_data->file_pointer+3);
	  adata(abfd).sym_filepos=
	    (file_ptr)((char *)(amiga_data->file_pointer +4)-(char *)amiga_data->first_byte);
	  adata(abfd).str_filepos=adata(abfd).sym_filepos+amiga_data->symtab_size;
	  amiga_data->file_pointer+=1+GL(amiga_data->file_pointer);
	}
      else /* NOT GNU DEBUG HUNK... */
	{
	  /* ignore it */
	  amiga_data->file_pointer+=1+GL(amiga_data->file_pointer);
	}

      /* there has to be an HUNK_END here... */
      if (GL(amiga_data->file_pointer++)!=HUNK_END)
	{
	  bfd_set_error(bfd_error_wrong_format);
	  return(false);
	}
      break;

    default:
      bfd_set_error(bfd_error_wrong_format);
      return false;
      break;
    }/* of switch hunk_type */


  return(true);

}/* Of amiga_handle_cdb_hunk */


/* Handle rest of a hunk 
   I.e.: Relocs, EXT, SYMBOLS... */
static boolean amiga_handle_rest(bfd *abfd, asection *current_section, boolean isload)
{
  amiga_data_type *amiga_data = AMIGA_DATA(abfd);
  int hunk_type;
  int type;
  int len;
  int n;
  unsigned long **p;
  struct amiga_raw_symbol *sp=NULL;


  while (1)/* loop */
    {
      hunk_type=GL(amiga_data->file_pointer++);


      switch (hunk_type)
	{
	case HUNK_END: /* Reached end */
	  return(true); /* Done */
	  break;

	  /* Relocs.... */
	case HUNK_RELOC8:
	case HUNK_RELOC16:
	case HUNK_RELOC32:
	case HUNK_DREL8:
	case HUNK_DREL16:
	case HUNK_DREL32:
	  /* We leave them alone, but attach them to the section,
	     so we can read them in later.....*/
	  
	  /* But we already set the flags HAS_RELOC, SEC_RELOC */
	  /* We also maintain the reloc count */
	  /* This is simply done by adding all addresses of relocs to an array in the section */
	  p=amiga_per_section(current_section)->raw_relocs;
	  if (amiga_per_section(current_section)->max_raw_relocs<=
	      amiga_per_section(current_section)->num_raw_relocs) /* allocate more space */
	    {
	      if (p==NULL)
		{
		  p=(unsigned long **)xmalloc(sizeof(unsigned long *)*10); /* initial */
		  amiga_per_section(current_section)->max_raw_relocs=10;
		  amiga_per_section(current_section)->num_raw_relocs=0;
		}
	      else /* More space */
		{
		  p=(unsigned long **)xrealloc((void *)p,
			     sizeof(unsigned long *)*(amiga_per_section(current_section)->max_raw_relocs+30));
		  amiga_per_section(current_section)->max_raw_relocs+=30;
		}

	      if (p==NULL)
		{
		  bfd_set_error(bfd_error_no_memory);
		  return(false);
		}
	      amiga_per_section(current_section)->raw_relocs=p;
	    }
	  p[amiga_per_section(current_section)->num_raw_relocs++]=amiga_data->file_pointer-1;
	  
	  abfd->flags|=HAS_RELOC;
	  current_section->flags|=SEC_RELOC;

	  /* Advance file_pointer */
	  while ((n=GL(amiga_data->file_pointer++)))
	    {
	      amiga_data->file_pointer+=1+n;
	      current_section->reloc_count+=n;
	    }

	  break;

	  /* Symbol definition */
	case HUNK_SYMBOL:
	  /* In a unit, we ignore these, since all symbol information comes with HUNK_EXT,
	     in a load file, these are added */
	  if (!isload)
	    {
	      int foo;
	      
	      while ((foo=GL(amiga_data->file_pointer))!=0)
		amiga_data->file_pointer+=foo+2;
	      amiga_data->file_pointer++;
	      break;
	    }
	  /* We add these, by falling through... */

	case HUNK_EXT:
	  /* We leave these alone, until they are requested by the user */
	  sp=amiga_per_section(current_section)->last;
	  
	  if (sp==NULL) /* First one added */
	    {
	      amiga_per_section(current_section)->last=amiga_per_section(current_section)->first=
		(struct amiga_raw_symbol *)(amiga_data->file_pointer-1);
	      *(amiga_data->file_pointer-1)=0;
	    }
	  else
	    {
	      sp->next=(struct amiga_raw_symbol *)(amiga_data->file_pointer-1);
	      *(amiga_data->file_pointer-1)=0;
	      amiga_per_section(current_section)->last=sp->next;
	    }
	  /* skip these */

	  while ((n=GL(amiga_data->file_pointer)))
	    {
	      amiga_data->file_pointer++;

	      type=(n>>24)&0xff;
	      len=n&0xffffff;
	      
	      amiga_data->file_pointer+=len; /* skip name */

	      switch(type)
		{
		case EXT_SYMB: /* Symbol hunks are relative to hunk start... */
		case EXT_DEF: /* def relative to hunk */
		case EXT_ABS: /* def absolute */
		  abfd->flags|=HAS_SYMS; /* We have symbols */
		  abfd->symcount++;
		  amiga_data->file_pointer++;
		  break;

		case EXT_REF8: /* 8 bit ref */
		case EXT_REF16: /* 16 bit ref */
		case EXT_REF32: /* 32 bit ref */
		case EXT_DEXT8: /* 8 bit base relative ref */
		case EXT_DEXT16: /* 16 bit " "*/
		case EXT_DEXT32: /* 32 bit " " */
		  abfd->flags|=HAS_SYMS;
		  n=GL(amiga_data->file_pointer);
		  if (n)
		    {
		      abfd->flags|=HAS_RELOC;
		      current_section->flags|=SEC_RELOC;
		    }
		  current_section->reloc_count+=n;
		  amiga_data->file_pointer+=1+n;
		  break;
		  
		case EXT_COMMON: /* Common ref/def */
		  abfd->flags|=HAS_SYMS;
		  abfd->symcount++;
		  n=GL(amiga_data->file_pointer+1);
		  if (n)
		    {
		      abfd->flags|=HAS_RELOC;
		      current_section->flags|=SEC_RELOC;
		    }
		      
		  current_section->reloc_count+=n;
		  amiga_data->file_pointer+=2+n;
		  break;
		  
		default: /* error */
		  
		  bfd_set_error(bfd_error_wrong_format);
		  return false;
		  break;
		}/* of switch type */
	      
	    }/* Of while */
	  amiga_data->file_pointer++;

	  break;


	default: /* error */

	  bfd_set_error(bfd_error_wrong_format);
	  return(false);
	  break;
	}/* Of switch */

    }/* of while */

  return(true);

}/* of amiga_handle_rest*/



static boolean
amiga_mkobject (abfd)
     bfd *abfd;
{
  struct amiga_data_struct *rawptr;

  rawptr = (struct amiga_data_struct *) bfd_zalloc (abfd, sizeof (struct amiga_data_struct));
  abfd->tdata.amiga_data = rawptr;

  return true;
}


static boolean amiga_write_section_contents(bfd *, asection *);
static boolean amiga_write_symbols(bfd *, asection *);

/* used with base relative linking */
extern int amiga_base_relative;

/* Write out the contents of a bfd */
static boolean
amiga_write_object_contents (abfd)
     bfd *abfd;
{
  struct amiga_data_struct *amiga_data=AMIGA_DATA(abfd);
  sec_ptr p;
  unsigned long n[5];
  int i;
  char b[3];

  /* Distinguish UNITS, LOAD Files
    Write out hunks+relocs+HUNK_EXT+HUNK_DEBUG (GNU format)*/
  DPRINT(5,("Entering write_object_conts\n"));

  abfd->output_has_begun=true; /* Output has begun */


  /* Distinguish Load files and Unit files */
  if (amiga_data->IsLoadFile)
    {
      DPRINT(5,("Writing Load file\n"));

      /* Write out load file header */
      n[0]=HUNK_HEADER;
      n[1]=0;
      n[2]=abfd->section_count;
      /* If amiga_base_relative is set, we write out one hunk less */
      if (amiga_base_relative)
	{
	  n[2]=abfd->section_count-1;
	  BFD_ASSERT(abfd->section_count==3); /* Or we are in big trouble */
	}

      n[3]=0;
      n[4]=n[2]-1;
      if (bfd_write((PTR)(n),sizeof(unsigned long),5,abfd)!=5*sizeof(unsigned long))
	return false;
      /* Write out sizes and memory specifiers... */
      /* We have to traverse the section list again, bad but no other way... */
      for (p=abfd->sections;p!=NULL;p=p->next)
	{
	  if (amiga_base_relative && (strcmp(p->name,".bss")==0)) /* skip */
	    continue;

	  /* Get  size for header*/
	  if (amiga_per_section(p)->real_length!=0)
	    n[0]=(amiga_per_section(p)->real_length)>>2;
	  else
	    n[0]=(p->_raw_size)>>2;
	  i=amiga_per_section(p)->attribute;
	  switch (i)
	    {
	    case MEMF_CHIP:
	      n[0]|=0x40000000;
	      i=1;
	      break;
	    case MEMF_FAST:
	      n[0]|=0x80000000;
	      i=1;
	      break;
	    case 0: /* nothing*/
	      i=1;
	      break;
	    default: /* Special one */
	      n[0]|=0xc0000000;
	      n[1]=i;
	      i=2;
	      break;
	    }/* Of switch */

	  if (bfd_write((PTR)(n),sizeof(unsigned long),i,abfd)!=i*sizeof(unsigned long))
	return false;
	}/* Of for */
    }
  else
    {/* Unit , no base-relative linking here.... */
      /* Write out unit header */
      DPRINT(5,("Writing Unit\n"));

      n[0]=HUNK_UNIT;
      if (bfd_write((PTR)(n),sizeof(unsigned long),1,abfd)!=sizeof(unsigned long))
	return false;

      i=(strlen(abfd->filename)+3)>>2; /* longword size of name */
      if (bfd_write((PTR)(&i),sizeof(unsigned long),1,abfd)!=sizeof(unsigned long))
	return false;
      if (bfd_write((PTR)(abfd->filename),sizeof(char),strlen(abfd->filename),abfd)!=strlen(abfd->filename))
	return false;
      /* Pad to lw boundary */
      b[0]=b[1]=b[2]='\0';
      i=(i<<2)-strlen(abfd->filename);
      if (i)
	if (bfd_write((PTR)(b),sizeof(char),i,abfd)!=i)
	  return false;
      /* That's all... for units :-)*/
    }

  /* Write out every section */
  for (p=abfd->sections;p!=NULL;p=p->next)
    {
      if (!amiga_write_section_contents(abfd,p)) /* Write out section data + relocs */
	return false;

      if (!amiga_write_symbols(abfd,p)) /* Write out symbols, incl HUNK_END */
	return false;

    }/* of for sections */

  /* Write out debug hunk, if requested */
  if (amiga_data->IsLoadFile && write_debug_hunk)
    {
      struct external_nlist {
	bfd_byte e_strx[BYTES_IN_WORD];       /* index into string table of name */
	bfd_byte e_type[1];                   /* type of symbol */
	bfd_byte e_other[1];                  /* misc info (usually empty) */
	bfd_byte e_desc[2];                   /* description field */
	bfd_byte e_value[BYTES_IN_WORD];      /* value of symbol */
      };

      extern boolean
	translate_to_native_sym_flags(bfd *, asymbol *, struct external_nlist *);

      /* We have to convert all the symbols in abfd to a.out style.... */
      struct external_nlist *data,*p; /* Were to put the data */
      char *strtab; /* string table */
      int str_size;
      int max_str_size;
      asymbol *sym;
      asection *s;

      if (abfd->symcount)
	{
	  if ((data=(struct external_nlist *)xmalloc(sizeof(struct external_nlist)*abfd->symcount))==NULL)
	    {
	    hunk_no_mem:
	      fprintf(stderr,"No memory for debug hunk, hunk not written\n");
	      return true;
	    }

	  str_size=0;
	  max_str_size=400; /* initial value */
	  if ((strtab=(char *)xmalloc(max_str_size))==NULL)
	    {
	      free((void *)data);
	      goto hunk_no_mem;
	    }
	  /* Now, set the .text, .data and .bss fields in the tdata struct (because
	     translate_to_native_sym_flags needs them... */
	  for (i=0,s=abfd->sections;s!=NULL;s=s->next)
	    if (strcmp(s->name,".text")==0)
	      {
		i|=1;
		adata(abfd).textsec=s;
	      }
	  else if (strcmp(s->name,".data")==0)
	    {
	      i|=2;
	      adata(abfd).datasec=s;
	    }
	  else if (strcmp(s->name,".bss")==0)
	    {
	      i|=4;
	      adata(abfd).bsssec=s;
	    }

	  if (i!=7) /* One section missing... */
	    {
free_all:
	      free(strtab);
	      free(data);
	      goto hunk_no_mem;
	    }


	  for (p=data,i=0;i<abfd->symcount;i++) /* Translate every symbol */
	    {
	      sym=abfd->outsymbols[i];
	      if (sym!=NULL) /* NULL entries have been written already.... */
		{
		  char *q;

		  if (!translate_to_native_sym_flags(abfd,sym,p))
		    goto free_all;
		  /* Now, add the name of sym to the stringtab */
		  while (max_str_size <= strlen(sym->name) + 1 + str_size)
		    { /* Realloc space */
		      if ((strtab=(char *)xrealloc(strtab, max_str_size+1024))==NULL)
			{
			  free(data);
			  goto hunk_no_mem;
			}
		      max_str_size+=1024;
		    }
		  PUT_WORD(abfd,str_size,&(p->e_strx[0])); /* Store index */
		  for (q=sym->name;strtab[str_size++]=*q++;)
		    ;
		  p++;
		}
	    }
		  
		  
	
	  /* Write out HUNK_DEBUG, size, 0413.... */
	  n[0]=HUNK_DEBUG;
	  n[1]=3+(((p-data)*sizeof(struct nlist)+str_size+3)>>2);
	  n[2]=0413L; /* Magic number */
	  n[3]=(p-data)*sizeof(struct nlist);
	  n[4]=str_size;
	  if (bfd_write((PTR)(n),sizeof(unsigned long),5,abfd)!=5*sizeof(unsigned long))
	    return false;

	  /* Write out symbols */
	  if (bfd_write((PTR)(data),sizeof(struct nlist),(p-data),abfd)!=(p-data)*sizeof(struct nlist))
	    return false;
	  /* Write string tab */
	  if (bfd_write((PTR)(strtab),sizeof(char),(str_size+3)&(~3),abfd)!=(str_size+3)&(~3))
	    return false;
	}/* Of if abfd->symcount */
    }/* Of write out debug hunk */

  return true;
}


/* Write out section contents, including relocs */
static boolean
amiga_write_section_contents(bfd *abfd, asection * section)
{
  unsigned long n[2];
  int i,j;
  unsigned int k;
  char b[3];
  struct reloc_cache_entry *r;
  asection *osection,*sec,*insection;
  asymbol *sym_p;
  unsigned long *reloc_data=NULL;
  int reloc_count=0;
  unsigned long *reloc_out=NULL;
  unsigned long reloc_types[6]={HUNK_RELOC32, HUNK_RELOC16, HUNK_RELOC8,
				  HUNK_DREL32,HUNK_DREL16, HUNK_DREL8};
  int reloc_counts[6]={0,0,0,0,0,0};
  int out_size=0;
  int max_hunk=-1;
  int size=0;
  char *c_p;
  unsigned char *values;

  DPRINT(5,("Entered Write-section-conts\n"));

  /* If we are base-relative linking and the section is .bss and abfd is a load file, then return */
  if (AMIGA_DATA(abfd)->IsLoadFile && amiga_base_relative && (strcmp(section->name,".bss")==0))
    return true; /* Nothing 2 do */

  /* WRITE out HUNK_NAME + section name */
  n[0]=HUNK_NAME;
  j=strlen(section->name);
  i=(j+3)>>2; /* LW len of name */
  n[1]=i;
  if (bfd_write((PTR)(n),sizeof(unsigned long),2,abfd)!=2*sizeof(unsigned long))
    return false;
  if (bfd_write((PTR)(section->name),sizeof(char),j,abfd)!=j)
    return false;
  b[0]=b[1]=b[2]='\0';
  if ((i<<2)-j!=0) /* Pad */
    if(bfd_write((PTR)(b),sizeof(char),(i<<2)-j,abfd)!=((i<<2)-j))
      return false;

  /* Depending on the type of the section, write out HUNK_{CODE|DATA|BSS} */
  if (section->flags&SEC_CODE) /* Code section */
    n[0]=HUNK_CODE;
  else if (section->flags&SEC_DATA) /* data section */
    n[0]=HUNK_DATA;
  else if (section->flags&SEC_ALLOC) /* BSS */
    n[0]=HUNK_BSS;
  else /* Error */
    {
      bfd_set_error(bfd_error_nonrepresentable_section);
      return(false);
    }

  DPRINT(10,("Section type is %lx\n",n[0]));

  /* Get attribute for section */
  switch (amiga_per_section(section)->attribute)
    {
    case MEMF_CHIP:
      n[0]|=HUNKF_CHIP;
      break;
    case MEMF_FAST:
      n[0]|=HUNKF_FAST;
      break;
    case 0:
      break;
    default: /* error , can't represent this */
      bfd_set_error(bfd_error_nonrepresentable_section);
      return(false);
      break;
    }/* Of switch */

  DPRINT(10,("Section attribute is %lx\n",n[0]));

  /* Get real size in n[1], this may be shorter than the size in the header */
  n[1]=section->_cooked_size>>2;
  if(n[1]==0)
    n[1]=section->_raw_size>>2;
  if (bfd_write((PTR)(n),sizeof(unsigned long),2,abfd)!=2*sizeof(unsigned long))
      return false;

  DPRINT(5,("Wrote code and size=%lx\n",n[1]));

  /* If a BSS hunk, we're done, else write out section contents */
  if (HUNK_VALUE(n[0])==HUNK_BSS)
    return true;

  DPRINT(5,("Non bss hunk...\n"));

  /* Traverse through the relocs, sample them in reloc_data, adjust section data to get 0 addend
     Then compactify reloc_data 
     Set the entry in the section for the reloc to NULL */

  /* Get memory for relocs, we use upper bound */
  if ((reloc_data=(unsigned long *)xmalloc(5*sizeof(unsigned long)*section->reloc_count))==NULL)
    {
      bfd_set_error(bfd_error_no_memory);
      return(false);
    }

  BFD_ASSERT((section->flags&SEC_IN_MEMORY)!=0);

  DPRINT(5,("Section has %d relocs\n",section->reloc_count));

  for (i=0;i<section->reloc_count;i++)
    {
      r=section->orelocation[i];

      DPRINT(5,("Doing reloc %d=%lx(%d)\n",i,r,r?r->howto->type:-1));

      sym_p=*(r->sym_ptr_ptr); /* The symbol for this section */

      insection=sym_p->section;

      DPRINT(5,("Sec for reloc is %lx(%s)\n",insection,insection->name));
      DPRINT(5,("Symbol for this reloc is %lx(%s)\n",sym_p, sym_p->name));
      /* Is reloc relative to a special section ? */
      if ((insection==bfd_abs_section_ptr)||(insection==bfd_com_section_ptr)||
	  (insection==bfd_und_section_ptr)||(insection==bfd_ind_section_ptr))
	continue; /* Nothing to do, since this translates to HUNK_EXT */
      
      r->addend+=sym_p->value; /* Add offset of symbol from section start */
      /* Address of reloc has been unchanged since original reloc, or has been adjusted by 
	 get_relocated_section_contents */
      /* For relocs, the vma of the target section is in the data, the addend is
	 -vma of that section =>No need to add vma*/
      /* Add in offset */
      r->addend+=insection->output_offset;
      osection=insection->output_section; /* target section */

      /* Determine which hunk to write, and index of target */
      reloc_data[5*reloc_count+1]=1; /* 1 reloc ==> Waste of space */
      for (j=0,sec=abfd->sections;sec!=NULL;sec=sec->next,j++)
	{
	  if (sec==osection)
	    break;
	}
   
      BFD_ASSERT(sec!=NULL);
      
      if (j>max_hunk) /* Maximum number used */
	max_hunk=j;

      reloc_data[5*reloc_count+2]=j;
      reloc_data[5*reloc_count+3]=r->address;
      reloc_data[5*reloc_count+4]=0;

      switch(r->howto->type) /* FIXME: Is this sufficient to distinguish them ?*/
	{
	  /* AMIGA specific */
	case HUNK_RELOC8:
	case HUNK_RELOC16:
	case HUNK_RELOC32:
	case HUNK_DREL8:
	case HUNK_DREL16:
	case HUNK_DREL32:
	  reloc_data[5*reloc_count+1]=r->howto->type;
	  if (r->howto->type>=HUNK_DREL32)
	    {
	      size=2-(r->howto->type-HUNK_DREL32);
	      reloc_counts[3+r->howto->type-HUNK_DREL32]++;
	    }
	  else
	    {
	      size=2-(r->howto->type-HUNK_RELOC32); /* 0:8bit, 1:16bit, 2:32 bit */
	      reloc_counts[r->howto->type-HUNK_RELOC32]++;
	    }

	  break;

	  /* Now, these may occur, if a.out was used as input */
	case 0: /* 8 bit ref */
	  reloc_data[5*reloc_count]=HUNK_RELOC8;
	  reloc_counts[2]++;
	  size=0;
	  break;
	case 1: /* 16 bit relative */
	  reloc_data[5*reloc_count]=HUNK_RELOC16;
	  reloc_counts[1]++;
	  size=1;
	  break;
	case 2: /* 32 bit relative */
	  reloc_data[5*reloc_count]=HUNK_RELOC32;
	  reloc_counts[0]++;
	  size=2;
	  break;

	case 9: /* 16 bit base rel */
	  reloc_data[5*reloc_count+1]=HUNK_DREL16;
	  reloc_counts[4]++;
	  size=1;
	  break;

	case 10: /* 32 bit baserel */
	  reloc_data[5*reloc_count+1]=HUNK_DREL32;
	  reloc_counts[3]++;
	  size=2;
	  break;

	  /* FIXME: There are other (pc relative) displacements left */
	default: /* Error, can't represent this */
	  bfd_set_error(bfd_error_nonrepresentable_section);
	  free((void *)reloc_data);
	  return false;
	  break;
	}/* Of switch */

      section->orelocation[i]=NULL;

      c_p=((char*)(section->contents))+r->address;
      DPRINT(5,("reloc address=%lx,addend=%lx\n",r->address,r->addend));

      /* There is no error checking with these.. */
      values=(unsigned char *)c_p;
      switch (size)
	{
	case 0: /* adjust byte */
	  j=(int)(*c_p);
	  DPRINT(5,("content(b):%x\n",j));
	  j+=r->addend;
	  *c_p=(signed char)j;
	  break;
	case 1: /* Adjust word */
	  k=values[0]|(values[1]<<8);
	  j=(int)k;
	  DPRINT(5,("content(w):%x\n",j));
	  j+=r->addend;
	  values[0]=j&0xff;
	  values[1]=(j&0xff00)>>8;
	  break;
	case 2: /* adjust long */
	  k=values[3]|(values[2]<<8)|(values[1]<<16)|(values[0]<<24);
	  j=k;
	  DPRINT(5,("content(l):%x\n",j));
	  DPRINT(5,("[3,2,1,0]=%x,%x,%x,%x\n",values[3],values[2],values[1],values[0]));
	  j+=r->addend;
	  values[3]=j&0xff;
	  values[2]=(j&0xff00)>>8;
	  values[1]=(j&0xff0000)>>16;
	  values[0]=(j&0xff000000)>>24;
	  break;
	}/* of switch */
      r->addend=0;
      DPRINT(5,("Did adjusting\n"));

      reloc_count++;
    }/* of for i */

  DPRINT(5,("Did all relocs\n"));

  /* We applied all the relocs, as far as possible to obtain 0 addend fields */
  /* Write the section contents */
  if(bfd_write((PTR)(section->contents),sizeof(char),section->_raw_size,abfd)!=section->_raw_size)
      return false;

  DPRINT(10,("Wrote contents, writing relocs now\n"));

  /* We gathered all the relocs, now compactify them */
  if (!reloc_count) /* No reloc found ==> We are done */
    {
      free((void *)reloc_data);
      return true;
    }

  /* There are some relocs.... */
  /* Get mem for output array */
  if ((reloc_out=(unsigned long *)xmalloc(5*sizeof(unsigned long)*reloc_count))==NULL)
    {
      bfd_set_error(bfd_error_no_memory);
      free((void *)reloc_data);
      return false;
    }

  /* Sample every reloc type */
  for (i=0,out_size=0;i<6;i++)
    if (reloc_counts[i]!=0) /* there are some relocs of these */
      {
	int k,num;

	reloc_out[out_size++]=reloc_types[i];
	/* sample all of these, for every hunk */
	for (j=0;j<=max_hunk;j++) /* For every hunk */
	  { /* Walk through reloc_data, */
	    num=0; /* # relocs to hunk j */
	    for (k=0;k<reloc_count;k++)
	      if (reloc_data[5*k+2]==j) /* Reloc to this hunk */
		{
		  if (!(num++)) /* one more and was first one */
		    {
		      reloc_out[out_size+1]=j; /* write hunk number */
		    }
		  reloc_out[out_size+1+num]=reloc_data[5*k+3]; /* offset */
		}
	    if (num) /* There was at least one ref to this hunk */
	      {
		reloc_out[out_size]=num; /* number of refs */
		out_size+=2+num; /* adjust */
	      }
	  } /* sampled all hunks */
	reloc_out[out_size++]=0; /* End of this reloc type */
      }/* Of if there is one reloc */


  DPRINT(10,("Sampled relocs, write them now\n"));

  /* Write out the relocs */

  if (bfd_write((PTR)reloc_out,sizeof(unsigned long),out_size,abfd)!=sizeof(unsigned long)*out_size)
    {
      free((void *)reloc_out);
      free((void *)reloc_data);
      return (false);
    }

  free ((void *)reloc_out);
  free ((void *)reloc_data);

  DPRINT(5,("Leaving write_section...\n"));

  return true;

}


/* Write out symbol information, including HUNK_EXT, DEFS, ABS */
/* In the case, we were linking base relative, the symbols of the .bss hunk have been converted already
to belong to the .data hunk */
static boolean amiga_write_symbols(bfd *abfd, asection *section)
{

  int i,j;
  struct reloc_cache_entry *r;
  asection *osection;
  asymbol *sym_p;
  char b[3]="\0\0\0";
  unsigned long n[3];
  int symbol_count;
  unsigned int symbol_header;
  unsigned long type;
  int len;

  /* If base rel linking and section is .bss ==> exit */
  if (amiga_base_relative && (strcmp(section->name,".bss")==0))
    return true;

  if (section->reloc_count==0 && abfd->symcount==0)
    {/* Write HUNK_END */
    alldone:
      DPRINT(5,("Leaving write_symbols\n"));
      n[0]=HUNK_END;
      return(bfd_write((PTR)n,sizeof(unsigned long),1,abfd)==sizeof(unsigned long));
    }

  symbol_count=0;
  symbol_header=HUNK_EXT;

  /* If this is Loadfile, then do not write HUNK_EXT, but rather HUNK_SYMB*/

 /* Write out all the symbol definitions, then HUNK_END 

     Now, first traverse the relocs, all entries that are non NULL
     have to be taken into account */
  /* Determine the type of HUNK_EXT to issue and build a single HUNK_EXT subtype */


  /*FIXME: We write out many HUNK_EXT's entries for references to the same symbol.. */
  for (i=0;i<section->reloc_count;i++)
    {
      r=section->orelocation[i];

      if (r==NULL) /* Empty entry */
	continue;

      sym_p=*(r->sym_ptr_ptr); /* The symbol for this section */

      osection=sym_p->section; /* The section the symbol belongs to */
      /* this section MUST be a special section */

      DPRINT(5,("Symbol is %s, section is %lx(%s)\n",sym_p->name,osection,osection->name));

      if (osection!=bfd_com_section_ptr) /* Not common symbol */
	{
	  DPRINT(5,("Non common ref\n"));
	  /* Add a reference to this HUNK */
	  if ((symbol_count++)==0) /* First write out the HUNK_EXT */
	    {
	      j=HUNK_EXT;
	      if(bfd_write((PTR)(&j),sizeof(int),1,abfd)!=sizeof(int))
		return false;
	    }

	  /* Determine type of ref */
	  switch(r->howto->type) /* FIXME: Is this sufficient to distinguish them ?*/
	    {
	      /* AMIGA specific */
	    case 0:
	    case HUNK_RELOC8:
	      type=EXT_REF8;
	      break;

	    case 1:
	    case HUNK_RELOC16:
	      type=EXT_REF16;
	      break;

	    case 2:
	    case HUNK_RELOC32:
	      type=EXT_REF32;
	      break;
	    case HUNK_DREL8:
	      type=EXT_DEXT8;
	      break;

	    case 9:
	    case HUNK_DREL16:
	      type=EXT_DEXT16;
	      break;

	    case 10:
	    case HUNK_DREL32:
	        type=EXT_DEXT32;
	      break;
	      
	      /* FIXME: There are other (pc relative) displacements left */
	    default: /* Error, can't represent this */
	      bfd_set_error(bfd_error_nonrepresentable_section);
	      return false;
	      break;
	    }/* Of switch */
	  DPRINT(5,("Type is %x\n",type));

	  type<<=24;
	  type&=0xff000000;

	  j=strlen(sym_p->name);
	  len=(j+3)>>2; /* LW Len of name */
	  
	  type|=(len & 0xffffff);
	  if (bfd_write((PTR)(&type),sizeof(unsigned long),1,abfd)!=sizeof(unsigned long))
	    return false;

	  if (bfd_write((PTR)(sym_p->name),sizeof(char),j,abfd)!=j)
	    return false;

	  if ((len<<2)-j!=0) /* Pad name */
	    if (bfd_write((PTR)(b),sizeof(char),(len<<2)-j,abfd)!=((len<<2)-j))
	      return false;

	  n[0]=1; /* 1 ref at address... */
	  n[1]=r->address;

	  if (bfd_write((PTR)(n),sizeof(unsigned long),2,abfd)!=2*sizeof(long))
	    return false;
	  
	  continue; /* Next relocation */
	}/* Of is ref to undefined or abs symbol */

      
      if (osection==bfd_com_section_ptr) /* ref to common symbol */
	{
	  DPRINT(5,("Common ref\n"));

	  /* If the reference is NOT 32 bit wide absolute , then issue warning */
	  if ((r->howto->type!=2)&&(r->howto->type!=HUNK_RELOC32))
	    fprintf(stderr,"Warning: Non 32 bit wide reference to common symbol %s\n",
		    sym_p->name);

	  if ((symbol_count++)==0) /* First write out the HUNK_EXT */
	    {
	      j=HUNK_EXT;
	      if(bfd_write((PTR)(&j),sizeof(int),1,abfd)!=sizeof(int))
		return false;
	    }

	  type=(EXT_COMMON<<24)&0xff000000;

	  j=strlen(sym_p->name);

	  len=(j+3)>>2;
	  type|=(len&0xffffff);

	  if (bfd_write((PTR)(&type),sizeof(unsigned long),1,abfd)!=sizeof(unsigned long))
	    return false;
	  if (bfd_write((PTR)(sym_p->name),sizeof(char),j,abfd)!=j)
	    return false;
	  
	  if ((len<<2)-j!=0) /* Pad name */
	    if (bfd_write((PTR)(b),sizeof(char),(len<<2)-j,abfd)!=((len<<2)-j))
	      return false;
	  n[0]=sym_p->value; /* Size of common block */
	  n[1]=1;
	  n[2]=r->address;

	  if (bfd_write((PTR)(n),sizeof(unsigned long),3,abfd)!=3*sizeof(long))
	    return false;
	  
	  continue;
	}/* Of is common section */

      DPRINT(10,("Failing...\n"));
      BFD_FAIL();
    }/* Of traverse relocs */
      
	  
  /* Now traverse the symbol table and write out all definitions, that are relative
     to this hunk */
  /* Absolute defs are always only written out with the first hunk */
  /* Don't write out COMMON symbols
                     local symbols
                     undefined symbols
		     indirect symbols
		     warning symbols
		     debugging symbols
		     warning symbols
		     constructor symbols, since they are unrepresentable in HUNK format..*/

  DPRINT(10,("Traversing symbol table\n"));
  symbol_header=(AMIGA_DATA(abfd)->IsLoadFile)?HUNK_SYMBOL:HUNK_EXT;
  for (i=0;i<abfd->symcount;i++)
    {
      sym_p=abfd->outsymbols[i];
      osection=sym_p->section;

      DPRINT(5,("%d. symbol(%s), osec=%x(%s)\n",i,sym_p->name,osection,osection->name));

      if ((osection==bfd_und_section_ptr)||(osection==bfd_com_section_ptr)||
	  (osection==bfd_ind_section_ptr))
	continue; /* Don't write these */

      /* Only write abs defs, if not writing A Loadfile */
      if ((osection==bfd_abs_section_ptr)&&(section->index==0)&&!AMIGA_DATA(abfd)->IsLoadFile) /* Write out abs defs */
	{
	  DPRINT(5,("Abs symbol\n"));
	  if ((symbol_count++)==0) /* First write out the HUNK_EXT */
	    {
	      if(bfd_write((PTR)(&symbol_header),sizeof(int),1,abfd)!=sizeof(int))
		return false;
	    }
	  type=(EXT_ABS<<24)&0xff000000;

	  j=strlen(sym_p->name);
	  
	  len=(j+3)>>2;
	  
	  type|=(len & 0xffffff);

	  if (bfd_write((PTR)(&type),sizeof(unsigned long),1,abfd)!=sizeof(unsigned long))
	    return false;
	  if (bfd_write((PTR)(sym_p->name),sizeof(char),j,abfd)!=j)
	    return false;
	  
	  if ((len<<2)-j!=0) /* Pad name */
	    if (bfd_write((PTR)(b),sizeof(char),(len<<2)-j,abfd)!=((len<<2)-j))
	      return false;
	  
	  n[0]=sym_p->value;
	  if (bfd_write((PTR)n,sizeof(unsigned long),1,abfd)!=sizeof(unsigned long))
	    return false;
	  continue;
	}/* Of abs def */
      if (osection==bfd_abs_section_ptr) /* Not first hunk. Already written */
	continue;

      /* If it is a warning symbol, or a constructor symbol or a debugging or a local symbol,
	 don't write it */
      if (sym_p->flags & (BSF_WARNING|BSF_CONSTRUCTOR|BSF_DEBUGGING|BSF_LOCAL))
	continue;

      /* Now, if osection==section, write it out */
      if (osection->output_section==section)
	{
	  DPRINT(5,("Writing it out\n"));

	  if ((symbol_count++)==0) /* First write out the header */
	    {
	      if(bfd_write((PTR)(&symbol_header),sizeof(int),1,abfd)!=sizeof(int))
		return false;
	    }
	  type=((symbol_header==HUNK_EXT?EXT_DEF:0)<<24)&0xff000000;

	  j=strlen(sym_p->name);
	  
	  len=(j+3)>>2;
	  
	  type|=(len & 0xffffff);

	  if (bfd_write((PTR)(&type),sizeof(unsigned long),1,abfd)!=sizeof(unsigned long))
	    return false;
	  if (bfd_write((PTR)(sym_p->name),sizeof(char),j,abfd)!=j)
	    return false;
	  
	  if ((len<<2)-j!=0) /* Pad name */
	    if (bfd_write((PTR)(b),sizeof(char),(len<<2)-j,abfd)!=((len<<2)-j))
	      return false;

	  n[0]=sym_p->value+sym_p->section->output_offset;
	  if (bfd_write((PTR)n,sizeof(unsigned long),1,abfd)!=sizeof(unsigned long))
	    return false;
	}/* Of this section */
    }/* Of for */

  DPRINT(10,("Did traversing\n"));
  if (symbol_count) /* terminate HUNK_EXT, HUNK_SYMBOL */
    {
      n[0]=0;
      if (bfd_write((PTR)n,sizeof(unsigned long),1,abfd)!=sizeof(unsigned long))
	return false;
    }
  DPRINT(5,("Leaving\n"));
  goto alldone; /* Write HUNK_END, return */

}


static boolean
amiga_get_section_contents (abfd, section, location, offset, count)
     bfd *abfd;
     sec_ptr section;
     PTR location;
     file_ptr offset;
     bfd_size_type count;
{
  memmove ((void *) location,
	   (void *) (((int) AMIGA_DATA(abfd)->first_byte)
		     + (int) section->filepos
		     + (int) offset),
	   count);
  return true;
}

boolean
amiga_new_section_hook (abfd, newsect)
     bfd *abfd;
     asection *newsect;
{
  newsect->used_by_bfd = (PTR) bfd_zalloc (abfd, sizeof (amiga_per_section_type));
  newsect->alignment_power = 2;
  amiga_per_section(newsect)->reloc_tail = NULL;
  return true;
}


/* This reads the symbol table, by following various pointers,
   set up by amiga_digest_file */
static boolean
amiga_slurp_symbol_table (abfd)
     bfd *abfd;
{
  /* slurp in symbols, associated with this bfd */
  amiga_data_type *amiga_data=AMIGA_DATA(abfd);
  asection *section;
  struct amiga_raw_symbol *sp;
  amiga_symbol_type *asp=NULL;
  unsigned long *lp;
  int len;
  int type;

  if (amiga_data->symbols)
    return true; /* already read */

  /* Symbols are associated with every section */
  for (section=abfd->sections;section!=NULL;section=section->next)
    {
      for (sp=amiga_per_section(section)->first;sp!=NULL;sp=sp->next)
	{
	  lp=&(sp->data[0]);
	  while (*lp)
	    {
	      type =(*lp)>>24; /* type of entry */
	      len=(*lp++)&0xffffff; /* namelength */
	      
	      switch(type)
		{
		case EXT_COMMON: /* Common reference/definition*/
		case EXT_ABS: /* Absolute */
		case EXT_DEF: /* Relative Definition */
		case EXT_SYMB: /* Same as EXT_DEF for load files */
		  /* Add symbol to section.... */
		  /* We don't have to increase symcount, this has already been done */
		  
		  asp=(amiga_symbol_type *)bfd_zalloc(abfd,sizeof(amiga_symbol_type));
		  if (!asp) /* No mem */
		    {
		      bfd_set_error(bfd_error_no_memory);
		      return(false);
		    }
		  
		  /* Add symbol to list of all symbols */
		  asp->next=NULL;
		  if (!amiga_data->symbols) /* first one */
		    {
		      amiga_data->symbols=amiga_data->symbol_tail=asp;
		    }
		  else
		    {
		      amiga_data->symbol_tail->next=asp;
		      amiga_data->symbol_tail=asp;
		    }

		  asp->symbol.section=((type==EXT_DEF)||(type==EXT_SYMB))?section:(type==EXT_COMMON)?
		       bfd_com_section_ptr:bfd_abs_section_ptr;
		  asp->symbol.flags=BSF_GLOBAL;
		  asp->symbol.value=lp[len];
		  asp->symbol.the_bfd=abfd;
		  asp->type=type;
		  asp->hunk_number=((type==EXT_DEF)||(type==EXT_SYMB))?section->target_index:
		     (type==EXT_ABS)?-1:-3;
		  if ((asp->symbol.name=bfd_alloc(abfd,(len<<2)+1))==NULL)
		    {
		      bfd_set_error(bfd_error_no_memory);
		      return(false);
		    } 
		  strncpy((char *)(asp->symbol.name),(char *)lp,len<<2);
		  ((char*)(asp->symbol.name))[(len<<2)]='\0';

		  /* Now store the symbol in the first name longword */
		  *lp=(unsigned long)asp;

		  lp+=len+1;

		  if (type==EXT_COMMON) /* skip refs */
		    lp+=(*lp)+1;

		  break;

		default: /* References to an undefined symbol */
		  /* Problem is that this symbol may be defined in another hunk WITHIN this bfd */
		  /* To avoid conflicts with bfd, we simply scan through the list of already defined symbols
		     and add a symbol only, if we did not find it */

		  for (asp=amiga_data->symbols;asp!=NULL;asp=asp->next)
		    {
		      if (strncmp((char *)lp,asp->symbol.name,len<<2)==0)
			break; /* Found symbol */
		    }

		  if (asp==NULL) /* Symbol not  defined */
		    {
		      asp=(amiga_symbol_type *)bfd_zalloc(abfd,sizeof(amiga_symbol_type));
		      if (!asp) /* No mem */
			{
			  bfd_set_error(bfd_error_no_memory);
			  return(false);
			}
		      
		      /* Add symbol to list of all symbols */
		      asp->next=NULL;
		      if (!amiga_data->symbols) /* first one */
			{
			  amiga_data->symbols=amiga_data->symbol_tail=asp;
			}
		      else
			{
			  amiga_data->symbol_tail->next=asp;
			  amiga_data->symbol_tail=asp;
			}
		      if ((asp->symbol.name=bfd_alloc(abfd,(len<<2)+1))==NULL)
			{
			  bfd_set_error(bfd_error_no_memory);
			  return(false);
			} 
		      strncpy((char *)(asp->symbol.name),(const char *)lp,len<<2);
		      ((char *)(asp->symbol.name))[(len<<2)]='\0';

		      asp->symbol.section=bfd_und_section_ptr;
		      asp->symbol.flags=0;
		      asp->symbol.the_bfd=abfd;
		      asp->hunk_number=-2;
		      asp->type=type;
		      abfd->symcount++;

		    }
		  
		  /* Store symbol in 1st LW of name */
		  *lp=(unsigned long)asp;
		  lp+=len;
		  lp+=1+(*lp); /* skip refs */
		  break;
		}/* Of switch type */
	    }/* Of while *lp */
	}/* of for sp */
    }/* Of for section */


  return true;
}


/* Get size of symtab */
long
amiga_get_symtab_upper_bound (abfd)
     bfd *abfd;
{
  /* We read in the symbol table first */
  if (!amiga_slurp_symbol_table (abfd))
    return -1;

  return (abfd->symcount != 0) ?
    (abfd->symcount+1) * (sizeof (amiga_symbol_type *)) : 0;
}

long
amiga_get_symtab (abfd, location)
     bfd *abfd;
     asymbol **location;
{
  amiga_symbol_type *symp;

  if(!amiga_slurp_symbol_table(abfd))
    return -1;

  if (abfd->symcount)
    {
      int i = 0;

      for (symp = AMIGA_DATA(abfd)->symbols;
			     symp != (amiga_symbol_type *) NULL;
			     symp = symp->next)
	{
	  location[i++] = &symp->symbol;
	}
    }
  return abfd->symcount;
}

asymbol *
amiga_make_empty_symbol (abfd)
     bfd *abfd;
{
  amiga_symbol_type *new =
    (amiga_symbol_type *) bfd_zalloc (abfd, sizeof (amiga_symbol_type));
  new->symbol.the_bfd = abfd;
  return &new->symbol;
}



void
amiga_get_symbol_info (ignore_abfd, symbol, ret)
      bfd *ignore_abfd;
      asymbol *symbol;
      symbol_info *ret;
{
  bfd_symbol_info (symbol, ret);
  if (symbol->name[0] == ' ')
    ret->name = "* empty table entry ";
  if (symbol->section==bfd_abs_section_ptr)
    ret->type = (symbol->flags & BSF_LOCAL) ? 'a' : 'A';
}



void
amiga_print_symbol (ignore_abfd, afile,  symbol, how)
      bfd *ignore_abfd;
      PTR afile;
      asymbol *symbol;
      bfd_print_symbol_type how;
{
  FILE *file = (FILE *)afile;
  
  switch (how) {
  case bfd_print_symbol_name:
    fprintf(file, "%s", symbol->name);
    break;
  case bfd_print_symbol_more:
    fprintf(stderr,"%4x %2x %2x",
	    (unsigned int)((amiga_symbol(symbol)->hunk_number)&0xffff),0,amiga_symbol(symbol)->type);
    break;
  case bfd_print_symbol_all:
      {
	CONST char *section_name = symbol->section->name;
	if (symbol->name[0] == ' ')
	  {
	    fprintf(file, "* empty table entry ");
	  }
	else
	  {
	    bfd_print_symbol_vandf ((PTR)file, symbol);

	    fprintf(file," %-5s %04x %02x %s",
		    section_name,
		    (unsigned int)((amiga_symbol(symbol)->hunk_number)&0xffff),  /* ->desc */
		    (unsigned) 0,                       /* ->other */
		                                        /* type */
		    symbol->name);                      /* ->name */
	}
      }
    break;
  }
}

 long
amiga_get_reloc_upper_bound (abfd, asect)
     bfd *abfd;
     sec_ptr asect;
{
  if (bfd_get_format (abfd) != bfd_object)
    {
      bfd_set_error(bfd_error_invalid_operation);
      return 0;
    }
  return sizeof (arelent *) * (asect->reloc_count + 1);
}


/* slurp in relocs , amiga_digest_file left various pointers for us*/
static boolean 
amiga_slurp_relocs(bfd *abfd, sec_ptr section, asymbol **symbols)
{
  struct amiga_raw_symbol *sp;
  amiga_symbol_type *asp;
  long *lp;
  
  int i,n,br,j;
  int index;
  long hunk_number;
  int type;

  /* First browse through raw_relocs */
  for (i=0;i<amiga_per_section(section)->num_raw_relocs;i++)
    {
      lp=amiga_per_section(section)->raw_relocs[i];

      /* lp points to RELOC ID */
      /* first determine type of reloc */
      switch (*lp)
	{
	case HUNK_RELOC32: /* 32 bit ref */
	case HUNK_RELOC16: /* 16 bit ref */
	case HUNK_RELOC8: /* 8 bit ref */
	case HUNK_DREL32: /* 32 bit ref baserel */
	case HUNK_DREL16: /* 16 bit baserel */
	case HUNK_DREL8: /* 8 bit baserel */
	  if (*lp<HUNK_DREL32)
	    { /*0:8bit, 1: 16bit, 2:32bit */
	      index=2-((*lp)-HUNK_RELOC32);
	      br=0; /* not base relative */
	    }
	  else
	    {
	      index=2-((*lp)-HUNK_DREL32);
	      br=1; /* base relative */
	    }

	  lp++;
	  while ((n=*lp++)) /* read offsets and hunk number */
	    {
	      hunk_number=(long)(*lp++);
	      for (j=0;j<n;j++)
		{ /* add relocs */

		  if (!amiga_add_reloc(abfd,section,lp[j],NULL,
				       amiga_howto_array[br][index],hunk_number))
		    return false;
		}/* of for */
	      lp+=n;
	    }/* of while */
	  break;

	default: /* error */
	  bfd_set_error(bfd_error_wrong_format);
	  return (false);
	  break;
	}/* of switch */

    }/* of for i */
  

  /* Now step through the raw_symbols and add all relocs in them */
  for (sp=amiga_per_section(section)->first;sp!=NULL;sp=sp->next)
    {
      lp=&(sp->data[0]);
      /* points to id */

      while (*lp) /* until end is reached */
	{
	  type=((*lp)>>24)&0xff;
	  n=(*lp++)&0xffffff;

	  /* lp points to symbol pointer (former 1st LW of name )*/
	  asp=(amiga_symbol_type *)(*lp);

	  switch (type)
	    {
	    case EXT_SYMB:
	    case EXT_DEF:
	    case EXT_ABS: /* no relocs here, just advance the pointer */
	      lp+=1+n;
	      break;

	    case EXT_COMMON: /* same as below, but advance lp by one to skip common size */
	      lp++;
	      /* Fall through */
	    default: /* reference to something */
	      /* skip name first */
	      lp+=n;

	      /* points to num of refs to hunk */
	      n=*lp++;

	      for (i=0;i<n;i++) /* refs follow */
		{
		  /* Add relocs to this section, relative to asp, offset is lp[i] */
		  /* determine howto first */
		  if (type==EXT_COMMON) /* 32 bit ref */
		    {
		      index=2;
		      br=0;
		    }
		  else
		    {
		      if (type>EXT_REF32)
			type--; /* skip EXT_COMMON gap */
		     
		     type-=EXT_REF32;
		     br=0;

		     if (type>2) /* base relative */
		       {
			 type-=3;
			 br=1;
		       }
		     index=2-type;
		    }/* of else */
		  
		  if (!amiga_add_reloc(abfd,section,lp[i],asp,amiga_howto_array[br][index],-4))
		    return false;
		}
	      
	      lp+=n;
	      
	      break;
	    }/* of switch */
	}/* of while *lp */
    }/* Of for sp*/

  return true;

}/* Of slurp_relocs */


long
amiga_canonicalize_reloc (abfd, section, relptr, symbols)
     bfd *abfd;
     sec_ptr section;
     arelent **relptr;
     asymbol **symbols;
{

  amiga_reloc_type *src = NULL;

  if (!amiga_slurp_relocs(abfd,section,symbols))
    return -1;

  src=(amiga_reloc_type *)section->relocation;
  while (src != (amiga_reloc_type *) 0)
    {
      *relptr++ = &src->relent;
      src = src->next;
    }
  *relptr = (arelent *) 0;

  return section->reloc_count;
}


/* Set section contents */
/* We do it the following way: 
   if this is a bss setion ==> error
   otherwise, we try to allocate space for this section,
   if  this has not already been done
   Then we set the memory area to the contents */
static boolean
amiga_set_section_contents (abfd, section, location, offset, count)
     bfd *abfd;
     sec_ptr section;
     unsigned char *location;
     file_ptr offset;
      int count;
{
  unsigned char *contents;

  if ((section->flags&SEC_HAS_CONTENTS)==0) /* BSS */
    {
      bfd_set_error(bfd_error_invalid_operation);
      return false;
    }
  
  if ((section->flags&SEC_IN_MEMORY)==0) /* Not in memory, so alloc space */
    {
      contents=bfd_zalloc(abfd,section->_raw_size);
      if (!contents)
	{
	  bfd_set_error(bfd_error_no_memory);
	  return false;
	}
      
      DPRINT(5,("Allocated %lx bytes at %lx\n",section->_raw_size,contents));

      section->contents=contents;
      section->flags|=SEC_IN_MEMORY;
    }
  else /* In memory */
    contents=section->contents;

  /* Copy mem */
  memmove(contents+offset,location,count);

  return(true);

}/* Of section_set_contents */


/* FIXME: Is this everything ? */
static boolean
amiga_set_arch_mach (abfd, arch, machine)
     bfd *abfd;
     enum bfd_architecture arch;
     unsigned long machine;
{
  bfd_default_set_arch_mach(abfd, arch, machine);

  if (arch == bfd_arch_m68k)
    {
      switch (machine)
	{
	case 68000:
	case 68008:
	case 68010:
	case 68020:
	case 68030:
	case 68040:
	case 68070:
	case 0:
	  return true;
	default:
	  return false;
	}
    }
  return false;
}

static int
DEFUN(amiga_sizeof_headers,(ignore_abfd, ignore),
      bfd *ignore_abfd AND
      boolean ignore)
{
  /* The amiga hunk format doesn't have headers.*/
  return 0;
}

/* Provided a BFD, a section and an offset into the section, calculate
   and return the name of the source file and the line nearest to the
   wanted location.  */
boolean
amiga_find_nearest_line(abfd, section, symbols, offset, filename_ptr,
			functionname_ptr, line_ptr)
     bfd *abfd;
     asection *section;
     asymbol **symbols;
     bfd_vma offset;
     char **filename_ptr;
     char **functionname_ptr;
     int *line_ptr;
{
  /* FIXME (see aoutx.h, for example) */
  return false;
}

static const struct reloc_howto_struct *
amiga_bfd_reloc_type_lookup (abfd, code)
       bfd *abfd;
       bfd_reloc_code_real_type code;
{
  switch (code)
    {
    case BFD_RELOC_8_PCREL:  return &howto_hunk_reloc8;
    case BFD_RELOC_16_PCREL: return &howto_hunk_reloc16;
    case BFD_RELOC_32_PCREL: return &howto_hunk_reloc32;
    case BFD_RELOC_8:        return &howto_hunk_drel8;
    case BFD_RELOC_16:       return &howto_hunk_drel16;
    case BFD_RELOC_CTOR:	/* FIXME - assumes 32 bits */
    case BFD_RELOC_32:       return &howto_hunk_drel32;
      /* FIXME: Add more cases here for base relative relocs*/
    default:                 return 0;
    }
}


/* We don't have core files.  */
#define	amiga_core_file_failing_command _bfd_dummy_core_file_failing_command
#define	amiga_core_file_failing_signal _bfd_dummy_core_file_failing_signal
#define	amiga_core_file_matches_executable_p _bfd_dummy_core_file_matches_executable_p

/* 101194: We do not use archive files, AMIGA Hunk load libs will be supported someday... ST */
#define	amiga_openr_next_archived_file	(bfd * (*)(bfd *, bfd*))bfd_nullvoidptr
#define	amiga_generic_stat_arch_elt	(int (*)(bfd *, struct stat *))_bfd_n1
#define	amiga_slurp_armap		(boolean (*)(bfd *))bfd_false
#define	amiga_slurp_extended_name_table	(boolean (*)(bfd *))bfd_false
#define amiga_update_armap_timestamp	(boolean (*)(bfd *))bfd_false
#define	amiga_write_armap		(boolean (*)(bfd *,unsigned int, struct orl*,\
unsigned int,int))bfd_false
#define	amiga_truncate_arname		(void (*)(bfd *,const char *, char*))bfd_void
#define amiga_construct_extended_name_table (boolean (*)(bfd *,char **, bfd_size_type *,\
 const char **))bfd_false

#define amiga_bfd_debug_info_start		bfd_void
#define amiga_bfd_debug_info_end		bfd_void
#define amiga_bfd_debug_info_accumulate	(PROTO(void,(*),(bfd*, struct sec *))) bfd_void

/* NOTE: We use a special get_relocated_section_contents both in amiga AND in a.out files.
   In addition, we use an own final_link routine, which is nearly identical to _bfd_generic_final_link */
extern bfd_byte *get_relocated_section_contents(bfd*, struct bfd_link_info *,
						struct bfd_link_order *, bfd_byte *,
						boolean, asymbol **);
#define amiga_bfd_get_relocated_section_contents get_relocated_section_contents
#define amiga_bfd_relax_section                   bfd_generic_relax_section
#define amiga_get_lineno (struct lineno_cache_entry *(*)())bfd_nullvoidptr
#define amiga_close_and_cleanup         _bfd_generic_close_and_cleanup
#define amiga_bfd_make_debug_symbol \
  (asymbol * (*)(bfd *, void *, unsigned long)) bfd_nullvoidptr
#define amiga_bfd_link_hash_table_create _bfd_generic_link_hash_table_create
#define amiga_bfd_link_add_symbols _bfd_generic_link_add_symbols
extern boolean amiga_final_link(bfd *, struct bfd_link_info *);
#define amiga_bfd_final_link amiga_final_link
#define amiga_bfd_free_cached_info	_bfd_generic_bfd_free_cached_info
#define amiga_bfd_is_local_label_name	bfd_generic_is_local_label_name
#define amiga_bfd_copy_private_section_data (boolean (*)(bfd *,sec_ptr,bfd *,sec_ptr))bfd_true
#define amiga_read_minisymbols _bfd_generic_read_minisymbols
#define amiga_minisymbol_to_symbol _bfd_generic_minisymbol_to_symbol
#define amiga_bfd_link_split_section _bfd_generic_link_split_section
#define amiga_bfd_copy_private_bfd_data _bfd_generic_bfd_copy_private_bfd_data
#define amiga_get_section_contents_in_window _bfd_generic_get_section_contents_in_window
#define amiga_bfd_print_private_bfd_data _bfd_generic_bfd_print_private_bfd_data
#define amiga_read_ar_hdr _bfd_generic_read_ar_hdr
#define amiga_get_elt_at_index _bfd_generic_get_elt_at_index
#define amiga_bfd_gc_sections bfd_generic_gc_sections

#if defined (amiga)
/* So that the JUMP_TABLE() macro below can work.  */
#undef amiga
#endif

bfd_target amiga_vec =
{
  "amiga",		/* name */
  bfd_target_amiga_flavour,
  BFD_ENDIAN_BIG,	/* data byte order is little */
  BFD_ENDIAN_BIG,	/* header byte order is little */
  HAS_RELOC | EXEC_P | HAS_LINENO | HAS_DEBUG | HAS_SYMS | HAS_LOCALS | WP_TEXT, /* object flags */
  SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_RELOC, /* section flags */
  '_',				/* symbol leading char */
  ' ',				/* ar_pad_char */
  31,				/* ar_max_namelen */
/*  2,				/* minimum align */
  bfd_getb64, bfd_getb_signed_64, bfd_putb64, bfd_getb32, bfd_getb_signed_32,
  bfd_putb32, bfd_getb16, bfd_getb_signed_16, bfd_putb16, /* data */
  bfd_getb64, bfd_getb_signed_64, bfd_putb64, bfd_getb32, bfd_getb_signed_32,
  bfd_putb32, bfd_getb16, bfd_getb_signed_16, bfd_putb16, /* hdrs */
  {
    /* bfd_check_format */
    _bfd_dummy_target,
    amiga_object_p,
    _bfd_dummy_target, /* AMIGA style like archives are not yet supported
			  archives are supported by a.out backend (ar) */
    _bfd_dummy_target
  },
  {
    /* bfd_set_format */
    bfd_false,
    amiga_mkobject,
    bfd_false,    /* Archives not supported yet */
    bfd_false
  },
  {
    /* bfd_write_contents */
    bfd_false,
    amiga_write_object_contents,
    bfd_false,    /*Archives not supported yet */
    bfd_false
  },
  BFD_JUMP_TABLE_GENERIC (amiga),
  BFD_JUMP_TABLE_COPY (amiga),
  BFD_JUMP_TABLE_CORE (_bfd_nocore),
  BFD_JUMP_TABLE_ARCHIVE (amiga),
  BFD_JUMP_TABLE_SYMBOLS (amiga),
  BFD_JUMP_TABLE_RELOCS (amiga),
  BFD_JUMP_TABLE_WRITE (amiga),
  BFD_JUMP_TABLE_LINK (amiga),
  BFD_JUMP_TABLE_DYNAMIC (_bfd_nodynamic),
  (PTR) 0
#if 0
/* fixme: no longer in use?  */
  /* How applications can find out about amiga relocation types (see
     documentation on reloc types).  */
  amiga_reloc_type_lookup
#endif
};

