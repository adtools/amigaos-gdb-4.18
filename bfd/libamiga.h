/* BFD back-end data structures for AmigaOS.
   Copyright (C) 1992-1994 Free Software Foundation, Inc.
   Contributed by Leonard Norrgard.
   Extended by Stephan Thesing Nov 94

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

#ifdef __STDC__
#define CAT3(a,b,c) a##b##c
#else
#define CAT3(a,b,c) a/**/b/**/c
#endif

#define GET_WORD bfd_h_get_32
#define GET_SWORD (int32_type)GET_WORD
#define PUT_WORD bfd_h_put_32
#define NAME(x,y) CAT3(x,_32_,y)
#define JNAME(x) CAT(x,_32)
#define BYTES_IN_WORD 4

#undef BITSPERBYTE
#undef LONGBITS
#undef MAXINT
#undef MININT

/* Hunk ID numbers.*/  
#define HUNK_UNIT       999
#define HUNK_NAME	1000
#define HUNK_CODE	1001
#define HUNK_DATA	1002
#define HUNK_BSS	1003
#define HUNK_RELOC32	1004
#define HUNK_RELOC16	1005
#define HUNK_RELOC8	1006
#define HUNK_EXT	1007
#define HUNK_SYMBOL	1008
#define HUNK_DEBUG	1009
#define HUNK_END	1010
#define HUNK_HEADER	1011
#define HUNK_OVERLAY	1013
#define HUNK_BREAK	1014
#define HUNK_DREL32	1015
#define HUNK_DREL16	1016
#define HUNK_DREL8	1017
#define HUNK_LIB	1018
#define HUNK_INDEX	1019

/* The hunk ID part.  */
#define HUNK_VALUE(hunk_id) ((hunk_id) & 0x3fffffff)

/* Attributes of a hunk.  */
#define HUNK_ATTRIBUTE(hunk_id) ((hunk_id) >> 30)
#define HUNK_ATTR_CHIP 0x01 /* Hunk contents must go into chip (graphics) memory.  */
#define HUNK_ATTR_FAST 0x02 /* fast */
#define HUNK_ATTR_FOLLOWS 0x03 /* Mem id follows */

#define MEMF_PUBLIC (1L<<0)
#define MEMF_CHIP   (1L<<1)
#define MEMF_FAST   (1L<<2)

#define HUNKF_ADVISORY	(1L<<29)
#define HUNKF_CHIP	(1L<<30)
#define HUNKF_FAST	(1L<<31)

/* HUNK_EXT sub-types.*/  
#define EXT_SYMB	0	/* Symbol table.  */
#define EXT_DEF		1	/* Relocatable definition.  */
#define EXT_ABS		2	/* Absolute definition. */
#define EXT_RES		3	/* Obsolete.  */
#define EXT_REF32	129	/* 32 bit reference to symbol.  */
#define EXT_COMMON	130	/* 32 bit reference to COMMON block.  */
#define EXT_REF16	131	/* 16 bit reference to symbol.  */
#define EXT_REF8	132	/*  8 bit reference to symbol.  */
#define EXT_DEXT32	133	/* 32 bit data releative reference.  */
#define EXT_DEXT16	134	/* 16 bit data releative reference.  */
#define EXT_DEXT8	135	/*  8 bit data releative reference.  */


typedef struct amiga_reloc {
  arelent relent;
  struct amiga_reloc *next;
  struct amiga_symbol *symbol;
  long target_hunk;
} amiga_reloc_type;

typedef struct amiga_symbol {
  asymbol symbol;
  struct amiga_symbol *next;
  long hunk_number;
  unsigned char type;
} amiga_symbol_type;

struct amiga_raw_symbol {
  struct amiga_raw_symbol *next;
  unsigned long data[1];
};

typedef struct amiga_per_section
{
  amiga_reloc_type *reloc_tail; /* last reloc */ /* first is in section->relocation */
  int attribute; /* Memory type required by this section */
  int real_length; /* This is the length, occuring in the hunk */
                   /* _raw_size may be larger than this */
  int max_raw_relocs; /* Size of array */
  int num_raw_relocs; /* # of relocs, this points to : */
  unsigned long  **raw_relocs; /* Points to array of raw_relocs */
                             /* every array element points to the raw data */
  struct amiga_raw_symbol *first;
  struct amiga_raw_symbol *last; /* tail */
  
} amiga_per_section_type;
#define amiga_per_section(x) ((amiga_per_section_type *)((x)->used_by_bfd))

/* The `tdata' struct for all a.out-like object file formats.
   Various things depend on this struct being around any time an a.out
   file is being handled.  An example is dbxread.c in GDB.  */

struct amiga_data {
  struct internal_exec *hdr;		/* exec file header */
  amiga_symbol_type *symbols;		/* symtab for input bfd */

  /* Filler, so we can pretend to be an a.out to GDB.  */
  asection *textsec;
  asection *datasec;
  asection *bsssec;

  /* The positions of the string table and symbol table.  */
  file_ptr sym_filepos;
  file_ptr str_filepos;

  unsigned int n_symbols;               /* number of symbols */

  /* Size of a relocation entry in external form */
  unsigned dummy_reloc_entry_size;

  /* Size of a symbol table entry in external form */
  unsigned symbol_entry_size;

  unsigned exec_bytes_size;
  unsigned vma_adjusted : 1;
};

typedef struct  amiga_data_struct {
  struct amiga_data a;

  unsigned long symtab_size;
  unsigned long stringtab_size;

  unsigned long *first_byte;
  unsigned long *file_end;
  unsigned long *file_pointer;
  amiga_symbol_type *symbols;
  amiga_symbol_type *symbol_tail;
  boolean IsLoadFile; /* If true, this is a load file (for output bfd only) */
  int maxsymbols;     /* Used by final_link routine to add symbols to output bfd.
                         This is the # of entries, allocated in abdfd->osymbols */
} amiga_data_type;

#define	adata(bfd)		((bfd)->tdata.amiga_data->a)

/* We take the address of the first element of an asymbol to ensure that the
   macro is only ever applied to an asymbol */
#define amiga_symbol(asymbol) ((amiga_symbol_type *)(&(asymbol)->the_bfd))

#define AMIGA_DATA(abfd) ((abfd)->tdata.amiga_data)

#define amiga_bfd_merge_private_bfd_data _bfd_generic_bfd_merge_private_bfd_data
#define amiga_bfd_copy_private_symbol_data _bfd_generic_bfd_copy_private_symbol_data
#define amiga_bfd_set_private_flags _bfd_generic_bfd_set_private_flags
