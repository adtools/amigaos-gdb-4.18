/* Header file for targets using CGEN: Cpu tools GENerator.

Copyright (C) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.

This file is part of GDB, the GNU debugger, and the GNU Binutils.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#ifndef CGEN_H
#define CGEN_H

/* ??? This file requires bfd.h but only to get bfd_vma.
   Seems like an awful lot to require just to get such a fundamental type.
   Perhaps the definition of bfd_vma can be moved outside of bfd.h.
   Or perhaps one could duplicate its definition in another file.
   Until such time, this file conditionally compiles definitions that require
   bfd_vma using BFD_VERSION.  */

/* Enums must be defined before they can be used.
   Allow them to be used in struct definitions, even though the enum must
   be defined elsewhere.
   If CGEN_ARCH isn't defined, this file is being included by something other
   than <arch>-desc.h.  */

/* Prepend the arch name, defined in <arch>-desc.h, and _cgen_ to symbol S.
   The lack of spaces in the arg list is important for non-stdc systems.
   This file is included by <arch>-desc.h.
   It can be included independently of <arch>-desc.h, in which case the arch
   dependent portions will be declared as "unknown_cgen_foo".  */

#ifndef CGEN_SYM
#define CGEN_SYM(s) CONCAT3 (unknown,_cgen_,s)
#endif

/* This file contains the static (unchanging) pieces and as much other stuff
   as we can reasonably put here.  It's generally cleaner to put stuff here
   rather than having it machine generated if possible.  */

/* The assembler syntax is made up of expressions (duh...).
   At the lowest level the values are mnemonics, register names, numbers, etc.
   Above that are subexpressions, if any (an example might be the
   "effective address" in m68k cpus).  Subexpressions are wip.
   At the second highest level are the insns themselves.  Above that are
   pseudo-insns, synthetic insns, and macros, if any.  */

/* Lots of cpu's have a fixed insn size, or one which rarely changes,
   and it's generally easier to handle these by treating the insn as an
   integer type, rather than an array of characters.  So we allow targets
   to control this.  When an integer type the value is in host byte order,
   when an array of characters the value is in target byte order.  */

typedef unsigned int CGEN_INSN_INT;
#if CGEN_INT_INSN_P
typedef CGEN_INSN_INT CGEN_INSN_BYTES;
typedef CGEN_INSN_INT *CGEN_INSN_BYTES_PTR;
#else
typedef unsigned char *CGEN_INSN_BYTES;
typedef unsigned char *CGEN_INSN_BYTES_PTR;
#endif

#ifdef __GNUC__
#define CGEN_INLINE __inline__
#else
#define CGEN_INLINE
#endif

enum cgen_endian
{
  CGEN_ENDIAN_UNKNOWN,
  CGEN_ENDIAN_LITTLE,
  CGEN_ENDIAN_BIG
};

/* Forward decl.  */

typedef struct cgen_insn CGEN_INSN;

/* Opaque pointer version for use by external world.  */

typedef struct cgen_cpu_desc *CGEN_CPU_DESC;

/* Attributes.
   Attributes are used to describe various random things associated with
   an object (ifield, hardware, operand, insn, whatever) and are specified
   as name/value pairs.
   Integer attributes computed at compile time are currently all that's
   supported, though adding string attributes and run-time computation is
   straightforward.  Integer attribute values are always host int's
   (signed or unsigned).  For portability, this means 32 bits.
   Integer attributes are further categorized as boolean, bitset, integer,
   and enum types.  Boolean attributes appear frequently enough that they're
   recorded in one host int.  This limits the maximum number of boolean
   attributes to 32, though that's a *lot* of attributes.  */

/* Type of attribute values.  */

typedef int CGEN_ATTR_VALUE_TYPE;

/* Struct to record attribute information.  */

typedef struct
{
  /* Number of non-boolean attributes.  */
  unsigned int num_nonbools;
  /* Boolean attributes.  */
  unsigned int bool;
  /* Non-boolean integer attributes.  */
  CGEN_ATTR_VALUE_TYPE nonbool[1];
} CGEN_ATTR;

/* Define a structure member for attributes with N non-boolean entries.
   The attributes are sorted so that the non-boolean ones come first.
   There is no maximum number of non-boolean attributes.
   There is a maximum of 32 boolean attributes (since they are all recorded
   in one host int).  */

#define CGEN_ATTR_TYPE(n) \
struct { unsigned int num_nonbools; \
	 unsigned int bool; \
	 CGEN_ATTR_VALUE_TYPE nonbool[(n) ? (n) : 1]; }

/* Return the boolean attributes.  */

#define CGEN_ATTR_BOOLS(a) ((a)->bool)

/* Boolean attribute numbers are offset by this much.  */

#define CGEN_ATTR_BOOL_OFFSET 32

/* Given a boolean attribute number, return its mask.  */

#define CGEN_ATTR_MASK(attr) (1 << ((attr) - CGEN_ATTR_BOOL_OFFSET))

/* Return the value of boolean attribute ATTR in ATTRS.  */

#define CGEN_BOOL_ATTR(attrs, attr) ((CGEN_ATTR_MASK (attr) & (attrs)) != 0)

/* Return value of attribute ATTR in ATTR_TABLE for OBJ.
   OBJ is a pointer to the entity that has the attributes
   (??? not used at present but is reserved for future purposes - eventually
   the goal is to allow recording attributes in source form and computing
   them lazily at runtime, not sure of the details yet).  */

#define CGEN_ATTR_VALUE(obj, attr_table, attr) \
((unsigned int) (attr) < CGEN_ATTR_BOOL_OFFSET \
 ? ((attr_table)->nonbool[attr]) \
 : ((CGEN_ATTR_BOOLS (attr_table) & CGEN_ATTR_MASK (attr)) != 0))

/* Attribute name/value tables.
   These are used to assist parsing of descriptions at run-time.  */

typedef struct
{
  const char * name;
  CGEN_ATTR_VALUE_TYPE value;
} CGEN_ATTR_ENTRY;

/* For each domain (ifld,hw,operand,insn), list of attributes.  */

typedef struct
{
  const char * name;
  const CGEN_ATTR_ENTRY * dfault;
  const CGEN_ATTR_ENTRY * vals;
} CGEN_ATTR_TABLE;

/* Parse result (also extraction result).

   The result of parsing an insn is stored here.
   To generate the actual insn, this is passed to the insert handler.
   When printing an insn, the result of extraction is stored here.
   To print the insn, this is passed to the print handler.

   It is machine generated so we don't define it here,
   but we do need a forward decl for the handler fns.

   There is one member for each possible field in the insn.
   The type depends on the field.
   Also recorded here is the computed length of the insn for architectures
   where it varies.
*/

typedef struct cgen_fields CGEN_FIELDS;

/* Total length of the insn, as recorded in the `fields' struct.  */
/* ??? The field insert handler has lots of opportunities for optimization
   if it ever gets inlined.  On architectures where insns all have the same
   size, may wish to detect that and make this macro a constant - to allow
   further optimizations.  */

#define CGEN_FIELDS_BITSIZE(fields) ((fields)->length)

/* Extraction support for variable length insn sets.  */

/* When disassembling we don't know the number of bytes to read at the start.
   So the first CGEN_BASE_INSN_SIZE bytes are read at the start and the rest
   are read when needed.  This struct controls this.  It is basically the
   disassemble_info stuff, except that we provide a cache for values already
   read (since bytes can typically be read several times to fetch multiple
   operands that may be in them), and that extraction of fields is needed
   in contexts other than disassembly.  */

typedef struct {
  /* A pointer to the disassemble_info struct.
     We don't require dis-asm.h so we use PTR for the type here.
     If NULL, BYTES is full of valid data (VALID == -1).  */
  PTR dis_info;
  /* Points to a working buffer of sufficient size.  */
  unsigned char *insn_bytes;
  /* Mask of bytes that are valid in INSN_BYTES.  */
  unsigned int valid;
} CGEN_EXTRACT_INFO;

/* Associated with each insn or expression is a set of "handlers" for
   performing operations like parsing, printing, etc.  These require a bfd_vma
   value to be passed around but we don't want all applications to need bfd.h.
   So this stuff is only provided if bfd.h has been included.  */

/* Parse handler.
   CD is a cpu table descriptor.
   INSN is a pointer to a struct describing the insn being parsed.
   STRP is a pointer to a pointer to the text being parsed.
   FIELDS is a pointer to a cgen_fields struct in which the results are placed.
   If the expression is successfully parsed, *STRP is updated.
   If not it is left alone.
   The result is NULL if success or an error message.  */
typedef const char * (cgen_parse_fn)
     PARAMS ((CGEN_CPU_DESC, const CGEN_INSN *insn_,
	      const char **strp_, CGEN_FIELDS *fields_));

/* Insert handler.
   CD is a cpu table descriptor.
   INSN is a pointer to a struct describing the insn being parsed.
   FIELDS is a pointer to a cgen_fields struct from which the values
   are fetched.
   INSNP is a pointer to a buffer in which to place the insn.
   PC is the pc value of the insn.
   The result is an error message or NULL if success.  */

#ifdef BFD_VERSION
typedef const char * (cgen_insert_fn)
     PARAMS ((CGEN_CPU_DESC, const CGEN_INSN *insn_,
	      CGEN_FIELDS *fields_, CGEN_INSN_BYTES_PTR insnp_,
	      bfd_vma pc_));
#else
typedef const char * (cgen_insert_fn) ();
#endif

/* Extract handler.
   CD is a cpu table descriptor.
   INSN is a pointer to a struct describing the insn being parsed.
   The second argument is a pointer to a struct controlling extraction
   (only used for variable length insns).
   EX_INFO is a pointer to a struct for controlling reading of further
   bytes for the insn.
   BASE_INSN is the first CGEN_BASE_INSN_SIZE bytes (host order).
   FIELDS is a pointer to a cgen_fields struct in which the results are placed.
   PC is the pc value of the insn.
   The result is the length of the insn in bits or zero if not recognized.  */

#ifdef BFD_VERSION
typedef int (cgen_extract_fn)
     PARAMS ((CGEN_CPU_DESC, const CGEN_INSN *insn_,
	      CGEN_EXTRACT_INFO *ex_info_, CGEN_INSN_INT base_insn_,
	      CGEN_FIELDS *fields_, bfd_vma pc_));
#else
typedef int (cgen_extract_fn) ();
#endif

/* Print handler.
   CD is a cpu table descriptor.
   INFO is a pointer to the disassembly info.
   Eg: disassemble_info.  It's defined as `PTR' so this file can be included
   without dis-asm.h.
   INSN is a pointer to a struct describing the insn being printed.
   FIELDS is a pointer to a cgen_fields struct.
   PC is the pc value of the insn.
   LEN is the length of the insn, in bits.  */

#ifdef BFD_VERSION
typedef void (cgen_print_fn)
     PARAMS ((CGEN_CPU_DESC, PTR info_, const CGEN_INSN *insn_,
	      CGEN_FIELDS *fields_, bfd_vma pc_, int len_));
#else
typedef void (cgen_print_fn) ();
#endif

/* Parse/insert/extract/print handlers.

   Indices into the handler tables.
   We could use pointers here instead, but 90% of them are generally identical
   and that's a lot of redundant data.  Making these unsigned char indices
   into tables of pointers saves a bit of space.
   Using indices also keeps assembler code out of the disassembler and
   vice versa.  */

struct cgen_opcode_handler
{
  unsigned char parse, insert, extract, print;
};

/* Assembler interface.

   The interface to the assembler is intended to be clean in the sense that
   libopcodes.a is a standalone entity and could be used with any assembler.
   Not that one would necessarily want to do that but rather that it helps
   keep a clean interface.  The interface will obviously be slanted towards
   GAS, but at least it's a start.
   ??? Note that one possible user of the assembler besides GAS is GDB.

   Parsing is controlled by the assembler which calls
   CGEN_SYM (assemble_insn).  If it can parse and build the entire insn
   it doesn't call back to the assembler.  If it needs/wants to call back
   to the assembler, cgen_parse_operand_fn is called which can either

   - return a number to be inserted in the insn
   - return a "register" value to be inserted
     (the register might not be a register per pe)
   - queue the argument and return a marker saying the expression has been
     queued (eg: a fix-up)
   - return an error message indicating the expression wasn't recognizable

   The result is an error message or NULL for success.
   The parsed value is stored in the bfd_vma *.  */

/* Values for indicating what the caller wants.  */

enum cgen_parse_operand_type
{
  CGEN_PARSE_OPERAND_INIT,
  CGEN_PARSE_OPERAND_INTEGER,
  CGEN_PARSE_OPERAND_ADDRESS
};

/* Values for indicating what was parsed.  */

enum cgen_parse_operand_result
{
  CGEN_PARSE_OPERAND_RESULT_NUMBER,
  CGEN_PARSE_OPERAND_RESULT_REGISTER,
  CGEN_PARSE_OPERAND_RESULT_QUEUED,
  CGEN_PARSE_OPERAND_RESULT_ERROR
};

#ifdef BFD_VERSION /* Don't require bfd.h unnecessarily.  */
typedef const char * (cgen_parse_operand_fn)
     PARAMS ((CGEN_CPU_DESC,
	      enum cgen_parse_operand_type, const char **, int, int,
	      enum cgen_parse_operand_result *, bfd_vma *));
#else
typedef const char * (cgen_parse_operand_fn) ();
#endif

/* Set the cgen_parse_operand_fn callback.  */

extern void cgen_set_parse_operand_fn
     PARAMS ((CGEN_CPU_DESC, cgen_parse_operand_fn));

/* Called before trying to match a table entry with the insn.  */

extern void cgen_init_parse_operand PARAMS ((CGEN_CPU_DESC));

/* Operand values (keywords, integers, symbols, etc.)  */

/* Types of assembler elements.  */

enum cgen_asm_type
{
  CGEN_ASM_KEYWORD, CGEN_ASM_MAX
};

/* List of hardware elements.  */

typedef struct cgen_hw_entry
{
  /* The type of this entry, one of `enum hw_type'.
     This is an int and not the enum as the latter may not be declared yet.  */
  int type;
  const struct cgen_hw_entry * next;
  char * name;
  enum cgen_asm_type asm_type;
  PTR asm_data;
#ifndef CGEN_HW_NBOOL_ATTRS
#define CGEN_HW_NBOOL_ATTRS 1
#endif
  CGEN_ATTR_TYPE (CGEN_HW_NBOOL_ATTRS) attrs;
#define CGEN_HW_ATTRS(hw) (&(hw)->attrs)
} CGEN_HW_ENTRY;

/* Return value of attribute ATTR in HW.  */

#define CGEN_HW_ATTR_VALUE(hw, attr) \
CGEN_ATTR_VALUE ((hw), CGEN_HW_ATTRS (hw), (attr))

extern const CGEN_HW_ENTRY * cgen_hw_lookup_by_name
     PARAMS ((CGEN_CPU_DESC, const char *));
extern const CGEN_HW_ENTRY * cgen_hw_lookup_by_num
     PARAMS ((CGEN_CPU_DESC, int));

/* The operand instance table refers to this directly.  */
extern const CGEN_HW_ENTRY CGEN_SYM (hw_table)[];

/* This struct is used to describe things like register names, etc.  */

typedef struct cgen_keyword_entry
{
  /* Name (as in register name).  */
  char * name;

  /* Value (as in register number).
     The value cannot be -1 as that is used to indicate "not found".
     IDEA: Have "FUNCTION" attribute? [function is called to fetch value].  */
  int value;

  /* Attributes.
     This should, but technically needn't, appear last.  It is a variable sized
     array in that one architecture may have 1 nonbool attribute and another
     may have more.  Having this last means the non-architecture specific code
     needn't care.  The goal is to eventually record
     attributes in their raw form, evaluate them at run-time, and cache the
     values, so this worry will go away anyway.  */
  /* ??? Moving this last should be done by treating keywords like insn lists
     and moving the `next' fields into a CGEN_KEYWORD_LIST struct.  */
  /* FIXME: Not used yet.  */
#ifndef CGEN_KEYWORD_NBOOL_ATTRS
#define CGEN_KEYWORD_NBOOL_ATTRS 1
#endif
  CGEN_ATTR_TYPE (CGEN_KEYWORD_NBOOL_ATTRS) attrs;

  /* Next name hash table entry.  */
  struct cgen_keyword_entry *next_name;
  /* Next value hash table entry.  */
  struct cgen_keyword_entry *next_value;
} CGEN_KEYWORD_ENTRY;

/* Top level struct for describing a set of related keywords
   (e.g. register names).

   This struct supports run-time entry of new values, and hashed lookups.  */

typedef struct cgen_keyword
{
  /* Pointer to initial [compiled in] values.  */
  CGEN_KEYWORD_ENTRY *init_entries;
  
  /* Number of entries in `init_entries'.  */
  unsigned int num_init_entries;
  
  /* Hash table used for name lookup.  */
  CGEN_KEYWORD_ENTRY **name_hash_table;
  
  /* Hash table used for value lookup.  */
  CGEN_KEYWORD_ENTRY **value_hash_table;
  
  /* Number of entries in the hash_tables.  */
  unsigned int hash_table_size;
  
  /* Pointer to null keyword "" entry if present.  */
  const CGEN_KEYWORD_ENTRY *null_entry;
} CGEN_KEYWORD;

/* Structure used for searching.  */

typedef struct
{
  /* Table being searched.  */
  const CGEN_KEYWORD *table;
  
  /* Specification of what is being searched for.  */
  const char *spec;
  
  /* Current index in hash table.  */
  unsigned int current_hash;
  
  /* Current element in current hash chain.  */
  CGEN_KEYWORD_ENTRY *current_entry;
} CGEN_KEYWORD_SEARCH;

/* Lookup a keyword from its name.  */

const CGEN_KEYWORD_ENTRY *cgen_keyword_lookup_name
  PARAMS ((CGEN_KEYWORD *, const char *));

/* Lookup a keyword from its value.  */

const CGEN_KEYWORD_ENTRY *cgen_keyword_lookup_value
  PARAMS ((CGEN_KEYWORD *, int));

/* Add a keyword.  */

void cgen_keyword_add PARAMS ((CGEN_KEYWORD *, CGEN_KEYWORD_ENTRY *));

/* Keyword searching.
   This can be used to retrieve every keyword, or a subset.  */

CGEN_KEYWORD_SEARCH cgen_keyword_search_init
  PARAMS ((CGEN_KEYWORD *, const char *));
const CGEN_KEYWORD_ENTRY *cgen_keyword_search_next
  PARAMS ((CGEN_KEYWORD_SEARCH *));

/* Operand value support routines.  */

extern const char *cgen_parse_keyword
     PARAMS ((CGEN_CPU_DESC, const char **, CGEN_KEYWORD *, long *));
#ifdef BFD_VERSION /* Don't require bfd.h unnecessarily.  */
extern const char *cgen_parse_signed_integer
     PARAMS ((CGEN_CPU_DESC, const char **, int, long *));
extern const char *cgen_parse_unsigned_integer
     PARAMS ((CGEN_CPU_DESC, const char **, int, unsigned long *));
extern const char *cgen_parse_address
     PARAMS ((CGEN_CPU_DESC, const char **, int, int,
	      enum cgen_parse_operand_result *, bfd_vma *));
extern const char *cgen_validate_signed_integer
     PARAMS ((long, long, long));
extern const char *cgen_validate_unsigned_integer
     PARAMS ((unsigned long, unsigned long, unsigned long));
#endif

/* Operand modes.  */

/* ??? This duplicates the values in arch.h.  Revisit.
   These however need the CGEN_ prefix [as does everything in this file].  */
/* ??? Targets may need to add their own modes so we may wish to move this
   to <arch>-opc.h, or add a hook.  */

enum cgen_mode {
  CGEN_MODE_VOID, /* ??? rename simulator's VM to VOID? */
  CGEN_MODE_BI, CGEN_MODE_QI, CGEN_MODE_HI, CGEN_MODE_SI, CGEN_MODE_DI,
  CGEN_MODE_UBI, CGEN_MODE_UQI, CGEN_MODE_UHI, CGEN_MODE_USI, CGEN_MODE_UDI,
  CGEN_MODE_SF, CGEN_MODE_DF, CGEN_MODE_XF, CGEN_MODE_TF,
  CGEN_MODE_TARGET_MAX,
  CGEN_MODE_INT, CGEN_MODE_UINT,
  CGEN_MODE_MAX
};

/* FIXME: Until simulator is updated.  */

#define CGEN_MODE_VM CGEN_MODE_VOID

/* This struct defines each entry in the operand table.  */

typedef struct cgen_operand
{
  /* Name as it appears in the syntax string.  */
  char *name;

  /* The hardware element associated with this operand.  */
  const CGEN_HW_ENTRY *hw;

  /* FIXME: We don't yet record ifield definitions, which we should.
     When we do it might make sense to delete start/length (since they will
     be duplicated in the ifield's definition) and replace them with a
     pointer to the ifield entry.  Note that as more complicated situations
     need to be handled, going more and more with an OOP paradigm will help
     keep the complication under control.  Of course, this was the goal from
     the start, but getting there in one step was too much too soon.  */

  /* Bit position.
     This is just a hint, and may be unused in more complex operands.
     May be unused for a modifier.  */
  unsigned char start;

  /* The number of bits in the operand.
     This is just a hint, and may be unused in more complex operands.
     May be unused for a modifier.  */
  unsigned char length;

#if 0 /* ??? Interesting idea but relocs tend to get too complicated,
	 and ABI dependent, for simple table lookups to work.  */
  /* Ideally this would be the internal (external?) reloc type.  */
  int reloc_type;
#endif

  /* Attributes.
     This should, but technically needn't, appear last.  It is a variable sized
     array in that one architecture may have 1 nonbool attribute and another
     may have more.  Having this last means the non-architecture specific code
     needn't care, now or tomorrow.  The goal is to eventually record
     attributes in their raw form, evaluate them at run-time, and cache the
     values, so this worry will go away anyway.  */
#ifndef CGEN_OPERAND_NBOOL_ATTRS
#define CGEN_OPERAND_NBOOL_ATTRS 1
#endif
  CGEN_ATTR_TYPE (CGEN_OPERAND_NBOOL_ATTRS) attrs;
#define CGEN_OPERAND_ATTRS(operand) (&(operand)->attrs)
} CGEN_OPERAND;

/* Return value of attribute ATTR in OPERAND.  */

#define CGEN_OPERAND_ATTR_VALUE(operand, attr) \
CGEN_ATTR_VALUE ((operand), CGEN_OPERAND_ATTRS (operand), (attr))

/* The operand instance table refers to this directly.  */
extern const CGEN_OPERAND CGEN_SYM (operand_table)[];

enum cgen_operand_type;

#define CGEN_OPERAND_INDEX(cd, operand) ((operand) - (cd)->operand_table)
/* FIXME: Rename, arch-desc.h defines this as the typedef of the enum.  */
#define CGEN_OPERAND_TYPE(cd, operand) \
  ((enum cgen_operand_type) CGEN_OPERAND_INDEX ((cd), (operand)))
#define CGEN_OPERAND_ENTRY(cd, n) (& (cd)->operand_table[n])

/* Instruction operand instances.

   For each instruction, a list of the hardware elements that are read and
   written are recorded.  */

/* The type of the instance.  */

enum cgen_opinst_type {
  /* End of table marker.  */
  CGEN_OPINST_END = 0,
  CGEN_OPINST_INPUT, CGEN_OPINST_OUTPUT
};

typedef struct
{
  /* The type of this operand.  */
  enum cgen_opinst_type type;

  /* Name of operand.  */
  const char *name;

  /* The hardware element referenced.  */
  const CGEN_HW_ENTRY *hw;

  /* The mode in which the operand is being used.  */
  enum cgen_mode mode;

  /* The operand table entry or NULL if there is none (i.e. an explicit
     hardware reference).  */
  const CGEN_OPERAND *operand;

  /* If `operand' is NULL, the index (e.g. into array of registers).  */
  int index;

  /* Attributes.
     ??? This perhaps should be a real attribute struct but there's
     no current need, so we save a bit of space and just have a set of
     flags.  The interface is such that this can easily be made attributes
     should it prove useful.  */
  unsigned int attrs;
#define CGEN_OPINST_ATTRS(opinst) ((opinst)->attrs)
/* Return value of attribute ATTR in OPINST.  */
#define CGEN_OPINST_ATTR(opinst, attr) \
((CGEN_OPINST_ATTRS (opinst) & (attr)) != 0)
/* Operand is conditionally referenced (read/written).  */
#define CGEN_OPINST_COND_REF 1
} CGEN_OPINST;

/* Syntax string.

   Each insn format and subexpression has one of these.

   The syntax "string" consists of characters (n > 0 && n < 128), and operand
   values (n >= 128), and is terminated by 0.  Operand values are 128 + index
   into the operand table.  The operand table doesn't exist in C, per se, as
   the data is recorded in the parse/insert/extract/print switch statements. */

#ifndef CGEN_MAX_SYNTAX_BYTES
#define CGEN_MAX_SYNTAX_BYTES 16
#endif

typedef struct
{
  unsigned char syntax[CGEN_MAX_SYNTAX_BYTES];
} CGEN_SYNTAX;

#define CGEN_SYNTAX_STRING(syn) (syn->syntax)
#define CGEN_SYNTAX_CHAR_P(c) ((c) < 128)
#define CGEN_SYNTAX_CHAR(c) (c)
#define CGEN_SYNTAX_FIELD(c) ((c) - 128)
#define CGEN_SYNTAX_MAKE_FIELD(c) ((c) + 128)

/* ??? I can't currently think of any case where the mnemonic doesn't come
   first [and if one ever doesn't building the hash tables will be tricky].
   However, we treat mnemonics as just another operand of the instruction.
   A value of 1 means "this is where the mnemonic appears".  1 isn't
   special other than it's a non-printable ASCII char.  */

#define CGEN_SYNTAX_MNEMONIC       1
#define CGEN_SYNTAX_MNEMONIC_P(ch) ((ch) == CGEN_SYNTAX_MNEMONIC)

/* Instruction fields.

   ??? We currently don't allow adding fields at run-time.
   Easy to fix when needed.  */

typedef struct cgen_ifld {
  /* Enum of ifield.  */
  int num;
#define CGEN_IFLD_NUM(f) ((f)->num)

  /* Name of the field, distinguishes it from all other fields.  */
  const char *name;
#define CGEN_IFLD_NAME(f) ((f)->name)

  /* Default offset, in bits, from the start of the insn to the word
     containing the field.  */
  int word_offset;
#define CGEN_IFLD_WORD_OFFSET(f) ((f)->word_offset)

  /* Default length of the word containing the field.  */
  int word_size;
#define CGEN_IFLD_WORD_SIZE(f) ((f)->word_size)

  /* Default starting bit number.
     Whether lsb=0 or msb=0 is determined by CGEN_INSN_LSB0_P.  */
  int start;
#define CGEN_IFLD_START(f) ((f)->start)

  /* Length of the field, in bits.  */
  int length;
#define CGEN_IFLD_LENGTH(f) ((f)->length)

#ifndef CGEN_IFLD_NBOOL_ATTRS
#define CGEN_IFLD_NBOOL_ATTRS 1
#endif
  CGEN_ATTR_TYPE (CGEN_IFLD_NBOOL_ATTRS) attrs;
#define CGEN_IFLD_ATTRS(f) (&(f)->attrs)
} CGEN_IFLD;

/* Return value of attribute ATTR in IFLD.  */
#define CGEN_IFLD_ATTR_VALUE(ifld, attr) \
CGEN_ATTR_VALUE ((ifld), CGEN_IFLD_ATTRS (ifld), (attr))

/* Instruction data.  */

/* Instruction formats.

   Instructions are grouped by format.  Associated with an instruction is its
   format.  Each insn's opcode table entry contains a format table entry.
   ??? There is usually very few formats compared with the number of insns,
   so one can reduce the size of the opcode table by recording the format table
   as a separate entity.  Given that we currently don't, format table entries
   are also distinguished by their operands.  This increases the size of the
   table, but reduces the number of tables.  It's all minutiae anyway so it
   doesn't really matter [at this point in time].

   ??? Support for variable length ISA's is wip.  */

/* Accompanying each iformat description is a list of its fields.  */

typedef struct {
  const CGEN_IFLD *ifld;
#define CGEN_IFMT_IFLD_IFLD(ii) ((ii)->ifld)
} CGEN_IFMT_IFLD;

#ifndef CGEN_MAX_IFMT_OPERANDS
#define CGEN_MAX_IFMT_OPERANDS 1
#endif

typedef struct
{
  /* Length that MASK and VALUE have been calculated to
     [VALUE is recorded elsewhere].
     Normally it is CGEN_BASE_INSN_BITSIZE.  On [V]LIW architectures where
     the base insn size may be larger than the size of an insn, this field is
     less than CGEN_BASE_INSN_BITSIZE.  */
  unsigned char mask_length;
#define CGEN_IFMT_MASK_LENGTH(ifmt) ((ifmt)->mask_length)

  /* Total length of instruction, in bits.  */
  unsigned char length;
#define CGEN_IFMT_LENGTH(ifmt) ((ifmt)->length)

  /* Mask to apply to the first MASK_LENGTH bits.
     Each insn's value is stored with the insn.
     The first step in recognizing an insn for disassembly is
     (opcode & mask) == value.  */
  CGEN_INSN_INT mask;
#define CGEN_IFMT_MASK(ifmt) ((ifmt)->mask)

  /* Instruction fields.
     +1 for trailing NULL.  */
  CGEN_IFMT_IFLD iflds[CGEN_MAX_IFMT_OPERANDS + 1];
#define CGEN_IFMT_IFLDS(ifmt) ((ifmt)->iflds)
} CGEN_IFMT;

/* Instruction values.  */

typedef struct
{
  /* The opcode portion of the base insn.  */
  CGEN_INSN_INT base_value;

#ifdef CGEN_MAX_EXTRA_OPCODE_OPERANDS
  /* Extra opcode values beyond base_value.  */
  unsigned long ifield_values[CGEN_MAX_EXTRA_OPCODE_OPERANDS];
#endif
} CGEN_IVALUE;

/* Instruction opcode table.
   This contains the syntax and format data of an instruction.  */

/* ??? Some ports already have an opcode table yet still need to use the rest
   of what cgen_insn has.  Plus keeping the opcode data with the operand
   instance data can create a pretty big file.  So we keep them separately.
   Not sure this is a good idea in the long run.  */

typedef struct
{
  /* Indices into parse/insert/extract/print handler tables.  */
  struct cgen_opcode_handler handlers;
#define CGEN_OPCODE_HANDLERS(opc) (& (opc)->handlers)

  /* Syntax string.  */
  CGEN_SYNTAX syntax;
#define CGEN_OPCODE_SYNTAX(opc) (& (opc)->syntax)

  /* Format entry.  */
  const CGEN_IFMT *format;
#define CGEN_OPCODE_FORMAT(opc) ((opc)->format)
#define CGEN_OPCODE_MASK_BITSIZE(opc) CGEN_IFMT_MASK_LENGTH (CGEN_OPCODE_FORMAT (opc))
#define CGEN_OPCODE_BITSIZE(opc) CGEN_IFMT_LENGTH (CGEN_OPCODE_FORMAT (opc))
#define CGEN_OPCODE_IFLDS(opc) CGEN_IFMT_IFLDS (CGEN_OPCODE_FORMAT (opc))

  /* Instruction opcode value.  */
  CGEN_IVALUE value;
#define CGEN_OPCODE_VALUE(opc) (& (opc)->value)
#define CGEN_OPCODE_BASE_VALUE(opc) (CGEN_OPCODE_VALUE (opc)->base_value)
#define CGEN_OPCODE_BASE_MASK(opc) CGEN_IFMT_MASK (CGEN_OPCODE_FORMAT (opc))
} CGEN_OPCODE;

/* Instruction attributes.
   This is made a published type as applications can cache a pointer to
   the attributes for speed.  */

#ifndef CGEN_INSN_NBOOL_ATTRS
#define CGEN_INSN_NBOOL_ATTRS 1
#endif
typedef CGEN_ATTR_TYPE (CGEN_INSN_NBOOL_ATTRS) CGEN_INSN_ATTR_TYPE;

/* Enum of architecture independent attributes.  */

#ifndef CGEN_ARCH
/* ??? Numbers here are recorded in two places.  */
typedef enum cgen_insn_attr {
  CGEN_INSN_ALIAS = CGEN_ATTR_BOOL_OFFSET
} CGEN_INSN_ATTR;
#endif

/* This struct defines each entry in the instruction table.  */

typedef struct
{
  /* Each real instruction is enumerated.  */
  /* ??? This may go away in time.  */
  int num;
#define CGEN_INSN_NUM(insn) ((insn)->base->num)

  /* Name of entry (that distinguishes it from all other entries).  */
  /* ??? If mnemonics have operands, try to print full mnemonic.  */
  const char *name;
#define CGEN_INSN_NAME(insn) ((insn)->base->name)

  /* Mnemonic.  This is used when parsing and printing the insn.
     In the case of insns that have operands on the mnemonics, this is
     only the constant part.  E.g. for conditional execution of an `add' insn,
     where the full mnemonic is addeq, addne, etc., and the condition is
     treated as an operand, this is only "add".  */
  const char *mnemonic;
#define CGEN_INSN_MNEMONIC(insn) ((insn)->base->mnemonic)

  /* Total length of instruction, in bits.  */
  int bitsize;
#define CGEN_INSN_BITSIZE(insn) ((insn)->base->bitsize)

#if 0 /* ??? Disabled for now as there is a problem with embedded newlines
	 and the table is already pretty big.  Should perhaps be moved
	 to a file of its own.  */
  /* Semantics, as RTL.  */
  /* ??? Plain text or bytecodes?  */
  /* ??? Note that the operand instance table could be computed at run-time
     if we parse this and cache the results.  Something to eventually do.  */
  const char *rtx;
#define CGEN_INSN_RTX(insn) ((insn)->base->rtx)
#endif

  /* Attributes.
     This must appear last.  It is a variable sized array in that one
     architecture may have 1 nonbool attribute and another may have more.
     Having this last means the non-architecture specific code needn't
     care.  The goal is to eventually record attributes in their raw form,
     evaluate them at run-time, and cache the values, so this worry will go
     away anyway.  */
  CGEN_INSN_ATTR_TYPE attrs;
#define CGEN_INSN_ATTRS(insn) (&(insn)->base->attrs)
/* Return value of attribute ATTR in INSN.  */
#define CGEN_INSN_ATTR_VALUE(insn, attr) \
CGEN_ATTR_VALUE ((insn), CGEN_INSN_ATTRS (insn), (attr))
} CGEN_IBASE;

/* Return non-zero if INSN is the "invalid" insn marker.  */

#define CGEN_INSN_INVALID_P(insn) (CGEN_INSN_MNEMONIC (insn) == 0)

/* Main struct contain instruction information.
   BASE is always present, the rest is present only if asked for.  */

struct cgen_insn
{
  /* ??? May be of use to put a type indicator here.
     Then this struct could different info for different classes of insns.  */
  /* ??? A speedup can be had by moving `base' into this struct.
     Maybe later.  */
  const CGEN_IBASE *base;
  const CGEN_OPCODE *opcode;
  const CGEN_OPINST *opinst;
};

/* Instruction lists.
   This is used for adding new entries and for creating the hash lists.  */

typedef struct cgen_insn_list
{
  struct cgen_insn_list *next;
  const CGEN_INSN *insn;
} CGEN_INSN_LIST;

/* Table of instructions.  */

typedef struct
{
  const CGEN_INSN *init_entries;
  unsigned int entry_size; /* since the attribute member is variable sized */
  unsigned int num_init_entries;
  CGEN_INSN_LIST *new_entries;
} CGEN_INSN_TABLE;

/* Return number of instructions.  This includes any added at run-time.  */

extern int cgen_insn_count PARAMS ((CGEN_CPU_DESC));
extern int cgen_macro_insn_count PARAMS ((CGEN_CPU_DESC));

/* Macros to access the other insn elements not recorded in CGEN_IBASE.  */

/* Fetch INSN's operand instance table.  */
/* ??? Doesn't handle insns added at runtime.  */
#define CGEN_INSN_OPERANDS(insn) ((insn)->opinst)

/* Return INSN's opcode table entry.  */
#define CGEN_INSN_OPCODE(insn) ((insn)->opcode)

/* Return INSN's handler data.  */
#define CGEN_INSN_HANDLERS(insn) CGEN_OPCODE_HANDLERS (CGEN_INSN_OPCODE (insn))

/* Return INSN's syntax.  */
#define CGEN_INSN_SYNTAX(insn) CGEN_OPCODE_SYNTAX (CGEN_INSN_OPCODE (insn))

/* Return size of base mask in bits.  */
#define CGEN_INSN_MASK_BITSIZE(insn) \
  CGEN_OPCODE_MASK_BITSIZE (CGEN_INSN_OPCODE (insn))

/* Return mask of base part of INSN.  */
#define CGEN_INSN_BASE_MASK(insn) \
  CGEN_OPCODE_BASE_MASK (CGEN_INSN_OPCODE (insn))

/* Return value of base part of INSN.  */
#define CGEN_INSN_BASE_VALUE(insn) \
  CGEN_OPCODE_BASE_VALUE (CGEN_INSN_OPCODE (insn))

/* Macro instructions.
   Macro insns aren't real insns, they map to one or more real insns.
   E.g. An architecture's "nop" insn may actually be an "mv r0,r0" or
   some such.

   Macro insns can expand to nothing (e.g. a nop that is optimized away).
   This is useful in multi-insn macros that build a constant in a register.
   Of course this isn't the default behaviour and must be explicitly enabled.

   Assembly of macro-insns is relatively straightforward.  Disassembly isn't.
   However, disassembly of at least some kinds of macro insns is important
   in order that the disassembled code preserve the readability of the original
   insn.  What is attempted here is to disassemble all "simple" macro-insns,
   where "simple" is currently defined to mean "expands to one real insn".

   Simple macro-insns are handled specially.  They are emitted as ALIAS's
   of real insns.  This simplifies their handling since there's usually more
   of them than any other kind of macro-insn, and proper disassembly of them
   falls out for free.  */

/* For each macro-insn there may be multiple expansion possibilities,
   depending on the arguments.  This structure is accessed via the `data'
   member of CGEN_INSN.  */

typedef struct cgen_minsn_expansion {
  /* Function to do the expansion.
     If the expansion fails (e.g. "no match") NULL is returned.
     Space for the expansion is obtained with malloc.
     It is up to the caller to free it.  */
  const char * (* fn) PARAMS ((const struct cgen_minsn_expansion *,
			       const char *, const char **, int *,
			       CGEN_OPERAND **));
#define CGEN_MIEXPN_FN(ex) ((ex)->fn)

  /* Instruction(s) the macro expands to.
     The format of STR is defined by FN.
     It is typically the assembly code of the real insn, but it could also be
     the original Scheme expression or a tokenized form of it (with FN being
     an appropriate interpreter).  */
  const char * str;
#define CGEN_MIEXPN_STR(ex) ((ex)->str)
} CGEN_MINSN_EXPANSION;

/* Normal expander.
   When supported, this function will convert the input string to another
   string and the parser will be invoked recursively.  The output string
   may contain further macro invocations.  */

extern const char * cgen_expand_macro_insn
     PARAMS ((CGEN_CPU_DESC, const struct cgen_minsn_expansion *,
	      const char *, const char **, int *, CGEN_OPERAND **));

/* The assembler insn table is hashed based on some function of the mnemonic
   (the actually hashing done is up to the target, but we provide a few
   examples like the first letter or a function of the entire mnemonic).  */

extern CGEN_INSN_LIST * cgen_asm_lookup_insn
     PARAMS ((CGEN_CPU_DESC, const char *));
#define CGEN_ASM_LOOKUP_INSN(cd, string) cgen_asm_lookup_insn ((cd), (string))
#define CGEN_ASM_NEXT_INSN(insn) ((insn)->next)

/* The disassembler insn table is hashed based on some function of machine
   instruction (the actually hashing done is up to the target).  */

extern CGEN_INSN_LIST * cgen_dis_lookup_insn
     PARAMS ((CGEN_CPU_DESC, const char *, CGEN_INSN_INT));
/* FIXME: delete these two */
#define CGEN_DIS_LOOKUP_INSN(cd, buf, value) cgen_dis_lookup_insn ((cd), (buf), (value))
#define CGEN_DIS_NEXT_INSN(insn) ((insn)->next)

/* The CPU description.
   A copy of this is created when the cpu table is "opened".
   All global state information is recorded here.
   Access macros are provided for "public" members.  */

typedef struct cgen_cpu_desc
{
  /* Current machine (a la BFD machine number).  */
  int mach;

  /* Current endian.  */
  enum cgen_endian endian;
#define CGEN_CPU_ENDIAN(cd) ((cd)->endian)

  /* Current insn endian.  */
  enum cgen_endian insn_endian;
#define CGEN_CPU_INSN_ENDIAN(cd) ((cd)->insn_endian)

  /* Hardware elements.  */
  const CGEN_HW_ENTRY *hw_list;

  /* Instruction fields.  */
  const CGEN_IFLD *ifld_table;

  /* Operands.  */
  const CGEN_OPERAND *operand_table;
#define CGEN_CPU_OPERAND_TABLE(cd) ((cd)->operand_table)

  /* Main instruction table.  */
  CGEN_INSN_TABLE insn_table;
#define CGEN_CPU_INSN_TABLE(cd) (& (cd)->insn_table)

  /* Macro instructions are defined separately and are combined with real
     insns during hash table computation.  */
  CGEN_INSN_TABLE macro_insn_table;

  /* Copy of CGEN_INT_INSN_P.  */
  int int_insn_p;

  /* Maximum length of any insn, in bytes.  */
  unsigned int max_insn_size;

  /* Operand parser callback.  */
  cgen_parse_operand_fn * parse_operand_fn;

  /* Parse/insert/extract/print cover fns for operands.  */
  const char * (*parse_operand)
     PARAMS ((CGEN_CPU_DESC, int opindex_, const char **,
	      CGEN_FIELDS *fields_));
#ifdef BFD_VERSION
  const char * (*insert_operand)
     PARAMS ((CGEN_CPU_DESC, int opindex_, CGEN_FIELDS *fields_,
	      CGEN_INSN_BYTES_PTR, bfd_vma pc_));
  int (*extract_operand)
     PARAMS ((CGEN_CPU_DESC, int opindex_, CGEN_EXTRACT_INFO *, CGEN_INSN_INT,
	      CGEN_FIELDS *fields_, bfd_vma pc_));
  void (*print_operand)
     PARAMS ((CGEN_CPU_DESC, int opindex_, PTR info_, CGEN_FIELDS * fields_,
	      void const *attrs_, bfd_vma pc_, int length_));
#else
  const char * (*insert_operand) ();
  int (*extract_operand) ();
  void (*print_operand) ();
#endif
#define CGEN_CPU_PARSE_OPERAND(cd) ((cd)->parse_operand)
#define CGEN_CPU_INSERT_OPERAND(cd) ((cd)->insert_operand)
#define CGEN_CPU_EXTRACT_OPERAND(cd) ((cd)->extract_operand)
#define CGEN_CPU_PRINT_OPERAND(cd) ((cd)->print_operand)

  /* Size of CGEN_FIELDS struct.  */
  unsigned int sizeof_fields;
#define CGEN_CPU_SIZEOF_FIELDS(cd) ((cd)->sizeof_fields)

  /* Set the bitsize field.  */
  void (*set_fields_bitsize) PARAMS ((CGEN_FIELDS *fields_, int size_));
#define CGEN_CPU_SET_FIELDS_BITSIZE(cd) ((cd)->set_fields_bitsize)

  /* CGEN_FIELDS accessors.  */
  int (*get_int_operand)
       PARAMS ((int opindex_, const CGEN_FIELDS *fields_));
  void (*set_int_operand)
       PARAMS ((int opindex_, CGEN_FIELDS *fields_, int value_));
#ifdef BFD_VERSION
  bfd_vma (*get_vma_operand)
       PARAMS ((int opindex_, const CGEN_FIELDS *fields_));
  void (*set_vma_operand)
       PARAMS ((int opindex_, CGEN_FIELDS *fields_, bfd_vma value_));
#else
  long (*get_vma_operand) ();
  void (*set_vma_operand) ();
#endif
#define CGEN_CPU_GET_INT_OPERAND(cd) ((cd)->get_int_operand)
#define CGEN_CPU_SET_INT_OPERAND(cd) ((cd)->set_int_operand)
#define CGEN_CPU_GET_VMA_OPERAND(cd) ((cd)->get_vma_operand)
#define CGEN_CPU_SET_VMA_OPERAND(cd) ((cd)->set_vma_operand)

  /* Instruction parse/insert/extract/print handlers.  */
  /* FIXME: make these types uppercase.  */
  cgen_parse_fn * const *parse_handlers;
  cgen_insert_fn * const *insert_handlers;
  cgen_extract_fn * const *extract_handlers;
  cgen_print_fn * const *print_handlers;
#define CGEN_PARSE_FN(cd, insn)   (cd->parse_handlers[(insn)->opcode->handlers.parse])
#define CGEN_INSERT_FN(cd, insn)  (cd->insert_handlers[(insn)->opcode->handlers.insert])
#define CGEN_EXTRACT_FN(cd, insn) (cd->extract_handlers[(insn)->opcode->handlers.extract])
#define CGEN_PRINT_FN(cd, insn)   (cd->print_handlers[(insn)->opcode->handlers.print])

  /* Return non-zero if insn should be added to hash table.  */
  int (* asm_hash_p) PARAMS ((const CGEN_INSN *));

  /* Assembler hash function.  */
  unsigned int (* asm_hash) PARAMS ((const char *));

  /* Number of entries in assembler hash table.  */
  unsigned int asm_hash_size;

  /* Return non-zero if insn should be added to hash table.  */
  int (* dis_hash_p) PARAMS ((const CGEN_INSN *));

  /* Disassembler hash function.  */
  unsigned int (* dis_hash) PARAMS ((const char *, CGEN_INSN_INT));

  /* Number of entries in disassembler hash table.  */
  unsigned int dis_hash_size;

  /* Assembler instruction hash table.  */
  CGEN_INSN_LIST **asm_hash_table;
  CGEN_INSN_LIST *asm_hash_table_entries;

  /* Disassembler instruction hash table.  */
  CGEN_INSN_LIST **dis_hash_table;
  CGEN_INSN_LIST *dis_hash_table_entries;
} CGEN_CPU_TABLE;

/* wip */
#ifndef CGEN_WORD_ENDIAN
#define CGEN_WORD_ENDIAN(cd) CGEN_CPU_ENDIAN (cd)
#endif
#ifndef CGEN_INSN_WORD_ENDIAN
#define CGEN_INSN_WORD_ENDIAN(cd) CGEN_CPU_INSN_ENDIAN (cd)
#endif

/* Prototypes of major functions.  */
/* FIXME: Move more CGEN_SYM-defined functions into CGEN_CPU_DESC.
   Note the init fns though, as that would drag in things that mightn't be
   used and might not even exist.  */

/* Open an ISA table for use.  */

extern CGEN_CPU_DESC CGEN_SYM (cpu_open) PARAMS ((int, enum cgen_endian));

/* Close it.  */

extern void CGEN_SYM (cpu_close) PARAMS ((CGEN_CPU_DESC));

/* Initialize the opcode table for use.
   Called by init_asm/init_dis.  */

extern void CGEN_SYM (init_opcode_table) PARAMS ((CGEN_CPU_DESC cd_));

/* Initialize the ibld table for use.
   Called by init_asm/init_dis.  */

extern void CGEN_SYM (init_ibld_table) PARAMS ((CGEN_CPU_DESC cd_));

/* Initialize an cpu table for assembler or disassembler use.
   These must be called immediately after cpu_open.  */

extern void CGEN_SYM (init_asm) PARAMS ((CGEN_CPU_DESC));
extern void CGEN_SYM (init_dis) PARAMS ((CGEN_CPU_DESC));

/* Initialize the operand instance table for use.  */

extern void CGEN_SYM (init_opinst_table) PARAMS ((CGEN_CPU_DESC cd_));

/* Change the mach and/or endianness.  */

extern void cgen_set_cpu PARAMS ((CGEN_CPU_DESC, int, enum cgen_endian));

extern const CGEN_INSN * CGEN_SYM (assemble_insn)
     PARAMS ((CGEN_CPU_DESC, const char *, CGEN_FIELDS *,
	      CGEN_INSN_BYTES_PTR, char **));

extern const CGEN_KEYWORD CGEN_SYM (operand_mach);
extern int CGEN_SYM (get_mach) PARAMS ((const char *));

/* Operand index computation.  */
extern const CGEN_INSN * cgen_lookup_insn
     PARAMS ((CGEN_CPU_DESC, const CGEN_INSN * insn_,
	      CGEN_INSN_INT int_value_, unsigned char *bytes_value_,
	      int length_, CGEN_FIELDS *fields_, int alias_p_));
extern void cgen_get_insn_operands
     PARAMS ((CGEN_CPU_DESC, const CGEN_INSN * insn_,
	      const CGEN_FIELDS *fields_, int *indices_));
extern const CGEN_INSN * cgen_lookup_get_insn_operands
     PARAMS ((CGEN_CPU_DESC, const CGEN_INSN *insn_,
	      CGEN_INSN_INT int_value_, unsigned char *bytes_value_,
	      int length_, int *indices_, CGEN_FIELDS *fields_));

/* Get/set fields in the CGEN_FIELDS struct.  */

extern int CGEN_SYM (get_int_operand)
     PARAMS ((int opindex_, const CGEN_FIELDS *fields_));
extern void CGEN_SYM (set_int_operand)
     PARAMS ((int opindex_, CGEN_FIELDS *fields_, int value_));
#ifdef BFD_VERSION /* Don't require bfd.h unnecessarily.  */
extern bfd_vma CGEN_SYM (get_vma_operand)
     PARAMS ((int opindex_, const CGEN_FIELDS *fields_));
extern void CGEN_SYM (set_vma_operand)
     PARAMS ((int opindex_, CGEN_FIELDS *fields_, bfd_vma value_));
#endif

extern const char * CGEN_SYM (parse_operand)
     PARAMS ((CGEN_CPU_DESC, int, const char **, CGEN_FIELDS *));

#ifdef BFD_VERSION /* Don't require bfd.h unnecessarily.  */
extern const char * CGEN_SYM (insert_operand)
     PARAMS ((CGEN_CPU_DESC, int, CGEN_FIELDS *, CGEN_INSN_BYTES_PTR,
	      bfd_vma));
extern int CGEN_SYM (extract_operand)
     PARAMS ((CGEN_CPU_DESC, int, CGEN_EXTRACT_INFO *, CGEN_INSN_INT,
	      CGEN_FIELDS *, bfd_vma));
extern void CGEN_SYM (print_operand)
     PARAMS ((CGEN_CPU_DESC, int opindex_, PTR info_, CGEN_FIELDS * fields_,
	      void const *attrs_, bfd_vma pc_, int length_));
#else
extern const char * CGEN_SYM (insert_operand) ();
extern int CGEN_SYM (extract_operand) ();
extern void CGEN_SYM (print_operand) ();
#endif

/* Cover fns to bfd_get/set.  */

extern CGEN_INSN_INT cgen_get_insn_value
     PARAMS ((CGEN_CPU_DESC, unsigned char *, int));
extern void cgen_put_insn_value
     PARAMS ((CGEN_CPU_DESC, unsigned char *, int, CGEN_INSN_INT));

/* Read in a cpu description file.
   ??? For future concerns, including adding instructions to the assembler/
   disassembler at run-time.  */

extern const char * cgen_read_cpu_file
     PARAMS ((CGEN_CPU_DESC, const char * filename_));

#endif /* CGEN_H */
