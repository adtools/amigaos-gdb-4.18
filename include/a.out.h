/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)a.out.h	5.6 (Berkeley) 4/30/91
 */

#ifndef	_AOUT_H_
#define	_AOUT_H_

#if defined(hp300) || defined(i386)
#define	__LDPGSZ	4096
#endif
#if defined(tahoe) || defined(vax)
#define	__LDPGSZ	1024
#endif
#ifdef __amigaos__
#define __LDPGSZ	8192
#endif

/* Valid magic number check. */
#define	N_BADMAG(ex) \
	((ex).a_magic != NMAGIC && (ex).a_magic != OMAGIC && \
	    (ex).a_magic != ZMAGIC)

/* Address of the bottom of the text segment. */
#define N_TXTADDR(ex)	((ex).a_magic == ZMAGIC ? __LDPGSZ : 0)

/* Address of the bottom of the data segment. */
#define N_DATADDR(ex) \
	(N_TXTADDR(ex) + ((ex).a_magic == OMAGIC ? (ex).a_text \
	: __LDPGSZ + ((ex).a_text - 1 & ~(__LDPGSZ - 1))))

#define N_BSSADDR(ex) (N_DATADDR(ex)+(ex).a_data)

/* Text segment offset. */
#define	N_TXTOFF(ex) \
	((ex).a_magic == ZMAGIC ? 0 : sizeof(struct exec))

/* Data segment offset. */
#define	N_DATOFF(ex) \
	(N_TXTOFF(ex) + ((ex).a_magic != ZMAGIC ? (ex).a_text \
	: __LDPGSZ + ((ex).a_text - 1 & ~(__LDPGSZ - 1))))

/* Symbol table offset. */
#define N_SYMOFF(ex) \
	(N_TXTOFF(ex) + (ex).a_text + (ex).a_data + (ex).a_trsize + \
	    (ex).a_drsize)

/* String table offset. */
#define	N_STROFF(ex) 	(N_SYMOFF(ex) + (ex).a_syms)

/* Relocation format. */
struct relocation_info {
	int r_address;			/* offset in text or data segment */
	unsigned int r_symbolnum : 24,	/* ordinal number of add symbol */
			 r_pcrel :  1,	/* 1 if value should be pc-relative */
			r_length :  2,	/* log base 2 of value's width */
			r_extern :  1,	/* 1 if need to add symbol to value */
			r_baserel:  1,	/* 1 if linkage table relative */
			r_jmptable :  1,	/* 1 if pc-relative to jump table */
				 :  2;	/* reserved */
};

/*
 * Symbol table entry format.  The #ifdef's are so that programs including
 * nlist.h can initialize nlist structures statically.
 */
struct nlist {
	union {
		char *n_name;	/* symbol name (in memory) */
		long n_strx;	/* file string table offset (on disk) */
	} n_un;

#define	N_UNDF	0x00		/* undefined */
#define	N_ABS	0x02		/* absolute address */
#define	N_TEXT	0x04		/* text segment */
#define	N_DATA	0x06		/* data segment */
#define	N_BSS	0x08		/* bss segment */
#define	N_INDR	0x0a		/* alias definition */
#define	N_SIZE	0x0c		/* pseudo type, defines a symbol's size */
#define	N_COMM	0x12		/* common reference */
#define	N_FN	0x1e		/* file name (N_EXT on) */
#define	N_WARN	0x1e		/* warning message (N_EXT off) */

#define	N_EXT	0x01		/* external (global) bit, OR'ed in */
#define	N_TYPE	0x1e		/* mask for all the type bits */
	unsigned char n_type;	/* type defines */

	char n_other;		/* spare */
#define	n_hash	n_desc		/* used internally by ld(1); XXX */
	short n_desc;		/* used by stab entries */
	unsigned long n_value;	/* address/value of the symbol */
};

#define	N_FORMAT	"%08x"	/* namelist value format; XXX */
#define	N_STAB		0x0e0	/* mask for debugger symbols -- stab(5) */

#endif /* !_AOUT_H_ */
