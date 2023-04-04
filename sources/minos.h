#ifndef __MANAGE_H__

#define __MANAGE_H__

/** @file minos.h
 *
 *  Contains all needed declarations and definitions for the tablebase
 *  low level file routines. These have been taken from the minos database
 *  system and modified somewhat.
 *
 *  !!!CAUTION!!!
 *  Changes in this file will most likely have consequences for the recovery
 *  mechanism (see checkpoint.c). You need to care for the code in checkpoint.c
 *  as well and modify the code there accordingly!
 */

/* #[ License : */
/*
 *   Copyright (C) 1984-2017 J.A.M. Vermaseren
 *   When using this file you are requested to refer to the publication
 *   J.A.M.Vermaseren "New features of FORM" math-ph/0010025
 *   This is considered a matter of courtesy as the development was paid
 *   for by FOM the Dutch physics granting agency and we would like to
 *   be able to track its scientific use to convince FOM of its value
 *   for the community.
 *
 *   This file is part of FORM.
 *
 *   FORM is free software: you can redistribute it and/or modify it under the
 *   terms of the GNU General Public License as published by the Free Software
 *   Foundation, either version 3 of the License, or (at your option) any later
 *   version.
 *
 *   FORM is distributed in the hope that it will be useful, but WITHOUT ANY
 *   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 *   details.
 *
 *   You should have received a copy of the GNU General Public License along
 *   with FORM.  If not, see <http://www.gnu.org/licenses/>.
 */
/* #] License : */ 

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <time.h>

/*
	The following typedef has been moved to form3.h where all the sizes
	are defined for the various memory models.
	We want MLONG to have a more or less fixed size.
	In form3.h we try to fix it at 8 bytes. This should make files exchangable
	between various 32-bits and 64-bits systems. At 4 bytes it might have
	problems with files of more than 2 Gbytes.

typedef long MLONG;
*/

#define MAXBASES 16 
#ifdef WORDSIZE32
#define NUMOBJECTS 1024
#define MAXINDEXSIZE 1000000000L
#define NAMETABLESIZE 1008
#define ELEMENTSIZE 256
#else
#define NUMOBJECTS 100
#define MAXINDEXSIZE 33000000L
#define NAMETABLESIZE 1008
#define ELEMENTSIZE 128
#endif

int minosread(FILE *f,char *buffer,MLONG size);
int minoswrite(FILE *f,char *buffer,MLONG size);

/*
	ELEMENTSIZE should make a nice number of sizeof(OBJECTS)
	Usually this will be much too much, but there are cases.....
*/

typedef struct iniinfo {
/* should contains only MLONG variables or convertiniinfo should be modified */
	MLONG	entriesinindex;
	MLONG	numberofindexblocks;
	MLONG	firstindexblock;
	MLONG	lastindexblock;
	MLONG	numberoftables;
	MLONG	numberofnamesblocks;
	MLONG	firstnameblock;
	MLONG	lastnameblock;
} INIINFO;

typedef struct objects {
/* if any changes, convertblock should be adapted too!!!! */
	MLONG position;				/* position of RHS= */
	MLONG size;                 /* size on disk (could be compressed) */
	MLONG date;                 /* Time stamp */
	MLONG tablenumber;          /* Number of table. Refers to name in special index */
	MLONG uncompressed;         /* uncompressed size if compressed. If not: 0 */
	MLONG spare1;
	MLONG spare2;
	MLONG spare3;
	char element[ELEMENTSIZE];  /* table element in character form */
} OBJECTS;

typedef struct indexblock {
	MLONG	flags;
	MLONG	previousblock;
	MLONG	position;
	OBJECTS	objects[NUMOBJECTS];
} INDEXBLOCK;

typedef struct nameblock {
	MLONG	previousblock;
	MLONG	position;
	char	names[NAMETABLESIZE];
} NAMESBLOCK;

typedef struct dbase {
	INIINFO		info;
	MLONG		mode;
	MLONG		tablenamessize;
	MLONG		topnumber;
	MLONG		tablenamefill;
	INDEXBLOCK	**iblocks;
	NAMESBLOCK  **nblocks;
	FILE		*handle;
	char		*name;
	char		*fullname;
	char		*tablenames;
} DBASE;

/*
typedef int (*SFUN)(char *);
typedef struct compile {
	char *keyword;
	SFUN func;
} MCFUNCTION;
 */
#define TODISK 0
#define FROMDISK 1
#define MDIRTYFLAG 1
#define MCLEANFLAG (~MDIRTYFLAG)
#define INANDOUT 0
#define INPUTONLY 1
#define OUTPUTONLY 2
#define NOCOMPRESS 4

extern int withoutflush;

#endif
