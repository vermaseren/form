
#ifndef __MANAGE_H__

#define __MANAGE_H__
 
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <time.h>

typedef long MLONG;
typedef short MWORD;

#define MAXBASES 16 
#define NUMOBJECTS 100
#define MAXINDEXSIZE 33000000L
#define NAMETABLESIZE 1008

int minosread(FILE *f,char *buffer,MLONG size);
int minoswrite(FILE *f,char *buffer,MLONG size);

/*
	ELEMENTSIZE should make a nice number of sizeof(OBJECTS)
	Usually this will be much too much, but there are cases.....
*/
#define ELEMENTSIZE 100

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
	time_t date;                /* Time stamp */
	MLONG tablenumber;          /* Number of table. Refers to name in special index */
	MLONG uncompressed;         /* uncompressed size if compressed. If not: 0 */
	MLONG dummy1;
	MLONG dummy2;
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
#define MCLEANFLAG -2L
#define DIRTYFLAG 1
#define INANDOUT 0
#define INPUTONLY 1
#define OUTPUTONLY 2
#define NOCOMPRESS 4

extern DBASE *dbases[];
extern int numdbases;
extern FILE *inhandle;
extern int withoutflush;

#endif
