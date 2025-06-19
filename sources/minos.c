/** @file minos.c
 * 
 *  These are the low level functions for the database part of the
 *  tablebases. These routines have been copied (and then adapted) from
 *	the minos database program. This file goes together with minos.h
 */
/* #[ License : */
/*
 *   Copyright (C) 1984-2023 J.A.M. Vermaseren
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
/*
  	#[ Includes :

	File contains the lowlevel routines for the management of primitive
	database-like files. The structures are explained in the file manage.h
	Original file for minos made by J.Vermaseren, april-1994.

	Note: the minos primitives for writing are never invoked in parallel.
*/

#include "form3.h"
#include "minos.h"
int withoutflush = 0;

/*
  	#] Includes : 
  	#[ Variables :
*/
 
static INDEXBLOCK scratchblock;
static NAMESBLOCK scratchnamesblock;
 
#define CFD(y,s,type,x,j) for(x=0,j=0;j<((int)sizeof(type));j++) \
	{x=(x<<8)+((*s++)&0x00FF);} y=x;
#define CTD(y,s,type,x,j) x=y;for(j=sizeof(type)-1;j>=0;j--){s[j]=x&0xFF; \
                      x>>=8;} s += sizeof(type);

/*
  	#] Variables : 
  	#[ Utilities :
 		#[ minosread :
*/

int minosread(FILE *f,char *buffer,MLONG size)
{
	MLONG x;
	while ( size > 0 ) {
		x = fread(buffer,sizeof(char),size,f);
		if ( x <= 0 ) return(-1);
		buffer += x;
		size -= x;
	}
	return(0);
}

/*
 		#] minosread : 
 		#[ minoswrite :
*/

int minoswrite(FILE *f,char *buffer,MLONG size)
{
	MLONG x;
	while ( size > 0 ) {
		x = fwrite(buffer,sizeof(char),size,f);
		if ( x <= 0 ) return(-1);
		buffer += x;
		size -= x;
	}
	if ( withoutflush == 0 ) fflush(f);
	return(0);
}

/*
 		#] minoswrite : 
 		#[ str_dup :
*/

char *str_dup(char *str)
{
	char *s, *t;
	int i;
	s = str;
	while ( *s ) s++;
	i = s - str + 1;
	if ( ( s = (char *)Malloc1((size_t)i,"a string copy") ) == 0 ) return(0);
	t = s;
	while ( *str ) *t++ = *str++;
	*t = 0;
	return(s);
}

/*
 		#] str_dup : 
 		#[ convertblock :
*/

void convertblock(INDEXBLOCK *in,INDEXBLOCK *out,int mode)
{
	char *s,*t;
	MLONG i, x;
	int j;
	OBJECTS *obj;
	switch ( mode ) {
	case TODISK:
		s = (char *)out;
		CTD(in->flags,s,MLONG,x,j)
		CTD(in->previousblock,s,MLONG,x,j)
		CTD(in->position,s,MLONG,x,j)
		for ( i = 0, obj = in->objects; i < NUMOBJECTS; i++, obj++ ) {
			CTD(obj->position,s,MLONG,x,j)
			CTD(obj->size,s,MLONG,x,j)
			CTD(obj->date,s,MLONG,x,j)
			CTD(obj->tablenumber,s,MLONG,x,j)
			CTD(obj->uncompressed,s,MLONG,x,j)
			CTD(obj->spare1,s,MLONG,x,j)
			CTD(obj->spare2,s,MLONG,x,j)
			CTD(obj->spare3,s,MLONG,x,j)
			t = obj->element;
			for ( j = 0; j < ELEMENTSIZE; j++ ) *s++ = *t++;
		}
		break;
	case FROMDISK:
		s = (char *)in;
		CFD(out->flags,s,MLONG,x,j)
		CFD(out->previousblock,s,MLONG,x,j)
		CFD(out->position,s,MLONG,x,j)
		for ( i = 0, obj = out->objects; i < NUMOBJECTS; i++, obj++ ) {
			CFD(obj->position,s,MLONG,x,j)
			CFD(obj->size,s,MLONG,x,j)
			CFD(obj->date,s,MLONG,x,j)
			CFD(obj->tablenumber,s,MLONG,x,j)
			CFD(obj->uncompressed,s,MLONG,x,j)
			CFD(obj->spare1,s,MLONG,x,j)
			CFD(obj->spare2,s,MLONG,x,j)
			CFD(obj->spare3,s,MLONG,x,j)
			t = obj->element;
			for ( j = 0; j < ELEMENTSIZE; j++ ) *t++ = *s++;
		}
		break;
	}
}

/*
 		#] convertblock : 
 		#[ convertnamesblock :
*/

void convertnamesblock(NAMESBLOCK *in,NAMESBLOCK *out,int mode)
{
	char *s;
	MLONG x;
	int j;
	switch ( mode ) {
	case TODISK:
		s = (char *)out;
		CTD(in->previousblock,s,MLONG,x,j)
		CTD(in->position,s,MLONG,x,j)
		for ( j = 0; j < NAMETABLESIZE; j++ ) out->names[j] = in->names[j];
		break;
	case FROMDISK:
		s = (char *)in;
		CFD(out->previousblock,s,MLONG,x,j)
		CFD(out->position,s,MLONG,x,j)
		for ( j = 0; j < NAMETABLESIZE; j++ ) out->names[j] = in->names[j];
		break;
	}
}

/*
 		#] convertnamesblock : 
 		#[ convertiniinfo :
*/

void convertiniinfo(INIINFO *in,INIINFO *out,int mode)
{
	char *s;
	MLONG i, x, *y;
	int j;
	switch ( mode ) {
	case TODISK:
		s = (char *)out; y = (MLONG *)in;
		for ( i = sizeof(INIINFO)/sizeof(MLONG); i > 0; i-- ) {
			CTD(*y,s,MLONG,x,j)
			y++;
		}
		break;
	case FROMDISK:
		s = (char *)in; y = (MLONG *)out;
		for ( i = sizeof(INIINFO)/sizeof(MLONG); i > 0; i-- ) {
			CFD(*y,s,MLONG,x,j)
			y++;
		}
		break;
	}
}

/*
 		#] convertiniinfo : 
 		#[ LocateBase :
*/

FILE *LocateBase(char **name, char **newname, char *iomode)
{
	FILE *handle;
	int  namesize, i;
	UBYTE *s, *to, *u1, *u2, *indir;	
	
	if ( ( handle = fopen(*name,iomode) ) != 0 ) {
		*newname = (char *)strDup1((UBYTE *)(*name),"LocateBase");
		return(handle);
	}
	namesize = 2; s = (UBYTE *)(*name);
	while ( *s ) { s++; namesize++; }
	indir = AM.IncDir;
	if ( indir ) {
		s = indir; i = 0;
		while ( *s ) { s++; i++; }
		*newname = (char *)Malloc1(namesize+i,"LocateBase");
		s = indir; to = (UBYTE *)(*newname);
		while ( *s ) *to++ = *s++;
		if ( to > (UBYTE *)(*newname) && to[-1] != SEPARATOR ) *to++ = SEPARATOR;
		s = (UBYTE *)(*name);
		while ( *s ) *to++ = *s++;
		*to = 0;
		if ( ( handle = fopen(*newname,iomode) ) != 0 ) {
			return(handle);
		}
		M_free(*newname,"LocateBase, incdir/file");
	}
	if ( AM.Path ) {
		u1 = AM.Path;
		while ( *u1 ) {
			u2 = u1; i = 0;
			while ( *u1 && *u1 != ':' ) {
				if ( *u1 == '\\' ) u1++;
				u1++; i++;
			}
			*newname = (char *)Malloc1(namesize+i,"LocateBase");
			s = u2; to = (UBYTE *)(*newname);
			while ( s < u1 ) {
				if ( *s == '\\' ) s++;
				*to++ = *s++;
			}
			if ( to > (UBYTE *)(*newname) && to[-1] != SEPARATOR ) *to++ = SEPARATOR;
			s = (UBYTE *)(*name);
			while ( *s ) *to++ = *s++;
			*to = 0;
			if ( ( handle = fopen(*newname,iomode) ) != 0 ) {
				return(handle);
			}
			M_free(*newname,"LocateBase Path/file");
			if ( *u1 ) u1++;
		}
	}
/*	Error1("LocateBase: Cannot find file",*name); */
	return(0);
}

/*
 		#] LocateBase : 
  	#] Utilities : 
  	#[ ReadIndex :
*/

int ReadIndex(DBASE *d)
{
	MLONG i;
	INDEXBLOCK **ib;
	NAMESBLOCK **ina;
	MLONG position, size;
/*
	Allocate the pieces one by one (makes it easier to give it back)
*/
	if ( d->info.numberofindexblocks <= 0 ) return(0);
	if ( sizeof(INDEXBLOCK)*d->info.numberofindexblocks > MAXINDEXSIZE ) {
		MesPrint("We need more than %ld bytes for the index.\n",MAXINDEXSIZE);
		MesPrint("The file %s may not be a proper database\n",d->name);
		return(-1);
	}
	size = sizeof(INDEXBLOCK *)*d->info.numberofindexblocks;
	if ( ( ib = (INDEXBLOCK **)Malloc1(size,"tb,index") ) == 0 ) return(-1);
	for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
		if ( ( ib[i] = (INDEXBLOCK *)Malloc1(sizeof(INDEXBLOCK),"index block") ) == 0 ) {
			for ( --i; i >= 0; i-- ) M_free(ib[i],"tb,indexblock");
			M_free(ib,"tb,index");
			return(-1);
		}
	}
	size = sizeof(NAMESBLOCK *)*d->info.numberofnamesblocks;
	if ( ( ina = (NAMESBLOCK **)Malloc1(size,"tb,indexnames") ) == 0 ) return(-1);
	for ( i = 0; i < d->info.numberofnamesblocks; i++ ) {
		if ( ( ina[i] = (NAMESBLOCK *)Malloc1(sizeof(NAMESBLOCK),"index names block") ) == 0 ) {
			for ( --i; i >= 0; i-- ) M_free(ina[i],"index names block");
			M_free(ina,"tb,indexnames");
			for ( i = 0; i < d->info.numberofindexblocks; i++ ) M_free(ib[i],"tb,indexblock");
			M_free(ib,"tb,index");
			return(-1);
		}
	}
/*
	Read the index blocks, from the back to the front. The links are only
	reliable that way.
*/
	position = d->info.lastindexblock;
	for ( i = d->info.numberofindexblocks - 1; i >= 0; i-- ) {
		fseek(d->handle,position,SEEK_SET);
		if ( minosread(d->handle,(char *)(&scratchblock),sizeof(INDEXBLOCK)) ) {
			MesPrint("Error while reading file %s\n",d->name);
thisiswrong:
			for ( i = 0; i < d->info.numberofnamesblocks; i++ ) M_free(ina[i],"index names block");
			M_free(ina,"tb,indexnames");
			for ( i = 0; i < d->info.numberofindexblocks; i++ ) M_free(ib[i],"tb,indexblock");
			M_free(ib,"tb,index");
			return(-1);
		}
		convertblock(&scratchblock,ib[i],FROMDISK);
		if ( ib[i]->position != position ||
		( ib[i]->previousblock <= 0 && i > 0 ) ) {
			MesPrint("File %s has inconsistent contents\n",d->name);
			goto thisiswrong;
		}
		position = ib[i]->previousblock;
	}
	d->info.firstindexblock = ib[0]->position;
	for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
		ib[i]->flags &= MCLEANFLAG;
	}
/*
	Read the names blocks, from the back to the front. The links are only
	reliable that way.
*/
	position = d->info.lastnameblock;
	for ( i = d->info.numberofnamesblocks - 1; i >= 0; i-- ) {
		fseek(d->handle,position,SEEK_SET);
		if ( minosread(d->handle,(char *)(&scratchnamesblock),sizeof(NAMESBLOCK)) ) {
			MesPrint("Error while reading file %s\n",d->name);
			goto thisiswrong;
		}
		convertnamesblock(&scratchnamesblock,ina[i],FROMDISK);
		if ( ina[i]->position != position ||
		( ina[i]->previousblock <= 0 && i > 0 ) ) {
			MesPrint("File %s has inconsistent contents\n",d->name);
			goto thisiswrong;
		}
		position = ina[i]->previousblock;
	}
	d->info.firstnameblock = ina[0]->position;
/*
	Give the old info back to the system.
*/
	if ( d->iblocks ) {
		for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
			if ( d->iblocks[i] ) M_free(d->iblocks[i],"d->iblocks[i]");
		}
		M_free(d->iblocks,"d->iblocks");
	}
	if ( d->nblocks ) {
		for ( i = 0; i < d->info.numberofnamesblocks; i++ ) {
			if ( d->nblocks[i] ) M_free(d->nblocks[i],"d->nblocks[i]");
		}
		M_free(d->nblocks,"d->nblocks");
	}
/*
	And substitute the new blocks
*/
	d->iblocks = ib;
	d->nblocks = ina;
	return(0);
}

/*
  	#] ReadIndex : 
  	#[ WriteIndexBlock :
*/

int WriteIndexBlock(DBASE *d,MLONG num)
{
	if ( num >= d->info.numberofindexblocks ) {
		MesPrint("Illegal number specified for number of index blocks\n");
		return(-1);
	}
	fseek(d->handle,d->iblocks[num]->position,SEEK_SET);
	convertblock(d->iblocks[num],&scratchblock,TODISK);
	if ( minoswrite(d->handle,(char *)(&scratchblock),sizeof(INDEXBLOCK)) ) {
		MesPrint("Error while writing an index block in file %s\n",d->name);
		MesPrint("File may be unreliable now\n");
		return(-1);
	}
	return(0);
}

/*
  	#] WriteIndexBlock : 
  	#[ WriteNamesBlock :
*/

int WriteNamesBlock(DBASE *d,MLONG num)
{
	if ( num >= d->info.numberofnamesblocks ) {
		MesPrint("Illegal number specified for number of names blocks\n");
		return(-1);
	}
	fseek(d->handle,d->nblocks[num]->position,SEEK_SET);
	convertnamesblock(d->nblocks[num],&scratchnamesblock,TODISK);
	if ( minoswrite(d->handle,(char *)(&scratchnamesblock),sizeof(NAMESBLOCK)) ) {
		MesPrint("Error while writing a names block in file %s\n",d->name);
		MesPrint("File may be unreliable now\n");
		return(-1);
	}
	return(0);
}

/*
  	#] WriteNamesBlock : 
  	#[ WriteIndex :

	Problem here is to get the links right.
*/

int WriteIndex(DBASE *d)
{
	MLONG i, position;
	if ( d->iblocks == 0 ) return(0);
	if ( d->nblocks == 0 ) return(0);
	for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
		if ( d->iblocks[i] == 0 ) {
			MesPrint("Error: unassigned index blocks. Cannot write\n");
			return(-1);
		}
	}
	for ( i = 0; i < d->info.numberofnamesblocks; i++ ) {
		if ( d->nblocks[i] == 0 ) {
			MesPrint("Error: unassigned names blocks. Cannot write\n");
			return(-1);
		}
	}
	d->info.lastindexblock = -1;
	for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
		position = d->iblocks[i]->position;
		if ( position <= 0 ) {
			fseek(d->handle,0,SEEK_END);
			position = ftell(d->handle);
			d->iblocks[i]->position = position;
			if ( i <= 0 ) d->iblocks[i]->previousblock = -1;
			else d->iblocks[i]->previousblock = d->iblocks[i-1]->position;
		}
		else fseek(d->handle,position,SEEK_SET);
		convertblock(d->iblocks[i],&scratchblock,TODISK);
		if ( minoswrite(d->handle,(char *)(&scratchblock),sizeof(INDEXBLOCK)) ) {
			MesPrint("Error while writing index of file %s",d->name);
			d->iblocks[i]->position = -1;
			return(-1);
		}
		d->info.lastindexblock = position;
	}
	d->info.lastnameblock = -1;
	for ( i = 0; i < d->info.numberofnamesblocks; i++ ) {
		position = d->nblocks[i]->position;
		if ( position <= 0 ) {
			fseek(d->handle,0,SEEK_END);
			position = ftell(d->handle);
			d->nblocks[i]->position = position;
			if ( i <= 0 ) d->nblocks[i]->previousblock = -1;
			else d->nblocks[i]->previousblock = d->nblocks[i-1]->position;
		}
		else fseek(d->handle,position,SEEK_SET);
		convertnamesblock(d->nblocks[i],&scratchnamesblock,TODISK);
		if ( minoswrite(d->handle,(char *)(&scratchnamesblock),sizeof(NAMESBLOCK)) ) {
			MesPrint("Error while writing index of file %s",d->name);
			d->nblocks[i]->position = -1;
			return(-1);
		}
		d->info.lastnameblock = position;
	}
	return(0);
}

/*
  	#] WriteIndex : 
  	#[ WriteIniInfo :
*/

int WriteIniInfo(DBASE *d)
{
	INIINFO inf;
	fseek(d->handle,0,SEEK_SET);
	convertiniinfo(&(d->info),&inf,TODISK);
	if ( minoswrite(d->handle,(char *)(&inf),sizeof(INIINFO)) ) {
		MesPrint("Error while writing masterindex of file %s",d->name);
		return(-1);
	}
	return(0);
}

/*
  	#] WriteIniInfo : 
  	#[ ReadIniInfo :
*/

int ReadIniInfo(DBASE *d)
{
	INIINFO inf;
	fseek(d->handle,0,SEEK_SET);
	if ( minosread(d->handle,(char *)(&inf),sizeof(INIINFO)) ) {
		MesPrint("Error while reading masterindex of file %s",d->name);
		return(-1);
	}
	convertiniinfo(&inf,&(d->info),FROMDISK);
	if ( d->info.entriesinindex < 0
	|| d->info.numberofindexblocks < 0
	|| d->info.lastindexblock < 0 ) {
		MesPrint("The file %s is not a proper database\n",d->name);
		return(-1);
	}
	return(0);
}

/*
  	#] ReadIniInfo : 
  	#[ GetDbase :
*/

DBASE *GetDbase(char *filename, MLONG rwmode)
{
	FILE *f;
	DBASE *d;
	char *newname;
	if ( rwmode == 0 ) {
		if ( ( f = LocateBase(&filename,&newname,"rb") ) == 0 ) {
	
			MesPrint("&Trying to open non-existent TableBase in readonly mode: %s", filename);
			Terminate(-1);
		}
	} else {
		if ( ( f = LocateBase(&filename,&newname,"r+b") ) == 0 ) {
	
			return(NewDbase(filename,0));
		}	
	}

/*	setbuf(f,0); */
	d = (DBASE *)From0List(&(AC.TableBaseList));
	d->mode = 0;
	d->tablenamessize = 0;
	d->topnumber = 0;
	d->tablenamefill = 0;
	d->iblocks = 0;
	d->nblocks = 0;
	d->tablenames = 0;
	d->rwmode = rwmode;

	d->info.entriesinindex = 0;
	d->info.numberofindexblocks = 0;
	d->info.firstindexblock = 0;
	d->info.lastindexblock = 0;
	d->info.numberoftables = 0;
	d->info.numberofnamesblocks = 0;
	d->info.firstnameblock = 0;
	d->info.lastnameblock = 0;

	d->name = str_dup(filename); /* For the moment just for the error messages */
	d->handle = f;
	if ( ReadIniInfo(d) || ReadIndex(d) ) { M_free(d,"index-d"); fclose(f); return(0); }
	if ( ComposeTableNames(d) < 0 ) { FreeTableBase(d); fclose(f); return(0); }
	// free allocation from previous str_dup
	M_free(d->name, "from str_dup");
	d->name = str_dup(filename);
	d->fullname = newname;
	return(d);
}

/*
  	#] GetDbase : 
  	#[ NewDbase :

	Creates a new database with 'number' entries in the index.
*/

DBASE *NewDbase(char *name,MLONG number)
{
	FILE *f;
	DBASE *d;
	MLONG numblocks, numnameblocks, i;
	char *s;
/*----------change 10-feb-2003 */
	int j, jj;
	MLONG t = (MLONG)(time(0));
/*-----------------------------*/
	if ( number < 0 ) number = 0;
	if ( ( f = fopen(name,"w+b") ) == 0 ) {
		MesPrint("Could not create a new file with name %s\n",name);
		return(0);
	}
	numblocks = (number+NUMOBJECTS-1)/NUMOBJECTS;
	numnameblocks = 1;
	if ( numblocks <= 0 ) numblocks = 1;
	if ( numnameblocks <= 0 ) numnameblocks = 1;
	d = (DBASE *)From0List(&(AC.TableBaseList));
	if ( ( d->iblocks = (INDEXBLOCK **)Malloc1(numblocks*sizeof(INDEXBLOCK *),
	"new database") ) == 0 ) {
		NumTableBases--;
		return(0);
	}
	d->tablenames = 0;
	d->tablenamessize = 0;
	d->topnumber = 0;
	d->tablenamefill = 0;
	d->rwmode = 1;

	d->mode = 0;
	if ( ( d->nblocks = (NAMESBLOCK **)Malloc1(sizeof(NAMESBLOCK *)*numnameblocks,
	"new database") ) == 0 ) {
		M_free(d->iblocks,"new database");
		NumTableBases--;
		return(0);
	}
	if ( ( f = fopen(name,"w+b") ) == 0 ) {
		MesPrint("Could not create new file %s\n",name);
		NumTableBases--;
		return(0);
	}
/*	setbuf(f,0); */
	d->name = str_dup(name);
	d->fullname = str_dup(name);
	d->handle = f;

	d->info.entriesinindex = number;
	d->info.numberofindexblocks = numblocks;
	d->info.numberofnamesblocks = numnameblocks;
	d->info.firstindexblock = 0;
	d->info.lastindexblock = 0;
	d->info.numberoftables = 0;
	d->info.firstnameblock = 0;
	d->info.lastnameblock = 0;

	if ( WriteIniInfo(d) ) {
getout:
		fclose(f);
		remove(d->fullname);
		if ( d->name ) { M_free(d->name,"name tablebase"); d->name = 0; }
		if ( d->fullname ) { M_free(d->fullname,"fullname tablebase"); d->fullname = 0; }
		M_free(d->nblocks,"new database");
		M_free(d->iblocks,"new database");
		NumTableBases--;
		return(0);
	}
	for ( i = 0; i < numblocks; i++ ) {
		if ( ( d->iblocks[i] = (INDEXBLOCK *)Malloc1(sizeof(INDEXBLOCK),
		"index blocks of new database") ) == 0 ) {
			while ( --i >= 0 ) M_free(d->iblocks[i],"index blocks of new database");
			goto getout;
		}
		if ( i > 0 ) d->iblocks[i]->previousblock = d->iblocks[i-1]->position;
		else d->iblocks[i]->previousblock = -1;
		d->iblocks[i]->position = ftell(f);
		// Initialise, to keep valgrind happy
		d->iblocks[i]->flags = -1;
/*----------change 10-feb-2003 */
/*
			Zero things properly. We don't want garbage in the file.
*/
			for ( j = 0; j < NUMOBJECTS; j++ ) {
				d->iblocks[i]->objects[j].date = t;
				d->iblocks[i]->objects[j].size = 0;
				d->iblocks[i]->objects[j].position = -1;
				d->iblocks[i]->objects[j].tablenumber = 0;
				d->iblocks[i]->objects[j].uncompressed = 0;
				d->iblocks[i]->objects[j].spare1 = 0;
				d->iblocks[i]->objects[j].spare2 = 0;
				d->iblocks[i]->objects[j].spare3 = 0;
				for ( jj = 0; jj < ELEMENTSIZE; jj++ ) d->iblocks[i]->objects[j].element[jj] = 0;
			}
		convertblock(d->iblocks[i],&scratchblock,TODISK);
		if ( minoswrite(d->handle,(char *)(&scratchblock),sizeof(INDEXBLOCK)) ) {
			MesPrint("Error while writing new index blocks\n");
			goto getout;
		}
	}
	for ( i = 0; i < numnameblocks; i++ ) {
		if ( ( d->nblocks[i] = (NAMESBLOCK *)Malloc1(sizeof(NAMESBLOCK),
		"names blocks of new database") ) == 0 ) {
			while ( --i >= 0 ) { M_free(d->nblocks[i],"names blocks of new database"); }
			for ( i = 0; i < numblocks; i++ ) M_free(d->iblocks[i],"index blocks of new database");
			goto getout;
		}
		if ( i > 0 ) d->nblocks[i]->previousblock = d->nblocks[i-1]->position;
		else d->nblocks[i]->previousblock = -1;
		d->nblocks[i]->position = ftell(f);
		s = d->nblocks[i]->names;
		for ( j = 0; j < NAMETABLESIZE; j++ ) *s++ = 0;
		convertnamesblock(d->nblocks[i],&scratchnamesblock,TODISK);
		if ( minoswrite(d->handle,(char *)(&scratchnamesblock),sizeof(NAMESBLOCK)) ) {
			MesPrint("Error while writing new names blocks\n");
			for ( i = 0; i < numnameblocks; i++ ) M_free(d->nblocks[i],"names blocks of new database");
			for ( i = 0; i < numblocks; i++ ) M_free(d->iblocks[i],"index blocks of new database");
			goto getout;
		}
	}
	d->info.firstindexblock = d->iblocks[0]->position;
	d->info.lastindexblock = d->iblocks[numblocks-1]->position;
	d->info.firstnameblock = d->nblocks[0]->position;
	d->info.lastnameblock = d->nblocks[numnameblocks-1]->position;
	if ( WriteIniInfo(d) ) {
		for ( i = 0; i < numnameblocks; i++ ) M_free(d->nblocks[i],"names blocks of new database");
		for ( i = 0; i < numblocks; i++ ) M_free(d->iblocks[i],"index blocks of new database");
		goto getout;
	}
	return(d);
}

/*
  	#] NewDbase : 
  	#[ FreeTableBase :
*/

void FreeTableBase(DBASE *d)
{
	int i, j, *old, *newL;
	LIST *L;
	for ( i = 0; i < d->info.numberofnamesblocks; i++ ) M_free(d->nblocks[i],"nblocks[i]");
	for ( i = 0; i < d->info.numberofindexblocks; i++ ) M_free(d->iblocks[i],"iblocks[i]");
	M_free(d->nblocks,"nblocks");
	M_free(d->iblocks,"iblocks");
	if ( d->tablenames ) M_free(d->tablenames,"d->tablenames");
	if ( d->name ) M_free(d->name,"d->name");
	if ( d->fullname ) M_free(d->fullname,"d->fullname");
	i = d - tablebases;
	if ( i < ( NumTableBases - 1 ) ) {
		L = &(AC.TableBaseList);
		j = ( ( NumTableBases - i - 1 ) * L->size ) / sizeof(int);
		old = (int *)d; newL = (int *)(d+1);
		while ( --j >= 0 ) *newL++ = *old++;
		j = L->size / sizeof(int);
		while ( --j >= 0 ) *newL++ = 0;
	}
	NumTableBases--;
	M_free(d,"tb,d");
}

/*
  	#] FreeTableBase : 
  	#[ ComposeTableNames :

		The nameblocks are supposed to be in memory.
		Hence we have to go through them
*/

int ComposeTableNames(DBASE *d)
{
	MLONG nsize = 0;
	int i, j, k;
	char *s, *t, *ss;
	d->topnumber = 0;
	i = 0; s = d->nblocks[i]->names; j = NAMETABLESIZE;
	while ( *s ) {
	  if ( *s ) d->topnumber++;
	  for ( k = 0; k < 2; k++ ) { /* name and argtail */
		while ( *s ) {
			j--;
			if ( j <= 0 ) {
				i++; if ( i >= d->info.numberofnamesblocks ) goto gotall;
				s = d->nblocks[i]->names; j = NAMETABLESIZE;
			}
			else s++;
		}
		j--;
		if ( j <= 0 ) {
			i++; if ( i >= d->info.numberofnamesblocks ) goto gotall;
			s = d->nblocks[i]->names; j = NAMETABLESIZE;
		}
		else s++;
	  }
	}
gotall:;
	nsize = (d->info.numberofnamesblocks-1)*NAMETABLESIZE +
				(s-d->nblocks[i]->names)+1;
	if ( ( d->tablenames = (char *)Malloc1((2*nsize+30)*sizeof(char),"tablenames") )
		== 0 ) { return(-1); }
	t = d->tablenames;
	d->tablenamessize = 2*nsize+30;
	d->tablenamefill = nsize-1;
	for ( k = 0; k < i; k++ ) {
		ss = d->nblocks[k]->names;
		for ( j = 0; j < NAMETABLESIZE; j++ ) *t++ = *ss++;
	}
	ss = d->nblocks[i]->names;
	while ( ss < s ) *t++ = *ss++;
	*t = 0;
	return(0);
}

/*
  	#] ComposeTableNames : 
  	#[ OpenDbase :
*/

DBASE *OpenDbase(char *filename)
{
	FILE *f;
	DBASE *d;
	char *newname;
	if ( ( f = LocateBase(&filename,&newname,"r+b") ) == 0 ) {
		MesPrint("Cannot open file %s\n",filename);
		return(0);
	}
/*	setbuf(f,0); */
	d = (DBASE *)From0List(&(AC.TableBaseList));
	d->name = filename; /* For the moment just for the error messages */
	d->handle = f;
	if ( ReadIniInfo(d) || ReadIndex(d) ) { M_free(d,"OpenDbase"); fclose(f); return(0); }
	if ( ComposeTableNames(d) ) {
		FreeTableBase(d);
		fclose(f);
		return(0);
	}
	d->name = str_dup(filename);
	d->fullname = newname;
	return(d);
}

/*
  	#] OpenDbase : 
  	#[ AddTableName :

	Adds a name of a table. Writes the namelist to disk.
	Returns the number of this tablename in the database.
	If the name was already in the table we return its value in negative.
	Zero is an error!
*/

MLONG AddTableName(DBASE *d,char *name,TABLES T)
{
	char *s, *t, *tt;
	int namesize, tailsize;
	MLONG newsize, i, num;
/*
	First search for the name in what we have already
*/
	if ( d->tablenames ) {
		num = 0;
		s = d->tablenames;
		while ( *s ) {
			num++;
			t = name;
			while ( ( *s == *t ) && *t ) { s++; t++; }
			if ( *s == *t ) { return(-num); }
			while ( *s ) s++;
			s++;
			while ( *s ) s++;
			s++;
		}
	}
/*
	This name has to be added
*/
	MesPrint("We add the name %s\n",name);
	t = name;
	while ( *t ) { t++; }
	namesize = t-name;
	if ( ( t = (char *)(T->argtail) ) != 0 ) {
		while ( *t ) { t++; }
		tailsize = t - (char *)(T->argtail);
	}
	else { tailsize = 0; }
	if ( d->tablenames == 0 ) {
		if ( ComposeTableNames(d) ) {
			FreeTableBase(d);
			M_free(d,"AddTableName");
			return(0);
		}
	}
	d->info.numberoftables++;
	while ( ( d->tablenamefill+namesize+tailsize+3 > d->tablenamessize )
					 || ( d->tablenames == 0 ) ) {
		newsize = 2*d->tablenamessize + 2*namesize + 2*tailsize + 6;
		if ( ( t = (char *)Malloc1(newsize*sizeof(char),"AddTableName") ) == 0 )
			return(0);
		tt = t;
		if ( d->tablenames ) {
			s = d->tablenames;
			for ( i = 0; i < d->tablenamefill; i++ ) *t++ = *s++;
			*t = 0;
			M_free(d->tablenames,"d->tablenames");
		}
		d->tablenames = tt;
		d->tablenamessize = newsize;
	}
	s = d->tablenames + d->tablenamefill;
	t = name;
	while ( *t ) *s++ = *t++;
	*s++ = 0;
	t = (char *)(T->argtail);
	while ( *t ) *s++ = *t++;
	*s++ = 0;
	*s = 0;
	d->tablenamefill = s - d->tablenames;
	d->topnumber++;
/*
	Now we have to synchronize
*/
	if ( PutTableNames(d) ) return(0);
	return(d->topnumber);
}

/*
  	#] AddTableName : 
  	#[ GetTableName :

	Gets a name of a table.
	Returns the number of this tablename in the database.
	Zero -> error
*/

MLONG GetTableName(DBASE *d,char *name)
{
	char *s, *t;
	MLONG num;
/*
	search for the name in what we have
*/
	if ( d->tablenames ) {
		num = 0;
		s = d->tablenames;
		while ( *s ) {
			num++;
			t = name;
			while ( ( *s == *t ) && *t ) { s++; t++; }
			if ( *s == *t ) { return(num); }
			while ( *s ) s++;
			s++;
			while ( *s ) s++;
			s++;
		}
	}
	return(0);
}

/*
  	#] GetTableName : 
  	#[ PutTableNames :

	Takes the names string in d->tablenames and puts it in the nblocks
	pieces. Writes what has been changed to disk.
*/

int PutTableNames(DBASE *d)
{
	NAMESBLOCK **nnew;
	int i, j, firstdif;
	MLONG m;
	char *s, *t;
/*
	Determine how many blocks are needed.
*/
	MLONG numblocks = d->tablenamefill/NAMETABLESIZE + 1;
	if ( d->info.numberofnamesblocks < numblocks ) {
/*
		We need more blocks. First make sure of the space for nblocks.
*/
		if ( ( nnew = (NAMESBLOCK **)Malloc1(sizeof(NAMESBLOCK *)*numblocks,
			"new names block") ) == 0 ) {
			return(-1);
		}
		for ( i = 0; i < d->info.numberofnamesblocks; i++ ) {
			nnew[i] = d->nblocks[i];
		}
		free(d->nblocks);
		d->nblocks = nnew;
		for ( ; i < numblocks; i++ ) {
			if ( ( d->nblocks[i] = (NAMESBLOCK *)Malloc1(sizeof(NAMESBLOCK),
			"additional names blocks ") ) == 0 ) {
				FreeTableBase(d);
				return(-1);
			}
			d->nblocks[i]->previousblock = -1;
			d->nblocks[i]->position = -1;
			s = d->nblocks[i]->names;
			for ( j = 0; j < NAMETABLESIZE; j++ ) *s++ = 0;
		}
		d->info.numberofnamesblocks = numblocks;
	}
/*
	Now look till where the new contents agree with the old.
*/
	firstdif = 0;
	i = 0; t = d->nblocks[i]->names; j = 0; s = d->tablenames;
	for ( m = 0; m < d->tablenamefill; m++ ) {
		if ( *s == *t ) {
			s++; t++; j++;
			if ( j >= NAMETABLESIZE ) {
				i++;
				t = d->nblocks[i]->names;
				j = 0;
			}
		}
		else {
			firstdif = i;
			for ( ; m < d->tablenamefill; m++ ) {
				*t++ = *s++; j++;
				if ( j >= NAMETABLESIZE ) {
					i++;
					t = d->nblocks[i]->names;
					j = 0;
				}
			}
			*t = 0;
			break;
		}
	}
	for ( i = 0; i < d->info.numberofnamesblocks; i++ ) {
		if ( i == firstdif ) break;
		if ( d->nblocks[i]->position < 0 ) { firstdif = i; break; }
	}
/*
	Now we have to (re)write the blocks, starting at firstdif.
*/
	for ( i = firstdif; i < d->info.numberofnamesblocks; i++ ) {
		if ( i > 0 ) d->nblocks[i]->previousblock = d->nblocks[i-1]->position;
		else d->nblocks[i]->previousblock = -1;
		if ( d->nblocks[i]->position < 0 ) {
 			fseek(d->handle,0,SEEK_END);
 			d->nblocks[i]->position = ftell(d->handle);
		}
		else fseek(d->handle,d->nblocks[i]->position,SEEK_SET);
		convertnamesblock(d->nblocks[i],&scratchnamesblock,TODISK);
		if ( minoswrite(d->handle,(char *)(&scratchnamesblock),sizeof(NAMESBLOCK)) ) {
			MesPrint("Error while writing names blocks\n");
			FreeTableBase(d);
			return(-1);
		}
	}
	d->info.lastnameblock = d->nblocks[d->info.numberofnamesblocks-1]->position;
	d->info.firstnameblock = d->nblocks[0]->position;
	return(WriteIniInfo(d));
}

/*
  	#] PutTableNames : 
  	#[ AddToIndex :
*/

int AddToIndex(DBASE *d,MLONG number)
{
	MLONG i, oldnumofindexblocks = d->info.numberofindexblocks;
	MLONG j, newnumofindexblocks, jj;
	INDEXBLOCK **ib;
	MLONG t = (MLONG)(time(0));
	if ( number == 0 ) return(0);
	else if ( number < 0 ) {
		if ( d->info.entriesinindex < -number ) {
			MesPrint("There are only %ld entries in the index of file %s\n",
			d->info.entriesinindex,d->name);
			return(-1);
		}
		d->info.entriesinindex += number;
dowrite:
		if ( WriteIniInfo(d) ) {
			d->info.entriesinindex -= number;
			MesPrint("File may be corrupted\n");
			return(-1);
		}
	}
	else if ( d->info.entriesinindex+number <=
	NUMOBJECTS*d->info.numberofindexblocks ) {
		d->info.entriesinindex += number;
		goto dowrite;
	}
	else {
		d->info.entriesinindex += number;
		newnumofindexblocks = d->info.numberofindexblocks + ((number -
            (NUMOBJECTS*d->info.numberofindexblocks - d->info.entriesinindex))
		    +NUMOBJECTS-1)/NUMOBJECTS;
		if ( ( ib = (INDEXBLOCK **)Malloc1(sizeof(INDEXBLOCK *)*newnumofindexblocks,
		"index") ) == 0 ) return(-1);
		for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
			ib[i] = d->iblocks[i];
		}		
		for ( i = d->info.numberofindexblocks; i < newnumofindexblocks; i++ ) {
			if ( ( ib[i] = (INDEXBLOCK *)Malloc1(sizeof(INDEXBLOCK),"index block") ) == 0 ) {
				FreeTableBase(d);
				return(-1);
			}
			if ( i > 0 ) ib[i]->previousblock = ib[i-1]->position;
			else ib[i]->previousblock = -1;
/*
			Zero things properly. We don't want garbage in the file.
*/
			for ( j = 0; j < NUMOBJECTS; j++ ) {
				ib[i]->objects[j].date = t;
				ib[i]->objects[j].size = 0;
				ib[i]->objects[j].position = -1;
				ib[i]->objects[j].tablenumber = 0;
				ib[i]->objects[j].uncompressed = 0;
				ib[i]->objects[j].spare1 = 0;
				ib[i]->objects[j].spare2 = 0;
				ib[i]->objects[j].spare3 = 0;
				for ( jj = 0; jj < ELEMENTSIZE; jj++ ) ib[i]->objects[j].element[jj] = 0;
			}
			fseek(d->handle,0,SEEK_END);
			ib[i]->position = ftell(d->handle);
			convertblock(ib[i],&scratchblock,TODISK);
			if ( minoswrite(d->handle,(char *)(&scratchblock),sizeof(INDEXBLOCK)) ) {
				MesPrint("Error while writing new index of file %s",d->name);
				FreeTableBase(d);
				return(-1);
			}
		}
		d->info.lastindexblock = ib[newnumofindexblocks-1]->position;
		d->info.firstindexblock = ib[0]->position;
		d->info.numberofindexblocks = newnumofindexblocks;
		if ( WriteIniInfo(d) ) {
			d->info.numberofindexblocks = oldnumofindexblocks;
			d->info.entriesinindex -= number;
			MesPrint("File may be corrupted\n");
			FreeTableBase(d);
			return(-1);
		}
		M_free(d->iblocks,"AddToIndex");
		d->iblocks = ib;
	}
	return(0);
}

/*
  	#] AddToIndex : 
  	#[ AddObject :
*/

MLONG AddObject(DBASE *d,MLONG tablenumber,char *arguments,char *rhs)
{
	MLONG number;
	number = d->info.entriesinindex;
	if ( AddToIndex(d,1) ) return(-1);
	if ( WriteObject(d,tablenumber,arguments,rhs,number) ) return(-1);
	return(number);
}

/*
  	#] AddObject : 
  	#[ FindTableNumber :
*/

MLONG FindTableNumber(DBASE *d,char *name)
{
	char *s = d->tablenames, *t, *ss;
	MLONG num = 0;
	ss = d->tablenames + d->tablenamefill;
	while ( s < ss ) {
		num++;
		t = name;
		while ( *s == *t && *t ) {
			s++; t++;
		}
		if ( *s == 0 && *t == 0 ) return(num);
		while ( *s ) s++;
		s++;
/*
		Skip also the argument tail
*/
		while ( *s ) s++;
		s++;
	}
	return(-1);		/* Name not found */
}

/*
  	#] FindTableNumber : 
  	#[ WriteObject :
*/

int WriteObject(DBASE *d,MLONG tablenumber,char *arguments,char *rhs,MLONG number)
{
	char *s, *a;
#ifdef WITHZLIB
	char *buffer = 0;
	uLongf newsize = 0, oldsize = 0;
	uLong ssize;
	int error = 0;
#endif
	MLONG i, j, position, size, n;
	OBJECTS *obj;
	if ( ( d->mode & INPUTONLY ) == INPUTONLY ) {
		MesPrint("Not allowed to write to input\n");
		return(-1);
	}
	if ( number >= d->info.entriesinindex ) {
		MesPrint("Reference to non-existing object number %ld\n",number+1);
		return(0);
	}
	j = number/NUMOBJECTS;
	i = number%NUMOBJECTS;
	obj = &(d->iblocks[j]->objects[i]);
	a = arguments;
	while ( *a ) a++;
	a++; n = a - arguments;
	if ( n > ELEMENTSIZE ) {
		MesPrint("Table element %s has more than %ld characters.\n",arguments,
			(MLONG)ELEMENTSIZE);
		return(-1);
	}
	s = obj->element;
	a = arguments;
	while ( *a ) *s++ = *a++;
	*s++ = 0;
	while ( n < ELEMENTSIZE ) { *s++ = 0; n++; }
	obj->spare1 = obj->spare2 = obj->spare3 = 0;

	fseek(d->handle,0,SEEK_END);
	position = ftell(d->handle);
	s = rhs;
	while ( *s ) s++;
	s++;
	size = s - rhs;
#ifdef WITHZLIB
	if ( ( d->mode & NOCOMPRESS ) == 0 ) {
		newsize = size + size/1000 + 20;
		if ( ( buffer = (char *)Malloc1(newsize*sizeof(char),"compress buffer") )
			== 0 ) {
			MesPrint("No compress used for element %s in file %s\n",arguments,d->name);
		}
	}
	else buffer = 0;
	if ( buffer ) {
		ssize = size;
#ifdef WITHZSTD
		// Force the use of zlib for compressed Tablebase entries, so that tablebases created
		// with zstd-supported FORM builds can be used by zstd-unsupported FORM builds.
		const int old_isUsingZSTDcompression = ZWRAP_isUsingZSTDcompression();
		ZWRAP_useZSTDcompression(0);
#endif
		if ( ( error = compress((Bytef *)buffer,&newsize,(Bytef *)rhs,ssize) ) != Z_OK ) {
			MesPrint("Error = %d\n",error);
			MesPrint("Due to error no compress used for element %s in file %s\n",arguments,d->name);
			M_free(buffer,"tb,WriteObject");
			buffer = 0;
		}
#ifdef WITHZSTD
		ZWRAP_useZSTDcompression(old_isUsingZSTDcompression);
#endif
	}
	if ( buffer ) {
		rhs = buffer;
		oldsize = size;
		size = newsize;
	}
#endif
	if ( minoswrite(d->handle,rhs,size) ) {
		MesPrint("Error while writing rhs\n");
		return(-1);
	}
	obj->position = position;
	obj->size = size;
	obj->date = (MLONG)(time(0));
	obj->tablenumber = tablenumber;
#ifdef WITHZLIB
	obj->uncompressed = oldsize;
	if ( buffer ) M_free(buffer,"tb,WriteObject");
#else
	obj->uncompressed = 0;
#endif
	return(WriteIndexBlock(d,j));
}

/*
  	#] WriteObject : 
  	#[ ReadObject :

	Returns a pointer to the proper rhs
*/

char *ReadObject(DBASE *d,MLONG tablenumber,char *arguments)
{
	OBJECTS *obj;
	MLONG i, j;
	char *buffer1, *s, *t;
#ifdef WITHZLIB
	char *buffer2 = 0;
	uLongf finallength = 0;
#endif
	if ( tablenumber > d->topnumber ) {
		MesPrint("Reference to non-existing table number in tablebase %s: %ld\n",
				d->name,tablenumber);
		return(0);
	}
/*
	Start looking for the object
*/
	for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
		for ( j = 0; j < NUMOBJECTS; j++ ) {
			if ( d->iblocks[i]->objects[j].tablenumber != tablenumber ) continue;
			s = arguments; t = d->iblocks[i]->objects[j].element;
			while ( *s == *t && *s ) { s++; t++; }
			if ( *t == 0 && *s == 0 ) goto foundelement;
		}
	}
	s = d->tablenames; i = 1;
	while ( *s ) {
		if ( i == tablenumber ) break;
		while ( *s ) s++;
		s++;
		while ( *s ) s++;
		s++;
		i++;
	}
	MesPrint("%s(%s) not found in tablebase %s\n",s,arguments,d->name);
	return(0);

foundelement:;
	obj = &(d->iblocks[i]->objects[j]);
	fseek(d->handle,obj->position,SEEK_SET);
	if ( ( buffer1 = (char *)Malloc1(obj->size,"reading rhs buffer1") ) == 0 ) {
		return(0);
	}
#ifdef WITHZLIB
	if ( obj->uncompressed > 0 ) {
		if ( ( buffer2 = (char *)Malloc1(obj->uncompressed,"reading rhs buffer2") ) == 0 ) {
			return(0);
		}
	}
	else buffer2 = 0;
#endif
	if ( minosread(d->handle,buffer1,obj->size) ) {
		MesPrint("Could not read rhs %s in file %s\n",arguments,d->name);
		M_free(buffer1,"tb,ReadObject");
#ifdef WITHZLIB
		if ( buffer2 ) M_free(buffer2,"tb,ReadObject");
#endif
		return(0);
	}
#ifdef WITHZLIB
	if ( buffer2 == 0 ) return(buffer1);
	finallength = obj->uncompressed;
	if ( uncompress((Bytef *)buffer2,&finallength,(Bytef *)buffer1,obj->size) != Z_OK ) {
		MesPrint("Cannot uncompress element %s in file %s\n",arguments,d->name);
		M_free(buffer1,"tb,ReadObject"); M_free(buffer2,"tb,ReadObject");
		return(0);
	}
	M_free(buffer1,"tb,ReadObject");
	return(buffer2);
#else
	return(buffer1);
#endif
}

/*
  	#] ReadObject : 
  	#[ ReadijObject :

	Returns a pointer to the proper rhs
*/

char *ReadijObject(DBASE *d,MLONG i,MLONG j,char *arguments)
{
	OBJECTS *obj;
	char *buffer1;
#ifdef WITHZLIB
	char *buffer2 = 0;
	uLongf finallength = 0;
#endif
	obj = &(d->iblocks[i]->objects[j]);
	fseek(d->handle,obj->position,SEEK_SET);
	if ( ( buffer1 = (char *)Malloc1(obj->size,"reading rhs buffer1") ) == 0 ) {
		return(0);
	}
#ifdef WITHZLIB
	if ( obj->uncompressed > 0 ) {
		if ( ( buffer2 = (char *)Malloc1(obj->uncompressed,"reading rhs buffer2") ) == 0 ) {
			return(0);
		}
	}
	else buffer2 = 0;
#endif
	if ( minosread(d->handle,buffer1,obj->size) ) {
		MesPrint("Could not read rhs %s in file %s\n",arguments,d->name);
		if ( buffer1 ) M_free(buffer1,"rhs buffer1");
#ifdef WITHZLIB
		if ( buffer2 ) M_free(buffer2,"rhs buffer2");
#endif
		return(0);
	}
#ifdef WITHZLIB
	if ( buffer2 == 0 ) return(buffer1);
	finallength = obj->uncompressed;
	if ( uncompress((Bytef *)buffer2,&finallength,(Bytef *)buffer1,obj->size) != Z_OK ) {
		MesPrint("Cannot uncompress element %s in file %s\n",arguments,d->name);
		if ( buffer1 ) M_free(buffer1,"rhs buffer1");
		if ( buffer2 ) M_free(buffer2,"rhs buffer2");
		return(0);
	}
	M_free(buffer1,"rhs buffer1");
	return(buffer2);
#else
	return(buffer1);
#endif
}

/*
  	#] ReadijObject : 
  	#[ ExistsObject :

	Returns 1 if Object exists
*/

int ExistsObject(DBASE *d,MLONG tablenumber,char *arguments)
{
	MLONG i, j;
	char *s, *t;
	if ( tablenumber > d->topnumber ) {
		MesPrint("Reference to non-existing table number in tablebase %s: %ld\n",
				d->name,tablenumber);
		return(0);
	}
/*
	Start looking for the object
*/
	for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
		for ( j = 0; j < NUMOBJECTS; j++ ) {
			if ( d->iblocks[i]->objects[j].tablenumber != tablenumber ) continue;
			s = arguments; t = d->iblocks[i]->objects[j].element;
			while ( *s == *t && *s ) { s++; t++; }
			if ( *t == 0 && *s == 0 ) return(1);
		}
	}
	return(0);
}

/*
  	#] ExistsObject : 
  	#[ DeleteObject :

	Returns 1 if Object has been deleted.
	We leave a hole. Actually the object is still there but has been
	inactivated. It can be reactivated by calling this routine again.
*/

int DeleteObject(DBASE *d,MLONG tablenumber,char *arguments)
{
	MLONG i, j;
	char *s, *t;
	if ( tablenumber > d->topnumber ) {
		MesPrint("Reference to non-existing table number in tablebase %s: %ld\n",
				d->name,tablenumber);
		return(0);
	}
/*
	Start looking for the object
*/
	for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
		for ( j = 0; j < NUMOBJECTS; j++ ) {
			if ( d->iblocks[i]->objects[j].tablenumber != tablenumber ) continue;
			s = arguments; t = d->iblocks[i]->objects[j].element;
			while ( *s == *t && *s ) { s++; t++; }
			if ( *t == 0 && *s == 0 ) {
				d->iblocks[i]->objects[j].tablenumber =
					-d->iblocks[i]->objects[j].tablenumber - 1;
				return(1);
			}
		}
	}
	return(0);
}

/*
  	#] DeleteObject : 
*/
