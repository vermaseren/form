/** @file spectator.c
 *
 *	File contains the code for the spectator files and their control.
 *
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
/*
  	#[ includes :
*/

#include "form3.h"

/*
  	#] includes : 
  	#[ Commentary :

	We use an array of SPECTATOR structs in AM.SpectatorFiles.
	When a spectator is removed this leaves a hole. This means that
	we cannot use AM.NumSpectatorFiles but always have to scan up to
	AM.SizeForSpectatorFiles which is the size of the array.
	An element is in use when it has a name. This is the name of the
	expression that is associated with it. There is also the number of
	the expression, but because the expressions are frequently renumbered
	at the end of a module, we always search for the spectators by name.
	The expression number is only valid in the current module.
	During execution we use the number of the spectator.

	The FILEHANDLE struct is borrowed from the structs for the scratch
	files, but we do not keep copies for all workers as with the scratch
	files. This brings some limitations (but saves much space). Basically
	the reading can only be done by one master or worker. And because
	we use the buffer both for writing and for reading we cannot read and
	write in the same module.

	Processor can see that an expression is to be filled with a spectator
	because we replace the compiler buffer number in the prototype by
	-specnum-1. Of course, after this filling has taken place we should
	make sure that in the next module there is a nonnegative number there.
	The input is then obtained from GetFromSpectator instead from GetTerm.
	This needed an extra argument in ThreadsProcessor. InParallelProcessor
	can figure it out by itself. ParFORM still needs to be treated for this.

	The writing is to a single buffer. Hence it needs a lock. It is possible
	to give all workers their own buffers (at great memory cost) and merge
	the results when needed. That would be friendlier on ParFORM. We ALWAYS
	assume that the order of the terms in the spectator file is random.

	In the first version there is no compression in the file. This could
	change in later versions because both the writing and the reading are
	purely sequential. Brackets are not possible.

	Currently, ParFORM allows use of spectators only in the sequential
	mode. The parallelization is switched off in modules containing
	ToSpectator or CopySpectator. Workers never create or access to
	spectator files. Their file handles are always -1. We leave the
	parallelization of modules with spectators for future work.

  	#] Commentary : 
  	#[ CoCreateSpectator :

	Syntax: CreateSpectator name_of_expr "filename";
*/

int CoCreateSpectator(UBYTE *inp)
{
	UBYTE *p, *q, *filename, c, cc;
	WORD c1, c2, numexpr = 0, specnum, HadOne = 0;
	FILEHANDLE *fh;
	while ( *inp == ',' ) inp++;
	if ( ( q = SkipAName(inp) ) == 0 || q[-1] == '_' ) {
		MesPrint("&Illegal name for expression");
		return(1);
	}
	c = *q; *q = 0;
	if ( GetVar(inp,&c1,&c2,ALLVARIABLES,NOAUTO) != NAMENOTFOUND ) {
		if ( c2 == CEXPRESSION && 
				Expressions[c1].status == DROPSPECTATOREXPRESSION ) {
			numexpr = c1;
			Expressions[numexpr].status = SPECTATOREXPRESSION;
			HadOne = 1;
		}
		else {
			MesPrint("&The name %s has been used already.",inp);
			*q = c;
			return(1);
		}
	}
	p = q+1;
	while ( *p == ',' ) p++;
	if ( *p != '"' ) goto Syntax;
	p++; filename = p;
	while ( *p && *p != '"' ) {
		if ( *p == '\\' ) p++;
		p++;
	}
	if ( *p != '"' ) goto Syntax;
	q = p+1;
	while ( *q && ( *q == ',' || *q == ' ' || *q == '\t' ) ) q++;
	if ( *q ) goto Syntax;
	cc = *p; *p = 0;
/*
	Now we need to: create a struct for the spectator file.
*/
	if ( HadOne == 0 )
		numexpr = EntVar(CEXPRESSION,inp,SPECTATOREXPRESSION,0,0,0);
	fh = AllocFileHandle(1,(char *)filename);
/*
	Make sure there is space in the AM.spectatorfiles array
*/
	if ( AM.NumSpectatorFiles >= AM.SizeForSpectatorFiles || AM.SpectatorFiles == 0 ) {
		int newsize, i;
		SPECTATOR *newspectators;
		if ( AM.SizeForSpectatorFiles == 0 ) {
			newsize = 10;
			AM.NumSpectatorFiles = AM.SizeForSpectatorFiles = 0;
		}
		else newsize = AM.SizeForSpectatorFiles*2;
		newspectators = (SPECTATOR *)Malloc1(newsize*sizeof(SPECTATOR),"Spectators");
		for ( i = 0; i < AM.NumSpectatorFiles; i++ )
			newspectators[i] = AM.SpectatorFiles[i];
		for ( ; i < newsize; i++ ) {
			newspectators[i].fh = 0;
			newspectators[i].name = 0;
			newspectators[i].exprnumber = -1;
			newspectators[i].flags = 0;
			PUTZERO(newspectators[i].position);
			PUTZERO(newspectators[i].readpos);
		}
		AM.SizeForSpectatorFiles = newsize;
		if ( AM.SpectatorFiles != 0 ) M_free(AM.SpectatorFiles,"Spectators");
		AM.SpectatorFiles = newspectators;
		specnum = AM.NumSpectatorFiles++;
	}
	else {
		for ( specnum = 0; specnum < AM.SizeForSpectatorFiles; specnum++ ) {
			if ( AM.SpectatorFiles[specnum].name == 0 ) break;
		}
		AM.NumSpectatorFiles++;
	}
	PUTZERO(AM.SpectatorFiles[specnum].position);
	AM.SpectatorFiles[specnum].name = (char *)(strDup1(inp,"Spectator expression name"));
	AM.SpectatorFiles[specnum].fh = fh;
	AM.SpectatorFiles[specnum].exprnumber = numexpr;
	*p = cc;
	return(0);
Syntax:
	MesPrint("&Proper syntax is: CreateSpectator,exprname,\"filename\";");
	return(-1);
}

/*
  	#] CoCreateSpectator : 
  	#[ CoToSpectator :
*/

int CoToSpectator(UBYTE *inp)
{
	UBYTE *q;
	WORD c1, numexpr;
	int i;
	while ( *inp == ',' ) inp++;
	if ( ( q = SkipAName(inp) ) == 0 || q[-1] == '_' ) {
		MesPrint("&Illegal name for expression");
		return(1);
	}
	if ( *q != 0 ) goto Syntax;
	if ( GetVar(inp,&c1,&numexpr,ALLVARIABLES,NOAUTO) == NAMENOTFOUND ||
			c1 != CEXPRESSION ) {
		MesPrint("&%s is not a valid expression.",inp);
		return(1);
	}
	if ( Expressions[numexpr].status != SPECTATOREXPRESSION ) {
		MesPrint("&%s is not an active spectator.",inp);
		return(1);
	}
	for ( i = 0; i < AM.SizeForSpectatorFiles; i++ ) {
	  if ( AM.SpectatorFiles[i].name != 0 ) {
		if ( StrCmp((UBYTE *)(AM.SpectatorFiles[i].name),(UBYTE *)(inp)) == 0 ) break;
	  }
	}
	if ( i >= AM.SizeForSpectatorFiles ) {
		MesPrint("&Spectator %s not found.",inp);
		return(1);
	}
	if ( ( AM.SpectatorFiles[i].flags & READSPECTATORFLAG ) != 0 ) {
		MesPrint("&Spectator %s: It is not permitted to read from and write to the same spectator in one module.",inp);
		return(1);
	}
	AM.SpectatorFiles[i].exprnumber = numexpr;
	Add3Com(TYPETOSPECTATOR,i);
#ifdef WITHMPI
	/*
	 * In ParFORM, ToSpectator has to be executed on the master.
	 */
	AC.mparallelflag |= NOPARALLEL_SPECTATOR;
#endif
	return(0);
Syntax:
	MesPrint("&Proper syntax is: ToSpectator,exprname;");
	return(-1);
}

/*
  	#] CoToSpectator : 
  	#[ CoRemoveSpectator :
*/

int CoRemoveSpectator(UBYTE *inp)
{
	UBYTE *q;
	WORD c1, numexpr;
	int i;
	SPECTATOR *sp;
	while ( *inp == ',' ) inp++;
	if ( ( q = SkipAName(inp) ) == 0 || q[-1] == '_' ) {
		MesPrint("&Illegal name for expression");
		return(1);
	}
	if ( *q != 0 ) goto Syntax;
	if ( GetVar(inp,&c1,&numexpr,ALLVARIABLES,NOAUTO) == NAMENOTFOUND ||
			c1 != CEXPRESSION ) {
		MesPrint("&%s is not a valid expression.",inp);
		return(1);
	}
	if ( Expressions[numexpr].status != SPECTATOREXPRESSION ) {
		MesPrint("&%s is not a spectator.",inp);
		return(1);
	}
	for ( i = 0; i < AM.SizeForSpectatorFiles; i++ ) {
		if ( StrCmp((UBYTE *)(AM.SpectatorFiles[i].name),(UBYTE *)(inp)) == 0 ) break;
	}
	if ( i >= AM.SizeForSpectatorFiles ) {
		MesPrint("&Spectator %s not found.",inp);
		return(1);
	}
	sp = AM.SpectatorFiles+i;
	Expressions[numexpr].status = DROPSPECTATOREXPRESSION;
	if ( sp->fh->handle != -1 ) {
		CloseFile(sp->fh->handle);
		sp->fh->handle = -1;
		remove(sp->fh->name);
	}
	M_free(sp->fh,"Temporary FileHandle");
	M_free(sp->name,"Spectator expression name");

	PUTZERO(sp->position);
	PUTZERO(sp->readpos);
	sp->fh = 0;
	sp->name = 0;
	sp->exprnumber = -1;
	sp->flags = 0;
	AM.NumSpectatorFiles--;
	return(0);
Syntax:
	MesPrint("&Proper syntax is: RemoveSpectator,exprname;");
	return(-1);
}

/*
  	#] CoRemoveSpectator : 
  	#[ CoEmptySpectator :
*/

int CoEmptySpectator(UBYTE *inp)
{
	UBYTE *q;
	WORD c1, numexpr;
	int i;
	SPECTATOR *sp;
	while ( *inp == ',' ) inp++;
	if ( ( q = SkipAName(inp) ) == 0 || q[-1] == '_' ) {
		MesPrint("&Illegal name for expression");
		return(1);
	}
	if ( *q != 0 ) goto Syntax;
	if ( GetVar(inp,&c1,&numexpr,ALLVARIABLES,NOAUTO) == NAMENOTFOUND ||
			c1 != CEXPRESSION ) {
		MesPrint("&%s is not a valid expression.",inp);
		return(1);
	}
	if ( Expressions[numexpr].status != SPECTATOREXPRESSION ) {
		MesPrint("&%s is not a spectator.",inp);
		return(1);
	}
	for ( i = 0; i < AM.SizeForSpectatorFiles; i++ ) {
		if ( StrCmp((UBYTE *)(AM.SpectatorFiles[i].name),(UBYTE *)(inp)) == 0 ) break;
	}
	if ( i >= AM.SizeForSpectatorFiles ) {
		MesPrint("&Spectator %s not found.",inp);
		return(1);
	}
	sp = AM.SpectatorFiles+i;
	if ( sp->fh->handle != -1 ) {
		CloseFile(sp->fh->handle);
		sp->fh->handle = -1;
		remove(sp->fh->name);
	}
	sp->fh->POfill = sp->fh->POfull = sp->fh->PObuffer;
	PUTZERO(sp->position);
	PUTZERO(sp->readpos);
	return(0);
Syntax:
	MesPrint("&Proper syntax is: EmptySpectator,exprname;");
	return(-1);
}

/*
  	#] CoEmptySpectator : 
  	#[ PutInSpectator :

	We need to use locks! There is only one file.
	The code was copied (and modified) from PutOut.
	Here we use no compression.
*/

int PutInSpectator(WORD *term,WORD specnum)
{
	GETBIDENTITY
	WORD i, *p, ret;
	LONG RetCode;
	SPECTATOR *sp = &(AM.SpectatorFiles[specnum]);
	FILEHANDLE *fi = sp->fh;

	if ( ( i = *term ) <= 0 ) return(0);
	LOCK(fi->pthreadslock);
	ret = i;
	p = fi->POfill;
	do {
		if ( p >= fi->POstop ) {
			if ( fi->handle < 0 ) {
				if ( ( RetCode = CreateFile(fi->name) ) >= 0 ) {
					fi->handle = (WORD)RetCode;
					PUTZERO(fi->filesize);
					PUTZERO(fi->POposition);
				}
				else {
					MLOCK(ErrorMessageLock);
					MesPrint("Cannot create spectator file %s",fi->name);
					MUNLOCK(ErrorMessageLock);
					UNLOCK(fi->pthreadslock);
					return(-1);
				}
			}
			SeekFile(fi->handle,&(sp->position),SEEK_SET);
			if ( ( RetCode = WriteFile(fi->handle,(UBYTE *)(fi->PObuffer),fi->POsize) ) != fi->POsize ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Error during spectator write. Disk full?");
				MesPrint("Attempt to write %l bytes on file %d at position %15p",
							fi->POsize,fi->handle,&(fi->POposition));
				MesPrint("RetCode = %l, Buffer address = %l",RetCode,(LONG)(fi->PObuffer));
				MUNLOCK(ErrorMessageLock);
				UNLOCK(fi->pthreadslock);
				return(-1);
			}
			ADDPOS(fi->filesize,fi->POsize);
			p = fi->PObuffer;
			ADDPOS(sp->position,fi->POsize);
			fi->POposition = sp->position;
		} 
		*p++ = *term++;
	} while ( --i > 0 );
	fi->POfull = fi->POfill = p;
	Expressions[AM.SpectatorFiles[specnum].exprnumber].counter++;
	UNLOCK(fi->pthreadslock);
	return(ret);
}

/*
  	#] PutInSpectator : 
  	#[ FlushSpectators :
*/

void FlushSpectators(VOID)
{
	SPECTATOR *sp = AM.SpectatorFiles;
	FILEHANDLE *fh;
	LONG RetCode;
	int i;
	LONG size;
	if ( AM.NumSpectatorFiles <= 0 ) return;
	for ( i = 0; i < AM.SizeForSpectatorFiles; i++, sp++ ) {
		if ( sp->name == 0 ) continue;
		fh = sp->fh;
		if ( ( sp->flags & READSPECTATORFLAG ) != 0 ) { /* reset for writing */
			sp->flags &= ~READSPECTATORFLAG;
			fh->POfill = fh->PObuffer;
			if ( fh->handle >= 0 ) {
				SeekFile(fh->handle,&(sp->position),SEEK_SET);
				fh->POposition = sp->position;
			}
			continue;
		}
		if ( fh->POfill <= fh->PObuffer ) continue; /* is clean */
		if ( fh->handle < 0 ) {	/* File needs to be created */
			if ( ( RetCode = CreateFile(fh->name) ) >= 0 ) {
				PUTZERO(fh->filesize);
				PUTZERO(fh->POposition);
				fh->handle = (WORD)RetCode;
			}
			else {
				MLOCK(ErrorMessageLock);
				MesPrint("Cannot create spectator file %s",fh->name);
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			PUTZERO(sp->position);
		}
		SeekFile(fh->handle,&(sp->position),SEEK_SET);
		size = (fh->POfill - fh->PObuffer)*sizeof(WORD);
		if ( ( RetCode = WriteFile(fh->handle,(UBYTE *)(fh->PObuffer),size) ) != size ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Write error synching spectator file. Disk full?");
			MesPrint("Attempt to write %l bytes on file %s at position %15p",
						size,fh->name,&(sp->position));
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		fh->POfill = fh->PObuffer;
		SeekFile(fh->handle,&(sp->position),SEEK_END);
		fh->POposition = sp->position;
	}
	return;
}

/*
  	#] FlushSpectators : 
  	#[ CoCopySpectator :
*/

int CoCopySpectator(UBYTE *inp)
{
	GETIDENTITY
	UBYTE *q, c, *exprname, *p;
	WORD c1, c2, numexpr;
	int specnum, error = 0;
	SPECTATOR *sp;
	while ( *inp == ',' ) inp++;
	if ( ( q = SkipAName(inp) ) == 0 || q[-1] == '_' ) {
		MesPrint("&Illegal name for expression");
		return(1);
	}
	if ( *q == 0 ) goto Syntax;
	c = *q; *q = 0;
	if ( GetVar(inp,&c1,&c2,ALLVARIABLES,NOAUTO) != NAMENOTFOUND ) {
		MesPrint("&%s is the name of an existing variable.",inp);
		return(1);
	}
	numexpr = EntVar(CEXPRESSION,inp,LOCALEXPRESSION,0,0,0);
	p = q;
	exprname = inp;
	*q = c;
	while ( *q == ' ' || *q == ',' || *q == '\t' ) q++;
	if ( *q != '=' ) goto Syntax;
	q++;
	while ( *q == ' ' || *q == ',' || *q == '\t' ) q++;
	inp = q;
	if ( ( q = SkipAName(inp) ) == 0 || q[-1] == '_' ) {
		MesPrint("&Illegal name for spectator expression");
		return(1);
	}
	if ( *q != 0 ) goto Syntax;
	if ( AM.NumSpectatorFiles <= 0 ) {
		MesPrint("&CopySpectator: There are no spectator expressions!");
		return(1);
	}
	sp = AM.SpectatorFiles;
	for ( specnum = 0; specnum < AM.SizeForSpectatorFiles; specnum++, sp++ ) {
	  if ( sp->name != 0 ) {
		if ( StrCmp((UBYTE *)(sp->name),(UBYTE *)(inp)) == 0 ) break;
	  }
	}
	if ( specnum >= AM.SizeForSpectatorFiles ) {
		MesPrint("&Spectator %s not found.",inp);
		return(1);
	}
	sp->flags |= READSPECTATORFLAG;
	PUTZERO(sp->fh->POposition);
	PUTZERO(sp->readpos);
	sp->fh->POfill = sp->fh->PObuffer;
	if ( sp->fh->handle >= 0 ) {
		SeekFile(sp->fh->handle,&(sp->fh->POposition),SEEK_SET);
	}
/*
	Now we have:
	1: The name of the target expression: numexpr
	2: The spectator: sp (or specnum).
	Time for some action. We need:
	a: Write a prototype to create the expression
	b: Signal to Processor that this is a spectator.
	   We do this by giving a negative compiler buffer number.
*/
	{
		WORD *OldWork, *w;
		POSITION pos;
		OldWork = w = AT.WorkPointer;
		*w++ = TYPEEXPRESSION;
		*w++ = 3+SUBEXPSIZE;
		*w++ = numexpr;
		AC.ProtoType = w;
		AR.CurExpr = numexpr;				/* Block expression numexpr */
		*w++ = SUBEXPRESSION;
		*w++ = SUBEXPSIZE;
		*w++ = numexpr;
		*w++ = 1;
		*w++ = -specnum-1;		/* Indicates "spectator" to Processor */
		FILLSUB(w)
		*w++ = 1;
		*w++ = 1;
		*w++ = 3;
		*w++ = 0;
		SeekScratch(AR.outfile,&pos);
		Expressions[numexpr].counter = 1; 
		Expressions[numexpr].onfile = pos; 
		Expressions[numexpr].whichbuffer = 0;
#ifdef PARALLELCODE
		Expressions[numexpr].partodo = AC.inparallelflag; 
#endif
		OldWork[2] = w - OldWork - 3;
		AT.WorkPointer = w;

		if ( PutOut(BHEAD OldWork+2,&pos,AR.outfile,0) < 0 ) {
			c = *p; *p = 0;
			MesPrint("&Cannot create expression %s",exprname);
			*p = c;
			error = -1;
		}
		else {
			OldWork[2] = 4+SUBEXPSIZE;
			OldWork[4] = SUBEXPSIZE;
			OldWork[5] = numexpr;
			OldWork[SUBEXPSIZE+3] = 1;
			OldWork[SUBEXPSIZE+4] = 1;
			OldWork[SUBEXPSIZE+5] = 3;
			OldWork[SUBEXPSIZE+6] = 0;
			if ( PutOut(BHEAD OldWork+2,&pos,AR.outfile,0) < 0
			|| FlushOut(&pos,AR.outfile,0) ) {
				c = *p; *p = 0;
				MesPrint("&Cannot create expression %s",exprname);
				*p = c;
				error = -1;
			}
			AR.outfile->POfull = AR.outfile->POfill;
		}
		OldWork[2] = numexpr;
/*
		Seems unnecessary (13-feb-2018)

		AddNtoL(OldWork[1],OldWork);
*/
		AT.WorkPointer = OldWork;
		if ( AC.dumnumflag ) Add2Com(TYPEDETCURDUM)
	}
#ifdef WITHMPI
	/*
	 * In ParFORM, substitutions of spectators has to be done on the master.
	 */
	AC.mparallelflag |= NOPARALLEL_SPECTATOR;
#endif
	return(error);
Syntax:
	MesPrint("&Proper syntax is: CopySpectator,exprname=spectatorname;");
	return(-1);
}

/*
  	#] CoCopySpectator : 
  	#[ GetFromSpectator :

	Note that if we did things right, we do not need a lock for the reading.
*/

WORD GetFromSpectator(WORD *term,WORD specnum)
{
	SPECTATOR *sp = &(AM.SpectatorFiles[specnum]);
	FILEHANDLE *fh = sp->fh;
	WORD i, size, *t = term;
	LONG InIn;
	if ( fh-> handle < 0 ) {
		*term = 0;
		return(0);
	}
/*
	sp->position marks the 'end' of the file: the point where writing should
	take place. sp->readpos marks from where to read.
	fh->POposition marks where the file is currently positioned.
	Note that when we read, we need to 
*/
	if ( ISZEROPOS(sp->readpos) ) {	/* we start reading. Fill buffer. */
FillBuffer:
		SeekFile(fh->handle,&(sp->readpos),SEEK_SET);
		InIn = ReadFile(fh->handle,(UBYTE *)(fh->PObuffer),fh->POsize);
		if ( InIn < 0 || ( InIn & 1 ) ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Error reading information for %s spectator",sp->name);
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		InIn /= sizeof(WORD);
		if ( InIn == 0 ) { *term = 0; return(0); }
		SeekFile(fh->handle,&(sp->readpos),SEEK_CUR);
		fh->POposition = sp->readpos;
		fh->POfull = fh->PObuffer+InIn;
		fh->POfill = fh->PObuffer;
	}
	if ( fh->POfill == fh->POfull ) { /* not even the size of the term! */
		if ( ISLESSPOS(sp->readpos,sp->position) ) goto FillBuffer;
		*term = 0;
		return(0);
	}
	size = *fh->POfill++; *t++ = size;
	for ( i = 1; i < size; i++ ) {
		if ( fh->POfill >= fh->POfull ) {
			SeekFile(fh->handle,&(sp->readpos),SEEK_SET);
			InIn = ReadFile(fh->handle,(UBYTE *)(fh->PObuffer),fh->POsize);
			if ( InIn < 0 || ( InIn & 1 ) ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Error reading information for %s spectator",sp->name);
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			InIn /= sizeof(WORD);
			if ( InIn == 0 ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Reading incomplete information for %s spectator",sp->name);
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			SeekFile(fh->handle,&(sp->readpos),SEEK_CUR);
			fh->POposition = sp->readpos;
			fh->POfull = fh->PObuffer+InIn;
			fh->POfill = fh->PObuffer;
		}
		*t++ = *fh->POfill++;
	}
	return(size);
}

/*
  	#] GetFromSpectator : 
  	#[ ClearSpectators :

	Removes all spectators.
	In case of .store, the ones that are protected by .global stay.
*/

void ClearSpectators(WORD par)
{
	SPECTATOR *sp = AM.SpectatorFiles;
	WORD numexpr, c1;
	int i;
	if ( AM.NumSpectatorFiles > 0 ) {
		for ( i = 0; i < AM.SizeForSpectatorFiles; i++, sp++ ) {
			if ( sp->name == 0 ) continue;
			if ( ( sp->flags & GLOBALSPECTATORFLAG ) == 1 && par == STOREMODULE ) continue;

			if ( GetVar((UBYTE *)(sp->name),&c1,&numexpr,ALLVARIABLES,NOAUTO) == NAMENOTFOUND ||
				c1 != CEXPRESSION ) {
				MesPrint("&%s is not a valid expression.",sp->name);
				continue;
			}
			Expressions[numexpr].status = DROPPEDEXPRESSION;
			if ( sp->fh->handle != -1 ) {
				CloseFile(sp->fh->handle);
				sp->fh->handle = -1;
				remove(sp->fh->name);
			}
			M_free(sp->fh,"Temporary FileHandle");
			M_free(sp->name,"Spectator expression name");
			PUTZERO(sp->position);
			sp->fh = 0;
			sp->name = 0;
			sp->exprnumber = -1;
			sp->flags = 0;
			AM.NumSpectatorFiles--;
		}
	}
}

/*
  	#] ClearSpectators : 
*/
