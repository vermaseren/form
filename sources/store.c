/** @file store.c
 * 
 *  Contains all functions that deal with store-files and the system
 *  independent save-files.
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
#define HIDEDEBUG
  	#[ Includes : store.c
*/

#include "form3.h"

/*
  	#] Includes : 
	#[ StoreExpressions :
 		#[ OpenTemp :

		Opens the scratch files for the input -> output operations.

*/

WORD OpenTemp()
{
	GETIDENTITY
	if ( AR.outfile->handle >= 0 ) {
		SeekFile(AR.outfile->handle,&(AR.outfile->filesize),SEEK_SET);
		AR.outfile->POposition = AR.outfile->filesize;
		AR.outfile->POfill = AR.outfile->PObuffer;
	}
	return(0);
}

/*
 		#] OpenTemp : 
 		#[ SeekScratch :
*/

VOID SeekScratch(FILEHANDLE *fi, POSITION *pos)
{
	*pos = fi->POposition;
	ADDPOS(*pos,(TOLONG(fi->POfill)-TOLONG(fi->PObuffer)));
}

/*
 		#] SeekScratch : 
 		#[ SetEndScratch :
*/

VOID SetEndScratch(FILEHANDLE *f, POSITION *position)
{
	if ( f->handle < 0 ) {
		SETBASEPOSITION(*position,(f->POfull-f->PObuffer)*sizeof(WORD));
	}
	else *position = f->filesize;
	SetScratch(f,position);
}

/*
 		#] SetEndScratch : 
 		#[ SetEndHScratch :
*/

VOID SetEndHScratch(FILEHANDLE *f, POSITION *position)
{
	if ( f->handle < 0 ) {
		SETBASEPOSITION(*position,(f->POfull-f->PObuffer)*sizeof(WORD));
		f->POfill = f->POfull;
	}
	else {
#ifdef HIDEDEBUG
		POSITION possize;
		PUTZERO(possize);
		SeekFile(f->handle,&possize,SEEK_END);
		MesPrint("SetEndHScratch: filesize(th) = %12p, filesize(ex) = %12p",&(f->filesize),
				&(possize));
#endif
		*position = f->filesize;
		f->POposition = f->filesize;
		f->POfill = f->POfull = f->PObuffer;
	}
/*	SetScratch(f,position); */
}

/*
 		#] SetEndHScratch : 
 		#[ SetScratch :
*/

VOID SetScratch(FILEHANDLE *f, POSITION *position)
{
	GETIDENTITY
	POSITION possize;
	LONG size, *whichInInBuf;
	if ( f == AR.hidefile ) whichInInBuf = &(AR.InHiBuf);
	else                    whichInInBuf = &(AR.InInBuf);
#ifdef HIDEDEBUG
	if ( f == AR.hidefile ) MesPrint("In the hide file");
	else MesPrint("In the input file");
	MesPrint("SetScratch to position %15p",position);
	MesPrint("POposition = %15p, full = %l, fill = %l"
		,&(f->POposition),(f->POfull-f->PObuffer)*sizeof(WORD)
		,(f->POfill-f->PObuffer)*sizeof(WORD));
#endif
	if ( ISLESSPOS(*position,f->POposition) ||
	ISGEPOSINC(*position,f->POposition,(f->POfull-f->PObuffer)*sizeof(WORD)) ) {
		if ( f->handle < 0 ) {
			if ( ISEQUALPOSINC(*position,f->POposition,
				(f->POfull-f->PObuffer)*sizeof(WORD)) ) goto endpos;
			MesPrint("Illegal position in SetScratch");
			Terminate(-1);
		}
		possize = *position;
		LOCK(AS.inputslock);
		SeekFile(f->handle,&possize,SEEK_SET);
		if ( ISNOTEQUALPOS(possize,*position) ) {
			UNLOCK(AS.inputslock);
			MesPrint("Cannot position file in SetScratch");
			Terminate(-1);
		}
#ifdef HIDEDEBUG
			MesPrint("SetScratch1(%w): position = %12p, size = %l, address = %x",position,f->POsize,f->PObuffer);
#endif
		if ( ( size = ReadFile(f->handle,(UBYTE *)(f->PObuffer),f->POsize) ) < 0
		|| ( size & 1 ) != 0 ) {
			UNLOCK(AS.inputslock);
			MesPrint("Read error in SetScratch");
			Terminate(-1);
		}
		UNLOCK(AS.inputslock);
		if ( size == 0 ) {
			f->PObuffer[0] = 0;
		}
		f->POfill = f->PObuffer;
		f->POposition = *position;
#ifdef WORD2
		*whichInInBuf = size >> 1;
#else
		*whichInInBuf = size / TABLESIZE(WORD,UBYTE);
#endif
		f->POfull = f->PObuffer + *whichInInBuf;
#ifdef HIDEDEBUG
			MesPrint("SetScratch2: size = %l, InInBuf = %l, fill = %l, full = %l"
			,size,*whichInInBuf,(f->POfill-f->PObuffer)*sizeof(WORD)
			,(f->POfull-f->PObuffer)*sizeof(WORD));
#endif
	}
	else {
endpos:
		DIFPOS(possize,*position,f->POposition);
		f->POfill = (WORD *)(BASEPOSITION(possize)+(UBYTE *)(f->PObuffer));
		*whichInInBuf = f->POfull-f->POfill;
	}
}

/*
 		#] SetScratch : 
 		#[ RevertScratch :

		Reverts the input/output directions. This way input comes
		always from AR.infile

*/

WORD RevertScratch()
{
	GETIDENTITY
	FILEHANDLE *f;
	if ( AR.infile->handle >= 0 && AR.infile->handle != AR.outfile->handle ) {
		CloseFile(AR.infile->handle);
		AR.infile->handle = -1;
		remove(AR.infile->name);
	}
	f = AR.infile; AR.infile = AR.outfile; AR.outfile = f;
	AR.infile->POfull = AR.infile->POfill;
	AR.infile->POfill = AR.infile->PObuffer;
	if ( AR.infile->handle >= 0 ) {
		POSITION scrpos;
		PUTZERO(scrpos);
		SeekFile(AR.infile->handle,&scrpos,SEEK_SET);
		if ( ISNOTZEROPOS(scrpos) ) {
			return(MesPrint("Error with scratch output."));
		}
		if ( ( AR.InInBuf = ReadFile(AR.infile->handle,(UBYTE *)(AR.infile->PObuffer)
			,AR.infile->POsize) ) < 0 || AR.InInBuf & 1 ) {
			return(MesPrint("Error while reading from scratch file"));
		}
		else {
			AR.InInBuf /= TABLESIZE(WORD,UBYTE);
		}
		AR.infile->POfull = AR.infile->PObuffer + AR.InInBuf;
	}
	PUTZERO(AR.infile->POposition);
	AR.outfile->POfill = AR.outfile->POfull = AR.outfile->PObuffer;
	PUTZERO(AR.outfile->POposition);
	PUTZERO(AR.outfile->filesize);
	return(0);
}

/*
 		#] RevertScratch : 
 		#[ ResetScratch :

		Resets the output scratch file to its beginning in such a way
		that the write routines can read it. The output buffers are
		left untouched as they may still be needed for extra declarations.

*/

WORD ResetScratch()
{
	GETIDENTITY
	FILEHANDLE *f;
	if ( AR.infile->handle >= 0 ) {
		CloseFile(AR.infile->handle); AR.infile->handle = -1;
		remove(AR.infile->name);
		PUTZERO(AR.infile->POposition);
		AR.infile->POfill = AR.infile->POfull = AR.infile->PObuffer;
	}
	if ( AR.outfile->handle >= 0 ) {
		POSITION scrpos;
		PUTZERO(scrpos);
		SeekFile(AR.outfile->handle,&scrpos,SEEK_SET);
		if ( ISNOTZEROPOS(scrpos) ) {
			return(MesPrint("Error with scratch output."));
		}
		if ( ( AR.InInBuf = ReadFile(AR.outfile->handle,(UBYTE *)(AR.outfile->PObuffer)
		,AR.outfile->POsize) ) < 0 || AR.InInBuf & 1 ) {
			return(MesPrint("Error while reading from scratch file"));
		}
		else AR.InInBuf /= TABLESIZE(WORD,UBYTE);
		AR.outfile->POfull = AR.outfile->PObuffer + AR.InInBuf;
	}
	else AR.outfile->POfull = AR.outfile->POfill;
	AR.outfile->POfill = AR.outfile->PObuffer;
	PUTZERO(AR.outfile->POposition);
	f = AR.outfile; AR.outfile = AR.infile; AR.infile = f;
	return(0);
}

/*
 		#] ResetScratch : 
 		#[ ReadFromScratch :

	Routine is used to copy files from scratch to hide.
*/

int ReadFromScratch(FILEHANDLE *fi, POSITION *pos, UBYTE *buffer, POSITION *length)
{
	GETIDENTITY
	LONG l = BASEPOSITION(*length);
	if ( fi->handle < 0 ) {
		memcpy(buffer,fi->POfill,l);
	}
	else {
		SeekFile(fi->handle,pos,SEEK_SET);
		if ( ReadFile(fi->handle,buffer,l) != l ) {
			if ( fi == AR.hidefile )
				MesPrint("Error reading from hide file.");
			else
				MesPrint("Error reading from scratch file.");
			return(-1);
		}
	}
	return(0);
}

/*
 		#] ReadFromScratch : 
 		#[ AddToScratch :

	Routine is used to copy files from scratch to hide.
*/

int AddToScratch(FILEHANDLE *fi, POSITION *pos, UBYTE *buffer, POSITION *length,
			 int withflush)
{
	GETIDENTITY
	LONG l = BASEPOSITION(*length), avail;
	DUMMYUSE(pos)
	fi->POfill = fi->POfull;
	while ( fi->POfill+l/sizeof(WORD) > fi->POstop ) {
		avail = (fi->POstop-fi->POfill)*sizeof(WORD);
		if ( avail > 0 ) {
			memcpy(fi->POfill,buffer,avail);
			l -= avail; buffer += avail;
		}
		if ( fi->handle < 0 ) {
			if ( ( fi->handle = (WORD)CreateFile(fi->name) ) < 0 ) {
				if ( fi == AR.hidefile )
					MesPrint("Cannot create hide file %s",fi->name);
				else
					MesPrint("Cannot create scratch file %s",fi->name);
				return(-1);
			}
			PUTZERO(fi->POposition);
		}
		SeekFile(fi->handle,&(fi->POposition),SEEK_SET);
		if ( WriteFile(fi->handle,(UBYTE *)fi->PObuffer,fi->POsize) != fi->POsize )
			goto writeerror;
		ADDPOS(fi->POposition,fi->POsize);
		fi->POfill = fi->POfull = fi->PObuffer;
	}
	if ( l > 0 ) {
		memcpy(fi->POfill,buffer,l);
		fi->POfill += l/sizeof(WORD);
		fi->POfull = fi->POfill;
	}
	if ( withflush && fi->handle >= 0 && fi->POfill > fi->PObuffer ) {	/* flush */
		l = (LONG)fi->POfill - (LONG)fi->PObuffer;
		SeekFile(fi->handle,&(fi->POposition),SEEK_SET);
		if ( WriteFile(fi->handle,(UBYTE *)fi->PObuffer,l) != l ) goto writeerror;
		ADDPOS(fi->POposition,fi->POsize);
		fi->POfill = fi->POfull = fi->PObuffer;
	}
	if ( withflush && fi->handle >= 0 )
		SETBASEPOSITION(fi->filesize,TellFile(fi->handle));
	return(0);
writeerror:
	if ( fi == AR.hidefile )
		MesPrint("Error writing to hide file. Disk full?");
	else
		MesPrint("Error writing to scratch file. Disk full?");
	return(-1);
}

/*
 		#] AddToScratch : 
 		#[ CoSave :

		The syntax of the save statement is:

		save filename
		save filename expr1 expr2

*/

int CoSave(UBYTE *inp)
{
	GETIDENTITY
	UBYTE *p, c;
	WORD n = 0, i;
	WORD error = 0, type, number;
	LONG RetCode = 0, wSize;
	EXPRESSIONS e;
	INDEXENTRY *ind;
	INDEXENTRY *indold;
	WORD TMproto[SUBEXPSIZE];
	POSITION scrpos, scrpos1, filesize;
	int ii, j = sizeof(FILEINDEX)/(sizeof(LONG));
	LONG *lo;
	while ( *inp == ',' ) inp++;
	p = inp;

#ifdef WITHMPI
	if(	PF.me != MASTER) return(0);
#endif

	if ( !*p ) return(MesPrint("No filename in save statement"));
	if ( FG.cTable[*p] > 1 && ( *p != '.' ) && ( *p != SEPARATOR ) && ( *p != ALTSEPARATOR ) )
				return(MesPrint("Illegal filename"));
	while ( *++p && *p != ',' ) {}
	c = *p;
	*p = 0;
	if ( !AP.preError ) {
		if ( ( RetCode = CreateFile((char *)inp) ) < 0 ) {
			return(MesPrint("Cannot open file %s",inp));
		}
	}
	AO.SaveData.Handle = (WORD)RetCode;
	PUTZERO(filesize);

	e = Expressions;
	n = NumExpressions;
	if ( c ) {			/* There follows a list of expressions */
		*p++ = c;
		inp = p;
		i = (WORD)(INFILEINDEX);
		if ( WriteStoreHeader(AO.SaveData.Handle) ) return(MesPrint("Error writing storage file header"));
/*		PUTZERO(AO.SaveData.Index.number); */
/*		PUTZERO(AO.SaveData.Index.next); */
		lo = (LONG *)(&AO.SaveData.Index);
		for ( ii = 0; ii < j; ii++ ) *lo++ = 0;
		SETBASEPOSITION(AO.SaveData.Position,(LONG)sizeof(STOREHEADER));
		ind = AO.SaveData.Index.expression;
		if ( !AP.preError && WriteFile(AO.SaveData.Handle,(UBYTE *)(&(AO.SaveData.Index))
		,(LONG)sizeof(struct FiLeInDeX))!= (LONG)sizeof(struct FiLeInDeX) ) goto SavWrt;
		SeekFile(AO.SaveData.Handle,&(filesize),SEEK_END);
/*		ADDPOS(filesize,sizeof(struct FiLeInDeX)); */

		do {			/* Scan the list */
			if ( !FG.cTable[*p] || *p == '[' ) {
				p = SkipAName(p);
				if ( p == 0 ) return(-1);
			}
			c = *p; *p = 0;
			if ( GetVar(inp,&type,&number,CEXPRESSION,NOAUTO) != NAMENOTFOUND ) {
				if ( e[number].status == STOREDEXPRESSION ) {
/*
		Here we have to locate the stored expression, copy its index entry
		possibly after making a new fileindex and then copy the whole
		expression.
*/
					if ( AP.preError ) goto NextExpr;
					TMproto[0] = EXPRESSION;
					TMproto[1] = SUBEXPSIZE;
					TMproto[2] = number;
					TMproto[3] = 1;
					{ int ie; for ( ie = 4; ie < SUBEXPSIZE; ie++ ) TMproto[ie] = 0; }
					AT.TMaddr = TMproto;
					if ( ( indold = FindInIndex(number,&AR.StoreData,0,0) ) != 0 ) {
						if ( i <= 0 ) {
/*
							AO.SaveData.Index.next = filesize;
*/
							SeekFile(AO.SaveData.Handle,&(AO.SaveData.Index.next),SEEK_END);
							scrpos = AO.SaveData.Position;
							SeekFile(AO.SaveData.Handle,&scrpos,SEEK_SET);
							if ( ISNOTEQUALPOS(scrpos,AO.SaveData.Position) ) goto SavWrt;
							if ( WriteFile(AO.SaveData.Handle,(UBYTE *)(&(AO.SaveData.Index))
									,(LONG)sizeof(struct FiLeInDeX)) != (LONG)sizeof(struct FiLeInDeX) )
								goto SavWrt;
							i = (WORD)(INFILEINDEX);
							AO.SaveData.Position = AO.SaveData.Index.next;
							lo = (LONG *)(&AO.SaveData.Index);
							for ( ii = 0; ii < j; ii++ ) *lo++ = 0;
							ind = AO.SaveData.Index.expression;
							scrpos = AO.SaveData.Position;
							SeekFile(AO.SaveData.Handle,&scrpos,SEEK_SET);
							if ( ISNOTEQUALPOS(scrpos,AO.SaveData.Position) ) goto SavWrt;
							if ( WriteFile(AO.SaveData.Handle,(UBYTE *)(&(AO.SaveData.Index))
							,(LONG)sizeof(struct FiLeInDeX)) != (LONG)sizeof(struct FiLeInDeX) )
								goto SavWrt;
							ADDPOS(filesize,sizeof(struct FiLeInDeX));
						}
						*ind = *indold;
/*
						ind->variables = SeekFile(AO.SaveData.Handle,&(AM.zeropos),SEEK_END);
*/
						ind->variables = filesize;
						ind->position = ind->variables;
						ADDPOS(ind->position,DIFBASE(indold->position,indold->variables));
						SeekFile(AR.StoreData.Handle,&(indold->variables),SEEK_SET);
						wSize = TOLONG(AT.WorkTop) - TOLONG(AT.WorkPointer);
						scrpos = ind->length;
						ADDPOS(scrpos,DIFBASE(ind->position,ind->variables));
						ADD2POS(filesize,scrpos);
						SETBASEPOSITION(scrpos1,wSize);
						do {
							if ( ISLESSPOS(scrpos,scrpos1) ) wSize = BASEPOSITION(scrpos);
							if ( ReadFile(AR.StoreData.Handle,(UBYTE *)AT.WorkPointer,wSize)
							!= wSize ) {
								MesPrint("ReadError");
								error = -1;
								goto EndSave;
							}
							if ( WriteFile(AO.SaveData.Handle,(UBYTE *)AT.WorkPointer,wSize)
							!= wSize ) goto SavWrt;
							ADDPOS(scrpos,-wSize);
						} while ( ISPOSPOS(scrpos) );
						ADDPOS(AO.SaveData.Index.number,1);
						ind++;
					}
					else error = -1;
					i--;
				}
				else {
					MesPrint("%s is not a stored expression",inp);
					error = -1;
				}
NextExpr:;
			}
			else {
				MesPrint("%s is not an expression",inp);
				error = -1;
			}
			*p = c;
			if ( c != ',' && c ) {
				MesComp("Illegal character",inp,p);
				error = -1;
				goto EndSave;
			}
			if ( c ) c = *++p;
			inp = p;
		} while ( c );
		if ( !AP.preError ) {
			scrpos = AO.SaveData.Position;
			SeekFile(AO.SaveData.Handle,&scrpos,SEEK_SET);
			if ( ISNOTEQUALPOS(scrpos,AO.SaveData.Position) ) goto SavWrt;
		}
		if ( !AP.preError &&
		WriteFile(AO.SaveData.Handle,(UBYTE *)(&(AO.SaveData.Index))
			,(LONG)sizeof(struct FiLeInDeX)) != (LONG)sizeof(struct FiLeInDeX) ) goto SavWrt;
	}
	else if ( !AP.preError ) {				/* All stored expressions should be saved. Easy */
		if ( n > 0 ) { do {
			if ( e->status == STOREDEXPRESSION ) break;
			e++;
		} while ( --n > 0 ); }
		if ( n ) {
			wSize = TOLONG(AT.WorkTop) - TOLONG(AT.WorkPointer);
			PUTZERO(scrpos);
			SeekFile(AR.StoreData.Handle,&scrpos,SEEK_SET);		/* Start at the beginning */
			scrpos = AR.StoreData.Fill;			/* Number of bytes to be copied */
			SETBASEPOSITION(scrpos1,wSize);
			do {
				if ( ISLESSPOS(scrpos,scrpos1) ) wSize = BASEPOSITION(scrpos);
				if ( ReadFile(AR.StoreData.Handle,(UBYTE *)AT.WorkPointer,wSize) != wSize ) {
					MesPrint("ReadError");
					error = -1;
					goto EndSave;
				}
				if ( WriteFile(AO.SaveData.Handle,(UBYTE *)AT.WorkPointer,wSize) != wSize )
					goto SavWrt;
				ADDPOS(scrpos,-wSize);
			} while ( ISPOSPOS(scrpos) );
		}
	}
EndSave:
	if ( !AP.preError ) {
		CloseFile(AO.SaveData.Handle);
		AO.SaveData.Handle = -1;
	}
	return(error);
SavWrt:
	MesPrint("WriteError");
	error = -1;
	goto EndSave;
}

/*
 		#] CoSave : 
 		#[ CoLoad :
*/

int CoLoad(UBYTE *inp)
{
	GETIDENTITY
	INDEXENTRY *ind;
	LONG RetCode;
	UBYTE *p, c;
	WORD num, i, error = 0;
	WORD type, number, silentload = 0;
	WORD TMproto[SUBEXPSIZE];
	POSITION scrpos,firstposition;
	while ( *inp == ',' ) inp++;
	p = inp;
	if ( ( *p == ',' && p[1] == '-' ) || *p == '-' ) {
		if ( *p == ',' ) p++;
		p++;
		if ( *p == 's' || *p == 'S' ) {
			silentload = 1;
			while ( *p && ( *p != ',' && *p != '-' && *p != '+'
			&& *p != SEPARATOR && *p != ALTSEPARATOR && *p != '.' ) ) p++;
		}
		else if ( *p != ',' ) {
			return(MesPrint("Illegal option in Load statement"));
		}
		while ( *p == ',' ) p++;
	}
	inp = p;
	if ( !*p ) return(MesPrint("No filename in load statement"));
	if ( FG.cTable[*p] > 1 && ( *p != '.' ) && ( *p != SEPARATOR ) && ( *p != ALTSEPARATOR ) )
				return(MesPrint("Illegal filename"));
	while ( *++p && *p != ',' ) {}
	c = *p;
	*p = 0;
	if ( ( RetCode = OpenFile((char *)inp) ) < 0 ) {
		return(MesPrint("Cannot open file %s",inp));
	}
 
	if ( SetFileIndex() ) {
		MesCall("CoLoad");
		SETERROR(-1)
	}

	AO.SaveData.Handle = (WORD)(RetCode);

#ifdef SYSDEPENDENTSAVE
	if ( ReadFile(AO.SaveData.Handle,(UBYTE *)(&(AO.SaveData.Index)),
		(LONG)sizeof(struct FiLeInDeX)) != (LONG)sizeof(struct FiLeInDeX) ) goto LoadRead;
#else
	if ( ReadSaveHeader() ) goto LoadRead;
	TELLFILE(AO.SaveData.Handle,&firstposition);
	if ( ReadSaveIndex(&AO.SaveData.Index) ) goto LoadRead;
#endif
	if ( c ) {			/* There follows a list of expressions */
		*p++ = c;
		inp = p;

		do {			/* Scan the list */
			if ( !FG.cTable[*p] || *p == '[' ) {
				p = SkipAName(p);
				if ( p == 0 ) return(-1);
			}
			c = *p; *p = 0;
			if ( GetVar(inp,&type,&number,ALLVARIABLES,NOAUTO) != NAMENOTFOUND ) {
				MesPrint("Conflicting name: %s",inp);
				error = -1;
			}
			else {
				if ( ( num = EntVar(CEXPRESSION,inp,STOREDEXPRESSION,0,0,0) ) >= 0 ) {
					TMproto[0] = EXPRESSION;
					TMproto[1] = SUBEXPSIZE;
					TMproto[2] = num;
					TMproto[3] = 1;
					{ int ie; for ( ie = 4; ie < SUBEXPSIZE; ie++ ) TMproto[ie] = 0; }
					AT.TMaddr = TMproto;
					SeekFile(AO.SaveData.Handle,&firstposition,SEEK_SET);
					AO.SaveData.Position = firstposition;
					if ( ReadSaveIndex(&AO.SaveData.Index) ) goto LoadRead;
					if ( ( ind = FindInIndex(num,&AO.SaveData,1,0) ) != 0 ) {
						if ( !error ) {
							if ( PutInStore(ind,num) ) error = -1;
							else if ( !AM.silent && silentload == 0 )
								MesPrint(" %s loaded",ind->name);
						}
/*
!!!	Added 1-feb-1998
*/
						Expressions[num].counter = -1;
					}
					else {
						MesPrint(" %s not found",inp);
						error = -1;
					}
				}
				else error = -1;
			}
			*p = c;
			if ( c != ',' && c ) {
				MesComp("Illegal character",inp,p);
				error = -1;
				goto EndLoad;
			}
			if ( c ) c = *++p;
			inp = p;
		} while ( c );
		scrpos = AR.StoreData.Position;
		SeekFile(AR.StoreData.Handle,&scrpos,SEEK_SET);
		if ( ISNOTEQUALPOS(scrpos,AR.StoreData.Position) ) goto LoadWrt;
		if ( WriteFile(AR.StoreData.Handle,(UBYTE *)(&(AR.StoreData.Index))
			,(LONG)sizeof(struct FiLeInDeX)) != (LONG)sizeof(struct FiLeInDeX) ) goto LoadWrt;
	}
	else {				/* All saved expressions should be stored. Easy */
		i = (WORD)BASEPOSITION(AO.SaveData.Index.number);
		ind = AO.SaveData.Index.expression;
#ifdef SYSDEPENDENTSAVE
		if ( i > 0 ) { do {
			if ( GetVar((UBYTE *)(ind->name),&type,&number,ALLVARIABLES,NOAUTO) != NAMENOTFOUND ) {
				MesPrint("Conflicting name: %s",ind->name);
				error = -1;
			}
			else {
				if ( ( num = EntVar(CEXPRESSION,(UBYTE *)(ind->name),STOREDEXPRESSION,0,0,0) ) >= 0 ) {
					if ( !error ) {
						if ( PutInStore(ind,num) ) error = -1;
						else if ( !AM.silent && silentload == 0 )
							MesPrint(" %s loaded",ind->name);
					}
				}
				else error = -1;
			}
			i--;
			if ( i == 0 && ISNOTZEROPOS(AO.SaveData.Index.next) ) {
				SeekFile(AO.SaveData.Handle,&(AO.SaveData.Index.next),SEEK_SET);
				if ( ReadFile(AO.SaveData.Handle,(UBYTE *)(&(AO.SaveData.Index)),
					(LONG)sizeof(struct FiLeInDeX)) != (LONG)sizeof(struct FiLeInDeX) ) goto LoadRead;
				i = (WORD)BASEPOSITION(AO.SaveData.Index.number);
				ind = AO.SaveData.Index.expression;
			}
			else ind++;
		} while ( i > 0 ); }
#else
		if ( i > 0 ) { 
			do {
				if ( GetVar((UBYTE *)(ind->name),&type,&number,ALLVARIABLES,NOAUTO) != NAMENOTFOUND ) {
					MesPrint("Conflicting name: %s",ind->name);
					error = -1;
				}
				else {
					if ( ( num = EntVar(CEXPRESSION,(UBYTE *)(ind->name),STOREDEXPRESSION,0,0,0) ) >= 0 ) {
						if ( !error ) {
							if ( PutInStore(ind,num) ) error = -1;
							else if ( !AM.silent && silentload == 0 )
								MesPrint(" %s loaded",ind->name);
						}
					}
					else error = -1;
				}
				i--;
				if ( i == 0 && (ISNOTZEROPOS(AO.SaveData.Index.next) || AO.bufferedInd) ) {
					SeekFile(AO.SaveData.Handle,&(AO.SaveData.Index.next),SEEK_SET);
					if ( ReadSaveIndex(&AO.SaveData.Index) ) goto LoadRead;
					i = (WORD)BASEPOSITION(AO.SaveData.Index.number);
					ind = AO.SaveData.Index.expression;
				}
				else ind++;
			} while ( i > 0 );
		}
#endif
	}
EndLoad:
#ifndef SYSDEPENDENTSAVE
	if ( AO.powerFlag ) {
		MesPrint("WARNING: min-/maxpower had to be adjusted!");
	}
	if ( AO.resizeFlag ) {
		MesPrint("ERROR: could not downsize data!");
		return ( -2 );
	}
#endif
	CloseFile(AO.SaveData.Handle);
	AO.SaveData.Handle = -1;
	SeekFile(AR.StoreData.Handle,&(AC.StoreFileSize),SEEK_END);
	return(error);
LoadWrt:
	MesPrint("WriteError");
	error = -1;
	goto EndLoad;
LoadRead:
	MesPrint("ReadError");
	error = -1;
	goto EndLoad;
}

/*
 		#] CoLoad : 
 		#[ DeleteStore :

		Routine deletes the contents of the entire storage file.
		We close the file and recreate it.
		If par > 0 we have to remove the expressions from the namelists.
*/

WORD DeleteStore(WORD par)
{
	GETIDENTITY
	char *s;
	WORD j, n = 0;
	EXPRESSIONS e_in, e_out;
	WORD DidClean = 0;
	if ( AR.StoreData.Handle >= 0 ) {
		if ( par > 0 ) {
			n = NumExpressions;
			j = 0;
			e_in = e_out = Expressions;
			if ( n > 0 ) { do {
				if ( e_in->status == STOREDEXPRESSION ) {
					NAMENODE *node = GetNode(AC.exprnames,
							AC.exprnames->namebuffer+e_in->name);
					node->type = CDELETE;
					DidClean = 1;
				}
				else {
					if ( e_out != e_in ) {
						NAMENODE *node;
						node = GetNode(AC.exprnames,
							AC.exprnames->namebuffer+e_in->name);
						node->number = (WORD)(e_out - Expressions);
						e_out->onfile = e_in->onfile;
						e_out->prototype = e_in->prototype;
						e_out->printflag = 0;
						e_out->status = e_in->status;
						e_out->name = e_in->name;
						e_out->inmem = e_in->inmem;
						e_out->counter = e_in->counter;
						e_out->numfactors = e_in->numfactors;
						e_out->numdummies = e_in->numdummies;
						e_out->compression = e_in->compression;
						e_out->namesize = e_in->namesize;
						e_out->whichbuffer = e_in->whichbuffer;
						e_out->hidelevel = e_in->hidelevel;
						e_out->node = e_in->node;
						e_out->replace = e_in->replace;
						e_out->vflags = e_in->vflags;
#ifdef PARALLELCODE
						e_out->partodo = e_in->partodo;
#endif
					}
					e_out++;
					j++;
				}
				e_in++;
			} while ( --n > 0 ); }
			NumExpressions = j;
			if ( DidClean ) CompactifyTree(AC.exprnames,EXPRNAMES);
		}
		AR.StoreData.Handle = -1;
		CloseFile(AC.StoreHandle);
		AC.StoreHandle = -1;
		{
/*
			Knock out the storage caches (25-apr-1990!)
*/
			STORECACHE st;
			st = (STORECACHE)(AT.StoreCache);
			while ( st ) {
				SETBASEPOSITION(st->position,-1);
				SETBASEPOSITION(st->toppos,-1);
				st = st->next;
			}
#ifdef WITHPTHREADS
			for ( j = 1; j < AM.totalnumberofthreads; j++ ) {
				st = (STORECACHE)(AB[j]->T.StoreCache);
				while ( st ) {
					SETBASEPOSITION(st->position,-1);
					SETBASEPOSITION(st->toppos,-1);
					st = st->next;
				}
			}
#endif
		}
		PUTZERO(AC.StoreFileSize);
		s = FG.fname; while ( *s ) s++;
#ifdef VMS
		*s = ';'; s[1] = '*'; s[2] = 0;
		remove(FG.fname);
		*s = 0;
#endif
		return(AC.StoreHandle = CreateFile(FG.fname));
	}
	else return(0);
}

/*
 		#] DeleteStore : 
 		#[ PutInStore :

		Copies the expression indicated by ind from a load file to the
		internal storage file. A return value of zero indicates that
		everything is OK.

*/

WORD PutInStore(INDEXENTRY *ind, WORD num)
{
	GETIDENTITY
	INDEXENTRY *newind;
	LONG wSize;
#ifndef SYSDEPENDENTSAVE
	LONG wSizeOut;
	LONG stage;
#endif
	POSITION scrpos,scrpos1;
	newind = NextFileIndex(&(Expressions[num].onfile));
	*newind = *ind;
#ifndef SYSDEPENDENTSAVE
	SETBASEPOSITION(newind->length, 0);
#endif
	newind->variables = AR.StoreData.Fill;
	SeekFile(AR.StoreData.Handle,&(newind->variables),SEEK_SET);
	if ( ISNOTEQUALPOS(newind->variables,AR.StoreData.Fill) ) goto PutErrS;
	newind->position = newind->variables;
#ifdef SYSDEPENDENTSAVE
	ADDPOS(newind->position,DIFBASE(ind->position,ind->variables));
#endif
	/* set read position to ind->variables */
	scrpos = ind->variables;
	SeekFile(AO.SaveData.Handle,&scrpos,SEEK_SET);
	if ( ISNOTEQUALPOS(scrpos,ind->variables) ) goto PutErrS;
	/* set max size for read-in */
	wSize = TOLONG(AT.WorkTop) - TOLONG(AT.WorkPointer);
#ifdef SYSDEPENDENTSAVE
	scrpos = ind->length;
	ADDPOS(scrpos,DIFBASE(ind->position,ind->variables));
	ADD2POS(AR.StoreData.Fill,scrpos);
#endif
	SETBASEPOSITION(scrpos1,wSize);
#ifndef SYSDEPENDENTSAVE
	/* prepare look-up table for tensor functions */
	if ( ind->nfunctions ) {
		AO.tensorList = (UBYTE *)Malloc1(MAXSAVEFUNCTION,"PutInStore");
	}
	SETBASEPOSITION(scrpos, DIFBASE(ind->position,ind->variables));
	/* copy variables first */
	stage = -1;
	do {
		wSize = TOLONG(AT.WorkTop) - TOLONG(AT.WorkPointer);
		if ( ISLESSPOS(scrpos,scrpos1) ) wSize = BASEPOSITION(scrpos);
		wSizeOut = wSize;
		if ( ReadSaveVariables(
			(UBYTE *)AT.WorkPointer, (UBYTE *)AT.WorkTop, &wSize, &wSizeOut, ind, &stage) ) {
			goto PutErrS;
		}
		if ( WriteFile(AR.StoreData.Handle, (UBYTE *)AT.WorkPointer, wSizeOut)
		!= wSizeOut ) goto PutErrS;
		ADDPOS(scrpos,-wSize);
		ADDPOS(newind->position, wSizeOut);
		ADDPOS(AR.StoreData.Fill, wSizeOut);
	} while ( ISPOSPOS(scrpos) );
	/* then copy the expression itself */
	wSize = TOLONG(AT.WorkTop) - TOLONG(AT.WorkPointer);
	scrpos = ind->length;
#endif
	do {
		wSize = TOLONG(AT.WorkTop) - TOLONG(AT.WorkPointer);
		if ( ISLESSPOS(scrpos,scrpos1) ) wSize = BASEPOSITION(scrpos);
#ifdef SYSDEPENDENTSAVE
		if ( ReadFile(AO.SaveData.Handle,(UBYTE *)AT.WorkPointer,wSize)
		!= wSize ) goto PutErrS;
		if ( WriteFile(AR.StoreData.Handle,(UBYTE *)AT.WorkPointer,wSize)
		!= wSize ) goto PutErrS;
		ADDPOS(scrpos,-wSize);
#else
		wSizeOut = wSize;

		if ( ReadSaveExpression((UBYTE *)AT.WorkPointer, (UBYTE *)AT.WorkTop, &wSize, &wSizeOut) ) {
			goto PutErrS;
		}

		if ( WriteFile(AR.StoreData.Handle, (UBYTE *)AT.WorkPointer, wSizeOut)
		!= wSizeOut ) goto PutErrS;
		ADDPOS(scrpos,-wSize);
		ADDPOS(AR.StoreData.Fill, wSizeOut);
		ADDPOS(newind->length, wSizeOut);
#endif
	} while ( ISPOSPOS(scrpos) );
	/* free look-up table for tensor functions */
	if ( ind->nfunctions ) {
		M_free(AO.tensorList,"PutInStore");
	}
	scrpos = AR.StoreData.Position;
	SeekFile(AR.StoreData.Handle,&scrpos,SEEK_SET);
	if ( ISNOTEQUALPOS(scrpos,AR.StoreData.Position) ) goto PutErrS;
	if ( WriteFile(AR.StoreData.Handle,(UBYTE *)(&AR.StoreData.Index),(LONG)sizeof(FILEINDEX))
	== (LONG)sizeof(FILEINDEX) ) return(0);
PutErrS:
	return(MesPrint("File error"));
}

/*
 		#] PutInStore : 
 		#[ GetTerm :

		Gets one term from input scratch stream.
		Puts it in 'term'.
		Returns the length of the term.

		Used by Processor        (proces.c)
		        WriteAll         (sch.c)
		        WriteOne         (sch.c)
		        GetMoreTerms     (store.c)
		        ToStorage        (store.c)
		        CoFillExpression (comexpr.c)
				FactorInExpr     (factor.c)
		        LoadOpti         (optim.c)
				PF_Processor     (parallel.c)
				ThreadsProcessor (threads.c)
		In multi thread/processor mode all calls are done by the master.
		Note however that other routines, used by the threads, can use
		the same file. Hence we need to be careful about SeekFile and locks.
*/
 
WORD GetTerm(PHEAD WORD *term)
{
	GETBIDENTITY
	WORD *inp, i, j = 0, len;
	LONG InIn, *whichInInBuf;
	WORD *r, *m, *mstop = 0, minsiz = 0, *bra = 0, *from;
	WORD first, *start = 0, testing = 0;
	FILEHANDLE *fi;
	AN.deferskipped = 0;
	if ( AR.GetFile == 2 ) {
		fi = AR.hidefile;
		whichInInBuf = &(AR.InHiBuf);
	}
	else {
		fi = AR.infile;
		whichInInBuf = &(AR.InInBuf);
	}
	InIn = *whichInInBuf;
	from = term;
	if ( AR.KeptInHold ) {
		r = AR.CompressBuffer;
		i = *r;
		AR.KeptInHold = 0;
		if ( i <= 0 ) { *term = 0; goto RegRet; }
		m = term;
		NCOPY(m,r,i);
		goto RegRet;
	}
	if ( AR.DeferFlag ) {
		m = AR.CompressBuffer;
		if ( *m > 0 ) {
			mstop = m + *m;
			mstop -= ABS(mstop[-1]);
			m++;
			while ( m < mstop ) {
				if ( *m == HAAKJE ) {
					testing = 1;
					mstop = m + m[1];
				    bra = (WORD *)(((UBYTE *)(term)) + 2*AM.MaxTer);
					m = AR.CompressBuffer+1;
					r = bra;
					while ( m < mstop ) *r++ = *m++;
					mstop = r;
					minsiz = WORDDIF(mstop,bra);
					goto ReStart;
/*
					We have the bracket to be tested in bra till mstop
*/
				}
				m += m[1];
			}
		}
	    bra = (WORD *)(((UBYTE *)(term)) + 2*AM.MaxTer);
		mstop = bra+1;
		*bra = 0;
		minsiz = 1;
		testing = 1;
	}
ReStart:
	first = 0;
	r = AR.CompressBuffer;
	if ( fi->handle >= 0 ) {
		if ( InIn <= 0 ) {
			ADDPOS(fi->POposition,(fi->POfull-fi->PObuffer)*sizeof(WORD));
			LOCK(AS.inputslock);
			SeekFile(fi->handle,&(fi->POposition),SEEK_SET);
			InIn = ReadFile(fi->handle,(UBYTE *)(fi->PObuffer),fi->POsize);
			UNLOCK(AS.inputslock);
			if ( ( InIn < 0 ) || ( InIn & 1 ) ) {
				goto GTerr;
			}
#ifdef WORD2
			InIn >>= 1;
#else
			InIn /= TABLESIZE(WORD,UBYTE);
#endif
			*whichInInBuf = InIn;
			if ( !InIn ) { *r = 0; *from = 0; goto RegRet; }
			fi->POfill = fi->PObuffer;
			fi->POfull = fi->PObuffer + InIn;
		}
		inp = fi->POfill;
		if ( ( len = i = *inp ) == 0 ) {
			(*whichInInBuf)--;
			(fi->POfill)++;
			*r = 0;
			*from = 0;
			goto RegRet;
		}
		if ( i < 0 ) {
			InIn--;
			inp++;
			r++;
			start = term;
			*term++ = -i + 1;
			while ( ++i <= 0 ) *term++ = *r++;
			if ( InIn > 0 ) {
				i = *inp++;
				InIn--;
				*start += i;
				*(AR.CompressBuffer) = len = *start;
			}
			else {
				first = 1;
				goto NewIn;
			}
		}
		InIn -= i;
		if ( InIn < 0 ) {
			j = (WORD)(- InIn);
			i -= j;
		}
		else j = 0;
		while ( --i >= 0 ) {
				*r++ = *term++ = *inp++;
		}
		if ( j ) {
NewIn:
			ADDPOS(fi->POposition,(fi->POfull-fi->PObuffer)*sizeof(WORD));
			LOCK(AS.inputslock);
			SeekFile(fi->handle,&(fi->POposition),SEEK_SET);
			InIn = ReadFile(fi->handle,(UBYTE *)(fi->PObuffer),fi->POsize);
			UNLOCK(AS.inputslock);
			if ( ( InIn <= 0 ) || ( InIn & 1 ) ) {
				goto GTerr;
			}
#ifdef WORD2
			InIn >>= 1;
#else
			InIn /= TABLESIZE(WORD,UBYTE);
#endif
			inp = fi->PObuffer;
			fi->POfull = inp + InIn;

			if ( first ) {
				j = *inp++;
				InIn--;
				*start += j;
				*(AR.CompressBuffer) = len = *start;
			}
			InIn -= j;
			while ( --j >= 0 ) { *r++ = *term++ = *inp++; }
		}
		fi->POfill = inp;
		*whichInInBuf = InIn;
		AR.DefPosition = fi->POposition;
		ADDPOS(AR.DefPosition,((UBYTE *)(fi->POfill)-(UBYTE *)(fi->PObuffer)));
	}
	else {
		inp = fi->POfill;
		if ( inp >= fi->POfull ) { *from = 0; goto RegRet; }
		len = j = *inp;
		if ( j < 0 ) {
			inp++;
			*term++ = *r++ = len = - j + 1 + *inp;
			while ( ++j <= 0 ) *term++ = *r++;
			j = *inp++;
		}
		else if ( !j ) j = 1;
		while ( --j >= 0 ) { *r++ = *term++ = *inp++; }
		fi->POfill = inp;
/*%%%%%ADDED 7-apr-2006 for Keep Brackets in bucket */
		SETBASEPOSITION(AR.DefPosition,((UBYTE *)(fi->POfill)-(UBYTE *)(fi->PObuffer)));
		if ( inp > fi->POfull ) {
			goto GTerr;
		}
	}
	if ( r >= AR.ComprTop ) {
		MesPrint("CompressSize of %10l is insufficient",AM.CompressSize);
		Terminate(-1);
	}
	AR.CompressPointer = r; *r = 0;
/*
		The next *from is a bug fix that made the program read in forbidden
		territory.
*/
	if ( testing && *from != 0 ) {
		WORD jj;
		r = from;
		jj = *r - 1 - ABS(*(r+*r-1));
		if ( jj < minsiz ) goto strip;
		r++;
		m = bra;
		while ( m < mstop ) {
			if ( *m != *r ) {
strip:			r = from;
				m = r + *r;
				mstop = m - ABS(m[-1]);
				r++;
				while ( r < mstop ) {
					if ( *r == HAAKJE ) {
						*r++ = 1;
						*r++ = 1;
						*r++ = 3;
						len = WORDDIF(r,from);
						*from = len;
						goto RegRet;
					}
					r += r[1];
				}
				goto RegRet;
			}
			m++;
			r++;
		}
		term = from;
		AN.deferskipped++;
		goto ReStart;
	}
RegRet:;
/*
			#[ debug :
*/
	{
		UBYTE OutBuf[140];
/*		if ( AP.DebugFlag ) { */
		if ( ( AP.PreDebug & DUMPINTERMS ) == DUMPINTERMS ) {
			MLOCK(ErrorMessageLock);
			AO.OutFill = AO.OutputLine = OutBuf;
			AO.OutSkip = 3;
			FiniLine();
			r = from;
			i = *r;
			TokenToLine((UBYTE *)("Input: "));
			if ( i == 0 ) {
				TokenToLine((UBYTE *)"zero");
			}
			else if ( i < 0 ) {
				TokenToLine((UBYTE *)"negative!!");
			}
			else {
				while ( --i >= 0 ) {
					TalToLine((UWORD)(*r++)); TokenToLine((UBYTE *)"  ");
				}
			}
			FiniLine();
			MUNLOCK(ErrorMessageLock);
		}
	}
/*
			#] debug : 
*/
	return(*from);
GTerr:
	MesPrint("Error while reading scratch file in GetTerm");
	Terminate(-1);
	return(-1);
}

/*
 		#] GetTerm : 
 		#[ GetOneTerm :

		Gets one term from stream AR.infile->handle.
		Puts it in 'term'.
		Returns the length of the term.
		Input is unbuffered.
		Compression via AR.CompressPointer
		par is actually in all calls a file handle

		Routine is called from
			DoOnePow           Get one power of an expression
			Deferred           Get the contents of a bracket
			GetFirstBracket
			FindBracket
		We should do something about the lack of buffering.
		Maybe a buffer of a few times AM.MaxTer (MaxTermSize*sizeof(WORD)).
		Each thread will need its own buffer!

		If par == 0 we use ReadPosFile which can fill the whole buffer.
		If par == 1 we use ReadFile and do actual read operations.

		Note: we cannot use ReadPosFile when running in the master thread.
*/

WORD GetOneTerm(PHEAD WORD *term, FILEHANDLE *fi, POSITION *pos, int par)
{
	GETBIDENTITY
	WORD i, *p;
	LONG j, siz;
	WORD *r, *rr = AR.CompressPointer;
	int error = 0;
	r = rr;
	if ( fi->handle >= 0 ) {
#ifdef READONEBYONE
#ifdef WITHPTHREADS
/*
		This code needs some investigation.
		It may be that we should do this always.
		It may be that even for workers it is no good.
		We may have to make a variable like AM.ReadDirect with
		if ( AM.ReadDirect ) par = 1;
		and a user command like
		On ReadDirect;
*/
		if ( AT.identity > 0 ) par = 1;
#endif
#endif
/*
		To be changed:
		1: check first whether the term lies completely inside the buffer
		2: if not a: use old strategy for AT.identity == 0 (master)
		          b: for workers, position file and read buffer
*/
		if ( par == 0 ) {
			siz = ReadPosFile(BHEAD fi,(UBYTE *)term,1L,pos);
		}
		else {
			LOCK(AS.inputslock);
			SeekFile(fi->handle,pos,SEEK_SET);
			siz = ReadFile(fi->handle,(UBYTE *)term,sizeof(WORD));
			UNLOCK(AS.inputslock);
			ADDPOS(*pos,siz);
		}
		if ( siz == sizeof(WORD) ) {
			p = term;
			j = i = *term++;
			if ( ( i > AM.MaxTer/((WORD)sizeof(WORD)) ) || ( -i >= AM.MaxTer/((WORD)sizeof(WORD)) ) )
			{
				error = 1;
				goto ErrGet;
			}
			r++;
			if ( i < 0 ) {
				*p = -i + 1;
				while ( ++i <= 0 ) *term++ = *r++;
				if ( par == 0 ) {
					siz = ReadPosFile(BHEAD fi,(UBYTE *)term,1L,pos);
				}
				else {
					LOCK(AS.inputslock);
					SeekFile(fi->handle,pos,SEEK_SET);
					siz = ReadFile(fi->handle,(UBYTE *)term,sizeof(WORD));
					UNLOCK(AS.inputslock);
					ADDPOS(*pos,sizeof(WORD));
				}
				if ( siz != sizeof(WORD) ) {
					error = 2;
					goto ErrGet;
				}
				*p += *term;
				j = *term;
				if ( ( j > AM.MaxTer/((WORD)sizeof(WORD)) ) || ( j <= 0 ) )
				{
					error = 3;
					goto ErrGet;
				}
				*rr = *p;
			}
			else {
				if ( !j ) return(0);
				j--;
			}
			i = (WORD)j;
			if ( par == 0 ) {
				siz = ReadPosFile(BHEAD fi,(UBYTE *)term,j,pos);
				j *= TABLESIZE(WORD,UBYTE);
			}
			else {
				j *= TABLESIZE(WORD,UBYTE);
				LOCK(AS.inputslock);
				SeekFile(fi->handle,pos,SEEK_SET);
				siz = ReadFile(fi->handle,(UBYTE *)term,j);
				UNLOCK(AS.inputslock);
				ADDPOS(*pos,j);
			}
			if ( siz != j ) {
				error = 4;
				goto ErrGet;
			}
			while ( --i >= 0 ) *r++ = *term++;
			if ( r >= AR.ComprTop ) {
				MLOCK(ErrorMessageLock);
				MesPrint("CompressSize of %10l is insufficient",AM.CompressSize);
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			AR.CompressPointer = r; *r = 0;
			return(*p);
		}
		error = 5;
	}
	else {
/*
		Here the whole expression is in the buffer.
*/
		fi->POfill = (WORD *)((UBYTE *)(fi->PObuffer) + BASEPOSITION(*pos));
		p = fi->POfill;
		if ( p >= fi->POfull ) { *term = 0; return(0); }
		j = i = *p;
		if ( i < 0 ) {
			p++;
			j = *r++ = *term++ = -i + 1 + *p;
			while ( ++i <= 0 ) *term++ = *r++;
			i = *p++;
		}
		if ( i == 0 ) { i = 1; *r++ = 0; *term++ = 0; }
		else { while ( --i >= 0 ) { *r++ = *term++ = *p++; } }
		fi->POfill = p;
		SETBASEPOSITION(*pos,(UBYTE *)(fi->POfill)-(UBYTE *)(fi->PObuffer));
		if ( p <= fi->POfull ) {
			if ( r >= AR.ComprTop ) {
				MLOCK(ErrorMessageLock);
				MesPrint("CompressSize of %10l is insufficient",AM.CompressSize);
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			AR.CompressPointer = r; *r = 0;
			return((WORD)j);
		}
		error = 6;
	}
ErrGet:
	MLOCK(ErrorMessageLock);
	MesPrint("Error while reading scratch file in GetOneTerm (%d)",error);
	MUNLOCK(ErrorMessageLock);
	Terminate(-1);
	return(-1);
}

/*
 		#] GetOneTerm : 
 		#[ GetMoreTerms :
	Routine collects more contents of brackets inside a function,
	indicated by the number in AC.CollectFun.
	The first term is in term already.
	We can keep calling GetTerm either till a bracket is finished 
	or till it would make the term too long (> AM.MaxTer/2)
	In all cases this function makes that the routine GetTerm
	has a term in 'hold', so the AR.KeptInHold flag must be turned on.
*/

WORD GetMoreTerms(WORD *term)
{
	GETIDENTITY
	WORD *t, *r, *m, *h, *tstop, i, inc, same;
	WORD extra;
	WORD retval = 0;
/*
	We use 23% as a quasi-random default value.
*/
	extra = ((AM.MaxTer/sizeof(WORD))*((LONG)100-AC.CollectPercentage))/100;
	if ( extra < 23 ) extra = 23;
/*
	First find the bracket pointer
*/
	t = term + *term;
	tstop = t - ABS(t[-1]);
	h = term+1;
	while ( *h != HAAKJE && h < tstop ) h += h[1];
	if ( h >= tstop ) return(retval);
	inc = FUNHEAD+ARGHEAD+1-h[1];
	same = WORDDIF(h,term) + h[1] - 1;
	r = m = t + inc;
	tstop = h + h[1];
	while ( t > tstop ) *--r = *--t;
	r--;
	*r = WORDDIF(m,r);
	while ( GetTerm(BHEAD m) > 0 ) {
		r = m + 1;
		t = m + *m - 1;
		if ( same > ( i = ( *m - ABS(*t) -1 ) ) ) { /* Must fail */
			if ( AC.AltCollectFun && AS.CollectOverFlag == 2 ) AS.CollectOverFlag = 3;
			break;
		}
		t = term+1;
		i = same;
		while ( --i >= 0 ) {
			if ( *r != *t ) {
				if ( AC.AltCollectFun && AS.CollectOverFlag == 2 ) AS.CollectOverFlag = 3;
				goto FullTerm;
			}
			r++; t++;
		}
		if ( ( WORDDIF(m,term) + i + extra ) > (WORD)(AM.MaxTer/sizeof(WORD)) ) {
/* 23 = 3 +20. The 20 is to have some extra for substitutions or whatever */
			if ( AS.CollectOverFlag == 0 && AC.AltCollectFun == 0 ) {
				Warning("Bracket contents too long in Collect statement");
				Warning("Contents spread over more than one term");
				Warning("If possible: increase MaxTermSize in setfile");
				AS.CollectOverFlag = 1;
			}
			else if ( AC.AltCollectFun ) {
				AS.CollectOverFlag = 2;
			}
			break;
		}
		tstop = m + *m;
		*m -= same;
		m++;
		while ( r < tstop ) *m++ = *r++;
		retval++;
		if ( extra == 23 ) extra = ((AM.MaxTer/sizeof(WORD))/6);
	}
FullTerm:
	h[1] = WORDDIF(m,h);
	if ( AS.CollectOverFlag > 1 ) {
		*h = AC.AltCollectFun;
		if ( AS.CollectOverFlag == 3 ) AS.CollectOverFlag = 1;
	}
	else *h = AC.CollectFun;
	h[2] |= DIRTYFLAG;
	h[FUNHEAD] = h[1] - FUNHEAD;
	h[FUNHEAD+1] = 0;
	if ( ToFast(h+FUNHEAD,h+FUNHEAD) ) {
		if ( h[FUNHEAD] <= -FUNCTION ) {
			h[1] = FUNHEAD+1;
			m = h + FUNHEAD+1;
		}
		else {
			h[1] = FUNHEAD+2;
			m = h + FUNHEAD+2;
		}
	}
	*m++ = 1;
	*m++ = 1;
	*m++ = 3;
	*term = WORDDIF(m,term);
	AR.KeptInHold = 1;
	return(retval);
}

/*
 		#] GetMoreTerms : 
 		#[ GetMoreFromMem :

*/

WORD GetMoreFromMem(WORD *term, WORD **tpoin)
{
	GETIDENTITY
	WORD *t, *r, *m, *h, *tstop, i, j, inc, same;
	LONG extra = 23;
/*
	First find the bracket pointer
*/
	t = term + *term;
	tstop = t - ABS(t[-1]);
	h = term+1;
	while ( *h != HAAKJE && h < tstop ) h += h[1];
	if ( h >= tstop ) return(0);
	inc = FUNHEAD+ARGHEAD+1-h[1];
	same = WORDDIF(h,term) + h[1] - 1;
	r = m = t + inc;
	tstop = h + h[1];
	while ( t > tstop ) *--r = *--t;
	r--;
	*r = WORDDIF(m,r);
	while ( **tpoin ) {
		r = *tpoin; j = *r;
		for ( i = 0; i < j; i++ ) m[i] = *r++;
		*tpoin = r;		
		r = m + 1;
		t = m + *m - 1;
		if ( same > ( i = ( *m - ABS(*t) -1 ) ) ) { /* Must fail */
			if ( AC.AltCollectFun && AS.CollectOverFlag == 2 ) AS.CollectOverFlag = 3;
			break;
		}
		t = term+1;
		i = same;
		while ( --i >= 0 ) {
			if ( *r != *t ) {
				if ( AC.AltCollectFun && AS.CollectOverFlag == 2 ) AS.CollectOverFlag = 3;
				goto FullTerm;
			}
			r++; t++;
		}
		if ( ( WORDDIF(m,term) + i + extra ) > (LONG)(AM.MaxTer/(2*sizeof(WORD))) ) {
/* 23 = 3 +20. The 20 is to have some extra for substitutions or whatever */
			if ( AS.CollectOverFlag == 0 && AC.AltCollectFun == 0 ) {
				Warning("Bracket contents too long in Collect statement");
				Warning("Contents spread over more than one term");
				Warning("If possible: increase MaxTermSize in setfile");
				AS.CollectOverFlag = 1;
			}
			else if ( AC.AltCollectFun ) {
				AS.CollectOverFlag = 2;
			}
			break;
		}
		tstop = m + *m;
		*m -= same;
		m++;
		while ( r < tstop ) *m++ = *r++;
		if ( extra == 23 ) extra = ((AM.MaxTer/sizeof(WORD))/6);
	}
FullTerm:
	h[1] = WORDDIF(m,h);
	if ( AS.CollectOverFlag > 1 ) {
		*h = AC.AltCollectFun;
		if ( AS.CollectOverFlag == 3 ) AS.CollectOverFlag = 1;
	}
	else *h = AC.CollectFun;
	h[2] |= DIRTYFLAG;
	h[FUNHEAD] = h[1] - FUNHEAD;
	h[FUNHEAD+1] = 0;
	if ( ToFast(h+FUNHEAD,h+FUNHEAD) ) {
		if ( h[FUNHEAD] <= -FUNCTION ) {
			h[1] = FUNHEAD+1;
			m = h + FUNHEAD+1;
		}
		else {
			h[1] = FUNHEAD+2;
			m = h + FUNHEAD+2;
		}
	}
	*m++ = 1;
	*m++ = 1;
	*m++ = 3;
	*term = WORDDIF(m,term);
	AR.KeptInHold = 1;
	return(0);
}

/*
 		#] GetMoreFromMem : 
 		#[ GetFromStore :

		Gets a single term from the storage file at position and puts
		it at 'to'.
		The value to be returned is the number of words read.
		Renumbering is done also.
		This is controled by the renumber table, given in 'renumber'

		This routine should work with a number of cache buffers. The
		exact number should be definable in form.set.
		The parameters are:
		AM.SizeStoreCache	(4096)
		The numbers are the proposed default values.

		The cache is a pure read cache.
*/

static int gfs = 0;

WORD GetFromStore(WORD *to, POSITION *position, RENUMBER renumber, WORD *InCompState, WORD nexpr)
{
	GETIDENTITY
	LONG RetCode, num, first = 0;
	WORD *from, *m;
	struct StOrEcAcHe sc;
	STORECACHE s;
	STORECACHE snext, sold;
	WORD *r, *rr = AR.CompressPointer;
	r = rr;
	gfs++;
	sc.next = AT.StoreCache;
	sold = s = &sc;
	snext = s->next;
	while ( snext ) {
		sold = s;
		s = snext;
		snext = s->next;
		if ( BASEPOSITION(s->position) == -1 ) break;
		if ( ISLESSPOS(*position,s->toppos) &&
		ISGEPOS(*position,s->position) ) {	/* Hit */
			if ( AT.StoreCache != s ) {
				sold->next = s->next;
				s->next = AT.StoreCache->next;
				AT.StoreCache = s;
			}
			from = (WORD *)(((UBYTE *)(s->buffer)) + DIFBASE(*position,s->position));
			num = *from;
			if ( !num ) { return(*to = 0); }
			*InCompState = (WORD)num;
			m = to;
			if ( num < 0 ) {
				from++;
				ADDPOS(*position,sizeof(WORD));
				*m++ = (WORD)(-num+1);
				r++;
				while ( ++num <= 0 ) *m++ = *r++;
				if ( ISLESSPOS(*position,s->toppos) ) {
					num = *from++;
					*to += (WORD)num;
					ADDPOS(*position,sizeof(WORD));
					*InCompState = (WORD)(num + 2);
				}
				else {
					first = 1;
					goto InNew;
				}
			}
PastCon:;
			while ( num > 0 && ISLESSPOS(*position,s->toppos) ) {
				*r++ = *m++ = *from++; ADDPOS(*position,sizeof(WORD)); num--;
			}
			if ( num > 0 ) {
InNew:
				SETBASEPOSITION(s->position,-1);
				SETBASEPOSITION(s->toppos,-1);
				LOCK(AM.storefilelock);
				SeekFile(AR.StoreData.Handle,position,SEEK_SET);
				RetCode = ReadFile(AR.StoreData.Handle,(UBYTE *)(s->buffer),AM.SizeStoreCache);
				UNLOCK(AM.storefilelock);
				if ( RetCode < 0 ) goto PastErr;
				if ( !RetCode ) return( *to = 0 );
				s->position = *position;
				s->toppos = *position;
				ADDPOS(s->toppos,RetCode);
				from = s->buffer;
				if ( first ) {
					num = *from++;
					ADDPOS(*position,sizeof(WORD));
					*to += (WORD)num;
/*					first = 0; */
					*InCompState = (WORD)(num + 2);
				}
				goto PastCon;
			}
			goto PastEnd;
		}
	}
	if ( AT.StoreCache ) {		/* Fill the last buffer */
		s->position = *position;
		LOCK(AM.storefilelock);
		SeekFile(AR.StoreData.Handle,position,SEEK_SET);
		RetCode = ReadFile(AR.StoreData.Handle,(UBYTE *)(s->buffer),AM.SizeStoreCache);
		UNLOCK(AM.storefilelock);
		if ( RetCode < 0 ) goto PastErr;
		if ( !RetCode ) return( *to = 0 );
		s->toppos = *position;
		ADDPOS(s->toppos,RetCode);
		if ( AT.StoreCache != s ) {
			sold->next = s->next;
			s->next = AT.StoreCache->next;
			AT.StoreCache = s;
		}
		m = to;
		from = s->buffer;
		num = *from;
		if ( !num ) { return( *to = 0 ); }
		*InCompState = (WORD)num;
		if ( num < 0 ) {
			*m++ = (WORD)(-num+1);
			r++;
			from++;
			ADDPOS(*position,sizeof(WORD));
			while ( ++num <= 0 ) *m++ = *r++;
			num = *from++;
			*to += (WORD)num;
			ADDPOS(*position,sizeof(WORD));
			*InCompState = (WORD)(num+2);
		}
		goto PastCon;
	}
/*		No caching available */
	LOCK(AM.storefilelock);
	SeekFile(AR.StoreData.Handle,position,SEEK_SET);
	RetCode = ReadFile(AR.StoreData.Handle,(UBYTE *)to,(LONG)sizeof(WORD));
	SeekFile(AR.StoreData.Handle,position,SEEK_CUR);
	UNLOCK(AM.storefilelock);
	if ( RetCode != sizeof(WORD) ) {
		*to = 0;
		return((WORD)RetCode);
	}
	if ( !*to ) return(0);
	m = to;
	if ( *to < 0 ) {
		num = *m++;
		*to = *r++ = (WORD)(-num + 1);
		while ( ++num <= 0 ) *m++ = *r++;
		LOCK(AM.storefilelock);
		SeekFile(AR.StoreData.Handle,position,SEEK_SET);
		RetCode = ReadFile(AR.StoreData.Handle,(UBYTE *)m,(LONG)sizeof(WORD));
		SeekFile(AR.StoreData.Handle,position,SEEK_CUR);
		UNLOCK(AM.storefilelock);
		if ( RetCode != sizeof(WORD) ) {
			MLOCK(ErrorMessageLock);
			MesPrint("@Error in compression of store file");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		num = *m;
		*to += (WORD)num;
		*InCompState = (WORD)(num + 2);
	}
	else {
		*InCompState = *to;
		num = *to - 1; m = to + 1; r = rr + 1;
	}
	first = num;
	num *= wsizeof(WORD);
	if ( num < 0 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("@Error in stored expressions file at position %9p",position);
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	LOCK(AM.storefilelock);
	SeekFile(AR.StoreData.Handle,position,SEEK_SET);
	RetCode = ReadFile(AR.StoreData.Handle,(UBYTE *)m,num);
	SeekFile(AR.StoreData.Handle,position,SEEK_CUR);
	UNLOCK(AM.storefilelock);
	if ( RetCode != num ) {
		MLOCK(ErrorMessageLock);
		MesPrint("@Error in stored expressions file at position %9p",position);
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	NCOPY(r,m,first);
PastEnd:
	*rr = *to;
	if ( r >= AR.ComprTop ) {
		MLOCK(ErrorMessageLock);
		MesPrint("CompressSize of %10l is insufficient",AM.CompressSize);
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	AR.CompressPointer = r; *r = 0;
	if ( !TermRenumber(to,renumber,nexpr) ) {
		MarkDirty(to,DIRTYSYMFLAG);
		if ( AR.CurDum > AM.IndDum && Expressions[nexpr].numdummies > 0 )
			MoveDummies(BHEAD to,AR.CurDum - AM.IndDum);
		return((WORD)*to);
	}
PastErr:
	MLOCK(ErrorMessageLock);
	MesCall("GetFromStore");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] GetFromStore : 
 		#[ DetVars :			VOID DetVars(term)

	Determines which variables are used in term.

	When par = 1 we are scanning a prototype expression which involves
	completely different rules.

*/

VOID DetVars(WORD *term, WORD par)
{
	GETIDENTITY
	WORD *stopper;
	WORD *t, sym;
	WORD *sarg;
	stopper = term + *term - 1;
	stopper = stopper - ABS(*stopper) + 1;
	term++;
	if ( par ) {		/* Prototype expression */
		WORD n;
		if ( ( n = NumSymbols ) > 0 ) {
			SYMBOLS tt;
			tt = symbols;
			do {
				(tt++)->flags &= ~INUSE;
			} while ( --n > 0 );
		}
		if ( ( n = NumIndices ) > 0 ) {
			INDICES tt;
			tt = indices;
			do {
				(tt++)->flags &= ~INUSE;
			} while ( --n > 0 );
		}
		if ( ( n = NumVectors ) > 0 ) {
			VECTORS tt;
			tt = vectors;
			do {
				(tt++)->flags &= ~INUSE;
			} while ( --n > 0 );
		}
		if ( ( n = NumFunctions ) > 0 ) {
			FUNCTIONS tt;
			tt = functions;
			do {
				(tt++)->flags &= ~INUSE;
			} while ( --n > 0 );
		}
		term += SUBEXPSIZE;
		while ( term < stopper ) {
			if ( *term == SYMTOSYM || *term == SYMTONUM ) {
				term += 2;
				AN.UsedSymbol[*term] = 1;
				symbols[*term].flags |= INUSE;
			}
			else if ( *term == VECTOVEC ) {
				term += 2;
				AN.UsedVector[*term-AM.OffsetVector] = 1;
				vectors[*term-AM.OffsetVector].flags |= INUSE;
			}
			else if ( *term == INDTOIND ) {
				term += 2;
				sym = indices[*term - AM.OffsetIndex].dimension;
				if ( sym < 0 ) AN.UsedSymbol[-sym] = 1;
				AN.UsedIndex[(*term) - AM.OffsetIndex] = 1;
				sym = indices[*term-AM.OffsetIndex].nmin4;
				if ( sym < -NMIN4SHIFT ) AN.UsedSymbol[-sym-NMIN4SHIFT] = 1;
				indices[*term-AM.OffsetIndex].flags |= INUSE;
			}
			else if ( *term == FUNTOFUN ) {
				term += 2;
				AN.UsedFunction[*term-FUNCTION] = 1;
				functions[*term-FUNCTION].flags |= INUSE;
			}
			term += 2;
		}
	}
	else {
		while ( term < stopper ) {
			t = term + term[1];
			if ( *term == SYMBOL ) {
				term += 2;
				do {
					AN.UsedSymbol[*term] = 1;
					term += 2;
				} while ( term < t );
			}
			else if ( *term == DOTPRODUCT ) {
				term += 2;
				do {
					AN.UsedVector[(*term++) - AM.OffsetVector] = 1;
					AN.UsedVector[(*term) - AM.OffsetVector] = 1;
					term += 2;
				} while ( term < t );
			}
			else if ( *term == VECTOR ) {
				term += 2;
				do {
					AN.UsedVector[(*term++) - AM.OffsetVector] = 1;
					if ( *term >= AM.OffsetIndex && *term < AM.DumInd ) {
						sym = indices[*term - AM.OffsetIndex].dimension;
						if ( sym < 0 ) AN.UsedSymbol[-sym] = 1;
						AN.UsedIndex[*term - AM.OffsetIndex] = 1;
						sym = indices[(*term++)-AM.OffsetIndex].nmin4;
						if ( sym < -NMIN4SHIFT ) AN.UsedSymbol[-sym-NMIN4SHIFT] = 1;
					}
					else term++;
				} while ( term < t );
			}
			else if ( *term == INDEX || *term == LEVICIVITA || *term == GAMMA
			|| *term == DELTA ) {
/*
Tensors:
				term += 2;
*/
				if ( *term == INDEX || *term == DELTA ) term += 2;
				else {
Tensors:
					term += FUNHEAD;
				}
				while ( term < t ) {
					if ( *term  >= AM.OffsetIndex && *term < AM.DumInd ) {
						sym = indices[*term - AM.OffsetIndex].dimension;
						if ( sym < 0 ) AN.UsedSymbol[-sym] = 1;
						AN.UsedIndex[(*term) - AM.OffsetIndex] = 1;
						sym = indices[*term-AM.OffsetIndex].nmin4;
						if ( sym < -NMIN4SHIFT ) AN.UsedSymbol[-sym-NMIN4SHIFT] = 1;
					}
					else if ( *term < (WILDOFFSET+AM.OffsetVector) )
						AN.UsedVector[(*term) - AM.OffsetVector] = 1;
					term++;
				}
			}
			else if ( *term == HAAKJE ) term = t;
			else {
				if ( *term > MAXBUILTINFUNCTION )
					AN.UsedFunction[(*term)-FUNCTION] = 1;
				if ( *term >= FUNCTION && functions[*term-FUNCTION].spec
				>= TENSORFUNCTION && term[1] > FUNHEAD ) goto Tensors;
				term += FUNHEAD;			/* First argument */
				while ( term < t ) {
					sarg = term;
					NEXTARG(sarg)
					if ( *term > 0 ) {
						sarg = term + *term;	/* End of argument */
						term += ARGHEAD;		/* First term in argument */
						if ( term < sarg ) { do {
							DetVars(term,par);
							term += *term;
						} while ( term < sarg ); }
					}
					else {
						if ( *term < -MAXBUILTINFUNCTION ) {
							AN.UsedFunction[-*term-FUNCTION] = 1;
						}
						else if ( *term == -SYMBOL ) {
							AN.UsedSymbol[term[1]] = 1;
						}
						else if ( *term == -INDEX ) {
							if ( term[1] < (WILDOFFSET+AM.OffsetVector) ) {
								AN.UsedVector[term[1]-AM.OffsetVector] = 1;
							}
							else if ( term[1] >= AM.OffsetIndex && term[1] < AM.DumInd ) {
								sym = indices[term[1] - AM.OffsetIndex].dimension;
								if ( sym < 0 ) AN.UsedSymbol[-sym] = 1;
								AN.UsedIndex[term[1] - AM.OffsetIndex] = 1;
								sym = indices[term[1]-AM.OffsetIndex].nmin4;
								if ( sym < -NMIN4SHIFT ) AN.UsedSymbol[-sym-NMIN4SHIFT] = 1;
							}
						}
						else if ( *term == -VECTOR || *term == -MINVECTOR ) {
							AN.UsedVector[term[1]-AM.OffsetVector] = 1;
						}
					}
					term = sarg;			/* Next argument */
				}
				term = t;
			}
		}
	}
}

/*
 		#] DetVars : 
 		#[ ToStorage :

	This routine takes an expression in the scratch buffer (indicated by e)
	and puts it in the storage file. The necessary actions are:

	1:	determine the list of the used variables.
	2:	make an index entry.
	3:	write the namelists.
	4:	copy the 'length' bytes of the expression.

*/

WORD ToStorage(EXPRESSIONS e, POSITION *length)
{
	GETIDENTITY
	WORD *w, i, j;
	WORD *term;
	INDEXENTRY *indexent;
	LONG size;
	POSITION indexpos, scrpos;
	FILEHANDLE *f;
	if ( ( indexent = NextFileIndex(&indexpos) ) == 0 ) {
		MesCall("ToStorage");
		SETERROR(-1)
	}
	indexent->CompressSize = 0;		/* thus far no compression */
	f = AR.infile; AR.infile = AR.outfile; AR.outfile = f;
	if ( e->status == HIDDENGEXPRESSION ) {
		AR.InHiBuf = 0; f = AR.hidefile; AR.GetFile = 2;
	}
	else {
		AR.InInBuf = 0; f = AR.infile;   AR.GetFile = 0;
	}
	if ( f->handle >= 0 ) {
		scrpos = e->onfile;
		SeekFile(f->handle,&scrpos,SEEK_SET);
		if ( ISNOTEQUALPOS(scrpos,e->onfile) ) {
			MesPrint(":::Error in Scratch file");
			goto ErrReturn;
		}
		f->POposition = e->onfile;
		f->POfull = f->PObuffer;
		if ( e->status == HIDDENGEXPRESSION ) AR.InHiBuf = 0;
		else                                  AR.InInBuf = 0;
	}
	else {
		f->POfill = (WORD *)((UBYTE *)(f->PObuffer)+BASEPOSITION(e->onfile));
	}
	w = AT.WorkPointer;
	AN.UsedSymbol   = w;	w += NumSymbols;
	AN.UsedVector   = w;	w += NumVectors;
	AN.UsedIndex    = w;	w += NumIndices;
	AN.UsedFunction = w;	w += NumFunctions;
	term = w;
    w = (WORD *)(((UBYTE *)(w)) + AM.MaxTer);
	if ( w > AT.WorkTop ) {
		MesWork();
		goto ErrReturn;
	}
	w = AN.UsedSymbol;
	i = NumSymbols + NumVectors + NumIndices + NumFunctions;
	do { *w++ = 0; } while ( --i > 0 );
	if ( GetTerm(BHEAD term) > 0 ) {
		DetVars(term,1);
		if ( GetTerm(BHEAD term) ) {
			do { DetVars(term,0); } while ( GetTerm(BHEAD term) > 0 );
		}
	}
	j = 0;
	w = AN.UsedSymbol;
	i = NumSymbols;
	while ( --i >= 0 ) { if ( *w++ ) j++; }
	indexent->nsymbols = j;
/*	size = j * sizeof(struct SyMbOl);  */
	j = 0;
	w = AN.UsedIndex;
	i = NumIndices;
	while ( --i >= 0 ) { if ( *w++ ) j++; }
	indexent->nindices = j;
/*	size += j * sizeof(struct InDeX);  */
	j = 0;
	w = AN.UsedVector;
	i = NumVectors;
	while ( --i >= 0 ) { if ( *w++ ) j++; }
	indexent->nvectors = j;
/*	size += j * sizeof(struct VeCtOr);  */
	j = 0;
	w = AN.UsedFunction;
	i = NumFunctions;
	while ( --i >= 0 ) { if ( *w++ ) j++; }
	indexent->nfunctions = j;
/*	size += j * sizeof(struct FuNcTiOn); */
	indexent->length = *length;
	indexent->variables = AR.StoreData.Fill;
/*	indexent->position = AR.StoreData.Fill + size; */
	StrCopy(AC.exprnames->namebuffer+e->name,(UBYTE *)(indexent->name));
	SeekFile(AR.StoreData.Handle,&(AR.StoreData.Fill),SEEK_SET);
	AO.wlen = 100000;
	AO.wpos = (UBYTE *)Malloc1(AO.wlen,"AO.wpos buffer");
	AO.wpoin = AO.wpos;
	{
		 SYMBOLS a;
		w = AN.UsedSymbol;
		a = symbols;
		j = 0;
		i = indexent->nsymbols;
		while ( --i >= 0 ) {
			while ( !*w ) { w++; a++; j++; }
			a->number = j;
			if ( VarStore((UBYTE *)a,(WORD)(sizeof(struct SyMbOl)),a->name,
				a->namesize) ) goto ErrToSto;
			w++; j++; a++;
		}
	}
	{
		 INDICES a;
		w = AN.UsedIndex;
		a = indices;
		j = 0;
		i = indexent->nindices;
		while ( --i >= 0 ) {
			while ( !*w ) { w++; a++; j++; }
			a->number = j;
			if ( VarStore((UBYTE *)a,(WORD)(sizeof(struct InDeX)),a->name,
				a->namesize) ) goto ErrToSto;
			w++; j++; a++;
		}
	}
	{
		 VECTORS a;
		w = AN.UsedVector;
		a = vectors;
		j = 0;
		i = indexent->nvectors;
		while ( --i >= 0 ) {
			while ( !*w ) { w++; a++; j++; }
			a->number = j;
			if ( VarStore((UBYTE *)a,(WORD)(sizeof(struct VeCtOr)),a->name,
				a->namesize) ) goto ErrToSto;
			w++; j++; a++;
		}
	}
	{
		 FUNCTIONS a;
		w = AN.UsedFunction;
		a = functions;
		j = 0;
		i = indexent->nfunctions;
		while ( --i >= 0 ) {
			while ( !*w ) { w++; a++; j++; }
			a->number = j;
			if ( VarStore((UBYTE *)a,(WORD)(sizeof(struct FuNcTiOn)),a->name,
				a->namesize) ) goto ErrToSto;
			w++; a++; j++;
		}
	}
	if ( VarStore((UBYTE *)0L,(WORD)0,(WORD)0,(WORD)0) ) goto ErrToSto;	/* Flush buffer */
	TELLFILE(AR.StoreData.Handle,&(indexent->position));
	indexent->size = (WORD)DIFBASE(indexent->position,indexent->variables);
/*
		The following code was added when it became apparent (30-jan-2007)
		that we need provisions for extra space without upsetting existing
		.sav files. Here we can put as much as we want.
		Look in GetTable on how to recover numdummies.
		Forgetting numdummies has been in there from the beginning.
*/
	if ( WriteFile(AR.StoreData.Handle,(UBYTE *)(&(e->numdummies)),(LONG)sizeof(WORD)) != 
		sizeof(WORD) ) {
			MesPrint("Error while writing storage file");
			goto ErrReturn;
	}
	if ( WriteFile(AR.StoreData.Handle,(UBYTE *)(&(e->numfactors)),(LONG)sizeof(WORD)) != 
		sizeof(WORD) ) {
			MesPrint("Error while writing storage file");
			goto ErrReturn;
	}
	if ( WriteFile(AR.StoreData.Handle,(UBYTE *)(&(e->vflags)),(LONG)sizeof(WORD)) != 
		sizeof(WORD) ) {
			MesPrint("Error while writing storage file");
			goto ErrReturn;
	}
	TELLFILE(AR.StoreData.Handle,&(indexent->position));
	if ( f->handle >= 0 ) {
		POSITION llength;
		llength = *length;
		SeekFile(f->handle,&(e->onfile),SEEK_SET);
		while ( ISPOSPOS(llength) ) {
			SETBASEPOSITION(scrpos,AO.wlen);
			if ( ISLESSPOS(llength,scrpos) ) size = BASEPOSITION(llength);
			else				             size = AO.wlen;
			if ( ReadFile(f->handle,AO.wpos,size) != size ) {
				MesPrint("Error while reading scratch file");
				goto ErrReturn;
			}
			if ( WriteFile(AR.StoreData.Handle,AO.wpos,size) != size ) {
				MesPrint("Error while writing storage file");
				goto ErrReturn;
			}
			ADDPOS(llength,-size);
		}
	}
	else {
		WORD *ppp;
		ppp = (WORD *)((UBYTE *)(f->PObuffer) + BASEPOSITION(e->onfile));
		if ( WriteFile(AR.StoreData.Handle,(UBYTE *)ppp,BASEPOSITION(*length)) != 
		BASEPOSITION(*length) ) {
			MesPrint("Error while writing storage file");
			goto ErrReturn;
		}
	}
	ADD2POS(*length,indexent->position);
	e->onfile = indexpos;
/*
	AR.StoreData.Fill = SeekFile(AR.StoreData.Handle,&(AM.zeropos),SEEK_END);
*/
	AR.StoreData.Fill = *length;
	SeekFile(AR.StoreData.Handle,&(AR.StoreData.Fill),SEEK_SET);
	scrpos = AR.StoreData.Position;
	ADDPOS(scrpos,sizeof(POSITION));
	SeekFile(AR.StoreData.Handle,&scrpos,SEEK_SET);
	if ( WriteFile(AR.StoreData.Handle,((UBYTE *)&(AR.StoreData.Index.number))
		,(LONG)(sizeof(POSITION))) != sizeof(POSITION) ) goto ErrInSto;
	SeekFile(AR.StoreData.Handle,&indexpos,SEEK_SET);
	if ( WriteFile(AR.StoreData.Handle,(UBYTE *)indexent,(LONG)(sizeof(INDEXENTRY))) !=
		sizeof(INDEXENTRY) ) goto ErrInSto;
	FlushFile(AR.StoreData.Handle);
	SeekFile(AR.StoreData.Handle,&(AC.StoreFileSize),SEEK_END);
	f = AR.infile; AR.infile = AR.outfile; AR.outfile = f;
	if ( AO.wpos ) M_free(AO.wpos,"AO.wpos buffer");
	AO.wpos = AO.wpoin = 0;
	return(0);
ErrToSto:
	MesPrint("---Error while storing namelists");
	goto ErrReturn;
ErrInSto:
	MesPrint("Error in storage");
ErrReturn:
	if ( AO.wpos ) M_free(AO.wpos,"AO.wpos buffer");
	AO.wpos = AO.wpoin = 0;
	f = AR.infile; AR.infile = AR.outfile; AR.outfile = f;
	return(-1);
}

/*
 		#] ToStorage : 
 		#[ NextFileIndex :
*/

INDEXENTRY *NextFileIndex(POSITION *indexpos)
{
	GETIDENTITY
	INDEXENTRY *ind;
	int i, j = sizeof(FILEINDEX)/(sizeof(LONG));
	LONG *lo;
	if ( AR.StoreData.Handle <= 0 ) {
		if ( SetFileIndex() ) {
			MesCall("NextFileIndex");
			return(0);
		}
		SETBASEPOSITION(AR.StoreData.Index.number,1);
#ifdef SYSDEPENDENTSAVE
		SETBASEPOSITION(*indexpos,(2*sizeof(POSITION)));
#else
		SETBASEPOSITION(*indexpos,(2*sizeof(POSITION)+sizeof(STOREHEADER)));
#endif
		return(AR.StoreData.Index.expression);
	}
	while ( BASEPOSITION(AR.StoreData.Index.number) >= (LONG)(INFILEINDEX) ) {
		if ( ISNOTZEROPOS(AR.StoreData.Index.next) ) {
			SeekFile(AR.StoreData.Handle,&(AR.StoreData.Index.next),SEEK_SET);
			AR.StoreData.Position = AR.StoreData.Index.next;
			if ( ReadFile(AR.StoreData.Handle,(UBYTE *)(&AR.StoreData.Index),(LONG)(sizeof(FILEINDEX))) !=
			(LONG)(sizeof(FILEINDEX)) ) goto ErrNextS;
		}
		else {
			PUTZERO(AR.StoreData.Index.number);
			SeekFile(AR.StoreData.Handle,&(AR.StoreData.Position),SEEK_SET);
			if ( WriteFile(AR.StoreData.Handle,(UBYTE *)(&(AR.StoreData.Fill)),(LONG)(sizeof(POSITION)))
			!= (LONG)(sizeof(POSITION)) ) goto ErrNextS;
			PUTZERO(AR.StoreData.Index.next);
			SeekFile(AR.StoreData.Handle,&(AR.StoreData.Fill),SEEK_SET);
			AR.StoreData.Position = AR.StoreData.Fill;
			lo = (LONG *)(&AR.StoreData.Index);
			for ( i = 0; i < j; i++ ) *lo++ = 0;
			if ( WriteFile(AR.StoreData.Handle,(UBYTE *)(&AR.StoreData.Index),(LONG)(sizeof(FILEINDEX))) !=
			(LONG)(sizeof(FILEINDEX)) ) goto ErrNextS;
			ADDPOS(AR.StoreData.Fill,sizeof(FILEINDEX));
		}
	}
	*indexpos = AR.StoreData.Position;
	ADDPOS(*indexpos,(2*sizeof(POSITION)) +
		BASEPOSITION(AR.StoreData.Index.number) * sizeof(INDEXENTRY));
	ind = &AR.StoreData.Index.expression[BASEPOSITION(AR.StoreData.Index.number)];
	ADDPOS(AR.StoreData.Index.number,1);
	return(ind);
ErrNextS:
	MesPrint("Error in storage file");
	return(0);
}

/*
 		#] NextFileIndex : 
 		#[ SetFileIndex :
*/

/**
 *  Reads the next file index and puts it into AR.StoreData.Index. TODO
 *
 *  @return  = 0 everything okay, != 0 an error occurred
 */

WORD SetFileIndex()
{
	GETIDENTITY
	int i, j = sizeof(FILEINDEX)/(sizeof(LONG));
	LONG *lo;
	if ( AR.StoreData.Handle < 0 ) {
		AR.StoreData.Handle = AC.StoreHandle;
		PUTZERO(AR.StoreData.Index.next);
		PUTZERO(AR.StoreData.Index.number);
#ifdef SYSDEPENDENTSAVE
		SETBASEPOSITION(AR.StoreData.Fill,sizeof(FILEINDEX));
#else
		if ( WriteStoreHeader(AR.StoreData.Handle) ) return(MesPrint("Error writing storage file header"));
		SETBASEPOSITION(AR.StoreData.Fill, (LONG)sizeof(FILEINDEX)+(LONG)sizeof(STOREHEADER));
#endif
		lo = (LONG *)(&AR.StoreData.Index);
		for ( i = 0; i < j; i++ ) *lo++ = 0;
		if ( WriteFile(AR.StoreData.Handle,(UBYTE *)(&AR.StoreData.Index),(LONG)(sizeof(FILEINDEX))) !=
		(LONG)(sizeof(FILEINDEX)) ) return(MesPrint("Error writing storage file"));
	}
	else {
		POSITION scrpos;
#ifdef SYSDEPENDENTSAVE
		PUTZERO(scrpos);
#else
		SETBASEPOSITION(scrpos, (LONG)(sizeof(STOREHEADER)));
#endif
		SeekFile(AR.StoreData.Handle,&scrpos,SEEK_SET);
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)(&AR.StoreData.Index),(LONG)(sizeof(FILEINDEX))) !=
		(LONG)(sizeof(FILEINDEX)) ) return(MesPrint("Error reading storage file"));
	}
#ifdef SYSDEPENDENTSAVE
	PUTZERO(AR.StoreData.Position);
#else
	SETBASEPOSITION(AR.StoreData.Position, (LONG)(sizeof(STOREHEADER)));
#endif
	return(0);
}

/*
 		#] SetFileIndex : 
 		#[ VarStore :

		The n -= sizeof(WORD); makes that the real length comes in the
		padding space, provided there is padding space (it seems so).
		The reading of the information assumes this is the case and hence
		things work....
*/

WORD VarStore(UBYTE *s, WORD n, WORD name, WORD namesize)
{
	GETIDENTITY
	UBYTE *t, *u;
	if ( s ) {
		n -= sizeof(WORD);
		t = (UBYTE *)AO.wpoin;
/*
		u = (UBYTE *)AT.WorkTop;
*/
		u = AO.wpos+AO.wlen;
		while ( n > 0 && t < u ) { *t++ = *s++; n--; }
		while ( t >= u ) {
			if ( WriteFile(AR.StoreData.Handle,AO.wpos,AO.wlen) != AO.wlen ) return(-1);
			t = AO.wpos;
			while ( n > 0 && t < u ) { *t++ = *s++; n--; }
		}
		s = AC.varnames->namebuffer + name;
		n = namesize;
		n += sizeof(void *)-1; n &= -(sizeof(void *));
		*((WORD *)t) = n;
		t += sizeof(WORD);
		while ( n > 0 && t < u ) {
			if ( namesize > 0 ) { *t++ = *s++; namesize--; }
			else { *t++ = 0; }
			n--;
		}
		while ( t >= u ) {
			if ( WriteFile(AR.StoreData.Handle,AO.wpos,AO.wlen) != AO.wlen ) return(-1);
			t = AO.wpos;
			while ( n > 0 && t < u ) {
				if ( namesize > 0 ) { *t++ = *s++; namesize--; }
				else { *t++ = 0; }
				n--;
			}
		}
		AO.wpoin = t;
	}
	else {
		LONG size;
		size = AO.wpoin - AO.wpos;
		if ( WriteFile(AR.StoreData.Handle,AO.wpos,size) != size ) return(-1);
		AO.wpoin = AO.wpos;
	}
	return(0);
}

/*
 		#] VarStore : 
 		#[ TermRenumber :

		renumbers the variables inside term according to the information
		in struct renumber.
		The search is binary. This avoided having to read/write the
		expression twice when it was stored.

*/

WORD TermRenumber(WORD *term, RENUMBER renumber, WORD nexpr)
{
	WORD *stopper;
/*!!!
WORD *memterm=term;
static LONG ctrap=0;
  !!!*/
	WORD *t, *sarg, n;
	stopper = term + *term - 1;
	stopper = stopper - ABS(*stopper) + 1;
	term++;
	while ( term < stopper ) {
/*!!!
ctrap++;
  !!!*/
		if ( *term == SYMBOL ) {
			t = term + term[1];
			term += 2;
			do {
				if ( ( n = FindrNumber(*term,&(renumber->symb)) ) < 0 ) goto ErrR;
				*term = renumber->symnum[n];
				term += 2;
			} while ( term < t );
		}
		else if ( *term == DOTPRODUCT ) {
			t = term + term[1];
			term += 2;
			do {
				if ( ( n = FindrNumber(*term,&(renumber->vect)) )
					 < 0 ) goto ErrR;
				*term++ = renumber->vecnum[n];
				if ( ( n = FindrNumber(*term,&(renumber->vect)) )
					 < 0 ) goto ErrR;
				*term = renumber->vecnum[n];
				term += 2;
			} while ( term < t );
		}
		else if ( *term == VECTOR ) {
			t = term + term[1];
			term += 2;
			do {
				if ( ( n = FindrNumber(*term,&(renumber->vect)) )
					 < 0 ) goto ErrR;
				*term++ = renumber->vecnum[n];
				if ( ( *term >= AM.OffsetIndex ) && ( *term < AM.IndDum ) ) {
					if ( ( n = FindrNumber(*term,&(renumber->indi)) )
						 < 0 ) goto ErrR;
					*term++ = renumber->indnum[n];
				}
				else term++;
			} while ( term < t );
		}
		else if ( *term == INDEX || *term == LEVICIVITA || *term == GAMMA
		|| *term == DELTA ) {
Tensors:
			t = term + term[1];
			if ( *term == INDEX || * term == DELTA ) term += 2;
			else term += FUNHEAD;
/*
			term += 2;
*/
			while ( term < t ) {
				if ( *term >= AM.OffsetIndex + WILDOFFSET ) {
/*
		Still TOBEDONE
*/
				}
				else if ( ( *term  >= AM.OffsetIndex ) && ( *term < AM.IndDum ) ) {
					if ( ( n = FindrNumber(*term,&(renumber->indi)) )
						 < 0 ) goto ErrR;
					*term = renumber->indnum[n];
				}
				else if ( *term < (WILDOFFSET+AM.OffsetVector) ) {
					if ( ( n = FindrNumber(*term,&(renumber->vect)) )
						 < 0 ) goto ErrR;
					*term = renumber->vecnum[n];
				}
				term++;
			}
		}
		else if ( *term == HAAKJE ) term += term[1];
		else {
			if ( *term > MAXBUILTINFUNCTION ) {
				if ( ( n = FindrNumber(*term,&(renumber->func)) )
					 < 0 ) goto ErrR;
				*term = renumber->funnum[n];
			}
			if ( *term >= FUNCTION && functions[*term-FUNCTION].spec
					>= TENSORFUNCTION && term[1] > FUNHEAD ) goto Tensors;
			t = term + term[1];			/* General stopper */
			term += FUNHEAD;			/* First argument */
			while ( term < t ) {
				sarg = term;
				NEXTARG(sarg)
				if ( *term > 0 ) {
/*
					Problem here:
					Marking the argument as dirty attacks the heap
					very heavily and costs much computer time.
*/
					*++term = 1;
					term += ARGHEAD-1;
					while ( term < sarg ) {
						if ( TermRenumber(term,renumber,nexpr) ) goto ErrR;
						term += *term;
					}
				}
				else {
					if ( *term <= -MAXBUILTINFUNCTION ) {
						if ( ( n = FindrNumber(-*term,&(renumber->func)) )
						 < 0 ) goto ErrR;
						*term = -renumber->funnum[n];
					}
					else if ( *term == -SYMBOL ) {
						term++;
						if ( ( n = FindrNumber(*term,
							&(renumber->symb)) ) < 0 ) goto ErrR;
						*term = renumber->symnum[n];
					}
					else if ( *term == -INDEX ) {
						term++;
						if ( *term >= AM.OffsetIndex + WILDOFFSET ) {
/*
			Still TOBEDONE
*/
						}
						else if ( ( *term  >= AM.OffsetIndex ) && ( *term < AM.IndDum ) ) {
							if ( ( n = FindrNumber(*term,&(renumber->indi)) )
								 < 0 ) goto ErrR;
							*term = renumber->indnum[n];
						}
						else if ( *term < (WILDOFFSET+AM.OffsetVector) ) {
							if ( ( n = FindrNumber(*term,&(renumber->vect)) )
								 < 0 ) goto ErrR;
							*term = renumber->vecnum[n];
						}
					}
					else if ( *term == -VECTOR || *term == -MINVECTOR ) {
						term++;
						if ( ( n = FindrNumber(*term,&(renumber->vect)) )
						 < 0 ) goto ErrR;
						*term = renumber->vecnum[n];
					}
				}
				term = sarg;			/* Next argument */
			}
			term = t;
		}
	}
	return(0);
ErrR:
	MesCall("TermRenumber");
	SETERROR(-1)
}

/*
 		#] TermRenumber : 
 		#[ FindrNumber :
*/

WORD FindrNumber(WORD n, VARRENUM *v)
{
	WORD *hi,*med,*lo;
	hi = v->hi;
	lo = v->lo;
	med = v->start;
	if ( *hi == 0 ) {
		if ( n != *hi ) {
			MesPrint("Serious problems coming up in FindrNumber");
			return(-1);
		}
		return(*hi);
	}
	while ( *med != n ) {
		if ( *med < n ) {
			if ( med == hi ) goto ErrFindr;
			lo = med;
			med = hi - ((WORDDIF(hi,med))/2);
		}
		else {
			if ( med == lo ) goto ErrFindr;
			hi = med;
			med = lo + ((WORDDIF(med,lo))/2);
		}
	}
	return(WORDDIF(med,v->lo));
ErrFindr:
/*
	Reconstruction:
*/
	{
		int i;
		i = WORDDIF(v->hi,v->lo);
		MesPrint("FindrNumber: n = %d, list has %d members",n,i);
		while ( i >= 0 ) {
			MesPrint("v->lo[%d] = %d",i,v->lo[i]); i--;
		}
		hi = v->hi;
		lo = v->lo;
		med = v->start;
		MesPrint("Start with %d,%d,%d",0,WORDDIF(med,v->lo),WORDDIF(hi,v->lo));
		while ( *med != n ) {
			if ( *med < n ) {
				if ( med == hi ) goto ErrFindr2;
				lo = med;
				med = hi - ((WORDDIF(hi,med))/2);
			}
			else {
				if ( med == lo ) goto ErrFindr2;
				hi = med;
				med = ((WORDDIF(med,lo))/2) + lo;
			}
			MesPrint("New: %d,%d,%d, *med = %d",WORDDIF(lo,v->lo),WORDDIF(med,v->lo),WORDDIF(hi,v->lo),*med);
		}
	}
	return(WORDDIF(med,v->lo));
ErrFindr2:
	return(MesPrint("Renumbering problems"));
}

/*
 		#] FindrNumber : 
 		#[ FindInIndex :

		Finds an expression in the storage index if it exists.
		If found it returns a pointer to the index entry, otherwise zero.
		par = 0		Search by address (--> f == &AR.StoreData, called by GetTable, CoSave )
		par = 1		Search by name    (--> f == &AO.SaveData,  called by CoLoad )

		When comparing parameter fields the parameters of the expression
		to be searched are in AT.TMaddr. This includes the primary expression
		and a possible FROMBRAC information. The FROMBRAC is always last.

		The parameter mode tells whether we should worry about arguments of
		a stored expression.
*/

INDEXENTRY *FindInIndex(WORD expr, FILEDATA *f, WORD par, WORD mode)
{
	GETIDENTITY
	INDEXENTRY *ind;
	WORD i, hand, *m;
	WORD *start, *stop, *stop2, *m2, nomatch = 0;
	POSITION stindex, indexpos, scrpos;
	LONG number, num;
	stindex = f->Position;
	m = AT.TMaddr;
	stop = m + m[1];
	m += SUBEXPSIZE;
	start = m;
	while ( m < stop ) {
		if ( *m == FROMBRAC || *m == WILDCARDS ) break;
		m += m[1];
	}
	stop = m;
	if ( !par ) hand = AR.StoreData.Handle;
	else        hand = AO.SaveData.Handle;
	for(;;) {
		if ( ( i = (WORD)BASEPOSITION(f->Index.number) ) != 0 ) {
			indexpos = f->Position;
			ADDPOS(indexpos,(2*sizeof(POSITION)));
			ind = f->Index.expression;
			do {
				if ( ( !par && ISEQUALPOS(indexpos,Expressions[expr].onfile) )
				|| ( par && !StrCmp(EXPRNAME(expr),(UBYTE *)(ind->name)) ) ) {
					nomatch = 1;
/*
MesPrint("index: position: %8p",&(ind->position));
MesPrint("index: length: %8p",&(ind->length));
MesPrint("index: variables: %8p",&(ind->variables));
MesPrint("index: nsymbols: %d",ind->nsymbols);
MesPrint("index: nindices: %d",ind->nindices);
MesPrint("index: nvectors: %d",ind->nvectors);
MesPrint("index: nfunctions: %d",ind->nfunctions);
MesPrint("index: size: %d",ind->size);
*/
					if ( par ) return(ind);
					scrpos = ind->position;
					SeekFile(hand,&scrpos,SEEK_SET);
					if ( ISNOTEQUALPOS(scrpos,ind->position) ) goto ErrGt2;
					if ( ReadFile(hand,(UBYTE *)AT.WorkPointer,(LONG)sizeof(WORD)) != 
					sizeof(WORD) || !*AT.WorkPointer ) goto ErrGt2;
					num = *AT.WorkPointer - 1;
					num *= wsizeof(WORD);
					if ( *AT.WorkPointer < 0 ||
					ReadFile(hand,(UBYTE *)(AT.WorkPointer+1),num) != num ) goto ErrGt2;
					m = start;	/* start of parameter field to be searched */
					m2 = AT.WorkPointer + 1;
					stop2 = m2 + m2[1];
					m2 += SUBEXPSIZE;
					while ( m < stop && m2 < stop2 ) {
						if ( *m == SYMBOL ) {
							if ( *m2 != SYMTOSYM ) break;
							m2[3] = m[2];
						}
						else if ( *m == INDEX ) {
							if ( m[2] >= 0 ) {
								if ( *m2 != INDTOIND ) break;
							}
							else {
								if ( *m2 != VECTOVEC ) break;
							}
							m2[3] = m[2];
						}
						else if ( *m >= FUNCTION ) {
							if ( *m2 != FUNTOFUN ) break;
							m2[3] = *m;
						}
						else {}
						m += m[1];
						m2 += m2[1];
					}
					if ( ( m >= stop && m2 >= stop2 ) || mode == 0 ) {
						AT.WorkPointer = stop2;

						return(ind);
					}
				}
				ind++;
				ADDPOS(indexpos,sizeof(INDEXENTRY));
			} while ( --i > 0 );
		}
		f->Position = f->Index.next;
#ifndef SYSDEPENDENTSAVE
		if ( !ISNOTZEROPOS(f->Position) ) ADDPOS(f->Position,sizeof(STOREHEADER));
		number = sizeof(struct FiLeInDeX);
#endif
		if ( ISEQUALPOS(f->Position,stindex) && !AO.bufferedInd ) goto ErrGetTab;
		if ( !par ) {
			SeekFile(AR.StoreData.Handle,&(f->Position),SEEK_SET);
			if ( ISNOTEQUALPOS(f->Position,AR.StoreData.Position) ) goto ErrGt2;
#ifndef SYSDEPENDENTSAVE
			if ( ReadFile(f->Handle, (UBYTE *)(&(f->Index)), number) != number ) goto ErrGt2;
#endif
		}
		else {
			SeekFile(AO.SaveData.Handle,&(f->Position),SEEK_SET);
			if ( ISNOTEQUALPOS(f->Position,AO.SaveData.Position) ) goto ErrGt2;
#ifndef SYSDEPENDENTSAVE
			if ( ReadSaveIndex(&f->Index) ) goto ErrGt2;
#endif
		}
#ifdef SYSDEPENDENTSAVE
		number = sizeof(struct FiLeInDeX);
		if ( ReadFile(f->Handle,(UBYTE *)(&(f->Index)),number) !=
		number ) goto ErrGt2;
#endif
	}
ErrGetTab:
	if ( nomatch ) {
		MesPrint("Parameters of expression %s don't match."
		,EXPRNAME(expr));
	}
	else {
		MesPrint("Cannot find expression %s",EXPRNAME(expr));
	}
	return(0);
ErrGt2:
	MesPrint("Readerror in IndexSearch");
	return(0);
}

/*
 		#] FindInIndex : 
 		#[ GetTable :

		Locates stored files and constructs the renumbering tables.
		They are allocated in the WorkSpace.
		First the expression data are located. The Index is treated
		as a circularly linked buffer which is paged forwardly.
		If the indexentry is located (in ind) the two renumber tables
		have to be constructed.
		Finally the prototype has to be put in the proper buffer, so
		that wildcards can be passed. There should be a test with
		an already existing prototype that is constructed by the
		pattern matcher. This has not been put in yet.

		There is a problem with the parallel processing.
		Feeding in the variables that were erased by a .store could in
		principle happen in different orders (ParFORM) or simultaneously
		(TFORM). The proper resolution is to have the compiler call GetTable
		when a stored expression is encountered.

		This has been mended in development of TFORM by reading the
		symbol tables during compilation. See the call to GetTable
		in the CodeGenerator.

		Next is the problem of FindInIndex which writes in AR.StoreData
		Copying this is expensive!

		This Doesn't work well for TFORM yet.!!!!!!!!
		e[x1,x2] versus e[x2,x1] messes up.
		For the rest is the reloading during execution not thread safe.

		The parameter mode tells whether we should worry about arguments
		of a stored expression.
*/

RENUMBER GetTable(WORD expr, POSITION *position, WORD mode)
{
	GETIDENTITY
	WORD i, j;
	WORD *w;
	RENUMBER r;
	LONG num, nsize, xx;
	WORD jsym, jind, jvec, jfun;
	WORD k, type, error = 0, *oldw, *neww, *oldwork = AT.WorkPointer;
	struct SyMbOl SyM;
	struct InDeX InD;
	struct VeCtOr VeC;
	struct FuNcTiOn FuN;
	INDEXENTRY *ind;
/*
	Prepare for FindInIndex to put the prototype in the WorkSpace.
	oldw will point at the "wildcards"
*/
/*
		Bug fix. Look also in Generator.
#ifndef WITHPTHREADS

	if ( ( r = Expressions[expr].renum ) != 0 ) { }
	else {
		Expressions[expr].renum = 
		r = (RENUMBER)Malloc1(sizeof(struct ReNuMbEr),"Renumber");
	}
#else
		r = (RENUMBER)Malloc1(sizeof(struct ReNuMbEr),"Renumber");
#endif
*/
		r = (RENUMBER)Malloc1(sizeof(struct ReNuMbEr),"Renumber");

	oldw = AT.WorkPointer + 1 + SUBEXPSIZE;
/*
	The protoype is loaded in the WorkSpace by the Index routine.
	After all it has to find an occurrence with the proper arguments.
	This sets the WorkPointer. Hence be careful now.
*/
	LOCK(AM.storefilelock);
	if ( ( ind = FindInIndex(expr,&AR.StoreData,0,mode) ) == 0 ) {
		UNLOCK(AM.storefilelock);
		return(0);
	}

	xx = ind->nsymbols+ind->nindices+ind->nvectors+ind->nfunctions;
	if ( xx == 0 ) {
		Expressions[expr].renumlists = 
		w = AN.dummyrenumlist;
	}
	else {
/*
#ifndef WITHPTHREADS
		Expressions[expr].renumlists = 
#endif
*/
		w = (WORD *)Malloc1(sizeof(WORD)*(xx*2),"VarSpace");
	}
	r->symb.lo = w;
	r->symb.start = w + ind->nsymbols/2;
	w += ind->nsymbols;
	r->symb.hi = w - 1;
	r->symnum = w;
	w += ind->nsymbols;

	r->indi.lo = w;
	r->indi.start = w + ind->nindices/2;
	w += ind->nindices;
	r->indi.hi = w - 1;
	r->indnum = w;
	w += ind->nindices;

	r->vect.lo = w;
	r->vect.start = w + ind->nvectors/2;
	w += ind->nvectors;
	r->vect.hi = w - 1;
	r->vecnum = w;
	w += ind->nvectors;

	r->func.lo = w;
	r->func.start = w + ind->nfunctions/2;
	w += ind->nfunctions;
	r->func.hi = w - 1;
	r->funnum = w;
/*	w += ind->nfunctions; */

	SeekFile(AR.StoreData.Handle,&(ind->variables),SEEK_SET);
	*position = ind->position;
	jsym = ind->nsymbols;
	jvec = ind->nvectors;
	jind = ind->nindices;
	jfun = ind->nfunctions;
/*
			#[ Symbols :
*/
	{
	SYMBOLS s = &SyM;
	w = r->symb.lo; j = jsym;
	for ( i = 0; i < j; i++ ) {
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)s,(LONG)(sizeof(struct SyMbOl)))
		!= sizeof(struct SyMbOl) ) goto ErrGt2;
		nsize = s->namesize; nsize += sizeof(void *)-1;
		nsize &= -sizeof(void *);
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)(AT.WorkPointer),nsize)
		!= nsize ) goto ErrGt2;
		*w = s->number;
		if ( ( s->flags & INUSE ) != 0 ) {
			/* Find the replacement. It must exist! */
			neww = oldw;
			while ( *neww != SYMTOSYM || neww[2] != *w ) neww += neww[1];
			k = neww[3];
		}
		else if ( GetVar((UBYTE *)AT.WorkPointer,&type,&k,ALLVARIABLES,NOAUTO) != NAMENOTFOUND ) {
			if ( type != CSYMBOL ) {
				MesPrint("Error: Conflicting types for %s",(AT.WorkPointer));
				error = -1;
			}
			else {
				if ( ( s->complex & (VARTYPEIMAGINARY|VARTYPECOMPLEX) ) !=
				( symbols[k].complex & (VARTYPEIMAGINARY|VARTYPECOMPLEX) ) ) {
					MesPrint("Warning: Conflicting complexity for %s",AT.WorkPointer);
					error = -1;
				}
				if ( ( s->complex & (VARTYPEROOTOFUNITY) ) !=
				( symbols[k].complex & (VARTYPEROOTOFUNITY) ) ) {
					MesPrint("Warning: Conflicting root of unity properties for %s",AT.WorkPointer);
					error = -1;
				}
				if ( ( s->complex & VARTYPEROOTOFUNITY ) == VARTYPEROOTOFUNITY ) {
					if ( s->maxpower != symbols[k].maxpower ) {
						MesPrint("Warning: Conflicting n in n-th root of unity properties for %s",AT.WorkPointer);
						error = -1;
					}
				}
				else if ( ( s->minpower !=
				symbols[k].minpower || s->maxpower !=
				symbols[k].maxpower ) && AC.WarnFlag ) {
					MesPrint("Warning: Conflicting power restrictions for %s",AT.WorkPointer);
				}
			}
		}
		else {
			if ( ( k = EntVar(CSYMBOL,(UBYTE *)(AT.WorkPointer),s->complex,s->minpower,
			s->maxpower,s->dimension) ) < 0 ) goto GetTcall;
		}
		*(w+j) = k;
		w++;
	}
	}
/*
			#] Symbols : 
			#[ Indices :
*/
	{
	INDICES s = &InD;
	w = r->indi.lo; j = jind;
	for ( i = 0; i < j; i++ ) {
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)s,(LONG)(sizeof(struct InDeX)))
		!= sizeof(struct InDeX) ) goto ErrGt2;
		nsize = s->namesize; nsize += sizeof(void *)-1;
		nsize &= -sizeof(void *);
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)(AT.WorkPointer),nsize)
		!= nsize ) goto ErrGt2;
		*w = s->number + AM.OffsetIndex;
		if ( s->dimension < 0 ) {	/* Relabel the dimension */
			s->dimension = -r->symnum[FindrNumber(-s->dimension,&(r->symb))];
			if ( s->nmin4 < -NMIN4SHIFT ) {	/* Relabel n-4 */
				s->nmin4 = -r->symnum[FindrNumber(-s->nmin4-NMIN4SHIFT
				,&(r->symb))]-NMIN4SHIFT;
			}
		}
		if ( ( s->flags & INUSE ) != 0 ) {
			/* Find the replacement. It must exist! */
			neww = oldw;
			while ( *neww != INDTOIND || neww[2] != *w ) neww += neww[1];
			k = neww[3] - AM.OffsetIndex;
		}
		else if ( s->type == DUMMY ) {
/*
-------->			Here we may have to execute some renumbering
*/
		}
		else if ( GetVar((UBYTE *)(AT.WorkPointer),&type,&k,ALLVARIABLES,NOAUTO) != NAMENOTFOUND ) {
			if ( type != CINDEX ) {
				MesPrint("Error: Conflicting types for %s",(AT.WorkPointer));
				error = -1;
			}
			else {
				if ( s->type !=
				indices[k].type ) {
					MesPrint("Warning: %s is also a dummy index",(AT.WorkPointer));
					error = -1;
					goto GetTb3;
				}
				if ( s->dimension != indices[k].dimension ) {
					MesPrint("Warning: Conflicting dimensions for %s",(AT.WorkPointer));
					error = -1;
				}
			}
		}
		else {
GetTb3:
			if ( ( k = EntVar(CINDEX,(UBYTE *)(AT.WorkPointer),
			s->dimension,0,s->nmin4,0) ) < 0 ) goto GetTcall;

		}
		*(w+j) = k + AM.OffsetIndex;
		w++;
	}
	}
/*
			#] Indices : 
			#[ Vectors :
*/
	{
	VECTORS s = &VeC;
	w = r->vect.lo; j = jvec;
	for ( i = 0; i < j; i++ ) {
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)s,(LONG)(sizeof(struct VeCtOr)))
		!= sizeof(struct VeCtOr) ) goto ErrGt2;
		nsize = s->namesize; nsize += sizeof(void *)-1;
		nsize &= -sizeof(void *);
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)(AT.WorkPointer),nsize)
		!= nsize ) goto ErrGt2;
		*w = s->number + AM.OffsetVector;
		if ( ( s->flags & INUSE ) != 0 ) {
			/* Find the replacement. It must exist! */
			neww = oldw;
			while ( *neww != VECTOVEC || neww[2] != *w ) neww += neww[1];
			k = neww[3] - AM.OffsetVector;
		}
		else if ( GetVar((UBYTE *)(AT.WorkPointer),&type,&k,ALLVARIABLES,NOAUTO) != NAMENOTFOUND ) {
			if ( type != CVECTOR ) {
				MesPrint("Error: Conflicting types for %s",(AT.WorkPointer));
				error = -1;
			}
			else {
				if ( ( s->complex & (VARTYPEIMAGINARY|VARTYPECOMPLEX) ) !=
				( vectors[k].complex & (VARTYPEIMAGINARY|VARTYPECOMPLEX) ) ) {
					MesPrint("Warning: Conflicting complexity for %s",(AT.WorkPointer));
					error = -1;
				}
			}
		}
		else {
			if ( ( k = EntVar(CVECTOR,(UBYTE *)(AT.WorkPointer),
			s->complex,0,0,s->dimension) ) < 0 ) goto GetTcall;
		}
		*(w+j) = k + AM.OffsetVector;
		w++;
	}
	}
/*
			#] Vectors : 
			#[ Functions :
*/
	{
	FUNCTIONS s = &FuN;
	w = r->func.lo; j = jfun;
	for ( i = 0; i < j; i++ ) {
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)s,(LONG)(sizeof(struct FuNcTiOn)))
		!= sizeof(struct FuNcTiOn) ) goto ErrGt2;
		nsize = s->namesize; nsize += sizeof(void *)-1;
		nsize &= -sizeof(void *);
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)(AT.WorkPointer),nsize)
		!= nsize ) goto ErrGt2;
		*w = s->number + FUNCTION;
		if ( ( s->flags & INUSE ) != 0 ) {
			/* Find the replacement. It must exist! */
			neww = oldw;
			while ( *neww != FUNTOFUN || neww[2] != *w ) neww += neww[1];
			k = neww[3] - FUNCTION;
		}
		else if ( GetVar((UBYTE *)(AT.WorkPointer),&type,&k,ALLVARIABLES,NOAUTO) != NAMENOTFOUND ) {
			if ( type != CFUNCTION ) {
				MesPrint("Error: Conflicting types for %s",(AT.WorkPointer));
				error = -1;
			}
			else {
				if ( s->complex != functions[k].complex ) {
					MesPrint("Warning: Conflicting complexity for %s",(AT.WorkPointer));
					error = -1;
				}
				else if ( s->symmetric != functions[k].symmetric ) {
					MesPrint("Warning: Conflicting symmetry properties for %s",(AT.WorkPointer));
					error = -1;
				}
				else if ( ( s->maxnumargs != functions[k].maxnumargs )
				|| ( s->minnumargs != functions[k].minnumargs ) ) {
					MesPrint("Warning: Conflicting argument restriction properties for %s",(AT.WorkPointer));
					error = -1;
				}
			}
		}
		else {
			if ( ( k = EntVar(CFUNCTION,(UBYTE *)(AT.WorkPointer),
			s->complex,s->commute,s->spec,s->dimension) ) < 0 ) goto GetTcall;
			functions[k].symmetric = s->symmetric;
			functions[k].maxnumargs = s->maxnumargs;
			functions[k].minnumargs = s->minnumargs;
		}
		*(w+j) = k + FUNCTION;
		w++;
	}
	}
/*
			#] Functions : 

	Now we skip the prototype. This sets the start position at the first term
*/
	if ( error ) {
		UNLOCK(AM.storefilelock);
		AT.WorkPointer = oldwork;
		return(0);
	}

	{
/*
		For clarity we look where we are.
		We want to know: is this position already known?
		Could we have inserted extra information here?

		nummystery indicates extra words. We have currently in order
		(if they exist)
			numdummies
			numfactors
			vflags
*/
		POSITION pos;
		int nummystery;
		TELLFILE(AR.StoreData.Handle,&pos);
		nummystery = DIFBASE(ind->position,pos);
/*
		MesPrint("--> We are at position       %8p",&pos);
		MesPrint("--> The index says        at %8p",&(ind->position));
		MesPrint("--> There are %d mystery bytes",nummystery);
*/
		if ( nummystery > 0 ) {
			if ( ReadFile(AR.StoreData.Handle,(UBYTE *)AT.WorkPointer,(LONG)sizeof(WORD)) != 
			sizeof(WORD) ) {
				UNLOCK(AM.storefilelock);
				AT.WorkPointer = oldwork;
				return(0);
			}
			Expressions[expr].numdummies = *AT.WorkPointer;
/*
			MesPrint("--> numdummies = %d",Expressions[expr].numdummies);
*/
			nummystery -= sizeof(WORD);
		}
		else {
			Expressions[expr].numdummies = 0;
		}
		if ( nummystery > 0 ) {
			if ( ReadFile(AR.StoreData.Handle,(UBYTE *)AT.WorkPointer,(LONG)sizeof(WORD)) != 
			sizeof(WORD) ) {
				UNLOCK(AM.storefilelock);
				AT.WorkPointer = oldwork;
				return(0);
			}
			if ( ( AS.OldNumFactors == 0 ) || ( AS.NumOldNumFactors < NumExpressions ) ) {
				WORD *buffer;
				int capacity = 20;
				if (capacity < NumExpressions) capacity = NumExpressions * 2;

				buffer = (WORD *)Malloc1(capacity * sizeof(WORD), "numfactors pointers");
				if (AS.OldNumFactors) {
					WCOPY(buffer, AS.OldNumFactors, AS.NumOldNumFactors);
					M_free(AS.OldNumFactors, "numfactors pointers");
				}
				AS.OldNumFactors = buffer;

				buffer = (WORD *)Malloc1(capacity * sizeof(WORD), "vflags pointers");
				if (AS.Oldvflags) {
					WCOPY(buffer, AS.Oldvflags, AS.NumOldNumFactors);
					M_free(AS.Oldvflags, "vflags pointers");
				}
				AS.Oldvflags = buffer;

				AS.NumOldNumFactors = capacity;
			}

			AS.OldNumFactors[expr] =
			Expressions[expr].numfactors = *AT.WorkPointer;
/*
			MesPrint("--> numfactors = %d",Expressions[expr].numfactors);
*/
			nummystery -= sizeof(WORD);
		}
		else {
			Expressions[expr].numfactors = 0;
		}
		if ( nummystery > 0 ) {
			if ( ReadFile(AR.StoreData.Handle,(UBYTE *)AT.WorkPointer,(LONG)sizeof(WORD)) != 
			sizeof(WORD) ) {
				UNLOCK(AM.storefilelock);
				AT.WorkPointer = oldwork;
				return(0);
			}
			AS.Oldvflags[expr] =
			Expressions[expr].vflags = *AT.WorkPointer;
/*
			MesPrint("--> vflags = %d",Expressions[expr].vflags);
*/
			nummystery -= sizeof(WORD);
		}
		else {
			Expressions[expr].vflags = 0;
		}
	}

	SeekFile(AR.StoreData.Handle,&(ind->position),SEEK_SET);
	if ( ReadFile(AR.StoreData.Handle,(UBYTE *)AT.WorkPointer,(LONG)sizeof(WORD)) != 
	sizeof(WORD) || !*AT.WorkPointer ) {
		UNLOCK(AM.storefilelock);
		AT.WorkPointer = oldwork;
		return(0);
	}
	num = *AT.WorkPointer - 1;
	num *= sizeof(WORD);
	if ( *AT.WorkPointer < 0 ||
	ReadFile(AR.StoreData.Handle,(UBYTE *)(AT.WorkPointer+1),num) != num ) {
		MesPrint("@Error in stored expressions file at position %10p",*position);
		UNLOCK(AM.storefilelock);
		AT.WorkPointer = oldwork;
		return(0);
	}
	UNLOCK(AM.storefilelock);
	ADDPOS(*position,num+sizeof(WORD));
	r->startposition = *position;
	AT.WorkPointer = oldwork;
	return(r);
GetTcall:
	UNLOCK(AM.storefilelock);
	AT.WorkPointer = oldwork;
	MesCall("GetTable");
	return(0);
ErrGt2:
	UNLOCK(AM.storefilelock);
	AT.WorkPointer = oldwork;
	MesPrint("Readerror in GetTable");
	return(0);
}

/*
 		#] GetTable : 
 		#[ CopyExpression :

		Copies from one scratch buffer to another.
		We assume here that the complete 'from' scratch buffer is taken.
		We also assume that the 'from' buffer is positioned at the end of
		the expression.

		The locks should be placed in the calling routine. We need basically
		AS.outputslock.
*/

int CopyExpression(FILEHANDLE *from, FILEHANDLE *to)
{
	POSITION posfrom, poscopy;
	LONG fullsize,i;
	WORD *t1, *t2;
	int RetCode;
	SeekScratch(from,&posfrom);
	if ( from->handle < 0 ) {	/* input is in memory */
		fullsize = (BASEPOSITION(posfrom))/sizeof(WORD);
		if ( ( to->POstop - to->POfull ) >= fullsize ) {
/*
			Fits inside the buffer of the output. This will be fast.
*/
			t1 = from->PObuffer;
			t2 = to->POfull;
			NCOPY(t2,t1,fullsize)
			to->POfull = to->POfill = t2;
			goto WriteTrailer;
		}
		if ( to->handle < 0 ) {		/* First open the file */
			if ( ( RetCode = CreateFile(to->name) ) >= 0 ) {
				to->handle = (WORD)RetCode;
				PUTZERO(to->filesize);
				PUTZERO(to->POposition);
			}
			else {
				MLOCK(ErrorMessageLock);
				MesPrint("Cannot create scratch file %s",to->name);
				MUNLOCK(ErrorMessageLock);
				return(-1);
			}
		}
		t1 = from->PObuffer;
		while ( fullsize > 0 ) {
			i = to->POstop - to->POfull;
			if ( i > fullsize ) i = fullsize;
			fullsize -= i;
			t2 = to->POfull;
			NCOPY(t2,t1,i)
			if ( fullsize > 0 ) {
				SeekFile(to->handle,&(to->POposition),SEEK_SET);
				if ( WriteFile(to->handle,((UBYTE *)(to->PObuffer)),to->POsize) != to->POsize ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Error while writing to disk. Disk full?");
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
				ADDPOS(to->POposition,to->POsize);
/*				SeekFile(to->handle,&(to->POposition),SEEK_CUR); */
				to->filesize = to->POposition;
				to->POfill = to->POfull = to->PObuffer;
			}
			else {
				to->POfill = to->POfull = t2;
			}
		}
		goto WriteTrailer;
	}
/*
	Now the input involves a file. This needs the use of the PObuffer of from.
	First make sure the tail of the buffer has been written
*/
	if ( ((UBYTE *)(from->POfill)-(UBYTE *)(from->PObuffer)) > 0 ) {
		if ( WriteFile(from->handle,((UBYTE *)(from->PObuffer)),((UBYTE *)(from->POfill)-(UBYTE *)(from->PObuffer)))
		!= ((UBYTE *)(from->POfill)-(UBYTE *)(from->PObuffer)) ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Error while writing to disk. Disk full?");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		SeekFile(from->handle,&(from->POposition),SEEK_CUR);
		posfrom = from->filesize = from->POposition;
		from->POfill = from->POfull = from->PObuffer;
	}
/*
	Now copy the complete contents
*/
	PUTZERO(poscopy);
	SeekFile(from->handle,&poscopy,SEEK_SET);
	while ( ISLESSPOS(poscopy,posfrom) ) {
		fullsize = ReadFile(from->handle,((UBYTE *)(from->PObuffer)),from->POsize);
		if ( fullsize < 0 || ( fullsize % sizeof(WORD) ) != 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Error while reading from disk while copying expression.");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		fullsize /= sizeof(WORD);
		from->POfull = from->PObuffer + fullsize;
		t1 = from->PObuffer;

		if ( ( to->POstop - to->POfull ) >= fullsize ) {
/*
			Fits inside the buffer of the output. This will be fast.
*/
			t2 = to->POfull;
			NCOPY(t2,t1,fullsize)
			to->POfill = to->POfull = t2;
		}
		else {
		  if ( to->handle < 0 ) {		/* First open the file */
			if ( ( RetCode = CreateFile(to->name) ) >= 0 ) {
				to->handle = (WORD)RetCode;
				PUTZERO(to->POposition);
				PUTZERO(to->filesize);
			}
			else {
				MLOCK(ErrorMessageLock);
				MesPrint("Cannot create scratch file %s",to->name);
				MUNLOCK(ErrorMessageLock);
				return(-1);
			}
		  }
		  while ( fullsize > 0 ) {
			i = to->POstop - to->POfull;
			if ( i > fullsize ) i = fullsize;
			fullsize -= i;
			t2 = to->POfull;
			NCOPY(t2,t1,i)
			if ( fullsize > 0 ) {
				SeekFile(to->handle,&(to->POposition),SEEK_SET);
				if ( WriteFile(to->handle,((UBYTE *)(to->PObuffer)),to->POsize) != to->POsize ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Error while writing to disk. Disk full?");
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
				ADDPOS(to->POposition,to->POsize);
/*				SeekFile(to->handle,&(to->POposition),SEEK_CUR); */
				to->filesize = to->POposition;
				to->POfill = to->POfull = to->PObuffer;
			}
			else {
				to->POfill = to->POfull = t2;
			}
		  }
		}
		SeekFile(from->handle,&poscopy,SEEK_CUR);
	}
WriteTrailer:
	if ( ( to->handle >= 0 ) && ( to->POfill > to->PObuffer ) ) {
		fullsize = (UBYTE *)(to->POfill) - (UBYTE *)(to->PObuffer);
/*
		PUTZERO(to->POposition);
		SeekFile(to->handle,&(to->POposition),SEEK_END);
*/
		SeekFile(to->handle,&(to->filesize),SEEK_SET);
		if ( WriteFile(to->handle,((UBYTE *)(to->PObuffer)),fullsize) != fullsize ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Error while writing to disk. Disk full?");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		ADDPOS(to->filesize,fullsize);
		to->POposition = to->filesize;
		to->POfill = to->POfull = to->PObuffer;
	}

	return(0);
}

/*
 		#] CopyExpression : 
 		#[ ExprStatus :
*/

#ifdef HIDEDEBUG

static UBYTE *statusexpr[] = {
	 (UBYTE *)"LOCALEXPRESSION"
	,(UBYTE *)"SKIPLEXPRESSION"
	,(UBYTE *)"DROPLEXPRESSION"
	,(UBYTE *)"DROPPEDEXPRESSION"
	,(UBYTE *)"GLOBALEXPRESSION"
	,(UBYTE *)"SKIPGEXPRESSION"
	,(UBYTE *)"DROPGEXPRESSION"
	,(UBYTE *)"UNKNOWN"
	,(UBYTE *)"STOREDEXPRESSION"
	,(UBYTE *)"HIDDENLEXPRESSION"
	,(UBYTE *)"HIDELEXPRESSION"
	,(UBYTE *)"DROPHLEXPRESSION"
	,(UBYTE *)"UNHIDELEXPRESSION"
	,(UBYTE *)"HIDDENGEXPRESSION"
	,(UBYTE *)"HIDEGEXPRESSION"
	,(UBYTE *)"DROPHGEXPRESSION"
	,(UBYTE *)"UNHIDEGEXPRESSION"
	,(UBYTE *)"INTOHIDELEXPRESSION"
	,(UBYTE *)"INTOHIDEGEXPRESSION"
};

void ExprStatus(EXPRESSIONS e)
{
	MesPrint("Expression %s(%d) has status %s(%d,%d). Buffer: %d, Position: %15p",
		AC.exprnames->namebuffer+e->name,(WORD)(e-Expressions),
		statusexpr[e->status],e->status,e->hidelevel,
		e->whichbuffer,&(e->onfile));
}

#endif

/*
 		#] ExprStatus : 
	#] StoreExpressions :
	#[ System Independent Saved Expressions :

	All functions concerned with the system independent reading of save-files
	are here. They are called by the functions CoLoad, PutInStore,
	SetFileIndex, FindInIndex. In case no translation (endianness flip,
	resizing of words, renumbering) has to be done, they just do simple file
	reading. The function SaveFileHeader() for writing a header with
	information about the system architecture, FORM version, etc. is also
	located here.

 		#[ Flip :
*/

#ifndef INT16
#error "INT16 not defined!"
#endif
#ifndef INT32
#error "INT32 not defined!"
#endif

/**
 *  Flips the endianness. This function will be called via function pointers.
 *  See struct O_const and ReadSaveHeader().
 *
 *  It is a general version for arbitrary word sizes.
 *  
 *  @param  p       pointer to data
 *  @param  length  length of data in bytes
 */
static void FlipN(UBYTE *p, int length)
{
	UBYTE *q, buf;
	q = p + length;
	do {
		--q;
		buf = *p; *p = *q; *q = buf;
	} while ( ++p != q );
}

/**
 *  Flips the endianness. This function will be called via function pointers.
 *  See struct O_const and ReadSaveHeader().
 *
 *  It is an optimized version for 16 bit (other versions for 32bit and 64bit
 *  do exist).
 *  
 *  @param  p  pointer to data
 */
static void Flip16(UBYTE *p)
{
	INT16 in = *((INT16 *)p);
	INT16 out = (INT16)( (((in) >> 8) & 0x00FF) | (((in) << 8) & 0xFF00) );
	*((INT16 *)p) = out;
}

/** @see Flip16() */
static void Flip32(UBYTE *p)
{
	INT32 in = *((INT32 *)p);
	INT32 out =
		( (((in) >> 24) & 0x000000FF) | (((in) >>  8) & 0x0000FF00) | \
		  (((in) <<  8) & 0x00FF0000) | (((in) << 24) & 0xFF000000) );
	*((INT32 *)p) = out;
}

/** @see Flip16() */
#ifdef INT64
static void Flip64(UBYTE *p)
{
	INT64 in = *((INT64 *)p);
	INT64 out =
		( (((in) >> 56) & (INT64)0x00000000000000FFLL) | (((in) >> 40) & (INT64)0x000000000000FF00LL) | \
		  (((in) >> 24) & (INT64)0x0000000000FF0000LL) | (((in) >>  8) & (INT64)0x00000000FF000000LL) | \
		  (((in) <<  8) & (INT64)0x000000FF00000000LL) | (((in) << 24) & (INT64)0x0000FF0000000000LL) | \
		  (((in) << 40) & (INT64)0x00FF000000000000LL) | (((in) << 56) & (INT64)0xFF00000000000000LL) );
	*((INT64 *)p) = out;
}
#else
static void Flip64(UBYTE *p) { FlipN(p, 8); }
#endif /* INT64 */

/** @see Flip16() */
static void Flip128(UBYTE *p) { FlipN(p, 16); }

/*
 		#] Flip : 
 		#[ Resize :
*/

/**
 *  Resizes words. This function will be called via function pointers. See
 *  struct O_const and ReadSaveHeader().
 *
 *  General version for arbitrary word sizes and big-endian machines.
 *
 *  @param  src  pointer to input data
 *  @param  dst  pointer to output data
 *  @param  slen number of bytes of input
 *  @param  dlen number of bytes of output
 */
static void ResizeDataBE(UBYTE *src, int slen, UBYTE *dst, int dlen)
{
	if ( slen > dlen ) {
		src += slen - dlen;
		while ( dlen-- ) { *dst++ = *src++; }
	}
	else {
		int i = dlen - slen;
		while ( i-- ) { *dst++ = 0; }
		while ( slen-- ) { *dst++ = *src++; }
	}
}

/**
 *  The same as ResizeDataBE() but for little-endian machines.
 */
static void ResizeDataLE(UBYTE *src, int slen, UBYTE *dst, int dlen)
{
	if ( slen > dlen ) {
		while ( dlen-- ) { *dst++ = *src++; }
	}
	else {
		int i = dlen - slen;
		while ( slen-- ) { *dst++ = *src++; }
		while ( i-- ) { *dst++ = 0; }
	}
}

/**
 *  Resizes words. This function will be called via function pointers. See
 *  struct O_const and ReadSaveHeader().
 *
 *  Specialized version for the specific combination of reading 16bit and
 *  writing 16bit (more versions for other bit-combinations do exist).
 *  
 *  No checking for too big numbers is done.
 *
 *  @param  src  pointer to input data
 *  @param  dst  pointer to output data
 */
static void Resize16t16(UBYTE *src, UBYTE *dst)
{
	*((INT16 *)dst) = *((INT16 *)src);
}

/** @see Resize16t16() */
static void Resize16t32(UBYTE *src, UBYTE *dst)
{
	INT16 in = *((INT16 *)src);
	INT32 out = (INT32)in;
	*((INT32 *)dst) = out;
}

/** @see Resize16t16() */
#ifdef INT64
static void Resize16t64(UBYTE *src, UBYTE *dst)
{
	INT16 in = *((INT16 *)src);
	INT64 out = (INT64)in;
	*((INT64 *)dst) = out;
}
#else
static void Resize16t64(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 2, dst, 8); }
#endif /* INT64 */

/** @see Resize16t16() */
static void Resize16t128(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 2, dst, 16); }

/** @see Resize16t16() */
static void Resize32t32(UBYTE *src, UBYTE *dst)
{
	*((INT32 *)dst) = *((INT32 *)src);
}

/** @see Resize16t16() */
#ifdef INT64
static void Resize32t64(UBYTE *src, UBYTE *dst)
{
	INT32 in = *((INT32 *)src);
	INT64 out = (INT64)in;
	*((INT64 *)dst) = out;
}
#else
static void Resize32t64(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 4, dst, 8); }
#endif /* INT64 */

/** @see Resize16t16() */
static void Resize32t128(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 4, dst, 16); }

/** @see Resize16t16() */
#ifdef INT64
static void Resize64t64(UBYTE *src, UBYTE *dst)
{
	*((INT64 *)dst) = *((INT64 *)src);
}
#else
static void Resize64t64(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 8, dst, 8); }
#endif /* INT64 */

/** @see Resize16t16() */
static void Resize64t128(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 8, dst, 16); }

/** @see Resize16t16() */
static void Resize128t128(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 16, dst, 16); }

/** @see Resize16t16() */
static void Resize32t16(UBYTE *src, UBYTE *dst)
{
	INT32 in = *((INT32 *)src);
	INT16 out = (INT16)in;
	if ( in > (1<<15)-1 || in < -(1<<15)+1 ) AO.resizeFlag |= 1;
	*((INT16 *)dst) = out;
}

/**
 *  The same as Resize32t16() but with checking for too big numbers.
 *
 *  The resizeFlag in struct O_const will be used to signal the result of the
 *  checking. This flag is used by CoLoad().
 */
static void Resize32t16NC(UBYTE *src, UBYTE *dst)
{
	INT32 in = *((INT32 *)src);
	INT16 out = (INT16)in;
	*((INT16 *)dst) = out;
}

#ifdef INT64
/** @see Resize16t16() */
static void Resize64t16(UBYTE *src, UBYTE *dst)
{
	INT64 in = *((INT64 *)src);
	INT16 out = (INT16)in;
	if ( in > (1<<15)-1 || in < -(1<<15)+1 ) AO.resizeFlag |= 1;
	*((INT16 *)dst) = out;
}
/** @see Resize32t16NC() */
static void Resize64t16NC(UBYTE *src, UBYTE *dst)
{
	INT64 in = *((INT64 *)src);
	INT16 out = (INT16)in;
	*((INT16 *)dst) = out;
}
#else
/** @see Resize16t16() */
static void Resize64t16(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 8, dst, 2); }
/** @see Resize32t16NC() */
static void Resize64t16NC(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 8, dst, 2); }
#endif /* INT64 */

#ifdef INT64
/** @see Resize16t16() */
static void Resize64t32(UBYTE *src, UBYTE *dst)
{
	INT64 in = *((INT64 *)src);
	INT32 out = (INT32)in;
	if ( in > ((INT64)1<<31)-1 || in < -((INT64)1<<31)+1 ) AO.resizeFlag |= 1;
	*((INT32 *)dst) = out;
}
/** @see Resize32t16NC() */
static void Resize64t32NC(UBYTE *src, UBYTE *dst)
{
	INT64 in = *((INT64 *)src);
	INT32 out = (INT32)in;
	*((INT32 *)dst) = out;
}
#else
/** @see Resize16t16() */
static void Resize64t32(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 8, dst, 4); }
/** @see Resize32t16NC() */
static void Resize64t32NC(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 8, dst, 4); }
#endif /* INT64 */

/** @see Resize16t16() */
static void Resize128t16(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 16, dst, 2); }

/** @see Resize32t16NC() */
static void Resize128t16NC(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 16, dst, 2); }

/** @see Resize16t16() */
static void Resize128t32(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 16, dst, 4); }

/** @see Resize32t16NC() */
static void Resize128t32NC(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 16, dst, 4); }

/** @see Resize16t16() */
static void Resize128t64(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 16, dst, 8); }

/** @see Resize32t16NC() */
static void Resize128t64NC(UBYTE *src, UBYTE *dst) { AO.ResizeData(src, 16, dst, 8); }

/*
 		#] Resize : 
 		#[ CheckPower and RenumberVec :
*/

/**
 *  Checks the size of exponents. If a checking fails, the powerFlag in struct
 *  O_const will be set. This flag is used by CoLoad().
 *
 *  @param  p  pointer to WORD containing exponent
 */
static void CheckPower32(UBYTE *p)
{
	if ( *((INT32 *)p) < -MAXPOWER ) {
		AO.powerFlag |= 0x01;
		*((INT32 *)p) = -MAXPOWER;
	}
	p += sizeof(INT32);
	if ( *((INT32 *)p) > MAXPOWER ) {
		AO.powerFlag |= 0x02;
		*((INT32 *)p) = MAXPOWER;
	}
}

/**
 *  Renumbers vectors by compensating for the different WILDOFFSET on the
 *  involved machines and FORM versions. The WILDOFFSET from the writing
 *  machine is coded in the header of the save-file.
 *
 *  @param  p  pointer to WORD containing vector code
 */
static void RenumberVec32(UBYTE *p)
{
/*	INT32 wildoffset = *((INT32 *)AO.SaveHeader.wildoffset); */
	void *dummy = (void *)AO.SaveHeader.wildoffset;  /* to remove a warning about strict-aliasing rules in gcc */
	INT32 wildoffset = *(INT32 *)dummy;
	INT32 in = *((INT32 *)p);
	in = in + 2*wildoffset;
	in = in - 2*WILDOFFSET;
	*((INT32 *)p) = in;
}

/*
 		#] CheckPower and RenumberVec : 
 		#[ ResizeCoeff :
*/

/**
 *  Resizes the coefficients of expressions and terms. The function only
 *  work on uniform data with a word size of 32bit (ReadSaveExpression()
 *  provides for that). The resizing then actually means whether zeros can be
 *  removed when going from 64bit to 32bit, or whether the coefficient size has
 *  to be doubled effectively when going from 32bit to 64bit. Both cases
 *  involve copying of words and a shrinking or growing of the memory used in
 *  @e *bout.
 *
 *  @param  bout  input and output buffer for coefficient
 *  @param  bend  end of input
 *  @param  top   end of buffer
 */
static void ResizeCoeff32(UBYTE **bout, UBYTE *bend, UBYTE *top)
{
	int i;
	INT32 sign;
	INT32 *in, *p;
	INT32 *out = (INT32 *)*bout;
	INT32 *end = (INT32 *)bend;

	if ( sizeof(WORD) == 2 ) {
		/* 4 -> 2 */
		INT32 len = (end - 1 - out) / 2;
		int zeros = 2;
		p = out + len - 1;

		if ( *p & 0xFFFF0000 ) --zeros;
		p += len;
		if ( *p & 0xFFFF0000 ) --zeros;

		in = end - 1;
		sign = ( *in-- > 0 ) ? 1 : -1;
		p = out + 4*len;
		if ( zeros == 2 ) p -= 2;
		out = p--;

		if ( zeros < 2 ) *p-- = *in >> 16;
		*p-- = *in-- & 0x0000FFFF;
		for ( i = 1; i < len; ++i ) {
			*p-- = *in >> 16;
			*p-- = *in-- & 0x0000FFFF;
		}
		if ( zeros < 2 ) *p-- = *in >> 16;
		*p-- = *in-- & 0x0000FFFF;
		for ( i = 1; i < len; ++i ) {
			*p-- = *in >> 16;
			*p-- = *in-- & 0x0000FFFF;
		}

		*out = (out - p) * sign;
		*bout = (UBYTE *)(out+1);

	}
	else {
		/* 2 -> 4 */
		INT32 len = (end - 1 - out) / 2;
		if ( len == 1 ) {
			*out = *(unsigned INT16 *)out;
			++out;
			*out = *(unsigned INT16 *)out;
			++out;
			++out;
		}
		else {
			p = out;
			*out = *(unsigned INT16 *)out;
			in = out + 1;
			for ( i = 1; i < len; ++i ) {
				/* shift */
				*out = (unsigned INT32)(*(unsigned INT16 *)out)
					+ ((unsigned INT32)(*(unsigned INT16 *)in) << 16);
				++in;
				if ( ++i == len ) break;
				/* copy */
				++out;
				*out = *(unsigned INT16 *)in;
				++in;
			}
			++out;
			*out = *(unsigned INT16 *)in;
			++in;
			for ( i = 1; i < len; ++i ) {
				/* shift */
				*out = (unsigned INT32)(*(unsigned INT16 *)out)
					+ ((unsigned INT32)(*(unsigned INT16 *)in) << 16);
				++in;
				if ( ++i == len ) break;
				/* copy */
				++out;
				*out = *(unsigned INT16 *)in;
				++in;
			}
			++out;
			if ( *in < 0 ) *out = -(out - p + 1);
			else *out = out - p + 1;
			++out;
		}

		if ( out > (INT32 *)top ) {
			MesPrint("Error in resizing coefficient!");
		}

		*bout = (UBYTE *)out;
	}
}

/*
 		#] ResizeCoeff : 
 		#[ WriteStoreHeader :
*/

#define SAVEREVISION 0x02

/**
 *  Writes header with information about system architecture and FORM revision
 *  to an open store file.
 *
 *  Called by SetFileIndex().
 *
 *  @param  handle  specifies open file to which header will be written
 *  @return         = 0 everything okay, != 0 an error occurred
 */
WORD WriteStoreHeader(WORD handle)
{
	/* template of the STOREHEADER */
	static STOREHEADER sh = {
		{ 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF },	/* store header mark */
		0, 0, 0, 0,											/* sizeof of WORD,LONG,POSITION,void* */
		{ 0 },												/* endianness check number */
		0, 0, 0, 0,											/* sizeof variable structs */
		{ 0 },												/* maxpower */
		{ 0 },												/* wildoffset */
		SAVEREVISION,										/* revision */
		{ 0 } };											/* reserved */
	int endian, i;

	/* if called for the first time ... */
	if ( sh.lenWORD == 0 ) {
		sh.lenWORD = sizeof(WORD);
		sh.lenLONG = sizeof(LONG);
		sh.lenPOS = sizeof(POSITION);
		sh.lenPOINTER = sizeof(void *);

		endian = 1;
		for ( i = 1; i < (int)sizeof(int); ++i ) {
			endian <<= 8;
			endian += i+1;
		}
		for ( i = 0; i < (int)sizeof(int); ++i ) sh.endianness[i] = ((char *)&endian)[i];

		sh.sSym = sizeof(struct SyMbOl);
		sh.sInd = sizeof(struct InDeX);
		sh.sVec = sizeof(struct VeCtOr);
		sh.sFun = sizeof(struct FuNcTiOn);

/*		*((WORD *)sh.maxpower) = MAXPOWER;
		*((WORD *)sh.wildoffset) = WILDOFFSET; */
        {
            WORD dumw[8];
            UBYTE *dummy;
            for ( i = 0; i < 8; i++ ) dumw[i] = 0;
            dummy = (UBYTE *)dumw;
            dumw[0] = (WORD)MAXPOWER;
            for ( i = 0; i < 16; i++ ) sh.maxpower[i] = dummy[i];
            dumw[0] = (WORD)WILDOFFSET;
            for ( i = 0; i < 16; i++ ) sh.wildoffset[i] = dummy[i];
        }
	}

	return ( WriteFile(handle,(UBYTE *)(&sh),(LONG)(sizeof(STOREHEADER)))
	         != (LONG)(sizeof(STOREHEADER)) );
}

/*
 		#] WriteStoreHeader : 
 		#[ CompactifySizeof :
*/

/**
 *  Utility function used by ReadSaveHeader() to convert a sizeof into a
 *  convenient array index.
 *
 *  @param  size  size in bytes
 *  @return       log_2(size) - 1
 */
static unsigned int CompactifySizeof(unsigned int size)
{
	switch ( size ) {
		case  2: return 0;
		case  4: return 1;
		case  8: return 2;
		case 16: return 3;
		default: MesPrint("Error compactifying size.");
		         return 3;
	}
}

/*
 		#] CompactifySizeof : 
 		#[ ReadSaveHeader :
*/

/**
 *  Reads the header in the save file and sets function pointers and flags
 *  according to the information found there. Must be called before any other
 *  ReadSave... function.
 *
 *  Currently works only for the exchange between 32bit and 64bit machines
 *  (WORD size must be 2 or 4 bytes)!
 *
 *  It is called by CoLoad().
 *
 *  @return   = 0 everything okay, != 0 an error occurred
 */

WORD ReadSaveHeader()
{
	/* Read-only tables of function pointers for conversions. */
	static VOID (*flipJumpTable[4])(UBYTE *) =
		{ Flip16, Flip32, Flip64, Flip128 };
	static VOID (*resizeJumpTable[4][4])(UBYTE *, UBYTE *) = /* "own x saved"-sizes  */
		{ { Resize16t16,  Resize32t16,  Resize64t16,  Resize128t16  },
		  { Resize16t32,  Resize32t32,  Resize64t32,  Resize128t32  },
		  { Resize16t64,  Resize32t64,  Resize64t64,  Resize128t64  },
		  { Resize16t128, Resize32t128, Resize64t128, Resize128t128 } };
	static VOID (*resizeNCJumpTable[4][4])(UBYTE *, UBYTE *) = /* "own x saved"-sizes  */
		{ { Resize16t16,  Resize32t16NC,  Resize64t16NC,  Resize128t16NC  },
		  { Resize16t32,  Resize32t32,  Resize64t32NC,  Resize128t32NC  },
		  { Resize16t64,  Resize32t64,  Resize64t64,  Resize128t64NC  },
		  { Resize16t128, Resize32t128, Resize64t128, Resize128t128 } };

	int endian, i;
	WORD idxW = CompactifySizeof(sizeof(WORD));
	WORD idxL = CompactifySizeof(sizeof(LONG));
	WORD idxP = CompactifySizeof(sizeof(POSITION));
	WORD idxVP = CompactifySizeof(sizeof(void *));

	AO.transFlag = 0;
	AO.powerFlag = 0;
	AO.resizeFlag = 0;
	AO.bufferedInd = 0;

	if ( ReadFile(AO.SaveData.Handle,(UBYTE *)(&AO.SaveHeader),
		(LONG)sizeof(STOREHEADER)) != (LONG)sizeof(STOREHEADER) )
		return(MesPrint("Error reading save file header"));

	/* check whether save-file has no header. if yes then it is an old version
	   of FORM -> go back to position 0 in file which then contains the first
	   index and skip the rest. */
	for ( i = 0; i < 8; ++i ) {
		if ( AO.SaveHeader.headermark[i] != 0xFF ) {
			POSITION p;
			PUTZERO(p);
			SeekFile(AO.SaveData.Handle, &p, SEEK_SET);
			return ( 0 );
		}
	}

	if ( AO.SaveHeader.revision != SAVEREVISION ) {
		return(MesPrint("Save file header from an old version. Cannot read this file."));
	}

	endian = 1;
	for ( i = 1; i < (int)sizeof(int); ++i ) {
		endian <<= 8;
		endian += i+1;
	}
	if ( ((char *)&endian)[0] < ((char *)&endian)[1] ) {
		/* this machine is big-endian */
		AO.ResizeData = ResizeDataBE;
	}
	else {
		/* this machine is little-endian */
		AO.ResizeData = ResizeDataLE;
	}

	/* set AO.transFlag if ANY conversion has to be done later */
	if ( AO.SaveHeader.endianness[0] > AO.SaveHeader.endianness[1] ) {
		AO.transFlag = ( ((char *)&endian)[0] < ((char *)&endian)[1] );
	}
	else {
		AO.transFlag = ( ((char *)&endian)[0] > ((char *)&endian)[1] );
	}
	if ( (WORD)AO.SaveHeader.lenWORD != sizeof(WORD) ) AO.transFlag |= 0x02;
	if ( (WORD)AO.SaveHeader.lenLONG != sizeof(LONG) ) AO.transFlag |= 0x04;
	if ( (WORD)AO.SaveHeader.lenPOS != sizeof(POSITION) ) AO.transFlag |= 0x08;
	if ( (WORD)AO.SaveHeader.lenPOINTER != sizeof(void *) ) AO.transFlag |= 0x10;

	AO.FlipWORD = flipJumpTable[idxW];
	AO.FlipLONG = flipJumpTable[idxL];
	AO.FlipPOS = flipJumpTable[idxP];
	AO.FlipPOINTER = flipJumpTable[idxVP];

	/* Works only for machines where WORD is not greater than 32bit ! */
	AO.CheckPower = CheckPower32;
	AO.RenumberVec = RenumberVec32;

	AO.ResizeWORD = resizeJumpTable[idxW][CompactifySizeof(AO.SaveHeader.lenWORD)];
	AO.ResizeNCWORD = resizeNCJumpTable[idxW][CompactifySizeof(AO.SaveHeader.lenWORD)];
	AO.ResizeLONG = resizeJumpTable[idxL][CompactifySizeof(AO.SaveHeader.lenLONG)];
	AO.ResizePOS = resizeJumpTable[idxP][CompactifySizeof(AO.SaveHeader.lenPOS)];
	AO.ResizePOINTER = resizeJumpTable[idxVP][CompactifySizeof(AO.SaveHeader.lenPOINTER)];

	{
		WORD dumw[8];
		UBYTE *dummy;
		for ( i = 0; i < 8; i++ ) dumw[i] = 0;
		dummy = (UBYTE *)dumw;
		for ( i = 0; i < 16; i++ ) dummy[i] = AO.SaveHeader.maxpower[i];
		AO.mpower = dumw[0];
    }

	return ( 0 );
}

/*
 		#] ReadSaveHeader : 
 		#[ ReadSaveIndex :
*/

/**
 *  Reads a FILEINDEX from the open save file specified by AO.SaveData.Handle.
 *  Translations for adjusting endianness and data sizes are done if necessary.
 *
 *  Depends on the assumption that sizeof(FILEINDEX) is the same everywhere.
 *  If FILEINDEX or INDEXENTRY change, then this functions has to be adjusted.
 *
 *  Called by CoLoad() and FindInIndex().
 *
 *  @param  fileind  contains the read FILEINDEX after succesful return. must
 *                   point to allocated, big enough memory.
 *  @return          = 0 everything okay, != 0 an error occurred
 */
WORD ReadSaveIndex(FILEINDEX *fileind)
{
	/* do we need some translation for the FILEINDEX? */
	if ( AO.transFlag ) {
		/* if a translated FILEINDEX can hold less entries than the original
		   FILEINDEX, then we need to buffer the extra entires in this static
		   variable (can happen going from 32bit to 64bit */
		static FILEINDEX sbuffer;

		FILEINDEX buffer;
		UBYTE *p, *q;
		int i;

		/* shortcuts */
		int lenW = AO.SaveHeader.lenWORD;
		int lenL = AO.SaveHeader.lenLONG;
		int lenP = AO.SaveHeader.lenPOS;

		/* if we have a buffered FILEINDEX then just return it */
		if ( AO.bufferedInd ) {
			*fileind = sbuffer;
			AO.bufferedInd = 0;
			return ( 0 );
		}

		if ( ReadFile(AO.SaveData.Handle, (UBYTE *)fileind, sizeof(FILEINDEX))
				!= sizeof(FILEINDEX) ) {
			return ( MesPrint("Error(1) reading stored expression.") );
		}

		/* do we need to flip the endianness? */
		if ( AO.transFlag & 1 ) {
			LONG number;
			/* padding bytes */
			int padp = lenL - ((lenW*5+(MAXENAME + 1)) & (lenL-1));
			p = (UBYTE *)fileind;
			AO.FlipPOS(p); p += lenP;			/* next */
			AO.FlipPOS(p);						/* number */
			AO.ResizePOS(p, (UBYTE *)&number);
			p += lenP;
			for ( i = 0; i < number; ++i ) {
				AO.FlipPOS(p); p += lenP;		/* position */
				AO.FlipPOS(p); p += lenP;		/* length */
				AO.FlipPOS(p); p += lenP;		/* variables */
				AO.FlipLONG(p); p += lenL;		/* CompressSize */
				AO.FlipWORD(p); p += lenW;		/* nsymbols */
				AO.FlipWORD(p); p += lenW;		/* nindices */
				AO.FlipWORD(p); p += lenW;		/* nvectors */
				AO.FlipWORD(p); p += lenW;		/* nfunctions */
				AO.FlipWORD(p); p += lenW;		/* size */
				p += padp;
			}
		}

		/* do we need to resize data? */
		if ( AO.transFlag > 1 ) {
			LONG number, maxnumber;
			int n;
			/* padding bytes */
			int padp = lenL - ((lenW*5+(MAXENAME + 1)) & (lenL-1));
			int padq = sizeof(LONG) - ((sizeof(WORD)*5+(MAXENAME + 1)) & (sizeof(LONG)-1));

			p = (UBYTE *)fileind; q = (UBYTE *)&buffer;
			AO.ResizePOS(p, q);						/* next */
			p += lenP; q += sizeof(POSITION);
			AO.ResizePOS(p, q);					/* number */
			p += lenP;
			number = BASEPOSITION(*((POSITION *)q));
			/* if FILEINDEX in file contains more entries than the FILEINDEX in
			   memory can contain, then adjust the numbers and prepare for
			   buffering */
			if ( number > (LONG)INFILEINDEX ) {
				AO.bufferedInd = number-INFILEINDEX;
				if ( AO.bufferedInd > (WORD)INFILEINDEX ) {
					/* can happen when reading 32bit and writing >=128bit.
					   Fix: more than one static buffer for FILEINDEX */
					return ( MesPrint("Too many index entries.") );
				}
				maxnumber = INFILEINDEX;
				SETBASEPOSITION(*((POSITION *)q),INFILEINDEX);
			}
			else {
				maxnumber = number;
			}
			q += sizeof(POSITION);
			/* read all INDEXENTRY that fit into the output buffer */
			for ( i = 0; i < maxnumber; ++i ) {
				AO.ResizePOS(p, q);					/* position */
				p += lenP; q += sizeof(POSITION);
				AO.ResizePOS(p, q);					/* length */
				p += lenP; q += sizeof(POSITION);
				AO.ResizePOS(p, q);					/* variables */
				p += lenP; q += sizeof(POSITION);
				AO.ResizeLONG(p, q);				/* CompressSize */
				p += lenL; q += sizeof(LONG);
				AO.ResizeWORD(p, q);				/* nsymbols */
				p += lenW; q += sizeof(WORD);
				AO.ResizeWORD(p, q);				/* nindices */
				p += lenW; q += sizeof(WORD);
				AO.ResizeWORD(p, q);				/* nvectors */
				p += lenW; q += sizeof(WORD);
				AO.ResizeWORD(p, q);				/* nfunctions */
				p += lenW; q += sizeof(WORD);
				AO.ResizeWORD(p, q);				/* size (unchanged!) */
				p += lenW; q += sizeof(WORD);
				n = MAXENAME + 1;
				NCOPYB(q, p, n)
				p += padp;
				q += padq;
			}
			/* read all the remaining INDEXENTRY and put them into the static buffer */
			if ( AO.bufferedInd ) {
				sbuffer.next = buffer.next;
				SETBASEPOSITION(sbuffer.number,AO.bufferedInd);
				q = (UBYTE *)&sbuffer + sizeof(POSITION) + sizeof(LONG);
				for ( i = maxnumber; i < number; ++i ) {
					AO.ResizePOS(p, q);				/* position */
					p += lenP; q += sizeof(POSITION);
					AO.ResizePOS(p, q);				/* length */
					p += lenP; q += sizeof(POSITION);
					AO.ResizePOS(p, q);				/* variables */
					p += lenP; q += sizeof(POSITION);
					AO.ResizeLONG(p, q);			/* CompressSize */
					p += lenL; q += sizeof(LONG);
					AO.ResizeWORD(p, q);			/* nsymbols */
					p += lenW; q += sizeof(WORD);
					AO.ResizeWORD(p, q);			/* nindices */
					p += lenW; q += sizeof(WORD);
					AO.ResizeWORD(p, q);			/* nvectors */
					p += lenW; q += sizeof(WORD);
					AO.ResizeWORD(p, q);			/* nfunctions */
					p += lenW; q += sizeof(WORD);
					AO.ResizeWORD(p, q);			/* size (unchanged!) */
					p += lenW; q += sizeof(WORD);
					n = MAXENAME + 1;
					NCOPYB(q, p, n)
					p += padp;
					q += padq;
				}
			}
			/* copy to output */
			p = (UBYTE *)fileind; q = (UBYTE *)&buffer; n = sizeof(FILEINDEX);
			NCOPYB(p, q, n)
		}
		return ( 0 );
	} else {
		return ( ReadFile(AO.SaveData.Handle, (UBYTE *)fileind, sizeof(FILEINDEX))
		         != sizeof(FILEINDEX) );
	}
}

/*
 		#] ReadSaveIndex : 
 		#[ ReadSaveVariables :
*/

/**
 *  Reads the variables from the open file specified by AO.SaveData.Handle. It
 *  reads the *size bytes and writes them to the *buffer. It is called by
 *  PutInStore().
 *
 *  If translation is necessary, the data might shrink or grow in size, then
 *  @e *size is adjusted so that the reading and writing fits into the memory
 *  from the buffer to the top. The actual number of read bytes is returned in
 *  @e *size, the number of written bytes is returned in @e *outsize.
 *
 *  If the *size is smaller than the actual size of the variables, this function
 *  will be called several times and needs to remember the current position in
 *  the variable structure. The parameter @e stage does this job. When
 *  ReadSaveVariables() is called for the first time, this parameter should
 *  have the value -1.
 *
 *  The parameter @e ind is used to get the number of variables.
 *
 *  @param  buffer   read variables are written into this allocated memory
 *  @param  top      upper end of allocated memory
 *  @param  size     number of bytes to read. might return a smaller number
 *                   of read bytes if translation was necessary
 *  @param  outsize  if translation has be done, outsize contains the number
 *                   of written bytes
 *  @param  ind      pointer of INDEXENTRY for the current expression. read-only
 *  @param  stage    should be -1 for the first call, will be increased by
 *                   ReadSaveVariables to memorize the position in the
 *                   variable structure
 *  @return          = 0 everything okay, != 0 an error occurred
 */
WORD ReadSaveVariables(UBYTE *buffer, UBYTE *top, LONG *size, LONG *outsize,\
                       INDEXENTRY *ind, LONG *stage)
{
	/* do we need some translation for the variables? */
	if ( AO.transFlag ) {
		/* counters for the number of already read symbols, indices, ... that
		   need to remain valid between different calls to ReadSaveVariables().
		   are initialized if stage == -1 */
		static WORD numReadSym;
		static WORD numReadInd;
		static WORD numReadVec;
		static WORD numReadFun;

		POSITION pos;
		UBYTE *in, *out, *pp = 0, *end, *outbuf;
		LONG numread;
		WORD namelen, realnamelen;
		/* shortcuts */
		WORD lenW = AO.SaveHeader.lenWORD;
		WORD lenL = AO.SaveHeader.lenLONG;
		WORD lenP = AO.SaveHeader.lenPOINTER;
		WORD flip = AO.transFlag & 1;

		/* remember file position in case we have to rewind */
		TELLFILE(AO.SaveData.Handle,&pos);

		/* decide on the position of the in and out buffers.
		   if the input is "bigger" than the output, we resize in-place, i.e.
		   we immediately overwrite the source data by the translated data. in
		   and out buffers start at the same place.
		   if not, we read from the end of the given buffer and write at the
		   beginning. */
		if ( (lenW > (WORD)sizeof(WORD))
		|| ( (lenW == (WORD)sizeof(WORD))
		     && ( (lenL > (WORD)sizeof(LONG))
		          || ( (lenL == (WORD)sizeof(LONG)) && lenP > (WORD)sizeof(void *))
		        )
		   ) ) {
			in = out = buffer;
			end = buffer + *size;
		}
		else {
			/* data will grow roughly by sizeof(WORD)/lenW. the exact value is
			   not important. if reading and writing areas start to overlap, the
			   reading will already be near the end of the data and overwriting
			   doesn't matter. */
			LONG newsize = (top - buffer) / (1 + sizeof(WORD)/lenW);
			end = top;
			out = buffer;
			in = end - newsize;
			if ( *size > newsize ) *size = newsize;
		}

		if ( ( numread = ReadFile(AO.SaveData.Handle, in, *size) ) != *size ) {
			return ( MesPrint("Error(2) reading stored expression.") );
		}

		*size = 0;
		*outsize = 0;

		/* first time in ReadSaveVariables(). initialize counters. */
		if ( *stage == -1 ) {
			numReadSym = 0;
			numReadInd = 0;
			numReadVec = 0;
			numReadFun = 0;
			++*stage;
		}

		while ( in < end ) {
			/* Symbols */
			if ( *stage == 0 ) {
				if ( ind->nsymbols <= numReadSym ) {
					++*stage;
					continue;
				}
				if ( end - in < AO.SaveHeader.sSym ) {
					goto RSVEnd;
				}
				if ( flip ) {
					pp = in;
					AO.FlipLONG(pp); pp += lenL;
					while ( pp < in + AO.SaveHeader.sSym ) {
						AO.FlipWORD(pp); pp += lenW;
					}
				}
				pp = in + AO.SaveHeader.sSym;
				AO.ResizeLONG(in, out); in += lenL; out += sizeof(LONG); /* name     */
				AO.CheckPower(in);
				AO.ResizeWORD(in, out); in += lenW;
				if ( *((WORD *)out) == -AO.mpower ) *((WORD *)out) = -MAXPOWER;
				out += sizeof(WORD);									 /* minpower */
				AO.ResizeWORD(in, out); in += lenW;
				if ( *((WORD *)out) == AO.mpower ) *((WORD *)out) = MAXPOWER;
				out += sizeof(WORD);									 /* maxpower */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* complex  */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* number   */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* flags    */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* node     */
				AO.ResizeWORD(in, out); in += lenW;                      /* namesize */
				realnamelen = *((WORD *)out);
				realnamelen += sizeof(void *)-1; realnamelen &= -(sizeof(void *));
				out += sizeof(WORD);
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* dimension */
				while ( in < pp ) {
					AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD);
				}
				namelen = *((WORD *)out-1); /* cares for padding "bug" */
				if ( end - in < namelen ) {
					goto RSVEnd;
				}
				*((WORD *)out-1) = realnamelen;
				*size += AO.SaveHeader.sSym + namelen;
				*outsize += sizeof(struct SyMbOl) + realnamelen;
				if ( realnamelen > namelen ) {
					int j = namelen;
					NCOPYB(out, in, j);
					out += realnamelen - namelen;
				}
				else {
					int j = realnamelen;
					NCOPYB(out, in, j);
					in += namelen - realnamelen;
				}
				++numReadSym;
				continue;
			}
			/* Indices */
			if ( *stage == 1 ) {
				if ( ind->nindices <= numReadInd ) {
					++*stage;
					continue;
				}
				if ( end - in < AO.SaveHeader.sInd ) {
					goto RSVEnd;
				}
				if ( flip ) {
					pp = in;
					AO.FlipLONG(pp); pp += lenL;
					while ( pp < in + AO.SaveHeader.sInd ) {
						AO.FlipWORD(pp); pp += lenW;
					}
				}
				pp = in + AO.SaveHeader.sInd;
				AO.ResizeLONG(in, out); in += lenL; out += sizeof(LONG); /* name      */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* type      */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* dimension */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* number    */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* flags     */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* nmin4     */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* node      */
				AO.ResizeWORD(in, out); in += lenW;                      /* namesize  */
				realnamelen = *((WORD *)out);
				realnamelen += sizeof(void *)-1; realnamelen &= -(sizeof(void *));
				out += sizeof(WORD);
				while ( in < pp ) {
					AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD);
				}
				namelen = *((WORD *)out-1); /* cares for padding "bug" */
				if ( end - in < namelen ) {
					goto RSVEnd;
				}
				*((WORD *)out-1) = realnamelen;
				*size += AO.SaveHeader.sInd + namelen;
				*outsize += sizeof(struct InDeX) + realnamelen;
				if ( realnamelen > namelen ) {
					int j = namelen;
					NCOPYB(out, in, j);
					out += realnamelen - namelen;
				}
				else {
					int j = realnamelen;
					NCOPYB(out, in, j);
					in += namelen - realnamelen;
				}
				++numReadInd;
				continue;
			}
			/* Vectors */
			if ( *stage == 2 ) {
				if ( ind->nvectors <= numReadVec ) {
					++*stage;
					continue;
				}
				if ( end - in < AO.SaveHeader.sVec ) {
					goto RSVEnd;
				}
				if ( flip ) {
					pp = in;
					AO.FlipLONG(pp); pp += lenL;
					while ( pp < in + AO.SaveHeader.sVec ) {
						AO.FlipWORD(pp); pp += lenW;
					}
				}
				pp = in + AO.SaveHeader.sVec;
				AO.ResizeLONG(in, out); in += lenL; out += sizeof(LONG); /* name     */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* complex  */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* number   */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* flags    */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* node     */
				AO.ResizeWORD(in, out); in += lenW;                      /* namesize */
				realnamelen = *((WORD *)out);
				realnamelen += sizeof(void *)-1; realnamelen &= -(sizeof(void *));
				out += sizeof(WORD);
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* dimension */
				while ( in < pp ) {
					AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD);
				}
				namelen = *((WORD *)out-1); /* cares for padding "bug" */
				if ( end - in < namelen ) {
					goto RSVEnd;
				}
				*((WORD *)out-1) = realnamelen;
				*size += AO.SaveHeader.sVec + namelen;
				*outsize += sizeof(struct VeCtOr) + realnamelen;
				if ( realnamelen > namelen ) {
					int j = namelen;
					NCOPYB(out, in, j)
					out += realnamelen - namelen;
				}
				else {
					int j = realnamelen;
					NCOPYB(out, in, j)
					in += namelen - realnamelen;
				}
				++numReadVec;
				continue;
			}
			/* Functions */
			if ( *stage == 3 ) {
				if ( ind->nfunctions <= numReadFun ) {
					++*stage;
					continue;
				}
				if ( end - in < AO.SaveHeader.sFun ) {
					goto RSVEnd;
				}
				if ( flip ) {
					pp = in;
					AO.FlipPOINTER(pp); pp += lenP;
					AO.FlipLONG(pp); pp += lenL;
					AO.FlipLONG(pp); pp += lenL;
					while ( pp < in + AO.SaveHeader.sFun ) {
						AO.FlipWORD(pp); pp += lenW;
					}
				}
				pp = in + AO.SaveHeader.sFun;
				outbuf = out;
				AO.ResizePOINTER(in, out); in += lenP; out += sizeof(void *); /* tabl */
				AO.ResizeLONG(in, out); in += lenL; out += sizeof(LONG); /* symminfo  */
				AO.ResizeLONG(in, out); in += lenL; out += sizeof(LONG); /* name      */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* commute   */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* complex   */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* number    */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* flags     */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* spec      */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* symmetric */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* numargs */
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* node      */
				AO.ResizeWORD(in, out); in += lenW;                      /* namesize  */
				realnamelen = *((WORD *)out);
				realnamelen += sizeof(void *)-1; realnamelen &= -(sizeof(void *));
				out += sizeof(WORD);
				AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD); /* dimension */
				while ( in < pp ) {
					AO.ResizeWORD(in, out); in += lenW; out += sizeof(WORD);
				}
				namelen = *((WORD *)out-1); /* cares for padding "bug" */
				if ( end - in < namelen ) {
					goto RSVEnd;
				}
				*((WORD *)out-1) = realnamelen;
				*size += AO.SaveHeader.sFun + namelen;
				*outsize += sizeof(struct FuNcTiOn) + realnamelen;
				if ( realnamelen > namelen ) {
					int j = namelen;
					NCOPYB(out, in, j);
					out += realnamelen - namelen;
				}
				else {
					int j = realnamelen;
					NCOPYB(out, in, j);
					in += namelen - realnamelen;
				}
				++numReadFun;
				/* we use the information whether a function is tensorial later in ReadSaveTerm */
				AO.tensorList[((FUNCTIONS)outbuf)->number+FUNCTION] =
					(UBYTE)(((FUNCTIONS)outbuf)->spec == TENSORFUNCTION);
				continue;
			}
			/* handle numdummies */
			if ( end - in >= lenW ) {
				if ( flip ) AO.FlipWORD(in);
				AO.ResizeWORD(in, out);
				*size += lenW;
				*outsize += sizeof(WORD);
			}
			/* handle numfactors */
			if ( end - in >= lenW ) {
				if ( flip ) AO.FlipWORD(in);
				AO.ResizeWORD(in, out);
				*size += lenW;
				*outsize += sizeof(WORD);
			}
			/* handle vflags */
			if ( end - in >= lenW ) {
				if ( flip ) AO.FlipWORD(in);
				AO.ResizeWORD(in, out);
				*size += lenW;
				*outsize += sizeof(WORD);
			}
			return ( 0 );
		}

RSVEnd:
		/* we are here because the remaining buffer cannot hold the next
		   struct. we position the file behind the last sucessfully translated
		   struct and return. */
		ADDPOS(pos, *size);
		SeekFile(AO.SaveData.Handle, &pos, SEEK_SET);
		return ( 0 );
	} else {
		return ( ReadFile(AO.SaveData.Handle, buffer, *size) != *size );
	}
}

/*
 		#] ReadSaveVariables : 
 		#[ ReadSaveTerm :
*/

/**
 *  Reads a single term from the given buffer at @e bin and write the
 *  translated term back to this buffer at @e bout.
 *
 *  ReadSaveTerm32() is currently the only instantiation of a
 *  ReadSaveTerm-function. It only deals with data that already has the correct
 *  endianness and that is resized to 32bit words but without being renumbered
 *  or translated in any other way. It uses the compress buffer
 *  AR.CompressBuffer.
 *
 *  The function is reentrant in order to cope with nested function arguments.
 *  It is called by ReadSaveExpression() and itself.
 *
 *  The @e return @e value indicates the position in the input buffer up to
 *  which the data has already been successfully processed. The parameter
 *  @e bout returns the corresponding position in the output buffer.
 *
 *  @param  bin        start of the input buffer
 *  @param  binend     end of the input buffer
 *  @param  bout       as input points to the beginning of the output buffer,
 *                     as output points behind the already translated data in
 *                     the output buffer
 *  @param  boutend    end of already decompressed data in output buffer
 *  @param  top        end of output buffer
 *  @param  terminbuf  flag whether decompressed data is already in the output
 *                     buffer. used in recursive calls
 *  @return            pointer to the next unprocessed data in the input buffer
 */
UBYTE *
ReadSaveTerm32(UBYTE *bin, UBYTE *binend, UBYTE **bout, UBYTE *boutend, UBYTE *top, int terminbuf)
{
	GETIDENTITY

	UBYTE *boutbuf;
	INT32 len, j, id;
	INT32 *r, *t, *coeff, *end, *newtermsize, *rend;
	INT32 *newsubtermp;
	INT32 *in = (INT32 *)bin;
	INT32 *out = (INT32 *)*bout;

	/* if called recursively the term is already decompressed in buffer.
	   is this the case? */
	if ( terminbuf ) {
		/* don't do any decompression, just adjust the pointers */
		len = *out;
		end = out + len;
		r = in + 1;
		rend = (INT32 *)boutend;
		coeff = end - ABS(*(end-1));
		newtermsize = (INT32 *)*bout;
		out = newtermsize + 1;
	}
	else {
		/* do deprompression of necessary. always return if the space in the
		   buffer is not sufficient */
		INT32 rbuf;
		r = (INT32 *)AR.CompressBuffer;
		rbuf = *r;
		len = j = *in;
		/* first copy from AR.CompressBuffer if necessary */
		if ( j < 0 ) {
			++in;
			if ( (UBYTE *)in >= binend ) {
				return ( bin );
			}
			*out = len = -j + 1 + *in;
			end = out + *out;
			if ( (UBYTE *)end >= top ) {
				return ( bin );
			}
			++out;
			*r++ = len;
			while ( ++j <= 0 ) {
				INT32 bb = *r++;
				*out++ = bb;
			}
			j = *in++;
		}
		else if ( j == 0 ) {
			/* care for padding words */
			while ( (UBYTE *)in < binend ) {
				*out++ = 0;
				if ( (UBYTE *)out > top ) {
					return ( (UBYTE *)bin );
				}

				*r++ = 0;
				++in;
			}
			*bout = (UBYTE *)out;
			return ( (UBYTE *)in );
		}
		else {
			end = out + len;
			if ( (UBYTE *)end >= top ) {
				return ( bin );
			}
		}
		if ( (UBYTE *)(in + j) >= binend ) {
			*(AR.CompressBuffer) = rbuf;
			return ( bin );
		}
		if ( (UBYTE *)out + j >= top ) {
			return ( bin );
		}
		/* second copy from input buffer */
		while ( --j >= 0 ) {
			INT32 bb = *in++;
			*r++ = *out++ = bb;
		}

		rend = r;
		r = (INT32 *)AR.CompressBuffer + 1;
		coeff = end - ABS(*(end-1));
		newtermsize = (INT32 *)*bout;
		out = newtermsize + 1;
	}

	/* iterate over subterms */
	while ( out < coeff ) {

		id = *out++;
		++r;
		t = out + *out - 1;
		newsubtermp = out;
		++out; ++r;

		if ( id == SYMBOL ) {
			while ( out < t ) {
				++out; ++r; /* symbol number */
				/* if exponent is too big, rewrite as exponent function */
				if ( ABS(*out) >= MAXPOWER ) { 
					INT32 *a, *b;
					INT32 n;
					INT32 num = *(out-1);
					INT32 exp = *out;
					coeff += 9;
					end += 9;
					t += 9;
					if ( (UBYTE *)end > top ) return ( bin );
					out -= 3;
					*out++ = EXPONENT;		/* id */
					*out++ = 13;			/* size */
					*out++ = 1;				/* dirtyflag */
					*out++ = -SYMBOL;		/* first short arg */
					*out++ = num;
					*out++ = 8;				/* second arg, size */
					*out++ = 0;				/* dirtyflag */
					*out++ = 6;				/* term size */
					*out++ = ABS(exp) & 0x0000FFFF;
					*out++ = ABS(exp) >> 16;
					*out++ = 1;	
					*out++ = 0;
					*out++ = ( exp < 0 ) ? -5 : 5;
					a = ++r;
					b = out;
					n = rend - r;
					NCOPYI32(b, a, n)
				}
				else {
					++out; ++r;
				}
			}
		}
		else if ( id == DOTPRODUCT ) {
			while ( out < t ) {
				AO.RenumberVec((UBYTE *)out); /* vector 1 */
				++out; ++r;
				AO.RenumberVec((UBYTE *)out); /* vector 2 */
				++out; ++r;
				/* if exponent is too big, rewrite as exponent function */
				if ( ABS(*out) >= MAXPOWER ) { 
					INT32 *a, *b;
					INT32 n;
					INT32 num1 = *(out-2);
					INT32 num2 = *(out-1);
					INT32 exp = *out;
					coeff += 17;
					end += 17;
					t += 17;
					if ( (UBYTE *)end > top ) return ( bin );
					out -= 4;
					*out++ = EXPONENT;		/* id */
					*out++ = 22;			/* size */
					*out++ = 1;				/* dirtyflag */
					*out++ = 11;			/* first arg, size */
					*out++ = 0;				/* dirtyflag */
					*out++ = 9;				/* term size */
					*out++ = DOTPRODUCT;	/* p1.p2 */
					*out++ = 5;				/* subterm size */
					*out++ = num1;			/* p1 */
					*out++ = num2;			/* p2 */
					*out++ = 1;				/* exponent */
					*out++ = 1;				/* coeff */
					*out++ = 1;
					*out++ = 3;
					*out++ = 8;				/* second arg, size */
					*out++ = 0;				/* dirtyflag */
					*out++ = 6;				/* term size */
					*out++ = ABS(exp) & 0x0000FFFF;
					*out++ = ABS(exp) >> 16;
					*out++ = 1;	
					*out++ = 0;
					*out++ = ( exp < 0 ) ? -5 : 5;
					a = ++r;
					b = out;
					n = rend - r;
					NCOPYI32(b, a, n)
				}
				else {
					++out; ++r;
				}
			}
		}
		else if ( id == VECTOR ) {
			while ( out < t ) {
				AO.RenumberVec((UBYTE *)out); /* vector number */
				++out; ++r;
				++out; ++r; /* index, do nothing */
			}
		}
		else if ( id == INDEX ) {
/*			INT32 vectoroffset = -2 * *((INT32 *)AO.SaveHeader.wildoffset); */
			void *dummy = (void *)AO.SaveHeader.wildoffset;  /* to remove a warning about strict-aliasing rules in gcc */
			INT32 vectoroffset = -2 * *((INT32 *)dummy);
			while ( out < t ) {
				/* if there is a vector, renumber it */
				if ( *out < vectoroffset ) {
					AO.RenumberVec((UBYTE *)out);
				}
				++out; ++r;
			}
		}
		else if ( id == SUBEXPRESSION ) {
			/* nothing to translate */
			while ( out < t ) {
				++out; ++r;
			}
		}
		else if ( id == DELTA ) {
			/* nothing to translate */
			r += t - out;
			out = t;
		}
		else if ( id == HAAKJE ) {
			/* nothing to translate */
			r += t - out;
			out = t;
		}
		else if ( id == GAMMA || id == LEVICIVITA || (id >= FUNCTION && AO.tensorList[id]) ) {
/*			INT32 vectoroffset = -2 * *((INT32 *)AO.SaveHeader.wildoffset); */
			void *dummy = (void *)AO.SaveHeader.wildoffset;  /* to remove a warning about strict-aliasing rules in gcc */
			INT32 vectoroffset = -2 * *((INT32 *)dummy);
			while ( out < t ) {
				/* if there is a vector as an argument, renumber it */
				if ( *out < vectoroffset ) {
					AO.RenumberVec((UBYTE *)out);
				}
				++out; ++r;
			}
		}
		else if ( id >= FUNCTION ) {
			INT32 *argEnd;
			UBYTE *newbin;

			++out; ++r; /* dirty flags */
			
			/* loop over arguments */
			while ( out < t ) {
				if ( *out < 0 ) {
					/* short notation arguments */
					switch ( -*out ) {
						case SYMBOL:
							++out; ++r;
							++out; ++r;
							break;
						case SNUMBER:
							++out; ++r;
							if ( sizeof(WORD) == 2 ) {
								/* resize if needed */
								if ( *out > (1<<15)-1 || *out < -(1<<15)+1 ) {
									INT32 *a, *b;
									INT32 n;
									INT32 num = *out;
									coeff += 6;
									end += 6;
									argEnd += 6;
									t += 6;
									if ( (UBYTE *)end > top ) return ( bin );
									--out;
									*out++ = 8;			/* argument size */
									*out++ = 0;			/* dirtyflag */
									*out++ = 6;			/* term size */
									*out++ = ABS(num) & 0x0000FFFF;
									*out++ = ABS(num) >> 16;
									*out++ = 1;
									*out++ = 0;
									*out++ = ( num < 0 ) ? -5 : 5;
									a = ++r;
									b = out;
									n = rend - r;
									NCOPYI32(b, a, n)
								}
								else {
									++out; ++r;
								}
							}
							else {
								++out; ++r;
							}
							break;
						case VECTOR:
							++out; ++r;
							AO.RenumberVec((UBYTE *)out);
							++out; ++r;
							break;
						case INDEX:
							++out; ++r;
							++out; ++r;
							break;
						case MINVECTOR:
							++out; ++r;
							AO.RenumberVec((UBYTE *)out);
							++out; ++r;
							break;
						default:
							if ( -*out >= FUNCTION ) {
								++out; ++r;
								break;
							} else {
								MesPrint("short function code %d not implemented.", *out);
								return ( (UBYTE *)in );
							}
					}
				}
				else {
					/* long arguments */
					INT32 *newargsize = out;
					argEnd = out + *out;
					++out; ++r;
					++out; ++r; /* dirty flags */
					while ( out < argEnd ) {
						INT32 *keepsizep = out + *out;
						INT32 lenbuf = *out;
						INT32 **ppp = &out; /* to avoid a compiler warning */
						/* recursion */
						newbin = ReadSaveTerm32((UBYTE *)r, binend, (UBYTE **)ppp, (UBYTE *)rend, top, 1);
						r += lenbuf;
						if ( newbin == (UBYTE *)r ) {
							return ( (UBYTE *)in );
						}
						/* if the term done by recursion has changed in size,
						   we need to move the rest of the data accordingly */
						if ( out > keepsizep ) {
							INT32 *a, *b;
							INT32 n;
							INT32 extention = out - keepsizep;
							a = r;
							b = out;
							n = rend - r;
							NCOPYI32(b, a, n)
							coeff += extention;
							end += extention;
							argEnd += extention;
							t += extention;
						}
						else if ( out < keepsizep ) {
							INT32 *a, *b;
							INT32 n;
							INT32 extention = keepsizep - out;
							a = keepsizep;
							b = out;
							n = rend - r;
							NCOPYI32(b, a, n)
							coeff -= extention;
							end -= extention;
							argEnd -= extention;
							t -= extention;
						}
					}
					*newargsize = out - newargsize;
				}
			}
		}
		else {
			MesPrint("ID %d not recognized.", id);
			return ( (UBYTE *)in );
		}

		*newsubtermp = out - newsubtermp + 1;
	}

	if ( (UBYTE *)end >= top ) {
		return ( bin );
	}

	/* do coefficient and adjust term size */
	boutbuf = *bout;
	*bout = (UBYTE *)out;

	ResizeCoeff32(bout, (UBYTE *)end, top);

	if ( *bout >= top ) {
		*bout = boutbuf;
		return ( bin );
	}

	*newtermsize = (INT32 *)*bout - newtermsize;

	return ( (UBYTE *)in );
}

/*
 		#] ReadSaveTerm : 
 		#[ ReadSaveExpression :
*/

/**
 *  Reads an expression from the open file specified by AO.SaveData.Handle.
 *  The endianness flip and a resizing without renumbering is done in this
 *  function. Thereafter the buffer consists of chunks with a uniform maximal
 *  word size (32bit at the moment). The actual renumbering is then done by
 *  calling the function ReadSaveTerm32(). The result is returned in @e buffer.
 *
 *  If the translation at some point doesn't fit into the buffer anymore, the
 *  function returns and must be called again. In any case @e size returns the
 *  number of successfully read bytes, @e outsize returns the number of
 *  successfully written bytes, and the file will be positioned at the next
 *  byte after the successfully read data.
 *
 *  It is called by PutInStore().
 *
 *  @param  buffer   output buffer, holds the (translated) expression
 *  @param  top      end of buffer
 *  @param  size     number of read bytes
 *  @param  outsize  number of written bytes
 *  @return          = 0 everything okay, != 0 an error occurred
 */
WORD ReadSaveExpression(UBYTE *buffer, UBYTE *top, LONG *size, LONG *outsize)
{
	if ( AO.transFlag ) {
		UBYTE *in, *end, *out, *outend, *p;
		POSITION pos;
		LONG half;
		WORD lenW = AO.SaveHeader.lenWORD;

		/* remember the last file position in case an expression cannot be
		   fully processed */
		TELLFILE(AO.SaveData.Handle,&pos);

		/* adjust 'size' depending on whether the translated data is bigger or
		   smaller */ 
		half = (top-buffer)/2;
		if ( *size > half ) *size = half;
		if ( lenW < (WORD)sizeof(WORD) ) {
			if ( *size * (LONG)sizeof(WORD)/lenW > half ) *size = half*lenW/(LONG)sizeof(WORD);
		}
		else {
			if ( *size > half ) *size = half;
		}

		/* depending on the necessary resizing we position the input pointer
		   either at the start of the buffer or in the middle. if the data will
		   roughly remain the same size, we need only one processing step, so
		   we put the 'in' at the middle and 'out' and the beginning. in the
		   other cases we need two processing steps, so first we put 'in' at
		   the beginning and write at the middle. the second step can then read
		   from the middle and put its results at the beginning. */
		in = out = buffer;
		if ( lenW == sizeof(WORD) ) in += half;
		else out += half;
		end = in + *size;
		outend = out + *size;

		if ( ReadFile(AO.SaveData.Handle, in, *size) != *size ) {
			return ( MesPrint("Error(3) reading stored expression.") );
		}

		if ( AO.transFlag & 1 ) {
			p = in;
			end -= lenW;
			while ( p <= end ) {
				AO.FlipWORD(p); p += lenW;
			}
			end += lenW;
		}

		if ( lenW > (WORD)sizeof(WORD) ) {
			/* renumber first */
			do {
				outend = out+*size;
				if ( outend > top ) outend = top;
				p = ReadSaveTerm32(in, end, &out, outend, top, 0);
				if ( p == in ) break;
				in = p;
			} while ( in <= end - lenW );
			/* then resize */
			*size = in - buffer;
			in = buffer + half;
			end = out;
			out = buffer;

			while ( in < end ) {
				/* resize without checking */
				AO.ResizeNCWORD(in, out);
				in += lenW; out += sizeof(WORD);
			}
		}
		else {
			if ( lenW < (WORD)sizeof(WORD) ) {
				/* resize first */
				while ( in < end ) {
					AO.ResizeWORD(in, out);
					in += lenW; out += sizeof(WORD);
				}
				in = buffer + half;
				end = out;
				out = buffer;
			}
			/* then renumber */
			do {
				p = ReadSaveTerm32(in, end, &out,  buffer+half, buffer+half, 0);
				if ( p == in ) break;
				in = p;
			} while ( in <= end - sizeof(WORD) );
			*size = (in - buffer - half) * lenW / (ULONG)sizeof(WORD);
		}
		*outsize = out - buffer;
		ADDPOS(pos, *size);
		SeekFile(AO.SaveData.Handle, &pos, SEEK_SET);

		return ( 0 );
	}
	else {
		return ( ReadFile(AO.SaveData.Handle, buffer, *size) != *size );
	}
}

/*
 		#] ReadSaveExpression : 
	#] System Independent Saved Expressions :
*/
