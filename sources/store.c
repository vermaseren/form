/*
  	#[ Includes : store.c
*/

#include "form3.h"

extern LONG deferskipped;
WORD *dummyrenumlist;

/*
  	#] Includes :
	#[ StoreExpressions :
 		#[ OpenTemp :

		Opens the scratch files for the input -> output operations.

*/

WORD
OpenTemp()
{
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

VOID
SeekScratch ARG2(FILEHANDLE *,fi,POSITION *,pos)
{
	*pos = fi->POposition;
	ADDPOS(*pos,(TOLONG(fi->POfill)-TOLONG(fi->PObuffer)));
}

/*
 		#] SeekScratch :
 		#[ SetEndScratch :
*/

VOID
SetEndScratch ARG2(FILEHANDLE *,f,POSITION *,position)
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

VOID
SetEndHScratch ARG2(FILEHANDLE *,f,POSITION *,position)
{
	if ( f->handle < 0 ) {
		SETBASEPOSITION(*position,(f->POfull-f->PObuffer)*sizeof(WORD));
	}
	else *position = f->filesize;
	SetScratch(f,position);
}

/*
 		#] SetEndHScratch :
 		#[ SetScratch :
*/

VOID
SetScratch ARG2(FILEHANDLE *,f,POSITION *,position)
{
	POSITION possize;
	LONG size;
/*
	if ( f != AR.infile && f != AS.hidefile ) {
		MesPrint("SetScratch: reading only!"); Terminate(-1);
	}
*/
	if ( ISLESSPOS(*position,f->POposition) ||
	ISGEPOSINC(*position,f->POposition,(f->POfull-f->PObuffer)*sizeof(WORD)) ) {
		if ( f->handle < 0 ) {
			if ( ISEQUALPOSINC(*position,f->POposition,
				(f->POfull-f->PObuffer)*sizeof(WORD)) ) goto endpos;
			MesPrint("Illegal position in SetScratch");
			Terminate(-1);
		}
		possize = *position;
		SeekFile(f->handle,&possize,SEEK_SET);
		if ( ISNOTEQUALPOS(possize,*position) ) {
			MesPrint("Cannot position file in SetScratch");
			Terminate(-1);
		}
/* --COMPRESS-- */
		if ( ( size = ReadFile(f->handle,(UBYTE *)(f->PObuffer),f->POsize) ) < 0
		|| ( size & 1 ) != 0 ) {
			MesPrint("Read error in SetScratch");
			Terminate(-1);
		}
		if ( size == 0 ) { f->PObuffer[0] = 0; }
		f->POfill = f->PObuffer;
		f->POposition = *position;
#ifdef WORD2
		AR.InInBuf = size >> 1;
#else
		AR.InInBuf = size / TABLESIZE(WORD,UBYTE);
#endif
		f->POfull = f->PObuffer + AR.InInBuf;
	}
	else {
endpos:
		DIFPOS(possize,*position,f->POposition);
		f->POfill = (WORD *)(BASEPOSITION(possize)+(UBYTE *)(f->PObuffer));
		if ( f != AS.hidefile) AR.InInBuf = f->POfull-f->POfill;
	}
}

/*
 		#] SetScratch :
 		#[ RevertScratch :

		Reverts the input/output directions. This way input comes
		always from AR.infile

*/

WORD
RevertScratch()
{
	FILEHANDLE *f;
	if ( AR.infile->handle >= 0 && AR.infile->handle != AR.outfile->handle ) {
		CloseFile(AR.infile->handle);
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
/* --COMPRESS-- */
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

WORD
ResetScratch()
{
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
/* --COMPRESS-- */
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
 		#[ CoSave :

		The syntax of the save statement is:

		save filename
		save filename expr1 expr2

*/

int CoSave ARG1(UBYTE *,inp)
{
	UBYTE *p, c;
	WORD n = 0, i;
	WORD error = 0, type, number;
	LONG RetCode = 0, wSize;
	EXPRESSIONS e;
	INDEXENTRY *ind;
	INDEXENTRY *indold;
	WORD TMproto[SUBEXPSIZE];
	POSITION scrpos, scrpos1, filesize;
	p = inp;

#ifdef PARALLEL
	if(	PF.me != MASTER) return(0);
#endif

	if ( !*p ) return(MesPrint("No filename in save statement"));
	if ( FG.cTable[*p] ) return(MesPrint("Illegal filename"));
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
		AO.SaveData.Index.number = 0;
		PUTZERO(AO.SaveData.Index.next);
		PUTZERO(AO.SaveData.Position);
		ind = AO.SaveData.Index.expression;
/* --COMPRESS-- */
		if ( !AP.preError && WriteFile(AO.SaveData.Handle,(UBYTE *)(&(AO.SaveData.Index))
		,(LONG)sizeof(struct FiLeInDeX))!= (LONG)sizeof(struct FiLeInDeX) ) goto SavWrt;
		ADDPOS(filesize,sizeof(struct FiLeInDeX));

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
					if ( ( indold = FindInIndex(number,&AO.StoreData,0) ) != 0 ) {
						if ( i <= 0 ) {
/*
							AO.SaveData.Index.next = SeekFile(AO.SaveData.Handle,&(AM.zeropos),SEEK_END);
*/
							AO.SaveData.Index.next = filesize;
							scrpos = AO.SaveData.Position;
							SeekFile(AO.SaveData.Handle,&scrpos,SEEK_SET);
							if ( ISNOTEQUALPOS(scrpos,AO.SaveData.Position) ) goto SavWrt;
/* --COMPRESS-- */
							if ( WriteFile(AO.SaveData.Handle,(UBYTE *)(&(AO.SaveData.Index))
									,(LONG)sizeof(struct FiLeInDeX)) != (LONG)sizeof(struct FiLeInDeX) )
								goto SavWrt;
							i = (WORD)(INFILEINDEX);
							AO.SaveData.Position = AO.SaveData.Index.next;
							AO.SaveData.Index.number = 0;
							PUTZERO(AO.SaveData.Index.next);
							ind = AO.SaveData.Index.expression;
							scrpos = AO.SaveData.Position;
							SeekFile(AO.SaveData.Handle,&scrpos,SEEK_SET);
							if ( ISNOTEQUALPOS(scrpos,AO.SaveData.Position) ) goto SavWrt;
/* --COMPRESS-- */
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
						SeekFile(AO.StoreData.Handle,&(indold->variables),SEEK_SET);
						wSize = TOLONG(AT.WorkTop) - TOLONG(AT.WorkPointer);
						scrpos = ind->length;
						ADDPOS(scrpos,DIFBASE(ind->position,ind->variables));
						ADD2POS(filesize,scrpos);
						SETBASEPOSITION(scrpos1,wSize);
						do {
							if ( ISLESSPOS(scrpos,scrpos1) ) wSize = BASEPOSITION(scrpos);
/* --COMPRESS--? */
							if ( ReadFile(AO.StoreData.Handle,(UBYTE *)AT.WorkPointer,wSize)
							!= wSize ) {
								MesPrint("ReadError");
								error = -1;
								goto EndSave;
							}
/* --COMPRESS--? */
							if ( WriteFile(AO.SaveData.Handle,(UBYTE *)AT.WorkPointer,wSize)
							!= wSize ) goto SavWrt;
							ADDPOS(scrpos,-wSize);
						} while ( ISPOSPOS(scrpos) );
						(AO.SaveData.Index.number)++;
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
/* --COMPRESS-- */
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
			SeekFile(AO.StoreData.Handle,&scrpos,SEEK_SET);		/* Start at the beginning */
			scrpos = AO.StoreData.Fill;			/* Number of bytes to be copied */
			SETBASEPOSITION(scrpos1,wSize);
			do {
				if ( ISLESSPOS(scrpos,scrpos1) ) wSize = BASEPOSITION(scrpos);
/* --COMPRESS--? */
				if ( ReadFile(AO.StoreData.Handle,(UBYTE *)AT.WorkPointer,wSize) != wSize ) {
					MesPrint("ReadError");
					error = -1;
					goto EndSave;
				}
/* --COMPRESS--? */
				if ( WriteFile(AO.SaveData.Handle,(UBYTE *)AT.WorkPointer,wSize) != wSize )
					goto SavWrt;
				ADDPOS(scrpos,-wSize);
			} while ( ISPOSPOS(scrpos) );
		}
	}
EndSave:
	if ( !AP.preError ) CloseFile(AO.SaveData.Handle);
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

int CoLoad ARG1(UBYTE *,inp)
{
	INDEXENTRY *ind;
	LONG RetCode;
	UBYTE *p, c;
	WORD num, i, error = 0;
	WORD type, number, silentload = 0;
	WORD TMproto[SUBEXPSIZE];
	POSITION scrpos;
	p = inp;
	if ( ( *p == ',' && p[1] == '-' ) || *p == '-' ) {
		if ( *p == ',' ) p++;
		p++;
		if ( *p == 's' || *p == 'S' ) {
			silentload = 1;
			while ( *p && ( *p != ',' && *p != '-' && *p != '+' ) ) p++;
		}
		else if ( *p != ',' ) {
			return(MesPrint("Illegal option in Load statement"));
		}
		while ( *p == ',' ) p++;
	}
	inp = p;
	if ( !*p ) return(MesPrint("No filename in load statement"));
	if ( FG.cTable[*p] ) return(MesPrint("Illegal filename"));
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
/* --COMPRESS--? */
	if ( ReadFile(AO.SaveData.Handle,(UBYTE *)(&(AO.SaveData.Index)),
		(LONG)sizeof(struct FiLeInDeX)) != (LONG)sizeof(struct FiLeInDeX) ) goto LoadRead;

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
				if ( ( num = EntVar(CEXPRESSION,inp,STOREDEXPRESSION,0,0) ) >= 0 ) {
					TMproto[0] = EXPRESSION;
					TMproto[1] = SUBEXPSIZE;
					TMproto[2] = num;
					TMproto[3] = 1;
					{ int ie; for ( ie = 4; ie < SUBEXPSIZE; ie++ ) TMproto[ie] = 0; }
					AT.TMaddr = TMproto;
					if ( ( ind = FindInIndex(num,&AO.SaveData,1) ) != 0 ) {
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
		scrpos = AO.StoreData.Position;
		SeekFile(AO.StoreData.Handle,&scrpos,SEEK_SET);
		if ( ISNOTEQUALPOS(scrpos,AO.StoreData.Position) ) goto LoadWrt;
/* --COMPRESS-- */
		if ( WriteFile(AO.StoreData.Handle,(UBYTE *)(&(AO.StoreData.Index))
			,(LONG)sizeof(struct FiLeInDeX)) != (LONG)sizeof(struct FiLeInDeX) ) goto LoadWrt;
	}
	else {				/* All saved expressions should be stored. Easy */
		i = (WORD)(AO.SaveData.Index.number);
		ind = AO.SaveData.Index.expression;
		if ( i > 0 ) { do {
			if ( GetVar((UBYTE *)(ind->name),&type,&number,ALLVARIABLES,NOAUTO) != NAMENOTFOUND ) {
				MesPrint("Conflicting name: %s",ind->name);
				error = -1;
			}
			else {
				if ( ( num = EntVar(CEXPRESSION,(UBYTE *)(ind->name),STOREDEXPRESSION,0,0) ) >= 0 ) {
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
/* --COMPRESS--? */
				if ( ReadFile(AO.SaveData.Handle,(UBYTE *)(&(AO.SaveData.Index)),
					(LONG)sizeof(struct FiLeInDeX)) != (LONG)sizeof(struct FiLeInDeX) ) goto LoadRead;
				i = (WORD)(AO.SaveData.Index.number);
				ind = AO.SaveData.Index.expression;
			}
			else ind++;
		} while ( i > 0 ); }
	}
EndLoad:
	CloseFile(AO.SaveData.Handle);
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

WORD
DeleteStore ARG1(WORD,par)
{
	char *s;
	WORD j, n = 0;
	EXPRESSIONS e_in, e_out;
	WORD DidClean = 0;
	if ( AO.StoreData.Handle >= 0 ) {
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
						e_out->printflag = 0;
						e_out->status = e_in->status;
						e_out->name = e_in->name;
						e_out->inmem = e_in->inmem;
					}
					e_out++;
					j++;
				}
				e_in++;
			} while ( --n > 0 ); }
			NumExpressions = j;
			if ( DidClean ) CompactifyTree(AC.exprnames);
		}
		AO.StoreData.Handle = -1;
		CloseFile(AC.StoreHandle);
		{
/*
			Knock out the storage caches (25-apr-1990!)
*/
			STORECACHE st;
			st = (STORECACHE)(AR.StoreCache);
			while ( st ) {
				SETBASEPOSITION(st->position,-1);
				SETBASEPOSITION(st->toppos,-1);
				st = st->next;
			}
		}
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

WORD
PutInStore ARG2(INDEXENTRY *,ind,WORD,num)
{
	INDEXENTRY *newind;
	LONG wSize;
	POSITION scrpos,scrpos1;
	newind = NextFileIndex(&(Expressions[num].onfile));
	*newind = *ind;
	newind->variables = AO.StoreData.Fill;
	SeekFile(AO.StoreData.Handle,&(newind->variables),SEEK_SET);
	if ( ISNOTEQUALPOS(newind->variables,AO.StoreData.Fill) ) goto PutErrS;
	newind->position = newind->variables;
	ADDPOS(newind->position,DIFBASE(ind->position,ind->variables));
	scrpos = ind->variables;
	SeekFile(AO.SaveData.Handle,&scrpos,SEEK_SET);
	if ( ISNOTEQUALPOS(scrpos,ind->variables) ) goto PutErrS;
	wSize = TOLONG(AT.WorkTop) - TOLONG(AT.WorkPointer);
	scrpos = ind->length;
	ADDPOS(scrpos,DIFBASE(ind->position,ind->variables));
	ADD2POS(AO.StoreData.Fill,scrpos);
	SETBASEPOSITION(scrpos1,wSize);
	do {
		if ( ISLESSPOS(scrpos,scrpos1) ) wSize = BASEPOSITION(scrpos);
/* --COMPRESS--? */
		if ( ReadFile(AO.SaveData.Handle,(UBYTE *)AT.WorkPointer,wSize)
		!= wSize ) goto PutErrS;
/* --COMPRESS--? */
		if ( WriteFile(AO.StoreData.Handle,(UBYTE *)AT.WorkPointer,wSize)
		!= wSize ) goto PutErrS;
		ADDPOS(scrpos,-wSize);
	} while ( ISPOSPOS(scrpos) );
	scrpos = AO.StoreData.Position;
	SeekFile(AO.StoreData.Handle,&scrpos,SEEK_SET);
	if ( ISNOTEQUALPOS(scrpos,AO.StoreData.Position) ) goto PutErrS;
/* --COMPRESS-- */
	if ( WriteFile(AO.StoreData.Handle,(UBYTE *)(&AO.StoreData.Index),(LONG)sizeof(FILEINDEX))
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

*/

WORD
GetTerm ARG1(WORD *,term)
{
	WORD *inp, i, j = 0, len;
	LONG InIn = AR.InInBuf, InIn2;
	WORD *r, *m, *mstop = 0, minsiz = 0, *bra = 0, *from;
	WORD first, *start = 0, testing = 0;
	FILEHANDLE *fi;
	deferskipped = 0;
	if ( AS.GetFile == 2 ) fi = AS.hidefile;
	else                   fi = AR.infile;
	from = term;
	if ( AS.KeptInHold ) {
		r = AM.CompressBuffer;
		i = *r;
		if ( i <= 0 ) { *term = 0; goto RegRet; }
		m = term;
		NCOPY(m,r,i);
		AS.KeptInHold = 0;
		goto RegRet;
	}
	if ( AR.DeferFlag ) {
		m = AM.CompressBuffer;
		if ( *m > 0 ) {
			mstop = m + *m;
			mstop -= ABS(mstop[-1]);
			m++;
			while ( m < mstop ) {
				if ( *m == HAAKJE ) {
					testing = 1;
					mstop = m + m[1];
					bra = term + 2*AM.MaxTer;
					m = AM.CompressBuffer+1;
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
		bra = term+2*AM.MaxTer;
		mstop = bra+1;
		*bra = 0;
		minsiz = 1;
		testing = 1;
	}
ReStart:
	first = 0;
	r = AM.CompressBuffer;
	if ( fi->handle >= 0 ) {
		if ( InIn <= 0 ) {
			ADDPOS(fi->POposition,(fi->POfull-fi->PObuffer)*sizeof(WORD));
			SeekFile(fi->handle,&(fi->POposition),SEEK_SET);
/* --COMPRESS-- */
			if ( ( ( InIn = ReadFile(fi->handle,(UBYTE *)(fi->PObuffer),
				fi->POsize) ) < 0 ) || ( InIn & 1 ) ) goto GTerr;
#ifdef WORD2
			InIn >>= 1;
#else
			InIn /= TABLESIZE(WORD,UBYTE);
#endif
			AR.InInBuf = InIn;
			if ( !InIn ) { *r = 0; *from = 0; goto RegRet; }
			fi->POfill = fi->PObuffer;
			fi->POfull = fi->PObuffer + InIn;
		}
		inp = fi->POfill;
		if ( ( len = i = *inp ) == 0 ) {
			AR.InInBuf--;
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
				*(AM.CompressBuffer) = len = *start;
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
			SeekFile(fi->handle,&(fi->POposition),SEEK_SET);
/* --COMPRESS-- */
			if ( ( InIn = ReadFile(fi->handle,(UBYTE *)(fi->PObuffer),fi->POsize) )
			<= 0 || ( InIn & 1 ) ) goto GTerr;
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
				*(AM.CompressBuffer) = len = *start;
			}
			InIn -= j;
			while ( --j >= 0 ) { *r++ = *term++ = *inp++; }
		}
		fi->POfill = inp;
		AR.InInBuf = InIn;
/*
		AR.DefPosition = SeekFile(fi->handle,0L,SEEK_CUR) - InIn*sizeof(WORD);
*/
		TELLFILE(fi->handle,&AR.DefPosition);
		InIn2 = InIn*sizeof(WORD); /* Don't do in one statement. sizeof-> size_t! */
		ADDPOS(AR.DefPosition,-InIn2);
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
		if ( inp > fi->POfull ) goto GTerr;
	}
	if ( r >= AM.ComprTop ) {
		MesPrint("CompressSize of %10l is insufficient",AM.CompressSize);
		Terminate(-1);
	}
	AR.CompressPointer = r; *r = 0;
	if ( testing ) {
		WORD j;
		r = from;
		j = *r - 1 - ABS(*(r+*r-1));
		if ( j < minsiz ) goto strip;
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
		deferskipped++;
		goto ReStart;
	}
RegRet:;
/*
			#[ debug :
	{
		UBYTE OutBuf[140];
		if ( AP.DebugFlag ) {
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
		}
	}
			#] debug :
*/
	return(*from);
GTerr:
	MesPrint("Error while reading scratch file");
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
*/

WORD
GetOneTerm ARG2(WORD *,term,WORD,par)
{
	WORD i, *p;
	LONG j;
	FILEHANDLE *fi;
	WORD *r, *rr = AR.CompressPointer;
	r = rr;
	if ( AR.GetOneFile == 2 ) fi = AS.hidefile;
	else                      fi = AR.infile;
	if ( par != -2 && fi->handle >= 0 ) {
/* --COMPRESS-- */
		if ( ReadFile(fi->handle,(UBYTE *)term,(LONG)sizeof(WORD)) == sizeof(WORD) ) {
			p = term;
			j = i = *term++;
			r++;
			if ( i < 0 ) {
				*p = -i + 1;
				while ( ++i <= 0 ) *term++ = *r++;
/* --COMPRESS-- */
				if ( ReadFile(fi->handle,(UBYTE *)term,(LONG)sizeof(WORD)) !=
				sizeof(WORD) ) {
					goto ErrGet;
				}
				*p += *term;
				j = *term;
				*rr = *p;
			}
			else {
				if ( !j ) return(0);
				j--;
			}
			i = (WORD)j;
			j *= TABLESIZE(WORD,UBYTE);
/* --COMPRESS-- */
			if ( ReadFile(fi->handle,(UBYTE *)term,j) != j ) {
				goto ErrGet;
			}
			while ( --i >= 0 ) *r++ = *term++;
			if ( r >= AM.ComprTop ) {
				MesPrint("CompressSize of %10l is insufficient",AM.CompressSize);
				Terminate(-1);
			}
			AR.CompressPointer = r; *r = 0;
			return(*p);
		}
	}
	else {
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
		if ( p <= fi->POfull ) {
			if ( r >= AM.ComprTop ) {
				MesPrint("CompressSize of %10l is insufficient",AM.CompressSize);
				Terminate(-1);
			}
			AR.CompressPointer = r; *r = 0;
			return((WORD)j);
		}
	}
ErrGet:
	MesPrint("Error while reading scratch file in GetOneTerm");
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
	has a term in 'hold', so the AS.KeptInHold flag must be turned on.
*/

WORD
GetMoreTerms ARG1(WORD *,term)
{
	WORD *t, *r, *m, *h, *tstop, i, inc, same;
	WORD extra;
/*	-------------change 17-feb-2003 */
	extra = ((AM.MaxTer/sizeof(WORD))*((long)100-AC.CollectPercentage))/100;
	if ( extra < 23 ) extra = 23;
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
	while ( GetTerm(m) > 0 ) {
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
	AS.KeptInHold = 1;
	return(0);
}

/*
 		#] GetMoreTerms :
 		#[ GetMoreFromMem :

*/

WORD
GetMoreFromMem ARG2(WORD *,term,WORD **,tpoin)
{
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
		if ( ( WORDDIF(m,term) + i + extra ) > (AM.MaxTer>>1) ) {
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
	AS.KeptInHold = 1;
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

WORD
GetFromStore ARG5(WORD *,to,POSITION *,position,RENUMBER,renumber,
	WORD *,InCompState,WORD,nexpr)
{
/*!!!
static cnum=0;
  !!!*/
	LONG RetCode, num, first = 0;
	WORD *from, *m;
	STORECACHE s;
	STORECACHE snext, sold;
	WORD *r, *rr = AR.CompressPointer;
	POSITION scrpos;
	r = rr;
/*!!!
cnum++;
  !!!*/
	sold = s = (STORECACHE)(&AR.StoreCache);
	snext = s->next;
	while ( snext ) {
		sold = s;
		s = snext;
		snext = s->next;
		if ( BASEPOSITION(s->position) == -1 ) break;
		if ( ISLESSPOS(*position,s->toppos) &&
		ISGEPOS(*position,s->position) ) {	/* Hit */
			if ( AR.StoreCache != s ) {
				sold->next = s->next;
				s->next = AR.StoreCache->next;
				AR.StoreCache = s;
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
			while ( num > 0 && ISLESSPOS(*position,s->toppos) ) {
				*r++ = *m++ = *from++; ADDPOS(*position,sizeof(WORD)); num--;
			}
			if ( num > 0 ) {
InNew:
				SETBASEPOSITION(s->position,-1);
				SETBASEPOSITION(s->toppos,-1);
				SeekFile(AO.StoreData.Handle,position,SEEK_SET);
/* --COMPRESS--? */
				if ( ( RetCode =
				ReadFile(AO.StoreData.Handle,(UBYTE *)(s->buffer),AM.SizeStoreCache) )
				 < 0 ) goto PastErr;
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
	if ( AR.StoreCache ) {		/* Fill the last buffer */
		s->position = *position;
		SeekFile(AO.StoreData.Handle,position,SEEK_SET);
/* --COMPRESS--? */
		if ( ( RetCode = ReadFile(AO.StoreData.Handle,(UBYTE *)(s->buffer),AM.SizeStoreCache) )
		 < 0 ) goto PastErr;
		if ( !RetCode ) return( *to = 0 );
		s->toppos = *position;
		ADDPOS(s->toppos,RetCode);
		if ( AR.StoreCache != s ) {
			sold->next = s->next;
			s->next = AR.StoreCache->next;
			AR.StoreCache = s;
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
PastCon:
		scrpos = *position; ADDPOS(scrpos,num*wsizeof(WORD));
		if ( ISGEPOS(scrpos,s->toppos) ) {
			MesPrint("Inconsistency in storage file");
			goto PastErr;
		}
		while ( --num >= 0 ) {
			*r++ = *m++ = *from++;
		}
		goto PastEnd;
	}
/*		No caching available */
	SeekFile(AO.StoreData.Handle,position,SEEK_SET);
/* --COMPRESS--? */
	if ( ( RetCode = ReadFile(AO.StoreData.Handle,(UBYTE *)to,(LONG)sizeof(WORD)) ) !=
	sizeof(WORD) ) {
		*to = 0;
		return((WORD)RetCode);
	}
	if ( !*to ) return(0);
	m = to;
	if ( *to < 0 ) {
		num = *m++;
		*to = *r++ = (WORD)(-num + 1);
		while ( ++num <= 0 ) *m++ = *r++;
/* --COMPRESS--? */
		if ( ( RetCode = ReadFile(AO.StoreData.Handle,(UBYTE *)m,(LONG)sizeof(WORD)) ) !=
		sizeof(WORD) ) {
			MesPrint("@Error in compression of store file");
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
/* --COMPRESS--? */
	if ( num < 0 || ( RetCode = ReadFile(AO.StoreData.Handle,(UBYTE *)m,num) ) != num ) {
		MesPrint("@Error in stored expressions file at position %9p",position);
		return(-1);
	}
	NCOPY(r,m,first);
PastEnd:
	*rr = *to;
	if ( r >= AM.ComprTop ) {
		MesPrint("CompressSize of %10l is insufficient",AM.CompressSize);
		Terminate(-1);
	}
	AR.CompressPointer = r; *r = 0;
	if ( !TermRenumber(to,renumber,nexpr) ) {
		MarkDirty(to,DIRTYSYMFLAG);
		return((WORD)*to);
	}
PastErr:
	MesCall("GetFromStore");
	SETERROR(-1)
}

/*
 		#] GetFromStore :
 		#[ DetVars :			VOID DetVars(term)

	Determines which variables are used in term.

	When par = 1 we are scanning a prototype expression which involves
	completely different rules.

*/

VOID
DetVars ARG2(WORD *,term,WORD,par)
{
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
				(tt++)->flags = 0;
			} while ( --n > 0 );
		}
		if ( ( n = NumIndices ) > 0 ) {
			INDICES tt;
			tt = indices;
			do {
				(tt++)->flags = 0;
			} while ( --n > 0 );
		}
		if ( ( n = NumVectors ) > 0 ) {
			VECTORS tt;
			tt = vectors;
			do {
				(tt++)->flags = 0;
			} while ( --n > 0 );
		}
		if ( ( n = NumFunctions ) > 0 ) {
			FUNCTIONS tt;
			tt = functions;
			do {
				(tt++)->flags = 0;
			} while ( --n > 0 );
		}
		term += SUBEXPSIZE;
		while ( term < stopper ) {
			if ( *term == SYMTOSYM || *term == SYMTONUM ) {
				term += 2;
				AN.UsedSymbol[*term] = 1;
				symbols[*term].flags = 1;
			}
			else if ( *term == VECTOVEC ) {
				term += 2;
				AN.UsedVector[*term-AM.OffsetVector] = 1;
				vectors[*term-AM.OffsetVector].flags = 1;
			}
			else if ( *term == INDTOIND ) {
				term += 2;
				sym = indices[*term - AM.OffsetIndex].dimension;
				if ( sym < 0 ) AN.UsedSymbol[-sym] = 1;
				AN.UsedIndex[(*term) - AM.OffsetIndex] = 1;
				sym = indices[*term-AM.OffsetIndex].nmin4;
				if ( sym < -NMIN4SHIFT ) AN.UsedSymbol[-sym-NMIN4SHIFT] = 1;
				indices[*term-AM.OffsetIndex].flags = 1;
			}
			else if ( *term == FUNTOFUN ) {
				term += 2;
				AN.UsedFunction[*term-FUNCTION] = 1;
				functions[*term-FUNCTION].flags = 1;
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

WORD
ToStorage ARG2(EXPRESSIONS,e,POSITION *,length)
{
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
/*	AR.infile->POfull = AR.infile->POfill; */
	AR.InInBuf = 0;
	if ( AR.infile->handle >= 0 ) {
		scrpos = e->onfile;
		SeekFile(AR.infile->handle,&scrpos,SEEK_SET);
		if ( ISNOTEQUALPOS(scrpos,e->onfile) ) {
			f = AR.infile; AR.infile = AR.outfile; AR.outfile = f;
			return(MesPrint(":::Error in Scratch file"));
		}
		AR.infile->POposition = e->onfile;
		AR.infile->POfull = AR.infile->PObuffer; AR.InInBuf = 0;
	}
	else {
		AR.infile->POfill = (WORD *)((UBYTE *)(AR.infile->PObuffer)+BASEPOSITION(e->onfile));
	}
	w = AT.WorkPointer;
	AN.UsedSymbol   = w;	w += NumSymbols;
	AN.UsedVector   = w;	w += NumVectors;
	AN.UsedIndex    = w;	w += NumIndices;
	AN.UsedFunction = w;	w += NumFunctions;
	term = w;			w += AM.MaxTer;
	if ( w > AT.WorkTop ) {
		f = AR.infile; AR.infile = AR.outfile; AR.outfile = f;
		return(MesWork());
	}
	AO.wpos = (UBYTE *)w;
	AO.wlen = TOLONG(AT.WorkTop) - TOLONG(w);
	w = AN.UsedSymbol;
	i = NumSymbols + NumVectors + NumIndices + NumFunctions;
	do { *w++ = 0; } while ( --i > 0 );
	if ( GetTerm(term) > 0 ) {
		DetVars(term,1);
		if ( GetTerm(term) ) {
			do { DetVars(term,0); } while ( GetTerm(term) > 0 );
		}
	}
	f = AR.infile; AR.infile = AR.outfile; AR.outfile = f;
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
	indexent->variables = AO.StoreData.Fill;
/*	indexent->position = AO.StoreData.Fill + size; */
	StrCopy(AC.exprnames->namebuffer+e->name,(UBYTE *)(indexent->name));
	AO.wpoin = AO.wpos;
	SeekFile(AO.StoreData.Handle,&(AO.StoreData.Fill),SEEK_SET);
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
/*
	indexent->position = SeekFile(AO.StoreData.Handle,0L,SEEK_CUR);
*/
	TELLFILE(AO.StoreData.Handle,&(indexent->position));
	indexent->size = (WORD)DIFBASE(indexent->position,indexent->variables);
	if ( AR.outfile->handle >= 0 ) {
		POSITION llength;
		llength = *length;
		SeekFile(AR.outfile->handle,&(e->onfile),SEEK_SET);
		while ( ISPOSPOS(llength) ) {
			SETBASEPOSITION(scrpos,AO.wlen);
			if ( ISLESSPOS(llength,scrpos) ) size = BASEPOSITION(llength);
			else				             size = AO.wlen;
/* --COMPRESS-- */
			if ( ReadFile(AR.outfile->handle,AO.wpos,size) != size )
				return(MesPrint("Error while reading scratch file"));
/* --COMPRESS--? */
			if ( WriteFile(AO.StoreData.Handle,AO.wpos,size) != size )
				return(MesPrint("Error while writing storage file"));
			ADDPOS(llength,-size);
		}
	}
	else {
		WORD *ppp;
		ppp = (WORD *)((UBYTE *)(AR.outfile->PObuffer) + BASEPOSITION(e->onfile));
/* --COMPRESS-- */
		if ( WriteFile(AO.StoreData.Handle,(UBYTE *)ppp,BASEPOSITION(*length)) != 
		BASEPOSITION(*length) )
			return(MesPrint("Error while writing storage file"));
	}
	ADD2POS(*length,indexent->position);
	e->onfile = indexpos;
/*
	AO.StoreData.Fill = SeekFile(AO.StoreData.Handle,&(AM.zeropos),SEEK_END);
*/
	AO.StoreData.Fill = *length;
	SeekFile(AO.StoreData.Handle,&(AO.StoreData.Fill),SEEK_SET);
	scrpos = AO.StoreData.Position;
	ADDPOS(scrpos,sizeof(POSITION));
	SeekFile(AO.StoreData.Handle,&scrpos,SEEK_SET);
/* --COMPRESS-- */
	if ( WriteFile(AO.StoreData.Handle,((UBYTE *)&(AO.StoreData.Index.number))
		,(LONG)(sizeof(LONG))) != sizeof(LONG) ) goto ErrInSto;
	SeekFile(AO.StoreData.Handle,&indexpos,SEEK_SET);
/* --COMPRESS-- */
	if ( WriteFile(AO.StoreData.Handle,(UBYTE *)indexent,(LONG)(sizeof(INDEXENTRY))) !=
		sizeof(INDEXENTRY) ) goto ErrInSto;
	FlushFile(AO.StoreData.Handle);
	return(0);
ErrToSto:
	return(MesPrint("---Error while storing namelists"));
ErrInSto:
	return(MesPrint("Error in storage"));
}

/*
 		#] ToStorage :
 		#[ NextFileIndex :
*/

INDEXENTRY *
NextFileIndex ARG1(POSITION *,indexpos)
{
	if ( AO.StoreData.Handle <= 0 ) {
		if ( SetFileIndex() ) {
			MesCall("NextFileIndex");
			return(0);
		}
		AO.StoreData.Index.number = 1;
		SETBASEPOSITION(*indexpos,(sizeof(LONG)+sizeof(POSITION)));
		return(AO.StoreData.Index.expression);
	}
	while ( AO.StoreData.Index.number >= (LONG)(INFILEINDEX) ) {
		if ( ISNOTZEROPOS(AO.StoreData.Index.next) ) {
			SeekFile(AO.StoreData.Handle,&(AO.StoreData.Index.next),SEEK_SET);
			AO.StoreData.Position = AO.StoreData.Index.next;
/* --COMPRESS--? */
			if ( ReadFile(AO.StoreData.Handle,(UBYTE *)(&AO.StoreData.Index),(LONG)(sizeof(FILEINDEX))) !=
			(LONG)(sizeof(FILEINDEX)) ) goto ErrNextS;
		}
		else {
			AO.StoreData.Index.number = 0;
			SeekFile(AO.StoreData.Handle,&(AO.StoreData.Position),SEEK_SET);
/* --COMPRESS-- */
			if ( WriteFile(AO.StoreData.Handle,(UBYTE *)(&(AO.StoreData.Fill)),(LONG)(sizeof(LONG)))
			!= (LONG)(sizeof(LONG)) ) goto ErrNextS;
			PUTZERO(AO.StoreData.Index.next);
			SeekFile(AO.StoreData.Handle,&(AO.StoreData.Fill),SEEK_SET);
			AO.StoreData.Position = AO.StoreData.Fill;
/* --COMPRESS-- */
			if ( WriteFile(AO.StoreData.Handle,(UBYTE *)(&AO.StoreData.Index),(LONG)(sizeof(FILEINDEX))) !=
			(LONG)(sizeof(FILEINDEX)) ) goto ErrNextS;
			ADDPOS(AO.StoreData.Fill,sizeof(FILEINDEX));
		}
	}
	*indexpos = AO.StoreData.Position;
	ADDPOS(*indexpos,(sizeof(LONG)+sizeof(POSITION)) +
		AO.StoreData.Index.number * sizeof(INDEXENTRY));
	return(&AO.StoreData.Index.expression[(AO.StoreData.Index.number)++]);
ErrNextS:
	MesPrint("Error in storage file");
	return(0);
}

/*
 		#] NextFileIndex :
 		#[ SetFileIndex :
*/

WORD
SetFileIndex()
{
	if ( AO.StoreData.Handle < 0 ) {
		AO.StoreData.Handle = AC.StoreHandle;
		PUTZERO(AO.StoreData.Index.next);
		AO.StoreData.Index.number = 0;
		SETBASEPOSITION(AO.StoreData.Fill,sizeof(FILEINDEX));
/* --COMPRESS-- */
		if ( WriteFile(AO.StoreData.Handle,(UBYTE *)(&AO.StoreData.Index),(LONG)(sizeof(FILEINDEX))) !=
		(LONG)(sizeof(FILEINDEX)) ) return(MesPrint("Error writing storage file"));
	}
	else {
		POSITION scrpos;
		PUTZERO(scrpos);
		SeekFile(AO.StoreData.Handle,&scrpos,SEEK_SET);
/* --COMPRESS--? */
		if ( ReadFile(AO.StoreData.Handle,(UBYTE *)(&AO.StoreData.Index),(LONG)(sizeof(FILEINDEX))) !=
		(LONG)(sizeof(FILEINDEX)) ) return(MesPrint("Error reading storage file"));
	}
	PUTZERO(AO.StoreData.Position);
	return(0);
}

/*
 		#] SetFileIndex :
 		#[ VarStore :
*/

WORD
VarStore ARG4(UBYTE *,s,WORD,n,WORD,name,WORD,namesize)
{
	UBYTE *t, *u;
	if ( s ) {
		n -= sizeof(WORD);
		t = (UBYTE *)AO.wpoin;
		u = (UBYTE *)AT.WorkTop;
		while ( n && t < u ) { *t++ = *s++; n--; }
		if ( t >= u ) {
/* --COMPRESS-- */
			if ( WriteFile(AO.StoreData.Handle,AO.wpos,AO.wlen) != AO.wlen ) return(-1);
			t = AO.wpos;
			do { *t++ = *s++; n--; } while ( n && t < u );
		}
		s = AC.varnames->namebuffer + name;
		n = namesize;
		n += sizeof(void *)-1; n &= -(sizeof(void *));
		*((WORD *)t) = n;
		t += sizeof(WORD);
		while ( n > 0 && t < u ) { *t++ = *s++; n--; }
		if ( n > 0 ) {
/* --COMPRESS-- */
			if ( WriteFile(AO.StoreData.Handle,AO.wpos,AO.wlen) != AO.wlen ) return(-1);
			t = AO.wpos;
			do { *t++ = *s++; n--; } while ( n && t < u );
		}
		AO.wpoin = t;
	}
	else {
		LONG size;
		size = AO.wpoin - AO.wpos;
/* --COMPRESS-- */
		if ( WriteFile(AO.StoreData.Handle,AO.wpos,size) != size ) return(-1);
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

WORD
TermRenumber ARG3(WORD *,term,RENUMBER,renumber,WORD,nexpr)
{
	WORD *stopper;
/*!!!
WORD *memterm=term;
static long ctrap=0;
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
				if ( *term >= AM.OffsetIndex ) {
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
				else if ( *term  >= AM.OffsetIndex ) {
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
						else if ( *term  >= AM.OffsetIndex ) {
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

WORD
FindrNumber ARG2(WORD,n,VARRENUM *,v)
{
	WORD *hi,*med,*lo;
	hi = v->hi;
	lo = v->lo;
	med = v->start;
	while ( *med != n ) {
		if ( *med < n ) {
			if ( med == hi ) goto ErrFindr;
			lo = med;
			med = hi - ((hi-med)>>1);
		}
		else {
			if ( med == lo ) goto ErrFindr;
			hi = med;
			med = ((med-lo)>>1) + lo;
		}
	}
	return(WORDDIF(med,v->lo));
ErrFindr:
	return(MesPrint("Renumbering problems"));
}

/*
 		#] FindrNumber :
 		#[ FindInIndex :

		Finds an expression in the storage index if it exists.
		If found it returns a pointer to the index entry, otherwise zero.
		par = 0		Search by address
		par = 1		Search by name

		When comparing parameter fields the parameters of the expression
		to be searched are in AT.TMaddr. This includes the primary expression
		and a possible FROMBRAC information. The FROMBRAC is always last.

*/

INDEXENTRY *
FindInIndex ARG3(WORD,expr,FILEDATA *,f,WORD,par)
{
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
	if ( !par ) hand = AO.StoreData.Handle;
	else        hand = AO.SaveData.Handle;
	for(;;) {
		if ( ( i = (WORD)(f->Index.number) ) != 0 ) {
			indexpos = f->Position;
			ADDPOS(indexpos,((sizeof(LONG))+sizeof(POSITION)));
			ind = f->Index.expression;
			do {
				if ( ( !par && ISEQUALPOS(indexpos,Expressions[expr].onfile) )
				|| ( par && !StrCmp(EXPRNAME(expr),(UBYTE *)(ind->name)) ) ) {
					nomatch = 1;
					if ( par ) return(ind);
					scrpos = ind->position;
					SeekFile(hand,&scrpos,SEEK_SET);
					if ( ISNOTEQUALPOS(scrpos,ind->position) ) goto ErrGt2;
/* --COMPRESS-- */
					if ( ReadFile(hand,(UBYTE *)AT.WorkPointer,(LONG)sizeof(WORD)) != 
					sizeof(WORD) || !*AT.WorkPointer ) goto ErrGt2;
					num = *AT.WorkPointer - 1;
					num *= wsizeof(WORD);
					if ( *AT.WorkPointer < 0 ||
/* --COMPRESS-- */
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
					if ( m >= stop && m2 >= stop2 ) {
						AT.WorkPointer = stop2;

						return(ind);
					}
				}
				ind++;
				ADDPOS(indexpos,sizeof(INDEXENTRY));
			} while ( --i > 0 );
		}
		f->Position = f->Index.next;
		if ( ISEQUALPOS(f->Position,stindex) ) goto ErrGetTab;
		if ( !par ) {
			SeekFile(AO.StoreData.Handle,&(f->Position),SEEK_SET);
			if ( ISNOTEQUALPOS(f->Position,AO.StoreData.Position) ) goto ErrGt2;
		}
		else {
			SeekFile(AO.SaveData.Handle,&(f->Position),SEEK_SET);
			if ( ISNOTEQUALPOS(f->Position,AO.SaveData.Position) ) goto ErrGt2;
		}
		number = sizeof(struct FiLeInDeX);
/* --COMPRESS-- */
		if ( ReadFile(f->Handle,(UBYTE *)(&(f->Index)),number) !=
		number ) goto ErrGt2;
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
		They are allocated in the AM.WorkSpace.
		First the expression data are located. The Index is treated
		as a circularly linked buffer which is paged forwardly.
		If the indexentry is located (in ind) the two renumber tables
		have to be constructed.
		Finally the prototype has to be put in the proper buffer, so
		that wildcards can be passed. There should be a test with
		an already existing prototype that is constructed by the
		pattern matcher. This has not been put in yet.

*/

RENUMBER
GetTable ARG2(WORD,expr,POSITION *,position)
{
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
	if ( ( r = Expressions[expr].renum ) != 0 ) {
/*
		~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		Problem here is E(x1,x2)-E(x2,x1)
		Second time the new assignment is not made!
		change made 20-nov-1997 (commented out two lines and put the
		Malloc1 call inside the else{})

		*position = r->startposition;
		return(r);
*/
	}
	else {
		Expressions[expr].renum = r =
			(RENUMBER)Malloc1(sizeof(struct ReNuMbEr),"Renumber");
	}

	oldw = AT.WorkPointer + 1 + SUBEXPSIZE;
/*
	The protoype is loaded in the WorkSpace by the Index routine.
	After all it has to find an occurrence with the proper arguments.
	This sets the WorkPointer. Hence be careful now.
*/
	if ( ( ind = FindInIndex(expr,&AO.StoreData,0) ) == 0 ) return(0);

	xx = ind->nsymbols+ind->nindices+ind->nvectors+ind->nfunctions;
	if ( xx == 0 ) {
		Expressions[expr].renumlists = w = dummyrenumlist;
	}
	else {
		Expressions[expr].renumlists = w =
			(WORD *)Malloc1(sizeof(WORD)*(xx*2),"VarSpace");
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

	SeekFile(AO.StoreData.Handle,&(ind->variables),SEEK_SET);
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
/* --COMPRESS--? */
		if ( ReadFile(AO.StoreData.Handle,(UBYTE *)s,(LONG)(sizeof(struct SyMbOl)))
		!= sizeof(struct SyMbOl) ) goto ErrGt2;
		nsize = s->namesize; nsize += sizeof(void *)-1;
		nsize &= -sizeof(void *);
/* --COMPRESS--? */
		if ( ReadFile(AO.StoreData.Handle,(UBYTE *)(AT.WorkPointer),nsize)
		!= nsize ) goto ErrGt2;
		*w = s->number;
		if ( s->flags ) {
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
				if ( ( s->minpower !=
				symbols[k].minpower || s->maxpower !=
				symbols[k].maxpower ) && AC.WarnFlag ) {
					MesPrint("Warning: Conflicting power restrictions for %s",AT.WorkPointer);
				}
			}
		}
		else {
			if ( ( k = EntVar(CSYMBOL,(UBYTE *)(AT.WorkPointer),s->complex,s->minpower,
			s->maxpower) ) < 0 ) goto GetTcall;
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
/* --COMPRESS--? */
		if ( ReadFile(AO.StoreData.Handle,(UBYTE *)s,(LONG)(sizeof(struct InDeX)))
		!= sizeof(struct InDeX) ) goto ErrGt2;
		nsize = s->namesize; nsize += sizeof(void *)-1;
		nsize &= -sizeof(void *);
/* --COMPRESS--? */
		if ( ReadFile(AO.StoreData.Handle,(UBYTE *)(AT.WorkPointer),nsize)
		!= nsize ) goto ErrGt2;
		*w = s->number + AM.OffsetIndex;
		if ( s->dimension < 0 ) {	/* Relabel the dimension */
			s->dimension = -r->symnum[FindrNumber(-s->dimension,&(r->symb))];
			if ( s->nmin4 < -NMIN4SHIFT ) {	/* Relabel n-4 */
				s->nmin4 = -r->symnum[FindrNumber(-s->nmin4-NMIN4SHIFT
				,&(r->symb))]-NMIN4SHIFT;
			}
		}
		if ( s->flags ) {
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
			s->dimension,0,0) ) < 0 ) goto GetTcall;

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
/* --COMPRESS-- */
		if ( ReadFile(AO.StoreData.Handle,(UBYTE *)s,(LONG)(sizeof(struct VeCtOr)))
		!= sizeof(struct VeCtOr) ) goto ErrGt2;
		nsize = s->namesize; nsize += sizeof(void *)-1;
		nsize &= -sizeof(void *);
/* --COMPRESS-- */
		if ( ReadFile(AO.StoreData.Handle,(UBYTE *)(AT.WorkPointer),nsize)
		!= nsize ) goto ErrGt2;
		*w = s->number + AM.OffsetVector;
		if ( s->flags ) {
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
			s->complex,0,0) ) < 0 ) goto GetTcall;
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
/* --COMPRESS-- */
		if ( ReadFile(AO.StoreData.Handle,(UBYTE *)s,(LONG)(sizeof(struct FuNcTiOn)))
		!= sizeof(struct FuNcTiOn) ) goto ErrGt2;
		nsize = s->namesize; nsize += sizeof(void *)-1;
		nsize &= -sizeof(void *);
/* --COMPRESS-- */
		if ( ReadFile(AO.StoreData.Handle,(UBYTE *)(AT.WorkPointer),nsize)
		!= nsize ) goto ErrGt2;
		*w = s->number + FUNCTION;
		if ( s->flags ) {
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
			}
		}
		else {
			if ( ( k = EntVar(CFUNCTION,(UBYTE *)(AT.WorkPointer),
			s->complex,s->commute,s->spec) ) < 0 ) goto GetTcall;
			functions[k].symmetric = s->symmetric;
		}
		*(w+j) = k + FUNCTION;
		w++;
	}
	}
/*
			#] Functions :

	Now we skip the prototype. This sets the start posiion at the first term
*/
	if ( error ) { AT.WorkPointer = oldwork; return(0); }
/* --COMPRESS-- */
	if ( ReadFile(AO.StoreData.Handle,(UBYTE *)AT.WorkPointer,(LONG)sizeof(WORD)) != 
	sizeof(WORD) || !*AT.WorkPointer ) { AT.WorkPointer = oldwork; return(0); }
	num = *AT.WorkPointer - 1;
	num *= sizeof(WORD);
/* --COMPRESS-- */
	if ( *AT.WorkPointer < 0 ||
	ReadFile(AO.StoreData.Handle,(UBYTE *)(AT.WorkPointer+1),num) != num ) {
		MesPrint("@Error in stored expressions file at position %10p",*position);
		AT.WorkPointer = oldwork;
		return(0);
	}
	ADDPOS(*position,num+sizeof(WORD));
	r->startposition = *position;
	AT.WorkPointer = oldwork;
	return(r);
GetTcall:
	AT.WorkPointer = oldwork;
	MesCall("GetTable");
	return(0);
ErrGt2:
	AT.WorkPointer = oldwork;
	MesPrint("Readerror in GetTable");
	return(0);
}

/*
 		#] GetTable :
	#] StoreExpressions :
*/

