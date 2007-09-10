/*
  	#[ Includes : store.c
*/

#include "form3.h"

/*
  	#] Includes : 
	#[ StoreExpressions :
 		#[ OpenTemp :

		Opens the scratch files for the input -> output operations.

*/

WORD
OpenTemp()
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
	GETIDENTITY
	POSITION possize;
	LONG size;
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
		if ( ( size = ReadFile(f->handle,(UBYTE *)(f->PObuffer),f->POsize) ) < 0
		|| ( size & 1 ) != 0 ) {
			UNLOCK(AS.inputslock);
			MesPrint("Read error in SetScratch");
			Terminate(-1);
		}
		UNLOCK(AS.inputslock);
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
		if ( f != AR.hidefile ) AR.InInBuf = f->POfull-f->POfill;
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
	GETIDENTITY
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
					if ( ( indold = FindInIndex(number,&AR.StoreData,0) ) != 0 ) {
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
						SeekFile(AR.StoreData.Handle,&(indold->variables),SEEK_SET);
						wSize = TOLONG(AT.WorkTop) - TOLONG(AT.WorkPointer);
						scrpos = ind->length;
						ADDPOS(scrpos,DIFBASE(ind->position,ind->variables));
						ADD2POS(filesize,scrpos);
						SETBASEPOSITION(scrpos1,wSize);
						do {
							if ( ISLESSPOS(scrpos,scrpos1) ) wSize = BASEPOSITION(scrpos);
/* --COMPRESS--? */
							if ( ReadFile(AR.StoreData.Handle,(UBYTE *)AT.WorkPointer,wSize)
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
			SeekFile(AR.StoreData.Handle,&scrpos,SEEK_SET);		/* Start at the beginning */
			scrpos = AR.StoreData.Fill;			/* Number of bytes to be copied */
			SETBASEPOSITION(scrpos1,wSize);
			do {
				if ( ISLESSPOS(scrpos,scrpos1) ) wSize = BASEPOSITION(scrpos);
/* --COMPRESS--? */
				if ( ReadFile(AR.StoreData.Handle,(UBYTE *)AT.WorkPointer,wSize) != wSize ) {
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
	GETIDENTITY
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
		scrpos = AR.StoreData.Position;
		SeekFile(AR.StoreData.Handle,&scrpos,SEEK_SET);
		if ( ISNOTEQUALPOS(scrpos,AR.StoreData.Position) ) goto LoadWrt;
/* --COMPRESS-- */
		if ( WriteFile(AR.StoreData.Handle,(UBYTE *)(&(AR.StoreData.Index))
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
		AR.StoreData.Handle = -1;
		CloseFile(AC.StoreHandle);
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
	GETIDENTITY
	INDEXENTRY *newind;
	LONG wSize;
	POSITION scrpos,scrpos1;
	newind = NextFileIndex(&(Expressions[num].onfile));
	*newind = *ind;
	newind->variables = AR.StoreData.Fill;
	SeekFile(AR.StoreData.Handle,&(newind->variables),SEEK_SET);
	if ( ISNOTEQUALPOS(newind->variables,AR.StoreData.Fill) ) goto PutErrS;
	newind->position = newind->variables;
	ADDPOS(newind->position,DIFBASE(ind->position,ind->variables));
	scrpos = ind->variables;
	SeekFile(AO.SaveData.Handle,&scrpos,SEEK_SET);
	if ( ISNOTEQUALPOS(scrpos,ind->variables) ) goto PutErrS;
	wSize = TOLONG(AT.WorkTop) - TOLONG(AT.WorkPointer);
	scrpos = ind->length;
	ADDPOS(scrpos,DIFBASE(ind->position,ind->variables));
	ADD2POS(AR.StoreData.Fill,scrpos);
	SETBASEPOSITION(scrpos1,wSize);
	do {
		if ( ISLESSPOS(scrpos,scrpos1) ) wSize = BASEPOSITION(scrpos);
/* --COMPRESS--? */
		if ( ReadFile(AO.SaveData.Handle,(UBYTE *)AT.WorkPointer,wSize)
		!= wSize ) goto PutErrS;
/* --COMPRESS--? */
		if ( WriteFile(AR.StoreData.Handle,(UBYTE *)AT.WorkPointer,wSize)
		!= wSize ) goto PutErrS;
		ADDPOS(scrpos,-wSize);
	} while ( ISPOSPOS(scrpos) );
	scrpos = AR.StoreData.Position;
	SeekFile(AR.StoreData.Handle,&scrpos,SEEK_SET);
	if ( ISNOTEQUALPOS(scrpos,AR.StoreData.Position) ) goto PutErrS;
/* --COMPRESS-- */
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
 
WORD
GetTerm BARG1(WORD *,term)
{
	GETBIDENTITY
	WORD *inp, i, j = 0, len;
	LONG InIn = AR.InInBuf;
	WORD *r, *m, *mstop = 0, minsiz = 0, *bra = 0, *from;
	WORD first, *start = 0, testing = 0;
	FILEHANDLE *fi;
	AN.deferskipped = 0;
	if ( AR.GetFile == 2 ) fi = AR.hidefile;
	else                   fi = AR.infile;
	from = term;
	if ( AR.KeptInHold ) {
		r = AR.CompressBuffer;
		i = *r;
		if ( i <= 0 ) { *term = 0; goto RegRet; }
		m = term;
		NCOPY(m,r,i);
		AR.KeptInHold = 0;
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
		AR.InInBuf = InIn;
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
	if ( testing ) {
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
		if ( ( AC.PreDebug & DUMPINTERMS ) == DUMPINTERMS ) {
			LOCK(ErrorMessageLock);
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
			UNLOCK(ErrorMessageLock);
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

WORD
GetOneTerm BARG4(WORD *,term,FILEHANDLE *,fi,POSITION *,pos,int,par)
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
			if ( ( i > AM.MaxTer ) || ( -i >= AM.MaxTer ) ) {
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
				if ( ( j > AM.MaxTer ) || ( j <= 0 ) ) {
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
				LOCK(ErrorMessageLock);
				MesPrint("CompressSize of %10l is insufficient",AM.CompressSize);
				UNLOCK(ErrorMessageLock);
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
				LOCK(ErrorMessageLock);
				MesPrint("CompressSize of %10l is insufficient",AM.CompressSize);
				UNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			AR.CompressPointer = r; *r = 0;
			return((WORD)j);
		}
		error = 6;
	}
ErrGet:
	LOCK(ErrorMessageLock);
	MesPrint("Error while reading scratch file in GetOneTerm (%d)",error);
	UNLOCK(ErrorMessageLock);
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

WORD
GetMoreTerms ARG1(WORD *,term)
{
	GETIDENTITY
	WORD *t, *r, *m, *h, *tstop, i, inc, same;
	WORD extra;
	WORD retval = 0;
/*
	We use 23% as a quasi-random default value.
*/
	extra = ((AM.MaxTer/sizeof(WORD))*((long)100-AC.CollectPercentage))/100;
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

WORD
GetMoreFromMem ARG2(WORD *,term,WORD **,tpoin)
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
		if ( ( WORDDIF(m,term) + i + extra ) > (AM.MaxTer/(2*sizeof(WORD))) ) {
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

WORD
GetFromStore ARG5(WORD *,to,POSITION *,position,RENUMBER,renumber,
	WORD *,InCompState,WORD,nexpr)
{
	GETIDENTITY
	LONG RetCode, num, first = 0;
	WORD *from, *m;
	STORECACHE s;
	STORECACHE snext, sold;
	WORD *r, *rr = AR.CompressPointer;
	r = rr;
	gfs++;
	sold = s = (STORECACHE)(&AT.StoreCache);
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
			LOCK(ErrorMessageLock);
			MesPrint("@Error in compression of store file");
			UNLOCK(ErrorMessageLock);
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
		LOCK(ErrorMessageLock);
		MesPrint("@Error in stored expressions file at position %9p",position);
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	LOCK(AM.storefilelock);
	SeekFile(AR.StoreData.Handle,position,SEEK_SET);
	RetCode = ReadFile(AR.StoreData.Handle,(UBYTE *)m,num);
	SeekFile(AR.StoreData.Handle,position,SEEK_CUR);
	UNLOCK(AM.storefilelock);
	if ( RetCode != num ) {
		LOCK(ErrorMessageLock);
		MesPrint("@Error in stored expressions file at position %9p",position);
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	NCOPY(r,m,first);
PastEnd:
	*rr = *to;
	if ( r >= AR.ComprTop ) {
		LOCK(ErrorMessageLock);
		MesPrint("CompressSize of %10l is insufficient",AM.CompressSize);
		UNLOCK(ErrorMessageLock);
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
	LOCK(ErrorMessageLock);
	MesCall("GetFromStore");
	UNLOCK(ErrorMessageLock);
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
	term = w;
    w = (WORD *)(((UBYTE *)(w)) + AM.MaxTer);
	if ( w > AT.WorkTop ) {
		f = AR.infile; AR.infile = AR.outfile; AR.outfile = f;
		return(MesWork());
	}
	AO.wpos = (UBYTE *)w;
	AO.wlen = TOLONG(AT.WorkTop) - TOLONG(w);
	w = AN.UsedSymbol;
	i = NumSymbols + NumVectors + NumIndices + NumFunctions;
	do { *w++ = 0; } while ( --i > 0 );
	if ( GetTerm(BHEAD term) > 0 ) {
		DetVars(term,1);
		if ( GetTerm(BHEAD term) ) {
			do { DetVars(term,0); } while ( GetTerm(BHEAD term) > 0 );
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
	indexent->variables = AR.StoreData.Fill;
/*	indexent->position = AR.StoreData.Fill + size; */
	StrCopy(AC.exprnames->namebuffer+e->name,(UBYTE *)(indexent->name));
	AO.wpoin = AO.wpos;
	SeekFile(AR.StoreData.Handle,&(AR.StoreData.Fill),SEEK_SET);
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
	if ( e->numdummies > 0 ) {
		if ( WriteFile(AR.StoreData.Handle,(UBYTE *)(&(e->numdummies)),(LONG)sizeof(WORD)) != 
			sizeof(WORD) ) return(MesPrint("Error while writing storage file"));
		TELLFILE(AR.StoreData.Handle,&(indexent->position));
	}
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
			if ( WriteFile(AR.StoreData.Handle,AO.wpos,size) != size )
				return(MesPrint("Error while writing storage file"));
			ADDPOS(llength,-size);
		}
	}
	else {
		WORD *ppp;
		ppp = (WORD *)((UBYTE *)(AR.outfile->PObuffer) + BASEPOSITION(e->onfile));
/* --COMPRESS-- */
		if ( WriteFile(AR.StoreData.Handle,(UBYTE *)ppp,BASEPOSITION(*length)) != 
		BASEPOSITION(*length) )
			return(MesPrint("Error while writing storage file"));
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
/* --COMPRESS-- */
	if ( WriteFile(AR.StoreData.Handle,((UBYTE *)&(AR.StoreData.Index.number))
		,(LONG)(sizeof(LONG))) != sizeof(LONG) ) goto ErrInSto;
	SeekFile(AR.StoreData.Handle,&indexpos,SEEK_SET);
/* --COMPRESS-- */
	if ( WriteFile(AR.StoreData.Handle,(UBYTE *)indexent,(LONG)(sizeof(INDEXENTRY))) !=
		sizeof(INDEXENTRY) ) goto ErrInSto;
	FlushFile(AR.StoreData.Handle);
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
	GETIDENTITY
	if ( AR.StoreData.Handle <= 0 ) {
		if ( SetFileIndex() ) {
			MesCall("NextFileIndex");
			return(0);
		}
		AR.StoreData.Index.number = 1;
		SETBASEPOSITION(*indexpos,(sizeof(LONG)+sizeof(POSITION)));
		return(AR.StoreData.Index.expression);
	}
	while ( AR.StoreData.Index.number >= (LONG)(INFILEINDEX) ) {
		if ( ISNOTZEROPOS(AR.StoreData.Index.next) ) {
			SeekFile(AR.StoreData.Handle,&(AR.StoreData.Index.next),SEEK_SET);
			AR.StoreData.Position = AR.StoreData.Index.next;
/* --COMPRESS--? */
			if ( ReadFile(AR.StoreData.Handle,(UBYTE *)(&AR.StoreData.Index),(LONG)(sizeof(FILEINDEX))) !=
			(LONG)(sizeof(FILEINDEX)) ) goto ErrNextS;
		}
		else {
			AR.StoreData.Index.number = 0;
			SeekFile(AR.StoreData.Handle,&(AR.StoreData.Position),SEEK_SET);
/* --COMPRESS-- */
			if ( WriteFile(AR.StoreData.Handle,(UBYTE *)(&(AR.StoreData.Fill)),(LONG)(sizeof(LONG)))
			!= (LONG)(sizeof(LONG)) ) goto ErrNextS;
			PUTZERO(AR.StoreData.Index.next);
			SeekFile(AR.StoreData.Handle,&(AR.StoreData.Fill),SEEK_SET);
			AR.StoreData.Position = AR.StoreData.Fill;
/* --COMPRESS-- */
			if ( WriteFile(AR.StoreData.Handle,(UBYTE *)(&AR.StoreData.Index),(LONG)(sizeof(FILEINDEX))) !=
			(LONG)(sizeof(FILEINDEX)) ) goto ErrNextS;
			ADDPOS(AR.StoreData.Fill,sizeof(FILEINDEX));
		}
	}
	*indexpos = AR.StoreData.Position;
	ADDPOS(*indexpos,(sizeof(LONG)+sizeof(POSITION)) +
		AR.StoreData.Index.number * sizeof(INDEXENTRY));
	return(&AR.StoreData.Index.expression[(AR.StoreData.Index.number)++]);
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
	GETIDENTITY
	if ( AR.StoreData.Handle < 0 ) {
		AR.StoreData.Handle = AC.StoreHandle;
		PUTZERO(AR.StoreData.Index.next);
		AR.StoreData.Index.number = 0;
		SETBASEPOSITION(AR.StoreData.Fill,sizeof(FILEINDEX));
/* --COMPRESS-- */
		if ( WriteFile(AR.StoreData.Handle,(UBYTE *)(&AR.StoreData.Index),(LONG)(sizeof(FILEINDEX))) !=
		(LONG)(sizeof(FILEINDEX)) ) return(MesPrint("Error writing storage file"));
	}
	else {
		POSITION scrpos;
		PUTZERO(scrpos);
		SeekFile(AR.StoreData.Handle,&scrpos,SEEK_SET);
/* --COMPRESS--? */
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)(&AR.StoreData.Index),(LONG)(sizeof(FILEINDEX))) !=
		(LONG)(sizeof(FILEINDEX)) ) return(MesPrint("Error reading storage file"));
	}
	PUTZERO(AR.StoreData.Position);
	return(0);
}

/*
 		#] SetFileIndex : 
 		#[ VarStore :
*/

WORD
VarStore ARG4(UBYTE *,s,WORD,n,WORD,name,WORD,namesize)
{
	GETIDENTITY
	UBYTE *t, *u;
	if ( s ) {
		n -= sizeof(WORD);
		t = (UBYTE *)AO.wpoin;
		u = (UBYTE *)AT.WorkTop;
		while ( n && t < u ) { *t++ = *s++; n--; }
		if ( t >= u ) {
/* --COMPRESS-- */
			if ( WriteFile(AR.StoreData.Handle,AO.wpos,AO.wlen) != AO.wlen ) return(-1);
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
			if ( WriteFile(AR.StoreData.Handle,AO.wpos,AO.wlen) != AO.wlen ) return(-1);
			t = AO.wpos;
			do { *t++ = *s++; n--; } while ( n && t < u );
		}
		AO.wpoin = t;
	}
	else {
		LONG size;
		size = AO.wpoin - AO.wpos;
/* --COMPRESS-- */
		if ( WriteFile(AR.StoreData.Handle,AO.wpos,size) != size ) return(-1);
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

WORD
FindrNumber ARG2(WORD,n,VARRENUM *,v)
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
		par = 0		Search by address
		par = 1		Search by name

		When comparing parameter fields the parameters of the expression
		to be searched are in AT.TMaddr. This includes the primary expression
		and a possible FROMBRAC information. The FROMBRAC is always last.

*/

INDEXENTRY *
FindInIndex ARG3(WORD,expr,FILEDATA *,f,WORD,par)
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
		if ( ( i = (WORD)(f->Index.number) ) != 0 ) {
			indexpos = f->Position;
			ADDPOS(indexpos,((sizeof(LONG))+sizeof(POSITION)));
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
			SeekFile(AR.StoreData.Handle,&(f->Position),SEEK_SET);
			if ( ISNOTEQUALPOS(f->Position,AR.StoreData.Position) ) goto ErrGt2;
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
*/

RENUMBER
GetTable ARG2(WORD,expr,POSITION *,position)
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
#ifndef WITHPTHREADS

	if ( ( r = Expressions[expr].renum ) != 0 ) { }
	else {
		Expressions[expr].renum = 
		r = (RENUMBER)Malloc1(sizeof(struct ReNuMbEr),"Renumber");
	}
#else
		r = (RENUMBER)Malloc1(sizeof(struct ReNuMbEr),"Renumber");
#endif

	oldw = AT.WorkPointer + 1 + SUBEXPSIZE;
/*
	The protoype is loaded in the WorkSpace by the Index routine.
	After all it has to find an occurrence with the proper arguments.
	This sets the WorkPointer. Hence be careful now.
*/
	LOCK(AM.storefilelock);
	if ( ( ind = FindInIndex(expr,&AR.StoreData,0) ) == 0 ) {
		UNLOCK(AM.storefilelock);
		return(0);
	}

	xx = ind->nsymbols+ind->nindices+ind->nvectors+ind->nfunctions;
	if ( xx == 0 ) {
		Expressions[expr].renumlists = 
		w = AN.dummyrenumlist;
	}
	else {
#ifndef WITHPTHREADS
		Expressions[expr].renumlists = 
#endif
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
/* --COMPRESS--? */
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)s,(LONG)(sizeof(struct SyMbOl)))
		!= sizeof(struct SyMbOl) ) goto ErrGt2;
		nsize = s->namesize; nsize += sizeof(void *)-1;
		nsize &= -sizeof(void *);
/* --COMPRESS--? */
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)(AT.WorkPointer),nsize)
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
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)s,(LONG)(sizeof(struct InDeX)))
		!= sizeof(struct InDeX) ) goto ErrGt2;
		nsize = s->namesize; nsize += sizeof(void *)-1;
		nsize &= -sizeof(void *);
/* --COMPRESS--? */
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
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)s,(LONG)(sizeof(struct VeCtOr)))
		!= sizeof(struct VeCtOr) ) goto ErrGt2;
		nsize = s->namesize; nsize += sizeof(void *)-1;
		nsize &= -sizeof(void *);
/* --COMPRESS-- */
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)(AT.WorkPointer),nsize)
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
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)s,(LONG)(sizeof(struct FuNcTiOn)))
		!= sizeof(struct FuNcTiOn) ) goto ErrGt2;
		nsize = s->namesize; nsize += sizeof(void *)-1;
		nsize &= -sizeof(void *);
/* --COMPRESS-- */
		if ( ReadFile(AR.StoreData.Handle,(UBYTE *)(AT.WorkPointer),nsize)
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
		}
		else {
			Expressions[expr].numdummies = 0;
		}
	}

	SeekFile(AR.StoreData.Handle,&(ind->position),SEEK_SET);
/* --COMPRESS-- */
	if ( ReadFile(AR.StoreData.Handle,(UBYTE *)AT.WorkPointer,(LONG)sizeof(WORD)) != 
	sizeof(WORD) || !*AT.WorkPointer ) {
		UNLOCK(AM.storefilelock);
		AT.WorkPointer = oldwork;
		return(0);
	}
	num = *AT.WorkPointer - 1;
	num *= sizeof(WORD);
/* --COMPRESS-- */
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

int
CopyExpression ARG2(FILEHANDLE *,from,FILEHANDLE *,to)
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
				LOCK(ErrorMessageLock);
				MesPrint("Cannot create scratch file %s",to->name);
				UNLOCK(ErrorMessageLock);
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
					LOCK(ErrorMessageLock);
					MesPrint("Error while writing to disk. Disk full?");
					UNLOCK(ErrorMessageLock);
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
			LOCK(ErrorMessageLock);
			MesPrint("Error while writing to disk. Disk full?");
			UNLOCK(ErrorMessageLock);
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
			LOCK(ErrorMessageLock);
			MesPrint("Error while reading from disk while copying expression.");
			UNLOCK(ErrorMessageLock);
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
				LOCK(ErrorMessageLock);
				MesPrint("Cannot create scratch file %s",to->name);
				UNLOCK(ErrorMessageLock);
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
					LOCK(ErrorMessageLock);
					MesPrint("Error while writing to disk. Disk full?");
					UNLOCK(ErrorMessageLock);
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
			LOCK(ErrorMessageLock);
			MesPrint("Error while writing to disk. Disk full?");
			UNLOCK(ErrorMessageLock);
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
	#] StoreExpressions :
*/

