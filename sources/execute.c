/** @file execute.c
 * 
 *	The routines that start the execution phase of a module.
 *	It also contains the routines for placing the bracket subterm.
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
  	#[ Includes : execute.c
*/

#include "form3.h"

/*
  	#] Includes : 
	#[ DoExecute :
 		#[ CleanExpr :

		par == 1 after .store or .clear
		par == 0 after .sort
*/

WORD CleanExpr(WORD par)
{
	GETIDENTITY
	WORD j, n, i;
	POSITION length;
	EXPRESSIONS e_in, e_out, e;
	int numhid = 0;
	NAMENODE *node;
	n = NumExpressions;
	j = 0;
	e_in = e_out = Expressions;
	if ( n > 0 ) { do {
		e_in->vflags &= ~( TOBEFACTORED | TOBEUNFACTORED );
		if ( par ) {
			if ( e_in->renumlists ) {
				if ( e_in->renumlists != AN.dummyrenumlist )
						M_free(e_in->renumlists,"Renumber-lists");
				e_in->renumlists = 0;
			}
			if ( e_in->renum ) {
				M_free(e_in->renum,"Renumber"); e_in->renum = 0;
			}
		}
		if ( e_in->status == HIDDENLEXPRESSION
		|| e_in->status == HIDDENGEXPRESSION ) numhid++;
		switch ( e_in->status ) {
			case SPECTATOREXPRESSION:
			case LOCALEXPRESSION:
			case HIDDENLEXPRESSION:
				if ( par ) {
					AC.exprnames->namenode[e_in->node].type = CDELETE;
					AC.DidClean = 1;
					if ( e_in->status != HIDDENLEXPRESSION )
						ClearBracketIndex(e_in-Expressions);
					break;
				}
				/* fall through */
			case GLOBALEXPRESSION:
			case HIDDENGEXPRESSION:
				if ( par ) {
#ifdef WITHMPI
					/*
					 * Broadcast the global expression from the master to the all workers.
					 */
					if ( PF_BroadcastExpr(e_in, e_in->status == HIDDENGEXPRESSION ? AR.hidefile : AR.outfile) ) return -1;
					if ( PF.me == MASTER ) {
#endif
					e = e_in;
					i = n-1;
					while ( --i >= 0 ) {
						e++;
						if ( e_in->status == HIDDENGEXPRESSION ) {
							if ( e->status == HIDDENGEXPRESSION
							|| e->status == HIDDENLEXPRESSION ) break;
						}
						else {
							if ( e->status == GLOBALEXPRESSION
							|| e->status == LOCALEXPRESSION ) break;
						}
					}
#ifdef WITHMPI
					}
					else {
						/*
						 * On the slaves, the broadcast expression is sitting at the end of the file.
						 */
						e = e_in;
						i = -1;
					}
#endif
					if ( i >= 0 ) {
						DIFPOS(length,e->onfile,e_in->onfile);
					}
					else {
						FILEHANDLE *f = e_in->status == HIDDENGEXPRESSION ? AR.hidefile : AR.outfile;
						if ( f->handle < 0 ) {
							SETBASELENGTH(length,TOLONG(f->POfull)
							 - TOLONG(f->PObuffer)
							 - BASEPOSITION(e_in->onfile));
						}
						else {
							SeekFile(f->handle,&(f->filesize),SEEK_SET);
							DIFPOS(length,f->filesize,e_in->onfile);
						}
					}
					if ( ToStorage(e_in,&length) ) {
						return(MesCall("CleanExpr"));
					}
					e_in->status = STOREDEXPRESSION;
					if ( e_in->status != HIDDENGEXPRESSION )
						ClearBracketIndex(e_in-Expressions);
				}
				/* fall through */
			case SKIPLEXPRESSION:
			case DROPLEXPRESSION:
			case DROPHLEXPRESSION:
			case DROPGEXPRESSION:
			case DROPHGEXPRESSION:
			case STOREDEXPRESSION:
			case DROPSPECTATOREXPRESSION:
				if ( e_out != e_in ) {
					node = AC.exprnames->namenode + e_in->node;
					node->number = e_out - Expressions;

					e_out->onfile = e_in->onfile;
					e_out->size = e_in->size;
					e_out->printflag = 0;
					if ( par ) e_out->status = STOREDEXPRESSION;
					else e_out->status = e_in->status;
					e_out->name = e_in->name;
					e_out->node = e_in->node;
					e_out->renum = e_in->renum;
					e_out->renumlists = e_in->renumlists;
					e_out->counter = e_in->counter;
					e_out->hidelevel = e_in->hidelevel;
					e_out->inmem = e_in->inmem;
					e_out->bracketinfo = e_in->bracketinfo;
					e_out->newbracketinfo = e_in->newbracketinfo;
					e_out->numdummies = e_in->numdummies;
					e_out->numfactors = e_in->numfactors;
					e_out->vflags = e_in->vflags;
					e_out->sizeprototype = e_in->sizeprototype;
				}
#ifdef PARALLELCODE
				e_out->partodo = 0;
#endif
				e_out++;
				j++;
				break;
			case DROPPEDEXPRESSION:
				break;
			default:
				AC.exprnames->namenode[e_in->node].type = CDELETE;
				AC.DidClean = 1;
				break;
		}
		e_in++;
	} while ( --n > 0 ); }
	UpdateMaxSize();
	NumExpressions = j;
	if ( numhid == 0 && AR.hidefile->PObuffer ) {
		if ( AR.hidefile->handle >= 0 ) {
			CloseFile(AR.hidefile->handle);
			remove(AR.hidefile->name);
			AR.hidefile->handle = -1;
		}
		AR.hidefile->POfull =
		AR.hidefile->POfill = AR.hidefile->PObuffer;
		PUTZERO(AR.hidefile->POposition);
	}
	FlushSpectators();
	return(0);
}

/*
 		#] CleanExpr : 
 		#[ PopVariables :

	Pops the local variables from the tables.
	The Expressions are reprocessed and their tables are compactified.

*/

WORD PopVariables()
{
	GETIDENTITY
	WORD i, j, retval;
	UBYTE *s;

	retval = CleanExpr(1);
	ResetVariables(1);

	if ( AC.DidClean ) CompactifyTree(AC.exprnames,EXPRNAMES);

	AC.CodesFlag = AM.gCodesFlag;
	AC.NamesFlag = AM.gNamesFlag;
	AC.StatsFlag = AM.gStatsFlag;
	AC.OldFactArgFlag = AM.gOldFactArgFlag;
	AC.TokensWriteFlag = AM.gTokensWriteFlag;
	AC.extrasymbols = AM.gextrasymbols;
	if ( AC.extrasym ) { M_free(AC.extrasym,"extrasym"); AC.extrasym = 0; }
	i = 1; s = AM.gextrasym; while ( *s ) { s++; i++; }
	AC.extrasym = (UBYTE *)Malloc1(i*sizeof(UBYTE),"extrasym");
	for ( j = 0; j < i; j++ ) AC.extrasym[j] = AM.gextrasym[j];
	AO.NoSpacesInNumbers = AM.gNoSpacesInNumbers;
	AO.IndentSpace = AM.gIndentSpace;
	AC.lUnitTrace = AM.gUnitTrace;
	AC.lDefDim = AM.gDefDim;
	AC.lDefDim4 = AM.gDefDim4;
	if ( AC.halfmod ) {
		if ( AC.ncmod == AM.gncmod && AC.modmode == AM.gmodmode ) {
			j = ABS(AC.ncmod);
			while ( --j >= 0 ) {
				if ( AC.cmod[j] != AM.gcmod[j] ) break;
			}
			if ( j >= 0 ) {
				M_free(AC.halfmod,"halfmod");
				AC.halfmod = 0; AC.nhalfmod = 0;
			}
		}
		else {
			M_free(AC.halfmod,"halfmod");
			AC.halfmod = 0; AC.nhalfmod = 0;
		}
	}
	if ( AC.modinverses ) {
		if ( AC.ncmod == AM.gncmod && AC.modmode == AM.gmodmode ) {
			j = ABS(AC.ncmod);
			while ( --j >= 0 ) {
				if ( AC.cmod[j] != AM.gcmod[j] ) break;
			}
			if ( j >= 0 ) {
				M_free(AC.modinverses,"modinverses");
				AC.modinverses = 0;
			}
		}
		else {
			M_free(AC.modinverses,"modinverses");
			AC.modinverses = 0;
		}
	}
	AN.ncmod = AC.ncmod = AM.gncmod;
	AC.npowmod = AM.gnpowmod;
	AC.modmode = AM.gmodmode;
	if ( ( ( AC.modmode & INVERSETABLE ) != 0 ) && ( AC.modinverses == 0 ) )
			MakeInverses();
	AC.funpowers = AM.gfunpowers;
	AC.lPolyFun = AM.gPolyFun;
	AC.lPolyFunInv = AM.gPolyFunInv;
	AC.lPolyFunType = AM.gPolyFunType;
	AC.lPolyFunExp = AM.gPolyFunExp;
	AR.PolyFunVar = AC.lPolyFunVar = AM.gPolyFunVar;
	AC.lPolyFunPow = AM.gPolyFunPow;
	AC.parallelflag = AM.gparallelflag;
	AC.ProcessBucketSize = AC.mProcessBucketSize = AM.gProcessBucketSize;
	AC.properorderflag = AM.gproperorderflag;
    AC.ThreadBucketSize = AM.gThreadBucketSize;
	AC.ThreadStats = AM.gThreadStats;
	AC.FinalStats = AM.gFinalStats;
	AC.OldGCDflag = AM.gOldGCDflag;
	AC.WTimeStatsFlag = AM.gWTimeStatsFlag;
	AC.ThreadsFlag = AM.gThreadsFlag;
	AC.ThreadBalancing = AM.gThreadBalancing;
	AC.ThreadSortFileSynch = AM.gThreadSortFileSynch;
	AC.ProcessStats = AM.gProcessStats;
	AC.OldParallelStats = AM.gOldParallelStats;
	AC.IsFortran90 = AM.gIsFortran90;
	AC.SizeCommuteInSet = AM.gSizeCommuteInSet;
	PruneExtraSymbols(AM.gnumextrasym);

	if ( AC.Fortran90Kind ) {
		M_free(AC.Fortran90Kind,"Fortran90 Kind");
		AC.Fortran90Kind = 0;
	}
	if ( AM.gFortran90Kind ) {
		AC.Fortran90Kind = strDup1(AM.gFortran90Kind,"Fortran90 Kind");
	}
	if ( AC.ThreadsFlag && AM.totalnumberofthreads > 1 ) AS.MultiThreaded = 1;
	{
		UWORD *p, *m;
		p = AM.gcmod;
		m = AC.cmod;
		j = ABS(AC.ncmod);
		NCOPY(m,p,j);
		p = AM.gpowmod;
		m = AC.powmod;
		j = AC.npowmod;
		NCOPY(m,p,j);
		if ( AC.DirtPow ) {
			if ( MakeModTable() ) {
				MesPrint("===No printing in powers of generator");
			}
			AC.DirtPow = 0;
		}
	}
	{
		WORD *p, *m;
		p = AM.gUniTrace;
		m = AC.lUniTrace;
		j = 4;
		NCOPY(m,p,j);
	}
	AC.Cnumpows = AM.gCnumpows;
	AC.OutputMode = AM.gOutputMode;
	AC.OutputSpaces = AM.gOutputSpaces;
	AC.OutNumberType = AM.gOutNumberType;
	AR.SortType = AC.SortType = AM.gSortType;
	AC.ShortStatsMax = AM.gShortStatsMax;
/*
	Now we have to clean up the commutation properties
*/
	for ( i = 0; i < NumFunctions; i++ ) functions[i].flags &= ~COULDCOMMUTE;
	if ( AC.CommuteInSet ) {
		WORD *g, *gg;
		g = AC.CommuteInSet;
		while ( *g ) {
			gg = g+1; g += *g;
			while ( gg < g ) {
				if ( *gg <= GAMMASEVEN && *gg >= GAMMA ) {
					functions[GAMMA-FUNCTION].flags |= COULDCOMMUTE;
					functions[GAMMAI-FUNCTION].flags |= COULDCOMMUTE;
					functions[GAMMAFIVE-FUNCTION].flags |= COULDCOMMUTE;
					functions[GAMMASIX-FUNCTION].flags |= COULDCOMMUTE;
					functions[GAMMASEVEN-FUNCTION].flags |= COULDCOMMUTE;
				}
				else {
					functions[*gg-FUNCTION].flags |= COULDCOMMUTE;
				}
			}
		}
	}
/*
	Clean up the dictionaries.
*/
	for ( i = AO.NumDictionaries-1; i >= AO.gNumDictionaries; i-- ) {
		RemoveDictionary(AO.Dictionaries[i]);
		M_free(AO.Dictionaries[i],"Dictionary");
	}
	for( ; i >= 0; i-- ) {
		ShrinkDictionary(AO.Dictionaries[i]);
	}
	AO.NumDictionaries = AO.gNumDictionaries;
	return(retval);
}

/*
 		#] PopVariables : 
 		#[ MakeGlobal :
*/

VOID MakeGlobal()
{
	WORD i, j, *pp, *mm;
	UWORD *p, *m;
	UBYTE *s;
	Globalize(0);

	AM.gCodesFlag = AC.CodesFlag;
	AM.gNamesFlag = AC.NamesFlag;
	AM.gStatsFlag = AC.StatsFlag;
    AM.gOldFactArgFlag = AC.OldFactArgFlag;
	AM.gextrasymbols = AC.extrasymbols;
	if ( AM.gextrasym ) { M_free(AM.gextrasym,"extrasym"); AM.gextrasym = 0; }
	i = 1; s = AC.extrasym; while ( *s ) { s++; i++; }
	AM.gextrasym = (UBYTE *)Malloc1(i*sizeof(UBYTE),"extrasym");
	for ( j = 0; j < i; j++ ) AM.gextrasym[j] = AC.extrasym[j];
	AM.gTokensWriteFlag= AC.TokensWriteFlag;
	AM.gNoSpacesInNumbers = AO.NoSpacesInNumbers;
	AM.gIndentSpace = AO.IndentSpace;
	AM.gUnitTrace = AC.lUnitTrace;
	AM.gDefDim = AC.lDefDim;
	AM.gDefDim4 = AC.lDefDim4;
	AM.gncmod = AC.ncmod;
	AM.gnpowmod = AC.npowmod;
	AM.gmodmode = AC.modmode;
	AM.gCnumpows = AC.Cnumpows;
	AM.gOutputMode = AC.OutputMode;
	AM.gOutputSpaces = AC.OutputSpaces;
	AM.gOutNumberType = AC.OutNumberType;
	AM.gfunpowers = AC.funpowers;
	AM.gPolyFun = AC.lPolyFun;
	AM.gPolyFunInv = AC.lPolyFunInv;
	AM.gPolyFunType = AC.lPolyFunType;
	AM.gPolyFunExp = AC.lPolyFunExp;
	AM.gPolyFunVar = AC.lPolyFunVar;
	AM.gPolyFunPow = AC.lPolyFunPow;
	AM.gparallelflag = AC.parallelflag;
	AM.gProcessBucketSize = AC.ProcessBucketSize;
	AM.gproperorderflag = AC.properorderflag;
    AM.gThreadBucketSize = AC.ThreadBucketSize;
	AM.gThreadStats = AC.ThreadStats;
	AM.gFinalStats = AC.FinalStats;
	AM.gOldGCDflag = AC.OldGCDflag;
	AM.gWTimeStatsFlag = AC.WTimeStatsFlag;
	AM.gThreadsFlag = AC.ThreadsFlag;
	AM.gThreadBalancing = AC.ThreadBalancing;
	AM.gThreadSortFileSynch = AC.ThreadSortFileSynch;
	AM.gProcessStats = AC.ProcessStats;
	AM.gOldParallelStats = AC.OldParallelStats;
	AM.gIsFortran90 = AC.IsFortran90;
	AM.gSizeCommuteInSet = AC.SizeCommuteInSet;
	AM.gnumextrasym = (cbuf+AM.sbufnum)->numrhs;
	if ( AM.gFortran90Kind ) {
		M_free(AM.gFortran90Kind,"Fortran 90 Kind");
		AM.gFortran90Kind = 0;
	}
	if ( AC.Fortran90Kind ) {
		AM.gFortran90Kind = strDup1(AC.Fortran90Kind,"Fortran 90 Kind");
	}
	p = AM.gcmod;
	m = AC.cmod;
	i = ABS(AC.ncmod);
	NCOPY(p,m,i);
	p = AM.gpowmod;
	m = AC.powmod;
	i = AC.npowmod;
	NCOPY(p,m,i);
	pp = AM.gUniTrace;
	mm = AC.lUniTrace;
	i = 4;
	NCOPY(pp,mm,i);
	AM.gSortType = AC.SortType;
	AM.gShortStatsMax = AC.ShortStatsMax;

	if ( AO.CurrentDictionary > 0 || AP.OpenDictionary > 0 ) {
		Warning("You cannot have an open or selected dictionary at a .global. Dictionary closed.");
		AP.OpenDictionary = 0;
		AO.CurrentDictionary = 0;
	}

	AO.gNumDictionaries = AO.NumDictionaries;
	for ( i = 0; i < AO.NumDictionaries; i++ ) {
		AO.Dictionaries[i]->gnumelements = AO.Dictionaries[i]->numelements;
	}
	if ( AM.NumSpectatorFiles > 0 ) {
		for ( i = 0; i < AM.SizeForSpectatorFiles; i++ ) {
			if ( AM.SpectatorFiles[i].name != 0 )
					AM.SpectatorFiles[i].flags |= GLOBALSPECTATORFLAG;
		}
	}
}

/*
 		#] MakeGlobal : 
 		#[ TestDrop :
*/

VOID TestDrop()
{
	EXPRESSIONS e;
	WORD j;
	for ( j = 0, e = Expressions; j < NumExpressions; j++, e++ ) {
		switch ( e->status ) {
			case SKIPLEXPRESSION:
				e->status = LOCALEXPRESSION;
				break;
			case UNHIDELEXPRESSION:
				e->status = LOCALEXPRESSION;
				ClearBracketIndex(j);
				e->bracketinfo = e->newbracketinfo; e->newbracketinfo = 0;
				break;
			case HIDELEXPRESSION:
				e->status = HIDDENLEXPRESSION;
				break;
			case SKIPGEXPRESSION:
				e->status = GLOBALEXPRESSION;
				break;
			case UNHIDEGEXPRESSION:
				e->status = GLOBALEXPRESSION;
				ClearBracketIndex(j);
				e->bracketinfo = e->newbracketinfo; e->newbracketinfo = 0;
				break;
			case HIDEGEXPRESSION:
				e->status = HIDDENGEXPRESSION;
				break;
			case DROPLEXPRESSION:
			case DROPGEXPRESSION:
			case DROPHLEXPRESSION:
			case DROPHGEXPRESSION:
			case DROPSPECTATOREXPRESSION:
				e->status = DROPPEDEXPRESSION;
				ClearBracketIndex(j);
				e->bracketinfo = e->newbracketinfo; e->newbracketinfo = 0;
				if ( e->replace >= 0 ) {
					Expressions[e->replace].replace = REGULAREXPRESSION;
					AC.exprnames->namenode[e->node].number = e->replace;
					e->replace = REGULAREXPRESSION;
				}
				else {
					AC.exprnames->namenode[e->node].type = CDELETE;
					AC.DidClean = 1;
				}
				break;
			case LOCALEXPRESSION:
			case GLOBALEXPRESSION:
				ClearBracketIndex(j);
				e->bracketinfo = e->newbracketinfo; e->newbracketinfo = 0;
				break;
			case HIDDENLEXPRESSION:
			case HIDDENGEXPRESSION:
				break;
			case INTOHIDELEXPRESSION:
				ClearBracketIndex(j);
				e->bracketinfo = e->newbracketinfo; e->newbracketinfo = 0;
				e->status = HIDDENLEXPRESSION;
				break;
			case INTOHIDEGEXPRESSION:
				ClearBracketIndex(j);
				e->bracketinfo = e->newbracketinfo; e->newbracketinfo = 0;
				e->status = HIDDENGEXPRESSION;
				break;
			default:
				ClearBracketIndex(j);
				e->bracketinfo = 0;
				break;
		}
		if ( e->replace == NEWLYDEFINEDEXPRESSION ) e->replace = REGULAREXPRESSION;
	}
}

/*
 		#] TestDrop : 
 		#[ PutInVflags :
*/

void PutInVflags(WORD nexpr)
{
	EXPRESSIONS e = Expressions + nexpr;
	POSITION *old;
	WORD *oldw;
	int i;
restart:;
	if ( AS.OldOnFile == 0 ) {
		AS.NumOldOnFile = 20;
		AS.OldOnFile = (POSITION *)Malloc1(AS.NumOldOnFile*sizeof(POSITION),"file pointers");
	}
	else if ( nexpr >= AS.NumOldOnFile ) {
		old = AS.OldOnFile;
		AS.OldOnFile = (POSITION *)Malloc1(2*AS.NumOldOnFile*sizeof(POSITION),"file pointers");
		for ( i = 0; i < AS.NumOldOnFile; i++ ) AS.OldOnFile[i] = old[i];
		AS.NumOldOnFile = 2*AS.NumOldOnFile;
		M_free(old,"process file pointers");
	}
	if ( AS.OldNumFactors == 0 ) {
		AS.NumOldNumFactors = 20;
		AS.OldNumFactors = (WORD *)Malloc1(AS.NumOldNumFactors*sizeof(WORD),"numfactors pointers");
		AS.Oldvflags = (WORD *)Malloc1(AS.NumOldNumFactors*sizeof(WORD),"vflags pointers");
	}
	else if ( nexpr >= AS.NumOldNumFactors ) {
		oldw = AS.OldNumFactors;
		AS.OldNumFactors = (WORD *)Malloc1(2*AS.NumOldNumFactors*sizeof(WORD),"numfactors pointers");
		for ( i = 0; i < AS.NumOldNumFactors; i++ ) AS.OldNumFactors[i] = oldw[i];
		M_free(oldw,"numfactors pointers");
		oldw = AS.Oldvflags;
		AS.Oldvflags = (WORD *)Malloc1(2*AS.NumOldNumFactors*sizeof(WORD),"vflags pointers");
		for ( i = 0; i < AS.NumOldNumFactors; i++ ) AS.Oldvflags[i] = oldw[i];
		AS.NumOldNumFactors = 2*AS.NumOldNumFactors;
		M_free(oldw,"vflags pointers");
	}
/*
	The next is needed when we Load a .sav file with lots of expressions.
*/
	if ( nexpr >= AS.NumOldOnFile || nexpr >= AS.NumOldNumFactors ) goto restart;
	AS.OldOnFile[nexpr] = e->onfile;
	AS.OldNumFactors[nexpr] = e->numfactors;
	AS.Oldvflags[nexpr] = e->vflags;
}

/*
 		#] PutInVflags : 
 		#[ DoExecute :
*/

WORD DoExecute(WORD par, WORD skip)
{
	GETIDENTITY
	WORD RetCode = 0;
	int i, oldmultithreaded = AS.MultiThreaded;
#ifdef PARALLELCODE
	int j;
#endif

	SpecialCleanup(BHEAD0);
	if ( skip ) goto skipexec;
	if ( AC.IfLevel > 0 ) {
		MesPrint(" %d endif statement(s) missing",AC.IfLevel);
		RetCode = 1;
	}
	if ( AC.WhileLevel > 0 ) {
		MesPrint(" %d endwhile statement(s) missing",AC.WhileLevel);
		RetCode = 1;
	}
	if ( AC.arglevel > 0 ) {
		MesPrint(" %d endargument statement(s) missing",AC.arglevel);
		RetCode = 1;
	}
	if ( AC.termlevel > 0 ) {
		MesPrint(" %d endterm statement(s) missing",AC.termlevel);
		RetCode = 1;
	}
	if ( AC.insidelevel > 0 ) {
		MesPrint(" %d endinside statement(s) missing",AC.insidelevel);
		RetCode = 1;
	}
	if ( AC.inexprlevel > 0 ) {
		MesPrint(" %d endinexpression statement(s) missing",AC.inexprlevel);
		RetCode = 1;
	}
	if ( AC.NumLabels > 0 ) {
		for ( i = 0; i < AC.NumLabels; i++ ) {
			if ( AC.Labels[i] < 0 ) {
				MesPrint(" -->Label %s missing",AC.LabelNames[i]);
				RetCode = 1;
			}
		}
	}
	if ( AC.SwitchLevel > 0 ) {
		MesPrint(" %d endswitch statement(s) missing",AC.SwitchLevel);
		RetCode = 1;
	}
	if ( AC.dolooplevel > 0 ) {
		MesPrint(" %d enddo statement(s) missing",AC.dolooplevel);
		RetCode = 1;
	}
	if ( AP.OpenDictionary > 0 ) {
		MesPrint(" Dictionary %s has not been closed.",
			AO.Dictionaries[AP.OpenDictionary-1]->name);
		AP.OpenDictionary = 0;
		RetCode = 1;
	}
	if ( RetCode ) return(RetCode);
	AR.Cnumlhs = cbuf[AM.rbufnum].numlhs;

	if ( ( AS.ExecMode = par ) == GLOBALMODULE ) AS.ExecMode = 0;
#ifdef PARALLELCODE
/*
		Now check whether we have either the regular parallel flag or the
		mparallel flag set.
		Next check whether any of the expressions has partodo set.
		If any of the above we need to check what the dollar status is.
*/
	AC.partodoflag = -1;
	if ( NumPotModdollars >= 0 ) {
		for ( i = 0; i < NumExpressions; i++ ) {
			if ( Expressions[i].partodo ) { AC.partodoflag = 1; break; }
		}
	}
#ifdef WITHMPI
	if ( AC.partodoflag > 0 && PF.numtasks < 3 ) {
		AC.partodoflag = 0;
	}
#endif
	if ( AC.partodoflag > 0 || ( NumPotModdollars > 0 && AC.mparallelflag == PARALLELFLAG ) ) {
	  if ( NumPotModdollars > NumModOptdollars ) {
		AC.mparallelflag |= NOPARALLEL_DOLLAR;
#ifdef WITHPTHREADS
		AS.MultiThreaded = 0;
#endif
		AC.partodoflag = 0;
	  }
	  else {
		for ( i = 0; i < NumPotModdollars; i++ ) {
		  for ( j = 0; j < NumModOptdollars; j++ ) 
			if ( PotModdollars[i] == ModOptdollars[j].number ) break;
		  if ( j >= NumModOptdollars ) {
			AC.mparallelflag |= NOPARALLEL_DOLLAR;
#ifdef WITHPTHREADS
			AS.MultiThreaded = 0;
#endif
			AC.partodoflag = 0;
			break;
		  }
		  switch ( ModOptdollars[j].type ) {
			case MODSUM:
			case MODMAX:
			case MODMIN:
			case MODLOCAL:
				break;
			default:
				AC.mparallelflag |= NOPARALLEL_DOLLAR;
				AS.MultiThreaded = 0;
				AC.partodoflag = 0;
				break;
		  }
		}
	  }
	}
	else if ( ( AC.mparallelflag & NOPARALLEL_USER ) != 0 ) {
#ifdef WITHPTHREADS
		AS.MultiThreaded = 0;
#endif
		AC.partodoflag = 0;
	}
	if ( AC.partodoflag == 0 ) {
		for ( i = 0; i < NumExpressions; i++ ) {
			Expressions[i].partodo = 0;
		}
	}
	else if ( AC.partodoflag == -1 ) {
		AC.partodoflag = 0;
	}
#endif
#ifdef WITHMPI
	/*
	 * Check RHS expressions.
	 */
	if ( AC.RhsExprInModuleFlag && (AC.mparallelflag == PARALLELFLAG || AC.partodoflag) ) {
		if (PF.rhsInParallel) {
			PF.mkSlaveInfile=1;
			if(PF.me != MASTER){
				PF.slavebuf.PObuffer=(WORD *)Malloc1(AM.ScratSize*sizeof(WORD),"PF inbuf");
				PF.slavebuf.POsize=AM.ScratSize*sizeof(WORD);
				PF.slavebuf.POfull = PF.slavebuf.POfill = PF.slavebuf.PObuffer;
				PF.slavebuf.POstop= PF.slavebuf.PObuffer+AM.ScratSize;
				PUTZERO(PF.slavebuf.POposition);
			}/*if(PF.me != MASTER)*/
		} 
		else {
			AC.mparallelflag |= NOPARALLEL_RHS;
			AC.partodoflag = 0;
			for ( i = 0; i < NumExpressions; i++ ) {
				Expressions[i].partodo = 0;
			}
		}
	}
	/*
	 * Set $-variables with MODSUM to zero on the slaves.
	 */
	if ( (AC.mparallelflag == PARALLELFLAG || AC.partodoflag) && PF.me != MASTER ) {
		for ( i = 0; i < NumModOptdollars; i++ ) {
			if ( ModOptdollars[i].type == MODSUM ) {
				DOLLARS d = Dollars + ModOptdollars[i].number;
				d->type = DOLZERO;
				if ( d->where && d->where != &AM.dollarzero ) M_free(d->where, "old content of dollar");
				d->where = &AM.dollarzero;
				d->size = 0;
				CleanDollarFactors(d);
			}
		}
	}
#endif
	AR.SortType = AC.SortType;
#ifdef WITHMPI
	if ( PF.me == MASTER )
#endif
	{
		if ( AC.SetupFlag ) WriteSetup();
		if ( AC.NamesFlag || AC.CodesFlag ) WriteLists();
	}
	if ( par == GLOBALMODULE ) MakeGlobal();
	if ( RevertScratch() ) return(-1);
	if ( AC.ncmod ) SetMods();
/*
	Warn if the module has to run in sequential mode due to some problems.
*/
#ifdef WITHMPI
	if ( PF.me == MASTER )
#endif
	{
		if ( !AC.ThreadsFlag || AC.mparallelflag & NOPARALLEL_USER ) {
			/* The user switched off the parallel execution explicitly. */
		}
		else if ( AC.mparallelflag & NOPARALLEL_DOLLAR ) {
			if ( AC.WarnFlag >= 2 ) {  /* HighWarning */
				int i, j, k, n;
				UBYTE *s, *s1;
				s = strDup1((UBYTE *)"","NOPARALLEL_DOLLAR s");
				n = 0;
				j = NumPotModdollars;
				for ( i = 0; i < j; i++ ) {
					for ( k = 0; k < NumModOptdollars; k++ )
						if ( ModOptdollars[k].number == PotModdollars[i] ) break;
					if ( k >= NumModOptdollars ) {
						/* global $-variable */
						if ( n > 0 )
							s = AddToString(s,(UBYTE *)", ",0);
						s = AddToString(s,(UBYTE *)"$",0);
						s = AddToString(s,DOLLARNAME(Dollars,PotModdollars[i]),0);
						n++;
					}
				}
				s1 = strDup1((UBYTE *)"This module is forced to run in sequential mode due to $-variable","NOPARALLEL_DOLLAR s1");
				if ( n != 1 )
					s1 = AddToString(s1,(UBYTE *)"s",0);
				s1 = AddToString(s1,(UBYTE *)": ",0);
				s1 = AddToString(s1,s,0);
				HighWarning((char *)s1);
				M_free(s,"NOPARALLEL_DOLLAR s");
				M_free(s1,"NOPARALLEL_DOLLAR s1");
			}
		}
		else if ( AC.mparallelflag & NOPARALLEL_RHS ) {
			HighWarning("This module is forced to run in sequential mode due to RHS expression names");
		}
		else if ( AC.mparallelflag & NOPARALLEL_CONVPOLY ) {
			HighWarning("This module is forced to run in sequential mode due to conversion to extra symbols");
		}
		else if ( AC.mparallelflag & NOPARALLEL_SPECTATOR ) {
			HighWarning("This module is forced to run in sequential mode due to tospectator/copyspectator");
		}
		else if ( AC.mparallelflag & NOPARALLEL_TBLDOLLAR ) {
			HighWarning("This module is forced to run in sequential mode due to $-variable assignments in tables");
		}
		else if ( AC.mparallelflag & NOPARALLEL_NPROC ) {
			HighWarning("This module is forced to run in sequential mode because there is only one processor");
		}
	}
/*
	Now the actual execution
*/
#ifdef WITHMPI
	/*
	 * Turn on AS.printflag to print runtime errors occurring on slaves.
	 */
	AS.printflag = 1;
#endif
	if ( AP.preError == 0 && ( Processor() || WriteAll() ) ) RetCode = -1;
#ifdef WITHMPI
	AS.printflag = 0;
#endif
/*
	That was it. Next is cleanup.
*/
	if ( AC.ncmod ) UnSetMods();
	AS.MultiThreaded = oldmultithreaded;
	TableReset();

/*[28sep2005 mt]:*/
#ifdef WITHMPI
	/* Combine and then broadcast modified dollar variables. */
	if ( NumPotModdollars > 0 ) {
		RetCode = PF_CollectModifiedDollars();
		if ( RetCode ) return RetCode;
		RetCode = PF_BroadcastModifiedDollars();
		if ( RetCode ) return RetCode;
	}
	/* Broadcast redefined preprocessor variables. */
	if ( AC.numpfirstnum > 0 ) {
		RetCode = PF_BroadcastRedefinedPreVars();
		if ( RetCode ) return RetCode;
	}
	/* Broadcast the list of objects converted to symbols in AM.sbufnum. */
	if ( AC.topolynomialflag & TOPOLYNOMIALFLAG ) {
		RetCode = PF_BroadcastCBuf(AM.sbufnum);
		if ( RetCode ) return RetCode;
	}
	/*
	 * Broadcast AR.expflags, which may be used on the slaves in the next module
	 * via ZERO_ or UNCHANGED_. It also broadcasts several flags of each expression.
	 */
	RetCode = PF_BroadcastExpFlags();
	if ( RetCode ) return RetCode;
	/*
	 * Clean the hide file on the slaves, which was used for RHS expressions
	 * broadcast from the master at the beginning of the module.
	 */
	if ( PF.me != MASTER && AR.hidefile->PObuffer ) {
		if ( AR.hidefile->handle >= 0 ) {
			CloseFile(AR.hidefile->handle);
			AR.hidefile->handle = -1;
			remove(AR.hidefile->name);
		}
		AR.hidefile->POfull = AR.hidefile->POfill = AR.hidefile->PObuffer;
		PUTZERO(AR.hidefile->POposition);
	}
#endif
#ifdef WITHPTHREADS
	for ( j = 0; j < NumModOptdollars; j++ ) {
		if ( ModOptdollars[j].dstruct ) {
/*
			First clean up dollar values.
*/
			for ( i = 0; i < AM.totalnumberofthreads; i++ ) {
				if ( ModOptdollars[j].dstruct[i].size > 0 ) {
					CleanDollarFactors(&(ModOptdollars[j].dstruct[i]));
					M_free(ModOptdollars[j].dstruct[i].where,"Local dollar value");
				}
			}
/*
			Now clean up the whole array.
*/
			M_free(ModOptdollars[j].dstruct,"Local DOLLARS");
			ModOptdollars[j].dstruct = 0;
		}
	}
#endif
/*:[28sep2005 mt]*/

/*
	@@@@@@@@@@@@@@@
	Now follows the code to invalidate caches for all objects in the
	PotModdollars. There are NumPotModdollars of them and PotModdollars
	is an array of WORD.
*/
/*
	Cleanup:
*/
#ifdef JV_IS_WRONG
/*
	Giving back this memory gives way too much activity with Malloc1
	Better to keep it and just put the number of used objects to zero (JV)
	If you put the lijst equal to NULL, please also make maxnum = 0
*/
	if ( ModOptdollars ) M_free(ModOptdollars, "ModOptdollars pointer");
	if ( PotModdollars ) M_free(PotModdollars, "PotModdollars pointer");
	
	/* ModOptdollars changed to AC.ModOptDolList.lijst because AIX C compiler complained. MF 30/07/2003. */
	AC.ModOptDolList.lijst = NULL;
	/* PotModdollars changed to AC.PotModDolList.lijst because AIX C compiler complained. MF 30/07/2003. */
	AC.PotModDolList.lijst = NULL;
#endif
	NumPotModdollars = 0;
	NumModOptdollars = 0;

skipexec:
/*
	Clean up the switch information.
	We keep the switch array and heap.
*/
if ( AC.SwitchInArray > 0 ) {
	for ( i = 0; i < AC.SwitchInArray; i++ ) {
		SWITCH *sw = AC.SwitchArray + i;
		if ( sw->table ) M_free(sw->table,"Switch table");
		sw->table = 0;
		sw->defaultcase.ncase = 0;
		sw->defaultcase.value = 0;
		sw->defaultcase.compbuffer = 0;
		sw->endswitch.ncase = 0;
		sw->endswitch.value = 0;
		sw->endswitch.compbuffer = 0;
		sw->typetable = 0;
		sw->maxcase = 0;
		sw->mincase = 0;
		sw->numcases = 0;
		sw->tablesize = 0;
		sw->caseoffset = 0;
		sw->iflevel = 0;
		sw->whilelevel = 0;
		sw->nestingsum = 0;
	}
	AC.SwitchInArray = 0;
	AC.SwitchLevel = 0;
}
#ifdef PARALLELCODE
	AC.numpfirstnum = 0;
#endif
	AC.DidClean = 0;
	AC.PolyRatFunChanged = 0;
	TestDrop();
	if ( par == STOREMODULE || par == CLEARMODULE ) {
		ClearOptimize();
		if ( par == STOREMODULE && PopVariables() ) RetCode = -1;
		if ( AR.infile->handle >= 0 ) {
			CloseFile(AR.infile->handle);
			remove(AR.infile->name);
			AR.infile->handle = -1;
		}
		AR.infile->POfill = AR.infile->PObuffer;
		PUTZERO(AR.infile->POposition);
		AR.infile->POfull = AR.infile->PObuffer;
		if ( AR.outfile->handle >= 0 ) {
			CloseFile(AR.outfile->handle);
			remove(AR.outfile->name);
			AR.outfile->handle = -1;
		}
		AR.outfile->POfull =
		AR.outfile->POfill = AR.outfile->PObuffer;
		PUTZERO(AR.outfile->POposition);
		if ( AR.hidefile->handle >= 0 ) {
			CloseFile(AR.hidefile->handle);
			remove(AR.hidefile->name);
			AR.hidefile->handle = -1;
		}
		AR.hidefile->POfull =
		AR.hidefile->POfill = AR.hidefile->PObuffer;
		PUTZERO(AR.hidefile->POposition);
		AC.HideLevel = 0;
		if ( par == CLEARMODULE ) {
			if ( DeleteStore(0) < 0 ) {
				MesPrint("Cannot restart the storage file");
				RetCode = -1;
			}
			else RetCode = 0;
			CleanUp(1);
			ResetVariables(2);
			AM.gProcessBucketSize = AM.hProcessBucketSize;
			AM.gparallelflag = PARALLELFLAG;
			AM.gnumextrasym = AM.ggnumextrasym;
			PruneExtraSymbols(AM.ggnumextrasym);
			IniVars();
		}
		ClearSpectators(par);
	}
	else {
		if ( CleanExpr(0) ) RetCode = -1;
		if ( AC.DidClean ) CompactifyTree(AC.exprnames,EXPRNAMES);
		ResetVariables(0);
		CleanUpSort(-1);
	}
	clearcbuf(AC.cbufnum);
	if ( AC.MultiBracketBuf != 0 ) {
	  for ( i = 0; i < MAXMULTIBRACKETLEVELS; i++ ) {
		if ( AC.MultiBracketBuf[i] ) {
			M_free(AC.MultiBracketBuf[i],"bracket buffer i");
			AC.MultiBracketBuf[i] = 0;
		}
	  }
	  AC.MultiBracketLevels = 0;
	  M_free(AC.MultiBracketBuf,"multi bracket buffer");
	  AC.MultiBracketBuf = 0;
	}

	return(RetCode);
}

/*
 		#] DoExecute : 
 		#[ PutBracket :

	Routine uses the bracket info to split a term into two pieces:
	1: the part outside the bracket, and
	2: the part inside the bracket.
	These parts are separated by a subterm of type HAAKJE.
	This subterm looks like: HAAKJE,3,level
	The level is used for nestings of brackets. The print routines
	cannot handle this yet (31-Mar-1988).

	The Bracket selector is in AT.BrackBuf in the form of a regular term,
	but without coefficient.
	When AR.BracketOn < 0 we have a socalled antibracket. The main effect
	is an exchange of the inner and outer part and where the coefficient goes.

	Routine recoded to facilitate b p1,p2; etc for dotproducts and tensors
	15-oct-1991
*/

WORD PutBracket(PHEAD WORD *termin)
{
	GETBIDENTITY
	WORD *t, *t1, *b, i, j, *lastfun;
	WORD *t2, *s1, *s2;
	WORD *bStop, *bb, *bf, *tStop;
	WORD *term1,*term2, *m1, *m2, *tStopa;
	WORD *bbb = 0, *bind, *binst = 0, bwild = 0, *bss = 0, *bns = 0, bset = 0;
	term1 = AT.WorkPointer+1;
	term2 = (WORD *)(((UBYTE *)(term1)) + AM.MaxTer);
	if ( ( (WORD *)(((UBYTE *)(term2)) + AM.MaxTer) ) > AT.WorkTop ) return(MesWork());
	if ( AR.BracketOn < 0 ) {
		t2 = term1; t1 = term2;		/* AntiBracket */
	}
	else {
		t1 = term1; t2 = term2;		/* Regular bracket */
	}
	b = AT.BrackBuf; bStop = b+*b; b++;
	while ( b < bStop ) {
		if ( *b == INDEX ) { bwild = 1; bbb = b+2; binst = b + b[1]; }
		if ( *b == SETSET ) { bset = 1; bss = b+2; bns = b + b[1]; }
		b += b[1];
	}

	t = termin; tStopa = t + *t; i = *(t + *t -1); i = ABS(i);
	if ( AR.PolyFun && AT.PolyAct ) tStop = termin + AT.PolyAct;
	else tStop = tStopa - i;
	t++;
	if ( AR.BracketOn < 0 ) {
		lastfun = 0;
		while ( t < tStop && *t >= FUNCTION
			&& functions[*t-FUNCTION].commute ) {
			b = AT.BrackBuf+1;
			while ( b < bStop ) {
				if ( *b == *t ) {
					lastfun = t;
					while ( t < tStop && *t >= FUNCTION
						&& functions[*t-FUNCTION].commute ) t += t[1];
					goto NextNcom1;
				}
				b += b[1];
			}
			if ( bset ) {
				b = bss;
				while ( b < bns ) {
					if ( b[1] == CFUNCTION ) { /* Set of functions */
						SETS set = Sets+b[0]; WORD i;
						for ( i = set->first; i < set->last; i++ ) {
							if ( SetElements[i] == *t ) {
								lastfun = t;
								while ( t < tStop && *t >= FUNCTION
									&& functions[*t-FUNCTION].commute ) t += t[1];
								goto NextNcom1;
							}
						}
					}
					b += 2;
				}
			}
			if ( bwild && *t >= FUNCTION && functions[*t-FUNCTION].spec ) {
				s1 = t + t[1];
				s2 = t + FUNHEAD;
				while ( s2 < s1 ) {
					bind = bbb;
					while ( bind < binst ) {
						if ( *bind == *s2 ) {
							lastfun = t;
							while ( t < tStop && *t >= FUNCTION
								&& functions[*t-FUNCTION].commute ) t += t[1];
							goto NextNcom1;
						}
						bind++;
					}
					s2++;
				}
			}
			t += t[1];
		}
NextNcom1:
		s1 = termin + 1;
		if ( lastfun ) {
			while ( s1 < lastfun ) *t2++ = *s1++;
			while ( s1 < t ) *t1++ = *s1++;
		}
		else {
			while ( s1 < t ) *t2++ = *s1++;
		}

	}
	else {
		lastfun = t;
		while ( t < tStop && *t >= FUNCTION
			&& functions[*t-FUNCTION].commute ) {
			b = AT.BrackBuf+1;
			while ( b < bStop ) {
				if ( *b == *t ) { lastfun = t + t[1]; goto NextNcom; }
				b += b[1];
			}
			if ( bset ) {
				b = bss;
				while ( b < bns ) {
					if ( b[1] == CFUNCTION ) { /* Set of functions */
						SETS set = Sets+b[0]; WORD i;
						for ( i = set->first; i < set->last; i++ ) {
							if ( SetElements[i] == *t ) {
								lastfun = t + t[1];
								goto NextNcom;
							}
						}
					}
					b += 2;
				}
			}
			if ( bwild && *t >= FUNCTION && functions[*t-FUNCTION].spec ) {
				s1 = t + t[1];
				s2 = t + FUNHEAD;
				while ( s2 < s1 ) {
					bind = bbb;
					while ( bind < binst ) {
						if ( *bind == *s2 ) { lastfun = t + t[1]; goto NextNcom; }
						bind++;
					}
					s2++;
				}
			}
NextNcom:
			t += t[1];
		}
		s1 = termin + 1;
		while ( s1 < lastfun ) *t1++ = *s1++;
		while ( s1 < t ) *t2++ = *s1++;
	}
/*
	Now we have only commuting functions left. Move the b pointer to them.
*/
	b = AT.BrackBuf + 1;
	while ( b < bStop && *b >= FUNCTION
		&& ( *b < FUNCTION || functions[*b-FUNCTION].commute ) ) {
		b += b[1];
	}
	bf = b;

	while ( t < tStop && ( bf < bStop || bwild || bset ) ) {
		b = bf;
		while ( b < bStop && *b != *t ) { b += b[1]; }
		i = t[1];
		if ( *t >= FUNCTION ) { /* We are in function territory */
			if ( b < bStop && *b == *t ) goto FunBrac;
			if ( bset ) {
				b = bss;
				while ( b < bns ) {
					if ( b[1] == CFUNCTION ) { /* Set of functions */
						SETS set = Sets+b[0]; WORD i;
						for ( i = set->first; i < set->last; i++ ) {
							if ( SetElements[i] == *t ) goto FunBrac;
						}
					}
					b += 2;
				}
			}
			if ( bwild && *t >= FUNCTION && functions[*t-FUNCTION].spec ) {
				s1 = t + t[1];
				s2 = t + FUNHEAD;
				while ( s2 < s1 ) {
					bind = bbb;
					while ( bind < binst ) {
						if ( *bind == *s2 ) goto FunBrac;
						bind++;
					}
					s2++;
				}
			}
			NCOPY(t2,t,i);
			continue;
FunBrac:	NCOPY(t1,t,i);
			continue;
		}
/*
	We have left: DELTA, INDEX, VECTOR, DOTPRODUCT, SYMBOL
*/
		if ( *t == DELTA ) {
			if ( b < bStop && *b == DELTA ) {
				b += b[1];
				NCOPY(t1,t,i);
			}
			else { NCOPY(t2,t,i); }
		}
		else if ( *t == INDEX ) {
			if ( bwild ) {
				m1 = t1; m2 = t2;
				*t1++ = *t; t1++; *t2++ = *t; t2++;
				bind = bbb;
				j = t[1] -2;
				t += 2;
				while ( --j >= 0 ) {
					while ( *bind < *t && bind < binst ) bind++;
					if ( *bind == *t && bind < binst ) {
						*t1++ = *t++;
					}
					else if ( bset ) {
						WORD *b3 = bss;
						while ( b3 < bns ) {
							if ( b3[1] == CVECTOR ) {
								SETS set = Sets+b3[0]; WORD i;
								for ( i = set->first; i < set->last; i++ ) {
									if ( SetElements[i] == *t ) {
										*t1++ = *t++;
										goto nextind;
									}
								}
							}
							b3 += 2;
						}
						*t2++ = *t++;
					}
					else *t2++ = *t++;
nextind:;
				}
				m1[1] = WORDDIF(t1,m1);
				if ( m1[1] == 2 ) t1 = m1;
				m2[1] = WORDDIF(t2,m2);
				if ( m2[1] == 2 ) t2 = m2;
			}
			else if ( bset ) {
				m1 = t1; m2 = t2;
				*t1++ = *t; t1++; *t2++ = *t; t2++;
				j = t[1] -2;
				t += 2;
				while ( --j >= 0 ) {
					WORD *b3 = bss;
					while ( b3 < bns ) {
						if ( b3[1] == CVECTOR ) {
							SETS set = Sets+b3[0]; WORD i;
							for ( i = set->first; i < set->last; i++ ) {
								if ( SetElements[i] == *t ) {
									*t1++ = *t++;
									goto nextind2;
								}
							}
						}
						b3 += 2;
					}
					*t2++ = *t++;
nextind2:;
				}
				m1[1] = WORDDIF(t1,m1);
				if ( m1[1] == 2 ) t1 = m1;
				m2[1] = WORDDIF(t2,m2);
				if ( m2[1] == 2 ) t2 = m2;
			}
			else {
				NCOPY(t2,t,i);
			}
		}
		else if ( *t == VECTOR ) {
			if ( ( b < bStop && *b == VECTOR ) || bwild ) {
				if ( b < bStop && *b == VECTOR ) {
					bb = b + b[1]; b += 2;
				}
				else bb = b;
				j = t[1] - 2;
				m1 = t1; m2 = t2; *t1++ = *t; *t2++ = *t; t1++; t2++; t += 2;
				while ( j > 0 ) {
					j -= 2;
					while ( b < bb && ( *b < *t ||
					( *b == *t && b[1] < t[1] ) ) ) b += 2;
					if ( b < bb && ( *t == *b && t[1] == b[1] ) ) {
						*t1++ = *t++; *t1++ = *t++; goto nextvec;
					}
					else if ( bwild ) {
						bind = bbb;
						while ( bind < binst ) {
							if ( *t == *bind || t[1] == *bind ) {
								*t1++ = *t++; *t1++ = *t++;
								goto nextvec;
							}
							bind++;
						}
					}
					if ( bset ) {
						WORD *b3 = bss;
						while ( b3 < bns ) {
							if ( b3[1] == CVECTOR ) {
								SETS set = Sets+b3[0]; WORD i;
								for ( i = set->first; i < set->last; i++ ) {
									if ( SetElements[i] == *t ) {
										*t1++ = *t++; *t1++ = *t++;
										goto nextvec;
									}
								}
							}
							b3 += 2;
						}
					}
					*t2++ = *t++; *t2++ = *t++;
nextvec:;
				}
				m1[1] = WORDDIF(t1,m1);
				if ( m1[1] == 2 ) t1 = m1;
				m2[1] = WORDDIF(t2,m2);
				if ( m2[1] == 2 ) t2 = m2;
			}
			else if ( bset ) {
				m1 = t1; *t1++ = *t; t1++;
				m2 = t2; *t2++ = *t; t2++;
				s2 = t + i; t += 2;
				while ( t < s2 ) {
					WORD *b3 = bss;
					while ( b3 < bns ) {
						if ( b3[1] == CVECTOR ) {
							SETS set = Sets+b3[0]; WORD i;
							for ( i = set->first; i < set->last; i++ ) {
								if ( SetElements[i] == *t ) {
									*t1++ = *t++; *t1++ = *t++;
									goto nextvec2;
								}
							}
						}
						b3 += 2;
					}
					*t2++ = *t++; *t2++ = *t++;
nextvec2:;
				}
				m1[1] = WORDDIF(t1,m1);
				if ( m1[1] == 2 ) t1 = m1;
				m2[1] = WORDDIF(t2,m2);
				if ( m2[1] == 2 ) t2 = m2;
			}
			else {
				NCOPY(t2,t,i);
			}
		}
		else if ( *t == DOTPRODUCT ) {
			if ( ( b < bStop && *b == *t ) || bwild ) {
				m1 = t1; *t1++ = *t; t1++;
				m2 = t2; *t2++ = *t; t2++;
				if ( b >= bStop || *b != *t ) { bb = b; s1 = b; }
				else {
					s1 = b + b[1]; bb = b + 2;
				}
				s2 = t + i; t += 2;
				while ( t < s2 && ( bb < s1 || bwild || bset ) ) {
					while ( bb < s1 && ( *bb < *t ||
					( *bb == *t && bb[1] < t[1] ) ) ) bb += 3;
					if ( bb < s1 && *bb == *t && bb[1] == t[1] ) {
						*t1++ = *t++; *t1++ = *t++; *t1++ = *t++; bb += 3;
						goto nextdot;
					}
					else if ( bwild ) {
						bind = bbb;
						while ( bind < binst ) {
							if ( *bind == *t || *bind == t[1] ) {
								*t1++ = *t++; *t1++ = *t++; *t1++ = *t++;
								goto nextdot;
							}
							bind++;
						}
					}
					if ( bset ) {
						WORD *b3 = bss;
						while ( b3 < bns ) {
							if ( b3[1] == CVECTOR ) {
								SETS set = Sets+b3[0]; WORD i;
								for ( i = set->first; i < set->last; i++ ) {
									if ( SetElements[i] == *t || SetElements[i] == t[1] ) {
										*t1++ = *t++; *t1++ = *t++; *t1++ = *t++;
										goto nextdot;
									}
								}
							}
							b3 += 2;
						}
					}
					*t2++ = *t++; *t2++ = *t++; *t2++ = *t++;
nextdot:;
				}
				while ( t < s2 ) *t2++ = *t++;
				m1[1] = WORDDIF(t1,m1);
				if ( m1[1] == 2 ) t1 = m1;
				m2[1] = WORDDIF(t2,m2);
				if ( m2[1] == 2 ) t2 = m2;
			}
			else if ( bset ) {
				m1 = t1; *t1++ = *t; t1++;
				m2 = t2; *t2++ = *t; t2++;
				s2 = t + i; t += 2;
				while ( t < s2 ) {
					WORD *b3 = bss;
					while ( b3 < bns ) {
						if ( b3[1] == CVECTOR ) {
							SETS set = Sets+b3[0]; WORD i;
							for ( i = set->first; i < set->last; i++ ) {
								if ( SetElements[i] == *t || SetElements[i] == t[1] ) {
									*t1++ = *t++; *t1++ = *t++; *t1++ = *t++;
									goto nextdot2;
								}
							}
						}
						b3 += 2;
					}
					*t2++ = *t++; *t2++ = *t++; *t2++ = *t++;
nextdot2:;
				}
				m1[1] = WORDDIF(t1,m1);
				if ( m1[1] == 2 ) t1 = m1;
				m2[1] = WORDDIF(t2,m2);
				if ( m2[1] == 2 ) t2 = m2;
			}
			else { NCOPY(t2,t,i); }
		}
		else if ( *t == SYMBOL ) {
			if ( b < bStop && *b == *t ) {
				m1 = t1; *t1++ = *t; t1++;
				m2 = t2; *t2++ = *t; t2++;
				s1 = b + b[1]; bb = b+2;
				s2 = t + i; t += 2;
				while ( bb < s1 && t < s2 ) {
					while ( bb < s1 && *bb < *t ) bb += 2;
					if ( bb >= s1 ) {
						if ( bset ) goto TrySymbolSet;
						break;
					}
					if ( *bb == *t ) { *t1++ = *t++; *t1++ = *t++; }
					else if ( bset ) {
						WORD *bbb;
TrySymbolSet:
						bbb = bss;
						while ( bbb < bns ) {
							if ( bbb[1] == CSYMBOL ) { /* Set of symbols */
								SETS set = Sets+bbb[0]; WORD i;
								for ( i = set->first; i < set->last; i++ ) {
									if ( SetElements[i] == *t ) {
										*t1++ = *t++; *t1++ = *t++;
										goto NextSymbol;
									}
								}
							}
							bbb += 2;
						}
						*t2++ = *t++; *t2++ = *t++;
					}
					else { *t2++ = *t++; *t2++ = *t++; } 
NextSymbol:;
				}
				while ( t < s2 ) *t2++ = *t++;
				m1[1] = WORDDIF(t1,m1);
				if ( m1[1] == 2 ) t1 = m1;
				m2[1] = WORDDIF(t2,m2);
				if ( m2[1] == 2 ) t2 = m2;
			}
			else if ( bset ) {
				WORD *bbb;
				m1 = t1; *t1++ = *t; t1++;
				m2 = t2; *t2++ = *t; t2++;
				s2 = t + i; t += 2;
				while ( t < s2 ) {
					bbb = bss;
					while ( bbb < bns ) {
						if ( bbb[1] == CSYMBOL ) { /* Set of symbols */
							SETS set = Sets+bbb[0]; WORD i;
							for ( i = set->first; i < set->last; i++ ) {
								if ( SetElements[i] == *t ) {
									*t1++ = *t++; *t1++ = *t++;
									goto NextSymbol2;
								}
							}
						}
						bbb += 2;
					}
					*t2++ = *t++; *t2++ = *t++;
NextSymbol2:;
				}
				m1[1] = WORDDIF(t1,m1);
				if ( m1[1] == 2 ) t1 = m1;
				m2[1] = WORDDIF(t2,m2);
				if ( m2[1] == 2 ) t2 = m2;
			}
			else { NCOPY(t2,t,i); }
		}
		else {
			NCOPY(t2,t,i);
		}
	}
	if ( ( i = WORDDIF(tStop,t) ) > 0 ) NCOPY(t2,t,i);
	if ( AR.BracketOn < 0 ) {
		s1 = t1; t1 = t2; t2 = s1;
	}
	do { *t2++ = *t++; } while ( t < (WORD *)tStopa );
	t = AT.WorkPointer;
	i = WORDDIF(t1,term1);
	*t++ = 4 + i + WORDDIF(t2,term2);
	t += i;
	*t++ = HAAKJE;
	*t++ = 3;
	*t++ = 0;			/* This feature won't be used for a while */
	i = WORDDIF(t2,term2);
	t1 = term2;
	if ( i > 0 ) NCOPY(t,t1,i);

	AT.WorkPointer = t;

	return(0);
}

/*
 		#] PutBracket : 
 		#[ SpecialCleanup :
*/

VOID SpecialCleanup(PHEAD0)
{
	GETBIDENTITY
	if ( AT.previousEfactor ) M_free(AT.previousEfactor,"Efactor cache");
	AT.previousEfactor = 0;
}

/*
 		#] SpecialCleanup : 
 		#[ SetMods :
*/

#ifndef WITHPTHREADS

void SetMods()
{
	int i, n;
	if ( AN.cmod != 0 ) M_free(AN.cmod,"AN.cmod");
	n = ABS(AN.ncmod);
	AN.cmod = (UWORD *)Malloc1(sizeof(WORD)*n,"AN.cmod");
	for ( i = 0; i < n; i++ ) AN.cmod[i] = AC.cmod[i];
}

#endif

/*
 		#] SetMods : 
 		#[ UnSetMods :
*/

#ifndef WITHPTHREADS

void UnSetMods()
{
	if ( AN.cmod != 0 ) M_free(AN.cmod,"AN.cmod");
	AN.cmod = 0;
}

#endif

/*
 		#] UnSetMods : 
	#] DoExecute :
	#[ Expressions :
 		#[ ExchangeExpressions :
*/

void ExchangeExpressions(int num1, int num2)
{
	GETIDENTITY
	WORD node1, node2, namesize, TMproto[SUBEXPSIZE];
	INDEXENTRY *ind;
	EXPRESSIONS e1, e2;
	LONG a;
	SBYTE *s1, *s2;
	int i;
	e1 = Expressions + num1;
	e2 = Expressions + num2;
	node1 = e1->node;
	node2 = e2->node;
	AC.exprnames->namenode[node1].number = num2;
	AC.exprnames->namenode[node2].number = num1;
	a = e1->name; e1->name = e2->name; e2->name = a;
	namesize = e1->namesize; e1->namesize = e2->namesize; e2->namesize = namesize;
	e1->node = node2;
	e2->node = node1;
	if ( e1->status == STOREDEXPRESSION ) {
/*
		Find the name in the index and replace by the new name
*/
		TMproto[0] = EXPRESSION;
		TMproto[1] = SUBEXPSIZE;
		TMproto[2] = num1;
		TMproto[3] = 1;
		{ int ie; for ( ie = 4; ie < SUBEXPSIZE; ie++ ) TMproto[ie] = 0; }
		AT.TMaddr = TMproto;
		ind = FindInIndex(num1,&AR.StoreData,0,0);
		s1 = (SBYTE *)(AC.exprnames->namebuffer+e1->name);
		i = e1->namesize;
		s2 = ind->name;
		NCOPY(s2,s1,i);
		*s2 = 0;
		SeekFile(AR.StoreData.Handle,&(e1->onfile),SEEK_SET);
		if ( WriteFile(AR.StoreData.Handle,(UBYTE *)ind,
		(LONG)(sizeof(INDEXENTRY))) != sizeof(INDEXENTRY) ) {
			MesPrint("File error while exchanging expressions");
			Terminate(-1);
		}
		FlushFile(AR.StoreData.Handle);
	}
	if ( e2->status == STOREDEXPRESSION ) {
/*
		Find the name in the index and replace by the new name
*/
		TMproto[0] = EXPRESSION;
		TMproto[1] = SUBEXPSIZE;
		TMproto[2] = num2;
		TMproto[3] = 1;
		{ int ie; for ( ie = 4; ie < SUBEXPSIZE; ie++ ) TMproto[ie] = 0; }
		AT.TMaddr = TMproto;
		ind = FindInIndex(num1,&AR.StoreData,0,0);
		s1 = (SBYTE *)(AC.exprnames->namebuffer+e2->name);
		i = e2->namesize;
		s2 = ind->name;
		NCOPY(s2,s1,i);
		*s2 = 0;
		SeekFile(AR.StoreData.Handle,&(e2->onfile),SEEK_SET);
		if ( WriteFile(AR.StoreData.Handle,(UBYTE *)ind,
		(LONG)(sizeof(INDEXENTRY))) != sizeof(INDEXENTRY) ) {
			MesPrint("File error while exchanging expressions");
			Terminate(-1);
		}
		FlushFile(AR.StoreData.Handle);
	}
}

/*
 		#] ExchangeExpressions : 
 		#[ GetFirstBracket :
*/

int GetFirstBracket(WORD *term, int num)
{
/*
		Gets the first bracket of the expression 'num'
		Puts it in term. If no brackets the answer is one.
		Routine should be thread-safe
*/
	GETIDENTITY
	POSITION position, oldposition;
	RENUMBER renumber;
	FILEHANDLE *fi;
	WORD type, *oldcomppointer, oldonefile, numword;
	WORD *t, *tstop;

	oldcomppointer = AR.CompressPointer;
	type = Expressions[num].status;
	if ( type == STOREDEXPRESSION ) {
		WORD TMproto[SUBEXPSIZE];
		TMproto[0] = EXPRESSION;
		TMproto[1] = SUBEXPSIZE;
		TMproto[2] = num;
		TMproto[3] = 1;
		{ int ie; for ( ie = 4; ie < SUBEXPSIZE; ie++ ) TMproto[ie] = 0; }
		AT.TMaddr = TMproto;
		PUTZERO(position);
		if ( ( renumber = GetTable(num,&position,0) ) == 0 ) {
			MesCall("GetFirstBracket");
			SETERROR(-1)
		}
		if ( GetFromStore(term,&position,renumber,&numword,num) < 0 ) {
			MesCall("GetFirstBracket");
			SETERROR(-1)
		}
/*
#ifdef WITHPTHREADS
*/
		if ( renumber->symb.lo != AN.dummyrenumlist )
			M_free(renumber->symb.lo,"VarSpace");
		M_free(renumber,"Renumber");
/*
#endif
*/
	}
	else {			/* Active expression */
		oldonefile = AR.GetOneFile;
		if ( type == HIDDENLEXPRESSION || type == HIDDENGEXPRESSION ) {
			AR.GetOneFile = 2; fi = AR.hidefile;
		}
		else {
			AR.GetOneFile = 0; fi = AR.infile;
		}
		if ( fi->handle >= 0 ) {
			PUTZERO(oldposition);
/*
			SeekFile(fi->handle,&oldposition,SEEK_CUR);
*/
			}
		else {
			SETBASEPOSITION(oldposition,fi->POfill-fi->PObuffer);
		}
		position = AS.OldOnFile[num];
		if ( GetOneTerm(BHEAD term,fi,&position,1) < 0
		|| ( GetOneTerm(BHEAD term,fi,&position,1) < 0 ) ) {
			MLOCK(ErrorMessageLock);
			MesCall("GetFirstBracket");
			MUNLOCK(ErrorMessageLock);
			SETERROR(-1)
		}
		if ( fi->handle >= 0 ) {
/*
			SeekFile(fi->handle,&oldposition,SEEK_SET);
			if ( ISNEGPOS(oldposition) ) {
				MLOCK(ErrorMessageLock);
				MesPrint("File error");
				MUNLOCK(ErrorMessageLock);
				SETERROR(-1)
			}
*/
		}
		else {
			fi->POfill = fi->PObuffer+BASEPOSITION(oldposition);
		}
		AR.GetOneFile = oldonefile;
	}
	AR.CompressPointer = oldcomppointer;
	if ( *term ) {
		tstop = term + *term; tstop -= ABS(tstop[-1]);
		t = term + 1;
		while ( t < tstop ) {
			if ( *t == HAAKJE ) break;
			t += t[1];
		}
		if ( t >= tstop ) {
			term[0] = 4; term[1] = 1; term[2] = 1; term[3] = 3;
		}
		else {
			*t++ = 1; *t++ = 1; *t++ = 3; *term = t - term;
		} 
	}
	else {
		term[0] = 4; term[1] = 1; term[2] = 1; term[3] = 3;
	}
	return(*term);
}

/*
 		#] GetFirstBracket : 
 		#[ GetFirstTerm :
*/

int GetFirstTerm(WORD *term, int num)
{
/*
		Gets the first term of the expression 'num'
		Puts it in term.
		Routine should be thread-safe
*/
	GETIDENTITY
	POSITION position, oldposition;
	RENUMBER renumber;
	FILEHANDLE *fi;
	WORD type, *oldcomppointer, oldonefile, numword;

	oldcomppointer = AR.CompressPointer;
	type = Expressions[num].status;
	if ( type == STOREDEXPRESSION ) {
		WORD TMproto[SUBEXPSIZE];
		TMproto[0] = EXPRESSION;
		TMproto[1] = SUBEXPSIZE;
		TMproto[2] = num;
		TMproto[3] = 1;
		{ int ie; for ( ie = 4; ie < SUBEXPSIZE; ie++ ) TMproto[ie] = 0; }
		AT.TMaddr = TMproto;
		PUTZERO(position);
		if ( ( renumber = GetTable(num,&position,0) ) == 0 ) {
			MesCall("GetFirstTerm");
			SETERROR(-1)
		}
		if ( GetFromStore(term,&position,renumber,&numword,num) < 0 ) {
			MesCall("GetFirstTerm");
			SETERROR(-1)
		}
/*
#ifdef WITHPTHREADS
*/
		if ( renumber->symb.lo != AN.dummyrenumlist )
			M_free(renumber->symb.lo,"VarSpace");
		M_free(renumber,"Renumber");
/*
#endif
*/
	}
	else {			/* Active expression */
		oldonefile = AR.GetOneFile;
		if ( type == HIDDENLEXPRESSION || type == HIDDENGEXPRESSION ) {
			AR.GetOneFile = 2; fi = AR.hidefile;
		}
		else {
			AR.GetOneFile = 0;
			if ( Expressions[num].replace == NEWLYDEFINEDEXPRESSION )
			     fi = AR.outfile;
			else fi = AR.infile;
		}
		if ( fi->handle >= 0 ) {
			PUTZERO(oldposition);
/*
			SeekFile(fi->handle,&oldposition,SEEK_CUR);
*/
			}
		else {
			SETBASEPOSITION(oldposition,fi->POfill-fi->PObuffer);
		}
		position = AS.OldOnFile[num];
		if ( GetOneTerm(BHEAD term,fi,&position,1) < 0
		|| ( GetOneTerm(BHEAD term,fi,&position,1) < 0 ) ) {
			MLOCK(ErrorMessageLock);
			MesCall("GetFirstTerm");
			MUNLOCK(ErrorMessageLock);
			SETERROR(-1)
		}
		if ( fi->handle >= 0 ) {
/*
			SeekFile(fi->handle,&oldposition,SEEK_SET);
			if ( ISNEGPOS(oldposition) ) {
				MLOCK(ErrorMessageLock);
				MesPrint("File error");
				MUNLOCK(ErrorMessageLock);
				SETERROR(-1)
			}
*/
		}
		else {
			fi->POfill = fi->PObuffer+BASEPOSITION(oldposition);
		}
		AR.GetOneFile = oldonefile;
	}
	AR.CompressPointer = oldcomppointer;
	return(*term);
}

/*
 		#] GetFirstTerm : 
 		#[ GetContent :
*/

int GetContent(WORD *content, int num)
{
/*
		Gets the content of the expression 'num'
		Puts it in content.
		Routine should be thread-safe
		The content is defined as the term that will make the expression 'num'
		with integer coefficients, no GCD and all common factors taken out,
		all negative powers removed when we divide the expression by this
		content.
*/
	GETIDENTITY
	POSITION position, oldposition;
	RENUMBER renumber;
	FILEHANDLE *fi;
	WORD type, *oldcomppointer, oldonefile, numword, *term, i;
	WORD *cbuffer = TermMalloc("GetContent");
	WORD *oldworkpointer = AT.WorkPointer;

	oldcomppointer = AR.CompressPointer;
	type = Expressions[num].status;
	if ( type == STOREDEXPRESSION ) {
		WORD TMproto[SUBEXPSIZE];
		TMproto[0] = EXPRESSION;
		TMproto[1] = SUBEXPSIZE;
		TMproto[2] = num;
		TMproto[3] = 1;
		{ int ie; for ( ie = 4; ie < SUBEXPSIZE; ie++ ) TMproto[ie] = 0; }
		AT.TMaddr = TMproto;
		PUTZERO(position);
		if ( ( renumber = GetTable(num,&position,0) ) == 0 ) goto CalledFrom;
		if ( GetFromStore(cbuffer,&position,renumber,&numword,num) < 0 ) goto CalledFrom;
		for(;;) {
			term = oldworkpointer;
			AR.CompressPointer = oldcomppointer;
			if ( GetFromStore(term,&position,renumber,&numword,num) < 0 ) goto CalledFrom;
			if ( *term == 0 ) break;
/*
			'merge' the two terms
*/
			if ( ContentMerge(BHEAD cbuffer,term) < 0 ) goto CalledFrom;
		}
/*
#ifdef WITHPTHREADS
*/
		if ( renumber->symb.lo != AN.dummyrenumlist )
			M_free(renumber->symb.lo,"VarSpace");
		M_free(renumber,"Renumber");
/*
#endif
*/
	}
	else {			/* Active expression */
		oldonefile = AR.GetOneFile;
		if ( type == HIDDENLEXPRESSION || type == HIDDENGEXPRESSION ) {
			AR.GetOneFile = 2; fi = AR.hidefile;
		}
		else {
			AR.GetOneFile = 0;
			if ( Expressions[num].replace == NEWLYDEFINEDEXPRESSION )
			     fi = AR.outfile;
			else fi = AR.infile;
		}
		if ( fi->handle >= 0 ) {
			PUTZERO(oldposition);
/*
			SeekFile(fi->handle,&oldposition,SEEK_CUR);
*/
			}
		else {
			SETBASEPOSITION(oldposition,fi->POfill-fi->PObuffer);
		}
		position = AS.OldOnFile[num];
		if ( GetOneTerm(BHEAD cbuffer,fi,&position,1) < 0 ) goto CalledFrom;
		AR.CompressPointer = oldcomppointer;
		if ( GetOneTerm(BHEAD cbuffer,fi,&position,1) < 0 ) goto CalledFrom;
/*
		Now go through the terms. For each term we have to test whether
		what is in cbuffer is also in that term. If not, we have to remove
		it from cbuffer. Additionally we have to accumulate the GCD of the
		numerators and the LCM of the denominators. This is all done in the
		routine ContentMerge.
*/
		for(;;) {
			term = oldworkpointer;
			AR.CompressPointer = oldcomppointer;
			if ( GetOneTerm(BHEAD term,fi,&position,1) < 0 ) goto CalledFrom;
			if ( *term == 0 ) break;
/*
			'merge' the two terms
*/
			if ( ContentMerge(BHEAD cbuffer,term) < 0 ) goto CalledFrom;
		}
		if ( fi->handle < 0 ) {
			fi->POfill = fi->PObuffer+BASEPOSITION(oldposition);
		}
		AR.GetOneFile = oldonefile;
	}
	AR.CompressPointer = oldcomppointer;
	for ( i = 0; i < *cbuffer; i++ ) content[i] = cbuffer[i];
	TermFree(cbuffer,"GetContent");
	AT.WorkPointer = oldworkpointer;
	return(*content);
CalledFrom:
	MLOCK(ErrorMessageLock);
	MesCall("GetContent");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] GetContent : 
 		#[ CleanupTerm :

		Removes noncommuting objects from the term
*/

int CleanupTerm(WORD *term)
{
	WORD *tstop, *t, *tfill, *tt;
	GETSTOP(term,tstop);
	t = term+1;
	while ( t < tstop ) {
		if ( *t >= FUNCTION && ( functions[*t-FUNCTION].commute || *t == DENOMINATOR ) ) {
			tfill = t; tt = t + t[1]; tstop = term + *term;
			while ( tt < tstop ) *tfill++ = *tt++;
			*term = tfill - term;
			tstop -= ABS(tfill[-1]);
		}
		else {
			t += t[1];
		}
	}
	return(0);
}

/*
 		#] CleanupTerm : 
 		#[ ContentMerge :
*/

WORD ContentMerge(PHEAD WORD *content, WORD *term)
{
	GETBIDENTITY
	WORD *cstop, csize, crsize, sign = 1, numsize, densize, i, tnsize, tdsize;
	UWORD *num, *den, *tnum, *tden;
	WORD *outfill, *outb = TermMalloc("ContentMerge"), *ct;
	WORD *t, *tstop, tsize, trsize, *told;
	WORD *t1, *t2, *c1, *c2, i1, i2, *out1;
	WORD didsymbol = 0, diddotp = 0, tfirst;
	cstop = content + *content;
	csize = cstop[-1];
	if ( csize < 0 ) { sign = -sign; csize = -csize; }
	cstop -= csize;
	numsize = densize = crsize = (csize-1)/2;
	num = NumberMalloc("ContentMerge");
	den = NumberMalloc("ContentMerge");
	for ( i = 0; i < numsize; i++ ) num[i] = (UWORD)(cstop[i]);
	for ( i = 0; i < densize; i++ ) den[i] = (UWORD)(cstop[i+crsize]);
	while ( num[numsize-1] == 0 ) numsize--;		
	while ( den[densize-1] == 0 ) densize--;		
/*
	First we do the coefficient
*/
	tstop = term + *term;
	tsize = tstop[-1];
	if ( tsize < 0 ) tsize = -tsize;
/*	else { sign = 1; } */
	tstop = tstop - tsize;
	tnsize = tdsize = trsize = (tsize-1)/2;
	tnum = (UWORD *)tstop; tden = (UWORD *)(tstop + trsize);
	while ( tnum[tnsize-1] == 0 ) tnsize--;
	while ( tden[tdsize-1] == 0 ) tdsize--;
	GcdLong(BHEAD num, numsize, tnum, tnsize, num, &numsize);
	if ( LcmLong(BHEAD den, densize, tden, tdsize, den, &densize) ) goto CalledFrom;
	outfill = outb + 1;
	ct = content + 1;
	t = term + 1;
	while ( ct < cstop ) {
		switch ( *ct ) {
			case SYMBOL:
				didsymbol = 1;
				t = term+1;
				while ( t < tstop && *t != *ct ) t += t[1];
				if ( t >= tstop ) break;
				t1 = t+2; t2 = t+t[1];
				c1 = ct+2; c2 = ct+ct[1];
				out1 = outfill; *outfill++ = *ct; outfill++;
				while ( c1 < c2 && t1 < t2 ) {
					if ( *c1 == *t1 ) {
						if ( t1[1] <= c1[1] ) {
							*outfill++ = *t1++; *outfill++ = *t1++;
							c1 += 2;
						}
						else {
							*outfill++ = *c1++; *outfill++ = *c1++;
							t1 += 2;
						}
					}
					else if ( *c1 < *t1 ) {
						if ( c1[1] < 0 ) {
							*outfill++ = *c1++; *outfill++ = *c1++;
						}
						else { c1 += 2; }
					}
					else {
						if ( t1[1] < 0 ) {
							*outfill++ = *t1++; *outfill++ = *t1++;
						}
						else t1 += 2;
					}
				}
				while ( c1 < c2 ) {
					if ( c1[1] < 0 ) { *outfill++ = c1[0]; *outfill++ = c1[1]; }
					c1 += 2;
				}
				while ( t1 < t2 ) {
					if ( t1[1] < 0 ) { *outfill++ = t1[0]; *outfill++ = t1[1]; }
					t1 += 2;
				}
				out1[1] = outfill - out1;
				if ( out1[1] == 2 ) outfill = out1;
				break;
			case DOTPRODUCT:
				diddotp = 1;
				t = term+1;
				while ( t < tstop && *t != *ct ) t += t[1];
				if ( t >= tstop ) break;
				t1 = t+2; t2 = t+t[1];
				c1 = ct+2; c2 = ct+ct[1];
				out1 = outfill; *outfill++ = *ct; outfill++;
				while ( c1 < c2 && t1 < t2 ) {
					if ( *c1 == *t1 && c1[1] == t1[1] ) {
						if ( t1[2] <= c1[2] ) {
							*outfill++ = *t1++; *outfill++ = *t1++; *outfill++ = *t1++;
							c1 += 3;
						}
						else {
							*outfill++ = *c1++; *outfill++ = *c1++; *outfill++ = *c1++;
							t1 += 3;
						}
					}
					else if ( *c1 < *t1 || ( *c1 == *t1 && c1[1] < t1[1] ) ) {
						if ( c1[2] < 0 ) {
							*outfill++ = *c1++; *outfill++ = *c1++; *outfill++ = *c1++;
						}
						else { c1 += 3; }
					}
					else {
						if ( t1[2] < 0 ) {
							*outfill++ = *t1++; *outfill++ = *t1++; *outfill++ = *t1++;
						}
						else t1 += 3;
					}
				}
				while ( c1 < c2 ) {
					if ( c1[2] < 0 ) { *outfill++ = c1[0]; *outfill++ = c1[1]; *outfill++ = c1[1]; }
					c1 += 3;
				}
				while ( t1 < t2 ) {
					if ( t1[2] < 0 ) { *outfill++ = t1[0]; *outfill++ = t1[1]; *outfill++ = t1[1]; }
					t1 += 3;
				}
				out1[1] = outfill - out1;
				if ( out1[1] == 2 ) outfill = out1;
				break;
			case INDEX:
				t = term+1;
				while ( t < tstop && *t != *ct ) t += t[1];
				if ( t >= tstop ) break;
				t1 = t+2; t2 = t+t[1];
				c1 = ct+2; c2 = ct+ct[1];
				out1 = outfill; *outfill++ = *ct; outfill++;
				while ( c1 < c2 && t1 < t2 ) {
					if ( *c1 == *t1 ) {
						*outfill++ = *c1++;
						t1 += 1;
					}
					else if ( *c1 < *t1 ) { c1 += 1; }
					else { t1 += 1; }
				}
				out1[1] = outfill - out1;
				if ( out1[1] == 2 ) outfill = out1;
				break;
			case VECTOR:
			case DELTA:
				t = term+1;
				while ( t < tstop && *t != *ct ) t += t[1];
				if ( t >= tstop ) break;
				t1 = t+2; t2 = t+t[1];
				c1 = ct+2; c2 = ct+ct[1];
				out1 = outfill; *outfill++ = *ct; outfill++;
				while ( c1 < c2 && t1 < t2 ) {
					if ( *c1 == *t1 && c1[1] && t1[1] ) {
						*outfill++ = *c1++; *outfill++ = *c1++;
						t1 += 2;
					}
					else if ( *c1 < *t1 || ( *c1 == *t1 && c1[1] < t1[1] ) ) {
						c1 += 2;
					}
					else {
						t1 += 2;
					}
				}
				out1[1] = outfill - out1;
				if ( out1[1] == 2 ) outfill = out1;
				break;
			case GAMMA:
			default:			/* Functions */
				told = t;
				t = term+1;
				while ( t < tstop ) {
					if ( *t != *ct ) { t += t[1]; continue; }
					if ( ct[1] != t[1] ) { t += t[1]; continue; }
					if ( ct[2] != t[2] ) { t += t[1]; continue; }
					t1 = t; t2 = ct; i1 = t1[1]; i2 = t2[1];
					while ( i1 > 0 ) {
						if ( *t1 != *t2 ) break;
						t1++; t2++; i1--;
					}
					if ( i1 != 0 ) { t += t[1]; continue; }
					t1 = t;
					for ( i = 0; i < i2; i++ ) { *outfill++ = *t++; }
/*
					Mark as 'used'. The flags must be different!
*/
					t1[2] |= SUBTERMUSED1;
					ct[2] |= SUBTERMUSED2;
					t = told;
					break;
				}
				break;
		}
		ct += ct[1];
	}
	if ( diddotp == 0 ) {
		t = term+1; while ( t < tstop && *t != DOTPRODUCT ) t += t[1];
		if ( t < tstop ) { /* now we need the negative powers */
			tfirst = 1; told = outfill;
			for ( i = 2; i < t[1]; i += 3 ) {
				if ( t[i+2] < 0 ) {
					if ( tfirst ) { *outfill++ = DOTPRODUCT; *outfill++ = 0; tfirst = 0; }
					*outfill++ = t[i]; *outfill++ = t[i+1]; *outfill++ = t[i+2];
				}
			}
			if ( outfill > told ) told[1] = outfill-told;
		}
	}
	if ( didsymbol == 0 ) {
		t = term+1; while ( t < tstop && *t != SYMBOL ) t += t[1];
		if ( t < tstop ) { /* now we need the negative powers */
			tfirst = 1; told = outfill;
			for ( i = 2; i < t[1]; i += 2 ) {
				if ( t[i+1] < 0 ) {
					if ( tfirst ) { *outfill++ = SYMBOL; *outfill++ = 0; tfirst = 0; }
					*outfill++ = t[i]; *outfill++ = t[i+1];
				}
			}
			if ( outfill > told ) told[1] = outfill-told;
		}
	}
/*
	Now put the coefficient back.
*/
	if ( numsize < densize ) {
		for ( i = numsize; i < densize; i++ ) num[i] = 0;
		numsize = densize;
	}
	else if ( densize < numsize ) {
		for ( i = densize; i < numsize; i++ ) den[i] = 0;
		densize = numsize;
	}
	for ( i = 0; i < numsize; i++ ) *outfill++ = num[i];
	for ( i = 0; i < densize; i++ ) *outfill++ = den[i];
	csize = numsize+densize+1;
	if ( sign < 0 ) csize = -csize;
	*outfill++ = csize;
	*outb = outfill-outb;
	NumberFree(den,"ContentMerge");
	NumberFree(num,"ContentMerge");
	for ( i = 0; i < *outb; i++ ) content[i] = outb[i];
	TermFree(outb,"ContentMerge");
/*
	Now we have to 'restore' the term to its original.
	We do not restore the content, because if anything was used the
	new content overwrites the old. 6-mar-2018 JV
*/
	t = term + 1;
	while ( t < tstop ) {
		if ( *t >= FUNCTION ) t[2] &= ~SUBTERMUSED1;
		t += t[1];
	}
	return(*content);
CalledFrom:
	MLOCK(ErrorMessageLock);
	MesCall("GetContent");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] ContentMerge : 
 		#[ TermsInExpression :
*/

LONG TermsInExpression(WORD num)
{
	LONG x = Expressions[num].counter;
	if ( x >= 0 ) return(x);
	return(-1);
}

/*
 		#] TermsInExpression : 
 		#[ SizeOfExpression :
*/

LONG SizeOfExpression(WORD num)
{
	LONG x = (LONG)(DIVPOS(Expressions[num].size,sizeof(WORD)));
	if ( x >= 0 ) return(x);
	return(-1);
}

/*
 		#] SizeOfExpression : 
 		#[ UpdatePositions :
*/

void UpdatePositions()
{
	EXPRESSIONS e = Expressions;
	POSITION *old;
	WORD *oldw;
	int i;
	if ( NumExpressions > 0 &&
		 ( AS.OldOnFile == 0 || AS.NumOldOnFile < NumExpressions ) ) {
		if ( AS.OldOnFile ) {
			old = AS.OldOnFile;
			AS.OldOnFile = (POSITION *)Malloc1(NumExpressions*sizeof(POSITION),"file pointers");
			for ( i = 0; i < AS.NumOldOnFile; i++ ) AS.OldOnFile[i] = old[i];
			AS.NumOldOnFile = NumExpressions;
			M_free(old,"process file pointers");
		}
		else {
			AS.OldOnFile = (POSITION *)Malloc1(NumExpressions*sizeof(POSITION),"file pointers");
			AS.NumOldOnFile = NumExpressions;
		}
	}
	if ( NumExpressions > 0 &&
		 ( AS.OldNumFactors == 0 || AS.NumOldNumFactors < NumExpressions ) ) {
		if ( AS.OldNumFactors ) {
			oldw = AS.OldNumFactors;
			AS.OldNumFactors = (WORD *)Malloc1(NumExpressions*sizeof(WORD),"numfactors pointers");
			for ( i = 0; i < AS.NumOldNumFactors; i++ ) AS.OldNumFactors[i] = oldw[i];
			M_free(oldw,"numfactors pointers");
			oldw = AS.Oldvflags;
			AS.Oldvflags = (WORD *)Malloc1(NumExpressions*sizeof(WORD),"vflags pointers");
			for ( i = 0; i < AS.NumOldNumFactors; i++ ) AS.Oldvflags[i] = oldw[i];
			AS.NumOldNumFactors = NumExpressions;
			M_free(oldw,"vflags pointers");
		}
		else {
			AS.OldNumFactors = (WORD *)Malloc1(NumExpressions*sizeof(WORD),"numfactors pointers");
			AS.Oldvflags = (WORD *)Malloc1(NumExpressions*sizeof(WORD),"vflags pointers");
			AS.NumOldNumFactors = NumExpressions;
		}
	}
	for ( i = 0; i < NumExpressions; i++ ) {
		AS.OldOnFile[i] = e[i].onfile;
		AS.OldNumFactors[i] = e[i].numfactors;
		AS.Oldvflags[i] = e[i].vflags;
	}
}

/*
 		#] UpdatePositions : 
 		#[ CountTerms1 :		LONG CountTerms1()

		Counts the terms in the current deferred bracket
		Is mainly an adaptation of the routine Deferred in proces.c
*/

LONG CountTerms1(PHEAD0)
{
	GETBIDENTITY
	POSITION oldposition, startposition;
	WORD *t, *m, *mstop, decr, i, *oldwork, retval;
	WORD *oldipointer = AR.CompressPointer;
	WORD oldGetOneFile = AR.GetOneFile, olddeferflag = AR.DeferFlag;
	LONG numterms = 0;
	AR.GetOneFile = 1;
	oldwork = AT.WorkPointer;
    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
	AR.DeferFlag = 0;
	startposition = AR.DefPosition;
/*
		Store old position
*/
	if ( AR.infile->handle >= 0 ) {
		PUTZERO(oldposition);
/*
		SeekFile(AR.infile->handle,&oldposition,SEEK_CUR);
*/
	}
	else {
		SETBASEPOSITION(oldposition,AR.infile->POfill-AR.infile->PObuffer);
		AR.infile->POfill = (WORD *)((UBYTE *)(AR.infile->PObuffer)
					+BASEPOSITION(startposition));
	}
/*
		Look in the CompressBuffer where the bracket contents start
*/
	t = m = AR.CompressBuffer;
	t += *t;
	mstop = t - ABS(t[-1]);
	m++;
	while ( *m != HAAKJE && m < mstop ) m += m[1];
	if ( m >= mstop ) {	/* No deferred action! */
		numterms = 1;
		AR.DeferFlag = olddeferflag;
		AT.WorkPointer = oldwork;
		AR.GetOneFile = oldGetOneFile;
		return(numterms);
	}
	mstop = m + m[1];
	decr = WORDDIF(mstop,AR.CompressBuffer)-1;

	m = AR.CompressBuffer;
	t = AR.CompressPointer;
	i = *m;
	NCOPY(t,m,i);
	AR.TePos = 0;
	AN.TeSuOut = 0;
/*
		Status:
		First bracket content starts at mstop.
		Next term starts at startposition.
		Decompression information is in AR.CompressPointer.
		The outside of the bracket runs from AR.CompressBuffer+1 to mstop.
*/
	AR.CompressPointer = oldipointer;
	for(;;) {
		numterms++;
		retval = GetOneTerm(BHEAD AT.WorkPointer,AR.infile,&startposition,0);
		if ( retval >= 0 ) AR.CompressPointer = oldipointer;
		if ( retval <= 0 ) break;
		t = AR.CompressPointer;
		if ( *t < (1 + decr + ABS(*(t+*t-1))) ) break;
		t++;
		m = AR.CompressBuffer+1;
		while ( m < mstop ) {
			if ( *m != *t ) goto Thatsit;
			m++; t++;
		}
	}
Thatsit:;
/*
		Finished. Reposition the file, restore information and return.
*/
	AT.WorkPointer = oldwork;
	if ( AR.infile->handle >= 0 ) {
/*
		SeekFile(AR.infile->handle,&oldposition,SEEK_SET);
*/
	}
	else {
		AR.infile->POfill = AR.infile->PObuffer + BASEPOSITION(oldposition);
	}
	AR.DeferFlag = olddeferflag;
	AR.GetOneFile = oldGetOneFile;
	return(numterms);
}

/*
 		#] CountTerms1 : 
 		#[ TermsInBracket :		LONG TermsInBracket(term,level)

	The function TermsInBracket_()
	Syntax:
		TermsInBracket_() : The current bracket in a Keep Brackets
		TermsInBracket_(bracket) : This bracket in the current expression
		TermsInBracket_(expression,bracket) : This bracket in the given expression
	All other specifications don't have any effect.
*/

#define CURRENTBRACKET 1
#define BRACKETCURRENTEXPR 2
#define BRACKETOTHEREXPR 3
#define NOBRACKETACTIVE 4

LONG TermsInBracket(PHEAD WORD *term, WORD level)
{
	WORD *t, *tstop, *b, *tt, *n1, *n2;
	int type = 0, i, num;
	LONG numterms = 0;
	WORD *bracketbuffer = AT.WorkPointer;
	t = term; GETSTOP(t,tstop);
	t++; b = bracketbuffer;
	while ( t < tstop ) {
		if ( *t != TERMSINBRACKET ) { t += t[1]; continue; }
		if ( t[1] == FUNHEAD || (
			 t[1] == FUNHEAD+2
			&& t[FUNHEAD] == -SNUMBER
			&& t[FUNHEAD+1] == 0
		 ) ) {
			if ( AC.ComDefer == 0 ) {
				type = NOBRACKETACTIVE;
			}
			else {
				type = CURRENTBRACKET;
			}
			*b = 0;
			break;
		}
		if ( t[FUNHEAD] == -EXPRESSION ) {
			if ( t[FUNHEAD+2] < 0 ) {
				if ( ( t[FUNHEAD+2] <= -FUNCTION ) && ( t[1] == FUNHEAD+3 ) ) {
					type = BRACKETOTHEREXPR;
					*b++ = FUNHEAD+4; *b++ = -t[FUNHEAD+2]; *b++ = FUNHEAD;
					for ( i = 2; i < FUNHEAD; i++ ) *b++ = 0;
					*b++ = 1; *b++ = 1; *b++ = 3;
					break;
				}
				else if ( ( t[FUNHEAD+2] > -FUNCTION ) && ( t[1] == FUNHEAD+4 ) ) {
					type = BRACKETOTHEREXPR;
					tt = t + FUNHEAD+2;
					switch ( *tt ) {
						case -SYMBOL:
							*b++ = 8; *b++ = SYMBOL; *b++ = 4; *b++ = tt[1];
							*b++ = 1; *b++ = 1; *b++ = 1; *b++ = 3;
							break;
						case -SNUMBER:
							if ( tt[1] == 1 ) {
								*b++ = 4; *b++ = 1; *b++ = 1; *b++ = 3;
							}
							else goto IllBraReq;
							break;
						default:
							goto IllBraReq;
					}
					break;
				}
			}
			else if ( ( t[FUNHEAD+2] == (t[1]-FUNHEAD-2) ) &&
					  ( t[FUNHEAD+2+ARGHEAD] == (t[FUNHEAD+2]-ARGHEAD) ) ) {
				type = BRACKETOTHEREXPR;
				tt = t + FUNHEAD + ARGHEAD; num = *tt;
				for ( i = 0; i < num; i++ ) *b++ = *tt++;
				break;
			}
		}
		else {
			if ( t[FUNHEAD] < 0 ) {
				if ( ( t[FUNHEAD] <= -FUNCTION ) && ( t[1] == FUNHEAD+1 ) ) {
					type = BRACKETCURRENTEXPR;
					*b++ = FUNHEAD+4; *b++ = -t[FUNHEAD+2]; *b++ = FUNHEAD;
					for ( i = 2; i < FUNHEAD; i++ ) *b++ = 0;
					*b++ = 1; *b++ = 1; *b++ = 3; *b = 0;
					break;
				}
				else if ( ( t[FUNHEAD] > -FUNCTION ) && ( t[1] == FUNHEAD+2 ) ) {
					type = BRACKETCURRENTEXPR;
					tt = t + FUNHEAD+2;
					switch ( *tt ) {
						case -SYMBOL:
							*b++ = 8; *b++ = SYMBOL; *b++ = 4; *b++ = tt[1];
							*b++ = 1; *b++ = 1; *b++ = 1; *b++ = 3;
							break;
						case -SNUMBER:
							if ( tt[1] == 1 ) {
								*b++ = 4; *b++ = 1; *b++ = 1; *b++ = 3;
							}
							else goto IllBraReq;
							break;
						default:
							goto IllBraReq;
					}
					break;
				}
			}
			else if ( ( t[FUNHEAD] == (t[1]-FUNHEAD) ) &&
					  ( t[FUNHEAD+ARGHEAD] == (t[FUNHEAD]-ARGHEAD) ) ) {
				type = BRACKETCURRENTEXPR;
				tt = t + FUNHEAD + ARGHEAD; num = *tt;
				for ( i = 0; i < num; i++ ) *b++ = *tt++;
				break;
			}
			else {
IllBraReq:;
				MLOCK(ErrorMessageLock);
				MesPrint("Illegal bracket request in termsinbracket_ function.");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
		}
		t += t[1];
	}
	AT.WorkPointer = b;
	if ( AT.WorkPointer + *term +4 > AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MesPrint("Called from termsinbracket_ function.");
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
/*
	We are now in the position to look for the bracket
*/
	switch ( type ) {
		case CURRENTBRACKET:
/*
			The code here should be rather similar to when we pick up
			the contents of the bracket. In our case we only count the
			terms though.
*/
			numterms = CountTerms1(BHEAD0);
			break;
		case BRACKETCURRENTEXPR:
/*
			Not implemented yet.
*/
			MLOCK(ErrorMessageLock);
			MesPrint("termsinbracket_ function currently only handles Keep Brackets.");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		case BRACKETOTHEREXPR:
			MLOCK(ErrorMessageLock);
			MesPrint("termsinbracket_ function currently only handles Keep Brackets.");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		case NOBRACKETACTIVE:
			numterms = 1;
			break;
	}
/*
	Now we have the number in numterms. We replace the function by it.
*/
	n1 = term; n2 = AT.WorkPointer; tstop = n1 + *n1;
	while ( n1 < t ) *n2++ = *n1++;
	i = numterms >> BITSINWORD;
	if ( i == 0 ) {
		*n2++ = LNUMBER; *n2++ = 4; *n2++ = 1; *n2++ = (WORD)(numterms & WORDMASK);
	}
	else {
		*n2++ = LNUMBER; *n2++ = 5; *n2++ = 2;
		*n2++ = (WORD)(numterms & WORDMASK); *n2++ = i;
	}
	n1 += n1[1];
	while ( n1 < tstop ) *n2++ = *n1++;
	AT.WorkPointer[0] = n2 - AT.WorkPointer;
	AT.WorkPointer = n2;
	if ( Generator(BHEAD n1,level) < 0 ) {
		AT.WorkPointer = bracketbuffer;
		MLOCK(ErrorMessageLock);
		MesPrint("Called from termsinbracket_ function.");
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
/*
	Finished. Reset things and return.
*/
	AT.WorkPointer = bracketbuffer;
	return(numterms);
}
/*
 		#] TermsInBracket :		LONG TermsInBracket(term,level) 
	#] Expressions :
*/
