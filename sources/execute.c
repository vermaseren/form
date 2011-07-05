/** @file execute.c
 * 
 *	The routines that start the execution phase of a module.
 *	It also contains the routines for placing the bracket subterm.
 */
/* #[ License : */
/*
 *   Copyright (C) 1984-2010 J.A.M. Vermaseren
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

/*[28sep2005 mt]:*/
#ifdef REMOVEDBY_MT
/*Moved to parallel.c:*/
#ifdef PARALLEL /* [04dec2002 df] */
PFDOLLARS *PFDollars;
#endif
#endif
/*:[28sep2005 mt]*/

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
			case LOCALEXPRESSION:
			case HIDDENLEXPRESSION:
				if ( par ) {
					AC.exprnames->namenode[e_in->node].type = CDELETE;
					AC.DidClean = 1;
					if ( e_in->status != HIDDENLEXPRESSION )
						ClearBracketIndex(e_in-Expressions);
					break;
				}
			case GLOBALEXPRESSION:
			case HIDDENGEXPRESSION:
				if ( par ) {
					e = e_in;
					i = n-1;
					while ( --i >= 0 ) {
						e++;
						if ( e->status == GLOBALEXPRESSION
						|| e->status == LOCALEXPRESSION ) break;
					}
					if ( i >= 0 ) {
						DIFPOS(length,e->onfile,e_in->onfile);
					}
					else {
						if ( AR.outfile->handle < 0 ) {
							SETBASELENGTH(length,TOLONG(AR.outfile->POfull)
							 - TOLONG(AR.outfile->PObuffer)
							 - BASEPOSITION(e_in->onfile));
						}
						else {
							SeekFile(AR.outfile->handle,&(AR.outfile->filesize),SEEK_SET);
							DIFPOS(length,AR.outfile->filesize,e_in->onfile);
						}
					}
					if ( ToStorage(e_in,&length) ) {
						return(MesCall("PopVariables"));
					}
					e_in->status = STOREDEXPRESSION;
					if ( e_in->status != HIDDENGEXPRESSION )
						ClearBracketIndex(e_in-Expressions);
				}
				/* Fall through is intentional */
			case SKIPLEXPRESSION:
			case DROPLEXPRESSION:
			case DROPHLEXPRESSION:
			case DROPGEXPRESSION:
			case DROPHGEXPRESSION:
			case STOREDEXPRESSION:
				if ( e_out != e_in ) {
					node = AC.exprnames->namenode + e_in->node;
					node->number = e_out - Expressions;

					e_out->onfile = e_in->onfile;
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
					e_out->vflags = e_in->vflags;
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
	AC.lPolyFunType = AM.gPolyFunType;
	AC.parallelflag = AC.mparallelflag = AM.gparallelflag;
	AC.SlavePatchSize = AC.mSlavePatchSize = AM.gSlavePatchSize;
	AC.properorderflag = AM.gproperorderflag;
    AC.ThreadBucketSize = AM.gThreadBucketSize;
	AC.ThreadStats = AM.gThreadStats;
	AC.FinalStats = AM.gFinalStats;
	AC.ThreadsFlag = AM.gThreadsFlag;
	AC.ThreadBalancing = AM.gThreadBalancing;
	AC.ThreadSortFileSynch = AM.gThreadSortFileSynch;
	AC.ProcessStats = AM.gProcessStats;
	AC.OldParallelStats = AM.gOldParallelStats;
	AC.IsFortran90 = AM.gIsFortran90;
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
	AC.OutputMode = AM.gOutputMode;
	AC.OutputSpaces = AM.gOutputSpaces;
	AC.OutNumberType = AM.gOutNumberType;
	AR.SortType = AC.SortType = AM.gSortType;
	AC.ShortStatsMax = AM.gShortStatsMax;
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
	AM.gOutputMode = AC.OutputMode;
	AM.gOutputSpaces = AC.OutputSpaces;
	AM.gOutNumberType = AC.OutNumberType;
	AM.gfunpowers = AC.funpowers;
	AM.gPolyFun = AC.lPolyFun;
	AM.gPolyFunType = AC.lPolyFunType;
	AM.gparallelflag = AC.parallelflag;
	AM.gSlavePatchSize = AC.SlavePatchSize;
	AM.gproperorderflag = AC.properorderflag;
    AM.gThreadBucketSize = AC.ThreadBucketSize;
	AM.gThreadStats = AC.ThreadStats;
	AM.gFinalStats = AC.FinalStats;
	AM.gThreadsFlag = AC.ThreadsFlag;
	AM.gThreadBalancing = AC.ThreadBalancing;
	AM.gThreadSortFileSynch = AC.ThreadSortFileSynch;
	AM.gProcessStats = AC.ProcessStats;
	AM.gOldParallelStats = AC.OldParallelStats;
	AM.gIsFortran90 = AC.IsFortran90;
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
				e->status = DROPPEDEXPRESSION;
				ClearBracketIndex(j);
				e->bracketinfo = e->newbracketinfo; e->newbracketinfo = 0;
				if ( e->replace >= 0 ) {
					Expressions[e->replace].replace = -1;
					AC.exprnames->namenode[e->node].number = e->replace;
					e->replace = -1;
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
				e->status = HIDDENLEXPRESSION;
				break;
			case INTOHIDEGEXPRESSION:
				e->status = HIDDENGEXPRESSION;
				break;
			default:
				ClearBracketIndex(j);
				break;
		}
	}
}

/*
 		#] TestDrop : 
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
#ifdef PARALLEL
	/*See comments to PF_PotModDollars in parallel.c*/
	PF_markPotModDollars();	
#endif
/*
		AC.mparallelflag was set to PARALLELFLAG in IniModule.
		It can be set to NOPARALLEL_MOPT or PARALLEL_MOPT (module option)
		by the compiler. If AC.mparallelflag contains PARALLEL_MOPT,
		it must be set to PARALLELFLAG, if AM.hparallelflag is PARALLELFLAG:
*/
	if ( ( AC.parallelflag == PARALLELFLAG ) ||
		( ( AC.mparallelflag & PARALLEL_MOPT ) != 0 ) ) {
		if( ( AC.mparallelflag & NOPARALLEL_MOPT ) == 0 ) {
			if( AM.hparallelflag != PARALLELFLAG ) {
				AC.mparallelflag |= ( AC.parallelflag = AM.hparallelflag );
#ifdef PARALLEL
				if ( AC.OldParallelStats )
				MesPrint("WARNING!: $ use in table - module %l is forced to run in sequential mode.", AC.CModule);
#endif
			}
			else {
				AC.mparallelflag = PARALLELFLAG;
			}
		}
	}
	else {
		AC.mparallelflag |= AC.parallelflag;
	}

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
	if ( AC.NumLabels > 0 ) {
		for ( i = 0; i < AC.NumLabels; i++ ) {
			if ( AC.Labels[i] < 0 ) {
				MesPrint(" -->Label %s missing",AC.LabelNames[i]);
				RetCode = 1;
			}
		}
	}
	if ( AC.dolooplevel > 0 ) {
		MesPrint(" %d enddo statement(s) missing",AC.dolooplevel);
		RetCode = 1;
	}
	if ( RetCode ) return(RetCode);
	AR.Cnumlhs = cbuf[AM.rbufnum].numlhs;

/*[28sep2005 mt]:*/
/*This code is never used*/
#ifdef REMOVEDBY_MT
/*
	@@@@@@@@@@@@@@@@ can be removed? [03dec2002 df]
	Here we invalidate caches of the slaves for all dollars changed by the
	preprocessor. They are in PPchangeddollars. there are NumPPchangeddollars
	of them. It is an array of WORD.
	This is finished with: NumPPchangeddollars = 0;
*/

#ifdef PARALLEL
	if ( AC.mparallelflag == PARALLELFLAG ) {
	  if ( PF.me == 0 ) {	 
/*
			maybe they are not needed at all for the moment or 
			should be combined from all slaves and again distributed
*/
	  } 
	  else {
		M_free(PPchangeddollars, "kill PPchangeddollars list");
			/* PPchangeddollars changed to AP.ChDollarList.lijst because AIX C compiler complained. MF 30/07/2003 */
		AP.ChDollarList.lijst = NULL;
		NumPPchangeddollars = 0;			 
	  } 
	}  
#endif /* PARALLEL */
#endif
/*:[28sep2005 mt]*/

	if ( ( AS.ExecMode = par ) == GLOBALMODULE ) AS.ExecMode = 0;
/*
	Now we compare whether all elements of PotModdollars are contained in
	ModOptdollars. If not, we may not run parallel.
*/
#ifdef PARALLEL
	if ( NumPotModdollars > 0 && AC.mparallelflag == PARALLELFLAG ) {
	  if ( NumPotModdollars > NumModOptdollars ) 
		AC.mparallelflag = NOPARALLEL_DOLLAR;
	  else 
		for ( i = 0; i < NumPotModdollars; i++ ) {
		  for ( j = 0; j < NumModOptdollars; j++ ) 
			if ( PotModdollars[i] == ModOptdollars[j].number ) break;
		  if ( j >= NumModOptdollars ) {
			AC.parallelflag = NOPARALLEL_DOLLAR;
			break;
		  }
		}
	}
#endif
#ifdef WITHPTHREADS
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
	if ( AC.partodoflag > 0 || ( NumPotModdollars > 0 && AC.mparallelflag == PARALLELFLAG ) ) {
	  if ( NumPotModdollars > NumModOptdollars ) {
		AC.mparallelflag = NOPARALLEL_DOLLAR;
		AS.MultiThreaded = 0;
		AC.partodoflag = 0;
	  }
	  else {
		for ( i = 0; i < NumPotModdollars; i++ ) {
		  for ( j = 0; j < NumModOptdollars; j++ ) 
			if ( PotModdollars[i] == ModOptdollars[j].number ) break;
		  if ( j >= NumModOptdollars ) {
			AC.mparallelflag = NOPARALLEL_DOLLAR;
			AS.MultiThreaded = 0;
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
				AC.mparallelflag = NOPARALLEL_DOLLAR;
				AS.MultiThreaded = 0;
				AC.partodoflag = 0;
				break;
		  }
		}
	  }
	}
	else if ( ( AC.mparallelflag & NOPARALLEL_USER ) != 0 ) {
		AS.MultiThreaded = 0;
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
#ifdef PARALLEL 
	if ( ( AC.mparallelflag & NOPARALLEL_DOLLAR ) != 0 ) {
		if ( AC.mparallelflag == NOPARALLEL_DOLLAR )	
			if ( AC.OldParallelStats )
			MesPrint("WARNING!: $-variables - module %l is forced to run in sequential mode.", AC.CModule);
	}
	if ( par == STOREMODULE ){
		if ( AC.mparallelflag == PARALLELFLAG )
			if ( AC.OldParallelStats )
			MesPrint("WARNING!: store module - module %l is forced to run in sequential mode.", AC.CModule);
		AC.mparallelflag = NOPARALLEL_STORE;
	}
/*[20oct2009 mt]:*/
	if ( ( AC.NumberOfRhsExprInModule > 0 ) &&
			( AC.mparallelflag == PARALLELFLAG ) ) {
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
			AC.mparallelflag = NOPARALLEL_RHS;
			if ( AC.OldParallelStats )
			MesPrint("WARNING!: RHS expression names - module %l is forced to run in sequential mode.", AC.CModule);
		}
	}
/*:[20oct2009 mt]*/
#endif
	AR.SortType = AC.SortType;
	if ( AC.SetupFlag ) WriteSetup();
	if ( AC.NamesFlag || AC.CodesFlag ) WriteLists();
	if ( par == GLOBALMODULE ) MakeGlobal();
	if ( RevertScratch() ) return(-1);
/*[20oct2009 mt]:*/
#ifdef PARALLEL
	AC.partodoflag = 0;
	if ( PF.numtasks >= 3 ) {
		for ( i = 0; i < NumExpressions; i++ ) {
			if ( Expressions[i].partodo > 0 ) { AC.partodoflag = 1; break; }
		}
	}
	else {
		for ( i = 0; i < NumExpressions; i++ ) {
			Expressions[i].partodo = 0;
		}
	}
#endif
	if ( AC.ncmod ) SetMods();
/*
	Now the actual execution
*/
#ifdef PARALLEL
	AS.printflag = 1;
#endif
	if ( AP.preError == 0 && ( Processor() || WriteAll() ) ) RetCode = -1;
#ifdef PARALLEL
	AS.printflag = 0;
#endif
/*
	That was it. Next is cleanup.
*/
	if ( AC.ncmod ) UnSetMods();
	AS.MultiThreaded = oldmultithreaded;
	TableReset();

/*[28sep2005 mt]:*/
#ifdef PARALLEL
	/*Here there was a long code written by df in December 2002. I re-write
	it completely and moved it to PF_mkDollarsParallel() in paralle.c:*/
	if(NumPotModdollars > 0)
		if( (RetCode=PF_mkDollarsParallel())!=0 )
			return(RetCode);
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
#ifdef WITHPTHREADS
	AC.numpfirstnum = 0;
#endif
	AC.DidClean = 0;
	AC.PolyRatFunChanged = 0;
	TestDrop();
	if ( par == STOREMODULE || par == CLEARMODULE ) {
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
			AM.gSlavePatchSize = AM.hSlavePatchSize;
			AM.gparallelflag = AM.hparallelflag;
			IniVars();
		}
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
	WORD *bbb = 0, *bind, *binst = 0, bwild = 0;
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
		if ( *b == INDEX ) { bwild = 1; bbb = b+2; binst = b + b[1]; break; }
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

	while ( t < tStop && ( bf < bStop || bwild ) ) {
		b = bf;
		while ( b < bStop && *b != *t ) { b += b[1]; }
		i = t[1];
		if ( *t >= FUNCTION ) { /* We are in function territory */
			if ( b < bStop && *b == *t ) goto FunBrac;
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
					else *t2++ = *t++;
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
				if ( *b == VECTOR && b < bStop ) {
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
					*t2++ = *t++; *t2++ = *t++;
nextvec:;
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
				while ( t < s2 && ( bb < s1 || bwild ) ) {
					while ( bb < s1 && ( *bb < *t ||
					( *bb == *t && bb[1] < t[1] ) ) ) bb += 3;
					if ( *bb == *t && bb[1] == t[1] && bb < s1 ) {
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
					*t2++ = *t++; *t2++ = *t++; *t2++ = *t++;
nextdot:;
				}
				while ( t < s2 ) *t2++ = *t++;
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
					while ( *bb < *t && bb < s1 ) bb += 2;
					if ( bb >= s1 ) break;
					if ( *bb == *t ) { *t1++ = *t++; *t1++ = *t++; }
					else { *t2++ = *t++; *t2++ = *t++; } 
				}
				while ( t < s2 ) *t2++ = *t++;
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
		ind = FindInIndex(num1,&AR.StoreData,0);
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
		ind = FindInIndex(num1,&AR.StoreData,0);
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
		Routine is called from Normalize. Hence it should be thread-safe
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
		if ( ( renumber = GetTable(num,&position) ) == 0 ) {
			MesCall("GetFirstBracket");
			SETERROR(-1)
		}
		if ( GetFromStore(term,&position,renumber,&numword,num) < 0 ) {
			MesCall("GetFirstBracket");
			SETERROR(-1)
		}
#ifdef WITHPTHREADS
		M_free(renumber->symb.lo,"VarSpace");
		M_free(renumber,"Renumber");
#endif
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
 		#[ UpdatePositions :
*/

void UpdatePositions()
{
	EXPRESSIONS e = Expressions;
	POSITION *old;
	int i;
	if ( NumExpressions > 0 &&
		 ( AS.OldOnFile == 0 || AS.NumOldOnFile < NumExpressions ) ) {
		if ( AS.OldOnFile ) {
			old = AS.OldOnFile;
			AS.OldOnFile = (POSITION *)Malloc1(NumExpressions*sizeof(POSITION),"file pointers");
			for ( i = 0; i < AS.NumOldOnFile; i++ ) AS.OldOnFile[i] = old[i];
			AS.NumOldOnFile = NumExpressions;
			M_free(old,"proces file pointers");
		}
		else {
			AS.OldOnFile = (POSITION *)Malloc1(NumExpressions*sizeof(POSITION),"file pointers");
			AS.NumOldOnFile = NumExpressions;
		}
	}
	for ( i = 0; i < NumExpressions; i++ ) { AS.OldOnFile[i] = e[i].onfile; }
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
/*
		if ( AR.infile->handle >= 0 ) {
			SeekFile(AR.infile->handle,&startposition,SEEK_SET);
		}
		else { AR.infile->POfill = (WORD *)((UBYTE *)(AR.infile->PObuffer) + BASEPOSITION(startposition)); }
		retval = GetOneTerm(BHEAD AT.WorkPointer,AR.infile->handle);
		if ( AR.infile->handle >= 0 ) {
			SeekFile(AR.infile->handle,&startposition,SEEK_CUR);
		}
		else { SETBASEPOSITION(startposition,(UBYTE *)(AR.infile->POfill)-(UBYTE *)(AR.infile->PObuffer)); }
*/
		if ( retval <= 0 ) break;

		AR.CompressPointer = oldipointer;
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
