/** @file proces.c
 * 
 *  Contains the central terms processor routines. This is the core of
 *	the virtual machine. All other files are to help these routines.
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
  	#[ Includes : proces.c
*/

#include "form3.h"

WORD printscratch[2];

/*
  	#] Includes : 
	#[ Processor :
 		#[ Processor :			WORD Processor()
*/
/**
 *	This is the central processor.
 *	It accepts a stream of Expressions which is accessed by calls to GetTerm.
 *	The expressions reside either in AR.infile or AR.hidefile
 *	The definitions of an expression are seen as an id-statement, so the
 *	primary Expressions should be written to the system of scratch files
 *	as single terms with an expression pointer. Each expression is terminated
 *	with a zero and the whole is terminated by two zeroes.
 *
 *	The routine DoExecute should determine whether results are to be
 *	printed, should revert the scratch I/O directions etc.
 *	In principle it is DoExecute that calls Processor.
 *
 *	@return if everything OK: 0. Otherwise error. The preprocessor
 *	        may continue with compilation though. Really fatal errors should
 *	        return on the spot by calling Terminate.
 */
 
WORD Processor()
{
	GETIDENTITY
	WORD *term, *t, i, retval = 0, size;
	EXPRESSIONS e;
	POSITION position;
	WORD last, LastExpression, fromspectator;
	LONG dd = 0;
	CBUF *C = cbuf+AC.cbufnum;
	int firstterm;
	CBUF *CC = cbuf+AT.ebufnum;
	WORD **w, *cpo, *cbo;
	FILEHANDLE *curfile, *oldoutfile = AR.outfile;
	WORD oldBracketOn = AR.BracketOn;
	WORD *oldBrackBuf = AT.BrackBuf;
	WORD oldbracketindexflag = AT.bracketindexflag;
#ifdef WITHPTHREADS
	int OldMultiThreaded = AS.MultiThreaded, Oldmparallelflag = AC.mparallelflag;
#endif
	if ( CC->numrhs > 0 || CC->numlhs > 0 ) {
		if ( CC->rhs ) {
			w = CC->rhs; i = CC->numrhs;
			do { *w++ = 0; } while ( --i > 0 );
		}
		if ( CC->lhs ) {
			w = CC->lhs; i = CC->numlhs;
			do { *w++ = 0; } while ( --i > 0 );
		}
		CC->numlhs = CC->numrhs = 0;
		ClearTree(AT.ebufnum);
		CC->Pointer = CC->Buffer;
	}

	if ( NumExpressions == 0 ) return(0);
	AR.expflags = 0;
	AR.CompressPointer = AR.CompressBuffer;
	AR.NoCompress = AC.NoCompress;
	term = AT.WorkPointer;
	if ( ( (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer) ) > AT.WorkTop ) return(MesWork());
	UpdatePositions();
	C->rhs[C->numrhs+1] = C->Pointer;
	AR.KeptInHold = 0;
	if ( AC.CollectFun ) AR.DeferFlag = 0;
	AR.outtohide = 0;
	AN.PolyFunTodo = 0;
#ifdef HIDEDEBUG
	MesPrint("Status at the start of Processor (HideLevel = %d)",AC.HideLevel);
	for ( i = 0; i < NumExpressions; i++ ) {
		e = Expressions+i;
		ExprStatus(e);
	}
#endif
/*
		Next determine the last expression. This is used for removing the
		input file when the final stage of the sort of this expression is
		reached. That can save up to 1/3 in disk space.
*/
	for ( i = NumExpressions-1; i >= 0; i-- ) {
		e = Expressions+i;
		if ( e->status == LOCALEXPRESSION || e->status == GLOBALEXPRESSION
		|| e->status == HIDELEXPRESSION || e->status == HIDEGEXPRESSION
		|| e->status == SKIPLEXPRESSION || e->status == SKIPGEXPRESSION
		|| e->status == UNHIDELEXPRESSION || e->status == UNHIDEGEXPRESSION
		|| e->status == INTOHIDELEXPRESSION || e->status == INTOHIDEGEXPRESSION
		) break;
	}
	last = i;
	for ( i = NumExpressions-1; i >= 0; i-- ) {
		AS.OldOnFile[i] = Expressions[i].onfile;
		AS.OldNumFactors[i] = Expressions[i].numfactors;
/*		AS.Oldvflags[i] = e[i].vflags; */
		AS.Oldvflags[i] = Expressions[i].vflags;
		Expressions[i].vflags &= ~(ISUNMODIFIED|ISZERO);
	}
#ifdef WITHPTHREADS
/*
		When we run with threads we have to make sure that all local input
		buffers are pointed correctly. Of course this isn't needed if we
		run on a single thread only.
*/
	if ( AC.partodoflag && AM.totalnumberofthreads > 1 ) {
		AS.MultiThreaded = 1; AC.mparallelflag = PARALLELFLAG;
	}
	if ( AS.MultiThreaded && AC.mparallelflag == PARALLELFLAG ) {
		SetWorkerFiles();
	}
/*
		We start with running the expressions with expr->partodo in parallel.
		The current model is: give each worker an expression. Wait for 
		workers to finish and tell them where to write.
		Then give them a new expression. Workers may have to wait for each
		other. This is also the case with the last one.
*/
	if ( AS.MultiThreaded && AC.mparallelflag == PARALLELFLAG ) {
		if ( InParallelProcessor() ) {
			retval = 1;
		}
		AS.MultiThreaded = OldMultiThreaded;
		AC.mparallelflag = Oldmparallelflag;
	}
#endif
#ifdef WITHMPI
 	if ( AC.RhsExprInModuleFlag && PF.rhsInParallel && (AC.mparallelflag == PARALLELFLAG || AC.partodoflag) ) {
		if ( PF_BroadcastRHS() ) {
			retval = -1;
		}
	}
	PF.exprtodo = -1; /* This means, the slave does not perform inparallel */
	if ( AC.partodoflag > 0 ) {
		if ( PF_InParallelProcessor() ) {
			retval = -1;
		}
	}
#endif
	for ( i = 0; i < NumExpressions; i++ ) {
#ifdef INNERTEST
		if ( AC.InnerTest ) {
			if ( StrCmp(AC.TestValue,(UBYTE *)INNERTEST) == 0 ) {
				MesPrint("Testing(Processor): value = %s",AC.TestValue);
			}
		}
#endif
		e = Expressions+i;
#ifdef WITHPTHREADS
		if ( AC.partodoflag > 0 && e->partodo > 0 && AM.totalnumberofthreads > 2 ) {
			e->partodo = 0;
			continue;
		}
#endif
#ifdef WITHMPI
		if ( AC.partodoflag > 0 && e->partodo > 0 && PF.numtasks > 2 ) {
			e->partodo = 0;
			continue;
		}
#endif
		AS.CollectOverFlag = 0;
		AR.expchanged = 0;
		if ( i == last ) LastExpression = 1;
		else             LastExpression = 0;
		if ( e->inmem ) {
/*
			#[ in memory : Memory allocated by poly.c only thusfar.
			               Here GetTerm cannot work.
			               For the moment we ignore this for parallelization.
*/
			WORD j;

			AR.GetFile = 0;
			SetScratch(AR.infile,&(e->onfile));
			if ( GetTerm(BHEAD term) <= 0 ) {
				MesPrint("(1) Expression %d has problems in scratchfile",i);
				retval = -1;
				break;
			}
			term[3] = i;
			AR.CurExpr = i;
			SeekScratch(AR.outfile,&position);
			e->onfile = position;
			if ( PutOut(BHEAD term,&position,AR.outfile,0) < 0 ) goto ProcErr;
			AR.DeferFlag = AC.ComDefer;
			NewSort(BHEAD0);
			AN.ninterms = 0;
			t = e->inmem;
			while ( *t ) {
				for ( j = 0; j < *t; j++ ) term[j] = t[j];
				t += *t;
				AN.ninterms++; dd = AN.deferskipped;
				if ( AC.CollectFun && *term <= (AM.MaxTer/(2*(LONG)(sizeof(WORD)))) ) {
					if ( GetMoreFromMem(term,&t) ) {
						LowerSortLevel(); goto ProcErr;
					}
				}
				AT.WorkPointer = term + *term;
				AN.RepPoint = AT.RepCount + 1;
				AN.IndDum = AM.IndDum;
				AR.CurDum = ReNumber(BHEAD term);
				if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
				if ( AN.ncmod ) {
					if ( ( AC.modmode & ALSOFUNARGS ) != 0 ) MarkDirty(term,DIRTYFLAG);
					else if ( AR.PolyFun ) PolyFunDirty(BHEAD term);
				}
			    else if ( AC.PolyRatFunChanged ) PolyFunDirty(BHEAD term);
				if ( Generator(BHEAD term,0) ) {
					LowerSortLevel(); goto ProcErr;
				}
				AN.ninterms += dd;
			}
			AN.ninterms += dd;
			if ( EndSort(BHEAD AM.S0->sBuffer,0) < 0 ) goto ProcErr;
			if ( AM.S0->TermsLeft ) e->vflags &= ~ISZERO;
			else e->vflags |= ISZERO;
			if ( AR.expchanged == 0 ) e->vflags |= ISUNMODIFIED;
				if ( AM.S0->TermsLeft ) AR.expflags |= ISZERO;
			if ( AR.expchanged ) AR.expflags |= ISUNMODIFIED;
			AR.GetFile = 0;
/*
			#] in memory : 
*/
		}
		else {
			AR.CurExpr = i;
			switch ( e->status ) {
			case UNHIDELEXPRESSION:
			case UNHIDEGEXPRESSION:
				AR.GetFile = 2;
#ifdef WITHMPI
	            if ( PF.me == MASTER ) SetScratch(AR.hidefile,&(e->onfile));
#else
				SetScratch(AR.hidefile,&(e->onfile));
				AR.InHiBuf = AR.hidefile->POfull-AR.hidefile->POfill;
#ifdef HIDEDEBUG
				MesPrint("Hidefile: onfile: %15p, POposition: %15p, filesize: %15p",&(e->onfile)
				,&(AR.hidefile->POposition),&(AR.hidefile->filesize));
				MesPrint("Set hidefile to buffer position %l/%l; AR.InHiBuf = %l"
					,(AR.hidefile->POfill-AR.hidefile->PObuffer)*sizeof(WORD)
					,(AR.hidefile->POfull-AR.hidefile->PObuffer)*sizeof(WORD)
					,AR.InHiBuf
				);
#endif
#endif
				curfile = AR.hidefile;
				goto commonread;
			case INTOHIDELEXPRESSION:
			case INTOHIDEGEXPRESSION:
				AR.outtohide = 1;
/*
				BugFix 12-feb-2016
				This may not work when the file is open and we move around.
				AR.hidefile->POfill = AR.hidefile->POfull;
*/
				SetEndHScratch(AR.hidefile,&position);
				/* fall through */
			case LOCALEXPRESSION:
			case GLOBALEXPRESSION:
				AR.GetFile = 0;
/*[20oct2009 mt]:*/
#ifdef WITHMPI
				if( ( PF.me == MASTER ) || (PF.mkSlaveInfile) )
#endif
                SetScratch(AR.infile,&(e->onfile));
/*:[20oct2009 mt]*/
				curfile = AR.infile;
commonread:;
#ifdef WITHMPI
				if ( PF_Processor(e,i,LastExpression) ) {
					MesPrint("Error in PF_Processor");
					goto ProcErr;
				}
/*[20oct2009 mt]:*/
				if ( AC.mparallelflag != PARALLELFLAG ){
					if(PF.me != MASTER)
						break;
#endif
/*:[20oct2009 mt]*/
				if ( GetTerm(BHEAD term) <= 0 ) {
#ifdef HIDEDEBUG
					MesPrint("Error condition 1a");
					ExprStatus(e);
#endif
					MesPrint("(2) Expression %d has problems in scratchfile(process)",i);
					retval = -1;
					break;
				}
				term[3] = i;
				if ( term[5] < 0 ) {	/* Fill with spectator */
					fromspectator = -term[5];
					PUTZERO(AM.SpectatorFiles[fromspectator-1].readpos);
					term[5] = AC.cbufnum;
				}
				else fromspectator = 0;
				if ( AR.outtohide ) {
					SeekScratch(AR.hidefile,&position);
					e->onfile = position;
					if ( PutOut(BHEAD term,&position,AR.hidefile,0) < 0 ) goto ProcErr;
				}
				else {
					SeekScratch(AR.outfile,&position);
					e->onfile = position;
					if ( PutOut(BHEAD term,&position,AR.outfile,0) < 0 ) goto ProcErr;
				}
				AR.DeferFlag = AC.ComDefer;
				AR.Eside = RHSIDE;
				if ( ( e->vflags & ISFACTORIZED ) != 0 ) {
					AR.BracketOn = 1;
					AT.BrackBuf = AM.BracketFactors;
					AT.bracketindexflag = 1;
				}
				if ( AT.bracketindexflag > 0 ) OpenBracketIndex(i);
#ifdef WITHPTHREADS
				if ( AS.MultiThreaded && AC.mparallelflag == PARALLELFLAG ) {
					if ( ThreadsProcessor(e,LastExpression,fromspectator) ) {
						MesPrint("Error in ThreadsProcessor");
						goto ProcErr;
					}
					if ( AR.outtohide ) {
						AR.outfile = oldoutfile;
						AR.hidefile->POfull = AR.hidefile->POfill;
					}
				}
				else
#endif
				{
					NewSort(BHEAD0);
					AR.MaxDum = AM.IndDum;
					AN.ninterms = 0;
					for(;;) {
					 if ( fromspectator ) size = GetFromSpectator(term,fromspectator-1);
					 else                 size = GetTerm(BHEAD term);
					 if ( size <= 0 ) break;
					 SeekScratch(curfile,&position);
					 if ( ( e->vflags & ISFACTORIZED ) != 0 && term[1] == HAAKJE ) {
					  StoreTerm(BHEAD term);
					 }
					 else {
					  AN.ninterms++; dd = AN.deferskipped;
					  if ( AC.CollectFun && *term <= (AM.MaxTer/(2*(LONG)(sizeof(WORD)))) ) {
						if ( GetMoreTerms(term) < 0 ) {
						  LowerSortLevel(); goto ProcErr;
						}
					    SeekScratch(curfile,&position);
					  }
					  AT.WorkPointer = term + *term;
					  AN.RepPoint = AT.RepCount + 1;
					  if ( AR.DeferFlag ) {
						AN.IndDum = Expressions[AR.CurExpr].numdummies + AM.IndDum;
						AR.CurDum = AN.IndDum;
					  }
					  else {
						AN.IndDum = AM.IndDum;
						AR.CurDum = ReNumber(BHEAD term);
					  }
					  if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
					  if ( AN.ncmod ) {
						if ( ( AC.modmode & ALSOFUNARGS ) != 0 ) MarkDirty(term,DIRTYFLAG);
						else if ( AR.PolyFun ) PolyFunDirty(BHEAD term);
					  }
					  else if ( AC.PolyRatFunChanged ) PolyFunDirty(BHEAD term);
					  if ( ( AR.PolyFunType == 2 ) && ( AC.PolyRatFunChanged == 0 )
						&& ( e->status == LOCALEXPRESSION || e->status == GLOBALEXPRESSION ) ) {
						PolyFunClean(BHEAD term);
					  }
					  if ( Generator(BHEAD term,0) ) {
						LowerSortLevel(); goto ProcErr;
					  }
					  AN.ninterms += dd;
					 }
					 SetScratch(curfile,&position);
					 if ( AR.GetFile == 2 ) {
						AR.InHiBuf = (curfile->POfull-curfile->PObuffer)
							-DIFBASE(position,curfile->POposition)/sizeof(WORD);
					 }
					 else {
						AR.InInBuf = (curfile->POfull-curfile->PObuffer)
							-DIFBASE(position,curfile->POposition)/sizeof(WORD);
					 }
					}
					AN.ninterms += dd;
					if ( LastExpression ) {
						UpdateMaxSize();
						if ( AR.infile->handle >= 0 ) {
							CloseFile(AR.infile->handle);
							AR.infile->handle = -1;
							remove(AR.infile->name);
							PUTZERO(AR.infile->POposition);
						}
						AR.infile->POfill = AR.infile->POfull = AR.infile->PObuffer;
					}
					if ( AR.outtohide ) AR.outfile = AR.hidefile;
					if ( EndSort(BHEAD AM.S0->sBuffer,0) < 0 ) goto ProcErr;
					if ( AR.outtohide ) {
						AR.outfile = oldoutfile;
						AR.hidefile->POfull = AR.hidefile->POfill;
					}
					e->numdummies = AR.MaxDum - AM.IndDum;
					UpdateMaxSize();
				}
				AR.BracketOn = oldBracketOn;
				AT.BrackBuf = oldBrackBuf;
				if ( ( e->vflags & TOBEFACTORED ) != 0 ) {
					poly_factorize_expression(e);
				}
				else if ( ( ( e->vflags & TOBEUNFACTORED ) != 0 )
				 && ( ( e->vflags & ISFACTORIZED ) != 0 ) ) {
					poly_unfactorize_expression(e);
				}
				AT.bracketindexflag = oldbracketindexflag;
				if ( AM.S0->TermsLeft )   e->vflags &= ~ISZERO;
				else                      e->vflags |= ISZERO;
				if ( AR.expchanged == 0 ) e->vflags |= ISUNMODIFIED;
				if ( AM.S0->TermsLeft ) AR.expflags |= ISZERO;
				if ( AR.expchanged )    AR.expflags |= ISUNMODIFIED;
				AR.GetFile = 0;
				AR.outtohide = 0;
/*[20oct2009 mt]:*/
#ifdef WITHMPI
				}
#endif
#ifdef WITHPTHREADS
				if ( e->status == INTOHIDELEXPRESSION ||
					 e->status == INTOHIDEGEXPRESSION ) {
					SetHideFiles();
				}
#endif
				break;
			case SKIPLEXPRESSION:
			case SKIPGEXPRESSION:
/*
				This can be greatly improved of course by file-to-file copy.
*/
#ifdef WITHMPI
				if ( PF.me != MASTER ) break;
#endif
				AR.GetFile = 0;
				SetScratch(AR.infile,&(e->onfile));
				if ( GetTerm(BHEAD term) <= 0 ) {
#ifdef HIDEDEBUG
					MesPrint("Error condition 1b");
					ExprStatus(e);
#endif
					MesPrint("(3) Expression %d has problems in scratchfile",i);
					retval = -1;
					break;
				}
				term[3] = i;
				AR.DeferFlag = 0;
				SeekScratch(AR.outfile,&position);
				e->onfile = position;
				*AM.S0->sBuffer = 0; firstterm = -1;
				do {
					WORD *oldipointer = AR.CompressPointer;
					WORD *comprtop = AR.ComprTop;
					AR.ComprTop = AM.S0->sTop;
					AR.CompressPointer = AM.S0->sBuffer;
					if ( firstterm > 0 ) {
						if ( PutOut(BHEAD term,&position,AR.outfile,1) < 0 ) goto ProcErr;
					}
					else if ( firstterm < 0 ) {
						if ( PutOut(BHEAD term,&position,AR.outfile,0) < 0 ) goto ProcErr;
						firstterm++;
					}
					else {
						if ( PutOut(BHEAD term,&position,AR.outfile,-1) < 0 ) goto ProcErr;
						firstterm++;
					}
					AR.CompressPointer = oldipointer;
					AR.ComprTop = comprtop;
				} while ( GetTerm(BHEAD term) );
				if ( FlushOut(&position,AR.outfile,1) ) goto ProcErr;
				UpdateMaxSize();
				break;
			case HIDELEXPRESSION:
			case HIDEGEXPRESSION:
#ifdef WITHMPI
				if ( PF.me != MASTER ) break;
#endif
				AR.GetFile = 0;
				SetScratch(AR.infile,&(e->onfile));
				if ( GetTerm(BHEAD term) <= 0 ) {
#ifdef HIDEDEBUG
					MesPrint("Error condition 1c");
					ExprStatus(e);
#endif
					MesPrint("(4) Expression %d has problems in scratchfile",i);
					retval = -1;
					break;
				}
				term[3] = i;
				AR.DeferFlag = 0;
				SetEndHScratch(AR.hidefile,&position);
				e->onfile = position;
#ifdef HIDEDEBUG
				if ( AR.hidefile->handle >= 0 ) {
					POSITION possize,pos;
					PUTZERO(possize);
					PUTZERO(pos);
					SeekFile(AR.hidefile->handle,&pos,SEEK_CUR);
					SeekFile(AR.hidefile->handle,&possize,SEEK_END);
					SeekFile(AR.hidefile->handle,&pos,SEEK_SET);
					MesPrint("Processor Hide1: filesize(th) = %12p, filesize(ex) = %12p",&(position),
							&(possize));
					MesPrint("    in buffer: %l",(AR.hidefile->POfill-AR.hidefile->PObuffer)*sizeof(WORD));
				}
#endif
				*AM.S0->sBuffer = 0; firstterm = -1;
				cbo = cpo = AM.S0->sBuffer;
				do {
					WORD *oldipointer = AR.CompressPointer;
					WORD *oldibuffer = AR.CompressBuffer;
					WORD *comprtop = AR.ComprTop;
					AR.ComprTop = AM.S0->sTop;
					AR.CompressPointer = cpo;
					AR.CompressBuffer = cbo;
					if ( firstterm > 0 ) {
						if ( PutOut(BHEAD term,&position,AR.hidefile,1) < 0 ) goto ProcErr;
					}
					else if ( firstterm < 0 ) {
						if ( PutOut(BHEAD term,&position,AR.hidefile,0) < 0 ) goto ProcErr;
						firstterm++;
					}
					else {
						if ( PutOut(BHEAD term,&position,AR.hidefile,-1) < 0 ) goto ProcErr;
						firstterm++;
					}
					cpo = AR.CompressPointer;
					cbo = AR.CompressBuffer;
					AR.CompressPointer = oldipointer;
					AR.CompressBuffer = oldibuffer;
					AR.ComprTop = comprtop;
				} while ( GetTerm(BHEAD term) );
#ifdef HIDEDEBUG
				if ( AR.hidefile->handle >= 0 ) {
					POSITION possize,pos;
					PUTZERO(possize);
					PUTZERO(pos);
					SeekFile(AR.hidefile->handle,&pos,SEEK_CUR);
					SeekFile(AR.hidefile->handle,&possize,SEEK_END);
					SeekFile(AR.hidefile->handle,&pos,SEEK_SET);
					MesPrint("Processor Hide2: filesize(th) = %12p, filesize(ex) = %12p",&(position),
							&(possize));
					MesPrint("    in buffer: %l",(AR.hidefile->POfill-AR.hidefile->PObuffer)*sizeof(WORD));
				}
#endif
				if ( FlushOut(&position,AR.hidefile,1) ) goto ProcErr;
				AR.hidefile->POfull = AR.hidefile->POfill;
#ifdef HIDEDEBUG
				if ( AR.hidefile->handle >= 0 ) {
					POSITION possize,pos;
					PUTZERO(possize);
					PUTZERO(pos);
					SeekFile(AR.hidefile->handle,&pos,SEEK_CUR);
					SeekFile(AR.hidefile->handle,&possize,SEEK_END);
					SeekFile(AR.hidefile->handle,&pos,SEEK_SET);
					MesPrint("Processor Hide3: filesize(th) = %12p, filesize(ex) = %12p",&(position),
							&(possize));
					MesPrint("    in buffer: %l",(AR.hidefile->POfill-AR.hidefile->PObuffer)*sizeof(WORD));
				}
#endif
/*
				Because we direct the e->onfile already to the hide file, we
				need to change the status of the expression. Otherwise the use
				of parts (or the whole) of the expression looks in the infile
				while the position is that of the hide file.
				We choose to get everything from the hide file. On average that
				should give least file activity.
*/
				if ( e->status == HIDELEXPRESSION ) {
					e->status = HIDDENLEXPRESSION;
					AS.OldOnFile[i] = e->onfile;
					AS.OldNumFactors[i] = Expressions[i].numfactors;
				}
				if ( e->status == HIDEGEXPRESSION ) {
					e->status = HIDDENGEXPRESSION;
					AS.OldOnFile[i] = e->onfile;
					AS.OldNumFactors[i] = Expressions[i].numfactors;
				}
#ifdef WITHPTHREADS
				SetHideFiles();
#endif
				UpdateMaxSize();
				break;
			case DROPPEDEXPRESSION:
			case DROPLEXPRESSION:
			case DROPGEXPRESSION:
			case DROPHLEXPRESSION:
			case DROPHGEXPRESSION:
			case STOREDEXPRESSION:
			case HIDDENLEXPRESSION:
			case HIDDENGEXPRESSION:
			case SPECTATOREXPRESSION:
			default:
				break;
		}
		}
		AR.KeptInHold = 0;
	}
	AR.DeferFlag = 0;
	AT.WorkPointer = term;
#ifdef HIDEDEBUG
	MesPrint("Status at the end of Processor (HideLevel = %d)",AC.HideLevel);
	for ( i = 0; i < NumExpressions; i++ ) {
		e = Expressions+i;
		ExprStatus(e);
	}
#endif
	return(retval);
ProcErr:
	AT.WorkPointer = term;
	if ( AM.tracebackflag ) MesCall("Processor");
	return(-1);
}
/*
 		#] Processor : 
 		#[ TestSub :			WORD TestSub(term,level)
*/
/**
 *	TestSub hunts for subexpression pointers.
 *	If one is found its power is given in AN.TeSuOut.
 *	and the returnvalue is 'expressionnumber'.
 *	If the expression number is negative it is an expression on disk.
 *
 *	In addition this routine tries to locate subexpression pointers
 *	in functions. It also notices that action must be taken with any
 *	of the special functions.
 *
 *	@param  term  The term in which TestSub hunts for potential action
 *	@param  level The number of the 'level' in the compiler buffer.
 *	@return The number of the (sub)expression that was encountered.
 *
 *	Other values that are returned are in AN.TeSuOut, AR.TePos, AT.TMbuff,
 *		AN.TeInFun, AN.Frozen, AT.TMaddr 
 *
 *	The level in the compiler buffer is more or less the number of the
 *	statement in the module. Hence it refers to the element in the lhs array.
 *
 *	This routine is one of the most important routines in FORM.
 */

WORD TestSub(PHEAD WORD *term, WORD level)
{
	GETBIDENTITY
	WORD *m, *t, *r, retvalue, funflag, j, oldncmod, nexpr;
	WORD *stop, *t1, *t2, funnum, wilds, tbufnum, stilldirty = 0;
	NESTING n;
	CBUF *C = cbuf+AT.ebufnum;
	LONG isp, i;
	TABLES T;
	COMPARE oldcompareroutine = AR.CompareRoutine;
	WORD oldsorttype = AR.SortType;
ReStart:
	tbufnum = 0; i = 0;
	AT.TMbuff = AM.rbufnum;
	funflag = 0;
	t = term;
	r = t + *t - 1;
	m = r - ABS(*r) + 1;
	t++;
	if ( t < m ) do {
		if ( *t == SUBEXPRESSION ) {
			/*
				Subexpression encountered
				There may be more than one.
				The old strategy was to take the last.
				A newer strategy was to take the lowest power first.
				The current strategy is that we compute the number of terms
				generated by this subexpression and take the minimum of that.
			*/

#ifdef WHICHSUBEXPRESSION

			WORD *tmin = t, AN.nbino;
/*			LONG minval = MAXLONG; */
			LONG minval = -1;
			LONG mm, mnum1 = 1;
			if ( AN.BinoScrat == 0 ) {
				AN.BinoScrat = (UWORD *)Malloc1((AM.MaxTal+2)*sizeof(UWORD),"GetBinoScrat");
			}
#endif
			if ( t[3] ) {
				r = t + t[1];
				while ( AN.subsubveto == 0 &&
							*r == SUBEXPRESSION && r < m && r[3] ) {
#ifdef WHICHSUBEXPRESSION
					mnum1++;
#endif
					if ( r[1] == t[1] && r[2] == t[2] && r[4] == t[4] ) {
						j = t[1] - SUBEXPSIZE;
						t1 = t + SUBEXPSIZE;
						t2 = r + SUBEXPSIZE;
						while ( j > 0 && *t1++ == *t2++ ) j--;
						if ( j <= 0 ) {
							t[3] += r[3];
							if ( t[3] == 0 ) {
								t1 = r + r[1];
								t2 = term + *term;
								*term -= r[1]+t[1];
								r = t;
								while ( t1 < t2 ) *r++ = *t1++;
								goto ReStart;
							}
							else {
								t1 = r + r[1];
								t2 = term + *term;
								*term -= r[1];
								m -= r[1];
								while ( t1 < t2 ) *r++ = *t1++;
								r = t;
							}
						}
					}
#ifdef WHICHSUBEXPRESSION

					else if ( t[2] >= 0 ) {
/*
						Compute Binom(numterms+power-1,power-1)
						We need potentially long arrithmetic.
						That is why we had to allocate AN.BinoScrat
*/
						if ( AN.last1 == t[3] && AN.last2 == cbuf[t[4]].NumTerms[t[2]] + t[3] - 1 ) {
							if ( AN.last3 > minval ) {
								minval = AN.last3; tmin = t;
							}
						}
						else {
						AN.last1 = t[3]; mm = AN.last2 = cbuf[t[4]].NumTerms[t[2]] + t[3] - 1;
						if ( t[3] == 1 ) {
							if ( mm > minval ) {
								minval = mm; tmin = t;
							}
						}
						else if ( t[3] > 0 ) {
							if ( mm > MAXPOSITIVE ) goto TooMuch;
							GetBinom(AN.BinoScrat,&AN.nbino,(WORD)mm,t[3]);
							if ( AN.nbino > 2 ) goto TooMuch;
							if ( AN.nbino == 2 ) {
								mm = AN.BinoScrat[1];
								mm = ( mm << BITSINWORD ) + AN.BinoScrat[0];
							}
							else if ( AN.nbino == 1 ) mm = AN.BinoScrat[0];
							else mm = 0;
							if ( mm > minval ) {
								minval = mm; tmin = t;
							}
						}
						AN.last3 = mm;
						}
					}
#endif
					t = r;
					r += r[1];
				}
#ifdef WHICHSUBEXPRESSION
				if ( mnum1 > 1 && t[2] >= 0 ) {
/*
					To keep the flowcontrol simple we duplicate some code here
*/
					if ( AN.last1 == t[3] && AN.last2 == cbuf[t[4]].NumTerms[t[2]] + t[3] - 1 ) {
						if ( AN.last3 > minval ) {
							minval = AN.last3; tmin = t;
						}
					}
					else {
					AN.last1 = t[3]; mm = AN.last2 = cbuf[t[4]].NumTerms[t[2]] + t[3] - 1;
					if ( t[3] == 1 ) {
						if ( mm > minval ) {
							minval = mm; tmin = t;
						}
					}
					else if ( t[3] > 0 ) {
						if ( mm > MAXPOSITIVE ) {
/*
							We will generate more terms than we can count
*/
TooMuch:;
							MLOCK(ErrorMessageLock);
							MesPrint("Attempt to generate more terms than FORM can count");
							MUNLOCK(ErrorMessageLock);
							Terminate(-1);
						}
						GetBinom(AN.BinoScrat,&AN.nbino,(WORD)mm,t[3]);
						if ( AN.nbino > 2 ) goto TooMuch;
						if ( AN.nbino == 2 ) {
							mm = AN.BinoScrat[1];
							mm = ( mm << BITSINWORD ) + AN.BinoScrat[0];
						}
						else if ( AN.nbino == 1 ) mm = AN.BinoScrat[0];
						else mm = 0;
						if ( mm > minval ) {
							minval = mm; tmin = t;
						}
					}
					AN.last3 = mm;
					}
				}
				t = tmin;
#endif
/*				AR.TePos = 0;  */
				AR.TePos = WORDDIF(t,term);
				AT.TMbuff = t[4];
				if ( t[4] == AM.dbufnum && (t+t[1]) < m && t[t[1]] == DOLLAREXPR2 ) {
					if ( t[t[1]+2] < 0 ) AT.TMdolfac = -t[t[1]+2];
					else {	/* resolve the element number */
						AT.TMdolfac = GetDolNum(BHEAD t+t[1],m)+1;
					}
				}
				else AT.TMdolfac = 0;
				if ( t[3] < 0 ) {
					AN.TeInFun = 1;
					AR.TePos = WORDDIF(t,term);
					return(t[2]);
				}
				else {
					AN.TeInFun = 0;
					AN.TeSuOut = t[3];
				}
				if ( t[2] < 0 ) {
					AN.TeSuOut = -t[3];
					return(-t[2]);
				}
				return(t[2]);
			}
		}
		else if ( *t == EXPRESSION ) {
			WORD *toTMaddr;
			i = -t[2] - 1;
			if ( t[3] < 0 ) {
				AN.TeInFun = 1;
				AR.TePos = WORDDIF(t,term);
				return(i);
			}
			nexpr = t[3];
			toTMaddr = m = AT.WorkPointer;
			AN.Frozen = 0;
/*
			We have to be very careful with respect to setting variables
			like AN.TeInFun, because we may still call Generator and that
			may change those variables. That is why we set them at the
			last moment only.
*/
			j = t[1];
			AT.WorkPointer += j;
			r = t;
			NCOPY(m,r,j);
			r = t + t[1];
			t += SUBEXPSIZE;
			while ( t < r ) {
				if ( *t == FROMBRAC ) {
					WORD *ttstop,*tttstop;
/*
					Note: Convention is that wildcards are done
					after the expression has been picked up. So
					no wildcard substitutions are needed here.
*/
					t += 2;
					AN.Frozen = m = AT.WorkPointer;
/*
					We should check now for subexpressions and if necessary
					we substitute them. Keep in mind: only one term allowed!

					In retrospect (26-jan-2010): take also functions that
					have a dirty flag on
*/
					j = *t; tttstop = t + j;
					GETSTOP(t,ttstop);
					*m++ = j; t++;
					while ( t < ttstop ) {
						if ( *t == SUBEXPRESSION ) break;
						if ( *t >= FUNCTION && ( ( t[2] & DIRTYFLAG ) == DIRTYFLAG ) ) break;
						j = t[1]; NCOPY(m,t,j);
					}
					if ( t < ttstop ) {
/*
						We ran into a subexpression or a function with a
						'dirty' argument. It could also be a $ or
						just e[(a^2)*b]. In all cases we should evaluate
*/
						while ( t < tttstop ) *m++ = *t++;
						*AT.WorkPointer = m-AT.WorkPointer;
						m = AT.WorkPointer;
						AT.WorkPointer = m + *m;
						NewSort(BHEAD0);
						if ( Generator(BHEAD m,AR.Cnumlhs) ) {
							LowerSortLevel(); goto EndTest;
						}
						if ( EndSort(BHEAD m,0) < 0 ) goto EndTest;
						AN.Frozen = m;
						if ( *m == 0 ) {
							*m++ = 4; *m++ = 1; *m++ = 1; *m++ = 3;
						}
						else if ( m[*m] != 0 ) {
							MLOCK(ErrorMessageLock);
							MesPrint("Bracket specification in expression should be one single term");
							MUNLOCK(ErrorMessageLock);
							Terminate(-1);
						}
						else {
							m += *m;
							m -= ABS(m[-1]);
							*m++ = 1; *m++ = 1; *m++ = 3;
							*AN.Frozen = m - AN.Frozen;
						}
					}
					else {
						while ( t < tttstop ) *m++ = *t++;
						*AT.WorkPointer = m-AT.WorkPointer;
						m = AT.WorkPointer;
						AT.WorkPointer = m + *m;
						if ( Normalize(BHEAD m) ) {
							MLOCK(ErrorMessageLock);
							MesPrint("Error while picking up contents of bracket");
							MUNLOCK(ErrorMessageLock);
							Terminate(-1);
						}
						if ( !*m ) {
							*m++ = 4; *m++ = 1; *m++ = 1; *m++ = 3;
						}
						else m += *m;
					}
					AT.WorkPointer = m;
					break;
				}
				t += t[1];
			}
			AN.TeInFun = 0;
			AR.TePos = 0;
			AN.TeSuOut = nexpr;
			AT.TMaddr = toTMaddr;
			return(i);
		}
		else if ( *t >= FUNCTION ) {
            if ( t[0] == EXPONENT ) {
                if ( t[1] == FUNHEAD+4 && t[FUNHEAD] == -SYMBOL &&
                t[FUNHEAD+2] == -SNUMBER && t[FUNHEAD+3] < MAXPOWER
                && t[FUNHEAD+3] > -MAXPOWER ) {
                    t[0] = SYMBOL;
                    t[1] = 4;
                    t[2] = t[FUNHEAD+1];
                    t[3] = t[FUNHEAD+3];
                    r = term + *term;
                    m = t + FUNHEAD+4;
                    t += 4;
                    while ( m < r ) *t++ = *m++;
                    *term = WORDDIF(t,term);
                    goto ReStart;
                }
				else if ( t[1] == FUNHEAD+ARGHEAD+11 && t[FUNHEAD] == ARGHEAD+9
				 && t[FUNHEAD+ARGHEAD] == 9 && t[FUNHEAD+ARGHEAD+1] == DOTPRODUCT
				 && t[FUNHEAD+ARGHEAD+8] == 3
				 && t[FUNHEAD+ARGHEAD+7] == 1
				 && t[FUNHEAD+ARGHEAD+6] == 1
				 && t[FUNHEAD+ARGHEAD+5] == 1
                 && t[FUNHEAD+ARGHEAD+9] == -SNUMBER
				 && t[FUNHEAD+ARGHEAD+10] < MAXPOWER
                 && t[FUNHEAD+ARGHEAD+10] > -MAXPOWER ) {
					t[0] = DOTPRODUCT;
					t[1] = 5;
					t[2] = t[FUNHEAD+ARGHEAD+3];
					t[3] = t[FUNHEAD+ARGHEAD+4];
					t[4] = t[FUNHEAD+ARGHEAD+10];
                    r = term + *term;
                    m = t + FUNHEAD+ARGHEAD+11;
                    t += 5;
                    while ( m < r ) *t++ = *m++;
                    *term = WORDDIF(t,term);
                    goto ReStart;
				}
            }
			funnum = *t;
			if ( *t >= FUNCTION + WILDOFFSET ) funnum -= WILDOFFSET;
			if ( *t == EXPONENT ) {
/*
				Test whether the second argument is an integer
*/
				r = t+FUNHEAD;
				NEXTARG(r)
				if ( *r == -SNUMBER && r[1] < MAXPOWER && r+2 == t+t[1] &&
				t[FUNHEAD] > -FUNCTION && ( t[FUNHEAD] != -SNUMBER
				|| t[FUNHEAD+1] != 0 ) && t[FUNHEAD] != ARGHEAD ) {
				  if ( r[1] == 0 ) {
					if ( t[FUNHEAD] == -SNUMBER && t[FUNHEAD+1] == 0 ) {
						MLOCK(ErrorMessageLock);
						MesPrint("Encountered 0^0. Fatal error.");
						MUNLOCK(ErrorMessageLock);
						SETERROR(-1);
					}
					*t = DUMMYFUN;
/*
						Now mark it clean to avoid further interference.
						Normalize will remove this object.
*/
					t[2] = 0;
				  }
				  else {
					/* Note that the case 0^ is treated in Normalize */

					t1 = AddRHS(AT.ebufnum,1);
					m = t + FUNHEAD;
					if ( *m > 0 ) {
						m += ARGHEAD;
						i = t[FUNHEAD] - ARGHEAD;
						while ( (t1 + i + 10) > C->Top )
							t1 = DoubleCbuffer(AT.ebufnum,t1,9);
						while ( --i >= 0 ) *t1++ = *m++;
					}
					else {
						if ( (t1 + 20) > C->Top )
							t1 = DoubleCbuffer(AT.ebufnum,t1,10);
						ToGeneral(m,t1,1);
						t1 += *t1;
					}
					*t1++ = 0;
					C->rhs[C->numrhs+1] = t1;
					C->Pointer = t1;

					/* No provisions yet for commuting objects */

					C->CanCommu[C->numrhs] = 1;
					*t++ = SUBEXPRESSION;
					*t++ = SUBEXPSIZE;
					*t++ = C->numrhs;
					*t++ = r[1];
					*t++ = AT.ebufnum;
#if SUBEXPSIZE > 5
Important: we may not have enough spots here
#endif
					FILLSUB(t)  /* Important: We have maybe only 5 spots! */
					r += 2;
					m = term + *term;
					do { *t++ = *r++; } while ( r < m );
					*term -= WORDDIF(r,t);
					goto ReStart;
				  }
				}
			}
			else if ( *t == SUMF1 || *t == SUMF2 ) {
/*
				What we are looking for is:
				1-st argument:	Single symbol or index.
				2-nd argument:	Number.
				3-rd argument:	Number.
				(4-th argument):Number.
				One more argument.
				This would activate the summation procedure.
				Note that the initiated recursion here can be done
				without upsetting the regular procedures.
*/
				WORD *tstop, lcounter, lcmin, lcmax, lcinc;
				tstop = t + t[1];
				r = t+FUNHEAD;
				if ( r+6 < tstop && r[2] == -SNUMBER && r[4] == -SNUMBER 
				&& ( ( r[0] == -SYMBOL )
				|| ( r[0] == -INDEX && r[1] >= AM.OffsetIndex
				&& r[3] >= 0 && r[3] < AM.OffsetIndex
				&& r[5] >= 0 && r[5] < AM.OffsetIndex ) ) ) {
					lcounter = r[0] == -INDEX ? -r[1]: r[1]; /* The loop counter */
					lcmin = r[3];
					lcmax = r[5];
					r += 6;
					if ( *r == -SNUMBER && r+2 < tstop ) {
						lcinc = r[1];
						r += 2;
					}
					else lcinc = 1;
					if ( r < tstop && ( ( *r > 0 && (r+*r) == tstop )
					|| ( *r <= -FUNCTION && r+1 == tstop )
					|| ( *r > -FUNCTION && *r < 0 && r+2 == tstop ) ) ) {
						m = AddRHS(AT.ebufnum,1);
						if ( *r > 0 ) {
							i = *r - ARGHEAD;
							r += ARGHEAD;
							while ( (m + i + 10) > C->Top )
								m = DoubleCbuffer(AT.ebufnum,m,11);
							while ( --i >= 0 ) *m++ = *r++;
						}
						else {
							while ( (m + 20) > C->Top )
								m = DoubleCbuffer(AT.ebufnum,m,12);
							ToGeneral(r,m,1);
							m += *m;
						}
						*m++ = 0;
						C->rhs[C->numrhs+1] = m;
						C->Pointer = m;
						m = AT.TMout;
						*m++ = 6;
						if ( *t == SUMF1 ) *m++ = SUMNUM1;
						else			   *m++ = SUMNUM2;
						*m++ = lcounter;
						*m++ = lcmin;
						*m++ = lcmax;
						*m++ = lcinc;
						m = t + t[1];
						r = C->rhs[C->numrhs];
/*
						Test now if the argument was already evaluated.
						In that case it needs a new subexpression prototype.
						In either case we replace the function now by a
						subexpression prototype.
*/
						if ( *r >= (SUBEXPSIZE+4)
						&& ABS(*(r+*r-1)) < (*r - 1)
						&& r[1] == SUBEXPRESSION ) {
							r++;
							i = r[1] - 5;
							*t++ = *r++; *t++ = *r++; *t++ = C->numrhs;
							r++; *t++ = *r++; *t++ = AT.ebufnum; r++;
							while ( --i >= 0 ) *t++ = *r++;
						}
						else {
							*t++ = SUBEXPRESSION;
							*t++ = 4+SUBEXPSIZE;
							*t++ = C->numrhs;
							*t++ = 1;
							*t++ = AT.ebufnum;
							FILLSUB(t)
							if ( lcounter < 0 ) {
								*t++ = INDTOIND;
								*t++ = 4;
								*t++ = -lcounter;
							}
							else {
								*t++ = SYMTONUM;
								*t++ = 4;
								*t++ = lcounter;
							}
							*t++ = lcmin;
						}
						t2 = term + *term;
						while ( m < t2 ) *t++ = *m++;
						*term = WORDDIF(t,term);
						AN.TeInFun = -C->numrhs;
						AR.TePos = 0;
						AN.TeSuOut = 0;
						AT.TMbuff = AT.ebufnum;
						return(C->numrhs);
					}
				}
			}
			else if ( *t == TOPOLOGIES ) {
/*
				Syntax:
				topologies_(nloops,nlegs,setvertexsizes,setext,setint[,options])
*/
				t1 = t+FUNHEAD; t2 = t+t[1];
				if ( *t1 == -SNUMBER && t1[1] >= 0 &&
					t1[2] == -SNUMBER && ( t1[3] >= 0 || t1[3] == -2 ) &&
					t1[4] == -SETSET && Sets[t1[5]].type == CNUMBER &&
					t1[6] == -SETSET && Sets[t1[7]].type == CVECTOR &&
					t1[8] == -SETSET && Sets[t1[9]].type == CVECTOR &&
					t1+10 <= t2 ) {
					if ( t1+10 == t2 || ( t1+12 <= t2 && ( t1[10] == -SNUMBER ||
						( t1[10] == -SETSET &&
							Sets[t1[5]].last-Sets[t1[5]].first ==
							Sets[t1[11]].last-Sets[t1[11]].first ) ) ) ) {
						AN.TeInFun = -15;
						AN.TeSuOut = 0;
						AR.TePos = -1;
						return(1);
					}
				}
			}
			else if ( *t == DIAGRAMS ) {
			}
			if ( functions[funnum-FUNCTION].spec == 0
				|| ( t[2] & (DIRTYFLAG|MUSTCLEANPRF) ) != 0 ) { funflag = 1; }
			if ( *t <= MAXBUILTINFUNCTION ) {
			  if ( *t <= DELTAP && *t >= THETA ) { /* Speeds up by 2 or 3 compares */
			  if ( *t == THETA || *t == THETA2 ) {
				WORD *tstop, *tt2, kk;
				tstop = t + t[1];
				tt2 = t + FUNHEAD;
				while ( tt2 < tstop ) {
					if ( *tt2 > 0 && tt2[1] != 0 ) goto DoSpec;
					NEXTARG(tt2)
				}
				if ( !AT.RecFlag ) {
					if ( ( kk = DoTheta(BHEAD t) ) == 0 ) {
						*term = 0;
						return(0);
					}
					else if ( kk > 0 ) {
						m = t + t[1];
						r = term + *term;
						while ( m < r ) *t++ = *m++;
						*term = WORDDIF(t,term);
						goto ReStart;
					}
				}
			  }
			  else if ( *t == DELTA2 || *t == DELTAP ) {
				WORD *tstop, *tt2, kk;
				tstop = t + t[1];
				tt2 = t + FUNHEAD;
				while ( tt2 < tstop ) {
					if ( *tt2 > 0 && tt2[1] != 0 ) goto DoSpec;
					NEXTARG(tt2)
				}
				if ( !AT.RecFlag ) {
					if ( ( kk = DoDelta(t) ) == 0 ) {
						*term = 0;
						return(0);
					}
					else if ( kk > 0 ) {
						m = t + t[1];
						r = term + *term;
						while ( m < r ) *t++ = *m++;
						*term = WORDDIF(t,term);
						goto ReStart;
					}
				}
			  } }
			  else if ( *t == DISTRIBUTION && t[FUNHEAD] == -SNUMBER
			  && t[FUNHEAD+1] >= -2 && t[FUNHEAD+1] <= 2
			  && t[FUNHEAD+2] == -SNUMBER
			  && t[FUNHEAD+4] <= -FUNCTION
			  && t[FUNHEAD+5] <= -FUNCTION ) {
				WORD *ttt = t+FUNHEAD+6, *tttstop = t+t[1];
				while ( ttt < tttstop ) {
					if ( *ttt == -DOLLAREXPRESSION ) break;
					NEXTARG(ttt);
				}
				if ( ttt >= tttstop ) {
					AN.TeInFun = -1;
					AN.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}
			  }
			  else if ( *t == DELTA3 && ((t[1]-FUNHEAD) & 1 ) == 0 ) {
				AN.TeInFun = -2;
				AN.TeSuOut = 0;
				AR.TePos = -1;
				return(1);
			  }
			  else if ( ( *t == TABLEFUNCTION ) && ( t[FUNHEAD] <= -FUNCTION )
			  && ( T = functions[-t[FUNHEAD]-FUNCTION].tabl ) != 0
			  && ( t[1] >= FUNHEAD+1+2*T->numind )
			  && ( t[FUNHEAD+1] == -SYMBOL ) ) {
/*
				The case of table_(tab,sym1,...,symn)
*/
				for ( isp = 0; isp < T->numind; isp++ ) {
					if ( t[FUNHEAD+1+2*isp] != -SYMBOL ) break;
				}
				if ( isp >= T->numind ) {
					AN.TeInFun = -3;
					AN.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}
			  }
			  else if ( *t == TABLEFUNCTION && t[FUNHEAD] <= -FUNCTION
			  && ( T = functions[-t[FUNHEAD]-FUNCTION].tabl ) != 0
			  && ( t[1] == FUNHEAD+2 )
			  && ( t[FUNHEAD+1] <= -FUNCTION ) ) {
/*
				The case of table_(tab,fun)
*/
				AN.TeInFun = -3;
				AN.TeSuOut = 0;
				AR.TePos = -1;
				return(1);
			  }
			  else if ( *t == FACTORIN ) {
				if ( t[1] == FUNHEAD+2 && t[FUNHEAD] == -DOLLAREXPRESSION ) {
					AN.TeInFun = -4;
					AN.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}
				else if ( t[1] == FUNHEAD+2 && t[FUNHEAD] == -EXPRESSION ) {
					AN.TeInFun = -5;
					AN.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}
			  }
			  else if ( *t == TERMSINBRACKET ) {
				if ( t[1] == FUNHEAD || (
					 t[1] == FUNHEAD+2
					&& t[FUNHEAD] == -SNUMBER
					&& t[FUNHEAD+1] == 0
				 ) ) {
					AN.TeInFun = -6;
					AN.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}
/*
				The other cases have not yet been implemented
				We still have to add the case of short arguments
				First the different bracket in same expression

				else if ( t[1] > FUNHEAD+ARGHEAD
						&& t[FUNHEAD] == t[1]-FUNHEAD
						&& t[FUNHEAD+ARGHEAD] == t[1]-FUNHEAD-ARGHEAD
						&& t[t[1]-1] == 3
						&& t[t[1]-2] == 1
						&& t[t[1]-3] == 1 ) {
					AN.TeInFun = -6;
					AN.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}

				Next the bracket in an other expression

				else if ( t[1] > FUNHEAD+ARGHEAD+2
						&& t[FUNHEAD] == -EXPRESSION
						&& t[FUNHEAD+2] == t[1]-FUNHEAD-2
						&& t[FUNHEAD+ARGHEAD+2] == t[1]-FUNHEAD-ARGHEAD-2
						&& t[t[1]-1] == 3
						&& t[t[1]-2] == 1
						&& t[t[1]-3] == 1 ) {
					AN.TeInFun = -6;
					AN.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}
*/
			  }
			  else if ( *t == EXTRASYMFUN ) {
				if ( t[1] == FUNHEAD+2 && (
					( t[FUNHEAD] == -SNUMBER && t[FUNHEAD+1] <= cbuf[AM.sbufnum].numrhs
							&& t[FUNHEAD+1] > 0 ) ||
					( t[FUNHEAD] == -SYMBOL && t[FUNHEAD+1] < MAXVARIABLES 
							&& t[FUNHEAD+1] >= MAXVARIABLES-cbuf[AM.sbufnum].numrhs ) ) ) {
					AN.TeInFun = -7;
					AN.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}
				else if ( t[1] == FUNHEAD ) {
					AN.TeInFun = -7;
					AN.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}
			  }
              else if ( *t == DIVFUNCTION || *t == REMFUNCTION
                        || *t == INVERSEFUNCTION || *t == MULFUNCTION
						|| *t == GCDFUNCTION ) {
                WORD *tf;
				int todo = 1, numargs = 0;
                tf = t + FUNHEAD;
                while ( tf < t + t[1] ) {
					DOLLARS d;
					if ( *tf == -DOLLAREXPRESSION ) {
						d = Dollars + tf[1];
						if ( d->type == DOLWILDARGS ) {
							WORD *tterm = AT.WorkPointer, *tw;
							WORD *ta = term, *tb = tterm, *tc, *td = term + *term;
							while ( ta < t ) *tb++ = *ta++;
							tc = tb;
							while ( ta < tf ) *tb++ = *ta++;
							tw = d->where+1;
							while ( *tw ) {
								if ( *tw < 0 ) {
									if ( *tw > -FUNCTION ) *tb++ = *tw++;
									*tb++ = *tw++;
								}
								else {
									int ia;
									for ( ia = 0; ia < *tw; ia++ ) *tb++ = *tw++;
								}
							}
							NEXTARG(ta)
							while ( ta < t+t[1] ) *tb++ = *ta++;
							tc[1] = tb-tc;
							while ( ta < td ) *tb++ = *ta++;
							*tterm = tb - tterm;
							{
								int ia, na = *tterm;
								ta = tterm; tb  = term;
								for ( ia = 0; ia < na; ia++ ) *tb++ = *ta++;
							}
							if ( tb > AT.WorkTop ) {
								MLOCK(ErrorMessageLock);
								MesWork();
								goto EndTest2;
							}
							AT.WorkPointer = tb;
							goto ReStart;
						}
					}
                    NEXTARG(tf);
                }
                tf = t + FUNHEAD;
                while ( tf < t + t[1] ) {
					numargs++;
					if ( *tf > 0 && tf[1] != 0 ) todo = 0;
                    NEXTARG(tf);
                }
				if ( todo && numargs == 2 ) {
					if ( *t == DIVFUNCTION ) AN.TeInFun = -9;
					else if ( *t == REMFUNCTION ) AN.TeInFun = -10;
					else if ( *t == INVERSEFUNCTION ) AN.TeInFun = -11;
					else if ( *t == MULFUNCTION ) AN.TeInFun = -14;
					else if ( *t == GCDFUNCTION ) AN.TeInFun = -8;
					AN.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}
				else if ( todo && numargs == 3 ) {
					if ( *t == DIVFUNCTION ) AN.TeInFun = -9;
					else if ( *t == REMFUNCTION ) AN.TeInFun = -10;
					else if ( *t == GCDFUNCTION ) AN.TeInFun = -8;
					AN.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}
				else if ( todo && *t == GCDFUNCTION ) {
					AN.TeInFun = -8;
					AN.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}
			  }
			  else if ( *t == PERMUTATIONS && ( ( t[1] >= FUNHEAD+1
				&& t[FUNHEAD] <= -FUNCTION ) || ( t[1] >= FUNHEAD+3
				&& t[FUNHEAD] == -SNUMBER && t[FUNHEAD+2] <= -FUNCTION ) ) ) {
				AN.TeInFun = -12;
				AN.TeSuOut = 0;
				AR.TePos = -1;
				return(1);
			  }
			  else if ( *t == PARTITIONS ) {
				if ( TestPartitions(t,&(AT.partitions)) ) {
					AT.partitions.where = t-term;
					AN.TeInFun = -13;
					AN.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}
			  }
			}
		}
		t += t[1];
	} while ( t < m );
	if ( funflag ) {	/* Search in functions */
DoSpec:
		t = term;
		AT.NestPoin->termsize = t;
		if ( AT.NestPoin == AT.Nest ) AN.EndNest = t + *t;
		t++;
		oldncmod = AN.ncmod;
		if ( t < m ) do {
			if ( *t < FUNCTION ) {
				t += t[1]; continue;
			}
			if ( AN.ncmod && ( ( AC.modmode & ALSOFUNARGS ) == 0 ) ) {
				if ( *t != AR.PolyFun ) AN.ncmod = 0;
				else                    AN.ncmod = oldncmod;
			}
			r = t + t[1];
			funnum = *t;
			if ( *t >= FUNCTION + WILDOFFSET ) funnum -= WILDOFFSET;
			if ( ( *t == NUMFACTORS || *t == FIRSTTERM || *t == CONTENTTERM )
			 && t[1] == FUNHEAD+2 &&
			( t[FUNHEAD] == -EXPRESSION || t[FUNHEAD] == -DOLLAREXPRESSION ) ) {
/*
				if ( *t == NUMFACTORS ) {
					This we leave for Normalize
				}
*/				
			}
			else if ( functions[funnum-FUNCTION].spec == 0 ) {
				AT.NestPoin->funsize = t + 1;
				t1 = t;
				t += FUNHEAD;
				while ( t < r ) {	/* Sum over arguments */
					if ( *t > 0 && t[1] ) {	/* Argument is dirty  */
						AT.NestPoin->argsize = t;
						AT.NestPoin++;
/*						stop = t + *t; */
						t2 = t;
						t += ARGHEAD;
						while ( t < AT.NestPoin[-1].argsize+*(AT.NestPoin[-1].argsize) ) {
											/* Sum over terms */
							AT.RecFlag++;
							i = *t;
							AN.subsubveto = 1;
/*
							AN.subsubveto repairs a bug that became apparent
							in an example by York Schroeder:
								f(k1.k1)*replace_(k1,2*k2)
							Is it possible to repair the counting of the various
							length indicators? (JV 1-jun-2010)
*/
							if ( ( retvalue = TestSub(BHEAD t,level) ) != 0 ) {
/*
								Possible size changes:
								Note defs at 471,467,460,400,425,328
*/
redosize:
								if ( i > *t ) {
									i -= *t;
									*t2 -= i;
									t1[1] -= i;
									t += *t;
									r = t + i;
									m = term + *term;
									while ( r < m ) *t++ = *r++;
									*term -= i;
								}
								AN.subsubveto = 0;
								t1[2] = 1;
								if ( *t1 == AR.PolyFun && AR.PolyFunType == 2 )
									t1[2] |= MUSTCLEANPRF;
								AT.RecFlag--;
								AT.NestPoin--;
								AN.TeInFun++;
								AR.TePos = 0;
								AN.ncmod = oldncmod;
								return(retvalue);
							}
							else {
								/*
								 * Somehow the next line fixes Issue #106.
								 */
								i = *t;
								Normalize(BHEAD t);
/*								if ( i > *t ) { retvalue = 1; goto redosize; } */
								/*
								 * Experimentally, the next line fixes Issue #105.
								 */
								if ( *t == 0 ) { retvalue = 1; goto redosize; }
								{
									WORD *tend = t + *t, *tt = t+1;
									stilldirty = 0;
									tend -= ABS(tend[-1]);
									while ( tt < tend ) {
										if ( *tt == SUBEXPRESSION ) {
											stilldirty = 1; break;
										}
										tt += tt[1];
									}
								}
								if ( i > *t ) {
									retvalue = 1;
									i -= *t;
									*t2 -= i;
									t1[1] -= i;
									t += *t;
									r = t + i;
									m = term + *term;
									while ( r < m ) *t++ = *r++;
									*term -= i;
									t = AT.NestPoin[-1].argsize + ARGHEAD;
								}
							}
							AN.subsubveto = 0;
							AT.RecFlag--;
							t += *t;
						}
						AT.NestPoin--;
/*
						Argument contains no subexpressions.
						It should be normalized and sorted.
						The main problem is the storage.
*/
						t = AT.NestPoin->argsize;
						j = *t;
						t += ARGHEAD;
						NewSort(BHEAD0);
						if ( *t1 == AR.PolyFun && AR.PolyFunType == 2 ) {
							AR.CompareRoutine = &CompareSymbols;
							AR.SortType = SORTHIGHFIRST;
						}
						if ( AT.WorkPointer < term + *term )
							AT.WorkPointer = term + *term;

						while ( t < AT.NestPoin->argsize+*(AT.NestPoin->argsize) ) {
							m = AT.WorkPointer;
							r = t + *t;
							do { *m++ = *t++; } while ( t < r );
							r = AT.WorkPointer;
							AT.WorkPointer = r + *r;
							if ( Normalize(BHEAD r) ) {
								if ( *t1 == AR.PolyFun && AR.PolyFunType == 2 ) {
									AR.SortType = oldsorttype;
									AR.CompareRoutine = oldcompareroutine;
									t1[2] |= MUSTCLEANPRF;
								}
								LowerSortLevel(); goto EndTest;
							}
							if ( AN.ncmod != 0 ) {
								if ( *r ) {
									if ( Modulus(r) ) {
										LowerSortLevel();
										AT.WorkPointer = r;
										if ( *t1 == AR.PolyFun && AR.PolyFunType == 2 ) {
											AR.SortType = oldsorttype;
											AR.CompareRoutine = oldcompareroutine;
											t1[2] |= MUSTCLEANPRF;
										}
										goto EndTest;
									}
								}
							}
							if ( AR.PolyFun > 0 ) {
								if ( PrepPoly(BHEAD r,1) != 0 ) goto EndTest;
							}
							if ( *r ) StoreTerm(BHEAD r);
							AT.WorkPointer = r;
						}
/* the next call had parameter 0. That was wrong!!!!!) */
						if ( EndSort(BHEAD AT.WorkPointer+ARGHEAD,1) < 0 ) goto EndTest;
						m = AT.WorkPointer+ARGHEAD;
						if ( *t1 == AR.PolyFun && AR.PolyFunType == 2 ) {
							AR.SortType = oldsorttype;
							AR.CompareRoutine = oldcompareroutine;
							t1[2] |= MUSTCLEANPRF;
						}
						while ( *m ) m += *m;
						i = WORDDIF(m,AT.WorkPointer);
						*AT.WorkPointer = i;
						AT.WorkPointer[1] = stilldirty;
						if ( ToFast(AT.WorkPointer,AT.WorkPointer) ) {
							m = AT.WorkPointer;
							if ( *m <= -FUNCTION ) { m++; i = 1; }
							else { m += 2; i = 2; }
						}
						j = i - j;
						if ( j > 0 ) {
							r = m + j;
							if ( r > AT.WorkTop ) {
								MLOCK(ErrorMessageLock);
								MesWork();
								goto EndTest2;
							}
							do { *--r = *--m; } while ( m > AT.WorkPointer );
							AT.WorkPointer = r;
							m = AN.EndNest;
							r = m + j;
							stop = AT.NestPoin->argsize+*(AT.NestPoin->argsize);
							do { *--r = *--m; } while ( m >= stop );
						}
						else if ( j < 0 ) {
							m = AT.NestPoin->argsize+*(AT.NestPoin->argsize);
							r = m + j;
							do { *r++ = *m++; } while ( m < AN.EndNest );
						}
						m = AT.NestPoin->argsize;
						r = AT.WorkPointer;
						while ( --i >= 0 ) *m++ = *r++;
						n = AT.Nest;
						while ( n <= AT.NestPoin ) {
							if ( *(n->argsize) > 0 && n != AT.NestPoin )
								*(n->argsize) += j;
							*(n->funsize) += j;
							*(n->termsize) += j;
							n++;
						}
						AN.EndNest += j;
/*						(AT.NestPoin->argsize)[1] = 0;  */
						if ( funnum == DENOMINATOR || funnum == EXPONENT ) {
							if ( Normalize(BHEAD term) ) {
/*
								In this case something has been substituted
								Either a $ or a replace_?????
								Originally we had here:

								goto EndTest;

								It seems better to restart.
*/
								AN.ncmod = oldncmod;
								goto ReStart;
							}
/*
							And size changes here?????
*/
						}
						AN.ncmod = oldncmod;
						goto ReStart;
					}
					else if ( *t == -DOLLAREXPRESSION ) {
						if ( ( *t1 == TERMSINEXPR || *t1 == SIZEOFFUNCTION )
								 && t1[1] == FUNHEAD+2 ) {}
						else {
							if ( AR.Eside != LHSIDE ) {
								AN.TeInFun = 1; AR.TePos = 0;
								AT.TMbuff = AM.dbufnum; t1[2] |= DIRTYFLAG;
								AN.ncmod = oldncmod;
								return(1);
							}
							AC.lhdollarflag = 1;
						}
					}
					else if ( *t == -TERMSINBRACKET ) {
						if ( AR.Eside != LHSIDE ) {
							AN.TeInFun = 1; AR.TePos = 0;
							t1[2] |= DIRTYFLAG;
							AN.ncmod = oldncmod;
							return(1);
						}
					}
					else if ( AN.ncmod != 0 && *t == -SNUMBER ) {
						if ( AN.ncmod == 1 || AN.ncmod == -1 ) {
							isp = (UWORD)(AC.cmod[0]);
							isp = t[1] % isp;
							if ( ( AC.modmode & POSNEG ) != 0 ) {
								if ( isp > (UWORD)(AC.cmod[0])/2 ) isp = isp - (UWORD)(AC.cmod[0]);
								else if ( -isp > (UWORD)(AC.cmod[0])/2 ) isp = isp + (UWORD)(AC.cmod[0]);
							}
							else {
								if ( isp < 0 ) isp += (UWORD)(AC.cmod[0]);
							}
							if ( isp <= MAXPOSITIVE && isp >= -MAXPOSITIVE ) {
								t[1] = isp;
							}
						}
					}
					NEXTARG(t)
				}
				if ( funnum >= FUNCTION && functions[funnum-FUNCTION].tabl ) {
/*
					Test whether the table catches
					Test 1: index arguments and range. i will be the number
						of the element in the table.
*/
					WORD rhsnumber, *oldwork = AT.WorkPointer, *Tpattern;
					WORD ii, *p;
					MINMAX *mm;
					T = functions[funnum-FUNCTION].tabl;
/*
					The next application of T->pattern isn't thread safe.
					p = T->pattern + FUNHEAD+1;
					The new code is in the next three lines and in the application
					ii = T->pattern[1]; p = Tpattern; pp = T->pattern;
					for ( i = 0; i < ii; i++ ) *p++ = *pp++;
					AT.WorkPointer = p; 
*/
#ifdef WITHPTHREADS
					Tpattern = T->pattern[AT.identity];
#else
					Tpattern = T->pattern;
#endif
					p = Tpattern + FUNHEAD+1;

					mm = T->mm;
					if ( T->sparse ) {
						t = t1+FUNHEAD;
						if ( T->numind == 0 ) { isp = 0; }
						else {
						  for ( i = 0; i < T->numind; i++, t += 2 ) {
							if ( *t != -SNUMBER ) break;
						  }
						  if ( i < T->numind ) goto teststrict;

						  isp = FindTableTree(T,t1+FUNHEAD,2);
						}
						if ( isp < 0 ) {
teststrict:					if ( T->strict == -2 ) {
								rhsnumber = AM.zerorhs;
								tbufnum = AM.zbufnum;
							}
							else if ( T->strict == -3 ) {
								rhsnumber = AM.onerhs;
								tbufnum = AM.zbufnum;
							}
							else if ( T->strict < 0 ) goto NextFun;
							else {
								MLOCK(ErrorMessageLock);
								MesPrint("Element in table is undefined");
								goto showtable;
							}
/*
							Copy the indices;
*/
							t = t1+FUNHEAD+1;
							for ( i = 0; i < T->numind; i++ ) {
								*p = *t; p+=2; t+=2;
							}
						}
						else {
							rhsnumber = T->tablepointers[isp+T->numind];
#if ( TABLEEXTENSION == 2 )
							tbufnum = T->bufnum;
#else
							tbufnum = T->tablepointers[isp+T->numind+1];
#endif
							t = t1+FUNHEAD+1;
							ii = T->numind;
							while ( --ii >= 0 ) {
								*p = *t; t += 2; p += 2;
							}
						}
						goto caughttable;
					}
					else {
						i = 0;
						t = t1 + FUNHEAD;
						j = T->numind;
						while ( --j >= 0 ) {
							if ( *t != -SNUMBER ) goto NextFun;
							t++;
							if ( *t < mm->mini || *t > mm->maxi ) {
								if ( T->bounds ) {
									MLOCK(ErrorMessageLock);
									MesPrint("Table boundary check. Argument %d",
									T->numind-j);
showtable:							AO.OutFill = AO.OutputLine = (UBYTE *)m;
									AO.OutSkip = 8;
									IniLine(0);
									WriteSubTerm(t1,1);
									FiniLine();
									MUNLOCK(ErrorMessageLock);
									SETERROR(-1)
								}
								goto NextFun;
							}
							i += ( *t - mm->mini ) * (LONG)(mm->size);
							*p = *t++;
							p += 2;
							mm++;
						}
/*
						Test now whether the entry exists.
*/
						i *= TABLEEXTENSION;
						if ( T->tablepointers[i] == -1 ) {
							if ( T->strict == -2 ) {
								rhsnumber = AM.zerorhs;
								tbufnum = AM.zbufnum;
							}
							else if ( T->strict == -3 ) {
								rhsnumber = AM.onerhs;
								tbufnum = AM.zbufnum;
							}
							else if ( T->strict < 0 ) goto NextFun;
							else {
								MLOCK(ErrorMessageLock);
								MesPrint("Element in table is undefined");
								goto showtable;
							}
						}
						else {
							rhsnumber = T->tablepointers[i];
#if ( TABLEEXTENSION == 2 )
							tbufnum = T->bufnum;
#else
							tbufnum = T->tablepointers[i+1];
#endif
						}
					}
/*
					If there are more arguments we have to do some
					pattern matching. This should be easy. We addapted the
					pattern, so that the array indices match already.
					Note that if there is no match the program will become
					very slow.
*/
caughttable:
#ifdef WITHPTHREADS
					AN.FullProto = T->prototype[AT.identity];
#else
					AN.FullProto = T->prototype;
#endif
					AN.WildValue = AN.FullProto + SUBEXPSIZE;
					AN.WildStop = AN.FullProto+AN.FullProto[1];
					ClearWild(BHEAD0);
					AN.RepFunNum = 0;
					AN.RepFunList = AN.EndNest;
				    AT.WorkPointer = (WORD *)(((UBYTE *)(AN.EndNest)) + AM.MaxTer/2);
					if ( AT.WorkPointer >= AT.WorkTop ) {
						MLOCK(ErrorMessageLock);
						MesWork();
						MUNLOCK(ErrorMessageLock);
					}
					wilds = 0;
/*					if ( MatchFunction(BHEAD T->pattern,t1,&wilds) > 0 ) {  } */
					if ( MatchFunction(BHEAD Tpattern,t1,&wilds) > 0 ) {
						AT.WorkPointer = oldwork;
						if ( AT.NestPoin != AT.Nest ) {
							AN.ncmod = oldncmod;
							return(1);
						}

						m = AN.FullProto;
						retvalue = m[2] = rhsnumber;
						m[4] = tbufnum;
						t = t1;
						j = t[1];
						i = m[1];
						if ( j > i ) {
							j = i - j;
							NCOPY(t,m,i);
							m = term + *term;
							while ( r < m ) *t++ = *r++;
							*term += j;
						}
						else if ( j < i ) {
							j = i-j;
							t = term + *term;
							while ( t >= r ) { t[j] = *t; t--; }
							t = t1;
							NCOPY(t,m,i);
							*term += j;
						}
						else {
							NCOPY(t,m,j);
						}
						AN.TeInFun = 0;
						AR.TePos = 0;
						AN.TeSuOut = -1;
						if ( AT.WorkPointer < term + *term ) AT.WorkPointer = term + *term;
						AT.TMbuff = tbufnum;
						AN.ncmod = oldncmod;
						return(retvalue);
					}
					AT.WorkPointer = oldwork;
				}
NextFun:;
			}
			else if ( ( t[2] & DIRTYFLAG ) != 0 ) {
				t += FUNHEAD;
				while ( t < r ) {
					if ( *t == FUNNYDOLLAR ) {
						if ( AR.Eside != LHSIDE ) {
							AN.TeInFun = 1;
							AR.TePos = 0;
							AT.TMbuff = AM.dbufnum;
							AN.ncmod = oldncmod;
							return(1);
						}
						AC.lhdollarflag = 1;
					}
					t++;
				}
			}
			t = r;
			AN.ncmod = oldncmod;
		} while ( t < m );
	}
	return(0);
EndTest:;
	MLOCK(ErrorMessageLock);
EndTest2:;
	MesCall("TestSub");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] TestSub : 
 		#[ InFunction :			WORD InFunction(term,termout)
*/
/**
 *		Makes the replacement of the subexpression with the number 'replac'
 *		in a function argument. Additional information is passed in some
 *		of the AR, AN, AT variables.
 *
 *		@param term     The input term
 *		@param termout  The output term
 *		@return         0: everything is fine, Negative: fatal, Positive: error.
 *
 *		Special attention should be given to nested functions!
 */

WORD InFunction(PHEAD WORD *term, WORD *termout)
{
	GETBIDENTITY
	WORD *m, *t, *r, *rr, sign = 1, oldncmod;
	WORD *u, *v, *w, *from, *to, 
		ipp, olddefer = AR.DeferFlag, oldPolyFun = AR.PolyFun, i, j;
	LONG numterms;
	from = t = term;
	r = t + *t - 1;
	m = r - ABS(*r) + 1;
	t++;
	while ( t < m ) {
		if ( *t >= FUNCTION+WILDOFFSET ) ipp = *t - WILDOFFSET;
		else ipp = *t;
		if ( AR.TePos ) {
			if ( ( term + AR.TePos ) == t ) {
				m = termout;
				while ( from < t ) *m++ = *from++;
				*m++ = DENOMINATOR;
				*m++ = t[1] + 4 + FUNHEAD + ARGHEAD;
				*m++ = DIRTYFLAG;
				FILLFUN3(m)
				*m++ = t[1] + 4 + ARGHEAD;
				*m++ = 1;
				FILLARG(m)
				*m++ = t[1] + 4;
				t[3] = -t[3];
				v = t + t[1];
				while ( t < v ) *m++ = *t++;
				from[3] = -from[3];
				*m++ = 1;
				*m++ = 1;
				*m++ = 3;
				r = term + *term;
				while ( t < r ) *m++ = *t++;
				if ( (m-termout) > (LONG)(AM.MaxTer/sizeof(WORD)) ) goto TooLarge;
				*termout = WORDDIF(m,termout);
				return(0);
			}
		}
		else if ( ( *t >= FUNCTION && functions[ipp-FUNCTION].spec == 0 )
		&& ( t[2] & DIRTYFLAG ) == DIRTYFLAG ) {
			m = termout;
			r = t + t[1];
			u = t;
			t += FUNHEAD;
			oldncmod = AN.ncmod;
			while ( t < r ) {	/* t points at an argument */
				if ( *t > 0 && t[1] ) {	/* Argument has been modified */
					WORD oldsorttype = AR.SortType;
					/* This whole argument must be redone */

					if ( ( AN.ncmod != 0 )
						&& ( ( AC.modmode & ALSOFUNARGS ) == 0 )
						&& ( *u != AR.PolyFun ) ) { AN.ncmod = 0; }
					AR.DeferFlag = 0;
					v = t + *t;
					t += ARGHEAD;		/* First term */
					w = 0;	/* to appease the compilers warning devices */
					while ( from < t ) {
						if ( from == u ) w = m;
						*m++ = *from++;
					}
					to = m;
					NewSort(BHEAD0);
					if ( *u == AR.PolyFun && AR.PolyFunType == 2 ) {
						AR.CompareRoutine = &CompareSymbols;
						AR.SortType = SORTHIGHFIRST;
					}
/*
					AR.PolyFun = 0;
*/
					while ( t < v ) {
						i = *t;
						NCOPY(m,t,i);
						m = to;
                        if ( AT.WorkPointer < m+*m ) AT.WorkPointer = m + *m;
						if ( Generator(BHEAD m,AR.Cnumlhs) ) {
							AN.ncmod = oldncmod;
							LowerSortLevel(); goto InFunc;
						}
					}
					/* w = the function */
					/* v = the next argument */
					/* u = the function */
					/* to is new argument */

					to -= ARGHEAD;
					if ( EndSort(BHEAD m,1) < 0 ) {
						AN.ncmod = oldncmod;
						goto InFunc;
					}
					AR.PolyFun = oldPolyFun;
					if ( *u == AR.PolyFun && AR.PolyFunType == 2 ) {
						AR.CompareRoutine = &Compare1;
						AR.SortType = oldsorttype;
					}
					while ( *m ) m += *m;
					*to = WORDDIF(m,to);
					to[1] = 1;  /* ??????? or rather 0?. 24-mar-2006 JV */
					if ( ToFast(to,to) ) {
						if ( *to <= -FUNCTION ) m = to+1;
						else m = to+2;
					}
					w[1] = WORDDIF(m,w) + WORDDIF(r,v);
					r = term + *term;
					t = v;
					while ( t < r ) *m++ = *t++;
					if ( (m-termout) > (LONG)(AM.MaxTer/sizeof(WORD)) ) goto TooLarge;
					*termout = WORDDIF(m,termout);
					AR.DeferFlag = olddefer;
					AN.ncmod = oldncmod;
					return(0);
				}
				else if ( *t == -DOLLAREXPRESSION ) {
					if ( AR.Eside == LHSIDE ) {
						NEXTARG(t)
						AC.lhdollarflag = 1;
					}
					else {
/*
						This whole argument must be redone
*/
					DOLLARS d = Dollars + t[1];
#ifdef WITHPTHREADS
					int nummodopt, dtype = -1;
					if ( AS.MultiThreaded ) {
						for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
							if ( t[1] == ModOptdollars[nummodopt].number ) break;
						}
						if ( nummodopt < NumModOptdollars ) {
							dtype = ModOptdollars[nummodopt].type;
							if ( dtype == MODLOCAL ) {
								d = ModOptdollars[nummodopt].dstruct+AT.identity;
							}
							else {
								LOCK(d->pthreadslockread);
							}
						}
					}
#endif
					oldncmod = AN.ncmod;
					if ( ( AN.ncmod != 0 )
						&& ( ( AC.modmode & ALSOFUNARGS ) == 0 )
						&& ( *u != AR.PolyFun ) ) { AN.ncmod = 0; }
					AR.DeferFlag = 0;
					v = t + 2;
					w = 0;	/* to appease the compilers warning devices */
					while ( from < t ) {
						if ( from == u ) w = m;
						*m++ = *from++;
					}
					to = m;
					switch ( d->type ) {
						case DOLINDEX:
							if ( d->index >= 0 && d->index < AM.OffsetIndex ) {
								*m++ = -SNUMBER; *m++ = d->index;
							}
							else { *m++ = -INDEX; *m++ = d->index; }
							break;
						case DOLZERO:
							*m++ = -SNUMBER; *m++ = 0; break;
						case DOLNUMBER:
							if ( d->where[0] == 4 &&
							( d->where[1] & MAXPOSITIVE ) == d->where[1] ) {
								*m++ = -SNUMBER;
								if ( d->where[3] >= 0 ) *m++ = d->where[1];
								else *m++ = -d->where[1];
								break;
							}
							/* fall through */
						case DOLTERMS:
/*
							Here we have the special case of the PolyRatFun
							That function may have a different sort of the
							terms in the argument.
*/
							to = m; r = d->where;
							*m++ = 0; *m++ = 1;
							FILLARG(m)
							while ( *r ) {
								i = *r; NCOPY(m,r,i)
							}
							*to = m-to;
							if ( ToFast(to,to) ) {
								if ( *to <= -FUNCTION ) m = to+1;
								else m = to+2;
							}
							else if ( *u == AR.PolyFun && AR.PolyFunType == 2 ) {
								AR.PolyFun = 0;
								NewSort(BHEAD0);
								AR.CompareRoutine = &CompareSymbols;
								r = to + ARGHEAD;
								while ( r < m ) {
									rr = r; r += *r;
									if ( SymbolNormalize(rr) ) goto InFunc;
									if ( StoreTerm(BHEAD rr) ) {
										AR.CompareRoutine = &Compare1;
										LowerSortLevel();
										Terminate(-1);
									}
								}
								if ( EndSort(BHEAD to+ARGHEAD,1) < 0 ) goto InFunc;
								AR.PolyFun = oldPolyFun;
								AR.CompareRoutine = &Compare1;
								m = to+ARGHEAD;
								if ( *m == 0 ) {
									*to = -SNUMBER;
									to[1] = 0;
									m = to + 2;
								}
								else {
									while ( *m ) m += *m;
									*t = m - to;
									if ( ToFast(to,to) ) {
										if ( *to <= -FUNCTION ) m = to+1;
										else m = to+2;
									}
								}
							}
							w[1] = w[1] - 2 + (m-to);
							break;
						case DOLSUBTERM:
							to = m; r = d->where;
							i = r[1];
							*m++ = i+4+ARGHEAD; *m++ = 1;
							FILLARG(m)
							*m++ = i+4;
							while ( --i >= 0 ) *m++ = *r++;
							*m++ = 1; *m++ = 1; *m++ = 3;
							if ( ToFast(to,to) ) {
								if ( *to <= -FUNCTION ) m = to+1;
								else m = to+2;
							}
							w[1] = w[1] - 2 + (m-to);
							break;
						case DOLARGUMENT:
							to = m; r = d->where;
							if ( *r > 0 ) {
								i = *r - 2;
								*m++ = *r++; *m++ = 1; r++;
								while ( --i >= 0 ) *m++ = *r++;
							}
							else if ( *r <= -FUNCTION ) *m++ = *r++;
							else { *m++ = *r++; *m++ = *r++; }
							w[1] = w[1] - 2 + (m-to);
							break;
						case DOLWILDARGS:
							to = m; r = d->where;
							if ( *r > 0 ) { /* Tensor arguments */
								i = *r++;
								while ( --i >= 0 ) {
									if ( *r < 0 ) {
										*m++ = -VECTOR; *m++ = *r++;
									}
									else if ( *r >= AM.OffsetIndex ) {
										*m++ = -INDEX; *m++ = *r++;
									}
									else { *m++ = -SNUMBER; *m++ = *r++; }
								}
							}
							else { /* Regular arguments */
								r++;
								while ( *r ) {
									if ( *r > 0 ) {
										i = *r - 2;
										*m++ = *r++; *m++ = 1; r++;
										while ( --i >= 0 ) *m++ = *r++;
									}
									else if ( *r <= -FUNCTION ) *m++ = *r++;
									else { *m++ = *r++; *m++ = *r++; }
								}
							}
							w[1] = w[1] - 2 + (m-to);
							break;
						case DOLUNDEFINED:
						default:
							MLOCK(ErrorMessageLock);
							MesPrint("!!!Undefined $-variable: $%s!!!",
							AC.dollarnames->namebuffer+d->name);
							MUNLOCK(ErrorMessageLock);
#ifdef WITHPTHREADS
							if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
							Terminate(-1);
					}
#ifdef WITHPTHREADS
					if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
					r = term + *term;
					t = v;
					while ( t < r ) *m++ = *t++;
					if ( (m-termout) > (LONG)(AM.MaxTer/sizeof(WORD)) ) goto TooLarge;
					*termout = WORDDIF(m,termout);
					AR.DeferFlag = olddefer;
					AN.ncmod = oldncmod;
					return(0);
				}
				}
				else if ( *t == -TERMSINBRACKET ) {
					if ( AC.ComDefer ) numterms = CountTerms1(BHEAD0);
					else               numterms = 1;
/*
					Compose the output term
					First copy the part till this function argument
					m points at the output term space
					u points at the start of the function
					t points at the start of the argument
*/
					w = 0;
					while ( from < t ) {
						if ( from == u ) w = m;
						*m++ = *from++;
					}
					if ( ( numterms & MAXPOSITIVE ) == numterms ) {
						*m++ = -SNUMBER; *m++ =  numterms & MAXPOSITIVE;
						w[1] += 1;
					}
					else if ( ( i = numterms >> BITSINWORD ) == 0 ) {
						*m++ = ARGHEAD+4;
						for ( j = 1; j < ARGHEAD; j++ ) *m++ = 0;
						*m++ = 4; *m++ = numterms & WORDMASK; *m++ = 1; *m++ = 3;
						w[1] += ARGHEAD+3;
					}
					else {
						*m++ = ARGHEAD+6;
						for ( j = 1; j < ARGHEAD; j++ ) *m++ = 0;
						*m++ = 6; *m++ = numterms & WORDMASK;
						*m++ = i; *m++ = 1; *m++ = 0; *m++ = 5;
						w[1] += ARGHEAD+5;
					}
					from++;  /* Skip our function */
					r = term + *term;
					while ( from < r ) *m++ = *from++;
					if ( (m-termout) > (LONG)(AM.MaxTer/sizeof(WORD)) ) goto TooLarge;
					*termout = WORDDIF(m,termout);
					return(0);
				}
				else { NEXTARG(t) }
			}
			t = u;
		}
		else if ( ( *t >= FUNCTION && functions[ipp-FUNCTION].spec )
		&& ( t[2] & DIRTYFLAG ) == DIRTYFLAG ) { /* Could be FUNNYDOLLAR */
			u = t; v = t + t[1];
			t += FUNHEAD;
			while ( t < v ) {
				if ( *t == FUNNYDOLLAR ) {
					if ( AR.Eside != LHSIDE ) {
					DOLLARS d = Dollars + t[1];
#ifdef WITHPTHREADS
					int nummodopt, dtype = -1;
					if ( AS.MultiThreaded ) {
						for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
							if ( t[1] == ModOptdollars[nummodopt].number ) break;
						}
						if ( nummodopt < NumModOptdollars ) {
							dtype = ModOptdollars[nummodopt].type;
							if ( dtype == MODLOCAL ) {
								d = ModOptdollars[nummodopt].dstruct+AT.identity;
							}
							else {
								LOCK(d->pthreadslockread);
							}
						}
					}
#endif
					oldncmod = AN.ncmod;
					if ( ( AN.ncmod != 0 )
						&& ( ( AC.modmode & ALSOFUNARGS ) == 0 )
						&& ( *u != AR.PolyFun ) ) { AN.ncmod = 0; }
					m = termout; w = 0;
					while ( from < t ) {
						if ( from == u ) w = m;
						*m++ = *from++;
					}
					to = m;
					switch ( d->type ) {
						case DOLINDEX:
							*m++ = d->index; break;
						case DOLZERO:
							*m++ = 0; break;
						case DOLNUMBER:
						case DOLTERMS:
							if ( d->where[0] == 4 && d->where[4] == 0
							&& d->where[3] == 3 && d->where[2] == 1
							&& d->where[1] < AM.OffsetIndex ) {
								*m++ = d->where[1];
							}
							else {
wrongtype:;
#ifdef WITHPTHREADS
								if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
								MLOCK(ErrorMessageLock);
								MesPrint("$%s has wrong type for tensor substitution",
								AC.dollarnames->namebuffer+d->name);
								MUNLOCK(ErrorMessageLock);
								AN.ncmod = oldncmod;
								return(-1);
							}
							break;
						case DOLARGUMENT:
							if ( d->where[0] == -INDEX ) {
								*m++ = d->where[1]; break;
							}
							else if ( d->where[0] == -VECTOR ) {
								*m++ = d->where[1]; break;
							}
							else if ( d->where[0] == -MINVECTOR ) {
								*m++ = d->where[1];
								sign = -sign;
								break;
							}
							else if ( d->where[0] == -SNUMBER ) {
								if ( d->where[1] >= 0
								&& d->where[1] < AM.OffsetIndex ) {
									*m++ = d->where[1]; break;
								}
							}
							goto wrongtype;
						case DOLWILDARGS:
							if ( d->where[0] > 0 ) {
								r = d->where; i = *r++;
								while ( --i >= 0 ) *m++ = *r++;
							}
							else {
								r = d->where + 1;
								while ( *r ) {
									if ( *r == -INDEX ) {
										*m++ = r[1]; r += 2; continue;
									}
									else if ( *r == -VECTOR ) {
										*m++ = r[1]; r += 2; continue;
									}
									else if ( *r == -MINVECTOR ) {
										*m++ = r[1]; r += 2;
										sign = -sign; continue;
									}
									else if ( *r == -SNUMBER ) {
										if ( r[1] >= 0
										&& r[1] < AM.OffsetIndex ) {
											*m++ = r[1]; r += 2; continue;
										}
									}
									goto wrongtype;
								}
							}
							break;
						case DOLSUBTERM:
							r = d->where;
							if ( *r == INDEX && r[1] == 3 ) {
								*m++ = r[2];
							}
							else goto wrongtype;
							break;
						case DOLUNDEFINED:
#ifdef WITHPTHREADS
							if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
							MLOCK(ErrorMessageLock);
							MesPrint("$%s is undefined in tensor substitution",
							AC.dollarnames->namebuffer+d->name);
							MUNLOCK(ErrorMessageLock);
							AN.ncmod = oldncmod;
							return(-1);
					}
#ifdef WITHPTHREADS
					if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
					w[1] = w[1] - 2 + (m-to);
					from += 2;
					term += *term;
					while ( from < term ) *m++ = *from++;
					if ( sign < 0 ) m[-1] = -m[-1];
					if ( (m-termout) > (LONG)(AM.MaxTer/sizeof(WORD)) ) goto TooLarge;
					*termout = m - termout;
					AN.ncmod = oldncmod;
					return(0);
				}
				else {
					AC.lhdollarflag = 1;
				}
				}
				t++;
			}
			t = u;
		}
		t += t[1];
	}
	MLOCK(ErrorMessageLock);
	MesPrint("Internal error in InFunction: Function not encountered.");
	if ( AM.tracebackflag ) {
		MesPrint("%w: AR.TePos = %d",AR.TePos);
		MesPrint("%w: AN.TeInFun = %d",AN.TeInFun);
		termout = term;
		AO.OutFill = AO.OutputLine = (UBYTE *)AT.WorkPointer + AM.MaxTer;
		AO.OutSkip = 3;
		FiniLine();
		i = *termout;
		while ( --i >= 0 ) {
			TalToLine((UWORD)(*termout++));
			TokenToLine((UBYTE *)"  ");
		}
		AO.OutSkip = 0;
		FiniLine();
		MesCall("InFunction");
	}
	MUNLOCK(ErrorMessageLock);
	return(1);

InFunc:
	MLOCK(ErrorMessageLock);
	MesCall("InFunction");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)

TooLarge:
	MLOCK(ErrorMessageLock);
	MesPrint("Output term too large. Try to increase MaxTermSize in the setup.");
	MesCall("InFunction");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}
 		
/*
 		#] InFunction : 
 		#[ InsertTerm :			WORD InsertTerm(term,replac,extractbuff,position,termout)
*/
/**
 *		Puts the terms 'term' and 'position' together into a single
 *		legal term in termout. replac is the number of the subexpression
 *		that should be replaced. It must be a positive term.
 *		When action is needed in the argument of a function all terms
 *		in that argument are dealt with recursively. The subexpression
 *		is sorted. Only one subexpression is done at a time this way.
 *
 *		@param term        the input term
 *		@param replac      number of the subexpression pointer to replace
 *		@param extractbuff number of the compiler buffer replac refers to
 *		@param position    position from where to take the term in the compiler buffer
 *		@param termout     the output term
 *		@param tepos       offset in term where the subexpression is.
 *		@return  Normal conventions (OK = 0).
 */

WORD InsertTerm(PHEAD WORD *term, WORD replac, WORD extractbuff, WORD *position, WORD *termout,
                WORD tepos)
{
	GETBIDENTITY
	WORD *m, *t, *r, i, l2, j;
	WORD *u, *v, l1, *coef;
	coef = AT.WorkPointer;
	if ( ( AT.WorkPointer = coef + 2*AM.MaxTal ) > AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	t = term;
	r = t + *t;
	l1 = l2 = r[-1];
	m = r - ABS(l2);
	if ( tepos > 0 ) {
		t = term + tepos;
		goto foundit;
	}
	t++;
	while ( t < m ) {
		if ( *t == SUBEXPRESSION && t[2] == replac && t[3] && t[4] == extractbuff ) {
			r = t + t[1];
			while ( *r == SUBEXPRESSION && r[2] == replac && r[3] && r < m && r[4] == extractbuff ) {
				t = r; r += r[1];
			}
foundit:;
			u = m;
			r = term;
			m = termout;
			do { *m++ = *r++; } while ( r < t );
			if ( t[1] > SUBEXPSIZE ) {
/*
				if this is a dollar expression there are no wildcards
*/
				i = *--m;
				if ( ( l2 = WildFill(BHEAD m,position,t) ) < 0 ) goto InsCall;
				*m = i;
				m += l2-1;
				l2 = *m;
				i = ( j = ABS(l2) ) - 1;
				r = coef + i;
				do { *--r = *--m; } while ( --i > 0 );
			}
			else {
				v = t;
				t = position;
				r = t + *t;
				l2 = r[-1];
				r -= ( j = ABS(l2) );
				t++;
				if ( t < r ) do { *m++ = *t++; } while ( t < r );
				t = v;
			}
			t += t[1];
			while ( t < u && *t == DOLLAREXPR2 ) t += t[1];
ComAct:		if ( t < u ) do { *m++ = *t++; } while ( t < u );
			if ( *r == 1 && r[1] == 1 && j == 3 ) {
				if ( l2 < 0 ) l1 = -l1;
				i = ABS(l1)-1;
				NCOPY(m,t,i);
				*m++ = l1;
			}
			else {
				if ( MulRat(BHEAD (UWORD *)u,REDLENG(l1),(UWORD *)r,REDLENG(l2),
				(UWORD *)m,&l1) ) goto InsCall;
				l2 = l1;
				l2 *= 2;
				if ( l2 < 0 ) {
					m -= l2;
					*m++ = l2-1;
				}
				else {
					m += l2;
					*m++ = l2+1;
				}
			}
			*termout = WORDDIF(m,termout);
			if ( (*termout)*((LONG)sizeof(WORD)) > AM.MaxTer ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Term too complex during substitution. MaxTermSize of %l is too small",AM.MaxTer);
				goto InsCall2;
			}
			AT.WorkPointer = coef;
			return(0);
		}
		t += t[1];
	}
/*
	The next action is for when there is no subexpression pointer.
	We append the extra term. Effectively the routine becomes now a
	merge routine for two terms.
*/
	v = t;
	u = m;
	r = term;
	m = termout;
	do { *m++ = *r++; } while ( r < t );
	t = position;
	r = t + *t;
	l2 = r[-1];
	r -= ( j = ABS(l2) );
	t++;
	if ( t < r ) do { *m++ = *t++; } while ( t < r );
	t = v;
	goto ComAct;

InsCall:
	MLOCK(ErrorMessageLock);
InsCall2:
	MesCall("InsertTerm");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] InsertTerm : 
 		#[ PasteFile :			WORD PasteFile(num,acc,pos,accf,renum,freeze,nexpr)
*/
/**
 *		Gets a term from stored expression expr and puts it in
 *		the accumulator at position number. It returns the length of the
 *		term that came from file.
 *
 *		@param number   number of partial terms to skip in accum
 *		@param accum    the accumulator
 *		@param position file position from where to get the stored term
 *		@param accfill  returns tail position in accum
 *		@param renumber the renumber struct for the variables in the stored expression
 *		@param freeze   information about if we need only the contents of a bracket
 *		@param nexpr    the number of the stored expression
 *		@return Normal conventions (OK = 0).
 */

LONG PasteFile(PHEAD WORD number, WORD *accum, POSITION *position, WORD **accfill,
               RENUMBER renumber, WORD *freeze, WORD nexpr)
{
	GETBIDENTITY
	WORD *r, l, *m, i;
	WORD *stop, *s1, *s2;
/*	POSITION AccPos; bug 12-apr-2008 JV */
	WORD InCompState;
	WORD *oldipointer;
	LONG retlength;
    stop = (WORD *)(((UBYTE *)(accum)) + 2*AM.MaxTer);
	*accum++ = number;
	while ( --number >= 0 ) accum += *accum;
	if ( freeze ) {
/*		AccPos = *position; bug 12-apr-2008 JV */
		oldipointer = AR.CompressPointer;
		do {
			AR.CompressPointer = oldipointer;
/*			if ( ( l = GetFromStore(accum,&AccPos,renumber,&InCompState,nexpr) ) < 0 ) bug 12-apr-2008 JV */
			if ( ( l = GetFromStore(accum,position,renumber,&InCompState,nexpr) ) < 0 )
				goto PasErr;
			if ( !l ) { *accum = 0; return(0); }
			r = accum;
			m = r + *r;
			m -= ABS(m[-1]);
			r++;
			while ( r < m && *r != HAAKJE ) r += r[1];
			if ( r >= m ) {
				if ( *freeze != 4 ) l = -1;
			}
			else {
/*
				The algorithm for accepting terms with a given (freeze)
				representation outside brackets is rather crude. A refinement
				would be to store the part outside the bracket and skip the
				term when this part doesn't alter (and is unacceptable).
				Once accepting one can keep accepting till the bracket alters
				and then one may stop the generation. It is necessary to
				set up a struct to remember the bracket and the progress
				status.
*/
				m = AT.WorkPointer;
				s2 = r;
				r = accum;
				*m++ = WORDDIF(s2,r) + 3;
				r++;
				while ( r < s2 ) *m++ = *r++;
				*m++ = 1; *m++ = 1; *m++ = 3;
				m = AT.WorkPointer;
				if ( Normalize(BHEAD AT.WorkPointer) ) goto PasErr;
				r = freeze;
				i = *m;
				while ( --i >= 0 && *m++ == *r++ ) {}
				if ( i > 0 ) {
					l = -1;
				}
				else {	/* Term to be accepted */
					r = accum;
					s1 = r + *r;
					r++;
					m = s2;
					m += m[1];
					do { *r++ = *m++; } while ( m < s1 );
					*accum = l = WORDDIF(r,accum);
				}
			}
		} while ( l < 0 );
		retlength = InCompState;
/*		retlength = DIFBASE(AccPos,*position) / sizeof(WORD);  bug 12-apr-2008 JV */
	}
	else {
		if ( ( l = GetFromStore(accum,position,renumber,&InCompState,nexpr) ) < 0 ) {
			MLOCK(ErrorMessageLock);
			MesCall("PasteFile");
			MUNLOCK(ErrorMessageLock);
			SETERROR(-1)
		}
		if ( l == 0 ) { *accum = 0; return(0); }
		retlength = InCompState;
	}
	accum += l;
	if ( accum > stop ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Buffer too small in PasteFile");
		MUNLOCK(ErrorMessageLock);
		SETERROR(-1)
	}
	*accum = 0;
	*accfill = accum;
	return(retlength);
PasErr:
	MLOCK(ErrorMessageLock);
	MesCall("PasteFile");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}
 		
/*
 		#] PasteFile : 
 		#[ PasteTerm :			WORD PasteTerm(number,accum,position,times,divby)
*/
/**
 *		Puts the term at position in the accumulator accum at position
 *		'number+1'. if times > 0 the coefficient of this term is
 *		multiplied by times/divby.
 *
 *		@param number The number of term fragments in accum that should be skipped
 *		@param accum  The accumulator of term fragments
 *		@param position A position in (typically) a compiler buffer from where
 *		                a (piece of a) term comes.
 *		@param times  Multiply the result by this
 *		@param divby  Divide the result by this.
 *
 *		This routine is typically used when we have to replace a (sub)expression
 *		pointer by a power of a (sub)expression. This uses mostly a binomial
 *		expansion and the new term is the old term multiplied one by one
 *		by terms of the new expression. The factors times and divby keep track
 *		of the binomial coefficient.
 *		Once this is complete, the routine FiniTerm will make the contents
 *		of the accumulator into a proper term that still needs to be normalized.
 */

WORD *PasteTerm(PHEAD WORD number, WORD *accum, WORD *position, WORD times, WORD divby)
{
	GETBIDENTITY
	WORD *t, *r, x, y, z;
	WORD *m, *u, l1, a[2];
    m = (WORD *)(((UBYTE *)(accum)) + AM.MaxTer);
/*    m = (WORD *)(((UBYTE *)(accum)) + 2*AM.MaxTer); */
	*accum++ = number;
	while ( --number >= 0 ) accum += *accum;
	if ( times == divby ) {
		t = position;
		r = t + *t;
		if ( t < r ) do { *accum++ = *t++; } while ( t < r );
	}
	else {
		u = accum;
		t = position;
		r = t + *t - 1;
		l1 = *r;
		r -= ABS(*r) - 1;
		if ( t < r ) do { *accum++ = *t++; } while ( t < r );
		if ( divby > times ) { x = divby; y = times; }
		else { x = times; y = divby; }
		z = x%y;
		while ( z ) { x = y; y = z; z = x%y; }
		if ( y != 1 ) { divby /= y; times /= y; }
		a[1] = divby;
		a[0] = times;
		if ( MulRat(BHEAD (UWORD *)t,REDLENG(l1),(UWORD *)a,1,(UWORD *)accum,&l1) ) {
			MLOCK(ErrorMessageLock);
			MesCall("PasteTerm");
			MUNLOCK(ErrorMessageLock);
			return(0);
		}
		x = l1;
		x *= 2;
		if ( x < 0 ) { accum -= x; *accum++ = x - 1; }
		else		 { accum += x; *accum++ = x + 1; }
		*u = WORDDIF(accum,u);
	}
	if ( accum >= m ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Buffer too small in PasteTerm");
		MUNLOCK(ErrorMessageLock);
		return(0);
	}
	*accum = 0;
	return(accum);
}

/*
 		#] PasteTerm : 
 		#[ FiniTerm :			WORD FiniTerm(term,accum,termout,number)
*/
/**
 *		Concatenates the contents of the accumulator into a single
 *		legal term, which replaces the subexpression pointer
 *
 *		@param term    the input term with the (sub)expression subterm
 *		@param accum   the accumulator with the term fragments
 *		@param termout the location where the output should be written
 *		@param number  the number of term fragments in the accumulator
 *		@param tepos   the position of the subterm in term to be replaced
 */

WORD FiniTerm(PHEAD WORD *term, WORD *accum, WORD *termout, WORD number, WORD tepos)
{
	GETBIDENTITY
	WORD *m, *t, *r, i, numacc, l2, ipp;
	WORD *u, *v, l1, *coef = AT.WorkPointer, *oldaccum;
	if ( ( AT.WorkPointer = coef + 2*AM.MaxTal ) > AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	oldaccum = accum;
	t = term;
	m = t + *t - 1;
	l1 = REDLENG(*m);
	i = ABS(*m) - 1;
	r = coef + i;
	do { *--r = *--m; } while ( --i > 0 ); /* Copies coefficient */
	if ( tepos > 0 ) {
		t = term + tepos;
		goto foundit;
	}
	t++;
	if ( t < m ) do {
		if ( ( ( *t == SUBEXPRESSION && ( *(r=t+t[1]) != SUBEXPRESSION
		|| r >= m || !r[3] ) ) || *t == EXPRESSION ) && t[2] == number && t[3] ) {
foundit:;
			u = m;
			r = term;
			m = termout;
			if ( r < t ) do { *m++ = *r++; } while ( r < t );
			numacc = *accum++;
			if ( numacc >= 0 ) do {
				if ( *t == EXPRESSION ) {
					v = t + t[1];
					r = t + SUBEXPSIZE;
					while ( r < v ) {
						if ( *r == WILDCARDS ) {
							r += 2;
							i = *--m;
							if ( ( l2 = WildFill(BHEAD m,accum,r) ) < 0 ) goto FiniCall;
							goto AllWild;
						}
						r += r[1];
					}
					goto NoWild;
				}
				else if ( t[1] > SUBEXPSIZE && t[SUBEXPSIZE] != FROMBRAC ) {
					i = *--m;
					if ( ( l2 = WildFill(BHEAD m,accum,t) ) < 0 ) goto FiniCall;
AllWild:			*m = i;
					m += l2-1;
					l2 = *m;
					m -= ABS(l2) - 1;
					r = m;
				}
				else {
NoWild:				r = accum;
					v = r + *r - 1;
					l2 = *v;
					v -= ABS(l2) - 1;
					r++;
					if ( r < v ) do { *m++ = *r++; } while ( r < v );
				}
				if ( *r == 1 && r[1] == 1 && ABS(l2) == 3 ) {
					if ( l2 < 0 ) l1 = -l1;
				}
				else {
					l2 = REDLENG(l2);
					if ( l2 == 0 ) {
						t = oldaccum;
						numacc = *t++;
						AO.OutSkip = 3;
						FiniLine();
						while ( --numacc >= 0 ) {
							i = *t;
							while ( --i >= 0 ) {
								TalToLine((UWORD)(*t++));
								TokenToLine((UBYTE *)"  ");
							}
						}
						AO.OutSkip = 0;
						FiniLine();
						goto FiniCall;
					}
					if ( MulRat(BHEAD (UWORD *)coef,l1,(UWORD *)r,l2,(UWORD *)coef,&l1) ) goto FiniCall;
					if ( AN.ncmod != 0 && TakeModulus((UWORD *)coef,&l1,AC.cmod,AN.ncmod,UNPACK|AC.modmode) ) goto FiniCall;
				}
				accum += *accum;
			} while ( --numacc >= 0 );
			if ( *t == SUBEXPRESSION ) {
				 while ( t+t[1] < u && t[t[1]] == DOLLAREXPR2 ) t += t[1];
			}
			t += t[1];
			if ( t < u ) do { *m++ = *t++; } while ( t < u );
			l2 = l1;
/*
			Code to economize when taking x = (a+b)/2
*/
			r = termout+1;
			while ( r < m ) {
				if ( *r == SUBEXPRESSION ) {
					t = r + r[1];
					l1 = (WORD)(cbuf[r[4]].CanCommu[r[2]]);
					while ( t < m ) {
						if ( *t == SUBEXPRESSION &&
							t[1] == r[1] && t[2] == r[2] && t[4] == r[4] ) {
								i = t[1] - SUBEXPSIZE;
								u = r + SUBEXPSIZE; v = t + SUBEXPSIZE;
								while ( i > 0 ) {
									if ( *v++ != *u++ ) break;
									i--;
								}
								if ( i <= 0 ) {
									u = r;
									r[3] += t[3];
									r = t + t[1];
									while ( r < m ) *t++ = *r++;
									m = t;
									r = u;
									goto Nextr;
								}
								if ( l1 && cbuf[t[4]].CanCommu[t[2]] ) break;
								while ( t+t[1] < m && t[t[1]] == DOLLAREXPR2 ) t += t[1];
						}
						else if ( l1 ) {
							if ( *t == SUBEXPRESSION && cbuf[t[4]].CanCommu[t[2]] )
								break;
							if ( *t >= FUNCTION+WILDOFFSET )
								ipp = *t - WILDOFFSET;
							else ipp = *t;
							if ( *t >= FUNCTION
							 && functions[ipp-FUNCTION].commute && l1 ) break;
							if ( *t == EXPRESSION ) break;
						}
						t += t[1];
					}
					r += r[1];
				}
				else r += r[1];
Nextr:;
			}

			i = ABS(l2);
			i *= 2;
			i++;
			l2 = ( l2 >= 0 ) ? i: -i;
			r = coef;
			while ( --i > 0 ) *m++ = *r++;
			*m++ = l2;
			*termout = WORDDIF(m,termout);
			AT.WorkPointer = coef;
			return(0);
		}
		t += t[1];
	} while ( t < m );
	AT.WorkPointer = coef;
	return(1);

FiniCall:
	MLOCK(ErrorMessageLock);
	MesCall("FiniTerm");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] FiniTerm : 
 		#[ Generator :			WORD Generator(BHEAD term,level)
*/
 
static WORD zeroDollar[] = { 0, 0 };
/*
static LONG debugcounter = 0;
*/

/**
 *		The heart of the program.
 *		Here the expansion tree is set up in one giant recursion
 *
 *		@param term   the input term. may be overwritten
 *		@param level  the level in the compiler buffer (number of statement)
 *		@return Normal conventions (OK = 0).
 *
 *		The routine looks first whether there are unsubstituted (sub)expressions.
 *		If so, one of them gets inserted term by term and the new term is
 *		used in a renewed call to Generator.
 *		If there are no (sub)expressions, the term is normalized, the
 *		compiler level is raised (next statement) and the program looks
 *		what type of statement this is. If this is a special statement it
 *		is either treated on the spot or the appropriate routine is called.
 *		If it is a substitution, the pattern matcher is called (TestMatch)
 *		which tells whether there was a match. If so we need to call
 *		TestSub again to test for (sub)expressions.
 *		If we run out of levels, the term receives a final treatment for
 *		modulus calculus and/or brackets and is then sent off to the
 *		sorting routines.
 */

WORD Generator(PHEAD WORD *term, WORD level)
{
	GETBIDENTITY
	WORD replac, *accum, *termout, *t, i, j, tepos, applyflag = 0, *StartBuf;
	WORD *a, power, power1, DumNow = AR.CurDum, oldtoprhs, oldatoprhs, retnorm, extractbuff;
	int *RepSto = AN.RepPoint, iscopy = 0;
	CBUF *C = cbuf+AM.rbufnum, *CC = cbuf + AT.ebufnum, *CCC = cbuf + AT.aebufnum;
	LONG posisub, oldcpointer, oldacpointer;
	DOLLARS d = 0;
	WORD numfac[5], idfunctionflag;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1, id;
#endif
	oldtoprhs = CC->numrhs;
	oldcpointer = CC->Pointer - CC->Buffer;
	oldatoprhs = CCC->numrhs;
	oldacpointer = CCC->Pointer - CCC->Buffer;
ReStart:
	if ( ( replac = TestSub(BHEAD term,level) ) == 0 ) {
		if ( applyflag ) { TableReset(); applyflag = 0; }
/*
		if ( AN.PolyNormFlag > 1 ) {
			if ( PolyFunMul(BHEAD term) < 0 ) goto GenCall;
			AN.PolyNormFlag = 0;
			if ( !*term ) goto Return0;
		}
*/
Renormalize:
		AN.PolyNormFlag = 0;
		AN.idfunctionflag = 0;
		if ( ( retnorm = Normalize(BHEAD term) ) != 0 ) {
			if ( retnorm > 0 ) {
				if ( AT.WorkPointer < term + *term ) AT.WorkPointer = term + *term;
				goto ReStart;
			}
			goto GenCall;
		}
		idfunctionflag = AN.idfunctionflag;
		if ( !*term ) { AN.PolyNormFlag = 0; goto Return0; }

		if ( AN.PolyNormFlag ) {
			if ( AN.PolyFunTodo == 0 ) {
				if ( PolyFunMul(BHEAD term) < 0 ) goto GenCall;
				if ( !*term ) { AN.PolyNormFlag = 0; goto Return0; }
			}
			else {
				WORD oldPolyFunExp = AR.PolyFunExp;
				AR.PolyFunExp = 0;
				if ( PolyFunMul(BHEAD term) < 0 ) goto GenCall;
				AT.WorkPointer = term+*term;
				AR.PolyFunExp = oldPolyFunExp;
				if ( !*term ) { AN.PolyNormFlag = 0; goto Return0; }
				if ( Normalize(BHEAD term) < 0 ) goto GenCall;
				if ( !*term ) { AN.PolyNormFlag = 0; goto Return0; }
				AT.WorkPointer = term+*term;
				if ( AN.PolyNormFlag ) {
					if ( PolyFunMul(BHEAD term) < 0 ) goto GenCall;
					if ( !*term ) { AN.PolyNormFlag = 0; goto Return0; }
					AT.WorkPointer = term+*term;
				}
				AN.PolyFunTodo = 0;
			}
		}
		if ( idfunctionflag > 0 ) {
			if ( TakeIDfunction(BHEAD term) ) {
				AT.WorkPointer = term + *term;
				goto ReStart;
			}
		}
		if ( AT.WorkPointer < (WORD *)(((UBYTE *)(term)) + AM.MaxTer) )
			 AT.WorkPointer = (WORD *)(((UBYTE *)(term)) + AM.MaxTer);
		do {
SkipCount:	level++;
			if ( level > AR.Cnumlhs ) {
				if ( AR.DeferFlag && AR.sLevel <= 0 ) {
#ifdef WITHMPI
				  if ( PF.me != MASTER && AC.mparallelflag == PARALLELFLAG && PF.exprtodo < 0 ) {
					if ( PF_Deferred(term,level) ) goto GenCall;
				  }
				  else
#endif
					if ( Deferred(BHEAD term,level) ) goto GenCall;
					goto Return0;
				}
				if ( AN.ncmod != 0 ) {
					if ( Modulus(term) ) goto GenCall;
					if ( !*term ) goto Return0;
				}
				if ( AR.CurDum > AM.IndDum && AR.sLevel <= 0 ) {
					WORD olddummies = AN.IndDum;
					AN.IndDum = AM.IndDum;
					ReNumber(BHEAD term); Normalize(BHEAD term);
					AN.IndDum = olddummies;
					if ( !*term ) goto Return0;
					olddummies = DetCurDum(BHEAD term);
					if ( olddummies > AR.MaxDum ) AR.MaxDum = olddummies;
				}
				if ( AR.PolyFun > 0 && ( AR.sLevel <= 0 || AN.FunSorts[AR.sLevel]->PolyFlag > 0 ) ) {
					if ( PrepPoly(BHEAD term,0) != 0 ) goto Return0;
				}
				else if ( AR.PolyFun > 0 ) {
					if ( PrepPoly(BHEAD term,1) != 0 ) goto Return0;
				}
				if ( AR.sLevel <= 0 && AR.BracketOn ) {
					if ( AT.WorkPointer < term + *term ) AT.WorkPointer = term + *term;
					termout = AT.WorkPointer;
					if ( AT.WorkPointer + *term + 3 > AT.WorkTop ) goto OverWork;
					if ( PutBracket(BHEAD term) ) return(-1);
					AN.RepPoint = RepSto;
					*AT.WorkPointer = 0;
					i = StoreTerm(BHEAD termout);
					AT.WorkPointer = termout;
					CC->numrhs = oldtoprhs;
					CC->Pointer = CC->Buffer + oldcpointer;
					CCC->numrhs = oldatoprhs;
					CCC->Pointer = CCC->Buffer + oldacpointer;
					return(i);
				}
				else {
					if ( AT.WorkPointer < term + *term ) AT.WorkPointer = term + *term;
					if ( AT.WorkPointer >= AT.WorkTop ) goto OverWork;
					*AT.WorkPointer = 0;
					AN.RepPoint = RepSto;
					i = StoreTerm(BHEAD term);
					CC->numrhs = oldtoprhs;
					CC->Pointer = CC->Buffer + oldcpointer;
					CCC->numrhs = oldatoprhs;
					CCC->Pointer = CCC->Buffer + oldacpointer;
					return(i);
				}
			}
			i = C->lhs[level][0];
			if ( i >= TYPECOUNT ) {
/*
			#[ Special action :
*/
				switch ( i ) {
				  case TYPECOUNT:
					if ( CountDo(term,C->lhs[level]) < C->lhs[level][2] ) {
						AT.WorkPointer = term + *term;
						goto Return0;
					}
					break;
				  case TYPEMULT:
					if ( MultDo(BHEAD term,C->lhs[level]) ) goto GenCall;
					goto ReStart;
				  case TYPEGOTO:
					level = AC.Labels[C->lhs[level][2]];
					break;
				  case TYPEDISCARD:
					AT.WorkPointer = term + *term;
					goto Return0;
				  case TYPEIF:
#ifdef WITHPTHREADS
					{
/*
						We may be writing in the space here when wildcards
						are involved in a match(). Hence we have to make
						a private copy here!!!!
*/
						WORD ic, jc, *ifcode, *jfcode;
						jfcode = C->lhs[level]; jc = jfcode[1];
						ifcode = AT.WorkPointer; AT.WorkPointer += jc;
						for ( ic = 0; ic < jc; ic++ ) ifcode[ic] = jfcode[ic];
						while ( !DoIfStatement(BHEAD ifcode,term) ) {
							level = C->lhs[level][2];
							if ( C->lhs[level][0] != TYPEELIF ) break;
						}
						AT.WorkPointer = ifcode;
					}
#else
					while ( !DoIfStatement(BHEAD C->lhs[level],term) ) {
						level = C->lhs[level][2];
						if ( C->lhs[level][0] != TYPEELIF ) break;
					}
#endif
					break;
				  case TYPEELIF:
					do {
						level = C->lhs[level][2];
					} while ( C->lhs[level][0] == TYPEELIF );
					break;
				  case TYPEELSE:
				  case TYPEENDIF:
					level = C->lhs[level][2];
					break;
				  case TYPESUMFIX:
					{
						WORD *cp = AR.CompressPointer, *op = AR.CompressPointer;
						WORD *tlhs = C->lhs[level] + 3, *m, jlhs;
						WORD theindex = C->lhs[level][2];
						if ( theindex < 0 ) {	/* $-variable */
#ifdef WITHPTHREADS
							int ddtype = -1;
							theindex = -theindex;
							d = Dollars + theindex;
							if ( AS.MultiThreaded ) {
								for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
									if ( theindex == ModOptdollars[nummodopt].number ) break;
								}
								if ( nummodopt < NumModOptdollars ) {
									ddtype = ModOptdollars[nummodopt].type;
									if ( ddtype == MODLOCAL ) {
										d = ModOptdollars[nummodopt].dstruct+AT.identity;
									}
									else {
										LOCK(d->pthreadslockread);
									}
								}
							}
#else
							theindex = -theindex;
							d = Dollars + theindex;
#endif

							if ( d->type != DOLINDEX
							|| d->index < AM.OffsetIndex
							|| d->index >= AM.OffsetIndex + WILDOFFSET ) {
								MLOCK(ErrorMessageLock);
								MesPrint("$%s should have been an index"
								,AC.dollarnames->namebuffer+d->name);
								AN.currentTerm = term;
								MesPrint("Current term: %t");
								AN.listinprint = printscratch;
								printscratch[0] = DOLLAREXPRESSION;
								printscratch[1] = theindex;
								MesPrint("$%s = %$"
								,AC.dollarnames->namebuffer+d->name);
								MUNLOCK(ErrorMessageLock);
#ifdef WITHPTHREADS
							if ( ddtype > 0 && ddtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
								goto GenCall;
							}
							theindex = d->index;
#ifdef WITHPTHREADS
							if ( ddtype > 0 && ddtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
						}
						cp[1] = SUBEXPSIZE+4;
						cp += SUBEXPSIZE;
						*cp++ = INDTOIND;
						*cp++ = 4;
						*cp++ = theindex;
						i = C->lhs[level][1] - 3;
						cp++;
						AR.CompressPointer = cp;
						while ( --i >= 0 ) {
							cp[-1] = *tlhs++;
							termout = AT.WorkPointer;
							if ( ( jlhs = WildFill(BHEAD termout,term,op)) < 0 )
								goto GenCall;
							m = term;
							jlhs = *m;
							while ( --jlhs >= 0 ) {
								if ( *m++ != *termout++ ) break;
							}
							if ( jlhs >= 0 ) {
								termout = AT.WorkPointer;
								AT.WorkPointer = termout + *termout;
								if ( Generator(BHEAD termout,level) ) goto GenCall;
								AT.WorkPointer = termout;
							}
							else {
								AR.CompressPointer = op;
								goto SkipCount;
							}
						}
						AR.CompressPointer = op;
						goto CommonEnd;
					}
				  case TYPESUM:
					{
						WORD *wp, *cp = AR.CompressPointer, *op = AR.CompressPointer;
						WORD theindex;
						WORD *ow;
/*
						At this point it is safest to determine CurDum
*/
						AR.CurDum = DetCurDum(BHEAD term);
						i = C->lhs[level][1]-2;
						wp = C->lhs[level] + 2;
						cp[1] = SUBEXPSIZE+4*i;
						cp += SUBEXPSIZE;
						while ( --i >= 0 ) {
							theindex = *wp++;
							if ( theindex < 0 ) {	/* $-variable */
#ifdef WITHPTHREADS
								int ddtype = -1;
								theindex = -theindex;
								d = Dollars + theindex;
								if ( AS.MultiThreaded ) {
									for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
										if ( theindex == ModOptdollars[nummodopt].number ) break;
									}
									if ( nummodopt < NumModOptdollars ) {
										ddtype = ModOptdollars[nummodopt].type;
										if ( ddtype == MODLOCAL ) {
											d = ModOptdollars[nummodopt].dstruct+AT.identity;
										}
										else {
											LOCK(d->pthreadslockread);
										}
									}
								}
#else
								theindex = -theindex;
								d = Dollars + theindex;
#endif
								if ( d->type != DOLINDEX
								|| d->index < AM.OffsetIndex
								|| d->index >= AM.OffsetIndex + WILDOFFSET ) {
									MLOCK(ErrorMessageLock);
									MesPrint("$%s should have been an index"
									,AC.dollarnames->namebuffer+d->name);
									AN.currentTerm = term;
									MesPrint("Current term: %t");
									AN.listinprint = printscratch;
									printscratch[0] = DOLLAREXPRESSION;
									printscratch[1] = theindex;
									MesPrint("$%s = %$"
									,AC.dollarnames->namebuffer+d->name);
									MUNLOCK(ErrorMessageLock);
#ifdef WITHPTHREADS
									if ( ddtype > 0 && ddtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
									goto GenCall;
								}
								theindex = d->index;
#ifdef WITHPTHREADS
								if ( ddtype > 0 && ddtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
							}
							*cp++ = INDTOIND;
							*cp++ = 4;
							*cp++ = theindex;
							*cp++ = ++AR.CurDum;
						}
						ow = AT.WorkPointer;
						AR.CompressPointer = cp;
						if ( WildFill(BHEAD ow,term,op) < 0 ) goto GenCall;
						AR.CompressPointer = op;
						i = ow[0];
						for ( j = 0; j < i; j++ ) term[j] = ow[j];
						AT.WorkPointer = ow;
						ReNumber(BHEAD term);
						goto Renormalize;
					}
				  case TYPECHISHOLM:
					if ( Chisholm(BHEAD term,level) ) goto GenCall;
CommonEnd:
					AT.WorkPointer = term + *term;
					goto Return0;
				  case TYPEARG:
					if ( ( i = execarg(BHEAD term,level) ) < 0 ) goto GenCall;
					level = C->lhs[level][2];
					if ( i > 0 ) goto ReStart;
					break;
				  case TYPENORM:
				  case TYPENORM2:
				  case TYPENORM3:
				  case TYPENORM4:
				  case TYPESPLITARG:
				  case TYPESPLITARG2:
				  case TYPESPLITFIRSTARG:
				  case TYPESPLITLASTARG:
				  case TYPEARGTOEXTRASYMBOL:
					if ( execarg(BHEAD term,level) < 0 ) goto GenCall;
					level = C->lhs[level][2];
					break;
				  case TYPEFACTARG:
				  case TYPEFACTARG2:
					{ WORD jjj;
					if ( ( jjj = execarg(BHEAD term,level) ) < 0 ) goto GenCall;
					if ( jjj > 0 ) goto ReStart;
					level = C->lhs[level][2];
					break; }
				  case TYPEEXIT:
					if ( C->lhs[level][2] > 0 ) {
						MLOCK(ErrorMessageLock);
						MesPrint("%s",C->lhs[level]+3);
						MUNLOCK(ErrorMessageLock);
					}
					Terminate(-1);
					goto GenCall;
				  case TYPESETEXIT:
					AM.exitflag = 1; /* no danger of race conditions */
					break;
				  case TYPEPRINT:
					AN.currentTerm = term;
					AN.numlistinprint = (C->lhs[level][1] - C->lhs[level][4] - 5)/2;
					AN.listinprint = C->lhs[level]+5+C->lhs[level][4];
					MLOCK(ErrorMessageLock);
					AO.ErrorBlock = 1;
					MesPrint((char *)(C->lhs[level]+5));
					AO.ErrorBlock = 0;
					MUNLOCK(ErrorMessageLock);
					break;
				  case TYPEFPRINT:
					{
					int oldFOflag;
					WORD oldPrintType, oldLogHandle = AC.LogHandle;
					AC.LogHandle = C->lhs[level][2];
					MLOCK(ErrorMessageLock);
					oldFOflag = AM.FileOnlyFlag;
					oldPrintType = AO.PrintType;
					if ( AC.LogHandle >= 0 ) {
						AM.FileOnlyFlag = 1;
						AO.PrintType |= PRINTLFILE;
					}
					AO.PrintType |= C->lhs[level][3];
					AN.currentTerm = term;
					AN.numlistinprint = (C->lhs[level][1] - C->lhs[level][4] - 5)/2;
					AN.listinprint = C->lhs[level]+5+C->lhs[level][4];
					MesPrint((char *)(C->lhs[level]+5));
					AO.PrintType = oldPrintType;
					AM.FileOnlyFlag = oldFOflag;
					MUNLOCK(ErrorMessageLock);
					AC.LogHandle = oldLogHandle;
					}
					break;
				  case TYPEREDEFPRE:
					j = C->lhs[level][2];
#ifdef WITHMPI
					{
						/*
						 * Regardless of parallel/nonparallel switch, we need to set
						 * AC.inputnumbers[ii], which indicates that the corresponding
						 * preprocessor variable is redefined and so we need to
						 * send/broadcast it.
						 */
						int ii;
						for ( ii = 0; ii < AC.numpfirstnum; ii++ ) {
							if ( AC.pfirstnum[ii] == j ) break;
						}
						AC.inputnumbers[ii] = AN.ninterms;
					}
#endif
#ifdef WITHPTHREADS
					if ( AS.MultiThreaded ) {
						int ii;
						for ( ii = 0; ii < AC.numpfirstnum; ii++ ) {
							if ( AC.pfirstnum[ii] == j ) break;
						}
						if ( AN.inputnumber < AC.inputnumbers[ii] ) break;
						LOCK(AP.PreVarLock);
						if ( AN.inputnumber >= AC.inputnumbers[ii] ) {
							a = C->lhs[level]+4;
							if ( a[a[-1]] == 0 )
								PutPreVar(PreVar[j].name,(UBYTE *)(a),0,1);
							else
								PutPreVar(PreVar[j].name,(UBYTE *)(a)
									,(UBYTE *)(a+a[-1]+1),1);
/*
							PutPreVar(PreVar[j].name,(UBYTE *)(C->lhs[level]+4),0,1);
*/
							AC.inputnumbers[ii] = AN.inputnumber;
						}
						UNLOCK(AP.PreVarLock);
					}
					else
#endif
					{
						a = C->lhs[level]+4;
						LOCK(AP.PreVarLock);
						if ( a[a[-1]] == 0 )
							PutPreVar(PreVar[j].name,(UBYTE *)(a),0,1);
						else
							PutPreVar(PreVar[j].name,(UBYTE *)(a)
								,(UBYTE *)(a+a[-1]+1),1);
						UNLOCK(AP.PreVarLock);
					}
					break;
				  case TYPERENUMBER:
					AT.WorkPointer = term + *term;
					if ( FullRenumber(BHEAD term,C->lhs[level][2]) ) goto GenCall;
					AT.WorkPointer = term + *term;
					if ( *term == 0 ) goto Return0;
					break;
				  case TYPETRY:
					if ( TryDo(BHEAD term,C->lhs[level],level) ) goto GenCall;
					AT.WorkPointer = term + *term;
					goto Return0;
				  case TYPEASSIGN:
					{ WORD onc = AR.NoCompress, oldEside = AR.Eside;
					WORD oldrepeat = *AN.RepPoint;
/*
					Here we have to assign an expression to a $ variable.
*/
					AR.Eside = RHSIDE;
					AR.NoCompress = 1;
					AN.cTerm = AN.currentTerm = term;
					AT.WorkPointer = term + *term;
					*AT.WorkPointer++ = 0;
					if ( AssignDollar(BHEAD term,level) ) goto GenCall;
					AT.WorkPointer = term + *term;
					AN.cTerm = 0;
					*AN.RepPoint = oldrepeat;
					AR.NoCompress = onc;
					AR.Eside = oldEside;
					break;
					}
				  case TYPEFINDLOOP:
					if ( Lus(term,C->lhs[level][3],C->lhs[level][4],
					C->lhs[level][5],C->lhs[level][6],C->lhs[level][2]) ) {
						AT.WorkPointer = term + *term;
						goto Renormalize;
					}
					break;
				  case TYPEINSIDE:
					if ( InsideDollar(BHEAD C->lhs[level],level) < 0 ) goto GenCall;
					level = C->lhs[level][2];
					break;
				  case TYPETERM:
					retnorm = execterm(BHEAD term,level);
					AN.RepPoint = RepSto;
					AR.CurDum = DumNow;
					CC->numrhs = oldtoprhs;
					CC->Pointer = CC->Buffer + oldcpointer;
					CCC->numrhs = oldatoprhs;
					CCC->Pointer = CCC->Buffer + oldacpointer;
					return(retnorm);
				  case TYPEDETCURDUM:
					AT.WorkPointer = term + *term;
					AR.CurDum = DetCurDum(BHEAD term);
					break;
				  case TYPEINEXPRESSION:
					{WORD *ll = C->lhs[level];
					int numexprs = (int)(ll[1]-3);
					ll += 3;
					while ( numexprs-- >= 0 ) {
						if ( *ll == AR.CurExpr ) break;
						ll++;
					}
					if ( numexprs < 0 ) level = C->lhs[level][2];
					}
					break;
				  case TYPEMERGE:
					AT.WorkPointer = term + *term;
					if ( DoShuffle(term,level,C->lhs[level][2],C->lhs[level][3]) )
						goto GenCall;
					AT.WorkPointer = term + *term;
					goto Return0;
				  case TYPESTUFFLE:
					AT.WorkPointer = term + *term;
					if ( DoStuffle(term,level,C->lhs[level][2],C->lhs[level][3]) )
						goto GenCall;
					AT.WorkPointer = term + *term;
					goto Return0;
				  case TYPETESTUSE:
					AT.WorkPointer = term + *term;
					if ( TestUse(term,level) ) goto GenCall;
					AT.WorkPointer = term + *term;
					break;
				  case TYPEAPPLY:
					AT.WorkPointer = term + *term;
					if ( ApplyExec(term,C->lhs[level][2],level) < C->lhs[level][2] ) {
						AT.WorkPointer = term + *term;
						*AN.RepPoint = 1;
						goto ReStart;
					}
					AT.WorkPointer = term + *term;
					break;
/*
				  case TYPEAPPLYRESET:
					AT.WorkPointer = term + *term;
					if ( ApplyReset(level) ) goto GenCall;
					AT.WorkPointer = term + *term;
					break;
*/
				  case TYPECHAININ:
					AT.WorkPointer = term + *term;
					if ( ChainIn(BHEAD term,C->lhs[level][2]) ) goto GenCall;
					AT.WorkPointer = term + *term;
					break;
				  case TYPECHAINOUT:
					AT.WorkPointer = term + *term;
					if ( ChainOut(BHEAD term,C->lhs[level][2]) ) goto GenCall;
					AT.WorkPointer = term + *term;
					break;
				  case TYPEFACTOR:
					AT.WorkPointer = term + *term;
					if ( DollarFactorize(BHEAD C->lhs[level][2]) ) goto GenCall;
					AT.WorkPointer = term + *term;
					break;
				  case TYPEARGIMPLODE:
					AT.WorkPointer = term + *term;
					if ( ArgumentImplode(BHEAD term,C->lhs[level]) ) goto GenCall;
					AT.WorkPointer = term + *term;
					break;
				  case TYPEARGEXPLODE:
					AT.WorkPointer = term + *term;
					if ( ArgumentExplode(BHEAD term,C->lhs[level]) ) goto GenCall;
					AT.WorkPointer = term + *term;
					break;
				  case TYPEDENOMINATORS:
					DenToFunction(term,C->lhs[level][2]);
					break;
				  case TYPEDROPCOEFFICIENT:
					DropCoefficient(BHEAD term);
					break;
				  case TYPETRANSFORM:
					AT.WorkPointer = term + *term;
					if ( RunTransform(BHEAD term,C->lhs[level]+2) ) goto GenCall;
					AT.WorkPointer = term + *term;
					if ( *term == 0 ) goto Return0;
					goto ReStart;
				  case TYPETOPOLYNOMIAL:
					AT.WorkPointer = term + *term;
					termout = AT.WorkPointer;
					if ( ConvertToPoly(BHEAD term,termout,C->lhs[level],0) < 0 ) goto GenCall;
					if ( *termout == 0 ) goto Return0;
					i = termout[0]; t = term; NCOPY(t,termout,i);
					AT.WorkPointer = term + *term;
					break;
				  case TYPEFROMPOLYNOMIAL:
					AT.WorkPointer = term + *term;
					termout = AT.WorkPointer;
					if ( ConvertFromPoly(BHEAD term,termout,0,numxsymbol,0,0) < 0 ) goto GenCall;
					if ( *term == 0 ) goto Return0;
					i = termout[0]; t = term; NCOPY(t,termout,i);
					AT.WorkPointer = term + *term;
					goto ReStart;
				  case TYPEDOLOOP:
					level = TestDoLoop(BHEAD C->lhs[level],level);
					if ( level < 0 ) goto GenCall;
					break;
				  case TYPEENDDOLOOP:
					level = TestEndDoLoop(BHEAD C->lhs[C->lhs[level][2]],C->lhs[level][2]);
					if ( level < 0 ) goto GenCall;
					break;
				  case TYPEDROPSYMBOLS:
					DropSymbols(BHEAD term);
					break;
				  case TYPEPUTINSIDE:
					AT.WorkPointer = term + *term;
					if ( PutInside(BHEAD term,C->lhs[level]) < 0 ) goto GenCall;
					AT.WorkPointer = term + *term;
					/*
					 * We need to call Generator() to convert slow notation to
					 * fast notation, which fixes Issue #30.
					 */
					if ( Generator(BHEAD term,level) < 0 ) goto GenCall;
					goto Return0;
				  case TYPETOSPECTATOR:
					if ( PutInSpectator(term,C->lhs[level][2]) < 0 ) goto GenCall;
					goto Return0;
				  case TYPECANONICALIZE:
					AT.WorkPointer = term + *term;
					if ( DoCanonicalize(BHEAD term,C->lhs[level]) ) goto GenCall;
					AT.WorkPointer = term + *term;
					if ( *term == 0 ) goto Return0;
					break;
				  case TYPESWITCH:
					AT.WorkPointer = term + *term;
					if ( DoSwitch(BHEAD term,C->lhs[level]) ) goto GenCall;
					goto Return0;
				  case TYPEENDSWITCH:
					AT.WorkPointer = term + *term;
					if ( DoEndSwitch(BHEAD term,C->lhs[level]) ) goto GenCall;
					goto Return0;
				}
				goto SkipCount;
/*
			#] Special action : 
*/
			}
		} while ( ( i = TestMatch(BHEAD term,&level) ) == 0 );
		if ( AT.WorkPointer < term + *term ) AT.WorkPointer = term + *term;
		if ( i > 0 ) replac = TestSub(BHEAD term,level);
		else replac = i;
		if ( replac >= 0 || AT.TMout[1] != SYMMETRIZE ) {
			*AN.RepPoint = 1;
			AR.expchanged = 1;
		}
		if ( replac < 0 ) {		/* Terms come from automatic generation */
AutoGen:	i = *AT.TMout;
			t = termout = AT.WorkPointer;
			if ( ( AT.WorkPointer += i ) > AT.WorkTop ) goto OverWork;
			accum = AT.TMout;
			while ( --i >= 0 ) *t++ = *accum++;
			if ( (*(FG.Operation[termout[1]]))(BHEAD term,termout,replac,level) ) goto GenCall;
			AT.WorkPointer = termout;
			goto Return0;
		}
	}
	if ( applyflag ) { TableReset(); applyflag = 0; }
/*	DumNow = AR.CurDum; */

	if ( AN.TeInFun ) {	/* Match in function argument */
		if ( AN.TeInFun < 0 && !AN.TeSuOut ) {

			if ( AR.TePos >= 0 ) goto AutoGen;
			switch ( AN.TeInFun ) {
				case -1:
					if ( DoDistrib(BHEAD term,level) ) goto GenCall;
					break;
				case -2:
					if ( DoDelta3(BHEAD term,level) ) goto GenCall;
					break;
				case -3:
					if ( DoTableExpansion(term,level) ) goto GenCall;
					break;
				case -4:
					if ( FactorIn(BHEAD term,level) ) goto GenCall;
					break;
				case -5:
					if ( FactorInExpr(BHEAD term,level) ) goto GenCall;
					break;
				case -6:
					if ( TermsInBracket(BHEAD term,level) < 0 ) goto GenCall;
					break;
				case -7:
					if ( ExtraSymFun(BHEAD term,level) < 0 ) goto GenCall;
					break;
				case -8:
					if ( GCDfunction(BHEAD term,level) < 0 ) goto GenCall;
					break;
				case -9:
					if ( DIVfunction(BHEAD term,level,0) < 0 ) goto GenCall;
					break;
				case -10:
					if ( DIVfunction(BHEAD term,level,1) < 0 ) goto GenCall;
					break;
				case -11:
					if ( DIVfunction(BHEAD term,level,2) < 0 ) goto GenCall;
					break;
				case -12:
					if ( DoPermutations(BHEAD term,level) ) goto GenCall;
					break;
				case -13:
					if ( DoPartitions(BHEAD term,level) ) goto GenCall;
					break;
				case -14:
					if ( DIVfunction(BHEAD term,level,3) < 0 ) goto GenCall;
					break;
				case -15:
					if ( GenTopologies(BHEAD term,level) < 0 ) goto GenCall;
					break;
				case -16:
					if ( GenDiagrams(BHEAD term,level) < 0 ) goto GenCall;
					break;
			}
		}
		else {
			termout = AT.WorkPointer;
		    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
			if ( AT.WorkPointer > AT.WorkTop ) goto OverWork;
			if ( InFunction(BHEAD term,termout) ) goto GenCall;
			AT.WorkPointer = termout + *termout;
			*AN.RepPoint = 1;
			AR.expchanged = 1;
			if ( *termout && Generator(BHEAD termout,level) < 0 ) goto GenCall;
			AT.WorkPointer = termout;
		}
	}
	else if ( replac > 0 ) {
		power = AN.TeSuOut;
		tepos = AR.TePos;
		if ( power < 0 ) {	/* Table expansion */
			power = -power; tepos = 0;
		}
		extractbuff = AT.TMbuff;
		if ( extractbuff == AM.dbufnum ) {
			d = DolToTerms(BHEAD replac);
			if ( d && d->where != 0 ) {
				iscopy = 1;
				if ( AT.TMdolfac > 0 ) {	/* We need a factor */
				  if ( AT.TMdolfac == 1 ) {
					if ( d->nfactors ) {
						numfac[0] = 4;
						numfac[1] = d->nfactors;
						numfac[2] = 1;
						numfac[3] = 3;
						numfac[4] = 0;
					}
					else {
						numfac[0] = 0;
					}
					StartBuf = numfac;
				  }
				  else {
					if ( (AT.TMdolfac-1) > d->nfactors && d->nfactors > 0 ) {
						MLOCK(ErrorMessageLock);
						MesPrint("Attempt to use an nonexisting factor %d of a $-variable",(WORD)(AT.TMdolfac-1));
						if ( d->nfactors == 1 )
							MesPrint("There is only one factor");
						else
							MesPrint("There are only %d factors",(WORD)(d->nfactors));
						MUNLOCK(ErrorMessageLock);
						goto GenCall;
					}
					if ( d->nfactors > 1 ) {
						DOLLARS dd;
						LONG dsize;
						WORD *td1, *td2;
						dd = Dollars + replac;
#ifdef WITHPTHREADS
						{
							int nummodopt, dtype = -1;
							if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
								for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
									if ( replac == ModOptdollars[nummodopt].number ) break;
								}
								if ( nummodopt < NumModOptdollars ) {
									dtype = ModOptdollars[nummodopt].type;
									if ( dtype == MODLOCAL ) {
										dd = ModOptdollars[nummodopt].dstruct+AT.identity;
									}
								}
							}
						}
#endif
						dsize = dd->factors[AT.TMdolfac-2].size;
/*
						We copy only the factor we need
*/
						if ( dsize == 0 ) {
							numfac[0] = 4;
							numfac[1] = d->factors[AT.TMdolfac-2].value;
							numfac[2] = 1;
							numfac[3] = 3;
							numfac[4] = 0;
							StartBuf = numfac;
							if ( numfac[1] < 0 ) {
								numfac[1] = -numfac[1];
								numfac[3] = -numfac[3];
							}
						}
						else {
						d->factors[AT.TMdolfac-2].where = td2 = (WORD *)Malloc1(
							(dsize+1)*sizeof(WORD),"Copy of factor");
						td1 = dd->factors[AT.TMdolfac-2].where;
						StartBuf = td2;
						d->size = dsize; d->type = DOLTERMS;
						NCOPY(td2,td1,dsize);
						*td2 = 0;
						}
					}
					else if ( d->nfactors == 1 ) {
						StartBuf = d->where;
					}
					else {
						MLOCK(ErrorMessageLock);
						if ( d->nfactors == 0 ) {
							MesPrint("Attempt to use factor %d of an unfactored $-variable",(WORD)(AT.TMdolfac-1));
						}
						else {
							MesPrint("Internal error. Illegal number of factors for $-variable");
						}
						MUNLOCK(ErrorMessageLock);
						goto GenCall;
					}
				  }
				}
				else StartBuf = d->where;
			}
			else {
				d = Dollars + replac;
				StartBuf = zeroDollar;
			}
			posisub = 0;
			i = DetCommu(d->where);
#ifdef WITHPTHREADS
			if ( AS.MultiThreaded ) {
				for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
					if ( replac == ModOptdollars[nummodopt].number ) break;
				}
				if ( nummodopt < NumModOptdollars ) {
					dtype = ModOptdollars[nummodopt].type;
					if ( dtype != MODLOCAL && dtype != MODSUM ) {
						if ( StartBuf[0] && StartBuf[StartBuf[0]] ) {
							MLOCK(ErrorMessageLock);
							MesPrint("A dollar variable with modoption max or min can have only one term");
							MUNLOCK(ErrorMessageLock);
							goto GenCall;
						}
						LOCK(d->pthreadslockread);
					}
				}
			}
#endif
		}
		else {
			StartBuf = cbuf[extractbuff].Buffer;
			posisub = cbuf[extractbuff].rhs[replac] - StartBuf;
			i = (WORD)cbuf[extractbuff].CanCommu[replac];
		}
		if ( power == 1 ) {		/* Just a single power */
			termout = AT.WorkPointer;
		    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
			if ( AT.WorkPointer > AT.WorkTop ) goto OverWork;
			while ( StartBuf[posisub] ) {
				if ( extractbuff == AT.allbufnum ) WildDollars(BHEAD &(StartBuf[posisub]));
			    AT.WorkPointer = (WORD *)(((UBYTE *)(termout)) + AM.MaxTer);
				if ( InsertTerm(BHEAD term,replac,extractbuff,
					&(StartBuf[posisub]),termout,tepos) < 0 ) goto GenCall;
				AT.WorkPointer = termout + *termout;
				*AN.RepPoint = 1;
				AR.expchanged = 1;
				posisub += StartBuf[posisub];
/*
					For multiple table substitutions it may be better to
					do modulus arithmetic right here
					Turns out to be not very effective.

				if ( AN.ncmod != 0 ) {
					if ( Modulus(termout) ) goto GenCall;
					if ( !*termout ) goto Return0;
				}
*/
#ifdef WITHPTHREADS
				if ( dtype > 0 && dtype != MODLOCAL && dtype != MODSUM ) { UNLOCK(d->pthreadslockread); }
				if ( ( AS.Balancing && CC->numrhs == 0 ) && StartBuf[posisub] ) {
					if ( ( id = ConditionalGetAvailableThread() ) >= 0 ) {
						if ( BalanceRunThread(BHEAD id,termout,level) < 0 ) goto GenCall;
					}
				}
				else
#endif
				if ( Generator(BHEAD termout,level) < 0 ) goto GenCall;
#ifdef WITHPTHREADS
				if ( dtype > 0 && dtype != MODLOCAL ) { dtype = 0; break; }
#endif
				if ( iscopy == 0 && ( extractbuff != AM.dbufnum ) ) {
/*
					There are cases in which a bigger buffer is created
					on the fly, like with wildcard buffers.
					We play it safe here. Maybe we can be more selective
					in some distant future?
*/
					StartBuf = cbuf[extractbuff].Buffer;
				}
			}
			if ( extractbuff == AT.allbufnum ) {
				CBUF *Ce = cbuf + extractbuff;
				Ce->Pointer = Ce->rhs[Ce->numrhs--];
			}
#ifdef WITHPTHREADS
			if ( dtype > 0 && dtype != MODLOCAL && dtype != MODSUM ) { UNLOCK(d->pthreadslockread); dtype = 0; }
#endif
			if ( iscopy ) {
				if ( d->nfactors > 1 ) {
					int j;
					for ( j = 0; j < d->nfactors; j++ ) {
						if ( d->factors[j].where ) M_free(d->factors[j].where,"Copy of factor");
					}
					M_free(d->factors,"Dollar factors");
				}
				M_free(d,"Copy of dollar variable");
				d = 0; iscopy = 0;
			}
			AT.WorkPointer = termout;
		}
		else if ( i <= 1 ) {		/* Use binomials */
			LONG posit, olw;
			WORD *same, *ow = AT.WorkPointer;
			LONG olpw = AT.posWorkPointer;
			power1 = power+1;
			WantAddLongs(power1);
			olw = posit = AT.lWorkPointer; AT.lWorkPointer += power1;
			same = ++AT.WorkPointer;
			a = accum = ( AT.WorkPointer += power1+1 );
		    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + 2*AM.MaxTer);
			if ( AT.WorkPointer > AT.WorkTop ) goto OverWork;
			AT.lWorkSpace[posit] = posisub;
			same[-1] = 0;
			*same = 1;
			*accum = 0;
			tepos = AR.TePos;
			i = 1;
			do {
				if ( StartBuf[AT.lWorkSpace[posit]] ) {
					if ( ( a = PasteTerm(BHEAD i-1,accum,
						&(StartBuf[AT.lWorkSpace[posit]]),i,*same) ) == 0 )
						goto GenCall;
					AT.lWorkSpace[posit+1] = AT.lWorkSpace[posit];
					same[1] = *same + 1;
					if ( i > 1 && AT.lWorkSpace[posit] < AT.lWorkSpace[posit-1] ) *same = 1;
					AT.lWorkSpace[posit] += StartBuf[AT.lWorkSpace[posit]];
					i++;
					posit++;
					same++;
				}
				else {
					i--; posit--; same--;
				}
				if ( i > power ) {
					termout = AT.WorkPointer = a;
				    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + 2*AM.MaxTer);
					if ( AT.WorkPointer > AT.WorkTop )
						goto OverWork;
					if ( FiniTerm(BHEAD term,accum,termout,replac,tepos) ) goto GenCall;
					AT.WorkPointer = termout + *termout;
					*AN.RepPoint = 1;
					AR.expchanged = 1;
#ifdef WITHPTHREADS
					if ( dtype > 0 && dtype != MODLOCAL && dtype != MODSUM ) { UNLOCK(d->pthreadslockread); }
					if ( ( AS.Balancing && CC->numrhs == 0 ) && ( i > 0 )
					&& ( id = ConditionalGetAvailableThread() ) >= 0 ) {
						if ( BalanceRunThread(BHEAD id,termout,level) < 0 ) goto GenCall;
					}
					else
#endif
					if ( Generator(BHEAD termout,level) ) goto GenCall;
#ifdef WITHPTHREADS
					if ( dtype > 0 && dtype != MODLOCAL ) { dtype = 0; break; }
#endif
					if ( iscopy == 0 && ( extractbuff != AM.dbufnum ) )
							StartBuf = cbuf[extractbuff].Buffer;
					i--; posit--; same--;
				}
			} while ( i > 0 );
#ifdef WITHPTHREADS
			if ( dtype > 0 && dtype != MODLOCAL && dtype != MODSUM ) { UNLOCK(d->pthreadslockread); dtype = 0; }
#endif
			if ( iscopy ) {
				if ( d->nfactors > 1 ) {
					int j;
					for ( j = 0; j < d->nfactors; j++ ) {
						if ( d->factors[j].where ) M_free(d->factors[j].where,"Copy of factor");
					}
					M_free(d->factors,"Dollar factors");
				}
				M_free(d,"Copy of dollar variable");
				d = 0; iscopy = 0;
			}
			AT.WorkPointer = ow; AT.lWorkPointer = olw; AT.posWorkPointer = olpw;
		}
		else {							/* No binomials */
			LONG posit, olw, olpw = AT.posWorkPointer;
			WantAddLongs(power);
			posit = olw = AT.lWorkPointer; AT.lWorkPointer += power;
			a = accum = AT.WorkPointer;
		    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + 2*AM.MaxTer);
			if ( AT.WorkPointer > AT.WorkTop ) goto OverWork;
			for ( i = 0; i < power; i++ ) AT.lWorkSpace[posit++] = posisub;
			posit = olw;
			*accum = 0;
			tepos = AR.TePos;
			i = 0;
			while ( i >= 0 ) {
				if ( StartBuf[AT.lWorkSpace[posit]] ) {
					if ( ( a = PasteTerm(BHEAD i,accum,
						&(StartBuf[AT.lWorkSpace[posit]]),1,1) ) == 0 ) goto GenCall;
					AT.lWorkSpace[posit] += StartBuf[AT.lWorkSpace[posit]];
					i++; posit++;
				}
				else {
					AT.lWorkSpace[posit--] = posisub;
					i--;
				}
				if ( i >= power ) {
					termout = AT.WorkPointer = a;
				    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + 2*AM.MaxTer);
					if ( AT.WorkPointer > AT.WorkTop ) goto OverWork;
					if ( FiniTerm(BHEAD term,accum,termout,replac,tepos) ) goto GenCall;
					AT.WorkPointer = termout + *termout;
					*AN.RepPoint = 1;
					AR.expchanged = 1;
#ifdef WITHPTHREADS
					if ( dtype > 0 && dtype != MODLOCAL && dtype != MODSUM ) { UNLOCK(d->pthreadslockread); }
					if ( ( AS.Balancing && CC->numrhs == 0 ) && ( i > 0 ) && ( id = ConditionalGetAvailableThread() ) >= 0 ) {
						if ( BalanceRunThread(BHEAD id,termout,level) < 0 ) goto GenCall;
					}
					else
#endif
					if ( Generator(BHEAD termout,level) ) goto GenCall;
#ifdef WITHPTHREADS
					if ( dtype > 0 && dtype != MODLOCAL && dtype != MODSUM ) { dtype = 0; break; }
#endif
					if ( iscopy == 0 && ( extractbuff != AM.dbufnum ) )
							StartBuf = cbuf[extractbuff].Buffer;
					i--; posit--;
				}
			}
#ifdef WITHPTHREADS
			if ( dtype > 0 && dtype != MODLOCAL && dtype != MODSUM ) { UNLOCK(d->pthreadslockread); dtype = 0; }
#endif
			if ( iscopy ) {
				if ( d->nfactors > 1 ) {
					int j;
					for ( j = 0; j < d->nfactors; j++ ) {
						if ( d->factors[j].where ) M_free(d->factors[j].where,"Copy of factor");
					}
					M_free(d->factors,"Dollar factors");
				}
				M_free(d,"Copy of dollar variable");
				d = 0; iscopy = 0;
			}
			AT.WorkPointer = accum;
			AT.lWorkPointer = olw;
			AT.posWorkPointer = olpw;
		}
	}
	else {								/* Expression from disk */
		POSITION StartPos;
		LONG position, olpw, opw, comprev, extra;
		RENUMBER renumber;
		WORD *Freeze, *aa, *dummies;
		replac = -replac-1;
		power = AN.TeSuOut;
		Freeze = AN.Frozen;
		if ( Expressions[replac].status == STOREDEXPRESSION ) {
			POSITION firstpos;
			SETSTARTPOS(firstpos);

/*			Note that AT.TMaddr is needed for GetTable just once! */
/*
			We need space for the previous term in the compression
			This is made available in AR.CompressBuffer, although we may get
			problems with this sooner or later. Hence we need to keep
			a set of pointers in AR.CompressBuffer
			Note that after the last call there has been no use made
			of AR.CompressPointer, so it points automatically at its original
			position!
*/
			WantAddPointers(power+1);
			comprev = opw = AT.pWorkPointer;
			AT.pWorkPointer += power+1;
			WantAddPositions(power+1);
			position = olpw = AT.posWorkPointer;
			AT.posWorkPointer += power + 1;

			AT.pWorkSpace[comprev++] = AR.CompressPointer;

			for ( i = 0; i < power; i++ ) {
				PUTZERO(AT.posWorkSpace[position]); position++;
			}
			position = olpw;
			if ( ( renumber = GetTable(replac,&(AT.posWorkSpace[position]),1) ) == 0 ) goto GenCall;
			dummies = AT.WorkPointer;
			*dummies++ = AR.CurDum;
			AT.WorkPointer += power+2;
			accum = AT.WorkPointer;
		    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + 2*AM.MaxTer);
			if ( AT.WorkPointer > AT.WorkTop ) goto OverWork;
			aa = AT.WorkPointer;
			*accum = 0;
			i = 0; StartPos = AT.posWorkSpace[position];
			dummies[i] = AR.CurDum;
			while ( i >= 0 ) {
skippedfirst:
				AR.CompressPointer = AT.pWorkSpace[comprev-1];
				if ( ( extra = PasteFile(BHEAD i,accum,&(AT.posWorkSpace[position])
						,&a,renumber,Freeze,replac) ) < 0 ) goto GenCall;
				if ( Expressions[replac].numdummies > 0 ) {
					AR.CurDum = dummies[i] + Expressions[replac].numdummies;
				}
				if ( NOTSTARTPOS(firstpos) ) {
					if ( ISMINPOS(firstpos) || ISEQUALPOS(firstpos,AT.posWorkSpace[position]) ) {
						firstpos = AT.posWorkSpace[position];
/*
						ADDPOS(AT.posWorkSpace[position],extra * sizeof(WORD));
*/
						goto skippedfirst;
					}
				}
				if ( extra ) { 
/*
					ADDPOS(AT.posWorkSpace[position],extra * sizeof(WORD));
*/
					i++; AT.posWorkSpace[++position] = StartPos;
					AT.pWorkSpace[comprev++] = AR.CompressPointer;
					dummies[i] = AR.CurDum;
				}
				else {
					PUTZERO(AT.posWorkSpace[position]); position--; i--;
					AR.CurDum = dummies[i];
					comprev--;
				}
				if ( i >= power ) {
					termout = AT.WorkPointer = a;
				    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + 2*AM.MaxTer);
					if ( AT.WorkPointer > AT.WorkTop ) goto OverWork;
					if ( FiniTerm(BHEAD term,accum,termout,replac,0) ) goto GenCall;
					if ( *termout ) {
						AT.WorkPointer = termout + *termout;
						*AN.RepPoint = 1;
						AR.expchanged = 1;
#ifdef WITHPTHREADS
						if ( ( AS.Balancing && CC->numrhs == 0 ) && ( i > 0 ) && ( id = ConditionalGetAvailableThread() ) >= 0 ) {
							if ( BalanceRunThread(BHEAD id,termout,level) < 0 ) goto GenCall;

						}
						else
#endif
						if ( Generator(BHEAD termout,level) ) goto GenCall;
					}
					i--; position--;
					AR.CurDum = dummies[i];
					comprev--;
				}
				AT.WorkPointer = aa;
			}
			AT.WorkPointer = accum;
			AT.posWorkPointer = olpw;
			AT.pWorkPointer = opw;
/*
		Bug fix. See also GetTable
#ifdef WITHPTHREADS
			M_free(renumber->symb.lo,"VarSpace");
			M_free(renumber,"Renumber");
#endif
*/
			if ( renumber->symb.lo != AN.dummyrenumlist )
				M_free(renumber->symb.lo,"VarSpace");
			M_free(renumber,"Renumber");

		}
		else {			/* Active expression */
			aa = accum = AT.WorkPointer;
			if ( ( (WORD *)(((UBYTE *)(AT.WorkPointer)) + 2 * AM.MaxTer + sizeof(WORD)) ) > AT.WorkTop )
					goto OverWork;
			*accum++ = -1; AT.WorkPointer++;
			if ( DoOnePow(BHEAD term,power,replac,accum,aa,level,Freeze) ) goto GenCall;
			AT.WorkPointer = aa;
		}
  	}
Return0:
	AR.CurDum = DumNow;
	AN.RepPoint = RepSto;
	CC->numrhs = oldtoprhs;
	CC->Pointer = CC->Buffer + oldcpointer;
	CCC->numrhs = oldatoprhs;
	CCC->Pointer = CCC->Buffer + oldacpointer;
	return(0);

GenCall:
	if ( AM.tracebackflag ) {
		termout = term;
		MLOCK(ErrorMessageLock);
		AO.OutFill = AO.OutputLine = (UBYTE *)AT.WorkPointer;
		AO.OutSkip = 3;
		FiniLine();
		i = *termout;
		while ( --i >= 0 ) {
			TalToLine((UWORD)(*termout++));
			TokenToLine((UBYTE *)"  ");
		}
		AO.OutSkip = 0;
		FiniLine();
		MesCall("Generator");
		MUNLOCK(ErrorMessageLock);
	}
	CC->numrhs = oldtoprhs;
	CC->Pointer = CC->Buffer + oldcpointer;
	CCC->numrhs = oldatoprhs;
	CCC->Pointer = CCC->Buffer + oldacpointer;
	return(-1);
OverWork:
	CC->numrhs = oldtoprhs;
	CC->Pointer = CC->Buffer + oldcpointer;
	CCC->numrhs = oldatoprhs;
	CCC->Pointer = CCC->Buffer + oldacpointer;
	MLOCK(ErrorMessageLock);
	MesWork();
	MUNLOCK(ErrorMessageLock);
	return(-1);
}

/*
 		#] Generator : 
 		#[ DoOnePow :			WORD DoOnePow(term,power,nexp,accum,aa,level,freeze)
*/
/**
 *		Routine gets one power of an expression in the scratch system.
 *		If there are more powers needed there will be a recursion.
 *
 *		No attempt is made to use binomials because we have no
 *		information about commutating properties.
 *
 *		There is a searching for the contents of brackets if needed.
 *		This searching may be rather slow because of the single links.
 *
 *		@param term   is the term we are adding to.
 *		@param power  is the power of the expression that we need.
 *		@param nexp   is the number of the expression.
 *		@param accum  is the accumulator of terms. It accepts the termfragments
 *			          that are made into a proper term in FiniTerm
 *		@param aa	  points to the start of the entire accumulator. In *aa
 *			          we store the number of term fragments that are in the
 *			          accumulator.
 *		@param level  is the current depth in the tree of statements. It is
 *			          needed to continue to the next operation/substitution
 *			          with each generated term
 *		@param freeze is the pointer to the bracket information that should
 *			          be matched.
 */
#ifdef WITHPTHREADS
char freezestring[] = "freeze<-xxxx";
#endif

WORD DoOnePow(PHEAD WORD *term, WORD power, WORD nexp, WORD * accum,
              WORD *aa, WORD level, WORD *freeze)
{
	GETBIDENTITY
	POSITION oldposition, startposition;
	WORD *acc, *termout, fromfreeze = 0;
	WORD *oldipointer = AR.CompressPointer;
	FILEHANDLE *fi;
	WORD type, retval;
	WORD oldGetOneFile = AR.GetOneFile;
	WORD olddummies = AR.CurDum;
	WORD extradummies = Expressions[nexp].numdummies;
/*
	The next code is for some tricky debugging. (5-jan-2010 JV)
	Normally it should be disabled.
*/
/*
#ifdef WITHPTHREADS
	if ( freeze ) {
		MLOCK(ErrorMessageLock);
		if ( AT.identity < 10 ) {
			freezestring[8] = '0'+AT.identity;
			freezestring[9] = '>';
			freezestring[10] = 0;
		}
		else if ( AT.identity < 100 ) {
			freezestring[8] = '0'+AT.identity/10;
			freezestring[9] = '0'+AT.identity%10;
			freezestring[10] = '>';
			freezestring[11] = 0;
		}
		else {
			freezestring[8] = 0;
		}
		PrintTerm(freeze,freezestring);
		MUNLOCK(ErrorMessageLock);
	}
#else
	if ( freeze ) PrintTerm(freeze,"freeze");
#endif
*/
	type = Expressions[nexp].status;
	if ( type == HIDDENLEXPRESSION || type == HIDDENGEXPRESSION
	 || type == DROPHLEXPRESSION || type == DROPHGEXPRESSION
	 || type == UNHIDELEXPRESSION || type == UNHIDEGEXPRESSION ) {
		AR.GetOneFile = 2; fi = AR.hidefile;
	}
	else {
		AR.GetOneFile = 0; fi = AR.infile;
	}
	if ( fi->handle >= 0 ) {
		PUTZERO(oldposition);
#ifdef WITHSEEK
		LOCK(AS.inputslock);
		SeekFile(fi->handle,&oldposition,SEEK_CUR);
		UNLOCK(AS.inputslock);
#endif
	}
	else {
		SETBASEPOSITION(oldposition,fi->POfill-fi->PObuffer);
	}
	if ( freeze && ( Expressions[nexp].bracketinfo != 0 ) ) {
		POSITION *brapos;
/*
		There is a bracket index
		AR.CompressPointer = oldipointer;
*/
		(*aa)++;
		power--;
		if ( ( brapos = FindBracket(nexp,freeze) ) == 0 )
			goto EndExpr;
		startposition = *brapos;
		goto doterms;
	}
	startposition = AS.OldOnFile[nexp];
	retval = GetOneTerm(BHEAD accum,fi,&startposition,0);
	if ( retval > 0 ) {			/* Skip prototype */
		(*aa)++;
		power--;
doterms:
		AR.CompressPointer = oldipointer;
		for (;;) {
			retval = GetOneTerm(BHEAD accum,fi,&startposition,0);
			if ( retval <= 0 ) break;
/*
			Here should come the code to test for [].
*/
			if ( freeze ) {
				WORD *t, *m, *r, *mstop;
				WORD *tset;
				t = accum;
				m = freeze;
				m += *m;
				m -= ABS(m[-1]);
				mstop = m;
				m = freeze + 1;
				r = t;
				r += *t;
				r -= ABS(r[-1]);
				t++;
				tset = t;
				while ( t < r && *t != HAAKJE ) t += t[1];
				if ( t >= r ) {
					if ( m < mstop ) {
						if ( fromfreeze ) goto EndExpr;
						goto NextTerm;
					}
					t = tset;
				}
				else {
					r = tset;
					while ( r < t && m < mstop ) {
						if ( *r == *m ) { m++; r++; }
						else {
							if ( fromfreeze ) goto EndExpr;
							goto NextTerm;
						}
					}
					if ( r < t || m < mstop ) {
						if ( fromfreeze ) goto EndExpr;
						goto NextTerm;
					}
				}
				fromfreeze = 1;
				r = tset;
				m = accum;
				m += *m;
				while ( t < m ) *r++ = *t++;
				*accum = WORDDIF(r,accum);
			}
			if ( extradummies > 0 ) {
				if ( olddummies > AM.IndDum ) {
					MoveDummies(BHEAD accum,olddummies-AM.IndDum);
				}
				AR.CurDum = olddummies+extradummies;
			}
			acc = accum;
			acc += *acc;
			if ( power <= 0 ) {
				termout = acc;
			    AT.WorkPointer = (WORD *)(((UBYTE *)(acc)) + 2*AM.MaxTer);
				if ( AT.WorkPointer > AT.WorkTop ) {
					MLOCK(ErrorMessageLock);
					MesWork();
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
				if ( FiniTerm(BHEAD term,aa,termout,nexp,0) ) goto PowCall;
				if ( *termout ) {
					MarkPolyRatFunDirty(termout)
/*					PolyFunDirty(BHEAD termout); */
					AT.WorkPointer = termout + *termout;
					*AN.RepPoint = 1;
					AR.expchanged = 1;
					if ( Generator(BHEAD termout,level) ) goto PowCall;
				}
			}
			else {
				if ( acc > AT.WorkTop ) {
					MLOCK(ErrorMessageLock);
					MesWork();
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
				if ( DoOnePow(BHEAD term,power,nexp,acc,aa,level,freeze) ) goto PowCall;
			}
NextTerm:;
			AR.CompressPointer = oldipointer;
		}
EndExpr:
		(*aa)--;
	}
	AR.CompressPointer = oldipointer;
	if ( fi->handle >= 0 ) {
#ifdef WITHSEEK
		LOCK(AS.inputslock);
		SeekFile(fi->handle,&oldposition,SEEK_SET);
		UNLOCK(AS.inputslock);
		if ( ISNEGPOS(oldposition) ) {
			MLOCK(ErrorMessageLock);
			MesPrint("File error");
			goto PowCall2;
		}
#endif
	}
	else {
		fi->POfill = fi->PObuffer + BASEPOSITION(oldposition);
	}
	AR.GetOneFile = oldGetOneFile;
	AR.CurDum = olddummies;
	return(0);
PowCall:;
	MLOCK(ErrorMessageLock);
#ifdef WITHSEEK
PowCall2:;
#endif
	MesCall("DoOnePow");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] DoOnePow : 
 		#[ Deferred :			WORD Deferred(term,level)
*/
/**
 *		Picks up the deferred brackets.
 *		These are the bracket contents of which we postpone the reading
 *		when we use the 'Keep Brackets' statement. These contents are
 *		multiplying the terms just before they are sent to the sorting
 *		system.
 *		Special attention goes to having it thread-safe
 *		We have to lock positioning the file and reading it in
 *		a thread specific buffer.
 *
 *		@param term  The term that must be multiplied by the contents of the
 *		             current bracket
 *		@param level The compiler level. This is needed because after
 *		             multiplying term by term we call Generator again.
 */

WORD Deferred(PHEAD WORD *term, WORD level)
{
	GETBIDENTITY
	POSITION startposition;
	WORD *t, *m, *mstop, *tstart, decr, oldb, *termout, i, *oldwork, retval;
	WORD *oldipointer = AR.CompressPointer, *oldPOfill = AR.infile->POfill;
	WORD oldGetOneFile = AR.GetOneFile;
	AR.GetOneFile = 1;
	oldwork = AT.WorkPointer;
    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
	termout = AT.WorkPointer;
	AR.DeferFlag = 0;
	startposition = AR.DefPosition;
/*
		Store old position
*/
	if ( AR.infile->handle >= 0 ) {
/*
		PUTZERO(oldposition);
		SeekFile(AR.infile->handle,&oldposition,SEEK_CUR);
*/
	}
	else {
/*
		SETBASEPOSITION(oldposition,AR.infile->POfill-AR.infile->PObuffer);
*/
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
		AT.WorkPointer = term + *term;
		if ( Generator(BHEAD term,level) ) goto DefCall;
		AR.DeferFlag = 1;
		AT.WorkPointer = oldwork;
		AR.GetOneFile = oldGetOneFile;
		return(0);
	}
	mstop = m + m[1];
	decr = WORDDIF(mstop,AR.CompressBuffer)-1;
	tstart = AR.CompressPointer + decr;

	m = AR.CompressBuffer;
	t = AR.CompressPointer;
	i = *m;
	NCOPY(t,m,i);
	oldb = *tstart;
	AR.TePos = 0;
	AN.TeSuOut = 0;
/*
		Status:
		First bracket content starts at mstop.
		Next term starts at startposition.
		Decompression information is in AR.CompressPointer.
		The outside of the bracket runs from AR.CompressBuffer+1 to mstop.
*/
	for(;;) {
		*tstart = *(AR.CompressPointer)-decr;
		AR.CompressPointer = AR.CompressPointer+AR.CompressPointer[0];
		if ( InsertTerm(BHEAD term,0,AM.rbufnum,tstart,termout,0) < 0 ) {
			goto DefCall;
		}
		*tstart = oldb;
		AT.WorkPointer = termout + *termout;
		if ( Generator(BHEAD termout,level) ) goto DefCall;
		AR.CompressPointer = oldipointer;
		AT.WorkPointer = termout;
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
	if ( AR.infile->handle < 0 ) AR.infile->POfill = oldPOfill;
	AR.DeferFlag = 1;
	AR.GetOneFile = oldGetOneFile;
	AT.WorkPointer = oldwork;
	return(0);
DefCall:;
	MLOCK(ErrorMessageLock);
	MesCall("Deferred");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] Deferred : 
 		#[ PrepPoly :			WORD PrepPoly(term,par)
*/
/**
 *		Routine checks whether the count of function AR.PolyFun is zero
 *		or one. If it is one and it has one scalarlike argument the
 *		coefficient of the term is pulled inside the argument.
 *		If the count is zero a new function is made with the coefficient
 *		as its only argument. The function should be placed at its
 *		proper position.
 *
 *		When this function is active it places the PolyFun as last
 *		object before the coefficient. This is needed because otherwise
 *		the compress algorithm has problems in MergePatches.
 *
 *		The bracket routine should also place the PolyFun at a
 *		comparable spot.
 *		The compression should then stop at the PolyFun. It doesn't
 *		really have to stop when writing the final result but this may
 *		be too complicated.
 *
 *		The parameter par tells whether we are at groundlevel or
 *		inside a function or dollar variable.
 */

WORD PrepPoly(PHEAD WORD *term,WORD par)
{
	GETBIDENTITY
	WORD count = 0, i, jcoef, ncoef;
	WORD *t, *m, *r, *tstop, *poly = 0, *v, *w, *vv, *ww;
	WORD *oldworkpointer = AT.WorkPointer;
/*
	The problem here is that the function will be forced into 'long'
	notation. After this -SNUMBER,1 becomes 6,0,4,1,1,3 and the
	pattern matcher cannot match a short 1 with a long 1.
	But because this is an undocumented feature for very special
	purposes, we don't do anything about it. (30-aug-2011)
*/
	if ( AR.PolyFunType == 2 && AR.PolyFunExp != 2 ) {
		WORD oldtype = AR.SortType;
		AR.SortType = SORTHIGHFIRST;
		if ( poly_ratfun_normalize(BHEAD term) != 0 ) Terminate(-1);
/*		if ( ReadPolyRatFun(BHEAD term) != 0 ) Terminate(-1); */
		oldworkpointer = AT.WorkPointer;
		AR.SortType = oldtype;
	}
	AT.PolyAct = 0;
	t = term;
	GETSTOP(t,tstop);
	t++;
	while ( t < tstop ) {
		if ( *t == AR.PolyFun ) {
			if ( count > 0 ) return(0);
			poly = t;
			count++;
		}
		t += t[1];
	}
	r = m = term + *term;
	i = ABS(m[-1]);
	if ( par > 0 ) {
		if ( count == 0 ) return(0);
		else if ( AR.PolyFunType == 1 || (AR.PolyFunType == 2 && AR.PolyFunExp == 2) )
			goto DoOne;
		else if ( AR.PolyFunType == 2 )
			goto DoTwo;
		else
			goto DoError;
	}
	else if ( count == 0 ) {
/*
 		#[ Create a PolyFun :
*/
		poly = t = tstop;
		if ( i == 3 && m[-2] == 1 && (m[-3]&MAXPOSITIVE) == m[-3] ) {
			*m++ = AR.PolyFun;
			if ( AR.PolyFunType == 1 || (AR.PolyFunType == 2 && AR.PolyFunExp == 2) ) {
				*m++ = FUNHEAD+2;
				FILLFUN(m)
				*m++ = -SNUMBER;
				*m = m[-2-FUNHEAD] < 0 ? -m[-4-FUNHEAD]: m[-4-FUNHEAD];
				m++;
			}
			else if ( AR.PolyFunType == 2 ) {
				*m++ = FUNHEAD+4;
				FILLFUN(m)
				*m++ = -SNUMBER;
				*m = m[-2-FUNHEAD] < 0 ? -m[-4-FUNHEAD]: m[-4-FUNHEAD];
				m++;
				*m++ = -SNUMBER;
				*m++ = 1;
			}
		}
		else {
			WORD *vm;
			r = tstop;
			if ( AR.PolyFunType == 1 || (AR.PolyFunType == 2 && AR.PolyFunExp == 2) ) {
				*m++ = AR.PolyFun;
				*m++ = FUNHEAD+ARGHEAD+i+1;
				FILLFUN(m)
				*m++ = ARGHEAD+i+1;
				*m++ = 0;
				FILLARG(m)
				*m++ = i+1;
				NCOPY(m,r,i);
			}
			else if ( AR.PolyFunType == 2 ) {
				WORD *num, *den, size, sign, sizenum, sizeden;
				if ( m[-1] < 0 ) { sign = -1; size = -m[-1]; }
				else             { sign =  1; size =  m[-1]; }
				num = m - size; size = (size-1)/2; den = num + size;
				sizenum = size; while ( num[sizenum-1] == 0 ) sizenum--;
				sizeden = size; while ( den[sizeden-1] == 0 ) sizeden--;
				v = m;
				AT.PolyAct = WORDDIF(v,term);
				*v++ = AR.PolyFun;
				v++;
				FILLFUN(v);
				vm = v;
				*v++ = ARGHEAD+2*sizenum+2;
				*v++ = 0;
				FILLARG(v);
				*v++ = 2*sizenum+2;
				for ( i = 0; i < sizenum; i++ ) *v++ = num[i];
				*v++ = 1;
				for ( i = 1; i < sizenum; i++ ) *v++ = 0;
				*v++ = sign*(2*sizenum+1);
				if ( ToFast(vm,vm) ) v = vm+2;
				vm = v;
				*v++ = ARGHEAD+2*sizeden+2;
				*v++ = 0;
				FILLARG(v);
				*v++ = 2*sizeden+2;
				for ( i = 0; i < sizeden; i++ ) *v++ = den[i];
				*v++ = 1;
				for ( i = 1; i < sizeden; i++ ) *v++ = 0;
				*v++ = 2*sizeden+1;
				if ( ToFast(vm,vm) ) v = vm+2;
				i = v-m;
				m[1] = i;
				w = num;
				NCOPY(w,m,i);
				*w++ = 1; *w++ = 1; *w++ = 3; *term = w - term;
				return(0);
			}
		}
/*
 		#] Create a PolyFun : 
*/
	}
	else if ( AR.PolyFunType == 1 || (AR.PolyFunType == 2 && AR.PolyFunExp == 2) ) {
		DoOne:;
/*
 		#[ One argument :
*/
		m = term + *term;
		r = poly + poly[1];
		if ( ( poly[1] == FUNHEAD+2 && poly[FUNHEAD+1] == 0
		&& poly[FUNHEAD] == -SNUMBER ) || poly[1] == FUNHEAD ) return(1);
		t = poly + FUNHEAD;
		if ( t >= r ) return(0);
		if ( m[-1] == 3 && *tstop == 1 && tstop[1] == 1 ) {
			i = poly[1];
			t = poly;
			NCOPY(m,t,i);
		}
		else if ( *t <= -FUNCTION ) {
			if ( t+1 < r ) return(0);	/* More than one argument */
			r = tstop;
			*m++ = AR.PolyFun;
			*m++ = FUNHEAD*2+ARGHEAD+i+1;
			FILLFUN(m)
			*m++ = FUNHEAD+ARGHEAD+i+1;
			*m++ = 0;
			FILLARG(m)
			*m++ = FUNHEAD+i+1;
			*m++ = -*t++;
			*m++ = FUNHEAD;
			FILLFUN(m)
			NCOPY(m,r,i);
		}
		else if ( *t < 0 ) {
			if ( t+2 < r ) return(0);	/* More than one argument */
			r = tstop;
			if ( *t == -SNUMBER ) {
				if ( t[1] == 0 ) return(1);	/* Term should be zero now */
				*m = AR.PolyFun;
				w = m+1;
				m += FUNHEAD+ARGHEAD;
				v = m;
				*m++ = 5+i;
				*m++ = SNUMBER;
				*m++ = 4;
				*m++ = t[1];
				*m++ = 1;
				NCOPY(m,r,i);
				if ( m >= AT.WorkSpace && m < AT.WorkTop )
					AT.WorkPointer = m;
				if ( Normalize(BHEAD v) ) Terminate(-1);
				AT.WorkPointer = oldworkpointer;
				m = w;
				if ( *v == 4 && v[2] == 1 && (v[1]&MAXPOSITIVE) == v[1] ) {
					*m++ = FUNHEAD+2;
					FILLFUN(m)
					*m++ = -SNUMBER;
					*m++ = v[3] < 0 ? -v[1] : v[1];
				}
				else if ( *v == 0 ) return(1);
				else {
					*m++ = FUNHEAD+ARGHEAD+*v;
					FILLFUN(m)
					*m++ = ARGHEAD+*v;
					*m++ = 0;
					FILLARG(m)
					m = v + *v;
				}
			}
			else if ( *t == -SYMBOL ) {
				*m++ = AR.PolyFun;
				*m++ = FUNHEAD+ARGHEAD+5+i;
				FILLFUN(m)
				*m++ = ARGHEAD+5+i;
				*m++ = 0;
				FILLARG(m)
				*m++ = 5+i;
				*m++ = SYMBOL;
				*m++ = 4;
				*m++ = t[1];
				*m++ = 1;
				NCOPY(m,r,i);
			}
			else return(0);			/* Not symbol-like */
		}
		else {
			if ( t + *t < r ) return(0); /* More than one argument */
			i = m[-1];
			*m++ = AR.PolyFun;
			w = m;
			m += ARGHEAD+FUNHEAD-1;
			t += ARGHEAD;
			jcoef = i < 0 ? (i+1)>>1:(i-1)>>1;
			v = t;
/*
			Test now the scalar nature of the argument.
			No indices allowed.
*/
			while ( t < r ) {
				WORD *vstop;
				vv = t + *t;
				vstop = vv - ABS(vv[-1]);
				t++;
				while( t < vstop ) {
					if ( *t == INDEX ) return(0);
					t += t[1];
				}
				t = vv;
			}
/*
			Now multiply each term by the coefficient.
*/
			t = v;
			while ( t < r ) {
				ww = m;
				v = t + *t;
				ncoef = v[-1];
				vv = v - ABS(ncoef);
                if ( ncoef < 0 ) ncoef++;
				else ncoef--;
				ncoef >>= 1;
				while ( t < vv ) *m++ = *t++;
				if ( MulRat(BHEAD (UWORD *)vv,ncoef,(UWORD *)tstop,jcoef,
					(UWORD *)m,&ncoef) ) Terminate(-1);
				ncoef *= 2;
				m += ABS(ncoef);
				if ( ncoef < 0 ) ncoef--;
				else ncoef++;
				*m++ = ncoef;
				*ww = WORDDIF(m,ww);
				if ( AN.ncmod != 0 ) {
					if ( Modulus(ww) ) Terminate(-1);
					if ( *ww == 0 ) return(1);
					m = ww + *ww;
				}
				t = v;
			}
			*w = (WORDDIF(m,w))+1;
			w[FUNHEAD-1] = w[0] - FUNHEAD;
			w[FUNHEAD] = 0;
			w[1] = 0; /* omission survived for years. 23-mar-2006 JV */
			w += FUNHEAD-1;
			if ( ToFast(w,w) ) {
				if ( *w <= -FUNCTION ) { w[-FUNHEAD+1] = FUNHEAD+1; m = w+1; }
				else { w[-FUNHEAD+1] = FUNHEAD+2; m = w+2; }
				
			}
		}
		t = poly + poly[1];
		while ( t < tstop ) *poly++ = *t++;
/*
 		#] One argument : 
*/
	}
	else if ( AR.PolyFunType == 2 ) {
		DoTwo:;
/*
 		#[ Two arguments :
*/
		WORD *num, *den, size, sign, sizenum, sizeden;
/*
		First make sure that the PolyFun is last
*/
		m = term + *term;
		if ( poly + poly[1] < tstop ) {
			for ( i = 0; i < poly[1]; i++ ) m[i] = poly[i];
			t = poly; v = poly + poly[1];
			while ( v < tstop ) *t++ = *v++;
			poly = t;
			for ( i = 0; i < m[1]; i++ ) t[i] = m[i];
			t += m[1];
		}
		AT.PolyAct = WORDDIF(poly,term);
/*
		If needed we convert the coefficient into a PolyRatFun and then
		we call poly_ratfun_normalize
*/
		if ( m[-1] == 3 && m[-2] == 1 && m[-3] == 1 ) return(0);
		if ( AR.PolyFunExp != 1 ) {
		if ( m[-1] < 0 ) { sign = -1; size = -m[-1]; } else { sign = 1; size = m[-1]; }
		num = m - size; size = (size-1)/2; den = num + size;
		sizenum = size; while ( num[sizenum-1] == 0 ) sizenum--;
		sizeden = size; while ( den[sizeden-1] == 0 ) sizeden--;
		v = m;
		*v++ = AR.PolyFun;
		*v++ = FUNHEAD + 2*(ARGHEAD+sizenum+sizeden+2);
/*		*v++ = MUSTCLEANPRF; */
		*v++ = 0;
		FILLFUN3(v);
		*v++ = ARGHEAD+2*sizenum+2;
		*v++ = 0;
		FILLARG(v);
		*v++ = 2*sizenum+2;
		for ( i = 0; i < sizenum; i++ ) *v++ = num[i];
		*v++ = 1;
		for ( i = 1; i < sizenum; i++ ) *v++ = 0;
		*v++ = sign*(2*sizenum+1);
		*v++ = ARGHEAD+2*sizeden+2;
		*v++ = 0;
		FILLARG(v);
		*v++ = 2*sizeden+2;
		for ( i = 0; i < sizeden; i++ ) *v++ = den[i];
		*v++ = 1;
		for ( i = 1; i < sizeden; i++ ) *v++ = 0;
		*v++ = 2*sizeden+1;
		w = num;
		i = v - m;
		NCOPY(w,m,i);
		}
		else {
			w = m-ABS(m[-1]);
		}
		*w++ = 1; *w++ = 1; *w++ = 3; *term = w - term;
		{
			WORD oldtype = AR.SortType;
			AR.SortType = SORTHIGHFIRST;
/*
			if ( count > 0 )
				poly_ratfun_normalize(BHEAD term);
			else
				ReadPolyRatFun(BHEAD term);
*/
			poly_ratfun_normalize(BHEAD term);

/*			oldworkpointer = AT.WorkPointer; */
			AR.SortType = oldtype;
		}
		goto endofit;
/*
 		#] Two arguments : 
*/
	}
	else {
		DoError:;
		MLOCK(ErrorMessageLock);
		MesPrint("Illegal value for PolyFunType in PrepPoly");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	r = term + *term;
	AT.PolyAct = WORDDIF(poly,term);
	while ( r < m ) *poly++ = *r++;
	*poly++ = 1;
	*poly++ = 1;
	*poly++ = 3;
	*term = WORDDIF(poly,term);
endofit:;
	return(0);
}

/*
 		#] PrepPoly : 
 		#[ PolyFunMul :			WORD PolyFunMul(term)
*/
/**
 *		Multiplies the arguments of multiple occurrences of the polyfun.
 *		In this routine we do the original PolyFun with one argument only.
 *		The PolyRatFun (PolyFunType = 2) is done in a dedicated routine
 *		in the file polywrap.cc
 *		The new result is written over the old result.
 *
 *		@param term It contains the input term and later the output.
 *		@return Normal conventions (OK = 0).
 */

WORD PolyFunMul(PHEAD WORD *term)
{
	GETBIDENTITY
	WORD *t, *fun1, *fun2, *t1, *t2, *m, *w, *ww, *tt1, *tt2, *tt4, *arg1, *arg2;
	WORD *tstop, i, dirty = 0, OldPolyFunPow = AR.PolyFunPow, minp1, minp2;
	WORD n1, n2, i1, i2, l1, l2, l3, l4, action = 0, noac = 0, retval = 0;
	if ( AR.PolyFunType == 2 && AR.PolyFunExp == 1 ) {
		WORD pow = 0, pow1;
		t = term + 1; t1 = term + *term; t1 -= ABS(t1[-1]);
		w = t;
		while ( t < t1 ) {
			if ( *t != AR.PolyFun ) {
SkipFun:
				if ( t == w ) { t += t[1]; w = t; }
				else { i = t[1]; NCOPY(w,t,i) }
				continue;
			}
			pow1 = 0;
			t2 = t + t[1]; t += FUNHEAD;
			if ( *t < 0 ) {
				if ( *t == -SYMBOL && t[1] == AR.PolyFunVar ) pow1++;
				else if ( *t != -SNUMBER ) goto NoLegal;
				t += 2;
			}
			else if ( t[0] == ARGHEAD+8 && t[ARGHEAD] == 8
			 && t[ARGHEAD+1] == SYMBOL && t[ARGHEAD+3] == AR.PolyFunVar
			 && t[ARGHEAD+5] == 1 && t[ARGHEAD+6] == 1 && t[ARGHEAD+7] == 3 ) {
				pow1 += t[ARGHEAD+4];
				t += *t;
			}
			else {
NoLegal:
				MLOCK(ErrorMessageLock);
				MesPrint("Illegal term with divergence in PolyRatFun");
				MesCall("PolyFunMul");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			if ( *t < 0 ) {
				if ( *t == -SYMBOL && t[1] == AR.PolyFunVar ) pow1--;
				else if ( *t != -SNUMBER ) goto NoLegal;
				t += 2;
			}
			else if ( t[0] == ARGHEAD+8 && t[ARGHEAD] == 8
			 && t[ARGHEAD+1] == SYMBOL && t[ARGHEAD+3] == AR.PolyFunVar
			 && t[ARGHEAD+5] == 1 && t[ARGHEAD+6] == 1 && t[ARGHEAD+7] == 3 ) {
				pow1 -= t[ARGHEAD+4];
				t += *t;
			}
			else goto NoLegal;
			if ( t == t2 ) pow += pow1;
			else goto SkipFun;
		}
		m = w;
		*w++ = AR.PolyFun; *w++ = 0; FILLFUN(w);
		if ( pow > 1 ) {
			*w++ = 8+ARGHEAD; *w++ = 0; FILLARG(w);
			*w++ = 8; *w++ = SYMBOL; *w++ = 4; *w++ = AR.PolyFunVar; *w++ = pow;
			*w++ = 1; *w++ = 1; *w++ = 3; *w++ = -SNUMBER; *w++ = 1;
		}
		else if ( pow == 1 ) {
			*w++ = -SYMBOL; *w++ = AR.PolyFunVar; *w++ = -SNUMBER; *w++ = 1;
		}
		else if ( pow < -1 ) {
			*w++ = -SNUMBER; *w++ = 1; *w++ = 8+ARGHEAD; *w++ = 0; FILLARG(w);
			*w++ = 8; *w++ = SYMBOL; *w++ = 4; *w++ = AR.PolyFunVar; *w++ = -pow;
			*w++ = 1; *w++ = 1; *w++ = 3;
		}
		else if ( pow == -1 ) {
			*w++ = -SNUMBER; *w++ = 1; *w++ = -SYMBOL; *w++ = AR.PolyFunVar;
		}
		else {
			*w++ = -SNUMBER; *w++ = 1; *w++ = -SNUMBER; *w++ = 1;
		}
		m[1] = w - m;
		*w++ = 1; *w++ = 1; *w++ = 3;
		*term = w - term;
		if ( w > AT.WorkSpace && w < AT.WorkTop ) AT.WorkPointer = w;
		return(0);
	}
ReStart:
	if ( AR.PolyFunType == 2 && ( ( AR.PolyFunExp != 2 )
	 || ( AR.PolyFunExp == 2 && AN.PolyNormFlag > 1 ) ) ) {
		WORD count1 = 0, count2 = 0, count3;
		WORD oldtype = AR.SortType;
		t = term + 1; t1 = term + *term; t1 -= ABS(t1[-1]);
		while ( t < t1 ) {
			if ( *t == AR.PolyFun ) {
			  if ( t[2] && dirty == 0 ) { /* Any dirty flag on? */
				dirty = 1;
/*				ReadPolyRatFun(BHEAD term); */
/*				ToPolyFunGeneral(BHEAD term); */
				poly_ratfun_normalize(BHEAD term);
				if ( term[0] == 0 ) return(0);
				count1 = 0;
				action++;
				goto ReStart;
			  }
			  t2 = t + t[1]; tt2 = t+FUNHEAD; count3 = 0;
			  while ( tt2 < t2 ) { count3++; NEXTARG(tt2); }
			  if ( count3 == 2 ) {
				count1++;
				if ( ( t[2] & MUSTCLEANPRF ) != 0 ) {	/* Better civilize this guy */
					action++;
					w = AT.WorkPointer;
					AR.SortType = SORTHIGHFIRST;
					t2 = t + t[1]; tt2 = t+FUNHEAD;
					while ( tt2 < t2 ) {
						if ( *tt2 > 0 ) {
							tt4 = tt2; tt1 = tt2 + ARGHEAD; tt2 += *tt2;
							NewSort(BHEAD0);
							while ( tt1 < tt2 ) {
								i = *tt1; ww = w; NCOPY(ww,tt1,i);
								AT.WorkPointer = ww;
								Normalize(BHEAD w);
								StoreTerm(BHEAD w);
							}
							EndSort(BHEAD w,1);
							ww = w; while ( *ww ) ww += *ww;
							if ( ww-w != *tt4-ARGHEAD ) { /* Little problem */
/*
								Solution: brute force copy
								Maybe it will never come here????
*/
								WORD *r1 = TermMalloc("PolyFunMul");
								WORD ii = (ww-w)-(*tt4-ARGHEAD); /* increment */
								WORD *r2 = tt4+ARGHEAD, *r3, *r4 = r1;
								i = r2 - term; r3 = term; NCOPY(r4,r3,i);
								i = ww-w; ww = w; NCOPY(r4,ww,i);
								r3 = tt2; i = term+*term-tt2; NCOPY(r4,r3,i);
								*r1 = i = r4-r1; r4 = term; r3 = r1;
								NCOPY(r4,r3,i);
								t[1] += ii; t1 += ii; *tt4 += ii;
								tt2 = tt4 + *tt4;
								TermFree(r1,"PolyFunMul");
							}
							else {
								i = ww-w; ww = w; tt1 = tt4+ARGHEAD;
								NCOPY(tt1,ww,i);
								AT.WorkPointer = w;
							}
						}
						else if ( *tt2 <= -FUNCTION ) tt2++;
						else tt2 += 2;
					}
					AR.SortType = oldtype;
				}
			  }
			}
			t += t[1];
		}
		if ( count1 <= 1 ) { goto checkaction; }
		if ( AR.PolyFunExp == 1 ) {
			t = term + *term; t -= ABS(t[-1]);
			*t++ = 1; *t++ = 1; *t++ = 3; *term = t - term;
		}
		{
			AR.SortType = SORTHIGHFIRST;
/*			retval = ReadPolyRatFun(BHEAD term); */
/*			ToPolyFunGeneral(BHEAD term); */
			retval = poly_ratfun_normalize(BHEAD term);
			if ( *term == 0 ) return(retval);
			AR.SortType = oldtype;
		}

		t = term + 1; t1 = term + *term; t1 -= ABS(t1[-1]);
		while ( t < t1 ) {
			if ( *t == AR.PolyFun ) {
			  t2 = t + t[1]; tt2 = t+FUNHEAD; count3 = 0;
			  while ( tt2 < t2 ) { count3++; NEXTARG(tt2); }
			  if ( count3 == 2 ) {
				count2++;
			  }
			}
			t += t[1];
		}
		if ( count1 >= count2 ) {
			t = term + 1;
			while ( t < t1 ) {
				if ( *t == AR.PolyFun ) {
					t2 = t;
					t = t + t[1];
					t2[2] |= (DIRTYFLAG|MUSTCLEANPRF);
					t2 += FUNHEAD;
					while ( t2 < t ) {
						if ( *t2 > 0 ) t2[1] = DIRTYFLAG;
						NEXTARG(t2);
					}
				}
				else t += t[1];
			}
		}

		w = term + *term;
		if ( w > AT.WorkSpace && w < AT.WorkTop ) AT.WorkPointer = w;
checkaction:
		if ( action ) retval = action;
		return(retval);
	}
retry:
	if ( term >= AT.WorkSpace && term+*term < AT.WorkTop )
		AT.WorkPointer = term + *term;
	GETSTOP(term,tstop);
	t = term+1;
	while ( *t != AR.PolyFun && t < tstop ) t += t[1];
	while ( t < tstop && *t == AR.PolyFun ) {
		if ( t[1] > FUNHEAD ) {
			if ( t[FUNHEAD] < 0 ) {
				if ( t[FUNHEAD] <= -FUNCTION && t[1] == FUNHEAD+1 ) break;
				if ( t[FUNHEAD]  > -FUNCTION && t[1] == FUNHEAD+2 ) {
					if ( t[FUNHEAD] == -SNUMBER && t[FUNHEAD+1] == 0 ) {
						*term = 0;
						return(0);
					}
					break;
				}
			}
			else if ( t[FUNHEAD] == t[1] - FUNHEAD ) break;
		}
		noac = 1;
		t += t[1];
	}
	if ( *t != AR.PolyFun || t >= tstop ) goto done;
	fun1 = t;
	t += t[1];
	while ( t < tstop && *t == AR.PolyFun ) {
		if ( t[1] > FUNHEAD ) {
			if ( t[FUNHEAD] < 0 ) {
				if ( t[FUNHEAD] <= -FUNCTION && t[1] == FUNHEAD+1 ) break;
				if ( t[FUNHEAD]  > -FUNCTION && t[1] == FUNHEAD+2 ) {
					if ( t[FUNHEAD] == -SNUMBER && t[FUNHEAD+1] == 0 ) {
						*term = 0;
						return(0);
					}
					break;
				}
			}
			else if ( t[FUNHEAD] == t[1] - FUNHEAD ) break;
		}
		noac = 1;
		t += t[1];
	}
	if ( *t != AR.PolyFun || t >= tstop ) goto done;
	fun2 = t;
/*
	We have two functions of the proper type.
	Count terms (needed for the specials)
*/
	t = fun1 + FUNHEAD;
	if ( *t < 0 ) {
		n1 = 1; arg1 = AT.WorkPointer;
		ToGeneral(t,arg1,1);
		AT.WorkPointer = arg1 + *arg1;
	}
	else {
		t += ARGHEAD;
		n1 = 0; t1 = fun1 + fun1[1]; arg1 = t;
		while ( t < t1 ) { n1++; t += *t; }
	}
	t = fun2 + FUNHEAD;
	if ( *t < 0 ) {
		n2 = 1; arg2 = AT.WorkPointer;
		ToGeneral(t,arg2,1);
		AT.WorkPointer = arg2 + *arg2;
	}
	else {
		t += ARGHEAD;
		n2 = 0; t2 = fun2 + fun2[1]; arg2 = t;
		while ( t < t2 ) { n2++; t += *t; }
	}
/*
	Now we can start the multiplications. We first multiply the terms
	without coefficients, then normalize, and finally put the coefficients
	in place. This is because one has often truncated series and the
	high powers may get killed, while their coefficients are the most
	expensive ones.
	Note: We may run into fun(-SNUMBER,value)
*/
	w = AT.WorkPointer;
	NewSort(BHEAD0);
	if ( AR.PolyFunType == 2 && AR.PolyFunExp == 2 ) {
		AT.TrimPower = 1;
/*
		We have to find the lowest power in both polynomials.
		This will be needed to temporarily correct the AR.PolyFunPow
*/
		minp1 = MAXPOWER;
		for ( t1 = arg1, i1 = 0; i1 < n1; i1++, t1 += *t1 ) {
			if ( *t1 == 4 ) {
				if ( minp1 > 0 ) minp1 = 0;
			}
			else if ( ABS(t1[*t1-1]) == (*t1-1) ) {
				if ( minp1 > 0 ) minp1 = 0;
			}
			else {
				if ( t1[1] == SYMBOL && t1[2] == 4 && t1[3] == AR.PolyFunVar ) {
					if ( t1[4] < minp1 ) minp1 = t1[4];
				}
				else {
					MesPrint("Illegal term in expanded polyratfun.");
					goto PolyCall;
				}
			}
		}
		minp2 = MAXPOWER;
		for ( t2 = arg2, i2 = 0; i2 < n2; i2++, t2 += *t2 ) {
			if ( *t2 == 4 ) {
				if ( minp2 > 0 ) minp2 = 0;
			}
			else if ( ABS(t2[*t2-1]) == (*t2-1) ) {
				if ( minp2 > 0 ) minp2 = 0;
			}
			else {
				if ( t2[1] == SYMBOL && t2[2] == 4 && t2[3] == AR.PolyFunVar ) {
					if ( t2[4] < minp2 ) minp2 = t2[4];
				}
				else {
					MesPrint("Illegal term in expanded polyratfun.");
					goto PolyCall;
				}
			}
		}
		AR.PolyFunPow += minp1+minp2;
	}
	for ( t1 = arg1, i1 = 0; i1 < n1; i1++, t1 += *t1 ) {
	for ( t2 = arg2, i2 = 0; i2 < n2; i2++, t2 += *t2 ) {
		m = w;
		m++;
		GETSTOP(t1,tt1);
		t = t1 + 1;
		while ( t < tt1 ) *m++ = *t++;
		GETSTOP(t2,tt2);
		t = t2+1;
		while ( t < tt2 ) *m++ = *t++;
		*m++ = 1; *m++ = 1; *m++ = 3; *w = WORDDIF(m,w);
		AT.WorkPointer = m;
		if ( Normalize(BHEAD w) ) { LowerSortLevel(); goto PolyCall; }
		if ( *w ) {
			m = w + *w;
			if ( m[-1] != 3 || m[-2] != 1 || m[-3] != 1 ) {
				l3 = REDLENG(m[-1]);
				m -= ABS(m[-1]);
				t = t1 + *t1 - 1;
				l1 = REDLENG(*t);
				if ( MulRat(BHEAD (UWORD *)m,l3,(UWORD *)tt1,l1,(UWORD *)m,&l4) ) {
					LowerSortLevel(); goto PolyCall; }
				if ( AN.ncmod != 0 && TakeModulus((UWORD *)m,&l4,AC.cmod,AN.ncmod,UNPACK|AC.modmode) ) {
					LowerSortLevel(); goto PolyCall; }
				if ( l4 == 0 ) continue;
				t = t2 + *t2 - 1;
				l2 = REDLENG(*t);
				if ( MulRat(BHEAD (UWORD *)m,l4,(UWORD *)tt2,l2,(UWORD *)m,&l3) ) {
					LowerSortLevel(); goto PolyCall; }
				if ( AN.ncmod != 0 && TakeModulus((UWORD *)m,&l3,AC.cmod,AN.ncmod,UNPACK|AC.modmode) ) {
					LowerSortLevel(); goto PolyCall; }
			}
			else {
				m -= 3;
				t = t1 + *t1 - 1;
				l1 = REDLENG(*t);
				t = t2 + *t2 - 1;
				l2 = REDLENG(*t);
				if ( MulRat(BHEAD (UWORD *)tt1,l1,(UWORD *)tt2,l2,(UWORD *)m,&l3) ) {
					LowerSortLevel(); goto PolyCall; }
				if ( AN.ncmod != 0 && TakeModulus((UWORD *)m,&l3,AC.cmod,AN.ncmod,UNPACK|AC.modmode) ) {
					LowerSortLevel(); goto PolyCall; }
			}
			if ( l3 == 0 ) continue;
			l3 = INCLENG(l3);
			m += ABS(l3);
			m[-1] = l3;
			*w = WORDDIF(m,w);
			AT.WorkPointer = m;
			if ( StoreTerm(BHEAD w) ) { LowerSortLevel(); goto PolyCall; }
		}
	}	
	}	
	if ( EndSort(BHEAD w,0) < 0 ) goto PolyCall;
	AR.PolyFunPow = OldPolyFunPow;
	AT.TrimPower = 0;
	if ( *w == 0 ) {
		*term = 0;
		return(0);
	}
	t = w;
	while ( *t ) t += *t;
	AT.WorkPointer = t;
	n1 = WORDDIF(t,w);
	t1 = term;
	while ( t1 < fun1 ) *t++ = *t1++;
	t2 = t;
	*t++ = AR.PolyFun;
	*t++ = FUNHEAD+ARGHEAD+n1;
	*t++ = 0;
	FILLFUN3(t)
	*t++ = ARGHEAD+n1;
	*t++ = 0;
	FILLARG(t)
	NCOPY(t,w,n1);
	if ( ToFast(t2+FUNHEAD,t2+FUNHEAD) ) {
		if ( t2[FUNHEAD] > -FUNCTION ) t2[1] = FUNHEAD+2;
		else t2[FUNHEAD] = FUNHEAD+1;
		t = t2 + t2[1];
	}
	t1 = fun1 + fun1[1];
	while ( t1 < fun2 ) *t++ = *t1++;
	t1 = fun2 + fun2[1];
	t2 = term + *term;
	while ( t1 < t2 ) *t++ = *t1++;
	*AT.WorkPointer = n1 = WORDDIF(t,AT.WorkPointer);
	if ( n1*((LONG)sizeof(WORD)) > AM.MaxTer ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Term too complex. Maybe increasing MaxTermSize can help");
		goto PolyCall2;
	}
	m = term; t = AT.WorkPointer;
	NCOPY(m,t,n1);
	action++;
	goto retry;
done:
	AT.WorkPointer = term + *term;
	if ( action && noac ) {
		if ( Normalize(BHEAD term) ) goto PolyCall;
		AT.WorkPointer = term + *term;
	}
	return(0);
PolyCall:;
	MLOCK(ErrorMessageLock);
PolyCall2:;
	AR.PolyFunPow = OldPolyFunPow;
	MesCall("PolyFunMul");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] PolyFunMul : 
	#] Processor :
*/
