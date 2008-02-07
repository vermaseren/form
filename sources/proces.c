/** @file proces.c
 *
 *  Contains the central terms processor routines. This is the core of
 *	the virtual machine. All other files are to help these routines.
 */
/*
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
 
WORD
Processor()
{
	GETIDENTITY
	WORD *term, *t, i, retval = 0;
	EXPRESSIONS e;
	POSITION position;
	WORD last, LastExpression;
	LONG dd = 0;
	CBUF *C = cbuf+AC.cbufnum;
	int firstterm;
	CBUF *CC = cbuf+AT.ebufnum;
	WORD **w;
	FILEHANDLE *curfile;
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

#ifdef PARALLEL
	PF.module++;
#endif
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
/*
		Next determine the last expression. This is used for removing the
		input file when the final stage of the sort of this expression is
		reached. That can safe up to 1/3 in disk space.
*/
	for ( i = NumExpressions-1; i >= 0; i-- ) {
		e = Expressions+i;
		if ( e->status == LOCALEXPRESSION || e->status == GLOBALEXPRESSION
		|| e->status == HIDELEXPRESSION || e->status == HIDEGEXPRESSION
		|| e->status == SKIPLEXPRESSION || e->status == SKIPGEXPRESSION
		|| e->status == UNHIDELEXPRESSION || e->status == UNHIDEGEXPRESSION
		) break;
	}
	last = i;
#ifdef WITHPTHREADS
/*
		When we run with threads we have to make sure that all local input
		buffers are pointed correctly. Of course this isn't needed if we
		run on a single thread only.
*/
	if ( AS.MultiThreaded && AC.mparallelflag == PARALLELFLAG ) {
		SetWorkerFiles();
	}
#endif
#ifdef WITHPTHREADS
/*
		We start with running the expressions with expr->partodo in parallel.
		The current model is: give each worker an expression. Wait for 
		workers to finish and tell them where to write.
		Then give them a new expression. Workers may have to wait for each
		other. This is also the case with the last one.
*/
	if ( ( AC.numpartodo > 0 ) && InParallelProcessor() ) {
		retval = 1;
	}
#endif
	for ( i = 0; i < NumExpressions; i++ ) {
		e = Expressions+i;
#ifdef WITHPTHREADS
		if ( AC.numpartodo > 0 && e->partodo > 0 ) {
			e->partodo = 0;
			continue;
		}
#endif
		AS.CollectOverFlag = 0;
		e->vflags &= ~ISUNMODIFIED;
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
				MesPrint("Expression %d has problems in scratchfile",i);
				retval = -1;
				break;
			}
			term[3] = i;
			AR.CurExpr = i;
			SeekScratch(AR.outfile,&position);
			e->onfile = position;
			if ( PutOut(BHEAD term,&position,AR.outfile,0) < 0 ) goto ProcErr;
			AR.DeferFlag = AC.ComDefer;
			NewSort();
			AN.ninterms = 0;
			t = e->inmem;
			while ( *t ) {
				for ( j = 0; j < *t; j++ ) term[j] = t[j];
				t += *t;
				AN.ninterms++; dd = AN.deferskipped;
				if ( AC.CollectFun && *term <= (AM.MaxTer/(2*sizeof(WORD))) ) {
					if ( GetMoreFromMem(term,&t) ) {
						LowerSortLevel(); goto ProcErr;
					}
				}
				AT.WorkPointer = term + *term;
				AN.RepPoint = AT.RepCount + 1;
				AR.CurDum = ReNumber(BHEAD term);
				if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
				if ( Generator(BHEAD term,0) ) {
					LowerSortLevel(); goto ProcErr;
				}
				AN.ninterms += dd;
			}
			AN.ninterms += dd;
			if ( EndSort(AM.S0->sBuffer,0) < 0 ) goto ProcErr;
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
#ifdef PARALLEL
	            if ( PF.me == MASTER ) SetScratch(AR.hidefile,&(e->onfile));
#else
				SetScratch(AR.hidefile,&(e->onfile));
#endif
				curfile = AR.hidefile;
				goto commonread;
			case LOCALEXPRESSION:
			case GLOBALEXPRESSION:
				AR.GetFile = 0;
#ifdef PARALLEL
				if ( PF.me == MASTER ) SetScratch(AR.infile,&(e->onfile));
#else
				SetScratch(AR.infile,&(e->onfile));
#endif
				curfile = AR.infile;
commonread:;
#ifdef PARALLEL
				if ( PF_Processor(e,i,LastExpression) ) {
					MesPrint("Error in PF_Processor");
					goto ProcErr;
				}
#else
				if ( GetTerm(BHEAD term) <= 0 ) {
					MesPrint("Expression %d has problems in scratchfile",i);
					retval = -1;
					break;
				}
				if ( AC.bracketindexflag ) OpenBracketIndex(i);
				term[3] = i;
				SeekScratch(AR.outfile,&position);
				e->onfile = position;
				if ( PutOut(BHEAD term,&position,AR.outfile,0) < 0 ) goto ProcErr;
				AR.DeferFlag = AC.ComDefer;
#ifdef WITHPTHREADS
				if ( AS.MultiThreaded && AC.mparallelflag == PARALLELFLAG ) {
					if ( ThreadsProcessor(e,LastExpression) ) {
						MesPrint("Error in ThreadsProcessor");
						goto ProcErr;
					}
				}
				else
#endif
				{
					NewSort();
					AR.MaxDum = AM.IndDum;
					AN.ninterms = 0;
					while ( GetTerm(BHEAD term) ) {
					  SeekScratch(curfile,&position);
					  AN.ninterms++; dd = AN.deferskipped;
					  if ( AC.CollectFun && *term <= (AM.MaxTer/(2*sizeof(WORD))) ) {
						if ( GetMoreTerms(term) < 0 ) {
						  LowerSortLevel(); goto ProcErr;
						}
					    SeekScratch(curfile,&position);
					  }
					  AT.WorkPointer = term + *term;
					  AN.RepPoint = AT.RepCount + 1;
					  AR.CurDum = ReNumber(BHEAD term);
					  if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
					  if ( Generator(BHEAD term,0) ) {
						LowerSortLevel(); goto ProcErr;
					  }
					  AN.ninterms += dd;
					  SetScratch(curfile,&position);
					  AR.InInBuf = (curfile->POfull-curfile->PObuffer)
							-DIFBASE(position,curfile->POposition)/sizeof(WORD);
					}
					AN.ninterms += dd;
					if ( LastExpression ) {
						if ( AR.infile->handle >= 0 ) {
							CloseFile(AR.infile->handle);
							AR.infile->handle = -1;
							remove(AR.infile->name);
							PUTZERO(AR.infile->POposition);
							AR.infile->POfill = AR.infile->POfull = AR.infile->PObuffer;
						}
					}
					if ( EndSort(AM.S0->sBuffer,0) < 0 ) goto ProcErr;
					e->numdummies = AR.MaxDum - AM.IndDum;
				}
				if ( AM.S0->TermsLeft )   e->vflags &= ~ISZERO;
				else                      e->vflags |= ISZERO;
				if ( AR.expchanged == 0 ) e->vflags |= ISUNMODIFIED;
				if ( AM.S0->TermsLeft ) AR.expflags |= ISZERO;
				if ( AR.expchanged )    AR.expflags |= ISUNMODIFIED;
				AR.GetFile = 0;
#endif
				break;
			case SKIPLEXPRESSION:
			case SKIPGEXPRESSION:
/*
				This can be greatly improved of course by file-to-file copy.
*/
#ifdef PARALLEL
				if ( PF.me != MASTER ) break;
#endif
				AR.GetFile = 0;
				SetScratch(AR.infile,&(e->onfile));
				if ( GetTerm(BHEAD term) <= 0 ) {
					MesPrint("Expression %d has problems in scratchfile",i);
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
				break;
			case HIDELEXPRESSION:
			case HIDEGEXPRESSION:
#ifdef PARALLEL
				if ( PF.me != MASTER ) break;
#endif
				AR.GetFile = 0;
				SetScratch(AR.infile,&(e->onfile));
				if ( GetTerm(BHEAD term) <= 0 ) {
					MesPrint("Expression %d has problems in scratchfile",i);
					retval = -1;
					break;
				}
				term[3] = i;
				AR.DeferFlag = 0;
				SetEndScratch(AR.hidefile,&position);
				e->onfile = position;
				*AM.S0->sBuffer = 0; firstterm = -1;
				do {
					WORD *oldipointer = AR.CompressPointer;
					WORD *comprtop = AR.ComprTop;
					AR.ComprTop = AM.S0->sTop;
					AR.CompressPointer = AM.S0->sBuffer;
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
					AR.CompressPointer = oldipointer;
					AR.ComprTop = comprtop;
				} while ( GetTerm(BHEAD term) );
				if ( FlushOut(&position,AR.hidefile,1) ) goto ProcErr;
				AR.hidefile->POfull = AR.hidefile->POfill;
				break;
			case DROPPEDEXPRESSION:
			case DROPLEXPRESSION:
			case DROPGEXPRESSION:
			case DROPHLEXPRESSION:
			case DROPHGEXPRESSION:
			case STOREDEXPRESSION:
			case HIDDENLEXPRESSION:
			case HIDDENGEXPRESSION:
			default:
				break;
		}
		}
		AR.KeptInHold = 0;
	}
	AR.DeferFlag = 0;
	AT.WorkPointer = term;
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

WORD
TestSub BARG2(WORD *,term,WORD,level)
{
	GETBIDENTITY
	WORD *m, *t, *r, retvalue, funflag, j;
	WORD *stop, *t1, *t2, funnum, wilds, tbufnum;
	NESTING n;
	CBUF *C = cbuf+AT.ebufnum;
	LONG isp, i;
	TABLES T;
	VOID *oldcompareroutine = AR.CompareRoutine;
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
				while ( *r == SUBEXPRESSION && r < m && r[3] ) {
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
							t1 = r + r[1];
							t2 = term + *term;
							*term -= r[1];
							m -= r[1];
							while ( t1 < t2 ) *r++ = *t1++;
							r = t;
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
							LOCK(ErrorMessageLock);
							MesPrint("Attempt to generate more terms than FORM can count");
							UNLOCK(ErrorMessageLock);
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
			i = -t[2] - 1;
			if ( t[3] < 0 ) {
				AN.TeInFun = 1;
				AR.TePos = WORDDIF(t,term);
				return(i);
			}
			AN.TeInFun = 0;
			AR.TePos = 0;
			AN.TeSuOut = t[3];
			AN.Frozen = 0;
			AT.TMaddr = m = AT.WorkPointer;
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
*/
					j = *t; tttstop = t + j;
					GETSTOP(t,ttstop);
					*m++ = j; t++;
					while ( t < ttstop ) {
						if ( *t == SUBEXPRESSION ) break;
						j = t[1]; NCOPY(m,t,j);
					}
					if ( t < ttstop ) {
/*
						We ran into a subexpression. It could be a $ or
						just e[(a^2)*b]. In either case we should evaluate
*/
						while ( t < tttstop ) *m++ = *t++;
						*AT.WorkPointer = m-AT.WorkPointer;
						m = AT.WorkPointer;
						AT.WorkPointer = m + *m;
						NewSort();
						if ( Generator(BHEAD m,AR.Cnumlhs) ) {
							LowerSortLevel(); goto EndTest;
						}
						if ( EndSort(m,0) < 0 ) goto EndTest;
						AN.Frozen = m;
						if ( *m == 0 ) {
							*m++ = 4; *m++ = 1; *m++ = 1; *m++ = 3;
						}
						else if ( m[*m] != 0 ) {
							LOCK(ErrorMessageLock);
							MesPrint("Bracket specification in expression should be one single term");
							UNLOCK(ErrorMessageLock);
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
							LOCK(ErrorMessageLock);
							MesPrint("Error while picking up contents of bracket");
							UNLOCK(ErrorMessageLock);
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
			return(i);
		}
		else if ( *t >= FUNCTION ) {
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
						LOCK(ErrorMessageLock);
						MesPrint("Encountered 0^0. Fatal error.");
						UNLOCK(ErrorMessageLock);
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
							t1 = DoubleCbuffer(AT.ebufnum,t1);
						while ( --i >= 0 ) *t1++ = *m++;
					}
					else {
						if ( (t1 + 20) > C->Top )
							t1 = DoubleCbuffer(AT.ebufnum,t1);
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
								m = DoubleCbuffer(AT.ebufnum,m);
							while ( --i >= 0 ) *m++ = *r++;
						}
						else {
							while ( (m + 20) > C->Top )
								m = DoubleCbuffer(AT.ebufnum,m);
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
			if ( functions[funnum-FUNCTION].spec == 0
				|| ( t[2] & DIRTYFLAG ) != 0 ) funflag = 1;
			if ( *t <= MAXBUILTINFUNCTION ) {
			if ( *t == THETA || *t == THETA2 ) {
				WORD *tstop, *tt2, kk;
				tstop = t + t[1];
				tt2 = t + FUNHEAD;
				while ( tt2 < tstop ) {
					if ( *tt2 > 0 && tt2[1] != 0 ) {
/*						funflag = 2; */
						goto DoSpec;
					}
					NEXTARG(tt2)
				}
				if ( !AT.RecFlag ) {
					if ( ( kk = DoTheta(t) ) == 0 ) {
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
					if ( *tt2 > 0 && tt2[1] != 0 ) {
/*						funflag = 2; */
						goto DoSpec;
					}
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
			}
			else if ( *t == DISTRIBUTION && t[FUNHEAD] == -SNUMBER
			&& t[FUNHEAD+1] >= -2 && t[FUNHEAD+1] <= 2
			&& t[FUNHEAD+2] == -SNUMBER
			&& t[FUNHEAD+4] <= -FUNCTION
			&& t[FUNHEAD+5] <= -FUNCTION ) {
				AN.TeInFun = -1;
				AN.TeSuOut = 0;
				AR.TePos = -1;
				return(1);
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
			else if ( *t == AM.polyfunnum ) {
				AN.TeInFun = -4;
				AN.TeSuOut = 0;
				AR.TePos = -1;
				return(1);
			}
			else if ( *t == AM.polygetremnum ) {
				AN.TeInFun = -5;
				AN.TeSuOut = 0;
				AR.TePos = -1;
				return(1);
			}
			else if ( *t == FACTORIN ) {
				if ( t[1] == FUNHEAD+2 && t[FUNHEAD] == -DOLLAREXPRESSION ) {
					AN.TeInFun = -6;
					AN.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}
				else if ( t[1] == FUNHEAD+2 && t[FUNHEAD] == -EXPRESSION ) {
					AN.TeInFun = -7;
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
					AN.TeInFun = -8;
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
					AN.TeInFun = -8;
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
					AN.TeInFun = -8;
					AN.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}
*/
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
		if ( t < m ) do {
			if ( *t < FUNCTION ) {
				t += t[1]; continue;
			}
			r = t + t[1];
			funnum = *t;
			if ( *t >= FUNCTION + WILDOFFSET ) funnum -= WILDOFFSET;
			if ( functions[funnum-FUNCTION].spec == 0 ) {
				AT.NestPoin->funsize = t + 1;
				t1 = t;
				t += FUNHEAD;
				while ( t < r ) {	/* Sum over arguments */
					if ( *t > 0 && t[1] ) {	/* Argument is dirty  */
						AT.NestPoin->argsize = t;
						AT.NestPoin++;
/*						stop = t + *t; */
/*						t2 = t; */
						t += ARGHEAD;
						while ( t < AT.NestPoin[-1].argsize+*(AT.NestPoin[-1].argsize) ) {
											/* Sum over terms */
							AT.RecFlag++;
/*							i = *t; */
							if ( ( retvalue = TestSub(BHEAD t,level) ) != 0 ) {
/*
								Possible size changes:
								Note defs at 471,467,460,400,425,328
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
*/
								t1[2] = 1;
								AT.RecFlag--;
								AT.NestPoin--;
								AN.TeInFun++;
								AR.TePos = 0;
								return(retvalue);
							}
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
						NewSort();
						if ( *t1 == AR.PolyFun && AR.PolyFunType == 2 ) {
							AR.CompareRoutine = &CompareSymbols;
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
								LowerSortLevel(); goto EndTest;
							}
							if ( AC.ncmod != 0 ) {
								if ( *r ) {
									if ( Modulus(r) ) {
										LowerSortLevel();
										AT.WorkPointer = r;
										goto EndTest;
									}
									if ( !*r ) {
										LowerSortLevel();
										AT.WorkPointer = r;
										return(0);
									}
								}
							}
							if ( *r ) StoreTerm(BHEAD r);
							AT.WorkPointer = r;
						}
						if ( EndSort(AT.WorkPointer+ARGHEAD,0) < 0 ) goto EndTest;
						m = AT.WorkPointer+ARGHEAD;
						if ( *t1 == AR.PolyFun && AR.PolyFunType == 2 ) {
							AR.CompareRoutine = oldcompareroutine;
						}
						while ( *m ) m += *m;
						i = WORDDIF(m,AT.WorkPointer);
						*AT.WorkPointer = i;
						AT.WorkPointer[1] = 0;
						if ( ToFast(AT.WorkPointer,AT.WorkPointer) ) {
							m = AT.WorkPointer;
							if ( *m <= -FUNCTION ) { m++; i = 1; }
							else { m += 2; i = 2; }
						}
						j = i - j;
						if ( j > 0 ) {
							r = m + j;
							if ( r > AT.WorkTop ) {
								LOCK(ErrorMessageLock);
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
								goto ReStart;
							}
/*
							And size changes here?????
*/
						}
						goto ReStart;
					}
					else if ( *t == -DOLLAREXPRESSION ) {
						if ( *t1 == TERMSINEXPR && t1[1] == FUNHEAD+2 ) {}
						else {
							if ( AR.Eside != LHSIDE ) {
								AN.TeInFun = 1; AR.TePos = 0;
								AT.TMbuff = AM.dbufnum; t1[2] |= DIRTYFLAG;
								return(1);
							}
							AC.lhdollarflag = 1;
						}
					}
					else if ( *t == -TERMSINBRACKET ) {
						if ( AR.Eside != LHSIDE ) {
							AN.TeInFun = 1; AR.TePos = 0;
							t1[2] |= DIRTYFLAG;
							return(1);
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
						for ( i = 0; i < T->numind; i++, t += 2 ) {
							if ( *t != -SNUMBER ) break;
						}
						if ( i < T->numind ) goto teststrict;

						isp = FindTableTree(T,t1+FUNHEAD,2);
						if ( isp < 0 ) goto teststrict;
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
									LOCK(ErrorMessageLock);
									MesPrint("Table boundary check. Argument %d",
									T->numind-j);
showtable:							AO.OutFill = AO.OutputLine = (UBYTE *)m;
									AO.OutSkip = 8;
									IniLine();
									WriteSubTerm(t1,1);
									FiniLine();
									UNLOCK(ErrorMessageLock);
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
teststrict:					if ( T->strict == -2 ) {
								term[0] = 0; return(0);
							}
							if ( T->strict < 0 ) goto NextFun;
							LOCK(ErrorMessageLock);
							MesPrint("Element in table is undefined");
							goto showtable;
						}
						rhsnumber = T->tablepointers[i];
#if ( TABLEEXTENSION == 2 )
						tbufnum = T->bufnum;
#else
						tbufnum = T->tablepointers[i+1];
#endif
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
						LOCK(ErrorMessageLock);
						MesWork();
						UNLOCK(ErrorMessageLock);
					}
					wilds = 0;
/*					if ( MatchFunction(BHEAD T->pattern,t1,&wilds) > 0 ) {  } */
					if ( MatchFunction(BHEAD Tpattern,t1,&wilds) > 0 ) {
						AT.WorkPointer = oldwork;
						if ( AT.NestPoin != AT.Nest ) return(1);

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
							return(1);
						}
						AC.lhdollarflag = 1;
					}
					t++;
				}
			}
			t = r;
		} while ( t < m );
	}
	return(0);
EndTest:;
	LOCK(ErrorMessageLock);
EndTest2:;
	MesCall("TestSub");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] TestSub : 
 		#[ InFunction :			WORD InFunction(term,termout)
*/
/**
 *		Makes the replacement the subexpression with the number 'replac'
 *		in a function argument. Additional information is passed in some
 *		of the AR, AN, AT variables.
 *
 *		@param term     The input term
 *		@param termout  The output term
 *		@return         0: everything is fine, Negative: fatal, Positive: error.
 *
 *		Special attention should be given to nested functions!
 */

WORD
InFunction ARG2(WORD *,term,WORD *,termout)
{
	GETIDENTITY
	WORD *m, *t, *r, *rr, sign = 1;
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
			while ( t < r ) {	/* t points at an argument */
				if ( *t > 0 && t[1] ) {	/* Argument has been modified */

					/* This whole argument must be redone */

					AR.DeferFlag = 0;
					v = t + *t;
					t += ARGHEAD;		/* First term */
					w = 0;	/* to appease the compilers warning devices */
					while ( from < t ) {
						if ( from == u ) w = m;
						*m++ = *from++;
					}
					to = m;
					NewSort();
					if ( *u == AR.PolyFun && AR.PolyFunType == 2 ) {
						AR.CompareRoutine = &CompareSymbols;
					}
					AR.PolyFun = 0;
					while ( t < v ) {
						i = *t;
						NCOPY(m,t,i);
						m = to;
                        if ( AT.WorkPointer < m+*m ) AT.WorkPointer = m + *m;
						if ( Generator(BHEAD m,AR.Cnumlhs) ) {
							LowerSortLevel(); goto InFunc;
						}
					}
					/* w = the function */
					/* v = the next argument */
					/* u = the function */
					/* to is new argument */

					to -= ARGHEAD;
					if ( EndSort(m,0) < 0 ) goto InFunc;
					AR.PolyFun = oldPolyFun;
					if ( *u == AR.PolyFun && AR.PolyFunType == 2 ) {
						AR.CompareRoutine = &Compare1;
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
					*termout = WORDDIF(m,termout);
					AR.DeferFlag = olddefer;
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
								d = ModOptdollars[nummodopt].dstruct+identity;
							}
							else {
								LOCK(d->pthreadslockread);
							}
						}
					}
#endif
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
								NewSort();
								AR.CompareRoutine = &CompareSymbols;
								r = to + ARGHEAD;
								while ( r < m ) {
									rr = r; r += *r;
									if ( SymbolNormalize(rr,0,2) ) goto InFunc;
									if ( StoreTerm(BHEAD rr) ) {
										AR.CompareRoutine = &Compare1;
										LowerSortLevel();
										Terminate(-1);
									}
								}
								if ( EndSort(to+ARGHEAD,0) < 0 ) goto InFunc;
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
							LOCK(ErrorMessageLock);
							MesPrint("!!!Undefined $-variable: $%s!!!",
							AC.dollarnames->namebuffer+d->name);
							UNLOCK(ErrorMessageLock);
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
					*termout = WORDDIF(m,termout);
					AR.DeferFlag = olddefer;
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
								d = ModOptdollars[nummodopt].dstruct+identity;
							}
							else {
								LOCK(d->pthreadslockread);
							}
						}
					}
#endif
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
								LOCK(ErrorMessageLock);
								MesPrint("$%s has wrong type for tensor substitution",
								AC.dollarnames->namebuffer+d->name);
								UNLOCK(ErrorMessageLock);
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
							LOCK(ErrorMessageLock);
							MesPrint("$%s is undefined in tensor substitution",
							AC.dollarnames->namebuffer+d->name);
							UNLOCK(ErrorMessageLock);
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
					*termout = m - termout;
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
	LOCK(ErrorMessageLock);
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
	UNLOCK(ErrorMessageLock);
	return(1);

InFunc:
	LOCK(ErrorMessageLock);
	MesCall("InFunction");
	UNLOCK(ErrorMessageLock);
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

WORD
InsertTerm BARG6(WORD *,term,WORD,replac,WORD,extractbuff,WORD *,position,WORD *,termout,WORD,tepos)
{
	GETBIDENTITY
	WORD *m, *t, *r, i, l2, j;
	WORD *u, *v, l1, *coef;
	coef = AT.WorkPointer;
	if ( ( AT.WorkPointer = coef + 2*AM.MaxTal ) > AT.WorkTop ) {
		LOCK(ErrorMessageLock);
		MesWork();
		UNLOCK(ErrorMessageLock);
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
				l2 <<= 1;
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
			if ( (*termout)*sizeof(WORD) > AM.MaxTer ) goto InsCall;
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
	LOCK(ErrorMessageLock);
	MesCall("InsertTerm");
	UNLOCK(ErrorMessageLock);
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

LONG
PasteFile ARG7(WORD,number,WORD *,accum,POSITION *,position,WORD **,accfill
			  ,RENUMBER,renumber,WORD *,freeze,WORD,nexpr)
{
	GETIDENTITY
	WORD *r, l, *m, i;
	WORD *stop, *s1, *s2;
	POSITION AccPos;
	WORD InCompState;
	WORD *oldipointer;
	LONG retlength;
    stop = (WORD *)(((UBYTE *)(accum)) + 2*AM.MaxTer);
	*accum++ = number;
	while ( --number >= 0 ) accum += *accum;
	if ( freeze ) {
		AccPos = *position;
		oldipointer = AR.CompressPointer;
		do {
			AR.CompressPointer = oldipointer;
			if ( ( l = GetFromStore(accum,&AccPos,renumber,&InCompState,nexpr) ) < 0 )
				goto PasErr;
			if ( !l ) { *accum = 0; return(0); }
/*			ADDPOS(AccPos,InCompState * wsizeof(WORD)); */
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
		retlength = DIFBASE(AccPos,*position) / sizeof(WORD);
	}
	else {
		if ( ( l = GetFromStore(accum,position,renumber,&InCompState,nexpr) ) < 0 ) {
			LOCK(ErrorMessageLock);
			MesCall("PasteFile");
			UNLOCK(ErrorMessageLock);
			SETERROR(-1)
		}
		if ( l == 0 ) { *accum = 0; return(0); }
		retlength = InCompState;
	}
	accum += l;
	if ( accum > stop ) {
		LOCK(ErrorMessageLock);
		MesPrint("Buffer too small in PasteFile");
		UNLOCK(ErrorMessageLock);
		SETERROR(-1)
	}
	*accum = 0;
	*accfill = accum;
	return(retlength);
PasErr:
	LOCK(ErrorMessageLock);
	MesCall("PasteFile");
	UNLOCK(ErrorMessageLock);
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

WORD *
PasteTerm BARG5(WORD,number,WORD *,accum,WORD *,position,WORD,times,WORD,divby)
{
	GETBIDENTITY
	WORD *t, *r, x, y, z;
	WORD *m, *u, l1, a[2];
    m = (WORD *)(((UBYTE *)(accum)) + 2*AM.MaxTer);
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
			LOCK(ErrorMessageLock);
			MesCall("PasteTerm");
			UNLOCK(ErrorMessageLock);
			return(0);
		}
		x = l1;
		x <<= 1;
		if ( x < 0 ) { accum -= x; *accum++ = x - 1; }
		else		 { accum += x; *accum++ = x + 1; }
		*u = WORDDIF(accum,u);
	}
	if ( accum >= m ) {
		LOCK(ErrorMessageLock);
		MesPrint("Buffer too small in PasteTerm");
		UNLOCK(ErrorMessageLock);
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

WORD
FiniTerm BARG5(WORD *,term,WORD *,accum,WORD *,termout,WORD,number,WORD,tepos)
{
	GETBIDENTITY
	WORD *m, *t, *r, i, numacc, l2, ipp;
	WORD *u, *v, l1, *coef = AT.WorkPointer, *oldaccum;
	if ( ( AT.WorkPointer = coef + 2*AM.MaxTal ) > AT.WorkTop ) {
		LOCK(ErrorMessageLock);
		MesWork();
		UNLOCK(ErrorMessageLock);
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
					if ( AC.ncmod != 0 && TakeModulus((UWORD *)coef,&l1,AC.cmod,AC.ncmod,0) ) goto FiniCall;
				}
				accum += *accum;
			} while ( --numacc >= 0 );
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
									 if ( *v++ != *u++ ) break; i--; }
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
			i <<= 1;
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
	LOCK(ErrorMessageLock);
	MesCall("FiniTerm");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] FiniTerm : 
 		#[ Generator :			WORD Generator(BHEAD term,level)
*/
 
static WORD zeroDollar[] = { 0, 0 };
/*
static long debugcounter = 0;
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

WORD
Generator BARG2(WORD *,term,WORD,level)
{
	GETBIDENTITY
	WORD replac, *accum, *termout, *t, i, j, tepos, applyflag = 0, *StartBuf;
	WORD *a, power, power1, DumNow = AR.CurDum, oldtoprhs, retnorm, extractbuff;
	int *RepSto = AN.RepPoint, iscopy = 0;
	CBUF *C = cbuf+AM.rbufnum, *CC = cbuf + AT.ebufnum;
	LONG posisub, oldcpointer;
	DOLLARS d = 0;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1, id;
#endif
#ifdef WITHPTHREADS
/*
if ( Expressions[0].bracketinfo != 0 &&
		 Expressions[0].bracketinfo->indexbuffer == 0 &&
		 Expressions[0].bracketinfo->bracketbuffer != 0 ) {
	LOCK(ErrorMessageLock);
	MesPrint("Problems with bracket index in thread %d",AT.identity);
	UNLOCK(ErrorMessageLock);
}
*/
#endif

	oldtoprhs = CC->numrhs;
	oldcpointer = CC->Pointer - CC->Buffer;

ReStart:
	if ( ( replac = TestSub(BHEAD term,level) ) == 0 ) {
		if ( applyflag ) { TableReset(); applyflag = 0; }
Renormalize:
		if ( ( retnorm = Normalize(BHEAD term) ) != 0 ) {
			if ( retnorm > 0 ) goto ReStart;
			goto GenCall;
		}
		if ( !*term ) goto Return0;
		if ( AN.PolyNormFlag ) {
			if ( PolyFunMul(BHEAD term) ) goto GenCall;
			if ( !*term ) goto Return0;
		}
		if ( AT.WorkPointer < (WORD *)(((UBYTE *)(term)) + AM.MaxTer) )
			 AT.WorkPointer = (WORD *)(((UBYTE *)(term)) + AM.MaxTer);
		do {
SkipCount:	level++;
			if ( level > AR.Cnumlhs ) {
				if ( AR.DeferFlag ) {
#ifdef PARALLEL
					/*[17sen2003 mt]: */
					if ( PF.me != MASTER && AC.mparallelflag == PARALLELFLAG ){
					if ( PF_Deferred(term,level) ) goto GenCall;
				  }
				  else
#endif
					if ( Deferred(BHEAD term,level) ) goto GenCall;
					goto Return0;
				}
				if ( AC.ncmod != 0 ) {
					if ( Modulus(term) ) goto GenCall;
					if ( !*term ) goto Return0;
				}
				if ( AR.CurDum > AM.IndDum && AR.sLevel <= 0 ) {
					ReNumber(BHEAD term); Normalize(BHEAD term);
					if ( !*term ) goto Return0;
					if ( AR.CurDum > AR.MaxDum ) AR.MaxDum = AR.CurDum;
				}
				if ( AR.PolyFun > 0 && AR.sLevel <= 0 ) {
					if ( PrepPoly(term) != 0 ) goto Return0;
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
					return(i);
				}
				else {
					if ( AT.WorkPointer < term + *term ) AT.WorkPointer = term + *term;
					*AT.WorkPointer = 0;
					AN.RepPoint = RepSto;
					i = StoreTerm(BHEAD term);
					CC->numrhs = oldtoprhs;
					CC->Pointer = CC->Buffer + oldcpointer;
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
					if ( MultDo(term,C->lhs[level]) ) goto GenCall;
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
						while ( !DoIfStatement(ifcode,term) ) {
							level = C->lhs[level][2];
							if ( C->lhs[level][0] != TYPEELIF ) break;
						}
						AT.WorkPointer = ifcode;
					}
#else
					while ( !DoIfStatement(C->lhs[level],term) ) {
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
								LOCK(ErrorMessageLock);
								MesPrint("$%s should have been an index"
								,AC.dollarnames->namebuffer+d->name);
								AN.currentTerm = term;
								MesPrint("Current term: %t");
								AN.listinprint = printscratch;
								printscratch[0] = DOLLAREXPRESSION;
								printscratch[1] = theindex;
								MesPrint("$%s = %$"
								,AC.dollarnames->namebuffer+d->name);
								UNLOCK(ErrorMessageLock);
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
/*
						At this point it is safest to determine CurDum
*/
						AR.CurDum = DetCurDum(term);
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
									LOCK(ErrorMessageLock);
									MesPrint("$%s should have been an index"
									,AC.dollarnames->namebuffer+d->name);
									AN.currentTerm = term;
									MesPrint("Current term: %t");
									AN.listinprint = printscratch;
									printscratch[0] = DOLLAREXPRESSION;
									printscratch[1] = theindex;
									MesPrint("$%s = %$"
									,AC.dollarnames->namebuffer+d->name);
									UNLOCK(ErrorMessageLock);
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
						AR.CompressPointer = cp;
						if ( WildFill(BHEAD term,term,op) < 0 ) goto GenCall;
						AR.CompressPointer = op;
						ReNumber(BHEAD term);
						goto Renormalize;
					}
				  case TYPECHISHOLM:
					if ( Chisholm(BHEAD term,level) ) goto GenCall;
CommonEnd:
					AT.WorkPointer = term + *term;
					goto Return0;
				  case TYPEARG:
					if ( ( i = execarg(term,level) ) < 0 ) goto GenCall;
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
					if ( execarg(term,level) < 0 ) goto GenCall;
					level = C->lhs[level][2];
					break;
				  case TYPEFACTARG:
				  case TYPEFACTARG2:
					{ WORD jjj;
					if ( ( jjj = execarg(term,level) ) < 0 ) goto GenCall;
					if ( jjj > 0 ) goto ReStart;
					level = C->lhs[level][2];
					break; }
				  case TYPEEXIT:
					LOCK(ErrorMessageLock);
					MesPrint("%s",C->lhs[level]+3);
					UNLOCK(ErrorMessageLock);
					goto GenCall;
				  case TYPESETEXIT:
					AM.exitflag = 1; /* no danger of race conditions */
					break;
				  case TYPEPRINT:
					AN.currentTerm = term;
					AN.numlistinprint = (C->lhs[level][1] - C->lhs[level][2] - 3)/2;
					AN.listinprint = C->lhs[level]+3+C->lhs[level][2];
					LOCK(ErrorMessageLock);
					MesPrint((char *)(C->lhs[level]+3));
					UNLOCK(ErrorMessageLock);
					break;
				  case TYPEFPRINT:
					{
					int oldFOflag;
					WORD oldPrintType;
					LOCK(ErrorMessageLock);
					oldFOflag = AM.FileOnlyFlag;
					oldPrintType = AO.PrintType;
					if ( AC.LogHandle >= 0 ) {
						AM.FileOnlyFlag = 1;
						AO.PrintType |= PRINTLFILE;
					}
					AN.currentTerm = term;
					AN.numlistinprint = (C->lhs[level][1] - C->lhs[level][2] - 3)/2;
					AN.listinprint = C->lhs[level]+3+C->lhs[level][2];
					MesPrint((char *)(C->lhs[level]+3));
					AO.PrintType = oldPrintType;
					AM.FileOnlyFlag = oldFOflag;
					UNLOCK(ErrorMessageLock);
					}
					break;
				  case TYPEREDEFPRE:
					j = C->lhs[level][2];
#ifdef PARALLEL
					/*[08nov2005 mt] PF.redef[j] = PF.ginterms;*/
					/*[14sep2005 mt]:*/
					if(PF.me == MASTER)
						/*off parallel. The master must collect preprovar for
							broadcasting itself:*/
						if(PF.redef[j] == 0)/*Not counted yet, count it:*/
							PF.mnumredefs++;
					/*:[14sep2005 mt]*/
					/*[08nov2005 mt]:*/
					PF.redef[j] = PF.ginterms;
					/*:[08nov2005 mt]*/
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
					if ( FullRenumber(term,C->lhs[level][2]) ) goto GenCall;
					AT.WorkPointer = term + *term;
					if ( *term == 0 ) goto Return0;
					break;
				  case TYPETRY:
					if ( TryDo(term,C->lhs[level],level) ) goto GenCall;
					AT.WorkPointer = term + *term;
					goto Return0;
				  case TYPEASSIGN:
					{ WORD onc = AR.NoCompress;
/*
					Here we have to assign an expression to a $ variable.
*/
					AR.NoCompress = 1;
					AN.cTerm = AN.currentTerm = term;
					AT.WorkPointer = term + *term;
					*AT.WorkPointer++ = 0;
					if ( AssignDollar(term,level) ) goto GenCall;
					AT.WorkPointer = term + *term;
					AN.cTerm = 0;
					AR.NoCompress = onc;
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
					if ( InsideDollar(C->lhs[level],level) < 0 ) goto GenCall;
					level = C->lhs[level][2];
					break;
				  case TYPETERM:
					return(execterm(term,level));
				  case TYPEDETCURDUM:
					AT.WorkPointer = term + *term;
					AR.CurDum = DetCurDum(term);
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
					if ( DoMerge(term,level,C->lhs[level][2],C->lhs[level][3]) )
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
				  case TYPEMODULUSGCD:
					AT.WorkPointer = term + *term;
					if ( ModulusGCD1(C->lhs[level][2],C->lhs[level][4],
					C->lhs[level][5],term,C->lhs[level][3]) ) goto GenCall;
					break;
				  case TYPECHAININ:
					AT.WorkPointer = term + *term;
					if ( ChainIn(term,C->lhs[level][2]) ) goto GenCall;
					AT.WorkPointer = term + *term;
					break;
				  case TYPECHAINOUT:
					AT.WorkPointer = term + *term;
					if ( ChainOut(term,C->lhs[level][2]) ) goto GenCall;
					AT.WorkPointer = term + *term;
					break;
				  case TYPEPOLYNORM:
					AT.WorkPointer = term + *term;
					if ( PolyNorm(BHEAD term,level,C->lhs[level][2],C->lhs[level][3]) ) goto GenCall;
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
			if ( AN.TeInFun == -1 && DoDistrib(BHEAD term,level) ) goto GenCall;
			else if ( AN.TeInFun == -2 && DoDelta3(term,level) ) goto GenCall;
			else if ( AN.TeInFun == -3 && DoTableExpansion(term,level) ) goto GenCall;
			else if ( AN.TeInFun == -4 && DoPolynomial(term,level) ) goto GenCall;
			else if ( AN.TeInFun == -5 && DoPolyGetRem(term,level) ) goto GenCall;
			else if ( AN.TeInFun == -6 && FactorIn(BHEAD term,level) ) goto GenCall;
			else if ( AN.TeInFun == -7 && FactorInExpr(BHEAD term,level) ) goto GenCall;
			else if ( AN.TeInFun == -8 && TermsInBracket(BHEAD term,level) < 0 ) goto GenCall;
		}
		else {
			termout = AT.WorkPointer;
		    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
			if ( AT.WorkPointer > AT.WorkTop ) goto OverWork;
			if ( InFunction(term,termout) ) goto GenCall;
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
			d = DolToTerms(replac);
			if ( d ) {
				iscopy = 1;
				StartBuf = d->where;
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
					if ( dtype != MODLOCAL ) {
						if ( StartBuf[0] && StartBuf[StartBuf[0]] ) {
							LOCK(ErrorMessageLock);
							MesPrint("A dollar variable with modoption max, min or sum can have only one term");
							UNLOCK(ErrorMessageLock);
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
			    AT.WorkPointer = (WORD *)(((UBYTE *)(termout)) + AM.MaxTer);
				if ( InsertTerm(BHEAD term,replac,extractbuff,
					&(StartBuf[posisub]),termout,tepos) < 0 ) goto GenCall;
				AT.WorkPointer = termout + *termout;
				*AN.RepPoint = 1;
				AR.expchanged = 1;
				posisub += StartBuf[posisub];
#ifdef WITHPTHREADS
				if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
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
				if ( iscopy == 0 ) {
/*
					There are cases in which a bigger buffer is created
					on the fly, like with wildcard buffers.
					We play it safe here. Maybe we can be more selective
					in some distant future?
*/
					StartBuf = cbuf[extractbuff].Buffer;
				}
			}
#ifdef WITHPTHREADS
			if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); dtype = 0; }
#endif
			if ( iscopy ) {
				M_free(d,"Copy of dollar variable");
				d = 0;
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
					if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
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
					if ( iscopy == 0 ) StartBuf = cbuf[extractbuff].Buffer;
					i--; posit--; same--;
				}
			} while ( i > 0 );
#ifdef WITHPTHREADS
			if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); dtype = 0; }
#endif
			if ( iscopy ) {
				if ( d ) {
					M_free(d,"Copy of dollar variable");
					d = 0;
				}
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
					if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
					if ( ( AS.Balancing && CC->numrhs == 0 ) && ( i > 0 ) && ( id = ConditionalGetAvailableThread() ) >= 0 ) {
						if ( BalanceRunThread(BHEAD id,termout,level) < 0 ) goto GenCall;
					}
					else
#endif
					if ( Generator(BHEAD termout,level) ) goto GenCall;
#ifdef WITHPTHREADS
					if ( dtype > 0 && dtype != MODLOCAL ) { dtype = 0; break; }
#endif
					if ( iscopy == 0 ) StartBuf = cbuf[extractbuff].Buffer;
					i--; posit--;
				}
			}
#ifdef WITHPTHREADS
			if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); dtype = 0; }
#endif
			if ( iscopy ) {
				if ( d ) {
					M_free(d,"Copy of dollar variable");
					d = 0;
				}
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
			if ( ( renumber = GetTable(replac,&(AT.posWorkSpace[position])) ) == 0 ) goto GenCall;
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
				if ( ( extra = PasteFile(i,accum,&(AT.posWorkSpace[position])
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
#ifdef WITHPTHREADS
			M_free(renumber->symb.lo,"VarSpace");
			M_free(renumber,"Renumber");
#endif
		}
		else {			/* Active expression */
			aa = accum = AT.WorkPointer;
			if ( ( (WORD *)(((UBYTE *)(AT.WorkPointer)) + 2 * AM.MaxTer + sizeof(WORD)) ) > AT.WorkTop )
					goto OverWork;
			*accum++ = -1; AT.WorkPointer++;
			if ( DoOnePow(term,power,replac,accum,aa,level,Freeze) ) goto GenCall;
			AT.WorkPointer = aa;
		}
  	}
Return0:
	AR.CurDum = DumNow;
	AN.RepPoint = RepSto;
	CC->numrhs = oldtoprhs;
	CC->Pointer = CC->Buffer + oldcpointer;
	return(0);

GenCall:
	if ( AM.tracebackflag ) {
		termout = term;
		LOCK(ErrorMessageLock);
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
		UNLOCK(ErrorMessageLock);
	}
	CC->numrhs = oldtoprhs;
	CC->Pointer = CC->Buffer + oldcpointer;
	return(-1);
OverWork:
	CC->numrhs = oldtoprhs;
	CC->Pointer = CC->Buffer + oldcpointer;
	LOCK(ErrorMessageLock);
	MesWork();
	UNLOCK(ErrorMessageLock);
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

WORD
DoOnePow ARG7(WORD *,term,WORD,power,WORD,nexp,WORD *,accum
			 ,WORD *,aa,WORD,level,WORD *,freeze)
{
	GETIDENTITY
	POSITION oldposition, startposition;
	WORD *acc, *termout, fromfreeze = 0;
	WORD *oldipointer = AR.CompressPointer;
	FILEHANDLE *fi;
	WORD type, retval;
	WORD oldGetOneFile = AR.GetOneFile;
	WORD olddummies = AR.CurDum;
	WORD extradummies = Expressions[nexp].numdummies;
	type = Expressions[nexp].status;
	if ( type == HIDDENLEXPRESSION || type == HIDDENGEXPRESSION ) {
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
		if ( ( brapos = FindBracket(&(Expressions[nexp]),freeze) ) == 0 )
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
					LOCK(ErrorMessageLock);
					MesWork();
					UNLOCK(ErrorMessageLock);
					return(-1);
				}
				if ( FiniTerm(BHEAD term,aa,termout,nexp,0) ) goto PowCall;
				if ( *termout ) {
					AT.WorkPointer = termout + *termout;
					*AN.RepPoint = 1;
					AR.expchanged = 1;
					if ( Generator(BHEAD termout,level) ) goto PowCall;
				}
			}
			else {
				if ( acc > AT.WorkTop ) {
					LOCK(ErrorMessageLock);
					MesWork();
					UNLOCK(ErrorMessageLock);
					return(-1);
				}
				if ( DoOnePow(term,power,nexp,acc,aa,level,freeze) ) goto PowCall;
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
			LOCK(ErrorMessageLock);
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
	LOCK(ErrorMessageLock);
#ifdef WITHSEEK
PowCall2:;
#endif
	MesCall("DoOnePow");
	UNLOCK(ErrorMessageLock);
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

WORD
Deferred BARG2(WORD *,term,WORD,level)
{
	GETBIDENTITY
	POSITION startposition;
	WORD *t, *m, *mstop, *tstart, decr, oldb, *termout, i, *oldwork, retval;
	WORD *oldipointer = AR.CompressPointer, *oldPOfill = AR.infile->POfill;
	WORD oldGetOneFile = AR.GetOneFile;
	WORD *copyspace = 0, *tbegin, olddummies = AR.CurDum;
	AR.GetOneFile = 1;
	oldwork = AT.WorkPointer;
    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
	if ( Expressions[AR.CurExpr].numdummies && AR.CurDum > AM.IndDum ) {
		copyspace = AT.WorkPointer;
	    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
	}
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
/*
		Now worry about dummy indices. If those are present and there are
		already dummy indices we have to make a copy, because otherwise
		the bracket scan gets messed up. This is what copyspace is for.
*/
		if ( copyspace ) {
			t = tbegin = copyspace; m = tstart; i = *tstart;
			NCOPY(t,m,i);
			MoveDummies(BHEAD tbegin,olddummies-AM.IndDum);
			AR.CurDum = olddummies + Expressions[AR.CurExpr].numdummies;
		}
		else tbegin = tstart;
		if ( InsertTerm(BHEAD term,0,AM.rbufnum,tbegin,termout,0) < 0 ) {
			goto DefCall;
		}
		*tstart = oldb;
		AT.WorkPointer = termout + *termout;
		if ( Generator(BHEAD termout,level) ) goto DefCall;
		AR.CompressPointer = oldipointer;
		AT.WorkPointer = termout;
		retval = GetOneTerm(BHEAD AT.WorkPointer,AR.infile,&startposition,0);

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
	if ( AR.infile->handle < 0 ) AR.infile->POfill = oldPOfill;
	AR.DeferFlag = 1;
	AR.GetOneFile = oldGetOneFile;
	AR.CurDum = olddummies;
	AT.WorkPointer = oldwork;
	return(0);
DefCall:;
	LOCK(ErrorMessageLock);
	MesCall("Deferred");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] Deferred : 
 		#[ PrepPoly :			WORD PrepPoly(term)
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
 */

WORD
PrepPoly ARG1(WORD *,term)
{
	GETIDENTITY
	WORD count = 0, i, jcoef, ncoef, j;
	WORD *t, *m, *r, *tstop, *poly = 0, *v, *w, *vv, *ww;
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *arg1, *arg2, *arg3;
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
	if ( count == 0 ) {
		poly = t = tstop;
		if ( i == 3 && m[-2] == 1 && (m[-3]&MAXPOSITIVE) == m[-3] ) {
			*m++ = AR.PolyFun;
			if ( AR.PolyFunType == 1 ) {
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
			r = tstop;
			*m++ = AR.PolyFun;
			if ( AR.PolyFunType == 1 ) {
				*m++ = FUNHEAD+ARGHEAD+i+1;
				FILLFUN(m)
				*m++ = ARGHEAD+i+1;
				*m++ = 0;
				FILLARG(m)
				*m++ = i+1;
				NCOPY(m,r,i);
			}
			else if ( AR.PolyFunType == 2 ) {
				*m++ = FUNHEAD+ARGHEAD+i+3;
				FILLFUN(m)
				*m++ = ARGHEAD+i+1;
				*m++ = 0;
				FILLARG(m)
				*m++ = i+1;
				NCOPY(m,r,i);
				*m++ = -SNUMBER;
				*m++ = 1;
			}
		}
	}
	else if ( AR.PolyFunType == 1 ) {
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
				ncoef <<= 1;
				m += ABS(ncoef);
				if ( ncoef < 0 ) ncoef--;
				else ncoef++;
				*m++ = ncoef;
				*ww = WORDDIF(m,ww);
				if ( AC.ncmod != 0 ) {
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
/*
 		#[ Two arguments :
*/
		m = term + *term;
		r = poly + poly[1];
		arg1 = arg2 = poly+FUNHEAD;
		if ( arg2 < r ) { NEXTARG(arg2) }
		arg3 = arg2;
		if ( arg3 < r ) { NEXTARG(arg3) }
		if ( arg3 != r || arg2 == r ) return(0); /* numargs != 2 */
/*
		Try whether there are zeroes. They make life either easy or hard.
*/
		if ( arg2[0] == -SNUMBER && arg2[1] == 0 ) {
			if ( arg1[0] == -SNUMBER && arg1[1] == 0 ) {
				LOCK(ErrorMessageLock);
				MesPrint("0/0 in PolyRatFun.");
				UNLOCK(ErrorMessageLock);
			}
			else {
				LOCK(ErrorMessageLock);
				MesPrint("Division by zero in PolyRatFun.");
				UNLOCK(ErrorMessageLock);
			}
			Terminate(-1);
		}
		if ( arg1[0] == -SNUMBER && arg1[1] == 0 ) return(1);
/*
		If the main coefficient is one we don't do anything
*/
		if ( m[-1] == 3 && *tstop == 1 && tstop[1] == 1 ) {
			i = poly[1];
			t = poly;
			NCOPY(m,t,i);
		}
		else if ( *t <= -FUNCTION ) {
IllegalContent:
			LOCK(ErrorMessageLock);
			MesPrint("The PolyRatFun may only contain symbols and/or numbers.");
			UNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		else if ( *arg1 < 0 ) {
			r = tstop;
			if ( *arg1 == -SNUMBER ) {
				w = m;
				*m = AR.PolyFun;
				m += FUNHEAD+ARGHEAD;
				v = m;
				*m++ = 5+i;
				*m++ = SNUMBER;
				*m++ = 4;
				*m++ = arg1[1];
				*m++ = 1;
				NCOPY(m,r,i);
				AT.WorkPointer = m;
				if ( Normalize(BHEAD v) ) Terminate(-1);
				AT.WorkPointer = oldworkpointer;
				m = w+1;
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
				if ( *arg2 < 0 ) { *m++ = arg2[0]; *m++ = arg2[1]; }
				else {
					j = *arg2;
					NCOPY(m,arg2,j);
				}
				w[1] = m - w;
			}
			else if ( *arg1 == -SYMBOL ) {
				w = m;
				*m++ = AR.PolyFun;
				*m++ = FUNHEAD+ARGHEAD+5+i;
				FILLFUN(m)
				*m++ = ARGHEAD+5+i;
				*m++ = 0;
				FILLARG(m)
				*m++ = 5+i;
				*m++ = SYMBOL;
				*m++ = 4;
				*m++ = arg1[1];
				*m++ = 1;
				NCOPY(m,r,i);
				if ( *arg2 < 0 ) { *m++ = arg2[0]; *m++ = arg2[1]; }
				else {
					j = *arg2;
					NCOPY(m,arg2,j);
				}
				w[1] = m - w;
			}
			else goto IllegalContent;
		}
		else {
			i = m[-1];
			w = m;
			*m++ = AR.PolyFun;
			m += ARGHEAD+FUNHEAD-1;
			arg1 += ARGHEAD;
			jcoef = i < 0 ? (i+1)>>1:(i-1)>>1;
			v = t = arg1;
/*
			Test now the scalar nature of the argument.
			No indices allowed.
*/
			while ( t < arg2 ) {
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
			while ( t < arg2 ) {
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
				ncoef <<= 1;
				m += ABS(ncoef);
				if ( ncoef < 0 ) ncoef--;
				else ncoef++;
				*m++ = ncoef;
				*ww = WORDDIF(m,ww);
				if ( AC.ncmod != 0 ) {
					if ( Modulus(ww) ) Terminate(-1);
					if ( *ww == 0 ) return(1);
					m = ww + *ww;
				}
				t = v;
			}
			w[FUNHEAD] = (WORDDIF(m,w)) - FUNHEAD;
			w[FUNHEAD+1] = 0;
			if ( ToFast(w+FUNHEAD,w+FUNHEAD) ) {
				if ( w[FUNHEAD] <= -FUNCTION ) {
					goto IllegalContent;
				}
				m = w + FUNHEAD+2;
			}
			if ( *arg2 < 0 ) { *m++ = arg2[0]; *m++ = arg2[1]; }
			else {
				j = *arg2; t = arg2;
				NCOPY(m,t,j);
			}
			w[1] = WORDDIF(m,w); w += 2;
			FILLFUN(w)
		}
		t = poly + poly[1];
		while ( t < tstop ) *poly++ = *t++;
/*
 		#] Two arguments :
*/
	}
	else {
		LOCK(ErrorMessageLock);
		MesPrint("Illegal value for PolyFunType in PrepPoly");
		UNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	r = term + *term;
	AT.PolyAct = WORDDIF(poly,term);
	while ( r < m ) *poly++ = *r++;
	*poly++ = 1;
	*poly++ = 1;
	*poly++ = 3;
	*term = WORDDIF(poly,term);
	return(0);
}

/*
 		#] PrepPoly : 
 		#[ PolyFunMul :			WORD PolyFunMul(term)
*/
/**
 *		Multiplies the arguments of multiple occurrences of the polyfun.
 *		In this routine we do the original PolyFun with one argument only.
 *		The PolyRatFun (PolyFunType = 2) is done is a dedicated routine
 *		in the file polynito.c
 *		The new result is written over the old result.
 *
 *		@param term It contains the input term and later the output.
 *		@return Normal conventions (OK = 0).
 */

WORD
PolyFunMul BARG1(WORD *,term)
{
	GETBIDENTITY
	WORD *t, *fun1, *fun2, *t1, *t2, *m, *w, *tt1, *tt2, *arg1, *arg2;
	WORD *tstop;
	WORD n1, n2, i1, i2, l1, l2, l3, l4, action = 0, noac = 0;
	if ( AR.PolyFunType == 2 ) return(PolyRatFunMul(BHEAD term));
retry:
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
	NewSort();
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
				if ( AC.ncmod != 0 && TakeModulus((UWORD *)m,&l4,AC.cmod,AC.ncmod,0) ) {
					LowerSortLevel(); goto PolyCall; }
				if ( l4 == 0 ) continue;
				t = t2 + *t2 - 1;
				l2 = REDLENG(*t);
				if ( MulRat(BHEAD (UWORD *)m,l4,(UWORD *)tt2,l2,(UWORD *)m,&l3) ) {
					LowerSortLevel(); goto PolyCall; }
				if ( AC.ncmod != 0 && TakeModulus((UWORD *)m,&l3,AC.cmod,AC.ncmod,0) ) {
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
				if ( AC.ncmod != 0 && TakeModulus((UWORD *)m,&l3,AC.cmod,AC.ncmod,0) ) {
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
	if ( EndSort(w,0) < 0 ) goto PolyCall;
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
	if ( n1*sizeof(WORD) > AM.MaxTer ) {
		LOCK(ErrorMessageLock);
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
	LOCK(ErrorMessageLock);
PolyCall2:;
	MesCall("PolyFunMul");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] PolyFunMul : 
	#] Processor :
*/
