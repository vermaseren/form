/*
  	#[ Includes : proces.c
*/

#include "form3.h"

#ifdef PARALLEL
#include "parallel.h"
#endif

WORD printscratch[2];

/*
  	#] Includes : 
	#[ Processor :
 		#[ Variables :
*/

LONG ninterms, deferskipped;
WORD *currentTerm;
WORD *listinprint;
WORD numlistinprint;

/*
 		#] Variables : 
 		#[ Processor :			WORD Processor()

	This is the central processor.
	It accepts a stream of Expressions in the stream indicated by AR.infile,
	which is accessed by a call to GetTerm.
	The definitions of an expression are seen as an id-statement, so the
	primary Expressions should be written to the system of scratch files
	as single terms with an expression pointer. Each expression is terminated
	with a zero and the whole is terminated by two zeroes.

	The routine DoExecute should determine whether results are to be
	printed, should revert the scratch I/O directions etc.

	Remark: The bug wrt AS.OldOnFile (had to be done first!) survived
	till 20-apr-1989. How can that be?

*/
 
WORD
Processor()
{
	WORD *term, *t, i, retval = 0;
	EXPRESSIONS e;
	POSITION position;
	WORD last, LastExpression;
	LONG dd = 0;
	CBUF *C = cbuf+AR.cbufnum;
	int firstterm;

#ifdef PARALLEL
	PF.module++;
#endif
	if ( NumExpressions == 0 ) return(0);
	AR.expflags = 0;
	AR.CompressPointer = AM.CompressBuffer;
	term = AR.WorkPointer;
	if ( ( AR.WorkPointer + AM.MaxTer ) > AM.WorkTop ) return(MesWork());
	UpdatePositions();
	C->rhs[C->numrhs+1] = C->Pointer;
	AR.KeptInHold = 0;
	if ( AC.CollectFun ) AR.DeferFlag = 0;
	for ( i = NumExpressions-1; i >= 0; i-- ) {
		e = Expressions+i;
		if ( e->status == LOCALEXPRESSION || e->status == GLOBALEXPRESSION
		|| e->status == HIDELEXPRESSION || e->status == HIDEGEXPRESSION
		|| e->status == SKIPLEXPRESSION || e->status == SKIPGEXPRESSION
		|| e->status == UNHIDELEXPRESSION || e->status == UNHIDEGEXPRESSION
		) break;
	}
	last = i;
	for ( i = 0; i < NumExpressions; i++ ) {
		AR.CollectOverFlag = 0;
		e = Expressions+i;
		e->vflags &= ~ISUNMODIFIED;
		AR.expchanged = 0;
		if ( i == last ) LastExpression = 1;
		else             LastExpression = 0;
		if ( e->inmem ) {	/* expression lies in memory! */
			WORD j;

			AR.GetFile = 0;
			SetScratch(AR.infile,&(e->onfile));
			if ( GetTerm(term) <= 0 ) {
				MesPrint("Expression %d has problems in scratchfile",i);
				retval = -1;
				break;
			}
			term[3] = i;
			AS.CurExpr = i;
			SeekScratch(AR.outfile,&position);
			e->onfile = position;
			if ( PutOut(term,&position,AR.outfile,0) < 0 ) goto ProcErr;
			AR.DeferFlag = AC.ComDefer;
			NewSort();
			ninterms = 0;
			t = e->inmem;
			while ( *t ) {
				for ( j = 0; j < *t; j++ ) term[j] = t[j];
				t += *t;
				ninterms++; dd = deferskipped;
				if ( AC.CollectFun && *term <= (AM.MaxTer>>1) ) {
					if ( GetMoreFromMem(term,&t) ) {
						LowerSortLevel(); goto ProcErr;
					}
				}
				AR.WorkPointer = term + *term;
				AR.RepPoint = AM.RepCount + 1;
				AR.CurDum = ReNumber(term);
				if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
				if ( Generator(term,0) ) {
					LowerSortLevel(); goto ProcErr;
				}
				ninterms += dd;
			}
			ninterms += dd;
			if ( EndSort(AM.S0->sBuffer,0) < 0 ) goto ProcErr;
			if ( AM.S0->TermsLeft ) e->vflags &= ~ISZERO;
			else e->vflags |= ISZERO;
			if ( AR.expchanged == 0 ) e->vflags |= ISUNMODIFIED;
				if ( AM.S0->TermsLeft ) AR.expflags |= ISZERO;
			if ( AR.expchanged ) AR.expflags |= ISUNMODIFIED;
			AR.GetFile = 0;
		}
		else {
			AS.CurExpr = i;
			switch ( e->status ) {
			case UNHIDELEXPRESSION:
			case UNHIDEGEXPRESSION:
				AR.GetFile = 2;
				SetScratch(AS.hidefile,&(e->onfile));
				goto commonread;
			case LOCALEXPRESSION:
			case GLOBALEXPRESSION:
				AR.GetFile = 0;

#ifdef PARALLEL
				if ( PF.me == MASTER ) SetScratch(AR.infile,&(e->onfile));
#else
				SetScratch(AR.infile,&(e->onfile));
#endif
commonread:		
#ifdef PARALLEL
				if ( ! PF_Processor(e,i,LastExpression) ) break;
				else{
				  MesPrint("Error in PF_Processor");
				  break;
				}
#endif
				if ( GetTerm(term) <= 0 ) {
				  MesPrint("Expression %d has problems in scratchfile",i);
				  retval = -1;
				  break;
				}
				if ( AC.bracketindexflag ) OpenBracketIndex(i);
				term[3] = i;
				SeekScratch(AR.outfile,&position);
				e->onfile = position;
				if ( PutOut(term,&position,AR.outfile,0) < 0 ) goto ProcErr;
				AR.DeferFlag = AC.ComDefer;
				NewSort();
				ninterms = 0;
				while ( GetTerm(term) ) {
				  ninterms++; dd = deferskipped;
				  if ( AC.CollectFun && *term <= (AM.MaxTer>>1) ) {
					if ( GetMoreTerms(term) ) {
					  LowerSortLevel(); goto ProcErr;
					}
				  }
				  AR.WorkPointer = term + *term;
				  AR.RepPoint = AM.RepCount + 1;
				  AR.CurDum = ReNumber(term);
				  if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
				  if ( Generator(term,0) ) {
					LowerSortLevel(); goto ProcErr;
				  }
				  ninterms += dd;
				}
				ninterms += dd;
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
				if ( AM.S0->TermsLeft ) e->vflags &= ~ISZERO;
				else e->vflags |= ISZERO;
				if ( AR.expchanged == 0 ) e->vflags |= ISUNMODIFIED;
				
				if ( AM.S0->TermsLeft ) AR.expflags |= ISZERO;
				if ( AR.expchanged ) AR.expflags |= ISUNMODIFIED;
				AR.GetFile = 0;
				break;
			case SKIPLEXPRESSION:
			case SKIPGEXPRESSION:
				AR.GetFile = 0;
				SetScratch(AR.infile,&(e->onfile));
				if ( GetTerm(term) <= 0 ) {
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
					WORD *comprtop = AM.ComprTop;
					AM.ComprTop = AM.S0->sTop;
					AR.CompressPointer = AM.S0->sBuffer;
					if ( firstterm > 0 ) {
						if ( PutOut(term,&position,AR.outfile,1) < 0 ) goto ProcErr;
					}
					else if ( firstterm < 0 ) {
						if ( PutOut(term,&position,AR.outfile,0) < 0 ) goto ProcErr;
						firstterm++;
					}
					else {
						if ( PutOut(term,&position,AR.outfile,-1) < 0 ) goto ProcErr;
						firstterm++;
					}
					AR.CompressPointer = oldipointer;
					AM.ComprTop = comprtop;
				} while ( GetTerm(term) );
				if ( FlushOut(&position,AR.outfile) ) goto ProcErr;
				break;
			case HIDELEXPRESSION:
			case HIDEGEXPRESSION:
				AR.GetFile = 0;
				SetScratch(AR.infile,&(e->onfile));
				if ( GetTerm(term) <= 0 ) {
					MesPrint("Expression %d has problems in scratchfile",i);
					retval = -1;
					break;
				}
				term[3] = i;
				AR.DeferFlag = 0;
				SetEndScratch(AS.hidefile,&position);
				e->onfile = position;
				*AM.S0->sBuffer = 0; firstterm = -1;
				do {
					WORD *oldipointer = AR.CompressPointer;
					WORD *comprtop = AM.ComprTop;
					AM.ComprTop = AM.S0->sTop;
					AR.CompressPointer = AM.S0->sBuffer;
					if ( firstterm > 0 ) {
						if ( PutOut(term,&position,AS.hidefile,1) < 0 ) goto ProcErr;
					}
					else if ( firstterm < 0 ) {
						if ( PutOut(term,&position,AS.hidefile,0) < 0 ) goto ProcErr;
						firstterm++;
					}
					else {
						if ( PutOut(term,&position,AS.hidefile,-1) < 0 ) goto ProcErr;
						firstterm++;
					}
					AR.CompressPointer = oldipointer;
					AM.ComprTop = comprtop;
				} while ( GetTerm(term) );
				if ( FlushOut(&position,AS.hidefile) ) goto ProcErr;
				AS.hidefile->POfull = AS.hidefile->POfill;
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
	AR.WorkPointer = term;
	return(retval);
ProcErr:
	AR.WorkPointer = term;
	if ( AM.tracebackflag ) MesCall("Processor");
	return(-1);
}
/*
 		#] Processor : 
 		#[ TestSub :			WORD TestSub(term,level)

	TestSub hunts for subexpression pointers.
	If one is found its power is given in AR.TeSuOut.
	and the returnvalue is 'expressionnumber'.
	If the expression number is negative it is an expression on disk.

	In addition this routine tries to locate subexpression pointers
	in functions. It also notices that action must be taken with any
	of the special functions.
*/

#ifdef WHICHSUBEXPRESSION

static UWORD *BinoScrat= 0;
WORD nbino, last1 = 0;
LONG last2 = 0, last3 = 0;

#endif

WORD
TestSub ARG2(WORD *,term,WORD,level)
{
	WORD *m, *t, *r, retvalue, funflag, j;
	WORD *stop, *t1, *t2, funnum, wilds, tbufnum = 0;
	NESTING n;
	CBUF *C = cbuf+AR.ebufnum;
	LONG isp, i = 0;
	TABLES T;
ReStart:
	AR.TMbuff = AM.rbufnum;
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

			WORD *tmin = t, nbino;
/*			LONG minval = MAXLONG; */
			LONG minval = -1;
			LONG mm, mnum1 = 1;
			if ( BinoScrat == 0 ) {
				BinoScrat = (UWORD *)Malloc1((AM.MaxTal+2)*sizeof(UWORD),"GetBinoScrat");
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
						That is why we had to allocate BinoScrat
*/
						if ( last1 == t[3] && last2 == cbuf[t[4]].NumTerms[t[2]] + t[3] - 1 ) {
							if ( last3 > minval ) {
								minval = last3; tmin = t;
							}
						}
						else {
						last1 = t[3]; mm = last2 = cbuf[t[4]].NumTerms[t[2]] + t[3] - 1;
						if ( t[3] == 1 ) {
							if ( mm > minval ) {
								minval = mm; tmin = t;
							}
						}
						else if ( t[3] > 0 ) {
							if ( mm > MAXPOSITIVE ) goto TooMuch;
							GetBinom(BinoScrat,&nbino,(WORD)mm,t[3]);
							if ( nbino > 2 ) goto TooMuch;
							if ( nbino == 2 ) {
								mm = BinoScrat[1];
								mm = ( mm << BITSINWORD ) + BinoScrat[0];
							}
							else if ( nbino == 1 ) mm = BinoScrat[0];
							else mm = 0;
							if ( mm > minval ) {
								minval = mm; tmin = t;
							}
						}
						last3 = mm;
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
					if ( last1 == t[3] && last2 == cbuf[t[4]].NumTerms[t[2]] + t[3] - 1 ) {
						if ( last3 > minval ) {
							minval = last3; tmin = t;
						}
					}
					else {
					last1 = t[3]; mm = last2 = cbuf[t[4]].NumTerms[t[2]] + t[3] - 1;
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
TooMuch:					MesPrint("Attempt to generate more terms than FORM can count");
							Terminate(-1);
						}
						GetBinom(BinoScrat,&nbino,(WORD)mm,t[3]);
						if ( nbino > 2 ) goto TooMuch;
						if ( nbino == 2 ) {
							mm = BinoScrat[1];
							mm = ( mm << BITSINWORD ) + BinoScrat[0];
						}
						else if ( nbino == 1 ) mm = BinoScrat[0];
						else mm = 0;
						if ( mm > minval ) {
							minval = mm; tmin = t;
						}
					}
					last3 = mm;
					}
				}
/*
currentTerm = term;
MesPrint("Choose %d; minval = %l, power = %d, mnum = %d-%d",tmin-term,minval,tmin[3],mnum1,mnum2);
*/
				t = tmin;
#endif
/*				AR.TePos = 0;  */
				AR.TePos = WORDDIF(t,term);
				AR.TMbuff = t[4];
				if ( t[3] < 0 ) {
					AR.TeInFun = 1;
					AR.TePos = WORDDIF(t,term);
					return(t[2]);
				}
				else {
					AR.TeInFun = 0;
					AR.TeSuOut = t[3];
				}
				if ( t[2] < 0 ) {
					AR.TeSuOut = -t[3];
					return(-t[2]);
				}
				return(t[2]);
			}
		}
		else if ( *t == EXPRESSION ) {
			i = -t[2] - 1;
			if ( t[3] < 0 ) {
				AR.TeInFun = 1;
				AR.TePos = WORDDIF(t,term);
				return(i);
			}
			AR.TeInFun = 0;
			AR.TePos = 0;
			AR.TeSuOut = t[3];
			AR.Frozen = 0;
			AR.TMaddr = m = AR.WorkPointer;
			j = t[1];
			AR.WorkPointer += j;
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
					AR.Frozen = m = AR.WorkPointer;
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
						*AR.WorkPointer = m-AR.WorkPointer;
						m = AR.WorkPointer;
						AR.WorkPointer = m + *m;
						NewSort();
						if ( Generator(m,cbuf[AM.rbufnum].numlhs) ) {
							LowerSortLevel(); goto EndTest;
						}
						if ( EndSort(m,0) < 0 ) goto EndTest;
						AR.Frozen = m;
						if ( *m == 0 ) {
							*m++ = 4; *m++ = 1; *m++ = 1; *m++ = 3;
						}
						else if ( m[*m] != 0 ) {
							MesPrint("Bracket specification in expression should be one single term");
							Terminate(-1);
						}
						else {
							m += *m;
							m -= ABS(m[-1]);
							*m++ = 1; *m++ = 1; *m++ = 3;
							*AR.Frozen = m - AR.Frozen;
						}
					}
					else {
						while ( t < tttstop ) *m++ = *t++;
						*AR.WorkPointer = m-AR.WorkPointer;
						m = AR.WorkPointer;
						AR.WorkPointer = m + *m;
						if ( Normalize(m) ) {
							MesPrint("Error while picking up contents of bracket");
							Terminate(-1);
						}
						if ( !*m ) {
							*m++ = 4; *m++ = 1; *m++ = 1; *m++ = 3;
						}
						else m += *m;
					}
					AR.WorkPointer = m;
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
				if ( *r == -SNUMBER && r[1] != 0 &&
				t[FUNHEAD] > -FUNCTION && ( t[FUNHEAD] != -SNUMBER
				|| t[FUNHEAD+1] != 0 ) && t[FUNHEAD] != ARGHEAD ) {

					/* Note that the cases ^0 and 0^ are treated in Normalize */

					t1 = AddRHS(AR.ebufnum,1);
					m = t + FUNHEAD;
					if ( *m > 0 ) {
						m += ARGHEAD;
						i = t[FUNHEAD] - ARGHEAD;
						while ( (t1 + i + 10) > C->Top )
							t1 = DoubleCbuffer(AR.ebufnum,t1);
						while ( --i >= 0 ) *t1++ = *m++;
					}
					else {
						if ( (t1 + 20) > C->Top )
							t1 = DoubleCbuffer(AR.ebufnum,t1);
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
					*t++ = AR.ebufnum;
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
						m = AddRHS(AR.ebufnum,1);
						if ( *r > 0 ) {
							i = *r - ARGHEAD;
							r += ARGHEAD;
							while ( (m + i + 10) > C->Top )
								m = DoubleCbuffer(AR.ebufnum,m);
							while ( --i >= 0 ) *m++ = *r++;
						}
						else {
							while ( (m + 20) > C->Top )
								m = DoubleCbuffer(AR.ebufnum,m);
							ToGeneral(r,m,1);
							m += *m;
						}
						*m++ = 0;
						C->rhs[C->numrhs+1] = m;
						C->Pointer = m;
						m = AR.TMout;
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
							r++; *t++ = *r++; *t++ = AR.ebufnum; r++;
							while ( --i >= 0 ) *t++ = *r++;
						}
						else {
							*t++ = SUBEXPRESSION;
							*t++ = 4+SUBEXPSIZE;
							*t++ = C->numrhs;
							*t++ = 1;
							*t++ = AR.ebufnum;
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
						AR.TeInFun = -C->numrhs;
						AR.TePos = 0;
						AR.TeSuOut = 0;
						AR.TMbuff = AR.ebufnum;
						return(C->numrhs);
					}
				}
			}
			if ( *t < FUNCTION + WILDOFFSET ) {
				if ( functions[funnum-FUNCTION].spec == 0
				|| ( t[2] & DIRTYFLAG ) != 0 ) funflag = 1;
			}
			else if ( *t >= FUNCTION + WILDOFFSET ) {
				if ( functions[funnum-FUNCTION-WILDOFFSET].spec == 0
				|| ( t[2] & DIRTYFLAG ) ) funflag = 1;
			}
			if ( *t <= MAXBUILTINFUNCTION ) {
			if ( *t == THETA || *t == THETA2 ) {
				WORD *tstop, *t2, kk;
				tstop = t + t[1];
				t2 = t + FUNHEAD;
				while ( t2 < tstop ) {
					if ( *t2 > 0 && t2[1] != 0 ) {
/*						funflag = 2; */
						goto DoSpec;
					}
					NEXTARG(t2)
				}
				if ( !AR.RecFlag ) {
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
				WORD *tstop, *t2, kk;
				tstop = t + t[1];
				t2 = t + FUNHEAD;
				while ( t2 < tstop ) {
					if ( *t2 > 0 && t2[1] != 0 ) {
/*						funflag = 2; */
						goto DoSpec;
					}
					NEXTARG(t2)
				}
				if ( !AR.RecFlag ) {
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
				AR.TeInFun = -1;
				AR.TeSuOut = 0;
				AR.TePos = -1;
				return(1);
			}
			else if ( *t == DELTA3 && ((t[1]-FUNHEAD) & 1 ) == 0 ) {
				AR.TeInFun = -2;
				AR.TeSuOut = 0;
				AR.TePos = -1;
				return(1);
			}
			else if ( *t == TABLEFUNCTION && t[FUNHEAD] <= -FUNCTION
			&& ( T = functions[-t[FUNHEAD]-FUNCTION].tabl ) != 0
			&& t[1] >= FUNHEAD+1+2*T->numind ) {
				for ( isp = 0; isp < T->numind; isp++ ) {
					if ( t[FUNHEAD+1+2*isp] != -SYMBOL ) break;
				}
				if ( isp >= T->numind ) {
					AR.TeInFun = -3;
					AR.TeSuOut = 0;
					AR.TePos = -1;
					return(1);
				}
			}
			else if ( *t == AM.polyfunnum ) {
				AR.TeInFun = -4;
				AR.TeSuOut = 0;
				AR.TePos = -1;
				return(1);
			}
			}
		}
		t += t[1];
	} while ( t < m );
	if ( funflag ) {	/* Search in functions */
DoSpec:
		t = term;
		AR.NestPoin->termsize = t;
		if ( AR.NestPoin == AR.Nest ) AR.EndNest = t + *t;
		t++;
		if ( t < m ) do {
			if ( *t < FUNCTION ) {
				t += t[1]; continue;
			}
			r = t + t[1];
			funnum = *t;
			if ( *t >= FUNCTION + WILDOFFSET ) funnum -= WILDOFFSET;
			if ( functions[funnum-FUNCTION].spec == 0 ) {
				AR.NestPoin->funsize = t + 1;
				t1 = t;
				t += FUNHEAD;
				while ( t < r ) {	/* Sum over arguments */
					if ( *t > 0 && t[1] ) {	/* Argument is dirty  */
						AR.NestPoin->argsize = t;
						AR.NestPoin++;
/*						stop = t + *t; */
/*						t2 = t; */
						t += ARGHEAD;
						while ( t < AR.NestPoin[-1].argsize+*(AR.NestPoin[-1].argsize) ) {
											/* Sum over terms */
							AR.RecFlag++;
/*							i = *t; */
							if ( ( retvalue = TestSub(t,level) ) != 0 ) {
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
								AR.RecFlag--;
								AR.NestPoin--;
								AR.TeInFun++;
								AR.TePos = 0;
								return(retvalue);
							}
							AR.RecFlag--;
							t += *t;
						}
						AR.NestPoin--;
/*
						Argument contains no subexpressions.
						It should be normalized and sorted.
						The main problem is the storage.
*/
						t = AR.NestPoin->argsize;
						j = *t;
						t += ARGHEAD;
						NewSort();
						if ( AR.WorkPointer < term + *term )
							AR.WorkPointer = term + *term;

						while ( t < AR.NestPoin->argsize+*(AR.NestPoin->argsize) ) {
							m = AR.WorkPointer;
							r = t + *t;
							do { *m++ = *t++; } while ( t < r );
							r = AR.WorkPointer;
							AR.WorkPointer = r + *r;
							if ( Normalize(r) ) {
								LowerSortLevel(); goto EndTest;
							}
							if ( AC.ncmod != 0 ) {
								if ( *r ) {
									if ( Modulus(r) ) {
										LowerSortLevel();
										AR.WorkPointer = r;
										goto EndTest;
									}
									if ( !*r ) {
										LowerSortLevel();
										AR.WorkPointer = r;
										return(0);
									}
								}
							}
							if ( *r ) StoreTerm(r);
							AR.WorkPointer = r;
						}

						if ( EndSort(AR.WorkPointer+ARGHEAD,0) < 0 ) goto EndTest;
						m = AR.WorkPointer+ARGHEAD;

						while ( *m ) m += *m;
						i = WORDDIF(m,AR.WorkPointer);
						*AR.WorkPointer = i;
						AR.WorkPointer[1] = 0;
						if ( ToFast(AR.WorkPointer,AR.WorkPointer) ) {
							m = AR.WorkPointer;
							if ( *m <= -FUNCTION ) { m++; i = 1; }
							else { m += 2; i = 2; }
						}
						j = i - j;
						if ( j > 0 ) {
							r = m + j;
							if ( r > AM.WorkTop ) {
								MesWork();
								goto EndTest;
							}
							do { *--r = *--m; } while ( m > AR.WorkPointer );
							AR.WorkPointer = r;
							m = AR.EndNest;
							r = m + j;
							stop = AR.NestPoin->argsize+*(AR.NestPoin->argsize);
							do { *--r = *--m; } while ( m >= stop );
						}
						else if ( j < 0 ) {
							m = AR.NestPoin->argsize+*(AR.NestPoin->argsize);
							r = m + j;
							do { *r++ = *m++; } while ( m < AR.EndNest );
						}
						m = AR.NestPoin->argsize;
						r = AR.WorkPointer;
						while ( --i >= 0 ) *m++ = *r++;
						n = AR.Nest;
						while ( n <= AR.NestPoin ) {
							if ( *(n->argsize) > 0 && n != AR.NestPoin )
								*(n->argsize) += j;
							*(n->funsize) += j;
							*(n->termsize) += j;
							n++;
						}
						AR.EndNest += j;
/*						(AR.NestPoin->argsize)[1] = 0;  */
						if ( funnum == DENOMINATOR || funnum == EXPONENT ) {
							if ( Normalize(term) ) goto EndTest;
/*
							And size changes here?????
*/
						}
						goto ReStart;
					}
					else if ( *t == -DOLLAREXPRESSION ) {
						if ( *t1 == TERMSINEXPR && t1[1] == FUNHEAD+2 ) {}
						else {
							if ( AC.Eside != LHSIDE ) {
								AR.TeInFun = 1; AR.TePos = 0;
								AR.TMbuff = AM.dbufnum; t1[2] |= DIRTYFLAG;
								return(1);
							}
							AR.lhdollarflag = 1;
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
					WORD rhsnumber, *oldwork = AR.WorkPointer;
					WORD ii, *p;
					MINMAX *mm;
					T = functions[funnum-FUNCTION].tabl;
					p = T->pattern + FUNHEAD+1;
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
#if ( TABLEXTENSION == 2 )
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
									MesPrint("Table boundary check. Argument %d",
									T->numind-j);
showtable:							AO.OutFill = AO.OutputLine = (UBYTE *)m;
									AO.OutSkip = 8;
									IniLine();
									WriteSubTerm(t1,1);
									FiniLine();
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
							MesPrint("Element in table is undefined");
							goto showtable;
						}
						rhsnumber = T->tablepointers[i];
#if ( TABLEXTENSION == 2 )
						tbufnum = T->bufum;
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
					AR.FullProto = T->prototype;
					AN.WildValue = AR.FullProto + SUBEXPSIZE;
					AN.WildStop = AR.FullProto+AR.FullProto[1];
					ClearWild();
					AN.RepFunNum = 0;
					AN.RepFunList = AR.EndNest;
					AR.WorkPointer = AR.EndNest + (AM.MaxTer >> 1);
					if ( AR.WorkPointer >= AM.WorkTop ) { MesWork(); }
					wilds = 0;
					if ( MatchFunction(T->pattern,t1,&wilds) > 0 ) {
						AR.WorkPointer = oldwork;
						if ( AR.NestPoin != AR.Nest ) return(1);

						m = T->prototype;
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
						AR.TeInFun = 0;
						AR.TePos = 0;
						AR.TeSuOut = -1;
						if ( AR.WorkPointer < term + *term ) AR.WorkPointer = term + *term;
						AR.TMbuff = tbufnum;
						return(retvalue);
					}
					AR.WorkPointer = oldwork;
				}
NextFun:;
			}
			else if ( ( t[2] & DIRTYFLAG ) != 0 ) {
				t += FUNHEAD;
				while ( t < r ) {
					if ( *t == FUNNYDOLLAR ) {
						if ( AC.Eside != LHSIDE ) {
							AR.TeInFun = 1;
							AR.TePos = 0;
							AR.TMbuff = AM.dbufnum;
							return(1);
						}
						AR.lhdollarflag = 1;
					}
					t++;
				}
			}
			t = r;
		} while ( t < m );
	}
	return(0);
EndTest:
	MesCall("TestSub");
	SETERROR(-1)
}

/*
 		#] TestSub : 
 		#[ InFunction :			WORD InFunction(term,termout)

		Makes the replacement of 'replac' in a function argument.

*/

WORD
InFunction ARG2(WORD *,term,WORD *,termout)
{
	WORD *m, *t, *r, sign = 1;
	WORD *u, *v, *w, *from, *to, 
		ipp, olddefer = AR.DeferFlag, oldPolyFun = AC.PolyFun, i;
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
					AC.PolyFun = 0;
					NewSort();
					while ( t < v ) {
						i = *t;
						NCOPY(m,t,i);
						m = to;
                        if ( AR.WorkPointer < m+*m ) AR.WorkPointer = m + *m;
						if ( Generator(m,cbuf[AM.rbufnum].numlhs) ) {
							LowerSortLevel(); goto InFunc;
						}
					}
					/* w = the function */
					/* v = the next argument */
					/* u = the function */
					/* to is new argument */

					to -= ARGHEAD;
					if ( EndSort(m,0) < 0 ) goto InFunc;
					AC.PolyFun = oldPolyFun;
					while ( *m ) m += *m;
					*to = WORDDIF(m,to);
					to[1] = 0;
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
					if ( AC.Eside == LHSIDE ) {
						NEXTARG(t)
						AR.lhdollarflag = 1;
					}
					else {
					DOLLARS d = Dollars + t[1];

					/* This whole argument must be redone */

					AR.DeferFlag = 0;
					v = t + 2;
					w = 0;	/* to appease the compilers warning devices */
					while ( from < t ) {
						if ( from == u ) w = m;
						*m++ = *from++;
					}
					to = m;
					AC.PolyFun = 0;
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
							MesPrint("!!!Undefined $-variable: $%s!!!",
							AC.dollarnames->namebuffer+d->name);
							Terminate(-1);
					}
					AC.PolyFun = oldPolyFun;
					r = term + *term;
					t = v;
					while ( t < r ) *m++ = *t++;
					*termout = WORDDIF(m,termout);
					AR.DeferFlag = olddefer;
					return(0);
				}
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
					if ( AC.Eside != LHSIDE ) {
					DOLLARS d = Dollars + t[1];
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
wrongtype:						MesPrint("$%s has wrong type for tensor substitution",
								AC.dollarnames->namebuffer+d->name);
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
							MesPrint("$%s is undefined in tensor substitution",
							AC.dollarnames->namebuffer+d->name);
							return(-1);
					}
					w[1] = w[1] - 2 + (m-to);
					from += 2;
					term += *term;
					while ( from < term ) *m++ = *from++;
					if ( sign < 0 ) m[-1] = -m[-1];
					*termout = m - termout;
					return(0);
				}
				else {
					AR.lhdollarflag = 1;
				}
				}
				t++;
			}
			t = u;
		}
		t += t[1];
	}
	return(1);

InFunc:
	MesCall("InFunction");
	SETERROR(-1)
}
 		
/*
 		#] InFunction : 
 		#[ InsertTerm :			WORD InsertTerm(term,replac,extractbuff,position,termout)

		Puts the terms 'term' and 'position' together into a single
		legal term in termout. replac is the number of the subexpression
		that should be replaced. It must be a positive term.
		When action is needed in the argument of a function all terms
		in that argument are dealt with recursively. The subexpression
		is sorted. Only one subexpression is done at a time this way.

*/

WORD
InsertTerm ARG6(WORD *,term,WORD,replac,WORD,extractbuff,WORD *,position,WORD *,termout,WORD,tepos)
{
	WORD *m, *t, *r, i, l2, j;
	WORD *u, *v, l1, *coef;
	coef = AR.WorkPointer;
	if ( ( AR.WorkPointer = coef + 2*AM.MaxTal ) > AM.WorkTop ) return(MesWork());
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
				if ( ( l2 = WildFill(m,position,t) ) < 0 ) goto InsCall;
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
				if ( MulRat((UWORD *)u,REDLENG(l1),(UWORD *)r,REDLENG(l2),
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
			if (*termout > AM.MaxTer ) goto InsCall;
			AR.WorkPointer = coef;
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
	MesCall("InsertTerm");
	SETERROR(-1)
}

/*
 		#] InsertTerm : 
 		#[ PasteFile :			WORD PasteFile(num,acc,pos,accf,renum,freeze,nexpr)

		Gets a term from stored expression expr and puts it in
		the accumulator at position number. It returns the length of the
		term that came from file.

*/

LONG
PasteFile ARG7(WORD,number,WORD *,accum,POSITION *,position,WORD **,accfill
			  ,RENUMBER,renumber,WORD *,freeze,WORD,nexpr)
{
	WORD *r, l, *m, i;
	WORD *stop, *s1, *s2;
	POSITION AccPos;
	WORD InCompState, retlength;
	WORD *oldipointer;
	stop = accum + 2*AM.MaxTer;
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
			ADDPOS(AccPos,InCompState * wsizeof(WORD));
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
				m = AR.WorkPointer;
				s2 = r;
				r = accum;
				*m++ = WORDDIF(s2,r) + 3;
				r++;
				while ( r < s2 ) *m++ = *r++;
				*m++ = 1; *m++ = 1; *m++ = 3;
				if ( Normalize(AR.WorkPointer) ) goto PasErr;
				m = AR.WorkPointer;
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
			MesCall("PasteFile");
			SETERROR(-1)
		}
		if ( l == 0 ) { *accum = 0; return(0); }
		retlength = InCompState;
	}
	accum += l;
	if ( accum > stop ) {
		MesPrint("Buffer too small in PasteFile");
		SETERROR(-1)
	}
	*accum = 0;
	*accfill = accum;
	return(retlength);
PasErr:
	MesCall("PasteFile");
	SETERROR(-1)
}
 		
/*
 		#] PasteFile : 
 		#[ PasteTerm :			WORD PasteTerm(number,accum,position,times,divby)

		Puts the term at position in the accumulator accum at position
		'number+1'. if times > 0 the coefficient of this term is
		multiplied by times/divby.

*/

WORD *
PasteTerm ARG5(WORD,number,WORD *,accum,WORD *,position,WORD,times,WORD,divby)
{
	WORD *t, *r, x, y, z;
	WORD *m, *u, l1, a[2];
	m = accum + 2*AM.MaxTer;
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
		if ( MulRat((UWORD *)t,REDLENG(l1),(UWORD *)a,1,(UWORD *)accum,&l1) ) {
			MesCall("PasteTerm");
			return(0);
		}
		x = l1;
		x <<= 1;
		if ( x < 0 ) { accum -= x; *accum++ = x - 1; }
		else		 { accum += x; *accum++ = x + 1; }
		*u = WORDDIF(accum,u);
	}
	if ( accum >= m ) {
		MesPrint("Buffer too small in PasteTerm"); return(0);
	}
	*accum = 0;
	return(accum);
}

/*
 		#] PasteTerm : 
 		#[ FiniTerm :			WORD FiniTerm(term,accum,termout,number)

		Concatenates the contents of the accumulator into a single
		legal term, which replaces the subexpression pointer

*/

WORD
FiniTerm ARG5(WORD *,term,WORD *,accum,WORD *,termout,WORD,number,WORD,tepos)
{
	WORD *m, *t, *r, i, numacc, l2, ipp;
	WORD *u, *v, l1, *coef = AR.WorkPointer, *oldaccum;
	if ( ( AR.WorkPointer = coef + 2*AM.MaxTal ) > AM.WorkTop ) return(MesWork());
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
							if ( ( l2 = WildFill(m,accum,r) ) < 0 ) goto FiniCall;
							goto AllWild;
						}
						r += r[1];
					}
					goto NoWild;
				}
				else if ( t[1] > SUBEXPSIZE && t[SUBEXPSIZE] != FROMBRAC ) {
					i = *--m;
					if ( ( l2 = WildFill(m,accum,t) ) < 0 ) goto FiniCall;
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
					if ( MulRat((UWORD *)coef,l1,(UWORD *)r,l2,(UWORD *)coef,&l1) ) goto FiniCall;
					if ( AC.ncmod != 0 && TakeModulus((UWORD *)coef,&l1,0) ) goto FiniCall;
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
			AR.WorkPointer = coef;
			return(0);
		}
		t += t[1];
	} while ( t < m );
	AR.WorkPointer = coef;
	return(1);

FiniCall:
	MesCall("FiniTerm");
	SETERROR(-1)
}

/*
 		#] FiniTerm : 
 		#[ Generator :			WORD Generator(term,level)

		The heart of the program
		Here the expansion tree is set up in one giant recursion
*/

WORD
Generator ARG2(WORD *,term,WORD,level)
{
	WORD replac, *accum, *termout, *t, i, j, tepos, applyflag = 0;
	WORD *a, power, power1, DumNow, oldtoprhs, retnorm, extractbuff;
	int *RepSto = AR.RepPoint, oldcbufnum = AR.cbufnum;
	CBUF *C = cbuf+AM.rbufnum, *CC = cbuf + AR.ebufnum;
	LONG posisub, oldcpointer;
	oldtoprhs = CC->numrhs;
	oldcpointer = CC->Pointer - CC->Buffer;

ReStart:
	if ( ( replac = TestSub(term,level) ) == 0 ) {
		if ( applyflag ) { TableReset(); applyflag = 0; }
Renormalize:
		if ( ( retnorm = Normalize(term) ) != 0 ) {
			if ( retnorm > 0 ) goto ReStart;
			goto GenCall;
		}
		if ( !*term ) goto Return0;
		if ( AR.PolyNorm ) {
			if ( PolyMul(term) ) goto GenCall;
			if ( !*term ) goto Return0;
		}
		if ( AR.WorkPointer < term + AM.MaxTer ) AR.WorkPointer = term + AM.MaxTer;
		do {
SkipCount:	level++;
			if ( level > C->numlhs ) {
				if ( AR.DeferFlag ) {
#ifdef PARALLEL
				  if ( PF.me != MASTER && AC.parallelflag == PARALLELFLAG ){
					if ( PF_Deferred(term,level) ) goto GenCall;
				  }
				  else
#endif
					if ( Deferred(term,level) ) goto GenCall;
					goto Return0;
				}
				if ( AC.ncmod != 0 ) {
					if ( Modulus(term) ) goto GenCall;
					if ( !*term ) goto Return0;
				}
				if ( AR.CurDum > AM.IndDum && AR.sLevel <= 0 ) {
					ReNumber(term); Normalize(term);
				}
				if ( AC.PolyFun > 0 && AR.sLevel <= 0 ) {
					if ( PrepPoly(term) != 0 ) goto Return0;
				}
				if ( AR.sLevel <= 0 && AR.BracketOn ) {
					if ( AR.WorkPointer < term + *term ) AR.WorkPointer = term + *term;
					termout = AR.WorkPointer;
					if ( AR.WorkPointer + *term + 3 > AM.WorkTop ) goto OverWork;
					if ( PutBracket(term) ) return(-1);
					AR.RepPoint = RepSto;
					*AR.WorkPointer = 0;
					i = StoreTerm(termout);
					AR.WorkPointer = termout;
					AR.cbufnum = oldcbufnum;
					CC->numrhs = oldtoprhs;
					CC->Pointer = CC->Buffer + oldcpointer;
					return(i);
				}
				else {
					if ( AR.WorkPointer < term + *term ) AR.WorkPointer = term + *term;
					*AR.WorkPointer = 0;
					AR.RepPoint = RepSto;
					i = StoreTerm(term);
					AR.cbufnum = oldcbufnum;
					CC->numrhs = oldtoprhs;
					CC->Pointer = CC->Buffer + oldcpointer;
					return(i);
				}
			}
			i = C->lhs[level][0];
			if ( i >= TYPECOUNT ) {
				switch ( i ) {
				  case TYPECOUNT:
					if ( CountDo(term,C->lhs[level]) < C->lhs[level][2] ) {
						AR.WorkPointer = term + *term;
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
					AR.WorkPointer = term + *term;
					goto Return0;
				  case TYPEIF:
					while ( !DoIfStatement(C->lhs[level],term) ) {
						level = C->lhs[level][2];
						if ( C->lhs[level][0] != TYPEELIF ) break;
					}
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
						WORD *t = C->lhs[level] + 3, *m, j;
						WORD theindex = C->lhs[level][2];
						if ( theindex < 0 ) {	/* $-variable */
							theindex = -theindex;
							if ( Dollars[theindex].type != DOLINDEX
							|| Dollars[theindex].index < AM.OffsetIndex
							|| Dollars[theindex].index >= AM.OffsetIndex + WILDOFFSET ) {
								MesPrint("$%s should have been an index"
								,AC.dollarnames->namebuffer+Dollars[theindex].name);
								currentTerm = term;
								MesPrint("Current term: %t");
								listinprint = printscratch;
								printscratch[0] = DOLLAREXPRESSION;
								printscratch[1] = theindex;
								MesPrint("$%s = %$"
								,AC.dollarnames->namebuffer+Dollars[theindex].name);
								goto GenCall;
							}
							theindex = Dollars[theindex].index;
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
							cp[-1] = *t++;
							termout = AR.WorkPointer;
							if ( ( j = WildFill(termout,term,op)) < 0 )
								goto GenCall;
							m = term;
							j = *m;
							while ( --j >= 0 ) {
								if ( *m++ != *termout++ ) break;
							}
							if ( j >= 0 ) {
								termout = AR.WorkPointer;
								AR.WorkPointer = termout + *termout;
								if ( Generator(termout,level) ) goto GenCall;
								AR.WorkPointer = termout;
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
								theindex = -theindex;
								if ( Dollars[theindex].type != DOLINDEX
								|| Dollars[theindex].index < AM.OffsetIndex
								|| Dollars[theindex].index >= AM.OffsetIndex + WILDOFFSET ) {
									MesPrint("$%s should have been an index"
									,AC.dollarnames->namebuffer+Dollars[theindex].name);
									currentTerm = term;
									MesPrint("Current term: %t");
									listinprint = printscratch;
									printscratch[0] = DOLLAREXPRESSION;
									printscratch[1] = theindex;
									MesPrint("$%s = %$"
									,AC.dollarnames->namebuffer+Dollars[theindex].name);
									goto GenCall;
								}
								theindex = Dollars[theindex].index;
							}
							*cp++ = INDTOIND;
							*cp++ = 4;
							*cp++ = theindex;
							*cp++ = ++AR.CurDum;
						}
						AR.CompressPointer = cp;
						if ( WildFill(term,term,op) < 0 ) goto GenCall;
						AR.CompressPointer = op;
						ReNumber(term);
						goto Renormalize;
					}
				  case TYPECHISHOLM:
					if ( Chisholm(term,level) ) goto GenCall;
CommonEnd:
					AR.WorkPointer = term + *term;
					goto Return0;
				  case TYPEARG:
					if ( ( i = execarg(term,level) ) < 0 ) goto GenCall;
					level = C->lhs[level][2];
					if ( i > 0 ) goto ReStart;
					break;
				  case TYPENORM:
				  case TYPENORM2:
				  case TYPENORM3:
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
					MesPrint("%s",C->lhs[level]+3);
					goto GenCall;
				  case TYPESETEXIT:
					AR.exitflag = 1;
					break;
				  case TYPEPRINT:
					currentTerm = term;
					numlistinprint = (C->lhs[level][1] - C->lhs[level][2] - 3)/2;
					listinprint = C->lhs[level]+3+C->lhs[level][2];
					MesPrint((char *)(C->lhs[level]+3));
					break;
				  case TYPEFPRINT:
					{
					int oldFOflag = AM.FileOnlyFlag;
					WORD oldPrintType = AO.PrintType;
					if ( AC.LogHandle >= 0 ) {
						AM.FileOnlyFlag = 1;
						AO.PrintType |= PRINTLFILE;
					}
					currentTerm = term;
					numlistinprint = (C->lhs[level][1] - C->lhs[level][2] - 3)/2;
					listinprint = C->lhs[level]+3+C->lhs[level][2];
					MesPrint((char *)(C->lhs[level]+3));
					AO.PrintType = oldPrintType;
					AM.FileOnlyFlag = oldFOflag;
					}
					break;
				  case TYPEREDEFPRE:
					j = C->lhs[level][2];
#ifdef PARALLEL
					PF.redef[j] = PF.ginterms;
#endif
					PutPreVar(PreVar[j].name,(UBYTE *)(C->lhs[level]+4),0,1);
					break;
				  case TYPERENUMBER:
					AR.WorkPointer = term + *term;
					if ( FullRenumber(term,C->lhs[level][2]) ) goto GenCall;
					AR.WorkPointer = term + *term;
					if ( *term == 0 ) goto Return0;
					break;
				  case TYPETRY:
					if ( TryDo(term,C->lhs[level],level) ) goto GenCall;
					AR.WorkPointer = term + *term;
					goto Return0;
				  case TYPEASSIGN:
					{ WORD onc = AR.NoCompress;
/*
					Here we have to assign an expression to a $ variable.
*/
					AR.NoCompress = 1;
					AR.cTerm = currentTerm = term;
					AR.WorkPointer = term + *term;
					*AR.WorkPointer++ = 0;
					if ( AssignDollar(term,level) ) goto GenCall;
					AR.WorkPointer = term + *term;
					AR.cTerm = 0;
					AR.NoCompress = onc;
					break;
					}
				  case TYPEFINDLOOP:
					if ( Lus(term,C->lhs[level][3],C->lhs[level][4],
					C->lhs[level][5],C->lhs[level][6],C->lhs[level][2]) ) {
						AR.WorkPointer = term + *term;
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
					AR.WorkPointer = term + *term;
					AR.CurDum = DetCurDum(term);
					break;
				  case TYPEINEXPRESSION:
					{WORD *ll = C->lhs[level];
					int numexprs = (int)(ll[1]-3);
					ll += 3;
					while ( numexprs-- >= 0 ) {
						if ( *ll == AS.CurExpr ) break;
						ll++;
					}
					if ( numexprs < 0 ) level = C->lhs[level][2];
					}
					break;
				  case TYPEMERGE:
					AR.WorkPointer = term + *term;
					if ( DoMerge(term,level,C->lhs[level][2],C->lhs[level][3]) )
						goto GenCall;
					AR.WorkPointer = term + *term;
					goto Return0;
				  case TYPETESTUSE:
					AR.WorkPointer = term + *term;
					if ( TestUse(term,level) ) goto GenCall;
					AR.WorkPointer = term + *term;
					break;
				  case TYPEAPPLY:
					AR.WorkPointer = term + *term;
					if ( ApplyExec(term,C->lhs[level][2],level) < C->lhs[level][2] ) {
						AR.WorkPointer = term + *term;
						*AR.RepPoint = 1;
						goto ReStart;
					}
					AR.WorkPointer = term + *term;
/*
				  case TYPEAPPLYRESET:
					AR.WorkPointer = term + *term;
					if ( ApplyReset(term,level) ) goto GenCall;
					AR.WorkPointer = term + *term;
					break;
*/
				  case TYPEMODULUSGCD:
					AR.WorkPointer = term + *term;
					if ( ModulusGCD1(C->lhs[level][2],C->lhs[level][4],
					C->lhs[level][5],term,C->lhs[level][3]) ) goto GenCall;
					break;
				  case TYPECHAININ:
					AR.WorkPointer = term + *term;
					if ( ChainIn(term,level,C->lhs[level][2]) ) goto GenCall;
					AR.WorkPointer = term + *term;
					break;
				  case TYPECHAINOUT:
					AR.WorkPointer = term + *term;
					if ( ChainOut(term,level,C->lhs[level][2]) ) goto GenCall;
					AR.WorkPointer = term + *term;
					break;
				}
				goto SkipCount;
			}
		} while ( ( i = TestMatch(term,&level) ) == 0 );
		if ( AR.WorkPointer < term + *term ) AR.WorkPointer = term + *term;
		if ( i > 0 ) replac = TestSub(term,level);
		else replac = i;
		if ( replac >= 0 || AR.TMout[1] != SYMMETRIZE ) {
			*AR.RepPoint = 1;
			AR.expchanged = 1;
		}
		if ( replac < 0 ) {		/* Terms come from automatic generation */
AutoGen:	i = *AR.TMout;
			t = termout = AR.WorkPointer;
			if ( ( AR.WorkPointer += i ) > AM.WorkTop ) goto OverWork;
			accum = AR.TMout;
			while ( --i >= 0 ) *t++ = *accum++;
			if ( (*(FG.Operation[termout[1]]))(term,termout,replac,level) ) goto GenCall;
			AR.WorkPointer = termout;
			goto Return0;
		}
	}
	if ( applyflag ) { TableReset(); applyflag = 0; }
	DumNow = AR.CurDum;

	if ( AR.TeInFun ) {	/* Match in function argument */
		if ( AR.TeInFun < 0 && !AR.TeSuOut ) {
			if ( AR.TePos >= 0 ) goto AutoGen;
			if ( AR.TeInFun == -1 && DoDistrib(term,level) ) goto GenCall;
			else if ( AR.TeInFun == -2 && DoDelta3(term,level) ) goto GenCall;
			else if ( AR.TeInFun == -3 && DoTableExpansion(term,level) ) goto GenCall;
			else if ( AR.TeInFun == -4 && DoPolynomial(term,level) ) goto GenCall;
		}
		else {
			termout = AR.WorkPointer;
			if ( ( AR.WorkPointer += AM.MaxTer ) > AM.WorkTop ) goto OverWork;
			if ( InFunction(term,termout) ) goto GenCall;
			AR.WorkPointer = termout + *termout;
			*AR.RepPoint = 1;
			AR.expchanged = 1;
			if ( *termout && Generator(termout,level) < 0 ) goto GenCall;
			AR.WorkPointer = termout;
		}
	}
	else if ( replac > 0 ) {
		power = AR.TeSuOut;
		tepos = AR.TePos;
		if ( power < 0 ) {	/* Table expansion */
			power = -power; tepos = 0;
		}
		extractbuff = AR.TMbuff;
		posisub = cbuf[extractbuff].rhs[replac] - cbuf[extractbuff].Buffer;
		i = (WORD)cbuf[extractbuff].CanCommu[replac];
		
		if ( power == 1 ) {		/* Just a single power */
			termout = AR.WorkPointer;
			if ( ( AR.WorkPointer += AM.MaxTer ) > AM.WorkTop ) goto OverWork;
			while ( cbuf[extractbuff].Buffer[posisub] ) {
				AR.WorkPointer = termout + AM.MaxTer;
				if ( InsertTerm(term,replac,extractbuff,
					&(cbuf[extractbuff].Buffer[posisub]),termout,tepos) < 0 ) goto GenCall;
				AR.WorkPointer = termout + *termout;
				*AR.RepPoint = 1;
				AR.expchanged = 1;
				posisub += cbuf[extractbuff].Buffer[posisub];
				if ( Generator(termout,level) < 0 ) goto GenCall;
			}
			AR.WorkPointer = termout;
		}
		else if ( i <= 1 ) {		/* Use binomials */
			LONG posit, olw;
			WORD *same, *ow = AR.WorkPointer;
			LONG olpw = AR.posWorkPointer;
			power1 = power+1;
			WantAddLongs(power1);
			olw = posit = AR.lWorkPointer; AR.lWorkPointer += power1;
			same = ++AR.WorkPointer;
			a = accum = ( AR.WorkPointer += power1+1 );
			if ( ( AR.WorkPointer += 2 * AM.MaxTer ) > AM.WorkTop ) goto OverWork;
			AR.lWorkSpace[posit] = posisub;
			same[-1] = 0;
			*same = 1;
			*accum = 0;
			tepos = AR.TePos;
			i = 1;
			do {
				if ( cbuf[extractbuff].Buffer[AR.lWorkSpace[posit]] ) {
					if ( ( a = PasteTerm(i-1,accum,
						&(cbuf[extractbuff].Buffer[AR.lWorkSpace[posit]]),i,*same) ) == 0 )
						goto GenCall;
					AR.lWorkSpace[posit+1] = AR.lWorkSpace[posit];
					same[1] = *same + 1;
					if ( i > 1 && AR.lWorkSpace[posit] < AR.lWorkSpace[posit-1] ) *same = 1;
					AR.lWorkSpace[posit] += cbuf[extractbuff].Buffer[AR.lWorkSpace[posit]];
					i++;
					posit++;
					same++;
				}
				else {
					i--; posit--; same--;
				}
				if ( i > power ) {
					termout = AR.WorkPointer = a;
					if ( ( AR.WorkPointer += 2 * AM.MaxTer ) > AM.WorkTop )
						goto OverWork;
					if ( FiniTerm(term,accum,termout,replac,tepos) ) goto GenCall;
					AR.WorkPointer = termout + *termout;
					*AR.RepPoint = 1;
					AR.expchanged = 1;
					if ( Generator(termout,level) ) goto GenCall;
					i--; posit--; same--;
				}
			} while ( i > 0 );
			AR.WorkPointer = ow; AR.lWorkPointer = olw; AR.posWorkPointer = olpw;
		}
		else {							/* No binomials */
			LONG posit, olw, olpw = AR.posWorkPointer;
			WantAddLongs(power);
			posit = olw = AR.lWorkPointer; AR.lWorkPointer += power;
			a = accum = AR.WorkPointer;
			if ( ( AR.WorkPointer += 2 * AM.MaxTer ) > AM.WorkTop )
				goto OverWork;
			for ( i = 0; i < power; i++ ) AR.lWorkSpace[posit++] = posisub;
			posit = olw;
			*accum = 0;
			tepos = AR.TePos;
			i = 0;
			while ( i >= 0 ) {
				if ( cbuf[extractbuff].Buffer[AR.lWorkSpace[posit]] ) {
					if ( ( a = PasteTerm(i,accum,
						&(cbuf[extractbuff].Buffer[AR.lWorkSpace[posit]]),1,1) ) == 0 ) goto GenCall;
					AR.lWorkSpace[posit] += cbuf[extractbuff].Buffer[AR.lWorkSpace[posit]];
					i++; posit++;
				}
				else {
					AR.lWorkSpace[posit--] = posisub;
					i--;
				}
				if ( i >= power ) {
					termout = AR.WorkPointer = a;
					if ( ( AR.WorkPointer += 2 * AM.MaxTer ) > AM.WorkTop )
						goto OverWork;
					if ( FiniTerm(term,accum,termout,replac,tepos) ) goto GenCall;
					AR.WorkPointer = termout + *termout;
					*AR.RepPoint = 1;
					AR.expchanged = 1;
					if ( Generator(termout,level) ) goto GenCall;
					i--; posit--;
				}
			}
			AR.WorkPointer = accum;
			AR.lWorkPointer = olw;
			AR.posWorkPointer = olpw;
		}
	}
	else {								/* Expression from disk */
		POSITION StartPos;
		LONG position, olpw, opw, comprev, extra;
		RENUMBER renumber;
		WORD *Freeze, *aa;
		replac = -replac-1;
		power = AR.TeSuOut;
		Freeze = AR.Frozen;
		if ( Expressions[replac].status == STOREDEXPRESSION ) {
			POSITION firstpos;
			SETSTARTPOS(firstpos);

/*			Note that AR.TMaddr is needed for GetTable just once! */
/*
			We need space for the previous term in the compression
			This is made available in AM.CompressBuffer, although we may get
			problems with this sooner or later. Hence we need to keep
			a set of pointers in AM.CompressBuffer
			Note that after the last call there has been no use made
			of AR.CompressPointer, so it points automatically at its original
			position!
*/
			WantAddPointers(power+1);
			comprev = opw = AR.pWorkPointer;
			AR.pWorkPointer += power+1;
			WantAddPositions(power+1);
			position = olpw = AR.posWorkPointer;
			AR.posWorkPointer += power + 1;

			AR.pWorkSpace[comprev++] = AR.CompressPointer;

			for ( i = 0; i < power; i++ ) {
				PUTZERO(AR.posWorkSpace[position]); position++;
			}
			position = olpw;
			if ( ( renumber = GetTable(replac,&(AR.posWorkSpace[position])) ) == 0 ) goto GenCall;
			accum = AR.WorkPointer;
			if ( ( AR.WorkPointer += 2 * AM.MaxTer ) > AM.WorkTop )
					goto OverWork;
			aa = AR.WorkPointer;
			*accum = 0;
			i = 0; StartPos = AR.posWorkSpace[position];
			while ( i >= 0 ) {
skippedfirst:
				AR.CompressPointer = AR.pWorkSpace[comprev-1];
				if ( ( extra = PasteFile(i,accum,&(AR.posWorkSpace[position])
						,&a,renumber,Freeze,replac) ) < 0 ) goto GenCall;
				if ( NOTSTARTPOS(firstpos) ) {
					if ( ISMINPOS(firstpos) || ISEQUALPOS(firstpos,AR.posWorkSpace[position]) ) {
						firstpos = AR.posWorkSpace[position];
						ADDPOS(AR.posWorkSpace[position],extra * sizeof(WORD));
						goto skippedfirst;
					}
				}
				if ( extra ) { 
					ADDPOS(AR.posWorkSpace[position],extra * sizeof(WORD));
					i++; AR.posWorkSpace[++position] = StartPos;
					AR.pWorkSpace[comprev++] = AR.CompressPointer;
				}
				else {
					PUTZERO(AR.posWorkSpace[position]); position--; i--;
					comprev--;
				}
				if ( i >= power ) {
					termout = AR.WorkPointer = a;
					if ( ( AR.WorkPointer += 2*AM.MaxTer ) > AM.WorkTop ) goto OverWork;
					if ( FiniTerm(term,accum,termout,replac,0) ) goto GenCall;
					if ( *termout ) {
						AR.WorkPointer = termout + *termout;
						*AR.RepPoint = 1;
						AR.expchanged = 1;
						if ( Generator(termout,level) ) goto GenCall;
					}
					i--; position--;
					comprev--;
				}
				AR.WorkPointer = aa;
			}
			AR.WorkPointer = accum;
			AR.posWorkPointer = olpw;
			AR.pWorkPointer = opw;
		}
		else {			/* Active expression */
			aa = accum = AR.WorkPointer;
			if ( ( AR.WorkPointer + 2 * AM.MaxTer + 1 ) > AM.WorkTop )
					goto OverWork;
			*accum++ = -1; AR.WorkPointer++;
			if ( DoOnePow(term,power,replac,accum,aa,level,Freeze) ) goto GenCall;
			AR.WorkPointer = aa;
		}
  	}
	AR.CurDum = DumNow;
Return0:
	AR.RepPoint = RepSto;
	AR.cbufnum = oldcbufnum;
	CC->numrhs = oldtoprhs;
	CC->Pointer = CC->Buffer + oldcpointer;
	return(0);

GenCall:
	if ( AM.tracebackflag ) {
		termout = term;
		AO.OutFill = AO.OutputLine = (UBYTE *)AR.WorkPointer;
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
	}
	AR.cbufnum = oldcbufnum;
	CC->numrhs = oldtoprhs;
	CC->Pointer = CC->Buffer + oldcpointer;
	return(-1);
OverWork:
	AR.cbufnum = oldcbufnum;
	CC->numrhs = oldtoprhs;
	CC->Pointer = CC->Buffer + oldcpointer;
	return(MesWork());
}

/*
 		#] Generator :
 		#[ DoOnePow :			WORD DoOnePow(term,power,nexp,accum,aa,level,freeze)

		Routine gets one power of an expression.
		If there are more powers needed there will be a recursion.

		No attempt is made to use binomials because we have no
		information about commutating properties.

		There is a searching for the contents of brackets if needed.
		This searching may be rather slow because of the single links.

		freeze is the pointer to the bracket information that should
			   be matched.
		nexp   is the number of the expression.
		accum  is the accumulator of terms. It accepts the termfragments
			   that are made into a proper term in FiniTerm
		term   is the term we are adding to.
		power  is the power of the expression that we need.
		level  is the current depth in the tree of statements. It is
			   needed to continue to the next operation/substitution
			   with each generated term
		aa	   points to the start of the entire accumulator. In *aa
			   we store the number of term fragments that are in the
			   accumulator.
*/

WORD
DoOnePow ARG7(WORD *,term,WORD,power,WORD,nexp,WORD *,accum
			 ,WORD *,aa,WORD,level,WORD *,freeze)
{
	POSITION oldposition;
	WORD *acc, *termout, fromfreeze = 0, hand;
	WORD *oldipointer = AR.CompressPointer;
	FILEHANDLE *fi;
	WORD /* first, */ type;
	WORD oldGetOneFile = AR.GetOneFile;
	BRACKETINFO *bi;
	type = Expressions[nexp].status;
	if ( type == HIDDENLEXPRESSION || type == HIDDENGEXPRESSION ) {
		AR.GetOneFile = 2; fi = AS.hidefile;
	}
	else {
		AR.GetOneFile = 0; fi = AR.infile;
	}
	hand = fi->handle;
	if ( hand >= 0 ) {
		PUTZERO(oldposition);
		SeekFile(fi->handle,&oldposition,SEEK_CUR);
	}
	else {
		SETBASEPOSITION(oldposition,fi->POfill-fi->PObuffer);
	}
	if ( freeze && ( ( bi = Expressions[nexp].bracketinfo ) != 0 ) ) {
		POSITION *brapos;
/*
		There is a bracket index
		AR.CompressPointer = oldipointer;
*/
		(*aa)++;
		power--;
		if ( ( brapos = FindBracket(&(Expressions[nexp]),freeze) ) == 0 )
			goto EndExpr;
		if ( hand >= 0 ) {
			SeekFile(hand,brapos,SEEK_SET);
		}
		else {
			fi->POfill = (WORD *)((UBYTE *)(fi->PObuffer) + BASEPOSITION(*brapos));
		}
		goto doterms;
	}
/*	first = 0; */
	if ( hand >= 0 ) {
		SeekFile(hand,&(AS.OldOnFile[nexp]),SEEK_SET);
		if ( ISNEGPOS(AS.OldOnFile[nexp]) < 0 ) {
			MesPrint("File error");
			goto PowCall;
		}
	}
	else {
		fi->POfill = (WORD *)((UBYTE *)(fi->PObuffer) + BASEPOSITION(AS.OldOnFile[nexp]));
	}
	if ( GetOneTerm(accum,hand) > 0 ) {			/* Skip prototype */
		(*aa)++;
		power--;
doterms:
		AR.CompressPointer = oldipointer;
		while ( GetOneTerm(accum,hand) > 0 ) {
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
			acc = accum;
			acc += *acc;
			if ( power <= 0 ) {
				termout = acc;
				if ( ( AR.WorkPointer = acc + 2*AM.MaxTer ) > AM.WorkTop ) return(MesWork());
				if ( FiniTerm(term,aa,termout,nexp,0) ) goto PowCall;
				if ( *termout ) {
					AR.WorkPointer = termout + *termout;
					*AR.RepPoint = 1;
					AR.expchanged = 1;
					if ( Generator(termout,level) ) goto PowCall;
				}
			}
			else {
				if ( acc > AM.WorkTop ) return(MesWork());
				if ( DoOnePow(term,power,nexp,acc,aa,level,freeze) ) goto PowCall;
			}
NextTerm:;
			AR.CompressPointer = oldipointer;
		}
EndExpr:
		(*aa)--;
	}
	AR.CompressPointer = oldipointer;
	if ( hand >= 0 ) {
		SeekFile(hand,&oldposition,SEEK_SET);
		if ( ISNEGPOS(oldposition) ) {
			MesPrint("File error");
			goto PowCall;
		}
	}
	else {
		fi->POfill = fi->PObuffer + BASEPOSITION(oldposition);
	}
	AR.GetOneFile = oldGetOneFile;
	return(0);
PowCall:
	MesCall("DoOnePow");
	SETERROR(-1)
}

/*
 		#] DoOnePow : 
 		#[ Deferred :			WORD Deferred(term,level)

		Picks up the deferred brackets.
*/

WORD
Deferred ARG2(WORD *,term,WORD,level)
{
	POSITION oldposition, startposition;
	WORD *t, *m, *mstop, *tstart, decr, oldb, *termout, i, *oldwork;
	WORD *oldipointer = AR.CompressPointer;
	WORD oldGetOneFile = AR.GetOneFile;
	AR.GetOneFile = 1;
	oldwork = AR.WorkPointer;
	AR.WorkPointer += AM.MaxTer;
	termout = AR.WorkPointer;
	AR.DeferFlag = 0;
	if ( AR.infile->handle >= 0 ) {
		PUTZERO(oldposition);
		SeekFile(AR.infile->handle,&oldposition,SEEK_CUR);
		startposition = AR.DefPosition;
		SeekFile(AR.infile->handle,&startposition,SEEK_SET);
		if ( ISNEGPOS(startposition) ) {
			MesPrint("File error");
			goto DefCall;
		}
	}
	else {
		SETBASEPOSITION(oldposition,AR.infile->POfill-AR.infile->PObuffer);
	}
	t = m = AM.CompressBuffer;
	t += *t;
	mstop = t - ABS(t[-1]);
	m++;
	while ( *m != HAAKJE && m < mstop ) m += m[1];
	if ( m >= mstop ) {	/* No deferred action! */
		AR.WorkPointer = term + *term;
		if ( Generator(term,level) ) goto DefCall;
		AR.DeferFlag = 1;
		AR.WorkPointer = oldwork;
		AR.GetOneFile = oldGetOneFile;
		return(0);
	}
	mstop = m + m[1];
	decr = WORDDIF(mstop,AM.CompressBuffer)-1;
	tstart = AR.CompressPointer + decr;

	m = AM.CompressBuffer;
	t = AR.CompressPointer;
	i = *m;
	NCOPY(t,m,i);
	oldb = *tstart;
	AR.TePos = 0;
	AR.TeSuOut = 0;
/*
	Status of affairs:
	First bracket content starts at mstop.
	Next term starts at startposition and the file has been positioned.
	Decompression information is in AR.CompressPointer.
	The outside of the bracket runs from AM.CompressBuffer+1 to mstop.
*/
	for(;;) {
		*tstart = *(AR.CompressPointer)-decr;
		AR.CompressPointer = AR.CompressPointer+AR.CompressPointer[0];

		if ( InsertTerm(term,0,AM.rbufnum,tstart,termout,0) < 0 ) {
			goto DefCall;
		}
		*tstart = oldb;
		AR.WorkPointer = termout + *termout;
		if ( Generator(termout,level) ) goto DefCall;
		AR.CompressPointer = oldipointer;
		AR.WorkPointer = termout;
		if ( GetOneTerm(AR.WorkPointer,AR.infile->handle) <= 0 ) break;
		AR.CompressPointer = oldipointer;
		t = AR.CompressPointer;
		if ( *t < (1 + decr + ABS(*(t+*t-1))) ) break;
		t++;
		m = AM.CompressBuffer+1;
		while ( m < mstop ) {
			if ( *m != *t ) goto Thatsit;
			m++; t++;
		}
	}
Thatsit:
	if ( AR.infile->handle >= 0 ) {
		SeekFile(AR.infile->handle,&oldposition,SEEK_SET);
		if ( ISNEGPOS(oldposition) ) {
			MesPrint("File error");
		}
	}
	else {
		AR.infile->POfill = AR.infile->PObuffer + BASEPOSITION(oldposition);
	}
	AR.DeferFlag = 1;
	AR.GetOneFile = oldGetOneFile;
	return(0);
DefCall:
	MesCall("Deferred");
	SETERROR(-1)
}

/*
 		#] Deferred : 
 		#[ PrepPoly :			WORD PrepPoly(term)

		Routine checks whether the count of function AC.PolyFun is zero
		or one. If it is one and it has one scalarlike argument the
		coefficient of the term is pulled inside the argument.
		If the count is zero a new function is made with the coefficient
		as its only argument. The function should be placed at its
		proper position.

		When this function is active it places the PolyFun as last
		object before the coefficient. This is needed because otherwise
		the compress algorithm has problems in MergePatches.

		The bracket routine should also place the PolyFun at a
		comparable spot.
		The compression should then stop at the PolyFun. It doesn't
		really have to stop when writing the final result but this may
		be too complicated.
*/

WORD
PrepPoly ARG1(WORD *,term)
{
	WORD count = 0, i, jcoef, ncoef, ofun = 0;
	WORD *t, *m, *r, *tstop, *poly = 0, *v, *w, *vv, *ww;
	WORD *oldworkpointer = AR.WorkPointer;
	AR.PolyAct = 0;
	t = term;
	GETSTOP(t,tstop);
	t++;
	while ( t < tstop ) {
		if ( *t == AC.PolyFun ) {
			if ( count > 0 ) return(0);
			poly = t;
			count++;
		}
		t += t[1];
	}
	r = m = term + *term;
	i = ABS(m[-1]);
	if ( count == 0 ) {
		if ( ofun ) return(0);
		poly = t = tstop;
		if ( i == 3 && m[-2] == 1 && (m[-3]&MAXPOSITIVE) == m[-3] ) {
			*m++ = AC.PolyFun;
			*m++ = FUNHEAD+2;
#if FUNHEAD > 2
			{ int ie = FUNHEAD-2; while ( ie-- > 0 ) *m++ = 0; }
#endif
			*m++ = -SNUMBER;
			*m = m[-2-FUNHEAD] < 0 ? -m[-4-FUNHEAD]: m[-4-FUNHEAD];
			m++;
		}
		else {
			r = tstop;
			*m++ = AC.PolyFun;
			*m++ = FUNHEAD+ARGHEAD+i+1;
#if FUNHEAD > 2
			{ int ie = FUNHEAD-2; while ( ie-- > 0 ) *m++ = 0; }
#endif
			*m++ = ARGHEAD+i+1;
			*m++ = 0;
#if ARGHEAD > 2
			{ int ie = ARGHEAD-2; while ( ie-- > 0 ) *m++ = 0; }
#endif
			*m++ = i+1;
			NCOPY(m,r,i);
		}
	}
	else {
		m = term + *term;
		r = poly + poly[1];
		if ( ( poly[1] == FUNHEAD+2 && poly[FUNHEAD+1] == 0
		&& poly[FUNHEAD] == -SNUMBER ) || poly[1] == FUNHEAD ) return(1);
		if ( ofun ) {
			*term -= poly[1];
			while ( r < m ) *poly++ = *r++;
			return(0);
		}
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
			*m++ = AC.PolyFun;
			*m++ = FUNHEAD*2+ARGHEAD+i+1;
#if FUNHEAD > 2
			{ int ie = FUNHEAD-2; while ( ie-- > 0 ) *m++ = 0; }
#endif
			*m++ = FUNHEAD+ARGHEAD+i+1;
			*m++ = 0;
#if ARGHEAD > 2
			{ int ie = ARGHEAD-2; while ( ie-- > 0 ) *m++ = 0; }
#endif
			*m++ = FUNHEAD+i+1;
			*m++ = -*t++;
			*m++ = FUNHEAD;
#if FUNHEAD > 2
			{ int ie = FUNHEAD-2; while ( ie-- > 0 ) *m++ = 0; }
#endif
			NCOPY(m,r,i);
		}
		else if ( *t < 0 ) {
			if ( t+2 < r ) return(0);	/* More than one argument */
			r = tstop;
			if ( *t == -SNUMBER ) {
				if ( t[1] == 0 ) return(1);	/* Term should be zero now */
				*m = AC.PolyFun;
				w = m+1;
				m += FUNHEAD+ARGHEAD;
				v = m;
				*m++ = 5+i;
				*m++ = SNUMBER;
				*m++ = 4;
				*m++ = t[1];
				*m++ = 1;
				NCOPY(m,r,i);
				AR.WorkPointer = m;
				if ( Normalize(v) ) Terminate(-1);
				AR.WorkPointer = oldworkpointer;
				m = w;
				if ( *v == 4 && v[2] == 1 && (v[1]&MAXPOSITIVE) == v[1] ) {
					*m++ = FUNHEAD+2;
#if FUNHEAD > 2
					{ int ie = FUNHEAD-2; while ( ie-- > 0 ) *m++ = 0; }
#endif
					*m++ = -SNUMBER;
					*m++ = v[3] < 0 ? -v[1] : v[1];
				}
				else if ( *v == 0 ) return(1);
				else {
					*m++ = FUNHEAD+ARGHEAD+*v;
#if FUNHEAD > 2
					{ int ie = FUNHEAD-2; while ( ie-- > 0 ) *m++ = 0; }
#endif
					*m++ = ARGHEAD+*v;
					*m++ = 0;
#if ARGHEAD > 2
					{ int ie = ARGHEAD-2; while ( ie-- > 0 ) *m++ = 0; }
#endif
					m = v + *v;
				}
			}
			else if ( *t == -SYMBOL ) {
				*m++ = AC.PolyFun;
				*m++ = FUNHEAD+ARGHEAD+5+i;
#if FUNHEAD > 2
				{ int ie = FUNHEAD-2; while ( ie-- > 0 ) *m++ = 0; }
#endif
				*m++ = ARGHEAD+5+i;
				*m++ = 0;
#if ARGHEAD > 2
				{ int ie = ARGHEAD-2; while ( ie-- > 0 ) *m++ = 0; }
#endif
				*m++ = 5+i;
				*m++ = SYMBOL;
				*m++ = 4;
				*m++ = t[1];
				*m++ = 1;
				NCOPY(m,r,i);
			}
			else return(0);			/* Not symbol-like */
			t += 2;
		}
		else {
			if ( t + *t < r ) return(0); /* More than one argument */
			i = m[-1];
			*m++ = AC.PolyFun;
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
				WORD *vv, *vstop;
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
				if ( MulRat((UWORD *)vv,ncoef,(UWORD *)tstop,jcoef,
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
			w += FUNHEAD-1;
			if ( ToFast(w,w) ) {
				if ( *w <= -FUNCTION ) { w[-FUNHEAD+1] = FUNHEAD+1; m = w+1; }
				else { w[-FUNHEAD+1] = FUNHEAD+2; m = w+2; }
				
			}
	        t = r;
		}
		t = poly + poly[1];
		while ( t < tstop ) *poly++ = *t++;
	}
	r = term + *term;
	AR.PolyAct = WORDDIF(poly,term);
	while ( r < m ) *poly++ = *r++;
	*poly++ = 1;
	*poly++ = 1;
	*poly++ = 3;
	*term = WORDDIF(poly,term);
	return(0);
}

/*
 		#] PrepPoly : 
 		#[ PolyMul :			WORD PolyMul(term) 
*/

WORD
PolyMul ARG1(WORD *,term)
{
	WORD *t, *fun1, *fun2, *t1, *t2, *m, *w, *tt1, *tt2, *arg1, *arg2;
	WORD *tstop;
	WORD n1, n2, i1, i2, l1, l2, l3, l4, action = 0, noac = 0;
retry:
	AR.WorkPointer = term + *term;
	GETSTOP(term,tstop);
	t = term+1;
	while ( *t != AC.PolyFun && t < tstop ) t += t[1];
	while ( t < tstop && *t == AC.PolyFun ) {
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
	if ( *t != AC.PolyFun || t >= tstop ) goto done;
	fun1 = t;
	t += t[1];
	while ( t < tstop && *t == AC.PolyFun ) {
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
	if ( *t != AC.PolyFun || t >= tstop ) goto done;
	fun2 = t;
/*
	We have two functions of the proper type.
	Count terms (needed for the specials)
*/
	t = fun1 + FUNHEAD;
	if ( *t < 0 ) {
		n1 = 1; arg1 = AR.WorkPointer;
		ToGeneral(t,arg1,1);
		AR.WorkPointer = arg1 + *arg1;
	}
	else {
		t += ARGHEAD;
		n1 = 0; t1 = fun1 + fun1[1]; arg1 = t;
		while ( t < t1 ) { n1++; t += *t; }
	}
	t = fun2 + FUNHEAD;
	if ( *t < 0 ) {
		n2 = 1; arg2 = AR.WorkPointer;
		ToGeneral(t,arg2,1);
		AR.WorkPointer = arg2 + *arg2;
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
	w = AR.WorkPointer;
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
		AR.WorkPointer = m;
		if ( Normalize(w) ) { LowerSortLevel(); goto PolyCall; }
		if ( *w ) {
			m = w + *w;
			if ( m[-1] != 3 || m[-2] != 1 || m[-3] != 1 ) {
				l3 = REDLENG(m[-1]);
				m -= ABS(m[-1]);
				t = t1 + *t1 - 1;
				l1 = REDLENG(*t);
				if ( MulRat((UWORD *)m,l3,(UWORD *)tt1,l1,(UWORD *)m,&l4) ) {
					LowerSortLevel(); goto PolyCall; }
				if ( AC.ncmod != 0 && TakeModulus((UWORD *)m,&l4,0) ) {
					LowerSortLevel(); goto PolyCall; }
				if ( l4 == 0 ) continue;
				t = t2 + *t2 - 1;
				l2 = REDLENG(*t);
				if ( MulRat((UWORD *)m,l4,(UWORD *)tt2,l2,(UWORD *)m,&l3) ) {
					LowerSortLevel(); goto PolyCall; }
				if ( AC.ncmod != 0 && TakeModulus((UWORD *)m,&l3,0) ) {
					LowerSortLevel(); goto PolyCall; }
			}
			else {
				m -= 3;
				t = t1 + *t1 - 1;
				l1 = REDLENG(*t);
				t = t2 + *t2 - 1;
				l2 = REDLENG(*t);
				if ( MulRat((UWORD *)tt1,l1,(UWORD *)tt2,l2,(UWORD *)m,&l3) ) {
					LowerSortLevel(); goto PolyCall; }
				if ( AC.ncmod != 0 && TakeModulus((UWORD *)m,&l3,0) ) {
					LowerSortLevel(); goto PolyCall; }
			}
			if ( l3 == 0 ) continue;
			l3 = INCLENG(l3);
			m += ABS(l3);
			m[-1] = l3;
			*w = WORDDIF(m,w);
			AR.WorkPointer = m;
			if ( StoreTerm(w) ) { LowerSortLevel(); goto PolyCall; }
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
	AR.WorkPointer = t;
	n1 = WORDDIF(t,w);
	t1 = term;
	while ( t1 < fun1 ) *t++ = *t1++;
	t2 = t;
	*t++ = AC.PolyFun;
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
	*AR.WorkPointer = n1 = WORDDIF(t,AR.WorkPointer);
	if ( n1 > AM.MaxTer ) {
		MesPrint("Term too complex. Maybe increasing MaxTermSize can help");
		goto PolyCall;
	}
	m = term; t = AR.WorkPointer;
	NCOPY(m,t,n1);
	action++;
	goto retry;
done:
	AR.WorkPointer = term + *term;
	if ( action && noac ) {
		if ( Normalize(term) ) goto PolyCall;
		AR.WorkPointer = term + *term;
	}
	return(0);
PolyCall:
	MesCall("PolyMul");
	SETERROR(-1)
}

/*
 		#] PolyMul : 
	#] Processor :
*/
