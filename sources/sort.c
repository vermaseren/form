/** @file sort.c
 * 
 *  Contains the sort routines.
 *	We distinguish levels of sorting.
 *	The ground level is the sorting of terms in an expression.
 *	When a term has functions, the arguments can contain terms that need
 *	sorting, which this then done by raising the level. This can happen
 *	recursively. NewSort and EndSort automatically raise and lower the
 *	level. Because the ground level does some special things, sometimes
 *	we have to raise immediately to the second level skipping the ground level.
 *
 *	Special routines for the parallel sorting are in the file threads.c
 *	Also the sorting of terms in polynomials is special but most of that is
 *	controled by changing the address of the compare routine. Other routines
 *	relevant for adding rational polynomials are in the file polynito.c
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
  	#[ Includes : sort.c

	Sort routines according to new conventions (25-jun-1997).
	This is more object oriented.
	The active sort is indicated by AT.SS which should agree with
	AN.FunSorts[AR.sLevel];

#define GZIPDEBUG
*/
#define NEWSPLITMERGE

#include "form3.h"

#ifdef WITHPTHREADS
UBYTE THRbuf[100];
#endif

#ifdef WITHSTATS
extern LONG numwrites;
extern LONG numreads;
extern LONG numseeks;
extern LONG nummallocs;
extern LONG numfrees;
#endif

LONG numcompares;

/*
  	#] Includes : 
	#[ SortUtilities :
 		#[ WriteStats :				VOID WriteStats(lspace,par)
*/
 
char *toterms[] = { "   ", " >>", "-->" };

/**
 *		Writes the statistics.
 *
 *		@param plspace The size in bytes currently occupied
 *		@param par
 *		par = 0 after a splitmerge.
 *		par = 1 after merge to sortfile.
 *		par = 2 after the sort
 *
 *		current expression is to be found in AR.CurExpr.
 *		terms are in S->TermsLeft.
 *		S->GenTerms.
 */

VOID WriteStats(POSITION *plspace, WORD par)
{
	GETIDENTITY
	LONG millitime, y = 0x7FFFFFFFL >> 1;
	WORD timepart;
	SORTING *S;
	POSITION pp;
	int use_wtime;
	if ( AT.SS == AT.S0 && AC.StatsFlag ) {
#ifdef WITHPTHREADS
		if ( AC.ThreadStats == 0 && identity > 0 ) return;
#elif defined(WITHMPI)
		if ( AC.OldParallelStats ) return;
		if ( ! AC.ProcessStats && PF.me != MASTER ) return;
#endif
		if ( Expressions == 0 ) return;

		if ( par == 0 ) {
			AR.ShortSortCount++;
			if ( AR.ShortSortCount < AC.ShortStatsMax ) return;
		}
		AR.ShortSortCount = 0;

		S = AT.SS;
		MLOCK(ErrorMessageLock);
		if ( AC.ShortStats ) {}
		else {
#ifdef WITHPTHREADS
			if ( identity > 0 ) {
				MesPrint("             Thread %d reporting",identity);
			}
			else {
				MesPrint("");
			}
#elif defined(WITHMPI)
			if ( PF.me != MASTER ) {
				MesPrint("             Process %d reporting",PF.me);
			}
			else {
				MesPrint("");
			}
#else
			MesPrint("");
#endif
		}
		/*
		 * We define WTimeStatsFlag as a flag to print the wall-clock time on
		 * the *master*, not in workers. This can be confusing in thread
		 * statistics when short statistics is used. Technically,
		 * TimeWallClock() is not thread-safe in TFORM.
		 */
		use_wtime = AC.WTimeStatsFlag;
#if defined(WITHPTHREADS)
		if ( use_wtime && identity > 0 ) use_wtime = 0;
#elif defined(WITHMPI)
		if ( use_wtime && PF.me != MASTER ) use_wtime = 0;
#endif
		millitime = use_wtime ? TimeWallClock(1) * 10 : TimeCPU(1);
		timepart = (WORD)(millitime%1000);
		millitime /= 1000;
		timepart /= 10;
		if ( AC.ShortStats ) {
#if defined(WITHPTHREADS) || defined(WITHMPI)
#ifdef WITHPTHREADS
		  if ( identity > 0 ) {
#else
		  if ( PF.me != MASTER ) {
			const int identity = PF.me;
#endif
			if ( par == 0 || par == 2 ) {
				SETBASEPOSITION(pp,y);
				if ( ISLESSPOS(*plspace,pp) ) {
					MesPrint("%d: %7l.%2is %8l>%10l%3s%10l:%10p %s %s",identity,
					millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
					S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
/*
					MesPrint("%d: %14s %17s %7l.%2is %8l>%10l%3s%10l:%10p",identity,
					EXPRNAME(AR.CurExpr),AC.Commercial,millitime,timepart,
					AN.ninterms,S->GenTerms,toterms[par],S->TermsLeft,plspace);
*/
				}
				else {
					y = 1000000000L;
					SETBASEPOSITION(pp,y);
					MULPOS(pp,100);
					if ( ISLESSPOS(*plspace,pp) ) {
						MesPrint("%d: %7l.%2is %8l>%10l%3s%10l:%11p %s %s",identity,
						millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
						S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
					}
					else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%d: %7l.%2is %8l>%10l%3s%10l:%12p %s %s",identity,
							millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%d: %7l.%2is %8l>%10l%3s%10l:%13p %s %s",identity,
							millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%d: %7l.%2is %8l>%10l%3s%10l:%14p %s %s",identity,
							millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%d: %7l.%2is %8l>%10l%3s%10l:%15p %s %s",identity,
							millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%d: %7l.%2is %8l>%10l%3s%10l:%16p %s %s",identity,
							millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%d: %7l.%2is %8l>%10l%3s%10l:%17p %s %s",identity,
							millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						} } } } }
					}
				}
			}
			else if ( par == 1 ) {
				SETBASEPOSITION(pp,y);
				if ( ISLESSPOS(*plspace,pp) ) {
					MesPrint("%d: %7l.%2is %10l:%10p",identity,millitime,timepart,
					S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
				}
				else {
					y = 1000000000L;
					SETBASEPOSITION(pp,y);
					MULPOS(pp,100);
					if ( ISLESSPOS(*plspace,pp) ) {
						MesPrint("%d: %7l.%2is %10l:%11p",identity,millitime,timepart,
						S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
					}
					else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%d: %7l.%2is %10l:%12p",identity,millitime,timepart,
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%d: %7l.%2is %10l:%13p",identity,millitime,timepart,
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%d: %7l.%2is %10l:%14p",identity,millitime,timepart,
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%d: %7l.%2is %10l:%15p",identity,millitime,timepart,
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%d: %7l.%2is %10l:%16p",identity,millitime,timepart,
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%d: %7l.%2is %10l:%17p",identity,millitime,timepart,
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						} } } } }
					}
				}
			} } else
#endif
			{
			if ( par == 0 || par == 2 ) {
				SETBASEPOSITION(pp,y);
				if ( ISLESSPOS(*plspace,pp) ) {
					MesPrint("%7l.%2is %8l>%10l%3s%10l:%10p %s %s",
					millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
					S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
/*
					MesPrint("%14s %17s %7l.%2is %8l>%10l%3s%10l:%10p",
					EXPRNAME(AR.CurExpr),AC.Commercial,millitime,timepart,
					AN.ninterms,S->GenTerms,toterms[par],S->TermsLeft,plspace);
*/
				}
				else {
					y = 1000000000L;
					SETBASEPOSITION(pp,y);
					MULPOS(pp,100);
					if ( ISLESSPOS(*plspace,pp) ) {
						MesPrint("%7l.%2is %8l>%10l%3s%10l:%11p %s %s",
						millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
						S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
					}
					else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%7l.%2is %8l>%10l%3s%10l:%12p %s %s",
							millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%7l.%2is %8l>%10l%3s%10l:%13p %s %s",
							millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%7l.%2is %8l>%10l%3s%10l:%14p %s %s",
							millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%7l.%2is %8l>%10l%3s%10l:%15p %s %s",
							millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%7l.%2is %8l>%10l%3s%10l:%16p %s %s",
							millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%7l.%2is %8l>%10l%3s%10l:%17p %s %s",
							millitime,timepart,AN.ninterms,S->GenTerms,toterms[par],
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						} } } } }
					}
				}
			}
			else if ( par == 1 ) {
				SETBASEPOSITION(pp,y);
				if ( ISLESSPOS(*plspace,pp) ) {
					MesPrint("%7l.%2is %10l:%10p",millitime,timepart,
					S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
				}
				else {
					y = 1000000000L;
					SETBASEPOSITION(pp,y);
					MULPOS(pp,100);
					if ( ISLESSPOS(*plspace,pp) ) {
						MesPrint("%7l.%2is %10l:%11p",millitime,timepart,
						S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
					}
					else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%7l.%2is %10l:%12p",millitime,timepart,
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%7l.%2is %10l:%13p",millitime,timepart,
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%7l.%2is %10l:%14p",millitime,timepart,
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%7l.%2is %10l:%15p",millitime,timepart,
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%7l.%2is %10l:%16p",millitime,timepart,
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						else {
						MULPOS(pp,10);
						if ( ISLESSPOS(*plspace,pp) ) {
							MesPrint("%7l.%2is %10l:%17p",millitime,timepart,
							S->TermsLeft,plspace,EXPRNAME(AR.CurExpr),AC.Commercial);
						}
						} } } } }
					}
				}
			}
		} }
		else {
		if ( par == 1 ) {
			if ( use_wtime ) {
				MesPrint("WTime = %7l.%2i sec",millitime,timepart);
			}
			else {
				MesPrint("Time = %7l.%2i sec",millitime,timepart);
			}
		}
		else {
#if ( BITSINLONG > 32 )
			if ( S->GenTerms >= 10000000000L ) {
				if ( use_wtime ) {
					MesPrint("WTime = %7l.%2i sec   Generated terms = %16l",
						millitime,timepart,S->GenTerms);
				}
				else {
					MesPrint("Time = %7l.%2i sec    Generated terms = %16l",
						millitime,timepart,S->GenTerms);
				}
			}
			else {
				if ( use_wtime ) {
					MesPrint("WTime = %7l.%2i sec   Generated terms = %10l",
						millitime,timepart,S->GenTerms);
				}
				else {
					MesPrint("Time = %7l.%2i sec    Generated terms = %10l",
						millitime,timepart,S->GenTerms);
				}
			}
#else
			if ( use_wtime ) {
				MesPrint("WTime = %7l.%2i sec   Generated terms = %10l",
					millitime,timepart,S->GenTerms);
			}
			else {
				MesPrint("Time = %7l.%2i sec    Generated terms = %10l",
					millitime,timepart,S->GenTerms);
			}
#endif
		}
#if ( BITSINLONG > 32 )
		if ( par == 0 )
			if ( S->TermsLeft >= 10000000000L ) {
				MesPrint("%16s%8l Terms %s = %16l",EXPRNAME(AR.CurExpr),
				AN.ninterms,FG.swmes[par],S->TermsLeft);
			}
			else {
				MesPrint("%16s%8l Terms %s = %10l",EXPRNAME(AR.CurExpr),
				AN.ninterms,FG.swmes[par],S->TermsLeft);
			}
		else {
			if ( S->TermsLeft >= 10000000000L ) {
#ifdef WITHPTHREADS
				if ( identity > 0 && par == 2 ) {
					MesPrint("%16s         Terms in thread = %16l",
					EXPRNAME(AR.CurExpr),S->TermsLeft);
				}
				else
#elif defined(WITHMPI)
				if ( PF.me != MASTER && par == 2 ) {
					MesPrint("%16s         Terms in process= %16l",
					EXPRNAME(AR.CurExpr),S->TermsLeft);
				}
				else
#endif
				{
					MesPrint("%16s         Terms %s = %16l",
					EXPRNAME(AR.CurExpr),FG.swmes[par],S->TermsLeft);
				}
			}
			else {
#ifdef WITHPTHREADS
				if ( identity > 0 && par == 2 ) {
					MesPrint("%16s         Terms in thread = %10l",
					EXPRNAME(AR.CurExpr),S->TermsLeft);
				}
				else
#elif defined(WITHMPI)
				if ( PF.me != MASTER && par == 2 ) {
					MesPrint("%16s         Terms in process= %10l",
					EXPRNAME(AR.CurExpr),S->TermsLeft);
				}
				else
#endif
				{
					MesPrint("%16s         Terms %s = %10l",
					EXPRNAME(AR.CurExpr),FG.swmes[par],S->TermsLeft);
				}
			}
		}
#else
		if ( par == 0 )
			MesPrint("%16s%8l Terms %s = %10l",EXPRNAME(AR.CurExpr),
			AN.ninterms,FG.swmes[par],S->TermsLeft);
		else {
#ifdef WITHPTHREADS
			if ( identity > 0 && par == 2 ) {
				MesPrint("%16s         Terms in thread = %10l",
				EXPRNAME(AR.CurExpr),S->TermsLeft);
			}
			else
#elif defined(WITHMPI)
			if ( PF.me != MASTER && par == 2 ) {
				MesPrint("%16s         Terms in process= %10l",
				EXPRNAME(AR.CurExpr),S->TermsLeft);
			}
			else
#endif
			{
				MesPrint("%16s         Terms %s = %10l",
				EXPRNAME(AR.CurExpr),FG.swmes[par],S->TermsLeft);
			}
		}
#endif
		SETBASEPOSITION(pp,y);
		if ( ISLESSPOS(*plspace,pp) ) {
			MesPrint("%24s Bytes used      = %10p",AC.Commercial,plspace);
		}
		else {
			y = 1000000000L;
			SETBASEPOSITION(pp,y);
			MULPOS(pp,100);
			if ( ISLESSPOS(*plspace,pp) ) {
				MesPrint("%24s Bytes used      =%11p",AC.Commercial,plspace);
			}
			else {
				MULPOS(pp,10);
				if ( ISLESSPOS(*plspace,pp) ) {
				MesPrint("%24s Bytes used     =%12p",AC.Commercial,plspace);
				}
				else {
				MULPOS(pp,10);
				if ( ISLESSPOS(*plspace,pp) ) {
				MesPrint("%24s Bytes used    =%13p",AC.Commercial,plspace);
				}
				else {
				MULPOS(pp,10);
				if ( ISLESSPOS(*plspace,pp) ) {
				MesPrint("%24s Bytes used   =%14p",AC.Commercial,plspace);
				}
				else {
				MULPOS(pp,10);
				if ( ISLESSPOS(*plspace,pp) ) {
				MesPrint("%24s Bytes used  =%15p",AC.Commercial,plspace);
				}
				else {
				MULPOS(pp,10);
				if ( ISLESSPOS(*plspace,pp) ) {
				MesPrint("%24s Bytes used =%16p",AC.Commercial,plspace);
				}
				else {
				MULPOS(pp,10);
				if ( ISLESSPOS(*plspace,pp) ) {
				MesPrint("%24s Bytes used=%17p",AC.Commercial,plspace);
				}
				} } } } }
			}
		} }
#ifdef WITHSTATS
		MesPrint("Total number of writes: %l, reads: %l, seeks, %l"
			,numwrites,numreads,numseeks);
		MesPrint("Total number of mallocs: %l, frees: %l"
			,nummallocs,numfrees);
#endif
		MUNLOCK(ErrorMessageLock);
	}
}

/*
 		#] WriteStats : 
 		#[ NewSort :				WORD NewSort()
*/
/**
 *		Starts a new sort.
 *		At the lowest level this is a 'main sort' with the struct according
 *		to the parameters in S0. At higher levels this is a sort for
 *		functions, subroutines or dollars.
 *		We prepare the arrays and structs.
 *
 *		@return Regular convention (OK -> 0)
 */

WORD NewSort(PHEAD0)
{
	GETBIDENTITY
	SORTING *S, **newFS;
	int i, newsize;
	if ( AN.SoScratC == 0 )
		AN.SoScratC = (UWORD *)Malloc1(2*(AM.MaxTal+2)*sizeof(UWORD),"NewSort");
	AR.sLevel++;
	if ( AR.sLevel >= AN.NumFunSorts ) {
		if ( AN.NumFunSorts == 0 ) newsize = 100;
		else newsize = 2*AN.NumFunSorts;
		newFS = (SORTING **)Malloc1((newsize+1)*sizeof(SORTING *),"FunSort pointers");
		for ( i = 0; i < AN.NumFunSorts; i++ ) newFS[i] = AN.FunSorts[i];
		for ( ; i <= newsize; i++ ) newFS[i] = 0;
		if ( AN.FunSorts ) M_free(AN.FunSorts,"FunSort pointers");
		AN.FunSorts = newFS; AN.NumFunSorts = newsize;
	}
	if ( AR.sLevel == 0 ) {

		numcompares = 0;

		AN.FunSorts[0] = AT.S0;
		if ( AR.PolyFun == 0 ) { AT.S0->PolyFlag = 0; }
		else if ( AR.PolyFunType == 1 ) { AT.S0->PolyFlag = 1; }
		else if ( AR.PolyFunType == 2 ) {
			if ( AR.PolyFunExp == 2
			  || AR.PolyFunExp == 3 ) AT.S0->PolyFlag = 1;
			else                      AT.S0->PolyFlag = 2;
		}
		AR.ShortSortCount = 0;
	}
	else {
		if ( AN.FunSorts[AR.sLevel] == 0 ) {
			AN.FunSorts[AR.sLevel] = AllocSort(
				AM.SLargeSize,AM.SSmallSize,AM.SSmallEsize,AM.STermsInSmall
					,AM.SMaxPatches,AM.SMaxFpatches,AM.SIOsize);
		}
		AN.FunSorts[AR.sLevel]->PolyFlag = 0;
	}
	AT.SS = S = AN.FunSorts[AR.sLevel];
	S->sFill = S->sBuffer;
	S->lFill = S->lBuffer;
	S->lPatch = 0;
	S->fPatchN = 0;
	S->GenTerms = S->TermsLeft = S->GenSpace = S->SpaceLeft = 0;
	S->PoinFill = S->sPointer;
	*S->PoinFill = S->sFill;
	if ( AR.sLevel > 0 ) { S->PolyWise = 0; }
	PUTZERO(S->SizeInFile[0]); PUTZERO(S->SizeInFile[1]); PUTZERO(S->SizeInFile[2]);
	S->sTerms = 0;
	PUTZERO(S->file.POposition);
	S->stage4 = 0;
	if ( AR.sLevel > AN.MaxFunSorts ) AN.MaxFunSorts = AR.sLevel;
/*
	The next variable is for the staged sort only.
	It should be treated differently

	PUTZERO(AN.OldPosOut);
*/
	return(0);
}

/*
 		#] NewSort : 
 		#[ EndSort :				WORD EndSort(PHEAD buffer,par)
*/
/**
 *		Finishes a sort.
 *		At AR.sLevel == 0 the output is to the regular output stream.
 *		When AR.sLevel > 0, the parameter par determines the actual output.
 *		The AR.sLevel will be popped.
 *		All ongoing stages are finished and if the sortfile is open
 *		it is closed.
 *		The statistics are printed when AR.sLevel == 0
 *		par == 0  Output to the buffer.
 *		par == 1  Sort for function arguments.
 *		          The output will be copied into the buffer.
 *		          It is assumed that this is in the WorkSpace.
 *		par == 2  Sort for $-variable. We return the address of the buffer
 *		          that contains the output in buffer (treated like WORD **).
 *		          We first catch the output in a file (unless we can
 *		          intercept things after the small buffer has been sorted)
 *		          Then we read from the file into a buffer.
 *		Only when par == 0 data compression can be attempted at AT.SS==AT.S0.
 *
 *		@param buffer buffer for output when needed
 *		@param par    See above
 *		@return If negative: error. If positive: number of words in output.
 */

LONG EndSort(PHEAD WORD *buffer, int par)
{
  GETBIDENTITY
  SORTING *S = AT.SS;
  WORD j, **ss, *to, *t;
  LONG sSpace, over, tover, spare, retval = 0, jj;
  POSITION position, pp;
  off_t lSpace;
  FILEHANDLE *fout = 0, *oldoutfile = 0, *newout = 0;

  if ( AM.exitflag && AR.sLevel == 0 ) return(0);
#ifdef WITHMPI 
  if( (retval = PF_EndSort()) > 0){
	oldoutfile = AR.outfile;
	retval = 0;
	goto RetRetval;
  } 
  else if(retval < 0){
	retval = -1; 
	goto RetRetval; 
  }
	/* PF_EndSort returned 0: for S != AM.S0 and slaves still do the regular sort */
#endif /* WITHMPI */
	oldoutfile = AR.outfile;
/*		PolyFlag repair action
	if ( S == AT.S0 ) {
		if ( AR.PolyFun == 0 ) { S->PolyFlag = 0; }
		else if ( AR.PolyFunType == 1 ) { S->PolyFlag = 1; }
		else if ( AR.PolyFunType == 2 ) {
			if ( AR.PolyFunExp == 2
			  || AR.PolyFunExp == 3 ) S->PolyFlag = 1;
			else                      S->PolyFlag = 2;
		}
		S->PolyWise = 0;
	}
	else {
		S->PolyFlag = S->PolyWise = 0;
	}
*/
	S->PolyWise = 0;
	*(S->PoinFill) = 0;
#ifdef SPLITTIME
		PrintTime((UBYTE *)"EndSort, before SplitMerge");
#endif
	S->sPointer[SplitMerge(BHEAD S->sPointer,S->sTerms)] = 0;
#ifdef SPLITTIME
		PrintTime((UBYTE *)"Endsort,  after SplitMerge");
#endif
	sSpace = 0;
	tover = over = S->sTerms;
	ss = S->sPointer;
	if ( over >= 0 ) {
		if ( S->lPatch > 0 || S->file.handle >= 0 ) {
			ss[over] = 0;
			sSpace = ComPress(ss,&spare);
			S->TermsLeft -= over - spare;
			if ( par == 1 ) { AR.outfile = newout = AllocFileHandle(0,(char *)0); }
		}
		else if ( S != AT.S0 ) {
			ss[over] = 0;
			if ( par == 2 ) {
				sSpace = 3;
				while ( ( t = *ss++ ) != 0 ) { sSpace += *t; }
				if ( AN.tryterm > 0 && ( (sSpace+1)*sizeof(WORD) < (size_t)(AM.MaxTer) ) ) {
					to = TermMalloc("$-sort space");
				}
				else {
					LONG allocsp = sSpace+1;
					if ( allocsp < MINALLOC ) allocsp = MINALLOC;
					allocsp = ((allocsp+7)/8)*8;
					to = (WORD *)Malloc1(allocsp*sizeof(WORD),"$-sort space");
					if ( AN.tryterm > 0 ) AN.tryterm = 0;
				}
				*((WORD **)buffer) = to;
				ss = S->sPointer;
				while ( ( t = *ss++ ) != 0 ) {
					j = *t; while ( --j >= 0 ) *to++ = *t++;
				}
				*to = 0;
				retval = sSpace + 1;
			}
			else {
				to = buffer;
				sSpace = 0;
				while ( ( t = *ss++ ) != 0 ) {
					j = *t;
					if ( ( sSpace += j ) > AM.MaxTer/((LONG)sizeof(WORD)) ) {
						MLOCK(ErrorMessageLock);
						MesPrint("Sorted function argument too long.");
						MUNLOCK(ErrorMessageLock);
						retval = -1; goto RetRetval;
					}
					while ( --j >= 0 ) *to++ = *t++;
				}
				*to = 0;
			}
			goto RetRetval;
		}
		else {
			POSITION oldpos;
			if ( S == AT.S0 ) {
				fout = AR.outfile;
				*AR.CompressPointer = 0;
				SeekScratch(AR.outfile,&position);
			}
			else {
				fout = &(S->file);
				PUTZERO(position);
			}
			oldpos = position;
			S->TermsLeft = 0;
/*
			Here we can go directly to the output.
*/
#ifdef WITHZLIB
			{ int oldgzipCompress = AR.gzipCompress;
				AR.gzipCompress = 0;
#endif
			if ( tover > 0 ) {
				ss = S->sPointer;
				while ( ( t = *ss++ ) != 0 ) {
					if ( *t ) S->TermsLeft++;
#ifdef WITHPTHREADS
					if ( AS.MasterSort && ( fout == AR.outfile ) ) { PutToMaster(BHEAD t); }
					else
#endif
					if ( PutOut(BHEAD t,&position,fout,1) < 0 ) {
						retval = -1; goto RetRetval;
					}
				}
			}
#ifdef WITHPTHREADS
			if ( AS.MasterSort && ( fout == AR.outfile ) ) { PutToMaster(BHEAD 0); }
			else
#endif
			if ( FlushOut(&position,fout,1) ) {
				retval = -1; goto RetRetval;
			}
#ifdef WITHZLIB
				AR.gzipCompress = oldgzipCompress;
			}
#endif
#ifdef WITHPTHREADS
			if ( AS.MasterSort && ( fout == AR.outfile ) ) goto RetRetval;
#endif
#ifdef WITHMPI
			if ( PF.me != MASTER && PF.exprtodo < 0 ) goto RetRetval;
#endif
			DIFPOS(oldpos,position,oldpos);
			S->SpaceLeft = BASEPOSITION(oldpos);
			WriteStats(&oldpos,(WORD)2);
			pp = oldpos;
			goto RetRetval;
		}
	}
	else if ( par == 1 && newout == 0 ) { AR.outfile = newout = AllocFileHandle(0,(char *)0); }
	sSpace++;
	lSpace = sSpace + (S->lFill - S->lBuffer) - (LONG)S->lPatch*(AM.MaxTer/sizeof(WORD));
/*         Note wrt MaxTer and lPatch: each patch starts with space for decompression */
/*         Not needed if only large buffer, but needed when using files (?) */
	SETBASEPOSITION(pp,lSpace);
	MULPOS(pp,sizeof(WORD));
	if ( S->file.handle >= 0 ) {
		ADD2POS(pp,S->fPatches[S->fPatchN]);
	}
	if ( S == AT.S0 ) {
		WORD oldLogHandle = AC.LogHandle;
		if ( AC.LogHandle >= 0 && AM.LogType && ( ( S->lPatch > 0 )
			|| S->file.handle >= 0 ) ) AC.LogHandle = -1;
		if ( S->lPatch > 0 || S->file.handle >= 0 ) { WriteStats(&pp,0); }
		AC.LogHandle = oldLogHandle;
	}
	if ( par == 2 ) { AR.outfile = newout = AllocFileHandle(0,(char *)0); }
	if ( S->lPatch > 0 ) {
		if ( ( S->lPatch >= S->MaxPatches ) ||
			( ( (WORD *)(((UBYTE *)(S->lFill + sSpace)) + 2*AM.MaxTer) ) >= S->lTop ) ) {
/*
			The large buffer is too full. Merge and write it
*/
#ifdef GZIPDEBUG
			MLOCK(ErrorMessageLock);
			MesPrint("%w EndSort: lPatch = %d, MaxPatches = %d,lFill = %x, sSpace = %ld, MaxTer = %d, lTop = %x"
					,S->lPatch,S->MaxPatches,S->lFill,sSpace,AM.MaxTer/sizeof(WORD),S->lTop);
			MUNLOCK(ErrorMessageLock);
#endif

			if ( MergePatches(1) ) {
				MLOCK(ErrorMessageLock);
				MesCall("EndSort");
				MUNLOCK(ErrorMessageLock);
				retval = -1; goto RetRetval;
			}
			S->lPatch = 0;
			pp = S->SizeInFile[1];
			MULPOS(pp,sizeof(WORD));
#ifndef WITHPTHREADS
			if ( S == AT.S0 )
#endif
			{
				WORD oldLogHandle = AC.LogHandle;
				POSITION pppp;
				SETBASEPOSITION(pppp,0);
				SeekFile(S->file.handle,&pppp,SEEK_CUR);
				SeekFile(S->file.handle,&pp,SEEK_END);
				SeekFile(S->file.handle,&pppp,SEEK_SET);
				if ( AC.LogHandle >= 0 && AM.LogType ) AC.LogHandle = -1;
				WriteStats(&pp,(WORD)1);
				AC.LogHandle = oldLogHandle;
				UpdateMaxSize();
			}
		}
		else {
			S->Patches[S->lPatch++] = S->lFill;
		    to = (WORD *)(((UBYTE *)(S->lFill)) + AM.MaxTer);
			if ( tover > 0 ) {
				ss = S->sPointer;
				while ( ( t = *ss++ ) != 0 ) {
					j = *t;
					if ( j < 0 ) j = t[1] + 2;
					while ( --j >= 0 ) *to++ = *t++;
				}
			}
			*to++ = 0;
			S->lFill = to;
			if ( S->file.handle < 0 ) {
				if ( MergePatches(2) ) {
					MLOCK(ErrorMessageLock);
					MesCall("EndSort");
					MUNLOCK(ErrorMessageLock);
					retval = -1; goto RetRetval;
				}
				if ( S == AT.S0 ) {
					pp = S->SizeInFile[2];
					MULPOS(pp,sizeof(WORD));
#ifdef WITHPTHREADS
					if ( AS.MasterSort && ( fout == AR.outfile ) ) goto RetRetval;
#endif
					WriteStats(&pp,2);
					UpdateMaxSize();
				}
				else {
					if ( par == 2 && newout->handle >= 0 ) {
						POSITION zeropos;
						PUTZERO(zeropos);
#ifdef ALLLOCK
						LOCK(newout->pthreadslock);
#endif
						SeekFile(newout->handle,&zeropos,SEEK_SET);
						to = (WORD *)Malloc1(BASEPOSITION(newout->filesize)+sizeof(WORD)*2
								,"$-buffer reading");
						if ( AN.tryterm > 0 ) AN.tryterm = 0;
						if ( ( retval = ReadFile(newout->handle,(UBYTE *)to,BASEPOSITION(newout->filesize)) ) !=
								BASEPOSITION(newout->filesize) ) {
							MLOCK(ErrorMessageLock);
							MesPrint("Error reading information for $ variable");
							MUNLOCK(ErrorMessageLock);
							M_free(to,"$-buffer reading");
							retval = -1;
						}
						else {
							*((WORD **)buffer) = to;
							retval /= sizeof(WORD);
						}
#ifdef ALLLOCK
						UNLOCK(newout->pthreadslock);
#endif
					}
					else if ( newout->handle >= 0 ) {	/* output too large */
TooLarge:
						MLOCK(ErrorMessageLock);
						MesPrint("(1)Output should fit inside a single term. Increase MaxTermSize?");
						MesCall("EndSort");
						MUNLOCK(ErrorMessageLock);
						retval = -1; goto RetRetval;
					}
					else {
						t = newout->PObuffer;
						if ( par == 2 ) {
							jj = newout->POfill - t;
							if ( AN.tryterm > 0 && ( (jj+2)*sizeof(WORD) < (size_t)(AM.MaxTer) ) ) {
								to = TermMalloc("$-sort space");
							}
							else {
								LONG allocsp = jj+2;
								if ( allocsp < MINALLOC ) allocsp = MINALLOC;
								allocsp = ((allocsp+7)/8)*8;
								to = (WORD *)Malloc1(allocsp*sizeof(WORD),"$-sort space");
								if ( AN.tryterm > 0 ) AN.tryterm = 0;
							}
							*((WORD **)buffer) = to;
							NCOPY(to,t,jj);
						}
						else {
							j = newout->POfill - t;
							to = buffer;
							if ( to >= AT.WorkSpace && to < AT.WorkTop && to+j > AT.WorkTop )
								goto WorkSpaceError;
							if ( j > AM.MaxTer ) goto TooLarge;
							NCOPY(to,t,j);
						}
					}
				}
				goto RetRetval;
			}
			if ( MergePatches(1) ) { /* --> SortFile */
				MLOCK(ErrorMessageLock);
				MesCall("EndSort");
				MUNLOCK(ErrorMessageLock);
				retval = -1; goto RetRetval;
			}
			UpdateMaxSize();
			pp = S->SizeInFile[1];
			MULPOS(pp,sizeof(WORD));
#ifndef WITHPTHREADS
			if ( S == AT.S0 )
#endif
			{
				WORD oldLogHandle = AC.LogHandle;
				POSITION pppp;
				SETBASEPOSITION(pppp,0);
				SeekFile(S->file.handle,&pppp,SEEK_CUR);
				SeekFile(S->file.handle,&pp,SEEK_END);
				SeekFile(S->file.handle,&pppp,SEEK_SET);
				if ( AC.LogHandle >= 0 && AM.LogType ) AC.LogHandle = -1;
				WriteStats(&pp,(WORD)1);
				AC.LogHandle = oldLogHandle;
			}
#ifdef WITHERRORXXX
			if ( S != AT.S0 ) {
/*
				This is wrong! We have sorted to the sort file.
				Things are not sitting in the output yet.
*/
				if ( newout->handle >= 0 ) goto TooLarge;
				t = newout->PObuffer;
				j = newout->POfill - t;
				to = buffer;
				if ( to >= AT.WorkSpace && to < AT.WorkTop && to+j > AT.WorkTop )
					goto WorkSpaceError;
				if ( j > AM.MaxTer ) goto TooLarge;
				NCOPY(to,t,j);
				goto RetRetval;
			}
#endif
		}
	}
	if ( S->file.handle >= 0 ) {
#ifdef GZIPDEBUG
		MLOCK(ErrorMessageLock);
		MesPrint("%w EndSort: fPatchN = %d, lPatch = %d, position = %12p"
			,S->fPatchN,S->lPatch,&(S->fPatches[S->fPatchN]));
		MUNLOCK(ErrorMessageLock);
#endif
		if ( S->lPatch <= 0 ) {
			StageSort(&(S->file));
			position = S->fPatches[S->fPatchN];
			ss = S->sPointer;
			if ( *ss ) {
#ifdef WITHZLIB
				*AR.CompressPointer = 0;
				if ( S == AT.S0 && AR.NoCompress == 0 && AR.gzipCompress > 0 )
					S->fpcompressed[S->fPatchN] = 1;
				else
					S->fpcompressed[S->fPatchN] = 0;
				SetupOutputGZIP(&(S->file));
#endif
				while ( ( t = *ss++ ) != 0 ) {
					if ( PutOut(BHEAD t,&position,&(S->file),1) < 0 ) {
						retval = -1; goto RetRetval;
					}
				}
				if ( FlushOut(&position,&(S->file),1) ) {
					retval = -1; goto RetRetval;
				}
				++(S->fPatchN);
				S->fPatches[S->fPatchN] = position;
				UpdateMaxSize();
#ifdef GZIPDEBUG
				MLOCK(ErrorMessageLock);
				MesPrint("%w EndSort+: fPatchN = %d, lPatch = %d, position = %12p"
					,S->fPatchN,S->lPatch,&(S->fPatches[S->fPatchN]));
				MUNLOCK(ErrorMessageLock);
#endif
			}
		}
		AR.Stage4Name = 0;
#ifdef WITHPTHREADS
		if ( AS.MasterSort && AC.ThreadSortFileSynch ) {
			if ( S->file.handle >= 0 ) {
				SynchFile(S->file.handle);
			}
		}
#endif
		UpdateMaxSize();
		if ( MergePatches(0) ) {
			MLOCK(ErrorMessageLock);
			MesCall("EndSort");
			MUNLOCK(ErrorMessageLock);
			retval = -1; goto RetRetval;
		}
		S->stage4 = 0;
#ifdef WITHPTHREADS
		if ( AS.MasterSort && ( fout == AR.outfile ) ) goto RetRetval;
#endif
		pp = S->SizeInFile[0];
		MULPOS(pp,sizeof(WORD));
		WriteStats(&pp,2);
		UpdateMaxSize();
	}
RetRetval:

#ifdef WITHMPI
	/* NOTE: PF_EndSort has been changed such that it sets S->TermsLeft. (TU 30 Jun 2011) */
	if ( AR.sLevel == 0 && (PF.me == MASTER || PF.exprtodo >= 0) ) {
		Expressions[AR.CurExpr].counter = S->TermsLeft;
		Expressions[AR.CurExpr].size = pp;
	}
#else
	if ( AR.sLevel == 0 ) {
		Expressions[AR.CurExpr].counter = S->TermsLeft;
		Expressions[AR.CurExpr].size = pp;
	}/*if ( AR.sLevel == 0 )*/
#endif
/*:[25nov2003 mt]*/
	if ( S->file.handle >= 0 && ( par != 1 ) && ( par != 2 ) ) {
				/* sortfile is still open */
		UpdateMaxSize();
#ifdef WITHZLIB
		ClearSortGZIP(&(S->file));
#endif
		CloseFile(S->file.handle);
		S->file.handle = -1;
		remove(S->file.name);
#ifdef GZIPDEBUG
		MLOCK(ErrorMessageLock);
		MesPrint("%wEndSort: sortfile %s removed",S->file.name);
		MUNLOCK(ErrorMessageLock);
#endif
	}
	AR.outfile = oldoutfile;
	AR.sLevel--;
	if ( AR.sLevel >= 0 ) AT.SS = AN.FunSorts[AR.sLevel];
	if ( par == 1 ) {
		if ( retval < 0 ) {
			UpdateMaxSize();
			if ( newout ) {
				DeAllocFileHandle(newout);
				newout = 0;
			}
		}
		else if ( newout ) {
		  if ( newout->handle >= 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("(2)Output should fit inside a single term. Increase MaxTermSize?");
			MesCall("EndSort");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		  }
		  else if ( newout->POfill > newout->PObuffer ) {
/*
			Here we have to copy the contents of the 'file' into
			the buffer. We assume that this buffer lies in the WorkSpace.
			Hence
*/
			j = newout->POfill-newout->PObuffer;
			if ( buffer >= AT.WorkSpace && buffer < AT.WorkTop && buffer+j > AT.WorkTop )
				goto WorkSpaceError;
			else {
				to = buffer; t = newout->PObuffer;
				while ( j-- > 0 ) *to++ = *t++;
			}
			UpdateMaxSize();
		  }
		  DeAllocFileHandle(newout);
		  newout = 0;
		}
	}
	else if ( par == 2 ) {
		if ( newout ) {
			if ( retval == 0 ) {
			  if ( newout->handle >= 0 ) {
/*
				output resides at the moment in a file
				Find the size, make a buffer, copy into the buffer and clean up.
*/
				POSITION zeropos;
				PUTZERO(position);
#ifdef ALLLOCK
				LOCK(newout->pthreadslock);
#endif
				SeekFile(newout->handle,&position,SEEK_END);
				PUTZERO(zeropos);
				SeekFile(newout->handle,&zeropos,SEEK_SET);
				to = (WORD *)Malloc1(BASEPOSITION(position)+sizeof(WORD)*3
						,"$-buffer reading");
				if ( AN.tryterm > 0 ) AN.tryterm = 0;
				if ( ( retval = ReadFile(newout->handle,(UBYTE *)to,BASEPOSITION(position)) ) !=
				BASEPOSITION(position) ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Error reading information for $ variable");
					MUNLOCK(ErrorMessageLock);
					M_free(to,"$-buffer reading");
					retval = -1;
				}
				else {
					*((WORD **)buffer) = to;
					retval /= sizeof(WORD);
				}
#ifdef ALLLOCK
				UNLOCK(newout->pthreadslock);
#endif
			  }
			  else {
/*
				output resides in the cache buffer and the file was never opened
*/
				LONG wsiz = newout->POfill - newout->PObuffer;
				if ( AN.tryterm > 0 && ( (wsiz+2)*sizeof(WORD) < (size_t)(AM.MaxTer) ) ) {
					to = TermMalloc("$-sort space");
				}
				else {
					LONG allocsp = wsiz+2;
					if ( allocsp < MINALLOC ) allocsp = MINALLOC;
					allocsp = ((allocsp+7)/8)*8;
					to = (WORD *)Malloc1(allocsp*sizeof(WORD),"$-buffer reading");
					if ( AN.tryterm > 0 ) AN.tryterm = 0;
				}
				*((WORD **)buffer) = to; t = newout->PObuffer;
				retval = wsiz;
				NCOPY(to,t,wsiz);
			  }
			}
			UpdateMaxSize();
			DeAllocFileHandle(newout);
			newout = 0;
		}
	}
	else {
		if ( newout ) {
			DeAllocFileHandle(newout);
			newout = 0;
		}
	}
/*
	if ( AR.sLevel < 0 ) {
		MesPrint(" number of calls to compare was %l",numcompares);
	}
*/
	return(retval);
WorkSpaceError:
	MLOCK(ErrorMessageLock);
	MesWork();
	MesCall("EndSort");
	MUNLOCK(ErrorMessageLock);
	Terminate(-1);
	return(-1);
}

/*
 		#] EndSort : 
 		#[ PutIn :					LONG PutIn(handle,position,buffer,take,npat)
*/
/**
 *	Reads a new patch from position in file handle.
 *	It is put at buffer, anything after take is moved forward.
 *	This would be part of a term that hasn't been used yet.
 *	Because of this there should be some space before the start of the buffer
 *
 *	@param file     The file system from which to read
 *	@param position The position from which to read
 *	@param buffer   The buffer into which to read
 *	@param take     The unused tail should be moved before the buffer
 *	@param npat		The number of the patch. Is needed if the information
 *	                was compressed with gzip, because each patch has its
 *	                own independent gzip encoding.
 */

LONG PutIn(FILEHANDLE *file, POSITION *position, WORD *buffer, WORD **take, int npat)
{
	LONG i, RetCode;
	WORD *from, *to;
#ifndef WITHZLIB
	DUMMYUSE(npat);
#endif
	from = buffer + ( file->POsize * sizeof(UBYTE) )/sizeof(WORD);
	i = from - *take;
	if ( i*((LONG)(sizeof(WORD))) > AM.MaxTer ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Problems in PutIn");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	to = buffer;
	while ( --i >= 0 ) *--to = *--from;
	*take = to;
#ifdef WITHZLIB
	if ( ( RetCode = FillInputGZIP(file,position,(UBYTE *)buffer
									,file->POsize,npat) ) < 0 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("PutIn: We have RetCode = %x while reading %x bytes",
			RetCode,file->POsize);
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
#else
#ifdef ALLLOCK
	LOCK(file->pthreadslock);
#endif
	SeekFile(file->handle,position,SEEK_SET);
	if ( ( RetCode = ReadFile(file->handle,(UBYTE *)buffer,file->POsize) ) < 0 ) {
#ifdef ALLLOCK
		UNLOCK(file->pthreadslock);
#endif
		MLOCK(ErrorMessageLock);
		MesPrint("PutIn: We have RetCode = %x while reading %x bytes",
			RetCode,file->POsize);
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
#ifdef ALLLOCK
	UNLOCK(file->pthreadslock);
#endif
#endif
	return(RetCode);
}

/*
 		#] PutIn : 
 		#[ Sflush :					WORD Sflush(file)
*/
/**
 *	Puts the contents of a buffer to output
 *	Only to be used when there is a single patch in the large buffer.
 *
 *	@param fi  The filesystem (or its cache) to which the patch should be written
 */

WORD Sflush(FILEHANDLE *fi)
{
	LONG size, RetCode;
#ifdef WITHZLIB
	GETIDENTITY
	int dobracketindex = 0;
	if ( AR.sLevel <= 0 && Expressions[AR.CurExpr].newbracketinfo
		&& ( fi == AR.outfile || fi == AR.hidefile ) ) dobracketindex = 1;
#endif
	if ( fi->handle < 0 ) {
		if ( ( RetCode = CreateFile(fi->name) ) >= 0 ) {
#ifdef GZIPDEBUG
			MLOCK(ErrorMessageLock);
			MesPrint("%w Sflush created scratch file %s",fi->name);
			MUNLOCK(ErrorMessageLock);
#endif
			fi->handle = (WORD)RetCode;
			PUTZERO(fi->filesize);
			PUTZERO(fi->POposition);
		}
		else {
			MLOCK(ErrorMessageLock);
			MesPrint("Cannot create scratch file %s",fi->name);
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
	}
#ifdef WITHZLIB
	if ( AT.SS == AT.S0 && !AR.NoCompress && AR.gzipCompress > 0
	&& dobracketindex == 0 ) {
		if ( FlushOutputGZIP(fi) ) return(-1);
		fi->POfill = fi->PObuffer;
	}
	else
#endif
	{
#ifdef ALLLOCK
	  LOCK(fi->pthreadslock);
#endif
	  size = (fi->POfill-fi->PObuffer)*sizeof(WORD);
	  SeekFile(fi->handle,&(fi->POposition),SEEK_SET);
	  if ( WriteFile(fi->handle,(UBYTE *)(fi->PObuffer),size) != size ) {
#ifdef ALLLOCK
		UNLOCK(fi->pthreadslock);
#endif
		MLOCK(ErrorMessageLock);
		MesPrint("Write error while finishing sort. Disk full?");
		MUNLOCK(ErrorMessageLock);
		return(-1);
	  }
	  ADDPOS(fi->filesize,size);
	  ADDPOS(fi->POposition,size);
	  fi->POfill = fi->PObuffer;
#ifdef ALLLOCK
	  UNLOCK(fi->pthreadslock);
#endif
	}
	return(0);
}

/*
 		#] Sflush : 
 		#[ PutOut :					WORD PutOut(term,position,file,ncomp)
*/
/**
 *	Routine writes one term to file handle at position. It returns
 *	the new value of the position.
 *
 *	NOTE:
 *		For 'final output' we may have to index the brackets.
 *		See the struct BRACKETINDEX.
 *		We should maintain:
 *		1: a list with brackets
 *			array with the brackets
 *		2: a list of objects of type BRACKETINDEX. It contains
 *			array with either pointers or offsets to the list of brackets.
 *			starting positions in the file.
 *		The index may be tied to a maximum size. In that case we may have to
 *		prune the list occasionally.
 *
 *	@param term     The term to be written
 *	@param position The position in the file. Afterwards it is updated
 *	@param fi       The file (or its cache) to which should be written
 *	@param ncomp    Information about what type of compression should be used
 */

WORD PutOut(PHEAD WORD *term, POSITION *position, FILEHANDLE *fi, WORD ncomp)
{
	GETBIDENTITY
	WORD i, *p, ret, *r, *rr, j, k, first;
	int dobracketindex = 0;
	LONG RetCode;

	if ( AT.SS != AT.S0 ) {
/*
		For this case no compression should be used
*/
		if ( ( i = *term ) <= 0 ) return(0);
		ret = i;
		ADDPOS(*position,i*sizeof(WORD));
		p = fi->POfill;
		do {
			if ( p >= fi->POstop ) {
				if ( fi->handle < 0 ) {
					if ( ( RetCode = CreateFile(fi->name) ) >= 0 ) {
#ifdef GZIPDEBUG
						MLOCK(ErrorMessageLock);
						MesPrint("%w PutOut created sortfile %s",fi->name);
						MUNLOCK(ErrorMessageLock);
#endif
						fi->handle = (WORD)RetCode;
						PUTZERO(fi->filesize);
						PUTZERO(fi->POposition);
/*
						Should not be here anymore?
#ifdef WITHZLIB
						fi->ziobuffer = 0;
#endif
*/
					}
					else {
						MLOCK(ErrorMessageLock);
						MesPrint("Cannot create scratch file %s",fi->name);
						MUNLOCK(ErrorMessageLock);
						return(-1);
					}
				}
#ifdef ALLLOCK
				LOCK(fi->pthreadslock);
#endif
				if ( fi == AR.hidefile ) {
					LOCK(AS.inputslock);
				}
				SeekFile(fi->handle,&(fi->POposition),SEEK_SET);
				if ( ( RetCode = WriteFile(fi->handle,(UBYTE *)(fi->PObuffer),fi->POsize) ) != fi->POsize ) {
					if ( fi == AR.hidefile ) {
						UNLOCK(AS.inputslock);
					}
#ifdef ALLLOCK
					UNLOCK(fi->pthreadslock);
#endif
					MLOCK(ErrorMessageLock);
					MesPrint("Write error during sort. Disk full?");
					MesPrint("Attempt to write %l bytes on file %d at position %15p",
								fi->POsize,fi->handle,&(fi->POposition));
					MesPrint("RetCode = %l, Buffer address = %l",RetCode,(LONG)(fi->PObuffer));
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
				ADDPOS(fi->filesize,fi->POsize);
				p = fi->PObuffer;
				ADDPOS(fi->POposition,fi->POsize);
				if ( fi == AR.hidefile ) {
					UNLOCK(AS.inputslock);
				}
#ifdef ALLLOCK
				UNLOCK(fi->pthreadslock);
#endif
#ifdef WITHPTHREADS
				if ( AS.MasterSort && AC.ThreadSortFileSynch ) {
					if ( fi->handle >= 0 ) SynchFile(fi->handle);
				}
#endif
			} 
			*p++ = *term++;
		} while ( --i > 0 );
		fi->POfull = fi->POfill = p;
		return(ret);
	}
	if ( ( AP.PreDebug & DUMPOUTTERMS ) == DUMPOUTTERMS ) {
			MLOCK(ErrorMessageLock);
#ifdef WITHPTHREADS
			sprintf((char *)(THRbuf),"PutOut(%d)",AT.identity);
			PrintTerm(term,(char *)(THRbuf));
#else
			PrintTerm(term,"PutOut");
#endif
			MesPrint("ncomp = %d, AR.NoCompress = %d, AR.sLevel = %d",ncomp,AR.NoCompress,AR.sLevel);
			MesPrint("File %s, position %p",fi->name,position);
			MUNLOCK(ErrorMessageLock);
	}

	if ( AR.sLevel <= 0 && Expressions[AR.CurExpr].newbracketinfo
		&& ( fi == AR.outfile || fi == AR.hidefile ) ) dobracketindex = 1;
	r = rr = AR.CompressPointer;
	first = j = k = ret = 0;
	if ( ( i = *term ) != 0 ) {
		if ( i < 0 ) {			/* Compressed term */
			i = term[1] + 2;
			if ( fi == AR.outfile || fi == AR.hidefile ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Ran into precompressed term");
				MUNLOCK(ErrorMessageLock);
				Crash();
				return(-1);
			}
		}
		else if ( !AR.NoCompress && ( ncomp > 0 ) && AR.sLevel <= 0 ) {	/* Must compress */
			if ( dobracketindex ) {
				PutBracketInIndex(BHEAD term,position);
			}
			j = *r++ - 1;
			p = term + 1;
			i--;
			if ( AR.PolyFun ) {
				WORD *polystop, *sa;
				sa = p + i;
				sa -= ABS(sa[-1]);
				polystop = p;
				while ( polystop < sa && *polystop != AR.PolyFun ) {
					polystop += polystop[1];
				}
				if ( polystop < sa ) {
					if ( AR.PolyFunType == 2 ) polystop[2] &= ~MUSTCLEANPRF;
					while ( i > 0 && j > 0 && *p == *r && p < polystop ) {
						i--; j--; k--; p++; r++;
					}
				}
				else {
					while ( i > 0 && j > 0 && *p == *r && p < sa ) { i--; j--; k--; p++; r++; }
				}
			}
			else {
				WORD *sa;
				sa = p + i;
				sa -= ABS(sa[-1]);
				while ( i > 0 && j > 0 && *p == *r && p < sa ) { i--; j--; k--; p++; r++; }
			}
			if ( k > -2 ) {
nocompress:
				j = i = *term;
				k = 0;
				p = term;
				r = rr;
				NCOPY(r,p,j);
			}
			else {
				*rr = *term;
				term = p;
				j = i;
				NCOPY(r,p,j);
				j = i;
				i += 2;
				first = 2;
			}
/*					Sabotage getting into the coefficient next time */
			r[-(ABS(r[-1]))] = 0;
			if ( r >= AR.ComprTop ) {
				MLOCK(ErrorMessageLock);
				MesPrint("CompressSize of %10l is insufficient",AM.CompressSize);
				MUNLOCK(ErrorMessageLock);
				Crash();
				return(-1);
			}
		}
		else if ( !AR.NoCompress && ( ncomp < 0 ) && AR.sLevel <= 0 ) {
				/* No compress but put in compress buffer anyway */
			if ( dobracketindex ) {
				PutBracketInIndex(BHEAD term,position);
			}
			j = *r++ - 1;
			p = term + 1;
			i--;
			if ( AR.PolyFun ) {
				WORD *polystop, *sa;
				sa = p + i;
				sa -= ABS(sa[-1]);
				polystop = p;
				while ( polystop < sa && *polystop != AR.PolyFun ) {
					polystop += polystop[1];
				}
				if ( polystop < sa ) {
					if ( AR.PolyFunType == 2 ) polystop[2] &= ~MUSTCLEANPRF;
					while ( i > 0 && j > 0 && *p == *r && p < polystop ) {
						i--; j--; k--; p++; r++;
					}
				}
				else {
					while ( i > 0 && j > 0 && *p == *r ) { i--; j--; k--; p++; r++; }
				}
			}
			else {
				while ( i > 0 && j > 0 && *p == *r ) { i--; j--; k--; p++; r++; }
			}
			goto nocompress;
		}
		else {
			if ( AR.PolyFunType == 2 ) {
				WORD *t, *tstop;
				tstop = term + *term;
				tstop -= ABS(tstop[-1]);
				t = term+1;
				while ( t < tstop ) {
					if ( *t == AR.PolyFun ) {
						t[2] &= ~MUSTCLEANPRF;
					}
					t += t[1];
				}
			}
			if ( dobracketindex ) {
				PutBracketInIndex(BHEAD term,position);
			}
		}
		ret = i;
		ADDPOS(*position,i*sizeof(WORD));
		p = fi->POfill;
		do {
			if ( p >= fi->POstop ) {
#ifdef WITHMPI /* [16mar1998 ar] */
			  if ( PF.me != MASTER && AR.sLevel <= 0 && (fi == AR.outfile || fi == AR.hidefile) && PF.parallel && PF.exprtodo < 0 ) {
				PF_BUFFER *sbuf = PF.sbuf;
				sbuf->fill[sbuf->active] = fi->POstop;
				PF_ISendSbuf(MASTER,PF_BUFFER_MSGTAG);
				p = fi->PObuffer = fi->POfill = fi->POfull =
				  sbuf->buff[sbuf->active];
				fi->POstop = sbuf->stop[sbuf->active];
			  }
			  else
#endif /* WITHMPI [16mar1998 ar] */
			  {
				if ( fi->handle < 0 ) {
					if ( ( RetCode = CreateFile(fi->name) ) >= 0 ) {
#ifdef GZIPDEBUG
						MLOCK(ErrorMessageLock);
						MesPrint("%w PutOut created sortfile %s",fi->name);
						MUNLOCK(ErrorMessageLock);
#endif
						fi->handle = (WORD)RetCode;
						PUTZERO(fi->filesize);
						PUTZERO(fi->POposition);
/*
						Should not be here?
#ifdef WITHZLIB
						fi->ziobuffer = 0;
#endif
*/
					}
					else {
						MLOCK(ErrorMessageLock);
						MesPrint("Cannot create scratch file %s",fi->name);
						MUNLOCK(ErrorMessageLock);
						return(-1);
					}
				}
#ifdef WITHZLIB
				if ( !AR.NoCompress && ncomp > 0 && AR.gzipCompress > 0
					&& dobracketindex == 0 && fi->zsp != 0 ) {
					fi->POfill = p;
					if ( PutOutputGZIP(fi) ) return(-1);
					p = fi->PObuffer;
				}
				else
#endif
				{
#ifdef ALLLOCK
				  LOCK(fi->pthreadslock);
#endif
				  if ( fi == AR.hidefile ) {
					LOCK(AS.inputslock);
				  }
				  SeekFile(fi->handle,&(fi->POposition),SEEK_SET);
				  if ( ( RetCode = WriteFile(fi->handle,(UBYTE *)(fi->PObuffer),fi->POsize) ) != fi->POsize ) {
					if ( fi == AR.hidefile ) {
						UNLOCK(AS.inputslock);
					}
#ifdef ALLLOCK
					UNLOCK(fi->pthreadslock);
#endif
					MLOCK(ErrorMessageLock);
					MesPrint("Write error during sort. Disk full?");
					MesPrint("Attempt to write %l bytes on file %d at position %15p",
								fi->POsize,fi->handle,&(fi->POposition));
					MesPrint("RetCode = %l, Buffer address = %l",RetCode,(LONG)(fi->PObuffer));
					MUNLOCK(ErrorMessageLock);
					return(-1);
				  }
				  ADDPOS(fi->filesize,fi->POsize);
				  p = fi->PObuffer;
				  ADDPOS(fi->POposition,fi->POsize);
				  if ( fi == AR.hidefile ) {
					UNLOCK(AS.inputslock);
				  }
#ifdef ALLLOCK
				  UNLOCK(fi->pthreadslock);
#endif
#ifdef WITHPTHREADS
				  if ( AS.MasterSort && AC.ThreadSortFileSynch ) {
					if ( fi->handle >= 0 ) SynchFile(fi->handle);
				  }
#endif
				}
			  }
			} 
			if ( first ) {
				if ( first == 2 ) *p++ = k;
				else *p++ = j;
				first--;
			}
			else *p++ = *term++;
/*
			if ( AP.DebugFlag ) {
				TalToLine((UWORD)(p[-1])); TokenToLine((UBYTE *)"  ");
			}
*/
		} while ( --i > 0 );
		fi->POfull = fi->POfill = p;
	}
/*
	if ( AP.DebugFlag ) {
		AO.OutSkip = 0;
		FiniLine();
	}
*/
	return(ret);
}

/*
 		#] PutOut : 
 		#[ FlushOut :				WORD FlushOut(position,file,compr)
*/
/**
 *	Completes output to an output file and writes the trailing zero.
 *
 *	@param position The position in the file after writing
 *	@param fi       The file (or its cache)
 *	@param compr	Indicates whether there should be compression with gzip.
 *	@return   Regular conventions (OK -> 0).
 */

WORD FlushOut(POSITION *position, FILEHANDLE *fi, int compr)
{
	GETIDENTITY
	LONG size, RetCode;
	int dobracketindex = 0;
#ifndef WITHZLIB
	DUMMYUSE(compr);
#endif
	if ( AR.sLevel <= 0 && Expressions[AR.CurExpr].newbracketinfo
		&& ( fi == AR.outfile || fi == AR.hidefile ) ) dobracketindex = 1;
#ifdef WITHMPI /* [16mar1998 ar] */
	if ( PF.me != MASTER && AR.sLevel <= 0 && (fi == AR.outfile || fi == AR.hidefile) && PF.parallel && PF.exprtodo < 0 ) {
		PF_BUFFER *sbuf = PF.sbuf;
		if ( fi->POfill >= fi->POstop ){
		  sbuf->fill[sbuf->active] = fi->POstop;
		  PF_ISendSbuf(MASTER,PF_BUFFER_MSGTAG);
		  fi->POfull = fi->POfill = fi->PObuffer = sbuf->buff[sbuf->active];
		  fi->POstop = sbuf->stop[sbuf->active];
		}
		*(fi->POfill)++ = 0;
		sbuf->fill[sbuf->active] = fi->POfill;
		PF_ISendSbuf(MASTER,PF_ENDBUFFER_MSGTAG);
		fi->PObuffer = fi->POfill = fi->POfull = sbuf->buff[sbuf->active];
		fi->POstop = sbuf->stop[sbuf->active];
		return(0);
	}
#endif /* WITHMPI [16mar1998 ar] */
	if ( fi->POfill >= fi->POstop ) {
		if ( fi->handle < 0 ) {
			if ( ( RetCode = CreateFile(fi->name) ) >= 0 ) {
#ifdef GZIPDEBUG
				MLOCK(ErrorMessageLock);
				MesPrint("%w FlushOut created scratch file %s",fi->name);
				MUNLOCK(ErrorMessageLock);
#endif
				PUTZERO(fi->filesize);
				PUTZERO(fi->POposition);
				fi->handle = (WORD)RetCode;
/*
				Should not be here?
#ifdef WITHZLIB
				fi->ziobuffer = 0;
#endif
*/
			}
			else {
				MLOCK(ErrorMessageLock);
				MesPrint("Cannot create scratch file %s",fi->name);
				MUNLOCK(ErrorMessageLock);
				return(-1);
			}
		}
#ifdef WITHZLIB
		if ( AT.SS == AT.S0 && !AR.NoCompress && AR.gzipCompress > 0
		&& dobracketindex == 0 && ( compr > 0 ) && fi->zsp != 0 ) {
			if ( PutOutputGZIP(fi) ) return(-1);
			fi->POfill = fi->PObuffer;
		}
		else
#endif
		{
#ifdef ALLLOCK
		  LOCK(fi->pthreadslock);
#endif
		  if ( fi == AR.hidefile ) {
			LOCK(AS.inputslock);
		  }
		  SeekFile(fi->handle,&(fi->POposition),SEEK_SET);
		  if ( ( RetCode = WriteFile(fi->handle,(UBYTE *)(fi->PObuffer),fi->POsize) ) != fi->POsize ) {
#ifdef ALLLOCK
			UNLOCK(fi->pthreadslock);
#endif
			if ( fi == AR.hidefile ) {
				UNLOCK(AS.inputslock);
			}
			MLOCK(ErrorMessageLock);
			MesPrint("Write error while sorting. Disk full?");
			MesPrint("Attempt to write %l bytes on file %d at position %15p",
						fi->POsize,fi->handle,&(fi->POposition));
			MesPrint("RetCode = %l, Buffer address = %l",RetCode,(LONG)(fi->PObuffer));
			MUNLOCK(ErrorMessageLock);
			return(-1);
		  }
		  ADDPOS(fi->filesize,fi->POsize);
		  fi->POfill = fi->PObuffer;
		  ADDPOS(fi->POposition,fi->POsize);
		  if ( fi == AR.hidefile ) {
			UNLOCK(AS.inputslock);
		  }
#ifdef ALLLOCK
		  UNLOCK(fi->pthreadslock);
#endif
#ifdef WITHPTHREADS
		  if ( AS.MasterSort && AC.ThreadSortFileSynch && fi != AR.hidefile ) {
			if ( fi->handle >= 0 ) SynchFile(fi->handle);
		  }
#endif
		}
	} 
	*(fi->POfill)++ = 0;
	fi->POfull = fi->POfill;
/*
	{
		UBYTE OutBuf[140];
		if ( AP.DebugFlag ) {
			AO.OutFill = AO.OutputLine = OutBuf;
			AO.OutSkip = 3;
			FiniLine();
			TokenToLine((UBYTE *)"End of expression written");
			FiniLine();
		}
	}
*/
	size = (fi->POfill-fi->PObuffer)*sizeof(WORD);
	if ( fi->handle >= 0 ) {
#ifdef WITHZLIB
		if ( AT.SS == AT.S0 && !AR.NoCompress && AR.gzipCompress > 0
		&& dobracketindex == 0 && ( compr > 0 ) && fi->zsp != 0 ) {
			if ( FlushOutputGZIP(fi) ) return(-1);
			fi->POfill = fi->PObuffer;
		}
		else
#endif
		{
#ifdef ALLLOCK
		  LOCK(fi->pthreadslock);
#endif
		  if ( fi == AR.hidefile ) {
			LOCK(AS.inputslock);
		  }
		  SeekFile(fi->handle,&(fi->POposition),SEEK_SET);
/*
		  MesPrint("FlushOut: writing %l bytes to position %12p",size,&(fi->POposition));
*/
		  if ( ( RetCode = WriteFile(fi->handle,(UBYTE *)(fi->PObuffer),size) ) != size ) {
#ifdef ALLLOCK
			UNLOCK(fi->pthreadslock);
#endif
			if ( fi == AR.hidefile ) {
				UNLOCK(AS.inputslock);
			}
			MLOCK(ErrorMessageLock);
			MesPrint("Write error while finishing sorting. Disk full?");
			MesPrint("Attempt to write %l bytes on file %d at position %15p",
						size,fi->handle,&(fi->POposition));
			MesPrint("RetCode = %l, Buffer address = %l",RetCode,(LONG)(fi->PObuffer));
			MUNLOCK(ErrorMessageLock);
			return(-1);
		  }
		  ADDPOS(fi->filesize,size);
		  ADDPOS(fi->POposition,size);
		  fi->POfill = fi->PObuffer;
		  if ( fi == AR.hidefile ) {
			UNLOCK(AS.inputslock);
		  }
#ifdef ALLLOCK
		  UNLOCK(fi->pthreadslock);
#endif
#ifdef WITHPTHREADS
		  if ( AS.MasterSort && AC.ThreadSortFileSynch ) {
			if ( fi->handle >= 0 ) SynchFile(fi->handle);
		  }
#endif
		}
	}
	if ( dobracketindex ) {
		BRACKETINFO *b = Expressions[AR.CurExpr].newbracketinfo;
		if ( b->indexfill > 0 ) {
			DIFPOS(b->indexbuffer[b->indexfill-1].next,*position,Expressions[AR.CurExpr].onfile);
		}
	}
#ifdef WITHZLIB
	if ( AT.SS == AT.S0 && !AR.NoCompress && AR.gzipCompress > 0
		&& dobracketindex == 0 && ( compr > 0 ) && fi->zsp != 0 ) {
		PUTZERO(*position);
		if ( fi->handle >= 0 ) {
#ifdef ALLLOCK
			LOCK(fi->pthreadslock);
#endif
			SeekFile(fi->handle,position,SEEK_END);
#ifdef ALLLOCK
			UNLOCK(fi->pthreadslock);
#endif
		}
		else {
			ADDPOS(*position,((UBYTE *)fi->POfill-(UBYTE *)fi->PObuffer));
		}
	}
	else
#endif
	{
		ADDPOS(*position,sizeof(WORD));
	}
	return(0);
}

/*
 		#] FlushOut : 
 		#[ AddCoef :				WORD AddCoef(pterm1,pterm2)
*/
/**
 *		Adds the coefficients of the terms *ps1 and *ps2.
 *		The problem comes when there is not enough space for a new
 *		longer coefficient. First a local solution is tried.
 *		If this is not succesfull we need to move terms around.
 *		The possibility of a garbage collection should not be
 *		ignored, as avoiding this costs very much extra space which
 *		is nearly wasted otherwise.
 *
 *		If the return value is zero the terms cancelled.
 *
 *		The resulting term is left in *ps1.
 */

WORD AddCoef(PHEAD WORD **ps1, WORD **ps2)
{
	GETBIDENTITY
	SORTING *S = AT.SS;
	WORD *s1, *s2;
	WORD l1, l2, i;
	WORD OutLen, *t, j;
	UWORD *OutCoef;
	OutCoef = AN.SoScratC;
	s1 = *ps1; s2 = *ps2;
	GETCOEF(s1,l1);
	GETCOEF(s2,l2);
	if ( AddRat(BHEAD (UWORD *)s1,l1,(UWORD *)s2,l2,OutCoef,&OutLen) ) {
		MLOCK(ErrorMessageLock);
		MesCall("AddCoef");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	if ( AN.ncmod != 0 ) {
		if ( ( AC.modmode & POSNEG ) != 0 ) {
			NormalModulus(OutCoef,&OutLen);
/*
			We had forgotten that this can also become smaller but the
			denominator isn't there. Correct in the other case
			17-may-2009 [JV]
*/
			j = ABS(OutLen); OutCoef[j] = 1;
			for ( i = 1; i < j; i++ ) OutCoef[j+i] = 0;
		}
		else if ( BigLong(OutCoef,OutLen,(UWORD *)AC.cmod,ABS(AN.ncmod)) >= 0 ) {
			SubPLon(OutCoef,OutLen,(UWORD *)AC.cmod,ABS(AN.ncmod),OutCoef,&OutLen);
			OutCoef[OutLen] = 1;
			for ( i = 1; i < OutLen; i++ ) OutCoef[OutLen+i] = 0;
		}
	}
	if ( !OutLen ) { *ps1 = *ps2 = 0; return(0); }
	OutLen *= 2;
	if ( OutLen < 0 ) i = - ( --OutLen );
	else			  i = ++OutLen;
	if ( l1 < 0 ) l1 = -l1;
	l1 *= 2; l1++;
	if ( i <= l1 ) {	/* Fits in 1 */
		l1 -= i;
		**ps1 -= l1;
		s2 = (WORD *)OutCoef;
		while ( --i > 0 ) *s1++ = *s2++;
		*s1++ = OutLen;
		while ( --l1 >= 0 ) *s1++ = 0;
		goto RegEnd;
	}
	if ( l2 < 0 ) l2 = -l2;
	l2 *= 2; l2++;
	if ( i <= l2 ) {	/* Fits in 2 */
		l2 -= i;
		**ps2 -= l2;
		s1 = (WORD *)OutCoef;
		while ( --i > 0 ) *s2++ = *s1++;
		*s2++ = OutLen;
		while ( --l2 >= 0 ) *s2++ = 0;
		*ps1 = *ps2;
		goto RegEnd;
	}

	/* Doesn't fit. Make a new term. */

	t = s1;
	s1 = *ps1;
	j = *s1++ + i - l1;		/* Space needed */
	if ( (S->sFill + j) >= S->sTop2 ) {
		GarbHand();

		s1 = *ps1;
		t = s1 + *s1 - 1;
		j = *s1++ + i - l1;		/* Space needed */
		l1 = *t;
		if ( l1 < 0 ) l1  = - l1;
		t -= l1-1;
	}
	s2 = S->sFill;
	*s2++ = j;
	while ( s1 < t ) *s2++ = *s1++;
	s1 = (WORD *)OutCoef;
	while ( --i > 0 ) *s2++ = *s1++;
	*s2++ = OutLen;
	*ps1 = S->sFill;
	S->sFill = s2;
RegEnd:
	*ps2 = 0;
	if ( **ps1 > AM.MaxTer/((LONG)(sizeof(WORD))) ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Term to complex after polynomial addition. MaxTermSize = %10l",
		AM.MaxTer/sizeof(WORD));
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	return(1);
}

/*
 		#] AddCoef : 
 		#[ AddPoly :				WORD AddPoly(pterm1,pterm2)
*/
/**
 *		Routine should be called when S->PolyWise != 0. It points then
 *		to the position of AR.PolyFun in both terms.
 *
 *		We add the contents of the arguments of the two polynomials.
 *		Special attention has to be given to special arguments.
 *		We have to reserve a space equal to the size of one term + the
 *		size of the argument of the other. The addition has to be done
 *		in this routine because not all objects are reentrant.
 *
 *		Newer addition (12-nov-2007).
 *		The PolyFun can have two arguments.
 *		In that case S->PolyFlag is 2 and we have to call the routine for
 *		adding rational polynomials.
 *		We have to be rather careful what happens with:
 *			The location of the output
 *			The order of the terms in the arguments
 *		At first we allow only univariate polynomials in the PolyFun.
 *		This restriction will be lifted a.s.a.p.
 *
 *		@param ps1 A pointer to the postion of the first term
 *		@param ps2 A pointer to the postion of the second term
 *		@return If zero the terms cancel. Otherwise the new term is in *ps1.
 */

WORD AddPoly(PHEAD WORD **ps1, WORD **ps2)
{
	GETBIDENTITY
	SORTING *S = AT.SS;
	WORD i;
	WORD *s1, *s2, *m, *w, *t, oldpw = S->PolyWise;
	s1 = *ps1 + S->PolyWise;
	s2 = *ps2 + S->PolyWise;
	w = AT.WorkPointer;
/*
	Add here the two arguments. Is a straight merge.
*/
	if ( S->PolyFlag == 2 && AR.PolyFunExp != 2 && AR.PolyFunExp != 3 ) {
		WORD **oldSplitScratch = AN.SplitScratch;
		LONG oldSplitScratchSize = AN.SplitScratchSize;
		LONG oldInScratch = AN.InScratch;
		WORD oldtype = AR.SortType;
		if ( (WORD *)((UBYTE *)w + AM.MaxTer) >= AT.WorkTop ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Program was adding polyratfun arguments");
			MesWork();
			MUNLOCK(ErrorMessageLock);
		}
		AR.SortType = SORTHIGHFIRST;
		S->PolyWise = 0;
		AN.SplitScratch = AN.SplitScratch1;
		AN.SplitScratchSize = AN.SplitScratchSize1;
		AN.InScratch = AN.InScratch1;
		poly_ratfun_add(BHEAD s1,s2);
		S->PolyWise = oldpw;
		AN.SplitScratch1 = AN.SplitScratch;
		AN.SplitScratchSize1 = AN.SplitScratchSize;
		AN.InScratch1 = AN.InScratch;
		AN.SplitScratch = oldSplitScratch;
		AN.SplitScratchSize = oldSplitScratchSize;
		AN.InScratch = oldInScratch;
		AT.WorkPointer = w;
		AR.SortType = oldtype;
		if ( w[1] <= FUNHEAD ||
			( w[FUNHEAD] == -SNUMBER && w[FUNHEAD+1] == 0 ) ) {
			*ps1 = *ps2 = 0; return(0);
		}
	}
	else {
		if ( w + s1[1] + s2[1] + 12 + ARGHEAD >= AT.WorkTop ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Program was adding polyfun arguments");
			MesWork();
			MUNLOCK(ErrorMessageLock);
		}
		AddArgs(BHEAD s1,s2,w);
	}
/*
	Now we need to store the result in a convenient place.
*/
	if ( w[1] <= FUNHEAD ) { *ps1 = *ps2 = 0; return(0); }
	if ( w[1] <= s1[1] || w[1] <= s2[1] ) {   /* Fits in place. */
		if ( w[1] > s1[1] ) {
			*ps1 = *ps2;
			s1 = s2;
		}
		t = s1 + s1[1];
		m = *ps1 + **ps1;
		i = w[1];
		NCOPY(s1,w,i);
		if ( s1 != t ) {
			while ( t < m ) *s1++ = *t++;
			**ps1 = WORDDIF(s1,(*ps1));
		}
		*ps2 = 0;
	}
	else {		/* Make new term */
#ifdef TESTGARB
		s2 = *ps2;
#endif
		*ps2 = 0;
		if ( (S->sFill + (**ps1 + w[1] - s1[1])) >= S->sTop2 ) {
#ifdef TESTGARB
			MesPrint("------Garbage collection-------");
#endif
			AT.WorkPointer += w[1];
			GarbHand();
			AT.WorkPointer = w;
			s1 = *ps1;
			if ( (S->sFill + (**ps1 + w[1] - s1[1])) >= S->sTop2 ) {
#ifdef TESTGARB
				UBYTE OutBuf[140];
				MLOCK(ErrorMessageLock);
				AO.OutFill = AO.OutputLine = OutBuf;
				AO.OutSkip = 3;
				FiniLine();
				i = *s2;
				while ( --i >= 0 ) {
					TalToLine((UWORD)(*s2++)); TokenToLine((UBYTE *)"  ");
				}
				FiniLine();
				AO.OutFill = AO.OutputLine = OutBuf;
				AO.OutSkip = 3;
				FiniLine();
				s2 = *ps1;
				i = *s2;
				while ( --i >= 0 ) {
					TalToLine((UWORD)(*s2++)); TokenToLine((UBYTE *)"  ");
				}
				FiniLine();
				AO.OutFill = AO.OutputLine = OutBuf;
				AO.OutSkip = 3;
				FiniLine();
				s2 = w;
				i = w[1];
				while ( --i >= 0 ) {
					TalToLine((UWORD)(*s2++)); TokenToLine((UBYTE *)"  ");
				}
				FiniLine();
				MesPrint("Please increase SmallExtension in %s",setupfilename);
				MUNLOCK(ErrorMessageLock);
#else
				MLOCK(ErrorMessageLock);
				MesPrint("Please increase SmallExtension in %s",setupfilename);
				MUNLOCK(ErrorMessageLock);
#endif
				Terminate(-1);
			}
		}
		t = *ps1;
		s2 = S->sFill;
		m = s2;
		i = S->PolyWise;
		NCOPY(s2,t,i);
		i = w[1];
		NCOPY(s2,w,i);
		t = t + t[1];
		w = *ps1 + **ps1;
		while ( t < w ) *s2++ = *t++;
		*m = WORDDIF(s2,m);
		*ps1 = m;
		S->sFill = s2;
		if ( *m > AM.MaxTer/((LONG)sizeof(WORD)) ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Term to complex after polynomial addition. MaxTermSize = %10l",
			AM.MaxTer/sizeof(WORD));
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
	}
	return(1);
}

/*
 		#] AddPoly : 
 		#[ AddArgs :				VOID AddArgs(arg1,arg2,to)
*/
 
#define INSLENGTH(x)  w[1] = FUNHEAD+ARGHEAD+x; w[FUNHEAD] = ARGHEAD+x;

/**
 *	Adds the arguments of two occurrences of the PolyFun.
 *	@param s1 Pointer to the first occurrence.
 *	@param s2 Pointer to the second occurrence.
 *	@param m  Pointer to where the answer should be.
 */

VOID AddArgs(PHEAD WORD *s1, WORD *s2, WORD *m)
{
	GETBIDENTITY
	WORD i1, i2;
	WORD *w = m, *mm, *t, *t1, *t2, *tstop1, *tstop2;
	WORD tempterm[8+FUNHEAD];

	*m++ = AR.PolyFun; *m++ = 0; FILLFUN(m)
	*m++ = 0; *m++ = 0; FILLARG(m)
	if ( s1[FUNHEAD] < 0 || s2[FUNHEAD] < 0 ) {
		if ( s1[FUNHEAD] < 0 ) {
			if ( s2[FUNHEAD] < 0 ) {	/* Both are special */
				if ( s1[FUNHEAD] <= -FUNCTION ) {
					if ( s2[FUNHEAD] == s1[FUNHEAD] ) {
						*m++ = 4+FUNHEAD; *m++ = -s1[FUNHEAD]; *m++ = FUNHEAD;
						FILLFUN(m)
						*m++ = 2; *m++ = 1; *m++ = 3;
						INSLENGTH(4+FUNHEAD)
					}
					else if ( s2[FUNHEAD] <= -FUNCTION ) {
						i1 = functions[-FUNCTION-s1[FUNHEAD]].commute != 0;
						i2 = functions[-FUNCTION-s2[FUNHEAD]].commute != 0;
						if ( ( !i1 && i2 ) || ( i1 == i2 && i1 > i2 ) ) {
							i1 = s2[FUNHEAD];
							s2[FUNHEAD] = s1[FUNHEAD];
							s1[FUNHEAD] = i1;
						}
						*m++ = 4+FUNHEAD; *m++ = -s1[FUNHEAD]; *m++ = FUNHEAD;
						FILLFUN(m)
						*m++ = 1; *m++ = 1; *m++ = 3;
						*m++ = 4+FUNHEAD; *m++ = -s2[FUNHEAD]; *m++ = FUNHEAD;
						FILLFUN(m)
						*m++ = 1; *m++ = 1; *m++ = 3;
						INSLENGTH(8+2*FUNHEAD)
					}
					else if ( s2[FUNHEAD] == -SYMBOL ) {
						*m++ = 8; *m++ = SYMBOL; *m++ = 4; *m++ = s2[FUNHEAD+1]; *m++ = 1;
						*m++ = 1; *m++ = 1; *m++ = 3;
						*m++ = 4+FUNHEAD; *m++ = -s1[FUNHEAD]; *m++ = FUNHEAD;
						FILLFUN(m)
						*m++ = 1; *m++ = 1; *m++ = 3;
						INSLENGTH(12+FUNHEAD)
					}
					else {		/* number */
						*m++ = 4;
						*m++ = ABS(s2[FUNHEAD+1]); *m++ = 1; *m++ = s2[FUNHEAD+1] < 0 ? -3: 3;
						*m++ = 4+FUNHEAD; *m++ = -s1[FUNHEAD]; *m++ = FUNHEAD;
						FILLFUN(m)
						*m++ = 1; *m++ = 1; *m++ = 3;
						INSLENGTH(8+FUNHEAD)
					}
				}
				else if ( s1[FUNHEAD] == -SYMBOL ) {
					if ( s2[FUNHEAD] == s1[FUNHEAD] ) {
						if ( s1[FUNHEAD+1] == s2[FUNHEAD+1] ) {
							*m++ = 8; *m++ = SYMBOL; *m++ = 4; *m++ = s1[FUNHEAD+1];
							*m++ = 1; *m++ = 2; *m++ = 1; *m++ = 3;
							INSLENGTH(8)
						}
						else {
							if ( s1[FUNHEAD+1] > s2[FUNHEAD+1] )
								{ i1 = s2[FUNHEAD+1]; i2 = s1[FUNHEAD+1]; }
							else { i1 = s1[FUNHEAD+1]; i2 = s2[FUNHEAD+1]; }
							*m++ = 8; *m++ = SYMBOL; *m++ = 4; *m++ = i1;
							*m++ = 1; *m++ = 1; *m++ = 1; *m++ = 3;
							*m++ = 8; *m++ = SYMBOL; *m++ = 4; *m++ = i2;
							*m++ = 1; *m++ = 1; *m++ = 1; *m++ = 3;
							INSLENGTH(16)
						}
					}
					else if ( s2[FUNHEAD] <= -FUNCTION ) {
						*m++ = 8; *m++ = SYMBOL; *m++ = 4; *m++ = s1[FUNHEAD+1]; *m++ = 1;
						*m++ = 1; *m++ = 1; *m++ = 3;
						*m++ = 4+FUNHEAD; *m++ = -s2[FUNHEAD]; *m++ = FUNHEAD;
						FILLFUN(m)
						*m++ = 1; *m++ = 1; *m++ = 3;
						INSLENGTH(12+FUNHEAD)
					}
					else {
						*m++ = 4;
						*m++ = ABS(s2[FUNHEAD+1]); *m++ = 1; *m++ = s2[FUNHEAD+1] < 0 ? -3: 3;
						*m++ = 8; *m++ = SYMBOL; *m++ = 4; *m++ = s1[FUNHEAD+1]; *m++ = 1;
						*m++ = 1; *m++ = 1; *m++ = 3;
						INSLENGTH(12)
					}
				}
				else {	/* Must be -SNUMBER! */
					if ( s2[FUNHEAD] <= -FUNCTION ) {
						*m++ = 4;
						*m++ = ABS(s1[FUNHEAD+1]); *m++ = 1; *m++ = s1[FUNHEAD+1] < 0 ? -3: 3;
						*m++ = 4+FUNHEAD; *m++ = -s2[FUNHEAD]; *m++ = FUNHEAD;
						FILLFUN(m)
						*m++ = 1; *m++ = 1; *m++ = 3;
						INSLENGTH(8+FUNHEAD)
					}
					else if ( s2[FUNHEAD] == -SYMBOL ) {
						*m++ = 4;
						*m++ = ABS(s1[FUNHEAD+1]); *m++ = 1; *m++ = s1[FUNHEAD+1] < 0 ? -3: 3;
						*m++ = 8; *m++ = SYMBOL; *m++ = 4; *m++ = s2[FUNHEAD+1]; *m++ = 1;
						*m++ = 1; *m++ = 1; *m++ = 3;
						INSLENGTH(12)
					}
					else {		/* Both are numbers. add. */
						LONG x1;
						x1 = (LONG)s1[FUNHEAD+1] + (LONG)s2[FUNHEAD+1];
						if ( x1 < 0 ) { i1 = (WORD)(-x1); i2 = -3; }
						else { i1 = (WORD)x1; i2 = 3; }
						if ( x1 && AN.ncmod != 0 ) {
							m[0] = 4;
							m[1] = i1;
							m[2] = 1;
							m[3] = i2;
							if ( Modulus(m) ) Terminate(-1);
							if ( *m == 0 ) w[1] = 0;
							else {
								if ( *m == 4 && ( m[1] & MAXPOSITIVE ) == m[1]
								&& m[3] == 3 ) {
									i1 = m[1];
									m -= ARGHEAD;
									*m++ = -SNUMBER;
									*m++ = i1;
									INSLENGTH(4)
								}
								else {
									INSLENGTH(*m)
									m += *m;
								}
							}							
						}
						else {
							if ( x1 == 0 ) {
								w[1] = FUNHEAD;
							}
							else if ( ( i1 & MAXPOSITIVE ) == i1 ) {
								m -= ARGHEAD;
								*m++ = -SNUMBER;
								*m++ = (WORD)x1;
								w[1] = FUNHEAD+2;
							}
							else {
								*m++ = 4; *m++ = i1; *m++ = 1; *m++ = i2;
								INSLENGTH(4)
							}
						}
					}
				}
			}
			else {	/* Only s1 is special */
s1only:
/*
			Compose a term in `tempterm'
*/
				t = tempterm;
				if ( s1[FUNHEAD] <= -FUNCTION ) {
					*t++ = 4+FUNHEAD; *t++ = -s1[FUNHEAD]; *t++ = FUNHEAD;
					FILLFUN(t)
					*t++ = 1; *t++ = 1; *t++ = 3;
				}
				else if ( s1[FUNHEAD] == -SYMBOL ) {
					*t++ = 8; *t++ = SYMBOL; *t++ = 4;
					*t++ = s1[FUNHEAD+1]; *t++ = 1;
					*t++ = 1; *t++ = 1; *t++ = 3;
				}
				else {
					*t++ = 4;   *t++ = ABS(s1[FUNHEAD+1]);
					*t++ = 1;   *t++ = s1[FUNHEAD+1] < 0 ? -3: 3;
				}
				tstop1 = t;
				s1 = tempterm;
				goto twogen;
			}
		}
		else {		/* Only s2 is special */
			t = s1;
			s1 = s2;
			s2 = t;
			goto s1only;
		}
	}
	else {
		int oldPolyFlag;
		tstop1 = s1 + s1[1];
		s1 += FUNHEAD+ARGHEAD;
twogen:
		tstop2 = s2 + s2[1];
		s2 += FUNHEAD+ARGHEAD;
/*
		Now we should merge the expressions in s1 and s2 into m.
*/
		oldPolyFlag = AT.SS->PolyFlag;
		AT.SS->PolyFlag = 0;
		while ( s1 < tstop1 && s2 < tstop2 ) {
			i1 = CompareTerms(s1,s2,(WORD)(-1));
			if ( i1 > 0 ) {
				i2 = *s1;
				NCOPY(m,s1,i2);
			}
			else if ( i1 < 0 ) {
				i2 = *s2;
				NCOPY(m,s2,i2);
			}
			else {	/* Coefficients should be added. */
				WORD i;
				t = s1+*s1;
				i1 = t[-1];
				i2 = *s1 - ABS(i1);
				t2 = s2 + i2;
				s2 += *s2;
				mm = m;
				NCOPY(m,s1,i2);
				t1 = s1;
				s1 = t;
				i2 = s2[-1];
/*
				t1,i1 is the first coefficient
				t2,i2 is the second coefficient
				It should be placed at m,i1
*/
				i1 = REDLENG(i1);
				i2 = REDLENG(i2);
				if ( AddRat(BHEAD (UWORD *)t1,i1,(UWORD *)t2,i2,(UWORD *)m,&i) ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Addition of coefficients of PolyFun");
					MUNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
				if ( i == 0 ) {
					m = mm;
				}
				else {
					i1 = INCLENG(i);
					m += ABS(i1);
					m[-1] = i1;
					*mm = WORDDIF(m,mm);
					if ( AN.ncmod != 0 ) {
						if ( Modulus(mm) ) Terminate(-1);
						if ( !*mm ) m = mm;
						else m = mm + *mm;
					}
				}
			}
		}
		while ( s1 < tstop1 ) *m++ = *s1++;
		while ( s2 < tstop2 ) *m++ = *s2++;
		w[1] = WORDDIF(m,w);
		w[FUNHEAD] = w[1] - FUNHEAD;
		if ( ToFast(w+FUNHEAD,w+FUNHEAD) ) {
			if ( w[FUNHEAD] <= -FUNCTION ) w[1] = FUNHEAD+1;
			else w[1] = FUNHEAD+2;
			if ( w[FUNHEAD] == -SNUMBER && w[FUNHEAD+1] == 0 ) w[1] = FUNHEAD;
		}
/*		AT.SS->PolyFlag = AR.PolyFunType;*/
		AT.SS->PolyFlag = oldPolyFlag;
	}
}

/*
 		#] AddArgs : 
 		#[ Compare1 :				WORD Compare1(term1,term2,level)
*/
/**
 *	Compares two terms. The answer is:
 *	0	equal ( with exception of the coefficient if level == 0. )
 *	>0	term1 comes first.
 *	<0	term2 comes first.
 *	Some special precautions may be needed to keep the CompCoef routine
 *	from generating overflows, although this is very unlikely in subterms.
 *	This routine should not return an error condition.
 *
 *	Originally this routine was called Compare.
 *	With the treatment of special polynomials with terms that contain only
 *	symbols and the need for extreme speed for the polynomial routines we
 *	made a special compare routine and now we store the address of the 
 *	current compare routine in AR.CompareRoutine and have a macro Compare
 *	which makes all existing code work properly and we can just replace the
 *	routine on a thread by thread basis (each thread has its own AR struct).
 *
 *	@param term1 First input term
 *	@param term2 Second input term
 *	@param level The sorting level (may influence on the result)
 *	@return 0	equal ( with exception of the coefficient if level == 0. )
 *	        >0	term1 comes first.
 *	        <0	term2 comes first.
 */

WORD Compare1(WORD *term1, WORD *term2, WORD level)
{
	GETIDENTITY
	SORTING *S = AT.SS;
	WORD *stopper1, *stopper2, *t2;
	WORD *s1, *s2, *t1;
	WORD *stopex1, *stopex2;
	WORD c1, c2;
	WORD prevorder;
	WORD count = -1, localPoly, polyhit = -1;

	if ( AR.sLevel == 0 ) {
		numcompares++;
	}

	if ( S->PolyFlag ) {
/*
		if ( S->PolyWise != 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("S->PolyWise is not zero!!!!!");
			MUNLOCK(ErrorMessageLock);
		}
*/
		count = 0; localPoly = 1; S->PolyWise = polyhit = 0;
		S->PolyFlag = AR.PolyFunType;
		if ( AR.PolyFunType == 2 &&
			 ( AR.PolyFunExp == 2 || AR.PolyFunExp == 3 ) ) S->PolyFlag = 1;
	}
	else { localPoly = 0; }
	prevorder = 0;
	GETSTOP(term1,s1);
	stopper1 = s1;
	GETSTOP(term2,stopper2);
	t1 = term1 + 1;
	t2 = term2 + 1;
	while ( t1 < stopper1 && t2 < stopper2 ) {
		if ( *t1 != *t2 ) {
			if ( *t1 == HAAKJE ) return(PREV(-1));
			if ( *t2 == HAAKJE ) return(PREV(1));
			if ( *t1 >= (FUNCTION-1) ) {
				if ( *t2 < (FUNCTION-1) ) return(PREV(-1));
				if ( *t1 < FUNCTION && *t2 < FUNCTION ) return(PREV(*t2-*t1));
				if ( *t1 < FUNCTION ) return(PREV(1));
				if ( *t2 < FUNCTION ) return(PREV(-1));
				c1 = functions[*t1-FUNCTION].commute;
				c2 = functions[*t2-FUNCTION].commute;
				if ( !c1 ) {
					if ( c2 ) return(PREV(1));
					else return(PREV(*t2-*t1));
				}
				else {
					if ( !c2 ) return(PREV(-1));
					else return(PREV(*t2-*t1));
				}
			}
			else return(PREV(*t2-*t1));
		}
		s1 = t1 + 2;
		s2 = t2 + 2;
		c1 = *t1;
		t1 += t1[1];
		t2 += t2[1];
		if ( localPoly && c1 < FUNCTION ) {
			polyhit = 1;
		}
		if ( c1 <= (FUNCTION-1)
		|| ( c1 >= FUNCTION && functions[c1-FUNCTION].spec ) ) {
			if ( c1 == SYMBOL ) {
				if ( *s1 == FACTORSYMBOL && *s2 == FACTORSYMBOL
				 && s1[-1] == 4 && s2[-1] == 4
				 && ( ( t1 < stopper1 && *t1 == HAAKJE )
				 || ( t1 == stopper1 && AT.fromindex ) ) ) {
/*
					We have to be very careful with the criteria here, because
					Compare1 is called both in the regular sorting and by the
					routine that makes the bracket index. In the last case
					there is no HAAKJE subterm.
*/
					if ( s1[1] != s2[1] ) return(s2[1]-s1[1]);
					s1 += 2; s2 += 2;
				}
				else if ( AR.SortType >= SORTPOWERFIRST ) {
					WORD i1 = 0, *r1;
					r1 = s1;
					while ( s1 < t1 ) { i1 += s1[1]; s1 += 2; }
					s1 = r1; r1 = s2;
					while ( s2 < t2 ) { i1 -= s2[1]; s2 += 2; }
					s2 = r1;
					if ( i1 ) {
						if ( AR.SortType >= SORTANTIPOWER ) i1 = -i1;
						return(PREV(i1));
					}
				}
				while ( s1 < t1 ) {
					if ( s2 >= t2 ) {
/*						return(PREV(1));  */
						if ( AR.SortType==SORTLOWFIRST ) {
							return(PREV((s1[1]>0?-1:1)));
						}
						else {
							return(PREV((s1[1]<0?-1:1)));
						}
					}
					if ( *s1 != *s2 ) {
/*						return(PREV(*s2-*s1)); */
						if ( AR.SortType==SORTLOWFIRST ) {
							if ( *s1 < *s2 ) {
								return(PREV((s1[1]<0?1:-1)));
							}
							else {
								return(PREV((s2[1]<0?-1:1)));
							}
						}
						else {
							if ( *s1 < *s2 ) {
								return(PREV((s1[1]<0?-1:1)));
							}
							else {
								return(PREV((s2[1]<0?1:-1)));
							}
						}
					}
					s1++; s2++;
					if ( *s1 != *s2 ) return(
						PREV((AR.SortType==SORTLOWFIRST?*s2-*s1:*s1-*s2)));
					s1++; s2++;
				}
				if ( s2 < t2 ) {
/*					return(PREV(-1));  */
					if ( AR.SortType==SORTLOWFIRST ) {
						return(PREV((s2[1]<0?-1:1)));
					}
					else {
						return(PREV((s2[1]<0?1:-1)));
					}
				}
			}
			else if ( c1 == DOTPRODUCT ) {
				if ( AR.SortType >= SORTPOWERFIRST ) {
					WORD i1 = 0, *r1;
					r1 = s1;
					while ( s1 < t1 ) { i1 += s1[2]; s1 += 3; }
					s1 = r1; r1 = s2;
					while ( s2 < t2 ) { i1 -= s2[2]; s2 += 3; }
					s2 = r1;
					if ( i1 ) {
						if ( AR.SortType >= SORTANTIPOWER ) i1 = -i1;
						return(PREV(i1));
					}
				}
				while ( s1 < t1 ) {
					if ( s2 >= t2 ) return(PREV(1));
					if ( *s1 != *s2 ) return(PREV(*s2-*s1));
					s1++; s2++;
					if ( *s1 != *s2 ) return(PREV(*s2-*s1));
					s1++; s2++;
					if ( *s1 != *s2 ) return(
						PREV((AR.SortType==SORTLOWFIRST?*s2-*s1:*s1-*s2)));
					s1++; s2++;
				}
				if ( s2 < t2 ) return(PREV(-1));
			}
			else {
				while ( s1 < t1 ) {
					if ( s2 >= t2 ) return(PREV(1));
					if ( *s1 != *s2 ) return(PREV(*s2-*s1));
					s1++; s2++;
				}
				if ( s2 < t2 ) return(PREV(-1));
			}
		}
		else {
#if FUNHEAD != 2
			s1 += FUNHEAD-2;
			s2 += FUNHEAD-2;
#endif
			if ( localPoly && c1 == AR.PolyFun ) {
				if ( count == 0 ) {
				  if ( S->PolyFlag == 1 ) {
					WORD i1, i2;
					if ( *s1 > 0 ) i1 = *s1;
					else if ( *s1 <= -FUNCTION ) i1 = 1;
					else i1 = 2;
					if ( *s2 > 0 ) i2 = *s2;
					else if ( *s2 <= -FUNCTION ) i2 = 1;
					else i2 = 2;
					if ( s1+i1 == t1 && s2+i2 == t2 ) {	/* This is the stuff */
/*
						Test for scalar nature
*/
						if ( !polyhit ) {
							WORD *u1, *u2, *ustop;
							if ( *s1 < 0 ) {
								if ( *s1 != -SNUMBER && *s1 != -SYMBOL && *s1 > -FUNCTION )
									goto NoPoly;
							}
							else {
								u1 = s1 + ARGHEAD;
								while ( u1 < t1 ) {
									u2 = u1 + *u1;
									ustop = u2 - ABS(u2[-1]);
									u1++;
									while ( u1 < ustop ) {
										if ( *u1 == INDEX ) goto NoPoly;
										u1 += u1[1];
									}
									u1 = u2;
								}
							}
							if ( *s2 < 0 ) {
								if ( *s2 != -SNUMBER && *s2 != -SYMBOL && *s2 > -FUNCTION )
									goto NoPoly;
							}
							else {
								u1 = s2 + ARGHEAD;
								while ( u1 < t2 ) {
									u2 = u1 + *u1;
									ustop = u2 - ABS(u2[-1]);
									u1++;
									while ( u1 < ustop ) {
										if ( *u1 == INDEX ) goto NoPoly;
										u1 += u1[1];
									}
									u1 = u2;
								}
							}
						}
						S->PolyWise = WORDDIF(s1,term1);
						S->PolyWise -= FUNHEAD;
						count = 1;
						continue;
					}
					else {
NoPoly:
						S->PolyWise = localPoly = 0;
					}
				  }
				  else if ( AR.PolyFunType == 2 ) {
					WORD i1, i2, i1a, i2a;
					if ( *s1 > 0 ) i1 = *s1;
					else if ( *s1 <= -FUNCTION ) i1 = 1;
					else i1 = 2;
					if ( *s2 > 0 ) i2 = *s2;
					else if ( *s2 <= -FUNCTION ) i2 = 1;
					else i2 = 2;
					if ( s1[i1] > 0 ) i1a = s1[i1];
					else if ( s1[i1] <= -FUNCTION ) i1a = 1;
					else i1a = 2;
					if ( s2[i2] > 0 ) i2a = s2[i2];
					else if ( s2[i2] <= -FUNCTION ) i2a = 1;
					else i2a = 2;
					if ( s1+i1+i1a == t1 && s2+i2+i2a == t2 ) {	/* This is the stuff */
/*
						Test for scalar nature
*/
						if ( !polyhit ) {
							WORD *u1, *u2, *ustop;
							if ( *s1 < 0 ) {
								if ( *s1 != -SNUMBER && *s1 != -SYMBOL && *s1 > -FUNCTION )
									goto NoPoly;
							}
							else {
								u1 = s1 + ARGHEAD;
								while ( u1 < s1+i1 ) {
									u2 = u1 + *u1;
									ustop = u2 - ABS(u2[-1]);
									u1++;
									while ( u1 < ustop ) {
										if ( *u1 == INDEX ) goto NoPoly;
										u1 += u1[1];
									}
									u1 = u2;
								}
							}
							if ( s1[i1] < 0 ) {
								if ( s1[i1] != -SNUMBER && s1[i1] != -SYMBOL && s1[i1] > -FUNCTION )
									goto NoPoly;
							}
							else {
								u1 = s1 +i1 + ARGHEAD;
								while ( u1 < t1 ) {
									u2 = u1 + *u1;
									ustop = u2 - ABS(u2[-1]);
									u1++;
									while ( u1 < ustop ) {
										if ( *u1 == INDEX ) goto NoPoly;
										u1 += u1[1];
									}
									u1 = u2;
								}
							}
							if ( *s2 < 0 ) {
								if ( *s2 != -SNUMBER && *s2 != -SYMBOL && *s2 > -FUNCTION )
									goto NoPoly;
							}
							else {
								u1 = s2 + ARGHEAD;
								while ( u1 < s2+i2 ) {
									u2 = u1 + *u1;
									ustop = u2 - ABS(u2[-1]);
									u1++;
									while ( u1 < ustop ) {
										if ( *u1 == INDEX ) goto NoPoly;
										u1 += u1[1];
									}
									u1 = u2;
								}
							}
							if ( s2[i2] < 0 ) {
								if ( s2[i2] != -SNUMBER && s2[i2] != -SYMBOL && s2[i2] > -FUNCTION )
									goto NoPoly;
							}
							else {
								u1 = s2 + i2 + ARGHEAD;
								while ( u1 < t2 ) {
									u2 = u1 + *u1;
									ustop = u2 - ABS(u2[-1]);
									u1++;
									while ( u1 < ustop ) {
										if ( *u1 == INDEX ) goto NoPoly;
										u1 += u1[1];
									}
									u1 = u2;
								}
							}
						}
						S->PolyWise = WORDDIF(s1,term1);
						S->PolyWise -= FUNHEAD;
						count = 1;
						continue;
					}
					else {
						S->PolyWise = localPoly = 0;
					}
				  }
				  else {
					S->PolyWise = localPoly = 0;
				  }
				}
				else {
					t1 = term1 + S->PolyWise;
					t2 = term2 + S->PolyWise;
					S->PolyWise = 0;
					localPoly = 0;
					continue;
				}
			}
			while ( s1 < t1 ) {
/*
				The next statement was added 9-nov-2001. It made a bad error
*/
				if ( s2 >= t2 ) return(PREV(-1));
/*
				There is a little problem here with fast arguments
				We don't want to sacrifice speed, but we like to
				keep a rational ordering. This last one suffers in
				the solution that has been choosen here.
*/
				if ( AC.properorderflag ) {
					WORD oldpolyflag;
					oldpolyflag = S->PolyFlag;
					S->PolyFlag = 0;
					if ( ( c2 = -CompArg(s1,s2) ) != 0 ) {
						S->PolyFlag = oldpolyflag; return(PREV(c2));
					}
					S->PolyFlag = oldpolyflag;
					NEXTARG(s1)
					NEXTARG(s2)
				}
				else {
					if ( *s1 > 0 ) {
						if ( *s2 > 0 ) {
							WORD oldpolyflag;
							stopex1 = s1 + *s1;
							if ( s2 >= t2 ) return(PREV(-1));
							stopex2 = s2 + *s2;
							s1 += ARGHEAD; s2 += ARGHEAD;
							oldpolyflag = S->PolyFlag;
							S->PolyFlag = 0;
							while ( s1 < stopex1 ) {
								if ( s2 >= stopex2 ) {
									S->PolyFlag = oldpolyflag; return(PREV(-1));
								}
								if ( ( c2 = CompareTerms(s1,s2,(WORD)1) ) != 0 ) {
									S->PolyFlag = oldpolyflag; return(PREV(c2));
								}
								s1 += *s1;
								s2 += *s2;
							}
							S->PolyFlag = oldpolyflag;
							if ( s2 < stopex2 ) return(PREV(1));
						}
						else return(PREV(1));
					}
					else {
						if ( *s2 > 0 ) return(PREV(-1));
						if ( *s1 != *s2 ) { return(PREV(*s1-*s2)); }
						if ( *s1 > -FUNCTION ) {
							if ( *++s1 != *++s2 ) { return(PREV(*s2-*s1)); }
						}
						s1++; s2++;
					}
				}
			}
			if ( s2 < t2 ) return(PREV(1));
		}
	}
	{
		if ( AR.SortType != SORTLOWFIRST ) {
			if ( t1 < stopper1 ) return(PREV(1));
			if ( t2 < stopper2 ) return(PREV(-1));
		}
		else {
			if ( t1 < stopper1 ) return(PREV(-1));
			if ( t2 < stopper2 ) return(PREV(1));
		}
	}
	if ( level == 3 ) return(CompCoef(term1,term2));
	if ( level >= 1 )
		return(CompCoef(term2,term1));
	return(0);
}

/*
 		#] Compare1 : 
 		#[ CompareSymbols :			WORD CompareSymbols(term1,term2,par)
*/
/**
 *	Compares the terms, based on the value of AN.polysortflag.
 *	If term1 < term2 the return value is -1
 *	If term1 > term2 the return value is  1
 *	If term1 = term2 the return value is  0
 *	The coefficients may differ.
 *	The terms contain only a single subterm of type SYMBOL.
 *	If AN.polysortflag = 0 it is a 'regular' compare.
 *	If AN.polysortflag = 1 the sum of the powers is more important
 *	par is a dummy parameter to make the parameter field identical
 *	to that of Compare1 which is the regular compare routine in sort.c
 */

WORD CompareSymbols(WORD *term1, WORD *term2, WORD par)
{
	GETIDENTITY
	int sum1, sum2;
	WORD *t1, *t2, *tt1, *tt2;
	int low, high;
	DUMMYUSE(par);
	if ( AR.SortType == SORTLOWFIRST ) { low = 1; high = -1; }
	else { low = -1; high = 1; }
	t1 = term1 + 1; tt1 = term1+*term1; tt1 -= ABS(tt1[-1]); t1 += 2;
	t2 = term2 + 1; tt2 = term2+*term2; tt2 -= ABS(tt2[-1]); t2 += 2;
	if ( AN.polysortflag > 0 ) {
		sum1 = 0; sum2 = 0;
		while ( t1 < tt1 ) { sum1 += t1[1]; t1 += 2; }
		while ( t2 < tt2 ) { sum2 += t2[1]; t2 += 2; }
		if ( sum1 < sum2 ) return(low);
		if ( sum1 > sum2 ) return(high);
		t1 = term1+3; t2 = term2 + 3;
	}
	while ( t1 < tt1 && t2 < tt2 ) {
		if ( *t1 > *t2 ) return(low);
		if ( *t1 < *t2 ) return(high);
		if ( t1[1] < t2[1] ) return(low);
		if ( t1[1] > t2[1] ) return(high);
		t1 += 2; t2 += 2;
	}
	if ( t1 < tt1 ) return(high);
	if ( t2 < tt2 ) return(low);
	return(0);
}

/*
 		#] CompareSymbols : 
 		#[ CompareHSymbols :		WORD CompareHSymbols(term1,term2,par)
*/
/**
 *	Compares terms that can have only SYMBOL and HAAKJE subterms.
 *	If term1 < term2 the return value is -1
 *	If term1 > term2 the return value is  1
 *	If term1 = term2 the return value is  0
 *	par is a dummy parameter to make the parameter field identical
 *	to that of Compare1 which is the regular compare routine in sort.c
 */

WORD CompareHSymbols(WORD *term1, WORD *term2, WORD par)
{
	GETIDENTITY
	WORD *t1, *t2, *tt1, *tt2, *ttt1, *ttt2;
	DUMMYUSE(par);
	DUMMYUSE(AT.WorkPointer);
	t1 = term1 + 1; tt1 = term1+*term1; tt1 -= ABS(tt1[-1]); t1 += 2;
	t2 = term2 + 1; tt2 = term2+*term2; tt2 -= ABS(tt2[-1]); t2 += 2;
	while ( t1 < tt1 && t2 < tt2 ) {
		if ( *t1 != *t2 ) {
			if ( t1[0] < t2[0] ) return(-1);
			return(1);
		}
		else if ( *t1 == HAAKJE ) {
			t1 += 3; t2 += 3; continue;
		}
		ttt1 = t1+t1[1]; ttt2 = t2+t2[1];
		while ( t1 < ttt1 && t2 < ttt2 ) {
			if ( *t1 > *t2 ) return(-1);
			if ( *t1 < *t2 ) return(1);
			if ( t1[1] < t2[1] ) return(-1);
			if ( t1[1] > t2[1] ) return(1);
			t1 += 2; t2 += 2;
		}
		if ( t1 < ttt1 ) return(1);
		if ( t2 < ttt2 ) return(-1);
	}
	if ( t1 < tt1 ) return(1);
	if ( t2 < tt2 ) return(-1);
	return(0);
}

/*
 		#] CompareHSymbols : 
 		#[ ComPress :				LONG ComPress(ss,n)
*/
/**
 *		Gets a list of pointers to terms and compresses the terms.
 *		In n it collects the number of terms and the return value
 *		of the function is the space that is occupied.
 *
 *		We have to pay some special attention to the compression of
 *		terms with a PolyFun. This PolyFun should occur only straight
 *		before the coefficient, so we can use the same trick as for
 *		the coefficient to sabotage compression of this object
 *		(Replace in the history the function pointer by zero. This
 *		is safe, because terms that would be identical otherwise would
 *		have been added).
 *
 *		@param ss Array of pointers to terms to be compressed.
 *		@param n  Number of pointers in ss.
 *		@return   Total number of words needed for the compressed result.
 */

LONG ComPress(WORD **ss, LONG *n)
{
	GETIDENTITY
	WORD *t, *s, j, k;
	LONG size = 0;
	int newsize, i;
/*
			#[ debug :

	WORD **sss = ss;

	if ( AP.DebugFlag ) {
		UBYTE OutBuf[140];
		MLOCK(ErrorMessageLock);
		MesPrint("ComPress:");
		AO.OutFill = AO.OutputLine = OutBuf;
		AO.OutSkip = 3;
		FiniLine();
		ss = sss;
		while ( *ss ) {
			s = *ss++;
			j = *s;
			if ( j < 0 ) {
				j = s[1] + 2;
			}
			while ( --j >= 0 ) {
				TalToLine((UWORD)(*s++)); TokenToLine((UBYTE *)"  ");
			}
			FiniLine();
		}
		AO.OutSkip = 0;
		FiniLine();
		MUNLOCK(ErrorMessageLock);
		ss = sss;
	}

			#] debug : 
*/
	*n = 0;
	if ( AT.SS == AT.S0 && !AR.NoCompress ) {
		if ( AN.compressSize == 0 ) {
			if ( *ss ) { AN.compressSize = **ss + 64; }
			else       { AN.compressSize = AM.MaxTer/sizeof(WORD) + 2; }
			AN.compressSpace = (WORD *)Malloc1(AN.compressSize*sizeof(WORD),"Compression");
		}
		AN.compressSpace[0] = 0;
		while ( *ss ) {
			k = 0;
			s = *ss;
			j = *s++;
			if ( j > AN.compressSize ) {
				newsize = j + 64;
				t = (WORD *)Malloc1(newsize*sizeof(WORD),"Compression");
				t[0] = 0;
				if ( AN.compressSpace ) {
					for ( i = 0; i < *AN.compressSpace; i++ ) t[i] = AN.compressSpace[i];
					M_free(AN.compressSpace,"Compression");
				}
				AN.compressSpace = t;
				AN.compressSize = newsize;
			}
			t = AN.compressSpace;
			i = *t - 1;
			*t++ = j; j--;
			if ( AR.PolyFun ) {
				WORD *polystop, *sa;
				sa = s + j;
				sa -= ABS(sa[-1]);
				polystop = s;
				while ( polystop < sa && *polystop != AR.PolyFun ) {
					polystop += polystop[1];
				}
				while ( i > 0 && j > 0 && *s == *t && s < polystop ) {
					i--; j--; s++; t++; k--;
				}
			}
			else {
				WORD *sa;
				sa = s + j;
				sa -= ABS(sa[-1]);
				while ( i > 0 && j > 0 && *s == *t && s < sa ) { i--; j--; s++; t++; k--; }
			}
			if ( k < -1 ) {
				s[-1] = j;
				s[-2] = k;
				*ss = s-2;
				size += j + 2;
			}
			else {
				size += *AN.compressSpace;
				if ( k == -1 ) { t--; s--; j++; }
			}
			while ( --j >= 0 ) *t++ = *s++;
/*					Sabotage getting into the coefficient next time */
			t = AN.compressSpace + *AN.compressSpace;
			t[-(ABS(t[-1]))] = 0;
			ss++;
			(*n)++;
		}
	}
	else {
		while ( *ss ) {
			size += *(*ss++);
			(*n)++;
		}
	}
/*
			#[ debug :

	if ( AP.DebugFlag ) {
		UBYTE OutBuf[140];
		AO.OutFill = AO.OutputLine = OutBuf;
		AO.OutSkip = 3;
		FiniLine();
		ss = sss;
		while ( *ss ) {
			s = *ss++;
			j = *s;
			if ( j < 0 ) {
				j = s[1] + 2;
			}
			while ( --j >= 0 ) {
				TalToLine((UWORD)(*s++)); TokenToLine((UBYTE *)"  ");
			}
			FiniLine();
		}
		AO.OutSkip = 0;
		FiniLine();
	}

			#] debug : 
*/
	return(size);
}

/*
 		#] ComPress : 
 		#[ SplitMerge :				VOID SplitMerge(Point,number)
*/
/**
 *		Algorithm by J.A.M.Vermaseren (31-7-1988)
 *
 *		Note that AN.SplitScratch and AN.InScratch are used also in GarbHand
 *
 *		Merge sort in memory. The input is an array of pointers.
 *		Sorting is done recursively by dividing the array in two equal parts
 *		and calling SplitMerge for each.
 *		When the parts are small enough we can do the compare and take the
 *		appropriate action.
 *		An addition is that we look for 'runs'. Sequences that are already
 *		ordered. This happens a lot when there is very little action in a
 *		module. This made FORM faster by a few percent.
 *
 *		@param  Pointer The array of pointers to the terms to be sorted.
 *		@param  number  The number of pointers in Pointer.
 *
 *		The terms are supposed to be sitting in the small buffer and there
 *		is supposed to be an extension to this buffer for when there are
 *		two terms that should be added and the result takes more space than
 *		each of the original terms. The notation guarantees that the result
 *		never needs more space than the sum of the spaces of the original
 *		terms.
 */

#ifdef NEWSPLITMERGE

LONG SplitMerge(PHEAD WORD **Pointer, LONG number)
{
	GETBIDENTITY
	SORTING *S = AT.SS;
	WORD **pp3, **pp1, **pp2;
	LONG i, newleft, newright, split;

	if ( number < 2 ) return(number);
	if ( number == 2 ) {
		pp1 = Pointer; pp2 = pp1 + 1;
		if ( ( i = CompareTerms(*pp1,*pp2,(WORD)0) ) < 0 ) {
			pp3 = (WORD **)(*pp1); *pp1 = *pp2; *pp2 = (WORD *)pp3;
		}
		else if ( i == 0 ) {
		  number--;
		  if ( S->PolyWise ) { if ( AddPoly(BHEAD pp1,pp2) == 0 ) number = 0; }
		  else               { if ( AddCoef(BHEAD pp1,pp2) == 0 ) number = 0; }
		}
		return(number);
	}
	split = number/2;
	newleft  = SplitMerge(BHEAD Pointer,split);
	newright = SplitMerge(BHEAD Pointer+split,number-split);
	if ( newright == 0 ) return(newleft);
/*
	We compare the last of the left with the first of the right
	If they are already in order, we will be done quickly.
	We may have to compactify the buffer because the recursion may
	have created holes. Also this compare may result in equal terms.
	Addition of 23-jul-1999. It makes things a bit faster.
*/
	if ( newleft > 0 && newright > 0 &&
	( i = CompareTerms(Pointer[newleft-1],Pointer[split],(WORD)0) ) >= 0 ) {
		pp2 = Pointer+split; pp1 = Pointer+newleft-1;
		if ( i == 0 ) {
		  if ( S->PolyWise ) {
			if ( AddPoly(BHEAD pp1,pp2) > 0 ) pp1++;
			else newleft--;
		  }
		  else {               
			if ( AddCoef(BHEAD pp1,pp2) > 0 ) pp1++;
			else newleft--;
		  }
		  pp2++; newright--;
		}
		else pp1++;
		newleft += newright;
		if ( pp1 < pp2 ) {
			while ( --newright >= 0 ) *pp1++ = *pp2++;
		}
		return(newleft);
	}

	if ( split >= AN.SplitScratchSize ) {
		AN.SplitScratchSize = (split*3)/2+100;
		if ( AN.SplitScratchSize > S->Terms2InSmall/2 )
			 AN.SplitScratchSize = S->Terms2InSmall/2;
		if ( AN.SplitScratch ) M_free(AN.SplitScratch,"AN.SplitScratch");
		AN.SplitScratch = (WORD **)Malloc1(AN.SplitScratchSize*sizeof(WORD *),"AN.SplitScratch");
	}
	pp3 = AN.SplitScratch; pp1 = Pointer;
	for ( i = 0; i < newleft; i++ ) *pp3++ = *pp1++;
	AN.InScratch = newleft;
	pp1 = AN.SplitScratch; pp2 = Pointer + split; pp3 = Pointer;
/*
		An improvement in the style of Timsort
*/
	while ( newleft > 8 ) {
		LONG nnleft = newleft/2;
		if ( ( i = CompareTerms(pp1[nnleft],*pp2,(WORD)0) ) < 0 ) break;
		pp3 += nnleft+1;
		pp1 += nnleft+1;
		newleft -= nnleft+1;
		if ( i == 0 ) {
			if ( S->PolyWise ) { i = AddPoly(BHEAD pp3-1,pp2); }
			else               { i = AddCoef(BHEAD pp3-1,pp2); }
			if ( i == 0 ) pp3--;
			pp2++;
			newright--;
			break;
		}
	}

	while ( newleft > 0 && newright > 0 ) {
		if ( ( i = CompareTerms(*pp1,*pp2,(WORD)0) ) < 0 ) {
			*pp3++ = *pp2++;
			newright--;
		}
		else if ( i > 0 ) {
			*pp3++ = *pp1++;
			newleft--;
		}
		else {
		  if ( S->PolyWise ) { if ( AddPoly(BHEAD pp1,pp2) > 0 ) *pp3++ = *pp1; }
		  else {               if ( AddCoef(BHEAD pp1,pp2) > 0 ) *pp3++ = *pp1; }
		  pp1++; pp2++; newleft--; newright--;
		}
	}
	for ( i = 0; i < newleft; i++ ) *pp3++ = *pp1++;
	if ( pp3 == pp2 ) {
		pp3 += newright;
	} else {
		for ( i = 0; i < newright; i++ ) *pp3++ = *pp2++;
	}
	AN.InScratch = 0;
	return(pp3 - Pointer);
}

#else

LONG SplitMerge(PHEAD WORD **Pointer, LONG number)
{
	GETBIDENTITY
	SORTING *S = AT.SS;
	WORD **pp3, **pp1, **pp2;
	LONG nleft, nright, i, newleft, newright;
	WORD **pptop;

	if ( number < 2 ) return(number);
	if ( number == 2 ) {
		pp1 = Pointer; pp2 = pp1 + 1;
		if ( ( i = CompareTerms(*pp1,*pp2,(WORD)0) ) < 0 ) {
			pp3 = (WORD **)(*pp1); *pp1 = *pp2; *pp2 = (WORD *)pp3;
		}
		else if ( i == 0 ) {
		  number--;
		  if ( S->PolyWise ) { if ( AddPoly(BHEAD pp1,pp2) == 0 ) { number = 0; } }
		  else {               if ( AddCoef(BHEAD pp1,pp2) == 0 ) { number = 0; } }
		}
		return(number);
	}
	pptop = Pointer + number;
	nleft = number >> 1; nright = number - nleft;
	newleft  = SplitMerge(BHEAD Pointer,nleft);
	newright = SplitMerge(BHEAD Pointer+nleft,nright);
/*
	We compare the last of the left with the first of the right
	If they are already in order, we will be done quickly.
	We may have to compactify the buffer because the recursion may
	have created holes. Also this compare may result in equal terms.
	Addition of 23-jul-1999. It makes things a bit faster.
*/
	if ( newleft > 0 && newright > 0 &&
	( i = CompareTerms(Pointer[newleft-1],Pointer[nleft],(WORD)0) ) >= 0 ) {
		pp2 = Pointer+nleft; pp1 = Pointer+newleft-1;
		if ( i == 0 ) {
		  if ( S->PolyWise ) {
			if ( AddPoly(BHEAD pp1,pp2) > 0 ) pp1++;
			else newleft--;
		  }
		  else {               
			if ( AddCoef(BHEAD pp1,pp2) > 0 ) pp1++;
			else newleft--;
		  }
		  *pp2++ = 0; newright--;
		}
		else pp1++;
		newleft += newright;
		if ( pp1 < pp2 ) {
			while ( --newright >= 0 ) *pp1++ = *pp2++;
			while ( pp1 < pptop ) *pp1++ = 0;
		}
		return(newleft);
	}
	if ( nleft > AN.SplitScratchSize ) {
		AN.SplitScratchSize = (nleft*3)/2+100;
		if ( AN.SplitScratchSize > S->Terms2InSmall/2 )
			 AN.SplitScratchSize = S->Terms2InSmall/2;
		if ( AN.SplitScratch ) M_free(AN.SplitScratch,"AN.SplitScratch");
		AN.SplitScratch = (WORD **)Malloc1(AN.SplitScratchSize*sizeof(WORD *),"AN.SplitScratch");
	}
	pp3 = AN.SplitScratch; pp1 = Pointer; i = nleft;
	do { *pp3++ = *pp1; *pp1++ = 0; } while ( *pp1 && --i > 0 );
	if ( i > 0 ) { *pp3 = 0; i--; }
	AN.InScratch = nleft - i;
	pp1 = AN.SplitScratch; pp2 = Pointer + nleft; pp3 = Pointer;
	while ( nleft > 0 && nright > 0 && *pp1 && *pp2 ) {
		if ( ( i = CompareTerms(*pp1,*pp2,(WORD)0) ) < 0 ) {
			*pp3++ = *pp2;
			*pp2++ = 0;
			nright--;
		}
		else if ( i > 0 ) {
			*pp3++ = *pp1;
			*pp1++ = 0;
			nleft--;
		}
		else {
		  if ( S->PolyWise ) { if ( AddPoly(BHEAD pp1,pp2) > 0 ) *pp3++ = *pp1; }
		  else {               if ( AddCoef(BHEAD pp1,pp2) > 0 ) *pp3++ = *pp1; }
		  *pp1++ = 0; *pp2++ = 0; nleft--; nright--;
		}
	}
	while ( --nleft  >= 0 && *pp1 ) { *pp3++ = *pp1; *pp1++ = 0; }
	while ( --nright >= 0 && *pp2 ) { *pp3++ = *pp2++; }
	nleft = pp3 - Pointer;
	while ( pp3 < pptop ) *pp3++ = 0;
	AN.InScratch = 0;
	return(nleft);
}

#endif

/*
 		#] SplitMerge : 
 		#[ GarbHand :				VOID GarbHand()
*/
/**
 *		Garbage collection that takes place when the small extension is full
 *		and we need to place more terms there.
 *		When this is the case there are many holes in the small buffer and
 *		the whole can be compactified.
 *		The major complication is the buffer for SplitMerge.
 *		There are to options for temporary memory:
 *		1: find some buffer that has enough space (maybe in the large
 *		   buffer).
 *		2: allocate a buffer. Give it back afterwards of course.
 *		If the small extension is properly dimensioned this routine should
 *		be called very rarely. Most of the time it will be called when the
 *		polyfun or polyratfun is active.
 */

VOID GarbHand()
{
	GETIDENTITY
	SORTING *S = AT.SS;
	WORD **Point, *s2, *t, *garbuf, i;
	LONG k, total = 0;
	int tobereturned = 0;
/*
	Compute the size needed. Put it in total.
*/
#ifdef TESTGARB
	MLOCK(ErrorMessageLock);
	MesPrint("in:  S->sFill = %x, S->sTop2 = %x",S->sFill,S->sTop2);
#endif
	Point = S->sPointer;
	k = S->sTerms;
	while ( --k >= 0 ) {
		if ( ( s2 = *Point++ ) != 0 ) { total += *s2; }
	}
	Point = AN.SplitScratch;
	k = AN.InScratch;
	while ( --k >= 0 ) {
		if ( ( s2 = *Point++ ) != 0 ) { total += *s2; }
	}
#ifdef TESTGARB
	MesPrint("total = %l, nterms = %l",2*total,AN.InScratch);
	MUNLOCK(ErrorMessageLock);
#endif
/*
	Test now whether it fits. If so deal with the problem inside
	the memory at the tail of the large buffer.
*/
	if ( S->lBuffer != 0 && S->lFill + total <= S->lTop ) {
		garbuf = S->lFill;
	}
	else {
		garbuf = (WORD *)Malloc1(total*sizeof(WORD),"Garbage buffer");
		tobereturned = 1;
	}
	t = garbuf;
	Point = S->sPointer;
	k = S->sTerms;
	while ( --k >= 0 ) {
		if ( *Point ) {
			s2 = *Point++;
			i = *s2;
			NCOPY(t,s2,i);
		}
		else { Point++; }
	}
	Point = AN.SplitScratch;
	k = AN.InScratch;
	while ( --k >= 0 ) {
		if ( *Point ) {
			s2 = *Point++;
			i = *s2;
			NCOPY(t,s2,i);
		}
		else Point++;
	}
	s2 = S->sBuffer;
	t = garbuf;
	Point = S->sPointer;
	k = S->sTerms;
	while ( --k >= 0 ) {
		if ( *Point ) {
			*Point++ = s2;
			i = *t;
			NCOPY(s2,t,i);
		}
		else { Point++; }
	}
	Point = AN.SplitScratch;
	k = AN.InScratch;
	while ( --k >= 0 ) {
		if ( *Point ) {
			*Point++ = s2;
			i = *t;
			NCOPY(s2,t,i);
		}
		else Point++;
	}
	S->sFill = s2;
#ifdef TESTGARB
	MLOCK(ErrorMessageLock);
	MesPrint("out: S->sFill = %x, S->sTop2 = %x",S->sFill,S->sTop2);
	if ( S->sFill >= S->sTop2 ) {
		MesPrint("We are in deep trouble");
	}
	MUNLOCK(ErrorMessageLock);
#endif
	if ( tobereturned ) M_free(garbuf,"Garbage buffer");
	return;
}

/*
 		#] GarbHand : 
 		#[ MergePatches :			WORD MergePatches(par)
*/
/**
 *	The general merge routine. Can be used for the large buffer
 *	and the file merging. The array S->Patches tells where the patches
 *	start S->pStop tells where they end (has to be computed first).
 *	The end of a 'line to be merged' is indicated by a zero. If
 *	the end is reached without running into a zero or a term
 *	runs over the boundary of a patch it is a file merging operation
 *	and a new piece from the file is read in.
 *
 *	@param par
 *	If par == 0 the sort is for file -> outputfile.
 *	If par == 1 the sort is for large buffer -> sortfile.
 *	If par == 2 the sort is for large buffer -> outputfile.
 *
 */

WORD MergePatches(WORD par)
{
	GETIDENTITY
	SORTING *S = AT.SS;
	WORD **poin, **poin2, ul, k, i, im, *m1;
	WORD *p, lpat, mpat, level, l1, l2, r1, r2, r3, c;
	WORD *m2, *m3, r31, r33, ki, *rr;
	UWORD *coef;
	POSITION position;
	FILEHANDLE *fin, *fout;
	int fhandle;
/*
	UBYTE *s;
*/
#ifdef WITHZLIB
	POSITION position2;
	int oldgzipCompress = AR.gzipCompress;
	if ( par == 2 ) {
		AR.gzipCompress = 0;
	}
#endif
	fin = &S->file;
	fout = &(AR.FoStage4[0]);
NewMerge:
	coef = AN.SoScratC;
	poin = S->poina; poin2 = S->poin2a;
	rr = AR.CompressPointer;
	*rr = 0;
/*
 		#[ Setup :
*/
	if ( par == 1 ) {
		fout = &(S->file);
		if ( fout->handle < 0 ) {
FileMake:
			PUTZERO(AN.OldPosOut);
			if ( ( fhandle = CreateFile(fout->name) ) < 0 ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Cannot create file %s",fout->name);
				MUNLOCK(ErrorMessageLock);
				goto ReturnError;
			}
#ifdef GZIPDEBUG
			MLOCK(ErrorMessageLock);
			MesPrint("%w MergePatches created output file %s",fout->name);
			MUNLOCK(ErrorMessageLock);
#endif
			fout->handle = fhandle;
			PUTZERO(fout->filesize);
			PUTZERO(fout->POposition);
/*
			Should not be here?
#ifdef WITHZLIB
			fout->ziobuffer = 0;
#endif
*/
#ifdef ALLLOCK
			LOCK(fout->pthreadslock);
#endif
			SeekFile(fout->handle,&(fout->filesize),SEEK_SET);
#ifdef ALLLOCK
			UNLOCK(fout->pthreadslock);
#endif
			S->fPatchN = 0;
			PUTZERO(S->fPatches[0]);
			fout->POfill = fout->PObuffer;	
			PUTZERO(fout->POposition);
		}
ConMer:
		StageSort(fout);
#ifdef WITHZLIB
		if ( S == AT.S0 && AR.NoCompress == 0 && AR.gzipCompress > 0 )
			S->fpcompressed[S->fPatchN] = 1;
		else
			S->fpcompressed[S->fPatchN] = 0;
		SetupOutputGZIP(fout);
#endif
	}
	else if ( par == 0 && S->stage4 > 0 ) {
/*
		We will have to do our job more than once.
		Input is from S->file and output will go to AR.FoStage4.
		The file corresponding to this last one must be made now.
*/
		AR.Stage4Name ^= 1;
/*
		s = (UBYTE *)(fout->name); while ( *s ) s++;
		if ( AR.Stage4Name ) s[-1] += 1;
		else                s[-1] -= 1;
*/
		S->iPatches = S->fPatches;
		S->fPatches = S->inPatches;
		S->inPatches = S->iPatches;
		(S->inNum) = S->fPatchN;
		AN.OldPosIn = AN.OldPosOut;
#ifdef WITHZLIB
		m1 = S->fpincompressed;
		S->fpincompressed = S->fpcompressed;
		S->fpcompressed = m1;
		for ( i = 0; i < S->inNum; i++ ) {
			S->fPatchesStop[i] = S->iPatches[i+1];
#ifdef GZIPDEBUG
			MLOCK(ErrorMessageLock);
			MesPrint("%w fPatchesStop[%d] = %10p",i,&(S->fPatchesStop[i]));
			MUNLOCK(ErrorMessageLock);
#endif
		}
#endif
		S->stage4 = 0;
		goto FileMake;
	}
	else {
#ifdef WITHZLIB
/*
		The next statement is just for now
*/
		AR.gzipCompress = 0;
#endif
		if ( par == 0 ) {
			S->iPatches = S->fPatches;
			S->inNum = S->fPatchN;
#ifdef WITHZLIB
			m1 = S->fpincompressed;
			S->fpincompressed = S->fpcompressed;
			S->fpcompressed = m1;
			for ( i = 0; i < S->inNum; i++ ) {
				S->fPatchesStop[i] = S->fPatches[i+1];
#ifdef GZIPDEBUG
				MLOCK(ErrorMessageLock);
				MesPrint("%w fPatchesStop[%d] = %10p",i,&(S->fPatchesStop[i]));
				MUNLOCK(ErrorMessageLock);
#endif
			}
#endif
		}
		fout = AR.outfile;
	}
	if ( par ) {				/* Mark end of patches */
		S->Patches[S->lPatch] = S->lFill;
		for ( i = 0; i < S->lPatch; i++ ) {
			S->pStop[i] = S->Patches[i+1]-1;
		    S->Patches[i] = (WORD *)(((UBYTE *)(S->Patches[i])) + AM.MaxTer);
		}
	}
	else {	/* Load the patches */
		S->lPatch = (S->inNum);
#ifdef WITHMPI
		if ( S->lPatch > 1 || ( (PF.exprtodo <0) && (fout == AR.outfile || fout == AR.hidefile ) ) ) {
#else
		if ( S->lPatch > 1 ) {
#endif
#ifdef WITHZLIB
			SetupAllInputGZIP(S);
#endif
			p = S->lBuffer;
			for ( i = 0; i < S->lPatch; i++ ) {
				p = (WORD *)(((UBYTE *)p)+2*AM.MaxTer+COMPINC*sizeof(WORD));
				S->Patches[i] = p;
				p = (WORD *)(((UBYTE *)p) + fin->POsize);
				S->pStop[i] = m2 = p;
#ifdef WITHZLIB
				PutIn(fin,&(S->iPatches[i]),S->Patches[i],&m2,i);
#else
				ADDPOS(S->iPatches[i],PutIn(fin,&(S->iPatches[i]),S->Patches[i],&m2,i));
#endif
			}
		}
	}
	if ( fout->handle >= 0 ) {
		PUTZERO(position);
#ifdef ALLLOCK
		LOCK(fout->pthreadslock);
#endif
		SeekFile(fout->handle,&position,SEEK_END);
		ADDPOS(position,((fout->POfill-fout->PObuffer)*sizeof(WORD)));
#ifdef ALLLOCK
		UNLOCK(fout->pthreadslock);
#endif
	}
	else {
		SETBASEPOSITION(position,(fout->POfill-fout->PObuffer)*sizeof(WORD));
	}
/*
 		#] Setup : 

	The old code had to be replaced because all output needs to go
	through PutOut. For this we have to go term by term and keep
	track of the compression.
*/
	if ( S->lPatch == 1 ) {	/* Single patch --> direct copy. Very rare. */
		LONG length;

		if ( fout->handle < 0 ) if ( Sflush(fout) ) goto PatCall;
		if ( par ) {		/* Memory to file */
#ifdef WITHZLIB
/*
			We fix here the problem that the thing needs to go through PutOut
*/
			m2 = m1 = *S->Patches; /* The m2 is to keep the compiler from complaining */
			while ( *m1 ) {
				if ( *m1 < 0 ) { /* Need to uncompress */
					i = -(*m1++); m2 += i; im = *m1+i+1;
					while ( i > 0 ) { *m1-- = *m2--; i--; }
					*m1 = im;
				}
#ifdef WITHPTHREADS
				if ( AS.MasterSort && ( fout == AR.outfile ) ) { im = PutToMaster(BHEAD m1); }
				else
#endif
				if ( ( im = PutOut(BHEAD m1,&position,fout,1) ) < 0 ) goto ReturnError;
				ADDPOS(S->SizeInFile[par],im);
				m2 = m1;
				m1 += *m1;
			}
#ifdef WITHPTHREADS
			if ( AS.MasterSort && ( fout == AR.outfile ) ) { PutToMaster(BHEAD 0); }
			else
#endif
			if ( FlushOut(&position,fout,1) ) goto ReturnError;
			ADDPOS(S->SizeInFile[par],1);
#else
/* old code */
			length = (LONG)(*S->pStop)-(LONG)(*S->Patches)+sizeof(WORD);
			if ( WriteFile(fout->handle,(UBYTE *)(*S->Patches),length) != length )
				goto PatwCall;
			ADDPOS(position,length);
			ADDPOS(fout->POposition,length);
			ADDPOS(fout->filesize,length);
			ADDPOS(S->SizeInFile[par],length/sizeof(WORD));
#endif
		}
		else {				/* File to file */
#ifdef WITHZLIB
/*
			Note: if we change FRONTSIZE we need to make the minimum value
			of SmallEsize in AllocSort correspondingly larger or smaller.
			Theoretically we could get close to 2*AM.MaxTer!
*/
			#define FRONTSIZE (2*AM.MaxTer)
			WORD *copybuf = (WORD *)(((UBYTE *)(S->sBuffer)) + FRONTSIZE);
			WORD *copytop;
			SetupAllInputGZIP(S);
			m1 = m2 = copybuf;
			position2 = S->iPatches[0];
			while ( ( length = FillInputGZIP(fin,&position2,
					(UBYTE *)copybuf,
					(S->SmallEsize*sizeof(WORD)-FRONTSIZE),0) ) > 0 ) {
				copytop = (WORD *)(((UBYTE *)copybuf)+length);
				while ( *m1 && ( ( *m1 > 0 && m1+*m1 < copytop ) ||
				( *m1 < 0 && ( m1+1 < copytop ) && ( m1+m1[1]+1 < copytop ) ) ) )
/*
	22-jun-2013 JV  Extremely nasty bug that has been around for a while.
	                What if the end is in the remaining part? We will loose terms!
				while ( *m1 && ( (WORD *)(((UBYTE *)(m1)) + AM.MaxTer ) < S->sTop2 ) )
*/
				{
					if ( *m1 < 0 ) { /* Need to uncompress */
						i = -(*m1++); m2 += i; im = *m1+i+1;
						while ( i > 0 ) { *m1-- = *m2--; i--; }
						*m1 = im;
					}
#ifdef WITHPTHREADS
					if ( AS.MasterSort && ( fout == AR.outfile ) ) {
						im = PutToMaster(BHEAD m1);
					}
					else
#endif
					if ( ( im = PutOut(BHEAD m1,&position,fout,1) ) < 0 ) goto ReturnError;
					ADDPOS(S->SizeInFile[par],im);
					m2 = m1;
					m1 += *m1;
				}
				if ( m1 < copytop && *m1 == 0 ) break;
/*
				Now move the remaining part 'back'
*/
			    m3 = copybuf;
				m1 = copytop;
				while ( m1 > m2 ) *--m3 = *--m1;
				m2 = m3;
				m1 = m2 + *m2;
			}
			if ( length < 0 ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Readerror");
				goto PatCall2;
			}
#ifdef WITHPTHREADS
			if ( AS.MasterSort && ( fout == AR.outfile ) ) { PutToMaster(BHEAD 0); }
			else
#endif
			if ( FlushOut(&position,fout,1) ) goto ReturnError;
			ADDPOS(S->SizeInFile[par],1);
#else
/* old code */
			SeekFile(fin->handle,&(S->iPatches[0]),SEEK_SET); /* needed for stage4 */
			while ( ( length = ReadFile(fin->handle,
					(UBYTE *)(S->sBuffer),S->SmallEsize*sizeof(WORD)) ) > 0 ) {
				if ( WriteFile(fout->handle,(UBYTE *)(S->sBuffer),length) != length )
					goto PatwCall;
				ADDPOS(position,length);
				ADDPOS(fout->POposition,length);
				ADDPOS(fout->filesize,length);
				ADDPOS(S->SizeInFile[par],length/sizeof(WORD));
			}
			if ( length < 0 ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Readerror");
				goto PatCall2;
			}
#endif
		}
		goto EndOfAll;
	}
	else if ( S->lPatch > 0 ) {

		/* More than one patch. Construct the tree. */

		lpat = 1;
		do { lpat *= 2; } while ( lpat < S->lPatch );
		mpat = ( lpat >> 1 ) - 1;
		k = lpat - S->lPatch;

		/* k is the number of empty places in the tree. they will
		   be at the even positions from 2 to 2*k */

		for ( i = 1; i < lpat; i++ ) {
			S->tree[i] = -1;
		}
		for ( i = 1; i <= k; i++ ) {
			im = ( i * 2 ) - 1;
			poin[im] = S->Patches[i-1];
			poin2[im] = poin[im] + *(poin[im]);
			S->used[i] = im;
			S->ktoi[im] = i-1;
			S->tree[mpat+i] = 0;
			poin[im-1] = poin2[im-1] = 0;
		}
		for ( i = (k*2)+1; i <= lpat; i++ ) {
			S->used[i-k] = i;
			S->ktoi[i] = i-k-1;
			poin[i] = S->Patches[i-k-1];
			poin2[i] = poin[i] + *(poin[i]);
		}
/*
		the array poin tells the position of the i-th element of the S->tree
		'S->used' is a stack with the S->tree elements that need to be entered
		into the S->tree. at the beginning this is S->lPatch. during the
		sort there will be only very few elements.
		poin2 is the next value of poin. it has to be determined
		before the comparisons as the position or the size of the
		term indicated by poin may change.
		S->ktoi translates a S->tree element back to its stream number.

		start the sort
*/
		level = S->lPatch;

		/* introduce one term */
OneTerm:
		k = S->used[level];
		i = k + lpat - 1;
		if ( !*(poin[k]) ) {
			do { if ( !( i >>= 1 ) ) goto EndOfMerge; } while ( !S->tree[i] );
			if ( S->tree[i] == -1 ) {
				S->tree[i] = 0;
				level--;
				goto OneTerm;
			}
			k = S->tree[i];
			S->used[level] = k;
			S->tree[i] = 0;
		}
/*
		move terms down the tree
*/
		while ( i >>= 1 ) {
			if ( S->tree[i] > 0 ) {
				if ( ( c = CompareTerms(poin[S->tree[i]],poin[k],(WORD)0) ) > 0 ) {
/*
					S->tree[i] is the smaller. Exchange and go on.
*/
					S->used[level] = S->tree[i];
					S->tree[i] = k;
					k = S->used[level];
				}
				else if ( !c ) {	/* Terms are equal */
					S->TermsLeft--;
/*
						Here the terms are equal and their coefficients
						have to be added.
*/
					l1 = *( m1 = poin[S->tree[i]] );
					l2 = *( m2 = poin[k] );
					if ( S->PolyWise ) {  /* Here we work with PolyFun */
						WORD *tt1, *w;
						tt1 = m1;
						m1 += S->PolyWise;
						m2 += S->PolyWise;
						if ( S->PolyFlag == 2 ) {
							w = poly_ratfun_add(BHEAD m1,m2);
							if ( *tt1 + w[1] - m1[1] > AM.MaxTer/((LONG)sizeof(WORD)) ) {
								MLOCK(ErrorMessageLock);
								MesPrint("Term too complex in PolyRatFun addition. MaxTermSize of %10l is too small",AM.MaxTer);
								MUNLOCK(ErrorMessageLock);
								Terminate(-1);
							}
							AT.WorkPointer = w;
						}
						else {
							w = AT.WorkPointer;
							if ( w + m1[1] + m2[1] > AT.WorkTop ) {
								MLOCK(ErrorMessageLock);
								MesPrint("A WorkSpace of %10l is too small",AM.WorkSize);
								MUNLOCK(ErrorMessageLock);
								Terminate(-1);
							}
							AddArgs(BHEAD m1,m2,w);
						}
						r1 = w[1];
						if ( r1 <= FUNHEAD
							|| ( w[FUNHEAD] == -SNUMBER && w[FUNHEAD+1] == 0 ) )
								 { goto cancelled; }
						if ( r1 == m1[1] ) {
							NCOPY(m1,w,r1);
						}
						else if ( r1 < m1[1] ) {
							r2 = m1[1] - r1;
							m2 = w + r1;
							m1 += m1[1];
							while ( --r1 >= 0 ) *--m1 = *--m2;
							m2 = m1 - r2;
							r1 = S->PolyWise;
							while ( --r1 >= 0 ) *--m1 = *--m2;
							*m1 -= r2;
							poin[S->tree[i]] = m1;
						}
						else {
							r2 = r1 - m1[1];
							m2 = tt1 - r2;
							r1 = S->PolyWise;
							m1 = tt1;
							*m1 += r2;
							poin[S->tree[i]] = m2;
							NCOPY(m2,m1,r1);
							r1 = w[1];
							NCOPY(m2,w,r1);
						}
					}
					else {
					  r1 = *( m1 += l1 - 1 );
					  m1 -= ABS(r1) - 1;
					  r1 = ( ( r1 > 0 ) ? (r1-1) : (r1+1) ) >> 1;
					  r2 = *( m2 += l2 - 1 );
					  m2 -= ABS(r2) - 1;
					  r2 = ( ( r2 > 0 ) ? (r2-1) : (r2+1) ) >> 1;

					  if ( AddRat(BHEAD (UWORD *)m1,r1,(UWORD *)m2,r2,coef,&r3) ) {
						MLOCK(ErrorMessageLock);
						MesCall("MergePatches");
						MUNLOCK(ErrorMessageLock);
						SETERROR(-1)
					  }

					  if ( AN.ncmod != 0 ) {
						if ( ( AC.modmode & POSNEG ) != 0 ) {
							NormalModulus(coef,&r3);
						}
						else if ( BigLong(coef,r3,(UWORD *)AC.cmod,ABS(AN.ncmod)) >= 0 ) {
							WORD ii;
							SubPLon(coef,r3,(UWORD *)AC.cmod,ABS(AN.ncmod),coef,&r3);
							coef[r3] = 1;
							for ( ii = 1; ii < r3; ii++ ) coef[r3+ii] = 0;
						}
					  }
					  r3 *= 2;
					  r33 = ( r3 > 0 ) ? ( r3 + 1 ) : ( r3 - 1 );
					  if ( r3 < 0 ) r3 = -r3;
					  if ( r1 < 0 ) r1 = -r1;
					  r1 *= 2;
					  r31 = r3 - r1;
					  if ( !r3 ) {		/* Terms cancel */
cancelled:
						ul = S->used[level] = S->tree[i];
						S->tree[i] = -1;
/*
						We skip to the next term in stream ul
*/
						im = *poin2[ul];
						if ( im < 0 ) {
							r1 = poin2[ul][1] - im + 1;
							m1 = poin2[ul] + 2;
							m2 = poin[ul] - im + 1;
							while ( ++im <= 0 ) *--m1 = *--m2;
							*--m1 = r1;
							poin2[ul] = m1;
							im = r1;
						}
						poin[ul] = poin2[ul];
						ki = S->ktoi[ul];
						if ( !par && (poin[ul] + im + COMPINC) >= S->pStop[ki]
						&& im > 0 ) {
#ifdef WITHZLIB
							PutIn(fin,&(S->iPatches[ki]),S->Patches[ki],&(poin[ul]),ki);
#else
							ADDPOS(S->iPatches[ki],PutIn(fin,&(S->iPatches[ki]),
							S->Patches[ki],&(poin[ul]),ki));
#endif
							poin2[ul] = poin[ul] + im;
						}
						else {
							poin2[ul] += im;
						}
						S->used[++level] = k;
						S->TermsLeft--;
					  }
					  else if ( !r31 ) {		/* copy coef into term1 */
						goto CopCof2;
					  }
					  else if ( r31 < 0 ) {		/* copy coef into term1
											and adjust the length of term1 */
						goto CopCoef;
					  }
					  else {
/*
							this is the dreaded calamity.
							is there enough space?
*/
						if( (poin[S->tree[i]]+l1+r31) >= poin2[S->tree[i]] ) {
/*
								no space! now the special trick for which
								we left 2*maxlng spaces open at the beginning
								of each patch.
*/
							if ( (l1 + r31) > AM.MaxTer/((LONG)sizeof(WORD)) ) {
								MLOCK(ErrorMessageLock);
								MesPrint("Coefficient overflow during sort");
								MUNLOCK(ErrorMessageLock);
								goto ReturnError;
							}
							m2 = poin[S->tree[i]];
							m3 = ( poin[S->tree[i]] -= r31 );
							do { *m3++ = *m2++; } while ( m2 < m1 );
							m1 = m3;
						}
CopCoef:
						*(poin[S->tree[i]]) += r31;
CopCof2:
						m2 = (WORD *)coef; im = r3;
						NCOPY(m1,m2,im);
						*m1 = r33;
					  }
					}
/*
					Now skip to the next term in stream k.
*/
NextTerm:
					im = poin2[k][0];
					if ( im < 0 ) {
						r1 = poin2[k][1] - im + 1;
						m1 = poin2[k] + 2;
						m2 = poin[k] - im + 1;
						while ( ++im <= 0 ) *--m1 = *--m2;
						*--m1 = r1;
						poin2[k] = m1;
						im = r1;
					}
					poin[k] = poin2[k];
					ki = S->ktoi[k];
					if ( !par && ( (poin[k] + im + COMPINC) >= S->pStop[ki] )
					&& im > 0 ) {
#ifdef WITHZLIB
						PutIn(fin,&(S->iPatches[ki]),S->Patches[ki],&(poin[k]),ki);
#else
						ADDPOS(S->iPatches[ki],PutIn(fin,&(S->iPatches[ki]),
						S->Patches[ki],&(poin[k]),ki));
#endif
						poin2[k] = poin[k] + im;
					}
					else {
						poin2[k] += im;
					}
					goto OneTerm;
				}
			}
			else if ( S->tree[i] < 0 ) {
				S->tree[i] = k;
				level--;
				goto OneTerm;
			}
		}
/*
			found the smallest in the set. indicated by k.
			write to its destination.
*/
#ifdef WITHPTHREADS
		if ( AS.MasterSort && ( fout == AR.outfile ) ) { im = PutToMaster(BHEAD poin[k]); }
		else
#endif
		if ( ( im = PutOut(BHEAD poin[k],&position,fout,1) ) < 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Called from MergePatches with k = %d (stream %d)",k,S->ktoi[k]);
			MUNLOCK(ErrorMessageLock);
			goto ReturnError;
		}
		ADDPOS(S->SizeInFile[par],im);
		goto NextTerm;
	}
	else {
		goto NormalReturn;
	}
EndOfMerge:
#ifdef WITHPTHREADS
		if ( AS.MasterSort && ( fout == AR.outfile ) ) { PutToMaster(BHEAD 0); }
		else
#endif
	if ( FlushOut(&position,fout,1) ) goto ReturnError;
	ADDPOS(S->SizeInFile[par],1);
EndOfAll:
	if ( par == 1 ) {	/* Set the fpatch pointers */
#ifdef WITHZLIB
		SeekFile(fout->handle,&position,SEEK_CUR);
#endif
		(S->fPatchN)++;
		S->fPatches[S->fPatchN] = position;
	}
	if ( par == 0 && fout != AR.outfile ) {
/*
			Output went to sortfile. We have two possibilities:
			1:	We are not finished with the current in-out cycle
				In that case we should pop to the next set of patches
			2:	We finished a cycle and should clean up the in file
				Then we restart the sort.
*/
		(S->fPatchN)++;
		S->fPatches[S->fPatchN] = position;
		if ( ISNOTZEROPOS(AN.OldPosIn) ) {		/* We are not done */

			SeekFile(fin->handle,&(AN.OldPosIn),SEEK_SET);
/*
			We don't need extra provisions for the zlib compression here.
			If part of an expression has been sorted, the whole has been so.
			This means that S->fpincompressed[] will remain the same
*/
			if ( (ULONG)ReadFile(fin->handle,(UBYTE *)(&(S->inNum)),(LONG)sizeof(WORD)) !=
				sizeof(WORD)
			  || (ULONG)ReadFile(fin->handle,(UBYTE *)(&AN.OldPosIn),(LONG)sizeof(POSITION)) !=
				sizeof(POSITION)
			  || (ULONG)ReadFile(fin->handle,(UBYTE *)S->iPatches,(LONG)((S->inNum)+1)
					*sizeof(POSITION)) != ((S->inNum)+1)*sizeof(POSITION) ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Read error fourth stage sorting");
				MUNLOCK(ErrorMessageLock);
				goto ReturnError;
			}
			*rr = 0;
#ifdef WITHZLIB
			for ( i = 0; i < S->inNum; i++ ) {
				S->fPatchesStop[i] = S->iPatches[i+1];
#ifdef GZIPDEBUG
				MLOCK(ErrorMessageLock);
				MesPrint("%w fPatchesStop[%d] = %10p",i,&(S->fPatchesStop[i]));
				MUNLOCK(ErrorMessageLock);
#endif
			}
#endif
			goto ConMer;
		}
		else {
/*
			if ( fin == &(AR.FoStage4[0]) ) {
				s = (UBYTE *)(fin->name); while ( *s ) s++;
				if ( AR.Stage4Name == 1 ) s[-1] -= 1;
				else                      s[-1] += 1;
			}
*/
/*			TruncateFile(fin->handle); */
			UpdateMaxSize();
#ifdef WITHZLIB
			ClearSortGZIP(fin);
#endif
			CloseFile(fin->handle);
			remove(fin->name);		/* Gives diskspace free again. */
#ifdef GZIPDEBUG
			MLOCK(ErrorMessageLock);
			MesPrint("%w MergePatches removed in file %s",fin->name);
			MUNLOCK(ErrorMessageLock);
#endif
/*
			if ( fin == &(AR.FoStage4[0]) ) {
				s = (UBYTE *)(fin->name); while ( *s ) s++;
				if ( AR.Stage4Name == 1 ) s[-1] += 1;
				else                      s[-1] -= 1;
			}
*/
			fin->handle = -1;
			{ FILEHANDLE *ff = fin; fin = fout; fout = ff; }
			PUTZERO(S->SizeInFile[0]);
			goto NewMerge;
		}
	}
	if ( par == 0 ) {
/*		TruncateFile(fin->handle); */
		UpdateMaxSize();
#ifdef WITHZLIB
		ClearSortGZIP(fin);
#endif
		CloseFile(fin->handle);
		remove(fin->name);
		fin->handle = -1;
#ifdef GZIPDEBUG
		MLOCK(ErrorMessageLock);
		MesPrint("%w MergePatches removed in file %s",fin->name);
		MUNLOCK(ErrorMessageLock);
#endif
	}
NormalReturn:
#ifdef WITHZLIB
	AR.gzipCompress = oldgzipCompress;
#endif
	return(0);
ReturnError:
#ifdef WITHZLIB
	AR.gzipCompress = oldgzipCompress;
#endif
	return(-1);
#ifndef WITHZLIB
PatwCall:
	MLOCK(ErrorMessageLock);
	MesPrint("Error while writing to file.");
	goto PatCall2;
#endif
PatCall:;
	MLOCK(ErrorMessageLock);
PatCall2:;
	MesCall("MergePatches");
	MUNLOCK(ErrorMessageLock);
#ifdef WITHZLIB
	AR.gzipCompress = oldgzipCompress;
#endif
	SETERROR(-1)
}

/*
 		#] MergePatches : 
 		#[ StoreTerm :				WORD StoreTerm(term)
*/
/**
 *	The central routine to accept terms, store them and keep things
 *	at least partially sorted. A call to EndSort will then complete
 *	storing and sorting.
 *
 *	@param term The term to be stored
 *	@return  Regular return conventions (OK -> 0)
 */

WORD StoreTerm(PHEAD WORD *term)
{
	GETBIDENTITY
	SORTING *S = AT.SS;
	WORD **ss, *lfill, j, *t;
	POSITION pp;
	LONG lSpace, sSpace, RetCode, over, tover;

	if ( ( ( AP.PreDebug & DUMPTOSORT ) == DUMPTOSORT ) && AR.sLevel == 0 ) {
#ifdef WITHPTHREADS
		sprintf((char *)(THRbuf),"StoreTerm(%d)",AT.identity);
		PrintTerm(term,(char *)(THRbuf));
#else
		PrintTerm(term,"StoreTerm");
#endif
	}
	if ( AM.exitflag && AR.sLevel == 0 ) return(0);
	S->sFill = *(S->PoinFill);
	if ( S->sTerms >= S->TermsInSmall || ( S->sFill + *term ) >= S->sTop ) {
/*
	The small buffer is full. It has to be sorted and written.
*/
		tover = over = S->sTerms;
		ss = S->sPointer;
		ss[over] = 0;
#ifdef SPLITTIME
		PrintTime((UBYTE *)"Before SplitMerge");
#endif
		ss[SplitMerge(BHEAD ss,over)] = 0;
#ifdef SPLITTIME
		PrintTime((UBYTE *)"After SplitMerge");
#endif
		sSpace = 0;
		if ( over > 0 ) {
			sSpace = ComPress(ss,&RetCode);
			S->TermsLeft -= over - RetCode;
		}
		sSpace++;

		lSpace = sSpace + (S->lFill - S->lBuffer)
				 - (AM.MaxTer/sizeof(WORD))*((LONG)S->lPatch);
		SETBASEPOSITION(pp,lSpace);
		MULPOS(pp,sizeof(WORD));
		if ( S->file.handle >= 0 ) {
			ADD2POS(pp,S->fPatches[S->fPatchN]);
		}
		if ( S == AT.S0 ) {	/* Only statistics at ground level */
			WORD oldLogHandle = AC.LogHandle;
			if ( AC.LogHandle >= 0 && AM.LogType ) AC.LogHandle = -1;
			WriteStats(&pp,(WORD)0);
			AC.LogHandle = oldLogHandle;
		}
		if ( ( S->lPatch >= S->MaxPatches ) ||
			( ( (WORD *)(((UBYTE *)(S->lFill + sSpace)) + 2*AM.MaxTer ) ) >= S->lTop ) ) {
/*
			The large buffer is too full. Merge and write it
*/
			if ( MergePatches(1) ) goto StoreCall;
/*
			pp = S->SizeInFile[1];
			ADDPOS(pp,sSpace);
			MULPOS(pp,sizeof(WORD));
*/
			SETBASEPOSITION(pp,sSpace);
			MULPOS(pp,sizeof(WORD));
			ADD2POS(pp,S->fPatches[S->fPatchN]);

			if ( S == AT.S0 ) {	/* Only statistics at ground level */
				WORD oldLogHandle = AC.LogHandle;
				if ( AC.LogHandle >= 0 && AM.LogType ) AC.LogHandle = -1;
				WriteStats(&pp,(WORD)1);
				AC.LogHandle = oldLogHandle;
			}
			S->lPatch = 0;
			S->lFill = S->lBuffer;
		}
		S->Patches[S->lPatch++] = S->lFill;
	    lfill = (WORD *)(((UBYTE *)(S->lFill)) + AM.MaxTer);
		if ( tover > 0 ) {
			ss = S->sPointer;
			while ( ( t = *ss++ ) != 0 ) {
				j = *t;
				if ( j < 0 ) j = t[1] + 2;
				while ( --j >= 0 ){
				  *lfill++ = *t++;
				}
			}
		}
		*lfill++ = 0;
		S->lFill = lfill;
		S->sTerms = 0;
		S->PoinFill = S->sPointer;
		*(S->PoinFill) = S->sFill = S->sBuffer;
	}
	j = *term;
	while ( --j >= 0 ) *S->sFill++ = *term++;
	S->sTerms++;
	S->GenTerms++;
	S->TermsLeft++;
	*++S->PoinFill = S->sFill;

	return(0);

StoreCall:
	MLOCK(ErrorMessageLock);
	MesCall("StoreTerm");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] StoreTerm : 
 		#[ StageSort :				VOID StageSort(FILEHANDLE *fout)
*/
/**
 *		Prepares a stage 4 or higher sort.
 *		Stage 4 sorts occur when the sort file contains more patches than
 *		can be merged in one pass.
 */

VOID StageSort(FILEHANDLE *fout)
{
	GETIDENTITY
	SORTING *S = AT.SS;
	if ( S->fPatchN >= S->MaxFpatches ) {
		POSITION position;
		if ( S != AT.S0 ) {
/*
			There are no proper provisions for stage 4 or higher sorts
			for function arguments and $ variables. The reason:
			The current code maps out the patches, based on the size of
			the buffers in the FoStage4 structs, while they are used
			inside the S->file struct that may have far smaller buffers.
			By itself that might still be repairable, but it goes completely
			wrong when during the sort polyRatFuns have to be added and they
			would go into stage4 (very rare but possible).
			The only really correct solution would be to put FoStage4 structs
			in all sort levels. Messy. (JV 8-oct-2018).
*/
			MLOCK(ErrorMessageLock);
			MesPrint("Currently Stage 4 sorts are not allowed for function arguments or $ variables.");
			MesPrint("Please increase correspondingsorting parameters (sub-) in the setup.");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		PUTZERO(position);
		MLOCK(ErrorMessageLock);
#ifdef WITHPTHREADS
		MesPrint("StageSort in thread %d",identity);
#elif defined(WITHMPI)
		MesPrint("StageSort in process %d",PF.me);
#else
		MesPrint("StageSort");
#endif
		MUNLOCK(ErrorMessageLock);
		SeekFile(fout->handle,&position,SEEK_END);
/*
		No extra compression data has to be written.
		S->fpincompressed should remain valid.
*/
		if ( (ULONG)WriteFile(fout->handle,(UBYTE *)(&(S->fPatchN)),(LONG)sizeof(WORD)) !=
			sizeof(WORD)
		  || (ULONG)WriteFile(fout->handle,(UBYTE *)(&(AN.OldPosOut)),(LONG)sizeof(POSITION)) !=
			sizeof(POSITION)
		  || (ULONG)WriteFile(fout->handle,(UBYTE *)(S->fPatches),(LONG)(S->fPatchN+1)
					*sizeof(POSITION)) != (S->fPatchN+1)*sizeof(POSITION) ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Write error while staging sort. Disk full?");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		AN.OldPosOut = position;
		fout->filesize = position;
		ADDPOS(fout->filesize,(S->fPatchN+2)*sizeof(POSITION) + sizeof(WORD));
		fout->POposition = fout->filesize;
		S->fPatches[0] = fout->filesize;
		S->fPatchN = 0;

		if ( AR.FoStage4[0].PObuffer == 0 ) {
			AR.FoStage4[0].PObuffer = (WORD *)Malloc1(AR.FoStage4[0].POsize*sizeof(WORD)
												,"Stage 4 buffer");
			AR.FoStage4[0].POfill   = AR.FoStage4[0].PObuffer;
			AR.FoStage4[0].POstop   = AR.FoStage4[0].PObuffer
						 + AR.FoStage4[0].POsize/sizeof(WORD);
#ifdef WITHPTHREADS
			AR.FoStage4[0].pthreadslock = dummylock;
#endif
		}
		if ( AR.FoStage4[1].PObuffer == 0 ) {
			AR.FoStage4[1].PObuffer = (WORD *)Malloc1(AR.FoStage4[1].POsize*sizeof(WORD)
												,"Stage 4 buffer");
			AR.FoStage4[1].POfill   = AR.FoStage4[1].PObuffer;
			AR.FoStage4[1].POstop   = AR.FoStage4[1].PObuffer
						 + AR.FoStage4[1].POsize/sizeof(WORD);
#ifdef WITHPTHREADS
			AR.FoStage4[1].pthreadslock = dummylock;
#endif
		}
		S->stage4 = 1;
	}
}

/*
 		#] StageSort : 
 		#[ SortWild :				WORD SortWild(w,nw)
*/
/**
 *	Sorts the wildcard entries in the parameter w. Double entries
 *	are removed. Full space taken is nw words.
 *	Routine serves for the reading of wildcards in the compiler.
 *	The entries come in the format:
 *	(type,4,number,0) in which the zero is reserved for the
 *	future replacement of 'number'.
 *
 *	@param w  buffer with wildcard entries.
 *	@param nw number of wildcard entries.
 *	@return  Normal conventions (OK -> 0)
 */

WORD SortWild(WORD *w, WORD nw)
{
	GETIDENTITY
	WORD *v, *s, *m, k, i;
	WORD *pScrat, *stop, *sv, error = 0;
	pScrat = AT.WorkPointer;
	if ( ( AT.WorkPointer + 8 * AM.MaxWildcards ) >= AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	stop = w + nw;
	i = 0;
	while ( i < nw ) {
		m = w + i;
		v = m + m[1];
		while ( v < stop && (
			 *v == FROMSET || *v == SETTONUM || *v == LOADDOLLAR ) ) v += v[1];
		while ( v < stop ) {
			if ( *v >= 0 ) {
				if ( AM.Ordering[*v] < AM.Ordering[*m] ) {
					m = v;
				}
				else if ( *v == *m ) {
					if ( v[2] < m[2] ) {
						m = v;
					}
					else if ( v[2] == m[2] ) {
						s = m + m[1];
						sv = v + v[1];
						if ( s < stop && ( *s == FROMSET
						 || *s == SETTONUM || *s == LOADDOLLAR ) ) {
							if ( sv < stop && ( *sv == FROMSET
							|| *sv == SETTONUM || *sv == LOADDOLLAR ) ) {
								if ( s[2] != sv[2] ) {
									error = -1;
									MLOCK(ErrorMessageLock);
									MesPrint("&Wildcard set conflict");
									MUNLOCK(ErrorMessageLock);
								}
							}
							*v = -1;
						}
						else {
							if ( sv < stop && ( *sv == FROMSET
							|| *sv == SETTONUM || *sv == LOADDOLLAR ) ) {
								*m = -1;
								m = v;
							}
							else {
								*v = -1;
							}
						}
					}
				}
			}
			v += v[1];
			while ( v < stop && ( *v == FROMSET
			 || *v == SETTONUM || *v == LOADDOLLAR ) ) v += v[1];
		}
		s = pScrat;
		v = m;
		k = m[1];
		NCOPY(s,m,k);
		while ( m < stop && ( *m == FROMSET
		 || *m == SETTONUM || *m == LOADDOLLAR ) ) {
			k = m[1];
			NCOPY(s,m,k);
		}
		*v = -1;
		pScrat = s;
		i = 0;
		while ( i < nw && ( w[i] < 0 || w[i] == FROMSET
		|| w[i] == SETTONUM || w[i] == LOADDOLLAR ) ) i += w[i+1];
	}
	AC.NwildC = k = WORDDIF(pScrat,AT.WorkPointer);
	s = AT.WorkPointer;
	m = w;
	NCOPY(m,s,k);
	AC.WildC = m;
	return(error);
}

/*
 		#] SortWild : 
 		#[ CleanUpSort :			VOID CleanUpSort(num)
*/
/**
 *		Partially or completely frees function sort buffers.
 */

void CleanUpSort(int num)
{
	GETIDENTITY
	SORTING *S;
	int minnum = num, i;
	if ( AN.FunSorts ) {
		if ( num == -1 ) {
			if ( AN.MaxFunSorts > 3 ) {
				minnum = (AN.MaxFunSorts+4)/2;
			}
			else minnum = 4;
		}
		else if ( minnum == 0 ) minnum = 1;
		for ( i = minnum; i < AN.NumFunSorts; i++ ) {
			S = AN.FunSorts[i];
			if ( S ) {
				if ( S->file.handle >= 0 ) {
/*					TruncateFile(S->file.handle); */
					UpdateMaxSize();
#ifdef WITHZLIB
					ClearSortGZIP(&(S->file));
#endif
					CloseFile(S->file.handle);
					S->file.handle = -1;
					remove(S->file.name);
#ifdef GZIPDEBUG
					MLOCK(ErrorMessageLock);
					MesPrint("%w CleanUpSort removed file %s",S->file.name);
					MUNLOCK(ErrorMessageLock);
#endif
				}
				M_free(S,"sorting struct");
			}
			AN.FunSorts[i] = 0;
		}
		AN.MaxFunSorts = minnum;
		if ( num == 0 ) {
			S = AN.FunSorts[0];
			if ( S ) {
				if ( S->file.handle >= 0 ) {
/*					TruncateFile(S->file.handle); */
					UpdateMaxSize();
#ifdef WITHZLIB
					ClearSortGZIP(&(S->file));
#endif
					CloseFile(S->file.handle);
					S->file.handle = -1;
					remove(S->file.name);
#ifdef GZIPDEBUG
					MLOCK(ErrorMessageLock);
					MesPrint("%w CleanUpSort removed file %s",S->file.name);
					MUNLOCK(ErrorMessageLock);
#endif
				}
			}
		}
	}
	for ( i = 0; i < 2; i++ ) {
		if ( AR.FoStage4[i].handle >= 0 ) {
			UpdateMaxSize();
#ifdef WITHZLIB
			ClearSortGZIP(&(AR.FoStage4[i]));
#endif
			CloseFile(AR.FoStage4[i].handle);
			remove(AR.FoStage4[i].name);
			AR.FoStage4[i].handle = -1;
#ifdef GZIPDEBUG
			MLOCK(ErrorMessageLock);
			MesPrint("%w CleanUpSort removed stage4 file %s",AR.FoStage4[i].name);
			MUNLOCK(ErrorMessageLock);
#endif
		}
	}
}

/*
 		#] CleanUpSort : 
 		#[ LowerSortLevel :         VOID LowerSortLevel()
*/
/**
 *		Lowers the level in the sort system.
 */

VOID LowerSortLevel()
{
	GETIDENTITY
	if ( AR.sLevel >= 0 ) {
		AR.sLevel--;
		if ( AR.sLevel >= 0 ) AT.SS = AN.FunSorts[AR.sLevel];
	}
}

/*
 		#] LowerSortLevel : 
 		#[ PolyRatFunSpecial :

		Keeps only the most divergent term in AR.PolyFunVar
		We assume that the terms are already in that notation.
*/

WORD *PolyRatFunSpecial(PHEAD WORD *t1, WORD *t2)
{
	WORD *oldworkpointer = AT.WorkPointer, *t, *r;
	WORD exp1, exp2;
	int i;
	t = t1+FUNHEAD;
	if ( *t == -SYMBOL ) {
		if ( t[1] != AR.PolyFunVar ) goto Illegal;
		exp1 = 1;
		if ( t[2] != -SNUMBER ) goto Illegal;
		t[3] = 1;
	}
	else if ( *t == -SNUMBER ) {
		t[1] = 1;
		t += 2;
		if ( *t == -SYMBOL ) {
			if ( t[1] != AR.PolyFunVar ) goto Illegal;
			exp1 = -1;
		}
		else if ( *t == -SNUMBER ) {
			t[1] = 1;
			exp1 = 0;
		}
		else if ( *t == ARGHEAD+8 && t[ARGHEAD] == 8 && t[ARGHEAD+1] == SYMBOL
			&& t[ARGHEAD+3] == AR.PolyFunVar ) {
			t[ARGHEAD+5] = 1;
			t[ARGHEAD+6] = 1;
			t[ARGHEAD+7] = 3;
			exp1 = -t[ARGHEAD+4];
		}
		else goto Illegal;
	}
	else if ( *t == ARGHEAD+8 && t[ARGHEAD] == 8 && t[ARGHEAD+1] == SYMBOL
		&& t[ARGHEAD+3] == AR.PolyFunVar ) {
		t[ARGHEAD+5] = 1;
		t[ARGHEAD+6] = 1;
		t[ARGHEAD+7] = 3;
		exp1 = t[ARGHEAD+4];
		t += *t;
		if ( *t != -SNUMBER ) goto Illegal;
		t[1] = 1;
	}
	else goto Illegal;

	t = t2+FUNHEAD;
	if ( *t == -SYMBOL ) {
		if ( t[1] != AR.PolyFunVar ) goto Illegal;
		exp2 = 1;
		if ( t[2] != -SNUMBER ) goto Illegal;
		t[3] = 1;
	}
	else if ( *t == -SNUMBER ) {
		t[1] = 1;
		t += 2;
		if ( *t == -SYMBOL ) {
			if ( t[1] != AR.PolyFunVar ) goto Illegal;
			exp2 = -1;
		}
		else if ( *t == -SNUMBER ) {
			t[1] = 1;
			exp2 = 0;
		}
		else if ( *t == ARGHEAD+8 && t[ARGHEAD] == 8 && t[ARGHEAD+1] == SYMBOL
			&& t[ARGHEAD+3] == AR.PolyFunVar ) {
			t[ARGHEAD+5] = 1;
			t[ARGHEAD+6] = 1;
			t[ARGHEAD+7] = 3;
			exp2 = -t[ARGHEAD+4];
		}
		else goto Illegal;
	}
	else if ( *t == ARGHEAD+8 && t[ARGHEAD] == 8 && t[ARGHEAD+1] == SYMBOL
		&& t[ARGHEAD+3] == AR.PolyFunVar ) {
		t[ARGHEAD+5] = 1;
		t[ARGHEAD+6] = 1;
		t[ARGHEAD+7] = 3;
		exp2 = t[ARGHEAD+4];
		t += *t;
		if ( *t != -SNUMBER ) goto Illegal;
		t[1] = 1;
	}
	else goto Illegal;

	if ( exp1 <= exp2 ) { i = t1[1]; r = t1; }
	else                { i = t2[1]; r = t2; }
	t = oldworkpointer;
	NCOPY(t,r,i)

	return(oldworkpointer);
Illegal:
	MesPrint("Illegal occurrence of PolyRatFun with divergent option");
	Terminate(-1);
	return(0);
}

/*
 		#] PolyRatFunSpecial : 
 		#[ SimpleSplitMerge :

		Sorts an array of WORDs. No adding of equal objects.
*/

VOID SimpleSplitMergeRec(WORD *array,WORD num,WORD *auxarray)
{
	WORD n1,n2,i,j,k,*t1,*t2;
	if ( num < 2 ) return;
	if ( num == 2 ) {
		if ( array[0] > array[1] ) {
			EXCH(array[0],array[1])
		}
		return;
	}
	n1 = num/2;
	n2 = num - n1;
	SimpleSplitMergeRec(array,n1,auxarray);
	SimpleSplitMergeRec(array+n1,n2,auxarray);
	if ( array[n1-1] <= array[n1] ) return;

	t1 = array; t2 = auxarray; i = n1; NCOPY(t2,t1,i);
	i = 0; j = n1; k = 0;
	while ( i < n1 && j < num ) {
		if ( auxarray[i] <= array[j] ) { array[k++] = auxarray[i++]; }
		else { array[k++] = array[j++]; }
	}
	while ( i < n1 ) array[k++] = auxarray[i++];
/*
	Remember: remnants of j are still in place!
*/
}

VOID SimpleSplitMerge(WORD *array,WORD num)
{
	WORD *auxarray = Malloc1(sizeof(WORD)*num/2,"SimpleSplitMerge");
	SimpleSplitMergeRec(array,num,auxarray);
	M_free(auxarray,"SimpleSplitMerge");
}

/*
 		#] SimpleSplitMerge : 
 		#[ BinarySearch :

		Searches in the sorted array with length num for the object x.
		If x is in the list, it returns the number of the array element
		that matched. If it is not in the list, it returns -1.
		If there are identical objects in the list, which one will
		match is quasi random.
*/

WORD BinarySearch(WORD *array,WORD num,WORD x)
{
	WORD i, bot, top, med;
	if ( num < 8 ) {
		for ( i = 0; i < num; i++ ) if ( array[i] == x ) return(i);
		return(-1);
	}
	if ( array[0] > x || array[num-1] < x ) return(-1);
	bot = 0; top = num-1; med = (top+bot)/2;
	do {
		if ( array[med] == x ) return(med);
		if ( array[med] < x ) { bot = med+1; }
		else { top = med-1; }
		med = (top+bot)/2;
	} while ( med >= bot && med <= top );
	return(-1);
}

/*
 		#] BinarySearch : 
	#] SortUtilities :
*/
