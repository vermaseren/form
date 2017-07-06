/** @file factor.c
 * 
 *	The routines for finding (one term) factors in dollars and expressions.
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
  	#[ Includes : factor.c
*/

#include "form3.h"

/*
  	#] Includes : 
  	#[ FactorIn :

	This routine tests for a factor in a dollar expression.

	Note that unlike with regular active or hidden expressions we cannot
	add memory as easily as dollars are rather volatile.
*/
int FactorIn(PHEAD WORD *term, WORD level)
{
	GETBIDENTITY
	WORD *t, *tstop, *m, *mm, *oldwork, *mstop, *n1, *n2, *n3, *n4, *n1stop, *n2stop;
	WORD *r1, *r2, *r3, *r4, j, k, kGCD, kGCD2, kLCM, jGCD, kkLCM, jLCM, size;
	UWORD *GCDbuffer, *GCDbuffer2, *LCMbuffer, *LCMb, *LCMc;
	int fromwhere = 0, i;
	DOLLARS d;
	t = term; GETSTOP(t,tstop); t++;
	while ( ( t < tstop ) && ( *t != FACTORIN || ( ( *t == FACTORIN )
	 && ( t[FUNHEAD] != -DOLLAREXPRESSION || t[1] != FUNHEAD+2 ) ) ) ) t += t[1];
	if ( t >= tstop ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Internal error. Could not find proper factorin_ function.");
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	oldwork = AT.WorkPointer;
	d = Dollars + t[FUNHEAD+1];
#ifdef WITHPTHREADS
	{
		int nummodopt, dtype = -1;
		if ( AS.MultiThreaded ) {
			for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
				if ( t[FUNHEAD+1] == ModOptdollars[nummodopt].number ) break;
			}
			if ( nummodopt < NumModOptdollars ) {
				dtype = ModOptdollars[nummodopt].type;
				if ( dtype == MODLOCAL ) {
					d = ModOptdollars[nummodopt].dstruct+AT.identity;
				}
			}
		}
	}
#endif

	if ( d->type == DOLTERMS ) {
		fromwhere = 1;
	}
	else if ( ( d = DolToTerms(BHEAD t[FUNHEAD+1]) ) == 0 ) {
/*
		The variable cannot convert to an expression
		We replace the function by 1.
*/
		m = oldwork; n1 = term;
		while ( n1 < t ) *m++ = *n1++;
		n1 = t + t[1]; tstop = term + *term;
		while ( n1 < tstop ) *m++ = *n1++;
		*oldwork = m - oldwork;
		AT.WorkPointer = m;
		if ( Generator(BHEAD oldwork,level) ) return(-1);
		AT.WorkPointer = oldwork;
		return(0);
	}
	if ( d->where[0] == 0 ) {
		if ( fromwhere == 0 ) {
			if ( d->factors ) M_free(d->factors,"Dollar factors");
			M_free(d,"Dollar in FactorIn_");
		}
		return(0);
	}
/*
	Now we have an expression in d->where. Find the symbolic factor that
	divides the expression and the numerical factor that makes all
	coefficients integer.

	For the symbolic factor we make a copy of the first term, and then
	go through all terms, scratching in the copy the objects that do not
	occur in the terms.
*/
	m = oldwork;
	mm = d->where;
	k = *mm - ABS((mm[*mm-1]));
	for ( j = 0; j < k; j++ ) *m++ = *mm++;
	mstop = m;
	*oldwork = k;
/*
	The copy is in place. Now search through the terms. Start at the second term
*/
	mm = d->where + d->where[0];
	while ( *mm ) {
		m = oldwork+1;
		r2 = mm+*mm;
		r2 -= ABS(r2[-1]);
		r1 = mm+1;
		while ( m < mstop ) {
			while ( r1 < r2 ) {
				if ( *r1 != *m ) {
					r1 += r1[1]; continue;
				}
/*
				Now the various cases
			#[ SYMBOL :
*/
				if ( *m == SYMBOL ) {
					n1 = m+2; n1stop = m+m[1];
					n2stop = r1+r1[1];
					while ( n1 < n1stop ) {
						n2 = r1+2;
						while ( n2 < n2stop ) {
							if ( *n1 != *n2 ) { n2 += 2; continue; }
							if ( n1[1] > 0 ) {
								if ( n2[1] < 0 ) { n2 += 2; continue; }
								if ( n2[1] < n1[1] ) n1[1] = n2[1];
							}
							else {
								if ( n2[1] > 0 ) { n2 += 2; continue; }
								if ( n2[1] > n1[1] ) n1[1] = n2[1];
							}
							break;
						}
						if ( n2 >= n2stop ) {	/* scratch symbol */
							if ( m[1] == 4 ) goto scratch;
							m[1] -= 2;
							n3 = n1; n4 = n1+2;
							while ( n4 < mstop ) *n3++ = *n4++;
							*oldwork = n3 - oldwork;
							mstop -= 2; n1stop -= 2;
							continue;
						}
						n1 += 2;
					}
					break;
				}
/*
			#] SYMBOL : 
			#[ DOTPRODUCT :
*/
				else if ( *m == DOTPRODUCT ) {
					n1 = m+2; n1stop = m+m[1];
					n2stop = r1+r1[1];
					while ( n1 < n1stop ) {
						n2 = r1+2;
						while ( n2 < n2stop ) {
							if ( *n1 != *n2 || n1[1] != n2[1] ) { n2 += 3; continue; }
							if ( n1[2] > 0 ) {
								if ( n2[2] < 0 ) { n2 += 3; continue; }
								if ( n2[2] < n1[2] ) n1[2] = n2[2];
							}
							else {
								if ( n2[2] > 0 ) { n2 += 3; continue; }
								if ( n2[2] > n1[2] ) n1[2] = n2[2];
							}
							break;
						}
						if ( n2 >= n2stop ) {	/* scratch symbol */
							if ( m[1] == 5 ) goto scratch;
							m[1] -= 3;
							n3 = n1; n4 = n1+3;
							while ( n4 < mstop ) *n3++ = *n4++;
							*oldwork = n3 - oldwork;
							mstop -= 3; n1stop -= 3;
							continue;
						}
						n1 += 3;
					}
					break;
				}
/*
			#] DOTPRODUCT : 
			#[ VECTOR :
*/
				else if ( *m == VECTOR ) {
/*
					Here we have to be careful if there is more than
					one of the same
*/
					n1 = m+2; n1stop = m+m[1];
					n2 = r1+2;n2stop = r1+r1[1];
					while ( n1 < n1stop ) {
						while ( n2 < n2stop ) {
							if ( *n1 == *n2 && n1[1] == n2[1] ) {
								n2 += 2; goto nextn1;
							}
							n2 += 2;
						}
						if ( n2 >= n2stop ) {	/* scratch symbol */
							if ( m[1] == 4 ) goto scratch;
							m[1] -= 2;
							n3 = n1; n4 = n1+2;
							while ( n4 < mstop ) *n3++ = *n4++;
							*oldwork = n3 - oldwork;
							mstop -= 2; n1stop -= 2;
							continue;
						}
						n2 = r1+2;
nextn1:					n1 += 2;
					}
					break;
				}
/*
			#] VECTOR : 
			#[ REMAINDER :
*/
				else {
/*
					Again: watch for multiple occurrences of the same object
*/
					if ( m[1] != r1[1] ) { r1 += r1[1]; continue; }
					for ( j = 2; j < m[1]; j++ ) {
						if ( m[j] != r1[j] ) break;
					}
					if ( j < m[1] ) { r1 += r1[1]; continue; }
					r1 += r1[1]; /* to restart at the next potential match */
					goto nextm;  /* match */
				}
/*
			#] REMAINDER : 
*/
			}
            if ( r1 >= r2 ) { /* no factor! */
scratch:;
				r3 = m + m[1]; r4 = m;
				while ( r3 < mstop ) *r4++ = *r3++;
				*oldwork = r4 - oldwork;
				if ( *oldwork == 1 ) goto nofactor;
				mstop = r4;
				r1 = mm + 1;
				continue;
			}
			r1 = mm + 1;
nextm:		m += m[1];
		}
		mm = mm + *mm;
	}

nofactor:;
/*
	For the coefficient we have to determine the LCM of the denominators
	and the GCD of the numerators.
*/
	GCDbuffer = NumberMalloc("FactorIn"); GCDbuffer2 = NumberMalloc("FactorIn");
	LCMbuffer = NumberMalloc("FactorIn"); LCMb = NumberMalloc("FactorIn"); LCMc = NumberMalloc("FactorIn");
	r1 = d->where;
/*
	First take the first term to load up the LCM and the GCD
*/
	r2 = r1 + *r1;
	j = r2[-1];
	r3 = r2 - ABS(j);
	k = REDLENG(j);
	if ( k < 0 ) k = -k;
	while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
	for ( kGCD = 0; kGCD < k; kGCD++ ) GCDbuffer[kGCD] = r3[kGCD];
	k = REDLENG(j);
	if ( k < 0 ) k = -k;
	r3 += k;
	while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
	for ( kLCM = 0; kLCM < k; kLCM++ ) LCMbuffer[kLCM] = r3[kLCM];
	r1 = r2;
/*
	Now go through the rest of the terms in this dollar buffer.
*/
	while ( *r1 ) {
		r2 = r1 + *r1;
		j = r2[-1];
		r3 = r2 - ABS(j);
		k = REDLENG(j);
		if ( k < 0 ) k = -k;
		while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
		if ( ( ( GCDbuffer[0] == 1 ) && ( kGCD == 1 ) ) ) {
/*
			GCD is already 1
*/
		}
		else if ( ( ( k != 1 ) || ( r3[0] != 1 ) ) ) {
			if ( GcdLong(BHEAD GCDbuffer,kGCD,(UWORD *)r3,k,GCDbuffer2,&kGCD2) ) {
				goto onerror;
			}
			kGCD = kGCD2;
			for ( i = 0; i < kGCD; i++ ) GCDbuffer[i] = GCDbuffer2[i];
		}
		else {
			kGCD = 1; GCDbuffer[0] = 1;
		}
		k = REDLENG(j);
		if ( k < 0 ) k = -k;
		r3 += k;
		while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
		if ( ( ( LCMbuffer[0] == 1 ) && ( kLCM == 1 ) ) ) {
			for ( kLCM = 0; kLCM < k; kLCM++ )
				LCMbuffer[kLCM] = r3[kLCM];
		}
		else if ( ( k != 1 ) || ( r3[0] != 1 ) ) {
			if ( GcdLong(BHEAD LCMbuffer,kLCM,(UWORD *)r3,k,LCMb,&kkLCM) ) {
				goto onerror;
			}
			DivLong((UWORD *)r3,k,LCMb,kkLCM,LCMb,&kkLCM,LCMc,&jLCM);
			MulLong(LCMbuffer,kLCM,LCMb,kkLCM,LCMc,&jLCM);
			for ( kLCM = 0; kLCM < jLCM; kLCM++ )
				LCMbuffer[kLCM] = LCMc[kLCM];
		}
		else {} /* LCM doesn't change */
		r1 = r2;
	}
/*
	Now put the factor together: GCD/LCM
*/
	r3 = (WORD *)(GCDbuffer);
	if ( kGCD == kLCM ) {
		for ( jGCD = 0; jGCD < kGCD; jGCD++ )
			r3[jGCD+kGCD] = LCMbuffer[jGCD];
		k = kGCD;
	}
	else if ( kGCD > kLCM ) {
		for ( jGCD = 0; jGCD < kLCM; jGCD++ )
			r3[jGCD+kGCD] = LCMbuffer[jGCD];
		for ( jGCD = kLCM; jGCD < kGCD; jGCD++ )
			r3[jGCD+kGCD] = 0;
		k = kGCD;
	}
	else {
		for ( jGCD = kGCD; jGCD < kLCM; jGCD++ )
			r3[jGCD] = 0;
		for ( jGCD = 0; jGCD < kLCM; jGCD++ )
			r3[jGCD+kLCM] = LCMbuffer[jGCD];
		k = kLCM;
	}
	j = 2*k+1;
	mm = m = oldwork + oldwork[0];
/*
	Now compose the new term
*/
	n1 = term;
	while ( n1 < t ) *m++ = *n1++;
	n1 += n1[1];
	n2 = oldwork+1;
	while ( n2 < mm ) *m++ = *n2++;
	while ( n1 < tstop ) *m++ = *n1++;
/*
	And the coefficient
*/
	size = term[*term-1];
	size = REDLENG(size);
	if ( MulRat(BHEAD (UWORD *)tstop,size,(UWORD *)r3,k,
								(UWORD *)m,&size) ) goto onerror;
	size = INCLENG(size);
	k = size < 0 ? -size: size;
	m[k-1] = size;
	m += k;
	*mm = (WORD)(m - mm);
	AT.WorkPointer = m;
	if ( Generator(BHEAD mm,level) ) goto onerror;
	AT.WorkPointer = oldwork;
	if ( fromwhere == 0 ) {
		if ( d->factors ) M_free(d->factors,"Dollar factors");
		M_free(d,"Dollar in FactorIn");
	}
	NumberFree(GCDbuffer,"FactorIn"); NumberFree(GCDbuffer2,"FactorIn");
	NumberFree(LCMbuffer,"FactorIn"); NumberFree(LCMb,"FactorIn"); NumberFree(LCMc,"FactorIn");
	return(0);
onerror:
	AT.WorkPointer = oldwork;
	MLOCK(ErrorMessageLock);
	MesCall("FactorIn");
	MUNLOCK(ErrorMessageLock);
	NumberFree(GCDbuffer,"FactorIn"); NumberFree(GCDbuffer2,"FactorIn");
	NumberFree(LCMbuffer,"FactorIn"); NumberFree(LCMb,"FactorIn"); NumberFree(LCMc,"FactorIn");
	return(-1);
}

/*
  	#] FactorIn : 
  	#[ FactorInExpr :

	This routine tests for a factor in an active or hidden expression.

	The factor from the last call is stored in a cache.
	Main problem here is whether the cache is global or private to each thread.
	A global cache gives most likely the same traffic jam for the computation
	as a local cache. The local cache may stay valid longer.
	In the future we may make it such that threads can look at the cache
	of the others, or even whether a certain factor is under construction.
*/

int FactorInExpr(PHEAD WORD *term, WORD level)
{
	GETBIDENTITY
	WORD *t, *tstop, *m, *oldwork, *mstop, *n1, *n2, *n3, *n4, *n1stop, *n2stop;
	WORD *r1, *r2, *r3, *r4, j, k, kGCD, kGCD2, kLCM, jGCD, kkLCM, jLCM, size, sign;
	WORD *newterm, expr = 0;
	WORD olddeferflag = AR.DeferFlag, oldgetfile = AR.GetFile, oldhold = AR.KeptInHold;
	WORD newgetfile, newhold;
	int i;
	EXPRESSIONS e;
	FILEHANDLE *file = 0;
	POSITION position, oldposition, startposition;
	WORD *oldcpointer = AR.CompressPointer;
	UWORD *GCDbuffer, *GCDbuffer2, *LCMbuffer, *LCMb, *LCMc;
	GCDbuffer = NumberMalloc("FactorInExpr"); GCDbuffer2 = NumberMalloc("FactorInExpr");
	LCMbuffer = NumberMalloc("FactorInExpr"); LCMb = NumberMalloc("FactorInExpr"); LCMc = NumberMalloc("FactorInExpr");
	t = term; GETSTOP(t,tstop); t++;
	while ( t < tstop ) {
		if ( *t == FACTORIN && t[1] == FUNHEAD+2 && t[FUNHEAD] == -EXPRESSION ) {
			expr = t[FUNHEAD+1];
			break;
		}
		t += t[1];
	}
	if ( t >= tstop ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Internal error. Could not find proper factorin_ function.");
		MUNLOCK(ErrorMessageLock);
		NumberFree(GCDbuffer,"FactorInExpr"); NumberFree(GCDbuffer2,"FactorInExpr");
		NumberFree(LCMbuffer,"FactorInExpr"); NumberFree(LCMb,"FactorInExpr"); NumberFree(LCMc,"FactorInExpr");
		return(-1);
	}
	oldwork = AT.WorkPointer;
	if ( AT.previousEfactor && ( expr == AT.previousEfactor[0] ) ) {
/*
		We have a hit in the cache. Construct the new term.
		At the moment AT.previousEfactor[1] is reserved for future flags
*/
		goto PutTheFactor;
	}
/*
	No hit. We have to do the work. We start with constructing the factor
	in the WorkSpace. Later we will move it to the cache.
	Finally we will jump to PutTheFactor.
*/
	e = Expressions + expr;
	switch ( e->status ) {
		case LOCALEXPRESSION:
		case SKIPLEXPRESSION:
		case DROPLEXPRESSION:
		case GLOBALEXPRESSION:
		case SKIPGEXPRESSION:
		case DROPGEXPRESSION:
/*
			Expression is to be found in the input Scratch file.
			Set the file handle and the position.
			The rest is done by GetTerm.
*/
			newhold = 0;
			newgetfile = 1;
			file = AR.infile;
			break;
		case HIDDENLEXPRESSION:
		case HIDDENGEXPRESSION:
		case HIDELEXPRESSION:
		case HIDEGEXPRESSION:
		case DROPHLEXPRESSION:
		case DROPHGEXPRESSION:
		case UNHIDELEXPRESSION:
		case UNHIDEGEXPRESSION:
/*
			Expression is to be found in the hide Scratch file.
			Set the file handle and the position.
			The rest is done by GetTerm.
*/
			newhold = 0;
			newgetfile = 2;
			file = AR.hidefile;
			break;
		case STOREDEXPRESSION:
/*
			This is an 'illegal' case
*/
			MLOCK(ErrorMessageLock);
			MesPrint("Error: factorin_ cannot determine factors in stored expressions.");
			MUNLOCK(ErrorMessageLock);
			NumberFree(GCDbuffer,"FactorInExpr"); NumberFree(GCDbuffer2,"FactorInExpr");
			NumberFree(LCMbuffer,"FactorInExpr"); NumberFree(LCMb,"FactorInExpr"); NumberFree(LCMc,"FactorInExpr");
			return(-1);
		case DROPPEDEXPRESSION:
/*
			We replace the function by 1.
*/
			m = oldwork; n1 = term;
			while ( n1 < t ) *m++ = *n1++;
			n1 = t + t[1]; tstop = term + *term;
			while ( n1 < tstop ) *m++ = *n1++;
			*oldwork = m - oldwork;
			AT.WorkPointer = m;
			if ( Generator(BHEAD oldwork,level) ) {
				NumberFree(GCDbuffer,"FactorInExpr"); NumberFree(GCDbuffer2,"FactorInExpr");
				NumberFree(LCMbuffer,"FactorInExpr"); NumberFree(LCMb,"FactorInExpr"); NumberFree(LCMc,"FactorInExpr");
				return(-1);
			}
			AT.WorkPointer = oldwork;
			NumberFree(GCDbuffer,"FactorInExpr"); NumberFree(GCDbuffer2,"FactorInExpr");
			NumberFree(LCMbuffer,"FactorInExpr"); NumberFree(LCMb,"FactorInExpr"); NumberFree(LCMc,"FactorInExpr");
			return(0);
		default:
			MLOCK(ErrorMessageLock);
			MesPrint("Error: Illegal expression in factorinexpr.");
			MUNLOCK(ErrorMessageLock);
			NumberFree(GCDbuffer,"FactorInExpr"); NumberFree(GCDbuffer2,"FactorInExpr");
			NumberFree(LCMbuffer,"FactorInExpr"); NumberFree(LCMb,"FactorInExpr"); NumberFree(LCMc,"FactorInExpr");
			return(-1);
	}
/*
	Before we start with the file we set the buffers for the coefficient
	For the coefficient we have to determine the LCM of the denominators
	and the GCD of the numerators.
*/
	position = AS.OldOnFile[expr];
	AR.DeferFlag = 0; AR.KeptInHold = newhold; AR.GetFile = newgetfile;
	SeekScratch(file,&oldposition);
	SetScratch(file,&position);
	if ( GetTerm(BHEAD oldwork) <= 0 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("(5) Expression %d has problems in scratchfile",expr);
		MUNLOCK(ErrorMessageLock);
		NumberFree(GCDbuffer,"FactorInExpr"); NumberFree(GCDbuffer2,"FactorInExpr");
		NumberFree(LCMbuffer,"FactorInExpr"); NumberFree(LCMb,"FactorInExpr"); NumberFree(LCMc,"FactorInExpr");
		return(-1);
	}
	SeekScratch(file,&startposition);
	SeekScratch(file,&position);
/*
	Load the first term in the workspace
*/
	if ( GetTerm(BHEAD oldwork) == 0 ) {
		SetScratch(file,&oldposition); /* We still need this untill Processor is clean */
		AR.DeferFlag = olddeferflag;
		oldwork[0] = 4; oldwork[1] = 1; oldwork[2] = 1; oldwork[3] = 3; 
		goto Complete;
	}
	SeekScratch(file,&position);
	AR.DeferFlag = olddeferflag; AR.KeptInHold = oldhold; AR.GetFile = oldgetfile;

	r2 = m = oldwork + *oldwork;
	j = m[-1];
	m -= ABS(j);
	*oldwork = (WORD)(m-oldwork);
	AT.WorkPointer = newterm = mstop = m;
/*
	Now take the coefficient of the first term to load up the LCM and the GCD
*/
	r3 = m;
	k = REDLENG(j);
	if ( k < 0 ) { k = -k; sign = -1; }
	else { sign = 1; }
	while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
	for ( kGCD = 0; kGCD < k; kGCD++ ) GCDbuffer[kGCD] = r3[kGCD];
	k = REDLENG(j);
	if ( k < 0 ) k = -k;
	r3 += k;
	while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
	for ( kLCM = 0; kLCM < k; kLCM++ ) LCMbuffer[kLCM] = r3[kLCM];
/*
	The copy and the coefficient are in place. Now search through the terms.
*/
	for (;;) {
		AR.DeferFlag = 0; AR.KeptInHold = newhold; AR.GetFile = newgetfile;
		SetScratch(file,&position);
		size = GetTerm(BHEAD newterm);
		SeekScratch(file,&position);
		AR.DeferFlag = olddeferflag; AR.KeptInHold = oldhold; AR.GetFile = oldgetfile;
		if ( size == 0 ) break;
		m = oldwork+1;
		r2 = newterm + *newterm;
		r2 -= ABS(r2[-1]);
		r1 = newterm+1;
		while ( m < mstop ) {
			while ( r1 < r2 ) {
				if ( *r1 != *m ) {
					r1 += r1[1]; continue;
				}
/*
				Now the various cases
			#[ SYMBOL :
*/
				if ( *m == SYMBOL ) {
					n1 = m+2; n1stop = m+m[1];
					n2stop = r1+r1[1];
					while ( n1 < n1stop ) {
						n2 = r1+2;
						while ( n2 < n2stop ) {
							if ( *n1 != *n2 ) { n2 += 2; continue; }
							if ( n1[1] > 0 ) {
								if ( n2[1] < 0 ) { n2 += 2; continue; }
								if ( n2[1] < n1[1] ) n1[1] = n2[1];
							}
							else {
								if ( n2[1] > 0 ) { n2 += 2; continue; }
								if ( n2[1] > n1[1] ) n1[1] = n2[1];
							}
							break;
						}
						if ( n2 >= n2stop ) {	/* scratch symbol */
							if ( m[1] == 4 ) goto scratch;
							m[1] -= 2;
							n3 = n1; n4 = n1+2;
							while ( n4 < mstop ) *n3++ = *n4++;
							*oldwork = n3 - oldwork;
							mstop -= 2; n1stop -= 2;
							continue;
						}
						n1 += 2;
					}
					break;
				}
/*
			#] SYMBOL : 
			#[ DOTPRODUCT :
*/
				else if ( *m == DOTPRODUCT ) {
					n1 = m+2; n1stop = m+m[1];
					n2stop = r1+r1[1];
					while ( n1 < n1stop ) {
						n2 = r1+2;
						while ( n2 < n2stop ) {
							if ( *n1 != *n2 || n1[1] != n2[1] ) { n2 += 3; continue; }
							if ( n1[2] > 0 ) {
								if ( n2[2] < 0 ) { n2 += 3; continue; }
								if ( n2[2] < n1[2] ) n1[2] = n2[2];
							}
							else {
								if ( n2[2] > 0 ) { n2 += 3; continue; }
								if ( n2[2] > n1[2] ) n1[2] = n2[2];
							}
							break;
						}
						if ( n2 >= n2stop ) {	/* scratch dotproduct */
							if ( m[1] == 5 ) goto scratch;
							m[1] -= 3;
							n3 = n1; n4 = n1+3;
							while ( n4 < mstop ) *n3++ = *n4++;
							*oldwork = n3 - oldwork;
							mstop -= 3; n1stop -= 3;
							continue;
						}
						n1 += 3;
					}
					break;
				}
/*
			#] DOTPRODUCT : 
			#[ VECTOR :
*/
				else if ( *m == VECTOR ) {
/*
					Here we have to be careful if there is more than
					one of the same
*/
					n1 = m+2; n1stop = m+m[1];
					n2 = r1+2;n2stop = r1+r1[1];
					while ( n1 < n1stop ) {
						while ( n2 < n2stop ) {
							if ( *n1 == *n2 && n1[1] == n2[1] ) {
								n2 += 2; goto nextn1;
							}
							n2 += 2;
						}
						if ( n2 >= n2stop ) {	/* scratch vector */
							if ( m[1] == 4 ) goto scratch;
							m[1] -= 2;
							n3 = n1; n4 = n1+2;
							while ( n4 < mstop ) *n3++ = *n4++;
							*oldwork = n3 - oldwork;
							mstop -= 2; n1stop -= 2;
							continue;
						}
						n2 = r1+2;
nextn1:					n1 += 2;
					}
					break;
				}
/*
			#] VECTOR : 
			#[ REMAINDER :
*/
				else {
/*
					Again: watch for multiple occurrences of the same object
*/
					if ( m[1] != r1[1] ) { r1 += r1[1]; continue; }
					for ( j = 2; j < m[1]; j++ ) {
						if ( m[j] != r1[j] ) break;
					}
					if ( j < m[1] ) { r1 += r1[1]; continue; }
					r1 += r1[1]; /* to restart at the next potential match */
					goto nextm;  /* match */
				}
/*
			#] REMAINDER : 
*/
			}
            if ( r1 >= r2 ) { /* no factor! */
scratch:;
				r3 = m + m[1]; r4 = m;
				while ( r3 < mstop ) *r4++ = *r3++;
				*oldwork = r4 - oldwork;
				if ( *oldwork == 1 ) goto nofactor;
				mstop = r4;
				r1 = newterm + 1;
				continue;
			}
			r1 = newterm + 1;
nextm:		m += m[1];
		}
nofactor:;
/*
		Now the coefficient
*/
		r2 = newterm + *newterm;
		j = r2[-1];
		r3 = r2 - ABS(j);
		k = REDLENG(j);
		if ( k < 0 ) k = -k;
		while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
		if ( ( ( GCDbuffer[0] == 1 ) && ( kGCD == 1 ) ) ) {
/*
			GCD is already 1
*/
		}
		else if ( ( ( k != 1 ) || ( r3[0] != 1 ) ) ) {
			if ( GcdLong(BHEAD GCDbuffer,kGCD,(UWORD *)r3,k,GCDbuffer2,&kGCD2) ) {
				goto onerror;
			}
			kGCD = kGCD2;
			for ( i = 0; i < kGCD; i++ ) GCDbuffer[i] = GCDbuffer2[i];
		}
		else {
			kGCD = 1; GCDbuffer[0] = 1;
		}
		k = REDLENG(j);
		if ( k < 0 ) k = -k;
		r3 += k;
		while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
		if ( ( ( LCMbuffer[0] == 1 ) && ( kLCM == 1 ) ) ) {
			for ( kLCM = 0; kLCM < k; kLCM++ )
				LCMbuffer[kLCM] = r3[kLCM];
		}
		else if ( ( k != 1 ) || ( r3[0] != 1 ) ) {
			if ( GcdLong(BHEAD LCMbuffer,kLCM,(UWORD *)r3,k,LCMb,&kkLCM) ) {
				goto onerror;
			}
			DivLong((UWORD *)r3,k,LCMb,kkLCM,LCMb,&kkLCM,LCMc,&jLCM);
			MulLong(LCMbuffer,kLCM,LCMb,kkLCM,LCMc,&jLCM);
			for ( kLCM = 0; kLCM < jLCM; kLCM++ )
				LCMbuffer[kLCM] = LCMc[kLCM];
		}
		else {} /* LCM doesn't change */
	}
	SetScratch(file,&oldposition); /* Needed until Processor is thread safe */
	AR.DeferFlag = olddeferflag;
/*
	Now put the term together in oldwork: GCD/LCM
	We have already the algebraic contents there.
*/
	r3 = (WORD *)(GCDbuffer);
	r4 = (WORD *)(LCMbuffer);
	r1 = oldwork + *oldwork;
	if ( kGCD == kLCM ) {
		for ( jGCD = 0; jGCD < kGCD; jGCD++ ) *r1++ = *r3++;
		for ( jGCD = 0; jGCD < kGCD; jGCD++ ) *r1++ = *r4++;
		k = 2*kGCD+1;
	}
	else if ( kGCD > kLCM ) {
		for ( jGCD = 0; jGCD < kGCD; jGCD++ ) *r1++ = *r3++;
		for ( jGCD = 0; jGCD < kLCM; jGCD++ ) *r1++ = *r4++;
		for ( jGCD = kLCM; jGCD < kGCD; jGCD++ ) *r1++ = 0;
		k = 2*kGCD+1;
	}
	else {
		for ( jGCD = 0; jGCD < kGCD; jGCD++ ) *r1++ = *r3++;
		for ( jGCD = kGCD; jGCD < kLCM; jGCD++ ) *r1++ = 0;
		for ( jGCD = 0; jGCD < kLCM; jGCD++ ) *r1++ = *r4++;
		k = 2*kLCM+1;
	}
	if ( sign < 0 ) *r1++ = -k;
	else *r1++ = k;
	*oldwork = (WORD)(r1-oldwork);
/*
	Now put the new term in the cache
*/
Complete:;
	if ( AT.previousEfactor ) M_free(AT.previousEfactor,"Efactor cache");
	AT.previousEfactor = (WORD *)Malloc1((*oldwork+2)*sizeof(WORD),"Efactor cache");
	AT.previousEfactor[0] = expr;
	r1 = oldwork; r2 = AT.previousEfactor + 2; k = *oldwork;
	NCOPY(r2,r1,k)
	AT.previousEfactor[1] = 0;
/*
	Now we construct the new term in the workspace.
*/
PutTheFactor:;
	if ( AT.WorkPointer + AT.previousEfactor[2] >= AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MesPrint("Called from factorin_");
		MUNLOCK(ErrorMessageLock);
		NumberFree(GCDbuffer,"FactorInExpr"); NumberFree(GCDbuffer2,"FactorInExpr");
		NumberFree(LCMbuffer,"FactorInExpr"); NumberFree(LCMb,"FactorInExpr"); NumberFree(LCMc,"FactorInExpr");
		return(-1);
	}
	n1 = oldwork; n2 = term; while ( n2 < t ) *n1++ = *n2++;
	n2 = AT.previousEfactor+2; GETSTOP(n2,n2stop); n3 = n2 + *n2;
	n2++; while ( n2 < n2stop ) *n1++ = *n2++;
	n2 = t + t[1]; while ( n2 < tstop ) *n1++ = *n2++;
	size = term[*term-1];
	size = REDLENG(size);
	k = n3[-1]; k = REDLENG(k);
	if ( MulRat(BHEAD (UWORD *)tstop,size,(UWORD *)n2stop,k,
							(UWORD *)n1,&size) ) goto onerror;
	size = INCLENG(size);
	k = size < 0 ? -size: size;
       n1 += k; n1[-1] = size;
	*oldwork = n1 - oldwork;
	AT.WorkPointer = n1;
	if ( Generator(BHEAD oldwork,level) ) {
		NumberFree(GCDbuffer,"FactorInExpr"); NumberFree(GCDbuffer2,"FactorInExpr");
		NumberFree(LCMbuffer,"FactorInExpr"); NumberFree(LCMb,"FactorInExpr"); NumberFree(LCMc,"FactorInExpr");
		return(-1);
	}
	AT.WorkPointer = oldwork;
	AR.CompressPointer = oldcpointer;
	NumberFree(GCDbuffer,"FactorInExpr"); NumberFree(GCDbuffer2,"FactorInExpr");
	NumberFree(LCMbuffer,"FactorInExpr"); NumberFree(LCMb,"FactorInExpr"); NumberFree(LCMc,"FactorInExpr");
	return(0);
onerror:
	AT.WorkPointer = oldwork;
	AR.CompressPointer = oldcpointer;
	MLOCK(ErrorMessageLock);
	MesCall("FactorInExpr");
	MUNLOCK(ErrorMessageLock);
	NumberFree(GCDbuffer,"FactorInExpr"); NumberFree(GCDbuffer2,"FactorInExpr");
	NumberFree(LCMbuffer,"FactorInExpr"); NumberFree(LCMb,"FactorInExpr"); NumberFree(LCMc,"FactorInExpr");
	return(-1);
}

/*
  	#] FactorInExpr : 
*/
