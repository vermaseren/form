/** @file argument.c
 * 
 *  Contains the routines that deal with the execution phase of the argument
 *	and related statements (like term)
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
  	#[ include : argument.c
*/

#include "form3.h"

/*
  	#] include : 
  	#[ execarg :

	Executes the subset of statements in an argument environment.
	The calling routine should be of the type
	if ( C->lhs[level][0] == TYPEARG ) {
		if ( execarg(term,level) ) goto GenCall;
		level = C->lhs[level][2];
		goto SkipCount;
	}
	Note that there will be cases in which extra space is needed.
	In addition the compare with C->numlhs isn't very fine, because we
	need to insert a different value (C->lhs[level][2]).
*/

WORD execarg(PHEAD WORD *term, WORD level)
{
	GETBIDENTITY
	WORD *t, *r, *m, *v;
	WORD *start, *stop, *rstop, *r1, *r2 = 0, *r3 = 0, *r4, *r5, *r6, *r7, *r8, *r9;
	WORD *mm, *mstop, *rnext, *rr, *factor, type, ngcd, nq;
	CBUF *C = cbuf+AM.rbufnum, *CC = cbuf+AT.ebufnum;
	WORD i, j, k, oldnumlhs = AR.Cnumlhs, count, action = 0, olddefer = AR.DeferFlag;
	WORD oldnumrhs = CC->numrhs, size, pow, jj;
	LONG oldcpointer = CC->Pointer - CC->Buffer, oldppointer = AT.pWorkPointer, lp;
	WORD *oldwork = AT.WorkPointer, *oldwork2, scale, renorm;
	WORD kLCM = 0, kGCD = 0, kGCD2, kkLCM = 0, jLCM = 0, jGCD, sign = 1;
	int ii;
	UWORD *EAscrat, *GCDbuffer = 0, *GCDbuffer2, *LCMbuffer, *LCMb, *LCMc;
	AT.WorkPointer += *term;
	start = C->lhs[level];
	AR.Cnumlhs = start[2];
	stop = start + start[1];
	type = *start;
	scale = start[4];
	renorm = start[5];
	start += TYPEARGHEADSIZE;
/*
  	#[ Dollars :
*/
	if ( renorm && start[1] != 0 ) {/* We have to evaluate $ symbols inside () */
		t = start+1; factor = oldwork2 = v = AT.WorkPointer;
		i = *t; t++;
		*v++ = i+3; i--; NCOPY(v,t,i);
		*v++ = 1; *v++ = 1; *v++ = 3;
		AT.WorkPointer = v;
		start = t; AR.Eside = LHSIDEX;
		NewSort(BHEAD0);
		if ( Generator(BHEAD factor,AR.Cnumlhs) ) {
			LowerSortLevel();
			AT.WorkPointer = oldwork;
			return(-1);
		}
		AT.WorkPointer = v;
		if ( EndSort(BHEAD factor,0) < 0 ) {}
		if ( *factor && *(factor+*factor) != 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("&$ in () does not evaluate into a single term");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		AR.Eside = RHSIDE;
		if ( *factor > 0 ) {
			v = factor+*factor;
			v -= ABS(v[-1]);
			*factor = v-factor;
		}
		AT.WorkPointer = v;
	}
	else {
		if ( *start < 0 ) {
			factor = start + 1;
			start += -*start;
		}
		else factor = 0;
	}
/*
  	#] Dollars : 
*/
	t = term;
	r = t + *t;
	rstop = r - ABS(r[-1]);
	t++;
/*
  	#[ Argument detection : + argument statement
*/
	while ( t < rstop ) {
		if ( *t >= FUNCTION && functions[*t-FUNCTION].spec == 0 ) {
/*
			We have a function. First count the number of arguments.
			Tensors are excluded.
*/
			count = 0;
			v = t;
			m = t + FUNHEAD;
			r = t + t[1];
			while ( m < r ) {
				count++;
				NEXTARG(m)
			}
			if ( count <= 0 ) { t += t[1]; continue; }
/*
			Now we take the arguments one by one and test for a match
*/
			for ( i = 1; i <= count; i++ ) {
				m = start;
				while ( m < stop ) {
					r = m + m[1];
					j = *r++;
					if ( j > 1 ) {
						while ( --j > 0 ) {
							if ( *r == i ) goto RightNum;
							r++;
						}
						m = r;
						continue;
					}
RightNum:
					if ( m[1] == 2 ) {
						m += 2;
						m += *m;
						goto HaveTodo;
					}
					else {
						r = m + m[1];
						m += 2;
						while ( m < r ) {
							if ( *m == CSET ) {
								r1 = SetElements + Sets[m[1]].first;
								r2 = SetElements + Sets[m[1]].last;
								while ( r1 < r2 ) {
									if ( *r1++ == *t ) goto HaveTodo;
								}
							}
							else if ( m[1] == *t ) goto HaveTodo;
							m += 2;
						}
					}
					m += *m;
				}
				continue;
HaveTodo:
/*
				If we come here we have to do the argument i (first is 1).
*/
				sign = 1;
				action = 1;
				v[2] |= DIRTYFLAG;
				r = t + FUNHEAD;
				j = i;
				while ( --j > 0 ) { NEXTARG(r) }
				if ( ( type == TYPESPLITARG ) || ( type == TYPESPLITFIRSTARG )
				 || ( type == TYPESPLITLASTARG ) ) {
					if ( *t > FUNCTION && *r > 0 ) {
						WantAddPointers(2);
						AT.pWorkSpace[AT.pWorkPointer++] = t;
						AT.pWorkSpace[AT.pWorkPointer++] = r;
					}
					continue;
				}
				else if ( type == TYPESPLITARG2 ) {
					if ( *t > FUNCTION && *r > 0 ) {
						WantAddPointers(2);
						AT.pWorkSpace[AT.pWorkPointer++] = t;
						AT.pWorkSpace[AT.pWorkPointer++] = r;
					}
					continue;
				}
				else if ( type == TYPEFACTARG || type == TYPEFACTARG2 ) {
					if ( *t > FUNCTION || *t == DENOMINATOR ) {
						if ( *r > 0 ) {
						mm = r + ARGHEAD; mstop = r + *r;
						if ( mm + *mm < mstop ) {
							WantAddPointers(2);
							AT.pWorkSpace[AT.pWorkPointer++] = t;
							AT.pWorkSpace[AT.pWorkPointer++] = r;
							continue;
						}
						if ( *mm == 1+ABS(mstop[-1]) ) continue;
						if ( mstop[-3] != 1 || mstop[-2] != 1
							|| mstop[-1] != 3 ) {
							WantAddPointers(2);
							AT.pWorkSpace[AT.pWorkPointer++] = t;
							AT.pWorkSpace[AT.pWorkPointer++] = r;
							continue;
						}
						GETSTOP(mm,mstop); mm++;
						if ( mm + mm[1] < mstop ) {
							WantAddPointers(2);
							AT.pWorkSpace[AT.pWorkPointer++] = t;
							AT.pWorkSpace[AT.pWorkPointer++] = r;
							continue;
						}
						if ( *mm == SYMBOL && ( mm[1] > 4 ||
							( mm[3] != 1 && mm[3] != -1 ) ) ) {
							WantAddPointers(2);
							AT.pWorkSpace[AT.pWorkPointer++] = t;
							AT.pWorkSpace[AT.pWorkPointer++] = r;
							continue;
						}
						else if ( *mm == DOTPRODUCT && ( mm[1] > 5 ||
							( mm[4] != 1 && mm[4] != -1 ) ) ) {
							WantAddPointers(2);
							AT.pWorkSpace[AT.pWorkPointer++] = t;
							AT.pWorkSpace[AT.pWorkPointer++] = r;
							continue;
						}
						else if ( ( *mm == DELTA || *mm == VECTOR )
							 && mm[1] > 4 ) {
							WantAddPointers(2);
							AT.pWorkSpace[AT.pWorkPointer++] = t;
							AT.pWorkSpace[AT.pWorkPointer++] = r;
							continue;
						}
						}
						else if ( factor && *factor == 4 && factor[2] == 1 ) {
							WantAddPointers(2);
							AT.pWorkSpace[AT.pWorkPointer++] = t;
							AT.pWorkSpace[AT.pWorkPointer++] = r;
							continue;
						}
						else if ( factor && *factor == 0
						&& ( *r == -SNUMBER && r[1] != 1 ) ) {
							WantAddPointers(2);
							AT.pWorkSpace[AT.pWorkPointer++] = t;
							AT.pWorkSpace[AT.pWorkPointer++] = r;
							continue;
						}
						else if ( *r == -MINVECTOR ) {
							WantAddPointers(2);
							AT.pWorkSpace[AT.pWorkPointer++] = t;
							AT.pWorkSpace[AT.pWorkPointer++] = r;
							continue;
						}
					}
					continue;
				}
				else if ( type == TYPENORM || type == TYPENORM2 || type == TYPENORM3 || type == TYPENORM4 ) {
					if ( *r < 0 ) {
						WORD rone;
						if ( *r == -MINVECTOR ) { rone = -1; *r = -INDEX; }
						else if ( *r != -SNUMBER || r[1] == 1 || r[1] == 0 ) continue;
						else { rone = r[1]; r[1] = 1; }
/*
						Now we must multiply the general coefficient by r[1]
*/
						if ( scale && ( factor == 0 || *factor ) ) {
							action = 1;
							v[2] |= DIRTYFLAG;
							if ( rone < 0 ) {
								if ( type == TYPENORM3 ) k = 1;
								else k = -1;
								rone = -rone;
							}
							else k = 1;
							r1 = term + *term;
							size = r1[-1];
							size = REDLENG(size);
							if ( scale > 0 ) {
								for ( jj = 0; jj < scale; jj++ ) {
									if ( Mully(BHEAD (UWORD *)rstop,&size,(UWORD *)(&rone),k) )
										goto execargerr;
								}
							}
							else {
								for ( jj = 0; jj > scale; jj-- ) {
									if ( Divvy(BHEAD (UWORD *)rstop,&size,(UWORD *)(&rone),k) )
										goto execargerr;
								}
							}
							size = INCLENG(size);
							k = size < 0 ? -size: size;
							rstop[k-1] = size;
							*term = (WORD)(rstop - term) + k;
						}
						continue;
					}
/*
					Now we have to find a reference term.
					If factor is defined and *factor != 0 we have to
					look for the first term that matches the pattern exactly
					Otherwise the first term plays this role
					If its coefficient is not one,
					we must set up a division of the whole argument by
					this coefficient, and a multiplication of the term
					when the type is not equal to TYPENORM2.
					We first multiply the coefficient of the term.
					Then we set up the division.

					First find the magic term
*/
					if ( type == TYPENORM4 ) {
/*
						For normalizing everything to integers we have to
						determine for all elements of this argument the LCM of
						the denominators and the GCD of the numerators.
*/
						GCDbuffer = NumberMalloc("execarg");
						GCDbuffer2 = NumberMalloc("execarg");
						LCMbuffer = NumberMalloc("execarg");
						LCMb = NumberMalloc("execarg"); LCMc = NumberMalloc("execarg");
						r4 = r + *r;
						r1 = r + ARGHEAD;
/*
						First take the first term to load up the LCM and the GCD
*/
						r2 = r1 + *r1;
						j = r2[-1];
						if ( j < 0 ) sign = -1;
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
						Now go through the rest of the terms in this argument.
*/
						while ( r1 < r4 ) {
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
									NumberFree(GCDbuffer,"execarg");
									NumberFree(GCDbuffer2,"execarg");
									NumberFree(LCMbuffer,"execarg");
									NumberFree(LCMb,"execarg"); NumberFree(LCMc,"execarg");
									goto execargerr;
								}
								kGCD = kGCD2;
								for ( ii = 0; ii < kGCD; ii++ ) GCDbuffer[ii] = GCDbuffer2[ii];
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
									NumberFree(GCDbuffer,"execarg"); NumberFree(GCDbuffer2,"execarg");
									NumberFree(LCMbuffer,"execarg"); NumberFree(LCMb,"execarg"); NumberFree(LCMc,"execarg");
									goto execargerr;
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
/*						NumberFree(GCDbuffer,"execarg"); GCDbuffer = 0; */
						NumberFree(GCDbuffer2,"execarg");
						NumberFree(LCMbuffer,"execarg");
						NumberFree(LCMb,"execarg"); NumberFree(LCMc,"execarg");
						j = 2*k+1;
/*
						Now we have to correct the overal factor

						We have a little problem here.
						r3 is in GCDbuffer and we returned that.
						At the same time we still use it.
						This works as long as each worker has its own TermMalloc
*/
						if ( scale && ( factor == 0 || *factor > 0 ) )
							goto ScaledVariety;
/*
						The if was added 28-nov-2012 to give MakeInteger also
						the (0) option.
*/
						if ( scale && ( factor == 0 || *factor ) ) {
							size = term[*term-1];
							size = REDLENG(size);
							if ( MulRat(BHEAD (UWORD *)rstop,size,(UWORD *)r3,k,
									(UWORD *)rstop,&size) ) goto execargerr;
							size = INCLENG(size);
							k = size < 0 ? -size: size;
							rstop[k-1] = size*sign;
							*term = (WORD)(rstop - term) + k;
						}
					}
					else {
						if ( factor && *factor >= 1 ) {
							r4 = r + *r;
							r1 = r + ARGHEAD;
							while ( r1 < r4 ) {
								r2 = r1 + *r1;
								r3 = r2 - ABS(r2[-1]);
								j = r3 - r1;
								r5 = factor;
								if ( j != *r5 ) { r1 = r2; continue; }
								r5++; r6 = r1+1;
								while ( --j > 0 ) {
									if ( *r5 != *r6 ) break;
									r5++; r6++;
								}
								if ( j > 0 ) { r1 = r2; continue; }
								break;
							}
							if ( r1 >= r4 ) continue;
						}
						else {
							r1 = r + ARGHEAD;
							r2 = r1 + *r1;
							r3 = r2 - ABS(r2[-1]);
						}
						if ( *r3 == 1 && r3[1] == 1 ) {
							if ( r2[-1] == 3 ) continue;
							if ( r2[-1] == -3 && type == TYPENORM3 ) continue;
						}
						action = 1;
						v[2] |= DIRTYFLAG;
						j = r2[-1];
						k = REDLENG(j);
						if ( j < 0 ) j = -j;
						if ( type == TYPENORM && scale && ( factor == 0 || *factor ) ) {
/*
							Now we correct the overal factor
*/
ScaledVariety:;
							size = term[*term-1];
							size = REDLENG(size);
							if ( scale > 0 ) {
								for ( jj = 0; jj < scale; jj++ ) {
									if ( MulRat(BHEAD (UWORD *)rstop,size,(UWORD *)r3,k,
										(UWORD *)rstop,&size) ) goto execargerr;
								}
							}
							else {
								for ( jj = 0; jj > scale; jj-- ) {
									if ( DivRat(BHEAD (UWORD *)rstop,size,(UWORD *)r3,k,
										(UWORD *)rstop,&size) ) goto execargerr;
								}
							}
							size = INCLENG(size);
							k = size < 0 ? -size: size;
							rstop[k-1] = size*sign;
							*term = (WORD)(rstop - term) + k;
						}
					}
/*
                  	We generate a statement for adapting all terms in the
					argument sucessively
*/
					r4 = AddRHS(AT.ebufnum,1);
					while ( (r4+j+12) > CC->Top ) r4 = DoubleCbuffer(AT.ebufnum,r4,3);
					*r4++ = j+1;
					i = (j-1)/2;  /* was (j-1)*2  ????? 17-oct-2017 */
					for ( k = 0; k < i; k++ ) *r4++ = r3[i+k];
					for ( k = 0; k < i; k++ ) *r4++ = r3[k];
					if ( ( type == TYPENORM3 ) || ( type == TYPENORM4 ) ) *r4++ = j*sign;
					else *r4++ = r3[j-1];
					*r4++ = 0;
					CC->rhs[CC->numrhs+1] = r4;
					CC->Pointer = r4;
					AT.mulpat[5] = CC->numrhs;
					AT.mulpat[7] = AT.ebufnum;
				}
				else if ( type == TYPEARGTOEXTRASYMBOL ) {
					WORD n;
					if ( r[0] < 0 ) {
						/* The argument is in the fast notation. */
						WORD tmp[MaX(9,FUNHEAD+5)];
						switch ( r[0] ) {
							case -SNUMBER:
								if ( r[1] == 0 ) {
									tmp[0] = 0;
								}
								else {
									tmp[0] = 4;
									tmp[1] = ABS(r[1]);
									tmp[2] = 1;
									tmp[3] = r[1] > 0 ? 3 : -3;
									tmp[4] = 0;
								}
								break;
							case -SYMBOL:
								tmp[0] = 8;
								tmp[1] = SYMBOL;
								tmp[2] = 4;
								tmp[3] = r[1];
								tmp[4] = 1;
								tmp[5] = 1;
								tmp[6] = 1;
								tmp[7] = 3;
								tmp[8] = 0;
								break;
							case -INDEX:
							case -VECTOR:
							case -MINVECTOR:
								tmp[0] = 7;
								tmp[1] = INDEX;
								tmp[2] = 3;
								tmp[3] = r[1];
								tmp[4] = 1;
								tmp[5] = 1;
								tmp[6] = r[0] != -MINVECTOR ? 3 : -3;
								tmp[7] = 0;
								break;
							default:
								if ( r[0] <= -FUNCTION ) {
									tmp[0] = FUNHEAD+4;
									tmp[1] = -r[0];
									tmp[2] = FUNHEAD;
									ZeroFillRange(tmp,3,1+FUNHEAD);
									tmp[FUNHEAD+1] = 1;
									tmp[FUNHEAD+2] = 1;
									tmp[FUNHEAD+3] = 3;
									tmp[FUNHEAD+4] = 0;
									break;
								}
								else {
									MLOCK(ErrorMessageLock);
									MesPrint("Unknown fast notation found (TYPEARGTOEXTRASYMBOL)");
									MUNLOCK(ErrorMessageLock);
									return(-1);
								}
						}
						n = FindSubexpression(tmp);
					}
					else {
						/*
						 * NOTE: writing to r[r[0]] is legal. As long as we work
						 * in a part of the term, at least the coefficient of
						 * the term must follow.
						 */
						WORD old_rr0 = r[r[0]];
						r[r[0]] = 0;  /* zero-terminated */
						n = FindSubexpression(r+ARGHEAD);
						r[r[0]] = old_rr0;
					}
					/* Put the new argument in the work space. */
					if ( AT.WorkPointer+2 > AT.WorkTop ) {
						MLOCK(ErrorMessageLock);
						MesWork();
						MUNLOCK(ErrorMessageLock);
						return(-1);
					}
					r1 = AT.WorkPointer;
					if ( scale ) {  /* means "tonumber" */
						r1[0] = -SNUMBER;
						r1[1] = n;
					}
					else {
						r1[0] = -SYMBOL;
						r1[1] = MAXVARIABLES-n;
					}
					/* We need r2, r3, m and k to shift the data. */
					r2 = r + (r[0] > 0 ? r[0] : r[0] <= -FUNCTION ? 1 : 2);
					r3 = r;
					m = r1+ARGHEAD+2;
					k = 2;
					goto do_shift;
				}
				r3 = r;
				AR.DeferFlag = 0;
				if ( *r > 0 ) {
					NewSort(BHEAD0);
					action = 1;
					r2 = r + *r;
					r += ARGHEAD;
					while ( r < r2 ) {	/* Sum over the terms */
						m = AT.WorkPointer;
						j = *r;
						while ( --j >= 0 ) *m++ = *r++;
						r1 = AT.WorkPointer;
						AT.WorkPointer = m;
/*
						What to do with dummy indices?
*/
						if ( type == TYPENORM || type == TYPENORM2 || type == TYPENORM3 || type == TYPENORM4 ) {
							if ( MultDo(BHEAD r1,AT.mulpat) ) goto execargerr;
							AT.WorkPointer = r1 + *r1;
						}
						if ( Generator(BHEAD r1,level) ) goto execargerr;
						AT.WorkPointer = r1;
					}
				}
				else {
					r2 = r + (( *r <= -FUNCTION ) ? 1:2);
					r1 = AT.WorkPointer;
					ToGeneral(r,r1,0);
					m = r1 + ARGHEAD;
					AT.WorkPointer = r1 + *r1;
					NewSort(BHEAD0);
					action = 1;
/*
					What to do with dummy indices?
*/
					if ( type == TYPENORM || type == TYPENORM2 || type == TYPENORM3 || type == TYPENORM4 ) {
						if ( MultDo(BHEAD m,AT.mulpat) ) goto execargerr;
						AT.WorkPointer = m + *m;
					}
					if ( (*m != 0 ) && Generator(BHEAD m,level) ) goto execargerr;
					AT.WorkPointer = r1;
				}
				if ( EndSort(BHEAD AT.WorkPointer+ARGHEAD,1) < 0 ) goto execargerr;
				AR.DeferFlag = olddefer;
/*
				Now shift the sorted entity over the old argument.
*/
				m = AT.WorkPointer+ARGHEAD;
				while ( *m ) m += *m;
				k = WORDDIF(m,AT.WorkPointer);
				*AT.WorkPointer = k;
				AT.WorkPointer[1] = 0;
				if ( ToFast(AT.WorkPointer,AT.WorkPointer) ) {
					if ( *AT.WorkPointer <= -FUNCTION ) k = 1;
					else k = 2;
				}
do_shift:
				if ( *r3 > 0 ) j = k - *r3;
				else if ( *r3 <= -FUNCTION ) j = k - 1;
				else j = k - 2;

				t[1] += j;
				action = 1;
				v[2] |= DIRTYFLAG;
				if ( j > 0 ) {
					r = m + j;
					while ( m > AT.WorkPointer ) *--r = *--m;
					AT.WorkPointer = r;
					m = term + *term;
					r = m + j;
					while ( m > r2 ) *--r = *--m;
				}
				else if ( j < 0 ) {
					r = r2 + j;
					r1 = term + *term;
					while ( r2 < r1 ) *r++ = *r2++;
				}
				r = r3;
				m = AT.WorkPointer;
				NCOPY(r,m,k);
				*term += j;
				rstop += j;
				CC->numrhs = oldnumrhs;
				CC->Pointer = CC->Buffer + oldcpointer;
			}
		}
		t += t[1];
	}
/*
  	#] Argument detection : 
  	#[ SplitArg : + varieties
*/
	if ( ( type == TYPESPLITARG || type == TYPESPLITARG2
	 || type == TYPESPLITFIRSTARG || type == TYPESPLITLASTARG ) && 
	AT.pWorkPointer > oldppointer ) {
		t = term+1;
		r1 = AT.WorkPointer + 1;
		lp = oldppointer;
		while ( t < rstop ) {
			if ( lp < AT.pWorkPointer && t == AT.pWorkSpace[lp] ) {
				v = t;
				m = t + FUNHEAD;
				r = t + t[1];
				r2 = r1; while ( t < m ) *r1++ = *t++;
				while ( m < r ) {
					t = m;
					NEXTARG(m)
					if ( lp >= AT.pWorkPointer || t != AT.pWorkSpace[lp+1] ) {
						if ( *t > 0 ) t[1] = 0;
						while ( t < m ) *r1++ = *t++;
						continue;
					}
/*
					Now we have a nontrivial argument that should be done.
*/
					lp += 2;
					action = 1;
					v[2] |= DIRTYFLAG;
					r3 = t + *t;
					t += ARGHEAD;
					if ( type == TYPESPLITFIRSTARG ) {
						r4 = r1; r5 = t; r7 = oldwork;
						*r1++ = *t + ARGHEAD;
						for ( i = 1; i < ARGHEAD; i++ ) *r1++ = 0;
						j = 0;
						while ( t < r3 ) {
							i = *t;
							if ( j == 0 ) {
								NCOPY(r7,t,i)
								j++;
							}
							else {
								NCOPY(r1,t,i)
							}
						}
						*r4 = r1 - r4;
						if ( j ) {
							if ( ToFast(r4,r4) ) {
								r1 = r4;
								if ( *r1 > -FUNCTION ) r1++;
								r1++;
							}
							r7 = oldwork;
							while ( --j >= 0 ) {
								r4 = r1; i = *r7;
								*r1++ = i+ARGHEAD; *r1++ = 0;
								FILLARG(r1);
								NCOPY(r1,r7,i)
								if ( ToFast(r4,r4) ) {
									r1 = r4;
									if ( *r1 > -FUNCTION ) r1++;
									r1++;
								}
							}
						}
						t = r3;
					}
					else if ( type == TYPESPLITLASTARG ) {
						r4 = r1; r5 = t; r7 = oldwork;
						*r1++ = *t + ARGHEAD;
						for ( i = 1; i < ARGHEAD; i++ ) *r1++ = 0;
						j = 0;
						while ( t < r3 ) {
							i = *t;
							if ( t+i >= r3 ) {
								NCOPY(r7,t,i)
								j++;
							}
							else {
								NCOPY(r1,t,i)
							}
						}
						*r4 = r1 - r4;
						if ( j ) {
							if ( ToFast(r4,r4) ) {
								r1 = r4;
								if ( *r1 > -FUNCTION ) r1++;
								r1++;
							}
							r7 = oldwork;
							while ( --j >= 0 ) {
								r4 = r1; i = *r7;
								*r1++ = i+ARGHEAD; *r1++ = 0;
								FILLARG(r1);
								NCOPY(r1,r7,i)
								if ( ToFast(r4,r4) ) {
									r1 = r4;
									if ( *r1 > -FUNCTION ) r1++;
									r1++;
								}
							}
						}
						t = r3;
					}
					else if ( factor == 0 || ( type == TYPESPLITARG2 && *factor == 0 ) ) {
						while ( t < r3 ) {
							r4 = r1;
							*r1++ = *t + ARGHEAD;					
							for ( i = 1; i < ARGHEAD; i++ ) *r1++ = 0;
							i = *t;
							while ( --i >= 0 ) *r1++ = *t++;
							if ( ToFast(r4,r4) ) {
								r1 = r4;
								if ( *r1 > -FUNCTION ) r1++;
								r1++;
							}
						}
					}
					else if ( type == TYPESPLITARG2 ) {
/*
						Here we better put the pattern matcher at work?
						Remember: there are no wildcards.
*/
						WORD *oRepFunList = AN.RepFunList;
						WORD *oWildMask = AT.WildMask, *oWildValue = AN.WildValue;
						AN.WildValue = AT.locwildvalue; AT.WildMask = AT.locwildvalue+2;
						AN.NumWild = 0;
						r4 = r1; r5 = t; r7 = oldwork;
						*r1++ = *t + ARGHEAD;
						for ( i = 1; i < ARGHEAD; i++ ) *r1++ = 0;
						j = 0;
						while ( t < r3 ) {
							AN.UseFindOnly = 0; oldwork2 = AT.WorkPointer;
							AN.RepFunList = r1;
							AT.WorkPointer = r1+AN.RepFunNum+2;
							i = *t;
							if ( FindRest(BHEAD t,factor) &&
							 ( AN.UsedOtherFind || FindOnce(BHEAD t,factor) ) ) {
								NCOPY(r7,t,i)
								j++;
							}
							else if ( factor[0] == FUNHEAD+1 && factor[1] >= FUNCTION ) {
								WORD *rr1 = t+1, *rr2 = t+i;
								rr2 -= ABS(rr2[-1]);
								while ( rr1 < rr2 ) {
									if ( *rr1 == factor[1] ) break;
									rr1 += rr1[1];
								}
								if ( rr1 < rr2 ) {
									NCOPY(r7,t,i)
									j++;
								}
								else {
									NCOPY(r1,t,i)
								}
							}
							else {
								NCOPY(r1,t,i)
							}
							AT.WorkPointer = oldwork2;
						}
						AN.RepFunList = oRepFunList;
						*r4 = r1 - r4;
						if ( j ) {
							if ( ToFast(r4,r4) ) {
								r1 = r4;
								if ( *r1 > -FUNCTION ) r1++;
								r1++;
							}
							r7 = oldwork;
							while ( --j >= 0 ) {
								r4 = r1; i = *r7;
								*r1++ = i+ARGHEAD; *r1++ = 0;
								FILLARG(r1);
								NCOPY(r1,r7,i)
								if ( ToFast(r4,r4) ) {
									r1 = r4;
									if ( *r1 > -FUNCTION ) r1++;
									r1++;
								}
							}
						}
						t = r3;
						AT.WildMask = oWildMask; AN.WildValue = oWildValue;
					}
					else {
/*
						This code deals with splitting off a single term
*/
						r4 = r1; r5 = t;
						*r1++ = *t + ARGHEAD;
						for ( i = 1; i < ARGHEAD; i++ ) *r1++ = 0;
						j = 0;
						while ( t < r3 ) {
							r6 = t + *t; r6 -= ABS(r6[-1]);
							if ( (r6 - t) == *factor ) {
								k = *factor - 1;
								for ( ; k > 0; k-- ) {
									if ( t[k] != factor[k] ) break;
								}
								if ( k <= 0 ) {
									j = r3 - t; t += *t; continue;
								}
							}
							else if ( (r6 - t) == 1 && *factor == 0 ) {
								j = r3 - t; t += *t; continue;
							}
							i = *t;
							NCOPY(r1,t,i)
						}
						*r4 = r1 - r4;
						if ( j ) {
							if ( ToFast(r4,r4) ) {
								r1 = r4;
								if ( *r1 > -FUNCTION ) r1++;
								r1++;
							}
							t = r3 - j;
							r4 = r1;
							*r1++ = *t + ARGHEAD;
							for ( i = 1; i < ARGHEAD; i++ ) *r1++ = 0;
							i = *t;
							while ( --i >= 0 ) *r1++ = *t++;
							if ( ToFast(r4,r4) ) {
								r1 = r4;
								if ( *r1 > -FUNCTION ) r1++;
								r1++;
							}
						}
						t = r3;
					}
				}
				r2[1] = r1 - r2;
			}
			else {
				r = t + t[1];
				while ( t < r ) *r1++ = *t++;
			}
		}
		r = term + *term;
		while ( t < r ) *r1++ = *t++;
		m = AT.WorkPointer;
		i = m[0] = r1 - m;
		t = term;
		while ( --i >= 0 ) *t++ = *m++;
		if ( AT.WorkPointer < m ) AT.WorkPointer = m;
	}
/*
  	#] SplitArg : 
  	#[ FACTARG :
*/
	if ( ( type == TYPEFACTARG || type == TYPEFACTARG2 ) && 
	AT.pWorkPointer > oldppointer ) {
		t = term+1;
		r1 = AT.WorkPointer + 1;
		lp = oldppointer;
		while ( t < rstop ) {
			if ( lp < AT.pWorkPointer && AT.pWorkSpace[lp] == t ) {
				v = t;
				m = t + FUNHEAD;
				r = t + t[1];
				r2 = r1; while ( t < m ) *r1++ = *t++;
				while ( m < r ) {
					rr = t = m;
					NEXTARG(m)
					if ( lp >= AT.pWorkPointer || AT.pWorkSpace[lp+1] != t ) {
						if ( *t > 0 ) t[1] = 0;
						while ( t < m ) *r1++ = *t++;
						continue;
					}
/*
					Now we have a nontrivial argument that should be studied.
					Try to find common factors.
*/
					lp += 2;
					if ( *t < 0 ) {
						if ( factor && ( *factor == 0 && *t == -SNUMBER ) ) {
							*r1++ = *t++;
							if ( *t == 0 ) *r1++ = *t++;
							else { *r1++ = 1; t++; }
							continue;
						}
						else if ( factor && *factor == 4 && factor[2] == 1 ) {
							if ( *t == -SNUMBER ) {
								if ( factor[3] < 0 || t[1] >= 0 ) {
									while ( t < m ) *r1++ = *t++;
								}
								else {
									*r1++ = -SNUMBER; *r1++ = -1;
									*r1++ = *t++; *r1++ = -*t++;
								}
							}
							else {
								while ( t < m ) *r1++ = *t++;
								*r1++ = -SNUMBER; *r1++ = 1;
							}
							continue;
						}
						else if ( *t == -MINVECTOR ) {
							*r1++ = -VECTOR; t++; *r1++ = *t++;
							*r1++ = -SNUMBER; *r1++ = -1;
							*r1++ = -SNUMBER; *r1++ = 1;
							continue;
						}
					}
/*
					Now we have a nontrivial argument
*/
					r3 = t + *t;
					t += ARGHEAD;  r5 = t; /* Store starting point */
					/* We have terms from r5 to r3 */
					if ( r5+*r5 == r3 && factor ) { /* One term only */
						if ( *factor == 0 ) {
							GETSTOP(t,r6);
							r9 = r1; *r1++ = 0; *r1++ = 1;
							FILLARG(r1);
							*r1++ = (r6-t)+3; t++;
							while ( t < r6 ) *r1++ = *t++;
							*r1++ = 1; *r1++ = 1; *r1++ = 3;
							*r9 = r1-r9;
							if ( ToFast(r9,r9) ) {
								if ( *r9 <= -FUNCTION ) r1 = r9+1;
								else r1 = r9+2;
							}
							t = r3; continue;
						}
						if ( factor[0] == 4 && factor[2] == 1 ) {
							GETSTOP(t,r6);
							r7 = r1; *r1++ = (r6-t)+3+ARGHEAD; *r1++ = 0;
							FILLARG(r1);
							*r1++ = (r6-t)+3; t++;
							while ( t < r6 ) *r1++ = *t++;
							*r1++ = 1; *r1++ = 1; *r1++ = 3;
							if ( ToFast(r7,r7) ) {
								if ( *r7 <= -FUNCTION ) r1 = r7+1;
								else r1 = r7+2;
							}
							if ( r3[-1] < 0 && factor[3] > 0 ) {
								*r1++ = -SNUMBER; *r1++ = -1;
								if ( r3[-1] == -3 && r3[-2] == 1
								&& ( r3[-3] & MAXPOSITIVE ) == r3[-3] ) {
									*r1++ = -SNUMBER; *r1++ = r3[-3];
								}
								else {
									*r1++ = (r3-r6)+1+ARGHEAD;
									*r1++ = 0;
									FILLARG(r1);
									*r1++ = (r3-r6+1);
									while ( t < r3 ) *r1++ = *t++;
									r1[-1] = -r1[-1];
								}
							}
							else {
								if ( ( r3[-1] == -3 || r3[-1] == 3 )
								&& r3[-2] == 1
								&& ( r3[-3] & MAXPOSITIVE ) == r3[-3] ) {
									*r1++ = -SNUMBER; *r1++ = r3[-3];
									if ( r3[-1] < 0 ) r1[-1] = - r1[-1];
								}
								else {
									*r1++ = (r3-r6)+1+ARGHEAD;
									*r1++ = 0;
									FILLARG(r1);
									*r1++ = (r3-r6+1);
									while ( t < r3 ) *r1++ = *t++;
								}
							}
							t = r3; continue;
						}
					}
/*
					Now we take the first term and look for its pieces
					inside the other terms.

					It is at this point that a more general factorization
					routine could take over (allowing for writing the output
					properly of course).
*/
					if ( AC.OldFactArgFlag == NEWFACTARG ) {
					if ( factor == 0 ) {
						WORD *oldworkpointer2 = AT.WorkPointer;
						AT.WorkPointer = r1 + AM.MaxTer+FUNHEAD;
						if ( ArgFactorize(BHEAD t-ARGHEAD,r1) < 0 ) {
							MesCall("ExecArg");
							return(-1);
						}
						AT.WorkPointer = oldworkpointer2;
						t = r3;
						while ( *r1 ) { NEXTARG(r1) }
					}
					else {
						rnext = t + *t;
						GETSTOP(t,r6);
						t++;
						t = r5; pow = 1;
						while ( t < r3 ) {
							t += *t; if ( t[-1] > 0 ) { pow = 0; break; }
						}
/*
						We have to add here the code for computing the GCD
						and to divide it out.

			#[ Numerical factor :
*/
						t = r5;
						EAscrat = (UWORD *)(TermMalloc("execarg"));
						if ( t + *t == r3 ) goto onetermnew;
						GETSTOP(t,r6);
						ngcd = t[t[0]-1];
						i = abs(ngcd)-1;
						while ( --i >= 0 ) EAscrat[i] = r6[i];
						t += *t;
						while ( t < r3 ) {
							GETSTOP(t,r6);
							i = t[t[0]-1];
							if ( AccumGCD(BHEAD EAscrat,&ngcd,(UWORD *)r6,i) ) goto execargerr;
							if ( ngcd == 3 && EAscrat[0] == 1 && EAscrat[1] == 1 ) break;
							t += *t;
						}
	 					if ( ngcd != 3 || EAscrat[0] != 1 || EAscrat[1] != 1 ) {
							if ( pow ) ngcd = -ngcd;
							t = r5; r9 = r1; *r1++ = t[-ARGHEAD]; *r1++ = 1;
							FILLARG(r1); ngcd = REDLENG(ngcd);
							while ( t < r3 ) {
								GETSTOP(t,r6);
								r7 = t; r8 = r1;
								while ( r7 < r6) *r1++ = *r7++;
								t += *t;
								i = REDLENG(t[-1]);
								if ( DivRat(BHEAD (UWORD *)r6,i,EAscrat,ngcd,(UWORD *)r1,&nq) ) goto execargerr;
								nq = INCLENG(nq);
								i = ABS(nq)-1;
								r1 += i; *r1++ = nq; *r8 = r1-r8;
							}
							*r9 = r1-r9;
							ngcd = INCLENG(ngcd);
							i = ABS(ngcd)-1;
							if ( factor && *factor == 0 ) {}
							else if ( ( factor && factor[0] == 4 && factor[2] == 1
							&& factor[3] == -3 ) || pow == 0 ) {
								r9 = r1; *r1++ = ARGHEAD+2+i; *r1++ = 0;
								FILLARG(r1); *r1++ = i+2;
								for ( j = 0; j < i; j++ ) *r1++ = EAscrat[j];
								*r1++ = ngcd;
								if ( ToFast(r9,r9) ) r1 = r9+2;
							}
							else if ( factor && factor[0] == 4 && factor[2] == 1
							&& factor[3] > 0 && pow ) {
								if ( ngcd < 0 ) ngcd = -ngcd;
								*r1++ = -SNUMBER; *r1++ = -1;
								r9 = r1; *r1++ = ARGHEAD+2+i; *r1++ = 0;
								FILLARG(r1); *r1++ = i+2;
								for ( j = 0; j < i; j++ ) *r1++ = EAscrat[j];
								*r1++ = ngcd;
								if ( ToFast(r9,r9) ) r1 = r9+2;
							}
							else {
								if ( ngcd < 0 ) ngcd = -ngcd;
								if ( pow ) { *r1++ = -SNUMBER; *r1++ = -1; }
								if ( ngcd != 3 || EAscrat[0] != 1 || EAscrat[1] != 1 ) {
									r9 = r1; *r1++ = ARGHEAD+2+i; *r1++ = 0;
									FILLARG(r1); *r1++ = i+2;
									for ( j = 0; j < i; j++ ) *r1++ = EAscrat[j];
									*r1++ = ngcd;
									if ( ToFast(r9,r9) ) r1 = r9+2;
								}
							}
	 					}
/*
			#] Numerical factor : 
*/
						else {
onetermnew:;
							if ( factor == 0 || *factor > 2 ) {
							if ( pow > 0 ) {
								*r1++ = -SNUMBER; *r1++ = -1;
								t = r5;
								while ( t < r3 ) {
									t += *t; t[-1] = -t[-1];
								}
							}
							t = rr; *r1++ = *t++; *r1++ = 1; t++;
							COPYARG(r1,t);
							while ( t < m ) *r1++ = *t++;
							}
						}
						TermFree(EAscrat,"execarg");
					}
					}
					else {	/* AC.OldFactArgFlag is ON */
					{
					WORD *mnext, ncom;
					rnext = t + *t;
					GETSTOP(t,r6);
					t++;
					if ( factor == 0 ) {
					  while ( t < r6 ) {
/*
			#[ SYMBOL :
*/
						if ( *t == SYMBOL ) {
							r7 = t; r8 = t + t[1]; t += 2;
							while ( t < r8 ) {
								pow = t[1];
								mm = rnext;
								while ( mm < r3 ) {
									mnext = mm + *mm;
									GETSTOP(mm,mstop); mm++;
									while ( mm < mstop ) {
										if ( *mm != SYMBOL ) mm += mm[1];
										else break;
									}
									if ( *mm == SYMBOL ) {
										mstop = mm + mm[1]; mm += 2;
										while ( *mm != *t && mm < mstop ) mm += 2;
										if ( mm >= mstop ) pow = 0;
										else if ( pow > 0 && mm[1] > 0 ) {
											if ( mm[1] < pow ) pow = mm[1];
										}
										else if ( pow < 0 && mm[1] < 0 ) {
											if ( mm[1] > pow ) pow = mm[1];
										}
										else pow = 0;
									}
									else pow = 0;
									if ( pow == 0 ) break;
									mm = mnext;
								}
								if ( pow == 0 ) { t += 2; continue; }
/*
								We have a factor
*/
								action = 1; i = pow;
								if ( i > 0 ) {
									while ( --i >= 0 ) {
										*r1++ = -SYMBOL;
										*r1++ = *t;
									}
								}
								else {
									while ( i++ < 0 ) {
										*r1++ = 8 + ARGHEAD;
										for ( j = 1; j < ARGHEAD; j++ ) *r1++ = 0;
										*r1++ = 8; *r1++ = SYMBOL;
										*r1++ = 4; *r1++ = *t; *r1++ = -1;
										*r1++ = 1; *r1++ = 1; *r1++ = 3;
									}
								}
/*
								Now we have to remove the symbols
*/
								t[1] -= pow;
								mm = rnext;
								while ( mm < r3 ) {
									mnext = mm + *mm;
									GETSTOP(mm,mstop); mm++;
									while ( mm < mstop ) {
										if ( *mm != SYMBOL ) mm += mm[1];
										else break;
									}
									mstop = mm + mm[1]; mm += 2;
									while ( mm < mstop && *mm != *t ) mm += 2;
									mm[1] -= pow;
									mm = mnext;
								}
								t += 2;
							}
						}
/*
			#] SYMBOL : 
			#[ DOTPRODUCT :
*/
						else if ( *t == DOTPRODUCT ) {
							r7 = t; r8 = t + t[1]; t += 2;
							while ( t < r8 ) {
								pow = t[2];
								mm = rnext;
								while ( mm < r3 ) {
									mnext = mm + *mm;
									GETSTOP(mm,mstop); mm++;
									while ( mm < mstop ) {
										if ( *mm != DOTPRODUCT ) mm += mm[1];
										else break;
									}
									if ( *mm == DOTPRODUCT ) {
										mstop = mm + mm[1]; mm += 2;
										while ( ( *mm != *t || mm[1] != t[1] )
											 && mm < mstop ) mm += 3;
										if ( mm >= mstop ) pow = 0;
										else if ( pow > 0 && mm[2] > 0 ) {
											if ( mm[2] < pow ) pow = mm[2];
										}
										else if ( pow < 0 && mm[2] < 0 ) {
											if ( mm[2] > pow ) pow = mm[2];
										}
										else pow = 0;
									}
									else pow = 0;
									if ( pow == 0 ) break;
									mm = mnext;
								}
								if ( pow == 0 ) { t += 3; continue; }
/*
								We have a factor
*/
								action = 1; i = pow;
								if ( i > 0 ) {
									while ( --i >= 0 ) {
										*r1++ = 9 + ARGHEAD;
										for ( j = 1; j < ARGHEAD; j++ ) *r1++ = 0;
										*r1++ = 9; *r1++ = DOTPRODUCT;
										*r1++ = 5; *r1++ = *t; *r1++ = t[1]; *r1++ = 1;
										*r1++ = 1; *r1++ = 1; *r1++ = 3;
									}
								}
								else {
									while ( i++ < 0 ) {
										*r1++ = 9 + ARGHEAD;
										for ( j = 1; j < ARGHEAD; j++ ) *r1++ = 0;
										*r1++ = 9; *r1++ = DOTPRODUCT;
										*r1++ = 5; *r1++ = *t; *r1++ = t[1]; *r1++ = -1;
										*r1++ = 1; *r1++ = 1; *r1++ = 3;
									}
								}
/*
								Now we have to remove the dotproducts
*/
								t[2] -= pow;
								mm = rnext;
								while ( mm < r3 ) {
									mnext = mm + *mm;
									GETSTOP(mm,mstop); mm++;
									while ( mm < mstop ) {
										if ( *mm != DOTPRODUCT ) mm += mm[1];
										else break;
									}
									mstop = mm + mm[1]; mm += 2;
									while ( mm < mstop && ( *mm != *t
										|| mm[1] != t[1] ) ) mm += 3;
									mm[2] -= pow;
									mm = mnext;
								}
								t += 3;
							}
						}
/*
			#] DOTPRODUCT : 
			#[ DELTA/VECTOR :
*/
						else if ( *t == DELTA || *t == VECTOR ) {
							r7 = t; r8 = t + t[1]; t += 2;
							while ( t < r8 ) {
								mm = rnext;
								pow = 1;
								while ( mm < r3 ) {
									mnext = mm + *mm;
									GETSTOP(mm,mstop); mm++;
									while ( mm < mstop ) {
										if ( *mm != *r7 ) mm += mm[1];
										else break;
									}
									if ( *mm == *r7 ) {
										mstop = mm + mm[1]; mm += 2;
										while ( ( *mm != *t || mm[1] != t[1] )
											&& mm < mstop ) mm += 2;
										if ( mm >= mstop ) pow = 0;
									}
									else pow = 0;
									if ( pow == 0 ) break;
									mm = mnext;
								}
								if ( pow == 0 ) { t += 2; continue; }
/*
								We have a factor
*/
								action = 1;
								*r1++ = 8 + ARGHEAD;
								for ( j = 1; j < ARGHEAD; j++ ) *r1++ = 0;
								*r1++ = 8; *r1++ = *r7;
								*r1++ = 4; *r1++ = *t; *r1++ = t[1];
								*r1++ = 1; *r1++ = 1; *r1++ = 3;
/*
								Now we have to remove the delta's/vectors
*/
								mm = rnext;
								while ( mm < r3 ) {
									mnext = mm + *mm;
									GETSTOP(mm,mstop); mm++;
									while ( mm < mstop ) {
										if ( *mm != *r7 ) mm += mm[1];
										else break;
									}
									mstop = mm + mm[1]; mm += 2;
									while ( mm < mstop && (
									 *mm != *t || mm[1] != t[1] ) ) mm += 2;
									*mm = mm[1] = NOINDEX;
									mm = mnext;
								}
								*t = t[1] = NOINDEX;
								t += 2;
							}
						}
/*
			#] DELTA/VECTOR : 
			#[ INDEX :
*/
						else if ( *t == INDEX ) {
							r7 = t; r8 = t + t[1]; t += 2;
							while ( t < r8 ) {
								mm = rnext;
								pow = 1;
								while ( mm < r3 ) {
									mnext = mm + *mm;
									GETSTOP(mm,mstop); mm++;
									while ( mm < mstop ) {
										if ( *mm != *r7 ) mm += mm[1];
										else break;
									}
									if ( *mm == *r7 ) {
										mstop = mm + mm[1]; mm += 2;
										while ( *mm != *t 
											&& mm < mstop ) mm++;
										if ( mm >= mstop ) pow = 0;
									}
									else pow = 0;
									if ( pow == 0 ) break;
									mm = mnext;
								}
								if ( pow == 0 ) { t++; continue; }
/*
								We have a factor
*/
								action = 1;
/*
								The next looks like an error.
								We should have here a VECTOR or INDEX like object

								*r1++ = 7 + ARGHEAD;
								for ( j = 1; j < ARGHEAD; j++ ) *r1++ = 0;
								*r1++ = 7; *r1++ = *r7;
								*r1++ = 3; *r1++ = *t;
								*r1++ = 1; *r1++ = 1; *r1++ = 3;

								Replace this by:  (11-apr-2007)
*/
								if ( *t < 0 ) { *r1++ = -VECTOR; }
								else { *r1++ = -INDEX; }
								*r1++ = *t;
/*
								Now we have to remove the index
*/
								*t = NOINDEX;
								mm = rnext;
								while ( mm < r3 ) {
									mnext = mm + *mm;
									GETSTOP(mm,mstop); mm++;
									while ( mm < mstop ) {
										if ( *mm != *r7 ) mm += mm[1];
										else break;
									}
									mstop = mm + mm[1]; mm += 2;
									while ( mm < mstop && 
									 *mm != *t ) mm += 1;
									*mm = NOINDEX;
									mm = mnext;
								}
								t += 1;
							}
						}
/*
			#] INDEX : 
			#[ FUNCTION :
*/
						else if ( *t >= FUNCTION ) {
/*
							In the next code we should actually look inside
							the DENOMINATOR or EXPONENT for noncommuting objects
*/
							if ( *t >= FUNCTION &&
								functions[*t-FUNCTION].commute == 0 ) ncom = 0;
							else ncom = 1;
							if ( ncom ) {
								mm = r5 + 1;
								while ( mm < t && ( *mm == DUMMYFUN
								|| *mm == DUMMYTEN ) ) mm += mm[1];
								if ( mm < t ) { t += t[1]; continue; }
							}
							mm = rnext; pow = 1;
							while ( mm < r3 ) {
								mnext = mm + *mm;
								GETSTOP(mm,mstop); mm++;
								while ( mm < mstop ) {
									if ( *mm == *t && mm[1] == t[1] ) {
										for ( i = 2; i < t[1]; i++ ) {
											if ( mm[i] != t[i] ) break;
										}
										if ( i >= t[1] )
											{ mm += mm[1]; goto nextmterm; }
									}
									if ( ncom && *mm != DUMMYFUN && *mm != DUMMYTEN )
										{ pow = 0; break; }
									mm += mm[1];
								}
								if ( mm >= mstop ) pow = 0;
								if ( pow == 0 ) break;
nextmterm:						mm = mnext;
							}
							if ( pow == 0 ) { t += t[1]; continue; }
/*
							Copy the function
*/
							action = 1;
							*r1++ = t[1] + 4 + ARGHEAD;
							for ( i = 1; i < ARGHEAD; i++ ) *r1++ = 0;
							*r1++ = t[1] + 4;
							for ( i = 0; i < t[1]; i++ ) *r1++ = t[i];
							*r1++ = 1; *r1++ = 1; *r1++ = 3;
/*
							Now we have to take out the functions
*/
							mm = rnext;
							while ( mm < r3 ) {
								mnext = mm + *mm;
								GETSTOP(mm,mstop); mm++;
								while ( mm < mstop ) {
									if ( *mm == *t && mm[1] == t[1] ) {
										for ( i = 2; i < t[1]; i++ ) {
											if ( mm[i] != t[i] ) break;
										}
										if ( i >= t[1] ) {
											if ( functions[*t-FUNCTION].spec > 0 )
												*mm = DUMMYTEN;
											else
												*mm = DUMMYFUN;
											mm += mm[1];
											goto nextterm;
										}
									}
									mm += mm[1];
								}
nextterm:						mm = mnext;
							}
							if ( functions[*t-FUNCTION].spec > 0 )
									*t = DUMMYTEN;
							else
									*t = DUMMYFUN;
							action = 1;
							v[2] = DIRTYFLAG;
							t += t[1];
						}
/*
			#] FUNCTION : 
*/
						else {
							t += t[1];
						}
					  }
					}
					t = r5; pow = 1;
					while ( t < r3 ) {
						t += *t; if ( t[-1] > 0 ) { pow = 0; break; }
					}
/*
					We have to add here the code for computing the GCD
					and to divide it out.
*/
/*
			#[ Numerical factor :
*/
					t = r5;
					EAscrat = (UWORD *)(TermMalloc("execarg"));
					if ( t + *t == r3 ) goto oneterm;
					GETSTOP(t,r6);
					ngcd = t[t[0]-1];
					i = abs(ngcd)-1;
					while ( --i >= 0 ) EAscrat[i] = r6[i];
					t += *t;
					while ( t < r3 ) {
						GETSTOP(t,r6);
						i = t[t[0]-1];
						if ( AccumGCD(BHEAD EAscrat,&ngcd,(UWORD *)r6,i) ) goto execargerr;
						if ( ngcd == 3 && EAscrat[0] == 1 && EAscrat[1] == 1 ) break;
						t += *t;
					}
 					if ( ngcd != 3 || EAscrat[0] != 1 || EAscrat[1] != 1 ) {
						if ( pow ) ngcd = -ngcd;
						t = r5; r9 = r1; *r1++ = t[-ARGHEAD]; *r1++ = 1;
						FILLARG(r1); ngcd = REDLENG(ngcd);
						while ( t < r3 ) {
							GETSTOP(t,r6);
							r7 = t; r8 = r1;
							while ( r7 < r6) *r1++ = *r7++;
							t += *t;
							i = REDLENG(t[-1]);
							if ( DivRat(BHEAD (UWORD *)r6,i,EAscrat,ngcd,(UWORD *)r1,&nq) ) goto execargerr;
							nq = INCLENG(nq);
							i = ABS(nq)-1;
							r1 += i; *r1++ = nq; *r8 = r1-r8;
						}
						*r9 = r1-r9;
						ngcd = INCLENG(ngcd);
						i = ABS(ngcd)-1;
						if ( factor && *factor == 0 ) {}
						else if ( ( factor && factor[0] == 4 && factor[2] == 1
						&& factor[3] == -3 ) || pow == 0 ) {
							r9 = r1; *r1++ = ARGHEAD+2+i; *r1++ = 0;
							FILLARG(r1); *r1++ = i+2;
							for ( j = 0; j < i; j++ ) *r1++ = EAscrat[j];
							*r1++ = ngcd;
							if ( ToFast(r9,r9) ) r1 = r9+2;
						}
						else if ( factor && factor[0] == 4 && factor[2] == 1
						&& factor[3] > 0 && pow ) {
							if ( ngcd < 0 ) ngcd = -ngcd;
							*r1++ = -SNUMBER; *r1++ = -1;
							r9 = r1; *r1++ = ARGHEAD+2+i; *r1++ = 0;
							FILLARG(r1); *r1++ = i+2;
							for ( j = 0; j < i; j++ ) *r1++ = EAscrat[j];
							*r1++ = ngcd;
							if ( ToFast(r9,r9) ) r1 = r9+2;
						}
						else {
							if ( ngcd < 0 ) ngcd = -ngcd;
							if ( pow ) { *r1++ = -SNUMBER; *r1++ = -1; }
							if ( ngcd != 3 || EAscrat[0] != 1 || EAscrat[1] != 1 ) {
								r9 = r1; *r1++ = ARGHEAD+2+i; *r1++ = 0;
								FILLARG(r1); *r1++ = i+2;
								for ( j = 0; j < i; j++ ) *r1++ = EAscrat[j];
								*r1++ = ngcd;
								if ( ToFast(r9,r9) ) r1 = r9+2;
							}
						}
 					}
/*
			#] Numerical factor : 
*/
					else {
oneterm:;
						if ( factor == 0 || *factor > 2 ) {
						if ( pow > 0 ) {
							*r1++ = -SNUMBER; *r1++ = -1;
							t = r5;
							while ( t < r3 ) {
								t += *t; t[-1] = -t[-1];
							}
						}
						t = rr; *r1++ = *t++; *r1++ = 1; t++;
						COPYARG(r1,t);
						while ( t < m ) *r1++ = *t++;
						}
					}
					TermFree(EAscrat,"execarg");
					}
                	} /* AC.OldFactArgFlag */
				}
				r2[1] = r1 - r2;
				action = 1;
				v[2] = DIRTYFLAG;
			}
			else {
				r = t + t[1];
				while ( t < r ) *r1++ = *t++;
			}
		}
		r = term + *term;
		while ( t < r ) *r1++ = *t++;
		m = AT.WorkPointer;
		i = m[0] = r1 - m;
		t = term;
		while ( --i >= 0 ) *t++ = *m++;
		if ( AT.WorkPointer < t ) AT.WorkPointer = t;
	}
/*
  	#] FACTARG : 
*/
	AR.Cnumlhs = oldnumlhs;
	if ( action && Normalize(BHEAD term) ) goto execargerr;
	AT.WorkPointer = oldwork;
	if ( AT.WorkPointer < term + *term ) AT.WorkPointer = term + *term;
	AT.pWorkPointer = oldppointer;
	if ( GCDbuffer ) NumberFree(GCDbuffer,"execarg");
	return(action);
execargerr:
	AT.WorkPointer = oldwork;
	AT.pWorkPointer = oldppointer;
	MLOCK(ErrorMessageLock);
	MesCall("execarg");
	MUNLOCK(ErrorMessageLock);
	return(-1);
}

/*
  	#] execarg : 
  	#[ execterm :
*/

WORD execterm(PHEAD WORD *term, WORD level)
{
	GETBIDENTITY
	CBUF *C = cbuf+AM.rbufnum;
	WORD oldnumlhs = AR.Cnumlhs;
	WORD maxisat = C->lhs[level][2];
	WORD *buffer1 = 0;
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *t1, i;
	WORD olddeferflag = AR.DeferFlag, tryterm = 0;
	AR.DeferFlag = 0;
	do {
		AR.Cnumlhs = C->lhs[level][3];
		NewSort(BHEAD0);
/*
		Normally for function arguments we do not use PolyFun/PolyRatFun.
		Hence NewSort sets the corresponding variables to zero.
		Here we overwrite that.
*/
		AN.FunSorts[AR.sLevel]->PolyFlag = ( AR.PolyFun != 0 ) ? AR.PolyFunType: 0;
		if ( AR.PolyFun == 0 ) { AN.FunSorts[AR.sLevel]->PolyFlag = 0; }
		else if ( AR.PolyFunType == 1 ) { AN.FunSorts[AR.sLevel]->PolyFlag = 1; }
		else if ( AR.PolyFunType == 2 ) {
			if ( AR.PolyFunExp == 2 ) AN.FunSorts[AR.sLevel]->PolyFlag = 1;
			else                      AN.FunSorts[AR.sLevel]->PolyFlag = 2;
		}
		if ( buffer1 ) {
			term = buffer1;
			while ( *term ) {
				t1 = oldworkpointer;
				i = *term; while ( --i >= 0 ) *t1++ = *term++;
				AT.WorkPointer = t1;
				if ( Generator(BHEAD oldworkpointer,level) ) goto exectermerr;
			}
		}
		else {
			if ( Generator(BHEAD term,level) ) goto exectermerr;
		}
		if ( buffer1 ) {
			if ( tryterm ) { TermFree(buffer1,"buffer in sort statement"); tryterm = 0; }
			else { M_free((void *)buffer1,"buffer in sort statement"); }
			buffer1 = 0;
		}
		AN.tryterm = 1;
		if ( EndSort(BHEAD (WORD *)((VOID *)(&buffer1)),2) < 0 ) goto exectermerr;
		tryterm = AN.tryterm; AN.tryterm = 0;
		level = AR.Cnumlhs;
	} while ( AR.Cnumlhs < maxisat );
	AR.Cnumlhs = oldnumlhs;
	AR.DeferFlag = olddeferflag;
	term = buffer1;
	while ( *term ) {
		t1 = oldworkpointer;
		i = *term; while ( --i >= 0 ) *t1++ = *term++;
		AT.WorkPointer = t1;
		if ( Generator(BHEAD oldworkpointer,level) ) goto exectermerr;
	}
	if ( tryterm ) { TermFree(buffer1,"buffer in term statement"); tryterm = 0; }
	else { M_free(buffer1,"buffer in term statement"); }
	buffer1 = 0;
	AT.WorkPointer = oldworkpointer;
	return(0);
exectermerr:
	AT.WorkPointer = oldworkpointer;
	AR.DeferFlag = olddeferflag;
	MLOCK(ErrorMessageLock);
	MesCall("execterm");
	MUNLOCK(ErrorMessageLock);
	return(-1);
}

/*
  	#] execterm : 
  	#[ ArgumentImplode :
*/

int ArgumentImplode(PHEAD WORD *term, WORD *thelist)
{
	GETBIDENTITY
	WORD *liststart, *liststop, *inlist;
	WORD *w, *t, *tend, *tstop, *tt, *ttstop, *ttt, ncount, i;
	int action = 0;
	liststop = thelist + thelist[1];
	liststart = thelist + 2;
	t = term;
	tend = t + *t;
	tstop = tend - ABS(tend[-1]);
	t++;
	while ( t < tstop ) {
		if ( *t >= FUNCTION ) {
			inlist = liststart;
			while ( inlist < liststop && *inlist != *t ) inlist += inlist[1];
			if ( inlist < liststop ) {
				tt = t; ttstop = t + t[1]; w = AT.WorkPointer;
				for ( i = 0; i < FUNHEAD; i++ ) *w++ = *tt++;
				while ( tt < ttstop ) {
					ncount = 0;
					if ( *tt == -SNUMBER && tt[1] == 0 ) {
						ncount = 1; ttt = tt; tt += 2;
						while ( tt < ttstop && *tt == -SNUMBER && tt[1] == 0 ) {
							ncount++; tt += 2;
						}
					}
					if ( ncount > 0 ) {
						if ( tt < ttstop && *tt == -SNUMBER && ( tt[1] == 1 || tt[1] == -1 ) ) {
							*w++ = -SNUMBER;
							*w++ = (ncount+1) * tt[1];
							tt += 2;
							action = 1;
						}
						else if ( ( tt[0] == tt[ARGHEAD] + ARGHEAD )
						&& ( ABS(tt[tt[0]-1]) == 3 )
						&& ( tt[tt[0]-2] == 1 )
						&& ( tt[tt[0]-3] == 1 ) ) { /* Single term with coef +/- 1 */
							i = *tt; NCOPY(w,tt,i)
							w[-3] = ncount+1;
							action = 1;
						}
						else if ( *tt == -SYMBOL ) {
							*w++ = ARGHEAD+8;
							*w++ = 0;
							FILLARG(w)
							*w++ = 8;
							*w++ = SYMBOL;
							*w++ = tt[1];
							*w++ = 1;
							*w++ = ncount+1; *w++ = 1; *w++ = 3;
							tt += 2;
							action = 1;
						}
						else if ( *tt <= -FUNCTION ) {
							*w++ = ARGHEAD+FUNHEAD+4;
							*w++ = 0;
							FILLARG(w)
							*w++ = -*tt++;
							*w++ = FUNHEAD+4;
							FILLFUN(w)
							*w++ = ncount+1; *w++ = 1; *w++ = 3;
							action = 1;
						}
						else {
							while ( ttt < tt ) *w++ = *ttt++;
							if ( tt < ttstop && *tt == -SNUMBER ) {
								*w++ = *tt++; *w++ = *tt++;
							}
						}
					}
					else if ( *tt <= -FUNCTION ) {
						*w++ = *tt++;
					}
					else if ( *tt < 0 ) {
						*w++ = *tt++;
						*w++ = *tt++;
					}
					else {
						i = *tt; NCOPY(w,tt,i)
					}
				}
				AT.WorkPointer[1] = w - AT.WorkPointer;
				while ( tt < tend ) *w++ = *tt++;
				ttt = AT.WorkPointer; tt = t;
				while ( ttt < w ) *tt++ = *ttt++;
				term[0] = tt - term;
				AT.WorkPointer = tt;
				tend = tt; tstop = tt - ABS(tt[-1]);
			}
		}
		t += t[1];
	}
	if ( action ) {
		if ( Normalize(BHEAD term) ) return(-1);
	}
	return(0);
}

/*
  	#] ArgumentImplode : 
  	#[ ArgumentExplode :
*/

int ArgumentExplode(PHEAD WORD *term, WORD *thelist)
{
	GETBIDENTITY
	WORD *liststart, *liststop, *inlist, *old;
	WORD *w, *t, *tend, *tstop, *tt, *ttstop, *ttt, ncount, i;
	int action = 0;
	LONG x;
	liststop = thelist + thelist[1];
	liststart = thelist + 2;
	t = term;
	tend = t + *t;
	tstop = tend - ABS(tend[-1]);
	t++;
	while ( t < tstop ) {
		if ( *t >= FUNCTION ) {
			inlist = liststart;
			while ( inlist < liststop && *inlist != *t ) inlist += inlist[1];
			if ( inlist < liststop ) {
				tt = t; ttstop = t + t[1]; w = AT.WorkPointer;
				for ( i = 0; i < FUNHEAD; i++ ) *w++ = *tt++;
				while ( tt < ttstop ) {
					if ( *tt == -SNUMBER && tt[1] != 0 ) {
						if ( tt[1] < AM.MaxTer/((WORD)sizeof(WORD)*4)
							&& tt[1] > -(AM.MaxTer/((WORD)sizeof(WORD)*4))
							&& ( tt[1] > 1 || tt[1] < -1 ) ) {
							ncount = ABS(tt[1]);
							while ( ncount > 1 ) {
								*w++ = -SNUMBER; *w++ = 0; ncount--;
							}
							*w++ = -SNUMBER;
							if ( tt[1] < 0 ) *w++ = -1;
							else             *w++ =  1;
							tt += 2;
							action = 1;
						}
						else {
							*w++ = *tt++; *w++ = *tt++;
						}
					}
					else if ( *tt <= -FUNCTION ) {
						*w++ = *tt++;
					}
					else if ( *tt < 0 ) {
						*w++ = *tt++;
						*w++ = *tt++;
					}
					else if ( tt[0] == tt[ARGHEAD]+ARGHEAD ) {
						ttt = tt + tt[0] - 1;
						i = (ABS(ttt[0])-1)/2; 
						if ( i > 1 ) {
TooMany:					old = AN.currentTerm;
							AN.currentTerm = term;
							MesPrint("Too many arguments in output of ArgExplode");
							MesPrint("Term = %t");
							AN.currentTerm = old;
							return(-1);
						}
						if ( ttt[-1] != 1 ) goto NoExplode;
						x = ttt[-2];
						if ( 2*x > (AT.WorkTop-w)-*term ) goto TooMany;
						ncount = x - 1;
						while ( ncount > 0 ) {
							*w++ = -SNUMBER; *w++ = 0; ncount--;
						}
						ttt[-2] = 1;
						i = *tt; NCOPY(w,tt,i)
						action = 1;
					}
					else {
NoExplode:
						i = *tt; NCOPY(w,tt,i)
					}
				}
				AT.WorkPointer[1] = w - AT.WorkPointer;
				while ( tt < tend ) *w++ = *tt++;
				ttt = AT.WorkPointer; tt = t;
				while ( ttt < w ) *tt++ = *ttt++;
				term[0] = tt - term;
				AT.WorkPointer = tt;
				tend = tt; tstop = tt - ABS(tt[-1]);
			}
		}
		t += t[1];
	}
	if ( action ) {
		if ( Normalize(BHEAD term) ) return(-1);
	}
	return(0);
}

/*
  	#] ArgumentExplode : 
  	#[ ArgFactorize :
*/
/**
 *	Factorizes an argument in general notation (meaning that the first
 *	word of the argument is a positive size indicator)
 *	Input (argin):   pointer to the complete argument
 *	Output (argout): Pointer to where the output should be written.
 *	                 This is in the WorkSpace
 *	Return value should be negative if anything goes wrong.
 *
 *	The notation of the output should be a string of arguments terminated
 *	by the number zero.
 *
 *	Originally we sorted in a way that the constants came last. This gave
 *	conflicts with the dollar and expression factorizations (in the expressions
 *	we wanted the zero first and then followed by the constants).
 */
#define NEWORDER

int ArgFactorize(PHEAD WORD *argin, WORD *argout)
{
/*
  	#[ step 0 : Declarations and initializations
*/
	WORD *argfree, *argextra, *argcopy, *t, *tstop, *a, *a1, *a2;
#ifdef NEWORDER
	WORD *tt;
#endif
	WORD startebuf = cbuf[AT.ebufnum].numrhs,oldword;
	WORD oldsorttype = AR.SortType, numargs;
	int error = 0, action = 0, i, ii, number, sign = 1;

	*argout = 0;
/*
  	#] step 0 : 
  	#[ step 1 : Take care of ordering
*/
	AR.SortType = SORTHIGHFIRST;
	if ( oldsorttype != AR.SortType ) {
		NewSort(BHEAD0);
		oldword = argin[*argin]; argin[*argin] = 0;
		t = argin+ARGHEAD;
		while ( *t ) {
			tstop = t + *t;
			if ( AN.ncmod != 0 ) {
				if ( AN.ncmod != 1 || ( (WORD)AN.cmod[0] < 0 ) ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Factorization modulus a number, greater than a WORD not implemented.");
					MUNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
				if ( Modulus(t) ) {
					MLOCK(ErrorMessageLock);
					MesCall("ArgFactorize");
					MUNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
				if ( !*t) { t = tstop; continue; }
			}
			StoreTerm(BHEAD t);
			t = tstop;
		}
		EndSort(BHEAD argin+ARGHEAD,0);
		argin[*argin] = oldword;
	}
/*
  	#] step 1 : 
  	#[ step 2 : take out the 'content'.
*/
	argfree = TakeArgContent(BHEAD argin,argout);
	{
		a1 = argout;
		while ( *a1 ) {
			if ( a1[0] == -SNUMBER && ( a1[1] == 1 || a1[1] == -1 ) ) {
				if ( a1[1] == -1 ) { sign = -sign; a1[1] = 1; }
				if ( a1[2] ) {
					a = t = a1+2; while ( *t ) NEXTARG(t);
					i = t - a1-2;
					t = a1; NCOPY(t,a,i);
					*t = 0;
					continue;
				}
				else {
					a1[0] = 0;
				}
				break;
			}
			else if ( a1[0] == FUNHEAD+ARGHEAD+4 && a1[ARGHEAD] == FUNHEAD+4
			&& a1[*a1-1] == 3 && a1[*a1-2] == 1 && a1[*a1-3] == 1
			&& a1[ARGHEAD+1] >= FUNCTION ) {
				a = t = a1+*a1; while ( *t ) NEXTARG(t);
				i = t - a;
				*a1 = -a1[ARGHEAD+1]; t = a1+1; NCOPY(t,a,i);
				*t = 0;
			}
			NEXTARG(a1);
		}
	}
	if ( argfree == 0 ) {
		argfree = argin;
	}
	else if ( argfree[0] == ( argfree[ARGHEAD]+ARGHEAD ) ) {
		Normalize(BHEAD argfree+ARGHEAD);
		argfree[0] = argfree[ARGHEAD]+ARGHEAD;
		argfree[1] = 0;
		if ( ( argfree[0] == ARGHEAD+4 ) && ( argfree[ARGHEAD+3] == 3 )
			&& ( argfree[ARGHEAD+1] == 1 ) && ( argfree[ARGHEAD+2] == 1 ) ) {
			goto return0;
		}
	}
	else {
/*
		The way we took out objects is rather brutish. We have to
		normalize
*/
		NewSort(BHEAD0);
		t = argfree+ARGHEAD;
		while ( *t ) {
			tstop = t + *t;
			Normalize(BHEAD t);
			StoreTerm(BHEAD t);
			t = tstop;
		}
		EndSort(BHEAD argfree+ARGHEAD,0);
		t = argfree+ARGHEAD;
		while ( *t ) t += *t;
		*argfree = t - argfree;
	}
/*
  	#] step 2 : 
  	#[ step 3 : look whether we have done this one already.
*/
	if ( ( number = FindArg(BHEAD argfree) ) != 0 ) {
		if ( number > 0 ) t = cbuf[AT.fbufnum].rhs[number-1];
		else              t = cbuf[AC.ffbufnum].rhs[-number-1];
/*
		Now position on the result. Remember we have in the cache:
					inputarg,0,outputargs,0
		t is currently at inputarg. *inputarg is always positive.
		in principle this holds also for the arguments in the output
		but we take no risks here (in case of future developments).
*/
		t += *t; t++;
		tstop = t;
		ii = 0;
		while ( *tstop ) {
			if ( *tstop == -SNUMBER && tstop[1] == -1 ) {
				sign = -sign; ii += 2;
			}
			NEXTARG(tstop);
		}
		a = argout; while ( *a ) NEXTARG(a);
#ifndef NEWORDER
		if ( sign == -1 ) { *a++ = -SNUMBER; *a++ = -1; *a = 0; sign = 1; }
#endif
		i = tstop - t - ii;
		ii = a - argout;
		a2 = a; a1 = a + i;
		*a1 = 0;
		while ( ii > 0 ) { *--a1 = *--a2; ii--; }
		a = argout;
		while ( *t ) {
			if ( *t == -SNUMBER && t[1] == -1 ) { t += 2; }
			else { COPY1ARG(a,t) }
		}
		goto return0;
	}
/*
  	#] step 3 : 
  	#[ step 4 : invoke ConvertToPoly

				We make a copy first in case there are no factors
*/
	argcopy = TermMalloc("argcopy");
	for ( i = 0; i <= *argfree; i++ ) argcopy[i] = argfree[i];

	tstop = argfree + *argfree;
	{
		WORD sumcommu = 0;
		t = argfree + ARGHEAD;
		while ( t < tstop ) {
			sumcommu += DoesCommu(t);
			t += *t;
		}
		if ( sumcommu > 1 ) {
			MesPrint("ERROR: Cannot factorize an argument with more than one noncommuting object");
			Terminate(-1);
		}
	}
	t = argfree + ARGHEAD;

	while ( t < tstop ) {
		if ( ( t[1] != SYMBOL ) && ( *t != (ABS(t[*t-1])+1) ) ) {
			action = 1; break;
		}
		t += *t;
	}
	if ( action ) {
		t = argfree + ARGHEAD;
		argextra = AT.WorkPointer;
		NewSort(BHEAD0);
		while ( t < tstop ) {
			if ( LocalConvertToPoly(BHEAD t,argextra,startebuf,0) < 0 ) {
				error = -1;
getout:
				AR.SortType = oldsorttype;
				TermFree(argcopy,"argcopy");
				if ( argfree != argin ) TermFree(argfree,"argfree");
				MesCall("ArgFactorize");
				Terminate(-1);
				return(-1);
			}
			StoreTerm(BHEAD argextra);
			t += *t; argextra += *argextra;
		}
		if ( EndSort(BHEAD argfree+ARGHEAD,0) ) { error = -2; goto getout; }
		t = argfree + ARGHEAD;
		while ( *t > 0 ) t += *t;
		*argfree = t - argfree;
	}
/*
  	#] step 4 : 
  	#[ step 5 : If not in the tables, we have to do this by hard work.
*/

	a = argout;
	while ( *a ) NEXTARG(a);
	if ( poly_factorize_argument(BHEAD argfree,a) < 0 ) {
		MesCall("ArgFactorize");
		error = -1;
	}
/*
  	#] step 5 : 
  	#[ step 6 : use now ConvertFromPoly

		        Be careful: there should be more than one argument now.
*/
	if ( error == 0 && action ) {
	  a1 = a; NEXTARG(a1);
	  if ( *a1 != 0 ) {
		CBUF *C = cbuf+AC.cbufnum;
		CBUF *CC = cbuf+AT.ebufnum;
		WORD *oldworkpointer = AT.WorkPointer;
		WORD *argcopy2 = TermMalloc("argcopy2"), *a1, *a2;
		a1 = a; a2 = argcopy2;
		while ( *a1 ) {
			if ( *a1 < 0 ) {
				if ( *a1 > -FUNCTION ) *a2++ = *a1++;
				*a2++ = *a1++; *a2 = 0;
				continue;
			}
			t = a1 + ARGHEAD;
			tstop = a1 + *a1;
			argextra = AT.WorkPointer;
			NewSort(BHEAD0);
			while ( t < tstop ) {
				if ( ConvertFromPoly(BHEAD t,argextra,numxsymbol,CC->numrhs-startebuf+numxsymbol
				,startebuf-numxsymbol,1) <= 0 ) {
					TermFree(argcopy2,"argcopy2");
					LowerSortLevel();
					error = -3;
					goto getout;
				}
				t += *t;
				AT.WorkPointer = argextra + *argextra;
/*
				ConvertFromPoly leaves terms with subexpressions. Hence:
*/
				if ( Generator(BHEAD argextra,C->numlhs) ) {
					TermFree(argcopy2,"argcopy2");
					LowerSortLevel();
					error = -4;
					goto getout;
				}
			}
			AT.WorkPointer = oldworkpointer;
			if ( EndSort(BHEAD a2+ARGHEAD,0) ) { error = -5; goto getout; }
			t = a2+ARGHEAD; while ( *t ) t += *t;
			*a2 = t - a2; a2[1] = 0; ZEROARG(a2);
			ToFast(a2,a2); NEXTARG(a2);
			a1 = tstop;
		}
		i = a2 - argcopy2;
		a2 = argcopy2; a1 = a;
		NCOPY(a1,a2,i);
		*a1 = 0;
		TermFree(argcopy2,"argcopy2");
/*
		Erase the entries we made temporarily in cbuf[AT.ebufnum]
*/
		CC->numrhs = startebuf;
	  }
	  else {	/* no factorization. recover the argument from before step 3. */
		for ( i = 0; i <= *argcopy; i++ ) a[i] = argcopy[i];
	  }
	}
/*
  	#] step 6 : 
  	#[ step 7 : Add this one to the tables.

				Possibly drop some elements in the tables
				when they become too full.
*/
	if ( error == 0 && AN.ncmod == 0 ) {
		if ( InsertArg(BHEAD argcopy,a,0) < 0 ) { error = -1; }
	}
/*
  	#] step 7 : 
  	#[ step 8 : Clean up and return.

		Change the order of the arguments in argout and a.
		Use argcopy as spare space.
*/
	ii = a - argout;
	for ( i = 0; i < ii; i++ ) argcopy[i] = argout[i];
	a1 = a;
	while ( *a1 ) {
		if ( *a1 == -SNUMBER && a1[1] < 0 ) {
			sign = -sign; a1[1] = -a1[1];
			if ( a1[1] == 1 ) {
				a2 = a1+2; while ( *a2 ) NEXTARG(a2);
				i = a2-a1-2; a2 = a1+2;
				NCOPY(a1,a2,i);
				*a1 = 0;
			}
			while ( *a1 ) NEXTARG(a1);
			break;
		}
		else {
			if ( *a1 > 0 && *a1 == a1[ARGHEAD]+ARGHEAD && a1[*a1-1] < 0 ) {
				a1[*a1-1] = -a1[*a1-1]; sign = -sign;
			}
			if ( *a1 == ARGHEAD+4 && a1[ARGHEAD+1] == 1 && a1[ARGHEAD+2] == 1 ) {
				a2 = a1+ARGHEAD+4; while ( *a2 ) NEXTARG(a2);
				i = a2-a1-ARGHEAD-4; a2 = a1+ARGHEAD+4;
				NCOPY(a1,a2,i);
				*a1 = 0;
				break;
			}
			while ( *a1 ) NEXTARG(a1);
			break;
		}
		NEXTARG(a1);
	}
	i = a1 - a;
	a2 = argout;
	NCOPY(a2,a,i);
	for ( i = 0; i < ii; i++ ) *a2++ = argcopy[i];
#ifndef NEWORDER
	if ( sign == -1 ) { *a2++ = -SNUMBER; *a2++ = -1; sign = 1; }
#endif
	*a2 = 0;
	TermFree(argcopy,"argcopy");
return0:
	if ( argfree != argin ) TermFree(argfree,"argfree");
	if ( oldsorttype != AR.SortType ) {
		AR.SortType = oldsorttype;
		a = argout;
		while ( *a ) {
			if ( *a > 0 ) {
				NewSort(BHEAD0);
				oldword = a[*a]; a[*a] = 0;
				t = a+ARGHEAD;
				while ( *t ) {
					tstop = t + *t;
					StoreTerm(BHEAD t);
					t = tstop;
				}
				EndSort(BHEAD a+ARGHEAD,0);
				a[*a] = oldword;
				a += *a;
			}
			else { NEXTARG(a); }
		}
	}
#ifdef NEWORDER
	t = argout; numargs = 0;
	while ( *t ) {
		tt = t;
		NEXTARG(t);
		if ( *tt == ABS(t[-1])+1+ARGHEAD && sign == -1 ) { t[-1] = -t[-1]; sign = 1; }
		else if ( *tt == -SNUMBER && sign == -1 ) { tt[1] = -tt[1]; sign = 1; }
		numargs++;
	}
	if ( sign == -1 ) {
		*t++ = -SNUMBER; *t++ = -1; *t = 0; sign = 1; numargs++;
	}
#else
/*
	Now we have to sort the arguments
	First have the number of 'nontrivial/nonnumerical' arguments
	Then make a piece of code like in FullSymmetrize with that number
	of arguments to be symmetrized.
	Put a function in front
	Call the Symmetrize routine
*/
	t = argout; numargs = 0;
	while ( *t && *t != -SNUMBER && ( *t < 0 || ( ABS(t[*t-1]) != *t-1 ) ) ) {
		NEXTARG(t);
		numargs++;
	}
#endif
	if ( numargs > 1 ) {
		WORD *Lijst;
		WORD x[3];
		x[0] = argout[-FUNHEAD];
		x[1] = argout[-FUNHEAD+1];
		x[2] = argout[-FUNHEAD+2];
		while ( *t ) { NEXTARG(t); }
		argout[-FUNHEAD] = SQRTFUNCTION;
		argout[-FUNHEAD+1] = t-argout+FUNHEAD;
		argout[-FUNHEAD+2] = 0;
		AT.WorkPointer = t+1;
		Lijst = AT.WorkPointer;
		for ( i = 0; i < numargs; i++ ) Lijst[i] = i;
		AT.WorkPointer += numargs;
		error = Symmetrize(BHEAD argout-FUNHEAD,Lijst,numargs,1,SYMMETRIC);
		AT.WorkPointer = Lijst;
		argout[-FUNHEAD] = x[0];
		argout[-FUNHEAD+1] = x[1];
		argout[-FUNHEAD+2] = x[2];
#ifdef NEWORDER
/*
		Now we have to get a potential numerical argument to the first position
*/
		tstop = argout; while ( *tstop ) { NEXTARG(tstop); }
		t = argout; number = 0;
		while ( *t ) {
			tt = t; NEXTARG(t);
			if ( *tt == -SNUMBER ) {
				if ( number == 0 ) break;
				x[0] = tt[1];
				while ( tt > argout ) { *--t = *--tt; }
				argout[0] = -SNUMBER; argout[1] = x[0];
				break;
			}
			else if ( *tt == ABS(t[-1])+1+ARGHEAD ) {
				if ( number == 0 ) break;
				ii = t - tt;
				for ( i = 0; i < ii; i++ ) tstop[i] = tt[i];
				while ( tt > argout ) { *--t = *--tt; }
				for ( i = 0; i < ii; i++ ) argout[i] = tstop[i];
				*tstop = 0;
				break;
			}
			number++;
		}
#endif
	}
/*
  	#] step 8 : 
*/
	return(error);
}

/*
  	#] ArgFactorize : 
  	#[ FindArg :
*/
/**
 *	Looks the argument up in the (workers) table.
 *	If it is found the number in the table is returned (plus one to make it positive).
 *	If it is not found we look in the compiler provided table.
 *	If it is found - the number in the table is returned (minus one to make it negative).
 *	If in neither table we return zero.
 */

WORD FindArg(PHEAD WORD *a)
{
	int number;
	if ( AN.ncmod != 0 ) return(0);	/* no room for mod stuff */
	number = FindTree(AT.fbufnum,a);
	if ( number >= 0 ) return(number+1);
	number = FindTree(AC.ffbufnum,a);
	if ( number >= 0 ) return(-number-1);
	return(0);
}

/*
  	#] FindArg : 
  	#[ InsertArg :
*/
/**
 *	Inserts the argument into the (workers) table.
 *	If the table is too full we eliminate half of it.
 *	The eliminated elements are the ones that have not been used
 *	most recently, weighted by their total use and age(?).
 *	If par == 0 it inserts in the regular factorization cache
 *	If par == 1 it inserts in the cache defined with the FactorCache statement
 */

WORD InsertArg(PHEAD WORD *argin, WORD *argout,int par)
{
	CBUF *C;
	WORD *a, i, bufnum;
	if ( par == 0 ) {
		bufnum = AT.fbufnum;
		C = cbuf+bufnum;
		if ( C->numrhs >= (C->maxrhs-2) ) CleanupArgCache(BHEAD AT.fbufnum);
	}
	else if ( par == 1 ) {
		bufnum = AC.ffbufnum;
		C = cbuf+bufnum;
	}
	else { return(-1); }
	AddRHS(bufnum,1);
	AddNtoC(bufnum,*argin,argin,1);
	AddToCB(C,0)
	a = argout; while ( *a ) NEXTARG(a);
	i = a - argout;
	AddNtoC(bufnum,i,argout,2);
	AddToCB(C,0)
	return(InsTree(bufnum,C->numrhs));
}

/*
  	#] InsertArg : 
  	#[ CleanupArgCache :
*/
/**
 *	Cleans up the argument factorization cache.
 *	We throw half the elements.
 *	For a weight of what we want to keep we use the product of
 *	usage and the number in the buffer.
 */

int CleanupArgCache(PHEAD WORD bufnum)
{
	CBUF *C = cbuf + bufnum;
	COMPTREE *boomlijst = C->boomlijst;
	LONG *weights = (LONG *)Malloc1(2*(C->numrhs+1)*sizeof(LONG),"CleanupArgCache");
	LONG w, whalf, *extraweights;
	WORD *a, *to, *from;
	int i,j,k;
	for ( i = 1; i <= C->numrhs; i++ ) {
		weights[i] = ((LONG)i) * (boomlijst[i].usage);
	}
/*
		Now sort the weights and determine the halfway weight
*/
	extraweights = weights+C->numrhs+1;
	SortWeights(weights+1,extraweights,C->numrhs);
	whalf = weights[C->numrhs/2+1];
/*
		We should drop everybody with a weight < whalf.
*/
	to = C->Buffer;
	k = 1;
	for ( i = 1; i <= C->numrhs; i++ ) {
		from = C->rhs[i]; w = ((LONG)i) * (boomlijst[i].usage);
		if ( w >= whalf ) {
			if ( i < C->numrhs-1 ) {
				if ( to == from ) {
					to = C->rhs[i+1];
				}
				else {
					j = C->rhs[i+1] - from;
					C->rhs[k] = to;
					NCOPY(to,from,j)
				}
			}
			else if ( to == from ) {
				to += *to + 1; while ( *to ) NEXTARG(to); to++;
			}
			else {
				a = from; a += *a+1; while ( *a ) NEXTARG(a); a++;
				j = a - from;
				C->rhs[k] = to;
				NCOPY(to,from,j)
			}
			weights[k++] = boomlijst[i].usage;
		}
	}
	C->numrhs = --k;
	C->Pointer = to;
/*
		Next we need to rebuild the tree.
		Note that this can probably be done much faster by using the
		remains of the old tree !!!!!!!!!!!!!!!!
*/
	ClearTree(AT.fbufnum);
	for ( i = 1; i <= k; i++ ) {
		InsTree(AT.fbufnum,i);
		boomlijst[i].usage = weights[i];
	}
/*
		And cleanup
*/
	M_free(weights,"CleanupArgCache");
	return(0);
}

/*
  	#] CleanupArgCache : 
  	#[ ArgSymbolMerge :
*/

int ArgSymbolMerge(WORD *t1, WORD *t2)
{
	WORD *t1e = t1+t1[1];
	WORD *t2e = t2+t2[1];
	WORD *t1a = t1+2;
	WORD *t2a = t2+2;
	WORD *t3;
	while ( t1a < t1e && t2a < t2e ) {
		if ( *t1a < *t2a ) {
			if ( t1a[1] >= 0 ) {
				t3 = t1a+2;
				while ( t3 < t1e ) { t3[-2] = *t3; t3[-1] = t3[1]; t3 += 2; }
				t1e -= 2;
			}
			else t1a += 2;
		}
		else if ( *t1a > *t2a ) {
			if ( t2a[1] >= 0 ) t2a += 2;
			else {
				t3 = t1e;
				while ( t3 > t1a ) { *t3 = t3[-2]; t3[1] = t3[-1]; t3 -= 2; }
				*t1a++ = *t2a++;
				*t1a++ = *t2a++;
				t1e += 2;
			}
		}
		else {
			if ( t2a[1] < t1a[1] ) t1a[1] = t2a[1];
			t1a += 2; t2a += 2;
		}
	}
	while ( t2a < t2e ) {
		if ( t2a[1] < 0 ) {
			*t1a++ = *t2a++;
			*t1a++ = *t2a++;
		}
		else t2a += 2;
	}
	while ( t1a < t1e ) {
		if ( t1a[1] >= 0 ) {
			t3 = t1a+2;
			while ( t3 < t1e ) { t3[-2] = *t3; t3[-1] = t3[1]; t3 += 2; }
			t1e -= 2;
		}
		else t1a += 2;
	}
	t1[1] = t1a - t1;
	return(0);
}

/*
  	#] ArgSymbolMerge : 
  	#[ ArgDotproductMerge :
*/

int ArgDotproductMerge(WORD *t1, WORD *t2)
{
	WORD *t1e = t1+t1[1];
	WORD *t2e = t2+t2[1];
	WORD *t1a = t1+2;
	WORD *t2a = t2+2;
	WORD *t3;
	while ( t1a < t1e && t2a < t2e ) {
		if ( *t1a < *t2a || ( *t1a == *t2a && t1a[1] < t2a[1] ) ) {
			if ( t1a[2] >= 0 ) {
				t3 = t1a+3;
				while ( t3 < t1e ) { t3[-3] = *t3; t3[-2] = t3[1]; t3[-1] = t3[2]; t3 += 3; }
				t1e -= 3;
			}
			else t1a += 3;
		}
		else if ( *t1a > *t2a || ( *t1a == *t2a && t1a[1] > t2a[1] ) ) {
			if ( t2a[2] >= 0 ) t2a += 3;
			else {
				t3 = t1e;
				while ( t3 > t1a ) { *t3 = t3[-3]; t3[1] = t3[-2]; t3[2] = t3[-1]; t3 -= 3; }
				*t1a++ = *t2a++;
				*t1a++ = *t2a++;
				*t1a++ = *t2a++;
				t1e += 3;
			}
		}
		else {
			if ( t2a[2] < t1a[2] ) t1a[2] = t2a[2];
			t1a += 3; t2a += 3;
		}
	}
	while ( t2a < t2e ) {
		if ( t2a[2] < 0 ) {
			*t1a++ = *t2a++;
			*t1a++ = *t2a++;
			*t1a++ = *t2a++;
		}
		else t2a += 3;
	}
	while ( t1a < t1e ) {
		if ( t1a[2] >= 0 ) {
			t3 = t1a+3;
			while ( t3 < t1e ) { t3[-3] = *t3; t3[-2] = t3[1]; t3[-1] = t3[2]; t3 += 3; }
			t1e -= 3;
		}
		else t1a += 2;
	}
	t1[1] = t1a - t1;
	return(0);
}

/*
  	#] ArgDotproductMerge : 
  	#[ TakeArgContent :
*/
/**
 *	Implements part of the old ExecArg in which we take common factors
 *	from arguments with more than one term.
 *	The common pieces are put in argout as a sequence of arguments.
 *	The part with the multiple terms that are now relative prime is
 *	put in argfree which is allocated via TermMalloc and is given as the
 *	return value.
 *	The difference with the old code is that negative powers are always
 *	removed. Hence it is as in MakeInteger in which only numerators will
 *	be left: now only zero or positive powers will be remaining.
 */

WORD *TakeArgContent(PHEAD WORD *argin, WORD *argout)
{
	GETBIDENTITY
	WORD *t, *rnext, *r1, *r2, *r3, *r5, *r6, *r7, *r8, *r9;
	WORD pow, *mm, *mnext, *mstop, *argin2 = argin, *argin3 = argin, *argfree;
	WORD ncom;
	int j, i, act;
	r5 = t = argin + ARGHEAD;
	r3 = argin + *argin;
	rnext = t + *t;
	GETSTOP(t,r6);
	r1 = argout;
	t++;
/*
		First pass: arrange everything but the symbols and dotproducts.
		They need separate treatment because we have to take out negative
		powers.
*/
	while ( t < r6 ) {
/*
			#[ DELTA/VECTOR :
*/
		if ( *t == DELTA || *t == VECTOR ) {
			r7 = t; r8 = t + t[1]; t += 2;
			while ( t < r8 ) {
				mm = rnext;
				pow = 1;
				while ( mm < r3 ) {
					mnext = mm + *mm;
					GETSTOP(mm,mstop); mm++;
					while ( mm < mstop ) {
						if ( *mm != *r7 ) mm += mm[1];
						else break;
					}
					if ( *mm == *r7 ) {
						mstop = mm + mm[1]; mm += 2;
						while ( ( *mm != *t || mm[1] != t[1] )
							&& mm < mstop ) mm += 2;
						if ( mm >= mstop ) pow = 0;
					}
					else pow = 0;
					if ( pow == 0 ) break;
					mm = mnext;
				}
				if ( pow == 0 ) { t += 2; continue; }
/*
				We have a factor
*/
				*r1++ = 8 + ARGHEAD;
				for ( j = 1; j < ARGHEAD; j++ ) *r1++ = 0;
				*r1++ = 8; *r1++ = *r7;
				*r1++ = 4; *r1++ = *t; *r1++ = t[1];
				*r1++ = 1; *r1++ = 1; *r1++ = 3;
				argout = r1;
/*
				Now we have to remove the delta's/vectors
*/
				mm = rnext;
				while ( mm < r3 ) {
					mnext = mm + *mm;
					GETSTOP(mm,mstop); mm++;
					while ( mm < mstop ) {
						if ( *mm != *r7 ) mm += mm[1];
						else break;
					}
					mstop = mm + mm[1]; mm += 2;
					while ( mm < mstop && (
					 *mm != *t || mm[1] != t[1] ) ) mm += 2;
					*mm = mm[1] = NOINDEX;
					mm = mnext;
				}
				*t = t[1] = NOINDEX;
				t += 2;
			}
		}
/*
			#] DELTA/VECTOR : 
			#[ INDEX :
*/
		else if ( *t == INDEX ) {
			r7 = t; r8 = t + t[1]; t += 2;
			while ( t < r8 ) {
				mm = rnext;
				pow = 1;
				while ( mm < r3 ) {
					mnext = mm + *mm;
					GETSTOP(mm,mstop); mm++;
					while ( mm < mstop ) {
						if ( *mm != *r7 ) mm += mm[1];
						else break;
					}
					if ( *mm == *r7 ) {
						mstop = mm + mm[1]; mm += 2;
						while ( *mm != *t 
							&& mm < mstop ) mm++;
						if ( mm >= mstop ) pow = 0;
					}
					else pow = 0;
					if ( pow == 0 ) break;
					mm = mnext;
				}
				if ( pow == 0 ) { t++; continue; }
/*
				We have a factor
*/
				if ( *t < 0 ) { *r1++ = -VECTOR; }
				else          { *r1++ = -INDEX; }
				*r1++ = *t;
				argout = r1;
/*
				Now we have to remove the index
*/
				*t = NOINDEX;
				mm = rnext;
				while ( mm < r3 ) {
					mnext = mm + *mm;
					GETSTOP(mm,mstop); mm++;
					while ( mm < mstop ) {
						if ( *mm != *r7 ) mm += mm[1];
						else break;
					}
					mstop = mm + mm[1]; mm += 2;
					while ( mm < mstop && 
					 *mm != *t ) mm += 1;
					*mm = NOINDEX;
					mm = mnext;
				}
				t += 1;
			}
		}
/*
			#] INDEX : 
			#[ FUNCTION :
*/
		else if ( *t >= FUNCTION ) {
/*
			In the next code we should actually look inside
			the DENOMINATOR or EXPONENT for noncommuting objects
*/
			if ( *t >= FUNCTION &&
				functions[*t-FUNCTION].commute == 0 ) ncom = 0;
			else ncom = 1;
			if ( ncom ) {
				mm = r5 + 1;
				while ( mm < t && ( *mm == DUMMYFUN
				|| *mm == DUMMYTEN ) ) mm += mm[1];
				if ( mm < t ) { t += t[1]; continue; }
			}
			mm = rnext; pow = 1;
			while ( mm < r3 ) {
				mnext = mm + *mm;
				GETSTOP(mm,mstop); mm++;
				while ( mm < mstop ) {
					if ( *mm == *t && mm[1] == t[1] ) {
						for ( i = 2; i < t[1]; i++ ) {
							if ( mm[i] != t[i] ) break;
						}
						if ( i >= t[1] )
							{ mm += mm[1]; goto nextmterm; }
					}
					if ( ncom && *mm != DUMMYFUN && *mm != DUMMYTEN )
						{ pow = 0; break; }
					mm += mm[1];
				}
				if ( mm >= mstop ) pow = 0;
				if ( pow == 0 ) break;
nextmterm:		mm = mnext;
			}
			if ( pow == 0 ) { t += t[1]; continue; }
/*
			Copy the function
*/
			*r1++ = t[1] + 4 + ARGHEAD;
			for ( i = 1; i < ARGHEAD; i++ ) *r1++ = 0;
			*r1++ = t[1] + 4;
			for ( i = 0; i < t[1]; i++ ) *r1++ = t[i];
			*r1++ = 1; *r1++ = 1; *r1++ = 3;
			argout = r1;
/*
			Now we have to take out the functions
*/
			mm = rnext;
			while ( mm < r3 ) {
				mnext = mm + *mm;
				GETSTOP(mm,mstop); mm++;
				while ( mm < mstop ) {
					if ( *mm == *t && mm[1] == t[1] ) {
						for ( i = 2; i < t[1]; i++ ) {
							if ( mm[i] != t[i] ) break;
						}
						if ( i >= t[1] ) {
							if ( functions[*t-FUNCTION].spec > 0 )
								*mm = DUMMYTEN;
							else
								*mm = DUMMYFUN;
							mm += mm[1];
							goto nextterm;
						}
					}
					mm += mm[1];
				}
nextterm:						mm = mnext;
			}
			if ( functions[*t-FUNCTION].spec > 0 )
					*t = DUMMYTEN;
			else
					*t = DUMMYFUN;
			t += t[1];
		}
/*
			#] FUNCTION : 
*/
		else {
			t += t[1];
		}
	}
/*
			#[ SYMBOL :

		Now collect all symbols. We can use the space after r1 as storage
*/
	t = argin+ARGHEAD;
	rnext = t + *t;
	r2 = r1;
	while ( t < r3 ) {
		GETSTOP(t,r6);
		t++;
		act = 0;
		while ( t < r6 ) {
			if ( *t == SYMBOL ) {
				act = 1;
				i = t[1];
				NCOPY(r2,t,i)
			}
			else { t += t[1]; }
		}
		if ( act == 0 ) {
			*r2++ = SYMBOL; *r2++ = 2;
		}
		t = rnext; rnext = rnext + *rnext;
	}
	*r2 = 0;
	argin2 = argin;
/*
		Now we have a list of all symbols as a sequence of SYMBOL subterms.
		Any symbol that is absent in a subterm has power zero.
		We now need a list of all minimum powers.
		This can be done by subsequent merges.
*/
	r7 = r1;          /* The first object into which we merge. */	
	r8 = r7 + r7[1];  /* The object that gets merged into r7.  */
	while ( *r8 ) {
		r2 = r8 + r8[1]; /* Next object */
		ArgSymbolMerge(r7,r8);
		r8 = r2;
	}
/*
		Now we have to divide by the object in r7 and take it apart as factors.
		The division can be simple if there are no negative powers.
*/
	if ( r7[1] > 2 ) {
		r8 = r7+2;
		r2 = r7 + r7[1];
		act = 0;
		pow = 0;
		while ( r8 < r2 ) {
			if ( r8[1] < 0 ) { act = 1; pow += -r8[1]*(ARGHEAD+8); }
			else { pow += 2*r8[1]; }
			r8 += 2;
		}
/*
		The amount of space we need to move r7 is given in pow
*/
		if ( act == 0 ) {	/* this can be done 'in situ' */
			t = argin + ARGHEAD;
			while ( t < r3 ) {
				rnext = t + *t;
				GETSTOP(t,r6);
				t++;
				while ( t < r6 ) {
					if ( *t != SYMBOL ) { t += t[1]; continue; }
					r8 = r7+2; r9 = t + t[1]; t += 2;
					while ( ( t < r9 ) && ( r8 < r2 ) ) {
						if ( *t == *r8 ) {
							t[1] -= r8[1]; t += 2; r8 += 2;
						}
						else { /* *t must be < than *r8 !!! */
							t += 2;
						}
					}
					t = r9;
				}
				t = rnext;
			}
/*
			And now the factors that go to argout.
			First we have to move r7 out of the way.
*/
			r8 = r7+pow; i = r7[1];
			while ( --i >= 0 ) r8[i] = r7[i];
			r2 += pow;
			r8 += 2;
			while ( r8 < r2 ) {
				for ( i = 0; i < r8[1]; i++ ) { *r1++ = -SYMBOL; *r1++ = *r8; }
				r8 += 2;
			}
		}
		else {	/* this needs a new location */
			argin2 = TermMalloc("TakeArgContent2");
/*
			We have to multiply the inverse of r7 into argin
			The answer should go to argin2.
*/
			r5 = argin2; *r5++ = 0; *r5++ = 0; FILLARG(r5);
			t = argin+ARGHEAD;
			while ( t < r3 ) {
				rnext = t + *t;
				GETSTOP(t,r6);
				r9 = r5;
				*r5++ = *t++ + r7[1];
				while ( t < r6 ) *r5++ = *t++;
				i = r7[1] - 2; r8 = r7+2;
				*r5++ = r7[0]; *r5++ = r7[1];
				while ( i > 0 ) { *r5++ = *r8++; *r5++ = -*r8++; i -= 2; }
				while ( t < rnext ) *r5++ = *t++;
				Normalize(BHEAD r9);
				r5 = r9 + *r9;
			}
			*r5 = 0;
			*argin2 = r5-argin2;
/*
			We may have to sort the terms in argin2.
*/
			NewSort(BHEAD0);
			t = argin2+ARGHEAD;
			while ( *t ) {
				StoreTerm(BHEAD t);
				t += *t;
			}
			t = argin2+ARGHEAD;
			if ( EndSort(BHEAD t,0) < 0 ) goto Irreg;
			while ( *t ) t += *t;
			*argin2 = t - argin2;
			r3 = t;
/*
			And now the factors that go to argout.
			First we have to move r7 out of the way.
*/
			r8 = r7+pow; i = r7[1];
			while ( --i >= 0 ) r8[i] = r7[i];
			r2 += pow;
			r8 += 2;
			while ( r8 < r2 ) {
				if ( r8[1] >= 0 ) {
					for ( i = 0; i < r8[1]; i++ ) { *r1++ = -SYMBOL; *r1++ = *r8; }
				}
				else {
					for ( i = 0; i < -r8[1]; i++ ) {
						*r1++ = ARGHEAD+8; *r1++ = 0;
						FILLARG(r1);
						*r1++ = 8; *r1++ = SYMBOL; *r1++ = 4; *r1++ = *r8;
						*r1++ = -1; *r1++ = 1; *r1++ = 1; *r1++ = 3;
					}
				}
				r8 += 2;
			}
			argout = r1;
		}
	}
/*
			#] SYMBOL : 
			#[ DOTPRODUCT :

		Now collect all dotproducts. We can use the space after r1 as storage
*/
	  t = argin2+ARGHEAD;
	  rnext = t + *t;
	  r2 = r1;
	  while ( t < r3 ) {
		GETSTOP(t,r6);
		t++;
		act = 0;
		while ( t < r6 ) {
			if ( *t == DOTPRODUCT ) {
				act = 1;
				i = t[1];
				NCOPY(r2,t,i)
			}
			else { t += t[1]; }
		}
		if ( act == 0 ) {
			*r2++ = DOTPRODUCT; *r2++ = 2;
		}
		t = rnext; rnext = rnext + *rnext;
	  }
	  *r2 = 0;
	  argin3 = argin2;
/*
		Now we have a list of all dotproducts as a sequence of DOTPRODUCT
		subterms. Any dotproduct that is absent in a subterm has power zero.
		We now need a list of all minimum powers.
		This can be done by subsequent merges.
*/
	  r7 = r1;          /* The first object into which we merge. */	
	  r8 = r7 + r7[1];  /* The object that gets merged into r7.  */
	  while ( *r8 ) {
		r2 = r8 + r8[1]; /* Next object */
		ArgDotproductMerge(r7,r8);
		r8 = r2;
	  }
/*
		Now we have to divide by the object in r7 and take it apart as factors.
		The division can be simple if there are no negative powers.
*/
	  if ( r7[1] > 2 ) {
		r8 = r7+2;
		r2 = r7 + r7[1];
		act = 0;
		pow = 0;
		while ( r8 < r2 ) {
			if ( r8[2] < 0 ) { pow += -r8[2]*(ARGHEAD+9); }
			else             { pow +=  r8[2]*(ARGHEAD+9); }
			r8 += 3;
		}
/*
		The amount of space we need to move r7 is given in pow
		For dotproducts we always need a new location
*/
		{
			argin3 = TermMalloc("TakeArgContent3");
/*
			We have to multiply the inverse of r7 into argin
			The answer should go to argin2.
*/
			r5 = argin3; *r5++ = 0; *r5++ = 0; FILLARG(r5);
			t = argin2+ARGHEAD;
			while ( t < r3 ) {
				rnext = t + *t;
				GETSTOP(t,r6);
				r9 = r5;
				*r5++ = *t++ + r7[1];
				while ( t < r6 ) *r5++ = *t++;
				i = r7[1] - 2; r8 = r7+2;
				*r5++ = r7[0]; *r5++ = r7[1];
				while ( i > 0 ) { *r5++ = *r8++; *r5++ = *r8++; *r5++ = -*r8++; i -= 3; }
				while ( t < rnext ) *r5++ = *t++;
				Normalize(BHEAD r9);
				r5 = r9 + *r9;
			}
			*r5 = 0;
			*argin3 = r5-argin3;
/*
			We may have to sort the terms in argin3.
*/
			NewSort(BHEAD0);
			t = argin3+ARGHEAD;
			while ( *t ) {
				StoreTerm(BHEAD t);
				t += *t;
			}
			t = argin3+ARGHEAD;
			if ( EndSort(BHEAD t,0) < 0 ) goto Irreg;
			while ( *t ) t += *t;
			*argin3 = t - argin3;
			r3 = t;
/*
			And now the factors that go to argout.
			First we have to move r7 out of the way.
*/
			r8 = r7+pow; i = r7[1];
			while ( --i >= 0 ) r8[i] = r7[i];
			r2 += pow;
			r8 += 2;
			while ( r8 < r2 ) {
				for ( i = ABS(r8[2]); i > 0; i-- ) {
					*r1++ = ARGHEAD+9; *r1++ = 0; FILLARG(r1);
					*r1++ = 9; *r1++ = DOTPRODUCT; *r1++ = 5; *r1++ = *r8;
					*r1++ = r8[1]; *r1++ = r8[2] < 0 ? -1: 1;
					*r1++ = 1; *r1++ = 1; *r1++ = 3;
				}
				r8 += 3;
			}
			argout = r1;
		}
	  }
/*
			#] DOTPRODUCT : 

	We have now in argin3 the argument stripped of negative powers and
	common factors. The only thing left to deal with is to make the
	coefficients integer. For that we have to find the LCM of the denominators
	and the GCD of the numerators. And to start with, the sign.
	We force the sign of the first term to be positive.
*/
	t = argin3 + ARGHEAD; pow = 1;
	t += *t;
	if ( t[-1] < 0 ) {
		pow = 0;
		t[-1] = -t[-1];
		while ( t < r3 ) {
			t += *t; t[-1] = -t[-1];
		}
	}
/*
	Now the GCD of the numerators and the LCM of the denominators:
*/
	argfree = TermMalloc("TakeArgContent1");
	if ( AN.cmod != 0 ) {
		r1 = MakeMod(BHEAD argin3,r1,argfree);
	}
	else {
		r1 = MakeInteger(BHEAD argin3,r1,argfree);
	}
	if ( pow == 0 ) {
		*r1++ = -SNUMBER;
		*r1++ = -1;
	}
	*r1 = 0;
/*
	Cleanup
*/
	if ( argin3 != argin2 ) TermFree(argin3,"TakeArgContent3");
	if ( argin2 != argin  ) TermFree(argin2,"TakeArgContent2");
	return(argfree);
Irreg:
	MesPrint("Irregularity while sorting argument in TakeArgContent");
	if ( argin3 != argin2 ) TermFree(argin3,"TakeArgContent3");
	if ( argin2 != argin  ) TermFree(argin2,"TakeArgContent2");
	Terminate(-1);
	return(0);
}

/*
  	#] TakeArgContent : 
  	#[ MakeInteger :
*/
/**
 *	For normalizing everything to integers we have to
 *	determine for all elements of this argument the LCM of
 *	the denominators and the GCD of the numerators.
 *	The input argument is in argin.
 *	The number that comes out should go to argout.
 *	The new pointer in the argout buffer is the return value.
 *	The normalized argument is in argfree.
 */

WORD *MakeInteger(PHEAD WORD *argin,WORD *argout,WORD *argfree)
{
	UWORD *GCDbuffer, *GCDbuffer2, *LCMbuffer, *LCMb, *LCMc;
	WORD *r, *r1, *r2, *r3, *r4, *r5, *rnext, i, k, j;
	WORD kGCD, kLCM, kGCD2, kkLCM, jLCM, jGCD;
	GCDbuffer = NumberMalloc("MakeInteger");
	GCDbuffer2 = NumberMalloc("MakeInteger");
	LCMbuffer = NumberMalloc("MakeInteger");
	LCMb = NumberMalloc("MakeInteger");
	LCMc = NumberMalloc("MakeInteger");
	r4 = argin + *argin;
	r = argin + ARGHEAD;
/*
	First take the first term to load up the LCM and the GCD
*/
	r2 = r + *r;
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
	Now go through the rest of the terms in this argument.
*/
	while ( r1 < r4 ) {
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
				NumberFree(GCDbuffer,"MakeInteger");
				NumberFree(GCDbuffer2,"MakeInteger");
				NumberFree(LCMbuffer,"MakeInteger");
				NumberFree(LCMb,"MakeInteger"); NumberFree(LCMc,"MakeInteger");
				goto MakeIntegerErr;
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
				NumberFree(GCDbuffer,"MakeInteger"); NumberFree(GCDbuffer2,"MakeInteger");
				NumberFree(LCMbuffer,"MakeInteger"); NumberFree(LCMb,"MakeInteger"); NumberFree(LCMc,"MakeInteger");
				goto MakeIntegerErr;
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
/*
	Now we have to write this to argout
*/
	if ( ( j == 3 ) && ( r3[1] == 1 ) && ( (WORD)(r3[0]) > 0 ) ) {
		*argout = -SNUMBER;
		argout[1] = r3[0];
		r1 = argout+2;
	}
	else {
		r1 = argout;
		*r1++ = j+1+ARGHEAD; *r1++ = 0; FILLARG(r1);
		*r1++ = j+1; r2 = r3;
		for ( i = 0; i < k; i++ ) { *r1++ = *r2++; *r1++ = *r2++; }
		*r1++ = j;
	}
/*
	Next we have to take the factor out from the argument.
	This cannot be done in location, because the denominator stuff can make
	coefficients longer.
*/
	r2 = argfree + 2; FILLARG(r2)
	while ( r < r4 ) {
		rnext = r + *r;
		j = ABS(rnext[-1]);
		r5 = rnext - j;
		r3 = r2;
		while ( r < r5 ) *r2++ = *r++;
		j = (j-1)/2;	/* reduced length. Remember, k is the other red length */
		if ( DivRat(BHEAD (UWORD *)r5,j,GCDbuffer,k,(UWORD *)r2,&i) ) {
			goto MakeIntegerErr;
		}
		i = 2*i+1;
		r2 = r2 + i;
		if ( rnext[-1] < 0 ) r2[-1] = -i;
		else                 r2[-1] =  i;
		*r3 = r2-r3;
		r = rnext;
	}
	*r2 = 0;
	argfree[0] = r2-argfree;
	argfree[1] = 0;
/*
	Cleanup
*/
	NumberFree(LCMc,"MakeInteger");
	NumberFree(LCMb,"MakeInteger");
	NumberFree(LCMbuffer,"MakeInteger");
	NumberFree(GCDbuffer2,"MakeInteger");
	NumberFree(GCDbuffer,"MakeInteger");
	return(r1);

MakeIntegerErr:
	MesCall("MakeInteger");
	Terminate(-1);
	return(0);
}

/*
  	#] MakeInteger : 
  	#[ MakeMod :
*/
/**
 *	Similar to MakeInteger but now with modulus arithmetic using only
 *	a one WORD 'prime'. We make the coefficient of the first term in the
 *	argument equal to one.
 *	Already the coefficients are taken modulus AN.cmod and AN.ncmod == 1
 */

WORD *MakeMod(PHEAD WORD *argin,WORD *argout,WORD *argfree)
{
	WORD *r, *instop, *r1, *m, x, xx, ix, ip;
	int i;
	r = argin; instop = r + *r; r += ARGHEAD;
	x = r[*r-3];
	if ( r[*r-1] < 0 ) x += AN.cmod[0];
	if ( GetModInverses(x,(WORD)(AN.cmod[0]),&ix,&ip) ) {
		Terminate(-1);
	}
	argout[0] = -SNUMBER;
	argout[1] = x;
	argout[2] = 0;
	r1 = argout+2;
/*
	Now we have to multiply all coefficients by ix.
	This does not make things longer, but we should keep to the conventions
	of MakeInteger.
*/
	m = argfree + ARGHEAD;
	while ( r < instop ) {
		xx = r[*r-3]; if ( r[*r-1] < 0 ) xx += AN.cmod[0];
		xx = (WORD)((((LONG)xx)*ix) % AN.cmod[0]);
		if ( xx != 0 ) {
			i = *r; NCOPY(m,r,i);
			m[-3] = xx; m[-1] = 3;
		}
		else { r += *r; }
	}
	*m = 0;
	*argfree = m - argfree;
	argfree[1] = 0;
	argfree += 2; FILLARG(argfree);
	return(r1);
}

/*
  	#] MakeMod : 
  	#[ SortWeights :
*/
/**
 *	Sorts an array of LONGS in the same way SplitMerge (in sort.c) works
 *	We use gradual division in two.
 */

void SortWeights(LONG *weights,LONG *extraspace,WORD number)
{
	LONG w, *fill, *from1, *from2;
	int n1,n2,i;
	if ( number >= 4 ) {
		n1 = number/2; n2 = number - n1;
		SortWeights(weights,extraspace,n1);
		SortWeights(weights+n1,extraspace,n2);
/*
		We copy the first patch to the extra space. Then we merge
		Note that a potential remaining n2 objects are already in place.
*/
		for ( i = 0; i < n1; i++ ) extraspace[i] = weights[i];
		fill = weights; from1 = extraspace; from2 = weights+n1;
		while ( n1 > 0 && n2 > 0 ) {
			if ( *from1 <= *from2 ) { *fill++ = *from1++; n1--; }
			else                    { *fill++ = *from2++; n2--; }
		}
		while ( n1 > 0 ) { *fill++ = *from1++; n1--; }
	}
/*
	Special cases
*/
	else if ( number == 3 ) { /* 6 permutations of which one is trivial */
		if ( weights[0] > weights[1] ) {
			if ( weights[1] > weights[2] ) {
				w = weights[0]; weights[0] = weights[2]; weights[2] = w;
			}
			else if ( weights[0] > weights[2] ) {
				w = weights[0]; weights[0] = weights[1];
				weights[1] = weights[2]; weights[2] = w;
			}
			else {
				w = weights[0]; weights[0] = weights[1]; weights[1] = w;
			}
		}
		else if ( weights[0] > weights[2] ) {
			w = weights[0]; weights[0] = weights[2];
			weights[2] = weights[1]; weights[1] = w;
		}
		else if ( weights[1] > weights[2] ) {
			w = weights[1]; weights[1] = weights[2]; weights[2] = w;
		}
	}
	else if ( number == 2 ) {
		if ( weights[0] > weights[1] ) {
			w = weights[0]; weights[0] = weights[1]; weights[1] = w;
		}
	}
	return;
}

/*
  	#] SortWeights : 
*/
