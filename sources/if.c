/** @file if.c
 * 
 *  Routines for the dealing with if statements.
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
  	#[ Includes : if.c
*/

#include "form3.h"

/*
  	#] Includes : 
  	#[ If statement :
 		#[ Syntax :

		The `if' is a conglomerate of statements: if,else,endif

		The if consists in principle of:

		if ( number );
			statements
		else;
			statements
		endif;

		The first set is taken when number != 0.
		The else is not mandatory.
		TRUE = 1 and FALSE = 0

		The number can be built up via a logical expression:

					expr1 condition expr2

		each expression can be a subexpression again. It has to be
		enclosed in parentheses in that case.
		Conditions are:
			>, >=, <, <=, ==, !=, ||, &&

		When Expressions are chained evaluation is from left to right,
		independent of whether this indicates nonsense.
		if ( a || b || c || d ); is a perfectly normal statement.
		if ( a >= b || c == d ); would be messed up. This should be:
		if ( ( a >= b ) || ( c == d ) );

		The building blocks of the Expressions are:

			Match(option,pattern)	The number of times pattern fits in term_
			Count(....)				The count value of term_
			Coeff[icient]			The coefficient of term_
			FindLoop(options)		Are there loops (as in ReplaceLoop).

		Implementation for internal notation:

		TYPEIF,length,gotolevel(if fail),EXPRTYPE,length,......

		EXPRTYPE can be:
			SHORTNUMBER			->,4,sign,size
			LONGNUMBER			->,|ncoef+2|,ncoef,numer,denom
			MATCH				->,patternsiz+3,keyword,pattern
			MULTIPLEOF			->,3,thenumber
			COUNT				->,countsiz+2,countinfo
			TYPEFINDLOOP		->,7 (findloop info)
			COEFFICIENT			->,2
			IFDOLLAR			->,3,dollarnumber
			SUBEXPR				->,size,dummy,size1,EXPRTYPE,length,...
								  ,2,condition1,size2,...
								This is like functions.

		Note that there must be a restriction to the number of nestings
		of parentheses in an if statement. It has been set to 10.

		The syntax of match corresponds to the syntax of the left side
		of an id statement. The only difference is the keyword
		MATCH vs TYPEIDNEW.

 		#] Syntax : 
 		#[ GetIfDollarNum :
*/

WORD GetIfDollarNum(WORD *ifp, WORD *ifstop)
{
	DOLLARS d;
	WORD num, *w;
	if ( ifp[2] < 0 ) { return(-ifp[2]-1); }
	d = Dollars+ifp[2];
	if ( ifp+3 < ifstop && ifp[3] == IFDOLLAREXTRA ) {
		if ( d->nfactors == 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Attempt to use a factor of an unfactored $-variable");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		num = GetIfDollarNum(ifp+3,ifstop);
		if ( num > d->nfactors ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Dollar factor number %s out of range",num);
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		if ( num == 0 ) {
			return(d->nfactors);
		}
		w = d->factors[num-1].where;
		if ( w == 0 ) return(d->factors[num].value);
getnumber:;
		if ( *w == 0 ) return(0);
		if ( *w == 4 && w[3] == 3 && w[2] == 1 && w[1] < MAXPOSITIVE && w[4] == 0 ) {
			return(w[1]);
		}
		if ( ( w[w[0]] != 0 ) || ( ABS(w[w[0]-1]) != w[0]-1 ) ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Dollar factor number expected but found expression");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		else {
			MLOCK(ErrorMessageLock);
			MesPrint("Dollar factor number out of range");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		return(0);
	}
/*
	Now we have just a dollar and should evaluate that into a short number
*/
	if ( d->type == DOLZERO ) {
		return(0);
	}
	else if ( d->type == DOLNUMBER || d->type == DOLTERMS ) {
		w = d->where; goto getnumber;
	}
	else {
		MLOCK(ErrorMessageLock);
		MesPrint("Dollar factor number is wrong type");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
		return(0);
	}
}

/*
 		#] GetIfDollarNum : 
 		#[ FindVar :
*/

int FindVar(WORD *v, WORD *term)
{
	WORD *t, *tstop, *m, *mstop, *f, *fstop, *a, *astop;
	GETSTOP(term,tstop);
	t = term+1;
	while ( t < tstop ) {
		if ( *v == *t && *v < FUNCTION ) {	/* VECTOR, INDEX, SYMBOL, DOTPRODUCT */
			switch ( *v ) {
				case SYMBOL:
					m = t+2; mstop = t+t[1];
					while ( m < mstop ) {
						if ( *m == v[1] ) return(1);
						m += 2;
					}
					break;
				case INDEX:
				case VECTOR:
InVe:
					m = t+2; mstop = t+t[1];
					while ( m < mstop ) {
						if ( *m == v[1] ) return(1);
						m++;
					}
					break;
				case DOTPRODUCT:
					m = t+2; mstop = t+t[1];
					while ( m < mstop ) {
						if ( *m == v[1] && m[1] == v[2] ) return(1);
						if ( *m == v[2] && m[1] == v[1] ) return(1);
						m += 3;
					}
					break;
			}
		}
		else if ( *v == VECTOR && *t == INDEX ) goto InVe;
		else if ( *v == INDEX && *t == VECTOR ) goto InVe;
		else if ( ( *v == VECTOR || *v == INDEX ) && *t == DOTPRODUCT ) {
			m = t+2; mstop = t+t[1];
			while ( m < mstop ) {
				if ( v[1] == m[0] || v[1] == m[1] ) return(1);
				m += 3;
			}
		}
		else if ( *t >= FUNCTION ) {
			if ( *v == FUNCTION && v[1] == *t ) return(1);
			if ( functions[*t-FUNCTION].spec > 0 ) {
			  if ( *v == VECTOR || *v == INDEX ) { /* we need to check arguments */
				int i;
				for ( i = FUNHEAD; i < t[1]; i++ ) {
					if ( v[1] == t[i] ) return(1);
				}
			  }
			}
			else {
			  fstop = t + t[1]; f = t + FUNHEAD;
			  while ( f < fstop ) { /* Do the arguments one by one */
				if ( *f <= 0 ) {
					switch ( *f ) {
						case -SYMBOL:
							if ( *v == SYMBOL && v[1] == f[1] ) return(1);
							f += 2;
							break;
						case -VECTOR:
						case -MINVECTOR:
						case -INDEX:
							if ( ( *v == VECTOR || *v == INDEX )
							 && ( v[1] == f[1] ) ) return(1);
							f += 2;
							break;
						case -SNUMBER:
							f += 2;
							break;
						default:
							if ( *v == FUNCTION && v[1] == -*f && *f <= -FUNCTION ) return(1);
							if ( *f <= -FUNCTION ) f++;
							else f += 2;
							break;
					}
				}
				else {
					a = f + ARGHEAD; astop = f + *f;
					while ( a < astop ) {
						if ( FindVar(v,a) == 1 ) return(1);
						a += *a;
					}
					f = astop;
				}
			  }
			}
		}
		t += t[1];
	}
	return(0);
}

/*
 		#] FindVar : 
 		#[ DoIfStatement :				WORD DoIfStatement(PHEAD ifcode,term)

		The execution time part of the if-statement.
		The arguments are a pointer to the TYPEIF and a pointer to the term.
		The answer is either 1 (success) or 0 (fail).
		The calling routine can figure out where to go in case of failure
		by picking up gotolevel.
		Note that the whole setup asks for recursions.
*/

WORD DoIfStatement(PHEAD WORD *ifcode, WORD *term)
{
	GETBIDENTITY
	WORD *ifstop, *ifp;
	UWORD *coef1 = 0, *coef2, *coef3, *cc;
	WORD ncoef1, ncoef2, ncoef3, i = 0, first, *r, acoef, ismul1, ismul2, j;
	UWORD *Spac1, *Spac2;
	ifstop = ifcode + ifcode[1];
	ifp = ifcode + 3;
	if ( ifp >= ifstop ) return(1);
	if ( ( ifp + ifp[1] ) >= ifstop ) {
		switch ( *ifp ) {
			case LONGNUMBER:
				if ( ifp[2] ) return(1);
				else return(0);
			case MATCH:
			case TYPEIF:
				if ( HowMany(BHEAD ifp,term) ) return(1);
				else return(0);
			case TYPEFINDLOOP:
				if ( Lus(term,ifp[3],ifp[4],ifp[5],ifp[6],ifp[2]) ) return(1);
				else return(0);
			case TYPECOUNT:
				if ( CountDo(term,ifp) ) return(1);
				else return(0);
			case COEFFI:
			case MULTIPLEOF:
				return(1);
			case IFDOLLAR:
				{
					DOLLARS d = Dollars + ifp[2];
#ifdef WITHPTHREADS
					int nummodopt, dtype = -1;
					if ( AS.MultiThreaded ) {
						for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
							if ( ifp[2] == ModOptdollars[nummodopt].number ) break;
						}
						if ( nummodopt < NumModOptdollars ) {
							dtype = ModOptdollars[nummodopt].type;
							if ( dtype == MODLOCAL ) {
								d = ModOptdollars[nummodopt].dstruct+AT.identity;
							}
						}
					}
					dtype = d->type;
#else
					int dtype = d->type;  /* We use dtype to make the operation atomic */
#endif
					if ( dtype == DOLZERO ) return(0);
					if ( dtype == DOLUNDEFINED ) {
						if ( AC.UnsureDollarMode == 0 ) {
							MesPrint("$%s is undefined",AC.dollarnames->namebuffer+d->name);
							Terminate(-1);
						}
					}
				}
				return(1);
			case IFEXPRESSION:
				r = ifp+2; j = ifp[1] - 2;
				while ( --j >= 0 ) {
					if ( *r == AR.CurExpr ) return(1);
					r++;
				}
				return(0);
			case IFISFACTORIZED:
				r = ifp+2; j = ifp[1] - 2;
				if ( j == 0 ) {
					if ( ( Expressions[AR.CurExpr].vflags & ISFACTORIZED ) != 0 )
						return(1);
					else
						return(0);
				}
				while ( --j >= 0 ) {
					if ( ( Expressions[*r].vflags & ISFACTORIZED ) == 0 ) return(0);
					r++;
				}
				return(1);
			case IFOCCURS:
				{
					WORD *OccStop = ifp + ifp[1];
					ifp += 2;
					while ( ifp < OccStop ) {
						if ( FindVar(ifp,term) == 1 ) return(1);
						if ( *ifp == DOTPRODUCT ) ifp += 3;
						else ifp += 2;
					}
				}
				return(0);
			default:
/*
				Now we have a subexpression. Test first for one with a single item.
*/
				if ( ifp[3] == ( ifp[1] + 3 ) ) return(DoIfStatement(BHEAD ifp,term));
				ifstop = ifp + ifp[1];
				ifp += 3;
				break;
		}
	}
/*
	Here is the composite condition.
*/
	coef3 = NumberMalloc("DoIfStatement");
	Spac1 = NumberMalloc("DoIfStatement");
	Spac2 = (UWORD *)(TermMalloc("DoIfStatement"));
	ncoef1 = 0; first = 1; ismul1 = 0;
	do {
		if ( !first ) {
			ifp += 2;
			if ( ifp[-2] == ORCOND && ncoef1 ) {
				coef1 = Spac1;
				ncoef1 = 1; coef1[0] = coef1[1] = 1;
				goto SkipCond;
			}
			if ( ifp[-2] == ANDCOND && !ncoef1 ) goto SkipCond;
		}
		coef2 = Spac2;
		ncoef2 = 1;
		ismul2 = 0;
		switch ( *ifp ) {
			case LONGNUMBER:
				ncoef2 = ifp[2];
				j = 2*(ABS(ncoef2));
				cc = (UWORD *)(ifp + 3);
				for ( i = 0; i < j; i++ ) coef2[i] = cc[i];
				break;
			case MATCH:
			case TYPEIF:
				coef2[0] = HowMany(BHEAD ifp,term);
				coef2[1] = 1;
				if ( coef2[0] == 0 ) ncoef2 = 0;
				break;
			case TYPECOUNT:
				acoef = CountDo(term,ifp);
				coef2[0] = ABS(acoef);
				coef2[1] = 1;
				if ( acoef == 0 ) ncoef2 = 0;
				else if ( acoef < 0 ) ncoef2 = -1;
				break;
			case TYPEFINDLOOP:
				acoef = Lus(term,ifp[3],ifp[4],ifp[5],ifp[6],ifp[2]);
				coef2[0] = ABS(acoef);
				coef2[1] = 1;
				if ( acoef == 0 ) ncoef2 = 0;
				else if ( acoef < 0 ) ncoef2 = -1;
				break;
			case COEFFI:
				r = term + *term;
				ncoef2 = r[-1];
				i = ABS(ncoef2);
				cc = (UWORD *)(r - i);
				if ( ncoef2 < 0 ) ncoef2 = (ncoef2+1)>>1;
				else ncoef2 = (ncoef2-1)>>1;
				i--; for ( j = 0; j < i; j++ ) coef2[j] = cc[j];
				break;
			case SUBEXPR:
				ncoef2 = coef2[0] = DoIfStatement(BHEAD ifp,term);
				coef2[1] = 1;
				break;
			case MULTIPLEOF:
				ncoef2 = 1;
				coef2[0] = ifp[2];
				coef2[1] = 1;
				ismul2 = 1;
				break;
			case IFDOLLAREXTRA:
				break;
			case IFDOLLAR:
				{
/*
					We need to abstract a long rational in coef2
					with length ncoef2. What if that cannot be done?
*/
					DOLLARS d = Dollars + ifp[2];
#ifdef WITHPTHREADS
					int nummodopt, dtype = -1;
					if ( AS.MultiThreaded ) {
						for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
							if ( ifp[2] == ModOptdollars[nummodopt].number ) break;
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
/*
					We have to pick up the IFDOLLAREXTRA pieces for [1], [$y] etc.
*/
					if ( ifp+3 < ifstop && ifp[3] == IFDOLLAREXTRA ) {
						if ( d->nfactors == 0 ) {
							MLOCK(ErrorMessageLock);
							MesPrint("Attempt to use a factor of an unfactored $-variable");
							MUNLOCK(ErrorMessageLock);
							Terminate(-1);
						} {
						WORD num = GetIfDollarNum(ifp+3,ifstop);
						WORD *w;
						while ( ifp+3 < ifstop && ifp[3] == IFDOLLAREXTRA ) ifp += 3;
						if ( num > d->nfactors ) {
							MLOCK(ErrorMessageLock);
							MesPrint("Dollar factor number %s out of range",num);
							MUNLOCK(ErrorMessageLock);
							Terminate(-1);
						}
						if ( num == 0 ) {
							ncoef2 = 1; coef2[0] = d->nfactors; coef2[1] = 1;
							break;
						}
						w = d->factors[num-1].where;
						if ( w == 0 ) {
							if ( d->factors[num-1].value < 0 ) {
								ncoef2 = -1; coef2[0] = -d->factors[num-1].value; coef2[1] = 1;
							}
							else {
								ncoef2 = 1; coef2[0] = d->factors[num-1].value; coef2[1] = 1;
							}
							break;
						}
						if ( w[*w] == 0 ) {
							r = w + *w - 1;
							i = ABS(*r);
							if ( i == ( *w-1 ) ) {
								ncoef2 = (i-1)/2;
								if ( *r < 0 ) ncoef2 = -ncoef2;
								i--; cc = coef2; r = w + 1;
								while ( --i >= 0 ) *cc++ = (UWORD)(*r++);
								break;
							}
						}
						goto generic;
						}
					}
					else {
					  switch ( d->type ) {
						case DOLUNDEFINED:
							if ( AC.UnsureDollarMode == 0 ) {
#ifdef WITHPTHREADS
								if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
								MLOCK(ErrorMessageLock);
								MesPrint("$%s is undefined",AC.dollarnames->namebuffer+d->name);
								MUNLOCK(ErrorMessageLock);
								Terminate(-1);
							}
							ncoef2 = 0; coef2[0] = 0; coef2[1] = 1;
							break;
						case DOLZERO:
							ncoef2 = coef2[0] = 0; coef2[1] = 1;
							break;
						case DOLSUBTERM:
							if ( d->where[0] != INDEX || d->where[1] != 3
							|| d->where[2] < 0 || d->where[2] >= AM.OffsetIndex ) {
								if ( AC.UnsureDollarMode == 0 ) {
#ifdef WITHPTHREADS
									if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
									MLOCK(ErrorMessageLock);
									MesPrint("$%s is of wrong type",AC.dollarnames->namebuffer+d->name);
									MUNLOCK(ErrorMessageLock);
									Terminate(-1);
								}
								ncoef2 = 0; coef2[0] = 0; coef2[1] = 1;
								break;
							}
							d->index = d->where[2];
							/* fall through */
						case DOLINDEX:
							if ( d->index == 0 ) {
								ncoef2 = coef2[0] = 0; coef2[1] = 1;
							}
							else if ( d->index > 0 && d->index < AM.OffsetIndex ) {
								ncoef2 = 1; coef2[0] = d->index; coef2[1] = 1;
							}
							else if ( AC.UnsureDollarMode == 0 ) {
#ifdef WITHPTHREADS
								if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
								MLOCK(ErrorMessageLock);
								MesPrint("$%s is of wrong type",AC.dollarnames->namebuffer+d->name);
								MUNLOCK(ErrorMessageLock);
								Terminate(-1);
							}
							ncoef2 = coef2[0] = 0; coef2[1] = 1;
							break;
						case DOLWILDARGS:
							if ( d->where[0] <= -FUNCTION ||
							( d->where[0] < 0 && d->where[2] != 0 )
							|| ( d->where[0] > 0 && d->where[d->where[0]] != 0 )
							) {
								if ( AC.UnsureDollarMode == 0 ) {
#ifdef WITHPTHREADS
									if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
									MLOCK(ErrorMessageLock);
									MesPrint("$%s is of wrong type",AC.dollarnames->namebuffer+d->name);
									MUNLOCK(ErrorMessageLock);
									Terminate(-1);
								}
								ncoef2 = coef2[0] = 0; coef2[1] = 1;
								break;
							}
							/* fall through */
						case DOLARGUMENT:
							if ( d->where[0] == -SNUMBER ) {
								if ( d->where[1] == 0 ) {
									ncoef2 = coef2[0] = 0;
								}
								else if ( d->where[1] < 0 ) {
									ncoef2 = -1;
									coef2[0] = -d->where[1];
								}
								else {
									ncoef2 = 1;
									coef2[0] = d->where[1];
								}
								coef2[1] = 1;
							}
							else if ( d->where[0] == -INDEX
							&& d->where[1] >= 0 && d->where[1] < AM.OffsetIndex ) {
								if ( d->where[1] == 0 ) {
									ncoef2 = coef2[0] = 0; coef2[1] = 1;
								}
								else {
									ncoef2 = 1; coef2[0] = d->where[1];
									coef2[1] = 1;
								}
							}
							else if ( d->where[0] > 0
							&& d->where[ARGHEAD] == (d->where[0]-ARGHEAD)
							&& ABS(d->where[d->where[0]-1]) ==
										(d->where[0] - ARGHEAD-1) ) {
								i = d->where[d->where[0]-1];
								ncoef2 = (ABS(i)-1)/2;
								if ( i < 0 ) { ncoef2 = -ncoef2; i = -i; }
								i--; cc = coef2; r = d->where + ARGHEAD+1;
								while ( --i >= 0 ) *cc++ = (UWORD)(*r++);
							}
							else {
								if ( AC.UnsureDollarMode == 0 ) {
#ifdef WITHPTHREADS
									if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
									MLOCK(ErrorMessageLock);
									MesPrint("$%s is of wrong type",AC.dollarnames->namebuffer+d->name);
									MUNLOCK(ErrorMessageLock);
									Terminate(-1);
								}
								ncoef2 = 0; coef2[0] = 0; coef2[1] = 1;
							}
							break;
   						case DOLNUMBER:
						case DOLTERMS:
							if ( d->where[d->where[0]] == 0 ) {
								r = d->where + d->where[0]-1;
								i = ABS(*r);
								if ( i == ( d->where[0]-1 ) ) {
									ncoef2 = (i-1)/2;
									if ( *r < 0 ) ncoef2 = -ncoef2;
									i--; cc = coef2; r = d->where + 1;
									while ( --i >= 0 ) *cc++ = (UWORD)(*r++);
									break;
								}
							}
generic:;
							if ( AC.UnsureDollarMode == 0 ) {
#ifdef WITHPTHREADS
								if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
								MLOCK(ErrorMessageLock);
								MesPrint("$%s is of wrong type",AC.dollarnames->namebuffer+d->name);
								MUNLOCK(ErrorMessageLock);
								Terminate(-1);
							}
							ncoef2 = 0; coef2[0] = 0; coef2[1] = 1;
							break;
					  }
					}
#ifdef WITHPTHREADS
					if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
				}
				break;
			case IFEXPRESSION:
				r = ifp+2; j = ifp[1] - 2; ncoef2 = 0;
				while ( --j >= 0 ) {
					if ( *r == AR.CurExpr ) { ncoef2 = 1; break; }
					r++;
				}
				coef2[0] = ncoef2;
				coef2[1] = 1;
				break;
			case IFISFACTORIZED:
				r = ifp+2; j = ifp[1] - 2;
				if ( j == 0 ) {
					ncoef2 = 0;
					if ( ( Expressions[AR.CurExpr].vflags & ISFACTORIZED ) != 0 ) {
						ncoef2 = 1;
					}
				}
				else {
					ncoef2 = 1;
					while ( --j >= 0 ) {
						if ( ( Expressions[*r].vflags & ISFACTORIZED ) == 0 ) {
							ncoef2 = 0;
							break;
						}
						r++;
					}
				}
				coef2[0] = ncoef2;
				coef2[1] = 1;
				break;
			case IFOCCURS:
				{
					WORD *OccStop = ifp + ifp[1], *ifpp = ifp+2;
					ncoef2 = 0;
					while ( ifpp < OccStop ) {
						if ( FindVar(ifpp,term) == 1 ) {
							ncoef2 = 1; break;
						}
						if ( *ifpp == DOTPRODUCT ) ifp += 3;
						else ifpp += 2;
					}
					coef2[0] = ncoef2;
					coef2[1] = 1;
				}
				break;
			default:
				break;
		}
		if ( !first ) {
			if ( ifp[-2] != ORCOND && ifp[-2] != ANDCOND ) {
				if ( ( ifp[-2] == EQUAL || ifp[-2] == NOTEQUAL ) &&
				( ismul2 || ismul1 ) ) {
					if ( ismul1 && ismul2 ) {
						if ( coef1[0] == coef2[0] ) i = 1;
						else i = 0;
					}
					else {
						if ( ismul1 ) {
							if ( ncoef2 )
								Divvy(BHEAD coef2,&ncoef2,coef1,ncoef1);
							cc = coef2; ncoef3 = ncoef2;
						}
						else {
							if ( ncoef1 )
								Divvy(BHEAD coef1,&ncoef1,coef2,ncoef2);
							cc = coef1; ncoef3 = ncoef1;
						}
						if ( ncoef3 < 0 ) ncoef3 = -ncoef3;
						if ( ncoef3 == 0 ) {
							if ( ifp[-2] == EQUAL ) i = 1;
							else i = 0;
						}
						else if ( cc[ncoef3] != 1 ) {
							if ( ifp[-2] == EQUAL ) i = 0;
							else i = 1;
						}
						else {
							for ( j = 1; j < ncoef3; j++ ) {
								if ( cc[ncoef3+j] != 0 ) break;
							}
							if ( j < ncoef3 ) {
								if ( ifp[-2] == EQUAL ) i = 0;
								else i = 1;
							}
							else if ( ifp[-2] == EQUAL ) i = 1;
							else i = 0;
						}
					}
					goto donemul;
				}
				else if ( AddRat(BHEAD coef1,ncoef1,coef2,-ncoef2,coef3,&ncoef3) ) {
					NumberFree(coef3,"DoIfStatement"); NumberFree(Spac1,"DoIfStatement"); TermFree(Spac2,"DoIfStatement");
					MesCall("DoIfStatement"); return(-1);
				}
				switch ( ifp[-2] ) {
					case GREATER:
						if ( ncoef3 > 0 ) i = 1;
						else i = 0;
						break;
					case GREATEREQUAL:
						if ( ncoef3 >= 0 ) i = 1;
						else i = 0;
						break;
					case LESS:
						if ( ncoef3 < 0 ) i = 1;
						else i = 0;
						break;
					case LESSEQUAL:
						if ( ncoef3 <= 0 ) i = 1;
						else i = 0;
						break;
					case EQUAL:
						if ( ncoef3 == 0 ) i = 1;
						else i = 0;
						break;
					case NOTEQUAL:
						if ( ncoef3 != 0 ) i = 1;
						else i = 0;
						break;
				}
donemul:		if ( i ) { ncoef2 = 1; coef2 = Spac2; coef2[0] = coef2[1] = 1; }
				else ncoef2 = 0;
				ismul1 = ismul2 = 0;
			}
		}
		else {
			first = 0;
		}
		coef1 = Spac1;
		i = 2*ABS(ncoef2);
		for ( j = 0; j < i; j++ ) coef1[j] = coef2[j];
		ncoef1 = ncoef2;
SkipCond:
		ifp += ifp[1];
	} while ( ifp < ifstop );

	NumberFree(coef3,"DoIfStatement"); NumberFree(Spac1,"DoIfStatement"); TermFree(Spac2,"DoIfStatement");
	if ( ncoef1 ) return(1);
	else return(0);
}

/*
 		#] DoIfStatement : 
 		#[ HowMany :					WORD HowMany(ifcode,term)

		Returns the number of times that the pattern in ifcode
		can be taken out from term. There is a subkey in ifcode[2];
		The notation is identical to the lhs of an id statement.
		Most of the code comes from TestMatch.
*/

WORD HowMany(PHEAD WORD *ifcode, WORD *term)
{
	GETBIDENTITY
	WORD *m, *t, *r, *w, power, RetVal, i, topje, *newterm;
	WORD *OldWork, *ww, *mm;
	int *RepSto, RepVal;
	int numdollars = 0;
	m = ifcode + IDHEAD;
	AN.FullProto = m;
	AN.WildValue = w = m + SUBEXPSIZE;
	m += m[1];
	AN.WildStop = m;
	OldWork = AT.WorkPointer;
	if ( ( ifcode[4] & 1 ) != 0 ) {	/* We have at least one dollar in the pattern */
		AR.Eside = LHSIDEX;
		ww = AT.WorkPointer; i = m[0]; mm = m;
		NCOPY(ww,mm,i);
		*OldWork += 3;
		*ww++ = 1; *ww++ = 1; *ww++ = 3;
		AT.WorkPointer = ww;
		RepSto = AN.RepPoint;
		RepVal = *RepSto;
		NewSort(BHEAD0);
		if ( Generator(BHEAD OldWork,AR.Cnumlhs) ) {
			LowerSortLevel();
			*RepSto = RepVal;
			AN.RepPoint = RepSto;
			AT.WorkPointer = OldWork;
			return(-1);
		}
		AT.WorkPointer = ww;
		if ( EndSort(BHEAD ww,0) < 0 ) {}
		*RepSto = RepVal;
		AN.RepPoint = RepSto;
		if ( *ww == 0 || *(ww+*ww) != 0 ) {
			if ( AP.lhdollarerror == 0 ) {
				MLOCK(ErrorMessageLock);
				MesPrint("&LHS must be one term");
				MUNLOCK(ErrorMessageLock);
				AP.lhdollarerror = 1;
			}
			AT.WorkPointer = OldWork;
			return(-1);
		}
		m = ww; AT.WorkPointer = ww = m + *m;
		if ( m[*m-1] < 0 ) { m[*m-1] = -m[*m-1]; }
		*m -= m[*m-1];
		AR.Eside = RHSIDE;
	}
	else {
		ww = term + *term;
		if ( AT.WorkPointer < ww ) AT.WorkPointer = ww;
	}
	ClearWild(BHEAD0);
	while ( w < AN.WildStop ) {
		if ( *w == LOADDOLLAR ) numdollars++;
		w += w[1];
	}
	AN.RepFunNum = 0;
	AN.RepFunList = AT.WorkPointer;
    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
	topje = cbuf[AT.ebufnum].numrhs;
	if ( AT.WorkPointer >= AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	AN.DisOrderFlag = ifcode[2] & SUBDISORDER;
	switch ( ifcode[2] & (~SUBDISORDER) ) {
		case SUBONLY :
			/* Must be an exact match */
			AN.UseFindOnly = 1; AN.ForFindOnly = 0;
/*
		Copy the term first to scratchterm. This is needed
		because of the Substitute.
*/
			i = *term;
			t = term; newterm = r = AT.WorkPointer;
			NCOPY(r,t,i); AT.WorkPointer = r;
			RetVal = 0;
			if ( FindRest(BHEAD newterm,m) && ( AN.UsedOtherFind ||
				FindOnly(BHEAD newterm,m) ) ) {
				Substitute(BHEAD newterm,m,1);
				if ( numdollars ) {
					WildDollars(BHEAD (WORD *)0);
					numdollars = 0;
				}
				ClearWild(BHEAD0);
				RetVal = 1;
			}
			else RetVal = 0;
			break;
		case SUBMANY :
/*
		Copy the term first to scratchterm. This is needed
		because of the Substitute.
*/
			i = *term;
			t = term; newterm = r = AT.WorkPointer;
			NCOPY(r,t,i); AT.WorkPointer = r;
			RetVal = 0;
			AN.UseFindOnly = 0;
			if ( ( power = FindRest(BHEAD newterm,m) ) > 0 ) {
				if ( ( power = FindOnce(BHEAD newterm,m) ) > 0 ) {
					AN.UseFindOnly = 0;
					do {
						Substitute(BHEAD newterm,m,1);
						if ( numdollars ) {
							WildDollars(BHEAD (WORD *)0);
							numdollars = 0;
						}
						ClearWild(BHEAD0);
						RetVal++;
					} while ( FindRest(BHEAD newterm,m) && (
						AN.UsedOtherFind || FindOnce(BHEAD newterm,m) ) );
				}
				else if ( power < 0 ) {
					do {
						Substitute(BHEAD newterm,m,1);
						if ( numdollars ) {
							WildDollars(BHEAD (WORD *)0);
							numdollars = 0;
						}
						ClearWild(BHEAD0);
						RetVal++;
					} while ( FindRest(BHEAD newterm,m) );
				}
			}
			else if ( power < 0 ) {
				if ( FindOnce(BHEAD newterm,m) ) {
					do {
						Substitute(BHEAD newterm,m,1);
						if ( numdollars ) {
							WildDollars(BHEAD (WORD *)0);
							numdollars = 0;
						}
						ClearWild(BHEAD0);
					} while ( FindOnce(BHEAD newterm,m) );
					RetVal = 1;
				}
			}
			break;
		case SUBONCE :
/*
		Copy the term first to scratchterm. This is needed
		because of the Substitute.
*/
			i = *term;
			t = term; newterm = r = AT.WorkPointer;
			NCOPY(r,t,i); AT.WorkPointer = r;
			RetVal = 0;
			AN.UseFindOnly = 0;
			if ( FindRest(BHEAD newterm,m) && ( AN.UsedOtherFind || FindOnce(BHEAD newterm,m) ) ) {
				Substitute(BHEAD newterm,m,1);
				if ( numdollars ) {
					WildDollars(BHEAD (WORD *)0);
					numdollars = 0;
				}
				ClearWild(BHEAD0);
				RetVal = 1;
			}
			else RetVal = 0;
			break;
		case SUBMULTI :
			RetVal = FindMulti(BHEAD term,m);
			break;
		case SUBVECTOR :
			RetVal = 0;
			for ( i = 0; i < *term; i++ ) ww[i] = term[i];			
			while ( ( power = FindAll(BHEAD ww,m,AR.Cnumlhs,ifcode) ) != 0 ) { RetVal += power; }
			break;
		case SUBSELECT :
			ifcode += IDHEAD;	ifcode += ifcode[1];	ifcode += *ifcode;
			AN.UseFindOnly = 1; AN.ForFindOnly = ifcode;
			if ( FindRest(BHEAD term,m) && ( AN.UsedOtherFind ||
					FindOnly(BHEAD term,m) ) ) RetVal = 1;
			else RetVal = 0;
			break;
		default :
			RetVal = 0;
			break;
	}
	AT.WorkPointer = AN.RepFunList;
	cbuf[AT.ebufnum].numrhs = topje;
	return(RetVal);
}

/*
 		#] HowMany : 
 		#[ DoubleIfBuffers :
*/

VOID DoubleIfBuffers()
{
	int newmax, i;
	WORD *newsumcheck;
	LONG *newheap, *newifcount;
	if ( AC.MaxIf == 0 ) newmax = 10;
	else                newmax = 2*AC.MaxIf;
	newheap = (LONG *)Malloc1(sizeof(LONG)*(newmax+1),"IfHeap");
	newsumcheck = (WORD *)Malloc1(sizeof(WORD)*(newmax+1),"IfSumCheck");
	newifcount = (LONG *)Malloc1(sizeof(LONG)*(newmax+1),"IfCount");
	if ( AC.MaxIf ) {
		for ( i = 0; i < AC.MaxIf; i++ ) {
			newheap[i] = AC.IfHeap[i];
			newsumcheck[i] = AC.IfSumCheck[i];
			newifcount[i] = AC.IfCount[i];
		}
		AC.IfStack = (AC.IfStack-AC.IfHeap) + newheap;
		M_free(AC.IfHeap,"AC.IfHeap");
		M_free(AC.IfCount,"AC.IfCount");
		M_free(AC.IfSumCheck,"AC.IfSumCheck");
	}
	else {
		AC.IfStack = newheap;
	}
	AC.IfHeap = newheap;
	AC.IfSumCheck = newsumcheck;
	AC.IfCount = newifcount;
	AC.MaxIf = newmax;
}

/*
 		#] DoubleIfBuffers : 
  	#] If statement : 
  	#[ Switch statement :
 		#[ DoSwitch :
*/

int DoSwitch(PHEAD WORD *term, WORD *lhs)
{
/*
	For the moment we ignore the compiler buffer problems.
*/
	WORD numdollar = lhs[2];
	WORD ncase = DolToNumber(BHEAD numdollar);
	SWITCHTABLE *swtab = FindCase(lhs[3],ncase);
	return(Generator(BHEAD term,swtab->value));
}

/*
 		#] DoSwitch : 
 		#[ DoEndSwitch :
*/

int DoEndSwitch(PHEAD WORD *term, WORD *lhs)
{
	SWITCH *sw = AC.SwitchArray+lhs[2];
	return(Generator(BHEAD term,sw->endswitch.value+1));
}

/*
 		#] DoEndSwitch : 
 		#[ FindCase :
*/

SWITCHTABLE *FindCase(WORD nswitch, WORD ncase)
{
/*
	First find the switch table and determine how we have to search.
*/
	SWITCH *sw = AC.SwitchArray+nswitch;
	WORD hi, lo, med;
	if ( sw->typetable == DENSETABLE ) {
		med = ncase - sw->caseoffset;
		if ( med >= sw->numcases || med < 0 ) return(&sw->defaultcase);
	}
	else {
/*
		We need a binary search in the table.
*/
		if ( ncase > sw->maxcase || ncase < sw->mincase ) return(&sw->defaultcase);
		hi = sw->numcases-1; lo = 0;
		for(;;) {
			med = (hi+lo)/2;
			if ( ncase == sw->table[med].ncase ) break;
            else if ( ncase > sw->table[med].ncase ) {
				lo = med+1;
				if ( lo > hi ) return(&sw->defaultcase);
			}
			else {
				hi = med-1;
				if ( hi < lo ) return(&sw->defaultcase);
			}
		}
	}
	return(&sw->table[med]);
}

/*
 		#] FindCase : 
 		#[ DoubleSwitchBuffers :
*/

int DoubleSwitchBuffers()
{
	int newmax, i;
	SWITCH *newarray;
	WORD *newheap;
	if ( AC.MaxSwitch == 0 ) newmax = 10;
	else                     newmax = 2*AC.MaxSwitch;
	newarray = (SWITCH *)Malloc1(sizeof(SWITCH)*(newmax+1),"SwitchArray");
	newheap = (WORD *)Malloc1(sizeof(WORD)*(newmax+1),"SwitchHeap");
	if ( AC.MaxSwitch ) {
		for ( i = 0; i < AC.MaxSwitch; i++ ) {
			newarray[i] = AC.SwitchArray[i];
			newheap[i] = AC.SwitchHeap[i];
		}
		M_free(AC.SwitchHeap,"AC.SwitchHeap");
		M_free(AC.SwitchArray,"AC.SwitchArray");
	}
	for ( i = AC.MaxSwitch; i <= newmax; i++ ) {
		newarray[i].table = 0;
		newarray[i].tablesize = 0;
		newarray[i].defaultcase.ncase = 0;
		newarray[i].defaultcase.value = 0;
		newarray[i].defaultcase.compbuffer = 0;
		newarray[i].endswitch.ncase = 0;
		newarray[i].endswitch.value = 0;
		newarray[i].endswitch.compbuffer = 0;
		newarray[i].typetable = 0;
		newarray[i].mincase = 0;
		newarray[i].maxcase = 0;
		newarray[i].numcases = 0;
		newarray[i].caseoffset = 0;
		newarray[i].iflevel = 0;
		newarray[i].whilelevel = 0;
		newarray[i].nestingsum = 0;
		newheap[i] = 0;
	}
	AC.SwitchArray = newarray;
	AC.SwitchHeap = newheap;
	AC.MaxSwitch = newmax;
	return(0);
}

/*
 		#] DoubleSwitchBuffers : 
 		#[ SwitchSplitMerge :

		Sorts an array of WORDs. No adding of equal objects.
*/

VOID SwitchSplitMergeRec(SWITCHTABLE *array,WORD num,SWITCHTABLE *auxarray)
{
	WORD n1,n2,i,j,k;
	SWITCHTABLE *t1,*t2, t;
	if ( num < 2 ) return;
	if ( num == 2 ) {
		if ( array[0].ncase > array[1].ncase ) {
			t = array[0]; array[0] = array[1]; array[1] = t;
		}
		return;
	}
	n1 = num/2;
	n2 = num - n1;
	SwitchSplitMergeRec(array,n1,auxarray);
	SwitchSplitMergeRec(array+n1,n2,auxarray);
	if ( array[n1-1].ncase <= array[n1].ncase ) return;

	t1 = array; t2 = auxarray; i = n1; NCOPY(t2,t1,i);
	i = 0; j = n1; k = 0;
	while ( i < n1 && j < num ) {
		if ( auxarray[i].ncase <= array[j].ncase ) { array[k++] = auxarray[i++]; }
		else { array[k++] = array[j++]; }
	}
	while ( i < n1 ) array[k++] = auxarray[i++];
/*
	Remember: remnants of j are still in place!
*/
}

VOID SwitchSplitMerge(SWITCHTABLE *array,WORD num)
{
	SWITCHTABLE *auxarray = (SWITCHTABLE *)Malloc1(sizeof(SWITCHTABLE)*num/2,"SwitchSplitMerge");
	SwitchSplitMergeRec(array,num,auxarray);
	M_free(auxarray,"SwitchSplitMerge");
}

/*
 		#] SwitchSplitMerge : 
  	#] Switch statement : 
*/
