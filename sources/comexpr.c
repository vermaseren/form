/** @file comexpr.c
 * 
 *  Compiler routines for statements that involve algebraic expressions.
 *	These involve definitions, id-statements, the multiply statement
 *	and the fill statement.
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
  	#[ Includes : compi2.c

	File contains most of what has to do with compiling expressions.
	Main supporting file: token.c
*/

#include "form3.h"
 
static struct id_options {
	UBYTE *name;
	int code;
	int dummy;
} IdOptions[] = {
	 {(UBYTE *)"multi",      SUBMULTI    ,0}
	,{(UBYTE *)"many",       SUBMANY     ,0}
	,{(UBYTE *)"only",       SUBONLY     ,0}
	,{(UBYTE *)"once",       SUBONCE     ,0}
	,{(UBYTE *)"ifmatch",    SUBAFTER    ,0}
	,{(UBYTE *)"ifnomatch",  SUBAFTERNOT ,0}
	,{(UBYTE *)"ifnotmatch", SUBAFTERNOT ,0}
	,{(UBYTE *)"disorder",   SUBDISORDER ,0}
	,{(UBYTE *)"select",     SUBSELECT   ,0}
	,{(UBYTE *)"all",        SUBALL      ,0}
};

/*
  	#] Includes : 
  	#[ CoLocal :
*/

int CoLocal(UBYTE *inp) { return(DoExpr(inp,LOCALEXPRESSION,0)); }

/*
  	#] CoLocal : 
  	#[ CoGlobal :
*/

int CoGlobal(UBYTE *inp) { return(DoExpr(inp,GLOBALEXPRESSION,0)); }

/*
  	#] CoGlobal : 
  	#[ CoLocalFactorized :
*/

int CoLocalFactorized(UBYTE *inp) { return(DoExpr(inp,LOCALEXPRESSION,1)); }

/*
  	#] CoLocalFactorized : 
  	#[ CoGlobalFactorized :
*/

int CoGlobalFactorized(UBYTE *inp) { return(DoExpr(inp,GLOBALEXPRESSION,1)); }

/*
  	#] CoGlobalFactorized : 
  	#[ DoExpr:


*/

int DoExpr(UBYTE *inp, int type, int par)
{
	GETIDENTITY
	int error = 0;
	UBYTE *p, *q, c;
	WORD *w, i, j = 0, c1, c2, *OldWork = AT.WorkPointer, osize;
	WORD jold = 0;
	POSITION pos;
	while ( *inp == ',' ) inp++;
	if ( par ) AC.ToBeInFactors = 1;
	else       AC.ToBeInFactors = 0;
	p = inp;
	while ( *p && *p != '=' ) {
		if ( *p == '(' ) SKIPBRA4(p)
		else if ( *p == '{' ) SKIPBRA5(p)
		else if ( *p == '[' ) SKIPBRA1(p)
		else p++;
	}
	if ( *p ) {		/* Variety with the = sign */
		if ( ( q = SkipAName(inp) ) == 0 || q[-1] == '_' ) {
			MesPrint("&Illegal name for expression");
			error = 1;
			if ( q[-1] == '_' ) {
				while ( FG.cTable[*q] < 2 || *q == '_' ) q++;
			}
		}
		else {
			c = *q; *q = 0;
			if ( GetVar(inp,&c1,&c2,ALLVARIABLES,NOAUTO) != NAMENOTFOUND ) {
				if ( c1 == CEXPRESSION ) {
					if ( Expressions[c2].status == STOREDEXPRESSION ) {
						MesPrint("&Illegal attempt to overwrite a stored expression");
						error = 1;
					}
					else {
						HighWarning("Expression is replaced by new definition");
						if ( AO.OptimizeResult.nameofexpr != NULL &&
							StrCmp(inp,AO.OptimizeResult.nameofexpr) == 0 ) {
							ClearOptimize();
						}
						if ( Expressions[c2].status != DROPPEDEXPRESSION ) {
							w = &(Expressions[c2].status);
							if ( *w == LOCALEXPRESSION || *w == SKIPLEXPRESSION )
								*w = DROPLEXPRESSION;
							else if ( *w == GLOBALEXPRESSION || *w == SKIPGEXPRESSION )
								*w = DROPGEXPRESSION;
							else if ( *w == HIDDENLEXPRESSION )
								*w = DROPHLEXPRESSION;
							else if ( *w == HIDDENGEXPRESSION )
								*w = DROPHGEXPRESSION;
						}
						AC.TransEname = Expressions[c2].name;
						j = EntVar(CEXPRESSION,0,type,0,0,0);
						Expressions[j].node = Expressions[c2].node;
						Expressions[c2].replace = j;
					}
				}
				else {
					MesPrint("&name of expression is also name of a variable");
					error = 1;
					j = EntVar(CEXPRESSION,inp,type,0,0,0);
				}
				jold = c2;
			}
			else {
/*
				Here we have to worry about reuse of the expression in the
				same module. That will need AS.Oldvflags but that may not
				be defined or have the proper value.
*/
				j = EntVar(CEXPRESSION,inp,type,0,0,0);
				jold = j;
			}
			*q = c;
			OldWork = w = AT.WorkPointer;
			*w++ = TYPEEXPRESSION;
			*w++ = 3+SUBEXPSIZE;
			*w++ = j;
			AC.ProtoType = w;
			AR.CurExpr = j;				/* Block expression j */
			*w++ = SUBEXPRESSION;
			*w++ = SUBEXPSIZE;
			*w++ = j;
			*w++ = 1;
			*w++ = AC.cbufnum;
			FILLSUB(w)

			if ( c == '(' ) {
				while ( *q == ',' || *q == '(' ) {
					inp = q+1;
					if ( ( q = SkipAName(inp) ) == 0 ) {
						MesPrint("&Illegal name for expression argument");
						error = 1;
						q = p - 1;
						break;
					}
					c = *q; *q = 0;
					if ( GetVar(inp,&c1,&c2,ALLVARIABLES,WITHAUTO) < 0 ) c1 = -1;
					switch ( c1 ) {
						case CSYMBOL :
							*w++ = SYMTOSYM; *w++ = 4; *w++ = c2; *w++ = 0;
							break;
						case CINDEX :
							*w++ = INDTOIND; *w++ = 4;
							*w++ = c2 + AM.OffsetIndex; *w++ = 0;
							break;
						case CVECTOR :
							*w++ = VECTOVEC; *w++ = 4;
							*w++ = c2 + AM.OffsetVector; *w++ = 0;
							break;
						case CFUNCTION :
							*w++ = FUNTOFUN; *w++ = 4; *w++ = c2 + FUNCTION; *w++ = 0;
							break;
						default :
							MesPrint("&Illegal expression parameter: %s",inp);
							error = 1;
							break;
					}
					*q = c;
				}
				if ( *q != ')' || q+1 != p ) {
					MesPrint("&Illegal use of arguments for expression");
					error = 1;
				}
				AC.ProtoType[1] = w - AC.ProtoType;
			}
			else if ( c != '=' ) {
/*
				The dummy accepted L F := RHS;
*/
				MesPrint("&Illegal LHS for expression definition");
				error = 1;
			}
			*w++ = 1;
			*w++ = 1;
			*w++ = 3;
			*w++ = 0;
			SeekScratch(AR.outfile,&pos);
			Expressions[j].counter = 1; 
			Expressions[j].onfile = pos; 
			Expressions[j].whichbuffer = 0;
#ifdef PARALLELCODE
			Expressions[j].partodo = AC.inparallelflag; 
#endif
			OldWork[2] = w - OldWork - 3;
			AT.WorkPointer = w;
/*
			Writing the expression prototype to disk and to the compiler
			buffer is done only after the RHS has been compiled because
			we don't know the number of the main level RHS yet.
*/
		}
		inp = p+1;
		ClearWildcardNames();
		osize = AC.ProtoType[1]; AC.ProtoType[1] = SUBEXPSIZE;
		PutInVflags(jold);
		if ( ( i = CompileAlgebra(inp,RHSIDE,AC.ProtoType) ) < 0 ) {
			AC.ProtoType[1] = osize;
			error = 1;
		}
		else if ( error == 0 ) {
			AC.ProtoType[1] = osize;
			AC.ProtoType[2] = i;
			if ( PutOut(BHEAD OldWork+2,&pos,AR.outfile,0) < 0 ) {
				MesPrint("&Cannot create expression");
				error = -1;
			}
			else {
				Expressions[j].sizeprototype = OldWork[2];
				OldWork[2] = 4+SUBEXPSIZE;
				OldWork[4] = SUBEXPSIZE;
				OldWork[5] = i;
				OldWork[SUBEXPSIZE+3] = 1;
				OldWork[SUBEXPSIZE+4] = 1;
				OldWork[SUBEXPSIZE+5] = 3;
				OldWork[SUBEXPSIZE+6] = 0;
				if ( PutOut(BHEAD OldWork+2,&pos,AR.outfile,0) < 0
				|| FlushOut(&pos,AR.outfile,0) ) {
					MesPrint("&Cannot create expression");
					error = -1;
				}
				AR.outfile->POfull = AR.outfile->POfill;
			}
			OldWork[2] = j;
/*
			Seems unnecessary (13-feb-2018)

			AddNtoL(OldWork[1],OldWork);
*/
			AT.WorkPointer = OldWork;
			if ( AC.dumnumflag ) Add2Com(TYPEDETCURDUM)
		}
		AC.ToBeInFactors = 0;
	}
	else {	/* Variety in which expressions change property */
/*
			This code got a major revision because it didn't
			take hidden expressions into account. (1-jun-2010 JV)
*/
		do {
			if ( ( q = SkipAName(inp) ) == 0 ) {
				MesPrint("&Illegal name(s) for expression(s)");
				return(1);
			}
			c = *q; *q = 0;
			if ( GetName(AC.exprnames,inp,&c2,NOAUTO) == NAMENOTFOUND ) {
				MesPrint("&%s is not a valid expression",inp);
				error = 1;
			}
			else {
				w = &(Expressions[c2].status);
				if ( type == LOCALEXPRESSION ) {
					switch ( *w ) {
						case GLOBALEXPRESSION:
							*w = LOCALEXPRESSION;
							break;
						case SKIPGEXPRESSION:
							*w = SKIPLEXPRESSION;
							break;
						case DROPGEXPRESSION:
							*w = DROPLEXPRESSION;
							break;
						case HIDDENGEXPRESSION:
							*w = HIDDENLEXPRESSION;
							break;
						case HIDEGEXPRESSION:
							*w = HIDELEXPRESSION;
							break;
						case UNHIDEGEXPRESSION:
							*w = UNHIDELEXPRESSION;
							break;
						case INTOHIDEGEXPRESSION:
							*w = INTOHIDELEXPRESSION;
							break;
						case DROPHGEXPRESSION:
							*w = DROPHLEXPRESSION;
							break;
					}
				}
				else if ( type == GLOBALEXPRESSION ) {
					switch ( *w ) {
						case LOCALEXPRESSION:
							*w = GLOBALEXPRESSION;
							break;
						case SKIPLEXPRESSION:
							*w = SKIPGEXPRESSION;
							break;
						case DROPLEXPRESSION:
							*w = DROPGEXPRESSION;
							break;
						case HIDDENLEXPRESSION:
							*w = HIDDENGEXPRESSION;
							break;
						case HIDELEXPRESSION:
							*w = HIDEGEXPRESSION;
							break;
						case UNHIDELEXPRESSION:
							*w = UNHIDEGEXPRESSION;
							break;
						case INTOHIDELEXPRESSION:
							*w = INTOHIDEGEXPRESSION;
							break;
						case DROPHLEXPRESSION:
							*w = DROPHGEXPRESSION;
							break;
					}
				}
/*
				old code
				if ( type != LOCALEXPRESSION || *w != STOREDEXPRESSION )
						*w = type;
*/
			}
			*q = c; inp = q+1;
		} while ( c == ',' );
		if ( c ) {
			MesPrint("&Illegal object in local or global redefinition");
			error = 1;
		}
	}
	return(error);
}

/*
  	#] DoExpr: 
  	#[ CoIdOld :
*/

int CoIdOld(UBYTE *inp)
{
	AC.idoption = 0;
	return(CoIdExpression(inp,TYPEIDOLD));
}

/*
  	#] CoIdOld : 
  	#[ CoId :
*/

int CoId(UBYTE *inp)
{
	AC.idoption = 0;
	return(CoIdExpression(inp,TYPEIDNEW));
}

/*
  	#] CoId : 
  	#[ CoIdNew :
*/

int CoIdNew(UBYTE *inp)
{
	AC.idoption = 0;
	return(CoIdExpression(inp,TYPEIDNEW));
}

/*
  	#] CoIdNew : 
  	#[ CoDisorder :
*/

int CoDisorder(UBYTE *inp)
{
	AC.idoption = SUBDISORDER;
	return(CoIdExpression(inp,TYPEIDNEW));
}

/*
  	#] CoDisorder : 
  	#[ CoMany :
*/

int CoMany(UBYTE *inp)
{
	AC.idoption = SUBMANY;
	return(CoIdExpression(inp,TYPEIDNEW));
}

/*
  	#] CoMany : 
  	#[ CoMulti :
*/

int CoMulti(UBYTE *inp)
{
	AC.idoption = SUBMULTI;
	return(CoIdExpression(inp,TYPEIDNEW));
}

/*
  	#] CoMulti : 
  	#[ CoIfMatch :
*/

int CoIfMatch(UBYTE *inp)
{
	AC.idoption = SUBAFTER;
	return(CoIdExpression(inp,TYPEIDNEW));
}

/*
  	#] CoIfMatch : 
  	#[ CoIfNoMatch :
*/

int CoIfNoMatch(UBYTE *inp)
{
	AC.idoption = SUBAFTERNOT;
	return(CoIdExpression(inp,TYPEIDNEW));
}

/*
  	#] CoIfNoMatch : 
  	#[ CoOnce :
*/

int CoOnce(UBYTE *inp)
{
	AC.idoption = SUBONCE;
	return(CoIdExpression(inp,TYPEIDNEW));
}

/*
  	#] CoOnce : 
  	#[ CoOnly :
*/

int CoOnly(UBYTE *inp)
{
	AC.idoption = SUBONLY;
	return(CoIdExpression(inp,TYPEIDNEW));
}

/*
  	#] CoOnly : 
  	#[ CoSelect :
*/

int CoSelect(UBYTE *inp)
{
	AC.idoption = SUBSELECT;
	return(CoIdExpression(inp,TYPEIDNEW));
}

/*
  	#] CoSelect : 
  	#[ CoIdExpression :

	First finish dealing with secondary keywords
*/

int CoIdExpression(UBYTE *inp, int type)
{
	GETIDENTITY
	int i, j, idhead, error = 0, MinusSign = 0, opt, retcode;
	WORD *w, *s, *m, *mm, *ww, *FirstWork, *OldWork, c1, numsets = 0,
		 oldnumrhs, *ow, oldEside;
	UBYTE *p, *pp, c;
	CBUF *C = cbuf+AC.cbufnum;
	LONG oldcpointer, x;
	FirstWork = OldWork = AT.WorkPointer;
/*
	Don't forget to change in StudyPattern if we change/add_to the
	following setup. 
	if ( type == TYPEIF ) idhead = IDHEAD-1;
	else                  
*/
	idhead = IDHEAD;
	AR.CurExpr = -1;
	w = AT.WorkPointer;
	*w++ = type;
	*w++ = idhead + SUBEXPSIZE;
	w++;
	if ( idhead >= IDHEAD ) *w++ = -1;
#if IDHEAD > 4
	for ( i = 4; i < idhead; i++ ) *w++ = 0;
#endif
	while ( *inp == ',' ) inp++;
	p = inp;
	if ( AC.idoption == SUBSELECT ) {
		p--;
		goto findsets;
	}
	else if ( ( AC.idoption == SUBAFTER ) || ( AC.idoption == SUBAFTERNOT ) ) {
		while ( *p && *p != '=' && *p != ',' ) {
			if ( *p == '(' ) SKIPBRA4(p)
			else if ( *p == '{' ) SKIPBRA5(p)
			else if ( *p == '[' ) SKIPBRA1(p)
			else p++;
		}
		if ( *p == '=' || *inp != '-' || inp[1] != '>' ) {
			MesPrint("&Illegal use if if[no]match in id statement");
			error = 1; goto AllDone;
		}
		if ( *p == 0 ) {
			MesPrint("&id-statement without = sign");
			error = 1; goto AllDone;
		}
		inp += 2; pp = inp;
		goto readlabel;
	}
	for(;;) {
		while ( *p && *p != '=' && *p != ',' ) {
			if ( *p == '(' ) SKIPBRA4(p)
			else if ( *p == '{' ) SKIPBRA5(p)
			else if ( *p == '[' ) SKIPBRA1(p)
			else p++;
		}
		if ( *p == '=' ) break;
		if ( *p == 0 ) {
			MesPrint("&id-statement without = sign");
			error = 1; goto AllDone;
		}
/*
		We have either a secondary option or a syntax error
*/
		pp = inp;
		while ( FG.cTable[*pp] == 0 ) pp++;
		c = *pp; *pp = 0;
		i = sizeof(IdOptions)/sizeof(struct id_options);
		while ( --i >= 0 ) {
			if ( StrICmp(inp,IdOptions[i].name) == 0 ) break;
		}
		if ( i < 0 ) {
			MesPrint("&Illegal option %s in id-statement",inp);
			*pp = c; error = 1; p++; inp = p; continue;
		}
		opt = IdOptions[i].code;
		*pp = c;
		inp = pp+1;
		switch ( opt ) {
			case SUBDISORDER:
				if ( pp != p ) goto IllField;
				AC.idoption |= SUBDISORDER;
				p++; inp = p;
				break;
			case SUBSELECT:
				if ( p != pp ) goto IllField;
				if ( ( AC.idoption & SUBMASK ) != 0 ) {
					if ( AC.idoption == SUBMULTI && type == TYPEIF ) {}
					else {
						MesPrint("&Conflicting options in id-statement");
						error = 1;
					}
				}
findsets:;
/*
				Now we read the sets
*/
				numsets = 0;
				for(;;) {
					inp = ++p;
					while ( *p && *p != '=' && *p != ',' ) {
						if ( *p == '(' ) SKIPBRA4(p)
						else if ( *p == '{' ) SKIPBRA5(p)
						else if ( *p == '[' ) SKIPBRA1(p)
						else p++;
					}
					if ( *p == '=' ) break;
					if ( *p == 0 ) {
						MesPrint("&id-statement without = sign");
						error = 1; goto AllDone;
					}
/*
					We have a set at inp.
*/
					if ( *inp == '{' ) {
						if ( p[-1] != '}' ) {
							c = *p; *p = 0;
							MesPrint("&Illegal temporary set: %s",inp);
							error = 1; *p = c;
						}
						else {
							inp++;
							c = p[-1]; p[-1] = 0;
							c1 = DoTempSet(inp,p-1);
							*w++ = c1;
							p[-1] = c;
							numsets++;
							if ( w[-1] < 0 ) error = 1;
						}
					}
					else {
						c = *p; *p = 0;
						if ( GetName(AC.varnames,inp,&c1,NOAUTO) != CSET ) {
							MesPrint("&%s is not a set",inp);
							error = 1;
						}
						else {
							if ( c1 < AM.NumFixedSets ) {
								MesPrint("&Built in sets are not allowed in the select option");
								error = 1;
							}
							else if ( Sets[c1].type == CRANGE ) {
								MesPrint("&Ranged sets are not allowed in the select option");
								error = 1;
							}
							numsets++;
							*w++ = c1;
						}
						*p = c;
					}
				}
/*
				Now exchange the positions a bit.
				Regular stuff at OldWork, numsets sets at FirstWork[idhead]
*/
				OldWork = w;
				for ( i = 0; i < idhead; i++ ) *w++ = FirstWork[i];
				AC.idoption = SUBSELECT;
				break;
			case SUBAFTER:
			case SUBAFTERNOT:
				if ( type == TYPEIF ) {
					MesPrint("&The if[no]match->label option is not allowed in an if statement");
					error = 1; goto AllDone;
				}
				if ( pp[0] != '-' || pp[1] != '>' ) goto IllField;
				pp += 2;	/* points now at the label */
				inp = pp;
				AC.idoption |= opt;
readlabel:
				while ( FG.cTable[*pp] <= 1 ) pp++;
				if ( pp != p ) {
					c = *p; *p = 0;
					MesPrint("&Illegal label %s in if[no]match option of id-statement",inp);
					*p = c; error = 1; inp = p+1; continue;
				}
				c = *p; *p = 0;
				OldWork[3] = GetLabel(inp);
				*p++ = c; inp = p;
				break;
			case SUBALL:
				x = 0;
				if ( *pp == '(' ) {
					if ( FG.cTable[*inp] == 1 ) {
						while ( *inp >= '0' && *inp <= '9' ) x = 10*x+*inp++ - '0';
					}
					else {
						pp++;
						while ( FG.cTable[*inp] == 0 ) inp++;
						c = *inp; *inp = 0;
						if ( StrICont(pp,(UBYTE *)"normalize") != 0 ) goto IllOpt;
						*inp = c;
						OldWork[4] |= NORMALIZEFLAG;
					}
					if ( *inp != ')' || inp+1 != p ) {
						c = *inp; *inp = 0;
IllOpt:
						MesPrint("&Illegal ALL option in id-statement: ",pp);
						*inp++ = c;
						error = 1;
						continue;
					}
					pp = inp;
					inp = pp+1;
				}
/*
				Note that the following statement limits x to 
*/
				if ( x > MAXPOSITIVE ) {
					MesPrint("&Requested maximum number of matches %l in ALL option in id-statement is greater than %l ",x,MAXPOSITIVE);
					error = 1;
				}
				OldWork[5] = x;
				if ( type != TYPEIDNEW ) {
				  if ( type == TYPEIDOLD ) {
					MesPrint("&Requested ALL option not allowed in idold/also statement.");
					error = 1;
				  }
				  else if ( type == TYPEIF ) {
					MesPrint("&Requested ALL option not allowed in if(match())");
					error = 1;
				  }
				  else {
					MesPrint("&ALL option only allowed in regular id-statement.");
					error = 1;
				  }
				}
				p++; inp = p;
				AC.idoption = opt;
				break;
			default:
				if ( pp != p ) {
IllField:			c = *p; *p = 0;
					MesPrint("&Illegal optionfield %s in id-statement",inp);
					*p = c; error = 1; inp = p+1; continue;
				}
				i = AC.idoption & SUBMASK;
				if ( i && i != opt ) {
					MesPrint("&Conflicting options in id-statement");
					error = 1;  continue;
				}
				else AC.idoption |= opt;
				while ( *p == ',' ) p++;
				inp = p;
				break;
		}
	}
	if ( ( AC.idoption & SUBMASK ) == 0 ) AC.idoption |= SUBMULTI;
	OldWork[2] = AC.idoption;
/*
	Now we have a field till the = sign
	Now the subexpression prototype
*/
	AC.ProtoType = w;
	*w++ = SUBEXPRESSION;
	*w++ = SUBEXPSIZE;
	*w++ = C->numrhs+1;
	*w++ = 1;
	*w++ = AC.cbufnum;
	FILLSUB(w)
	AC.WildC = w;
	AC.NwildC = 0;
	AT.WorkPointer = s = w + 4*AM.MaxWildcards + 8;
/*
	Now read the LHS
*/
	ClearWildcardNames();
	oldcpointer = AddLHS(AC.cbufnum) - C->Buffer;

	*p = 0;
	oldnumrhs = C->numrhs;
	if ( ( retcode = CompileAlgebra(inp,LHSIDE,AC.ProtoType) ) < 0 ) { error = 1; }
	else AC.ProtoType[2] = retcode;
	*p = '='; inp = p+1;
	AT.WorkPointer = s;
	if ( AC.NwildC && SortWild(w,AC.NwildC) ) error = 1;

		/* Make the LHS pointers ready */

	OldWork[1] = AC.WildC-OldWork;
	OldWork[idhead+1] = OldWork[1] - idhead;
	w = AC.WildC;
	AT.WorkPointer = w;
	s = C->rhs[C->numrhs];
/*
	Now check whether wildcards get converted to dollars (for PARALLEL)
*/
	{
		WORD *tw, *twstop;
		tw = AC.ProtoType; twstop = tw + tw[1]; tw += SUBEXPSIZE;
		while ( tw < twstop ) {
			if ( *tw == LOADDOLLAR ) {
				AddPotModdollar(tw[2]);
			}
			tw += tw[1];
		}
	}
/*
	We have the expression in the compiler buffers.
	The main level is at lhs[numlhs]
	The partial lhs (including ProtoType) is in OldWork (in WorkSpace)
	We need to load the result at w after the prototype
	Because these sort routines don't use the WorkSpace
	there should not be a conflict
*/
	if ( !error && *s == 0 ) {
IllLeft:MesPrint("&Illegal LHS");
		AC.lhdollarflag = 0;
		return(1);
	}
	if ( !error && *(s+*s) != 0 ) {
		MesPrint("&LHS should be one term only");
		return(1);
	}
	if ( error == 0 ) {
		WORD oldpolyfun = AR.PolyFun;
		if ( NewSort(BHEAD0) || NewSort(BHEAD0) ) {
			if ( !error ) error = 1;
			return(error);
		}
		AN.RepPoint = AT.RepCount + 1;
        ow = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
		mm = s; ww = ow; i = *mm;
		while ( --i >= 0 ) *ww++ = *mm++; AT.WorkPointer = ww;
		AC.lhdollarflag = 0; oldEside = AR.Eside; AR.Eside = LHSIDE;
		AR.Cnumlhs = C->numlhs;
		AR.PolyFun = 0;
		if ( Generator(BHEAD ow,C->numlhs) ) {
			AR.Eside = oldEside;
			LowerSortLevel(); LowerSortLevel(); AR.PolyFun = oldpolyfun; goto IllLeft;
		}
		AR.Eside = oldEside;
		AT.WorkPointer = w;
		if ( EndSort(BHEAD w,0) < 0 ) { LowerSortLevel(); AR.PolyFun = oldpolyfun; goto IllLeft; }
		AR.PolyFun = oldpolyfun;
		if ( *w == 0 || *(w+*w) != 0 ) {
			MesPrint("&LHS must be one term");
			AC.lhdollarflag = 0;
			return(1);
		}
		LowerSortLevel();
		if ( AC.lhdollarflag ) MarkDirty(w,DIRTYFLAG);
	}
	AT.WorkPointer = w + *w;
	AC.DumNum = 0;
/*
	Everything is now after OldWork. We can pop the compilerbuffer.
	Next test for illegal things like a coefficient
	At this point we have:
	w = the term of the LHS
*/
	C->Pointer = C->Buffer + oldcpointer;
	C->numrhs = oldnumrhs;
	C->numlhs--;

	m = w + *w - 3;
	AC.vectorlikeLHS = 0;
	if ( !error ) {
	  if ( m[2] != 3 || m[1] != 1 || *m != 1 ) {
		if ( *m == 1 && m[1] == 1 && m[2] == -3 ) {
			MinusSign = 1;
		}
		else {
			MesPrint("&Coefficient in LHS");
			error = 1;
			AC.DumNum = 0;
			*w -= ABS(m[2])-3;
		}
	  }
	  if ( *w == 7 && w[1] == INDEX && w[3] < 0 ) {
		if ( ( AC.idoption & SUBMASK ) != 0 &&  ( AC.idoption & SUBMASK ) !=
		SUBMULTI ) {
			MesPrint("&Illegal option for substitution of a vector");
			error = 1;
		}
		AC.DumNum = AM.IndDum;
		OldWork[2] = ( OldWork[2] - ( OldWork[2] & SUBMASK ) ) | SUBVECTOR;
		c1 = w[3];
			/* We overwrite the LHS */
		*w++ = INDTOIND;
		*w++ = 4;
		*w++ = AC.DumNum + WILDOFFSET;
		*w++ = 0;
		w[0] = 5;
		w[1] = VECTOR;
		w[2] = 4;
		w[3] = c1;
		w[4] = AC.DumNum + WILDOFFSET;
		OldWork[idhead+1] = w - OldWork - idhead;
		AC.vectorlikeLHS = 1;
	  }
	  else {
		AC.DumNum = 0;
		*w -= 3;
		i = OldWork[2] & SUBMASK;
		m = w + *w;
		if ( i == 0 || i == SUBMULTI ) {
			s = w+1;
			while ( s < m ) {
				if ( *s == SYMBOL ) {
					j = s[1]/2; s += 2;
					while ( --j >= 0 ) {
						if ( ABS(s[1]) > 2*MAXPOWER ) {
							OldWork[2] = ( OldWork[2] - i ) | SUBONCE;
							break;
						}
						s += 2;
					}
					if ( j >= 0 ) break;
				}
				else if ( *s == DOTPRODUCT ) {
					j = s[1]/3; s += 2;
					while ( --j >= 0 ) {
						if ( ABS(s[2]) > 2*MAXPOWER ) {
							OldWork[2] = ( OldWork[2] - i ) | SUBONCE;
							break;
						}
						else if ( s[1] >= -(2*WILDOFFSET) || s[0] >= -(2*WILDOFFSET) ) {
							OldWork[2] = ( OldWork[2] - i ) | SUBMANY;
							i = SUBMANY;
						}
						s += 3;
					}
					if ( j >= 0 ) break;
				}
				else {
					OldWork[2] = ( OldWork[2] - i ) | SUBMANY;
					break;
				}
			}
		}
		if ( ( OldWork[2] & SUBMASK ) == 0 ) OldWork[2] |= SUBMULTI;
	  }
	  if ( ( OldWork[2] & SUBMASK ) == SUBSELECT ) {
/*
		Paste the SETSET information after the pattern.
		Important note: We will still get function information for the
		smart patternmatching after it. To distinguish them we need to have
		that SETSET != m*n+1 in which m is the number of words per function
		and n the number of functions. Currently (29-may-1997) m = 4.
*/
		*m++ = SETSET;
		*m++ = numsets+2;
		s = FirstWork + idhead;
		while ( --numsets >= 0 ) *m++ = *s++;
	  }
	  else {
		m = w + *w;
	  }
	}
/*
	We keep the whole thing in OldWork for the moment.
	We still have to add the number of the RHS expression.
	There is also some opportunity now to be smart about the pattern.
	This is needed for complicated wildcarding with symmetric functions.
	We do this in a special routine during compile time to make sure
	that we loose as little time as possible (during running) if there
	is no need to be smart.
*/
	*m++ = 0;
	OldWork[1] = m - OldWork;
	AC.ProtoType = OldWork+idhead;
	if ( !error ) {
		if ( StudyPattern(OldWork) ) error = 1;
	}
	AT.WorkPointer = OldWork + OldWork[1];
	if ( AC.lhdollarflag ) OldWork[4] |= DOLLARFLAG;
	AC.lhdollarflag = 0;
/*
	Test whether the id/idold configuration is fine.
*/
	if ( type == TYPEIDOLD ) {
		WORD ci = C->numlhs;
		while ( ci >= 1 ) {
			if ( C->lhs[ci][0] == TYPEIDNEW ) {
				if ( (C->lhs[ci][2] & SUBMASK) == SUBALL ) {
					MesPrint("&Idold/also cannot follow an id,all statement.");
					error = 1;
				}
				break;
			}
			else if ( C->lhs[ci][0] == TYPEDETCURDUM ) { ci--; continue; }
			else if ( C->lhs[ci][0] == TYPEIDOLD ) { ci--; continue; }
			else ci = 0;
		}
		if ( ci < 1 ) {
			MesPrint("&Idold/also should follow an id/idnew statement.");
			error = 1;
		}
	}
/*
	Now the right hand side.
*/
	if ( type != TYPEIF ) {
		if ( ( retcode = CompileAlgebra(inp,RHSIDE,AC.ProtoType) ) < 0 ) error = 1;
		else {
			AC.ProtoType[2] = retcode;
			AC.DumNum = 0;
			if ( MinusSign ) {	/* Flip the sign of the RHS */
				w = C->rhs[retcode];
				while ( *w ) { w += *w; w[-1] = -w[-1]; }
			}
			if ( AC.dumnumflag ) Add2Com(TYPEDETCURDUM)
		}
	}
/*
	Actual adding happens only now after numrhs insertion
*/
	if ( !error ) { AddNtoL(OldWork[1],OldWork); }
AllDone:
	AC.lhdollarflag = 0;
	AT.WorkPointer = FirstWork;
	return(error);
}

/*
  	#] CoIdExpression : 
  	#[ CoMultiply :
*/

static WORD mularray[13] = { TYPEMULT, SUBEXPSIZE+3, 0, SUBEXPRESSION,
		SUBEXPSIZE, 0, 1, 0, 0, 0, 0, 0, 0 };

int CoMultiply(UBYTE *inp)
{
	UBYTE *p;
	int error = 0, RetCode;
	mularray[2] = 0;		/* right multiply is default */
	while ( *inp == ',' ) inp++;
/*	if ( inp[-1] == '-' || inp[-1] == '+' ) inp--; */
	p = SkipField(inp,0);
	if ( *p ) {
		*p = 0;
		if ( StrICont(inp,(UBYTE *)"left") == 0 )       mularray[2] = 1;
		else if ( StrICont(inp,(UBYTE *)"right") == 0 ) mularray[2] = 0;
		else {
			MesPrint("&Illegal option in multiply statement or ; forgotten.");
			return(1);
		}
		*p = ',';
		inp = p + 1;
	}
	ClearWildcardNames();
	while ( *inp == ',' ) inp++;
	AC.ProtoType = mularray+3;
	mularray[7] = AC.cbufnum;
	if ( ( RetCode = CompileAlgebra(inp,RHSIDE,AC.ProtoType) ) < 0 ) error = 1;
	else {
		mularray[5] = RetCode;
		AddNtoL(SUBEXPSIZE+3,mularray);
		if ( AC.dumnumflag ) Add2Com(TYPEDETCURDUM)
	}
	return(error);
}

/*
  	#] CoMultiply : 
  	#[ CoFill :

	Special additions for tablebase-like tables added 12-aug-2002
*/

int CoFill(UBYTE *inp)
{
	GETIDENTITY
	WORD error = 0, x, funnum, type, *oldwp = AT.WorkPointer;
	int i, oldcbufnum = AC.cbufnum, nofill = 0, numover, redef = 0;
	WORD *w, *wold, *Tprototype;
	UBYTE *p = inp, c, *inp1;
	TABLES T = 0, oldT;
	LONG newreservation, sum = 0;
	UBYTE *p1, *p2, *p3, *p4, *fake = 0;
	int tablestub = 0;
	if ( AC.exprfillwarning == 1 ) AC.exprfillwarning = 0;
/*
	Read the name of the function and test that it is in the table.
*/
	p1 = inp;
	if ( ( p = SkipAName(inp) ) == 0 ) return(1);
	p2 = p;
	c = *p; *p = 0;
	if ( ( GetVar(inp,&type,&funnum,CFUNCTION,WITHAUTO) == NAMENOTFOUND )
	|| ( T = functions[funnum].tabl ) == 0 || ( T->numind > 0 && c != '(' ) ) {
		MesPrint("&%s should be a table with argument(s)",inp);
		*p = c; return(1);
	}
	oldT = T;
	*p++ = c;
	if ( T->numind == 0 ) {
		if ( c == '(' ) {
			if ( *p != ')' ) {
				c = *p; *p = 0;
				MesPrint("&%s should be a table without arguments",inp);
				*p = c; return(1);
			}
			else { p++; }
		}
		else { p--; }
		sum = 0;
		p3 = p;
		goto andagain;
	}
	for ( sum = 0, i = 0, w = oldwp; i < T->numind; i++ ) {
		ParseSignedNumber(x,p);
		if ( FG.cTable[p[-1]] != 1 || ( *p != ',' && *p != ')' ) ) {
			MesPrint("&Table arguments in fill statement should be numbers");
			return(1);
		}
		if ( T->sparse ) *w++ = x;
		else if ( x < T->mm[i].mini || x > T->mm[i].maxi ) {
			MesPrint("&Value %d for argument %d of table out of bounds",x,i+1);
			error = 1; nofill = 1;
		}
		else sum += ( x - T->mm[i].mini ) * T->mm[i].size;
		if ( *p == ')' ) break;
		p++;
	}
	p3 = p;
	if ( *p != ')' || i < ( T->numind - 1 ) ) {
		MesPrint("&Incorrect number of table arguments in fill statement. Should be %d"
		,T->numind);
		error = 1; nofill = 1;
	}
	AT.WorkPointer = w;
	if ( T->sparse == 0 ) sum *= TABLEEXTENSION;
andagain:;
	AC.cbufnum = T->bufnum;
	if ( T->sparse ) {
		i = FindTableTree(T,oldwp,1);
		if ( i >= 0 ) {
			sum = i + T->numind;
			if ( tablestub == 0 && ( ( T->sparse & 2 ) == 2 ) && ( T->mode != 0 )
			&& ( AC.vetotablebasefill == 0 ) ) {
/*
				This redefinition does not need a new stub
*/
				functions[funnum].tabl = T = T->spare;
				tablestub = 1;
				goto andagain;
			}
			redef = 1;
			goto redef;
		}
		if ( T->totind >= T->reserved ) {
			if ( T->reserved == 0 ) newreservation = 20;
			else newreservation = T->reserved;
/*
			while ( T->totind >= newreservation && newreservation <
					MAXTABLECOMBUF*(T->numind+TABLEEXTENSION) )
			if ( newreservation > MAXTABLECOMBUF*T->numind ) newreservation =
					5*(T->numind+TABLEEXTENSION);
*/
			while ( T->totind >= newreservation && newreservation < MAXTABLECOMBUF )
					newreservation = 2*newreservation;
			if ( newreservation > MAXTABLECOMBUF ) newreservation = MAXTABLECOMBUF;
			if ( T->totind >= newreservation ) {
				MesPrint("@More than %ld elements in sparse table",MAXTABLECOMBUF);
				AC.cbufnum = oldcbufnum;
				Terminate(-1);
			}
			wold = (WORD *)Malloc1(newreservation*sizeof(WORD)*
								(T->numind+TABLEEXTENSION),"tablepointers");
			for ( i = T->reserved*(T->numind+TABLEEXTENSION)-1; i >= 0; i-- )
				wold[i] = T->tablepointers[i];
			if ( T->tablepointers ) M_free(T->tablepointers,"tablepointers");
			T->tablepointers = wold;
			T->reserved = newreservation;
		}
		w = oldwp;
		for ( sum = T->totind*(T->numind+TABLEEXTENSION), i = 0; i < T->numind; i++ ) {
			T->tablepointers[sum++] = *w++;
		}
		InsTableTree(T,T->tablepointers+sum-T->numind);
#if TABLEEXTENSION == 2
		T->tablepointers[sum+TABLEEXTENSION-1] = -1;  /* New element! */
#else
		T->tablepointers[sum+1] = T->bufnum;
		T->tablepointers[sum+2] = -1;
		T->tablepointers[sum+3] = -1;
		T->tablepointers[sum+4] = 0;
		T->tablepointers[sum+5] = 0;
#endif
	}
	else {
		if ( !nofill && T->tablepointers[sum] >= 0 ) {
redef:;
			if ( AC.vetofilling ) nofill = 1;
			else {
				Warning("Table element was already defined. New definition will be used");
			}
		}
#if TABLEEXTENSION == 2
		T->tablepointers[sum+TABLEEXTENSION-1] = -1;  /* New element! */
#else
		T->tablepointers[sum+1] = T->bufnum;
		T->tablepointers[sum+2] = -1;
		T->tablepointers[sum+3] = -1;
		T->tablepointers[sum+4] = 0;
		T->tablepointers[sum+5] = 0;
#endif
	}
	if ( T->numind ) { p++; }
	if ( *p != '=' ) {
		MesPrint("&Fill statement misses = sign after the table element");
		AC.cbufnum = oldcbufnum;
		AT.WorkPointer = oldwp;
		functions[funnum].tabl = oldT;
		return(1);
	}
	if ( tablestub == 0 && T->mode == 1 && AC.vetotablebasefill == 0 ) {
/*
		Here we construct a righthandside from the indices and the wildcards
*/
		int numfake;
		tablestub = 1;
		p4 = T->argtail;
		while ( *p4 ) p4++;
		numfake = (p4-T->argtail)+(p3-p1)+10;

		fake = (UBYTE *)Malloc1(numfake*sizeof(UBYTE),"Fill fake rhs");
		p = fake;
		*p++ = 't'; *p++ = 'b'; *p++ = 'l'; *p++ = '_'; *p++ = '(';
		p4 = p1; while ( p4 < p2 ) *p++ = *p4++; *p++ = ',';
		p4 = p2+1; while ( p4 < p3 ) *p++ = *p4++;
		if ( T->argtail ) {
			p4 = T->argtail + 1;
			while ( FG.cTable[*p4] == 1 ) p4++;
			while ( *p4 ) {
				if ( *p4 == '?' && p[-1] != ',' ) {
					p4++;
					if ( FG.cTable[*p4] == 0 || *p4 == '$' || *p4 == '[' ) {
						p4 = SkipAName(p4);
						if ( *p4 == '[' ) {
							SKIPBRA1(p4);
						}
					}
					else if ( *p4 == '{' ) {
						SKIPBRA2(p4);
					}
					else if ( *p4 ) { *p++ = *p4++; continue; }
				}
				else *p++ = *p4++;
			}
		}
		*p++ = ')';
		*p = 0;
		inp1 = fake;
/*		AT.WorkPointer += T->numind; */
	}
	else {
		inp1 = ++p;
	}
	c = 0;
/*
	Now we have the indices and p points to the rhs.
*/
	numover = 0;
	AC.tablefilling = funnum;
	while ( *inp1 ) {
		p = SkipField(inp1,0);
		c = *p; *p = 0;
#ifdef WITHPTHREADS
		Tprototype = T->prototype[0];
#else
		Tprototype = T->prototype;
#endif
		if ( ( i = CompileAlgebra(inp1,RHSIDE,Tprototype) ) < 0 ) { error = 1; i = 0; }
		if ( !nofill ) {
			T->tablepointers[sum] = i;
			T->tablepointers[sum+1] = T->bufnum;
		}
		AC.DumNum = 0;
		*p = c;
		if ( T->sparse || c == 0 ) break;
		inp1 = ++p;
#if ( TABLEEXTENSION == 2 )
		sum++;
#else
		sum += 2;
#endif
		if ( !nofill && T->tablepointers[sum] >= 0 ) numover++;
#if ( TABLEEXTENSION == 2 )
		sum++;
#else
		sum += TABLEEXTENSION-2;
#endif
	}
	if ( AC.exprfillwarning == 1 ) {
		AC.exprfillwarning = 2;
		Warning("Use of expressions and/or $variables in Fill statements is potentially very dangerous.");
	}
	AC.tablefilling = 0;
	if ( T->sparse && c != 0 ) {
		MesPrint("&In sparse tables one can fill only one element at a time");
		error = 1;
	}
	else if ( numover ) {
		if ( numover == 1 )
			Warning("one element was overwritten. New definition will be used");
		else if ( AC.WarnFlag )
			MesPrint("&Warning: %d elements were overwritten. New definitions will be used",numover);
	}
	if ( T->sparse ) {
		if ( redef == 0 ) T->totind++;
	}
	else T->defined++;
/*
	NumSets = AC.SetList.numtemp;
	NumSetElements = AC.SetElementList.numtemp;
*/
	if ( fake ) {
		M_free(fake,"Fill fake rhs");
		fake = 0;
		functions[funnum].tabl = T = T->spare;
		p = p3;
		goto andagain;
	}
	AC.cbufnum = oldcbufnum;
	AC.SymChangeFlag = 1;
	AT.WorkPointer = oldwp;
	functions[funnum].tabl = oldT;
	return(error);
}

/*
  	#] CoFill : 
  	#[ CoFillExpression :

	Syntax: FillExpression table = expression(x1,...,xn);
	The arguments should have been bracketed. Each corresponds to one
	of the dimensions of the table. Then the bracket with x1^2*x3^4
	will fill the (2,0,4) element of the table (if n=3 of course).
	Brackets that don't fit will be skipped. It just gives a warning.

	New option (13-jul-2005)
	Syntax: FillExpression table = expression(f);
	The table indices are arguments of the function f which should
	have been bracketed before.
*/

int CoFillExpression(UBYTE *inp)
{
	GETIDENTITY
	UBYTE *p, c;
	WORD type, funnum, expnum, symnum, numsym = 0, *oldwork = AT.WorkPointer;
	WORD *brackets, *term, brasize, *b, *m, *w, *pw, *tstop, zero = 0;
	WORD oldcbuf = AC.cbufnum, curelement = 0;
	int weneedit, i, j, numzero, pow;
	TABLES T = 0;
	LONG newreservation, numcommu, sum;
	POSITION oldposition;
	FILEHANDLE *fi; 
	CBUF *C;
	WORD numdummies;

	AN.IndDum = AM.IndDum;
	if ( ( p = SkipAName(inp) ) == 0 ) return(1);
	c = *p; *p = 0;
	if ( ( GetVar(inp,&type,&funnum,CFUNCTION,NOAUTO) == NAMENOTFOUND )
	|| ( T = functions[funnum].tabl ) == 0 ) {
		MesPrint("&%s should be a previously declared table",inp);
		*p = c; return(1);
	}
	*p++ = c;
	if ( T->spare ) T = T->spare;
	C = cbuf + T->bufnum;
	if ( c != '=' ) {
		MesPrint("&No = sign in FillExpression statement");
		return(1);
	}
	inp = p;
	if ( ( p = SkipAName(inp) ) == 0 ) return(1);
	c = *p; *p = 0;
	if ( ( type = GetName(AC.exprnames,inp,&expnum,NOAUTO) ) == NAMENOTFOUND
	|| c != '(' || (
		Expressions[expnum].status != LOCALEXPRESSION &&
		Expressions[expnum].status != SKIPLEXPRESSION &&
		Expressions[expnum].status != DROPLEXPRESSION &&
		Expressions[expnum].status != GLOBALEXPRESSION &&
		Expressions[expnum].status != SKIPGEXPRESSION &&
		Expressions[expnum].status != DROPGEXPRESSION ) ) {
		MesPrint("&%s should be an active expression with arguments",inp);
		*p = c; return(1);
	}
	if ( Expressions[expnum].inmem ) {
		MesPrint("&%s cannot be used in a FillExpression statement in the same %n\
        module that it has been redefined",inp);
		*p = c; return(1);
	}
	*p++ = c;
	while ( *p ) {
		inp = p;
		if ( ( p = SkipAName(inp) ) == 0 ) return(1);
		c = *p; *p = 0;

		if ( GetVar(inp,&type,&symnum,-1,NOAUTO) == NAMENOTFOUND ) {
			MesPrint("&%s should be a previously declared symbol or function",inp);
			*p = c; return(1);
		}
		else if ( type == CSYMBOL ) {
			*p++ = c;
			*AT.WorkPointer++ = symnum;
			numsym++;
		}
		else if ( type == CFUNCTION ) {
			numsym = -1;
			*p++ = c;
			if ( c != ')' ) {
				MesPrint("&Argument should be a single function or a list of symbols");
				return(1);
			}
			symnum += FUNCTION;
			*AT.WorkPointer++ = symnum;
		}
		else {
			MesPrint("&%s should be a previously declared symbol or function",inp);
			*p = c; return(1);
		}
/*
		if ( GetVar(inp,&type,&symnum,CSYMBOL,NOAUTO) == NAMENOTFOUND ) {
			if ( numsym > 0 ) {
				MesPrint("&%s should be a previously declared symbol",inp);
				*p = c; return(1);
			}
			else {
				if ( GetVar(inp,&type,&symnum,CFUNCTION,NOAUTO) == NAMENOTFOUND ) {
					MesPrint("&%s should be a previously declared symbol or function",inp);
					*p = c; return(1);
				}
				numsym = -1;
				*p++ = c;
				if ( c != ')' ) {
					MesPrint("&Argument should be a single function or a list of symbols");
					*p = c; return(1);
				}
				symnum += FUNCTION;
				*AT.WorkPointer++ = symnum;
				break;
			}
		}
		*p++ = c;
		*AT.WorkPointer++ = symnum;
		numsym++;
*/
		if ( c == ')' ) break;
		if ( c != ',' ) {
			MesPrint("&Illegal separator in FillExpression statement");
			goto noway;
		}
	}
	if ( *p ) {
		MesPrint("&Illegal end of FillExpression statement");
		goto noway;
	}
/*
	We have the number of the table in funnum.
	The number of the expression in expnum, the table struct in T
	and either the numbers of the symbols in oldwork (there are numsym of them)
	or the number of the function in oldwork (just one and numsym = -1).
	We don't sort them!!!!
*/
	if ( ( numsym > 0 ) && ( T->numind != numsym ) ) {
		MesPrint("&This table needs %d symbols for its array indices");
		goto noway;
	}
	EXCHINOUT
#ifdef WITHMPI
	/*
	 * The workers can't access to the data of the input expression. We need to
	 * broadcast it to all the workers.
	 */
	PF_BroadcastExpr(&Expressions[expnum], AR.infile);
	if ( PF.me == MASTER ) {
		/*
		 * Restore the file position on the master.
		 */
		POSITION pos;
		SetEndScratch(AR.infile, &pos);
	}
#endif
	fi = AR.infile;
	if ( fi->handle >= 0 ) {
		PUTZERO(oldposition);
		SeekFile(fi->handle,&oldposition,SEEK_CUR);
		SetScratch(fi,&(Expressions[expnum].onfile));
/*		SeekFile(fi->handle,&(Expressions[expnum].onfile),SEEK_SET); */
		if ( ISNEGPOS(Expressions[expnum].onfile) ) {
			MesPrint("&File error in FillExpression");
			BACKINOUT
			goto noway;
		}
	}
	else {
/*
		Note: Because everything fits inside memory we never get problems
		with excessive file sizes.
*/
		SETBASEPOSITION(oldposition,(UBYTE *)(fi->POfill)-(UBYTE *)(fi->PObuffer));
		fi->POfill = (WORD *)((UBYTE *)(fi->PObuffer) + BASEPOSITION(Expressions[expnum].onfile));
	}
	pw = AT.WorkPointer;
	if ( numsym < 0 ) { brackets = pw + 1; }
	else { brackets = pw + numsym; }
	brasize = -1; weneedit = 0; /* stands for we need it */
    term = (WORD *)(((UBYTE *)(brackets)) + AM.MaxTer);
    AT.WorkPointer = (WORD *)(((UBYTE *)(term)) + AM.MaxTer);
	AC.cbufnum = T->bufnum;
	AC.tablefilling = funnum;
	if ( GetTerm(BHEAD term) > 0 ) {			/* Skip prototype */
		while ( GetTerm(BHEAD term) > 0 ) {
			GETSTOP(term,tstop);
			w = m = term + 1;
			while ( m < tstop && *m != HAAKJE ) m += m[1];
			if ( *m != HAAKJE ) {
				MesPrint("&Illegal attempt to put an expression without brackets in a table");
				BACKINOUT
				goto noway;
			}
			if ( brasize == m - w ) {
				b = brackets;
				while ( *b == *w && w < m ) { b++; w++; }
				if ( w == m ) { /* Same as current bracket. Copy. */
					if ( weneedit ) {
						m += m[1] - 1;
						*m = *term - (m-term);
						AddNtoC(AC.cbufnum,*m,m,3);
						numdummies = DetCurDum(BHEAD term) - AM.IndDum;
						if ( numdummies > T->numdummies ) T->numdummies = numdummies;
					}
					continue; /* Next term */
				}
			}
			if ( weneedit ) {
				AddNtoC(AC.cbufnum,1,&zero,4);	/* Terminate old bracket */
				numcommu = numcommute(C->rhs[curelement],&(C->NumTerms[curelement]));
				C->CanCommu[curelement] = numcommu;
			}
			b = brackets; w = term + 1;
			if ( numsym < 0 ) pw = oldwork + 1;
			else              pw = oldwork + numsym;
			while ( w < m ) *b++ = *w++;
			brasize = b - brackets;
/*
			Now compute the element. See whether we need it
*/
			if ( numsym < 0 ) {
				WORD *bb;
				if ( *brackets != symnum || brasize != brackets[1] ) {
					weneedit = 0; continue;	/* Cannot work! */
				}
/*
				Now count the number of arguments and whether they are numbers
*/
				b = brackets + FUNHEAD;
				bb = brackets+brackets[1];
				i = 0;
				while ( b < bb ) {
					if ( *b != -SNUMBER ) break;
					i++;
					b += 2;
				}
				if ( b < bb || i != T->numind ) {
					weneedit = 0; continue;	/* Cannot work! */
				}
			}
			else if ( brasize > 0 && ( *brackets != SYMBOL 
			|| brackets[1] < brasize || (brackets[1]-2) > numsym*2 ) ) {
				weneedit = 0; continue;	/* Cannot work! */
			}
			numzero = 0; sum = 0;
			if ( numsym > 0 ) {
			  for ( i = 0; i < numsym; i++ ) {
				if ( brasize > 0 ) {
					b = brackets + 2; j = brackets[1]-2;
					while ( j > 0 ) {
						if ( *b == oldwork[i] ) break;
						j -= 2; b += 2;
					}
					if ( j <= 0 ) {  /* it was not there */
						numzero++; pow = 0;
						if ( 2*numzero+brackets[1]-2 > numsym*2 ) {
							weneedit = 0; goto nextterm;
						}
					}
					else pow = b[1];
				}
				else pow = 0;
				if ( T->sparse ) *pw++ = pow;
				else if ( pow < T->mm[i].mini || pow > T->mm[i].maxi ) {
					weneedit = 0; goto nextterm;
				}
				else sum += ( pow - T->mm[i].mini ) * T->mm[i].size;
			  }
			}
			else {
			  b = brackets + FUNHEAD;
			  sum = 0;
			  for ( i = 0; i < T->numind; i++ ) {
				pow = b[1];
				b += 2;
				if ( T->sparse ) { *pw++ = pow; }
				else if ( pow < T->mm[i].mini || pow > T->mm[i].maxi ) {
					weneedit = 0; goto nextterm;
				}
				else sum += ( pow - T->mm[i].mini ) * T->mm[i].size;
			  }
			}
			weneedit = 1;
			if ( T->sparse ) {
				if ( numsym < 0 ) pw = oldwork + 1;
				else              pw = oldwork + T->numind;
				i = FindTableTree(T,pw,1);
				if ( i >= 0 ) {
					sum = i+T->numind;
/*
Wrong!!!!			C->rhs[T->tablepointers[sum]] = C->Pointer;
*/
					C->Pointer--; /* Back up over the zero */
					goto newentry;
				}
				if ( T->totind >= T->reserved ) {
					if ( T->reserved == 0 ) newreservation = 20;
					else newreservation = T->reserved;
/*
					while ( T->totind >= newreservation && newreservation <
								MAXTABLECOMBUF*(T->numind+TABLEEXTENSION) )
							newreservation = 2*newreservation;
					if ( newreservation > MAXTABLECOMBUF*T->numind ) newreservation =
								MAXTABLECOMBUF*(T->numind+TABLEEXTENSION);
*/
/*---Copied from Fill---------------------------*/
					while ( T->totind >= newreservation && newreservation < MAXTABLECOMBUF )
							newreservation = 2*newreservation;
					if ( newreservation > MAXTABLECOMBUF ) newreservation = MAXTABLECOMBUF;
					if ( T->totind >= newreservation ) {
						MesPrint("@More than %ld elements in sparse table",MAXTABLECOMBUF);
						AC.cbufnum = oldcbuf;
						AT.WorkPointer = oldwork;
						Terminate(-1);
					}
/*---Copied from Fill---------------------------*/
					if ( T->totind >= newreservation ) {
						MesPrint("@More than %ld elements in sparse table",MAXTABLECOMBUF);
						AC.cbufnum = oldcbuf;
						AT.WorkPointer = oldwork;
						Terminate(-1);
					}
					w = (WORD *)Malloc1(newreservation*sizeof(WORD)*
							(T->numind+TABLEEXTENSION),"tablepointers");
					for ( i = T->reserved*(T->numind+TABLEEXTENSION)-1; i >= 0; i-- )
						w[i] = T->tablepointers[i];
					if ( T->tablepointers ) M_free(T->tablepointers,"tablepointers");
					T->tablepointers = w;
					T->reserved = newreservation;
				}
				if ( numsym < 0 ) pw = oldwork + 1;
				else              pw = oldwork + numsym;
				for ( sum = T->totind*(T->numind+TABLEEXTENSION), i = 0; i < T->numind; i++ ) {
					T->tablepointers[sum++] = *pw++;
				}
				InsTableTree(T,T->tablepointers+sum-T->numind);
				(T->totind)++;
			}
#if ( TABLEEXTENSION != 2 )
			else {
				sum *= TABLEEXTENSION;
			}
#endif
/*
			Start a new entry. Copy the element.
*/
			AddRHS(T->bufnum,0);
			T->tablepointers[sum] = C->numrhs;
#if ( TABLEEXTENSION == 2 )
			T->tablepointers[sum+TABLEEXTENSION-1] = -1;
#else
			T->tablepointers[sum+1] = T->bufnum;
			T->tablepointers[sum+2] = -1;
			T->tablepointers[sum+3] = -1;
			T->tablepointers[sum+4] = 0;
			T->tablepointers[sum+5] = 0;
#endif
newentry:	if ( *m == HAAKJE ) { m += m[1] - 1; }
			else m--;
			*m = *term - (m-term);
			AddNtoC(AC.cbufnum,*m,m,5);
			curelement = T->tablepointers[sum];
nextterm:;
		}
		if ( weneedit ) {
			AddNtoC(AC.cbufnum,1,&zero,6);	/* Terminate old bracket */
			numcommu = numcommute(C->rhs[curelement],&(C->NumTerms[curelement]));
			C->CanCommu[curelement] = numcommu;
		}
	}
	if ( fi->handle >= 0 ) {
		SetScratch(fi,&(oldposition));
	}
	else {
		fi->POfill = (WORD *)((UBYTE *)(fi->PObuffer) + BASEPOSITION(oldposition));
	}
	BACKINOUT
	AC.cbufnum = oldcbuf;
	AC.tablefilling = 0;
	AT.WorkPointer = oldwork;
	return(0);
noway:
	BACKINOUT
	AC.cbufnum = oldcbuf;
	AC.tablefilling = 0;
	AT.WorkPointer = oldwork;
	return(1);
}

/*
  	#] CoFillExpression : 
  	#[ CoPrintTable :

	Syntax
		PrintTable [+f] [+s] tablename [>[>] file];
	All defined elements are written with individual Fill statements.
	If a file is specified, the result is written to file only.
	The flags of the print statement apply as much as possible.
	We make use of the regular write routines.
*/

int CoPrintTable(UBYTE *inp)
{
	GETIDENTITY
	int fflag = 0, sflag = 0, addflag = 0, error = 0, sum, i, j;
	UBYTE *filename, *p, c, buffer[100], *s, *oldoutputline = AO.OutputLine;
	WORD type, funnum, *expr, *m, num;
	TABLES T = 0;
	WORD oldSkip = AO.OutSkip, oldMode = AC.OutputMode, oldHandle = AC.LogHandle;
	WORD oldType = AO.PrintType, *oldwork = AT.WorkPointer;
	UBYTE *oldFill = AO.OutFill, *oldLine = AO.OutputLine;
#ifdef WITHMPI
	if ( PF.me != MASTER ) return 0;
#endif
/*
	First the flags
*/
	while ( *inp == '+' ) {
		inp++;
		if ( *inp == 'f' || *inp == 'F' ) { fflag = 1; inp++; }
		else if ( *inp == 's' || *inp == 'S' ) { sflag = PRINTONETERM; inp++; }
		else {
			MesPrint("&Illegal + option in PrintTable statement");
			error = 1; inp++;
		}
		while ( *inp != ',' && *inp && *inp != '+' ) {
			if ( !error ) {
				if ( *inp ) {
					MesPrint("&Illegal + option in PrintTable statement");
					inp++;
				}
				else {
					MesPrint("&Unfinished PrintTable statement");
					return(1);
				}
				error = 1;
			}
			inp++;
		}
		if ( *inp == ',' ) inp++;
	}
/*
	Now the name of the table
*/
	if ( ( p = SkipAName(inp) ) == 0 ) return(1);
	c = *p; *p = 0;
	if ( ( GetVar(inp,&type,&funnum,CFUNCTION,NOAUTO) == NAMENOTFOUND )
	|| ( T = functions[funnum].tabl ) == 0 ) {
		MesPrint("&%s should be a previously declared table",inp);
		*p = c; return(1);
	}
	if ( T->spare && T->mode == 1 ) T = T->spare;
	*p++ = c;
/*
	Check for a filename. Runs to the end of the statement.
*/
	filename = 0;
	if ( c == '>' ) {
		if ( *p == '>' ) { addflag = 1; p++; }
		filename = p;
	}
	else filename = 0;

	if ( filename ) {
		if ( addflag ) AC.LogHandle = OpenAddFile((char *)filename);
		else           AC.LogHandle = CreateFile((char *)filename);
		if ( AC.LogHandle < 0 ) {
			MesPrint("&Cannot open file '%s' properly",filename);
			error = 1; goto finally;
		}
		AO.PrintType = PRINTLFILE;
	}
	else if ( fflag && AC.LogHandle >= 0 ) {
		AO.PrintType = PRINTLFILE;
	}
	AO.OutFill = AO.OutputLine = (UBYTE *)AT.WorkPointer;
	AT.WorkPointer += 2*AC.LineLength;

	AO.PrintType |= sflag;
	AC.OutputMode = 0;
	AO.IsBracket = 0;
	AO.OutSkip = 0;
	AR.DeferFlag = 0;
	AC.outsidefun = 1;
	if ( AC.LogHandle == oldHandle ) FiniLine();
	AO.OutputLine = AO.OutFill = (UBYTE *)Malloc1(AC.LineLength+20,"PrintTable");
	AO.OutStop = AO.OutFill + AC.LineLength;
	for ( i = 0; i < T->totind; i++ ) {
		if ( !T->sparse && T->tablepointers[i*TABLEEXTENSION] < 0 ) continue;
		TokenToLine((UBYTE *)"Fill ");
		TokenToLine((UBYTE *)(VARNAME(functions,funnum)));
		TokenToLine((UBYTE *)"(");
		AO.OutSkip = 3;
		if ( T->sparse ) {
			sum = i * ( T->numind + TABLEEXTENSION );
			for ( j = 0; j < T->numind; j++, sum++ ) {
				if ( j > 0 ) TokenToLine((UBYTE *)",");
				num = T->tablepointers[sum];
				s = buffer; s = NumCopy(num,s);
				TokenToLine(buffer);
			}
			expr = cbuf[T->bufnum].rhs[T->tablepointers[sum]];
		}
		else {
			for ( j = 0; j < T->numind; j++ ) {
				if ( j > 0 ) {
					TokenToLine((UBYTE *)",");
					num = T->mm[j].mini + ( i % T->mm[j-1].size ) / T->mm[j].size;
				}
				else {
					num = T->mm[j].mini + i / T->mm[j].size;
				}
				s = buffer; s = NumCopy(num,s);
				TokenToLine(buffer);
			}
			expr = cbuf[T->bufnum].rhs[T->tablepointers[TABLEEXTENSION*i]];
		}
		TOKENTOLINE(") =",")=");
		if ( sflag ) {
			FiniLine();
			if ( AC.OutputSpaces != NOSPACEFORMAT ) TokenToLine((UBYTE *)"   ");
		}
		m = expr;
/*
		WORD lbrac, first;
		lbrac = 0; first = 1;
		while ( *m ) {
			if ( WriteTerm(m,&lbrac,first,1,0) ) {
				MesPrint("Error while writing table");
				error = 1;
				goto finally;
			}
			first = 0;
			m += *m;
		}
		if ( first ) { TOKENTOLINE(" 0","0") }
		else if ( lbrac ) { TOKENTOLINE(" )",")") }
*/
		while ( *m ) m += *m;
		if ( m > expr ) {
			if ( WriteExpression(expr,(LONG)(m-expr)) ) { error = 1; goto finally; }
			AO.OutSkip = 0;
		}
		else {
			TokenToLine((UBYTE *)"0");
		}
		TokenToLine((UBYTE *)";");
		FiniLine();
	}
	M_free(AO.OutputLine,"PrintTable");
	AO.OutputLine = AO.OutFill = oldoutputline;
/*
	Reset the file pointers and parameters if any. Close file if needed.
*/
finally:
	AO.OutSkip     = oldSkip;
	AC.OutputMode  = oldMode;
	AC.LogHandle   = oldHandle;
	AO.PrintType   = oldType;
	AO.OutFill     = oldFill;
	AO.OutputLine  = oldLine;
	AT.WorkPointer = oldwork;
	AC.outsidefun  = 0;
	return(error);
}

/*
  	#] CoPrintTable : 
  	#[ CoAssign :

	This statement has an easy syntax:
		$name = expression
*/

static WORD AssignLHS[14] = { TYPEASSIGN, 3+SUBEXPSIZE, 0,
							SUBEXPRESSION, SUBEXPSIZE, 0, 1, 0, 0,0,0,0,0 };

int CoAssign(UBYTE *inp)
{
	int error = 0, retcode;
	UBYTE *name, c;
	WORD number;
	if ( *inp != '$' ) {
nolhs:	MesPrint("&assign statement should have a dollar variable in the LHS");
		return(1);
	}
	inp++; name = inp;
	if ( FG.cTable[*inp] != 0 ) goto nolhs;
	while ( FG.cTable[*inp] < 2 ) inp++;
	if ( AP.PreAssignFlag == 2 ) {
		if ( *inp == '_' ) inp++;
	}
	if ( ( *inp == ',' && inp[1] != '=' ) && ( *inp != '=' ) ) {
		MesPrint("&assign statement should have only a dollar variable in the LHS");
		return(1);
	}
	c = *inp;
	*inp = 0;
	if ( GetName(AC.dollarnames,name,&number,NOAUTO) == NAMENOTFOUND ) {
		number = AddDollar(name,DOLUNDEFINED,0,0);
	}
	*inp = c;
	if ( c == ',' ) inp++;
	*inp++ = '=';
	if ( *inp == ',' ) inp++;
/*
	Fake a Prototype and read the RHS
*/
	AssignLHS[7] = AC.cbufnum;
	retcode = CompileAlgebra(inp,RHSIDE,(AssignLHS+3));
	if ( retcode < 0 ) error = 1;
	AC.DumNum = 0;
/*
	Now add the LHS
*/
	AssignLHS[2] = number;
	AssignLHS[5] = retcode;
	AddNtoL(AssignLHS[1],AssignLHS);
/*
	Add to the list of potentially modified dollars (for PARALLEL)
*/
	AddPotModdollar(number);
	return(error);
}

/*
  	#] CoAssign : 
  	#[ CoDeallocateTable :

	Syntax: DeallocateTable tablename(s);
	Should work only for sparse tables.
	Action: Cleans all definitions of elements of a table as if there have
	        never been any fill statements.
*/

int CoDeallocateTable(UBYTE *inp)
{
	UBYTE *p, c;
	TABLES T = 0;
	WORD type, funnum, i;
	c = *inp;
	while ( c ) {
		while ( *inp == ',' ) inp++;
		if ( *inp == 0 ) break;
		if ( ( p = SkipAName(inp) ) == 0 ) return(1);
		c = *p; *p = 0;
		if ( ( GetVar(inp,&type,&funnum,CFUNCTION,NOAUTO) == NAMENOTFOUND )
		|| ( T = functions[funnum].tabl ) == 0 ) {
			MesPrint("&%s should be a previously declared table",inp);
			*p = c; return(1);
		}
		if ( T->sparse == 0 ) {
			MesPrint("&%s should be a sparse table",inp);
			*p = c; return(1);
		}
		if ( T->tablepointers ) M_free(T->tablepointers,"tablepointers");
		ClearTableTree(T);
		for (i = 0; i < T->buffersfill; i++ ) { /* was <= */
			finishcbuf(T->buffers[i]);
		}
		T->bufnum = inicbufs();
		T->buffersfill = 0;
		T->buffers[T->buffersfill++] = T->bufnum;
		T->tablepointers = 0;
		T->boomlijst = 0;
		T->totind = 0;
		T->reserved = 0;

		if ( T->spare ) {
			TABLES TT = T->spare;
			if ( TT->tablepointers ) M_free(TT->tablepointers,"tablepointers");
			ClearTableTree(TT);
			for (i = 0; i < TT->buffersfill; i++ ) { /* was <= */
				finishcbuf(TT->buffers[i]);
			}
			TT->bufnum = inicbufs();
			TT->buffersfill = 0;
			TT->buffers[T->buffersfill++] = T->bufnum;
			TT->tablepointers = 0;
			TT->boomlijst = 0;
			TT->totind = 0;
			TT->reserved = 0;
		}
		*p++ = c;
		inp = p;
	}
 	return(0);
}

/*
  	#] CoDeallocateTable : 
  	#[ CoFactorCache :
*/
/**
 *	Reads the FactorCache statement which is like a fill statement for
 *	the factorization cache. Syntax:
 *		FactorCache,expression:factor1,...,factorn;
 *	This statement is mainly for testing purposes, because there are severe
 *	restrictions on the use of the expression (no common GCD, no denominators)
 *	The expression is worked out by FORM and properly normalized and sorted.
 */

/*
int CoFactorCache(UBYTE *inp)
{
	Code to be added in due time
	We need to read 'expression', get its terms through Generator and sort them.
	We store the result in the WorkSpace in argument notation.
	This will be argin.
	Then we do the same with the sequence of factors. They form argout.
	The whole is put in the buffer with the call
		InsertArg(BHEAD argin,argout,1)
	return(0);
}
*/

/*
  	#] CoFactorCache : 
*/
