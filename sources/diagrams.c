/** @file diagrams.c
 * 
 *  Contains the wrapper routines for diagram manipulations.
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
  	#[ Includes : diagrams.c
*/

#include "form3.h"
 
static WORD one = 1;

/*
  	#] Includes : 
  	#[ CoCanonicalize :

	Syntax:
		Canonicalize,mainoption,...
	With the main options currently:
		Canonicalize,topology,vertexfunction,edgefunction,OutDollar,extraoptions;
		Canonicalize,polynomial,InDollar,set_or_setname_or_dollar,OutDollar,extraoptions;
	The vertex function needs to have the format (assume it is called vx):
		vx(p1,p2,-p3)  or vx(-p1,p2,p3,-p4) etc.
	The external lines have a vertex with only one line.
	All momenta that form connections should be unique.
	In principle the - signs are not relevant for the topology, but they
	may exist already in the remaining part of the diagram. They are also
	part of the canonical form.
	The edge function can be used in different ways, depending on the options.
	The extraoption(s) should be nonnegative integers or $variables that evaluate
	into nonnegative integers (integers less than 2^31).
	The Indollar variable contains the polynomial to be canonicalized.
	The OutDollar variable should be the name of a $-variable (as in $out) which
	will be filled with a replace_ function. The canonicalization can then
	be executed in the whole term with the   Multiply $out;  command.
*/

int CoCanonicalize(UBYTE *s)
{
	WORD args[10], *a, num;
	UBYTE *t, c;
	args[0] = TYPECANONICALIZE;
	a = args+2;
	while ( *s == ',' || *s == '\t' || *s == ' ' ) s++;
	t = s; while ( FG.cTable[*s] == 0 ) s++;
	c = *s; *s = 0;
	if ( StrICmp(t,(UBYTE *)("topology")) == 0 ) {
		*s = c; *a++ = 0;
		while ( *s == ',' || *s == '\t' || *s == ' ' ) s++;
		s = GetFunction(s,a);
		while ( *s == ',' || *s == '\t' || *s == ' ' ) s++;
		s = GetFunction(s,a+1);
		if ( *a == 0 || a[1] == 0 ) return(1);
		a += 2;
	}
	else if ( StrICmp(t,(UBYTE *)("polynomial")) == 0 ) {
		*s = c; *a++ = 1;
		while ( *s == ',' || *s == '\t' || *s == ' ' ) s++;
/*
		Now get the name of the input $-variable
*/
		if ( *s != '$' ) {
			MesPrint("&Canonicalize statement needs a $-variable for its input.");
			return(1);
		}
		s++; t = s; while ( FG.cTable[*s] < 2 ) s++;
		c = *s; *s = 0;
		if ( GetName(AC.dollarnames,t,&num,NOAUTO) == CDOLLAR ) *a++ = num;
		else { *a++ = AddDollar(t,DOLINDEX,&one,1); }
		*s = c;
/*
		Now the set
*/
		if ( *s == '{' ) {
			t = s+1; SKIPBRA2(s)
			c = *s; *s = 0;
			*a++ = DoTempSet(t,s);
			*s++ = c;
		}
		else if ( FG.cTable[*s] == 0 || *s == '[' ) {
			t = s;
			if ( ( s = SkipAName(s) ) == 0 ) {
				MesPrint("&Illegal name for set in Canonicalize statement: %s",t);
				return(1);
			}
			c = *s; *s = 0;
			if ( GetName(AC.varnames,t,a,WITHAUTO) == CSET ) {
				if ( Sets[*a].type != CSYMBOL ) {
					MesPrint("&In Canonicalize: %s is not a set of symbols.",t);
					return(1);
				}
			}
			else {
				MesPrint("&In Canonicalize: %s is not a set.",t);
				return(1);
			}
			*s = c; a++;
		}
		else if ( *s == '$' ) {
			s++; t = s; while ( FG.cTable[*s] < 2 ) s++;
			c = *s; *s = 0;
			if ( GetName(AC.dollarnames,t,&num,NOAUTO) == CDOLLAR ) *a++ = -num-2;
			else {
				MesPrint("&In Canonicalize: %s is undefined.",t-1);
				return(1);
			}
			*s = c;
		}
		else {
			MesPrint("&In Canonicalize: Illegal third(=set) argument.");
			return(1);
		}
	}
	else {
		MesPrint("&Unrecognized option in Canonicalize statement: %s",t);
		return(1);
	}
/*
	Now get the name of the output $-variable
*/
	while ( *s == ',' || *s == '\t' || *s == ' ' ) s++;
	if ( *s != '$' ) {
		MesPrint("&Canonicalize statement needs a $-variable for its output.");
		return(1);
	}
	s++; t = s; while ( FG.cTable[*s] < 2 ) s++;
	c = *s; *s = 0;
	if ( GetName(AC.dollarnames,t,&num,NOAUTO) == CDOLLAR ) *a++ = num;
	else { *a++ = AddDollar(t,DOLINDEX,&one,1); }
	*s = c;
/*
	Now the options. At the moment we just do one of them.
	(the first extra option is relevant to determine the use of the edge function)
	In the future we may have to be more flexible.
*/
	a[0] = 0;  /* default value */
	while ( *s == ',' || *s == '\t' || *s == ' ' ) s++;
	if ( *s != 0 ) {
		s = GetNumber(s,a);
		if ( *a == -1 ) return(1);
		while ( *s == ',' || *s == '\t' || *s == ' ' ) s++;
		a++;
	}
/*
	Now complete the args string and put it in the compiler buffer
*/
	args[1] = a-args;
	AddNtoL(args[1],args);
	return(0);
}

/*
  	#] CoCanonicalize : 
  	#[ DoCanonicalize :

	Does the canonicalization. The output term overwrites the input term.
*/

int DoCanonicalize(PHEAD WORD *term, WORD *params)
{
	WORD args[10];
	int i;
/*
	First check whether we need to expand dollars;
*/
	for ( i = 0; i < params[1]; i++ ) args[i] = params[i];
	if ( args[2] == 0 ) { /* topology */
		for ( i = 3; i < 5; i++ ) {
			if ( args[i] < 0 ) { /* This is a dollar */
				args[i] = DolToFunction(BHEAD -args[i]-2);
				if ( args[i] == 0 ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Value of $-variable in Canonicalize statement should be a function.");
					MUNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
			}
		}
		for ( i = 6; i < args[1]; i++ ) { /* Extra options */
			if ( args[i] < 0 ) { /* This is a dollar */
				args[i] = DolToNumber(BHEAD -args[i]-2);
				if ( args[i] < 0 ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Value of $-variable in Canonicalize statement should be a nonnegative number < %l.",(LONG)MAXPOSITIVE);
					MUNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
			}
		}
		switch ( args[6] ) {
			case 1: {/* pass the vertex and edge functions. */
				WORD *tstop, *t, *tedge, *te;
				tstop = term + *term; tstop -= ABS(tstop[-1]);
				t = term+1;
				tedge = AT.WorkPointer; te = tedge+1;
				while ( t < tstop ) {
					if ( *t != args[3] && *t != args[4] ) { t += t[1]; continue; }
					for ( i = 0; i < t[1]; i++ ) te[i] = t[i];
					te += t[1]; t += t[1];
				}
				*te++ = 1; *te++ = 1; *te++ = 3;
				tedge[0] = te-tedge;
				AT.WorkPointer = te;
/*
				DoVertexCanonicalize(BHEAD term,tedge,args[3],args[4],args[5],args[6]);
*/
				AT.WorkPointer = tedge;
			} break;
			case 2: {/* pass the edge functions only */
				WORD *tstop, *t, *tedge, *te;
				tstop = term + *term; tstop -= ABS(tstop[-1]);
				t = term+1;
				tedge = AT.WorkPointer; te = tedge+1;
				while ( t < tstop ) {
					if ( *t != args[4] ) { t += t[1]; continue; }
					for ( i = 0; i < t[1]; i++ ) te[i] = t[i];
					te += t[1]; t += t[1];
				}
				*te++ = 1; *te++ = 1; *te++ = 3;
				tedge[0] = te-tedge;
				AT.WorkPointer = te;
/*
				DoEdgeCanonicalize(BHEAD term,tedge,args[5]);
*/
				AT.WorkPointer = tedge;
			} break;
			default: {
				DoTopologyCanonicalize(BHEAD term,args[3],args[4],args+5);
			} break;
		}
/*
		Call here the topology canonicalization
		We will have the arguments:
			args[3]: The function used as vertex function
			args[4]: The function used as edge function
			args[5]: The number of the dollar to be used for the output
			args[6]: Potentially other options (like saying how to use args[4]).
			term:    The term in which the topology resides.
*/

	}
	else if ( args[2] == 1 ) { /* polynomial */
		WORD *symlist, nsymlist;
		for ( i = 6; i < args[1]; i++ ) { /* Extra options */
			if ( args[i] < 0 ) { /* This is a dollar */
				args[i] = DolToNumber(BHEAD -args[i]-2);
				if ( args[i] < 0 ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Value of $-variable in Canonicalize statement should be a nonnegative number < %l.",(LONG)MAXPOSITIVE);
					MUNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
			}
		}
/*
		Now we sort out the set. We create a pointer to the list of set
		elements, and we determine the number of elements in the set.
*/
		symlist = AT.WorkPointer;
		if ( args[4] < -1 ) { /* Dollar that should expand into a list of symbols */
			DOLLARS d = Dollars - args[4] - 2;
			WORD *ds, *insym;
			if ( d->type != DOLWILDARGS ) {
NoWildArg:
				MLOCK(ErrorMessageLock);
				MesPrint("Value of $-variable in Canonicalize statement should be a argument wildcard of symbol arguments.");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			insym = symlist; ds = d->where+1;
			while ( *ds ) {
				if ( *ds != -SYMBOL ) goto NoWildArg;
				*insym++ = ds[1];
				ds += 2;
			}
			nsymlist = insym-symlist;
		}
		else { /*if ( args[4] >= 0 ) */
			WORD *ss, *sy, n;
			ss = (WORD *)(AC.SetElementList.lijst)+Sets[args[4]].first;
			nsymlist = n = Sets[args[4]].last-Sets[args[4]].first;
			sy = symlist = AT.WorkPointer;
			NCOPY(sy,ss,n);
		}
		AT.WorkPointer = symlist+nsymlist;
/*
		Call here the polynomial canonicalization
		We will have the arguments:
			args[3]: The number of the dollar to be used for the input
			symlist: an array of symbols
			nsymlist: the number of symbols in symlist
			args[5]: The number of the dollar to be used for the output
			args[6]: Potentially other options.
*/
/*
		DoPolynomialCanonicalize(BHEAD args[3],symlist,nsymlist,args[5],args[6]);
*/
		AT.WorkPointer = symlist;
	}
	return(0);
}

/*
  	#] DoCanonicalize : 
  	#[ GenTopologies :

	This function has the syntax
		topologies_(nloops,    Number of loops
			nlegs,             Number of legs
			setvertexsizes,    A set which tells which vertices are allowed like {3,4}.
			set_extmomenta,    The name of a set with the external momenta
			set_intmomenta     The name of a set with the internal momenta
			[,options])
	The output will be using the built in functions vertex_ and edge_.

	The test for whether this function can be evaluated is in TestSub (inside
	file proces.c) (search for the string TOPOLOGIES). 
	This passes the code -15 in AN.TeInFun to Generator, which then calls
	the GenTopologies routine.
*/

WORD GenTopologies(PHEAD WORD *term,WORD level)
{
	WORD *t1, *tt1, *tstop, *t, *tt;
	WORD *oldworkpointer = AT.WorkPointer;
	WORD option1 = 0, option2 = 0, setoption = -1;
	WORD retval;
/*

	We have to go through the testing procedure again, because there could
	be more than one topologies_ function and not all have to be expandable.
*/
	tstop = term+*term; tstop -= ABS(tstop[-1]);
	tt = term+1;
	while ( tt < tstop ) {
		t = tt; tt = t+t[1];
		if ( *t != TOPOLOGIES ) continue;
		tt = t + t[1]; t1 = t + FUNHEAD;
		if ( t1+10 > tt || *t1 != -SNUMBER || t1[1] < 0 ||      /* loops */
			t1[2] != -SNUMBER || ( t1[3] < 0 && t1[3] != -2 ) ||/* legs */
			t1[4] != -SETSET || Sets[t1[5]].type != CNUMBER ||  /* set vertices */
			t1[6] != -SETSET || Sets[t1[7]].type != CVECTOR ||  /* outvectors */
			t1[8] != -SETSET || Sets[t1[9]].type != CVECTOR ) continue;
		tt1 = t1 + 10;
		if ( tt1+2 <= tt && tt1[0] == -SETSET ) {
			if ( Sets[t1[5]].last-Sets[t1[5]].first !=
				 Sets[tt1[1]].last-Sets[tt1[1]].first ) continue;
			setoption = tt1[1]; tt1 += 2;
		}
		if ( tt1+2 <= tt && tt1[0] == -SNUMBER ) { option1 = tt1[1]; tt1 += 2; }
		if ( tt1+2 <= tt && tt1[0] == -SNUMBER ) { option2 = tt1[1]; tt1 += 2; }
		AT.setinterntopo = t1[9];
		AT.setexterntopo = t1[7];
		AT.TopologiesTerm = term;
		AT.TopologiesStart = t;
		AT.TopologiesLevel = level;
		AT.TopologiesOptions[0] = option1;
		AT.TopologiesOptions[1] = option2;
		retval = GenerateTopologies(BHEAD t1[1],t1[3],t1[5],setoption);
		AT.WorkPointer = oldworkpointer;
		return(retval);
	}
	MLOCK(ErrorMessageLock);
	MesPrint("Internal error: topologies_ function not encountered.");
	MUNLOCK(ErrorMessageLock);
	return(-1);

}

/*
  	#] GenTopologies : 
  	#[ GenDiagrams :
*/

WORD GenDiagrams(PHEAD WORD *term,WORD level)
{
#ifdef WITHPTHREADS
	DUMMYUSE(B)
#endif
	DUMMYUSE(term)
	DUMMYUSE(level)
	return(0);
}

/*
  	#] GenDiagrams : 
  	#[ DoTopologyCanonicalize :

	term: The term
	vert: the vertex function
	edge: the edge function
	args[0]: the number of the output dollar
	args[1]: options
	return value should be zero if all is correct.

	The external lines connect to an 'external vertex' which has only one line.
	The vertices are of a type: vertex_(p1,p2,-p3)*vertex_(p3,p4,p5) etc.
	The edges indicate noninteger powers of the lines:
		edge_(p1,2) means 1/p1.p1^(2*ep)
*/

int DoTopologyCanonicalize(PHEAD WORD *term,WORD vert,WORD edge,WORD *args)
{
	int nvert = 0, nvert2, i, ii, jj, flipnames = 0, nparts, level, num;
	WORD *tstop, *t, *tt, *tend, *td;
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *termcopy = TermMalloc("TopologyCanonize1");
	WORD *vet= TermMalloc("TopologyCanonize2");
	WORD *partition, *environ, *connect, *pparts, *p;
/*
	WORD *pparts;
*/
	WORD momenta[150],flips[50],nmomenta = 0, nflips = 0;
/*
	Step one: the vertices should get a number. We copy the term for this.
	          We need a high number for the vertex function to make sure 
	          that it comes after the edge function in the sorting.
*/
	if ( args[0] < args[1] ) { flipnames = 1; }
	tend = term + *term; tend -= ABS(tend[-1]); t = term+1; tt = termcopy+1;
	while ( t < tend ) {
		if ( *t == vert ) {
			for ( i = FUNHEAD; i < t[1]; i += 2 ) {
				if ( t[i] == -VECTOR || ( t[i] == -INDEX && t[i+1] < 0 ) ) {
					momenta[nmomenta++] = -VECTOR;
					momenta[nmomenta++] = t[i+1];
				}
				else if ( t[i] == -MINVECTOR ) {
					momenta[nmomenta++] = -MINVECTOR;
					momenta[nmomenta++] = t[i+1];
				}
				else goto notgoodvertex;
				momenta[nmomenta++] = nvert;
			}
			ii = FUNHEAD; i = t[1]-FUNHEAD;
			NCOPY(tt,t,ii)
			if ( flipnames ) tt[-FUNHEAD] = edge;
			tt[-FUNHEAD+1] += 2;
			*tt++ = -CNUMBER; *tt++ = nvert++;
		}
		else if ( *t == edge && flipnames ) {
			i = t[1] - 1; *tt++ = vert; t++;
		}
		else {
notgoodvertex:
			i = t[1];
		}
		NCOPY(tt,t,i)
	}
	while ( t < tend ) *tt++ = *t++;
	termcopy[0] = tt - termcopy;
	if ( flipnames ) EXCH(edge,vert)
	nvert2 = nvert*nvert;
/*
	Sort the momenta. Keep the sign order.
*/
	for ( i = 0; i < nmomenta-3; i+=3 ) {
		jj = i;
		while ( jj >= 0 && momenta[jj+4] > momenta[jj+1] ) {
			EXCH(momenta[jj+5],momenta[jj+2])
			EXCH(momenta[jj+4],momenta[jj+1])
			EXCH(momenta[jj+3],momenta[jj])
			jj -= 3;
		}
	}
/*
	Step two: make now the edge functions in the proper notation.
*/
	t = vet+1;
	for ( i = 0; i < nmomenta; i += 6 ) {
		if ( momenta[i] == -VECTOR && momenta[i+3] == -MINVECTOR
			 && momenta[i+1] == momenta[i+4] ) {
		}
		else if ( momenta[i] == -MINVECTOR && momenta[i+3] == -VECTOR
			 && momenta[i+1] == momenta[i+4] ) {
			flips[nflips++] = momenta[i+1];
			DUMMYUSE(flips[nflips-1]);
		}
		else { /* something wrong with the momenta */
			MLOCK(ErrorMessageLock);
			MesPrint("No momentum conservation or wrong momenta in Canonicalize statement");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		*t++ = EDGE; *t++ = FUNHEAD+10; FILLFUN(t)
		*t++ = -SNUMBER; *t++ = momenta[i+2];
		*t++ = -SNUMBER; *t++ = momenta[i+5];
		*t++ = -VECTOR; *t++ = momenta[i+1];
		*t++ = -SNUMBER; *t++ = 0; /* provisional power/color, multiple of ep */
		*t++ = -SNUMBER; *t++ = 0; /* provisional power/color, integer */
	}
	tend = t;
	*t++ = 1; *t++ = 1; *t++ = 3; vet[0] = t-vet; *t = 0;
/*
	Now the powers of the denominators
*/
	tstop = termcopy+*termcopy; tstop -= ABS(tstop[-1]); td = termcopy+1;
	while ( td < tstop ) {
		if ( *td == edge && td[1] == FUNHEAD+4 ) {
			if ( td[FUNHEAD+2] == -SNUMBER && ( td[FUNHEAD] == -VECTOR || td[FUNHEAD] == -INDEX
			|| td[FUNHEAD] == -MINVECTOR ) ) {}
			else {
				MLOCK(ErrorMessageLock);
				MesPrint("Illegal argument in edge function in Canonicalize statement");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			tt = vet+1;
			while ( tt < tend ) {
				if ( tt[FUNHEAD+5] == td[FUNHEAD+1] ) { tt[FUNHEAD+7] = td[FUNHEAD+3]; break; }
				tt += tt[1];
			}
		}
		else if ( *td == DOTPRODUCT ) break;
		td += td[1];
	}
	if ( td < tstop ) {
		tt = vet+1;
		while ( tt < tend ) {
/*
			tt[FUNHEAD+5] is a vector. We look for dotproducts with twice tt[FUNHEAD+5]
*/
			for ( i = 2; i < td[1]; i += 3 ) {
				if ( td[i] == tt[FUNHEAD+5] && td[i+1] == tt[FUNHEAD+5] ) {
					tt[FUNHEAD+9] = td[i+2];
					break;
				}
			}
			tt += tt[1];
		}
	}
	Normalize(BHEAD vet);	
/*
	Now we have a term `vet' in the proper notation and we can start.
	To keep track of the shattering we use an array of 2*nvert.
	each entry is Number,marker
	When the marker is zero, the vertices are in the same partition.
	For the environment we need a matrix that is nvert x nvert
	At the same time we keep the connectivity matrix, because that will
	save much time later.
	The partitions are stored in a matrix as well. This allows them to be
	treated as a stack. The entries are separated by 0 if they belong to
	the same part, and by a 1 when they belong to different parts.
*/
	partition = AT.WorkPointer; AT.WorkPointer += 2*nvert2;
	for ( i = 0; i < nvert; i++ ) { partition[2*i] = i; partition[2*i+1] = 0; }
	partition[2*i-1] = -1;  /* end of the first part which is currently all vertices */
	nparts = 1;

	connect = AT.WorkPointer; AT.WorkPointer += nvert2;
	for ( i = 0; i < nvert2; i++ ) connect[i] = 0;
	tstop = vet+*vet; tstop -= ABS(tstop[-1]); t = vet+1;
	while ( t < tstop ) {
		if ( *t == EDGE ) {
			connect[t[FUNHEAD+1]*nvert+t[FUNHEAD+3]]++;
			connect[t[FUNHEAD+3]*nvert+t[FUNHEAD+1]]++;
		}
		t += t[1];
	}
for ( i = 0; i < nvert; i++ ) {
MesPrint("connectivity: %d -- %a",i,nvert,connect+i*nvert);
}
/*
	Create the environment matrix and sort it.
*/
	environ = AT.WorkPointer; AT.WorkPointer += nvert2;
/*
	And now the refinement process starts.
*/
	WantAddPointers(nvert+1);
	for ( i = 0; i < nvert2; i++ ) environ[i] = 0;
	level = 0;
	pparts = partition;
	while ( nparts < nvert ) {
		nparts = DoShattering(BHEAD connect,environ,pparts,nvert);
		if ( nparts < nvert ) { /* raise level and make a copy and split a part */
			p = pparts + 2*nvert;
			level++; 
			for ( i = 0; i < 2*nvert; i++ ) p[i] = pparts[i];
			for ( ii = 0; ii < 2*nvert; ii += 2 ) {
				if ( p[ii+1] == 0 ) { /* found a part with more than one */
					num = 2; i = ii+2;
					while ( p[i+1] == 0 ) { num++; i += 2; }


					p[ii+1] = -1; pparts = p;
					break;
				}
			}


		}
MesPrint("partition: %d -- %a",nparts,2*nvert,pparts);

	}
/*
	Just for now
*/
	PutTermInDollar(vet,args[0]);

	
	TermFree(vet,"TopologyCanonize2");
	TermFree(termcopy,"TopologyCanonize1");
	AT.WorkPointer = oldworkpointer;
	return(0);
}

/*
  	#] DoTopologyCanonicalize : 
  	#[ DoShattering :
*/

int DoShattering(PHEAD WORD *connect, WORD *environ, WORD *partitions, WORD nvert)
{
	int nparts, i, j, ii, jj, iii, jjj, newmarker;
	WORD **p = AT.pWorkSpace + AT.pWorkPointer, *part, *endpart;
	WORD *poin1, *poin2;
#ifdef SHATBUG
MesPrint("Entering DoShattering. partitions = %a",2*nvert,partitions);
#endif
restart:
/*
	Determine the number of parts
	p will be an array with pointers to the parts.
	We made space for this array in the calling routine and because this
	routine is not calling any other routines we do not need to raise
	the pointer in this stack (AT.pWorkPointer).
*/
	nparts = 0; newmarker = 0;
	part = partitions; endpart = part + 2*nvert;
	p[0] = part;
	while ( part < endpart ) {
		if ( part[1] != 0 ) { p[++nparts] = part+2; }
		part += 2;
	}
	for ( i = 0; i < nparts; i++ ) 
		AT.WorkPointer[i] = (p[i+1]-p[i])/2;
#ifdef SHATBUG
MesPrint("DoShattering: calculated the pointers");
MesPrint("DoShattering: sizes: %a",nparts,AT.WorkPointer);
MesPrint("DoShattering: p[0]: %a, p[1]: %a",6,p[0],6,p[1]);
#endif
	for ( i = 0; i < nparts; i++ ) {
	 if ( AT.WorkPointer[i] > 1 ) {
	  for ( j = 0; j < nparts; j++ ) {
/*
		Shatter part i wrt part j.
		if there is action, go to restart.
*/			
		for ( ii = 0; ii < AT.WorkPointer[i]; ii++ ) {
		  for ( jj = 0; jj < AT.WorkPointer[j]; jj++ ) {
			environ[ii*AT.WorkPointer[j]+jj] += connect[p[i][2*ii]*nvert+p[j][2*jj]];
		  }
		}
#ifdef SHATBUG
for ( ii = 0; ii < AT.WorkPointer[i]; ii++ ) {
MesPrint("Environ(%d,%d): %a",i,j,AT.WorkPointer[j],environ+ii*AT.WorkPointer[j]);
}
#endif
/*
		Sort the rows internally, then sort the rows wrt each other
		and finally place new markers. If a new marker, we restart.
		Don't forget to clean up the environ array.
*/
		for ( ii = 0; ii < nvert; ii++ ) {
		  poin1 = environ+ii*AT.WorkPointer[j];
		  for ( jj = 0; jj < AT.WorkPointer[j]-1; jj++ ) {
			jjj = jj;
			while ( jjj >= 0 && poin1[jjj+1] > poin1[jjj] ) {
				EXCH(poin1[jjj+1],poin1[jjj])
				jjj--;
			}
		  }
		}
#ifdef SHATBUG
for ( ii = 0; ii < AT.WorkPointer[i]; ii++ ) {
MesPrint("environ(%d,%d): %a",i,j,AT.WorkPointer[j],environ+ii*AT.WorkPointer[j]);
}
#endif
		for ( ii = 0; ii < AT.WorkPointer[i]-1; ii++ ) {
			poin2 = environ+ii*AT.WorkPointer[j]; poin1 = poin2+AT.WorkPointer[j];
			iii = ii;
			while ( iii >= 0 && ( CmpArray(poin1,poin2,AT.WorkPointer[j]) < 0 ) ) {
				EXCHN(poin2,poin1,AT.WorkPointer[j])
				EXCH(p[i][2*iii+2],p[i][2*iii])
				iii--; poin1 = poin2; poin2 = poin1-AT.WorkPointer[j];
			}
		}
#ifdef SHATBUG
for ( ii = 0; ii < AT.WorkPointer[i]; ii++ ) {
MesPrint("environ(%d,%d): %a",i,j,AT.WorkPointer[j],environ+ii*AT.WorkPointer[j]);
}
MesPrint("partitions = %a",2*nvert,partitions);
#endif
		for ( ii = 0; ii < AT.WorkPointer[i]-1; ii++ ) {
			poin2 = environ+ii*AT.WorkPointer[j]; poin1 = poin2+AT.WorkPointer[j];
			if ( CmpArray(poin1,poin2,AT.WorkPointer[j]) == 0 ) continue;
			p[i][2*ii+1] = -1; nparts++; newmarker++;
		}
#ifdef SHATBUG
MesPrint("partitions = %a",2*nvert,partitions);
#endif
/*
		Clear environ. This is probably faster than just clearing the whole array.
		Maybe in the future a test could be done on nvert to decide how to clear.
*/
		for ( ii = 0; ii < AT.WorkPointer[i]; ii++ ) {
		  for ( jj = 0; jj < AT.WorkPointer[j]; jj++ ) {
			environ[ii*AT.WorkPointer[j]+jj] = 0;
		  }
		}
		if ( newmarker ) { goto restart; }
	  }
	 }
	}
	return(nparts);
}

/*
  	#] DoShattering : 
*/
