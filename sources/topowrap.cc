/** @file topowrap.cc
 *
 *	routines for conversion of topology and diagram output to FORM notation
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
/*
  	#] License : 
  	#[ includes :
*/

extern "C" {
#include "form3.h"
}
 
#include "gentopo.h"

//using namespace std;
 
#define MAXPOINTS 120

typedef struct ToPoTyPe {
	int cldeg[MAXPOINTS], clnum[MAXPOINTS], clext[MAXPOINTS];
	int ncl, nloops, nlegs, npadding;
	WORD *vert;
	WORD *vertmax;
	WORD nvert;
	WORD sopi;
} TOPOTYPE;

/*
  	#] includes : 
  	#[ GenerateVertices :

	Routine is to be used recursively to work its way through a list
	of possible vertices. The array of vertices is in TopoInf->vert
	with TopoInf->nvert the number of possible vertices.
	Currently we allow in TopoInf->vert only vertices with 3 or more edges.

	We work with a point system. Each n-point vertex contributes n-2 points.
	When all points are assigned, we can call mgraph->generate().

	The number of vertices of a given number of edges is stored in
	TopoInf->clnum[..] but the loop that determines how many there are
	may be limited by the corresponding element in TopoInf->vertmax[level]
*/

int GenerateVertices(TOPOTYPE *TopoInf, int pointsremaining, int level)
{
	int i, j;

	for ( i = pointsremaining, j = 0; i >= 0; i -= TopoInf->vert[level]-2, j++ ) {
		if ( TopoInf->vertmax && TopoInf->vertmax[level] >= 0
					 && j > TopoInf->vertmax[level] ) break;
		if ( i == 0 ) { // We got one!
		    MGraph *mgraph;
			TopoInf->cldeg[TopoInf->ncl] = TopoInf->vert[level];
			TopoInf->clnum[TopoInf->ncl] = j;
			TopoInf->clext[TopoInf->ncl] = 0;
			TopoInf->ncl++;

			mgraph = new MGraph(0, TopoInf->ncl, TopoInf->cldeg,
					     TopoInf->clnum, TopoInf->clext, TopoInf->sopi);

		    mgraph->generate();

			delete mgraph;

			TopoInf->ncl--;

			break;
		}
		if ( level < TopoInf->nvert-1 ) {
			if ( j > 0 ) {
				TopoInf->cldeg[TopoInf->ncl] = TopoInf->vert[level];
				TopoInf->clnum[TopoInf->ncl] = j;
				TopoInf->clext[TopoInf->ncl] = 0;
				TopoInf->ncl++;
			}
			if ( GenerateVertices(TopoInf,i,level+1) < 0 ) return(-1);
			if ( j > 0 ) { TopoInf->ncl--; }
		}
	}
	return(0);
}

/*
  	#] GenerateVertices : 
  	#[ GenerateTopologies :

	Note that setmax, option1 and option2 are optional.
	Default values are -1,0,0

	vert is a pointer to a set of numbers indicating the types of vertices
	     that are allowed.
	nvert is the number of elements in vert.
*/

WORD GenerateTopologies(PHEAD WORD nloops, WORD nlegs, WORD setvert, WORD setmax)
{
	TOPOTYPE TopoInf;
	int i, points, identical = 0;
	DUMMYUSE(AT.nfac);

	if ( nlegs == -2 ) {
		nlegs = 2;
		identical = 1;
	}
	TopoInf.vert = &(SetElements[Sets[setvert].first]);
	TopoInf.nvert = Sets[setvert].last-Sets[setvert].first;

	if ( setmax >= 0 ) TopoInf.vertmax = &(SetElements[Sets[setmax].first]);
	else               TopoInf.vertmax = 0;

//	point counting: an n-point vertex counts for n-2 points.

	points = 2*nloops-2+nlegs;
	if ( points >= MAXPOINTS ) {
		MLOCK(ErrorMessageLock);
		MesPrint("GenerateTopologies: %d loops and %d legs considered excessive",nloops,nlegs);
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

//	First the external nodes.

	for ( i = 0; i < nlegs; i++ ) {
		TopoInf.cldeg[i] = 1; TopoInf.clnum[i] = 1; TopoInf.clext[i] = 1;
	}
	if ( identical == 1 ) {	/* Only propagator topologies..... */
		nlegs = 1;
		TopoInf.clnum[0] = 2;
	}
	TopoInf.ncl = nlegs;
	TopoInf.sopi = 1;  // For now

	if ( GenerateVertices(&TopoInf,points,0) != 0 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Called from GenerateTopologies with %d loops and %d legs considered excessive",nloops,nlegs);
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	return(0);
}

/*
  	#] GenerateTopologies : 
  	#[ toForm :
*/

//==============================================================
// Output for FROM called by genTopo each time a new graph is
// generated
//
void toForm(EGraph *egraph)
{
	GETIDENTITY;
//
//	skipext : boolean; whether to skip external node/line in the output.
//
//	const int skipext = 1;    

	int n, lg, ed, i, fromset;

	WORD *termout = AT.WorkPointer;
	WORD *t, *tt, *ttstop, *ttend, *ttail;

	t = termout+1;

//	First pick up part of the original term

	tt = AT.TopologiesTerm + 1;
	i = AT.TopologiesStart - tt;
	NCOPY(t,tt,i)
	ttail = tt+tt[1];

//	Now the vertices/nodes
//	Options are in AT.TopologiesOptions[]

	for ( n = 0; n < egraph->nNodes; n++ ) {
		if ( ( AT.TopologiesOptions[1] &1 ) == 1 && egraph->nodes[n].ext ) continue;
		tt = t;
		*t++ = VERTEX; t++; FILLFUN(t);
		*t++ = -SNUMBER; *t++ = n;
		for ( lg = 0; lg < egraph->nodes[n].deg; lg++ ) {
			ed = egraph->nodes[n].edges[lg];
			if ( ed >= 0 ) { *t++ = -VECTOR; }
			else { ed = -ed; *t++ = -MINVECTOR; }

//			Now we need to pick up the proper set element.

			fromset =  egraph->edges[ed].ext ? AT.setexterntopo : AT.setinterntopo;
			*t++ = SetElements[Sets[fromset].first+egraph->edges[ed].momn-1];
		}
		tt[1] = t-tt;
	}

	if ( ( AT.TopologiesOptions[0] & 1 ) == 1 ) {
//		Note that the edges count from 1.
		for ( n = 1; n <= egraph->nEdges; n++ ) {
			if ( ( AT.TopologiesOptions[1] & 1 ) == 1 && egraph->edges[n].ext ) continue;
			tt = t;
			*t++ = EDGE; t++; FILLFUN(t);
			*t++ = -SNUMBER; *t++ = egraph->edges[n].nodes[0];
			*t++ = -SNUMBER; *t++ = egraph->edges[n].nodes[1];
			*t++ = -VECTOR;
			fromset =  egraph->edges[n].ext ? AT.setexterntopo : AT.setinterntopo;
			*t++ = SetElements[Sets[fromset].first+egraph->edges[n].momn-1];
			tt[1] = t-tt;
		}
	}

//	Now the tail end

	ttend = AT.TopologiesTerm; ttend = ttend+ttend[0];
	ttstop = ttend - ABS(ttend[-1]);
	i = ttstop - ttail;
	NCOPY(t,ttail,i)

//	Finally the coefficient
//	The topological coefficient should be in egraph->wsum, egraph->nwsum
//	as a FORM Long number

#ifdef WITHFACTOR
	if ( egraph->nwsum == 1 && egraph->wsum[0] == 1 ) {
		while (ttstop < ttend ) *t++ = *ttstop++;
	}
	else {
		WORD newsize;
		newsize = ttend[-1];
		newsize = REDLENG(newsize);
		if ( Divvy(BHEAD (UWORD *)ttstop,&newsize,egraph->wsum,egraph->nwsum) )
			goto OnError;
		newsize = INCLENG(newsize);
		i = ABS(newsize)-1;
		NCOPY(t,ttstop,i)
		*t++ = newsize;
	}
#else
	while (ttstop < ttend ) *t++ = *ttstop++;
#endif
	*termout = t - termout;

	AT.WorkPointer = t;

	if ( Generator(BHEAD termout,AT.TopologiesLevel) < 0 ) {
// OnError:
		MLOCK(ErrorMessageLock);
		MesPrint("Called from the topologies routine toForm");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	AT.WorkPointer = termout;
}

/*
  	#] toForm : 
*/

