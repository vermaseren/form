// { ( [

#include <iostream>
#include <iomanip>
#include <sstream>
#include <cstdio>
#include <stdlib.h>

/*
#ifdef NOFORM
#else
extern "C" {
#include "form3.h"
}
#endif
*/

#define DEFGLOBAL

#include "gentopo.h"

using namespace std;

#define DEBUGM 0

// Generate scalar connected Feynman graphs.

//==============================================================
// compile options
const int CHECK    = True;
const int MONITOR  = False;

const int OPTPRINT = 0;

const int DEBUG0 = False;
const int DEBUG1 = False;
const int DEBUG  = False;

// for debugging memory use

//==============================================================
class MGraph;
class EGraph;
class Options;

#define Extern 
#define Global
#define Local 

// Macro functions
#define CLWIGHTD(x)   (5*(x))
#define CLWIGHTO(x)   (3*(x)-2)

// External functions
Extern void toForm(EGraph *egraph);
Extern int  countPhiCon(int ex, int lp, int v4);

// Temporal definition: they should be replaced by FROM functions.
Local BigInt factorial(int n);
Local BigInt ipow(int n, int p);

// Local functions
Local int nextPerm(int nelem, int nclass, int *cl, int *r, int *q, int *p, int count);

Local void erEnd(const char *msg);
Local int  *newArray(int size, int val);
Local void deletArray(int *a);
Local void copyArray(int size, int *a0, int *a1);
Local int  **newMat(int n0, int n1, int val);
Local void deleteMat(int **m, int n0);
Local void printArray(int n, int *p);
Local void printMat(int n0, int n1, int **m);
Local void bsort(int n, int *ord, int *a);
Local void prMomStr(int mom, const char *ms, int mn);

#if DEBUGM
static int countNMC = 0;
static int countEG = 0;
static int countMG = 0;
#endif

static const char *optnames[OPT_Size] = OPT_NAMES;
static int         optdefaults[OPT_Size] = OPT_DEFAULTS;

static const char *egndnames[EG_ND_Size] = EG_ND_NAMES;
static const char *egednames[EG_ED_Size] = EG_ED_NAMES;

//==============================================================
// class Options
//--------------------------------------------------------------
static int optcall = 0;

Options::Options()
{
    int j;

    if (optcall == 0) {
        OPT_names = optnames;
    }
    optcall++;

    // defualt values
    for (j = 0; j < OPT_Size; j++) {
        values[j] = optdefaults[j];
    }
}

//--------------------------------------------------------------
void Options::copy(Options *opt)
{
    int j;

    for (j = 0; j < OPT_Size; j++) {
        values[j] = opt->values[j];
    }
}

//--------------------------------------------------------------
void Options::print()
{
    int j;
    
    printf("Options\n");
    for (j=0; j < OPT_Size; j++) {
        printf("        %-50s : %2d (%2d)\n", 
               OPT_names[j], values[j], optdefaults[j]);
    }
}

//==============================================================
// class ENode
//--------------------------------------------------------------
ENode::ENode()
{ 
    klow  = NULL;
    edges = NULL;
    deg   = 0;
}

//--------------------------------------------------------------
#if 0
ENode::ENode(int loops, int maxdeg)
{ 
    edges = new int[maxdeg]; 
    klow  = new int[loops+1]; 
    deg   = 0;
}
#endif

//--------------------------------------------------------------
ENode::~ENode()
{ 
    if (klow != NULL) {
        delete[] klow; 
        delete[] edges;
    }
}

//--------------------------------------------------------------
void ENode::copy(ENode *en)
{ 
    int d;

    deg = en->deg;
    extloop = en->extloop;
    ndtype  = en->ndtype;
    for (d = 0; d < deg; d++) {
        edges[d] = en->edges[d];
    }
}

//--------------------------------------------------------------
void ENode::init(int loops, int maxdeg)
{ 
    deg   = 0;
    if (klow != NULL) {
        return;
    }
    edges = new int[maxdeg]; 
    klow  = new int[loops+1]; 
}

//--------------------------------------------------------------
void ENode::setType(int typ)
{ 
    if (ndtype != EG_ND_Undef && ndtype != typ) { 
        printf("*** ndtype is already defined : old=%d, new = %d\n",
               ndtype, typ);
        erEnd("ndtype is already defined");
    }
    ndtype = typ;
}


//==============================================================
// class EEdge
//--------------------------------------------------------------
EEdge::EEdge()
{
    emom = NULL;
    lmom = NULL;
}

//--------------------------------------------------------------
EEdge::~EEdge()
{
    if (lmom != NULL) {
        delete[] lmom;
    }
    if (emom != NULL) {
        delete[] emom;
    }
}

//--------------------------------------------------------------
void EEdge::copy(EEdge *ee)
{
    nodes[0] = ee->nodes[0];
    nodes[1] = ee->nodes[1];
    ext      = ee->ext;
    momn     = ee->momn;
    dir      = ee->dir;
    edtype   = ee->edtype;
}

//--------------------------------------------------------------
void EEdge::init(int nedges, int nloops)
{
    if (emom != NULL || lmom != NULL) {
        erEnd("*** emom or lmom is not NULL");
    }
    emom = new int[nedges+1];
    lmom = new int[nloops];
}

//--------------------------------------------------------------
void EEdge::setType(int typ)
{ 
    if (edtype != EG_ED_Undef) {
        erEnd("*** edtype is already defined");
    }
    edtype = typ;
}

//--------------------------------------------------------------
void EEdge::setEMom(int nedges, int *em, int dir)
{
    int e;

    for (e = 1; e <= nedges; e++) {
        if (em[e] != 0) {
            emom[e] = dir;
        }
    }
}

//--------------------------------------------------------------
void EEdge::setLMom(int k, int dir)
{
    lmom[k] = dir;
}

//==============================================================
// class EGraph
//--------------------------------------------------------------
static int egraphcall = 0;

EGraph::EGraph(int nnodes, int nedges, int mxdeg)
{
    // Arguments
    //   nnodes : the number of nodes
    //   nedges : the number of edges
    //   mxdeg  : the maximum number of degrees of nodes

    int j;

    mgraph = NULL;
    if (egraphcall == 0) {
        EG_ND_names = egndnames;
        EG_ED_names = egednames;
    }
    egraphcall++;

    sNodes  = nnodes;
    sEdges  = nedges;
    sMaxdeg = mxdeg;
    sLoops  = sEdges - sNodes + 1;   // assume connected graph

    pId     = -1;
    gId     = -1;
    gSubId  = -1;

    nNodes  = sNodes;
    nEdges  = sEdges;
    nLoops  = 0;
    nExtern = 0;

    nodes = new ENode[sNodes];
    for (j = 0; j < sNodes; j++) {
        nodes[j].init(sNodes, sMaxdeg);
    }
    edges = new EEdge[sEdges+1];
    for (j = 0; j <= sEdges; j++) {
        edges[j].init(sEdges, sLoops);
    }

    bidef   = new int[sNodes];
    bilow   = new int[sNodes];
    bicount = -1;

    extMom  = new int[sEdges+1];

#if DEBUGM
    printf("+++ new    EGraph %d\n", ++countEG);
    if(countEG > 100) { exit(1); }
#endif
}

//--------------------------------------------------------------
EGraph::~EGraph()
{
    delete[] extMom;

    delete[] bilow;
    delete[] bidef;

    delete[] nodes;
    delete[] edges;

#if DEBUGM
    printf("+++ delete EGraph %d\n", countEG--);
#endif
}

//--------------------------------------------------------------
void EGraph::copy(EGraph *eg)
{
    int nd, ed;

    if (sNodes < eg->nNodes || sEdges < eg->nEdges || 
        sMaxdeg < eg->sMaxdeg || sLoops < eg->nLoops) {
        erEnd("EGraph::copy: sizes are too small");
    }
    mgraph = eg->mgraph;
    options.copy(&(eg->options));

    pId     = eg->pId;
    gId     = eg->gId;
    gSubId  = eg->gSubId;
    nNodes  = eg->nNodes;
    nEdges  = eg->nEdges;
    nExtern = eg->nExtern;
    nLoops  = eg->nLoops;

    for (nd = 0; nd < nNodes; nd++) {
        nodes[nd].copy(&(eg->nodes[nd]));
    }
    for (ed = 0; ed <= nEdges; ed++) {
        edges[ed].copy(&(eg->edges[ed]));
    }

    nsym    = eg->nsym;
    esym    = eg->esym;
    
    bicount = -1;
}

//--------------------------------------------------------------
void EGraph::setExtLoop(int nd, int val)
{
    // set the node 'nd' being an external node (-1) or a looped vertex

    nodes[nd].extloop = val;
}

//--------------------------------------------------------------
void EGraph::endSetExtLoop(void)
{
    // end of calling 'setExtLoop'.

    int n;

    nExtern = 0;
    for (n = 0; n < nNodes; n++) {
        if (isExternal(n)) {
            nExtern++;
        }
    }
}

//--------------------------------------------------------------
void EGraph::fromMGraph(MGraph *mg)
{
    // construct EGraph from adjacency matrix.
    // This function should be called after
    //   EGraph(), setExtLoop() and endSetExtLoop().

    int n0, n1, ed, e, eext, eint;

    mgraph = mg;
    if (sNodes < mg->nNodes || sEdges < mg->nEdges || sMaxdeg < mg->maxdeg) {
        erEnd("EGraph::fromMGraph: sizes are too small");
    }
    if (sLoops < mg->nLoops) {
        printf("too small sLoops : %d < %d\n", sLoops, mg->nLoops);
        erEnd("too small sLoops");
    }
    nNodes  = mg->nNodes;
    nEdges  = mg->nEdges;
    nLoops  = mg->nLoops;
    nExtern = mg->nExtern;
    pId     = mg->pId;
    gId     = mg->cDiag;
    gSubId  = 0;
    nsym    = mg->nsym;
    esym    = mg->esym;
    options.copy(&(mg->options));

    for (n0 = 0; n0 < nNodes; n0++) {
        nodes[n0].deg = 0;
    }
    ed = 1;
    for (n0 = 0; n0 < nNodes; n0++) {
        for (e = 0; e < mg->adjMat[n0][n0]/2; e++, ed++) {
            edges[ed].nodes[0] = n0;
            edges[ed].nodes[1] = n0;
            nodes[n0].edges[nodes[n0].deg++] = - ed;
            nodes[n0].edges[nodes[n0].deg++] =   ed;
            edges[ed].ext = isExternal(n0);
        }
        for (n1 = n0+1; n1 < nNodes; n1++) {
            for (e = 0; e < mg->adjMat[n0][n1]; e++, ed++) {
                edges[ed].nodes[0] = n0;
                edges[ed].nodes[1] = n1;
                nodes[n0].edges[nodes[n0].deg++] = - ed;
                nodes[n1].edges[nodes[n1].deg++] =   ed;
                edges[ed].ext = (isExternal(n0) || isExternal(n1));
            }
        }
    }
    if (CHECK) {
        if (ed != nEdges+1) {
            printf("*** EGraph::init: ed=%d != nEdges=%d\n", ed, nEdges);
            erEnd("*** EGraph::init: illegal connection");
        }
    }

    // name of momenta
    eext = 1;
    eint = 1;
    for (ed = 1; ed <= nEdges; ed++) {
        if (edges[ed].ext) {
            edges[ed].momn = eext++;
        } else {
            edges[ed].momn = eint++;
        }
        edges[ed].cut = False;
    }

    bicount = -1;

}

//--------------------------------------------------------------
void EGraph::print()
{
    // print the EGraph
  
    int  nd, lg, ed, nlp, n, k;
    Bool zero;
  
    nlp = nEdges - nNodes + 1;
    printf("\n");
    printf("EGraph: pId=%d gId=%ld gSubId=%ld, nExtern=%d nLoops=%d nNodes=%d nEdges=%d\n",
           pId, gId, gSubId, nExtern, nlp, nNodes, nEdges);
    printf("        sym = (%ld * %ld) \n", nsym, esym);
    printf("  Nodes\n");
    for (nd = 0; nd < nNodes; nd++) {
        if (isExternal(nd)) {
            printf("    %2d Extern ", nd);
        } else {
            printf("    %2d Vertex ", nd);
        }
        printf("deg=%d, %-8s [", nodes[nd].deg, 
               EG_ND_names[nodes[nd].ndtype]);
        for (lg = 0; lg < nodes[nd].deg; lg++) {
            printf(" %3d", nodes[nd].edges[lg]);
        }
        printf("]\n");
    }
    printf("  Edges\n");
    for (ed = 1; ed <= nEdges; ed++) {
        if (edges[ed].ext) {
            printf("    %2d Extern ", ed);
        } else {
            printf("    %2d Intern ", ed);
        }
        printf("%-9s ", EG_ED_names[edges[ed].edtype]);
        printf(" [%3d %3d]: ", edges[ed].nodes[1], edges[ed].nodes[0]);
        printf("c%2d: ", edges[ed].opicomp);
        printf("%s%d =", (edges[ed].ext)?"Q":"p", edges[ed].momn);
        zero = True;
        for (n = 1; n <= nEdges; n++) {
            if (edges[n].ext) {
                if (edges[ed].emom[n] != 0) {
                    prMomStr(edges[ed].emom[n], "Q", n);
                    zero = False;
                }
            }
        }
        for (k = 0; k < nLoops; k++) {
            if (edges[ed].lmom[k] != 0) {
                prMomStr(edges[ed].lmom[k], "k", k);
                zero = False;
            }
        }
        if (zero) {
            printf(" 0");
        }
        printf("\n");
    }
    if (bicount > 0) {
        printf("+++ gId=%ld,gSubId=%ld,nopicomp=%d, nopi2p=%d, opi2plp=%d\n",
               gId, gSubId, nopicomp, nopi2p, opi2plp);
    }
}

//--------------------------------------------------------------
int EGraph::dirEdge(int n, int e)
{
    if (nodes[n].edges[e] > 0) {
        return 1;
    } else {
        return -1;
    }
}

//--------------------------------------------------------------
int EGraph::cmpMom(int *lm0, int *em0, int *lm1, int *em1)
{
    int lp, ed, cmp, sgn = 0;

    for (lp = 0; lp < nLoops; lp++) {
        if (lm0[lp] != 0) {
            if (lm1[lp] == 0) {
                return 1;
            } else if (sgn == 0) {
                sgn = lm0[lp]*lm1[lp];   // cancel first non-zero elements
                if (abs(sgn) != 1) {
                    erEnd("cmpMom : illegal element of lmom");
                }
            } else {
                cmp = lm0[lp] - sgn*lm1[lp];
                if (cmp != 0) {
                    return cmp;
                }
            }
        } else if (lm1[lp] != 0) {   // lm0[lp] == 0
            return -1;
        }
    }
    for (ed = 1; ed <= nEdges; ed++) {
        if (em0[ed] != 0) {
            if (em1[ed] == 0) {
                return 1;
            } else if (sgn == 0) {
                sgn = em0[ed]*em1[ed];   // cancel first non-zero elements
                if (abs(sgn) != 1) {
                    erEnd("cmpMom : illegal element of emom");
                }
            } else {
                cmp = em0[ed] - sgn*em1[ed];
                if (cmp != 0) {
                    return cmp;
                }
            }
        } else if (em1[ed] != 0) {   // em0[ed] == 0
            return -1;
        }
    }
    return 0;
}

//--------------------------------------------------------------
int EGraph::groupLMom(int *grp, int *ed2gr)
{
    //  grp[g] : the number of elements in the group g (in [0,...,(ngrp-1)])
    //  ed2gr[ed] : group number of edge ed.

    int ed0, ed1, cmp, ngrp = -1;

    grp[0] = 0;
    for (ed0 = 1; ed0 <= nEdges; ed0++) {
        ed2gr[ed0] = -1;
        grp[ed0]   = 0;
    }

    for (ed0 = 1; ed0 <= nEdges; ed0++) {
        if (ed2gr[ed0] < 0) {
            ngrp++;
            ed2gr[ed0] = ngrp;
            grp[ngrp]++;
            for (ed1 = ed0+1; ed1 <= nEdges; ed1++) {
                cmp = cmpMom(edges[ed0].lmom, edges[ed0].emom, 
                             edges[ed1].lmom, edges[ed1].emom);
                if (cmp == 0) {
                    ed2gr[ed1] = ngrp;
                    grp[ngrp]++;
                }
            }
        }
    }
    ngrp++;

    return ngrp;
}

//--------------------------------------------------------------
Bool EGraph::isOptE(void)
{
    EGraph edupv = EGraph(sNodes, sEdges, sMaxdeg);
    EGraph *edup = &edupv;
    int    grp[nEdges+1], ed2gr[nEdges+1], ngrp;
    int    g, ed;
    Bool   ok;
    int    minopi2p;

    biconnE();
    if (DEBUG) {
        print();
    }

    if (options.values[OPT_No2PtL1PI] == 0) {
        return True;
    }

    if (nExtern == 2 && nopicomp == 1) {
        minopi2p = 2;
    } else {
        minopi2p = 1;
    }

    if (opi2plp > 0 && nopi2p >= minopi2p) {
        if (options.values[OPT_No2PtL1PI] > 0) {
            if (DEBUG) {
                printf("isOptE:1:False\n");
            }
            return False;
        } else if (options.values[OPT_No2PtL1PI] < 0) {
            if (DEBUG) {
                printf("isOptE:1:True\n");
            }
            return True;
        }
    }

    edup->copy(this);
    ngrp = groupLMom(grp, ed2gr);

    
    for (g = 0; g < ngrp; g++) {
        ok = True;
        if (grp[g] < 2) {
            continue;
        }
        for (ed = 1; ed <= nEdges; ed++) {
            if (ed2gr[ed] == g) {
                if (edges[ed].ext) {
                    ok = False;
                    break;
                }
                edup->edges[ed].cut = True;
            } else {
                edup->edges[ed].cut = False;
            }
        }
        if (ok) {
            edup->gSubId++;
            edup->biconnE();
            if (DEBUG) {
                edup->print();
            }

            if (edup->nExtern == 2 && edup->nopicomp == 1) {
                minopi2p = 2;
            } else {
                minopi2p = 1;
            }

            if (edup->opi2plp > 0 && edup->nopi2p >= minopi2p) {
                if (options.values[OPT_No2PtL1PI] > 0) {
                    if (DEBUG) {
                        printf("isOptE:2:False\n");
                    }
                    return False;
                } else if (options.values[OPT_No2PtL1PI] < 0) {
                    if (DEBUG) {
                        printf("isOptE:2:True\n");
                    }
                    return True;
                }
            }
        }
    }

    if (options.values[OPT_No2PtL1PI] > 0) {
        if (DEBUG) {
            printf("isOptE:3:True\n");
        }
        return True;
    } else if (options.values[OPT_No2PtL1PI] < 0) {
        if (DEBUG) {
            printf("isOptE:3:False\n");
        }
        return False;
    } else {
        erEnd("isOptE: illegal control");
        return False;
    }
}

//--------------------------------------------------------------
int EGraph::findRoot(void)
{
    int root, vr, er, e, nd0, nd1;

    root = -1;
    vr   = -1;
    er   = -1;
    for (e = 1; e <= nEdges; e++) {
        if (edges[e].cut || edges[e].edtype == EG_ED_Deleted) {
            continue;
        }
        if (edges[e].visited) {
            continue;
        }
        if (root < 0) {
            if (edges[e].ext) {
                nd0 = edges[e].nodes[0];
                nd1 = edges[e].nodes[1];
                if (isExternal(nd0) && !isExternal(nd1)) {
                    root = nd1;
                    break;
                } else if (!isExternal(nd0) && isExternal(nd1)) {
                    root = nd0;
                    break;
                }
            }
        }
        if (er < 0) {
            er = edges[e].nodes[0];
        }
        if (vr < 0) {
            vr = edges[e].nodes[0];
        }
    }
    // if root >= 0 : vertex adjacent to an external node
    if (root < 0) {
        // if vr >= 0 : a vertex
        if (vr >= 0) {
            root = vr;
        
        // no vertex, only external nodes;
        } else {
            root = er;
        }
    }
    return root;
}

//--------------------------------------------------------------
void EGraph::biconnE(void)
{
    int e, ie, root, n;
    int extlst[nEdges+1], intlst[nEdges+1];
    int opiext, opiloop;

    biinitE();
    bconn = 0;      // the number of connected components

    for (e = 1; e <= nEdges; e++) {
        root = findRoot();   // root of a spanning tree to be searched.
        if (root < 0) {
            break;
        }
        bisearchE(root, extlst, intlst, &opiext, &opiloop);
        opi2plp = max(opi2plp, opiloop);
        if (isExternal(root)) {
            opiext++;
        }
        if (opiext == 2) {
            opi2plp = max(opi2plp, opiloop);
            nopi2p++;
        }
        for (ie = 1; ie <= nEdges; ie++) {
            if (intlst[ie]) {
                edges[ie].opicomp = nopicomp;
            }
        }
        nopicomp++;

        bconn++;    // the number of connected component
    }

    // momentum conservation of external particles
    extMomConsv();

    if (CHECK) {
        chkMomConsv();
    }

    if (DEBUG1) {
        printf("node, bidef, bilow, klow\n");
        for (n = 0; n < nNodes; n++) {
            printf("%2d: %2d : %2d : %p", n, bidef[n], bilow[n], nodes[n].klow);
        }

    }
    return;
}

//--------------------------------------------------------------
void EGraph::biinitE(void)
{
    int n, j, e;

    bicount = 0;                  // counter of visiting node
    loopm   = 0;                  // # of independent loop momentum 

    nopicomp = 0;
    opi2plp  = 0;
    nopi2p   = 0;

    for (n = 0; n < nNodes; n++) {
        bidef[n] = -1;
        bilow[n] = -1;
        nodes[n].ndtype  = EG_ND_Undef;    
        for (j = 0; j < nLoops; j++) {
            nodes[n].klow[j] = -1;
        }
    }

    for (e = 1; e <= nEdges; e++) {
        edges[e].visited = False;
        edges[e].conid   = -1;                  // connected component
        edges[e].edtype  = EG_ED_Undef;    
        edges[e].opicomp = -1;
        for (j = 1; j <= nEdges; j++) {
            edges[e].emom[j] = 0;            // coefficients of ext. mom.
        }
        for (j = 0; j < nLoops; j++) {
            edges[e].lmom[j] = 0;            // coefficients of loop mom.
        }
    }
}

//--------------------------------------------------------------
void EGraph::bisearchE(int nd, int *extlst, int *intlst, int *opiext, int *opiloop)
{
    int k, e, ie, ed, td, dir, j;
    int opiext1, opiloop1;
    int extlst1[nEdges+1];   // set of external nodes below
    int intlst1[nEdges+1];   // set of vertices in the 1PI component

    (*opiext)  = 0;
    (*opiloop) = 0;

    // visit node 'nd'
    bidef[nd] = bicount;
    bilow[nd] = bicount;
    for (k = 0; k < nLoops; k++) {
        nodes[nd].klow[k] = bicount;
    }
    bicount++;
    for (j = 1; j <= nEdges; j++) {
        extlst[j] = False;
        intlst[j] = False;
    }
    // the case of nd being external is handled at the bottom of this func.

    // go to children : nd --> ed --> td
    for (e = 0; e < nodes[nd].deg; e++) {
        ed = abs(nodes[nd].edges[e]);
        if (edges[ed].edtype == EG_ED_Deleted) {
            // permanently deleted edge
            continue;
        } else if (edges[ed].visited) {
            continue;
        } else if (edges[ed].cut) {
            // temporalily cut edge
            (*opiext)++;
            continue;
        } else if (!isExternal(nd) && edges[ed].ext) {
            edges[ed].visited = True;
            extlst[ed] = True;
            edges[ed].emom[ed] = 1;
            (*opiext)++;
            continue;
        }
  
        // momentum is assigned on the backward move.
        edges[ed].visited = True;
        td  = edges[ed].nodes[0];
        dir = 1;
        if (td == nd) {
            td = edges[ed].nodes[1];
            dir = -1;
        }
  
        if (DEBUG1) {
            printf("->: nd = %d, td = %d, ed = %d\n", nd, td, ed);
        }
  
        if (bidef[td] >= 0) {
            // already visited : a back edge is found
            bilow[nd] = min(bilow[nd], bilow[td]);
            edges[ed].setType(EG_ED_Back);
            intlst[ed] = True;
    
            // create new loop momentum
            k = loopm++;
            nodes[nd].klow[k] = min(nodes[nd].klow[k], nodes[td].klow[k]);
            edges[ed].setLMom(k, dir);
            (*opiloop)++;
    
            // self-loop
            if (td == nd) {
                nodes[nd].setType(EG_ND_CPoint);
            }
    
            if (DEBUG1) {
                printf("be: nd = %d, td = %d, ed= %d, ext=%p\n",nd, td, ed, extlst);
            }
  
        } else {
            // go down further
            bisearchE(td, extlst1, intlst1, &opiext1, &opiloop1);
    
            // the set of external particles
            for (j = 1; j <= nEdges; j++) {
                extlst[j] = extlst[j] || extlst1[j];
            }
    
            // loop momenta
            for (k = 0; k < nLoops; k++) {
                if (nodes[td].klow[k] <= nodes[nd].klow[k]) {
                    // inside loop 'k'
                    edges[ed].setLMom(k, dir);
                }
                nodes[nd].klow[k] = min(nodes[nd].klow[k], nodes[td].klow[k]);
            }
                 
            if (bilow[td] >= bidef[nd]) {
                // a cut point is found
                if (nodes[nd].ndtype == EG_ND_Undef) {
                    nodes[nd].setType(EG_ND_CPoint);
                } else if (nodes[nd].ndtype != EG_ND_CPoint) {
                    printf("bisearch: node %d is a cut point (not undef %d)\n", 
                           nd, nodes[nd].ndtype);
                }
            }
                 
            if (bilow[td] > bidef[nd]) {
                // a bridge is found
                if (edges[ed].edtype == EG_ED_Undef) {
                    edges[ed].setType(EG_ED_Bridge);
                } else if (edges[ed].edtype != EG_ED_Bridge) {
                    printf("bisearch: edges %d is a bridge (not undef %d)\n", 
                           ed, edges[ed].edtype);
                }
                if (!edges[ed].ext) {
                    opiext1++;            // from bridge
                    for (ie = 1; ie <= nEdges; ie++) {
                        if (intlst1[ie]) {
                            edges[ie].opicomp = nopicomp;
                            intlst1[ie] = False;
                        }
                    }
                    if (opiext1 == 2) {
                        opi2plp = max(opi2plp, opiloop1);
                        nopi2p++;
                    }
                    nopicomp++;
                }
                opiext1  = 1;    // from bridge
                opiloop1 = 0;
    
            } else {
                // not a bridge
                bilow[nd] = min(bilow[nd], bilow[td]);
                if (edges[ed].edtype == EG_ED_Undef) {
                    edges[ed].setType(EG_ED_Inloop);
                } else if (edges[ed].edtype != EG_ED_Inloop) {
                    printf("bisearch: edges %d is not undef (%d)\n", 
                           ed, edges[ed].edtype);
                }
                intlst1[ed] = True;
            }
            for (ie = 1; ie <= nEdges; ie++) {
                intlst[ie] = intlst[ie] || intlst1[ie];
            }
            (*opiext)  += opiext1;
            (*opiloop) += opiloop1;
    
            if (DEBUG1) {
                printf("nm: nd = %d, td = %d, ed= %d, extlst=%p\n",
                        nd, td, ed, extlst);
            }
    
            // linear combination of external momenta
            edges[ed].setEMom(nEdges, extlst1, dir);
        }
  
        if (DEBUG1) {
          printf("<-: nd = %d, td = %d, ed = %d\n", nd, td, ed);
        }
    }

    return;
}

//--------------------------------------------------------------
void EGraph::extMomConsv(void)
{
    int e, ex, lex, rs;

    lex = -1;
    for (e = 1; e <= nEdges; e++) {
        if (edges[e].ext) {
            lex = e;
            extMom[e] = edges[e].emom[e];
        } else {
            extMom[e] = 0;
        }
    }
    if (lex < 0) {
        return;
    }
    for (e = 1; e <= nEdges; e++) {
        rs = edges[e].emom[lex];
        if (rs != 0) {
            for (ex = 1; ex <= nEdges; ex++) {
                edges[e].emom[ex] -= rs*extMom[ex];
            }
        }
    }
}

//--------------------------------------------------------------
void EGraph::chkMomConsv(void)
{
    // check momentum conservation

    int  esum[nEdges+1];
    int  lsum[nLoops];
    Bool ok, okn;
    int  n, ex, lk, ej, e, dir;

    if (bicount < 1) {
        return;
    }
    ok = True;
    for (n = 0; n < nNodes; n++) {
        if (isExternal(n)) {
            continue;
        }
        for (ex = 1; ex <= nEdges; ex++) {
            esum[ex] = 0;
        }
        for (lk = 0; lk < nLoops; lk++) {
            lsum[lk] = 0;
        }
        for (ej = 0; ej < nodes[n].deg; ej++) {
            e = abs(nodes[n].edges[ej]);
            if (edges[e].nodes[0] == edges[e].nodes[0]) {
                continue;
            }
            dir = dirEdge(n, ej);
            for (ex = 1; ex <= nEdges; ex++) {
                if (edges[e].ext) {
                    esum[ex] += dir*edges[e].emom[ex];
                }
            }
            for (lk = 0; lk < nLoops; lk++) {
                lsum[lk] += dir*edges[e].lmom[lk];
            }
        }
  
        okn = True;
        for (ex = 1; ex <= nEdges; ex++) {
            if (esum[ex] != 0) {
                okn = False;
                printf("chkMomConsv:n=%d, esum[%d]=%d\n", n, ex, esum[ex]);
            }
        }
  
        for (lk = 0; lk < nLoops; lk++) {
            if (lsum[lk] != 0) {
                okn = False;
                printf("chkMomConsv:n=%d, lsum[%d]=%d\n", n, lk, lsum[lk]);
            }
        }
  
        if (!okn) {
            ok = False;
            printf("*** Violation of momentum conservation at node =%d\n",n);
       }
    }

    if (!ok) {
        print();
        erEnd("inconsistent momentum");
    }
}

//==============================================================
// class MNode : nodes in MGraph
//--------------------------------------------------------------
MNode::MNode(int vid, int vdeg, int vextlp, int vclss)
{
    // Arguments
    //   vid    : identifier of the vertex
    //   vdeg   : degree of the node
    //   vextlp : external node (-1) or looped vertex
    //   vclss  : class of the node

    id      = vid;       // id of the node
    deg     = vdeg;      // degree of the node
    freelg  = vdeg;      // number of free legs
    clss    = vclss;     // class to which the node belogns
    extloop = vextlp;    // external node or not
}

//===============================================================
//  class of node-classes for MGraph
//--------------------------------------------------------------
class MNodeClass {
  public:
    int  *clist;                   // the number of nodes in each class
    int  *ndcl;                    // node --> class
    int **clmat;                   // matrix used for classification
    int  *flist;                   // the first node in each class
    int  *clord;                   // ordering of classes
    int   nNodes;                  // the number of nodes
    int   nClasses;                // the number of classes
    int   maxdeg;                  // maximal value of degree(node)

    int   flg0;
    int   flg1;
    int   flg2;

    // dummy
    // int   idummy;

    MNodeClass(int nnodes, int nclasses);
    ~MNodeClass();
    void  init(int *cl, int mxdeg, int **adjmat);
    void  copy(MNodeClass* mnc);
    int   clCmp(int nd0, int nd1, int cn);
    void  printMat(void);

    void  mkFlist(void);
    void  mkNdCl(void);
    void  mkClMat(int **adjmat);
    void  incMat(int nd, int td, int val);
    int   cmpArray(int *a0, int *a1, int ma);
    void  reorder(MGraph *mg);
};

//--------------------------------------------------------------
MNodeClass::MNodeClass(int nnodes, int nclasses)
{
    // Input
    //   nnodes   : possible maximal number of nodes
    //   nclasses : possible maximal number of classes

    nNodes   = nnodes;
    nClasses = nclasses;
    clist    = new int[nClasses];
    ndcl     = new int[nNodes];
    clmat    = newMat(nNodes, nClasses, 0);
    flist    = new int[nClasses+1];
    clord    = new int[nClasses];

#if DEBUGM
    printf("+++ new    MNodeClass %d\n", ++countNMC);
    if(countNMC > 100) { exit(1); }
#endif
}

//--------------------------------------------------------------
MNodeClass::~MNodeClass()
{
    delete[] clord;
    delete[] flist;
    deleteMat(clmat, nNodes);
    delete[] ndcl;
    delete[] clist;

#if DEBUGM
    printf("+++ delete MNodeClass %d\n", countNMC--);
#endif
}

//--------------------------------------------------------------
void MNodeClass::init(int *cl, int mxdeg, int **adjmat)
{
    // initialize the object
    // Argument
    //   cl[j]        : list of number of nodes in the j-th class
    //   mxdeg        : maximum degree to nodes
    //   adjmat[j][k] : adjacency matrix

    int j;

    for (j = 0; j < nClasses; j++) {
        clist[j]  = cl[j];
        clord[j]  = j;
    }
    maxdeg   = mxdeg;
    mkNdCl();
    mkClMat(adjmat);
    mkFlist();
    flg0 = 0;
    flg1 = 0;
    flg2 = 0;
}

//--------------------------------------------------------------
void MNodeClass::copy(MNodeClass* mnc)
{
    // copy MNodeClass 'mnc' to this object

    int j, k;

    for (k = 0; k < nClasses; k++) {
        clist[k] = mnc->clist[k];
    }
    maxdeg   = mnc->maxdeg;
    for (j = 0; j < nNodes; j++) {
        ndcl[j] = mnc->ndcl[j];
        for (k = 0; k < nClasses; k++) {
            clmat[j][k] = mnc->clmat[j][k];
        }
    }
    for (k = 0; k < nClasses+1; k++) {
        flist[k] = mnc->flist[k];
    }
}

//--------------------------------------------------------------
void MNodeClass::mkFlist(void)
{
    //  Construct flist
    //    The set of nodes in class 'c' is [flist[c],...,flist[c+1]-1]

    int  j, f;

    f  = 0;
    for (j = 0; j < nClasses; j++) {
        flist[j] = f;
        f += clist[j];
    }
    flist[nClasses] = f;
}

//--------------------------------------------------------------
void MNodeClass::mkNdCl(void)
{
    //  Construct ndcl
    //    ndcl[nd] = (the class id in which node 'nd' belongs)

    int  c, k;
    int  nd = 0;

    for (c = 0; c < nClasses; c++) {
        for (k = 0; k < clist[c]; k++) {
            ndcl[nd++] = c;
        }
    }
}

//--------------------------------------------------------------
int MNodeClass::clCmp(int nd0, int nd1, int cn)
{
    //  Comparison of two nodes 'nd0' and 'nd1'
    //    in lexicographic ordering of [class, connection configuration]

    int cmp;
    // Wether two nodes are in a same class or not.

    cmp = ndcl[nd0] - ndcl[nd1];
    if (cmp != 0) {
        return cmp;
    }

    // Sign '-' signifies the reverse ordering
    cmp = - cmpArray(clmat[nd0], clmat[nd1], cn);
    if (cmp != 0) {
        return cmp;
    }
    return cmp;
}
    
//--------------------------------------------------------------
void MNodeClass::mkClMat(int **adjmat)
{
    //  Construct a matrix 'clmat[nd][tc]' which is the number
    //  of edges connecting 'nd' and all nodes in class 'tc'.

    int  nd, td, tc;

    for (nd = 0; nd < nNodes; nd++) {
        for (td = 0; td < nNodes; td++) {
            tc = ndcl[td];                      // another node
            if (nd == td) {
                clmat[nd][tc] += CLWIGHTD(adjmat[nd][td]);
            } else {
                clmat[nd][tc] += CLWIGHTO(adjmat[nd][td]);
            }
        }
    }
}

//--------------------------------------------------------------
void MNodeClass::incMat(int nd, int td, int val)
{
    //  Increase the number of edges by 'val' between 'nd' and 'td'.

    int tdc = ndcl[td];             // class of 'td'
    clmat[nd][tdc] += val;          // modify matrix 'clmat'.
}

//--------------------------------------------------------------
void MNodeClass::printMat(void)
{
    //  Print configuration matrix.

    int j1, j2;

    printf("\n");
  
    printf("nClasses=%d", nClasses);
    printf(" clord="); printArray(nClasses, clord); printf("\n");
    printf("flg = (%d, %d, %d)\n", flg0, flg1, flg2);

    // the first line
    printf("nd: cl:   ");
    for (j2 = 0; j2 < nClasses; j2++) {
        printf("%2d ", j2);
    }
    printf("\n");
  
    // print raw
    for (j1 = 0; j1 < nNodes; j1++) {
        printf("%2d; %2d: [", j1, ndcl[j1]);
        for (j2 = 0; j2 < nClasses; j2++) {
            printf(" %2d", clmat[j1][j2]);
        }
        printf("]\n");
    }
}

#if 0
//--------------------------------------------------------------
int MNodeClass::cmpArray(int *a0, int *a1, int ma)
{
    for (int j = 0; j < ma; j++) {
        if (a0[j] < a1[j]) {
            return -1;
        } else if (a0[j] > a1[j]) {
            return 1;
        }
    }
    return 0;
}
#else
//--------------------------------------------------------------
int MNodeClass::cmpArray(int *a0, int *a1, int ma)
{
    int j, k;

    for (k = 0; k < ma; k++) {
        j = clord[k];
        if (a0[j] < a1[j]) {
            return -1;
        } else if (a0[j] > a1[j]) {
            return 1;
        }
    }
    return 0;
}
#endif

//--------------------------------------------------------------
void MNodeClass::reorder(MGraph *mg)
{
    int flg[nClasses];
    int co, cn;
    int f0, f1, f2;
  
    f0 = 0;
    f1 = 0;
    f2 = 0;
    for (co = 0; co < nClasses; co++) {
        cn  = flist[co];
        if (mg->nodes[cn]->freelg < 1) {
            flg[co] = 0;
            f0++;
        } else if (mg->nodes[cn]->freelg < mg->nodes[cn]->deg) {
            flg[co] = mg->nodes[cn]->deg + maxdeg*clist[co];
            f1++;
        } else {
            flg[co] = maxdeg*(mg->nodes[cn]->deg + maxdeg*clist[co]);
            f2++;
        }
    }
    if (f0 < nClasses) {
        bsort(nClasses, clord, flg);
    }
    flg0 = f0;
    flg1 = f0 + f1;
    flg2 = f0 + f1 + f2;
}

//===============================================================
//  class MGraph : scalar graph expressed by matrix form
//---------------------------------------------------------------
MGraph::MGraph(int pid, int ncl, int *cldeg, int *clnum, int *cltyp, int *opt)
{
    int nn, ne, j, k;

    // initial conditions
    nClasses = ncl;
    clist    = new int[ncl];

    mindeg   = -1;
    maxdeg   = -1;
    ne = 0;
    nn = 0;
    for (j = 0; j < nClasses; j++) {
        clist[j] = clnum[j];
        nn += clnum[j];
        ne += cldeg[j]*clnum[j];
        if (mindeg < 0) {
            mindeg = cldeg[j];
        } else {
            mindeg = min(mindeg, cldeg[j]);
        }
        if (maxdeg < 0) {
            maxdeg = cldeg[j];
        } else {
            maxdeg = max(maxdeg, cldeg[j]);
        }
    }
    if (ne % 2 != 0) {
        printf("Sum of degrees are not even\n");
        for (j = 0; j < nClasses; j++) {
            printf("class %2d: %2d %2d %2d\n", 
                   j, cldeg[j], clnum[j], cltyp[j]);
        }
        erEnd("illegal degrees of nodes");
    }
    pId    = pid;
    nNodes = nn;
    nodes = new MNode*[nNodes];

    for (j = 0; j < OPT_Size; j++) {
        options.values[j] = opt[j];
    }
    if (OPTPRINT >= 1) {
        printf("MGraph::MGraph(%d, nclasses=%d;\n", pid, ncl);
        for (j = 0; j < ncl; j++) {
            printf("    class %d: deg=%d, num=%d, typ=%d\n", j, cldeg[j], clnum[j], cltyp[j]);
        }
        options.print();
    }

    nEdges = ne / 2;
    nLoops = nEdges - nNodes + 1;

    egraph = new EGraph(nNodes, nEdges, maxdeg);
    nn = 0;
    nExtern = 0;
    for (j = 0; j < nClasses; j++) {
        for (k = 0; k < clist[j]; k++, nn++) {
            nodes[nn] = new MNode(nn, cldeg[j], cltyp[j], j);
            egraph->setExtLoop(nn, cltyp[j]);
            if (cltyp[j] < 0) {
                nExtern++;
            }
        }
    }
    egraph->endSetExtLoop();

    // generated set of graphs
    cDiag          = 0;
    c1PI           = 0;
    cNoTadpole     = 0;
    cNoTadBlock    = 0;
    c1PINoTadBlock = 0;

    // the current graph
    adjMat    = newMat(nNodes, nNodes, 0);
    nsym      = ToBigInt(0);
    esym      = ToBigInt(0);
    n1PIComps = 0;
    wscon     = ToFraction(0, 1);
    wsopi     = ToFraction(0, 1);

    // current node classification
    curcl    = new MNodeClass(nNodes, nClasses);

    // measures of efficiency
    ngen     = 0;
    ngconn   = 0;

    if (MONITOR) {
        nCallRefine    = 0;
        discardRefine  = 0;
        discardDisc    = 0;
        discardIso     = 0;
    }

    // work space for isIsomorphic
    modmat = newMat(nNodes, nNodes, 0);
    permp  = newArray(nNodes, 0);
    permq  = newArray(nNodes, 0);
    permr  = newArray(nNodes, 0);

    // work space for bisearchM
    bidef = newArray(nNodes, 0);
    bilow = newArray(nNodes, 0);
    bicount = 0;

#if DEBUGM
    printf("+++ new    MGraph %d\n", ++countMG);
    if(countEG > 100) { exit(1); }
#endif
}

//---------------------------------------------------------------
MGraph::~MGraph()
{
    int j;

    deletArray(bilow);
    deletArray(bidef);
    deletArray(permr);
    deletArray(permq);
    deletArray(permp);

    deleteMat(modmat, nNodes);
    delete curcl;
    deleteMat(adjMat, nNodes);
    delete egraph;
    for (j = 0; j < nNodes; j++) {
        delete nodes[j];
    }
    delete[] nodes;
    delete[] clist;

#if DEBUGM
    printf("+++ delete MGraph %d\n", countMG--);
#endif
}
  
//---------------------------------------------------------------
void MGraph::printAdjMat(MNodeClass *cl)
{
    int j1, j2;

    printf("      ");
    for (j2 = 0; j2 < nNodes; j2++) {
        printf(" %2d", j2);
    }
    printf("\n");
    for (j1 = 0; j1 < nNodes; j1++) {
        printf("%2d : [", j1);
        for (j2 = 0; j2 < nNodes; j2++) {
            printf(" %2d", adjMat[j1][j2]);
        }
        printf("] %2d\n", cl->ndcl[j1]);
    }
}

//---------------------------------------------------------------
Bool MGraph::isConnected(void)
{
    //  Check graph can be a connected one.
    //  If a connected component without free leg is not the whole graph then
    //  return False, otherwise return True.

    int j, n, nv;

    for (j = 0; j < nNodes; j++) {
        nodes[j]->visited = -1;
    }
    if (visit(0)) {
        return True;
    }
    nv = 0;
    for (n = 0; n < nNodes; n++) {
        if (nodes[n]->visited >= 0) {
            nv++;
        }
    }
    return (nv == nNodes);
}

//---------------------------------------------------------------
Bool MGraph::visit(int nd)
{
    //  Visiting connected node used for 'isConnected'
    //  If child nodes has free legs, then this function returns True.
    //  otherwise it returns False.
  
    int td;
  
    // This node has free legs.
    if (nodes[nd]->freelg > 0) {
        return True;
    }
    nodes[nd]->visited = 0;
    for (td = 0; td < nNodes; td++) {
        if ((adjMat[nd][td] > 0) and (nodes[td]->visited < 0)) {
            if (visit(td)) {
                return True;
            }
        }
    }
    // all the child nodes has no free legs.
    return False;
}

//---------------------------------------------------------------
Bool MGraph::isIsomorphic(MNodeClass *cl)
{
    //  Check whether the current graph is the Representative of a isomorphic class.
    //    nsym = symmetry factor by the permutation of nodes.
    //    esym = symmetry factor by the permutation of edge.
    //  If this graph is not a representative, then returns False.

    int j1, j2, cmp, count, nself;

    nsym = ToBigInt(0);
    esym = ToBigInt(1);

    count = 0;
    while (True) {
        count = nextPerm(nNodes, cl->nClasses, cl->clist, 
                         permr, permq, permp, count);

      if (count < 0) {
          // calculate permutations of edges
          esym = ToBigInt(1);
          for (j1 = 0; j1 < nNodes; j1++) {
              if (adjMat[j1][j1] > 0) {
                  nself = adjMat[j1][j1]/2;
                  esym *= factorial(nself);
                  esym *= ipow(2, nself);
              }
              for (j2 = j1+1; j2 < nNodes; j2++) {
                  if (adjMat[j1][j2] > 0) {
                        esym *= factorial(adjMat[j1][j2]);
                  }
              }
          }
          return True;
      }

      permMat(nNodes, permp, adjMat, modmat);
      cmp = compMat(nNodes, adjMat, modmat);
      if (cmp < 0) {
          return False;
      } else if (cmp == 0) {
          // save permutation ???
          nsym = nsym + ToBigInt(1);
      }
    }
}

//---------------------------------------------------------------
void MGraph::permMat(int size, int *perm, int **mat0, int **mat1)
{
    // apply permutation to matrix 'mat0' and obtain 'mat1'

    int j1, j2;

    for (j1 = 0; j1 < size; j1++) {
        for (j2 = 0; j2 < size; j2++) {
            mat1[j1][j2] = mat0[perm[j1]][perm[j2]];
        }
    }
}

//---------------------------------------------------------------
int MGraph::compMat(int size, int **mat0, int **mat1)
{
    // comparison of matrix

    int j1, j2, cmp;

    for (j1 = 0; j1 < size; j1++) {
        for (j2 = 0; j2 < size; j2++) {
            cmp = mat0[j1][j2] - mat1[j1][j2];
            if (cmp != 0) {
                return cmp;
            }
        }
    }
    return 0;
}

//---------------------------------------------------------------
MNodeClass *MGraph::refineClass(MNodeClass *cl)
{
    //  Refine the classification
    //    cl : the current 'MNodeClass' object
    //    cn : the class number
    //  Returns (the new class number corresponds to 'cn') if OK, or
    //          'None' if ordering condition is not satisfied

    MNodeClass *ccl = cl;
    MNodeClass *xcl = NULL;
    MNodeClass *ncl = NULL;
    int         ucl[nNodes];
    int         ccn = cl->nClasses;
    int         nucl, nce;
    int         td, cmp;
    int         nccl;

    if (DEBUG1) {
        printf("refineClass:begin\n");
    }

    if (MONITOR) {
        nCallRefine++;
    }
    nucl = 0;
    while (ccl->nClasses != nucl) {    // repeat refinement.
        nce  = 0;
        nucl = 0;
        for (td = 1; td < nNodes; td++) {
          // 'td' is the next node and the current node is 'td-1'.
          // Count up the number of the elements in the current class 
          // corresponding to the current node
          nce++;
          cmp = ccl->clCmp(td-1, td, ccn);
  
          // the ordering condition is not satisfied.
          if (cmp > 0) {
              if (DEBUG1) {
                  printf("refine: cls = ");
                  printArray(cl->nClasses, cl->clist); printf("\n");
                  cl->printMat();
                  printf("clmat\n");
                  ccl->printMat();
                  printf("refine: discard: cls = ");
                  printArray(ccl->nClasses, ccl->clist); printf("\n");
                  printf("ucl = ");
                  printArray(nucl, ucl); printf("\n");
              }
            if (ccl != cl) {
                delete ccl;
            }
            return NULL;
  
          } else if (cmp < 0) {
              // 'td' is in the next class to the current node.
              ucl[nucl++] = nce;       // close the current class
  
              // start new class
              nce = 0;
          }
          // nothing to do for the case of 'cmp == 0'.
  
        }
  
        // close array 'ucl'.
        ucl[nucl++] = nce + 1;
    
        // inconsistent
        if (nucl < ccl->nClasses) {
          erEnd("refineClasses : smaller number of classes");
  
        // preparation of the next repetition.
        } else {
            xcl = new MNodeClass(nNodes, nucl);
            xcl->init(ucl, maxdeg, adjMat);
            ccn = xcl->nClasses;
    
            nccl = ccl->nClasses;
    
            if (ccl != cl) {
                delete ccl;
            }
            ccl = xcl;
            if (ccl->nClasses == nccl) {
                ccl->reorder(this);
                return ccl;
            }
    
            nucl = 0;
        }
    }

    if (DEBUG1) {
        printf("refine: ncl = ");
        printArray(ncl->nClasses, ncl->clist); printf("\n");
        ncl->printMat();
    }

    return ncl;
}

//---------------------------------------------------------------
void MGraph::biconnM(void)
{
    //  Count the number of 1PI components.
    //  The algorithm is described in
    //  A.V. Aho, J.E. Hopcroft and J.D. Ullman
    //   'The Design and Analysis of Computer Algorithms', Chap. 5
    //   1974, Addison-Wesley.

    int j, k, next, loop, root, vr;
    int minn;

    // initialization
    bicount    = 0;
    nExtEdges  = 0;
    n1PIComps  = 0;
    nBlocks    = 0;
    nBridges   = 0;
    nTadpoles  = 0;
    nTadBlocks = 0;
    for (j = 0; j < nNodes; j++) {
        bidef[j] = -1;
        bilow[j] = -1;
    }

    if (nLoops < 0) {
        return;
    }

    // find a root for the root of bisearch
    // root must be (1) a vertex adjacent to an external node
    // or (2) an vertex when there are not external nodes.
    root = -1;
    vr   = -1;
    for (j = 0; j < nNodes; j++) {
        if (isExternal(j)) {
            nExtEdges += nodes[j]->deg;
            if (root < 0) {
                for (k = 0; k < nNodes; k++) {
                    if (adjMat[j][k] != 0 && !isExternal(k)) {
                        root = k;
                        break;
                    }
                }
            }
        } else if (vr < 0) {
           vr = j;
        }
    }
    // case (2)
    if (root < 0) {
        root = vr;
    }

    if (root >= 0) {
        bisearchM(root, -1, 0, &next, &loop);
        n1PIComps = nBridges - nExtEdges + 1;

    // no vertex.
    } else {
        // no vertices
        // Connected ==> only when ex=2, loop=0 
        nBridges  = 1;
        nExtEdges = 1;
        n1PIComps = nBridges - nExtEdges;
    }

    // the structure of the current graph
    if (n1PIComps == 1) {
        opi = True;
    } else {
        opi = False;
    }
    if (nTadpoles > 0) {
        tadpole = True;
    } else {
        tadpole = False;
    }
    minn = (nExtEdges <= 1) ? 2 : 1;
    if (nTadBlocks >= minn) {
        tadBlock = True;
    } else {
        tadBlock = False;
    }
}

//---------------------------------------------------------------
void MGraph::bisearchM(int nd, int pd, int ned, int *next, int *loop)
{
    //  Search biconnected component
    //    visit : pd --> nd --> td
    //    ned : the number of edges between pd and nd.
    //  Ouput
    //    next : the number of external edges below nd.

    int td, next1, loop1;
    Bool clr;

    bidef[nd] = bicount;
    bilow[nd] = bicount;
    *next = 0;
    *loop = 0;
    bicount++;

    // external node
    if (isExternal(nd)) {
        if (pd < 0) {
            erEnd("*** bisearchM : node is external");
        }
        *next     = ned;
        return;
    }

    for (td = 0; td < nNodes; td++) {
        // td is not adjacent to nd
        if (adjMat[td][nd] < 1) {
            continue;
        }
  
        // self-loop : pd --> nd --> nd
        if (td == nd) {
            *loop      += adjMat[nd][nd]/2;
            nBlocks    += adjMat[nd][nd]/2;
            nTadBlocks += adjMat[nd][nd]/2;
            continue;
  
        // back to the parent : pd --> nd --> pd,  pd is a vertex
        // Back edges when the connection is multi-edges (ned > 1)
        } else if (td == pd) {
            if (adjMat[nd][td] > 1) {
                bilow[nd] = min(bilow[nd], bidef[pd]);
                *loop += adjMat[nd][pd] - 1;
            }
            continue;
  
        // back edge
        } else if (bidef[td] >= 0) {
            bilow[nd] = min(bilow[nd], bidef[td]);
            *loop += adjMat[nd][pd];
  
        // new node
        } else {
  
            bisearchM(td, nd, adjMat[td][nd], &next1, &loop1);
  
            clr = False;
            // block
            if (bilow[td] >= bidef[nd]) {
                nBlocks++;
                if (next1 < 1 && loop1 > 0) {
                    nTadBlocks++;
                }
            }
            // bridge
            if (bilow[td] > bidef[nd]) {
                nBridges++;
                if (next1 < 1 && loop1 > 0) {
                    nTadpoles++;
                }
                clr = True;
            }
            if (clr) {
                loop1 = 0;
            }
            bilow[nd] = min(bilow[nd], bilow[td]);
            (*next) += next1;
            (*loop) += loop1;
        }
    }
}

//---------------------------------------------------------------
long MGraph::generate(void)
{
    //  Generate graphs
    //    the generation process starts from 'connectClass'.

    MNodeClass *cl;

    if (DEBUG1) {
        printf("+++ generate : begin\n");
    }
    // Initial classification of nodes.
    cl = new MNodeClass(nNodes, nClasses);
    cl->init(clist, maxdeg, adjMat);
    cl->reorder(this);
    connectClass(cl);

    // Print the result.
    if (OPTPRINT >= 1) {
        printf("\n");
        printf("* Total %ld Graphs; %ld 1PI", cDiag, c1PI);
        printf(" wscon = %g (%g 1PI)", wscon, wsopi);
        printf("\n");

        if (MONITOR) {
            printf("* refine:                     %ld\n", nCallRefine);
            printf("* discarded for refinement:   %ld\n", discardRefine);
            printf("* discarded for disconnected: %ld\n", discardDisc);
            printf("* discarded for duplication:  %ld\n", discardIso);
        }
    }
    delete cl;

    if (DEBUG1) {
        printf("+++ generate : end %ld\n", cDiag);
    }
    return cDiag;
}

//---------------------------------------------------------------
void MGraph::connectClass(MNodeClass *cl)
{
    // Connect nodes in a class to others

    int sc, sn;
    MNodeClass *xcl;

    if (DEBUG1) {
        printf("connectClass:begin:");
        cl->printMat();
        printf("\n");
    }
    xcl = refineClass(cl);


    if (xcl == NULL) {
        if (MONITOR) {
            discardRefine++;
        }
    } else {
        if (xcl->flg0 >= xcl->nClasses) {
            newGraph(xcl);
        } else {
            sc = xcl->clord[xcl->flg0];
            sn = xcl->flist[sc];
            connectNode(xcl->flg0, sn, xcl);
        }
    }
    if (xcl != cl && xcl != NULL) {
        delete xcl;
    }
    if (DEBUG0) {
        printf("connectClass:end\n");
    }
}

//------------------------------------------------------------
void MGraph::connectNode(int so, int ss, MNodeClass *cl)
{
    int sc, sn;

    sc = cl->clord[so];
    if (DEBUG1) {
        printf("connectNode:begin:(%d,%d, %d)\n", so, sc, ss);
    }

    if (ss >= cl->flist[sc+1]) {
        connectClass(cl);
        if (DEBUG0) {
            printf("connectNode:end1\n");
        }
        return;
    }

    for (sn = ss; sn < cl->flist[sc+1]; sn++) {
        connectLeg(so, sn, so, sn, cl);
        if (DEBUG0) {
            printf("connectNode:end2\n");
        }
        return;
    }

    if (DEBUG0) {
        printf("connectNode:end3\n");
    }
}

//------------------------------------------------------------
void MGraph::connectLeg(int so, int sn, int to, int ts, MNodeClass *cl)
{
    //  Add one connection between two legs.
    //    1. select another node to connect
    //    2. determine multiplicity of the connection
    // 
    //   Arguments
    //     cn : the current class
    //     nd : the current node to be connected.
    //     nextnd : {nextnd, ...} is the possible target node of the connection.
    //     cl     : the current node class

    int sc, tc, tn, maxself, nc2, nc, maxcon, ts1, ncm, to1;

    sc = cl->clord[so];

    if (DEBUG1) {
        printf("connectLeg:begin:(%d,%d,%d; %d,,%d)", so,sc,sn, to,ts);
        printAdjMat(cl);
    }

    if (sn >= cl->flist[sc+1]) {
        erEnd("*** connectLeg : illegal control");
        return;

    // There remains no free legs in the node 'sn' : move to next node.
    } else if (nodes[sn]->freelg < 1) {

        if (!isConnected()) {
            if (DEBUG0) {
                printf("connectLeg:disconnected\n");
            }
            if (MONITOR) {
                discardDisc++;
            }
        } else {
            if (DEBUG0) {
                printf("connectLeg: call conNode\n");
            }

            // next node in the current class.
            connectNode(so, sn+1, cl);
        }
        return;
    }

    // connect a free leg of the current node 'sn'.
    for (to1 = to; to1 < cl->nClasses; to1++) {
        tc = cl->clord[to1];
        if (to1 == to) {
            ts1 = ts;
        } else {
            ts1 = cl->flist[tc];
        }
        for (tn = ts1; tn < cl->flist[tc+1]; tn++) {
            if (DEBUG0) {
                printf("connectLeg:2:%d=>try %d\n", sn, tn);
            }
    
            if (sc == tc && sn > tn) {
                if (DEBUG0) {
                    printf("connectLeg:4\n");
                }
                continue;
    
            } else if (nodes[tn]->freelg < 1) {
                continue;
    
            // self-loop
            } else if (sn == tn) {
                if (nNodes > 1) {
                    // there are two or more nodes in the graph : 
                    // avoid disconnected graph
                    maxself = min((nodes[sn]->freelg)/2, (nodes[sn]->deg-1)/2);
                } else {
                    // there is only one node the graph.
                    maxself = nodes[sn]->freelg/2;
                }
      
                // If we can assume no tadpole, the following line can be used.
                if (options.values[OPT_1PI] and nNodes > 2) {
                     maxself = min((nodes[sn]->deg-2)/2, maxself);
                }
      
                // for the number of connection.
                for (nc2 = maxself; nc2 > 0; nc2--) {
                    nc = 2*nc2;
                    if (DEBUG0) {
                        printf("connectLeg: call conLeg: (same) %d=>%d(%d)\n",
                               sn, tn,nc);
                    }
                    ncm = CLWIGHTD(nc);
                    adjMat[sn][sn] = nc;
                    nodes[sn]->freelg -= nc;
                    cl->incMat(sn, tn, ncm);
        
                    // next connection
                    connectLeg(so, sn, to1, tn+1, cl);
        
                    // restore the configuration
                    cl->incMat(tn, sn, - ncm);
                    adjMat[sn][sn] = 0;
                    nodes[sn]->freelg += nc;
                    if (DEBUG0) {
                        printf("connectLeg: ret  conLeg: (same) %d=>%d(%d)\n",
                               sn, tn,nc);
                    }
                }
    
            // connections between different nodes.
            } else {
                // maximum possible connection number
                maxcon = min(nodes[sn]->freelg, nodes[tn]->freelg);
      
                // avoid disconnected graphs.
                if (nNodes > 2 && nodes[sn]->deg == nodes[tn]->deg) {
                    maxcon = min(maxcon, nodes[sn]->deg-1);
                }
      
                if (CHECK) {
                    if ((adjMat[sn][tn] != 0) || 
                        (adjMat[sn][tn] != adjMat[tn][sn])) {
                        printf("*** inconsistent connection: sn=%d, tn=%d",
                               sn, tn);
                        printAdjMat(cl);
                        erEnd("*** inconsistent connection ");
                    }
                }
      
                // vary number of connections
                for (nc = maxcon; nc > 0; nc--) {
                    if (DEBUG0) {
                        printf("connectLeg: call conLeg: (diff) %d=>%d(%d)\n",
                               sn, tn,nc);
                    }
                    ncm = CLWIGHTO(nc);
                    adjMat[sn][tn] = nc;
                    adjMat[tn][sn] = nc;
                    nodes[sn]->freelg -= nc;
                    nodes[tn]->freelg -= nc;
                    cl->incMat(sn, tn, ncm);
                    cl->incMat(tn, sn, ncm);
        
                    // next connection
                    connectLeg(so, sn, to1, tn+1, cl);
          
                    // restore configuration
                    cl->incMat(sn, tn, - ncm);
                    cl->incMat(tn, sn, - ncm);
                    adjMat[sn][tn] = 0;
                    adjMat[tn][sn] = 0;
                    nodes[sn]->freelg += nc;
                    nodes[tn]->freelg += nc;
                    if (DEBUG0) {
                        printf("connectLeg: ret  conLeg: (diff) %d=>%d(%d)\n",
                               sn, tn,nc);
                        printAdjMat(cl);
                        printf("\n");
                    }
                }
            }
        } 
    }
    if (DEBUG0) {
      printf("connectLeg:end3:(%d,%d,%d,%d)\n", sc, sn, tc, ts);
    }
   
}

//------------------------------------------------------------
Bool MGraph::isOptM(void)
{
    Bool ok = True;

    if (options.values[OPT_1PI] > 0) {
        ok = ok && opi;
    } else if (options.values[OPT_1PI] < 0) {
        ok = ok && !opi;
    }
    if (options.values[OPT_NoTadpole] > 0) {
        ok = ok && !tadpole;
    } else if (options.values[OPT_NoTadpole] < 0) {
        ok = ok && tadpole;
    }
    if (options.values[OPT_No1PtBlock] > 0) {
        ok = ok && !tadBlock;
    } else if (options.values[OPT_No1PtBlock] < 0) {
        ok = ok && tadBlock;
    }

    return ok;
}

//------------------------------------------------------------
void MGraph::newGraph(MNodeClass *cl)
{
    //  A new candidate diagram is obtained.
    //  It may be a disconnected graph
    //
    //  Things to be done in the steps followed by this function.
    //  1. convert data format which treat the edges as objects.
    //  2. definition of loop momenta to the edges.
    //  3. analysis of loop structure in a graph.
    //  4. assignment of particles to edges and vertices to nodes
    //  5. recalculate symmetry factor


    int connected;
    MNodeClass *xcl;

    ngen++;
    // printf("newGraph: %ld\n", ngen);

    // refine class and check ordering condition
    xcl = refineClass(cl);
    if (xcl == NULL) {
        // printf("newGraph: fail refine\n");
        if (MONITOR) {
            discardRefine++;
        }

    // check whether this is a connected graph or not.
    } else {
        connected = isConnected();
        if (!connected) {
            // printf("newGraph: not connected\n");
            if (DEBUG0) {
                printf("+++ disconnected graph %ld\n", ngen);
                xcl->printMat();
            }
  
            if (MONITOR) {
                discardDisc++;
            }
  
        // connected graph : check isomorphism of the graph
        } else {
            ngconn++;
            if (!isIsomorphic(xcl)) {
                // printf("newGraph: not isomorphic\n");
                if (DEBUG1) {
                    printf("+++ duplicated graph %ld, %ld\n", ngen, ngconn);
                    xcl->printMat();
                }
    
                if (MONITOR) {
                    discardIso++;
                }
    
            // We got a new connected and a unique representation of a class.
            } else {
      
                biconnM();
                if (DEBUG1) {
                    printf("Graph : %ld (%ld)\n", cDiag, ngen);
                    printf("bicount    = %d  ", bicount);
                    printf("nExtEdges  = %d  ", nExtEdges);
                    printf("n1PIComps  = %d  ", n1PIComps);
                    printf("nBlocks    = %d\n", nBlocks);
                    printf("nBridges   = %d  ", nBridges);
                    printf("nTadBlocks = %d  ", nTadBlocks);
                    printf("nTadpoles  = %d\n", nTadpoles);
                }
  
                if (isOptM()) {
                    egraph->fromMGraph(this);
                    if (egraph->isOptE()) {
                        curcl->copy(xcl);
                        // # generated graphs
                        cDiag++;
                        egraph->gId = cDiag;
                        wscon = wscon + ToFraction(1, nsym*esym);
      
                        // # generated 1PI graphs
                        if (opi) {
                            wsopi = wsopi + ToFraction(1, nsym*esym);
                            c1PI++;
                        }
      
                        // # no tadpolses
                        if (!tadpole) {
                            cNoTadpole++;
                        }
                        // # no tadBlocks
                        if (!tadBlock) {
                            cNoTadBlock++;
                            if (opi) {
                                c1PINoTadBlock++;
                            }
                        }
              
                        if (OPTPRINT >= 2) {
                            printf("\n");
                            printf("Graph : %ld (%ld) 1PI com. = %d", 
                                   cDiag, ngen, n1PIComps);
                            printf(" sym. factor = (%ld*%ld)\n", nsym, esym);
                            printAdjMat(cl);
                            // cl->printMat();
                            if (MONITOR) {
                                printf("refine:                     %ld\n", 
                                       nCallRefine);
                                printf("discarded for refinement:   %ld\n", 
                                       discardRefine);
                                printf("discarded for disconnected: %ld\n",
                                       discardDisc);
                                printf("discarded for duplication:  %ld\n",
                                       discardIso);
                            }
                        }
              
                        // go to next step
                        toForm(egraph);
                    }
                }
            }
        }
    }
    if (xcl != cl && xcl != NULL) {
      delete xcl;
    }
}

//------------------------------------------------------------
Local int nextPerm(int nelem, int nclass, int *cl, int *r, int *q, int *p, int count)
{
    // Sequatial generation of all permutations.

    int  j, k, n, e, t;
    Bool b;

    for (j = 0; j < nelem; j++) {
        p[j] = j;
    }
    if (count < 1) {
        for (j = 0; j < nelem; j++) {
            q[j] = 0;
            r[j] = 0;
        }
        j = 0;
        for (k = 0; k < nclass; k++) {
            n = cl[k];
            for (e = 0; e < n; e++) {
                r[j] = n - e - 1;
                j++;
            }
        }
        if (j != nelem) {
            erEnd("*** inconsistent # elements");
        }
        return 1;
    }
    b = False;
    for (j = nelem-1; j >= 0; j--) {
        if (q[j] < r[j]) {
            for (k = j+1; k < nelem; k++) {
                q[k] = 0;
            }
            q[j]++;
            b = True;
            break;
        }
    }
    if (!b) {
        return (-count);
    }

    for (j = 0; j < nelem; j++) {
        k = j + q[j];
        t = p[j];
        p[j] = p[k];
        p[k] = t;
    }
    return count + 1;
}

//------------------------------------------------------------
Local BigInt factorial(int n)
{
    // returns 1 for n < 1

    int r, j;

    r = 1;
    for (j = 2; j <= n; j++) {
        r *= j;
    }
    return r;
}

//------------------------------------------------------------
Local BigInt ipow(int n, int p)
{
    int r, j;

    r = 1;
    for (j = 0; j < p; j++) {
        r *= n;
    }
    return r;
}

//------------------------------------------------------------
Local void erEnd(const char *msg)
{
    printf("*** gentopo: Error : %s\n", msg);
    abort();
}

//------------------------------------------------------------
Local int   *newArray(int size, int val)
{
    // memory allocation of an array

    int *a, j;

    a = new int[size];
    for (j = 0; j < size; j++) {
        a[j] = val;
    }
    return a;
}

//------------------------------------------------------------
Local void deletArray(int *a)
{
    delete[] a;
}

//------------------------------------------------------------
Local void copyArray(int size, int *a0, int *a1)
{
    int j;
    for (j = 0; j < size; j++) {
        a1[j] = a0[j];
    }
}

//------------------------------------------------------------
Local int  **newMat(int n0, int n1, int val)
{
    int **m, j;

    m = new int*[n0];
    for (j = 0; j < n0; j++) {
        m[j] = newArray(n1, val);
    }
    return m;
}

//------------------------------------------------------------
Local void  deleteMat(int **m, int n0)
{
    int j;

    for (j = 0; j < n0; j++) {
        deletArray(m[j]);
    }
    delete[] m;
}

//------------------------------------------------------------
Local void bsort(int n, int *ord, int *a)
{
    int i, j, t;

    for (j = n-1; j > 0; j--) {
        for (i = 0; i < j; i++) {
            if(a[ord[i+1]] < a[ord[i]]) {
                t = ord[i];
                ord[i] = ord[i+1];
                ord[i+1] = t;
            }
        }
    }
#if 0
    printf("+++ bsort: n=%d\n", n);
    for (i = 0; i < n; i++) {
        printf("%d: o=%d ao=%d\n", i, ord[i], a[ord[i]]);
    }
#endif
}
//--------------------------------------------------------------
Local void prMomStr(int mom, const char *ms, int mn)
{
    if (mom == 0) {
        return;
    } else if (mom == 1) {
        printf(" + %s%d", ms, mn);
    } else if (mom > 0) {
        printf(" + %d*%s%d", mom, ms, mn);
    } else if (mom == -1) {
        printf(" - %s%d", ms, mn);
    } else {
        printf(" - %d*%s%d", -mom, ms, mn);
    }
}

//--------------------------------------------------------------
Local void printArray(int n, int *p)
{

    int j;

    printf("[");
    for (j = 0; j < n; j++) {
        printf(" %2d", p[j]);
    }
    printf("]");
}

//--------------------------------------------------------------
Local void printMat(int n0, int n1, int **m)
{
    int j;

    for (j = 0; j < n0; j++) {
        printArray(n1, m[j]);
        printf("\n");
    }
}

//--------------------------------------------------------------
Global void testPerm()
{
    // Function tests 'nextPerm()

    int nelem, nperm, nclist, n, count;
    int *p, *q, *r;
    int clist[] = {1, 2, 2, 3};
  
    nclist = sizeof(clist)/sizeof(int);
    nelem = 0;
    nperm = 1;
    for (n = 0; n < nclist; n++) {
        nelem += clist[n];
        nperm *= factorial(clist[n]);
    }
  
    printf("+++ clist = (%d) ", nclist);
    printArray(nclist, clist);
    printf("\n");
    printf("+++ nelem = %d, nperm = %d\n", nelem, nperm);
  
    p  = new int[nelem];
    q  = new int[nelem];
    r  = new int[nelem];
    count = 0;
    while (True) {
        count = nextPerm(nelem, nclist, clist, r, q, p, count);
        if (count < 0) {
            count = - count;
            break;
        }
        printf("%4d:", count);
        printArray(nelem, p);
        printf("\n");
  
        if (count > nperm) {
            break;
        }
    }
    if (count != nperm) {
        printf("*** %d != %d\n", count, nperm);
    }
    delete[] p;
    delete[] q;
    delete[] r;
}

// } ) ]

