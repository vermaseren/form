// { ( [

#ifdef HAVE_CONFIG_H
#ifndef CONFIG_H_INCLUDED
#define CONFIG_H_INCLUDED
#include <config.h>
#endif
#endif

#include <iostream>
#include <iomanip>
#include <sstream>
#include <cstdio>
#include <stdlib.h>

#include "gentopo.h"

#define DUMMYUSE(x) (void)(x)

using namespace std;

//	The next limitation is imposed by the fact that the latest compilers
//	can give warnings on declarations like "int dtcl1[nNodes]"
//	With a bit of overkil there should be no real problems.
#define MAXNODES 100
#define MAXNCLASSES 100

// Generate scalar connected Feynman graphs.

//==============================================================

typedef int Bool;
const int True  = 1;
const int False = 0;

// compile options
const int CHECK    = True;
//const int MONITOR  = True;
const int MONITOR  = False;

const int OPTPRINT = False;

const int DEBUG0 = False;
//const int DEBUG1 = False;
const int DEBUG  = False;

// for debugging memory use
#define DEBUGM   False

//==============================================================
class MGraph;
class EGraph;

#define Extern 
#define Global
#define Local 

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
Local void deleteMat(int **m, int n0, int n1);
Local void printArray(int n, int *p);
Local void printMat(int n0, int n1, int **m);

#if DEBUGM
static int countNMC = 0;
static int countEG = 0;
static int countMG = 0;
#endif

//==============================================================
// class EGraph

EGraph::EGraph(int nnodes, int nedges, int mxdeg)
{
    int j;

    nNodes  = nnodes;
    nEdges  = nedges;
    maxdeg  = mxdeg;    // maxmum value of degree of nodes
    nExtern = 0;

    nodes = new ENode[nNodes];
    edges = new EEdge[nEdges+1];
    for (j = 0; j < nNodes; j++) {
      nodes[j].deg   = 0;
      nodes[j].edges = new int[maxdeg];
    }
#if DEBUGM
    printf("+++ new    EGraph %d\n", ++countEG);
    if(countEG > 100) { exit(1); }
#endif
}

EGraph::~EGraph()
{
    int j;

    for (j = 0; j < nNodes; j++) {
      delete[] nodes[j].edges;
    }
    delete[] nodes;
    delete[] edges;
#if DEBUGM
    printf("+++ delete EGraph %d\n", countEG--);
#endif
}

// construct EGraph from adjacency matrix
void EGraph::init(int pid, long gid, int **adjmat, Bool sopi, BigInt nsm, BigInt esm)
{
    int n0, n1, ed, e, eext, eint;
//    Bool ok;

    pId  = pid;
    gId  = gid;
    opi  = sopi;
    nsym = nsm;
    esym = esm;

    for (n0 = 0; n0 < nNodes; n0++) {
        nodes[n0].deg = 0;
    }
    ed = 1;
    for (n0 = 0; n0 < nNodes; n0++) {
      for (e = 0; e < adjmat[n0][n0]/2; e++, ed++) {
        edges[ed].nodes[0] = n0;
        edges[ed].nodes[1] = n0;
        nodes[n0].edges[nodes[n0].deg++] = - ed;
        nodes[n0].edges[nodes[n0].deg++] =   ed;
        edges[ed].ext = nodes[n0].ext;
      }
      for (n1 = n0+1; n1 < nNodes; n1++) {
        for (e = 0; e < adjmat[n0][n1]; e++, ed++) {
          edges[ed].nodes[0] = n0;
          edges[ed].nodes[1] = n1;
          nodes[n0].edges[nodes[n0].deg++] = - ed;
          nodes[n1].edges[nodes[n1].deg++] =   ed;
          edges[ed].ext = (nodes[n0].ext || nodes[n1].ext);
        }
      }
    }
    if (CHECK) {
      if (ed != nEdges+1) {
        printf("*** EGraph::init: ed=%d != nEdges=%d\n", ed, nEdges);
        erEnd("*** EGraph::init: illegal connection\n");
      }
    }

    // name of momenta
    eext = 1;
    eint = 1;
    for (ed = 1; ed <= nEdges; ed++) {
      if (edges[ed].ext) {
        edges[ed].momn    = eext++;
        edges[ed].momc[0] = 'Q';
        edges[ed].momc[1] = ((char)0);
      } else {
        edges[ed].momn    = eint++;
        edges[ed].momc[0] = 'P';
        edges[ed].momc[1] = ((char)0);
      }
    }
}

// set external particle to node 'nd'
void EGraph::setExtern(int nd, Bool val)
{
    nodes[nd].ext = val;
}

// end of calling 'setExtern'
void EGraph::endSetExtern(void)
{
    int n;

    nExtern = 0;
    for (n = 0; n < nNodes; n++) {
      if (nodes[n].ext) {
        nExtern++;
      }
    }
}

// print the EGraph
void EGraph::print()
{
  int nd, lg, ed, nlp;

  nlp = nEdges - nNodes + 1;
  printf("\n");
  printf("EGraph: pId=%d gId=%ld nExtern=%d nLoops=%d nNodes=%d nEdges=%d\n",
         pId, gId, nExtern, nlp, nNodes, nEdges);
  printf("        sym = (%ld * %ld) maxdeg=%d\n", nsym, esym, maxdeg);
  printf("  Nodes\n");
  for (nd = 0; nd < nNodes; nd++) {
    if (nodes[nd].ext) {
      printf("    %2d Extern ", nd);
    } else {
      printf("    %2d Vertex ", nd);
    }
    printf("deg=%d [", nodes[nd].deg);
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
    printf("%s%d", (edges[ed].ext)?"Q":"p", edges[ed].momn);
    printf(" [%3d %3d]\n", edges[ed].nodes[1], edges[ed].nodes[0]);
  }
}

//==============================================================
// class MNode : nodes in MGraph

MNode::MNode(int vid, int vdeg, Bool vext, int vclss)
{
    id     = vid;    // id of the node
    deg    = vdeg;   // degree of the node
    freelg = vdeg;   // number of free legs
    clss   = vclss;  // class to which the node belogns
    ext    = vext;   // external node or not
}

//===============================================================
//  class of node-classes for MGraph
//
class MNodeClass {
  public:
    int   nNodes;                  // the number of nodes
    int   nClasses;                // the number of classes
    int  *clist;                   // the number of nodes in each class
    int  *ndcl;                    // node --> class
    int **clmat;                   // matrix used for classification
    int  *flist;                   // the first node in each class
    int   maxdeg;                  // maximal value of degree(node)
    int   forallignment;

    MNodeClass(int nnodes, int ncl);
    ~MNodeClass();
    void  init(int *cl, int mxdeg, int **adjmat);
    void  copy(MNodeClass* mnc);
    int   clCmp(int nd0, int nd1, int cn);
    void  printMat(void);

    void  mkFlist(void);
    void  mkNdCl(void);
    void  mkClMat(int **adjmat);
    void  incMat(int nd, int td, int val);
    Bool  chkOrd(int nd, int ndc, MNodeClass *cl, int *dtcl);
    int   cmpArray(int *a0, int *a1, int ma);
};

MNodeClass::MNodeClass(int nnodes, int ncl)
{
    nNodes   = nnodes;
    nClasses = ncl;
    clist    = new int[nClasses];
    ndcl     = new int[nNodes];
    clmat    = newMat(nNodes, nClasses, 0);
    flist    = new int[nClasses+1];

#if DEBUGM
    printf("+++ new    MNodeClass %d\n", ++countNMC);
    if(countNMC > 100) { exit(1); }
#endif
}

MNodeClass::~MNodeClass()
{
    delete[] clist;
    delete[] ndcl;
    deleteMat(clmat, nNodes, nClasses);
    delete[] flist;

#if DEBUGM
    printf("+++ delete MNodeClass %d\n", countNMC--);
#endif
}

void MNodeClass::init(int *cl, int mxdeg, int **adjmat)
{
    int j;

    for (j = 0; j < nClasses; j++) {
      clist[j] = cl[j];
    }
    maxdeg   = mxdeg;
    mkNdCl();
    mkClMat(adjmat);
    mkFlist();
}

void MNodeClass::copy(MNodeClass* mnc)
{
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

//  Construct flist
//    The set of nodes in class 'cl' is [flist[cl],...,flist[cl+1]-1]
void MNodeClass::mkFlist(void)
{
    int  j, f;

    f  = 0;
    for (j = 0; j < nClasses; j++) {
      flist[j] = f;
      f += clist[j];
    }
    flist[nClasses] = f;
}

//  Construct ndcl
//    ndcl[nd] = (the class id in which node 'nd' belongs)
void MNodeClass::mkNdCl(void)
{
    int  c, k;
    int  nd = 0;

    for (c = 0; c < nClasses; c++) {
      for (k = 0; k < clist[c]; k++) {
        ndcl[nd++] = c;
      }
    }
}

//  Comparison of two nodes 'nd0' and 'nd1'
//    Ordering is lexicographic (class, connection configuration)
int MNodeClass::clCmp(int nd0, int nd1, int cn)
{

    // Wether two nodes are in a same class or not.
    int cmp = ndcl[nd0] - ndcl[nd1];
    if (cmp != 0) {
      return cmp;
    }

    // Sign '-' signifies the reverse ordering
    cmp = - cmpArray(clmat[nd0], clmat[nd1], cn);
    if (cmp != 0) {
      return cmp;
    }
    // for particles ???
    return cmp;
}
    
//  Construct a matrix 'clmat[nd][tc]' which is the number
//  of edges connecting 'nd' and all nodes in class 'tc'.
void MNodeClass::mkClMat(int **adjmat)
{
    int  nd, td, tc;

    for (nd = 0; nd < nNodes; nd++) {
      for (td = 0; td < nNodes; td++) {
        tc = ndcl[td];                      // another node
        clmat[nd][tc] += adjmat[nd][td];    // the number of edges 'nd'--'td'
      }

    }
}

//  Increase the number of edges by 'val' between 'nd' and 'td'.
void MNodeClass::incMat(int nd, int td, int val)
{
    int tdc = ndcl[td];             // class of 'td'
    clmat[nd][tdc] += val;          // modify matrix 'clmat'.
}

//  Check whether the configuration satisfies the ordering condition or not.
Bool MNodeClass::chkOrd(int nd, int ndc, MNodeClass *cl, int *dtcl)
{
    Bool tcl[MAXNCLASSES];
//    Bool tcl[cl->nClasses];
    int  tn, tc, mxn, cmp, n;
	DUMMYUSE(nd);

    for (tc = 0; tc < cl->nClasses; tc++) {
        tcl[tc] = False;
    }
    for (tn = 0; tn < nNodes; tn++) {
      if (dtcl[tn] == 0) {
        tcl[cl->ndcl[tn]] = False;
      }
    }
    for (tc = 0; tc < cl->nClasses; tc++) {
      if (tcl[tc] && ndc != tc) {
        mxn = flist[tc+1];
        for (n = flist[tc]+1; n < mxn; n++) {
          cmp = - clmat[n-1][tc] + clmat[n][tc];
          if (cmp > 0) {
            return False;
          }
        }
      }
    }
    return True;
}

//  Print configuration matrix.
void MNodeClass::printMat(void)
{
    int j1, j2;

    cout << endl;
  
    // the first line
    cout << setw(2) << "nd" << ": " << setw(2) << "cl:   ";
    for (j2 = 0; j2 < nClasses; j2++) {
      cout << setw(2) << j2 << " ";
    }
    cout << endl;
  
    // print raw
    for (j1 = 0; j1 < nNodes; j1++) {
      cout << setw(2) << j1 << ": " << setw(2) << ndcl[j1] << ": [";
      for (j2 = 0; j2 < nClasses; j2++) {
        cout << " " << setw(2) << clmat[j1][j2];
      }
      cout << "] " << endl;
    }
}

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

//===============================================================
//  class MGraph : scalar graph expressed by matrix form

MGraph::MGraph(int pid, int ncl, int *cldeg, int *clnum, int *clext, Bool sopi)
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
                 j, cldeg[j], clnum[j], clext[j]);
        }
        erEnd("illegal degrees of nodes");
    }
    pId    = pid;
    nNodes = nn;
    nodes = new MNode*[nNodes];

    selOPI = sopi;

    nEdges = ne / 2;
    nLoops = nEdges - nNodes + 1;

    egraph = new EGraph(nNodes, nEdges, maxdeg);
    nn = 0;
    for (j = 0; j < nClasses; j++) {
      for (k = 0; k < clist[j]; k++, nn++) {
        nodes[nn] = new MNode(nn, cldeg[j], clext[j], j);
        egraph->setExtern(nn, clext[j]);
      }
    }
    egraph->endSetExtern();

    // generated set of graphs
    ndiag    = 0;
    n1PI     = 0;

    // the current graph
    adjMat   = newMat(nNodes, nNodes, 0);
    nsym     = ToBigInt(0);
    esym     = ToBigInt(0);
    c1PI     = 0;
    wsum  = ToFraction(0, 1);
    wsopi = ToFraction(0, 1);

    // current node classification
    curcl    = new MNodeClass(nNodes, nClasses);

    // measures of efficiency
    ngen     = 0;
    ngconn   = 0;

    if (MONITOR) {
      nCallRefine    = 0;
      discardOrd     = 0;
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
	DUMMYUSE(padding);

#if DEBUGM
    printf("+++ new    MGraph %d\n", ++countMG);
    if(countEG > 100) { exit(1); }
#endif
}

MGraph::~MGraph()
{
    int j;

    deletArray(bilow);
    deletArray(bidef);
    deletArray(permr);
    deletArray(permq);
    deletArray(permp);

    deleteMat(modmat, nNodes, nNodes);
    delete curcl;
    deleteMat(adjMat, nNodes, nNodes);
    delete egraph;
    for (j = 0; j < nNodes; j++) {
      delete nodes[j];
    }
    delete[] nodes;
    delete[] clist;

#if DEBUGM
    printf("+++ delete MGraph %d\n", countMG++);
#endif
}
  

void MGraph::printAdjMat(MNodeClass *cl)
{
    int j1, j2;

    cout << "     ";
    for (j2 = 0; j2 < nNodes; j2++) {
      cout << " " << setw(2) << j2;
    }
    cout << endl;
    for (j1 = 0; j1 < nNodes; j1++) {
      cout << setw(2) << j1 << ": [";
      for (j2 = 0; j2 < nNodes; j2++) {
        cout << " " << setw(2) << adjMat[j1][j2];
      }
      cout << "] " << cl->ndcl[j1] << endl;
    }
}

//---------------------------------------------------------------
//  Check graph can be a connected one.
//  If a connected component without free leg is not the whole graph then
//  return False, otherwise return True.
Bool MGraph::isConnected(void)
{
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

//  Visiting connected node used for 'isConnected'
//  If child nodes has free legs, then this function returns True.
//  otherwise it returns False.
Bool MGraph::visit(int nd)
{
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

//  Check whether the current graph is the Representative of a isomorphic class.
//    nsym = symmetry factor by the permutation of nodes.
//    esym = symmetry factor by the permutation of edge.
//  If this graph is not a representative, then returns False.
Bool MGraph::isIsomorphic(MNodeClass *cl)
{
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

// apply permutation to matrix 'mat0' and obtain 'mat1'
void MGraph::permMat(int size, int *perm, int **mat0, int **mat1)
{
    int j1, j2;

    for (j1 = 0; j1 < size; j1++) {
        for (j2 = 0; j2 < size; j2++) {
            mat1[j1][j2] = mat0[perm[j1]][perm[j2]];
        }
    }
}

// comparison of matrix
int MGraph::compMat(int size, int **mat0, int **mat1)
{
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


//  Refine the classification
//    cl : the current 'MNodeClass' object
//    cn : the class number
//  Returns (the new class number corresponds to 'cn') if OK, or
//          'None' if ordering condition is not satisfied
MNodeClass *MGraph::refineClass(MNodeClass *cl, int cn)
{
    MNodeClass *ccl = cl;
    int         ccn = cn;
    MNodeClass *ncl = NULL;
    int         ucl[MAXNODES];
    int         nucl, nce;
    int         td, cmp;

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
          if (DEBUG) {
            cout << "refine: cls = " << cl->clist << endl;
            cl->printMat();
            cout << "clmat" << endl;
            ccl->printMat();
            cout << "refine: discard: cls = " << ccl->clist 
                 << "ucl = " << ucl << endl;
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
  
      // class is not modified
      if (nucl == ccl->nClasses) {
        ncl = ccl;
        break;

      // inconsistent
      } else if (nucl < ccl->nClasses) {
        erEnd("refineClasses : smaller number of classes");

      // preparation of the next repetition.
      } else {
        if (cn == ccl->nClasses) {
          td = nNodes;
        } else {
          td = ccl->flist[cn+1]-1;
        }
        if (ccl != cl) {
          delete ccl;
        }
        ccl = new MNodeClass(nNodes, nucl);
        ccl->init(ucl, maxdeg, adjMat);
        if (td == nNodes) {
          ccn = ccl->nClasses;
        } else {
          ccn = ccl->ndcl[td];
        }
        nucl = 0;
      }
    }

    if (DEBUG) {
      cout << "refine: ucl = " << ucl << endl;
      cout << "refine: ncl = " << ncl->clist << endl;
      ncl->printMat();
    }

    return ncl;
}

//  Search biconnected component
//    visit : pd --> nd --> td
//    ne : the number of edges between pd and nd.
void MGraph::bisearchM(int nd, int pd, int ne)
{
    int td;

    bidef[nd] = bicount;
    bilow[nd] = bicount;
    bicount++;

    for (td = 0; td < nNodes; td++) {
      if (nodes[td]->ext) {  // ignore external node
        continue;

      } else if (td == nd) {         // pd --> nd --> nd
        continue;

      } else if (td == pd) {       // pd --> nd --> pd
        if (ne > 1) {
            bilow[nd] = min(bilow[nd], bidef[pd]);
        }

      } else if (adjMat[td][nd] < 1) {  // td is not adjacent to nd
        continue;

      } else if (bidef[td] >= 0) {  // back edge
        bilow[nd] = min(bilow[nd], bidef[td]);

      // new node
      } else {

        bisearchM(td, nd, adjMat[td][nd]);

        // ordinary case
        if (bilow[td] > bidef[nd]) {
          nBridges++;
        }
        bilow[nd] = min(bilow[nd], bilow[td]);
      }
    }

    // nd is the starting point and not an external line
    // if (pd < 0 && (!nodes[nd]->ext || nodes[nd]->deg > 1)) {
    // if (pd < 0 && !(nodes[nd]->ext && nodes[nd]->deg ==1)) {
    if (pd < 0 && nodes[nd]->deg != 1) {
      // nBridges += nodes[nd]->deg;
      nBridges++;
    }
}

//  Count the number of 1PI components.
//  The algorithm is described in
//  A.V. Aho, J.E. Hopcroft and J.D. Ullman
//   'The Design and Analysis of Computer Algorithms', Chap. 5
//   1974, Addison-Wesley.

int MGraph::count1PI(void)
{
    int j;

    if (nLoops < 0) {
      return 1;
    }

    // initialization
    bicount  = 0;
    nBridges = 0;
    for (j = 0; j < nNodes; j++) {
      bidef[j] = -1;
      bilow[j] = -1;
    }
    
    bisearchM(0, -1, 0);

    return nBridges;
}

//---------------------------------------------------------------
//  Generate graphs
//    the generation process starts from 'connectClass'.

long MGraph::generate(void)
{
    MNodeClass *cl;
    int dscl[MAXNODES];
    int n;

    for (n = 0; n < nNodes; n++) {
      dscl[n] = False;
    }

    // Initial classification of nodes.
    cl = new MNodeClass(nNodes, nClasses);
    cl->init(clist, maxdeg, adjMat);
    connectClass(cl, dscl);

    // Print the result.

    if (MONITOR) {
    cout << endl;
    cout << endl;
    cout << "* Total " << ndiag << " Graphs.";
    cout << "(" << n1PI << " 1PI)";
    // cout << " wsum = " << wsum << "(" << wsopi << "1PI)" << endl;
    cout << endl;
	}

    if (MONITOR) {
      cout << "* refine:                  " << nCallRefine << endl;
      cout << "* discard for ordering:     " << discardOrd << endl;
      cout << "* discard for refinement:  " << discardRefine << endl;
      cout << "* discard for disconnected: " << discardDisc << endl;
      cout << "* discard for duplication:  " << discardIso << endl;
    }
    delete cl;

    return ndiag;
}

int MGraph::findNextCl(MNodeClass *cl, int *dscl)
{
    int mine, cr, c, n, me;

    mine = -1;
    cr   = -1;
    for (c = 0; c < cl->nClasses; c++) {
      n = cl->flist[c];
      if (!dscl[n]) {
        me = nodes[n]->freelg;
        if (me > 0) {
          if (nodes[n]->freelg < nodes[n]->deg) {
            return c;
          }
          if (mine < 0 || mine > me) {
            mine = me;
            cr = c;
          }
        }
      }
    }
    return cr;
}

int MGraph::findNextTCl(MNodeClass *cl, int *dtcl)
{
    int c, n, mine, cr, me;

    mine = -1;
    cr   = -1;
    for (c = 0; c < cl->nClasses; c++) {
      for (n = cl->flist[c]; n < cl->flist[c+1]; n++) {
        if (!dtcl[n]) {
          me = nodes[n]->freelg;
          if (me > 0) {
            if (nodes[n]->freelg < nodes[n]->deg) {
              return c;
            }
            if (mine < 0 || mine > me) {
              mine = me;
              cr = c;
            }
          }
        }
      }
    }
    return cr;
}

// Connect nodes in a class to others
void MGraph::connectClass(MNodeClass *cl, int *dscl)
{
    int sc, sn;
    MNodeClass *xcl;

    if (DEBUG0) {
      printf("connectClass:begin:");
      printf(" dscl="); printArray(nNodes, dscl);
      printf("\n");
    }
    xcl = refineClass(cl, cl->nClasses);

    if (xcl == NULL) {
      if (MONITOR) {
        discardRefine++;
      }
    } else {
      sc = findNextCl(xcl, dscl);
      if (sc < 0) {
        newGraph(cl);
      } else {
        sn = xcl->flist[sc];
        connectNode(sc, sn, xcl, dscl);
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
void MGraph::connectNode(int sc, int ss, MNodeClass *cl, int *dscl)
{
    int sn;
    int dtcl[MAXNODES];

    if (DEBUG0) {
      printf("connectNode:begin:(%d,%d)", sc, ss);
      printf(" dscl="); printArray(nNodes, dscl);
      printf("\n");
    }
    if (ss >= cl->flist[sc+1]) {
      connectClass(cl, dscl);
      if (DEBUG0) {
        printf("connectNode:end1\n");
      }
      return;
    }

    copyArray(nNodes, dscl, dtcl);
    for (sn = ss; sn < cl->flist[sc+1]; sn++) {
      if (!dscl[sn]) {
        connectLeg(sc, sn, sc, sn, cl, dscl, dtcl);
        if (DEBUG0) {
          printf("connectNode:end2\n");
        }
        return;
      }
    }
    if (DEBUG0) {
      printf("connectNode:end3\n");
    }
}

//  Add one connection between two legs.
//    1. select another node to connect
//    2. determine multiplicity of the connection
// 
//   Arguments
//     cn : the current class
//     nd : the current node to be connected.
//     nextnd : {nextnd, ...} is the possible target node of the connection.
//     cl     : the current node class

void MGraph::connectLeg(int sc, int sn, int tc, int ts, MNodeClass *cl, int *dscl, int* dtcl)
{
    int tn, maxself, nc2, nc, maxcon, ts1, wc, ncm;
    int dtcl1[MAXNODES];

    if (DEBUG0) {
      printf("connectLeg:begin:(%d,%d,%d,%d)", sc, sn, tc, ts);
      printf(" dscl="); printArray(nNodes, dscl);
      printf(" dtcl="); printArray(nNodes, dtcl);
      printf("\n");
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
        dscl[sn] = True;

        // next node in the current class.
        connectNode(sc, sn+1, cl, dscl);

        dscl[sn] = False;
      }

    // connect a free leg of the current node 'sn'.
    } else { 
      copyArray(nNodes, dtcl, dtcl1);

      ts1 = ts;
      for (wc = 0; wc < nNodes; wc++) {
        if (ts1 >= cl->flist[tc+1]) {
          tc = findNextTCl(cl, dtcl1);
          if (DEBUG0) {
            printf("connectLeg:1:tc=%d\n", tc);
          }
          if (tc < 0) {
            if (DEBUG0) {
              printf("connectLeg:end1:(%d,%d,%d,%d)\n", sc, sn, tc, ts);
            }
            return;
          }
          if (tc != sc && !cl->chkOrd(sn, sc, cl, dtcl)) {
            if (MONITOR) {
              discardOrd++;
            }
            if (DEBUG0) {
              printf("connectLeg:end2:(%d,%d,%d,%d)\n", sc, sn, tc, ts);
            }
            return;
          }
        }

        ts1 = -1;
  
        // repeat for all possible target nodes
        // for all tn in tc
        if (DEBUG0) {
          printf("connectLeg:2:tc=%d, fl:%d-->%d\n", tc, cl->flist[tc], cl->flist[tc+1]);
        }
        for (tn = cl->flist[tc]; tn < cl->flist[tc+1]; tn++) {
          if (DEBUG0) {
            printf("connectLeg:2:%d=>try %d\n", sn, tn);
          }

          if (dtcl1[tn]) {
            if (DEBUG0) {
              printf("connectLeg:3\n");
            }
            continue;
          }
          dtcl1[tn] = True;
  
          if (sc == tc && sn > tn) {
            if (DEBUG0) {
              printf("connectLeg:4\n");
            }
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
            if (selOPI and nNodes > 2) {
               maxself = min((nodes[sn]->deg-2)/2, maxself);
            }
  
            // vary the number of connection.
            for (nc2 = maxself; nc2 > 0; nc2--) {
              // if nNodes > 1 and ndg == nc2:
              //   // disconnected
              //   continue
              nc = 2*nc2;
              if (DEBUG0) {
                printf("connectLeg: call conLeg: (same) %d=>%d(%d)\n", sn, tn,nc);
              }
              ncm = 5*nc;
              adjMat[sn][sn] = nc;
              nodes[sn]->freelg -= nc;
              cl->incMat(sn, tn, ncm);
              ts1 = tn + 1;
  
              // next connection
              connectLeg(sc, sn, tc, ts1, cl, dscl, dtcl1);
  
              // restore the configuration
              cl->incMat(tn, sn, - ncm);
              adjMat[sn][sn] = 0;
              nodes[sn]->freelg += nc;
              if (DEBUG0) {
                printf("connectLeg: ret  conLeg: (same) %d=>%d(%d)\n", sn, tn,nc);
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
              if ((adjMat[sn][tn] != 0) || (adjMat[sn][tn] != adjMat[tn][sn])) {
                printf("*** inconsistent connection: sn=%d, tn=%d", sn, tn);
                printf(" dscl="); printArray(nNodes, dscl);
                printf(" dtcl="); printArray(nNodes, dtcl);
                printf("\n");
                printAdjMat(cl);
                erEnd("*** inconsistent connection ");
              }
            }
  
            // vary number of connections
            for (nc = maxcon; nc > 0; nc--) {
              if (DEBUG0) {
                printf("connectLeg: call conLeg: (diff) %d=>%d(%d)", sn, tn,nc);
                printf(" dtcl="); printArray(nNodes, dtcl);
                printf("\n");
              }
              adjMat[sn][tn] = nc;
              adjMat[tn][sn] = nc;
              nodes[sn]->freelg -= nc;
              nodes[tn]->freelg -= nc;
              cl->incMat(sn, tn, nc);
              cl->incMat(tn, sn, nc);
              ts1 = tn + 1;
  
              // next connection
              connectLeg(sc, sn, tc, ts1, cl, dscl, dtcl1);
    
              // restore configuration
              cl->incMat(sn, tn, - nc);
              cl->incMat(tn, sn, - nc);
              adjMat[sn][tn] = 0;
              adjMat[tn][sn] = 0;
              nodes[sn]->freelg += nc;
              nodes[tn]->freelg += nc;
              if (DEBUG0) {
                printf("connectLeg: ret  conLeg: (diff) %d=>%d(%d)\n", sn, tn,nc);
                printf(" dtcl="); printArray(nNodes, dtcl);
                printAdjMat(cl);
                printf("\n");
              }
            }
          }
        } 
        if (ts1 < 0) {
          ts1 = cl->flist[tc+1];
        }
      } // for wc
    } 
    if (DEBUG0) {
      printf("connectLeg:end3:(%d,%d,%d,%d)\n", sc, sn, tc, ts);
    }
}

//  A new candidate diagram is obtained.
//  It may be a disconnected graph

void MGraph::newGraph(MNodeClass *cl)
{
    int connected;
    MNodeClass *xcl;
    Bool sopi;

    ngen++;
    // printf("newGraph: %d\n", ngen);

    // refine class and check ordering condition
    xcl = refineClass(cl, cl->nClasses);
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
          cout << "+++ disconnected graph" << ngen << endl;
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
          if (DEBUG) {
            cout << "+++ duplicated graph" << ngen << ngconn << endl;
            xcl->printMat();
          }

          if (MONITOR) {
            discardIso++;
          }

        // We got a new connected and a unique representation of a class.
        } else {

          c1PI =  count1PI();
          if (!selOPI || c1PI == 1) {
            wsum = wsum + ToFraction(1, nsym*esym);
            if (c1PI == 1) {
               wsopi = wsopi + ToFraction(1, nsym*esym);
            }
            curcl->copy(xcl);
            ndiag++;
            sopi = (c1PI == 1);
            if (sopi) {
              n1PI++;
            }
  
            if (OPTPRINT) {
              cout << endl;
              cout << "Graph :" << ndiag 
                   << "(" << ngen << ")" 
                   << " 1PI comp. = " << c1PI 
                   << " sym. factor = (" << nsym << "*" << esym << ")" 
                   << endl;
              printAdjMat(cl);
              // cl->printMat();
              if (MONITOR) {
                cout << "refine:                  " << nCallRefine << endl;
                cout << "discard for ordering:     " << discardOrd << endl;
                cout << "discard for refinement:  " << discardRefine << endl;
                cout << "discard for disconnected: " << discardDisc << endl;
                cout << "discard for duplication:  " << discardIso << endl;
              }
            }
  
            // go to next step
            egraph->init(pId, ndiag, adjMat, sopi, nsym, esym);
            toForm(egraph);
          }
        }
      }
    }
// printf("in newGraph cl=%p, xcl=%p\n", cl, xcl);
    if (xcl != cl && xcl != NULL) {
      delete xcl;
    }
}


//  go to next step
//  1. convert data format which treat the edges as objects.
//  2. definition of loop momenta to the edges.
//  3. analysis of loop structure in a graph.
//  4. assignment of particles to edges and vertices to nodes
//  5. recalculate symmetry factor

// Permutation
Local int nextPerm(int nelem, int nclass, int *cl, int *r, int *q, int *p, int count)
{
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

Local BigInt factorial(int n)
/* return (n < 1) ? 1 : n!
 */
{
    int r, j;

    r = 1;
    for (j = 2; j <= n; j++) {
        r *= j;
    }
    return r;
}

Local BigInt ipow(int n, int p)
{
    int r, j;
	DUMMYUSE(p);

    r = 1;
    for (j = 2; j <= n; j++) {
        r *= n;
    }
    return r;
}


Local void erEnd(const char *msg)
{
    printf("*** Error : %s\n", msg);
    exit(1);
}

/* memory allocation */
Local int   *newArray(int size, int val)
{
    int *a, j;

    a = new int[size];
    for (j = 0; j < size; j++) {
        a[j] = val;
    }
    return a;
}

Local void deletArray(int *a)
{
    delete[] a;
}

Local void copyArray(int size, int *a0, int *a1)
{
    int j;
    for (j = 0; j < size; j++) {
        a1[j] = a0[j];
    }
}

Local int  **newMat(int n0, int n1, int val)
{
    int **m, j;

    m = new int*[n0];
    for (j = 0; j < n0; j++) {
        m[j] = newArray(n1, val);
    }
    return m;
}

Local void  deleteMat(int **m, int n0, int n1)
{
    int j;
	DUMMYUSE(n1);

    for (j = 0; j < n0; j++) {
        deletArray(m[j]);
    }
    delete[] m;
}

//==============================================================
// Functions for testing the program.
//
//--------------------------------------------------------------
// Testing function nextPerm
//
Local void printArray(int n, int *p)
{
    int j;

    printf("[");
    for (j = 0; j < n; j++) {
        printf(" %2d", p[j]);
    }
    printf("]");
}

Local void printMat(int n0, int n1, int **m)
{
    int j;

    for (j = 0; j < n0; j++) {
        printArray(n1, m[j]);
        printf("\n");
    }
}

Global void testPerm()
{
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
    delete p;
    delete q;
    delete r;
}

// } ) ]

