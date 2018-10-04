#pragma once

// Temporal definition of big integer
#define BigInt long
#define ToBigInt(x)  ((BigInt) (x))
//
// Temporal definition of ratio of two big integers
#define Fraction double
#define ToFraction(x, y)  (((double) (x))/((double) (y)))

//==============================================================
// Output to FROM

//==============================================================
// class of nodes for EGraph

class ENode {
  public:
    int  deg;
    int  ext;
    int *edges;
};

class EEdge {
  public:
    int  nodes[2];
    int  ext;
    int  momn;
    char momc[2];       // no more used
    // momentum is printed like: ("%s%d", (enode.ext)?"Q":"p", enode.momn)
	char padding[6];
};

class EGraph {
  public:
    long gId;
    int  pId;
    int nNodes;
    int nEdges;
    int maxdeg;
    int nExtern;

    int opi;
    BigInt nsym, esym;

    ENode *nodes;
    EEdge *edges;

    EGraph(int nnodes, int nedges, int mxdeg);
    ~EGraph();
    void print(void);
    void init(int pid, long gid, int **adjmat, int sopi, BigInt nsym, BigInt esym);

    void setExtern(int nd, int val);
    void endSetExtern(void);
};

//==============================================================
// class of nodes for MGraph

class MNode {
  public:
    int id;               // node id
    int deg;              // degree(node) = the number of legs
    int clss;             // initial class number in which the node belongs
    int ext;

    int freelg;           // the number of free legs
    int visited;

    MNode(int id, int deg, int ext, int clss);
};

//===============================================================
//  class of node-classes for MGraph
class MNodeClass;

//===============================================================
//  class of scalar graph expressed by matrix form
// 
//  Input  : the classified set of nodes.
//  Output : control passed to 'EGraph(self)'

class MGraph {

  public:

    // initial conditions
    int pId;               // process/subprocess ID
    int nNodes;            // the number of nodes
    int nEdges;            // the number of edges
    int nLoops;            // the number of loops
    MNode **nodes;          // table of MNode object
    int *clist;             // list of initial classes
    int nClasses;          // the number of initial classes
    // int ndcl;              // node --> initial class number
    int mindeg;            // minimum value of degree of nodes
    int maxdeg;            // maximum value of degree of nodes
  
    int selOPI;           // flag to select 1PI graphs

    // generated set of graphs
    long ndiag;             // the total number of generated graphs
    long n1PI;              // the total number of 1PI graphs
    int  nBridges;          // the number of bridges
  
    // the current graph
    int c1PI;               // the number of 1PI components
    int **adjMat;           // adjacency matrix
    BigInt nsym;               // symmetry factor from nodes
    BigInt esym;               // symmetry factor from edges
    Fraction wsum;         // weighted sum of graphs
    Fraction wsopi;        // weighted sum of 1PI graphs
    MNodeClass *curcl;      // the current 'MNodeClass' object
    EGraph *egraph;
  
    // measures of efficiency
    long ngen;              // generated graph before check
    long ngconn;            // generated connected graph before check
  
    long nCallRefine;        
    long discardOrd;        
    long discardRefine;        
    long discardDisc;        
    long discardIso;        

    // functions */
    MGraph(int pid, int ncl, int *cldeg, int *clnum, int *clext, int sopi);
    ~MGraph();
    long   generate(void);

  private:
    // work space for isomorphism
    int **modmat;           // permutated adjacency matrix
    int *permp;
    int *permq;
    int *permr;

    // work space for biconnected component
    int *bidef;
    int *bilow;
    int  bicount;
	int  padding;

    void   printAdjMat(MNodeClass *cl);
    int    isConnected(void);
    int    visit(int nd);
    int    isIsomorphic(MNodeClass *cl);
    void   permMat(int size, int *perm, int **mat0, int **mat1);
    int    compMat(int size, int **mat0, int **mat1);
    MNodeClass *refineClass(MNodeClass *cl, int cn);
    void   bisearchM(int nd, int pd, int ne);
    int    count1PI(void);

    int    findNextCl(MNodeClass *cl, int *dscl);
    int    findNextTCl(MNodeClass *cl, int *dcl);
    void   connectClass(MNodeClass *cl, int *dscl);
    void   connectNode(int sc, int ss, MNodeClass *cl, int *dscl);
    void   connectLeg(int sc, int sn, int tc, int ts, MNodeClass *cl, int *dscl, int* dtcl);
    void   newGraph(MNodeClass *cl);

};
