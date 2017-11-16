#pragma once

//==============================================================
// Temporal definition of big integer
#define BigInt long
#define ToBigInt(x)  ((BigInt) (x))
//
// Temporal definition of ratio of two big integers
#define Fraction double
#define ToFraction(x, y)  (((double) (x))/((double) (y)))

//==============================================================
// classes
class EGraph;
class MGraph;

typedef int Bool;
const int True  = 1;
const int False = 0;

//==============================================================
// Output to FROM
void toForm(EGraph *egraph);

//==============================================================
// class of options

#define OPT_1PI           0
#define OPT_NoTadpole     1
#define OPT_No1PtBlock    2
#define OPT_No2PtL1PI     3
#define OPT_Size          4

#define OPT_NAMES  {\
    "Only 1PI graphs",\
    "Exclude graphs with Tadpoles (2 Edge connected)",\
    "Exclude graphs with Tadpole blocks",\
    "Exclude graphs with 2-point subgraphs",\
}

#define OPT_DEFAULTS { True, True, False, False }

#ifdef DEFGLOBAL
       const char **OPT_names;
#else
extern const char **OPT_names;
#endif

//--------------------------------------------------------------
class Options {
  public:
    int values[OPT_Size];      // array of options

    // default options
    Options();
    Bool normalize(int print);
    void copy(Options *opt);
    void print(void);

};

//==============================================================
// class of nodes for EGraph

#define EG_ED_Undef   0
#define EG_ED_Deleted 1
#define EG_ED_Back    2
#define EG_ED_Bridge  3
#define EG_ED_Inloop  4
#define EG_ED_Size    5
#define EG_ED_NAMES   {"Undef", "Deleted", "Back_Edge", \
                       "Bridge", "In_Loop"}

#define EG_ND_Undef   0
#define EG_ND_Deleted 1
#define EG_ND_Initial 2
#define EG_ND_Final   3
#define EG_ND_CPoint  4
#define EG_ND_VBlock  5
#define EG_ND_Inblock 6
#define EG_ND_Size    7
#define EG_ND_NAMES   {"Undef", "Deleted", "Init", "Final", "CPoint", "VBlock", "In_Block"}

#ifdef DEFGLOBAL
       const char **EG_ND_names;
       const char **EG_ED_names;
#else
extern const char **EG_ND_names;
extern const char **EG_ED_names;
#endif

class ENode;
class EEdge;

//--------------------------------------------------------------
class ENode {
  public:
    int *edges;
    int *klow;               // used in EGraph::biconn()
    int  deg;
    int  extloop;
    int  ndtype;

    int  padding0;

    ENode();
    ENode(int loops, int maxdeg);
    ~ENode();
    void init(int loops, int maxdeg);
    void copy(ENode *en);
    void setType(int typ);

};

//--------------------------------------------------------------
class EEdge {
  // momentum is printed like: ("%s%d", (enode.ext)?"Q":"p", enode.momn)
  public:
    int  nodes[2];
    int  ext;
    int  momn;
    int  dir;              // direction consistent with momn ?

    Bool cut;
    int  visited;
    int  conid;            // connected component
    int  edtype;           // type
    int opicomp;

    int *emom;             // external momenta
    int *lmom;             // loop momenta
    int *extMom;           // set of external momenta.


    EEdge();
    ~EEdge();
    void copy(EEdge *ee);
    void init(int nnodes, int nelges);

    void setType(int typ);
    void setLMom(int k, int dir);
    void setEMom(int nedges, int *extn, int dir);
};

class EGraph {
  public:
    Options options;       // table of options
    MGraph  *mgraph;

    int sNodes;            // memory size
    int sEdges;            // memory size
    int sMaxdeg;           // memory size
    int sLoops;            // memory size

    long gId;
    long gSubId;
    int  pId;
    int nNodes;
    int nEdges;
    int nExtern;

    BigInt nsym, esym;

    ENode *nodes;
    EEdge *edges;          // edges[nEdges+1]: index starts from 1

    int nLoops;

    int bconn;
    int *bidef;
    int *bilow;
    int *extMom;
    int bicount;
    int loopm;
    int opiCount;

    int nopicomp;
    int opi2plp;
    int nopi2p;

    EGraph(int nnodes, int nedges, int mxdeg);
    ~EGraph();
    void copy(EGraph *eg);
    void print(void);
    void fromMGraph(MGraph *mgraph);
    Bool isOptE(void);

    Bool isExternal(int nd)  { return (nodes[nd].extloop < 0); };

    void setExtLoop(int nd, int val);
    void endSetExtLoop(void);

    void biconnE(void);
    void biinitE(void);
    void bisearchE(int nd, int *extlst, int *intlst, int *opiext, int *opiloop);

    int  findRoot(void);
    int  dirEdge(int n, int e);
    void extMomConsv(void);
    int  cmpMom(int *lm0, int *em0, int *lm1, int *em1);
    int  groupLMom(int *grp, int *ed2gr);

    void chkMomConsv(void);
};

//==============================================================
// class of nodes for MGraph

class MNode {
  public:
    int id;               // node id
    int deg;              // degree(node) = the number of legs
    int clss;             // initial class number in which the node belongs
    int extloop;

    int freelg;           // the number of free legs
    int visited;

    MNode(int id, int deg, int extloop, int clss);
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
    MNode **nodes;         // table of MNode object
    int   *clist;          // list of initial classes
    Options options;       // table of options
    int pId;               // process/subprocess ID
    int nNodes;            // the number of nodes
    int nEdges;            // the number of edges
    int nLoops;            // the number of loops
    int nExtern;           // the number of external nodes
    int nClasses;          // the number of initial classes
    int mindeg;            // minimum value of degree of nodes
    int maxdeg;            // maximum value of degree of nodes

    // the current graph
    int **adjMat;           // adjacency matrix
    MNodeClass *curcl;      // the current 'MNodeClass' object
    EGraph *egraph;
    BigInt nsym;               // symmetry factor from nodes
    BigInt esym;               // symmetry factor from edges
    // generated set of graphs
    long cDiag;             // the total number of generated graphs
    long c1PI;              // the total number of 1PI graphs
    long cNoTadpole;        // the total number of graphs without tadpoles
    long cNoTadBlock;       // the total number of graphs without tad-blocks
    long c1PINoTadBlock;    // the total number of 1PI graphs without tad-blocks
    Fraction wscon;         // weighted sum of graphs
    Fraction wsopi;         // weighted sum of 1PI graphs

    // measures of efficiency
    long ngen;              // generated graph before check
    long ngconn;            // generated connected graph before check
  
    long nCallRefine;        
    long discardRefine;        
    long discardDisc;        
    long discardIso;        

    // for options
    int  nExtEdges;         // the number of external edges
    int  n1PIComps;         // the number of (tree and looped) 1PI components
    int  nBridges;          // the number of bridges
    int  nBlocks;           // the number of blocks
    int  nTadpoles;         // the number of tadpoles (looped 2 edge connected)
    int  nTadBlocks;        // the number of tadpole-blocks (looped 2 connected)
    Bool opi;
    Bool tadpole;
    Bool tadBlock;
  
    // functions */
    MGraph(int pid, int ncl, int *cldeg, int *clnum, int *clexl, int *opt);
    ~MGraph();
    long   generate(void);
    Bool   isExternal(int nd)  { return (nodes[nd]->extloop < 0); };
    void   printAdjMat(MNodeClass *cl);

  private:
    int padding0;

    // work space for isomorphism
    int **modmat;           // permutated adjacency matrix
    int *permp;
    int *permq;
    int *permr;

    // work space for biconnected component
    int *bidef;
    int *bilow;
    int  bicount;

    int padding1;

    Bool   isConnected(void);
    Bool   visit(int nd);
    Bool   isIsomorphic(MNodeClass *cl);
    void   permMat(int size, int *perm, int **mat0, int **mat1);
    int    compMat(int size, int **mat0, int **mat1);
    MNodeClass *refineClass(MNodeClass *cl);
    void   bisearchM(int nd, int pd, int ned, int *next, int *loop);
    void   biconnM(void);
    Bool   isOptM(void);

    void   connectClass(MNodeClass *cl);
    void   connectNode(int sc, int ss, MNodeClass *cl);
    void   connectLeg(int sc, int sn, int tc, int ts, MNodeClass *cl);
    void   newGraph(MNodeClass *cl);

};
