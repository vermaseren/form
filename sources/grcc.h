#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

//==============================================================
extern "C" {
#include "grccparam.h"
}

//==============================================================
// Name space of this library
// See 'grccparam.h' for #define
#ifdef GRCC_NAMESPACE
namespace Grcc {
#endif

//==============================================================
// For debugging
//
// #define MONITOR     // in gaph elimination
// #define CHECK       // check consistency
#define DEBUGABORT
// #define DEBUG
// #define DEBUG0
// #define DEBUG1
// #define DEBUGM

//==============================================================
// Common types
//
typedef int Bool;
const int True  = 1;
const int False = 0;


//==============================================================
// Big integer (may be replaced by suitable one)

#define BigInt       long
#define ToBigInt(x)  ((BigInt) (x))

//==============================================================
// Macro functions
#define Real(x)      ((double) (x))
#define Abs(x)       (((x) >= 0)? (x): (-x))
#define Sign(x)      (((x) >= 0)? (1): (-1))
#define Max(x, y)    ((x) > (y) ? (x) : (y))
#define Min(x, y)    ((x) < (y) ? (x) : (y))
#define Second(t) ((double)(*(t)=(double)(clock()/((double)CLOCKS_PER_SEC))))
#define isATExternal(x) ((x)==GRCC_AT_Initial||(x)==GRCC_AT_Final||(x)==GRCC_AT_External)

//==============================================================
// types and classes
typedef int  Edge2n[2];   // pair of nodes expressing an edge
class Assign;
class AStack;
class EEdge;
class EGraph;
class ENode;
class Interaction;
class MGraph;
class MNodeClass;
class MCOpi; 
class MCBridge;
class MCBlock;
class MConn;
class Model;
class MOrbits;
class Options;
class Output;
class Particle;
class PNodeClass;
class Process;
class SGroup;
class SProcess;
class DCGraph;

//==============================================================
// type of output functions
typedef Bool OutEGB(EGraph *, void *);
typedef void OutEG(EGraph *, void *);
typedef void ErExit(const char *msg, void *);

//==============================================================
class Options {
  public:
    
    Model      *model;
    Process    *proc;
    SProcess   *sproc;
    Output     *out;         // for output
    OutEGB     *outmg;       // call back function of mgraph
    OutEGB     *endmg;       // call back function of end of mgraph
    OutEG      *outag;       // call back function of agraph
    void       *argmg;       // additional argument to outmg
    void       *argemg;      // additional argument to endmg
    void       *argag;       // additional argument to outag

    //switches for graph generation
    int values[GRCC_OPT_Size];      // array of options

    int         DUMMYPADDING;

    // measuring time
    double time0;
    double time1;

    //------------------
    Options(void);
    ~Options(void);
    void setDefaultValue(void);

    void setValue(int ind, int val);
    int  getValue(int ind);

    void print(void);

    void setOutputF(Bool outgrf, const char *fname);
    void setOutputP(Bool outgrp, const char *fname);
    void printLevel(int l);

    void printModel(void);
    void setOutMG(OutEGB *omg, void *pt);
    void setOutAG(OutEG  *oag, void *pt);
    void setEndMG(OutEGB *omg, void *pt);
    void setErExit(ErExit  *ere, void *pt);
    const OptDef *getDef(void);

    void begin(Model *mdl);
    void end(void);
    void beginProc(Process *prc);
    void endProc(void);
    void beginSubProc(SProcess *sprc);
    void endSubProc(void);
    void newMGraph(MGraph *mgr);
    void newAGraph(EGraph *egr);
    void outModel(void);
};

//==============================================================
class Output {
  public:

    Options    *opt;
    Model      *model;
    Process    *proc;
    SProcess   *sproc;
    char       *outgrf;
    FILE       *outgrfp;
    char       *outgrp;
    FILE       *outgrpp;
    int         procId;
    Bool        outproc;

    Output(Options *optn);
    ~Output(void);

    void setOutgrf(const char *fname);
    void setOutgrp(const char *fname);
    Bool outBeginF(Model *mdl, Bool pr);
    Bool outBeginP(Model *mdl, Bool pr);
    void outEndF(void);
    void outEndP(void);
    void outProcBeginF(Process *prc);
    void outProcBeginP(Process *prc);
    void outProcBegin0(int next, int couple, int loop);
    void outSProcBeginF(SProcess *sprc);
    void outSProcBeginP(SProcess *sprc);
    void outProcEndF(void);
    void outProcEndP(void);
    void outEGraphF(EGraph *egraph);
    void outEGraphP(EGraph *egraph);
    void outModelF(void);
    void outModelP(void);

};

//==============================================================
class Fraction {
  public:
    BigInt num, den;
    double ratio;

    Fraction() { num = 0;  den = 1; };
    Fraction(BigInt n, BigInt d);

    void   print(const char *msg);
    void   setValue(BigInt n, BigInt d);
    void   setValue(Fraction &f);
    void   add(BigInt n, BigInt d);
    void   add(Fraction f);
    void   sub(Fraction f);
    BigInt gcd(BigInt n0, BigInt n1);
    void   normal(void);
    Bool   isEq(Fraction f);
};

//**************************************************************
// Model
//==============================================================
//--------------------------------------------------------------
class Particle {
  public:
    Model *mdl;             // the model
    char  *name;            // the name of the particle
    char  *aname;           // the name of the anti-particle
    int    id;              // id of this particle
    int    ptype;           // the type of partice : GRCC_PT_Scalar, etc.
    int    neutral;         // True if particle==anti-particle
    int    pcode;           // Particle code
    int    acode;           // Anti-particle code
    int    cmindeg;         // min(leg of connectable interactions)
    int    cmaxdeg;         // max(leg of connectable interactions)

    int    extonly;         // can appear only as external particle

    //-----------------------------
    Particle(Model *modl, int pid, PInput *pinp);
    ~Particle(void);

    char *particleName(int p);
    int   particleCode(int p);
    char *interactionName(int p);
    char *aparticle(void);
    void  prParticle(void);
    int   isNeutral(void);
    const char *typeName(void);
    const char *typeGName(void);

};

//--------------------------------------------------------------
class Interaction {
  public:
    Model *mdl;           // the model
    char  *name;          // the name of the interaction
    int   *plist;        // the list of particle codes
    int   *clist;        // the oders of coupling constants
    int   *slist;        // the sorted list of particle codes

    int    id;            // id of this interaction
    int    csum;          // the total oders of coupling constants
    int    nlegs;         // the number of legs
    int    loop;          // the number of loops
    int    icode;         // user defined interaction code 

    int    nplist;        // the list of particle codes
    int    nclist;        // the oders of coupling constants
    int    nslist;        // the sorted list of particle codes

    //-----------------------------
    Interaction(Model *modl, int iid, const char* nam, int icode, int *cpl, int nlgs, int *plst, int csm, int lp);
    ~Interaction(void);

    void prInteraction(void);
};

//--------------------------------------------------------------
class Model {
  public:
    char         *name;        // the name of this model
    char        **cnlist;       // the list of coupling constant names
    int           ncouple;     // the number of coupling consants.

    int           nParticles;  // the number of particles
    Particle    **particles;   // the list of particles
    int           pdef;        // the def. of partcls is ended

    // list of particles and anti-p. without Undef.
    int           nallPart;     
    int           allPart[GRCC_MAXMPARTICLES2];

    // list of particles and anti-p. without ones of extloop=True
    int           nintPart;     
    int           intPart[GRCC_MAXMPARTICLES2];

    int           nInteracts;  // the number of interactions.
    Interaction **interacts;   // the list of interactions
    int           vdef;        // the definition of interaction is ended

    int           maxnlegs;    // maximum number of legs of an interaction
    int           maxcpl;      // maximum number of coupling const.
    int           maxloop;     // maximum number of loops inside an intr.

    // classification of interactions
    int          *cplgcp;      // coupling constants
    int          *cplglg;      // degree
    int          *cplgnvl;     // number of vertices
    int         **cplgvl;      // list of vertices
    int           ncplgcp;     // the number of classes

    int           defpart;     // GRCC_DEFBYNAME or GRCC_DEFBYCODE
    int           skipFLine;   // skip analysis fermion line

    int          DUMMYPADDING;

    // methods
    Model(MInput *minp);
    ~Model(void);

    void  prModel(void);
    void  addParticle(PInput *pinp);
    void  addParticleEnd(void);
    void  addInteraction(IInput *iinp);
    void  addInteractionEnd(void);
    int   findParticleName(const char *name);
    int   findParticleCode(int pcd);
    int   findInteractionName(const char *name);
    int   findInteractionCode(int icd);
    char *particleName(int p);
    int   particleCode(int p);
    int   normalParticle(int pt);
    int   antiParticle(int pt);
    int  *allParticles(int *len);
    int   findMClass(const int cpl, const int dgr);
    void  prParticleArray(int n, int *a, const char *msg);
};

//**************************************************************
// Process
//===============================================================
class PNodeClass {
  public:
    SProcess *sproc;
    int      *deg;       // The degree of each node
    int      *type;      // The type of the class : GRCC_AT_xxx
    int      *particle;  // The particle code of external node.
    int      *couple;    // The orders of coupling constans
    int      *cmindeg;   // min(deg of connectable vertex)
    int      *cmaxdeg;   // max(deg of connectable vertex)
    int      *count;     // The number of nodes in the class
    int      *cl2nd;     // cl2nd[class] <= nd < cl2nd[class+1] = set of nodes
    int      *nd2cl;     // nd2cl[node]  = class
    int      *cl2mcl;    // cl2mcl[class] = class defined in the model.
    int       nnodes;
    int       nclass;

    PNodeClass(SProcess *sprc, int nnods, int nclss, NCInput *cls);
    PNodeClass(SProcess *sprc, int nnods, int nclss, int *dgs, int *typ, int *ptcl, int *cpl, int *cnt, int *cmind, int *cmaxd);
    ~PNodeClass(void);

    void prPNodeClass(void);
    void prElem(int e);
};

//===============================================================
class SProcess {
  //  In sprocess the number of nodes and edges are fixed.
  //  A node is considered as 
  //    (degree, total order of coupling constants),
  //  which pair of deta defines a class of nodes.
 
  public:
    Model       *model;       // model
    Process     *proc;        // mother process 
    Options     *opt;         // options
    PNodeClass  *pnclass;     // list of classes (cpl and deg is determined)
    AStack      *astack;

    MGraph      *mgraph;
    EGraph      *egraph;
    Assign      *agraph;

    int         *cl2nd;       // (code of nodes in the class[c])
                              //  = [cl2nd[c], ..., cl2nd[c+1]-1]
    int         *nd2cl;       // node nd is in the class pnclass[nd2cl[nd]]
    int          nclass;      // the number of classes

    int          ninitl;      // the number of initial particles
    int          nfinal;      // the number of final particles
    int          nvert;       // the number of vertices

    int          clist[GRCC_MAXNCPLG];  // coupling constans of the process
    int          id;          // sprocess id
    int          loop;        // the number of loops
    int          nNodes;      // the number of nodes
    int          nEdges;      // the number of edges
    int          nExtern;     // the number of external particles
    int          ncouple;     // the number of coupling constants
    int          tCouple;     // the total coupling constants

    int          DUMMYPADDING;

    BigInt       mgrcount;    // count generated mgraph
    BigInt       agrcount;    // count generated agraph
    BigInt       extperm;    // count generated agraph

    // the results of the graph generation
    BigInt   nMGraphs;        // the number of generated M-graphs
    BigInt   nMOPI;           // the number of 1PI M-graphs
    Fraction wMGraphs;        // the weighted sum of M-graphs
    Fraction wMOPI;           // the weighted sum of 1PI M-graphs

    BigInt   nAGraphs;        // the number of generated A-graphs
    BigInt   nAOPI;           // the number of 1PI A-graphs
    Fraction wAGraphs;        // the weighted sum of A-graphs
    Fraction wAOPI;           // the weighted sum of 1PI A-graphs

    // methods
    SProcess(Model *mdl, Process *prc, Options *opts, int sid, int *clst, int ncls, NCInput *cls);
    SProcess(Model *mdl, Process *prc, Options *opts, int sid, int *clst, int ncls, int *cdeg, int *ctyp, int *ptcl, int *cpl, int *cnum, int *cmind, int *cmaxd);

    ~SProcess(void);

    void prSProcess(void);
    BigInt generate(void);
    void assign(MGraph *mgr);

    int toMNodeClass(int *ctyp, int *cldeg, int *clnum, int *cmind, int *cmaxd);
    PNodeClass *match(MGraph *mgr);

    void endMGraph(MGraph *mgr);
    void endAGraph(EGraph *egr);

    void resultMGraph(BigInt nmgraphs, Fraction mwsum, BigInt nmopi, Fraction mwopi);
    void resultAGraph(BigInt nagraphs, Fraction awsum, BigInt naopi, Fraction awopi);

};

//===============================================================
class Process {
  public:
    Model       *model;            // model used for this process
    Options     *opt;              // options
    BigInt       mgrcount;         // count generated mgraph
    BigInt       agrcount;         // count generated agraph
    int         *initlPart;        // list of ids of initial particles
    int         *finalPart;        // list of ids of final particles

    int          id;               // process id
    int          ninitl;           // the number of initial particles
    int          nfinal;           // the number of final particles
    int          ctotal;           // the total number of coupling constants
    int          nExtern;          // the number of external particles
    int          loop;             // the number of external particles
    int          maxnlegs;         // the maximum possible degree of node
    int          clist[GRCC_MAXNCPLG];  // coupling constans of the process


    // table of sprocesses 
    int        nSubproc;           // the number of sprocesses
    SProcess  *sptbl[GRCC_MAXSUBPROCS]; // the table of sprocesses
    SProcess  *sproc;              // the current sprocess

    // stack for the assignment;
    AStack      *astack;

    // the number of graphs
    BigInt       ngraphs;
    BigInt       nopi;
    BigInt       wgraphs;
    BigInt       wopi;

    // the results of the graph generation
    BigInt       nMGraphs;             // the number of generated M-graphs
    BigInt       nMOPI;                // the number of 1PI M-graphs
    Fraction     wMGraphs;             // the weighted sum of M-graphs
    Fraction     wMOPI;                // the weighted sum of 1PI M-graphs

    BigInt       nAGraphs;             // the number of generated A-graphs
    BigInt       nAOPI;                // the number of 1PI A-graphs
    Fraction     wAGraphs;             // the weighted sum of A-graphs
    Fraction     wAOPI;                // the weighted sum of 1PI A-graphs

    double       sec;

    Process(int pid, Model *model, Options *opt, int nin, int *initlPart, int nfin, int *finalPart, int *coupling);
    Process(int pid, Model *model, Options *opt, FGInput *fgi);
    ~Process(void);

    void prProcess(void);
    void mkSProcess(void);

};

//**************************************************************
// classes for EGraph
//==============================================================
// Constants

#define GRCC_ED_Undef   0
#define GRCC_ED_Deleted 1
#define GRCC_ED_Extern  2
#define GRCC_ED_Back    3
#define GRCC_ED_Bridge  4
#define GRCC_ED_Inloop  5
#define GRCC_ED_Size    6
#define GRCC_ED_NAMES   {"Undef", "Deleted", "Extern", "Back_Edge", "Bridge", "In_Loop"}

#define GRCC_ND_Undef   0
#define GRCC_ND_Deleted 1
#define GRCC_ND_Initial 2
#define GRCC_ND_Final   3
#define GRCC_ND_CPoint  4
#define GRCC_ND_VBlock  5
#define GRCC_ND_Inblock 6
#define GRCC_ND_Size    7
#define GRCC_ND_NAMES   {"Undef", "Deleted", "Init", "Final", "CPoint", "VBlock", "In_Block"}

//--------------------------------------------------------------
class ENode {
  public:
    EGraph *egraph;
    int    *edges;           // list of edges
                             // the value is \pm [(edge index)+1]

    int     id;              // id of the enode
    int     maxdeg;          // maximum degree
    int     deg;             // degree
    int     extloop;         // loop inside this node
    int     ndtype;          // type of the node
    int     intrct;          // assigned interaction/particle (ext.)

    // used in EGraph::biconn()
    int    *klow; 
    int     visited;

    int    DUMMYPADDING;

    //--------------------------------
    // functions
    ENode(void);
    ENode(EGraph *egrph, int loops, int sdeg);
    ~ENode(void);
    void initAss(EGraph *egrph, int nid, int sdg);
    void setId(EGraph *egrph, const int nid);
    void copy(ENode *en);
    void setExtern(int typ, int pt);
    void setType(int typ);
    void print(void);

};

//--------------------------------------------------------------
class EEdge {
  // momentum is printed like: ("%s%d", (enode.ext)?"Q":"p", enode.momn)
  public:
    EGraph *egraph;        // egraph

    int  id;               // id
    int  ext;
    int  ptcl;             // assigned particle (agraph)
    int  deleted;          // deleted edge, if true
    int  nodes[2];         // nodes of bothsides
    int  nlegs[2];         // nodes of both side (agraph)

    // for biconn
    int *emom;             // external momenta
    int *lmom;             // loop momenta
    int *extMom;           // set of external momenta.

    Bool cut;
    int  visited;
    int  conid;            // connected component
    int  edtype;           // type
    int  opicomp;          // id of 1PI component
    int  dir;              // direction of momentum


    //--------------------------------
    // functions
    EEdge(void);
    EEdge(EGraph *egrph, int nedges, int nloops);
    ~EEdge(void);
    void copy(EEdge *ee);
    void setId(EGraph *egrph, const int eid);
    void print(void);

    void setType(int typ);
    void setLMom(int k, int dir);
    void setEMom(int nedges, int *extn, int dir);
};

//--------------------------------------------------------------
// type of fermion lines

typedef enum {FL_Open, FL_Closed} FLType;

//--------------------------------------------------------------
class EFLine {
  public:
    int    elist[GRCC_MAXNODES];     // list of  (\pm [(edge index)+1])
    FLType ftype;
    int    fkind;
    int    nlist;

    EFLine(void);
    void print(const char *msg);
};

//--------------------------------------------------------------
class EGraph {
  public:
    Options    *opt;          // table of options
    Model      *model;
    Process    *proc;
    SProcess   *sproc;
    MGraph     *mgraph;
    MConn      *econn;

    ENode **nodes;
    EEdge **edges;          // edges[nEdges+1]: index starts from 1

    BigInt mId;             // id of mgraph
    BigInt aId;             // id of agraph in the same mgraph
    BigInt sId;             // sequenctial no. of agraph
    BigInt gSubId;          // ???
    Bool   assigned;        // mgraph (False) or agraph (True)

    int    fsign;
    BigInt nsym, esym;      // symmetry factor with symm. ext.
    BigInt nsym1;           // symmetry factor without symm. ext.
    BigInt extperm;         // the order of group of symm. ext.
    BigInt multp;           // multiplicity of graph in symm. ext.

    int    pId;

    int   sNodes;            // memory size
    int   sEdges;            // memory size
    int   sMaxdeg;           // memory size
    int   sLoops;            // memory size

    int    nNodes;
    int    nEdges;
    int    nExtern;
    int    maxdeg;            // maximum value of degree of nodes
    int    nLoops;
    int    totalc;            // total order of coupling constants

    // biconnect
    int    nopicomp;
    int    opi2plp;
    int    nopi2p;
    int    nadj2ptv;           // the no. of edges connecting 2point vertices

    int    DUMMYPADDING;

    int   *bidef;
    int   *bilow;
    int   *extMom;
    int    bconn;
    int    bicount;
    int    loopm;
    int    opiCount;

    // Fermion lines
    EFLine *flines[GRCC_MAXFLINES];
    int     nflines;

    int    DUMMYPADDING1;

    //--------------------------
    // functions
    EGraph(int nnodes, int nedges, int mxdeg);
    ~EGraph(void);

    void copy(EGraph *eg);
    void print(void);
    void fromDGraph(DGraph *dg);
    void fromMGraph(MGraph *mgraph);
    Bool isOptE(void);

    ENode *setExtern(int n0, int pt, int ndtp);
    Bool   isExternal(int nd)  { return (nodes[nd]->extloop < 0); };
    Bool   isFermion(int nd);

    void setExtLoop(int nd, int val);
    void endSetExtLoop(void);

    int connComp(void);
    int connVisit(int nd, int ncc);

    void biconnE(void);
    void biinitE(void);
    void bisearchE(int nd, int *extlst, int *intlst, int *opiext, int *opiloop);

    int  findRoot(void);
    int  dirEdge(int n, int e);
    void extMomConsv(void);
    int  cmpMom(int *lm0, int *em0, int *lm1, int *em1);
    int  groupLMom(int *grp, int *ed2gr);

    void chkMomConsv(void);

    void prFLines(void);
    void getFLines(void);
    int  fltrace(int fk, int nd0, int *fl);
    void addFLine(const FLType ft, int fk, int nfl, int *fl);

    int  legParticle(int ed, int lg);

};

//**************************************************************
//  Symmetry group of graphs
//===============================================================
class SGroup {
  public:
    BigInt  size;              // # of allocated elements
    BigInt  nelem;             // # of saved elements
    int   **elem;              // saved elements
    int     nnodes;            // # of nodes.
    int     neclass;           // # of classes
    int     eclass[GRCC_MAXNODES];  // table of classes
    int     cgen;              // counter for the generation
    int     csav;              // counter for the saved elements
    int     permg[GRCC_MAXNODES];   // resulting permutation
    int     perms[GRCC_MAXNODES];   // curr. elem of saved ones.

    int     pgr[GRCC_MAXNODES];  // work
    int     pgq[GRCC_MAXNODES];  // work
    int     psr[GRCC_MAXNODES];  // work
    int     psq[GRCC_MAXNODES];  // work

    //-------------------------------
    // functions

    SGroup(void);
    ~SGroup(void);

    void  print(void);
    void  newGroup(int nelm, int nclss, int *clss);
    void  clearGroup(void);
    void  delGroup(void);
    int  *genNext(void);
    void  addGroup(int *p);
    BigInt  nElem(void);
    int  *nextElem(void);
};

//**************************************************************
// MGraph
//==============================================================
// class of nodes for MGraph

class MNode {
  public:
    int id;        // node id
    int deg;       // degree(node) = the number of legs
    int clss;      // initial class number in which the node belongs
    int extloop;
    int cmindeg;
    int cmaxdeg;

    int freelg;    // the number of free legs
    int visited;

    MNode(int id, int clss, NCInput *mgi);
    MNode(int id, int deg, int extloop, int clss, int cmind, int cmaxd);
};

//===============================================================
//  class of scalar graph expressed by matrix form
// 
//  Input  : the classified set of nodes.
//  Output : control passed to 'EGraph(self)'

class MGraph {

  public:

    // initial conditions
    Options  *opt;         // options

    // symmbery group
    SGroup   *group;       // symmetry group
    MOrbits  *orbits;      // Orbits of nodes with respect to symmetry group

    MNode **nodes;         // table of MNode object
    BigInt mId;            // process/sprocess ID
    int   *clist;          // list of initial classes
    int    pId;            // process/sprocess ID
    int    nNodes;         // the number of nodes
    int    nEdges;         // the number of edges
    int    nLoops;         // the number of loops
    int    nExtern;        // the number of external nodes
    int    nClasses;       // the number of initial classes
    int    mindeg;         // minimum value of degree of nodes
    int    maxdeg;         // maximum value of degree of nodes

    // the current graph
    int **adjMat;          // adjacency matrix
    MNodeClass *curcl;     // the current 'MNodeClass' object
    EGraph *egraph;
    BigInt nsym;           // symmetry factor from nodes
    BigInt esym;           // symmetry factor from edges

    // generated set of graphs
    BigInt cDiag;          // the total number of generated graphs
    BigInt c1PI;           // the total number of 1PI graphs
    BigInt cNoTadpole;     // the total number of graphs without tadpoles
    BigInt cNoTadBlock;    // the total number of graphs without tad-blocks
    BigInt c1PINoTadBlock; // the total number of 1PI graphs without tad-blocks
    Fraction wscon;         // weighted sum of graphs
    Fraction wsopi;         // weighted sum of 1PI graphs

    // measures of efficiency
    BigInt ngen;              // generated graph before check
    BigInt ngconn;            // generated connected graph before check
  
    BigInt nCallRefine;        
    BigInt discardRefine;        
    BigInt discardDisc;        
    BigInt discardIso;        

    // for options
    Bool opi;
    Bool opiloop;
    Bool extself;
    Bool selfloop;
    Bool tadpole;
    Bool tadblock;
    Bool block;

    int    DUMMYPADDING1;

    // table of n edge-connected components
    MConn *mconn;

    // work space for isomorphism
    int **modmat;           // permutated adjacency matrix

    // work space for biconnected component
    int *bidef;
    int *bilow;
    int  bicount;

    int    DUMMYPADDING2;

    //----------
    // functions 
    MGraph(int pid, int ncl, NCInput *mgi, Options *opt);
    MGraph(int pid, int ncl, int *cldeg, int *clnum, int *clexl, int *cmind, int *cmaxd, Options *opt);
    ~MGraph(void);

    void   init(void);
    BigInt   generate(void);
    Bool   isExternal(int nd)  { return (nodes[nd]->extloop < 0); };
    void   printAdjMat(MNodeClass *cl);
    void   print(void);

    Bool   isConnected(void);
    Bool   visit(int nd);
    Bool   isIsomorphic(MNodeClass *cl);
    void   permMat(int size, int *perm, int **mat0, int **mat1);
    int    compMat(int size, int **mat0, int **mat1);
    MNodeClass *refineClass(MNodeClass *cl);
    void   bisearchME(int nd, int pd, int ned, MCOpi *mopi, MCBlock *mblk, int *next, int *nart);
    void   biconnME(void);
    Bool   isOptM(void);

    void   connectClass(MNodeClass *cl);
    void   connectNode(int sc, int ss, MNodeClass *cl);
    void   connectLeg(int sc, int sn, int tc, int ts, MNodeClass *cl);
    void   newGraph(MNodeClass *cl);

};

//===============================================================
//  class of node-classes for MGraph
//--------------------------------------------------------------
class MNodeClass {
  public:
    int   clmat[GRCC_MAXNODES][GRCC_MAXNODES];   // matrix used for classification
    int   clist[GRCC_MAXNODES];    // the number of nodes in each class
    int   ndcl[GRCC_MAXNODES];     // node --> class
    int   flist[GRCC_MAXNODES+1];  // the first node in each class
    int   clord[GRCC_MAXNODES];    // ordering of classes
    int   cmindeg[GRCC_MAXNODES];    // min(deg of connectable node)
    int   cmaxdeg[GRCC_MAXNODES];    // max(deg of connectable node)
    int   nNodes;         // the number of nodes
    int   nClasses;       // the number of classes
    int   maxdeg;         // maximal value of degree(node)

    int   flg0;
    int   flg1;
    int   flg2;

    MNodeClass(int nnodes, int nclasses);
    ~MNodeClass(void);

    void  init(int *cl, int mxdeg, int **adjmat);
    void  copy(MNodeClass* mnc);
    int   clCmp(int nd0, int nd1, int cn);
    void  printMat(void);

    void  mkFlist(void);
    void  mkNdCl(void);
    void  mkClMat(int **adjmat);
    void  incMat(int nd, int td, int val);
    int   cmpMNCArray(int *a0, int *a1, int ma);
    void  reorder(MGraph *mg);
};


//===============================================================
//  class of 1PI component
//--------------------------------------------------------------
class MCOpi {
  public:
    int *nodes;    // array of nodes in 
    int  nnodes;   // # nodes in the 1PI component
    int  nlegs;    // # leg of the 1PI component
    int  next;     // # external particles of the 1PI comp.
    int  nedges;   // # edges in the 1PI comp.
    int  loop;     // # loops in the 1PI comp.
    int  ctloop;   // # loops in the couter terms in the OP comp.

    MCOpi(void);
    ~MCOpi(void);

    void init(void);
};

//===============================================================
//  class of bridge
//--------------------------------------------------------------
class MCBridge {
  public:
    Edge2n nodes;  // nodes at the both size of the bridge
    int    next;   // # momenta of ext. particles flowing on the bridge

    MCBridge(void);
    ~MCBridge(void);
};

//===============================================================
//  class of block
//--------------------------------------------------------------
class MCBlock {
  public:
    Edge2n *edges;   // array of edges in the block
    int     nmedges; // # edges in the block
    int     nartps;  // # articulation points of the block
    int     loop;    // # loop in the block

    int    DUMMYPADDING;

    MCBlock(void);
    ~MCBlock(void);
    void init(void);
};

//===============================================================
//  class of table of MConn
//--------------------------------------------------------------
class MConn {
  public:
    // 2-edge connected components
    MCOpi     *opics;       // table of n-edge-connected components
    MCBridge  *bridges;     // table of bridges
    MCBlock   *blocks;      // table of blocks
    int       *articuls;    // buffer for nodes for articulation points

    int       *opisp;       // buffer for nodes in 1PI components
    int       *opistk;      // stack of nodes for 1PI compoinents.
    Edge2n    *blksp;       // buffer for edges in blocks
    Edge2n    *blkstk;      // stack of edges for blocks
    int        snodes;      // # nodes
    int        sedges;      // # edges

    // opi components (edge-connected)
    int        nopic;       // # 1PI components (n1PIComps)
    int        nlpopic;     // # looped 1PI components (n1PIComps)
    int        nctopic;     // # 1PI components of one couter term.
    int        nbridges;    // # bridges
    int        ne0bridges;  // # bridges whose next=0
    int        ne1bridges;  // # bridges whose next=1
    int        nselfloops;  

    // blocks (node-connected)
    int        nblocks;     // # blocks
    int        na1blocks;   // # bridges whose next=0
    int        narticuls;   // # articulation points
    int        neblocks;    // # effective looped blocks

    // indices to work spaces
    int        nopisp;      // # used in opisp
    int        opistkptr;   // # stack pointer of opistk
    int        nblksp;      // # used in blksp
    int        blkstkptr;   // # stack pointer of tlkstk

    int    DUMMYPADDING;

    MConn(int nnod, int nedg);
    ~MConn(void);

    void init(void);
    void pushNode(int nd);
    void pushEdge(int n0, int n1);
    void addOPIc(MCOpi *mopi, int stp);
    void addBridge(int n0, int n1, int nex, int nextot);
    void addArtic(int nd, int mul);
    void addBlock(MCBlock *eblk, int stp);
    void addBlockSelf(int nd, int mul);
    void print(void);
};

//===============================================================
//  class of orbits of nodes
//--------------------------------------------------------------
class MOrbits {
  // Usage
  // 1. node ==> orbit
  //    (class) = nd2or[(node)]
  //
  // 2. class ==> nodes
  //    for (c = 0; c < nClass; c++) {
  //        for (j = flist[c]; j < flist[c+1]; j++) {
  //            (node) = or2nd[c];
  //        }
  //    }
  public:
    int nOrbits;
    int nNodes;
    int nd2or[GRCC_MAXNODES];
    int or2nd[GRCC_MAXNODES];
    int flist[GRCC_MAXNODES+1];

    MOrbits(void);
    ~MOrbits(void);

    void print(void);

    // construction of orbits
    void initPerm(int nnodes);
    void fromPerm(int *perm);
    void toOrbits(void);
};

//**************************************************************
// Particle assignment
//===============================================================
typedef int CheckPt[2];

typedef enum {
    AS_UnAssLegs, AS_Assigned, AS_Assigned0, AS_AssExt, AS_Impossible
} NCandSt;

//===============================================================
class NCand {
  // List of candidats for the assignment of interactions to a vertex

  public:
    int      deg;                     // degree of the node
    NCandSt  st;                      // status
    int      nilist;                  // length of the list
    int      ilist[GRCC_MAXMINTERACT];     // list of candidates

    //========
    NCand(const NCandSt sta, const int dega, const int nilst, int *ilst);
    ~NCand(void);

    // print
    void prNCand(const char* msg);
};

//===============================================================
class ECand {
  // List of candidats for the assignment of particles to an edge

  public:
    Bool  det;                     // determined or not
    int   nplist;                  // size of the list of candidates
    int   plist[GRCC_MAXMPARTICLES2];   // list of candidates

    ECand(int dt, int nplist, int *plst);
    ~ECand(void);

    // print
    void   prECand(const char *msg);
};

//===============================================================
class ANode {
  // Connection information of a node to others
  //
  //  (this node) -- (adjacent edge) -- (next node)
  //    (n0, j)            e                n1
  //
  // e  = (aedges[j], aelegs[j])
  // n1 = anodes[j]

  public:
    int    deg;              // degree of the node
    int    nlegs;            // the number of legs already assigned
    int   *anodes;           // anodes[j] = (next node of leg j)
    int   *aedges;           // aedges[j] = (next edge of leg j)
    int   *aelegs;           // aelegs[j] = (leg of the next edge)
    NCand *cand;             // candidate list

    ANode(int dg);
    ~ANode(void);

    int  newleg(void);        // get a new leg
};

//===============================================================
class AEdge {
// Connection information of an edge
// (self) ==> nodes [(nodes[0], nlegs[0]), (nodes[1], nlegs[1])]
// particle 'ptcl' flows from leg=0 to 1.

  public:

    ECand *cand;            // candidate
    int    nodes[2];        // nodes of both sides of the edge
    int    nlegs[2];        // nodes of both sides of the edge
    int    ptcl;            // particle defined in the model

    int    DUMMYPADDING;

    AEdge(int n0, int l0, int n1, int l1);
    ~AEdge(void);
};

//===============================================================
class Assign {
// Class for particle/interaction assignment.

  public:

    EGraph     *egraph;       // EGraph object
    MGraph     *mgraph;       // MGraph object
    SProcess   *sproc;        // Sub-process object
    Process    *proc;         // Process object
    Model      *model;        // Model object
    Options    *opt;          // Option object
    AStack     *astack;       // Stack for saving candidates
    PNodeClass *pnclass;      // class of nodes
    MOrbits    *orbits;       // orbits of nodes by symmetry group

    int         nNodes;       // the number of nodes
    int         nEdges;       // the number of edges
    int         nExtern;      // the number of external particles.
    int         nETotal;      // the total number of edges

    BigInt      nAGraphs;     // the number of assigned graphs
    Fraction    wAGraphs;     // the weighted sum of graphs
    BigInt      nAOPI;        // the number of assigned 1PI
    Fraction    wAOPI;        // the weighted sum of 1PI

    ANode     **nodes;        // table of nodes
    AEdge     **edges;        // table of edges

    CheckPt     checkpoint0;  // save stack pointers

    int         cplleft[GRCC_MAXNCPLG];  // coupling constants left

    //===========================================
    Assign(SProcess *sprc, MGraph *mgr, PNodeClass *pnc);
    ~Assign(void);

    //===========================================
    // print lists of candidates
    void    prCand(const char *msg);

    // check
    void checkAG(const char *msg);

    //===========================================
    // control of assignment

    // entry point of assignment
    Bool    assignAllVertices(void);

    // select a source node for assignment
    Bool    selectVertex(void);
    Bool    selectVertexSimp(int lastv);

    // select a source leg of the node for assignment
    Bool    selectLeg(int v, int lastlg);

    // assign a vertex a interaction
    Bool    assignVertex(int v);

    // assignment procedure is finished.  Needs check.
    Bool    allAssigned(void);

    //===========================================
    // Input and output

    // Input from mgraph
    Bool    fromMGraph(void);

    // add an edge
    void    addEdge(int n0, int n1, int nplist, int *plist);

    // connect nodes and edges.
    void    connect(int n0, int l0, int eg, int el, int n1, int l1);

    // Output to egraph
    Bool    fillEGraph(int aid, BigInt nsym, BigInt esym, BigInt nsym1);

    // reorder legs in accordance with the interaction definition
    int    *reordLeg(int n, int *reord, int *plist, int *used);

    //===========================================
    // direction of particle on an edge and at (node, leg)

    // convert particle code
    //   at node-leg ('n', 'ln') <==> edge at ('n', 'ln')
    int     getLegParticle(int n,  int ln);

    // convert particle 'pt' on the edge to ('n', 'ln')
    int     legEdgeParticle(int n, int ln, int pt);

    // convert candidate list at ('v', 'lg') into in-coming direction
    int     legPart(int v, int lg, int nplst, int *plst, int *rlist, const int size);

    // convert candidate list at ('v', 'lg') into in-coming direction
    int     candPart(int v, int ln, int *plist, const int size);

    //===========================================
    // assignment

    // find a vertex to be assigned
    int     selUnAssVertex(void);
    int     selUnAssVertexSimp(int lastv);

    // find a leg of the vertex to be assigned
    int     selUnAssLeg(int v, int lastlg);

    // assign the interaction 'ia' to the vertex 'v'.
    NCandSt assignIVertex(int v, int ia);

    // assign particle 'pt' the the leg 'ln' of the node 'n'.
    Bool    assignPLeg(int n, int ln, int pt);
    Bool    isOrdPLeg(int n, int ln, int pt);
    Bool    detEdge(int e);

    //===========================================
    // canndidates

    // construct lists of assigned / unassigned particles at a node
    Bool    candPartClassify(int v, int *npdass, int *pdass, int *npuass, int *puass, const int size);

    // update candidate list for a vertex 'v'.
    Bool    updateCandNode(int v);

    //===========================================
    // filter 

    Bool    checkOrderCpl(void);
    Bool    isOrdLegs(void);
    Bool    isIsomorphic(MNodeClass *cl, BigInt *nsym, BigInt *esym, BigInt *nsym1);
    int     cmpPermGraph(int *p, MNodeClass *cl);
    int     cmpNodes(int nd0, int nd1, MNodeClass *cn);
    BigInt  edgeSym(void);

    void    saveCouple(int *sav);
    void    restoreCouple(int *sav);
    Bool    subCouple(int *cpl);

#ifdef CHECK
    //===========================================
    // check
    Bool    checkCand(const char *msg);

    void    checkNode(int n, const char *msg);
#endif
};

//**************************************************************
// Stack of candidate lists
//===============================================================
class NStack {
// Stack for backtracking method for a node.

  public:
    int     noden;
    int     deg;
    NCandSt st;
    int     nilist;
    int     ilist[GRCC_MAXMINTERACT];

    NStack() { };
    ~NStack() { };

    void print(const char *msg);

};

//===============================================================
class EStack {
// Stack for backtracking method for an edge.

  public:
    int    edgen;
    int    det;
    int    nplist;
    int    plist[GRCC_MAXMPARTICLES2];

    EStack() { };
    ~EStack() { };

    void print(const char *msg);
};


//===============================================================
class AStack {
// Stack for backtracking method for an edge.

  public:
    Assign     *agraph;

    NStack    **nStack;       // stack for nodes
    int         nStackP;      // stack pointer for nodes
    int         nSize;        // stack size

    EStack    **eStack;       // stack for edges
    int         eStackP;      // stack pointer for edges
    int         eSize;        // stack size

    AStack(int nSize, int eSize);
    ~AStack(void);

    void setAGraph(Assign *ag);

    void checkPoint(CheckPt sav);
    void restore(CheckPt sav);
    void restoreMsg(CheckPt sav, const char *msg);
    void prStack(void);

    void pushNode(int n);
    void pushEdge(int e);

  private:
    void restoreNode(int spr);
    void restoreEdge(int spr);
};

//==============================================================
// end of namespace Grcc
#ifdef GRCC_NAMESPACE
}
#endif
