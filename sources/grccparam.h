#ifdef GRCC_PARAM_H
#else
#define GRCC_PARAM_H

/*==============================================================
 * Parameters
 */
/*
#define GRCC_NAMESPACE
*/

#define GRCC_MAXNCPLG         4
#define GRCC_MAXLEGS         10
#define GRCC_MAXMPARTICLES   50
#define GRCC_MAXMINTERACT   200
#define GRCC_MAXSUBPROCS    500
#define GRCC_MAXNODES        20
#define GRCC_MAXEDGES       100
#define GRCC_MAXNSTACK       50
#define GRCC_MAXESTACK       50
#define GRCC_MAXGROUP      1000
#define GRCC_MAXPSLIST      500

#define GRCC_MAXMPARTICLES2  (2*GRCC_MAXMPARTICLES-1)
#define GRCC_MAXFLINES       GRCC_MAXEDGES

#define GRCC_FRACERROR       (1.0e-10)

/* error message */
/*
#define GRCC_Stderr          stderr
*/
#define GRCC_Stderr          stdout
/*
#define GRCC_ABORT()         exit(1)
*/
#define GRCC_ABORT()         abort()

/*---------------------------------------------------------------
 * Constants
 */

/* model definition */
#define GRCC_DEFBYNAME   0    /* define interactions by particle name */
#define GRCC_DEFBYCODE   1    /* define interactions by particle code */

/* types of particles */
#define GRCC_PT_Undef      0
#define GRCC_PT_Scalar     1
#define GRCC_PT_Dirac      2
#define GRCC_PT_Majorana   3
#define GRCC_PT_Vector     4
#define GRCC_PT_Ghost      5
#define GRCC_PT_Size       6

#define GRCC_PTS_Undef      "undef"
#define GRCC_PTS_Scalar     "scalar"
#define GRCC_PTS_Dirac      "dirac"
#define GRCC_PTS_Majorana   "majorana"
#define GRCC_PTS_Vector     "vector"
#define GRCC_PTS_Ghost      "ghost"

#define GRCC_PT_Table  { GRCC_PTS_Undef, GRCC_PTS_Scalar, GRCC_PTS_Dirac, GRCC_PTS_Majorana, GRCC_PTS_Vector, GRCC_PTS_Ghost}

#define GRCC_PT_GTable { "Undef", "Scalar", "Fermion", "Majorana", "Vector", "Ghost"}

/* for mgraph generation */
#define GRCC_AT_Vertex     0
#define GRCC_AT_External  -1
#define GRCC_AT_Initial   -2
#define GRCC_AT_Final     -3

#define GRCC_AT_NdStr(x)   ((x)>=GRCC_AT_Vertex   ?"Vertex":       \
                           ((x)==GRCC_AT_External ?"External":     \
                           ((x)==GRCC_AT_Final    ?"Fianl":        \
                           ((x)==GRCC_AT_Initial  ?"Initial":"Undef"))))

/* graph generation */
#define GRCC_MGraph   0       /* mgraph (topology) generation */
#define GRCC_AGraph   1       /* agraph generation */

/* options for graph greneration */
#define GRCC_OPT_Step            0
#define GRCC_OPT_Outgrf          1
#define GRCC_OPT_1PI             2
#define GRCC_OPT_NoTadpole       3
#define GRCC_OPT_No1PtBlock      4
#define GRCC_OPT_No2PtL1PI       5
#define GRCC_OPT_Block           6
#define GRCC_OPT_SymmInitial     7
#define GRCC_OPT_SymmFinal       8
#define GRCC_OPT_Size            9

/*---------------------------------------------------------------
 * data types for model definition
 */
typedef struct {
    const char *name;               /* name of the model */
          int   defpart;            /* particle is defined by name or code */
                                    /* GRCC_DEFBYNAME or GRCC_DEFBYCODE */
          int   ncouple;            /* No. of coupling constants */
    const char *cnamlist[GRCC_MAXNCPLG]; /* Names of coupling constants */
} MInput;

typedef struct {
    const char *name;               /* name of particle */
    const char *aname;              /* name of anti-particle */
          int   pcode;              /* code of particle */
          int   acode;              /* code of anti-particle */
    const char *ptypen;             /* type : 'GRCC_PTS_Scalar' etc */
          long  ptypec;             /* type : 'GRCC_PT_Scalar' etc */
} PInput;

typedef struct {
    const char *name;                 /* name of interaction */
          int   icode;                /* code of interaction */
          int   nplistn;              /* the number of legs */
    const char *plistn[GRCC_MAXLEGS];    /* list of particle names */
          int   plistc[GRCC_MAXLEGS];    /* list of particle codes */
          int   cvallist[GRCC_MAXNCPLG]; /* coupling constants */
} IInput;

/*---------------------------------------------------------------
 * data types for process definition
 */
typedef struct {
    long          ninitl;              /* the number of initial particles */
    const char   *initln[GRCC_MAXNODES];    /* list of initial particles (name) */
    int           initlc[GRCC_MAXNODES];    /* list of final particles (code) */
    long          nfinal;              /* the number of final particles */
    const char   *finaln[GRCC_MAXNODES];    /* list of final particles (name) */
    int           finalc[GRCC_MAXNODES];    /* list of final particles (code) */
    int           coupl[GRCC_MAXNCPLG];     /* list of orders of c. consts */
} FGInput;

/* class of node : input for SProcess */
typedef struct {
    int         cdeg;                 /* degree of each node */
    int         ctyp;                 /* typde : GRCC_AT_xxx */
    int         cnum;                 /* the number of nodes in the class */
    int         cple;                 /* total order of c.c. of each node */
                                      /* = 0 for external particle */
    const char *pname;                /* particle name defined in the model */
                                      /* = NULL for vertex */
    long        pcode;                /* particle code defined in the model */
                                      /* = 0 for vertex */
                                      /* long = int + padding */
} PGInput;

/*---------------------------------------------------------------
 */
#endif
