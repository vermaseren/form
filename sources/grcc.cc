// { ( [
//**************************************************************
// grcc.cc

extern "C" {
#include "form3.h"
}

#include <iostream>
#include <iomanip>
#include <sstream>
#include <cstdio>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// optimization : best combination depends on process by process
#define SIMPSEL

// possible extensions of the program
// #define ORBITS

//--------------------------------------------------------------
#include "grcc.h"
#if 0
extern "C" {
#include "form3.h"
}
#endif

#ifdef GRCC_NAMESPACE
using namespace Grcc;
#endif

//--------------------------------------------------------------
// Macro functions
#define CLWIGHTD(x)   (5*(x))
#define CLWIGHTO(x)   (3*(x)-2)

//--------------------------------------------------------------
// Static variables
#define GRCC_OPT_NAMES  {\
    "Generate particle assigned graphs",\
    "Ouput to file",\
    "Only 1PI graphs",\
    "Exclude graphs with Tadpoles (2 edge connected)",\
    "Exclude graphs with Tadpole blocks (2 node connected)",\
    "Exclude graphs with 2-point subgraphs",\
    "Exclude graphs with more than one block",\
    "Symmetrize initial particls",\
    "Symmetrize final particls",\
}

#define GRCC_OPT_DEFAULTS { GRCC_AGraph, False, True, True, False, False, False, False, False }

// Global variables
static int  optdefaults[GRCC_OPT_Size] = GRCC_OPT_DEFAULTS;
static const char* optnames[GRCC_OPT_Size] = GRCC_OPT_NAMES;

static int      prlevel = 2;
static ErExit  *erExit  = NULL;
static void    *erExitArg  = NULL;

//**************************************************************
// Utility functions
//==============================================================

static void   erEnd(const char *msg);
static Bool   nextPart(int nelem, int nclist, int *clist, int *nl, int *r);
static void   prilist(int n, const int *a, const char *msg);
static int   *newArray(int size, int val);
static int   *deleteArray(int *a);
static int  **newMat(int n0, int n1, int val);
static int  **deleteMat(int **m, int n0);
static void   prIntArray(int n, int *p, const char *msg);
static void   prIntArrayErr(int n, int *p, const char *msg);
static int   *intdup(int n, int *v);
static int   *delintdup(int *v);
static void   bsort(int n, int *ord, int *a);
static void   sorti(int n, int *a);
static int    sortb(int n, int *a);
static int    toSList(int n, int *a);
static int    intSetAdd(int n, int *a, int v, const int size);
static int    intSListAdd(int n, int *a, int v, const int size);
static int    cmpArray(int na, int *a, int nb, int *b);
static Bool   leqArray(int n, int *a, int *b);
static void   prMomStr(int mom, const char *ms, int mn);
static void   listDiff(int na, int *a, int nb, int *b, int *np, int *p, int *nq, int *q, int *nr, int *r);
static Bool   isSubSList(int na, int *a, int nb, int *b);
static int    subtrSet(int na, int *a, int nb, int *b, int *c, int size);
static int    nextPerm(int nelem, int nclass, int *cl, int *r, int *q, int *p, int count);
static BigInt ipow(int n, int p);
static BigInt factorial(int n);

// for debugging
#ifdef CHECK
static Bool   isIn(int n, int *a, int v);
#endif

//==============================================================
// class Options
//--------------------------------------------------------------
Options::Options(void)
{
    model = NULL;
    proc  = NULL;
    sproc = NULL;

    outmg = NULL;
    outag = NULL;

    argmg = NULL;
    argag = NULL;

    setDefaultValue();

    // for output
    out   = new Output(this);

    // subrpocess
    sproc = NULL;

    // measuring time
    time0  = 0.0;
    time1  = 0.0;
}

//--------------------------------------------------------------
Options::~Options(void)
{
    outmg = NULL;
    outag = NULL;

    argmg = NULL;
    argag = NULL;

    delete out;
}

//--------------------------------------------------------------
void Options::setDefaultValue(void)
{
    int j;

    // defualt values
    for (j = 0; j < GRCC_OPT_Size; j++) {
        values[j] = optdefaults[j];
    }

    values[GRCC_OPT_Step] = GRCC_AGraph;

    // print level
    prlevel = 1;
}


//--------------------------------------------------------------
void Options::setOutMG(OutEG *omg, void *pt)
{
    outmg = omg;
    argmg = pt;
}

//--------------------------------------------------------------
void Options::setOutAG(OutEG *oag, void *pt)
{
    outag = oag;
    argag = pt;
}

//--------------------------------------------------------------
void Options::setErExit(ErExit *ere, void *erearg)
{
    erExit    = ere;
    erExitArg = erearg;
}

//--------------------------------------------------------------
void Options::printLevel(int l)
{
    prlevel = l;
}

//--------------------------------------------------------------
void Options::setValue(int ind, int val)
{
    if (0 <= ind && ind < GRCC_OPT_Size) {
        values[ind] = val;
    } else {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** Options::setValue : invalid index=%d ",
                    ind);
            fprintf(GRCC_Stderr, "(val=%d)\n", val);
        }
        erEnd("Options::setValue : invalid index");
    }
}

//--------------------------------------------------------------
int Options::getValue(int ind)
{
    if (0 <= ind && ind < GRCC_OPT_Size) {
        return values[ind];
    } else {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** Options::getValue : invalid index=%d\n", ind);
        }
        erEnd("Options::getValue : invalid index");
    }
    return -1;
}

//--------------------------------------------------------------
void Options::print()
{
    int j;
    
    printf("Options\n");
    printf("+++ GRCC_OPT_Size=%d, print level=%d: message %19s: value (default)\n",
           GRCC_OPT_Size, prlevel, "");
    for (j=0; j < GRCC_OPT_Size; j++) {
        printf("   %4d %-54s : %2d (%2d)\n", 
               j, optnames[j], values[j], optdefaults[j]);
    }
}

//--------------------------------------------------------------
void Options::setOutput(Bool outf, const char *fname)
{
    values[GRCC_OPT_Outgrf] = outf;
    out->setOutgrf(fname);
}

//--------------------------------------------------------------
void Options::printModel(void)
{
    if (prlevel > 0) {
        if (model != NULL) {
            model->prModel();
        } else {
            printf("*** model is not defined\n");
        }
    }
}

//--------------------------------------------------------------
void Options::outModel(void)
{
    if (out != NULL && model != NULL) {
        out->outModel();
    }
}

//--------------------------------------------------------------
void Options::begin(Model *mdl)
{
    model = mdl;
    if (out != NULL) {
        out->outBegin(mdl, values[GRCC_OPT_Outgrf]);
    }
}

//--------------------------------------------------------------
void Options::end(void)
{
    if (prlevel > 0) {
        printf("Optimization: ");
#ifdef SIMPSEL
        printf("SIMPSEL=1 ");
#else
        printf("SIMPSEL=0 ");
#endif
        printf("\n");
    }

    if (out != NULL && values[GRCC_OPT_Outgrf]) {
        out->outEnd();
    }
    model = NULL;
}
//
//--------------------------------------------------------------
void Options::beginProc(Process *prc)
{
    proc = prc;
    if (out != NULL && values[GRCC_OPT_Outgrf]) {
        out->outProcBegin(prc);
    }
    if (proc != NULL) {
        proc->mgrcount = 0;
        proc->agrcount = 0;
    }
}

//--------------------------------------------------------------
void Options::endProc(void)
{
    int k;

    if (prlevel > 0) {
        printf("\n");
        printf("+++ Proc %d: ext=%d, loop=%d, order=",
               proc->id, proc->nExtern, proc->loop);
        prIntArray(model->ncouple, proc->clist, ": ");
        model->prParticleArray(proc->ninitl, proc->initlPart, "-->");
        model->prParticleArray(proc->nfinal, proc->finalPart, "");
        printf(" (%8.2f sec)\n", proc->sec);
    
    
        if (!values[GRCC_OPT_1PI]) {
            printf("    Proc    %d: Total M-Graphs=%ld, M-Graphs=",
                   proc->id, proc->nMGraphs);
            proc->wMGraphs.print(" (Conn)\n");
        }
        printf("    Proc    %d: Total M-Graphs=%ld, M-Graphs=",
               proc->id, proc->nMOPI);
        proc->wMOPI.print(" (1PI)\n");
    
        if (!values[GRCC_OPT_1PI]) {
            printf("    Proc    %d: Total A-Graphs=%ld, A-Graphs=",
                   proc->id, proc->nAGraphs);
            proc->wAGraphs.print(" (Conn)\n");
        }
        printf("    Proc    %d: Total A-Graphs=%ld, A-Graphs=",
               proc->id, proc->nAOPI);
        proc->wAOPI.print(" (1PI)\n");

        printf("#  { %d,{", proc->ninitl);
        for (k = 0; k < proc->ninitl; k++) {
            if (k != 0) {
                printf(", ");
            }
            printf("\"%s\"", proc->model->particleName(proc->initlPart[k]));
        }
        printf("}, %d,{", proc->nfinal);
        for (k = 0; k < proc->nfinal; k++) {
            if (k != 0) {
                printf(", ");
            }
            printf("\"%s\"", proc->model->particleName(proc->finalPart[k]));
        }
        printf("}, {");
        for (k = 0; k < model->ncouple; k++) {
            if (k != 0) {
                printf(", ");
            }
            printf("%d", proc->clist[k]);
        }
        printf("},%6ldL,%6ldL,%3ldL, -1.0, %4.2f},\n",
               proc->nAOPI, proc->wAOPI.num, proc->wAOPI.den, proc->sec);
    }

    if (out != NULL && values[GRCC_OPT_Outgrf]) {
        out->outProcEnd();
    }
    proc = NULL;
}

//--------------------------------------------------------------
void Options::beginSubProc(SProcess *sprc)
{
    sproc = sprc;

    // 'beginProc' is called before
    if (proc != NULL) {
        return;
    }
    if (out != NULL && values[GRCC_OPT_Outgrf]) {
        out->outSProcBegin(sprc);
    }
    sproc->mgrcount = 0;
    sproc->agrcount = 0;
}

//--------------------------------------------------------------
void Options::endSubProc(void)
{
    MGraph *mgraph = sproc->mgraph;

    if (sproc != NULL) {
        sproc->resultMGraph(mgraph->cDiag, mgraph->wscon,
                            mgraph->c1PI, mgraph->wsopi);
    }

    if (prlevel > 1) {
        printf("\n");
        printf("+++ Subproc %d: ext=%d, loop=%d, nodes=%d, edges=%d\n",
               sproc->id, sproc->nExtern, sproc->loop,
               sproc->nNodes, sproc->nEdges);
    
        if (!values[GRCC_OPT_1PI]) {
            printf("    Subproc %d: Total M-Graphs=%ld, M-Wsum=",
                   sproc->id, sproc->nMGraphs);
            sproc->wMGraphs.print(" (Conn)\n");
        }
        printf("    Subproc %d: Total M-Graphs=%ld, M-Wsum=",
               sproc->id, sproc->nMOPI);
        sproc->wMOPI.print(" (1PI)\n");
    
        if (!values[GRCC_OPT_1PI]) {
            printf("    Subproc %d: Total A-Graphs=%ld, A-Wsum=", 
                   sproc->id, sproc->nAGraphs);
            sproc->wAGraphs.print(" (Conn)\n");
        }
        printf("    Subproc %d: Total A-Graphs=%ld, A-Wsum=", 
               sproc->id, sproc->nAOPI);
        sproc->wAOPI.print(" (1PI)\n");
    }
    if (prlevel > 0) {
        printf("\n");
        printf("* Total %ld MGraphs; %ld 1PI", mgraph->cDiag, mgraph->c1PI);
        printf(" wscon = ");
        mgraph->wscon.print(" ( ");
        mgraph->wsopi.print(" 1PI)\n");

#ifdef MONITOR
        printf("* refine:                     %ld\n", mgraph->nCallRefine);
        printf("* discarded for refinement:   %ld\n", mgraph->discardRefine);
        printf("* discarded for disconnected: %ld\n", mgraph->discardDisc);
        printf("* discarded for duplication:  %ld\n", mgraph->discardIso);
#endif
    }

    if (proc != NULL) {
        return;
    }
    if (out != NULL && values[GRCC_OPT_Outgrf]) {
        out->outProcEnd();
    }
}

//--------------------------------------------------------------
void Options::newMGraph(MGraph *mgr)
{
    MGraph *mgraph = mgr;

#ifdef DEBUG1
    if (proc != NULL) {
        printf("+++ New EGraph (MG): %ld\n", proc->mgrcount);
    } else if (sproc != NULL) {
        printf("+++ New EGraph (MG): %ld\n", sproc->mgrcount);
    }
#endif
    if (proc != NULL) {
        proc->mgrcount++;
        mgr->egraph->mId = proc->mgrcount;
    } else if (sproc != NULL) {
        sproc->mgrcount++;
        mgr->egraph->mId = sproc->mgrcount;
    }

    if (out != NULL && values[GRCC_OPT_Outgrf] && values[GRCC_OPT_Step] == GRCC_MGraph) {
        out->outEGraph(mgraph->egraph);
    }

    if (sproc != NULL) {
        sproc->endMGraph(mgr);
    }
    if (outmg != NULL) {
        (*outmg)(mgr->egraph, argmg);
    }
}

//--------------------------------------------------------------
void Options::newAGraph(EGraph *egraph)
{
    Fraction sf, zr;

#ifdef DEBUG1
    if (proc != NULL) {
        printf("+++ New EGraph (AG): %ld (%ld)\n",
               proc->agrcount, proc->mgrcount);
    } else if (sproc != NULL) {
        printf("+++ New EGraph (AG): %ld (%ld)\n",
               sproc->agrcount, sproc->mgrcount);
    }
#endif
    if (proc != NULL) {
        proc->agrcount++;
        egraph->sId = proc->agrcount;
    } else if (sproc != NULL) {
        sproc->agrcount++;
        egraph->sId = sproc->agrcount;
    }

    if (sproc != NULL) {
        zr = Fraction(0, 1);
        if ((egraph->nsym)*(egraph->esym) != 0) {
            sf = Fraction(egraph->extperm, egraph->nsym*egraph->esym);
        } else {
            sf = zr;
        }
        if (egraph->mgraph->opi) {
            sproc->resultAGraph(1, sf, 1, sf);
        } else {
            sproc->resultAGraph(1, sf, 0, zr);
        }
    }

    if (out != NULL && values[GRCC_OPT_Outgrf]) {
        out->outEGraph(egraph);
    }

    sproc->endAGraph(egraph);
    if (outag != NULL) {
        (*outag)(egraph, argag);
    }
}

//==============================================================
Output::Output(Options *optn)
{
    opt     = optn;
    model   = NULL;
    proc    = NULL;
    outgrfp = NULL;
    procId  = -1;
    outgrf  = NULL;
    outproc = False;

}

//--------------------------------------------------------------
Output::~Output()
{
    if (outgrfp != NULL) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** file has not been closed : \"%s\"\n", outgrf);
        }
        fclose(outgrfp);
        outgrfp = NULL;
        erEnd("file has not been closed");
    }
    if (outgrf != NULL) {
        free(outgrf);
        outgrf = NULL;
    }
}

//--------------------------------------------------------------
void Output::setOutgrf(const char *fname)
{
    if (outgrf != NULL && strcmp(outgrf, fname) == 0) {
        return;
    }

    if (outgrf != NULL) {
        delete outgrf;
    }
    if (fname == NULL || strlen(fname) < 1) {
        outgrf = NULL;
    } else {
        outgrf = strdup(fname);
    }
}

//--------------------------------------------------------------
Bool Output::outBegin(Model *mdl, Bool outgrff)
{
    model = mdl;

    if (outgrf == NULL || model == NULL || strlen(outgrf) < 1) {
        outgrfp = NULL;
        return True;
    } else if (outgrfp != NULL) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** outBegin: \"%s\" is already opened\n", outgrf);
        }
        erEnd("outBegin: file is already opened\n");
    }
 
    if (!outgrff) {
        return True;
    }
    if ((outgrfp = fopen(outgrf, "w")) == NULL) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** outBegin: cannot open %s\n", outgrf);
        }
        return False;
    }
    fprintf(outgrfp, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
    fprintf(outgrfp, "%% Generated by 'grcc'\n");
    fprintf(outgrfp, "Version={2,2,0,0};\n");
    fprintf(outgrfp, "Model=\"./%s.mdl\";\n", model->name);

    return True;
}

//--------------------------------------------------------------
void Output::outEnd(void)
{
    if (outgrfp == NULL) {
        return;
    }
    fprintf(outgrfp, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
    fprintf(outgrfp, "End=1;\n");
    fprintf(outgrfp, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
    fclose(outgrfp);
    outgrfp = NULL;

    free(outgrf);
    outgrf = NULL;
}

//--------------------------------------------------------------
void Output::outProcBegin(Process *prc)
{
    int k, ex;

    if (prc == NULL) {
        return;
    }

    proc    = prc;
    sproc   = NULL;
    procId  = proc->id;

    if (outgrfp == NULL) {
        return;
    }
    if (outproc) {
        return;
    }
    outproc = True;

    fprintf(outgrfp, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
    fprintf(outgrfp, "Process=%d;\n", proc->id);
    fprintf(outgrfp, "External=%d;\n", proc->ninitl+proc->nfinal);
    for (k = 0, ex = 0; k < proc->ninitl; k++, ex++) {
        fprintf(outgrfp, "%4d= initial %s;\n", ex,
                proc->model->particleName(proc->initlPart[k]));
    }
    for (k = 0; k < proc->nfinal; k++, ex++) {
        fprintf(outgrfp, "%4d= final   %s;\n", ex,
                proc->model->particleName(proc->finalPart[k]));
    }
    fprintf(outgrfp, "Eend;\n");
    for (k = 0; k < proc->model->ncouple; k++) {
        fprintf(outgrfp, "%s=%d;  ", proc->model->cnlist[k], proc->clist[k]);
    }
    fprintf(outgrfp, "Loop=%d;\n", proc->loop);
    fprintf(outgrfp, "OPI=%s;\n", (opt->values[GRCC_OPT_1PI] ? "Yes" : "No"));
    fprintf(outgrfp, "Assign=%s;\n", (opt->values[GRCC_OPT_Step]==GRCC_AGraph ? "Yes" : "No"));
    fprintf(outgrfp, "%% Options : dummy values\n");
    fprintf(outgrfp, "Expand=Yes; ExpMin=0; ExpMax=-1;\n");
    fprintf(outgrfp, "Block=No; Tadpole=Yes; Extself=Yes;\n");
    fprintf(outgrfp, "Selfe=Yes; Countert=No; AnyCT=No;Undefp=No;\n");
}

//--------------------------------------------------------------
void Output::outSProcBegin(SProcess *sprc)
{
    int j, k, ex;
    PNodeClass *pnc;

    if (sprc == NULL) {
        return;
    }

    sproc  = sprc;
    procId = sproc->id;
    pnc    = sproc->pnclass;

    if (outgrfp == NULL) {
        return;
    }
    if (outproc) {
        return;
    }
    outproc = True;
    fprintf(outgrfp, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
    fprintf(outgrfp, "Process=%d;\n", sproc->id);
    fprintf(outgrfp, "External=%d;\n", sproc->nExtern);

    ex = 0;
    for (j = 0; j < pnc->nclass; j++) {
        if (pnc->type[j] == GRCC_AT_Initial) {
            for (k = 0; k < pnc->count[j]; k++) {
                fprintf(outgrfp, "%4d= initial %s;\n", ex,
                        model->particleName(pnc->particle[j]));
                ex++;
            }
        } else if (pnc->type[j] == GRCC_AT_Final) {
            for (k = 0; k < pnc->count[j]; k++) {
                fprintf(outgrfp, "%4d= final %s;\n", ex,
                        model->particleName(-pnc->particle[j]));
                ex++;
            }
        }
    }

    fprintf(outgrfp, "Eend;\n");
    for (k = 0; k < sproc->model->ncouple; k++) {
        fprintf(outgrfp, "%s=%d;  ", sproc->model->cnlist[k], sproc->clist[k]);
    }
    fprintf(outgrfp, "Loop=%d;\n", sproc->loop);
    fprintf(outgrfp, "OPI=%s;\n", (opt->values[GRCC_OPT_1PI] ? "Yes" : "No"));
    fprintf(outgrfp, "Assign=%s;\n", (opt->values[GRCC_OPT_Step]==GRCC_AGraph ? "Yes" : "No"));
    fprintf(outgrfp, "%% Options : dummy values\n");
    fprintf(outgrfp, "Expand=Yes; ExpMin=0; ExpMax=-1;\n");
    fprintf(outgrfp, "Block=No; Tadpole=Yes; Extself=Yes;\n");
    fprintf(outgrfp, "Selfe=Yes; Countert=No; AnyCT=No;Undefp=No;\n");
}

//--------------------------------------------------------------
void Output::outProcEnd(void)
{
    if (outgrfp == NULL) {
        return;
    }
    if (proc != NULL) {
        fprintf(outgrfp, "%%------------------------------\n");
        fprintf(outgrfp, "Pend=%d;\n", proc->id);
    } else if (sproc != NULL) {
        fprintf(outgrfp, "%%------------------------------\n");
        fprintf(outgrfp, "Pend=%d;\n", sproc->id);
    }
    fflush(outgrfp);
}

//--------------------------------------------------------------
void Output::outEGraph(EGraph *egraph)
{
    int nd, lg, ptcl, ed, intr, j, k, loop;
    Model *mdl = egraph->model;
    Bool popt;
    EFLine *fl;

    if (outgrfp == NULL) {
        return;
    }
    fprintf(outgrfp, "%%------------------------------\n");
    if (egraph->assigned) {
        fprintf(outgrfp, "Graph=%ld;\n", egraph->sId);
        fprintf(outgrfp, "%% AGraph=%ld;\n", egraph->aId);
    } else {
        fprintf(outgrfp, "Graph=%ld;\n", egraph->mId);
    }
    fprintf(outgrfp, "Gtype=%ld;\n", egraph->mId);
    if (egraph->assigned) {
        fprintf(outgrfp, "Sfactor=%ld;\n", 
                egraph->nsym * egraph->esym * egraph->fsign);
    } else {
        fprintf(outgrfp, "Sfactor=%ld;\n", egraph->nsym * egraph->esym);
    }
    fprintf(outgrfp, "%% ExtPerm=%ld; NSym=%ld; ESym=%ld; NSym1=%ld; Multp=%ld;\n", 
            egraph->extperm, egraph->nsym, egraph->esym, egraph->nsym1, egraph->multp);
    fprintf(outgrfp, "Vertex=%d;\n", egraph->nNodes - egraph->nExtern);

    for (nd = 0; nd < egraph->nNodes; nd++) {
        fprintf(outgrfp, "%4d", nd);
        if (mdl != NULL && egraph->assigned && !egraph->isExternal(nd)) {
            intr = egraph->nodes[nd]->intrct;
            loop = mdl->interacts[intr]->loop;
            popt = False;

            // not tree vertex
            if (loop > 0) {
                fprintf(outgrfp, "[loop=%d", loop);
                popt = True;
            }

            // multiple coupling constants
            if (mdl->ncouple > 1) {
                if (popt) {
                    fprintf(outgrfp, ",order={");
                } else {
                    fprintf(outgrfp, "[order={");
                    popt = True;
                }
                for (j = 0; j < mdl->interacts[intr]->nclist; j++) {
                    if (j != 0) {
                        fprintf(outgrfp, ",");
                    }
                    fprintf(outgrfp, "%d", mdl->interacts[intr]->clist[j]);
                }
                fprintf(outgrfp, "}");
            }
            if (popt) {
                fprintf(outgrfp, "]");
            }
        } else if (mdl != NULL && !egraph->isExternal(nd)) {
            loop = egraph->nodes[nd]->extloop;
            // not tree vertex
            if (loop > 0) {
                fprintf(outgrfp, "[loop=%d]", loop);
            }
        }
        fprintf(outgrfp, "={");

        // list of legs
        for (lg = 0; lg < egraph->nodes[nd]->deg; lg++) {
            if (lg != 0) {
                fprintf(outgrfp, ", ");
            }
            ed  = Abs(egraph->nodes[nd]->edges[lg])-1;
            fprintf(outgrfp, "%4d", ed+1);
            ptcl = egraph->edges[ed]->ptcl;
            if (mdl != NULL && ptcl != 0 && egraph->assigned) {
                if (egraph->nodes[nd]->edges[lg] < 0) {
                    ptcl = mdl->normalParticle(-ptcl);
                } else {
                    ptcl = mdl->normalParticle(ptcl);
                }
                fprintf(outgrfp, "[%s]", mdl->particleName(ptcl));
            } else {
                fprintf(outgrfp, "[undef]");
            }
        }
        fprintf(outgrfp, "};\n");
    }
    // print Fermion line as comment line
    fprintf(outgrfp, "%%  FLines=%d; FSign=%d; sId=%ld;\n",
            egraph->nflines, egraph->fsign, egraph->sId);
    for (j = 0; j < egraph->nflines; j++) {
        fl = egraph->flines[j];
        fprintf(outgrfp, "%% %4d", j);
        if (fl->ftype == FL_Open) {
            fprintf(outgrfp, "[Open]=[");
        } else if (fl->ftype == FL_Closed) {
            fprintf(outgrfp, "[Loop]=[");
        } else {
            fprintf(outgrfp, " ?%d", fl->ftype);
        }
        for (k = 0; k < fl->nlist; k++) {
            if (k != 0) {
                fprintf(outgrfp, ", ");
            }
            fprintf(outgrfp, "%d", Abs(fl->elist[k]));
        }
        fprintf(outgrfp, "];\n");
    }
    fprintf(outgrfp, "Vend;\n");
    fprintf(outgrfp, "Gend;\n");
}

//--------------------------------------------------------------
void Output::outModel(void)
{
    char        *mdlfn;
    FILE        *mdlfp;
    Particle    *pt;
    Interaction *vt;
    int          j, k, l, ln;
    const char        *ptn;

    if (model == NULL) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** Output::outModel : model is not defined\n");
        }
        return;
    }
    ln = strlen(model->name)+5;
    mdlfn = new char[ln];
    snprintf(mdlfn, ln, "%s.mdl", model->name);

    if ((mdlfp = fopen(mdlfn, "w")) == NULL) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** Output::outModel : cannot open \"%s\"\n",
                    mdlfn);
        }
        return;
    }

    for (l = 0; l < 60; l++) { putc('%', mdlfp); } putc('\n', mdlfp);
    fprintf(mdlfp, "%% File \"%s\"\n", mdlfn);
    fprintf(mdlfp, "%% Generated by graph generater\n");
    fprintf(mdlfp, " Version={2,2,0};\n");

    // copupling constants
    fprintf(mdlfp, " Order={");
    for (j = 0; j < model->ncouple; j++) {
        if (j != 0) {
            fprintf(mdlfp, ", ");
        }
        fprintf(mdlfp, "%s", model->cnlist[j]);
    }
    fprintf(mdlfp, "};\n");

    for (l = 0; l < 60; l++) { putc('%', mdlfp); } putc('\n', mdlfp);
    fprintf(mdlfp, "%% particles\n");
    for (l = 0; l < 60; l++) { putc('%', mdlfp); } putc('\n', mdlfp);
    for (j = 1; j < model->nParticles; j++) {
        pt = model->particles[j];
        fprintf(mdlfp, " Particle=%s; Antiparticle=%s;\n",
                pt->name, pt->aname);
        ptn = pt->typeGName();
        fprintf(mdlfp, " PType=%s; Charge=0; Color=1; Mass=0; Width=0;\n", ptn);
        fprintf(mdlfp, " PCode=%d; Massless; \n", pt->pcode);
        fprintf(mdlfp, " Pend;\n");
        fprintf(mdlfp, "%%\n");
    }
    for (l = 0; l < 60; l++) { putc('%', mdlfp); } putc('\n', mdlfp);
    fprintf(mdlfp, "%% interactions\n");
    for (l = 0; l < 60; l++) { putc('%', mdlfp); } putc('\n', mdlfp);
    for (j = 0; j < model->nInteracts; j++) {
        vt = model->interacts[j];
        fprintf(mdlfp, " Vertex={");
        for (k = 0; k < vt->nplist; k++) {
            if (k != 0) {
                fprintf(mdlfp, ", ");
            }
            fprintf(mdlfp, "%s", model->particleName(vt->plist[k]));
        }
        fprintf(mdlfp, "}; ");
        for (k = 0; k < model->ncouple; k++) {
            fprintf(mdlfp, "%s=%d; ", model->cnlist[k], vt->clist[k]);
        }
        fprintf(mdlfp, "FName=%s; Vend;\n", vt->name);
    }
    for (l = 0; l < 60; l++) { putc('%', mdlfp); } putc('\n', mdlfp);
    fprintf(mdlfp, " Mend;\n");
    for (l = 0; l < 60; l++) { putc('%', mdlfp); } putc('\n', mdlfp);
    fclose(mdlfp);
    delete[] mdlfn;
}

//**************************************************************
// model.cc

//==============================================================

static const char *ptypenames[]  = GRCC_PT_Table;
static const char *pgtypenames[] = GRCC_PT_GTable;

//==============================================================
// class Particle
//--------------------------------------------------------------
Particle::Particle(Model *modl, int pid, PInput *pinp)
{
    static char buff[100];
    static int  nbuff=100;
    int j;

    mdl     = modl;                   // the model
    id      = pid;                    // id of this particle
    if (pinp->name == NULL || strlen(pinp->name) < 1) {
        if (pinp->pcode != 0) {
            snprintf(buff, nbuff, "pa%d", pinp->pcode);
            name = strdup(buff);
        } else {
            snprintf(buff, nbuff, "pi%d", pid);
            name = strdup(buff);
        }
    } else {
        name = strdup(pinp->name);     // the name of the particle
    }
    if (pinp->aname == NULL) {
        if (pinp->acode != 0) {
            snprintf(buff, nbuff, "ap%d", Abs(pinp->acode));
            aname = strdup(buff);
        } else {
            snprintf(buff, nbuff, "ai%d", pid);
            aname = strdup(buff);
        }
    } else {
        aname   = strdup(pinp->aname); // the name of the anti-particle
    }
    pcode   = pinp->pcode;
    acode   = pinp->acode;
    if (mdl->defpart == GRCC_DEFBYCODE) {
        neutral = (pcode == acode);
    } else {
        neutral = ((strcmp(name, aname)==0) ? True : False);
    }

    // convert ptype string to its code.
    if (mdl->defpart == GRCC_DEFBYCODE) {
        if (pinp->ptypec < GRCC_PT_Undef || pinp->ptypec > GRCC_PT_Size) {
            if (prlevel > 0) {
                fprintf(GRCC_Stderr, "*** particle type is not defined: %ld\n",
                        pinp->ptypec);
            }
            erEnd("particle type is not defined (illegal code)");
        }
        ptype = pinp->ptypec;
    } else {
        if (pinp->ptypen == NULL) {
            erEnd("particle type is not defined (NULL)");
        }
        ptype = -1;
        for (j = 0; j < GRCC_PT_Size; j++) {
            if (strcmp(pinp->ptypen, ptypenames[j]) == 0) {
                ptype = j;
                break;
            }
        }
        if (ptype < 0) {
            if (prlevel > 0) {
                fprintf(GRCC_Stderr, "*** particle type \"%s\" is not defined",
                        pinp->ptypen);
            }
            erEnd("particle type is not defined (name)");
        }
    }
    if (ptype == GRCC_PT_Undef && ptype != 0) {
        erEnd("illegal ptype");
    }
}

//--------------------------------------------------------------
Particle::~Particle()
{
    free(aname);
    free(name);
}

//--------------------------------------------------------------
char *Particle::particleName(int p)
{
    if (p < 0) {
        return aname;
    } else {
        return name;
    }
}

//--------------------------------------------------------------
int Particle::particleCode(int p)
{
    if (p < 0) {
        return acode;
    } else {
        return pcode;
    }
}

//--------------------------------------------------------------
int Particle::isNeutral(void)
{
    return neutral;
}

//--------------------------------------------------------------
const char *Particle::typeName(void)
{
    return ptypenames[ptype];
}

//--------------------------------------------------------------
const char *Particle::typeGName(void)
{
    return pgtypenames[ptype];
}

//--------------------------------------------------------------
char *Particle::aparticle(void)
{
    static char prtcl[] = "Particle";

    if (isNeutral()) {
        return prtcl;
    } else {
        return aname;
    }
}

//--------------------------------------------------------------
void Particle::prParticle(void)
{
    if (isNeutral()) {
        printf("pid=%d, name=\"%s\", real_field, ", id, name);
    } else {
        printf("pid=%d, name=\"%s\", aname=\"%s\", ", id, name, aname);
    }
    printf("ptype=%s, pcode=%d, acode=%d\n", ptypenames[ptype], pcode, acode);
}

//==============================================================
// class Interaction
//--------------------------------------------------------------
Interaction::Interaction(Model *modl, int iid, const char *nam, int icd, int *cpl, int nlgs, int *plst, int csm, int lp)
{
    static char buff[100];
    static int  nbuff=100;
    int j;

    mdl    = modl;         // the model
    id     = iid;          // id of this interaction
    icode  = icd;          // id of this interaction
    csum   = csm;          // the total oders of coupling constants
    nlegs  = nlgs;         // the size of plist[]
    loop   = lp;           // the number of loops

    if (nam == NULL || strlen(nam) < 1) {
        snprintf(buff, nbuff, "vt%d", icd);
        name = strdup(buff);
    } else {
        name = strdup(nam);  // the name of the interaction
    }
    nclist = mdl->ncouple;
    clist  = new int[nclist];
    icode  = icd;
    for (j = 0; j < mdl->ncouple; j++) {
        clist[j] = cpl[j];
    }
    plist  = new int[nlegs];
    nplist = nlegs;
    for (j = 0; j < nlegs; j++) {
        plist[j] = plst[j];
    }
    nslist = 0;
    slist  = NULL;
}

//--------------------------------------------------------------
Interaction::~Interaction()
{
    if (slist != NULL) {
        delete[] slist;
    }
    if (plist != NULL) {
        delete[] plist;
    }
    if (clist != NULL) {
        delete[] clist;
    }
    free(name);
}

//--------------------------------------------------------------
void Interaction::prInteraction(void)
{
    int j;

    printf("vid=%d, icode=%d, name=\"%s\", loop=%d, csum=%d, cpl=",
          id, icode, name, loop, csum);
    prIntArray(mdl->ncouple, clist, ", legs=[");
    for (j = 0; j < nplist; j++) {
        if (j != 0) {
            printf(", ");
        }
        printf("%s", mdl->particleName(plist[j]));
    }
    printf("];\n");

}

//==============================================================
// class Model
//--------------------------------------------------------------
Model::Model(MInput *minp)
{
    static PInput pundef = {"undef", "undef", 0, 0, "undef", 0};
    int j;

    if (minp->name == NULL || strlen(minp->name) < 1) {
        erEnd("model should have a name");
    }
    name    = strdup(minp->name);
    ncouple = minp->ncouple;
    if (ncouple >= GRCC_MAXNCPLG) {
        erEnd("too many coupling constants in the model (GRCC_MAXNCPLG)");
    }
    // cnlist = minp->cnamlist;
    cnlist = new char*[ncouple];
    for (j = 0; j < ncouple; j++) {
        cnlist[j] = strdup(minp->cnamlist[j]);
    }

    nParticles = 0;
    particles  = new Particle*[GRCC_MAXMPARTICLES];
    for (j = 0; j < GRCC_MAXMPARTICLES; j++) {
        particles[j] = NULL;
    }
    pdef = False;

    nInteracts = 0;
    interacts  = new Interaction*[GRCC_MAXMINTERACT];
    for (j = 0; j < GRCC_MAXMINTERACT; j++) {
        interacts[j] = NULL;
    }
    vdef       = False;

    if (particles == NULL || interacts == NULL) {
        erEnd("memory allocation failed");
    }

    if (minp->defpart != GRCC_DEFBYNAME && minp->defpart != GRCC_DEFBYCODE) {
        erEnd("illegal value of defpart");
    }

    defpart = minp->defpart;

    maxnlegs   = 0;
    maxcpl     = 0;
    maxloop    = 0;
    ncplgcp    = 0;

    addParticle(&pundef);
}

//--------------------------------------------------------------
Model::~Model()
{
    int j;

    for (j = ncplgcp-1; j >= 0; j--) {
        cplgvl[j] = delintdup(cplgvl[j]);
    }
    delete[] cplgvl;
    cplgnvl = delintdup(cplgnvl);
    cplglg  = delintdup(cplglg);
    cplgcp  = delintdup(cplgcp);

    for (j = GRCC_MAXMINTERACT-1; j >= 0; j--) {
        if (interacts[j] != NULL) {
            interacts[j]->slist = delintdup(interacts[j]->slist);
            delete interacts[j];
            interacts[j] = NULL;
        }
    }
    delete[] interacts;
    interacts = NULL;

    for (j = GRCC_MAXMPARTICLES-1; j >= 0; j--) {
        if (particles[j] != NULL) {
            delete particles[j];
            particles[j] = NULL;
        }
    }
    delete[] particles;
    particles = NULL;

    for (j=ncouple-1; j>=0; j--) {
        free(cnlist[j]);
    }
    delete[] cnlist;

    free(name);
}

//--------------------------------------------------------------
void Model::prModel(void)
{
    static char hdr[] = "#=================================================\n";
    static char hd1[] = "#-------------------------------------------------\n";
    int j;

    printf("%s", hdr);
    printf("Model=\"%s\", ", name);
    printf("coupling=[");
    for (j = 0; j < ncouple; j++) {
        if (j != 0) {
            printf(", ");
        }
        printf("\"%s\"", cnlist[j]);
    }
    printf("];\n");
    printf("%s", hd1);
    printf("Particles=%d;\n", nParticles);
    for (j = 1; j < nParticles; j++) {
        particles[j]->prParticle();
    }
    printf("EndParticle;\n");
    printf("%s", hd1);
    printf("Interactions=%d;\n", nInteracts);
    for (j = 0; j < nInteracts; j++) {
        interacts[j]->prInteraction();
    }
    printf("EndInteraction;\n");
    printf("%s", hd1);
    printf("InteractionTable;\n");
    printf("#  %d class in (total coupling, degree)\n", ncplgcp);
    for (j = 0; j < ncplgcp; j++) {
        printf("  class=%d: cp=%d, lg=%d, vl=", j, cplgcp[j], cplglg[j]);
        prIntArray(cplgnvl[j], cplgvl[j], "\n");
    }
    printf("EndInteractionTable;\n");
    printf("%s", hdr);
}

//--------------------------------------------------------------
void Model::addParticle(PInput *pinp)
{
    int  nid, aid, pid, pcd, acd;

    if (nParticles >= GRCC_MAXMPARTICLES) {
        erEnd("particle table overflow (GRCC_MAXMPARTICLES)");
    }

    // uniqueness check
    if (defpart == GRCC_DEFBYCODE) {
        pcd = findParticleCode(pinp->pcode);
        acd = findParticleCode(pinp->acode);
        if (pcd > 0 || acd > 0) {
            if (prlevel > 0) {
                fprintf(GRCC_Stderr, "*** particle code [%d, %d] ", 
                        pinp->pcode, pinp->acode);
                fprintf(GRCC_Stderr, "is already used.\n");
            }
            erEnd("particle code is already defined");
        }
    } else {
        if (pinp->name == NULL || pinp->aname == NULL ||
            strlen(pinp->name) < 1 || strlen(pinp->aname) < 1) {
            erEnd("no name of particle or anti-particle.");
        }
        nid = findParticleName(pinp->name);
        aid = findParticleName(pinp->aname);
        if (nid > 0 || aid > 0) {
            if (prlevel > 0) {
                fprintf(GRCC_Stderr, "*** particle name [%s, %s] ", 
                        pinp->name, pinp->aname);
                fprintf(GRCC_Stderr, "is already used.\n");
            }
            erEnd("particle name is already defined.");
        }
    }

    pid = nParticles++;
    particles[pid] = new Particle(this, pid, pinp);
}

//--------------------------------------------------------------
void Model::addParticleEnd(void)
{
    int p, ap;

    pdef = True;

    nallPart = 0;
    for (p = nParticles-1; p >= 1; p--) {
        ap = antiParticle(p);
        if (ap != p) {
            allPart[nallPart++] = ap;
        }
    }
    for (p = 1; p < nParticles; p++) {
        allPart[nallPart++] = p;
    }
    sorti(nallPart, allPart);
}

//--------------------------------------------------------------
void Model::addInteraction(IInput *iinp)
{
    int vid, nlegs, j, k, c;
    int csum, lp2, lp;
    int plist[GRCC_MAXLEGS];

    if (!pdef) {
        erEnd("call 'addParticleEnd' before 'addInteraction'");
    }
    if (nInteracts >= GRCC_MAXMINTERACT) {
        erEnd("too many interactions in the model (GRCC_MAXMINTERACT)");
    }
    nlegs = iinp->nplistn;
    if (nlegs >= GRCC_MAXLEGS) {
        erEnd("too many legs in an interaction (GRCC_MAXLEGS)");
    }

    // check identifier
    if (defpart == GRCC_DEFBYCODE) {
        if (iinp->icode < 0) {
            if (prlevel > 0) {
                fprintf(GRCC_Stderr, "*** vertex code %d should be positive\n",
                        iinp->icode);
            }
            erEnd("vertex code should be positive");
        } else {
            vid = findInteractionCode(iinp->icode);
            if (vid > 0) {
                if (prlevel > 0) {
                    fprintf(GRCC_Stderr, "*** vertex code %d is already used\n",
                            iinp->icode);
                }
                erEnd("vertex code is already used");
            }
        }
    } else {
        if (iinp->name == NULL || strlen(iinp->name) < 1) {
            erEnd("no name of vertex\n");
        }
        vid = findInteractionName(iinp->name);
        if (vid >= 0) {
            if (prlevel > 0) {
                fprintf(GRCC_Stderr, "*** vertex name %s is already used\n",
                iinp->name);
                erEnd("vertex name is already used");
            }
        }
    }
    vid = nInteracts++;

    for (j = 0; j < nlegs; j++) {
        if (defpart == GRCC_DEFBYCODE) {
            plist[j] = findParticleCode(iinp->plistc[j]);
            if (plist[j] == 0) {
                if (prlevel > 0) {
                    fprintf(GRCC_Stderr, "*** particle code %d ",
                            iinp->plistc[j]);
                    fprintf(GRCC_Stderr, "is not defined\n");
                }
                erEnd("particle is not defined (code)");
            }
        } else {
            plist[j] = findParticleName(iinp->plistn[j]);
            if (plist[j] == 0) {
                if (prlevel > 0) {
                    fprintf(GRCC_Stderr, "*** particle name %s ",
                            iinp->plistn[j]);
                    fprintf(GRCC_Stderr, "is not defined\n");
                }
                erEnd("particle is not defined (name)");
            }
        }
    }

    csum = 0;
    for (j = 0; j < ncouple; j++) {
        c = iinp->cvallist[j];
        if (c < 0) {
            if (prlevel > 0) {
                fprintf(GRCC_Stderr, "*** illegal value of c-constants \n");
                for (k = 0; k < ncouple; k++) {
                      fprintf(GRCC_Stderr, "    %s = %d\n",
                              cnlist[k], iinp->cvallist[k]);
                }
            }
            erEnd("illegal value of c-constants");
        }
        csum += c;
    }
    if (csum < 0) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** illegal total coupling-constants %d\n",
                    csum);
            for (k = 0; k < ncouple; k++) {
                fprintf(GRCC_Stderr, "    %s = %d\n", 
                        cnlist[k], iinp->cvallist[k]);
            }
        }
        erEnd("illegal value of c-constants");
    }

    // assume : csum = nlegs - 2 + 2*loop
    lp2 = csum - nlegs + 2;
    if (lp2 % 2 != 0 || lp2 < 0) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** illegal coupling const : ");
            fprintf(GRCC_Stderr, "nlegs - 2 + 2*loop: 2*loop = %d\n", lp2);
        }
    }
    lp = lp2/2;

    maxnlegs = Max(maxnlegs, nlegs);
    maxloop  = Max(maxloop , lp);
    maxcpl   = Max(maxcpl  , csum);

    interacts[vid] = new Interaction(this, vid, iinp->name, iinp->icode, 
                             iinp->cvallist, nlegs, plist, csum, lp);
}

//--------------------------------------------------------------
void Model::addInteractionEnd(void)
{
    int lst[GRCC_MAXMINTERACT];
    int tcp[GRCC_MAXMINTERACT], tlg[GRCC_MAXMINTERACT];
    int tnvl[GRCC_MAXMINTERACT], *tvl[GRCC_MAXMINTERACT];
    Interaction *vt;
    int cp, lg, j, k;

    vdef = True;
    ncplgcp = 0;
    for (cp = 0; cp <= maxcpl; cp++) {

        for (lg = 0; lg <= maxnlegs; lg++) {
            k = 0;
            for (j = 0; j < nInteracts; j++) {
                vt = interacts[j];
                if (vt->csum == cp && vt->nlegs == lg) {
                    lst[k++] = j;
                }
            }
            if (k > 0) {
                tcp[ncplgcp]  = cp;
                tlg[ncplgcp]  = lg;
                tnvl[ncplgcp] = k;
                tvl[ncplgcp]  = intdup(k, lst);
                ncplgcp++;
            }
        }
    }
    cplgcp  = intdup(ncplgcp, tcp);
    cplglg  = intdup(ncplgcp, tlg);
    cplgnvl = intdup(ncplgcp, tnvl);
    cplgvl  = new int*[ncplgcp];
    for (j = 0; j < ncplgcp; j++) {
        if (cplgnvl[j] > 0) {
            cplgvl[j] = tvl[j];
        } else {
            cplgvl[j] = NULL;
        }
    }

    for (j = 0; j < nInteracts; j++) {
        vt = interacts[j];
        if (vt->slist != NULL) {
            erEnd("addInteractionEnd: vt->nslist != NULL");
        }
        vt->slist  = intdup(vt->nplist, vt->plist);
        vt->nslist = toSList(vt->nplist, vt->slist);
    }
#ifdef DEBUG
    prModel();
#endif
}

//--------------------------------------------------------------
int Model::findParticleName(const char *name)
{
    int j;

    for (j = 0; j < nParticles; j++) {
        if (strcmp(name, particles[j]->name) == 0) {
            return j;
        }
    }
    for (j = 0; j < nParticles; j++) {
        if (strcmp(name, particles[j]->aname) == 0) {
            return -j;
        }
    }
    return 0;
}

//--------------------------------------------------------------
int Model::findParticleCode(int pcd)
{
    int j;

    for (j = 0; j < nParticles; j++) {
        if (pcd == particles[j]->pcode) {
            return j;
        } else if (pcd == particles[j]->acode) {
            return -j;
        }
    }
    return 0;
}

//--------------------------------------------------------------
char *Model::particleName(int p)
{
    int q;

    if (Abs(p) >= nParticles) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "\n*** Model::particleName: ");
            fprintf(GRCC_Stderr, "illegal particle id=%d\n", p);
        }
        erEnd("Model::particleName: illegal particle");
    }
    if (p < 0) {
        q = - p;
    } else {
        q = p;
    }
    return particles[q]->particleName(p);
}

//--------------------------------------------------------------
int Model::particleCode(int p)
{
    int q;

    if (Abs(p) >= nParticles) {
        fprintf(GRCC_Stderr, "\n*** Model::particleCode: illegal particle id=%d\n",
                p);
        erEnd("Model::particleCode: illegal particle");
    }
    if (p < 0) {
        q = - p;
    } else {
        q = p;
    }
    return particles[q]->particleCode(p);
}

//--------------------------------------------------------------
void Model::prParticleArray(int n, int *a, const char *msg)
{
    int j;

    printf("[");
    for (j = 0; j < n; j++) {
        if (j != 0) {
            printf(", ");
        }
        printf("%s", particleName(a[j]));
    }
    printf("]%s", msg);
}

//--------------------------------------------------------------
int Model::findMClass(const int cpl, const int dgr)
{
    int j;

    for (j = 0; j < ncplgcp; j++) {
        if (cplgcp[j] == cpl && cplglg[j] == dgr) {
            return j;
        }
    }
    return -1;
}

//--------------------------------------------------------------
int Model::normalParticle(int pt)
{
    int ptc = Abs(pt);
    Particle *p;

    if (ptc >= nParticles) {
        return 0;
    }
    p = particles[ptc];
    if (p->isNeutral()) {
        return ptc;
    } else {
        return pt;
    }
}

//--------------------------------------------------------------
int Model::antiParticle(int pt)
{
    int ptc = Abs(pt);
    Particle *p;

    p = particles[ptc];
    if (p->isNeutral()) {
        return ptc;
    } else {
        return -pt;
    }
}

//--------------------------------------------------------------
int *Model::allParticles(int *len)
{
    *len = nallPart;
    return allPart;
}

//--------------------------------------------------------------
int Model::findInteractionName(const char *name)
{
    int j;

    for (j = 0; j < nInteracts; j++) {
        if (strcmp(name, interacts[j]->name) == 0) {
            return j;
        }
    }
    return -1;
}

//--------------------------------------------------------------
int Model::findInteractionCode(int icd)
{
    int j;

    for (j = 0; j < nInteracts; j++) {
        if (icd == interacts[j]->icode) {
            return j;
        }
    }
    return -1;
}

//**************************************************************
// proc.cc

//==============================================================
// class PNodeClass
//--------------------------------------------------------------
PNodeClass::PNodeClass(SProcess *spc, int nnods, int nclss, int *dgs, int *typ, int *ptcl, int *cpl, int *cnt)
{
    //  Create a class of nodes
    //    nnodes : # nodes
    //    nclss  : # classes
    //    dgs  : degree of a node, which is common to the vertices in a class
    //    typ  : initial/final/vertex
    //    ptcl : particle code or list of possible interactions 
    //    cpl  : values of coupling constants.
    //    cnt  : the number of nodes in this class
    // 
    int j, k, nn, lp2;
    Bool ok = True;

    sproc    = spc;
    nnodes   = nnods;
    nclass   = nclss;
    deg      = new int[nclass];
    type     = new int[nclass];
    particle = new int[nclass];
    count    = new int[nclass];
    couple   = new int[nclass];
    for (j = 0; j < nclass; j++) {
        deg[j]      = dgs[j];
        type[j]     = typ[j];
        particle[j] = ptcl[j];
        count[j]    = cnt[j];
        couple[j]   = cpl[j];
        lp2 = (cpl[j]-dgs[j]+2);
        if (!isATExternal(type[j]) && (lp2 % 2 != 0 || lp2 < 0)) {
            if (prlevel > 0) {
                fprintf(GRCC_Stderr, "*** PNodeClass: illegal loop: "
                       ": 2*loop=cpl[%d](%d)-deg[%d](%d)+2 =2*loop = %d\n",
                       j, cpl[j], j, dgs[j], lp2);
                for (k = 0; k < nclss; k++) {
                    fprintf(GRCC_Stderr, "k=%d, dgs=%d, typ=%d, ptcl=%d, ",
                           k, dgs[k], typ[k], ptcl[k]);
                    fprintf(GRCC_Stderr, "cpl=%d, cnt=%d\n", cpl[k], cnt[k]);
                }
            }
            ok = False;
        }
    }
    if (!ok) {
        erEnd("PNodeClass: illegal loop");
    }
    cl2nd   = new int[nclass+1];
    nd2cl   = new int[nnodes];
    cl2mcl  = new int[nnodes];

    nn = 0;
    for (j = 0; j < nclass; j++) {
        cl2nd[j]  = nn;
        for (k = 0; k < count[j]; k++, nn++) {
            nd2cl[nn]  = j;
        }
        if (isATExternal(type[j])) {
            cl2mcl[j] = -1;
        } else {
            cl2mcl[j] = spc->model->findMClass(couple[j], deg[j]);
            if (cl2mcl[j] < 0) {
                if (prlevel > 0) {
                    fprintf(GRCC_Stderr, "*** PNodeClass : no vertex : ");
                    fprintf(GRCC_Stderr, "coupling=%d, degree=%d\n",
                           couple[j], deg[j]);
                }
                erEnd("PNodeClass : no vertex");
            }
        }
    }
    cl2nd[nclass] = nnodes;
}

//--------------------------------------------------------------
PNodeClass::~PNodeClass()
{
    delete[] cl2mcl;
    delete[] nd2cl;
    delete[] cl2nd;
    delete[] couple;
    delete[] count;
    delete[] particle;
    delete[] type;
    delete[] deg;
}

//--------------------------------------------------------------
void PNodeClass::prPNodeClass(void)
{
    int j;

    printf("+++ PNodeClass: nclass=%d, nnodes=%d\n", nclass, nnodes);
    for (j = 0; j < nclass; j++) {
        prElem(j);
    }
    printf("\n");
}

//--------------------------------------------------------------
void PNodeClass::prElem(int j)
{
    int tp;

    printf("%3d: %-7s(%2d), ", j, GRCC_AT_NdStr(type[j]), type[j]);
    printf("deg=%d, count=%d, nodes[%d--%d], couple=%d",
           deg[j], count[j], cl2nd[j], cl2nd[j+1], couple[j]);
    tp = type[j];
    if (isATExternal(tp)) {
        if (sproc->model != NULL) {
            printf(", ptcl=%s ", sproc->model->particleName(particle[j]));
        } else {
            printf(", ptcl=%d ", particle[j]);
        }
    }
    printf("\n");

}

//==============================================================
// class SProcess
//--------------------------------------------------------------
SProcess::SProcess(Model *mdl, Process *prc, Options *opts, int sid, int *clst, int ncls, int *cdeg, int *ctyp, int *ptcl, int *cpl, int *cnum)
{
    // Construct a Subprocess object
    //    mdl         : model
    //    prc         : process (may be NULL)
    //    opts        : options
    //    sid         : id of the sprocess
    //    ncls        : the number of classes
    //    cdeg[ncls]  : the table of degrees of nodes.
    //    ctyp[ncls]  : the table of nodes in the classes
    //    ptcl[ncls]  : particle(External)/interaction code(Internal)
    //    cpl[ncls]   : the table of total order of coupling constants.
    //    cnum[ncls]  : the table of nodes in the classes

    int j, cp, lp2, ndeg, nvrt, tcpl0;
    bool ok;

    id       = sid;
    model    = mdl;
    proc     = prc;
    opt      = opts;
    nclass   = ncls;

    nNodes   = 0;
    nEdges   = 0;
    nExtern  = 0;
    tCouple  = 0;
    loop     = 0;
    mgraph   = NULL;
    egraph   = NULL;
    astack   = NULL;

    mgrcount = 0;
    agrcount = 0;
    extperm  = 1;

    // the results of the graph generation
    nMGraphs = 0;               // the number of generated M-graphs
    nMOPI    = 0;               // the number of 1PI M-graphs
    wMGraphs.setValue(0,1);   // the weighted sum of M-graphs
    wMOPI.setValue(0,1);      // the weighted sum of 1PI M-graphs

    nAGraphs = 0;               // the number of generated A-graphs
    nAOPI    = 0;               // the number of 1PI A-graphs
    wAGraphs.setValue(0,1);   // the weighted sum of A-graphs
    wAOPI.setValue(0,1);      // the weighted sum of 1PI A-graphs

    // check input
    nNodes  = 0;
    nExtern = 0;

    ndeg     = 0;
    nvrt     = 0;
    ok = True;

    tcpl0 = 0;
    for (j = 0; j < model->ncouple; j++) {
        clist[j] = clst[j];
        tcpl0 += clist[j];
    }
    for (j = 0; j < nclass; j++) {
        cp  = cpl[j];
        if (cp > 0) {
            lp2 = cpl[j] - cdeg[j] + 2;
            if (lp2 % 2 != 0 || lp2 < 0) {
                if (prlevel > 0) {
                    fprintf(GRCC_Stderr, "*** SProcess::SProcess: illegal loop: "
                           "class %d, cp=%d, deg=%d, lp2=%d\n",
                           j, cp, cdeg[j], lp2);
                }
                ok = False;
            }
        }
        tCouple +=      cp*cnum[j];
        ndeg    += cdeg[j]*cnum[j];

        if (!isATExternal(ctyp[j])) {
            nvrt += cnum[j];
        } else if (isATExternal(ctyp[j])) {
            if (model->normalParticle(ptcl[j]) == 0) {
                if (prlevel > 0) {
                    fprintf(GRCC_Stderr, "*** SProcess::SProcess: ");
                    fprintf(GRCC_Stderr, "illegal particle code: ");
                    fprintf(GRCC_Stderr, "code: class %d, ptcl=%d\n", j, ptcl[j]);
                }
                ok = False;
            } else {
                nExtern += cnum[j];
            }
            extperm *= factorial(cnum[j]);
  
        } else {
            if (prlevel > 0) {
                fprintf(GRCC_Stderr, "*** SProcess::SProcess: illegal type: ");
                fprintf(GRCC_Stderr, "class %d, type=%d\n", j, ctyp[j]);
            }
            ok = False;
        }
    }
    if (tcpl0 != tCouple) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** SProcess::SProcess: ");
            fprintf(GRCC_Stderr, "illegal coupling constants:");
            fprintf(GRCC_Stderr, " %d != %d\n", tcpl0, tCouple);
        }
        ok = False;
    }
    if (ndeg % 2 == 0) {
        nEdges = ndeg/2;
    } else {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** SProcess::SProcess: ");
            fprintf(GRCC_Stderr, "illegal total deg = %d (not even)\n", ndeg);
        }
        ok = False;
    }
    nNodes = nvrt + nExtern;
    if (nNodes >= GRCC_MAXNODES) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** SProcess::SProcess: ");
            fprintf(GRCC_Stderr, "too many nodes = %d\n", nNodes);
            fprintf(GRCC_Stderr, "    nExtern=%d, nvert=%d (GRCC_MAXNODES)\n",
                    nExtern, nvert);
        }
        ok = False;
    }
    if (!ok) {
        prSProcess();
        erEnd("SProcess::SProcess: illegal input");
    }
    if (proc == NULL) {
        opt->beginProc(NULL);
    }

    lp2 = tCouple - nExtern + 2;
    if (lp2 % 2 != 0 || lp2 < 0) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** SProcess::SProcess: illegal loop: "
                   "tCouple=%d, nExtern=%d, 2*loop=%d\n",
                   tCouple, nExtern, lp2);
        }
        ok = False;
    }
    loop = lp2/2;

    // stack for assignment
    if (opt->values[GRCC_OPT_Step] == GRCC_AGraph) {
        astack = new AStack(GRCC_MAXNSTACK, GRCC_MAXESTACK);
    }

    // save to PNodeClass object
    pnclass = new PNodeClass(this, nNodes, nclass, cdeg, ctyp, ptcl, cpl, cnum);
}

//--------------------------------------------------------------
SProcess::~SProcess()
{
    delete pnclass;
    delete astack;
    delete mgraph;
}

//--------------------------------------------------------------
void SProcess::prSProcess(void)
{
    printf("\n");
    printf("+++ Subprocess %d, class=%d\n", id, nclass);
    pnclass->prPNodeClass();
}

//--------------------------------------------------------------
BigInt SProcess::generate(void)
{
    //  Entry point of Feynman graph generation
    int  cldeg[GRCC_MAXNODES], clnum[GRCC_MAXNODES], cltyp[GRCC_MAXNODES];
    int  ncl;
    BigInt ng;

    ncl = toMNodeClass(cltyp, cldeg, clnum);

    opt->beginSubProc(this);

    mgraph = new MGraph(id, ncl, cldeg, clnum, cltyp, opt);

    ng = mgraph->generate();

    opt->endSubProc();

    delete mgraph;
    mgraph = NULL;

    return ng;
}

//--------------------------------------------------------------
int SProcess::toMNodeClass(int *cltyp, int *cldeg, int *clnum)
{
    int j;

    for (j = 0; j < nclass; j++) {
        if (isATExternal(pnclass->type[j])) {
            cltyp[j] = -1;
        } else {
            cltyp[j] = (pnclass->couple[j]-pnclass->deg[j]+2)/2;
        }
        cldeg[j] = pnclass->deg[j];
        clnum[j] = pnclass->count[j];
    }
    return nclass;
}

//--------------------------------------------------------------
void SProcess::assign(MGraph *mgr)
{
    PNodeClass *pnc;

    pnc = match(mgr);

    agraph = new Assign(this, mgr, pnc);

#ifdef CHECK
    agraph->checkAG("SProcess::assign");
#endif
    delete pnc;
    delete agraph;
    agraph = NULL;
}

//--------------------------------------------------------------
PNodeClass *SProcess::match(MGraph *mgr)
{
    PNodeClass *pnc = NULL;

    int typ[GRCC_MAXNODES], dgs[GRCC_MAXNODES];
    int cnt[GRCC_MAXNODES], ptcl[GRCC_MAXNODES];
    int cpl[GRCC_MAXNODES];
    int n2m[GRCC_MAXNODES];
    int j, k, l, nd, mc, mcl, mclss;

    if (mgr->nNodes != nNodes) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** SProcess::match: different nNodes=%d != %d\n",
                   nNodes, mgr->nNodes);
        }
        erEnd("SProcess::match: different nNodes");
    }

    mcl = toMNodeClass(typ, dgs, cnt);

    nd = 0;
    for (j = 0; j < mcl; j++) {
        for (k = 0; k < cnt[j]; k++, nd++) {
            n2m[nd] = j;
        }
    }

    nd    = 0;
    mclss = mgr->curcl->nClasses;
    for (j = 0; j < mclss; j++) {
        for (k = 0; k < mgr->curcl->clist[j]; k++, nd++) {
            mc = mgr->nodes[nd]->clss;
            if (mc != n2m[nd]) {
                if (prlevel > 0) {
                    fprintf(GRCC_Stderr, "*** SProcess::match: ");
                    fprintf(GRCC_Stderr, "inconsistent class\n");
                    for (l = 0; l < nNodes; l++) {
                        fprintf(GRCC_Stderr, "    %d: %d, %d\n", 
                                j, mgr->nodes[l]->clss, n2m[l]);
                    }
                }
                erEnd("SProcess::match: inconsistent class");
            }
        }
    }
    for (j = 0; j < mcl; j++) {
        dgs[j]  = pnclass->deg[j];
        typ[j]  = pnclass->type[j];
        ptcl[j] = pnclass->particle[j];
        cnt[j]  = pnclass->count[j];
        cpl[j]  = pnclass->couple[j];
    }
    pnc = new PNodeClass(this, nNodes, mcl, dgs, typ, ptcl, cpl, cnt);

    return pnc;
}

//--------------------------------------------------------------
void SProcess::resultMGraph(BigInt nmgraphs, Fraction mwsum, BigInt nmopi, Fraction mwopi)
{
    nMGraphs += nmgraphs;
    nMOPI    += nmopi;
    wMGraphs.add(mwsum);
    wMOPI.add(mwopi);
}

//--------------------------------------------------------------
void SProcess::resultAGraph(BigInt nmgraphs, Fraction mwsum, BigInt nmopi, Fraction mwopi)
{
    nAGraphs += nmgraphs;
    nAOPI    += nmopi;
    wAGraphs.add(mwsum);
    wAOPI.add(mwopi);
}

//--------------------------------------------------------------
void SProcess::endMGraph(MGraph *mgr)
{
    egraph = mgr->egraph;

    if (opt->values[GRCC_OPT_Step] != GRCC_MGraph) {
        assign(mgr);
    }
}

//--------------------------------------------------------------
void SProcess::endAGraph(EGraph *egr)
{
    egraph = egr;
}


//==============================================================
// class Process
//--------------------------------------------------------------
Process::Process(int pid, Model *modl, Options *optn, int nini, int *intlPrt, int nfin, int *finlPrt, int *coupling)
{
    //  Define a process and construct a set of sprocesses
    //    model     : Model object
    //    opt       : Option object
    //    initlPart : list of particle-id of initlPart particles
    //    finalPart : list of particle-id of final particles
    //    coupling  : list of coupling constants

    int j, lp2;
    Bool ok;

    id        = pid;
    model     = modl;
    opt       = optn;
    ninitl    = nini;
    nfinal    = nfin;
    initlPart = intdup(ninitl, intlPrt);
    finalPart = intdup(nfinal, finlPrt);

    // count total number of coupling constants.
    ctotal = 0;
    for (j = 0; j < GRCC_MAXNCPLG; j++) {
        if (j < model->ncouple) {
            clist[j] = coupling[j];
            ctotal  += coupling[j];
        } else {
            clist[j] = 0;
        }
    }

    nExtern   =  0;
    loop      = -1;
    maxnlegs  =  0;
    mgrcount = 0;
    agrcount = 0;

    // table of sprocesses 
    nSubproc  = 0;
    sproc   = NULL;

    // the results of the graph generation
    nMGraphs = 0;                 // the number of generated M-graphs
    nMOPI    = 0;                 // the number of 1PI M-graphs
    wMGraphs.setValue(0,1);       // the weighted sum of M-graphs
    wMOPI.setValue(0,1);          // the weighted sum of 1PI M-graphs

    nAGraphs = 0;                 // the number of generated A-graphs
    nAOPI    = 0;                 // the number of 1PI A-graphs
    wAGraphs.setValue(0,1);       // the weighted sum of A-graphs
    wAOPI.setValue(0,1);          // the weighted sum of 1PI A-graphs

    ok = True;

    // numbers
    nExtern  = ninitl + nfinal;
    maxnlegs = Max(nExtern, model->maxnlegs);
    
    // count loops
    lp2 = ctotal - nExtern + 2;
    if (lp2 % 2 != 0) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** cannot generate : 2*loop is odd : " 
                    "2*loop = %d, ctotal=%d, nExtern=%d\n",
                    lp2, ctotal, nExtern);
        }
        ok = False;
    } else if (lp2 < 0) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** cannot make a connected graph : "
                    "2*loop=%d, ctotal=%d, nExtern=%d\n",
                    lp2, ctotal, nExtern);
        }
        ok = False;
    }

    loop = lp2 / 2;

    if (!ok) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** Process: illegal input: lp2 = %d\n", lp2);
        }
        erEnd("Process: illegal input");
    }

#ifdef DEBUG
    // print message
    prProcess();
#endif

    // construct sprocesses
    mkSProcess();
}

//--------------------------------------------------------------
Process::~Process()
{
    initlPart = delintdup(initlPart);
    finalPart = delintdup(finalPart);
    delete sproc;
}

//--------------------------------------------------------------
void Process::prProcess(void)
{
    printf("+++ process options : OPI = %d, (Step) = %d, coupling=%d: ",
           opt->values[GRCC_OPT_1PI], opt->values[GRCC_OPT_Step], ctotal);
    prIntArray(model->ncouple, clist, "\n");
}

//--------------------------------------------------------------
void Process::mkSProcess(void)
{
    //  Construct sprocesses
    //  Each sprocess is a set of nodes whose degree and oder of 
    //  coupling constants are determined.
    //  Only the total coupling constants are considered here even if
    //  two or more coupling constants are defined in the model

    int r, j, k, lin2, nvtx, nleg, nc, n0;
    int nl[GRCC_MAXMINTERACT];
    int cdeg[GRCC_MAXMINTERACT], ctyp[GRCC_MAXMINTERACT];
    int ptcl[GRCC_MAXMINTERACT], cnum[GRCC_MAXMINTERACT];
    int cple[GRCC_MAXMINTERACT];
    double proct0 = 0, proct1 = 0;

    // output file to start process
    opt->beginProc(this);

    // starting time
    Second(&proct0);

    // generate all possible number of (nl[i]), 
    //   \sum_i nl[i]*cplgcp[i] = (total number of coupling constants)

    r = -1;
    nSubproc = 0;
    ngraphs  = 0;
    nopi     = 0;
    wgraphs  = 0;
    wopi     = 0;

    // for partitions of (leg, order)
    
    while (nextPart(ctotal, model->ncplgcp, model->cplgcp, nl, &r)) {
#ifdef DEBUG
        printf("nextPart:ctotal=%d, nc=%d, c=", ctotal, model->ncplgcp);
        prIntArray(model->ncplgcp, model->cplgcp, ", nl=");
        prIntArray(model->ncplgcp, nl, "");
        printf(", r=%d\n", r);
#endif
        // count the total number of vertices and legs.
        nvtx = 0;
        nleg = 0;
        for (j = 0; j < model->ncplgcp; j++) {
            nvtx += nl[j];
            nleg += nl[j]*model->cplglg[j];
        }

        // count loops
        lin2 = nExtern + nleg;
        if (lin2 % 2 != 0) {
            continue;
        }
        loop = lin2/2 - nExtern - nvtx + 1;
        if (loop < 0) {
            continue;
        }

        nc = 0;
        if (opt->values[GRCC_OPT_SymmInitial]) {
            n0 = nc;
            for (j = 0; j < ninitl; j++) {
                for (k = n0; k < nc; k++) {
                    if (ptcl[k] == initlPart[j]) {
                        cnum[k]++;
                        break;
                    }
                }
                if (k >= nc) {
                    cdeg[nc] = 1;
                    ctyp[nc] = GRCC_AT_Initial;
                    ptcl[nc] = initlPart[j];
                    cple[nc] = 0;
                    cnum[nc] = 1;
                    nc++;
                }
            }
        } else {
            for (j = 0; j < ninitl; j++) {
                cdeg[nc] = 1;
                ctyp[nc] = GRCC_AT_Initial;
                ptcl[nc] = initlPart[j];
                cple[nc] = 0;
                cnum[nc] = 1;
                nc++;
            }
        }
        if (opt->values[GRCC_OPT_SymmFinal]) {
            n0 = nc;
            for (j = 0; j < nfinal; j++) {
                for (k = n0; k < nc; k++) {
                    if (ptcl[k] == model->antiParticle(finalPart[j])) {
                        cnum[k]++;
                        break;
                    }
                }
                if (k >= nc) {
                    cdeg[nc] = 1;
                    ctyp[nc] = GRCC_AT_Final;
                    ptcl[nc] = model->antiParticle(finalPart[j]);
                    cple[nc] = 0;
                    cnum[nc] = 1;
                    nc++;
                }
            }
        } else {
            for (j = 0; j < nfinal; j++) {
                cdeg[nc] = 1;
                ctyp[nc] = GRCC_AT_Final;
                ptcl[nc] = model->antiParticle(finalPart[j]);
                cple[nc] = 0;
                cnum[nc] = 1;
                nc++;
            }
        }
        for (j = 0; j < model->ncplgcp; j++) {
            if (nl[j] > 0) {
                cdeg[nc] = model->cplglg[j];
                // number of loops
                ctyp[nc] = (model->cplgcp[j] - model->cplglg[j] + 2)/2;
                ptcl[nc] = 0;
                cple[nc] = model->cplgcp[j];
                cnum[nc] = nl[j];
                nc++;
            }
        }
#if 0
        for (j = 0; j < nc; j++) {
            printf("%d/%d: deg=%d, typ=%d, ptcl=%d, cple=%d, num=%d\n",
                   j, nc, cdeg[j], ctyp[j], ptcl[j], cple[j], cnum[j]);
        }
#endif
        // create a sprocess ??? to be rewritten
        sproc = new SProcess(model, this, opt, nSubproc, clist, nc,
                             cdeg, ctyp, ptcl, cple, cnum);

  
      // list of sprocesses
        if (nSubproc >= GRCC_MAXSUBPROCS) {
              erEnd("Subclass: too many sprocesses (GRCC_MAXSUBPROCS)");
        }
        sptbl[nSubproc++] = sproc;

#ifdef DEBUG
        // print sprocesses
        sproc->prSProcess();
#endif
#if 0
sproc->prSProcess();
#endif

        // generate M-graphs
        sproc->generate();

        // summation of the numbers and weighted numbers of graphs
        nMGraphs += sproc->nMGraphs;
        nMOPI    += sproc->nMOPI;
        wMGraphs.add(sproc->wMGraphs);
        wMOPI.add(sproc->wMOPI);

        nAGraphs += sproc->nAGraphs;
        nAOPI    += sproc->nAOPI;
        wAGraphs.add(sproc->wAGraphs);
        wAOPI.add(sproc->wAOPI);

        if (nAGraphs > 0) {
            ngraphs  = nAGraphs;
        } else {
            ngraphs  = nAGraphs;
        }
        delete sproc;
        sproc = NULL;
    }

    // ending time
    Second(&proct1);
    sec = proct1-proct0;

    // output file to finish process
    opt->endProc();
}

//**************************************************************
// mgraph.cc
//==============================================================
//--------------------------------------------------------------
MNodeClass::MNodeClass(int nnodes, int nclasses)
{
    // Input
    //   nnodes   : possible maximal number of nodes
    //   nclasses : possible maximal number of classes

    int j, k;

    nNodes   = nnodes;
    nClasses = nclasses;
    for (j = 0; j < nNodes; j++) {
        for (k = 0; k < nNodes; k++) {
            clmat[j][k] = 0;
        }
    }
}

//--------------------------------------------------------------
MNodeClass::~MNodeClass()
{
}

//==============================================================
// class MNodeClass
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

    nClasses = mnc->nClasses;
    for (k = 0; k < nClasses; k++) {
        clist[k] = mnc->clist[k];
    }
    maxdeg   = mnc->maxdeg;
    for (j = 0; j < nNodes; j++) {
        ndcl[j]  = mnc->ndcl[j];
        clord[j] = mnc->clord[j];
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
    cmp = - cmpMNCArray(clmat[nd0], clmat[nd1], cn);
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
    printf(" clord="); prIntArray(nClasses, clord, "");
    printf(" flist="); prIntArray(nClasses, flist, "\n");
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

//--------------------------------------------------------------
int MNodeClass::cmpMNCArray(int *a0, int *a1, int ma)
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

//--------------------------------------------------------------
void MNodeClass::reorder(MGraph *mg)
{
    int flg[GRCC_MAXNODES];
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
//  class MGraph : scalar graph expressed by matrix form
//---------------------------------------------------------------
MGraph::MGraph(int pid, int ncl, int *cldeg, int *clnum, int *cltyp, Options *opts)
{
    int nn, ne, j, k;

    // initial conditions
    nClasses = ncl;
    clist    = new int[ncl];
    opt      = opts;
    mId      = -1;

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
            mindeg = Min(mindeg, cldeg[j]);
        }
        if (maxdeg < 0) {
            maxdeg = cldeg[j];
        } else {
            maxdeg = Max(maxdeg, cldeg[j]);
        }
    }
    if (ne % 2 != 0) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "Sum of degrees are not even\n");
            for (j = 0; j < nClasses; j++) {
                fprintf(GRCC_Stderr, "class %2d: %2d %2d %2d\n", 
                       j, cldeg[j], clnum[j], cltyp[j]);
            }
        }
        erEnd("illegal degrees of nodes");
    }
    pId    = pid;
    nNodes = nn;
    nodes  = new MNode*[nNodes];
    group  = new SGroup();
#ifdef ORBITS
    orbits = new MOrbits();
#endif

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
    wscon     = Fraction(0, 1);
    wsopi     = Fraction(0, 1);

    // current node classification
    curcl    = new MNodeClass(nNodes, nClasses);

    // measures of efficiency
    ngen     = 0;
    ngconn   = 0;

#ifdef MONITOR
    nCallRefine    = 0;
    discardRefine  = 0;
    discardDisc    = 0;
    discardIso     = 0;
#endif

    // work space for isIsomorphic
    modmat = newMat(nNodes, nNodes, 0);

    // work space for bisearchM
    bidef = newArray(nNodes, 0);
    bilow = newArray(nNodes, 0);
    bicount = 0;

}

//---------------------------------------------------------------
MGraph::~MGraph()
{
    int j;

    // group->delGroup();

    bilow = deleteArray(bilow);
    bidef = deleteArray(bidef);


    modmat = deleteMat(modmat, nNodes);
    delete curcl;
    adjMat = deleteMat(adjMat, nNodes);
#ifdef ORBITS
    delete orbits;
#endif
    delete egraph;
    delete group;
    for (j = 0; j < nNodes; j++) {
        delete nodes[j];
        nodes[j] = NULL;
    }
    delete[] nodes;
    delete[] clist;

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
void MGraph::print(void)
{
    int j;

    printf("MGraph: pId=%d, cDiag=%ld, c1PI=%ld\n", pId, cDiag, c1PI);
    printf("     nNodes=%d, nEdges=%d, nExtern=%d, nLoops=%d "
           "mindeg=%d, maxdeg=%d, sym=(%ld, %ld)\n",
           nNodes, nEdges, nExtern, nLoops, mindeg, maxdeg, nsym, esym);
    printf("  Nodes=%d\n", nNodes);
    for (j = 0; j < nNodes; j++) {
        printf("    %2d: id=%d, deg=%d, clss=%d, extloop=%d, freelg=%d\n",
               j, nodes[j]->id, nodes[j]->deg, nodes[j]->clss, nodes[j]->extloop, nodes[j]->freelg);
    }
        
    curcl->printMat();
    printf("\n");
 
    printAdjMat(curcl);
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

    int j1, j2, cmp, nself;
    int *perm;

    nsym = ToBigInt(0);
    esym = ToBigInt(1);

    group->newGroup(nNodes, cl->nClasses, cl->clist);

#ifdef ORBITS
    orbits->initPerm(nNodes);
#endif

    while (True) {
        perm = group->genNext();

        if (perm == NULL) {
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
  
        permMat(nNodes, perm, adjMat, modmat);
        cmp = compMat(nNodes, adjMat, modmat);
        if (cmp < 0) {
            return False;
        } else if (cmp == 0) {
            // if ngroup < 0, symmetry group is $S_{-group}$.
            group->addGroup(perm);
  
            nsym = nsym + ToBigInt(1);
#ifdef ORBITS
            orbits->fromPerm(perm);
#endif
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
    int         ucl[GRCC_MAXNODES];
    int         ccn = cl->nClasses;
    int         nucl, nce;
    int         td, cmp;
    int         nccl;

#ifdef MONITOR
    nCallRefine++;
#endif
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
    
#ifdef CHECK
        // inconsistent
        if (nucl < ccl->nClasses) {
            erEnd("refineClasses : smaller number of classes");
        }
#endif
  
        // preparation of the next repetition.
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
#ifdef CHECK
        if (pd < 0) {
            erEnd("bisearchM : node is external");
        }
#endif
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
                bilow[nd] = Min(bilow[nd], bidef[pd]);
                *loop += adjMat[nd][pd] - 1;
            }
            continue;
  
        // back edge
        } else if (bidef[td] >= 0) {
            bilow[nd] = Min(bilow[nd], bidef[td]);
            // changed !!!
            // *loop += adjMat[nd][pd];
            *loop += adjMat[nd][td];
  
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
            bilow[nd] = Min(bilow[nd], bilow[td]);
            (*next) += next1;
            (*loop) += loop1;
        }
    }
}

//---------------------------------------------------------------
BigInt MGraph::generate(void)
{
    //  Generate graphs
    //    the generation process starts from 'connectClass'.

    MNodeClass *cl;

    // Initial classification of nodes.
    cl = new MNodeClass(nNodes, nClasses);
    cl->init(clist, maxdeg, adjMat);
    cl->reorder(this);
    connectClass(cl);

    delete cl;

    return cDiag;
}

//---------------------------------------------------------------
void MGraph::connectClass(MNodeClass *cl)
{
    // Connect nodes in a class to others

    int sc, sn;
    MNodeClass *xcl;

    xcl = refineClass(cl);

    if (xcl == NULL) {
#ifdef MONITOR
        discardRefine++;
#endif
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
}

//------------------------------------------------------------
void MGraph::connectNode(int so, int ss, MNodeClass *cl)
{
    int sc, sn;

    sc = cl->clord[so];
    if (ss >= cl->flist[sc+1]) {
        connectClass(cl);
        return;
    }

    for (sn = ss; sn < cl->flist[sc+1]; sn++) {
        connectLeg(so, sn, so, sn, cl);
        return;
    }
}

//------------------------------------------------------------
void MGraph::connectLeg(int so, int sn, int to, int ts, MNodeClass *cl)
{
    //  Add one connection between two legs.
    //    1. select another node to connect
    //    2. determine multiplicity of the connection

    int sc, tc, tn, maxself, nc2, nc, maxcon, ts1, ncm, to1;

    sc = cl->clord[so];

#ifdef CHECK
    if (sn >= cl->flist[sc+1]) {
        erEnd("connectLeg : illegal control");
        return;
    }
#endif

    // There remains no free legs in the node 'sn' : move to next node.
    if (nodes[sn]->freelg < 1) {

        if (!isConnected()) {
#ifdef MONITOR
            discardDisc++;
#endif
        } else {
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
            if (sc == tc && sn > tn) {
                continue;
    
            } else if (nodes[tn]->freelg < 1) {
                continue;
    
            // self-loop
            } else if (sn == tn) {
                if (nNodes > 1) {
                    // there are two or more nodes in the graph : 
                    // avoid disconnected graph
                    maxself = Min((nodes[sn]->freelg)/2, (nodes[sn]->deg-1)/2);
                } else {
                    // there is only one node the graph.
                    maxself = nodes[sn]->freelg/2;
                }
      
                // If we can assume no tadpole, the following line can be used.
                if (opt->values[GRCC_OPT_1PI] and nNodes > 2) {
                     maxself = Min((nodes[sn]->deg-2)/2, maxself);
                }
      
                // for the number of connection.
                for (nc2 = maxself; nc2 > 0; nc2--) {
                    nc = 2*nc2;
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
                }
    
            // connections between different nodes.
            } else {
                // maximum possible connection number
                maxcon = Min(nodes[sn]->freelg, nodes[tn]->freelg);
      
                // avoid disconnected graphs.
                if (nNodes > 2 && nodes[sn]->deg == nodes[tn]->deg) {
                    maxcon = Min(maxcon, nodes[sn]->deg-1);
                }
      
#ifdef CHECK
                if ((adjMat[sn][tn] != 0) || 
                    (adjMat[sn][tn] != adjMat[tn][sn])) {
                    printf("*** inconsistent connection: sn=%d, tn=%d",
                           sn, tn);
                    printAdjMat(cl);
                    erEnd("inconsistent connection ");
                }
#endif
      
                // vary number of connections
                for (nc = maxcon; nc > 0; nc--) {
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
                }
            }
        } 
    }
}

//------------------------------------------------------------
Bool MGraph::isOptM(void)
{
    Bool ok = True;

    if (opt->values[GRCC_OPT_1PI] > 0) {
        ok = ok && opi;
    } else if (opt->values[GRCC_OPT_1PI] < 0) {
        ok = ok && !opi;
    }
    if (opt->values[GRCC_OPT_NoTadpole] > 0) {
        ok = ok && !tadpole;
    } else if (opt->values[GRCC_OPT_NoTadpole] < 0) {
        ok = ok && tadpole;
    }
    if (opt->values[GRCC_OPT_No1PtBlock] > 0) {
        ok = ok && !tadBlock;
    } else if (opt->values[GRCC_OPT_No1PtBlock] < 0) {
        ok = ok && tadBlock;
    }
    if (opt->values[GRCC_OPT_Block] > 0) {
        ok = ok && (nBlocks == nExtern+1);
    } else if (opt->values[GRCC_OPT_Block] < 0) {
        ok = ok && (nBlocks != nExtern+1);
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

    // refine class and check ordering condition
    xcl = refineClass(cl);
    if (xcl == NULL) {
#ifdef MONITOR
        discardRefine++;
#endif

    // check whether this is a connected graph or not.
    } else {
        connected = isConnected();
        if (!connected) {
#ifdef MONITOR
            discardDisc++;
#endif
  
        // connected graph : check isomorphism of the graph
        } else {
            ngconn++;
            if (!isIsomorphic(xcl)) {
#ifdef MONITOR
                discardIso++;
#endif
    
            // We got a new connected and a unique representation of a class.
            } else {
                biconnM();
                if (isOptM()) {
                    egraph->fromMGraph(this);
                    if (egraph->isOptE()) {
                        curcl->copy(xcl);
#ifdef ORBITS
                        orbits->toOrbits();
#endif

                        // # generated graphs
                        cDiag++;
                        egraph->mId = cDiag;
                        wscon.add(1, nsym*esym);
      
                        // # generated 1PI graphs
                        if (opi) {
                            wsopi.add(1, nsym*esym);
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
              
#ifdef MONITOR
                        printf("\n");
                        printf("Graph : %ld (%ld) 1PI com. = %d", 
                               cDiag, ngen, n1PIComps);
                        printf(" sym. factor = (%ld*%ld)\n", nsym, esym);
                        printAdjMat(cl);
                        // cl->printMat();
                        orbits->print();
#  ifdef DEBUG1
                        printf("refine:                     %ld\n", 
                               nCallRefine);
                        printf("discarded for refinement:   %ld\n", 
                               discardRefine);
                        printf("discarded for disconnected: %ld\n",
                               discardDisc);
                        printf("discarded for duplication:  %ld\n",
                               discardIso);
#  endif
#endif
                        // go to next step
                        opt->newMGraph(this);
                    }
                }
            }
        }
    }
    if (xcl != cl && xcl != NULL) {
      delete xcl;
    }
}

//==============================================================
// class MOrbits: orbits of nodes
//--------------------------------------------------------------
MOrbits::MOrbits()
{
    nOrbits = 0;
    nNodes  = 0;
}

//--------------------------------------------------------------
MOrbits::~MOrbits()
{
}

//--------------------------------------------------------------
void MOrbits::print(void)
{
    int c;

    printf("Orbits : nOrbits=%d: nd2or=", nOrbits);
    prIntArray(nNodes, nd2or, "\n");
    for (c = 0; c < nOrbits; c++) {
        printf("    %2d: (%2d -- %2d) :", c, flist[c], flist[c+1]-1);
        prIntArray(flist[c+1]-flist[c], or2nd+flist[c], "\n");
    }
}

//---------------------------------------------------------------
void MOrbits::initPerm(int nnodes)
{
    int j;

    nNodes = nnodes;
    for (j = 0; j < nNodes; j++) {
        nd2or[j] = j;
    }
}

//---------------------------------------------------------------
void MOrbits::fromPerm(int *perm)
{
    // update the orbits of nodes by the permutation

    int j, j1, k, l;

    for (j = 0; j < nNodes; j++) {
        if (nd2or[j] != j) {
            // not the first element of an old orbit
            continue;
        }
        // merge orbits to orbit 'j'
        k = j;
        for (j1 = 0; j1 < nNodes && (k = perm[k]) != j; j1++) {
            if (nd2or[k] == j) {
                // 'k' is already in 'j'
                ;
            } else {
                // old orbit 'k' had several elements: merge to orbit 'j'
                for (l = 0; l < nNodes; l++) {
                    if (nd2or[l] == k) {
                        nd2or[l] = j;
                    }
                }
            }
        }     
#ifdef CHECK
        if (j1 >= nNodes) {
            printf("*** fromPerm: illegal control: j=%d, k=%d\n", j, k);
            printf("perm=");
            prIntArray(nNodes, perm,  " nd2or=");
            prIntArray(nNodes, nd2or, "\n");
            erEnd("fromPerm: illegal control");
        }
#endif
    }
}

//--------------------------------------------------------------
void MOrbits::toOrbits(void)
{
    // fill elements from 'nd2or'
    int nd, nel, k, lor;

    lor = -1;
    nel = 0;
    nOrbits = 0;
    for (nd = 0; nd < nNodes; nd++) {
        if (nd2or[nd] > lor) {
            // lowest element of a new orbit
            lor = nd2or[nd];
            flist[nOrbits++] = nel;
            for (k = nd; k < nNodes; k++) {
                if (nd2or[k] == lor) {
                    or2nd[nel++] = k;
                }
            }
        }
    }
    flist[nOrbits] = nNodes;
}

//**************************************************************
// sgroup.cc
//==============================================================
// compile options
//============================================================
// table of group elements
//------------------------------------------------------------
SGroup::SGroup(void)
{
    // should be called before graph generation
    nnodes = -1;
    nelem  = 0;
    elem   = NULL;
}

//------------------------------------------------------------
SGroup::~SGroup(void)
{
    int j;

    if (elem != NULL) {
        for (j = GRCC_MAXGROUP-1; j >= 0; j--) {
            if (elem[j] != NULL) {
                delete[] elem[j];
                elem[j] = NULL;
            }
        }
        delete[] elem;
        elem = NULL;
    }
}

//------------------------------------------------------------
void SGroup::print(void)
{
    int j;

    printf("SGroup : nnodes = %d, nelem=%ld, cgen=%d, csav=%d\n",
           nnodes, nelem, cgen, csav);
    printf("  eclass =");
    prIntArray(neclass, eclass, "\n");
    for (j = 0; j < nelem; j++) {
        printf("    %4d: ", j);
        prIntArray(nnodes, elem[j], "\n");
    }
}

//------------------------------------------------------------
void SGroup::newGroup(int nnd, int nclss, int *clss)
{
    int j;

    clearGroup();
    nnodes  = nnd;
    neclass = nclss;

    if (neclass > GRCC_MAXNODES) {
        erEnd("newGroup : too many classes (GRCC_MAXNODES)");
    }
    for (j = 0; j < neclass; j++) {
        eclass[j] = clss[j];
    }

    nelem = 0;
    if (elem == NULL) {
        elem  = new int*[GRCC_MAXGROUP];
        for (j = 0; j < GRCC_MAXGROUP; j++) {
            elem[j] = NULL;
        }
    }
}

//------------------------------------------------------------
void SGroup::clearGroup(void)
{
    nelem  = 0;
    csav   = -1;
    cgen   = -1;
}

//------------------------------------------------------------
void SGroup::delGroup(void)
{
    int j;

    for (j = 0; j < GRCC_MAXGROUP; j++) {
        if (elem[j] != NULL) {
            delete[] elem[j];
            elem[j] = NULL;
        }
    }
    delete[] elem;
    elem = NULL;
    clearGroup();
}

//------------------------------------------------------------
int *SGroup::genNext(void)
{
    cgen = nextPerm(nnodes, neclass, eclass, pgr, pgq, permg, cgen);
    if (cgen < 0) {
        return NULL;
    }
    return permg;
}

//------------------------------------------------------------
void SGroup::addGroup(int *p)
{
    int j;

    if (nelem >= GRCC_MAXGROUP) {
        erEnd("addGroup : too many elements (GRCC_MAXGROUP)");
    }
    if (elem[nelem] == NULL) {
        elem[nelem] = new int[GRCC_MAXNODES];
    }
    for (j = 0; j < nnodes; j++) {
        elem[nelem][j] = p[j];
    }
    nelem++;
}

//------------------------------------------------------------
BigInt SGroup::nElem(void)
{
    return nelem;
}

//------------------------------------------------------------
int *SGroup::nextElem(void)
{
    if (nelem < 0) {
        cgen = nextPerm(nelem, neclass, eclass,
                     psr, psq, perms, cgen);
        return perms;
    }
    if (cgen < 0) {
        cgen = 0;
    }
    if (cgen >= nelem) {
        return NULL;
    } else {
        return elem[cgen++];
    }
}

//**************************************************************
// egraph.cc
// Generate scalar connected Feynman graphs.
//==============================================================
static const char *GRCC_ND_names[] = GRCC_ND_NAMES;
static const char *GRCC_ED_names[] = GRCC_ED_NAMES;

//==============================================================
// class ENode
//--------------------------------------------------------------
ENode::ENode()
{ 
    id      = -1;
    egraph  = NULL;
    edges   = NULL;
    maxdeg  = 0;
    deg     = 0;

    klow    = NULL;
    extloop = 0;
    ndtype  = GRCC_ND_Undef;

    intrct  = -1;
}

//--------------------------------------------------------------
ENode::ENode(EGraph *egrph, int loops, int sdeg)

{ 
    id      = -1;
    egraph  = egrph;
    edges   = NULL;
    maxdeg  = sdeg;
    deg     = 0;

    klow    = NULL;
    extloop = 0;
    ndtype  = GRCC_ND_Undef;

    intrct  = -1;

    edges = new int[sdeg]; 
    klow  = new int[loops+1]; 
}

//--------------------------------------------------------------
ENode::~ENode()
{ 
    if (klow != NULL) {
        delete[] klow; 
        delete[] edges;
        klow  = NULL;
        edges = NULL;;
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
void ENode::initAss(EGraph *egrph, int nid, int sdg)
{
    id      = nid;
    egraph  = egrph;
    maxdeg  = sdg;
}

//--------------------------------------------------------------
void ENode::setId(EGraph *egrph, const int nid)
{ 
    id      = nid;
    egraph  = egrph;
}

//--------------------------------------------------------------
void ENode::setExtern(int typ, int pt)
{
    ndtype = typ;
    intrct = pt;
}

//--------------------------------------------------------------
void ENode::setType(int typ)
{ 
#ifdef CHECK
    if (ndtype != GRCC_ND_Undef && ndtype != typ) { 
        fprintf(GRCC_Stderr, "*** ndtype is already defined : old=%d, new = %d\n",
               ndtype, typ);
        erEnd("ndtype is already defined");
    }
#endif
    ndtype = typ;
}

//--------------------------------------------------------------
void ENode::print(void)
{
    int j;

    printf("Enode %d deg=%d/%d, intr=%d ",
           id, deg, maxdeg, intrct);
    if (egraph->bicount > 0) {
        printf("(%-8s) ", GRCC_ND_names[ndtype]);
    }
    printf("edge=[");
    for (j = 0; j < deg; j++) {
        printf(" %d", edges[j]);
    }
    printf("]\n");
}

//==============================================================
// class EEdge
//--------------------------------------------------------------
EEdge::EEdge()
{
    id       = -1;
    egraph   = NULL;
    nodes[0] = -1;
    nodes[1] = -1;
    // nlegs[0] = -1;
    // nlegs[1] = -1;
    ptcl     = GRCC_PT_Undef;
    deleted  = False;

    edtype   = GRCC_ED_Undef;

    emom = NULL;
    lmom = NULL;
}

//--------------------------------------------------------------
EEdge::EEdge(EGraph *egrph, int nedges, int nloops)
{
    id       = -1;
    egraph   = egrph;
    nodes[0] = -1;
    nodes[1] = -1;
    nlegs[0] = -1;
    nlegs[1] = -1;
    ptcl     = GRCC_PT_Undef;
    deleted  = False;

    edtype   = GRCC_ED_Undef;

    emom = new int[nedges+1];
    lmom = new int[nloops];
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
void EEdge::setId(EGraph *egrph, const int eid)
{
    id      = eid;
    egraph  = egrph;
}

//--------------------------------------------------------------
void EEdge::setType(int typ)
{ 
#ifdef CHECK
    if (edtype != GRCC_ED_Undef) {
        erEnd("edtype is already defined");
    }
#endif
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

//--------------------------------------------------------------
void EEdge::print(void)
{
    int zero, n, k;

    printf("Edge %d ext=%d ptcl=%2d ", id, ext, ptcl);
    if (egraph == NULL || !egraph->assigned) {
        printf("[%d, %d]", nodes[0], nodes[1]);
    } else {
        printf("[(%d,%d), (%d,%d)]", nodes[0], nlegs[0], nodes[1], nlegs[0]);
    }

    if (egraph != NULL && egraph->bicount > 0) {
        printf("    %-9s ", GRCC_ED_names[edtype]);
        printf("c%2d: ", opicomp);
        printf("%s%d =", (ext)?"Q":"p", momn);
        zero = True;
        for (n = 1; n <= egraph->nEdges; n++) {
            if (ext) {
                if (emom[n] != 0) {
                    prMomStr(emom[n], "Q", n);
                    zero = False;
                }
            }
        }
        for (k = 0; k < egraph->nLoops; k++) {
            if (lmom[k] != 0) {
                prMomStr(lmom[k], "k", k);
                zero = False;
            }
        }
        if (zero) {
            printf(" 0");
        }
    }
    printf("\n");
}

//==============================================================
// class EFLine
//--------------------------------------------------------------
void EFLine::print(const char *msg)
{
    if (ftype == FL_Open) {
        printf(" Open");
    } else if (ftype == FL_Closed) {
        printf(" Loop");
    } else {
        printf(" ?%d", ftype);
    }
    printf(" len=%d ", nlist);
    prIntArray(nlist, elist, msg);
}

//==============================================================
// class EGraph
//--------------------------------------------------------------
EGraph::EGraph(int nnodes, int nedges, int mxdeg)
{
    // Arguments
    //   nnodes : the number of nodes
    //   nedges : the number of edges
    //   mxdeg  : the maximum number of degrees of nodes

    int j;

    opt     = NULL;
    mgraph  = NULL;

    sNodes  = nnodes;
    sEdges  = nedges;
    sMaxdeg = mxdeg;
    sLoops  = sEdges - sNodes + 1;   // assume connected graph

    pId     = -1;
    mId     = -1;
    gSubId  = -1;
    assigned = False;   // 2019.01.22 fixed

    nNodes  = sNodes;
    nEdges  = sEdges;
    nLoops  = 0;
    nExtern = 0;

    nsym    = 1;
    esym    = 1;
    extperm = 1;
    nsym1   = 1;
    multp   = 1;

    nodes = new ENode*[sNodes];
    for (j = 0; j < sNodes; j++) {
        nodes[j] = new ENode(this, sNodes, sMaxdeg);
    }
    edges = new EEdge*[sEdges+1];
    for (j = 0; j <= sEdges; j++) {
        edges[j] = new EEdge(this, sEdges, sLoops);
    }

    bidef   = new int[sNodes];
    bilow   = new int[sNodes];
    bicount = -1;

    extMom  = new int[sEdges+1];

    fsign   = 1;
    nflines = 0;
    for (j = 0; j < GRCC_MAXFLINES; j++) {
        flines[j] = NULL;
    }

}

//--------------------------------------------------------------
EGraph::~EGraph()
{
    int j;

    for (j = GRCC_MAXFLINES-1; j >= 0; j--) {
        if (flines[j] != NULL) {
            delete flines[j];
            flines[j] = NULL;
        }
    }

    delete[] extMom;

    delete[] bilow;
    delete[] bidef;

    for (j = 0; j <= sEdges; j++) {
        delete edges[j];
        edges[j] = NULL;
    }
    delete[] edges;

    for (j = 0; j < sNodes; j++) {
        delete nodes[j];
        nodes[j] = NULL;
    }
    delete[] nodes;

}

//--------------------------------------------------------------
void EGraph::copy(EGraph *eg)
{
    int nd, ed;

#ifdef CHECK
    if (sNodes < eg->nNodes || sEdges < eg->nEdges || 
        sMaxdeg < eg->sMaxdeg || sLoops < eg->nLoops) {
        erEnd("EGraph::copy: sizes are too small");
    }
#endif
    mgraph  = eg->mgraph;
    opt     = mgraph->opt;

    pId     = eg->pId;
    mId     = eg->mId;
    gSubId  = eg->gSubId;
    nNodes  = eg->nNodes;
    nEdges  = eg->nEdges;
    nExtern = eg->nExtern;
    nLoops  = eg->nLoops;

    for (nd = 0; nd < nNodes; nd++) {
        nodes[nd]->copy(eg->nodes[nd]);
    }
    for (ed = 0; ed <= nEdges; ed++) {
        edges[ed]->copy(eg->edges[ed]);
    }

    nsym    = eg->nsym;
    esym    = eg->esym;
    extperm = eg->extperm;
    nsym1   = eg->nsym1;
    multp   = eg->multp;
    
    bicount = -1;
}

//--------------------------------------------------------------
void EGraph::setExtLoop(int nd, int val)
{
    // set the node 'nd' being an external node (-1) or a looped vertex

    nodes[nd]->extloop = val;
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
    int j, ni, nf;
    PNodeClass *pnc;
    int k;

#ifdef CHECK
    if (sNodes < mg->nNodes || sEdges < mg->nEdges || sMaxdeg < mg->maxdeg) {
        erEnd("EGraph::fromMGraph: sizes are too small");
    }
    if (sLoops < mg->nLoops) {
        printf("too small sLoops : %d < %d\n", sLoops, mg->nLoops);
        erEnd("too small sLoops");
    }
#endif
    mgraph  = mg;
    sproc   = mgraph->opt->sproc;
    if (sproc == NULL) {
        proc  = NULL;
        model = NULL;
    } else {
        proc  = sproc->proc;
        model = sproc->model;
    }
    opt     = mgraph->opt;

    nNodes  = mg->nNodes;
    nEdges  = mg->nEdges;
    nLoops  = mg->nLoops;
    nExtern = mg->nExtern;
    pId     = mg->pId;
    mId     = mg->cDiag;
    aId     = -1;
    gSubId  = 0;
    nsym    = mg->nsym;
    esym    = mg->esym;
    extperm = 1;
    nsym1   = nsym;
    multp   = 1;
    maxdeg  = mg->maxdeg;

    ed = 1;
    for (n0 = 0; n0 < nNodes; n0++) {
        nodes[n0]->deg = 0;
    }
    for (n0 = 0; n0 < nNodes; n0++) {
        for (e = 0; e < mg->adjMat[n0][n0]/2; e++, ed++) {
            edges[ed]->nodes[0] = n0;
            edges[ed]->nodes[1] = n0;
            nodes[n0]->edges[nodes[n0]->deg++] = - ed;
            nodes[n0]->edges[nodes[n0]->deg++] =   ed;
            edges[ed]->ext = isExternal(n0);
        }
        for (n1 = n0+1; n1 < nNodes; n1++) {
            for (e = 0; e < mg->adjMat[n0][n1]; e++, ed++) {
                edges[ed]->nodes[0] = n0;
                edges[ed]->nodes[1] = n1;
                nodes[n0]->edges[nodes[n0]->deg++] = - ed;
                nodes[n1]->edges[nodes[n1]->deg++] =   ed;
                edges[ed]->ext = (isExternal(n0) || isExternal(n1));
            }
        }
    }
#ifdef CHECK
    if (ed != nEdges+1) {
        printf("*** EGraph::init: ed=%d != nEdges=%d\n",
               ed, nEdges);
        erEnd("EGraph::init: illegal connection");
    }
#endif

    for (j = 0; j < nNodes; j++) {
        nodes[j]->setId(this, j);
        nodes[j]->intrct = mg->nodes[j]->extloop;
    }

    for (j = 0; j < nEdges; j++) {
        edges[j]->setId(this, j);
    }

    ni = 0;
    nf = 0;
    if (proc != NULL) {
        ni = proc->ninitl;
        for (j = 0; j < ni; j++) {
            setExtern(j, proc->initlPart[j], GRCC_ND_Initial);
        }

        nf = proc->nfinal;
        for (j = 0; j <  nf; j++) {
            setExtern(j+ni, proc->finalPart[j], GRCC_ND_Final);
        }
    } else if (sproc != NULL) {
        pnc = sproc->pnclass;
        for (j = 0; j < sproc->pnclass->nclass; j++) {
            if (pnc->type[j] == GRCC_AT_Initial || pnc->type[j] == GRCC_AT_External) {
                for (k = pnc->cl2nd[j]; k < pnc->cl2nd[j+1]; k++) {
                    setExtern(j, k, GRCC_ND_Initial);
                    ni++;
                }
            } else if (sproc->pnclass->type[j] == GRCC_AT_Final) {
                for (k = pnc->cl2nd[j]; k < pnc->cl2nd[j+1]; k++) {
                    setExtern(j, k, GRCC_ND_Final);
                    nf++;
                }
            }
                    
        }
    }

    nExtern = ni + nf;

    // name of momenta
    eext = 1;
    eint = 1;
    for (ed = 1; ed <= nEdges; ed++) {
        if (edges[ed]->ext) {
            edges[ed]->momn = eext++;
        } else {
            edges[ed]->momn = eint++;
        }
        edges[ed]->cut = False;
    }

    bicount = -1;
}

//--------------------------------------------------------------
ENode *EGraph::setExtern(int n0, int pt, int ndtyp)
{
    ENode *nd;
    int npt, ept, eg;

    nd = nodes[n0];
    if (nd->deg != 1) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** illegal external particle %d, %d, %d\n",
                    n0,pt,ndtyp);
        }
        erEnd("illegal external particle");
    }

    // particle comes into the node
    if (ndtyp == GRCC_ND_Initial) {
        npt = pt;
    } else if (ndtyp == GRCC_ND_Final) {
        npt = -pt;
    } else {
        npt = 0;
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** illegal type of an external particle : "
                   "node = %d, particle = %d, type = %d", n0, pt, ndtyp);
        }
        erEnd("illegal type of an external particle");
    }

    // edge attached to the external node
    eg = n0;

    // particle flows on e from leg=0 to 1.
    if (model != NULL){
        npt = model->normalParticle(npt);
        ept = model->normalParticle(-npt);
    } else {
        npt = 0;
        ept = 0;
    }

    nd->setExtern(ndtyp, npt);
    edges[eg]->ptcl = ept;

    return nd;
}

//--------------------------------------------------------------
void EGraph::print(void)
{
    // print the EGraph
  
    int  nd, ed, nlp;
  
    nlp = nEdges - nNodes + 1;
    printf("\nEGraph\n");
    printf("    pId=%d, gSubId=%ld, mId=%ld, aId=%ld\n",
           pId, gSubId, mId, aId);
    printf("    sNodes=%d, sEdges=%d, sMaxdeg=%d, sLoops=%d\n",
           sNodes, sEdges, sMaxdeg, sLoops);
    printf("    nNodes=%d, nEdges=%d, nExtern=%d, nLoops=%d\n",
           nNodes, nEdges, nExtern, nlp);
    printf("    ");
    if (model == NULL) {
        printf("model=NULL,");
    } else {
        printf("model=\"%s\",", model->name);
    }
    if (proc == NULL) {
        printf("proc=NULL,");
    } else {
        printf("proc=%d,", proc->id);
    }
    if (sproc == NULL) {
        printf("sproc=NULL,");
    } else {
        printf("sproc=%d,", sproc->id);
    }
    printf("\n");
    printf("    asssigned=%d, sym = (%ld * %ld) ", 
           assigned, nsym, esym);
    printf("extperm=%ld, nsym1=%ld, multp=%ld\n", 
           extperm, nsym1, multp);

    printf("  Nodes\n");
    for (nd = 0; nd < nNodes; nd++) {
        if (isExternal(nd)) {
            printf("    %2d Extern ", nd);
        } else {
            printf("    %2d Vertex ", nd);
        }
        nodes[nd]->print();
    }
    printf("  Edges\n");
    for (ed = 0; ed < nEdges; ed++) {
        if (edges[ed]->ext) {
            printf("    %2d Extern ", ed);
        } else {
            printf("    %2d Intern ", ed);
        }
        edges[ed]->print();
    }
    if (bicount > 0) {
        printf("  Biconn: nopicomp=%d, nopi2p=%d, opi2plp=%d\n",
               nopicomp, nopi2p, opi2plp);
    }
    printf("\n");

    if (nflines > 0) {
        prFLines();
    }
}
//--------------------------------------------------------------
void EGraph::prFLines(void)
{
    int j;

    printf("  Fermion lines %d, sign=%d (mId=%ld, aId=%ld)\n", 
            nflines, fsign, mId, aId);
    for (j = 0; j < nflines; j++) {
        printf("%4d ", j);
        flines[j]->print("\n");
    }
}

//--------------------------------------------------------------
int EGraph::dirEdge(int n, int e)
{
    if (nodes[n]->edges[e] > 0) {
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
#ifdef CHECK
                if (Abs(sgn) != 1) {
                    erEnd("cmpMom : illegal element of lmom");
                }
#endif
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
#ifdef CHECK
                if (Abs(sgn) != 1) {
                    erEnd("cmpMom : illegal element of emom");
                }
#endif
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
                cmp = cmpMom(edges[ed0]->lmom, edges[ed0]->emom, 
                             edges[ed1]->lmom, edges[ed1]->emom);
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
    int    grp[GRCC_MAXEDGES+1], ed2gr[GRCC_MAXEDGES+1];
    int    g, ed, ngrp;
    Bool   ok;
    int    minopi2p;

    biconnE();
    if (opt->values[GRCC_OPT_No2PtL1PI] == 0) {
        return True;
    }

    if (nExtern == 2 && nopicomp == 1) {
        minopi2p = 2;
    } else {
        minopi2p = 1;
    }

    if (opi2plp > 0 && nopi2p >= minopi2p) {
        if (opt->values[GRCC_OPT_No2PtL1PI] > 0) {
            return False;
        } else if (opt->values[GRCC_OPT_No2PtL1PI] < 0) {
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
                if (edges[ed]->ext) {
                    ok = False;
                    break;
                }
                edup->edges[ed]->cut = True;
            } else {
                edup->edges[ed]->cut = False;
            }
        }
        if (ok) {
            edup->gSubId++;
            edup->biconnE();

            if (edup->nExtern == 2 && edup->nopicomp == 1) {
                minopi2p = 2;
            } else {
                minopi2p = 1;
            }

            if (edup->opi2plp > 0 && edup->nopi2p >= minopi2p) {
                if (opt->values[GRCC_OPT_No2PtL1PI] > 0) {
                    return False;
                } else if (opt->values[GRCC_OPT_No2PtL1PI] < 0) {
                    return True;
                }
            }
        }
    }

    if (opt->values[GRCC_OPT_No2PtL1PI] > 0) {
        return True;
    } else if (opt->values[GRCC_OPT_No2PtL1PI] < 0) {
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
        if (edges[e]->cut || edges[e]->edtype == GRCC_ED_Deleted) {
            continue;
        }
        if (edges[e]->visited) {
            continue;
        }
        if (root < 0) {
            if (edges[e]->ext) {
                nd0 = edges[e]->nodes[0];
                nd1 = edges[e]->nodes[1];
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
            er = edges[e]->nodes[0];
        }
        if (vr < 0) {
            vr = edges[e]->nodes[0];
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
    int e, ie, root;
    int extlst[GRCC_MAXEDGES+1], intlst[GRCC_MAXEDGES+1];
    int opiext, opiloop;

    biinitE();
    bconn = 0;      // the number of connected components

    for (e = 1; e <= nEdges; e++) {
        root = findRoot();   // root of a spanning tree to be searched.
        if (root < 0) {
            break;
        }
        bisearchE(root, extlst, intlst, &opiext, &opiloop);
        opi2plp = Max(opi2plp, opiloop);
        if (isExternal(root)) {
            opiext++;
        }
        if (opiext == 2) {
            opi2plp = Max(opi2plp, opiloop);
            nopi2p++;
        }
        for (ie = 1; ie <= nEdges; ie++) {
            if (intlst[ie]) {
                edges[ie]->opicomp = nopicomp;
            }
        }
        nopicomp++;

        bconn++;    // the number of connected component
    }

    // momentum conservation of external particles
    extMomConsv();

#ifdef CHECK
    chkMomConsv();
#endif

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
        nodes[n]->ndtype  = GRCC_ND_Undef;    
        for (j = 0; j < nLoops; j++) {
            nodes[n]->klow[j] = -1;
        }
    }

    for (e = 1; e <= nEdges; e++) {
        edges[e]->visited = False;
        edges[e]->conid   = -1;                  // connected component
        edges[e]->edtype  = GRCC_ED_Undef;    
        edges[e]->opicomp = -1;
        for (j = 1; j <= nEdges; j++) {
            edges[e]->emom[j] = 0;            // coefficients of ext. mom.
        }
        for (j = 0; j < nLoops; j++) {
            edges[e]->lmom[j] = 0;            // coefficients of loop mom.
        }
    }
}

//--------------------------------------------------------------
void EGraph::bisearchE(int nd, int *extlst, int *intlst, int *opiext, int *opiloop)
{
    int k, e, ie, ed, td, dir, j;
    int opiext1, opiloop1;
    int extlst1[GRCC_MAXEDGES+1];   // set of external nodes below
    int intlst1[GRCC_MAXEDGES+1];   // set of vertices in the 1PI component

    (*opiext)  = 0;
    (*opiloop) = 0;

    // visit node 'nd'
    bidef[nd] = bicount;
    bilow[nd] = bicount;
    for (k = 0; k < nLoops; k++) {
        nodes[nd]->klow[k] = bicount;
    }
    bicount++;
    for (j = 1; j <= nEdges; j++) {
        extlst[j] = False;
        intlst[j] = False;
    }
    // the case of nd being external is handled at the bottom of this func.

    // go to children : nd --> ed --> td
    for (e = 0; e < nodes[nd]->deg; e++) {
        ed = Abs(nodes[nd]->edges[e]);
        if (edges[ed]->edtype == GRCC_ED_Deleted) {
            // permanently deleted edge
            continue;
        } else if (edges[ed]->visited) {
            continue;
        } else if (edges[ed]->cut) {
            // temporalily cut edge
            (*opiext)++;
            continue;
        } else if (!isExternal(nd) && edges[ed]->ext) {
            edges[ed]->visited = True;
            extlst[ed] = True;
            edges[ed]->emom[ed] = 1;
            (*opiext)++;
            continue;
        }
  
        // momentum is assigned on the backward move.
        edges[ed]->visited = True;
        td  = edges[ed]->nodes[0];
#ifdef CHECK
        if (td < 0 || td >= nNodes) {
            printf("*** EGraph::bisearchE: illega td=%d (0..%d), ed=%d\n",
                   td, nNodes, ed);
            print();
            erEnd("EGraph::bisearchE: illega td");
        }
#endif
        dir = 1;
        if (td == nd) {
            td = edges[ed]->nodes[1];
            dir = -1;
        }
  
        if (bidef[td] >= 0) {
            // already visited : a back edge is found
            bilow[nd] = Min(bilow[nd], bilow[td]);
            edges[ed]->setType(GRCC_ED_Back);
            intlst[ed] = True;
    
            // create new loop momentum
            k = loopm++;
            nodes[nd]->klow[k] = Min(nodes[nd]->klow[k], nodes[td]->klow[k]);
            edges[ed]->setLMom(k, dir);
            (*opiloop)++;
    
            // self-loop
            if (td == nd) {
                nodes[nd]->setType(GRCC_ND_CPoint);
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
                if (nodes[td]->klow[k] <= nodes[nd]->klow[k]) {
                    // inside loop 'k'
                    edges[ed]->setLMom(k, dir);
                }
                nodes[nd]->klow[k] = Min(nodes[nd]->klow[k], nodes[td]->klow[k]);
            }
                 
            if (bilow[td] >= bidef[nd]) {
                // a cut point is found
                if (nodes[nd]->ndtype == GRCC_ND_Undef) {
                    nodes[nd]->setType(GRCC_ND_CPoint);
                } else if (nodes[nd]->ndtype != GRCC_ND_CPoint) {
                    if (prlevel > 0) {
                        fprintf(GRCC_Stderr, "bisearch: node %d is a cut point ",
                                nd);
                        fprintf(GRCC_Stderr, "(not undef %d)\n", nodes[nd]->ndtype);
                    }
                }
            }
                 
            if (bilow[td] > bidef[nd]) {
                // a bridge is found
                if (edges[ed]->edtype == GRCC_ED_Undef) {
                    edges[ed]->setType(GRCC_ED_Bridge);
                } else if (edges[ed]->edtype != GRCC_ED_Bridge) {
                    if (prlevel > 0) {
                        fprintf(GRCC_Stderr, "bisearch: edges %d is a bridge ", ed);
                        fprintf(GRCC_Stderr, "(not undef %d)\n", edges[ed]->edtype);
                    }
                }
                if (!edges[ed]->ext) {
                    opiext1++;            // from bridge
                    for (ie = 1; ie <= nEdges; ie++) {
                        if (intlst1[ie]) {
                            edges[ie]->opicomp = nopicomp;
                            intlst1[ie] = False;
                        }
                    }
                    if (opiext1 == 2) {
                        opi2plp = Max(opi2plp, opiloop1);
                        nopi2p++;
                    }
                    nopicomp++;
                }
                opiext1  = 1;    // from bridge
                opiloop1 = 0;
    
            } else {
                // not a bridge
                bilow[nd] = Min(bilow[nd], bilow[td]);
                if (edges[ed]->edtype == GRCC_ED_Undef) {
                    edges[ed]->setType(GRCC_ED_Inloop);
                } else if (edges[ed]->edtype != GRCC_ED_Inloop) {
                    if (prlevel > 0) {
                        fprintf(GRCC_Stderr, "bisearch: ");
                        fprintf(GRCC_Stderr, "edges %d is not undef (%d)\n", 
                               ed, edges[ed]->edtype);
                    }
                }
                intlst1[ed] = True;
            }
            for (ie = 1; ie <= nEdges; ie++) {
                intlst[ie] = intlst[ie] || intlst1[ie];
            }
            (*opiext)  += opiext1;
            (*opiloop) += opiloop1;
    
            // linear combination of external momenta
            edges[ed]->setEMom(nEdges, extlst1, dir);
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
        if (edges[e]->ext) {
            lex = e;
            extMom[e] = edges[e]->emom[e];
        } else {
            extMom[e] = 0;
        }
    }
    if (lex < 0) {
        return;
    }
    for (e = 1; e <= nEdges; e++) {
        rs = edges[e]->emom[lex];
        if (rs != 0) {
            for (ex = 1; ex <= nEdges; ex++) {
                edges[e]->emom[ex] -= rs*extMom[ex];
            }
        }
    }
}

//--------------------------------------------------------------
void EGraph::chkMomConsv(void)
{
    // check momentum conservation

    int  esum[GRCC_MAXEDGES+1];
    int  lsum[GRCC_MAXNODES];
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
        for (ej = 0; ej < nodes[n]->deg; ej++) {
            e = Abs(nodes[n]->edges[ej]);
            if (edges[e]->nodes[0] == edges[e]->nodes[0]) {
                continue;
            }
            dir = dirEdge(n, ej);
            for (ex = 1; ex <= nEdges; ex++) {
                if (edges[e]->ext) {
                    esum[ex] += dir*edges[e]->emom[ex];
                }
            }
            for (lk = 0; lk < nLoops; lk++) {
                lsum[lk] += dir*edges[e]->lmom[lk];
            }
        }
  
        okn = True;
        for (ex = 1; ex <= nEdges; ex++) {
            if (esum[ex] != 0) {
                okn = False;
                fprintf(GRCC_Stderr, "chkMomConsv:n=%d, esum[%d]=%d\n",
                        n, ex, esum[ex]);
            }
        }
  
        for (lk = 0; lk < nLoops; lk++) {
            if (lsum[lk] != 0) {
                okn = False;
                fprintf(GRCC_Stderr, "chkMomConsv:n=%d, lsum[%d]=%d\n",
                        n, lk, lsum[lk]);
   
            }
        }
  
        if (!okn) {
            ok = False;
            fprintf(GRCC_Stderr, "*** Violation of momentum conservation ");
            fprintf(GRCC_Stderr, "at node =%d\n",n);
       }
    }

    if (!ok) {
        print();
        erEnd("inconsistent momentum");
    }
}

//--------------------------------------------------------------
int  EGraph::isFermion(int ed)
{
    // return if particle on the edge ed follows Fermi statistics or not.

    int ptcl, ptype;

    ptcl  = edges[ed]->ptcl;
    ptype = model->particles[Abs(ptcl)]->ptype;

    return (ptype == GRCC_PT_Dirac || ptype == GRCC_PT_Majorana || ptype == GRCC_PT_Ghost);
}

//--------------------------------------------------------------
int  EGraph::fltrace(int *fl)
{
    int nfl, k, i, nd, nd0, e, ed, fgcnt;

    nfl = 1;
    e = fl[0];
    if (e > 0) {
        nd0 = edges[e-1]->nodes[0];
    } else {
        nd0 = edges[-e-1]->nodes[1];
    }
    for (k = 0; k < nNodes; k++) {
        e = fl[k];
        if (e > 0) {
            nd = edges[e-1]->nodes[1];
        } else {
            nd = edges[-e-1]->nodes[0];
        }
        if (isExternal(nd) || nd == nd0) {
            fgcnt = 1;
            break;
        }
        fgcnt = 0;
        ed = 0;
        for (i = 0; i < nodes[nd]->deg; i++) {
            e  = nodes[nd]->edges[i];
            ed = Abs(e) - 1;
            if (!edges[ed]->visited) {
                edges[ed]->visited = True;
                if (isFermion(ed)) {
                    fgcnt++;
                    if (fgcnt == 1) {
                        fl[nfl++] = - e;
                        break;
                    }
                }
            }
        }
#ifdef CHECK
        if (fgcnt == 0) {
            printf("*** fline: Fermion number does not conserve\n");
            printf("    nd=%d, e=%d, ed=%d, fgcnt=%d\n",
                   nd, e, ed, fgcnt);
            erEnd("fline: Fermion number does not conserve");
        } else if (fgcnt > 1) {
            printf("*** fline: Four fermi interaction ?\n");
            printf("    nd=%d, e=%d, ed=%d, fgcnt=%d\n",
                   nd, e, ed, fgcnt);
            erEnd("fline: Four fermi interaction ?");
        }
#endif
    }
#ifdef CHECK
    if (k >= nNodes) {
        printf("*** fline: illegal control\n");
        printf("    nfl=%d, ", nfl);
        prIntArray(nfl, fl, "\n");
        erEnd("fline: illegal control");
    }
#endif
    return nfl;
}

//--------------------------------------------------------------
void EGraph::getFLines(void)
{
    int fl[GRCC_MAXNODES];
    int nextn, exto[GRCC_MAXNODES];
    int ptcl, e, ed, nd, floop, nswap, nfl;

    nflines = 0;
    for (ed = 0; ed < nEdges; ed++) {
        edges[ed]->visited = False;
    }

    nextn = 0;
    for (nd = 0; nd < nNodes; nd++) {
        if (isExternal(nd)) {
            e  = nodes[nd]->edges[0];
            ed = Abs(e) - 1;
            if (edges[ed]->visited) {
                continue;
            }
            if (isFermion(ed)) {
                fl[0] = - e;
                nfl = 1;
                edges[ed]->visited = True;

                nfl = fltrace(fl);
                addFLine(FL_Open, nfl, fl);
                e  = fl[nfl-1];
                if (e > 0) {
                    exto[nextn++] = edges[e-1]->nodes[1];
                } else {
                    exto[nextn++] = edges[-e-1]->nodes[0];
                }
            }
        }
    }

    // closed Fermion line
    floop = 0;
    for (ed = 0; ed < nEdges; ed++) {
        if (edges[ed]->visited) {
            continue;
        }
        ptcl = edges[ed]->ptcl;
        if (isFermion(ed)) {
            if (ptcl > 0) {
                e = ed + 1;
            } else {
                e = - ed - 1;
            }
            nfl = 1;
            fl[0] = e;
            edges[ed]->visited = True;
            nfl = fltrace(fl);
            addFLine(FL_Closed, nfl, fl);
            floop++;
        }
    }
    if (nextn > 0) {
        nswap = sortb(nextn, exto);
    } else {
        nswap = 0;
    }
    if ((floop+nswap) % 2 == 0) {
        fsign = 1;
    } else {
        fsign = -1;
    }
}

//--------------------------------------------------------------
void EGraph::addFLine(const FLType ft, int nfl, int *fl)
{
    int j;

    if (nflines >= GRCC_MAXFLINES) {
        erEnd("too many Fermion lines (GRCC_MAXEDGES)");
    }
    if (flines[nflines] == NULL) {
        flines[nflines] = new EFLine();
    }
    flines[nflines]->ftype = ft;
    flines[nflines]->nlist = nfl;
    for (j = 0; j < nfl; j++) {
        flines[nflines]->elist[j] = fl[j];
    }
    nflines++;

}
//**************************************************************
// assign.cc
//==============================================================
// Assign particles to the edges and interactions to nodes.
// Completed graph data is saved in the form of EGraph.

// method : selection of assignable node

#ifdef DEBUG1
static int nordleg   = 0;
static int nopleg    = 0;
static int niso      = 0;
static int niso1     = 0;
static int niso11    = 0;
static int niso111   = 0;
static int niso112   = 0;
static int niso12    = 0;
static int niso13    = 0;
static int niso14    = 0;
static int niso2     = 0;
static int nivord    = 0;
#endif

//===============================================================
// class NCand
NCand::NCand(const NCandSt sta, const int dega, const int nilst, int *ilst)
{
    // candidate list of interactions attached to a vertex.

    int j;

    deg    = dega;
    st     = sta;
    nilist = nilst;
    for (j = 0; j < nilist; j++) {
        ilist[j] = ilst[j];
    }

#ifdef CHECK
    if (st == AS_Assigned) {
        if (nilist != 1) {
            erEnd("illegal assignment");
        }
    } else if (st == AS_AssExt) {
        if (nilist != 1) {
            erEnd("illegal assignment");
        }
    }
#endif
}

//---------------------------------------------------------------
NCand::~NCand()
{
}

//---------------------------------------------------------------
void NCand::prNCand(const char* msg)
{
    printf("%d %d ",  st, deg);
    prIntArray(nilist, ilist, msg);
}

//===============================================================
// class ECand
ECand::ECand(int dt, int nplst, int *plst)
{
    int j;

    det    = dt;
    nplist = nplst;
    for (j = 0; j < nplist; j++) {
        plist[j] = plst[j];
    }

    if (det && nplist != 1) {
        if (prlevel > 0) {
            fprintf(GRCC_Stderr, "*** ECand : len(plist) != 1 : det=%d ", det);
            prIntArrayErr(nplist, plist, "\n");
        }
        erEnd("ECand : len(plist) != 1");
    }
}

//---------------------------------------------------------------
ECand::~ECand()
{
}

//---------------------------------------------------------------
void ECand::prECand(const char *msg)
{
    prIntArray(nplist, plist, "");
    printf(" (det=%d)%s", det, msg);
}

//===============================================================
// class ANode
ANode::ANode(int dg)
{
    int j;

    deg    = dg;
    nlegs  = 0;
    anodes = new int[deg];
    aedges = new int[deg];
    aelegs = new int[deg];
    cand   = NULL;
    for (j = 0; j < deg; j++) {
        anodes[j] = -1;
        aedges[j] = -1;
        aelegs[j] = -1;
    }
}

//---------------------------------------------------------------
ANode::~ANode(void)
{
    if (cand != NULL) {
        delete cand;
        cand = NULL;
    }
    if (aelegs != NULL) {
        delete[] aelegs;
        delete[] aedges;
        delete[] anodes;
        aelegs = NULL;
        aedges = NULL;
        anodes = NULL;
    }
}

//---------------------------------------------------------------
int ANode::newleg(void)
{
    // add a slot for a leg

    int lg = nlegs;

    nlegs++;
#ifdef CHECK
    if (nlegs > deg) {
        fprintf(GRCC_Stderr, "*** ANode::newleg : nlegs = %d > deg = %d\n",
                nlegs, deg);
        erEnd("ANode::newleg : nlegs > deg");
    }
#endif

    return lg;
}

//===============================================================
// class AEdge
AEdge::AEdge(int n0, int l0, int n1, int l1)
{
    nodes[0]  = n0;       // node of 0 side of the edge
    nodes[1]  = n1;       // node of 1 side of the edge
    nlegs[0]  = l0;       // leg ot the node of 0 side of the edge
    nlegs[1]  = l1;       // leg ot the node of 1 side of the edge
    ptcl   = GRCC_PT_Undef;    // particle defined in the model
    cand   = NULL;        // candidate
}

//---------------------------------------------------------------
AEdge::~AEdge()
{
    if (cand != NULL) {
        delete cand;
    }
}

//===============================================================
// class Assign
Assign::Assign(SProcess *sprc, MGraph *mgr, PNodeClass *pnc)
{
    Bool ok;
    int  j;

    if (sprc == NULL) {
        erEnd("Assign: sproc == NULL");
    }

    // pointers to related objects
    sproc        = sprc;
    model        = sproc->model;
    opt          = sproc->opt;
    mgraph       = mgr;
    pnclass      = pnc;

    egraph = mgraph->egraph;
    if (egraph == NULL) {
        erEnd("Assign: egraph == NULL");
    }

    astack       = sproc->astack;
    if (astack == NULL) {
        erEnd("Assign: astack == NULL");
    }

#ifdef DEBUG1
    if (pnclass == NULL) {
        printf("+++ Assign::Assign : pnclass = NULL\n");
    } else {
        printf("+++ Assign::Assign : pnclass :\n");
        pnclass->prPNodeClass();
    }
#endif

    nNodes       = mgraph->nNodes;
    nEdges       = mgraph->nEdges;
    nExtern      = sproc->nExtern;
    nETotal      = 0;               // total number of edges

    // counters of graphs
    nAGraphs     = 0;
    wAGraphs     = Fraction(0,1);
    nAOPI        = 0;
    wAOPI        = Fraction(0,1);

    nodes        = new ANode*[nNodes];
    edges        = new AEdge*[nEdges];

    for (j = 0; j < nNodes; j++) {
        nodes[j] = NULL;
    }
    for (j = 0; j < nEdges; j++) {
        edges[j] = NULL;
    }
    for (j = 0; j < model->ncouple; j++) {
        cplleft[j] = sproc->clist[j];
    }

    // initialize astack
    astack->setAGraph(this);
    astack->checkPoint(checkpoint0);

    // import from mgraph
    ok = fromMGraph();
#ifdef CHECK
    checkAG("Assign::Assign");
#endif

    if (ok) {
        // for debugging
#ifdef DEBUG0
        printf("+++ End of initialization : candidate:\n");
        prCand("init");
        printf("\n");
#endif

        // start assignment
        assignAllVertices();
#ifdef DEBUG1
        printf("mId=%ld, sId=%ld, ordleg=%d, ivord=%d, ",
               egraph->mId, egraph->sId, nordleg, nivord);
        printf("niso=%d [%d(%d {%d, %d}, %d, %d, %d) %d]\n",
               niso, niso1, niso11, niso111, niso112, niso12, niso13, niso14,
               niso2);
#endif

    } else {
        // cannot assign
    }

    // restore from the stack
#ifdef CHECK
    astack->restoreMsg(checkpoint0, "Assign");  
#else
    astack->restore(checkpoint0);  
#endif
}

//===============================================================
Assign::~Assign()
{
    int j;

    for (j = 0; j < nEdges; j++) {
        delete edges[j];
        edges[j] = NULL;
    }
    delete[] edges;
    edges = NULL;

    for (j = 0; j < nNodes; j++) {
        delete nodes[j];
        nodes[j] = NULL;
    }
    delete[] nodes;
    nodes = NULL;
}

//---------------------------------------------------------------
void Assign::prCand(const char *msg)
{
    //  Print candidte table
    int    n, e, ne;
    AEdge *ed;

    printf("\n");
    printf("+++ Candidate list: %s\n", msg);
    printf("  Nodes %d\n", nNodes);
    for (n = 0; n < nNodes; n++) {
       printf("%d: edges=", n);
       prIntArray(nodes[n]->deg, nodes[n]->aedges, ": cand=");
       if (nodes[n]->cand == NULL) {
           printf("NULL\n");
       } else {
           nodes[n]->cand->prNCand("\n");
       }
    }
    ne = Min(nEdges, nETotal);
    printf("  Edges %d\n", ne);
    for (e = 0; e < ne; e++) {
       ed = edges[e];
       if (ed == NULL) {
           printf("NULL_Edge\n");
       } else {
           printf("%d: %d->%d: cand=", e, ed->nodes[0], ed->nodes[1]);
           if (edges[e]->cand == NULL) {
               printf("NULL\n");
           } else {
               edges[e]->cand->prECand("\n");
           }
       }
    }
}

//===============================================================
void Assign::checkAG(const char *msg)
{
    int n, lg;
    Bool ok = True;

    for (n = 0; n < nNodes; n++) {
        for (lg = 0; lg < nodes[n]->deg; lg++) {
            if (nodes[n]->aedges[lg] < 0) {
                printf("*** checkAG:%s: n=%d, lg=%d, aedges=%d\n",
                       msg, n, lg, nodes[n]->aedges[lg]);
                ok = False;
            }
        }
    }
    if (!ok) {
        prCand("checkAG");
        erEnd("checkAG: failed");
    }
}

//===============================================================
// control of the process of particle/interaction assignment

//---------------------------------------------------------------
Bool Assign::assignAllVertices(void)
{
    //  Entry point of the assignment procedure
    Bool   ok;

#ifdef CHECK
    checkCand("assignAllVertices:1");
#endif

#ifdef DEBUG0
    printf("+++ particle assignment for '%ld'\n", mgraph->mId);
#endif

    // start main part
#ifdef SIMPSEL
    ok = selectVertexSimp(-1);
#else
    ok = selectVertex();
#endif

#ifdef DEBUG0
    printf("\n");
    printf("+++ Total %ld assigned graphs for '%ld'\n",
          nAGraphs, mgraph->mId);
    printf("result: %ld ", nAGraphs);
    wAGraphs.print(" ");
    printf("%ld ", nAOPI);
    wAOPI.print("\n");
#endif

#ifdef CHECK
    checkCand("assignAllVertices:2");
#endif

    return ok;
}

//---------------------------------------------------------------
Bool Assign::selectVertexSimp(int lastv)
{
    //  Select a vertex for assignment of particles to legs
    //
    //  Argument
    //    lastv : previously assigned vertex; Nothing when lastv<0.

    Bool ok;
    int v;

#ifdef CHECK
        checkCand("selectVertexSimp");
#endif

    // find a vertex to which interaction is assigned
    v = selUnAssVertexSimp(lastv);

    // no more vertex
    if (v < 0) {

        ok = allAssigned();
        if (ok) {
            opt->newAGraph(egraph);
        }

        return ok;
    }

    // select a leg and assign a particle to it
    ok = selectLeg(v, -1);

    return ok;
}

//---------------------------------------------------------------
Bool Assign::selectVertex(void)
{
    //  Select a vertex for assignment of particles to legs
    //
    //  Argument
    //    lastv : previously assigned vertex; Nothing when lastv<0.

    Bool ok;
    int v;

#ifdef CHECK
        checkCand("selectVertex");
#endif

    // find a vertex to which interaction is assigned
    v = selUnAssVertex();

    // no more vertex
    if (v < 0) {

        ok = allAssigned();
        if (ok) {
            opt->newAGraph(egraph);
        }

        return ok;
    }

    // select a leg and assign a particle to it
    ok = selectLeg(v, -1);

    return ok;
}

//---------------------------------------------------------------
Bool Assign::selectLeg(int v, int lastlg)
{
    //  Select a leg of vertex 'v' for assignment of particle,
    //  where the last assigned leg was 'lastlg'.

    int      pt, ln, j;
    Bool     ok;
    CheckPt  sav, sav0;
    int      nplist, plist[GRCC_MAXMPARTICLES2];

#ifdef CHECK
    checkNode(v, "selectLeg:0");
    checkCand("selectLeg");
#endif

    // find a leg to be assigned
    ln = selUnAssLeg(v, lastlg);

    // no more assignable leg for the current node
    if (ln < 0) {
        // move to next vertex
        ok = assignVertex(v);

        return ok;
    }

    // make the list of possible particles, incoming to the vertex
    astack->checkPoint(sav0);
    nplist = candPart(v, ln, plist, GRCC_MAXMPARTICLES2);

    // no candidate
    if (nplist < 1) {
        return False;
    }

    // assign each of possible particle to the leg 'lg'.
    astack->checkPoint(sav);
    for (j = 0; j < nplist; j++) {
        pt = plist[j];

        // try to assign 'pt' to (v, ln)
        if(assignPLeg(v, ln, pt)) {
            // move to next leg
            selectLeg(v, ln);
        }

#ifdef CHECK
        astack->restoreMsg(sav, "selectLeg2");
#else
        astack->restore(sav);
#endif
    }

#ifdef CHECK
    astack->restoreMsg(sav0, "selectLeg2");
#else
    astack->restore(sav0);
#endif
    return True;
}

//---------------------------------------------------------------
void Assign::saveCouple(int *sav)
{
    int j;

    if (model->ncouple > 1) {
        for (j = 0; j < model->ncouple; j++) {
            sav[j] = cplleft[j];
        }
    }
}

//---------------------------------------------------------------
void Assign::restoreCouple(int *sav)
{
    int j;

    if (model->ncouple > 1) {
        for (j = 0; j < model->ncouple; j++) {
            cplleft[j] = sav[j];
        }
    }
}

//---------------------------------------------------------------
Bool Assign::subCouple(int *cpl)
{
    int j;

    if (model->ncouple > 1) {
        for (j = 0; j < model->ncouple; j++) {
            cplleft[j] -= cpl[j];
            if (cplleft[j] < 0) {
                return False;
            }
        }
    }
    return True;
}

//---------------------------------------------------------------
Bool Assign::assignVertex(int v)
{
    //  Assign interactions to vertex 'v'

    Bool      done, ok;
    CheckPt   sav, sav1;
    NCand    *nc;
    int       ia, n, j, cl;
    NCandSt   vst;
    Bool      ok1;
    int       savc0[GRCC_MAXNCPLG], savc1[GRCC_MAXNCPLG];

#ifdef CHECK
    checkCand("assignVertex");
#endif

    // save the configuration
    astack->checkPoint(sav);
    saveCouple(savc0);

    // assign to all vertices with only one candicate
    done = False;
    for (n = 0; n < nNodes; n++) {

        // check if vertex
        cl = pnclass->nd2cl[n];
        if (!isATExternal(pnclass->type[cl])) {

            nc = nodes[n]->cand;
            if (nc->st == AS_UnAssLegs && nc->nilist == 1) {

                // bind interaction to the vertex
                ia = nc->ilist[0];
                vst = assignIVertex(n, ia);
                if (vst == AS_Impossible) {
                    // discard the current configuration
#ifdef CHECK
                    astack->restoreMsg(sav, "assignVertex");
#else
                    astack->restore(sav);
#endif
                    restoreCouple(savc0);
                    return False;

                } else if (vst == AS_Assigned) {
                    if (!subCouple(model->interacts[ia]->clist)) {
#ifdef CHECK
                        astack->restoreMsg(sav, "assignVertex");
#else
                        astack->restore(sav);
#endif
                        restoreCouple(savc0);
                        return False;
                    }
                }
                if (n == v) {
                    done = (vst == AS_Assigned);
                }
            }
        }
    }

    // uniquely determined
    ok = True;
    if (done) {
        // move to the next vertex
#ifdef SIMPSEL
        ok = selectVertexSimp(v);
#else
        ok = selectVertex();
#endif

#ifdef CHECK
        astack->restoreMsg(sav, "assignVertex");
#else
        astack->restore(sav);
#endif
        restoreCouple(savc0);

        return ok;
    }

    // there are still several possibilities.

    astack->checkPoint(sav1);
    saveCouple(savc1);
    for (j = 0; j < nodes[v]->cand->nilist; j++) {
        ia = nodes[v]->cand->ilist[j];

        // bind interaction to the vertex
        ok1 = True;
        vst = assignIVertex(v, ia);
        if (vst == AS_Assigned) {
            ok1 = subCouple(model->interacts[ia]->clist);
        }
        if (ok1 && (vst == AS_Assigned || vst == AS_Assigned0)) {
            // move to the next vertex
#ifdef SIMPSEL
            ok = selectVertexSimp(v);
#else
            ok = selectVertex();
#endif
        }

#ifdef CHECK
        astack->restoreMsg(sav1, "assignVertex");
#else
        astack->restore(sav1);
#endif
        restoreCouple(savc1);
    }

#ifdef CHECK
    astack->restoreMsg(sav, "assignVertex");
#else
    astack->restore(sav);
#endif
    restoreCouple(savc0);

    return ok;
}

//---------------------------------------------------------------
Bool Assign::allAssigned(void)
{
    // Assignment of particles and interactions is finished.
    // Check isomorphism etc. and count symmetry factor
    // The sign from Fermi statistics is calculated inf fillEGraph

    BigInt  nsym, esym, nsym1;
    MNodeClass *cl;
    Bool ok = True;

#ifdef CHECK
    checkCand("allAssigned");
#endif

    // check order of coupling constants
#ifdef CHECK
    if (!checkOrderCpl()) {
        erEnd("allAssigned: checkOrderCpl = False");
    }
#endif

    // check duplication by violating ordering condition
    if (!isOrdLegs()) {
#ifdef DEBUG1
        nordleg++;
#endif
        return False;
    }

    // classification of nodes
    cl = mgraph->curcl;

    // check isomorphism and calculate sfactor
    nsym = 0;
    esym = 0;

    ok = isIsomorphic(cl, &nsym, &esym, &nsym1);
    if (!ok || nsym < 1 || esym < 1) {
#ifdef DEBUG1
        niso++;
#endif
        return False;
    }

    //--------------------------
    // Now we got a new agraph.

    // Update counters
    nAGraphs++;
    wAGraphs.add(1,nsym*esym);
    if (mgraph->opi) {
        nAOPI++;
        wAOPI.add(1,nsym*esym);
    }

#ifdef DEBUG1
    for (int j = 0; j < model->ncouple; j++) {
        if (cplleft[j] != 0) {
            printf("nAgraphs=%ld: 0 != cplleft =", nAGraphs);
            prIntArray(model->ncouple, cplleft, "\n");
            break;
        }
    }
#endif

#ifdef CHECK
    checkAG("allAssigned");
#endif
    // fill imformation to resulting Egraph
    fillEGraph(nAGraphs, nsym, esym, nsym1);

#ifdef DEBUG0
    printf("Assigned graph = %ld, sym = (%ld, %ld) ", nAGraphs, nsym, esym);
    prCand("allAssigned ");
#endif

    return True;
}

//===============================================================
// Interface to MGraph and EGraph
//---------------------------------------------------------------
Bool Assign::fromMGraph(void)
{
    // Import from MGraph

    int     npall, *pall;
    int     np[1];
    Bool    ok;
    int     j, k, n, nn, n1, deg, nc, ptcl, cl, typ, mcl, cl1, typ1;
    int     lcn, ia;
    int     nilist, ilist[GRCC_MAXMINTERACT];
    NCandSt st;

    // create ANodes
    for (j = 0; j < nNodes; j++) {
        nodes[j] = new ANode(mgraph->nodes[j]->deg);
    }

    // initializaion of edge table
    nETotal = 0;

    // List of all particles
    pall = model->allParticles(&npall);

    // add nodes
    for (n = 0; n < mgraph->nNodes; n++) {
        cl   = pnclass->nd2cl[n];
        typ  = pnclass->type[cl];

        // external particle
        if (isATExternal(typ)) {

#ifdef CHECK
            if (mgraph->nodes[n]->deg != 1) {
                printf("*** assign:fromMGraph : "
                       "external but deg[%d] = %d != 1, type=%d\n",
                       n, mgraph->nodes[n]->deg, typ);
                mgraph->print();
                erEnd("assign:fromMGraph : illegal external particle");
            }
#endif
            np[0] = pnclass->particle[cl];
            nodes[n]->cand = new NCand(AS_AssExt, 1, 1, np);
            for (n1 = 0; n1 < mgraph->nNodes; n1++) {
                nc = mgraph->adjMat[n][n1];
                for (k = 0; k < nc; k++) {
                    addEdge(n, n1, 1, np);
                }
            }

        // vertex
        } else {

            // candidates of vertices
            deg    = mgraph->nodes[n]->deg;
            st     = AS_UnAssLegs;
            cl     = sproc->pnclass->nd2cl[n];     // class in the process
            mcl    = sproc->pnclass->cl2mcl[cl];   // class in the model

            if (nodes[n]->cand != NULL) {
                delete nodes[n]->cand;
            }
            lcn = model->ncouple;

            // multiple coupling constants are defined in the model.
            if (lcn > 1) {
                nilist = 0;
                for (j = 0; j < model->cplgnvl[mcl]; j++) {
                    ia = model->cplgvl[mcl][j];

                    // select by coupling constants
                    if (leqArray(lcn, model->interacts[ia]->clist, cplleft)) {
                        ilist[nilist++] = ia;
                    }
                }
                nodes[n]->cand = new NCand(st, deg, nilist, ilist);

            // there is only one coupling constant
            } else {
                nodes[n]->cand = 
                    new NCand(st, deg, model->cplgnvl[mcl], model->cplgvl[mcl]);
            }

            // vertices
            for (n1 = n; n1 < mgraph->nNodes; n1++) {
                cl1   = pnclass->nd2cl[n1];
                typ1  = pnclass->type[cl1];
                if (!isATExternal(typ1)) {
                    nc = mgraph->adjMat[n][n1];
                    if (n == n1) {
                        nc = nc/2;
                    }
                    for (k = 0; k < nc; k++) {
                        addEdge(n, n1, npall, pall);
                    }
                }
            }
        }
    }
#ifdef CHECK
    if (nETotal != nEdges) {
        printf("*** Assign::fromMGraph nETotal=%d != nEdges=%d\n",
               nETotal, nEdges);
        erEnd("Assign::fromMGraph nETotal= != nEdges");
    }
#endif

    // assign particle of an edge next to an external particle
    for (n = 0; n < mgraph->nNodes; n++) {
        cl  = pnclass->nd2cl[n];
        typ = pnclass->type[cl];
        if (isATExternal(typ)) {

            cl   = pnclass->nd2cl[n];
            ptcl = pnclass->particle[cl];

            // particle ptcl     flows in to   the node and
            // particle (- ptcl) flows in from the edge.

#ifdef CHECK
            if (ptcl == 0) {
                erEnd("fromMGraph: ptcl=0");
            }
#endif
            ok = assignPLeg(n, 0, - ptcl);
            if (!ok) {
                // impossible config
                return False;
            }
            nn = nodes[n]->anodes[0];
            ok = updateCandNode(nn);
            if (!ok) {
                // impossible config
                return False;
            }
        }
    }

#ifdef CHECK
    checkCand("fromMGraph");
    checkAG("fromMGraph");
#endif

    return True;
}

//---------------------------------------------------------------
void Assign::addEdge(int n0, int n1, int nplist, int *plist)
{
    //  add an edge and set connection information
    //  n0 -- (new edge) -- n1, with candidates 'plist'

    int lg0, lg1, eid;
    AEdge *aed;

#ifdef CHECK
    if (n0 >= nNodes || n1 >= nNodes) {
        printf("*** Assign::addEdge : undefined nodes %d: [%d, %d]",
              nETotal, n0, n1);
        erEnd("Assign::addEdge : undefined nodes");
    }
#endif

    // legs to be connected
    lg0 = nodes[n0]->newleg();
    lg1 = nodes[n1]->newleg();

    // create edge
#ifdef CHECK
    if (nETotal >= nEdges) {
        erEnd("too many edges");
    }
#endif

    aed = new AEdge(n0, lg0, n1, lg1);
    aed->cand = new ECand(False, nplist, plist);

#ifdef CHECK
    if (edges[nETotal] != NULL) {
        erEnd("edges[nETotal] != NULL");
    }
#endif

    // registor to the edge table
    eid = nETotal;
    edges[nETotal++] = aed;

    // connect edge and nodes
    connect(n0, lg0, eid, 0, n1, lg1);
}

//---------------------------------------------------------------
void Assign::connect(int n0, int l0, int eg, int el, int n1, int l1)
{
    //  Connect (n0, l0) -- (eg, el) -- (nl)

    ANode *nd0, *nd1;
    AEdge *ed;
    int    eo;

    nd0 = nodes[n0];
    nd1 = nodes[n1];
    ed  = edges[eg];
    eo  = 1 - el;

    nd0->anodes[l0] = n1;
    nd0->aedges[l0] = eg;
    nd0->aelegs[l0] = el;

    nd1->anodes[l1] = n0;
    nd1->aedges[l1] = eg;
    nd1->aelegs[l1] = eo;

    ed->nodes[el] = n0;
    ed->nlegs[el] = l0;

    ed->nodes[eo] = n1;
    ed->nlegs[eo] = l1;
}

//---------------------------------------------------------------
Bool Assign::fillEGraph(int aid, BigInt nsym, BigInt esym, BigInt nsym1)
{
    //  Set variables of egraph with the result of particle assignment
    //  Adjust the ordering of legs of vertices in a consistent way
    //  with the interaction defined in the model.

    ANode *an;
    ENode *en;
    int    work[3][GRCC_MAXLEGS];
    int    n, lr, e;
    int   *elist;
    int    lg, ed, eg, ei;
#ifdef CHECK
    int    cl;
#endif

    egraph->aId   = aid;
    egraph->nsym  = nsym;
    egraph->esym  = esym;
    egraph->bicount = -1;
    if (sproc != NULL) {
        egraph->extperm = sproc->extperm;
    } else {
        egraph->extperm = 1;
    }
    egraph->nsym1 = nsym1;
    egraph->multp = (egraph->extperm * egraph->nsym1) / egraph->nsym;

#ifdef CHECK
    checkAG("fillEGraph:0");
#endif
    // external node
    for (n = 0; n < nNodes; n++) {
        // vertices
#ifdef CHECK
        cl  = pnclass->nd2cl[n];
        if (isATExternal(pnclass->type[cl])) {
            ;
        } else if (nodes[n]->cand->st != AS_Assigned) {
            printf("*** fillEGraph : node %d is not assigned", n);
            prCand("fillEGraph: node");
            erEnd("fillEGraph : node is not assigned");
        }
#endif

        an = nodes[n];
        en = egraph->nodes[n];

        en->initAss(egraph, n, an->deg);
        
        en->intrct = an->cand->ilist[0];
        elist = reordLeg(n, work[0], work[1], work[2]);
        for (lr = 0; lr < nodes[n]->deg; lr++) {
            if (elist == NULL) {
                lg = lr;
            } else {
                lg = elist[lr];
            }
#ifdef CHECK
            if (lg < 0 || lg >= nodes[n]->deg) {
                printf("*** fillEGraph: n=%d, lr=%d: 0 <= lg=%d < %d\n",
                       n, lr, lg, nodes[n]->deg);
                erEnd("fillEGrah: illegal reordering");
            }
#endif
            ed = an->aedges[lg];
            eg = an->aelegs[lg];

            ei = (2*eg-1)*(ed+1);
            en->edges[lr] = ei;

            egraph->edges[ed]->nodes[eg] = n;
            egraph->edges[ed]->nlegs[eg] = lr;
        }
    }
    // edges
    for (e = 0; e < nEdges; e++) {
#ifdef CHECK
        if (edges[e]->cand->nplist != 1) {
            printf("*** fillEGraph : edge %d is not assigned", e);
            prCand("fillEGraph: edge");
            erEnd("fillEGraph : edge is not assigned");
        }
#endif
        egraph->edges[e]->ptcl = edges[e]->cand->plist[0];
    }

    // the id and a flag
    egraph->assigned = True;

    // analyse fermion line and determine Fermi statistical sign factor
    egraph->getFLines();

#ifdef CHECK
    checkAG("fillEGraph:0");
#endif
    return True;

}

//---------------------------------------------------------------
int *Assign::reordLeg(int n, int *reord, int *plist, int *used)
{
    //  Reorder legs of node 'n' according with the definition of
    //  the interaction in the model

    int lg, ia, lr, deg, pt, ed;
    int *ilegs;
#ifdef CHECK
    Bool found;
#endif

    // external node
    if (nodes[n]->cand->st == AS_AssExt) {
        reord[0] = 0;
        return 0;
    }

    // degree of the node
    deg = nodes[n]->deg;

    if (deg <= 1) {
        reord[0] = 0;
        return 0;
    }

    // list of particles at the legs of the node 'n'
    for (lg = 0; lg < deg; lg++) {
        ed = nodes[n]->aedges[lg];
        pt = edges[ed]->cand->plist[0];
        plist[lg] = legEdgeParticle(n, lg, pt);
        used[lg]  = 0;
    }

    // list of legs in the interaction
    ia    = nodes[n]->cand->ilist[0];
    ilegs = model->interacts[ia]->plist;

    // reorder
#ifdef CHECK
    found = False;
#endif
    for (lr = 0; lr < deg; lr++) {
        for (lg = 0; lg < deg; lg++) {
            if (used[lg] == 0 && plist[lg] == ilegs[lr]) {
                reord[lr] = lg;
                used[lg]  = 1;
#ifdef CHECK
                found = True;
#endif
                break;
            }
        }

#ifdef CHECK
        if (!found) {
            printf("*** reordLeg: illegal list of particles:"
                   "interaction %d ", ia);
            prIntArray(deg, ilegs, "; ");
            printf("vertex %d ", n);
            prIntArray(deg, plist, "\n");
            prCand("reordLeg");
            erEnd("reordLeg: illegal list of particles");
        }
#endif
    }

    return reord;
}


//==============================================================
// Adjust the direction of a particle on an edge and on a leg of
// node.
//---------------------------------------------------------------
int Assign::getLegParticle(int n,  int ln)
{
    //  Get particle code of (n, ln) when particle 'pt' runs
    //  in the direction of the edge at (n, ln).
    //
    //  Get particle code in the direction the edge at (n, ln)
    //  when particle 'pt' is at leg (n, ln).
    //
    //  These two cases are realized by the same function

    ANode *nd;
    int    elg, ed, pt;

    nd  = nodes[n];
    elg = nd->aelegs[ln];
    ed  = nd->aedges[ln];
    pt  = edges[ed]->cand->plist[0];

    // normalization of sign for neutral particle
    if (elg == 0) {
        return model->normalParticle(-pt);
    } else {
        return model->normalParticle(pt);
    }
}

//---------------------------------------------------------------
int Assign::legEdgeParticle(int n, int ln, int pt)
{
    //  Get particle code of (n, ln) when particle 'pt' runs
    //  in the direction of the edge at (n, ln).
    //
    //  Get particle code in the direction the edge at (n, nl)
    //  when particle 'pt' is at leg (n, ln).
    //
    //  These two cases are realized by the same function

    // normalization of sign for neutral particle

    if (nodes[n]->aelegs[ln] == 0) {
        return model->normalParticle(-pt);
    } else {
        return model->normalParticle(pt);
    }
}

//---------------------------------------------------------------
int Assign::legPart(int v, int lg, int nplist, int *plist, int *rlist, const int size)
{
    // Convert list of candidate particles in 'plist' at (v, lg) ==> edge
    // or edge ==> (v, lg)

    int    j, nrlist;

    nrlist = 0;
    for (j = 0; j < nplist; j++) {
        nrlist = intSetAdd(nrlist, rlist, 
                           legEdgeParticle(v, lg, plist[j]), size);
    }
    return nrlist;
}

//---------------------------------------------------------------
int Assign::candPart(int v, int ln, int *plist, const int size)
{
    //  update candidate particles incoming to (v, ln)

    int      en;
    int      ntplist;
    ECand   *ec;

    en = nodes[v]->aedges[ln];

#ifdef CHECK
    if (edges[en]->cand->det) {
        printf("*** candPart : particle of leg (%d, %d) "
               "is assigned to %d\n",
               v, ln, edges[en]->cand->plist[0]);
        checkCand("candPart");
        mgraph->printAdjMat(mgraph->curcl);
        prCand("candPart");
        erEnd("candPart : particle of leg is assigned");
    }
#endif

    ec = edges[en]->cand;

    ntplist = legPart(v, ln, ec->nplist, ec->plist, plist, size);

    return  ntplist;
}

//==============================================================
// tools for assignment

//---------------------------------------------------------------
int Assign::selUnAssVertexSimp(int lastv)
{
    //  Select a vertex for assignment to its leg.
    //  We take one with minimum number of candidates
    //
    //  There may be better method.

    int v0, v;

    // select vertex one by one in the sequential order of node number
    v0 = Max(lastv+1, nExtern);
    for (v = v0; v < nNodes; v++) {
        if (nodes[v]->cand->st == AS_UnAssLegs) {
            return v;
        }
    }
    return -1;
}

//---------------------------------------------------------------
int Assign::selUnAssVertex(void)
{
    //  Select a vertex for assignment to its leg.
    //  We take one with minimum number of candidates
    //
    //  There may be better method.

    int v0, v, nl;

    // select vertex one by one in the sequential order of node number
    v0 = -1;
    nl = 0;
    for (v = 0; v < nNodes; v++) {
        if (nodes[v]->cand->st == AS_UnAssLegs) {
            if (nodes[v]->cand->nilist == 1) {
                return v;
            } else if (nodes[v]->cand->nilist > 1) {
                // select node with fewer candidates
                if (v0 < 0 || nodes[v]->cand->nilist < nl) {
                    v0 = v;
                    nl = nodes[v]->cand->nilist;
                }
            }
        }
    }
    return v0;
}

//---------------------------------------------------------------
int Assign::selUnAssLeg(int v, int lastlg)
{
    // Select a leg of vertex 'v' for the assignment.
    // The lastly selected leg was 'lastlg'.

    int lg0, lg, e;
#ifdef CHECK
    int n0, n1;
#endif

    // lg0 = Max(lastlg, lastlg + 1);
    lg0 = lastlg + 1;

    for (lg = lg0; lg < nodes[v]->deg; lg++) {

#ifdef CHECK
        if (lastlg >= 0) {
            n0 = nodes[v]->anodes[lastlg];
        } else {
            n0 = -1;
        }
        n1 = nodes[v]->anodes[lg];
        if (n0 > n1) {
            printf("*** selUnAssLeg: n0=%d > n1=%d\n", n0, n1);
            printf("*** illegal connection\n");
            erEnd("selUnAssLeg: n0 > n1");
        }
#endif
  
        e  = nodes[v]->aedges[lg];
        if (!edges[e]->cand->det) {
            return lg;
        }

    }
    return -1;
}

//---------------------------------------------------------------
NCandSt Assign::assignIVertex(int v, int ia)
{
    //  Try to assign interaction 'ia' to 'v'

    int     lplist[GRCC_MAXLEGS], iplist[GRCC_MAXLEGS];
    int     lg, e, pt, deg;


    if (nodes[v]->cand->st == AS_Assigned || nodes[v]->cand->st == AS_AssExt) {
        return AS_Assigned0;
    }

    deg = nodes[v]->deg;
    if (deg != model->interacts[ia]->nlegs) {
        return AS_Impossible;
    }

    // list of particles at the legs of the vertex
    for (lg = 0; lg < deg; lg++) {
        e = nodes[v]->aedges[lg];
        if (edges[e]->cand->nplist != 1) {
            return AS_UnAssLegs;
        } else {
            pt = edges[e]->cand->plist[0];
            lplist[lg] = legEdgeParticle(v, lg, pt);
        }
        iplist[lg] = model->interacts[ia]->plist[lg];
    }

    sorti(deg, lplist);
    sorti(deg, iplist);

    // from interaction
    if (cmpArray(deg, lplist, deg, iplist) == 0) {
        // orders of coupling constants should be checked ???
        astack->pushNode(v);
        nodes[v]->cand->st = AS_Assigned;
        nodes[v]->cand->nilist = 1;
        nodes[v]->cand->ilist[0] = ia;
        return AS_Assigned;
    }

    return AS_Impossible;
}

//---------------------------------------------------------------
Bool Assign::assignPLeg(int n, int ln, int pt)
{
    //  A particle 'pt' is assigned to leg 'ln' of node 'n'

    ANode *nd;
    int e, ept; 
    Bool ok;

#ifdef CHECK
    checkNode(n, "assignPLeg0");
    checkCand("assignPLeg");
#endif

    // nodes and the edge
    nd  = nodes[n];
    e   = nd->aedges[ln];

    // already determined
    if (edges[e]->cand->det) {
        return True;
    }

    if (!isOrdPLeg(n, ln, pt)) {
#ifdef DEBUG1
        nopleg++;
#endif
        return False;
    }

    // particle on the edge
    ept = legEdgeParticle(n, ln, pt);

#ifdef CHECK
    if (!isIn(edges[e]->cand->nplist, edges[e]->cand->plist, ept)) {
        printf("*** assignPLeg: particle %d is not in the cand. of e=%d",
                ept, e);
        edges[e]->cand->prECand("\n");
        prCand("assignPLeg");
        erEnd("assignPLeg: particle is not in the cand.");
    }
#endif

    // save the current configuration
    astack->pushEdge(e);

    // determine the particle on the edge
    edges[e]->cand->nplist   = 1;
    edges[e]->cand->plist[0] = ept;

    ok = detEdge(e);

    return ok;
}

//---------------------------------------------------------------
Bool Assign::detEdge(int e)
{
    Bool ok0, ok1;

    if (edges[e]->cand->nplist != 1) {
        return False;
    }
    edges[e]->cand->det = True;

    // update candidate list
    ok0 = updateCandNode(edges[e]->nodes[0]);
    if (ok0) {
        ok1 = updateCandNode(edges[e]->nodes[1]);
    } else {
        ok1 = False;
    }

    return (ok0 && ok1);
}

//---------------------------------------------------------------
Bool Assign::isOrdPLeg(int n, int ln, int pt)
{
    int e0, el, ln0, ep0, pt0, nn, n0, n1, ln1, pt1, e1, ep1;

    if (nodes[n]->deg < 2) {
        return True;
    }
    nn = nodes[n]->anodes[ln];
    if (n <= nn) {
        n0  = n;
        n1  = nn;
        ln0 = ln;
        pt0 = pt;
    } else {
        n0 = nn;
        n1 = n;
        e0  = nodes[n]->aedges[ln];
        el  = nodes[n]->aelegs[ln];
        ln0 = edges[e0]->nlegs[1-el];
        ep0 = legEdgeParticle(n,  ln,  pt);
        pt0 = legEdgeParticle(n0, ln0, ep0);
    }

    for (ln1 = 0; ln1 < nodes[n0]->deg; ln1++) {
        if (ln1 == ln0 || n1 != nodes[n0]->anodes[ln1]) {
            continue;
        }
        e1  = nodes[n0]->aedges[ln1];
        if (!edges[e1]->cand->det) {
            continue;
        }
        ep1 = edges[e1]->cand->plist[0];
        pt1 = legEdgeParticle(n0, ln1, ep1);
        if ((ln0 < ln1 && pt0 > pt1) ||
            (ln0 > ln1 && pt0 < pt1)) {
            return False;
        }
    }
    return True;
}

//==============================================================
// Operations of candidate
//---------------------------------------------------------------
Bool Assign::candPartClassify(int v, int *npdass, int *pdass, int *npuass, int *puass, const int size)
{
    // Construct the classified lists of particles possible to assign to
    // the legs of node v
    //
    // pdass = (duplicated set of particles where edges has a unique
    // candidate)
    // puass = (duplicated set of other candidate particles)

    int lg, e, npl, ass, jd, ju, j, pt, pts;

    jd = 0;
    ju = 0;
    for (lg = 0; lg < nodes[v]->deg; lg++) {
        e   = nodes[v]->aedges[lg];
        npl = edges[e]->cand->nplist;
        if (npl < 1) {
            return False;
        }
        ass = (npl == 1);
        for (j = 0; j < edges[e]->cand->nplist; j++) {
            pt  = edges[e]->cand->plist[j];
            pts = legEdgeParticle(v, lg, pt);
            if (ass) {
                jd = intSListAdd(jd, pdass, pts, size);
            } else {
                ju = intSListAdd(ju, puass, pts, size);
            }
        }
    }
    *npdass = jd;
    *npuass = ju;

    return True;
}

//---------------------------------------------------------------
Bool Assign::updateCandNode(int v)
{
    //  Update the configuration
    //  - nodes[v]->cand->ilist
    //  - edges[e]->cand->plist for the adjacent edge
    //  - nodes[n]->cand->unass for the adjacent node 'n' of 'e'

    int   npdass,  pdass[GRCC_MAXPSLIST];
    int   npuass,  puass[GRCC_MAXPSLIST];
    int    nsub0,   sub0[GRCC_MAXPSLIST];
    int  niplist, iplist[GRCC_MAXPSLIST];
    int      nd0,     d0[GRCC_MAXMPARTICLES2];
    int      nd1,     d1[GRCC_MAXMPARTICLES2];
    int      ns0,     s0[GRCC_MAXMPARTICLES2];
    int  neplist, eplist[GRCC_MAXMPARTICLES2];
    int  nitlist, itlist[GRCC_MAXMINTERACT];
    int e, it, j, k, i, lg;

    // external node
    if (nodes[v]->cand->st == AS_AssExt) {
#ifdef CHECK
        e = nodes[v]->aedges[0];
        if (e < 0 || edges[e]->cand->nplist != 1) {
            printf("*** illegal external node: v=%d e=%d :", v, e);
            prCand("updateCandNode");
            erEnd("illegal external node");
        }
#endif

        return True;
    }

    // impossible configuration.
    if (nodes[v]->cand->nilist < 1) {
        return False;
    }

    // list of possible particles on the leges of the vertex.
    // pdass = (set of assigned particles)
    // puass = (list of particles of edges with two of more candidates)

    niplist = 0;
    nitlist = 0;
    if (!candPartClassify(v, &npdass, pdass, &npuass, puass, GRCC_MAXPSLIST)) {
        return False;
    }

    if (npdass < 1) {
        return False;
    }

    // from interaction : slist
    // Conditions :
    // 1. pdass \subset slist
    // 2. slist-pdass \subset puass
    // from interaction : slist
    //  conditions : pdass \subset slist \subset (pdass + puass)
    //  sub0 = slist  -   pdass,
    nitlist = 0;
    for (j = 0; j < nodes[v]->cand->nilist; j++) {
        it = nodes[v]->cand->ilist[j];
        // conditions: 
        // 1. pdass \subset slist <==> pdass-slist = \emptyset
        // 2. slist-pdass \subset puass <==> (slist-pdass)-puass = \emptyset
        if (isSubSList(npdass, pdass, 
                       model->interacts[it]->nslist, 
                       model->interacts[it]->slist) ) {
            nsub0 = subtrSet(model->interacts[it]->nslist, 
                             model->interacts[it]->slist,
                             npdass, pdass, sub0, GRCC_MAXPSLIST);
            if (nsub0 < 1) {
                // slist == pdass : no additional candidate particle
                nitlist = intSetAdd(nitlist, itlist, it, GRCC_MAXMINTERACT);

            } else {
                if (isSubSList(nsub0, sub0, npuass, puass)) {
                    nitlist = intSetAdd(nitlist, itlist, it, GRCC_MAXMINTERACT);
                    for (k = 0; k < nsub0; k++) {
                        niplist = intSetAdd(niplist,iplist,sub0[k],GRCC_MAXPSLIST);
                    }
                }
            }
        }
    }

    if (nitlist < 1) {
        return False;
    }

    if (nitlist != nodes[v]->cand->nilist) {
        astack->pushNode(v);
        nodes[v]->cand->nilist = nitlist;
        for (i = 0; i < nitlist; i++) {
            nodes[v]->cand->ilist[i] = itlist[i];
        }
#ifdef CHECK
    } else if (nitlist > nodes[v]->cand->nilist) {
        erEnd("larger ncand");
#endif
    }

    // edge
    for (lg = 0; lg < nodes[v]->deg; lg++) {
        e = nodes[v]->aedges[lg];
        if (edges[e]->cand->nplist > 1) {
            // elist = {candidate particles in the direction of the edge}
            // s0 = plist \cap elist
            neplist = legPart(v, lg, niplist, iplist, eplist, GRCC_MAXMPARTICLES2);
            listDiff(edges[e]->cand->nplist, edges[e]->cand->plist,
                     neplist, eplist,
                     &nd0, d0, &ns0, s0, &nd1, d1);

            if (ns0 < 1) {
                return False;
            }
            if (ns0 < edges[e]->cand->nplist) {
                astack->pushEdge(e);
                edges[e]->cand->nplist = ns0;
                for (i = 0; i < ns0; i++) {
                    edges[e]->cand->plist[i] = s0[i];
                }
            }
        }
    }
    for (lg = 0; lg < nodes[v]->deg; lg++) {
        e = nodes[v]->aedges[lg];
        if (edges[e]->cand->nplist == 1 && !edges[e]->cand->det) {
            if(!detEdge(e)) {
                return False;
            }
        }
    }

    return True;
}

//==============================================================
// Check order of coupling constants, duplication and isomorphism
//--------------------------------------------------------------
Bool Assign::checkOrderCpl(void)
{
    //  Check order of coupling constants

    int ord[GRCC_MAXNCPLG];
    int lcn, n, ia, j, cl;

    // list of coupling constants
    lcn = model->ncouple;

    if (lcn == 1) {
        return True;
    }

    // sum up for each coupling constants
    for (j = 0; j < GRCC_MAXNCPLG; j++) {
        ord[j] = 0;
    }
    for (n = 0; n < nNodes; n++) {
        cl = pnclass->nd2cl[n];
        if (!isATExternal(pnclass->type[cl])) {
            ia = nodes[n]->cand->ilist[0];
            for (j = 0; j < lcn; j++) {
                ord[j] += model->interacts[ia]->clist[j];
            }
        }
    }

    // comparison
    for (j = 0; j < lcn; j++) {
        if (sproc->clist[j] != ord[j]) {
            return False;
        }
    }
    return True;
}

//--------------------------------------------------------------
Bool Assign::isOrdLegs(void)
{
    //  Whether graph is duplicated because of the violation
    //  of ordering in the case of multiple connections
    //    v0 <-- v --> v1

    int v, l0, v0, l1, v1, e0, e1, q0, q1, p0, p1, cl;

    for (v = 0; v < nNodes; v++) {
        cl = pnclass->nd2cl[v];
        if (!isATExternal(pnclass->type[cl])) {
            for (l0 = 0; l0 < nodes[v]->deg; l0++) {
                v0 = nodes[v]->anodes[l0];
                for (l1 = l0+1; l1 < nodes[v]->deg; l1++) {
                    v1 = nodes[v]->anodes[l1];
    
                    if (v0 == v1 && v <= v0) {
                        // multiple connections (v, l0) and (v, l1)
                        e0 = nodes[v]->aedges[l0];
                        e1 = nodes[v]->aedges[l1];
                        q0 = edges[e0]->cand->plist[0];
                        q1 = edges[e1]->cand->plist[0];
                        p0 = legEdgeParticle(v, l0, q0);
                        p1 = legEdgeParticle(v, l1, q1);
                        if (p0 > p1) {
                            // violation of ordering
                            return False;
                        }
                    }
                }
            }
        }
    }

    return True;
}

//--------------------------------------------------------------
Bool Assign::isIsomorphic(MNodeClass *cl, BigInt *nsym, BigInt *esym, BigInt *nsym1)
{
    //  Check whether the current graph is the representive of 
    //  a isomorphic class.
    //  Returns (nsym, esym)
    //    nsym = symmetry factor by the permutation of nodes.
    //    esym = symmetry factor by the permutation of edge.
    //  If this graph is not a repressentative, then returns (0,0).

    int j, cmp, n, cln;
    BigInt ngelem;
    int *p;
    Bool invext;

    ngelem = mgraph->group->nElem();
#ifdef CHECK
    if (mgraph->nsym > 1 && ngelem <= 1) {
        printf("*** isIsomorphic: illegal group: "
               "ngelem=%ld, mgraph->sym=(%ld, %ld)\n",
               ngelem, mgraph->nsym, mgraph->esym);
        erEnd("Assign::isIsomorphic: illegal group");
    }
#endif
    if (ngelem == 0) {
        *nsym  = 1;
        *nsym1 = 1;
    } else {
        *nsym  = 0;
        *nsym1 = 0;
        for (j = 0; j < ngelem; j++) {
            // check the graph is the representative and count 'nsym'.

            //p = mgraph->group->nextElem();
            p = mgraph->group->elem[j];

            cmp = cmpPermGraph(p, cl);

            if (cmp < 0) {        // duplicated graph
#ifdef DEBUG1
                niso1++;
#endif
                return False;
            } else if(cmp == 0) { // not duplicated
                (*nsym)++;

                if (opt->values[GRCC_OPT_SymmInitial] || opt->values[GRCC_OPT_SymmFinal]) {
                    invext = True;
                    for (n = 0; n < nNodes; n++) {
                        cln = pnclass->nd2cl[n];
                        if (!isATExternal(pnclass->type[cln])) {
                            continue;
                        }
                        if (p[n] != n) {
                            invext = False;
                        }
                    }
                    if (invext) {
                        (*nsym1)++;
                    }
                } else {
                    (*nsym1)++;
                }
            }
        }
    }

    // calculate permutations of edges
    *esym = edgeSym();
    if (*esym < 1) {
#ifdef DEBUG1
        niso2++;
#endif
        return False;
    }

    return True;
}

//--------------------------------------------------------------
int Assign::cmpPermGraph(int *p, MNodeClass *cl)
{
    //  compare the graph with one permutated by 'p'.

    int n, cmp, n1, n2, p1, p2, j;
    ANode *nd1, *np1;
    // ANode *nd2, *np2;
    int    njn, jn[GRCC_MAXLEGS];
    int    njp, jp[GRCC_MAXLEGS];

#ifdef CHECK
    if (p == NULL) {
        erEnd("Assign::cmpPermGraph: p==NULL");
    }
#endif
#ifdef DEBUG1
    printf("cmpPermGraph:0: p=");
    prIntArray(nNodes, p, "\n");
#endif
    for (n = 0; n < nNodes; n++) {
        if (!isATExternal(pnclass->type[pnclass->nd2cl[n]])) {
            cmp = cmpNodes(n, p[n], cl);
            if (cmp != 0) {
#ifdef DEBUG1
                if (cmp < 0) { niso11++; }
#endif
                return cmp;
            }
        }
    }

    njn = 0;
    njp = 0;
    for (n1 = 0; n1 < nNodes; n1++) {
        p1  = p[n1];
        nd1 = nodes[n1];
        np1 = nodes[p1];
        for (n2 = n1; n2 < nNodes; n2++) {
            if (mgraph->adjMat[n1][n2] == 0) {
                continue;
            }

            p2  = p[n2];
            if (p1 == n1 && p2 == n2) {
                continue;
            }
            cmp = mgraph->adjMat[n1][n2] - mgraph->adjMat[p1][p2];
            if (cmp != 0) {
#ifdef DEBUG1
                if (cmp < 0) { niso12++; }
#endif
                return cmp;
            }

            njn = 0;
            njp = 0;
            for (j = 0; j < nd1->deg; j++) {
                if (nd1->anodes[j] == n2) {
                    jn[njn++] = getLegParticle(n1, j);
                }
            }

            for (j = 0; j < np1->deg; j++) {
                if (np1->anodes[j] == p2) {
                    jp[njp++] = getLegParticle(p1, j);
                }
            }

            cmp = njn - njp;
            if (cmp != 0) {
#ifdef DEBUG1
                if (cmp < 0) { niso13++; }
#endif
                return cmp;
            }

            // ignore ordering for multiple connections
            if (mgraph->adjMat[n1][n2] >= 2) {
                sorti(njn, jn);
                sorti(njn, jp);
            }

            for (j = 0; j < njn; j++) {
                cmp = jn[j] - jp[j];
                if (cmp != 0) {
#ifdef DEBUG1
                    if (cmp < 0) { niso14++; }
#endif
                    return cmp;
                }
            }
        }
    }

    return 0;
}

//--------------------------------------------------------------
int Assign::cmpNodes(int nd0, int nd1, MNodeClass *cn)
{
    //  Comarison of two nodes 'nd0' and 'nd1'
    //    Ordering is lexcographical (class, connection configuration)
    //

    int cmp;

    // Wether two nodes are in a same class or not.
    cmp = cn->ndcl[nd0] - cn->ndcl[nd1];
    if (cmp != 0) {
#ifdef DEBUG1
        if (cmp < 0) { niso111++; }
#endif
        return cmp;
    }

    // interaction
    cmp = nodes[nd0]->cand->ilist[0] - nodes[nd1]->cand->ilist[0];
#ifdef DEBUG1
    if (cmp < 0) { niso112++; }
#endif
    return cmp;
}

//--------------------------------------------------------------
BigInt Assign::edgeSym(void)
{
    //  calculate permutations of edges

    int lg[GRCC_MAXLEGS], lt[GRCC_MAXLEGS], lc;
    ANode *nd1;
    int n1, n2, j, k, mult;
    BigInt  esym;

    esym = 1;
    for (n1 = 0; n1 < nNodes; n1++) {
        nd1 = nodes[n1];
        for (n2 = n1; n2 < nNodes; n2++) {
            if (mgraph->adjMat[n1][n2] > 1) {
                lc = 0;
                for (j = 0; j < nd1->deg; j++) {
                    if (nd1->anodes[j] == n2) {
                        lg[lc] = 1;
                        lt[lc] = getLegParticle(n1, j);
                        lc++;
                    }
                }
                for (j = 0; j < lc-1; j++) {
                    if (lg[j] > 0) {
                        for (k = lc-1; k > j; k--) {
                            if (lt[j] == lt[k]) {
                                lg[j] += lg[k];
                                lg[k]  = 0;
                            }
                        }
                    }
                }

                // calculate symmetry factor
                for (j = 0; j < lc; j++) {
                    if (lg[j] < 1) {
                        continue;
                    }
                    mult = lg[j];
                    if (mult > 1) {
                        if (n1 == n2) {
                            // self-loop
                            esym *= ipow(2,mult/2)*factorial(mult/2);
                        } else {
                            esym *= factorial(mult);
                        }
                    }
                }
            }
        }
    }

    return esym;
}

#ifdef CHECK
//==============================================================
// check
//--------------------------------------------------------------
Bool Assign::checkCand(const char *msg)
{
    //  Check the consistency of Candidate data

    Bool  ok;
    ANode *na;
    NCand *nc;
    ECand *ec;
    int   n, lg, e;

    // check 'nodes->cand'
    ok = True;
    for (n = 0; n < nNodes; n++) {
        na = nodes[n];
        nc = na->cand;

        // check unassigned vertex
        if (nc->st == AS_UnAssLegs) {

        // check assigned vertex
        } else if (nc->st == AS_Assigned) {
            if (nc->nilist < 1) {
                printf("*** checkCand:7:%s:status (%d) of node %d says"
                       " interaction is assigned to %d but ilist=",
                       msg, nc->st, n, nc->st);
                prIntArray(nc->nilist, nc->ilist, "\n");
                ok = False;
            }

            for (lg = 0; lg < nc->deg; lg++) {
                e  = na->aedges[lg];
                ec = edges[e]->cand;
                if (ec->nplist != 1) {
                    printf("*** checkCand:8:%s:status (%d) of node %d says"
                          " interaction is assigned "
                          " but unassigned edge %d is found\n",
                          msg, nc->st, n, e);
                    ok = False;
                }
            }

        // external particle
        } else if (nc->st == AS_AssExt) {
            // pt  = nc->ilist[0];
            // *** pte = legEdgeParticle(n, 0, - pt);
        } else {
            printf("*** checkCand:10:%s:illegal status of node %d : %d",
                   msg, n, nc->st);
            ok = False;
        }
    }

    // check edges

    for (e = 0; e < nEdges; e++) {
        ec = edges[e]->cand;
        if (ec != NULL && ec->nplist < 1) {
            printf("*** checkCand:12:%s:illegal edge %d\n", msg, e);
            ok = False;
        }
    }

    if (!ok) {
        printf("*** checkCand:15:%s:illegal configuration\n", msg);
        prCand("checkCand");
        printf("*** checkCand:16:illegal configuration\n");
        erEnd("checkCand:16:illegal configuration");
    }
    return ok;
}

//--------------------------------------------------------------
void Assign::checkNode(int n, const char *msg)
{
    int j, it;

    for (j = 0; j < nodes[n]->cand->nilist; j++) {
        it = nodes[n]->cand->ilist[j];
        if (Abs(it) >= GRCC_MAXMINTERACT) {
            printf("*** %s: n=%d, j=%d, it=%d\n", msg, n, j, it);
            nodes[n]->cand->prNCand(msg);
            printf("\n");
            erEnd("checkNode:illegal it");
        }
    }
}
#endif // CHECK

//**************************************************************
// astack.cc
//==============================================================
// Assign particles to the edges and interactions to nodes.
// Completed graph data is saved in the form of EGraph.

//=============================================================
// stack operations
//--------------------------------------------------------------
void NStack::print(const char *msg)
{
    printf("  node=%d, deg=%d, st=%d, ilist=", noden, deg, st);
    prilist(nilist, ilist, msg);
}

//--------------------------------------------------------------
void EStack::print(const char *msg)
{
    printf("  edge=%d, det=%d, plist=", edgen, det);
    prilist(nplist, plist, msg);
}

//--------------------------------------------------------------
AStack::AStack(int nsize, int esize)
{
    int j;

    agraph  = NULL;

    nSize   = nsize;
    eSize   = esize;
    if (nSize > 0) {
        nStack = new NStack*[nSize];
    } else {
        nStack = NULL;
    }
    if (eSize > 0) {
        eStack = new EStack*[eSize];
    } else {
        eStack = NULL;
    }

    nStackP = 0;
    eStackP = 0;

    for (j = 0; j < nSize; j++) {
        nStack[j] = new NStack();
    }

    for (j = 0; j < eSize; j++) {
        eStack[j] = new EStack();
    }
}

//--------------------------------------------------------------
AStack::~AStack(void)
{
    int j;

    if (eStack != NULL) {
        for (j = eSize-1; j >= 0; j--) {
            if (eStack[j] != NULL) {
                delete eStack[j];
                eStack[j] = NULL;
            }
        }
        delete[] eStack;
        eStack = NULL;
    }

    if (nStack != NULL) {
        for (j = nSize-1; j >= 0; j--) {
            if (nStack[j] != NULL) {
                delete nStack[j];
                nStack[j] = NULL;
            }
        }
        delete[] nStack;
        nStack = NULL;
    }
}

//--------------------------------------------------------------
void AStack::setAGraph(Assign *ag)
{
    agraph = ag;
}

//--------------------------------------------------------------
void AStack::pushNode(int n)
{
    NCand  *nc;
    NStack *ns;
    int j;

#ifdef CHECK
    if (agraph == NULL) {
        erEnd("pushNode: agraph is not defined");
    }
#endif
    if (nStackP >= GRCC_MAXNSTACK) {
        erEnd("N-stack overflow (GRCC_MAXNSTACK)");
    }
    nc = agraph->nodes[n]->cand;
    if (nc->nilist >= GRCC_MAXMINTERACT) {
        erEnd("N-stack: too long list (GRCC_MAXMINTERACT)");
    }
    ns = nStack[nStackP];
    ns->noden  = n;
    ns->deg    = nc->deg;
    ns->st     = nc->st;
    ns->nilist = nc->nilist;
    for (j = 0; j < nc->nilist; j++) {
        ns->ilist[j] = nc->ilist[j];
    }
    nStackP++;
}

//--------------------------------------------------------------
void AStack::pushEdge(int e)
{
    ECand  *ec;
    EStack *es;
    int j;

#ifdef CHECK
    if (agraph == NULL) {
        erEnd("pushEdge: agraph is not defined");
    }
#endif
    if (eStackP >= GRCC_MAXESTACK) {
        erEnd("E-stack overflow (GRCC_MAXESTACK)");
    }
    ec = agraph->edges[e]->cand;
    if (ec->nplist >= GRCC_MAXMPARTICLES2) {
        erEnd("E-stack: Too long list (GRCC_MAXMPARTICLES)");
    }
    es = eStack[eStackP];
    es->edgen  = e;
    es->det    = ec->det;
    es->nplist = ec->nplist;
    for (j = 0; j < ec->nplist; j++) {
        es->plist[j] = ec->plist[j];
    }
    eStackP++;
}

//--------------------------------------------------------------
void AStack::checkPoint(CheckPt sav)
{
    sav[0] = nStackP;
    sav[1] = eStackP;
}

//--------------------------------------------------------------
void AStack::restoreNode(int spr)
{
    NStack *stc;
    int     sp, n, j;

    for (sp = nStackP-1; sp >= spr; sp--) {
        stc = nStack[sp];
        n = stc->noden;
        agraph->nodes[n]->cand->deg     = stc->deg;
        agraph->nodes[n]->cand->st      = stc->st;
        agraph->nodes[n]->cand->nilist  = stc->nilist;
        for (j = 0; j < stc->nilist; j++) {
            agraph->nodes[stc->noden]->cand->ilist[j]  = stc->ilist[j];
        }
    }
    nStackP = spr;
}

//--------------------------------------------------------------
void AStack::restoreEdge(int spr)
{
    EStack *stc;
    int     sp, e, j;

    for (sp = eStackP-1; sp >= spr; sp--) {
        stc = eStack[sp];
        e   = eStack[sp]->edgen;
        agraph->edges[e]->cand->det = stc->det;
        agraph->edges[e]->cand->nplist = stc->nplist;
        for (j = 0; j < stc->nplist; j++) {
            agraph->edges[e]->cand->plist[j] = stc->plist[j];
        }
    }
    eStackP = spr;
}

//--------------------------------------------------------------
void AStack::restore(CheckPt sav)
{
    restoreNode(sav[0]);
    restoreEdge(sav[1]);
}

#ifdef CHECK
//--------------------------------------------------------------
void AStack::restoreMsg(CheckPt sav, const char *msg)
{
    if (agraph == NULL) {
        erEnd("restore: agraph is not defined");
    }

    restore(sav);

    if (!agraph->checkCand("restore")) {
        printf("restore is called from %s\n", msg);
    }
}
#endif

//--------------------------------------------------------------
void AStack::prStack(void)
{
    int j;

    printf("+++ prStack : (%d, %d)", nStackP, eStackP);
    for (j = 0; j < nStackP; j++) {
        printf("N:%4d ", j);
        nStack[j]->print("\n");
    }
    for (j = 0; j < eStackP; j++) {
        printf("E:%4d ", j);
        eStack[j]->print("\n");
    }
}

//**************************************************************
// class Fraction
//==============================================================
Fraction::Fraction(BigInt n, BigInt d)
{
    BigInt g;

    g     = gcd(n, d);
    num   = n/g;
    den   = d/g;
    ratio = Real(n)/Real(d);
}

//--------------------------------------------------------------
void Fraction::print(const char *msg)
{
    double err = Abs(Real(num)/Real(den) - ratio);

    if (err > GRCC_FRACERROR) {
        printf("%ld/%ld(%g)(overflow)%s", num, den, ratio, msg);
    } else {
        printf("%ld/%ld(%g)%s", num, den, ratio, msg);
    }
}

//--------------------------------------------------------------
void Fraction::setValue(BigInt n, BigInt d) 
{ 
    num = n;  
    den = d; 
    ratio = Real(n)/Real(d); 
}

//--------------------------------------------------------------
void  Fraction::setValue(Fraction &f) 
{ 
    num   = f.num;  
    den   = f.den; 
    ratio = f.ratio; 
}

//--------------------------------------------------------------
BigInt Fraction::gcd(BigInt n0, BigInt n1)
{
    BigInt nn[2], r;
    int    nc;

    nn[0] = Abs(n0);
    nn[1] = Abs(n1);
    nc    = 0;

    while (1) {
        r = nn[nc] % nn[1-nc];
        if (r == 0) {
            return nn[1-nc];
        }
        nn[nc] = r;
        nc = 1 - nc;
    }
}

//--------------------------------------------------------------
void Fraction::normal(void)
{
    BigInt g;

    if (den == 0) {
        erEnd("Fraction: den==0");
    }
    g   = gcd(num, den);
    num /= g;
    den /= g;
    if (den < 0) {
        num = - num;
        den = - den;
    }
}

//--------------------------------------------------------------
void Fraction::add(BigInt n, BigInt d)
{
    BigInt g, d1;

    if (d < 0) {
        n = -n;
        d = -d;
    }
    if (den < 0) {
        num = - num;
        den = - den;
    }
    g    = gcd(den, d);
    d1   = d/g;
    num  = num * d1 + n * (den/g);
    den  = d1*den;
    g    = gcd(num, den);
    num /= g;
    den /= g;
    ratio += Real(n)/Real(d);
}

//--------------------------------------------------------------
void Fraction::add(Fraction f)
{
    double r = ratio + f.ratio;

    add(f.num, f.den);
    ratio = r;
}

//--------------------------------------------------------------
void Fraction::sub(Fraction f)
{
    double r = ratio - f.ratio;

    add(-f.num, f.den);
    ratio = r;
}

//--------------------------------------------------------------
Bool Fraction::isEq(Fraction f)
{
    return (num == f.num && den == f.den);
}

//**************************************************************
// common.cc
//==============================================================
static void erEnd(const char *msg)
{
    if (erExit != NULL) {
        (*erExit)(msg, erExitArg);
    }
    fprintf(GRCC_Stderr, "*** Error : %s\n", msg);
    GRCC_ABORT();
}

//------------------------------------------------------------
static Bool nextPart(int nelem, int nclist, int *clist, int *nl, int *r)
{
    int rem, pn, j;

    if (*r < 0) {
        *r = 0;
        rem = nelem;
        for (j = 0; j < nclist; j++) {
            nl[j] = rem/clist[j];
            rem -= nl[j]*clist[j];
        }
        if (rem == 0) {
            return True;
        }
    } else {
        rem = 0;
    }
    for (int c = 0; c < 100; c++) {
        rem += nl[nclist-1]*clist[nclist-1];
        for (pn = nclist-2; pn >= 0 && nl[pn] == 0; pn--) {
            ;
        }
        if (pn < 0) {
            return False;
        }
        rem += clist[pn];
        nl[pn]--;
        for (j = pn+1; j < nclist; j++) {
            nl[j] = rem/clist[j];
            rem -= nl[j]*clist[j];
        }
        if (rem == 0) {
            return True;
        }
    }
    printf("*** nextPart : too many repetition\n");
    return False;
}

//------------------------------------------------------------
static int   *intdup(int n, int *a)
{
    int *r, j;

    r = new int[n];
    for (j = 0; j < n; j++) {
        r[j] = a[j];
    }
    return r;
}

//------------------------------------------------------------
static int   *delintdup(int *a)
{
    if (a != NULL) {
        delete[] a;
    }
    return NULL;
}

//------------------------------------------------------------
static void   prilist(int n, const int *a, const char *msg)
{
    printf("[");
    for (int j = 0; j < n; j++) {
        if (j!=0) printf(", ");
        printf("%d", a[j]);
    }
    printf("]%s", msg);
}

//------------------------------------------------------------
static int nextPerm(int nelem, int nclass, int *cl, int *r, int *q, int *p, int count)
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
#ifdef CHECK
        if (j != nelem) {
            erEnd("inconsistent # elements");
        }
#endif
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
static BigInt factorial(int n)
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
static BigInt ipow(int n, int p)
{
    int r, j;

    r = 1;
    for (j = 0; j < p; j++) {
        r *= n;
    }
    return r;
}

//------------------------------------------------------------
static int   *newArray(int size, int val)
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
static int   *deleteArray(int *a)
{
    // memory allocation of an array

    delete[] a;
    return NULL;
}

//------------------------------------------------------------
static int  **newMat(int n0, int n1, int val)
{
    int **m, j, k;

    m = new int*[n0];
    for (j = 0; j < n0; j++) {
        m[j] = new int[n1];
        for (k = 0; k < n1; k++) {
            m[j][k] = val;
        }
    }
    return m;
}

//------------------------------------------------------------
static int **deleteMat(int **m, int n0)
{
    int j;

    for (j = n0-1; j >= 0; j--) {
        delete[] m[j];
    }
    delete[] m;

    return NULL;
}

//------------------------------------------------------------
static void bsort(int n, int *ord, int *a)
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
}

//--------------------------------------------------------------
static void prMomStr(int mom, const char *ms, int mn)
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
static void prIntArray(int n, int *p, const char *msg)
{

    int j;

    printf("[");
    for (j = 0; j < n; j++) {
        if (j!=0) printf(", ");
        printf("%2d", p[j]);
    }
    printf("]%s", msg);
}

//--------------------------------------------------------------
static void prIntArrayErr(int n, int *p, const char *msg)
{

    int j;

    fprintf(GRCC_Stderr, "[");
    for (j = 0; j < n; j++) {
        if (j!=0) fprintf(GRCC_Stderr, ", ");
        fprintf(GRCC_Stderr, "%2d", p[j]);
    }
    fprintf(GRCC_Stderr, "]%s", msg);
}

//--------------------------------------------------------------
static void sortrec(int l, int r, int *a)
{
    // quick sort : taken from N. Wirth
    int i,j, x, w;

    i = l;
    j = r;
    x = a[(l+r)/2];
    do {
        while(a[i] < x) i++;
        while(x < a[j]) j--;
        if (i <= j) {
            w = a[i];  a[i] = a[j];  a[j] = w;
            i++;  j--;
        }
    } while (i < j);
    if (l < j) sortrec(l, j, a);
    if (i < r) sortrec(i, r, a);
}

//--------------------------------------------------------------
static void sorti(int size, int *a)
{
    sortrec(0, size-1, a);    
}

//------------------------------------------------------------
static int sortb(int n, int *a)
{
    int i, j, t, nswap;

    nswap = 0;
    for (j = n-1; j > 0; j--) {
        for (i = 0; i < j; i++) {
            if(a[i+1] < a[i]) {
                t = a[i];
                a[i] = a[i+1];
                a[i+1] = t;
                nswap++;
            }
        }
    }

    // the sign of the permutation is (-1)^{nswap}
    return nswap;
}

#ifdef CHECK
//--------------------------------------------------------------
static Bool isIn(int n, int *a, int v)
{
    int j;

    for (j = 0; j < n; j++) {
        if (a[j] == v) {
            return True;
        }
    }
    return False;
}
#endif

//--------------------------------------------------------------
static int intSetAdd(int n, int *a, int v, const int size)
{
    // Add 'v' into 'a'.  
    // 'n' is the current # of element.
    // 'a' is assumed sorted without duplicated elements.
    // Size of 'a' should be large enough.

    int j, k;

    if (n >= size) {
        fprintf(GRCC_Stderr, "*** intSetAdd : array out of range (>%d)\n", size);
        erEnd("intSetAdd : array out of range (GRCC_MAXPSLIST)");
    }
    for (j = 0; j < n; j++) {
        if (a[j] > v) {
            break;
        } else if (a[j] == v) {
            return n;
        }
    }

    // 'j' can be equal to 'n'
    for (k = n-1; k >= j; k--) {
        a[k+1] = a[k];
    }
    a[j] = v;
    return n+1;
}

//--------------------------------------------------------------
static int intSListAdd(int n, int *a, int v, const int size)
{
    // Add 'v' into 'a'.  
    // 'n' is the current # of element.
    // 'a' is assumed sorted without duplicated elements.
    // Size of 'a' should be large enough.

    int j, k;

    if (n >= size) {
        fprintf(GRCC_Stderr, "*** intSListAdd : array out of range (>%d)\n", size);
        erEnd("intSListAdd : array out of range");
    }
    for (j = 0; j < n; j++) {
        if (a[j] >= v) {
            break;
        }
    }

    // 'j' can be equal to 'n'
    for (k = n-1; k >= j; k--) {
        a[k+1] = a[k];
    }
    a[j] = v;
    return n+1;
}

//--------------------------------------------------------------
static int    cmpArray(int na, int *a, int nb, int *b)
{
    int j, cmp;

    cmp = na - nb;
    if (cmp != 0) {
        return cmp;
    }
    for (j = 0; j < na; j++) {
        cmp = a[j] - b[j];
        if (cmp != 0) {
            return cmp;
        }
    }
    return 0;
}

//--------------------------------------------------------------
static Bool   leqArray(int n, int *a, int *b)
{
    int j;

    for (j = 0; j < n; j++) {
        if (a[j] > b[j]) {
            return False;
        }
    }
    return True;
}

//--------------------------------------------------------------
// convert list to sorted list
static int   toSList(int n, int *a)
{
    sorti(n, a);
    return n;
}

//--------------------------------------------------------------
// as set operation assuming a and b are sorted with duplication
//     p := a \ b;  q := a \cap b;  r := b \ a;
// p, q, r are sorted without duplication
static void   listDiff(int na, int *a, int nb, int *b, int *np, int *p, int *nq, int *q, int *nr, int *r)
{
    int ja, jb;

    *np = *nq = *nr = 0;
    ja = jb = 0;
    while (ja < na && jb < nb) {
        while (ja < na && a[ja] < b[jb]) {
            p[(*np)++] = a[ja++];
        }
        while (jb < nb && b[jb] < a[ja]) {
            r[(*nr)++] = b[jb++];
        }
        if (ja < na && jb < nb && a[ja] == b[jb]) {
            q[(*nq)++] = a[ja++];
            jb++; 
        }
    }
    for (; ja < na; ja++) {
        p[(*np)++] = a[ja];
    }
    for (; jb < nb; jb++) {
        r[(*nr)++] = b[jb];
    }
}

//--------------------------------------------------------------
static int isSubSList(int na, int *a, int nb, int *b)
{
    int ja, jb;

    ja = jb = 0;
    while (ja < na && jb < nb) {
        for ( ; jb < nb && b[jb] < a[ja]; jb++) {
            ;
        }
        if (jb >= nb || b[jb] > a[ja]) {
            return False;
        }
        for ( ; jb < nb && ja < na && b[jb] == a[ja]; jb++, ja++) {
            ;
        }
    }
    return (ja >= na);
}

//--------------------------------------------------------------
static int subtrSet(int na, int *a, int nb, int *b, int *c, int size)
{
    // set subtraction 'c' := 'a' - 'b'.  
    // 'a' and 'b' are sorted lists (not always unique)
    // 'c' is the set (sorted and unique elements)

    int ja, jb, jc;

    jc = 0;
    ja = jb = 0;
    while (ja < na && jb < nb) {
        while (ja < na && a[ja] < b[jb]) {
            if (jc == 0 || c[jc-1] != a[ja]) {
                if (jc >= size) {
                    erEnd("subsutSet: too small size of array (GRCC_MAXPSLIST)");
                }
                c[jc++] = a[ja];
            }
            ja++;
        }
        while (jb < nb && b[jb] < a[ja]) {
            ;
        }
        if (ja < na && jb < nb && a[ja] == b[jb]) {
            ja++;
            jb++; 
        }
    }
    for (; ja < na; ja++) {
        if (jc == 0 || c[jc-1] != a[ja]) {
            c[jc++] = a[ja];
        }
    }
    return jc;
}

// } ) ]
