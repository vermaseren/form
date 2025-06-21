//	#[ Includes : diawrap.cc

extern "C" {
#include "form3.h"
}

#include "grccparam.h"
#include "grcc.h"
 
#define MAXPOINTS 120

typedef struct ToPoTyPe {
	WORD *vert;
	WORD *vertmax;
	Options *opt;
	int cldeg[MAXPOINTS], clnum[MAXPOINTS], clext[MAXPOINTS];
	int cmind[MAXLEGS+1],cmaxd[MAXLEGS+1];
	int ncl, nvert;
} TOPOTYPE;

static void ProcessDiagram(EGraph *eg, void *ti);
static int processVertex(TOPOTYPE *TopoInf, int pointsremaining, int level);

//	#] Includes : 
//	#[ LoadModel :

int LoadModel(MODEL *m)
{
//
//	First check whether there was a model already.
//	In the Kaneko setup there can be only one at the same time.
//	Hence if there was one we need to remove it first.
//	Unless of course, it was already the model we want.
//
	if ( m->grccmodel != NULL ) return(0);

	int i,j,k;
//
//	First the information that goes into the Model struct.
//	Note that new Model takes over (does not copy) minp.cnamlist.
//
	MInput minp;
	PInput pinp;
	IInput iinp;
	if ( m->ncouplings > GRCC_MAXNCPLG ) {
		MesPrint("Too many coupling constants in model. Current limit is %d.",(WORD)GRCC_MAXNCPLG);
		MesPrint("Suggestion: recompile Form with a larger value for GRCC_MAXNCPLG");
		return(-1);
	}
	minp.defpart = GRCC_DEFBYCODE;
	minp.name = (char *)(m->name);
	minp.ncouple = m->ncouplings;
	for ( i = 0; i < GRCC_MAXNCPLG; i++ ) minp.cnamlist[i] = NULL;
	for ( i = 0; i < minp.ncouple; i++ )
		minp.cnamlist[i] = (char *)(VARNAME(symbols,m->couplings[i]));
	Model *mdl = new Model(&minp);
//
//	Now the particles
//
	for ( i = 0; i < m->nparticles; i++ ) {
		if ( minp.defpart == GRCC_DEFBYCODE ) {
			pinp.name = NULL;
			pinp.aname = NULL;
			pinp.pcode = m->vertices[i]->particles[0].number;
			pinp.acode = m->vertices[i]->particles[1].number;
			switch ( m->vertices[i]->particles[0].spin ) {
			  case 1:
				pinp.ptypec = GRCC_PT_Scalar;
			  break;
			  case -1:
				pinp.ptypec = GRCC_PT_Ghost;
			  break;
			  case 2:
				if ( m->vertices[i]->particles[0].type == 0 )
					pinp.ptypec = GRCC_PT_Majorana;
				else
					pinp.ptypec = GRCC_PT_Undef;
			  break;
			  case -2:
				if ( m->vertices[i]->particles[0].type == 0 )
					pinp.ptypec = GRCC_PT_Majorana;
				else
					pinp.ptypec = GRCC_PT_Dirac;
			  break;
			  case 3:
				pinp.ptypec = GRCC_PT_Vector;
			  break;
			  default:
				pinp.ptypec = GRCC_PT_Undef;
			  break;
			}
		}
		else {
			pinp.name   = (char *)(VARNAME(functions,m->vertices[i]->particles[0].number));
			pinp.aname  = (char *)(VARNAME(functions,m->vertices[i]->particles[1].number));
			switch ( m->vertices[i]->particles[0].spin ) {
			  case 1:
				pinp.ptypen = "scalar";
			  break;
			  case -1:
				pinp.ptypen = "ghost";
			  break;
			  case 2:
				if ( m->vertices[i]->particles[0].type == 0 )
					pinp.ptypen = "majorana";
				else
					pinp.ptypen = "undef";
			  break;
			  case -2:
				if ( m->vertices[i]->particles[0].type == 0 )
					pinp.ptypen = "majorana";
				else
					pinp.ptypen = "dirac";
			  break;
			  case 3:
				pinp.ptypen = "vector";
			  break;
			  default:
				pinp.ptypen = "undef";
			  break;
			}
		}
		pinp.extonly = m->vertices[i]->externonly;
		mdl->addParticle(&pinp);
	}
	mdl->addParticleEnd();
//
//	Now the vertices
//
	for ( i = m->nparticles; i < m->invertices; i++ ) {
		VERTEX *v = m->vertices[i];
		if ( minp.defpart == GRCC_DEFBYCODE ) {
			iinp.icode = NODEFUNCTION+i;
			iinp.name = NULL;
		}
		else {
			iinp.name = "node_";
		}
		iinp.nplistn = v->nparticles;
		for ( j = 0; j < iinp.nplistn; j++ ) {
			if ( minp.defpart == GRCC_DEFBYCODE ) {
				iinp.plistc[j] = v->particles[j].number;
			}
			else {
				iinp.plistn[j] = (char *)(VARNAME(functions,v->particles[j].number));
			}
		}
//
//		We need a properly ordered list of coupling constants
//		The ordered list is in m->couplings.
//		For each vertex they are in v->couplings.
//
//		iinp.cvallist = (int *)Malloc1(m->ncouplings*sizeof(int),"couplings");

		for ( j = 0; j < m->ncouplings; j++ ) {
			iinp.cvallist[j] = 0;
			for ( k = 0; k < v->ncouplings; k += 2 ) {
				if ( v->couplings[k] == m->couplings[j] ) {
					iinp.cvallist[j] = v->couplings[k+1];
					break;
				}
			}
		}
		mdl->addInteraction(&iinp);
	}
	mdl->addInteractionEnd();
	m->grccmodel = (void *)mdl;
	return(0);
}

//	#] LoadModel : 
//	#[ ConvertParticle :

int ConvertParticle(Model *model,int formnum)
{
//
//  Returns the grcc number of the particle, because grcc does not convert
//
    int i;
    for ( i = 0; i < model->nParticles; i++ ) {
        if ( model->particles[i]->pcode == formnum ) { return(i); }
        else if ( model->particles[i]->acode == formnum ) { return(-i); }
    }
    MesPrint("Particle %d not found in model %s",formnum,model->name);
    Terminate(-1);
    return(0);
}

//	#] ConvertParticle : 
//	#[ ReConvertParticle :

int ReConvertParticle(Model *model,int grccnum)
{
//
//  Returns the grcc number of the particle, because grcc does not convert
//
	if ( grccnum < 0 ) { return(model->particles[-grccnum]->acode); }
	else { return(model->particles[grccnum]->pcode); }
}

//	#] ReConvertParticle : 
//	#[ numParticle :

int numParticle(MODEL *m,WORD n)
{
	int i;
	for ( i = 0; i < m->nparticles; i++ ) {
		if ( m->vertices[i]->particles[0].number == n ) return(i);
		if ( m->vertices[i]->particles[1].number == n ) return(i);
	}
	MesPrint("numParticle: particle %d not found in model",n);
	Terminate(-1);
	return(-1);
}

//	#] numParticle : 
//	#[ ProcessDiagram :

void ProcessDiagram(EGraph *eg, void *ti)
{
//
//	This is the routine that gets a complete diagram and passes it on
//	to Form (Generator) for further algebraic manipulations.
//	The term is picked up from AT.diaterm and a new term is constructed
//	in the workspace.
//
	GETIDENTITY
	TERMINFO *info = (TERMINFO *)ti;

	if ( ( info->flags & TOPOLOGIESONLY ) == TOPOLOGIESONLY ) return;

	WORD *term = info->term, *newterm, *oldworkpointer = AT.WorkPointer;
	WORD *tdia = term + info->diaoffset;
	WORD *tail = tdia + tdia[1];
	WORD *tend = term + *term;
	WORD *fill, *startfill, *cfill, *afill;
	int i, j, intr;
	Model *model = (Model *)info->currentModel;
	MODEL *m = (MODEL *)info->currentMODEL;
	int numlegs, vect, edge;

	newterm = term + *term;
	for ( i = 1; i < info->diaoffset; i++ ) newterm[i] = term[i];
	fill = newterm + info->diaoffset;
//
//	Now get the nodes
//
	if ( ( info->flags & NONODES ) == 0 ) {
	  for ( i = 0; i < eg->nNodes; i++ ) {
//
//		node_(number,coupling,particle_1(momentum_1),...,particle_n(momentum_n))
//
		numlegs = eg->nodes[i]->deg;
		startfill = fill;
		*fill++ = NODEFUNCTION;
		*fill++ = 0;
		FILLFUN(fill)
		*fill++ = -SNUMBER; *fill++ = i+1;
//
//		Now we put the coupling constants. This is done inside the
//		function for when we want to work with counterterms.
//
		if ( !eg->isExternal(i) ) {
			afill = fill; *fill++ = 0; *fill++ = 1; FILLARG(fill)
			cfill = fill; *fill++ = 0;
			intr = eg->nodes[i]->intrct;
			for ( j = 0; j < model->interacts[intr]->nclist; j++ ) {
				if ( model->interacts[intr]->clist[j] != 0 ) {
					*fill++ = SYMBOL; *fill++ = 4;
					*fill++ = m->couplings[j];
					*fill++ = model->interacts[intr]->clist[j];
				}
			}
			*fill++ = 1; *fill++ = 1; *fill++ = 3;
			*cfill = fill - cfill;
			*afill = fill - afill;
		}
		else {
			*fill++ = -SNUMBER;
			*fill++ = 1;
		}
//
//		Now the particles and their momenta.
//
		for ( j = 0; j < numlegs; j++ ) {
			*fill++ = ARGHEAD+FUNHEAD+6;
			*fill++ = 0;
			FILLARG(fill)
			edge = eg->nodes[i]->edges[j];
			vect = ABS(edge)-1;
			*fill++ = 6+FUNHEAD;
			int a;
			if ( edge < 0 ) { a = ReConvertParticle(model,-eg->edges[vect]->ptcl); }
			else {            a = ReConvertParticle(model,eg->edges[vect]->ptcl); }
			*fill++ = a;
			*fill++ = FUNHEAD+2;
			FILLFUN(fill)
			*fill++ = edge < 0 ? -MINVECTOR: -VECTOR;
			if ( numlegs == 1 || vect < info->numextern ) { // Look up in set of external momenta
				*fill++ = SetElements[Sets[info->externalset].first+vect];
			}
			else { // Look up in set of internal momenta set
				*fill++ = SetElements[Sets[info->internalset].first+(vect-eg->nExtern)];
			}
			*fill++ = 1; *fill++ = 1; *fill++ = 3;
		}
		startfill[1] = fill-startfill;
	  }
	}
	if ( ( info->flags & WITHEDGES ) == WITHEDGES ) {
		for ( i = 0; i < eg->nEdges; i++ ) {
			int n1 = eg->edges[i]->nodes[0];
			int n2 = eg->edges[i]->nodes[1];
//			int l1 = eg->edges[i]->nlegs[0];
//			int l2 = eg->edges[i]->nlegs[1];

			startfill = fill;
			*fill++ = EDGE;
			*fill++ = 0;
			FILLFUN(fill)
			*fill++ = -SNUMBER; *fill++ = i+1; // number of the edge
//
			*fill++ = ARGHEAD+FUNHEAD+6;
			*fill++ = 0;
			FILLARG(fill)
			*fill++ = 6+FUNHEAD;
			int a = ReConvertParticle(model,eg->edges[i]->ptcl);
			*fill++ = a;
			*fill++ = FUNHEAD+2;
			FILLFUN(fill)
				*fill++ = -VECTOR;
			// Look up in set of internal momenta set
			if ( i < info->numextern ) { // Look up in set of external momenta
				*fill++ = SetElements[Sets[info->externalset].first+i];
			}
			else { // Look up in set of internal momenta set
				*fill++ = SetElements[Sets[info->internalset].first+(i-eg->nExtern)];
			}
			*fill++ = 1; *fill++ = 1; *fill++ = 3;
//
			*fill++ = -SNUMBER; *fill++ = n1+1; // number of the node from
			*fill++ = -SNUMBER; *fill++ = n2+1; // number of the node to
			startfill[1] = fill - startfill;
		}
	}
	if ( ( info->flags & WITHBLOCKS ) == WITHBLOCKS ) {
		for ( i = 0; i < eg->econn->nblocks; i++ ) {
			startfill = fill;
			*fill++ = BLOCK;
			*fill++ = 0;
			FILLFUN(fill);
			*fill++ = -SNUMBER;
			*fill++ = i+1;
			*fill++ = -SNUMBER;
			*fill++ = eg->econn->blocks[i].loop;
//
//			Now we have to make a list of all nodes inside this block
//
			int bnodes[GRCC_MAXNODES], k;
			WORD *argfill = fill, *funfill;
			*fill++ = 0; *fill++ = 0; FILLARG(fill)
			*fill++ = 0;
			for ( k = 0; k < GRCC_MAXNODES; k++ ) bnodes[k] = 0;
			for ( k = 0; k < eg->econn->blocks[i].nmedges; k++ ) {
				bnodes[eg->econn->blocks[i].edges[k][0]] = 1;
				bnodes[eg->econn->blocks[i].edges[k][1]] = 1;
			}
			for ( k = 0; k < GRCC_MAXNODES; k++ ) {
				if ( bnodes[k] == 0 ) continue;
//
//				Now we put the node inside this argument.
//
				funfill = fill;
				*fill++ = NODEFUNCTION;
				*fill++ = 0;
				FILLFUN(fill)
				*fill++ = -SNUMBER; *fill++ = k+1;
				numlegs = eg->nodes[k]->deg;
				for ( j = 0; j < numlegs; j++ ) {
					edge = eg->nodes[k]->edges[j];
					vect = ABS(edge)-1;
					*fill++ = -VECTOR;
					if ( numlegs == 1 || vect < info->numextern ) { // Look up in set of external momenta
						*fill++ = SetElements[Sets[info->externalset].first+vect];
					}
					else { // Look up in set of internal momenta set
						*fill++ = SetElements[Sets[info->internalset].first+(vect-info->numextern)];
					}
				}
				funfill[1] = fill-funfill;
			}
			*fill++ = 1; *fill++ = 1; *fill++ = 3;
			*argfill = fill - argfill;
			argfill[ARGHEAD] = argfill[0] - ARGHEAD;
			startfill[1] = fill-startfill;
		}
	}
	if ( ( info->flags & WITHONEPI ) == WITHONEPI ) {
		for ( i = 0; i < eg->econn->nopic; i++ ) {
			startfill = fill;
			*fill++ = ONEPI;
			*fill++ = 0;
			FILLFUN(fill);
			*fill++ = -SNUMBER;
			*fill++ = i+1;
			*fill++ = -ONEPI;
			for ( j = 0; j < eg->econn->opics[i].nnodes; j++ ) {
				*fill++ = -SNUMBER;
				*fill++ = eg->econn->opics[i].nodes[j]+1;
			}
			startfill[1] = fill-startfill;
		}
	}
//
//	Topology counter. We have exaggerated a bit with the eye on the far future.
//
	if ( info->numtopo-1 < MAXPOSITIVE ) {
		*fill++ = TOPO; *fill++ = FUNHEAD+2; FILLFUN(fill)
		*fill++ = -SNUMBER; *fill++ = (WORD)(info->numtopo-1);
	}
	else if ( info->numtopo-1 < FULLMAX-1 ) {
		*fill++ = TOPO; *fill++ = FUNHEAD+ARGHEAD+4; FILLFUN(fill)
		*fill++ = ARGHEAD+4; *fill++ = 0; FILLARG(fill)
		*fill++ = 4;
		*fill++ = (WORD)((info->numtopo-1) & WORDMASK);
		*fill++ = 1; *fill++ = 3;
	}
	else {	// for now: science fiction
		*fill++ = TOPO; *fill++ = FUNHEAD+ARGHEAD+6; FILLFUN(fill)
		*fill++ = ARGHEAD+6; *fill++ = 0; FILLARG(fill)
		*fill++ = 6; *fill++ = (WORD)((info->numtopo-1) >> BITSINWORD);
		*fill++ = (WORD)((info->numtopo-1) & WORDMASK);
		*fill++ = 0; *fill++ = 1; *fill++ = 5;
	}
//
//	Symmetry factors. We let Normalize do the multiplication.
//
	if ( eg->nsym != 1 ) {
		*fill++ = SNUMBER; *fill++ = 4; *fill++ = (WORD)eg->nsym; *fill++ = -1;
	}
	if ( eg->esym != 1 ) {
		*fill++ = SNUMBER; *fill++ = 4; *fill++ = (WORD)eg->esym; *fill++ = -1;
	}
	if ( eg->extperm != 1 ) {
		*fill++ = SNUMBER; *fill++ = 4; *fill++ = (WORD)eg->extperm; *fill++ = 1;
	}
//
//	finish it off
//
	while ( tail < tend ) *fill++ = *tail++;
	if ( eg->fsign < 0 ) fill[-1] = -fill[-1];
	*newterm = fill - newterm;
	AT.WorkPointer = fill;

//	MesPrint("<> %a",newterm[0],newterm);

	Generator(BHEAD newterm,info->level);
	AT.WorkPointer = oldworkpointer;
}

//	#] ProcessDiagram : 
//	#[ fendMG :

Bool fendMG(EGraph *eg, void *ti)
{
	DUMMYUSE(eg);
	DUMMYUSE(ti);
	return True;
}

//	#] fendMG : 
//	#[ ProcessTopology :

Bool ProcessTopology(EGraph *eg, void *ti)
{
//
//	This routine is called for each new topology.
//	New convention: return True;  generate the corresponding diagrams if needed
//	                return False; skip diagram generation (when asked for).
//
	TERMINFO *info = (TERMINFO *)ti;
#ifdef WITHEARLYVETO
	if ( ( ( info->flags & CHECKEXTERN ) == CHECKEXTERN ) && info->currentMODEL != NULL ) {
		int i, j;
		int numlegs, vect, edge;
		for ( i = 0; i < eg->nNodes; i++ ) {
			if ( eg->isExternal(i) ) continue;
			numlegs = eg->nodes[i]->deg;
			for ( j = 0; j < numlegs; j++ ) {
				edge = eg->nodes[i]->edges[j];
				vect = ABS(edge)-1;
				if ( vect < info->numextern && info->legcouple[vect][numlegs] == 0 ) {

//					This cannot be.

					info->numtopo++;
					return False;
				}
			}
		}
	}
#endif
	if ( ( info->flags & TOPOLOGIESONLY ) == 0 ) {
		info->numtopo++;
		return True;
	}
//
//	Now we are just generating topologies.
//
	GETIDENTITY
	WORD *term = info->term, *newterm, *oldworkpointer = AT.WorkPointer;
	WORD *tdia = term + info->diaoffset;
	WORD *tail = tdia + tdia[1];
	WORD *tend = term + *term;
	WORD *fill, *startfill;
	Model *model = (Model *)info->currentModel;
	MODEL *m = (MODEL *)info->currentMODEL;
	int i, j;
	int numlegs, vect, edge;

	newterm = term + *term;
	for ( i = 1; i < info->diaoffset; i++ ) newterm[i] = term[i];
	fill = newterm + info->diaoffset;
//
//	Now get the nodes
//
	for ( i = 0; i < eg->nNodes; i++ ) {
//
//		node_(number,momentum_1,...,momentum_n)
//
		numlegs = eg->nodes[i]->deg;
		startfill = fill;
		*fill++ = NODEFUNCTION;
		*fill++ = 0;
		FILLFUN(fill)
		*fill++ = -SNUMBER; *fill++ = i+1;
		if ( model != NULL && m != NULL ) {
			if ( !eg->isExternal(i) ) {
				WORD *afill = fill; *fill++ = 0; *fill++ = 1; FILLARG(fill)
				WORD *cfill = fill; *fill++ = 0;
				int cpl = 2*eg->nodes[i]->extloop+eg->nodes[i]->deg-2;
				*fill++ = SYMBOL; *fill++ = 4;
				*fill++ = m->couplings[0];
				*fill++ = cpl;
				*fill++ = 1; *fill++ = 1; *fill++ = 3;
				*cfill = fill - cfill;
				*afill = fill - afill;
				if ( *afill == ARGHEAD+8 && afill[ARGHEAD+4] == 1 ) {
					fill = afill; *fill++ = -SYMBOL; *fill++ = afill[ARGHEAD+3];
				}
			}
			else {
				*fill++ = -SNUMBER;
				*fill++ = 1;
			}
		}
//
//		Now the momenta.
//
		for ( j = 0; j < numlegs; j++ ) {
			edge = eg->nodes[i]->edges[j];
			vect = ABS(edge)-1;
			*fill++ = edge < 0 ? -MINVECTOR: -VECTOR;
			if ( numlegs == 1 || vect < info->numextern ) { // Look up in set of external momenta
				*fill++ = SetElements[Sets[info->externalset].first+vect];
			}
			else { // Look up in set of internal momenta set
				*fill++ = SetElements[Sets[info->internalset].first+(vect-info->numextern)];
			}
		}
		startfill[1] = fill-startfill;
	}
	if ( ( info->flags & WITHEDGES ) == WITHEDGES ) {
		for ( i = 0; i < eg->nEdges; i++ ) {
			int n1 = eg->edges[i]->nodes[0];
			int n2 = eg->edges[i]->nodes[1];
//			int l1 = eg->edges[i]->nlegs[0];
//			int l2 = eg->edges[i]->nlegs[1];
			startfill = fill;
			*fill++ = EDGE;
			*fill++ = 0;
			FILLFUN(fill)
			*fill++ = -SNUMBER; *fill++ = i+1; // number of the edge
//
			*fill++ = -VECTOR;
			if ( i < info->numextern ) { // Look up in set of external momenta
				*fill++ = SetElements[Sets[info->externalset].first+i];
			}
			else { // Look up in set of internal momenta set
				*fill++ = SetElements[Sets[info->internalset].first+(i-eg->nExtern)];
			}
//
			*fill++ = -SNUMBER; *fill++ = n1+1; // number of the node from
			*fill++ = -SNUMBER; *fill++ = n2+1; // number of the node to
			startfill[1] = fill - startfill;
		}
	}
//
//	Block information
//
	if ( ( info->flags & WITHBLOCKS ) == WITHBLOCKS ) {
		for ( i = 0; i < eg->econn->nblocks; i++ ) {
			startfill = fill;
			*fill++ = BLOCK;
			*fill++ = 0;
			FILLFUN(fill);
			*fill++ = -SNUMBER;
			*fill++ = i+1;
			*fill++ = -SNUMBER;
			*fill++ = eg->econn->blocks[i].loop;
//
//			Now we have to make a list of all nodes inside this block
//
			int bnodes[GRCC_MAXNODES], k;
			WORD *argfill = fill, *funfill;
			*fill++ = 0; *fill++ = 0; FILLARG(fill)
			*fill++ = 0;
			for ( k = 0; k < GRCC_MAXNODES; k++ ) bnodes[k] = 0;
			for ( k = 0; k < eg->econn->blocks[i].nmedges; k++ ) {
				bnodes[eg->econn->blocks[i].edges[k][0]] = 1;
				bnodes[eg->econn->blocks[i].edges[k][1]] = 1;
			}
			for ( k = 0; k < GRCC_MAXNODES; k++ ) {
				if ( bnodes[k] == 0 ) continue;
//
//				Now we put the node inside this argument.
//
				funfill = fill;
				*fill++ = NODEFUNCTION;
				*fill++ = 0;
				FILLFUN(fill)
				*fill++ = -SNUMBER; *fill++ = k+1;
				numlegs = eg->nodes[k]->deg;
				for ( j = 0; j < numlegs; j++ ) {
					edge = eg->nodes[k]->edges[j];
					vect = ABS(edge)-1;
					*fill++ = -VECTOR;
					if ( numlegs == 1 || vect < info->numextern ) { // Look up in set of external momenta
						*fill++ = SetElements[Sets[info->externalset].first+vect];
					}
					else { // Look up in set of internal momenta set
						*fill++ = SetElements[Sets[info->internalset].first+(vect-info->numextern)];
					}
				}
				funfill[1] = fill-funfill;
			}
			*fill++ = 1; *fill++ = 1; *fill++ = 3;
			*argfill = fill - argfill;
			argfill[ARGHEAD] = argfill[0] - ARGHEAD;
			startfill[1] = fill-startfill;
		}

//		if ( eg->econn->narticuls > 0 ) {
//			startfill = fill;
//			*fill++ = BLOCK;
//			*fill++ = 0;
//			FILLFUN(fill);
//			for ( i = 0; i < eg->econn->snodes; i++ ) {
//				if ( eg->econn->articuls[i] != 0 ) {
//					*fill++ = -SNUMBER;
//					*fill++ = i+1;
//				}
//			}
//			startfill[1] = fill-startfill;
//		}
	}
	if ( ( info->flags & WITHONEPI ) == WITHONEPI ) {
		for ( i = 0; i < eg->econn->nopic; i++ ) {
			startfill = fill;
			*fill++ = ONEPI;
			*fill++ = 0;
			FILLFUN(fill);
			*fill++ = -SNUMBER;
			*fill++ = i+1;
			*fill++ = -ONEPI;
			for ( j = 0; j < eg->econn->opics[i].nnodes; j++ ) {
				*fill++ = -SNUMBER;
				*fill++ = eg->econn->opics[i].nodes[j]+1;
			}
			startfill[1] = fill-startfill;
		}
	}
//
//	Topology counter. We have exaggerated a bit with the eye on the far future.
//
	if ( info->numtopo < MAXPOSITIVE ) {
		*fill++ = TOPO; *fill++ = FUNHEAD+2; FILLFUN(fill)
		*fill++ = -SNUMBER; *fill++ = (WORD)(info->numtopo);
	}
	else if ( info->numtopo < FULLMAX-1 ) {
		*fill++ = TOPO; *fill++ = FUNHEAD+ARGHEAD+4; FILLFUN(fill)
		*fill++ = ARGHEAD+4; *fill++ = 0; FILLARG(fill)
		*fill++ = 4;
		*fill++ = (WORD)(info->numtopo & WORDMASK);
		*fill++ = 1; *fill++ = 3;
	}
	else {	// for now: science fiction
		*fill++ = TOPO; *fill++ = FUNHEAD+ARGHEAD+6; FILLFUN(fill)
		*fill++ = ARGHEAD+6; *fill++ = 0; FILLARG(fill)
		*fill++ = 6; *fill++ = (WORD)(info->numtopo >> BITSINWORD);
		*fill++ = (WORD)(info->numtopo & WORDMASK);
		*fill++ = 0; *fill++ = 1; *fill++ = 5;
	}
//
//	Symmetry factors. We let Normalize do the multiplication.
//
	if ( eg->nsym != 1 ) {
		*fill++ = SNUMBER; *fill++ = 4; *fill++ = (WORD)eg->nsym; *fill++ = -1;
	}
	if ( eg->esym != 1 ) {
		*fill++ = SNUMBER; *fill++ = 4; *fill++ = (WORD)eg->esym; *fill++ = -1;
	}
//
//	finish it off
//
	while ( tail < tend ) *fill++ = *tail++;
	if ( eg->fsign < 0 ) fill[-1] = -fill[-1];
	*newterm = fill - newterm;
	AT.WorkPointer = fill;

//MesPrint("<> %a",*newterm,newterm);

	Generator(BHEAD newterm,info->level);
	AT.WorkPointer = oldworkpointer;
	info->numtopo++;
	return False;
}

//	#] ProcessTopology : 
//	#[ GenDiagrams :

int GenDiagrams(PHEAD WORD *term, WORD level)
{
	Model *model;
	MODEL *m;
	Options *opt;
	Process *proc;
	int pid = 1, x;
	int babble = 0;    // Later we may set this at the FORM code level
	TERMINFO info;
	WORD inset,outset,*coupl,setnum,optionnumber = 0;
	int i, j, cpl[GRCC_MAXNCPLG];
	int ninitl, initlPart[GRCC_MAXLEGS], nfinal, finalPart[GRCC_MAXLEGS];
	for ( i = 0; i < GRCC_MAXNCPLG; i++ ) cpl[i] = 0;
//
//	Here we create an object of type Option and load it up.
//	Next we run the diagram generation on it.
//
	info.term = term;
	info.level = level;
	info.diaoffset = AR.funoffset;
	info.externalset = term[info.diaoffset+FUNHEAD+7];
	info.internalset = term[info.diaoffset+FUNHEAD+9];
	info.flags = 0;
	inset = term[info.diaoffset+FUNHEAD+3];
	outset = term[info.diaoffset+FUNHEAD+5];
	coupl = term + info.diaoffset + FUNHEAD + 10;
	if ( *coupl < 0 ) {
		if ( term[info.diaoffset+1] > FUNHEAD + 12 ) {
			optionnumber = term[info.diaoffset+FUNHEAD+13];
		}
	}
	else {
		if ( term[info.diaoffset+1] > *coupl+FUNHEAD+10 )
			optionnumber = term[info.diaoffset+*coupl+FUNHEAD+11];
	}
	setnum = term[info.diaoffset+FUNHEAD+1];

	m = AC.models[SetElements[Sets[setnum].first]];
	LoadModel(m);
	model = (Model *)m->grccmodel;

	info.currentModel = (void *)model;
	info.currentMODEL = (void *)m;
	info.numdia = 0;
	info.numtopo = 1;
	info.flags = optionnumber;

	opt = new Options();

	opt->setOutAG(ProcessDiagram, &info);
	opt->setOutMG(ProcessTopology, &info);
//	opt->setEndMG(fendMG, &info);

	opt->values[GRCC_OPT_1PI] = ( optionnumber & ONEPARTICLEIRREDUCIBLE ) == ONEPARTICLEIRREDUCIBLE;
	opt->values[GRCC_OPT_NoTadpole] = ( optionnumber & NOTADPOLES ) == NOTADPOLES;
//
//	Next are snails:
//
	opt->values[GRCC_OPT_No1PtBlock] = ( optionnumber & NOTADPOLES ) == NOTADPOLES;
//
	if ( ( optionnumber & WITHINSERTIONS ) == WITHINSERTIONS ) {
		opt->values[GRCC_OPT_No2PtL1PI] = True;
		opt->values[GRCC_OPT_NoAdj2PtV] = True;
		opt->values[GRCC_OPT_No2PtL1PI] = True;
	}
	else {
		opt->values[GRCC_OPT_NoAdj2PtV] = True;
	}
	opt->values[GRCC_OPT_SymmInitial] = ( optionnumber & WITHSYMMETRIZE ) == WITHSYMMETRIZE;
	opt->values[GRCC_OPT_SymmFinal] = ( optionnumber & WITHSYMMETRIZE ) == WITHSYMMETRIZE;

//	opt->values[GRCC_OPT_Block] = ( optionnumber & WITHBLOCKS ) == WITHBLOCKS;

	opt->setOutputF(False,"");
	opt->setOutputP(False,"");
	opt->printLevel(babble);

//	opt->values[GRCC_OPT_Step]       = GRCC_AGraph;

//	Load the various arrays.

    ninitl = Sets[inset].last - Sets[inset].first;
    for ( i = 0; i < ninitl; i++ ) {
        x = SetElements[Sets[inset].first+i];
        initlPart[i] = ConvertParticle(model,x);
		info.legcouple[i] = m->vertices[numParticle(m,x)]->couplings;
    }
    nfinal = Sets[outset].last - Sets[outset].first;
    for ( i = 0; i < nfinal; i++ ) {
        x = SetElements[Sets[outset].first+i];
        finalPart[i] = ConvertParticle(model,x);
		info.legcouple[i+ninitl] = m->vertices[numParticle(m,x)]->couplings;
    }
	info.numextern = ninitl + nfinal;
	for ( i = 2; i <= MAXLEGS; i++ ) {
		if ( m->legcouple[i] == 1 ) {
			for ( j = 0; j < info.numextern; j++ ) {
				if ( info.legcouple[j][i] == 0 ) { info.flags |= CHECKEXTERN; goto Go_on; }
			}
		}
	}
Go_on:;
//
//	Now we have to sort out the coupling constants.
//	The argument at coupl can be of type -SNUMBER, -SYMBOL or generic
//	It has however already be tested for syntax.
//	Note that one cannot have 1 for the coupling constants.
//	In that case one should select 0 loops or something equivalent.
//
	if ( *coupl == -SNUMBER ) {	// Number of loops
//
//		This is the complicated case.
//		We have to compute the number of coupling constants and then
//		generate diagrams for all combinations with the proper power.
//
		int nc = coupl[1]*2 + ninitl + nfinal - 2;
		int *scratch = (int *)Malloc1(nc*sizeof(int),"DistrN");
		scratch[0] = -2; // indicating startup cq first call.

		if ( ( info.flags & TOPOLOGIESONLY ) == 0 ) {
			while ( DistrN(nc,cpl,m->ncouplings,scratch) ) {
				proc = new Process(pid, model, opt,
        	               ninitl, initlPart, nfinal, finalPart, cpl);
				delete proc;
				info.numtopo = 1;
			}
		}
		else {
			cpl[0] = nc;
			proc = new Process(pid, model, opt,
       	               ninitl, initlPart, nfinal, finalPart, cpl);
			delete proc;
		}
		M_free(scratch,"DistrN");
		opt->end();
		delete opt;
		return(0);
	}
	else if ( *coupl == -SYMBOL ) {	// Just a single power of one constant
		for ( i = 0; i < m->ncouplings; i++ ) {
			if ( m->couplings[i] == coupl[1] ) {
				cpl[i] = 1;
				break;
			}
		}
	}
	else {	// One term with powers of coupling constants
		WORD *t, *tstop;
		t = coupl + ARGHEAD+3;
		tstop = coupl+*coupl; tstop -= ABS(tstop[-1]);
		while ( t < tstop ) {
			for ( i = 0; i < m->ncouplings; i++ ) {
				if ( m->couplings[i] == *t ) {
					cpl[i] = t[1];
					break;
				}
			}
			t += 2;
		}
	}
/*
	And now the generation:
*/
	proc = new Process(pid, model, opt,
                       ninitl, initlPart, nfinal, finalPart, cpl);
	opt->end();
	delete proc;
	delete opt;
	return(0);
}

//	#] GenDiagrams : 
//	#[ processVertex :

//	Routine is to be used recursively to work its way through a list
//	of possible vertices. The array of vertices is in TopoInf->vert
//	with TopoInf->nvert the number of possible vertices.
//	Currently we allow in TopoInf->vert only vertices with 3 or more edges.
//
//	We work with a point system. Each n-point vertex contributes n-2 points.
//	When all points are assigned, we can call mgraph->generate().
//
//	The number of vertices of a given number of edges is stored in
//	TopoInf->clnum[..] but the loop that determines how many there are
//	may be limited by the corresponding element in TopoInf->vertmax[level]

int processVertex(TOPOTYPE *TopoInf, int pointsremaining, int level)
{
	int i, j;

	for ( i = pointsremaining, j = 0; i >= 0; i -= TopoInf->vert[level]-2, j++ ) {
		if ( TopoInf->vertmax && TopoInf->vertmax[level] >= 0
					 && j > TopoInf->vertmax[level] ) break;
		if ( i == 0 ) { // We got one!
			TopoInf->cldeg[TopoInf->ncl] = TopoInf->vert[level];
			TopoInf->clnum[TopoInf->ncl] = j;
			TopoInf->clext[TopoInf->ncl] = 0;
			TopoInf->ncl++;

			MGraph *mgraph = new MGraph(1, TopoInf->ncl, TopoInf->cldeg,
					             TopoInf->clnum, TopoInf->clext,
								 TopoInf->cmind, TopoInf->cmaxd, TopoInf->opt);

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
			if ( processVertex(TopoInf,i,level+1) < 0 ) return(-1);
			if ( j > 0 ) { TopoInf->ncl--; }
		}
	}
	return(0);
}

//	#] processVertex : 
//	#[ GenTopologies :

#define TOPO_MAXVERT 10

int GenTopologies(PHEAD WORD *term, WORD level)
{
	Options *opt = new Options;
	int nlegs, nloops, i, identical;
	TERMINFO info;
	WORD *t, *t1, *tstop;
	TOPOTYPE TopoInf;
	SETS s;
//
	info.term = term;
	info.level = level;
	info.diaoffset = AR.funoffset;
	info.flags = 0;
 
	t = term + info.diaoffset;  // the function
	t1 = t + FUNHEAD;           // its arguments
	tstop = t + t[1];

	info.externalset = t1[7];
	info.internalset = t1[9];

	s = &(Sets[t1[5]]);
	TopoInf.nvert = s->last - s->first;
	TopoInf.vert  = &(SetElements[s->first]);

	nloops = t1[1];
	nlegs = t1[3];

	info.numextern = nlegs;

	for ( i = 0; i <= MAXLEGS; i++ ) { TopoInf.cmind[i] = TopoInf.cmaxd[i] = 0; }

	t1 += 10;
	if ( t1 < tstop && t1[0] == -SETSET ) {
		TopoInf.vertmax = &(SetElements[Sets[t1[1]].first]);
		t1 += 2;
	}
	else TopoInf.vertmax = NULL;

	info.flags |= TOPOLOGIESONLY;  // this is the topologies_ function after all.
	if ( t1 < tstop && t1[0] == -SNUMBER ) {
		if ( ( t1[1] &   NONODES ) ==   NONODES ) info.flags |=   NONODES;
		if ( ( t1[1] & WITHEDGES ) == WITHEDGES ) info.flags |= WITHEDGES;
		if ( ( t1[1] & WITHBLOCKS ) == WITHBLOCKS ) info.flags |= WITHBLOCKS;
		if ( ( t1[1] & WITHONEPI ) == WITHONEPI ) info.flags |= WITHONEPI;
		opt->values[GRCC_OPT_1PI] = ( t1[1] & ONEPARTICLEIRREDUCIBLE ) == ONEPARTICLEIRREDUCIBLE;
//		opt->values[GRCC_OPT_NoTadpole] = ( t1[1] & NOTADPOLES ) == NOTADPOLES;
		opt->values[GRCC_OPT_NoTadpole] = ( t1[1] & NOSNAILS ) == NOSNAILS;
		opt->values[GRCC_OPT_No1PtBlock] = ( t1[1] & NOTADPOLES ) == NOTADPOLES;
		opt->values[GRCC_OPT_NoExtSelf] = ( t1[1] & NOEXTSELF ) == NOEXTSELF;

		if ( ( t1[1] & WITHINSERTIONS ) == WITHINSERTIONS ) {
			opt->values[GRCC_OPT_No2PtL1PI] = True;
			opt->values[GRCC_OPT_NoAdj2PtV] = True;
			opt->values[GRCC_OPT_No2PtL1PI] = True;
		}
		opt->values[GRCC_OPT_SymmInitial] = ( t1[1] & WITHSYMMETRIZE ) == WITHSYMMETRIZE;
	}

	info.numdia = 0;
	info.numtopo = 1;

	opt->setOutAG(ProcessDiagram, &info);
	opt->setOutMG(ProcessTopology, &info);
//
//	Now we should sum over all possible vertices and run MGraph for
//	each combination. This is done by recursion in the processVertex routine
//	First load up the relevant arrays.
//

//	First the external nodes.

	if ( nlegs == -2 ) {
		nlegs = 2;
		identical = 1;
	}
	for ( i = 0; i < nlegs; i++ ) {
		TopoInf.cldeg[i] = 1; TopoInf.clnum[i] = 1; TopoInf.clext[i] = -1;
	}
	int points = 2*nloops-2+nlegs;

	if ( identical == 1 ) {	/* Only propagator topologies..... */
		nlegs = 1;
		TopoInf.clnum[0] = 2;
	}
	TopoInf.ncl = nlegs;
	TopoInf.opt = opt;

	if ( points >= MAXPOINTS ) {
		MLOCK(ErrorMessageLock);
		MesPrint("GenTopologies: %d loops and %d legs considered excessive",nloops,nlegs);
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	if ( processVertex(&TopoInf,points,0) != 0 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Called from GenTopologies with %d loops and %d legs",nloops,nlegs);
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	delete opt;
	return(0);
}

//	#] GenTopologies : 

