/*
  	#[ Explanations :

	This is a second generation much simplified version of model.c
	Syntax:
		Model QED;
			Particle electron,positron,-2;
			Particle photon,+3;
			Vertex electron,positron,photon: g;
		EndModel;
	or:
		Model QCD;
			Particle quark,antiquark,-2;
			Particle ghost,antighost,-1;
			Particle gluon,+3;
			Vertex quark,antiquark,gluon: g;
			Vertex qhost,antighost,gluon: g;
			Vertex gluon,gluon,gluon: g;
			Vertex gluon,gluon,gluon,gluon: g^2;
		EndModel;
	Remarks:
	1: if no second particle is given, a particle and its antiparticle are the same
	2: Spin statistics is by dimension of SU(2) representation with a sign.
	3: In a vertex we need to mention the coupling constants.
	4: Internally a particle gives a vertex with only two lines.
	5: All particles must have been declared before any vertex.

	The diagrams should be provided as a collection of nodes and edges 
	with momenta with a direction substituted. The other parameters (like
	Lorentz indices, color indices etc.) can then be provided in a procedure
	that should go with this mode definition.
	In principle the edges are not needed if there are no 'insertions', but
	because we would like to have the insertions as well we need the edges.

  	#] Explanations : 
  	#[ Includes : model.c
*/

#include "form3.h"
 
/*
  	#] Includes : 
  	#[ CreateVertex :
*/

VERTEX *CreateVertex(MODEL *m)
{
	VERTEX *v, **new;
	WORD newsize;
	int i;
	if ( m->invertices >= m->sizevertices ) {
		if ( m->sizevertices == 0 ) newsize = 20;
		else newsize = 2*m->sizevertices;
		new = (VERTEX **)Malloc1(newsize*sizeof(VERTEX *),"m->vertices");
		for ( i = 0; i < m->sizevertices; i++ ) new[i] = m->vertices[i];
		if ( m->sizevertices > 0 ) M_free(m->vertices,"m->vertices");
		m->vertices = new;
		m->sizevertices = newsize;
	}
	v = (VERTEX *)Malloc1(sizeof(VERTEX),"VERTEX");
	m->vertices[m->invertices++] = v;
	v->nparticles = 0;
	v->ncouplings = 0;
	v->type = 0;
	v->error = 0;
	v->externonly = 0;
	v->spare = 0;
	return(v);
}

/*
  	#] CreateVertex : 
  	#[ ReadParticle :
*/

UBYTE *ReadParticle(UBYTE *s, VERTEX *v, MODEL *m, int par)
{
	UBYTE *name, c;
	PARTICLE *p = v->particles + v->nparticles++;
	WORD type, funnum;
	int i, j;
	SKIPBLANKS(s)
	name = s; s = SkipName(s); c = *s; *s = 0;
	if ( GetVar(name,&type,&funnum,CFUNCTION,NOAUTO) == NAMENOTFOUND ) {
		p->number = AddFunction(name,0,VERTEXFUNCTION,0,0,0,-1,-1) + FUNCTION;
	}
	else if ( par == 1 && type == CFUNCTION && functions[funnum].spec == VERTEXFUNCTION ) {
		p->number = funnum+FUNCTION;
	}
	else if ( par == 0 && type == CFUNCTION && functions[funnum].spec == VERTEXFUNCTION ) {
/*
		We should check whether this particle exists already in this model.
		If so, this will be an error.
*/
		for ( i = 0; i < m->nparticles-1; i++ ) {
		  for ( j = 0; j < m->vertices[i]->nparticles; j++ ) {
			if ( m->vertices[i]->particles[j].number == funnum ) {
				MesPrint("&Illegal attempt to redefine a particle in the same model: %s",name);
				v->error = 1;
			}
		  }
		}
		p->number = funnum+FUNCTION;
	}
	else {
		MesPrint("&Name of particle previously declared as another variable: %s",name);
		v->error = 1;
	}
	*s = c;
	while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	return(s);
}

/*
  	#] ReadParticle :
  	#[ CoModel :
*/

int CoModel(UBYTE *s)
{
	UBYTE *name, c;
	MODEL *m = (MODEL *)Malloc1(sizeof(MODEL),"Model");
	WORD numberofset, *e;
	SETS set;
	SKIPBLANKS(s)
	AC.ModelLevel++;
	int i;
/*
	We now have to add the model to the list of models.
*/
	if ( AC.modelspace == 0 || AC.nummodels >= AC.modelspace ) {
		int newspace = 2*AC.modelspace+2, i;
		MODEL **models = (MODEL **)Malloc1((sizeof(MODEL *))*newspace,"AC.models");
		for ( i = 0; i < AC.modelspace; i++ ) models[i] = AC.models[i];
		if ( AC.models ) M_free(AC.models,"AC.models");
		AC.modelspace = newspace;
		AC.models = models;
	}

	AC.models[AC.nummodels++] = m;

	m->vertices = 0;
	m->couplings = 0;
	m->nparticles = m->nvertices = m->sizevertices = m->invertices = 0;
	m->sizecouplings = 0;
	m->error = 0;
	m->grccmodel = NULL;

	name = s;
	if ( FG.cTable[*s] == 0 ) {
		while ( FG.cTable[*s] <= 1 ) s++;
		c = *s; *s = 0;
		m->name = strDup1(name,"Model name");
		*s = c;
		SKIPBLANKS(s)
		if ( *s != 0 ) {
			MesPrint("&Illegal option in model statement: %s",s);
			return(1);
		}
	}
	else {
		m->name = strDup1((UBYTE *)"---","Model name");
		m->error = 1;
		MesPrint("&Illegal name for model: %s",name);
		return(1);
	}
/*
	Now make it a set with one element. The type of the set is rather special
*/
	if ( GetName(AC.varnames,m->name,&numberofset,NOAUTO) != NAMENOTFOUND ) {
		MesPrint("&Name conflict with name %s of model",m->name);
		return(1);
	}
	numberofset = AddSet(name,0);
	set = Sets + numberofset;
	set->type = CMODEL;
	e = (WORD *)FromVarList(&AC.SetElementList);
	*e = AC.nummodels-1;
	set->last++;
	AC.SetList.numtemp = AC.SetList.num;
	AC.SetElementList.numtemp = AC.SetElementList.num;

	for ( i = 0; i <= MAXLEGS; i++ ) m->legcouple[i] = 0;
	m->legcouple[2] = 1;

	return(0);
}

/*
  	#] CoModel : 
  	#[ CoParticle :
*/

int CoParticle(UBYTE *s)
{
	MODEL *m;
	VERTEX *v;
	int i;
	if ( AC.nummodels <= 0 ) {
		MesPrint("&No open model for particle statement");
		return(1);
	}
	m = AC.models[AC.nummodels-1];
	if ( m->nvertices > 0 ) {
		MesPrint("&In a model description Particle statements should be before Vertex statements.");
		m->error = 1;
		return(1);
	}
	v = CreateVertex(m);
	m->nparticles++;
	s = ReadParticle(s,v,m,0);
	v->particles[0].type = 1;
	while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	if ( v->error == 0 ) {
		if ( FG.cTable[*s] == 0 ) {
			s = ReadParticle(s,v,m,0);
			v->particles[1].type = -1;
			if ( v->particles[0].number == v->particles[1].number ) {
				v->particles[0].type = 0;
				v->particles[1].type = 0;
			}
		}
		else {	/* Particle = antiparticle */
			v->particles[1] = v->particles[0];
			v->nparticles++;
			v->particles[0].type = 0;
			v->particles[1].type = 0;
		}
	}
	if ( v->error == 0 && ( *s == '+' || *s == '-' ) ) {
		int x = 0, sign = ( *s ==  '-' ) ? -1: 1;
		s++;
		while ( FG.cTable[*s] == 1 ) { x = 10*x + *s++ - '0'; }
		if ( x == 0 ) {
			v->error = 1;
			MesPrint("&Spin goes by dimension of SU(2) representation. Zero is not allowed.");
		}
		else { v->particles[0].spin = v->particles[1].spin = sign*x; }
		SKIPBLANKS(s)
	}
	else if ( v->error == 0 ) {
		v->particles[0].spin = v->particles[1].spin = 1;
		v->particles[0].type = 0;
		v->particles[1].type = 0;
	}
/*
	Here will come future options
*/
	while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	while ( v->error == 0 && *s != 0 && FG.cTable[*s] == 0 ) {
		UBYTE *opt = s, c;
		while ( FG.cTable[*s] == 0 ) s++;
		c = *s; *s = 0;
		if ( StrICont(opt,(UBYTE *)"external") == 0 ) { v->externonly = 1; }
		else {
			MesPrint("&Unrecognized option %s in particle statement.",opt);
			v->error = 1;
		}
		*s = c;
		while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	}
/*
	The couplings array will be used for registering in what kind of vertices the
	particle is involved. (example: in QCD the quarks only in 2 and 3-point vertices)
*/
	for ( i = 0; i < MAXPARTICLES; i++ ) v->couplings[i] = 0;
	v->couplings[2] = 1;
	return(v->error);
}

/*
  	#] CoParticle :
  	#[ CoVertex :
*/

int CoVertex(UBYTE *s)
{
	UBYTE *ss, *name, c;
	WORD type;
	MODEL *m;
	VERTEX *v;
	if ( AC.ModelLevel <= 0 ) {
		MesPrint("&No open model for vertex statement");
		return(1);
	}
	m = AC.models[AC.nummodels-1];
/*
	Get an object of type VERTEX
*/
	v = CreateVertex(m);
	m->nvertices++;
	SKIPBLANKS(s);
	while ( *s && *s != ':' ) {
		if ( v->error == 0 ) {
			s = ReadParticle(s,v,m,1);
			if ( v->error ) m->error = 1;
		}
		else {
			while ( *s && *s != ':' ) {
				if ( *s == '[' ) { SKIPBRA1(s) }
				else if ( *s == '(' ) { SKIPBRA3(s) }
				else if ( *s == 0 ) {
					v->error = 1;
					MesPrint("&No coupling constant in vertex statement.");
					break;
				}
			}
			break;
		}
	}
	if ( v->error == 0 ) {
	  if ( *s == ':' ) { /* read symbols and powers */
		s++; SKIPBLANKS(s);
		while ( *s ) {
			if ( v->ncouplings >= 2*MAXCOUPLINGS ) {
				MesPrint("&More than %d coupling constants in vertex.",(WORD)MAXCOUPLINGS);
				MesPrint("    Recompile with a larger value for MAXCOUPLINGS.");
				return(1);
			}
			ss = s; s = SkipName(s); c = *s; *s = 0; name = ConstructName(ss,0);
			if ( GetVar(name,&type,&v->couplings[v->ncouplings],CSYMBOL,WITHAUTO)
				 == NAMENOTFOUND ) {
				WORD minpow = -MAXPOWER;
				WORD maxpow =  MAXPOWER;
				WORD cplx = 0, dim = 0;
				v->couplings[v->ncouplings] = AddSymbol(name,minpow,maxpow,cplx,dim);
			}
			*s = c;
			v->ncouplings++;
/*
			Now possibly a power
*/
			if ( *s == '^' ) { /* we need an integer */
				WORD x = 0; WORD sign = 1;
				s++;
				while ( *s == '-' || *s == '+' ) {
					if ( *s == '-' ) sign = -sign;
					s++;
				}
				while ( FG.cTable[*s] == 1 ) x = 10*x + *s++ - '0';
				v->couplings[v->ncouplings++] = x;
			}
			else {
				v->couplings[v->ncouplings++] = 1;
			}
			SKIPBLANKS(s);
			if ( *s == '*' ) { s++; continue; }
			if ( *s != ',' ) break;
			s++;
			SKIPBLANKS(s);
		}
	  }
	  else {
		v->error = 1;
		MesPrint("&A vertex statement needs at least one coupling constant.");
	  }
	}
	if ( v->error == 0 ) {
/*
		Register which types of vertices are possible for each particle.
*/
		int i, j;
		for ( i = 0; i < v->nparticles; i++ ) {
			for ( j = 0; j < m->nparticles; j++ ) {
				if ( m->vertices[j]->particles[0].number == v->particles[i].number ) break;
				if ( m->vertices[j]->particles[1].number == v->particles[i].number ) break;
			}
			m->vertices[j]->couplings[v->nparticles] = 1;
		}
	}
	return(v->error);
}

/*
  	#] CoVertex : 
  	#[ CoEndModel :
*/

int CoEndModel(UBYTE *s)
{
	int i, j, k, kk;
	WORD csize = 0, *newcouplings, newsize;
	MODEL *m;
	VERTEX *v;
	if ( AC.ModelLevel <= 0 ) {
		MesPrint("&EndModel statement without matching Model statement");
		return(1);
	}
	m = AC.models[AC.nummodels-1];
/*
	Now create a list of all coupling constants.
	Note that we do not expect an astronomical number of them and hence
	we use a simple insertion algorithm.
*/
	m->ncouplings = 0;
	for ( i = 0; i < m->nvertices; i++ ) {
		v = m->vertices[i+m->nparticles];
		m->legcouple[v->nparticles] = 1;
		if ( m->ncouplings + v->ncouplings > csize ) {
			if ( csize == 0 ) newsize = v->ncouplings + 10;
			else newsize = 2*csize;
			newcouplings = (WORD *)Malloc1(newsize*sizeof(WORD),"m->couplings");
			for ( k = 0; k < m->ncouplings; k++ ) newcouplings[k] = m->couplings[k];
			if ( csize > 0 ) M_free(m->couplings,"m->couplings");
			m->couplings = newcouplings;
			csize = newsize;
		}
		for ( j = 0; j < v->ncouplings; j += 2 ) {
			WORD sym = v->couplings[j];
			for ( k = 0; k < m->ncouplings; k++ ) {
				if ( sym == m->couplings[k] ) break;
				if ( sym < m->couplings[k] ) {
					for ( kk = m->ncouplings; kk > k; k-- )
						m->couplings[kk] = m->couplings[kk-1];
					m->couplings[k] = sym;
					m->ncouplings++;
					break;
				}
			}
			if ( k >= m->ncouplings ) m->couplings[m->ncouplings++] = sym;
		}
	}
	AC.ModelLevel--;
	DUMMYUSE(s)
	return(LoadModel(m));
}

/*
  	#] CoEndModel : 
*/
