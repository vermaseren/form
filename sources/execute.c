/*
  	#[ Includes : execute.c
*/

#include "form3.h"

#ifdef PARALLEL /* [03dec2002 df] */
#include "parallel.h"
#endif

extern WORD *dummyrenumlist;

#ifdef PARALLEL /* [04dec2002 df] */
PFDOLLARS *PFDollars; 
#endif

/*
  	#] Includes :
 	#[ DoExecute :
 		#[ CleanExpr :

		par == 1 after .store or .clear
		par == 0 after .sort
*/

WORD
CleanExpr ARG1(WORD,par)
{
	WORD j, n, i;
	POSITION length;
	EXPRESSIONS e_in, e_out, e;
	int numhid = 0;
	NAMENODE *node;
	n = NumExpressions;
	j = 0;
	e_in = e_out = Expressions;
	if ( n > 0 ) { do {
		if ( par ) {
			if ( e_in->renum ) {
				M_free(e_in->renum,"Renumber"); e_in->renum = 0;
			}
			if ( e_in->renumlists ) {
				if ( e_in->renumlists != dummyrenumlist )
						M_free(e_in->renumlists,"Renumber-lists");
				e_in->renumlists = 0;
			}
		}
		if ( e_in->status == HIDDENLEXPRESSION
		|| e_in->status == HIDDENGEXPRESSION ) numhid++;
		switch ( e_in->status ) {
			case LOCALEXPRESSION:
			case HIDDENLEXPRESSION:
				if ( par ) {
					AC.exprnames->namenode[e_in->node].type = CDELETE;
					AC.DidClean = 1;
					if ( e_in->status != HIDDENLEXPRESSION )
						ClearBracketIndex(e_in-Expressions);
					break;
				}
			case GLOBALEXPRESSION:
			case HIDDENGEXPRESSION:
				if ( par ) {
					e = e_in;
					i = n-1;
					while ( --i >= 0 ) {
						e++;
						if ( e->status == GLOBALEXPRESSION
						|| e->status == LOCALEXPRESSION ) break;
					}
					if ( i >= 0 ) {
						DIFPOS(length,e->onfile,e_in->onfile);
					}
					else {
						if ( AR.outfile->handle < 0 ) {
							SETBASELENGTH(length,TOLONG(AR.outfile->POfull)
							 - TOLONG(AR.outfile->PObuffer)
							 - BASEPOSITION(e_in->onfile));
						}
						else {
							SeekFile(AR.outfile->handle,&(AR.outfile->filesize),SEEK_SET);
							DIFPOS(length,AR.outfile->filesize,e_in->onfile);
						}
					}
					if ( ToStorage(e_in,&length) ) {
						return(MesCall("PopVariables"));
					}
					e_in->status = STOREDEXPRESSION;
					if ( e_in->status != HIDDENGEXPRESSION )
						ClearBracketIndex(e_in-Expressions);
				}
				/* Fall through is intentional */
			case SKIPLEXPRESSION:
			case DROPLEXPRESSION:
			case DROPHLEXPRESSION:
			case DROPGEXPRESSION:
			case DROPHGEXPRESSION:
			case STOREDEXPRESSION:
				if ( e_out != e_in ) {
					node = AC.exprnames->namenode + e_in->node;
					node->number = e_out - Expressions;

					e_out->onfile = e_in->onfile;
					e_out->printflag = 0;
					if ( par ) e_out->status = STOREDEXPRESSION;
					else e_out->status = e_in->status;
					e_out->name = e_in->name;
					e_out->node = e_in->node;
					e_out->renum = e_in->renum;
					e_out->renumlists = e_in->renumlists;
					e_out->counter = e_in->counter;
					e_out->hidelevel = e_in->hidelevel;
					e_out->inmem = e_in->inmem;
					e_out->bracketinfo = e_in->bracketinfo;
					e_out->newbracketinfo = e_in->newbracketinfo;
				}
				e_out++;
				j++;
				break;
			case DROPPEDEXPRESSION:
				break;
			default:
				AC.exprnames->namenode[e_in->node].type = CDELETE;
				AC.DidClean = 1;
				break;
		}
		e_in++;
	} while ( --n > 0 ); }
	NumExpressions = j;
	if ( numhid == 0 && AS.hidefile->PObuffer ) {
		if ( AS.hidefile->handle >= 0 ) {
			CloseFile(AS.hidefile->handle);
			remove(AS.hidefile->name);
			AS.hidefile->handle = -1;
		}
		AS.hidefile->POfull =
		AS.hidefile->POfill = AS.hidefile->PObuffer;
		PUTZERO(AS.hidefile->POposition);
	}
	return(0);
}

/*
 		#] CleanExpr :
 		#[ PopVariables :

	Pops the local variables from the tables.
	The Expressions are reprocessed and their tables are compactified.

*/

WORD
PopVariables()
{
	WORD j, retval;

	retval = CleanExpr(1);
	ResetVariables(1);

	if ( AC.DidClean ) CompactifyTree(AC.exprnames);

	AC.CodesFlag = AM.gCodesFlag;
	AC.NamesFlag = AM.gNamesFlag;
	AC.StatsFlag = AM.gStatsFlag;
	AC.lUnitTrace = AM.gUnitTrace;
	AC.lDefDim = AM.gDefDim;
	AC.lDefDim4 = AM.gDefDim4;
	AC.ncmod = AM.gncmod;
	AC.npowmod = AM.gnpowmod;
	AC.funpowers = AM.gfunpowers;
	AC.lPolyFun = AM.gPolyFun;
	AC.parallelflag = AC.mparallelflag = AM.gparallelflag;
	AC.SlavePatchSize = AC.mSlavePatchSize = AM.gSlavePatchSize;
	AC.properorderflag = AM.gproperorderflag;
	{
		WORD *p, *m;
		p = AM.gcmod;
		m = AC.cmod;
		j = ABS(AC.ncmod);
		NCOPY(m,p,j);
		p = AM.gpowmod;
		m = AC.powmod;
		j = AC.npowmod;
		NCOPY(m,p,j);
		if ( AC.DirtPow ) {
			if ( MakeModTable() ) {
				MesPrint("===No printing in powers of generator");
			}
			AC.DirtPow = 0;
		}
		p = AM.gUniTrace;
		m = AC.lUniTrace;
		j = 4;
		NCOPY(m,p,j);
	}
	AC.OutputMode = AM.gOutputMode;
	AC.OutputSpaces = AM.gOutputSpaces;
	AC.OutNumberType = AM.gOutNumberType;
	AC.SortType = AM.gSortType;
	return(retval);
}

/*
 		#] PopVariables :
 		#[ MakeGlobal :
*/

VOID
MakeGlobal ARG0
{
	WORD i, *p, *m;
	Globalize(0);

	AM.gCodesFlag = AC.CodesFlag;
	AM.gNamesFlag = AC.NamesFlag;
	AM.gStatsFlag = AC.StatsFlag;
	AM.gUnitTrace = AC.lUnitTrace;
	AM.gDefDim = AC.lDefDim;
	AM.gDefDim4 = AC.lDefDim4;
	AM.gncmod = AC.ncmod;
	AM.gnpowmod = AC.npowmod;
	AM.gOutputMode = AC.OutputMode;
	AM.gOutputSpaces = AC.OutputSpaces;
	AM.gOutNumberType = AC.OutNumberType;
	AM.gfunpowers = AC.funpowers;
	AM.gPolyFun = AC.lPolyFun;
	AM.gparallelflag = AC.parallelflag;
	AM.gSlavePatchSize = AC.SlavePatchSize;
	AM.gproperorderflag = AC.properorderflag;
	p = AM.gcmod;
	m = AC.cmod;
	i = ABS(AC.ncmod);
	NCOPY(p,m,i);
	p = AM.gpowmod;
	m = AC.powmod;
	i = AC.npowmod;
	NCOPY(p,m,i);
	p = AM.gUniTrace;
	m = AC.lUniTrace;
	i = 4;
	NCOPY(p,m,i);
	AM.gSortType = AC.SortType;
}

/*
 		#] MakeGlobal :
 		#[ TestDrop :
*/

VOID
TestDrop()
{
	EXPRESSIONS e;
	WORD j;
	for ( j = 0, e = Expressions; j < NumExpressions; j++, e++ ) {
		switch ( e->status ) {
			case SKIPLEXPRESSION:
				e->status = LOCALEXPRESSION;
				break;
			case UNHIDELEXPRESSION:
				e->status = LOCALEXPRESSION;
				ClearBracketIndex(j);
				e->bracketinfo = e->newbracketinfo; e->newbracketinfo = 0;
				break;
			case HIDELEXPRESSION:
				e->status = HIDDENLEXPRESSION;
				break;
			case SKIPGEXPRESSION:
				e->status = GLOBALEXPRESSION;
				break;
			case UNHIDEGEXPRESSION:
				e->status = GLOBALEXPRESSION;
				ClearBracketIndex(j);
				e->bracketinfo = e->newbracketinfo; e->newbracketinfo = 0;
				break;
			case HIDEGEXPRESSION:
				e->status = HIDDENGEXPRESSION;
				break;
			case DROPLEXPRESSION:
			case DROPGEXPRESSION:
			case DROPHLEXPRESSION:
			case DROPHGEXPRESSION:
				e->status = DROPPEDEXPRESSION;
				ClearBracketIndex(j);
				e->bracketinfo = e->newbracketinfo; e->newbracketinfo = 0;
				if ( e->replace >= 0 ) {
					Expressions[e->replace].replace = -1;
					AC.exprnames->namenode[e->node].number = e->replace;
					e->replace = -1;
				}
				else {
					AC.exprnames->namenode[e->node].type = CDELETE;
					AC.DidClean = 1;
				}
				break;
			case LOCALEXPRESSION:
			case GLOBALEXPRESSION:
				ClearBracketIndex(j);
				e->bracketinfo = e->newbracketinfo; e->newbracketinfo = 0;
				break;
			case HIDDENLEXPRESSION:
			case HIDDENGEXPRESSION:
				break;
			default:
				ClearBracketIndex(j);
				break;
		}
	}
}

/*
 		#] TestDrop :
 		#[ DoExecute :
*/

WORD
DoExecute ARG2(WORD,par,WORD,skip)
{
	WORD RetCode = 0;
	int i, j;

	/*[30jan2004 mt]:*/
	/*AC.mparallelflag was set to PARALLELFLAG in IniModule. It can be set to 
		NOPARALLEL_MOPT or PARALLEL_MOPT (module option) by compiler. If 
		AC.mparallelflag contains PARALLEL_MOPT, it must be set to PARALLELFLAG, if
		AM.hparallelflag is PARALLELFLAG:*/
	if( (AC.parallelflag == PARALLELFLAG) || (AC.mparallelflag & PARALLEL_MOPT) ){
		if(!(AC.mparallelflag & NOPARALLEL_MOPT)){
			if(AM.hparallelflag != PARALLELFLAG){
				AC.mparallelflag |=(AC.parallelflag=AM.hparallelflag);
/*[19feb2004 mt] - to prevent warnins in sequential mode I inseret this #ifdef PARALLEL:*/
#ifdef PARALLEL 
				MesPrint("WARNING!: $ use in table - the module %d is forced to run in sequential mode.", AC.CModule);
#endif
			}else
				AC.mparallelflag = PARALLELFLAG;
		}/*if(!(AC.mparallelflag & NOPARALLEL_MOPT))*/
	}else{/*AC.parallelflag != PARALLELFLAG*/
		AC.mparallelflag |= AC.parallelflag;
	}
	/*:[30jan2004 mt]*/

	if ( skip ) goto skipexec;
	if ( AC.IfLevel > 0 ) {
		MesPrint(" %d endif statement(s) missing",AC.IfLevel);
		RetCode = 1;
	}
	if ( AC.WhileLevel > 0 ) {
		MesPrint(" %d endwhile statement(s) missing",AC.WhileLevel);
		RetCode = 1;
	}
	if ( AC.arglevel > 0 ) {
		MesPrint(" %d endargument statement(s) missing",AC.arglevel);
		RetCode = 1;
	}
	if ( AC.termlevel > 0 ) {
		MesPrint(" %d endterm statement(s) missing",AC.termlevel);
		RetCode = 1;
	}
	if ( AC.insidelevel > 0 ) {
		MesPrint(" %d endinside statement(s) missing",AC.insidelevel);
		RetCode = 1;
	}
	if ( AC.NumLabels > 0 ) {
		for ( i = 0; i < AC.NumLabels; i++ ) {
			if ( AC.Labels[i] < 0 ) {
				MesPrint(" -->Label %s missing",AC.LabelNames[i]);
				RetCode = 1;
			}
		}
	}
	if ( RetCode ) return(RetCode);
/*
	@@@@@@@@@@@@@@@@ can be removed? [03dec2002 df]
	Here we invalidate caches of the slaves for all dollars changed by the
	preprocessor. They are in PPchangeddollars. there are NumPPchangeddollars
	of them. It is an array of WORD.
	This is finished with: NumPPchangeddollars = 0;
*/

#ifdef PARALLEL /* [04dec2002 df] */
	/*[30jan2004 mt]:*/
	/*if (AC.parallelflag != NOPARALLELFLAG) {*/
	if (AC.mparallelflag == PARALLELFLAG) {
	/*:[30jan2004 mt]*/
	  if (PF.me == 0) {	 
	    /* maybe they are not needed at all for the moment or 
	       should be combined from all slaves and again distributed */
	  } 
	  else {
	    M_free(PPchangeddollars, "kill PPchangeddollars list");
            /* PPchangeddollars changed to AP.ChDollarList.lijst because AIX C compiler complained. MF 30/07/2003 */
	    AP.ChDollarList.lijst = NULL;
	    NumPPchangeddollars = 0; 	 	 	 
	  } 
	}  

#endif /* PARALLEL [04dec2002 df] */

	if ( ( AS.ExecMode = par ) == GLOBALMODULE ) AS.ExecMode = 0;
/*
	Now we compare whether all elements of PotModdollars are contained in
	ModOptdollars. If not, we may not run parallel.
*/
	/*[30jan2004 mt:*/
/*	if ( NumPotModdollars > 0 && AC.parallelflag != NOPARALLELFLAG ) {*/
	if ( NumPotModdollars > 0 && AC.mparallelflag == PARALLELFLAG ) {
	/*:[30jan2004 mt*/
	  if ( NumPotModdollars > NumModOptdollars ) 
		/*[30jan2004 mt]:*/		
	    /*AC.parallelflag = NOPARALLELFLAG;*/
	    AC.mparallelflag = NOPARALLEL_DOLLAR;
		/*:[30jan2004 mt]*/
	  else 
	    for ( i = 0; i < NumPotModdollars; i++ ) {
	      for ( j = 0; j < NumModOptdollars; j++ ) 
		if ( PotModdollars[i] == ModOptdollars[j].number ) break;
	      if ( j >= NumModOptdollars ) {
		/*[30jan2004 mt]:*/
		/*AC.parallelflag = NOPARALLELFLAG;*/
		AC.parallelflag = NOPARALLEL_DOLLAR;
		/*:[30jan2004 mt]*/
		break;
	      }
	    }
	}
	/*[30jan2004 mt:*/
#ifdef PARALLEL 
 	if(AC.mparallelflag & NOPARALLEL_DOLLAR){
		/*If the only bit is NOPARALLEL_DOLLAR, then the user wants this module
			to run in parallel.: */
		if(AC.mparallelflag == NOPARALLEL_DOLLAR)	
			MesPrint("WARNING!: dollar variables - the module %d is forced to run in sequential mode.", AC.CModule);
	}/*if(AC.mparallelflag & NOPARALLEL_DOLLAR)*/
	/*:[30jan2004 mt]*/
#endif
#ifdef PARALLEL
	/*[07nov2003 mt]: ??? -Why? Should be AC.mparallelflag? */
	/*if ( par == STOREMODULE ) AC.parallelflag = NOPARALLELFLAG; */
	/*[30jan2004 mt]:*/ /*Detailed flags here*/
	if ( par == STOREMODULE ){
/*[19feb2004 mt] - to prevent warnins in sequential mode I inseret this #ifdef PARALLEL:*/
#ifdef PARALLEL
		if(AC.mparallelflag == PARALLELFLAG)
			MesPrint("WARNING!: store module - the module %d is forced to run in sequential mode.", AC.CModule);
#endif
		AC.mparallelflag = NOPARALLEL_STORE;
	}/*if ( par == STOREMODULE )*/
	/*:[30jan2004 mt]*/
	/*:[07nov2003 mt]*/
#endif

	/*[07nov2003 mt]:*/
#ifdef PARALLEL
	if( (AC.NumberOfRhsExprInModule>0) && (AC.mparallelflag == PARALLELFLAG) ){
		MesPrint("WARNING!: RHS expression names - the module %d is forced to run in sequential mode.", AC.CModule);
		/*[30jan2004 mt]:*/
		/*AC.mparallelflag = NOPARALLELFLAG;*/
		AC.mparallelflag = NOPARALLEL_RHS;
		/*:[30jan2004 mt]*/
	}/*if( (AC.NumberOfRhsExprInModule>0) && (AC.mparallelflag == PARALLELFLAG) )*/
#endif
	/*:[07nov2003 mt]*/

	if ( AC.SetupFlag ) WriteSetup();
	if ( AC.NamesFlag || AC.CodesFlag ) WriteLists();
	if ( par == GLOBALMODULE ) MakeGlobal();
	if ( RevertScratch() ) return(-1);
	if ( AP.preError == 0 && ( Processor() || WriteAll() ) ) RetCode = -1;
	TableReset();
	if ( AC.tableuse ) { M_free(AC.tableuse,"tableuse"); AC.tableuse = 0; }
/*
	@@@@@@@@@@@@@@@ can be removed? [03dec2002 df]
	Here should be the code to combine dollars from the various slaves
	when we are running on more than one processor.
	There are NumModOptdollars of them in the array ModOptdollars which
	is an array of objects of type MODOPTDOLLAR. Like:
	for ( i = 0; i < NumModOptdollars; i++ ) {
		for ( j = 0; j < NumPotModdollars; j++ ) {
			if ( ModOptdollars[i] == PotModdollars[j] ) break;
		}
		if ( j >= NumPotModdollars ) continue;
		switch ( ModOptdollars[i].type ) {
			case MODSUM:
				Collect here the results from all slaves for
				Dollars[ModOptdollars[i].number].
                etc.
			break;
			case MODMAX:
			break;
			case MODMIN:
			break;
			case MODNOKEEP:
			break;
			default:
				MesPrint("Serious internal error with module option");
				Terminate(-1);
			break;
		}
	}
*/

#ifdef PARALLEL /* [04dec2002 df] */

	if (PF.me == 0) {

	  PFDollars = (PFDOLLARS *)Malloc1(NumDollars*sizeof(PFDOLLARS), "pointer to PFDOLLARS");

	  for (i = 1; i < NumDollars; i++) {
	    PFDollars[i].slavebuf = (WORD**)Malloc1(PF.numtasks*sizeof(WORD*),
						    "pointer to array of slave buffers");
	    for (j = 0; j < PF.numtasks; j++) {
	      PFDollars[i].slavebuf[j] = &(AM.dollarzero);
	    }
	  }
	}
	/*[30jan2004 mt]:*/
	/*
	if (AC.mparallelflag != NOPARALLELFLAG && NumModOptdollars > 0 && NumPotModdollars > 0) {
	*/
	if (AC.mparallelflag == PARALLELFLAG && NumModOptdollars > 0 && NumPotModdollars > 0) {
	/*:[30jan2004 mt]*/
	  int attach = 0, error;
	  int source, src, tag, index, namesize;
	  UBYTE *name, *p, *textdoll;
	  WORD type, *where, *r;
	  LONG size;
	  DOLLARS  d, newd;


	  if (PF.me == 0) {
	    for (source = 1; source < PF.numtasks; source++) {

	      PF_Receive(PF_ANY_SOURCE, PF_DOLLAR_MSGTAG, &src, &tag);
	      PF_UnPack(&attach, 1, PF_INT);

	      if (attach) {
		switch(attach) {
		case PF_ATTACH_DOLLAR:
		  {
		    /*   printf(" Received PF_DOLLAR_MSGTAG from slave %d\n", src); */
		    for (i = 0; i < NumPotModdollars; i++) {

		      PF_UnPack(&namesize, 1, PF_INT);

		      name = (UBYTE*)Malloc1(namesize, "dollar name");

		      PF_UnPack(name, namesize, PF_BYTE);
		      PF_UnPack(&type, 1, PF_WORD);

		      if (type != DOLZERO) {
			PF_UnPack(&size, 1, PF_LONG);
			where = (WORD*)Malloc1(sizeof(WORD)*(size+1), "dollar content");
			PF_UnPack(where, size+1, PF_WORD);
		      } 
		      else {
			where = &(AM.dollarzero);
		      }
		      index = GetDollar(name);
		      for ( j = 0; j < NumModOptdollars; j++ ) {
			if (ModOptdollars[j].number = index) {
			  PFDollars[index].type = ModOptdollars[j].type;
			  break;
			}
		      }
		      if (j >= NumModOptdollars) printf(" Error in dollar transfer \n");
		      PFDollars[index].slavebuf[src] = where;
		      if (name) M_free(name, "dollar name");
		    }
		  }
		default:
              break;
		}
	      }
	    }/*for (source = 1; source < PF.numtasks; source++)*/

	    for (i = 0; i < NumPotModdollars; i++) {
	      index = PotModdollars[i];
	      switch (PFDollars[index].type) {
              case MODSUM:
		{
		  if (SumDollars(index)) MesPrint("error in SumDollars");
		  break;
		}
              case MODMAX:
		{
		  if (MaxDollar(index)) MesPrint("error in MaxDollar");
		  break;
		}
              case MODMIN:
		{
		  if (MinDollar(index)) MesPrint("error in MinDollar");
		  break;
		}
              case MODNOKEEP:
		{
		  d = Dollars + index;
		  if (d->where && d->where != &(AM.dollarzero)) {
		    M_free(d->where, "old content of dollar");
		  }
		  
		  d->type  = DOLZERO;
		  d->where = &(AM.dollarzero);
		  d->size  = 0;
		  
		  cbuf[AM.dbufnum].rhs[index] = d->where;
		  
		  break;
		}
              default:
		{
		  MesPrint("Serious internal error with module option");
		  Terminate(-1);
		  break;
		}
	      }
	      /*      textdoll = WriteDollarToBuffer(index);
		      printf(" new $-content is %s\n", textdoll); */
	    }

	    /*   Broadcasting to slaves new values of dollar variables                */

	    /*   printf("\n\n Broadcasting dollar values back to slaves ...\n\n");    */

	    PF_BroadCast(0);

	    for (i = 0; i < NumPotModdollars; i++) {
	      index = PotModdollars[i];
	      
	      p = name = AC.dollarnames->namebuffer+Dollars[index].name;
	      namesize = 1;

	      while(*p++) namesize++;

	      if (!(newd=DolToTerms(index))) {
		newd=0; /* this type of dollars will not be send to master */
	      }

	      PF_Pack(&namesize, 1, PF_INT);
	      PF_Pack(name, namesize, PF_BYTE);

	      if (newd != 0) {
		PF_Pack(&(newd->type), 1, PF_WORD);
		PF_Pack(&(newd->size), 1, PF_LONG);
		PF_Pack(newd->where, newd->size+1, PF_WORD);
	      } else {
		type = DOLZERO;
		PF_Pack(&type, 1, PF_WORD);
	      }
	    }
	    
	    PF_BroadCast(1);
	  } 
	  else {
	    PF_Send(MASTER, PF_DOLLAR_MSGTAG, 0);

	    attach = PF_ATTACH_DOLLAR;
	    PF_Pack(&attach, 1, PF_INT);

	    for (i = 0; i < NumPotModdollars; i++) {

	      index = PotModdollars[i];
	      p = name  = AC.dollarnames->namebuffer+Dollars[index].name;
	      namesize = 1;
	      while(*p++) namesize++;

	      if (!(newd=DolToTerms(index))) {
		newd=0; /* this type of dollars will not be send to master */
	      }

	      /* tmp code
		 printf(" after to DOLTERMS conversion ...\n");
		 textdoll = WriteDollarToBuffer(index);
		 printf(" $ name is %s\n", name);
		 printf(" $ content is %s\n", textdoll);
		 tmp code */

	      PF_Pack(&namesize, 1, PF_INT);
	      PF_Pack(name, namesize, PF_BYTE);

	      if (newd != 0) {
		PF_Pack(&(newd->type), 1, PF_WORD);
		PF_Pack(&(newd->size), 1, PF_LONG);
		PF_Pack(newd->where, newd->size+1, PF_WORD);
	      } 
	      else {
		type = DOLZERO;
		PF_Pack(&type, 1, PF_WORD);
	      }

	    }

	    PF_Send(MASTER, PF_DOLLAR_MSGTAG, 1);

	    /* Receiving new dollar values from MASTER process ... */

	    PF_BroadCast(1);

	    for (i = 0; i < NumPotModdollars; i++) {

	      PF_UnPack(&namesize, 1, PF_INT);

	      name = (UBYTE*)Malloc1(namesize, "dollar name");
	      PF_UnPack(name, namesize, PF_BYTE);

	      PF_UnPack(&type, 1, PF_WORD);

	      if (type != DOLZERO) {
		PF_UnPack(&size, 1, PF_LONG);
		where = (WORD*)Malloc1(sizeof(WORD)*(size+1), "dollar content");
		PF_UnPack(where, size+1, PF_WORD);
	      } 
	      else {
		where = &(AM.dollarzero);
	      }

	      index = GetDollar(name);

	      d = Dollars + index;
	      if (d->where && d->where != &(AM.dollarzero)) {
		M_free(d->where, "old content of dollar");
	      }

	      d->type  = type;
	      d->where = where;
	      
	      if (type != DOLZERO) {
		if (where == 0 || *where == 0) {
		  d->type  = DOLZERO;
		  if (where) M_free(where, "received dollar content");
		  d->where = &(AM.dollarzero); d->size  = 0;
		} 
		else {
		  r = d->where; while(*r) r += *r;
		  d->size = r - d->where;
		}
	      }
	      	      
	      cbuf[AM.dbufnum].rhs[index] = d->where;

	      if (name) M_free(name, "dollar name");
	      
	    }

	  }

	}

	if (PF.me == 0) {

	  for (i = 1; i < NumDollars; i++) {
	    for (j = 0; j < PF.numtasks; j++) {
	      if (PFDollars[i].slavebuf[j] != &(AM.dollarzero)) {
		M_free(PFDollars[i].slavebuf[j], "slave buffer");
	      }
	    }
	    M_free(PFDollars[i].slavebuf, "pointer to slave buffers");
	  }

	  M_free(PFDollars, "pointer to PFDOLLARS");

	}

#endif /* PARALLEL [04dec2002 df] */

/*
	@@@@@@@@@@@@@@@
	Now follows the code to invalidate caches for all objects in the
	PotModdollars. There are NumPotModdollars of them and PotModdollars
	is an array of WORD.
*/
/*
	Cleanup:
*/
        if(ModOptdollars) M_free(ModOptdollars, "ModOptdollars pointer");
	if(PotModdollars) M_free(PotModdollars, "PotModdollars pointer");
	
	/* ModOptdollars changed to AC.ModOptDolList.lijst because AIX C compiler complained. MF 30/07/2003. */
	AC.ModOptDolList.lijst = NULL;
	/* PotModdollars changed to AC.PotModDolList.lijst because AIX C compiler complained. MF 30/07/2003. */
	AC.PotModDolList.lijst = NULL;

	NumPotModdollars = 0;
	NumModOptdollars = 0;

skipexec:
	AC.DidClean = 0;
	TestDrop();
	if ( par == STOREMODULE || par == CLEARMODULE ) {
		if ( par == STOREMODULE && PopVariables() ) RetCode = -1;
		if ( AR.infile->handle >= 0 ) {
			CloseFile(AR.infile->handle);
			remove(AR.infile->name);
			AR.infile->handle = -1;
		}
		AR.infile->POfill = AR.infile->PObuffer;
		PUTZERO(AR.infile->POposition);
		AR.infile->POfull = AR.infile->PObuffer;
		if ( AR.outfile->handle >= 0 ) {
			CloseFile(AR.outfile->handle);
			remove(AR.outfile->name);
			AR.outfile->handle = -1;
		}
		AR.outfile->POfull =
		AR.outfile->POfill = AR.outfile->PObuffer;
		PUTZERO(AR.outfile->POposition);
		if ( AS.hidefile->handle >= 0 ) {
			CloseFile(AS.hidefile->handle);
			remove(AS.hidefile->name);
			AS.hidefile->handle = -1;
		}
		AS.hidefile->POfull =
		AS.hidefile->POfill = AS.hidefile->PObuffer;
		PUTZERO(AS.hidefile->POposition);
		AC.HideLevel = 0;
		if ( par == CLEARMODULE ) {
			if ( DeleteStore(0) < 0 ) {
				MesPrint("Cannot restart the storage file");
				RetCode = -1;
			}
			else RetCode = 0;
			CleanUp(1);
			ResetVariables(2);
			AM.gSlavePatchSize = AM.hSlavePatchSize;
			AM.gparallelflag = AM.hparallelflag;
			IniVars();
		}
	}
	else {
		if ( CleanExpr(0) ) RetCode = -1;
		if ( AC.DidClean ) CompactifyTree(AC.exprnames);
		ResetVariables(0);
		CleanUpSort(-1);
	}
	return(RetCode);
}

/*
 		#] DoExecute :
 		#[ PutBracket :

	Routine uses the bracket info to split a term into two pieces:
	1: the part outside the bracket, and
	2: the part inside the bracket.
	These parts are separated by a subterm of type HAAKJE.
	This subterm looks like: HAAKJE,3,level
	The level is used for nestings of brackets. The print routines
	cannot handle this yet (31-Mar-1988).

	The Bracket selector is in AR.BrackBuf in the form of a regular term,
	but without coefficient.
	When AR.BracketOn < 0 we have a socalled antibracket. The main effect
	is an exchange of the inner and outer part and where the coefficient goes.

	Routine recoded to facilitate b p1,p2; etc for dotproducts and tensors
	15-oct-1991
*/

WORD
PutBracket ARG1(WORD *,termin)
{
	WORD *t, *t1, *b, i, j, *lastfun;
	WORD *t2, *s1, *s2;
	WORD *bStop, *bb, *tStop;
	WORD *term1,*term2, *m1, *m2, *tStopa;
	WORD *bbb = 0, *bind, *binst = 0, bwild = 0;
	term1 = AR.WorkPointer+1;
	term2 = term1 + AM.MaxTer;
	if ( ( term2 + AM.MaxTer ) > AM.WorkTop ) return(MesWork());
	if ( AR.BracketOn < 0 ) {
		t2 = term1; t1 = term2;		/* AntiBracket */
	}
	else {
		t1 = term1; t2 = term2;		/* Regular bracket */
	}
	b = AR.BrackBuf; bStop = b+*b; b++;
	while ( b < bStop ) {
		if ( *b == INDEX ) { bwild = 1; bbb = b+2; binst = b + b[1]; break; }
		b += b[1];
	}

	t = termin; tStopa = t + *t; i = *(t + *t -1); i = ABS(i);
	if ( AC.PolyFun && AR.PolyAct ) tStop = termin + AR.PolyAct;
	else tStop = tStopa - i;
	t++;
	if ( AR.BracketOn < 0 ) {
		lastfun = 0;
		while ( t < tStop && *t >= FUNCTION
			&& functions[*t-FUNCTION].commute ) {
			b = AR.BrackBuf+1;
			while ( b < bStop ) {
				if ( *b == *t ) {
					lastfun = t;
					while ( t < tStop && *t >= FUNCTION
						&& functions[*t-FUNCTION].commute ) t += t[1];
					goto NextNcom1;
				}
				b += b[1];
			}
			if ( bwild && *t >= FUNCTION && functions[*t-FUNCTION].spec ) {
				s1 = t + t[1];
				s2 = t + FUNHEAD;
				while ( s2 < s1 ) {
					bind = bbb;
					while ( bind < binst ) {
						if ( *bind == *s2 ) {
							lastfun = t;
							while ( t < tStop && *t >= FUNCTION
								&& functions[*t-FUNCTION].commute ) t += t[1];
							goto NextNcom1;
						}
						bind++;
					}
					s2++;
				}
			}
			t += t[1];
		}
NextNcom1:
		s1 = termin + 1;
		if ( lastfun ) {
			while ( s1 < lastfun ) *t2++ = *s1++;
			while ( s1 < t ) *t1++ = *s1++;
		}
		else {
			while ( s1 < t ) *t2++ = *s1++;
		}

	}
	else {
		lastfun = t;
		while ( t < tStop && *t >= FUNCTION
			&& functions[*t-FUNCTION].commute ) {
			b = AR.BrackBuf+1;
			while ( b < bStop ) {
				if ( *b == *t ) { lastfun = t + t[1]; goto NextNcom; }
				b += b[1];
			}
			if ( bwild && *t >= FUNCTION && functions[*t-FUNCTION].spec ) {
				s1 = t + t[1];
				s2 = t + FUNHEAD;
				while ( s2 < s1 ) {
					bind = bbb;
					while ( bind < binst ) {
						if ( *bind == *s2 ) { lastfun = t + t[1]; goto NextNcom; }
						bind++;
					}
					s2++;
				}
			}
NextNcom:
			t += t[1];
		}
		s1 = termin + 1;
		while ( s1 < lastfun ) *t1++ = *s1++;
		while ( s1 < t ) *t2++ = *s1++;
	}
/*
	Now we have only commuting functions left. Move the b pointer to them.
*/
	b = AR.BrackBuf + 1;
	while ( b < bStop && *b >= FUNCTION
		&& ( *b < FUNCTION || functions[*b-FUNCTION].commute ) ) {
		b += b[1];
	}

	while ( t < tStop && ( b < bStop || bwild ) ) {
		while ( b < bStop && *b != *t ) {
			if ( *b > *t && *t < FUNCTION ) b += b[1];
			else if ( *t >= FUNCTION && *b >= FUNCTION && *b < *t ) b += b[1];
			else break;
		}
/*		while ( b < bStop && *b > *t ) b += b[1]; */
		i = t[1];
		if ( *t >= FUNCTION ) { /* We are in function territory */
			if ( b < bStop && *b == *t ) goto FunBrac;
			if ( bwild && *t >= FUNCTION && functions[*t-FUNCTION].spec ) {
				s1 = t + t[1];
				s2 = t + FUNHEAD;
				while ( s2 < s1 ) {
					bind = bbb;
					while ( bind < binst ) {
						if ( *bind == *s2 ) goto FunBrac;
						bind++;
					}
					s2++;
				}
			}
			NCOPY(t2,t,i);
			continue;
FunBrac:	NCOPY(t1,t,i);
			continue;
		}
/*
	We have left: DELTA, INDEX, VECTOR, DOTPRODUCT, SYMBOL
*/
		if ( *t == DELTA ) {
			if ( b < bStop && *b == DELTA ) {
				b += b[1];
				NCOPY(t1,t,i);
			}
			else { NCOPY(t2,t,i); }
		}
		else if ( *t == INDEX ) {
			if ( bwild ) {
				m1 = t1; m2 = t2;
				*t1++ = *t; t1++; *t2++ = *t; t2++;
				bind = bbb;
				j = t[1] -2;
				t += 2;
				while ( --j >= 0 ) {
					while ( *bind < *t && bind < binst ) bind++;
					if ( *bind == *t && bind < binst ) {
						*t1++ = *t++;
					}
					else *t2++ = *t++;
				}
				m1[1] = WORDDIF(t1,m1);
				if ( m1[1] == 2 ) t1 = m1;
				m2[1] = WORDDIF(t2,m2);
				if ( m2[1] == 2 ) t2 = m2;
			}
			else {
				NCOPY(t2,t,i);
			}
		}
		else if ( *t == VECTOR ) {
			if ( ( b < bStop && *b == VECTOR ) || bwild ) {
				if ( *b == VECTOR && b < bStop ) {
					bb = b + b[1]; b += 2;
				}
				else bb = b;
				j = t[1] - 2;
				m1 = t1; m2 = t2; *t1++ = *t; *t2++ = *t; t1++; t2++; t += 2;
				while ( j > 0 ) {
					j -= 2;
					while ( b < bb && ( *b < *t ||
					( *b == *t && b[1] < t[1] ) ) ) b += 2;
					if ( b < bb && ( *t == *b && t[1] == b[1] ) ) {
						*t1++ = *t++; *t1++ = *t++; goto nextvec;
					}
					else if ( bwild ) {
						bind = bbb;
						while ( bind < binst ) {
							if ( *t == *bind || t[1] == *bind ) {
								*t1++ = *t++; *t1++ = *t++;
								goto nextvec;
							}
							bind++;
						}
					}
					*t2++ = *t++; *t2++ = *t++;
nextvec:;
				}
				m1[1] = WORDDIF(t1,m1);
				if ( m1[1] == 2 ) t1 = m1;
				m2[1] = WORDDIF(t2,m2);
				if ( m2[1] == 2 ) t2 = m2;
			}
			else {
				NCOPY(t2,t,i);
			}
		}
		else if ( *t == DOTPRODUCT ) {
			if ( ( b < bStop && *b == *t ) || bwild ) {
				m1 = t1; *t1++ = *t; t1++;
				m2 = t2; *t2++ = *t; t2++;
				if ( b >= bStop || *b != *t ) { bb = b; s1 = b; }
				else {
					s1 = b + b[1]; bb = b + 2;
				}
				s2 = t + i; t += 2;
				while ( t < s2 && ( bb < s1 || bwild ) ) {
					while ( bb < s1 && ( *bb < *t ||
					( *bb == *t && bb[1] < t[1] ) ) ) bb += 3;
					if ( *bb == *t && bb[1] == t[1] && bb < s1 ) {
						*t1++ = *t++; *t1++ = *t++; *t1++ = *t++; bb += 3;
						goto nextdot;
					}
					else if ( bwild ) {
						bind = bbb;
						while ( bind < binst ) {
							if ( *bind == *t || *bind == t[1] ) {
								*t1++ = *t++; *t1++ = *t++; *t1++ = *t++;
								goto nextdot;
							}
							bind++;
						}
					}
					*t2++ = *t++; *t2++ = *t++; *t2++ = *t++;
nextdot:;
				}
				while ( t < s2 ) *t2++ = *t++;
				m1[1] = WORDDIF(t1,m1);
				if ( m1[1] == 2 ) t1 = m1;
				m2[1] = WORDDIF(t2,m2);
				if ( m2[1] == 2 ) t2 = m2;
			}
			else { NCOPY(t2,t,i); }
		}
		else if ( *t == SYMBOL ) {
			if ( b < bStop && *b == *t ) {
				m1 = t1; *t1++ = *t; t1++;
				m2 = t2; *t2++ = *t; t2++;
				s1 = b + b[1]; bb = b+2;
				s2 = t + i; t += 2;
				while ( bb < s1 && t < s2 ) {
					while ( *bb < *t && bb < s1 ) bb += 2;
					if ( bb >= s1 ) break;
					if ( *bb == *t ) { *t1++ = *t++; *t1++ = *t++; }
					else { *t2++ = *t++; *t2++ = *t++; } 
				}
				while ( t < s2 ) *t2++ = *t++;
				m1[1] = WORDDIF(t1,m1);
				if ( m1[1] == 2 ) t1 = m1;
				m2[1] = WORDDIF(t2,m2);
				if ( m2[1] == 2 ) t2 = m2;
			}
			else { NCOPY(t2,t,i); }
		}
		else {
			NCOPY(t2,t,i);
		}
	}
	if ( ( i = WORDDIF(tStop,t) ) > 0 ) NCOPY(t2,t,i);
	if ( AR.BracketOn < 0 ) {
		s1 = t1; t1 = t2; t2 = s1;
	}
	do { *t2++ = *t++; } while ( t < (WORD *)tStopa );
	t = AR.WorkPointer;
	i = WORDDIF(t1,term1);
	*t++ = 4 + i + WORDDIF(t2,term2);
	t += i;
	*t++ = HAAKJE;
	*t++ = 3;
	*t++ = 0;			/* This feature won't be used for a while */
	i = WORDDIF(t2,term2);
	t1 = term2;
	if ( i > 0 ) NCOPY(t,t1,i);

	AR.WorkPointer = t;

	return(0);
}

/*
 		#] PutBracket :
 	#] DoExecute :
 	#[ Expressions :
 		#[ ExchangeExpressions :
*/

void ExchangeExpressions ARG2(int,num1,int,num2)
{
	WORD node1, node2, namesize, TMproto[SUBEXPSIZE];
	INDEXENTRY *ind;
	EXPRESSIONS e1, e2;
	LONG a;
	SBYTE *s1, *s2;
	int i;
	e1 = Expressions + num1;
	e2 = Expressions + num2;
	node1 = e1->node;
	node2 = e2->node;
	AC.exprnames->namenode[node1].number = num2;
	AC.exprnames->namenode[node2].number = num1;
	a = e1->name; e1->name = e2->name; e2->name = a;
	namesize = e1->namesize; e1->namesize = e2->namesize; e2->namesize = namesize;
	e1->node = node2;
	e2->node = node1;
	if ( e1->status == STOREDEXPRESSION ) {
/*
		Find the name in the index and replace by the new name
*/
		TMproto[0] = EXPRESSION;
		TMproto[1] = SUBEXPSIZE;
		TMproto[2] = num1;
		TMproto[3] = 1;
		{ int ie; for ( ie = 4; ie < SUBEXPSIZE; ie++ ) TMproto[ie] = 0; }
		AR.TMaddr = TMproto;
		ind = FindInIndex(num1,&AO.StoreData,0);
		s1 = (SBYTE *)(AC.exprnames->namebuffer+e1->name);
		i = e1->namesize;
		s2 = ind->name;
		NCOPY(s2,s1,i);
		*s2 = 0;
		SeekFile(AO.StoreData.Handle,&(e1->onfile),SEEK_SET);
		if ( WriteFile(AO.StoreData.Handle,(UBYTE *)ind,
		(LONG)(sizeof(INDEXENTRY))) != sizeof(INDEXENTRY) ) {
			MesPrint("File error while exchanging expressions");
			Terminate(-1);
		}
		FlushFile(AO.StoreData.Handle);
	}
	if ( e2->status == STOREDEXPRESSION ) {
/*
		Find the name in the index and replace by the new name
*/
		TMproto[0] = EXPRESSION;
		TMproto[1] = SUBEXPSIZE;
		TMproto[2] = num2;
		TMproto[3] = 1;
		{ int ie; for ( ie = 4; ie < SUBEXPSIZE; ie++ ) TMproto[ie] = 0; }
		AR.TMaddr = TMproto;
		ind = FindInIndex(num1,&AO.StoreData,0);
		s1 = (SBYTE *)(AC.exprnames->namebuffer+e2->name);
		i = e2->namesize;
		s2 = ind->name;
		NCOPY(s2,s1,i);
		*s2 = 0;
		SeekFile(AO.StoreData.Handle,&(e2->onfile),SEEK_SET);
		if ( WriteFile(AO.StoreData.Handle,(UBYTE *)ind,
		(LONG)(sizeof(INDEXENTRY))) != sizeof(INDEXENTRY) ) {
			MesPrint("File error while exchanging expressions");
			Terminate(-1);
		}
		FlushFile(AO.StoreData.Handle);
	}
}

/*
 		#] ExchangeExpressions :
 		#[ GetFirstBracket :
*/

int GetFirstBracket ARG2(WORD *,term,int,num)
{
/*
		Gets the first bracket of the expression 'num'
		Puts it in term. If no brackets the answer is one.
*/
	POSITION position, oldposition;
	RENUMBER renumber;
	FILEHANDLE *fi;
	WORD type, *oldcomppointer, oldonefile, hand, numword;
	WORD *t, *tstop;

	oldcomppointer = AR.CompressPointer;
	type = Expressions[num].status;
	if ( type == STOREDEXPRESSION ) {
		WORD TMproto[SUBEXPSIZE];
		TMproto[0] = EXPRESSION;
		TMproto[1] = SUBEXPSIZE;
		TMproto[2] = num;
		TMproto[3] = 1;
		{ int ie; for ( ie = 4; ie < SUBEXPSIZE; ie++ ) TMproto[ie] = 0; }
		AR.TMaddr = TMproto;
		PUTZERO(position);
		if ( ( renumber = GetTable(num,&position) ) == 0 ) {
			MesCall("GetFirstBracket");
			SETERROR(-1)
		}
		if ( GetFromStore(term,&position,renumber,&numword,num) < 0 ) {
			MesCall("GetFirstBracket");
			SETERROR(-1)
		}
	}
	else {			/* Active expression */
		oldonefile = AR.GetOneFile;
		if ( type == HIDDENLEXPRESSION || type == HIDDENGEXPRESSION ) {
			AR.GetOneFile = 2; fi = AS.hidefile;
		}
		else {
			AR.GetOneFile = 0; fi = AR.infile;
		}
		hand = fi->handle;
		if ( hand >= 0 ) {
			PUTZERO(oldposition);
			SeekFile(fi->handle,&oldposition,SEEK_CUR);
			SeekFile(hand,&(AS.OldOnFile[num]),SEEK_SET);
			if ( ISNEGPOS(AS.OldOnFile[num]) ) {
				MesPrint("File error");
				SETERROR(-1)
			}
		}
		else {
			SETBASEPOSITION(oldposition,fi->POfill-fi->PObuffer);
			fi->POfill = (WORD *)((UBYTE *)(fi->PObuffer) + BASEPOSITION(AS.OldOnFile[num]));
		}
		if ( GetOneTerm(term,hand) < 0 || GetOneTerm(term,hand) < 0 ) {
			MesCall("GetFirstBracket");
			SETERROR(-1)
		}
		if ( hand >= 0 ) {
			SeekFile(hand,&oldposition,SEEK_SET);
			if ( ISNEGPOS(oldposition) ) {
				MesPrint("File error");
				SETERROR(-1)
			}
		}
		else {
			fi->POfill = fi->PObuffer+BASEPOSITION(oldposition);
		}
		AR.GetOneFile = oldonefile;
	}
	AR.CompressPointer = oldcomppointer;
	if ( *term ) {
		tstop = term + *term; tstop -= ABS(tstop[-1]);
		t = term + 1;
		while ( t < tstop ) {
			if ( *t == HAAKJE ) break;
			t += t[1];
		}
		if ( t >= tstop ) {
			term[0] = 4; term[1] = 1; term[2] = 1; term[3] = 3;
		}
		else {
			*t++ = 1; *t++ = 1; *t++ = 3; *term = t - term;
		} 
	}
	else {
		term[0] = 4; term[1] = 1; term[2] = 1; term[3] = 3;
	}
	return(*term);
}

/*
 		#] GetFirstBracket :
 		#[ TermsInExpression :
*/

LONG TermsInExpression ARG1(WORD,num)
{
	LONG x = Expressions[num].counter;
	if ( x >= 0 ) return(x);
	return(-1);
}

/*
 		#] TermsInExpression :
 		#[ UpdatePositions :
*/

void UpdatePositions()
{
	EXPRESSIONS e = Expressions;
	POSITION *old;
	int i;
/*   Original:														  */
/*  	if ( AS.OldOnFile == 0 || AS.NumOldOnFile < NumExpressions ) { */
/*  Changed by albert at July 27 1999  */
	if ( NumExpressions > 0 &&
		 (AS.OldOnFile == 0 || AS.NumOldOnFile < NumExpressions) ) {
/*  end of changes 													  */
		if ( AS.OldOnFile ) {
			old = AS.OldOnFile;
			AS.OldOnFile = (POSITION *)Malloc1(NumExpressions*sizeof(POSITION),"file pointers");
			for ( i = 0; i < AS.NumOldOnFile; i++ ) AS.OldOnFile[i] = old[i];
			AS.NumOldOnFile = NumExpressions;
			M_free(old,"proces file pointers");
		}
		else {
			AS.OldOnFile = (POSITION *)Malloc1(NumExpressions*sizeof(POSITION),"file pointers");
			AS.NumOldOnFile = NumExpressions;
		}
	}
	for ( i = 0; i < NumExpressions; i++ ) { AS.OldOnFile[i] = e[i].onfile; }
}

/*
 		#] UpdatePositions :
 	#] Expressions :
*/






/* temporary commentary for forcing cvs merge */
