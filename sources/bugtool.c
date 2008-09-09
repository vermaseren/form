/** @file bugtool.c
 *
 *  Low level routines for debugging
 */
/*
  	#[ Includes :
*/

#include "form3.h"
 
/*
  	#] Includes :
  	#[ ExprStatus :
*/

static UBYTE *statusexpr[] = {
	 (UBYTE *)"LOCALEXPRESSION"
	,(UBYTE *)"SKIPLEXPRESSION"
	,(UBYTE *)"DROPLEXPRESSION"
	,(UBYTE *)"DROPPEDEXPRESSION"
	,(UBYTE *)"GLOBALEXPRESSION"
	,(UBYTE *)"SKIPGEXPRESSION"
	,(UBYTE *)"DROPGEXPRESSION"
	,(UBYTE *)"UNKNOWN"
	,(UBYTE *)"STOREDEXPRESSION"
	,(UBYTE *)"HIDDENLEXPRESSION"
	,(UBYTE *)"HIDELEXPRESSION"
	,(UBYTE *)"DROPHLEXPRESSION"
	,(UBYTE *)"UNHIDELEXPRESSION"
	,(UBYTE *)"HIDDENGEXPRESSION"
	,(UBYTE *)"HIDEGEXPRESSION"
	,(UBYTE *)"DROPHGEXPRESSION"
	,(UBYTE *)"UNHIDEGEXPRESSION"
	,(UBYTE *)"INTOHIDELEXPRESSION"
	,(UBYTE *)"INTOHIDEGEXPRESSION"
};

void ExprStatus(EXPRESSIONS e)
{
	MesPrint("Expression %s(%d) has status %s(%d,%d). Buffer: %d, Position: %15p",
		AC.exprnames->namebuffer+e->name,(WORD)(e-Expressions),
		statusexpr[e->status],e->status,e->hidelevel,
		e->whichbuffer,&(e->onfile));
}

/*
  	#] ExprStatus :
*/
