/** @file comtool.h
 *
 *  Utility routines for the compiler.
 */
/* #[ License : */
/*
 *   Copyright (C) 1984-2017 J.A.M. Vermaseren
 *   When using this file you are requested to refer to the publication
 *   J.A.M.Vermaseren "New features of FORM" math-ph/0010025
 *   This is considered a matter of courtesy as the development was paid
 *   for by FOM the Dutch physics granting agency and we would like to
 *   be able to track its scientific use to convince FOM of its value
 *   for the community.
 *
 *   This file is part of FORM.
 *
 *   FORM is free software: you can redistribute it and/or modify it under the
 *   terms of the GNU General Public License as published by the Free Software
 *   Foundation, either version 3 of the License, or (at your option) any later
 *   version.
 *
 *   FORM is distributed in the hope that it will be useful, but WITHOUT ANY
 *   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 *   details.
 *
 *   You should have received a copy of the GNU General Public License along
 *   with FORM.  If not, see <http://www.gnu.org/licenses/>.
 */
/* #] License : */ 
#ifndef FORM_COMTOOL_H_
#define FORM_COMTOOL_H_
/*
  	#[ Includes :
*/

#include "form3.h"

/*
  	#] Includes : 
  	#[ Inline functions :
*/

/**
 * Skips white-spaces in the buffer. Here the white-spaces includes commas,
 * which is treated as a space in FORM.
 *
 * @param[in,out]  s  The pointer to the buffer.
 */
static inline void SkipSpaces(UBYTE **s)
{
	const char *p = (const char *)*s;
	while ( *p == ' ' || *p == ',' || *p == '\t' ) p++;
	*s = (UBYTE *)p;
}

/**
 * Checks if the next word in the buffer is the given keyword, with ignoring
 * case. If found, the pointer is moved such that the keyword is consumed in the
 * buffer, and this function returns a non-zero value.
 *
 * @param[in,out]  s    The pointer to the buffer. Changed if the keyword found.
 * @param          opt  The optional keyword.
 * @return              1 if the keyword found, otherwise 0.
 */
static inline int ConsumeOption(UBYTE **s, const char *opt)
{
	const char *p = (const char *)*s;
	while ( *p && *opt && tolower(*p) == tolower(*opt) ) {
		p++;
		opt++;
	}
	/* Check if `opt` ended. */
	if ( !*opt ) {
		/* Check if `*p` is a word boundary. */
		if ( !*p || !(FG.cTable[(unsigned char)*p] == 0 ||
		              FG.cTable[(unsigned char)*p] == 1 || *p == '_' ||
		              *p == '$') ) {
			/* Consume the option. Skip the trailing spaces. */
			*s = (UBYTE *)p;
			SkipSpaces(s);
			return(1);
		}
	}
	return(0);
}

/*
  	#] Inline functions : 
*/
#endif  /* FORM_COMTOOL_H_ */
