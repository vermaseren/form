/** @file checkpoint.c
 *
 *  Contains all functions that deal with the recovery mechanism controlled and
 *  activated by the On Checkpoint switch.
 *
 */
/*
  	#[ Includes :
*/

#include "form3.h"

/*
  	#] Includes :
  	#[ Declarations :
*/

/**
 *  filename for the recovery file
 */
char *recoveryfile = "FORMrecv.tmp";
/**
 *  filename for the intermediate recovery file. only if the write is
 *  completely successful, this file will be moved/renamed to the one
 *  named by recoveryfile. this offers atomicity for the snapshot generation.
 */
char *intermedfile = "FORMrecv.XXX";

/*
  	#] Declarations :
  	#[ CheckRecoveryFile :
*/

/**
 *  Checks whether a snapshot/recovery file exists.
 *  Returns 1 if it exists, 0 otherwise.
 */
int CheckRecoveryFile()
{
	FILE *fd;
	if ( (fd = fopen(recoveryfile, "r")) ) {
		fclose(fd);
		return 1;
	}
	return 0;
}

/*
  	#] CheckRecoveryFile :
  	#[ RecoveryFilename :
*/

/**
 *  Returns pointer to recovery filename. 
 */
char *RecoveryFilename()
{
	return recoveryfile;
}

/*
  	#] RecoveryFilename :
  	#[ DoRecovery :
*/

/* ONLY A STUB AT THE MOMENT */

/**
 *  Reads from the recovery file and restores all necessary variables and
 *  states in FORM, so that the execution can recommence in preprocessor() as
 *  if no restart of FORM had occured.
 */
int DoRecovery()
{
	FILE *fd;

	printf("#### DOING RECOVERY ###\n"); fflush(0);

	if ( !(fd = fopen(recoveryfile, "r")) ) {
		return 1;
	}

	if ( fclose(fd) ) {
		return 3;
	}

	return 0;
}

/*
  	#] DoRecovery :
  	#[ DoSnapshot :
*/

/* ONLY A STUB AT THE MOMENT */

/**
 *  Writes all relevant information for a recovery to the recovery file. It
 *  writes first to an intermediate file and then only if everything went well
 *  it renames this intermediate file to the final recovery file.
 */
int DoSnapshot()
{
	FILE *fd;

	printf("#### DOING SNAPSHOT ###\n"); fflush(0);

	if ( !(fd = fopen(intermedfile, "w")) ) {
		return 1;
	}

	if ( fclose(fd) ) {
		return 3;
	}

	if ( rename(intermedfile, recoveryfile) ) {
		return 4;
	}

	return 0;
}

/*
  	#] DoSnapshot :
  	#[ DoCheckpoint :
*/

/* ONLY A STUB AT THE MOMENT */

/**
 *  Checks whether a snapshot should be done. Calls DoSnapshot() to create the
 *  snapshot.
 */
void DoCheckpoint ARG0
{
	printf("#### DOING CHECKPOINT ###\n"); fflush(0);

	LONG timestamp = Timer(0);
	if ( timestamp - AC.CheckpointStamp > AC.CheckpointInterval ) {
		/* call script */
		if ( DoSnapshot() ) {
			/* handle error */
		}
		/* call script */
		/* print status message */
	}
	AC.CheckpointStamp = Timer(0);
}

/*
  	#] DoCheckpoint :
*/
