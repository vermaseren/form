#ifndef `TEST'
  #message Use -D TEST=XXX
  #terminate
#else
  #include `NAME_' # `TEST'
#endif
.end

*--#[ checkpoint_1 :
* This program is intended to be executed 4 times, for example,
*   form -D TEST=checkpoint_1 checkpoint.frm  # ok
*   form -D TEST=checkpoint_1 checkpoint.frm  # fail
*   form -D TEST=checkpoint_1 -R checkpoint.frm  # fail
*   form -D TEST=checkpoint_1 -R checkpoint.frm  # ok
* We check the result at the end of the program, by which we expect
* possible deficits in snapshot/recovery would be detected.
* Unix is required. ParFORM is not supported.
#ifndef `N'
  #define N "8"
#endif

#ifdef `QUIET'
  Off stats;
#endif

#define StopThenContinueLockFilePrefix ".stc-"
#define StopThenContinueCounter "0"

#procedure StopThenContinue(?exitcode)
  #ifdef `?exitcode'
    #define exitcode "`?exitcode'"
  #else
    #define exitcode "-1"
  #endif
  #define counter "`StopThenContinueCounter++'"
  #define lockfilename "`StopThenContinueLockFilePrefix'`counter'.tmp"
* NOTE: for timestamps, the checkpoint feature uses TimeWallClock(),
* which has only a centisecond precision. We may need some sleep
* to guarantee that a snapshot file will be created (for low N,
* in particular; though the wall clock value is supposed to increase
* monotonically, I experienced cases where
* TimeWallClock(1) - AC.CheckpointStamp == -1).
* 0.05 seconds should be enough, but the POSIX sleep command assumes
* an integral number as its argument; we fall back to 1 second when
* it does not work.
  #system sleep 0.05 2>/dev/null || sleep 1
  .sort:StopThenContinue-`counter';
  #system -e test -f `lockfilename'
  #define error "`SYSTEMERROR_'"
  #write <`lockfilename'> ""
  #close <`lockfilename'>
  #if `error'
    #message Program intentionally stopped at `counter' (exitcode=`exitcode')
    #terminate `?exitcode'
  #endif
  #remove <`lockfilename'>
#endprocedure

#procedure AssertEqual(expr1,expr2)
  #$AssertEqualExpr1 = `expr1';
  #$AssertEqualExpr2 = `expr2';
  #$AssertEqualExpr3 = $AssertEqualExpr1 - $AssertEqualExpr2;
  #$AssertEqualExpr1 = nterms_($AssertEqualExpr3);
  #if `$AssertEqualExpr1' == 0
    #message Assertion OK: `expr1' == `expr2'
  #else
    #message Assertion Failed: `expr1' == `expr2'
    #terminate
  #endif
#endprocedure

#call StopThenContinue(0)

On checkpoint;
Auto S x;
L F = (x1+...+x15);
L G = (x1+...+x15)^`N';
#$n1 = 6;
#$e1 = 1+x1+...+x15;
#define a1 "20"
.sort

#call StopThenContinue()

id x1 = x1 + x2 + x3 + x4 + x5;
#$n2 = 7;
#$e2 = x1+...+x15;
#define a2 "21"
.sort

#call StopThenContinue()

id x6 = -x1;
id x7 = -x2;
id x8 = -x3;
id x9 = -x4;
id x10 = -x5;
id x11 = -x2;
id x12 = -x3;
id x13 = -x4;
id x14 = -x5;
#$e3 = $e1 - $e2;
#message x15 -> {`$n1'*`$n2'-`a1'-`a2'+`$e3'}
id x15 = {`$n1'*`$n2'-`a1'-`a2'+`$e3'};
#ifndef `QUIET'
  Print;
#endif
.sort
#call AssertEqual(F,2)
#call AssertEqual(G,{2^`N'})
.end
#system -e `FORM' 1.frm
#system -e `FORM' -R 1.frm
#system `FORM' -R 1.frm
.end
#require unix?
#pend_if mpi? || valgrind?
assert stdout =~ exact_pattern("Assertion OK: F")
assert stdout =~ exact_pattern("Assertion OK: G")
*--#] checkpoint_1 : 
