#:FilePatches 16
#:LargePatches 2
#:TermsInSmall 4

#ifndef `TEST'
  #message Use -D TEST=XXX
  #terminate
#else
  #include `NAME_' # `TEST'
#endif
.end

*--#[ multithreaded_1 :
#:FilePatches 16
#:LargePatches 2
#:TermsInSmall 4

* NOTE: tform -w4 -F -D TEST=multithreaded_1 multithreaded.frm
* should write the whole output to the log file (if not broken).
* Nothing should be printed in the standard output.
* This test checks the correct behaviour of threadstats discussed in:
* https://github.com/vermaseren/form/issues/470

#ifndef `N1'
  #define N1 "1"
#endif

#ifndef `N2'
  #define N2 "2"
#endif

#ifndef `N3'
  #define N3 "2"
#endif

#ifdef `TEST'

S x1,...,x9;
L F = (x1+...+x9)^`N1';
.sort
multiply (x1+...+x9)^`N2';
.sort
multiply (x1+...+x9)^`N3';
.sort
repeat id x1? = 1;
P +f;

#endif

.end
L F = 1;
P;
.sort
#system `FORM' -F -D TEST 1.frm
.end
assert succeeded?
# If the -F option in the above execution of (T)FORM works correctly,
# then F = 59049 is not printed in the standard output. So, the last
# printed expression of F must be 1.
assert result("F") =~ expr("1")
# The result should be written in the log file.
assert file("1.log") =~ pattern("59049")
*--#] multithreaded_1 :
