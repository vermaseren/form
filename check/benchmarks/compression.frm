#ifndef `TEST'
  #message Use -D TEST=XXX
  #terminate
#else
  #include `NAME_' # `TEST'
#endif
.end

*--#[ compress-zstd :
#-

* This is a benchmark to test compression performance. It makes a
* large-ish expression, and then reads, sorts, writes it ITER times
* without doing much else.

* Keep expressions in memory. We want to test only the sort files.
* NTERMS=2^20 and PADDING=25 uses ~2.5G of scratch, and the test
* takes O(2 mins).
#: ScratchSize 3G
* Write out patches more regularly during sorting
#: TermsInSmall 10K

Off statistics;
Off threadstats;

* This is the default when FORM is compiled with zstd support.
*On compress,zstd;

#define NTERMS "{2^20}"
#define PADDING "25"
#define BLOCKS "16"
#define ITER "10"

Symbol n,m,x,y;
CFunction f,g,h,sum;

Local test =
	#do b = 1,`BLOCKS'
		+ f(`b') * sum(x,1,{`NTERMS'/`BLOCKS'},g(x))
	#enddo
	- (1+`BLOCKS')*`NTERMS'*(`BLOCKS'+`NTERMS')/4/`BLOCKS'
	;
.sort
Identify sum(?a) = sum_(?a);
* Make the terms larger.
Identify f(n?)*g(m?) = f(n)*g(m) * h((n*x+m*y)^`PADDING');

* Pointlessly read and sort and write the terms
#do i = 1,`ITER'
	#message i = `i' / `ITER'
	.sort
#enddo

#message Finish up
Identify h(x?) = 1;
Identify f(x?) = x;
Identify g(x?) = x;

Print;
.end
#time_dilation 20.0
assert succeeded?
assert result("test") =~ expr("0")
*--#] compress-zstd : 
