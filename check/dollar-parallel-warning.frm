* Test warning message when modifying a dollar variable forces
* a module into sequential mode

#ifndef `TEST'
  #message Use -D TEST=XXX
  #terminate
#else
  #include `NAME_' # `TEST'
#endif
.end

*--#[ warning :
*  need an expression with a non-zero value
Local expr = 1;
*  and a new module, since parallel execution does not work
*  in the module defining an expression
.sort
$a = 1;
.end
#require threaded?
assert warning?("This module is forced to run in sequential mode due to $-variable: $a")
*--#] warning :

* same as `warning` with a longer variable name
*--#[ warning-long :
Local expr = 1;
.sort
$n1MdWu6rNU1d29yW3ukhzV7YuY = 1;
.end
#require threaded?
assert warning?("This module is forced to run in sequential mode due to $-variable: $n1MdWu6rNU1d29yW3ukhzV7YuY")
*--#] warning-long :

* assigning in the preprocessor should not veto parallel execution
*--#[ preprocessor :
Local expr = 1;
.sort
#$a = 1;
.end
assert succeeded?
*--#] preprocessor :

* assigning through pattern matching
*--#[ pattern :
Local expr = 1;
.sort
Symbol x;
id x?$a = x;
.end
#require threaded?
assert warning?("This module is forced to run in sequential mode due to $-variable: $a")
*--#] pattern :

* don't veto parallel execution if there is a matching moduleoption statement

*--#[ local :
Local expr = 1;
.sort
$a = 1;
moduleoption local $a;
.end
assert succeeded?
*--#] local :

*--#[ sum :
Local expr = 1;
.sort
$a = 1;
moduleoption sum $a;
.end
assert succeeded?
*--#] sum :

*--#[ minimum :
Local expr = 1;
.sort
$a = 1;
moduleoption minimum $a;
.end
assert succeeded?
*--#] minimum :

*--#[ maximum :
Local expr = 1;
.sort
$a = 1;
moduleoption maximum $a;
.end
assert succeeded?
*--#] maximum :

* *do veto* if the moduleoption statement is for the wrong variable
*--#[ wrong-moduleoption :
Local expr = 1;
.sort
#$b = 1;
$a = 1;
moduleoption local $b;
.end
#require threaded?
assert warning?("This module is forced to run in sequential mode due to $-variable: $a")
*--#] wrong-moduleoption :
