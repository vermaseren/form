FORM Test Suite
===============

This directory contains a collection of test cases that can be used for
verifying the behaviour of FORM. It also has a script to run the test cases and
check the results.

Prerequisites
-------------

The test runner script is written in [Ruby](https://www.ruby-lang.org/)
and requires Ruby 1.9 or later. The script uses the so-called `test/unit`
library. In some Linux distributions the library is installed together with
Ruby, while some distributions may have the library as an optional package,
or one may need to manually install
[test-unit](http://test-unit.github.io/test-unit/en/) via the `gem` command.
Currently, the script runs only on Unix-like systems.

Usage
-----

### From the build system

To use the test suite from the automatic build system
(see also the [INSTALL](../INSTALL) file),
run

```
# in the root build directory
make check
```

which tests the executables (release versions) compiled by the build system.

### Testing in the standalone mode

Alternatively, one can run the test runner script directly:

```
# in the "check" directory
./check.rb
```

By default, it tests `form` found in $PATH.
To check another executable, give the path as a command line option:

```
./check.rb /path/to/form
```

One can specify a TFORM (or ParFORM) executable in this way.
TFORM and ParFORM will be run with 4 CPUs (can be changed by the `--cpu N`
option).

By default, all test cases in all FORM files (`*.frm`) found in the `check`
directory (not in subdirectories) are used. To select test cases or FORM files
to be run, give their names as command line options, for example,

```
./check.rb examples.frm
./check.rb Issue8
```

For more advanced options, see the help message shown by the `--help` option.

Writing tests
-------------

### Where to add test cases?

Currently, the standard test set (run by default) consists of 3 files:

- `examples.frm`: Examples found in the manual.
- `features.frm`: Test cases for newly added features.
- `fixes.frm`: Test cases for bug fixes.

Each test case in these files should finish in a short time: the timeout is set
to 10 seconds. Bigger tests that take more time are put in subdirectories
(e.g., forcer) and should be specified by command-line options when the test
suite is invoked.

### Structure of a test case

A test case is given as a fold in a FORM file. A simple example is:

```
*--#[ Test1 :
S x;
L F = (1+x)^2;
P;
.end
assert succeeded?
assert result("F") =~ expr("1 + 2*x + x^2")
*--#] Test1 : 
```

The fold name `Test1` gives the name of the test case, which should be unique.
The part before `.end` is a normal FORM program. After `.end`, one can write
a Ruby program to check the results. In this example, `assert` method (which is
provided by some unit test class) is used for checking whether its argument is
`true`. The first assertion checks `succeeded?`, which gives `true` if the FORM
successfully finishes. The second assertion checks the printed result of the
expression `F` by a regular expression matching (`=~`). In the left-hand side,
`result("F")` returns the (lastly) printed output for the expression `F` as
a string. In the right-hand side, `expr("...")` makes a regular expression with
removing white spaces in its argument. Since `expr()` removes all white spaces,
one can also put new lines, for example,

```
*--#[ Test2 :
S x;
L F = (1+x)^2;
P +s;
.end
assert succeeded?
assert result("F") =~ expr("
       + 1
       + 2*x
       + x^2
")
*--#] Test2 : 
```

which is convenient to copy and paste a long output from a terminal.

### Tips

- To verify that FORM finishes with a certain error, one can use
  `assert compile_error?` or `assert runtime_error?`.
- Two or more FORM programs, separated by `.end`, can be put in a test case.
  Then the part after the last `.end` is for Ruby.
- To skip a test case for some condition, one can specify it by `#pend_if`.
  (See the result of grepping `pend_if` in the existing files.)
- When a test case requires other text files, one can use `#prepare write`.
  (See the result of grepping `prepare` in the existing files.)
