# FORM Test Suite

This directory contains a collection of test cases that can be used for
verifying the behaviour of FORM. It also has a script to run the test cases and
check the results.

## Prerequisites

The test runner script is written in [Ruby](https://www.ruby-lang.org/)
and requires Ruby 2.0 or later. The script uses the so-called `test/unit`
library. In some Linux distributions the library is installed together with
Ruby, while some distributions may have the library as an optional package,
or one may need to manually install
[test-unit](http://test-unit.github.io/test-unit/en/) via the `gem` command.

## Usage

### From the build system

To use the test suite from the automatic build system
(see also the [INSTALL](../INSTALL) file),
run the following command:

```bash
# in the root build directory
make check
```

which tests the executables (release versions) compiled by the build system.

### Testing in standalone mode

Alternatively, one can run the test runner script directly:

```bash
# in the "check" directory
./check.rb
```

By default, this tests `form` found in `$PATH`.
To test another executable, specify its path as a command-line argument:

```bash
./check.rb /path/to/form
```

One can also specify a TFORM (or ParFORM) executable in this way.
TFORM and ParFORM will be run with 4 CPUs (can be changed by the `--cpu N`
option).

By default, all test cases in all FORM files (`*.frm`) found in the `check`
directory (not in subdirectories) are used. To select test cases or FORM files
to be run, specify their names as command-line arguments. For example:

```bash
./check.rb Issue8
./check.rb 'divmod_*'
./check.rb examples.frm
```

For more advanced options, refer to the help message using the `--help` option.

## Writing tests

### Where to add test cases?

Currently, the standard test set (run by default) consists of 4 files:

- `examples.frm`: Examples provided in the manual.
- `features.frm`: Test cases for newly added features.
- `fixes.frm`: Test cases for bug fixes.
- `user.frm`: Test cases contributed by users.

Each test case in these files should finish in a short time: the timeout is set
to 10 seconds. Bigger tests that take more time are put in subdirectories
(e.g., `forcer`) and should be specified by command-line options when the test
suite is invoked:

```bash
./check.rb -C forcer  # The Forcer library must be available in FORMPATH.
```

### Structure of a test case

A test case is given as a fold in a FORM file.
The following is a simple example:

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
The part before `.end` is a normal FORM program.
After `.end`, one can write a Ruby program to check the results.

The `assert` method checks whether its argument evaluates to `true`.
In this example:
- The first assertion verifies `succeeded?`, which returns `true` if the FORM finishes successfully.
- The second assertion checks the printed result of the
expression `F` by a regular expression matching (`=~`).
  - On the left-hand side, `result("F")` returns the (lastly) printed output
    for the expression `F` as a string.
  - On the right-hand side, `expr("...")` creates a regular expression
    by removing white spaces in its argument.

Since `expr()` removes all white spaces,
one can include new lines in the argument.
For example:

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

Two or more FORM programs, separated by `.end`, can be put in a test case.
The part after the last `.end` is considered as a Ruby program.
For example:
```
*--#[ Test3 :
S x;
G F = (1+x)^2;
P;
.store
Save out.sav;
.end
Load out.sav;
L G = F;
P;
.end
assert succeeded?
assert result("F") =~ expr("1 + 2*x + x^2")
assert result("G") =~ expr("1 + 2*x + x^2")
*--#] Test3 : 
```

Some test cases need to run only under specific conditions.
In such cases, one can use special instructions starting with `#`.
For example:
```
*--#[ Test4 :
S x;
L F =
#pipe echo "(1+x)^2"
;
P;
.end
#require unix?
assert succeeded?
assert result("F") =~ expr("1 + 2*x + x^2")
*--#] Test4 : 
```
In this example, `#require unix?` ensures that the test runs
only on Unix, where `#pipe` is expected to work.

### Available methods

#### Execution configuration

- `timeout → integer or float`  
  Timeout duration in seconds.
- `ncpu → integer`  
  Number of assigned CPUs.
- `total_memory → integer`  
  Total physical memory available in bytes.
- `serial? → bool`  
  `true` if FORM is the serial version, otherwise `false`.
- `threaded? → bool`  
  `true` if FORM is the multithreaded version (TFORM), otherwise `false`.
- `mpi? → bool`  
  `true` if FORM is the MPI version (ParFORM), otherwise `false`.
- `valgrind? → bool`  
  `true` if FORM is running under Valgrind, otherwise `false`.
- `wordsize → integer`  
  Word size in bytes used by FORM (`4` on 64-bit systems).
- `cygwin? → bool`  
  `true` if running on Cygwin, otherwise `false`.
- `mac? → bool`  
  `true` if running on macOS, otherwise `false`.
- `linux? → bool`  
  `true` if running on Linux, otherwise `false`.
- `unix? → bool`  
  `true` if running on Unix, otherwise `false`.
- `windows? → bool`  
  `true` if running on Windows, otherwise `false`.
- `travis? → bool`  
  `true` if running on Travis CI, otherwise `false`.
- `github? → bool`  
  `true` if running on GitHub Actions, otherwise `false`.

#### Job status

- `return_value → integer`  
  Exit status of the FORM job.
- `finished? → bool`  
  `true` if the FORM job finished within the timeout, otherwise `false`.
- `succeeded? → bool`  
  `true` if the FORM job finished without any problems, otherwise `false`.
- `warning? → bool`  
  `true` if the FORM job issued a warning, otherwise `false`.
- `warning?(expected_message : string) → bool`  
  `true` if the FORM job issued the expected warning, otherwise `false`.

The following methods are similar to `warning?`,
but they check for preprocessor errors, compile-time errors,
and run-time errors, respectively:

- `preprocess_error? → bool`  
  `preprocess_error?(expected_message : string) → bool`
- `compile_error? → bool`  
  `compile_error?(expected_message : string) → bool`
- `runtime_error? → bool`  
  `runtime_error?(expected_message : string) → bool`

#### Standard streams

- `stdout → string`  
  Standard output of the FORM job.
- `stderr → string`  
  Standard error of the FORM job.

#### Expressions

The following methods assume the default format for printing expressions:

- `result(expr_name : string) → string`  
  The last printed output of the specified expression.
- `result(expr_name : string, index : integer) → string`  
  The printed output of the specified expression at the given index (zero-based).
- `exact_result(expr_name : string) → string`  
  `exact_result(expr_name : string, index : integer) → string`  
  Similar to `result`, but returns the exact output, preserving line breaks and whitespaces.

The following methods assume the default format for statistics:

- `nterms(expr_name : string) → integer`  
  `nterms(expr_name : string, index : integer) → integer`  
  The number of terms as reported in the statistics for the specified expression.
- `bytesize(expr_name : string) → integer`  
  `bytesize(expr_name : string, index : integer) → integer`  
  The size in bytes as reported in the statistics for the specified expression.

#### Helper methods

- `exact_pattern(str : string) → regexp`  
  Regular expression constructed from the given text with escaping any special characters.
- `pattern(str : string) → regexp`  
  Similar to `exact_pattern`, but ignores whitespaces.
- `expr(str : string) → regexp`  
  Similar to `pattern`, but matches only with the whole expression.
- `file(filename : string) → string`  
  `read(filename : string) → string`  
  Text in the specified file.
- `write(filename : string, text : string) → nil`  
  Writes a text into the specified file.

### Available instructions

- `#require <condition>`  
  Ensures that the test is executed only if the specified `<condition>` is met.
- `#pend_if <condition>`  
  Marks the test as pending if the specified `<condition>` is met.
- `#prepare <statement>`  
  Executes the given `<statement>` before running the test.
  For example:
  ```
  #prepare write "foo.prc", "#procedure foo\n#message foo\n#endprocedure"
  ```
- `#ulimit <limits>`  
  Sets the resource limits. This is done via the `ulimit` command.
  For example:
  ```
  #require linux?
  #ulimit -v 8_000_000
  ```
  This sets the maximum amount of virtual memory available to
  8,000,000 KiB (~ 8GB).
- `#time_dilation <dilation>`  
  Multiplies the timeout by the specified `<dilation>` factor.
  For example:
  ```
  #time_dilation 2.0
  ```
