FORM
====

[![Test](https://github.com/vermaseren/form/actions/workflows/test.yml/badge.svg?branch=master)](https://github.com/vermaseren/form/actions?query=branch%3Amaster)
[![Coverage Status](https://coveralls.io/repos/github/vermaseren/form/badge.svg?branch=master)](https://coveralls.io/github/vermaseren/form?branch=master)

FORM is a Symbolic Manipulation System. It reads symbolic expressions from files and executes symbolic/algebraic transformations upon them. The answers are returned in a textual mathematical representation. As its landmark feature, the size of the considered expressions in FORM is only limited by the available disk space and not by the available RAM. FORM has been essential for many state-of-the-art computations in High Energy Physics.

FORM's original author is Jos Vermaseren of NIKHEF, the Dutch institute for subatomic physics. Other people that have made contributions can be found in the file "[AUTHORS](AUTHORS)".

Quick examples
--------------

The following FORM program repeatedly matches the power of a variable `x` in the expression `E`, as long as the power is more than 1 and creates two new terms with lower power:

```form
Symbol x,n;
Local E = x^10;

repeat id x^n?{>1} = x^(n-1) + x^(n-2);

Print;
.end
```

and yields `E = 34 + 55*x`.

The following FORM program matches the function `f` that has any arguments before encountering an `x` and any arguments after, and switches them around:

```form
CFunction f;
Symbol x;
Local E = f(1,2,x,3,4);

id f(?a,x,?b) = f(?b,?a);

Print;
.end
```

and yields `E = f(3,4,1,2)`.

FORM can match many more complicated patterns and has many more features, as documented in the [additional information](#additional-information).

Build instructions
------------

Before building FORM, it is advised to install the optional dependencies `gmp` and `zlib` for better performance. To quickly build FORM, install the `autoconf` and `automake` packages. Then, after cloning the repository, run:

```sh
autoreconf -i
./configure
make
make install
```

For more advanced build options, see the file "[INSTALL](INSTALL)".


Additional information
----------------------

The latest reference manual can be found [here](https://github.com/vermaseren/form/releases/download/v4.3.1/form-4.3.1-manual.pdf) and the Form Cookbook can be found [here](https://github.com/vermaseren/form/wiki/FORM-Cookbook).

More background information, a collection of FORM programs, and a number of courses can be found on the official [FORM website](http://www.nikhef.nl/~form) and on the [Wiki](https://github.com/vermaseren/form/wiki).

Information about copying and licensing of this software can be found in the file "[COPYING](COPYING)".


Bugs and remarks
----------------
For reporting bugs, asking questions, giving remarks and suggestions, we welcome you to use the [Issue Tracker](https://github.com/vermaseren/form/issues).
