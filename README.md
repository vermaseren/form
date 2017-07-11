FORM
====

[![Build Status](https://travis-ci.org/vermaseren/form.svg?branch=master)](https://travis-ci.org/vermaseren/form)
[![Coverage Status](https://coveralls.io/repos/github/vermaseren/form/badge.svg?branch=master)](https://coveralls.io/github/vermaseren/form?branch=master)

FORM is a Symbolic Manipulation System. It reads symbolic expressions from files
and executes symbolic/algebraic transformations upon them. The answers are
returned in a textual mathematical representation. As its landmark feature, the
size of the considered expressions in FORM is only limited by the available
disk space and not by the available RAM.

FORM's original author is Jos Vermaseren of NIKHEF, the Dutch institute for
subatomic physics. Other people that have made contributions can be found in the
file "[AUTHORS](AUTHORS)".


Build instructions
------------

Before building FORM, it is advised to install the optional dependencies `gmp`
and `zlib` for better performance. To quickly build FORM, install the `autoconf`
and `automake` packages. Then, after cloning the repository, run:

    autoreconf -i
    ./configure
    make
    make install

For more advanced build options, see the file "[INSTALL](INSTALL)".


Additional Information
----------------------

Information about copying and licencing of this software can be found in the
file "[COPYING](COPYING)".

More background information a collection of FORM
programs a number of courses and an online version of the manual can be 
found on the official FORM website: http://www.nikhef.nl/~form.


Bugs and remarks
----------------
Bugs can be reported via the
[Issue Tracker](https://github.com/vermaseren/form/issues) of Github.

The issue tracker can also be used for questions, remarks and suggestions. 
In the past the FORM [forum](http://www.nikhef.nl/~form/forum/) was used 
for this but we will discontinue the forum in the future.
