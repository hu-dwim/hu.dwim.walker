# hu.dwim.walker

## What

It's a [Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp) code walker. It
can turn a [SEXP](https://en.wikipedia.org/wiki/S-expression) encoding of CL
code into a structure that contains much more information than the SEXP's.

## Why

The alternative projects at the time were not good enough for our needs, which
included a [delimited
continuation](https://en.wikipedia.org/wiki/Delimited_continuation) lib called
[hu.dwim.delico](https://github.com/hu-dwim/hu.dwim.delico), a rewrite of
[iterate](https://iterate.common-lisp.dev/) in the form of
[hu.dwim.reiterate](https://github.com/hu-dwim/hu.dwim.reiterate), a SEXP -> SQL
query compiler and lisp-side execution engine in hu.dwim.perec, and possibly
others.

## Who

Originally written by [Marco Baringer](https://github.com/segv) in his
[arnesi](https://bese.common-lisp.dev/arnesi.html) library.

Afterwards it got stripped down and renamed in the form of darcs commits mostly
by [attila@lendvai.name](mailto:attila@lendvai.name). The full history is
available in this git repository.

## Where

The primary communication channel is the facilities on
[the project's GitHub page](https://github.com/hu-dwim/hu.dwim.walker).

It also has a half-baked page in our [metadata driven
GUI](http://dwim.hu/project/hu.dwim.walker), but don't expect much from it.

## Status

AFAIR, it's pretty stable and covers mostly all of CL. Any issues should be
recorded and commented in the form of failing tests in the test suite.

## Alternatives

* [clast](http://clast.sourceforge.net/)

* [agnostic-lizard](https://gitlab.common-lisp.net/mraskin/agnostic-lizard) (its
  readme has some comments on the comparison of the alternatives)

* [macroexpand-dammit](https://github.com/guicho271828/macroexpand-dammit)

* [trivial-macroexpand-all](https://github.com/cbaggers/trivial-macroexpand-all)

* [screamer](https://github.com/nikodemus/screamer) and
  [iterate](https://iterate.common-lisp.dev/) also have internal code walker
  implementations.
