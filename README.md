# Fu

_Fu_ is a down to earth rewrite of [AnyDSL](https://github.com/AnyDSL), a compiler
framework for high-performance applications.

## Goals

The goals are two-fold: First, this project should simplify every part of the compiler,
by designing everything around a core IR that is easier to manipulate and reason with.
Each statement of the high-level language should be easy to translate into that IR,
by applying simple de-sugaring rules. This means that there is no longer a decoupling
of the IR with the front-end of the compiler. In other words, there is no longer an IR
like [Thorin](https://github.com/AnyDSL/thorin) and a front-end like
[Artic](https://github.com/AnyDSL/artic) or [Impala](https://github.com/AnyDSL/impala).
There is only one language/IR: _Fu_. This has the benefit of allowing each transformation
to be readable by the programmer, up to the point where the program is translated to
assembly. Moreover, this allows serialization of the program in a trivial manner: At any
point during compilation, the program can be stored to disk and parsed again.

The second important goal is to keep some level of compatibility with the existing front-ends.
This means that the syntax should try to be as close as possible to the existing one, except
when there are good reasons not to do so.

## Documentation

First, build the documentation using Jekyll (you'll need to have a working Ruby installation):

```
cd doc
make init # Downloads and installs jekyll, bundle, and the required dependencies
make
```

Then, go to the [index](doc/_site/index.html).
