peval
=====

This package contains a partial evaluator for Curry
implemented by Björn Peemöller (CAU Kiel), based on the preceding work of
Elvira Albert, German Vidal (UPV), and Michael Hanus (CAU Kiel).

The directory `examples` contains various examples
demonstrating the power of the partial evaluator.

The directory `test` contains a lot of smaller examples to test
particular aspects of the partial evaluator with the `TestDriver`.
Since complex software is not perfect, the following
tests still lead to failures:

    Testing Natural semantics and None abstraction with 229 test(s):
    ...
    Testing test/papers/dsl11                : FAILED
    Testing test/papers/wflp/colormap01      : FAILED
    Testing test/papers/wflp/kmp             : FAILED
    
    Testing Natural semantics and WFO abstraction with 229 test(s):
    
    Testing Natural semantics and WQO abstraction with 229 test(s):
    ...
    Testing test/funpats/base/base13         : FAILED
    Testing test/papers/dsl11                : FAILED
    Testing test/papers/wflp/colormap01      : FAILED
    Testing test/papers/wflp/kmp             : FAILED
