# infnan

`infnan.f90` implements a module that provides some functions for checking Inf/NaN. They aim to work
even when compilers are invoked with aggressive optimization flags, including particularly `gfortran -Ofast`.

Many ways exist to implement functions like `is_nan`. However, not all of them work with
aggressive optimization flags. For example, there are such discussions on
[StackOverflow](https://stackoverflow.com/questions/15944614) and [Fotran Discourse](https://fortran-lang.discourse.group/t/checking-inf-nan-when-compilers-are-invoked-with-aggressive-optimization-flags/1851).

The `ieee_is_nan` included in `ieee_arithmetic` of `gfortran 9.3.0` does not work with aggressive
optimization flags like `-Ofast`.
Given the fact that `-Ofast` implies `-ffinite-math-only`, we cannot blame `ieee_arithmetic` for
the failure. In addition, some compilers
(`gfortran 9.3.0`, `ifort 21.0`, and `nagfor 7.0`) may not behave in the most desirable way concerning
the return kind of `ieee_is_nan` when some special compilation flags are imposed, as has been discussed
on [Fortran Discourse](https://fortran-lang.discourse.group/t/is-this-expected-fortran-standard-is-not-respected-by-gfortran-fdefault-integer-8-ifort-i8-or-nagfor-i8).

My choice of implementation is totally empirical, in the sense that I have not studied in-depth what
the aggressive optimization flags really do, but only made some tests and found some implementation
that worked correctly. In other words, I do not know why my implementation works but other
implementations may not. The story may change when compilers are changed/updated.

The good news is, I have tested the functions using ten compilers with the most 
aggressive optimization flags that I can figure out, and they all succeeded. In particular, the functions work well with `gfortran -Ofast` for `gfortran` 9.3.0 and 10.3.0, 
but those based on `ieee_arithmetic` fail. Such a test can be done by

```
cd test && make clean && make gtest
```

However, we did observe failures. 
See the [issues](https://github.com/zaikun/infnan/issues) for details. I will be very happy to hear your suggestions on how to fix them.
