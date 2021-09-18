# infnan

`infnan.f90` implements a module providing some functions for checking Inf/NaN, which aim to work
even when compilers are invoked with aggressive optimization flags, such as `gfortran -Ofast`.

There are many ways to implement functions like `is_nan`. However, not all of them work with
aggressive optimization flags. For example, there are such discussions on
[StackOverflow](https://stackoverflow.com/questions/15944614/is-it-possible-to-make-isnan-work-in-gfortran-o3-ffast-math/69217593?noredirect=1#comment122344217_69217593).

The `ieee_is_nan` included in `ieee_arithmetic` of `gfortran 9.3.0` does not work with aggressive
optimization flags like `-Ofast`.
Given the fact that `-Ofast` implies `-ffinite-math-only`, we cannot blame `ieee_arithmetic` for
the failure In addition, some compilers
(`gfortran 9.3.0`, `ifort 21.0`, and `nagfor 7.0`) are buggy concerning the return
type of `ieee_is_nan` when some special compilation options are imposed, as has been discussed
on [Fortran Discourse](https://fortran-lang.discourse.group/t/is-this-expected-fortran-standard-is-not-respected-by-gfortran-fdefault-integer-8-ifort-i8-or-nagfor-i8).

My choice of implementation is totally empirical, in the sense that I have not studied in-depth what the aggressive optimization flags really do, but only made some tests and found some implementation that worked correctly. In other words, I do not know why my implementation works but other implementations may not. The story may change when compilers are changed/updated.

The good news is, I have tested the functions on 9 compilers with the most aggressive optimization flags that I can figure out, all of which are happy. In particular, the functions work well with `gfortran -Ofast` (`gfortran` version 9.3.0), but those based on `ieee_arithmetic` fail. Such a test can be done by
```
cd test; make gtest
```
