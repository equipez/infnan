###############################################################
# This Makefile tests IS_NAN, IS_FINITE, IS_INF, etc using the
# following compilers with aggressive optimization flags.
#
# g95: G95 (GCC 4.0.3 (g95 0.94!) Jan 17 2013)
# af95: Absoft 64-bit Pro Fortran 21.0.0
# AMD AOCC flang: Flang in AMD clang version 12.0.0
# flang: Flang in clang version 7.1.0
# gfortran: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
# ifort: ifort (IFORT) 2021.2.0 20210228
# nagfor: NAG Fortran Compiler Release 7.0(Yurakucho) Build 7036
# pgfortran: pgfortran (aka nvfortran) 21.3-0 LLVM 64-bit x86-64
# sunf95: Oracle Developer Studio 12.6
# ifx: ifx (IFORT) 2021.2.0 Beta 20210317
#
# Tested on Ubuntu 20.04 with Linux 5.4.0-77-generic x86_64

.PHONY: test testd clean

SRC = ../consts.f90 ../infnan.f90 ieee_infnan.f90 testinfnan.f90

test:
	make -s 9test
	make -s atest
	make -s dtest
	make -s ftest
	make -s gtest
	make -s itest
	make -s ntest
	make -s ptest
	make -s stest
	make -s xtest

# Fortran compilers with aggressive optimization flags
9test: FCO = g95 -O3 -ffast-math
atest: FCO = af95 -no-pie -O4
dtest: FCO = /opt/AMD/aocc-compiler-3.1.0/bin/flang -O3 -ffast-math -Ofast
ftest: FCO = flang -O3 -ffast-math -Ofast
gtest: FCO = gfortran -O3 -ffast-math -Ofast
itest: FCO = ifort -O3 -fast
ntest: FCO = nagfor -O4 -ieee=full
ptest: FCO = pgfortran -O4 -fast
#stest: FCO = sunf95 -O5 -fast -ftrap=%none  # IS_NAN does not work in this case.
stest: FCO = sunf95 -O5 -ftrap=%none
xtest: FCO = ifx -O3 -fast

%test: $(SRC)
	$(FCO) -o $@ $^
	./$@
	@make clean

clean:
	rm -f *.o *.mod *.dbg *test


###### The code below is for debugging. #####################
testd:
	make -s 9testd
	make -s atestd
	make -s dtestd
	make -s ftestd
	make -s gtestd
	make -s itestd
	make -s ntestd
	make -s ptestd
	make -s stestd
	make -s xtestd

9testd: FCD = g95 -c -g -Wall -Wextra -fbounds-check -fimplicit-none
atestd: FCD = af95  -c -g -m1 -en
dtestd: FCD = /opt/AMD/aocc-compiler-3.1.0/bin/flang -c -Wall -Wextra -Mstandard
ftestd: FCD = flang -c -Wall -Wextra -Mstandard
gtestd: FCD = gfortran -c -g -Wall -Wextra -Wampersand -Wconversion -Wconversion-extra -Wuninitialized -Wmaybe-uninitialized  -Wsurprising -Waliasing  -Wimplicit-interface -Wimplicit-procedure -Wintrinsics-std -Wunderflow -Wuse-without-only -Wrealloc-lhs -Wrealloc-lhs-all -Wdo-subscript  -Wunused-parameter  -fwhole-file  -fimplicit-none -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow
itestd: FCD = ifort -c -g -warn all -check all  -fpe0 -debug extended -fimplicit-none #-names as_is
ntestd: FCD = nagfor -c -info -g -gline -u -C -C=all -C=alias -C=dangling -C=intovf -C=undefined -kind=unique -Warn=allocation -Warn=constant_coindexing -Warn=reallocation -Warn=subnormal -colour=error:red,warn:magenta,info:yellow
ptestd: FCD = pgfortran  -c -g -Minform=warn
stestd: FCD = sunf95  -c -g -w3 -u -ansi #-U
xtestd: FCD = ifx -c -g -warn all -fpe0 -debug extended -fimplicit-none

%testd: $(SRC)
	$(FCD) $^
	@make clean
################################################################
