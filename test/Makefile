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
# nvfortran: nvfortran (aka pgfortran) 21.3-0 LLVM 64-bit x86-64
# sunf95: Oracle Developer Studio 12.6
# ifx: ifx (IFORT) 2021.2.0 Beta 20210317
#
# Tested on Ubuntu 20.04 with Linux 5.4.0-77-generic x86_64

.PHONY: test testd clean

SRC = ../consts.f90 ../inf.f90 ../infnan.f90 ieee_infnan.f90 testinfnan.f90 test.f90

test:
	make -s 9test
	make -s atest
	make -s dtest
	make -s ftest
	make -s gtest
	make -s itest
	make -s ntest
	make -s stest
	make -s vtest
	make -s xtest

# Fortran compilers with aggressive optimization flags
9test: FCO = g95 -O3 -ffast-math

AFORT :=
ifneq ("$(wildcard /etc/debian_version)","")
	AFORT = af95 -no-pie
else
	AFORT = af95
endif
atest: FCO = $(AFORT) -O4

AFLANG := $(shell find -L /opt/AMD -type f -executable -name flang -print 2> /dev/null | sort | tail -n 1)
dtest: FCO = $(AFLANG) -O4 -Ofast -ffast-math  # As of AMD clang version 13.0., -Ofast implies -O3, but it is not clear whether it implies -ffast-math

ftest: FCO = flang -O3 -Ofast -ffast-math  # No documentation found about the relation between -Ofast, -O3, and -ffast-math

gtest: FCO = gfortran -Ofast  # -Ofast implies -O3 and -ffast-math

itest: FCO = ifort -fast  # -fast implies -Ofast, which in turn implies -O3

ntest: FCO = nagfor -O4 -ieee=full  # -ieee is needed for using NaN, or we will encounter "Arithmetic exception: Floating invalid operation"

VFORT := $(shell find -L /opt/nvidia -type f -executable -name nvfortran -print 2> /dev/null | sort | tail -n 1)
vtest: FCO = $(VFORT) -O4 -fast  # -fast implies -O2, but the highest optimization level is -O4

stest: FCO = sunf95 -O5 -fast -ftrap=%none

xtest: FCO = ifx -fast

%test: $(SRC)
	$(FCO) -o $@ $^
	./$@
	@make clean

clean:
	rm -f *.o *.mod *.dbg *test


###### The code below is for debugging and linting the code. #####################
testd:
	make -s 9testd
	make -s atestd
	make -s dtestd
	make -s ftestd
	make -s gtestd
	make -s itestd
	make -s ntestd
	make -s stestd
	make -s vtestd
	make -s xtestd

9testd: FCD = g95 -c -g -Wall -Wextra -fbounds-check -fimplicit-none
atestd: FCD = $(AFORT)  -c -g -m1 -en
dtestd: FCD = $(AFLANG) -c -Wall -Wextra -Mstandard
ftestd: FCD = flang -c -Wall -Wextra -Mstandard
gtestd: FCD = gfortran -c -g -Wall -Wextra -Wampersand -Wconversion -Wconversion-extra -Wuninitialized -Wmaybe-uninitialized  -Wsurprising -Waliasing  -Wimplicit-interface -Wimplicit-procedure -Wintrinsics-std -Wunderflow -Wuse-without-only -Wrealloc-lhs -Wrealloc-lhs-all -Wdo-subscript  -Wunused-parameter  -fwhole-file  -fimplicit-none -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow
itestd: FCD = ifort -c -g -warn all -check all  -fpe0 -debug extended -fimplicit-none #-names as_is
ntestd: FCD = nagfor -c -info -g -gline -u -C -C=all -C=alias -C=dangling -C=intovf -C=undefined -kind=unique -Warn=allocation -Warn=constant_coindexing -Warn=subnormal -colour=error:red,warn:magenta,info:yellow
vtestd: FCD = $(VFORT)  -c -g -Minform=warn
stestd: FCD = sunf95  -c -g -w3 -u -ansi #-U
xtestd: FCD = ifx -c -g -warn all -fpe0 -debug extended -fimplicit-none

%testd: $(SRC)
	$(FCD) $^
	@make clean
################################################################
