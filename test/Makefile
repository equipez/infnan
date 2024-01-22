###############################################################
# This Makefile tests IS_NAN, IS_FINITE, IS_INF, etc using the
# following compilers with aggressive optimization flags.
#
# g95
# af95 (Absoft 64-bit Pro Fortran)
# AMD AOCC flang
# Huawei Bisheng flang
# Classic flang
# gfortran
# ifort
# ifx
# nagfor
# nvfortran (aka pgfortran)
# sunf95

.PHONY: test testd clean

SRC = ../consts.f90 ../huge.f90 ../inf.f90 ../infnan.f90 ieee_infnan.f90 testinfnan.f90 test.f90

test:
	make -s 9test
	make -s atest
	make -s dtest
	make -s ftest
	make -s gtest
	make -s itest
	make -s ntest
	make -s rtest
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

AFLANG := $(shell find -L /opt/AMD -type f -executable -name flang -print 2>/dev/null | sort | tail -n 1)
dtest: FCO = $(AFLANG) -O4 -Ofast -ffast-math  # As of AMD clang version 13.0., -Ofast implies -O3, but it is not clear whether it implies -ffast-math

ftest: FCO = flang -O3 -Ofast -ffast-math  # No documentation found about the relation between -Ofast, -O3, and -ffast-math

# Zaikun 20231003: gtest fails if gfortran-13 is invoked with -Ofast -flto, which runs the standard link-time optimizer.
gtest: FCO = gfortran -Ofast -flto # -Ofast implies -O3 and -ffast-math
#gtest: FCO = gfortran -Ofast # -Ofast implies -O3 and -ffast-math

itest: FCO = ifort -fast  # -fast implies -Ofast, which in turn implies -O3

ntest: FCO = nagfor -O4 -ieee=full  # -ieee is needed for using NaN, or we will encounter "Arithmetic exception: Floating invalid operation"

VFORT := $(shell find -L /opt/nvidia -type f -executable -name nvfortran -print 2>/dev/null | sort | tail -n 1)
vtest: FCO = $(VFORT) -O4 -fast  # -fast implies -O2, but the highest optimization level is -O4

RFORT := $(shell find -L /opt/arm -type f -executable -name armflang -print 2>/dev/null | sort | tail -n 1)
rtest: FCO = $(RFORT) -Ofast # -Ofast implies -O3 and -ffast-math

stest: FCO = sunf95 -O5 -fast -ftrap=%none

# According to https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2024-0/fast.html
# -fast sets the following options:
# -ipo, -O3, -no-prec-div,-static, -fp-model fast=2, and -xHost
# However, as of ifx (IFX) 2024.0.2, -fast will lead to the following errors on GitHub Actions:
# ---------------------------------------------------------------------------
# 'native' is not a recognized processor for this target (ignoring processor)
# LLVM ERROR: 64-bit code requested on a subtarget that doesn't support it!
# error #5629: **Internal compiler error: abort signal raised** Please report this error along with the circumstances in which it occurred in a Software Problem Report.
# ---------------------------------------------------------------------------
# Strangely, this will not happen if we specify the flags manually, and this is not observed on local machines.
#xtest: FCO = ifx -fast
xtest: FCO = ifx -ipo -O3 -no-prec-div -static -fp-model fast=2 -xHost

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
	make -s rtestd
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
rtestd: FCD = $(RFORT) -c -Wall -Wextra -Mstandard
stestd: FCD = sunf95  -c -g -w3 -u -ansi #-U
xtestd: FCD = ifx -c -g -warn all -fpe0 -debug extended -fimplicit-none

%testd: $(SRC)
	$(FCD) $^
	@make clean
################################################################
