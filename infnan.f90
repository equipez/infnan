module infnan_mod
! 1. This module provides functions for checking Inf/NaN. They aim to work even when compilers are
! invoked with aggressive optimization flags, such as `gfortran -Ofast`.
!
! 2. There are many ways to implement functions like `is_nan`. However, not all of them work with
! aggressive optimization flags. For example, for `gfortran 9.3.0`, the `ieee_is_nan` included in
! `ieee_arithmetic` does not work with `gfortran -Ofast`. See the following for discussions
! https://stackoverflow.com/questions/15944614/is-it-possible-to-make-isnan-work-in-gfortran-o3-ffast-math
!
! 3. My choice of implementation is totally empirical, in the sense that I have not studied in-depth
! what the aggressive optimization flags really do, but only made some tests and found thee
! implementations that worked correctly. In other words, I do not know why my implementation works
! but other implementations may not. The story may change when compilers are changed/updated.
!
! 4. Even though the functions involve invocation of ABS and HUGE, their performance (in terms of
! CPU time) turns out comparable to or even better than the functions in `ieee_arithmetic`.
!
! 5. N.B.: Do NOT change the implementations without thorough testing. The implementations are
! delicate. For example, when compilers are invoked with aggressive optimization flags,
! (X <= HUGE(X) .AND. X >= -HUGE(X)) differs from (ABS(X) <= HUGE(X)) ,
! (X > HUGE(X) .OR. X < -HUGE(X)) differs from (ABS(X) > HUGE(X)) .

use consts_mod, only : SP, DP
implicit none
private
public :: is_nan, is_finite, is_inf, is_posinf, is_neginf


interface is_nan
    module procedure is_nan_sp, is_nan_dp
end interface is_nan

interface is_finite
    module procedure is_finite_sp, is_finite_dp
end interface is_finite

interface is_posinf
    module procedure is_posinf_sp, is_posinf_dp
end interface is_posinf

interface is_neginf
    module procedure is_neginf_sp, is_neginf_dp
end interface is_neginf

interface is_inf
    module procedure is_inf_sp, is_inf_dp
end interface is_inf


contains


elemental pure function is_nan_sp(x) result(y)
implicit none
real(SP), intent(in) :: x
logical :: y
!y = (.not. (x <= huge(x) .and. x >= -huge(x))) .and. (.not. abs(x) > huge(x))
y = (.not. x == x)
end function is_nan_sp

elemental pure function is_nan_dp(x) result(y)
implicit none
real(DP), intent(in) :: x
logical :: y
y = (.not. (x <= huge(x) .and. x >= -huge(x))) .and. (.not. abs(x) > huge(x))
end function is_nan_dp


elemental pure function is_finite_sp(x) result(y)
implicit none
real(SP), intent(in) :: x
logical :: y
y = (x <= huge(x) .and. x >= -huge(x))
end function is_finite_sp

elemental pure function is_finite_dp(x) result(y)
implicit none
real(DP), intent(in) :: x
logical :: y
y = (x <= huge(x) .and. x >= -huge(x))
end function is_finite_dp


elemental pure function is_inf_sp(x) result(y)
implicit none
real(SP), intent(in) :: x
logical :: y
y = (abs(x) > huge(x))
end function is_inf_sp

elemental pure function is_inf_dp(x) result(y)
implicit none
real(DP), intent(in) :: x
logical :: y
y = (abs(x) > huge(x))
end function is_inf_dp


elemental pure function is_posinf_sp(x) result(y)
implicit none
real(SP), intent(in) :: x
logical :: y
y = (abs(x) > huge(x)) .and. (x > 0)
end function is_posinf_sp

elemental pure function is_posinf_dp(x) result(y)
implicit none
real(DP), intent(in) :: x
logical :: y
y = (abs(x) > huge(x)) .and. (x > 0)
end function is_posinf_dp


elemental pure function is_neginf_sp(x) result(y)
implicit none
real(SP), intent(in) :: x
logical :: y
y = (abs(x) > huge(x)) .and. (x < 0)
end function is_neginf_sp

elemental pure function is_neginf_dp(x) result(y)
implicit none
real(DP), intent(in) :: x
logical :: y
y = (abs(x) > huge(x)) .and. (x < 0)
end function is_neginf_dp


end module infnan_mod
