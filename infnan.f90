module infnan_mod
! 1. infnan_mod together with inf_mod provides functions for checking Inf/NaN. They aim to work even when
! compilers are invoked with aggressive optimization flags, e.g., `gfortran -Ofast`.
!
! 2. There are many ways to implement functions like `is_nan`. However, not all of them work with
! aggressive optimization flags. For example, for `gfortran 9.3.0`, the `ieee_is_nan` included in
! `ieee_arithmetic` does not work with `gfortran -Ofast`. See the following for discussions
! https://stackoverflow.com/questions/15944614
!
! 3. My choice of implementation is totally empirical, in the sense that I have not studied in-depth
! what the aggressive optimization flags really do, but only made some tests and found the
! implementations that worked correctly. In other words, I do not know why my implementation works
! but other implementations may not. The story may change when compilers are changed/updated.
!
! 4. Do NOT change the functions without thorough testing. Their implementations are delicate. For
! example, when compilers are invoked with aggressive optimization flags,
! (X <= HUGE(X) .AND. X >= -HUGE(X)) may differ from (ABS(X) <= HUGE(X)) ,
! (X > HUGE(X) .OR. X < -HUGE(X)) may differ from (ABS(X) > HUGE(X)) , and
! (ABS(X) > HUGE(X) .AND. X > 0) may differ from (X > HUGE(X)) .
!
! 5. is_nan must be implemented in a file separated from is_inf and is_finite. Otherwise, is_nan may
! not work with some compilers invoked with aggressive optimization flags e.g., ifx -fast with
! ifx 2022.1.0 or flang -Ofast with flang 15.0.3.
!
! 6. Even though the functions involve invocation of ABS and HUGE, their performance (in terms of
! CPU time) turns out comparable to or even better than the functions in `ieee_arithmetic`.

use inf_mod, only : is_finite, is_inf, is_posinf, is_neginf
implicit none
private
public :: is_finite, is_inf, is_posinf, is_neginf, is_nan

interface is_nan
    module procedure is_nan_sp, is_nan_dp
end interface is_nan


contains


elemental pure function is_nan_sp(x) result(y)
use consts_mod, only : SP
use inf_mod, only : is_finite, is_inf
implicit none
real(SP), intent(in) :: x
logical :: y
!y = (.not. (x <= huge(x) .and. x >= -huge(x))) .and. (.not. abs(x) > huge(x))  ! Does not always work
y = (.not. is_finite(x)) .and. (.not. is_inf(x))
end function is_nan_sp

elemental pure function is_nan_dp(x) result(y)
use consts_mod, only : DP
use inf_mod, only : is_finite, is_inf
implicit none
real(DP), intent(in) :: x
logical :: y
!y = (.not. (x <= huge(x) .and. x >= -huge(x))) .and. (.not. abs(x) > huge(x))  ! Does not always work
y = (.not. is_finite(x)) .and. (.not. is_inf(x))
end function is_nan_dp

end module infnan_mod
