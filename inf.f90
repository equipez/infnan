module inf_mod
! infnan_mod together with inf_mod provides functions for checking Inf/NaN. They aim to work even when
! compilers are invoked with aggressive optimization flags, e.g., `gfortran -Ofast`.
! See infnan.f90 for more comments.
use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_negative_inf, ieee_positive_inf
use, non_intrinsic :: consts_mod, only : SP, DP
use, non_intrinsic :: inf_values_mod, only : posinf, neginf
implicit none
private
public :: is_finite, is_inf, is_posinf, is_neginf

!interface posinf
!    module procedure posinf_sp, posinf_dp
!end interface posinf

!interface neginf
!    module procedure neginf_sp, neginf_dp
!end interface neginf

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


!pure function posinf_sp(x) result(y)
!use, non_intrinsic :: consts_mod, only : SP
!implicit none
!real(SP), intent(in) :: x
!real(QP) :: y
!y = ieee_value(x, ieee_positive_inf)
!end function posinf_sp

!pure function posinf_dp(x) result(y)
!use, non_intrinsic :: consts_mod, only : DP
!implicit none
!real(DP), intent(in) :: x
!real(QP) :: y
!y = ieee_value(x, ieee_positive_inf)
!end function posinf_dp


!pure function neginf_sp(x) result(y)
!use, non_intrinsic :: consts_mod, only : SP
!implicit none
!real(SP), intent(in) :: x
!real(QP) :: y
!y = ieee_value(x, ieee_negative_inf)
!end function neginf_sp

!pure function neginf_dp(x) result(y)
!use, non_intrinsic :: consts_mod, only : DP
!implicit none
!real(DP), intent(in) :: x
!real(QP) :: y
!y = ieee_value(x, ieee_negative_inf)
!end function neginf_dp


elemental pure function is_finite_sp(x) result(y)
use consts_mod, only : SP
implicit none
real(SP), intent(in) :: x
logical :: y
!y = (x <= huge(x) .and. x >= -huge(x))
y = (x < posinf(x) .and. x > neginf(x))
!y = abs(x) < posinf(x)
end function is_finite_sp

elemental pure function is_finite_dp(x) result(y)
use consts_mod, only : DP
implicit none
real(DP), intent(in) :: x
logical :: y
!y = (x <= huge(x) .and. x >= -huge(x))
y = (x < posinf(x) .and. x > neginf(x))
!y = abs(x) < posinf(x)
end function is_finite_dp


elemental pure function is_inf_sp(x) result(y)
use consts_mod, only : SP
implicit none
real(SP), intent(in) :: x
logical :: y
!y = (abs(x) > huge(x))
y = (abs(x) >= posinf(x))
end function is_inf_sp

elemental pure function is_inf_dp(x) result(y)
use consts_mod, only : DP
implicit none
real(DP), intent(in) :: x
logical :: y
!y = (abs(x) > huge(x))
y = (abs(x) >= posinf(x))
end function is_inf_dp


elemental pure function is_posinf_sp(x) result(y)
use consts_mod, only : SP
implicit none
real(SP), intent(in) :: x
logical :: y
!y = (abs(x) > huge(x)) .and. (x > 0)
y = ((abs(x) >= posinf(x)) .and. (x > 0))
end function is_posinf_sp

elemental pure function is_posinf_dp(x) result(y)
use consts_mod, only : DP
implicit none
real(DP), intent(in) :: x
logical :: y
!y = (abs(x) > huge(x)) .and. (x > 0)
y = ((abs(x) >= posinf(x)) .and. (x > 0))
end function is_posinf_dp


elemental pure function is_neginf_sp(x) result(y)
use consts_mod, only : SP
implicit none
real(SP), intent(in) :: x
logical :: y
!y = (abs(x) > huge(x)) .and. (x < 0)
y = ((abs(x) >= posinf(x)) .and. (x < 0))
!y = (x <= neginf(x))
end function is_neginf_sp

elemental pure function is_neginf_dp(x) result(y)
use consts_mod, only : DP
implicit none
real(DP), intent(in) :: x
logical :: y
!y = (abs(x) > huge(x)) .and. (x < 0)
y = ((abs(x) >= posinf(x)) .and. (x < 0))
!y = (x <= neginf(x))
end function is_neginf_dp

end module inf_mod
