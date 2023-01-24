module nan_mod
! infnan_mod together with inf_mod and nan_mod provide functions for checking Inf/NaN. They aim 
! to work even when compilers are invoked with aggressive optimization flags, e.g., `gfortran -Ofast`.
! See infnan.f90 for more comments.

implicit none
private
public :: is_nan

interface is_nan
    module procedure is_nan_sp, is_nan_dp
end interface is_nan


contains


elemental pure function is_nan_sp(x) result(y)
use consts_mod, only : SP
use inf_mod, only: is_finite, is_inf
implicit none
real(SP), intent(in) :: x
logical :: y
!y = (.not. (x <= huge(x) .and. x >= -huge(x))) .and. (.not. abs(x) > huge(x))  ! Does not always work
y = (.not. is_finite(x)) .and. (.not. is_inf(x))
end function is_nan_sp

elemental pure function is_nan_dp(x) result(y)
use consts_mod, only : DP
use inf_mod, only: is_finite, is_inf
implicit none
real(DP), intent(in) :: x
logical :: y
!y = (.not. (x <= huge(x) .and. x >= -huge(x))) .and. (.not. abs(x) > huge(x))  ! Does not always work
y = (.not. is_finite(x)) .and. (.not. is_inf(x))
end function is_nan_dp

end module nan_mod
