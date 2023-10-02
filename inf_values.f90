module inf_values_mod

use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_negative_inf, ieee_positive_inf
use, non_intrinsic :: consts_mod, only : SP, DP
implicit none
private
public :: posinf, neginf
interface posinf
    module procedure posinf_sp, posinf_dp
end interface posinf

interface neginf
    module procedure neginf_sp, neginf_dp
end interface neginf

contains

pure function posinf_sp(x) result(y)
use, non_intrinsic :: consts_mod, only : SP
implicit none
real(SP), intent(in) :: x
real(SP) :: y
y = ieee_value(x, ieee_positive_inf)
end function posinf_sp

pure function posinf_dp(x) result(y)
use, non_intrinsic :: consts_mod, only : DP
implicit none
real(DP), intent(in) :: x
real(DP) :: y
y = ieee_value(x, ieee_positive_inf)
end function posinf_dp


pure function neginf_sp(x) result(y)
use, non_intrinsic :: consts_mod, only : SP
implicit none
real(SP), intent(in) :: x
real(SP) :: y
y = ieee_value(x, ieee_negative_inf)
end function neginf_sp

pure function neginf_dp(x) result(y)
use, non_intrinsic :: consts_mod, only : DP
implicit none
real(DP), intent(in) :: x
real(DP) :: y
y = ieee_value(x, ieee_negative_inf)
end function neginf_dp


end module inf_values_mod
