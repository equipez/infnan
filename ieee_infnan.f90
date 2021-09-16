module ieee_infnan_mod

use, non_intrinsic :: consts_mod, only : SP, DP
use, intrinsic :: ieee_arithmetic, only : ieee_is_nan, ieee_is_finite
implicit none
private
public :: ieee_is_inf, ieee_is_posinf, ieee_is_neginf

interface ieee_is_inf
    module procedure ieee_is_inf_sp, ieee_is_inf_dp
end interface ieee_is_inf

interface ieee_is_posinf
    module procedure ieee_is_posinf_sp, ieee_is_posinf_dp
end interface ieee_is_posinf

interface ieee_is_neginf
    module procedure ieee_is_neginf_sp, ieee_is_neginf_dp
end interface ieee_is_neginf


contains


elemental pure function ieee_is_inf_sp(x) result(y)
real(SP), intent(in) :: x
logical :: y
y = (.not. ieee_is_nan(x)) .and. (.not. ieee_is_finite(x))
end function ieee_is_inf_sp

elemental pure function ieee_is_inf_dp(x) result(y)
real(DP), intent(in) :: x
logical :: y
y = (.not. ieee_is_nan(x)) .and. (.not. ieee_is_finite(x))
end function ieee_is_inf_dp


elemental pure function ieee_is_posinf_sp(x) result(y)
real(SP), intent(in) :: x
logical :: y
y = (.not. ieee_is_nan(x)) .and. (.not. ieee_is_finite(x)) .and. (x > 0)
end function ieee_is_posinf_sp

elemental pure function ieee_is_posinf_dp(x) result(y)
real(DP), intent(in) :: x
logical :: y
y = (.not. ieee_is_nan(x)) .and. (.not. ieee_is_finite(x)) .and. (x > 0)
end function ieee_is_posinf_dp


elemental pure function ieee_is_neginf_sp(x) result(y)
real(SP), intent(in) :: x
logical :: y
y = (.not. ieee_is_nan(x)) .and. (.not. ieee_is_finite(x)) .and. (x < 0)
end function ieee_is_neginf_sp

elemental pure function ieee_is_neginf_dp(x) result(y)
real(DP), intent(in) :: x
logical :: y
y = (.not. ieee_is_nan(x)) .and. (.not. ieee_is_finite(x)) .and. (x < 0)
end function ieee_is_neginf_dp


end module ieee_infnan_mod
