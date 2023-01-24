module inf_mod

use consts_mod, only : SP, DP
implicit none
private
public :: is_finite, is_inf, is_posinf, is_neginf

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

end module inf_mod
