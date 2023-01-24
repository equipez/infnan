module nan_mod
use consts_mod, only : SP, DP
use inf_mod, only: is_finite, is_inf, is_posinf, is_neginf
implicit none
private
public :: is_nan

interface is_nan
    module procedure is_nan_sp, is_nan_dp
end interface is_nan


contains


elemental pure function is_nan_sp(x) result(y)
implicit none
real(SP), intent(in) :: x
logical :: y
!y = (.not. (x <= huge(x) .and. x >= -huge(x))) .and. (.not. abs(x) > huge(x))  ! Does not always work
y = (.not. is_finite(x)) .and. (.not. is_inf(x))
end function is_nan_sp

elemental pure function is_nan_dp(x) result(y)
implicit none
real(DP), intent(in) :: x
logical :: y
!y = (.not. (x <= huge(x) .and. x >= -huge(x))) .and. (.not. abs(x) > huge(x))  ! Does not always work
y = (.not. is_finite(x)) .and. (.not. is_inf(x))
end function is_nan_dp

end module nan_mod
