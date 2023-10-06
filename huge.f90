module huge_mod

implicit none
private
public :: huge_value

interface huge_value
    module procedure huge_value_sp, huge_value_dp
end interface huge_value


contains


pure function huge_value_sp(x) result(y)
use, non_intrinsic :: consts_mod, only : SP
implicit none
real(SP), intent(in) :: x
real(SP) :: y
y = huge(x)
end function huge_value_sp

pure function huge_value_dp(x) result(y)
use, non_intrinsic :: consts_mod, only : DP
implicit none
real(DP), intent(in) :: x
real(DP) :: y
y = huge(x)
end function huge_value_dp


end module huge_mod
