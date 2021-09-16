module infnanfun_mod

use consts_mod, only : SP, DP
implicit none
private
public :: inffun, nanfun

interface inffun
    module procedure inffun_sp, inffun_dp
end interface inffun

interface nanfun
    module procedure nanfun_sp, nanfun_dp
end interface nanfun


contains


elemental pure function inffun_sp(x) result(y)
real(SP), intent(in) :: x
real(SP) :: y
y = x**2
end function inffun_sp

elemental pure function inffun_dp(x) result(y)
real(DP), intent(in) :: x
real(DP) :: y
y = x**2
end function inffun_dp


elemental pure function nanfun_sp(x) result(y)
real(SP), intent(in) :: x
real(SP) :: y
y = x**2
y = y * (1.0 / (x**2 + 1.0))
end function nanfun_sp

elemental pure function nanfun_dp(x) result(y)
real(DP), intent(in) :: x
real(DP) :: y
y = x**2
y = y * (1.0D0 / (x**2 + 1.0D0))
end function nanfun_dp


end module infnanfun_mod
