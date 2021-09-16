module test_infnan_mod

use, non_intrinsic :: consts_mod, only : SP, DP
implicit none
private
public :: test_infnan


contains


subroutine test_infnan()
use, intrinsic :: ieee_arithmetic, only : ieee_is_nan, ieee_is_finite
use, non_intrinsic :: ieee_infnan_mod, only : ieee_is_inf, ieee_is_posinf, ieee_is_neginf
use, non_intrinsic :: infnanfun_mod, only : inffun, nanfun
use, non_intrinsic :: infnan_mod, only : is_nan, is_finite, is_inf, is_posinf, is_neginf
implicit none

real(SP) :: INF_SP, NAN_SP
real(DP) :: INF_DP, NAN_DP
real(SP) :: x(13)
real(DP) :: y(size(x))
logical :: check_nan(size(x))
logical :: check_finite(size(x))
logical :: check_inf(size(x))
logical :: check_posinf(size(x))
logical :: check_neginf(size(x))

INF_SP = inffun(huge(0.0))
INF_DP = inffun(huge(0.0D0))
NAN_SP = nanfun(huge(0.0))
NAN_DP = nanfun(huge(0.0D0))

x = [NAN_SP, -NAN_SP, INF_SP, -INF_SP, &
    & huge(0.0_SP), -huge(0.0_SP), epsilon(0.0), -epsilon(0.0), tiny(0.0), -tiny(0.0), &
    & 0.0_SP, 1.0_SP, -1.0_SP]
y = [NAN_DP, -NAN_DP, INF_DP, -INF_DP, &
    huge(0.0_DP), -huge(0.0_DP), epsilon(0.0D0), -epsilon(0.0D0), tiny(0.0D0), -tiny(0.0D0), &
    & 0.0_DP, 1.0_DP, -1.0_DP]
check_nan = [.true., .true., .false., .false., &
    &.false., .false., .false., .false., .false., .false., &
    & .false., .false., .false.]
check_finite = [.false., .false., .false., .false., &
    & .true., .true., .true., .true., .true., .true., &
    & .true., .true., .true.]
check_inf = [.false., .false., .true., .true., &
    & .false., .false., .false., .false., .false., .false., &
    & .false., .false., .false.]
check_posinf = [.false., .false., .true., .false., &
    & .false., .false., .false., .false., .false., .false., &
    & .false., .false., .false.]
check_neginf = [.false., .false., .false., .true., &
    & .false., .false., .false., .false., .false., .false., &
    & .false., .false., .false.]

write (*, *) 'Testing data:'
write (*, '(1A,  /(1P, 15D11.3))') 'Single precision: ', x
write (*, '(1A,  /(1P, 15D11.3))') 'Double precision: ', y

write (*, *) 'IS_NAN(X): ', all(is_nan(x) .eqv. check_nan)
write (*, *) 'IEEE_IS_NAN(X): ', all(ieee_is_nan(x) .eqv. check_nan)
write (*, *) 'IS_NAN(Y): ', all(is_nan(y) .eqv. check_nan)
write (*, *) 'IEEE_IS_NAN(Y): ', all(ieee_is_nan(y) .eqv. check_nan)

write (*, *) 'IS_FINITE(X): ', all(is_finite(x) .eqv. check_finite)
write (*, *) 'IEEE_IS_FINITE(X): ', all(ieee_is_finite(x) .eqv. check_finite)
write (*, *) 'IS_FINITE(Y): ', all(is_finite(y) .eqv. check_finite)
write (*, *) 'IEEE_IS_FINITE(Y): ', all(ieee_is_finite(y) .eqv. check_finite)

write (*, *) 'IS_INF(X): ', all(is_inf(x) .eqv. check_inf)
write (*, *) 'IEEE_IS_INF(X): ', all(ieee_is_inf(x) .eqv. check_inf)
write (*, *) 'IS_INF(Y): ', all(is_inf(y) .eqv. check_inf)
write (*, *) 'IEEE_IS_INF(Y): ', all(ieee_is_inf(y) .eqv. check_inf)

write (*, *) 'IS_POSINF(X): ', all(is_posinf(x) .eqv. check_posinf)
write (*, *) 'IEEE_IS_POSINF(X): ', all(ieee_is_posinf(x) .eqv. check_posinf)
write (*, *) 'IS_POSINF(X): ', all(is_posinf(y) .eqv. check_posinf)
write (*, *) 'IEEE_IS_POSINF(X): ', all(ieee_is_posinf(y) .eqv. check_posinf)

write (*, *) 'IS_NEGINF(X): ', all(is_neginf(x) .eqv. check_neginf)
write (*, *) 'IEEE_IS_NEGINF(X): ', all(ieee_is_neginf(x) .eqv. check_neginf)
write (*, *) 'IS_NEGINF(Y): ', all(is_neginf(y) .eqv. check_neginf)
write (*, *) 'IEEE_IS_NEGINF(Y): ', all(ieee_is_neginf(y) .eqv. check_neginf)

end subroutine test_infnan


end module test_infnan_mod


program test
use, non_intrinsic :: test_infnan_mod, only : test_infnan

call test_infnan()

end program test
