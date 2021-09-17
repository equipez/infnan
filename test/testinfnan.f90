module test_infnan_mod

use, non_intrinsic :: consts_mod, only : SP, DP
implicit none
private
public :: test_infnan


contains


function test_infnan() result(success)
use, intrinsic :: ieee_arithmetic, only : ieee_is_nan, ieee_is_finite, ieee_value, &
    & ieee_quiet_nan, ieee_signaling_nan, ieee_positive_inf, ieee_negative_inf, &
    & ieee_positive_normal, ieee_negative_normal, ieee_positive_denormal, ieee_negative_denormal, &
    & ieee_positive_zero, ieee_negative_zero
use, non_intrinsic :: ieee_infnan_mod, only : ieee_is_inf, ieee_is_posinf, ieee_is_neginf
use, non_intrinsic :: infnan_mod, only : is_nan, is_finite, is_inf, is_posinf, is_neginf

implicit none

logical :: success

real(SP) :: PINF_SP, NINF_SP, QNAN_SP, SNAN_SP, PNORMAL_SP, NNORMAL_SP, PDENORMAL_SP, NDENORMAL_SP, &
    & PZERO_SP, NZERO_SP
real(DP) :: PINF_DP, NINF_DP, QNAN_DP, SNAN_DP, PNORMAL_DP, NNORMAL_DP, PDENORMAL_DP, NDENORMAL_DP, &
    & PZERO_DP, NZERO_DP
real(SP) :: x(19)
real(DP) :: y(size(x))
logical :: check_nan(size(x))
logical :: check_finite(size(x))
logical :: check_inf(size(x))
logical :: check_posinf(size(x))
logical :: check_neginf(size(x))
logical :: success_SP
logical :: success_DP

PINF_SP = ieee_value(1.0, ieee_positive_inf)
NINF_SP = ieee_value(1.0, ieee_negative_inf)
QNAN_SP = ieee_value(1.0, ieee_quiet_nan)
SNAN_SP = ieee_value(1.0, ieee_signaling_nan)
PNORMAL_SP = ieee_value(1.0, ieee_positive_normal)
NNORMAL_SP = ieee_value(1.0, ieee_negative_normal)
PDENORMAL_SP = ieee_value(1.0, ieee_positive_denormal)
NDENORMAL_SP = ieee_value(1.0, ieee_negative_denormal)
PZERO_SP = ieee_value(1.0, ieee_positive_zero)
NZERO_SP = ieee_value(1.0, ieee_negative_zero)

PINF_DP = ieee_value(1.0D0, ieee_positive_inf)
NINF_DP = ieee_value(1.0D0, ieee_negative_inf)
QNAN_DP = ieee_value(1.0D0, ieee_quiet_nan)
SNAN_DP = ieee_value(1.0D0, ieee_signaling_nan)
PNORMAL_DP = ieee_value(1.0D0, ieee_positive_normal)
NNORMAL_DP = ieee_value(1.0D0, ieee_negative_normal)
PDENORMAL_DP = ieee_value(1.0D0, ieee_positive_denormal)
NDENORMAL_DP = ieee_value(1.0D0, ieee_negative_denormal)
PZERO_DP = ieee_value(1.0D0, ieee_positive_zero)
NZERO_DP = ieee_value(1.0D0, ieee_negative_zero)

x = [PINF_SP, NINF_SP, &
    & QNAN_SP, SNAN_SP, &
    & PNORMAL_SP, NNORMAL_SP, &
    & PDENORMAL_SP, NDENORMAL_SP, &
    & PZERO_SP, NZERO_SP, &
    & huge(0.0), -huge(0.0), &
    & epsilon(0.0), -epsilon(0.0), &
    & tiny(0.0), -tiny(0.0), &
    & 0.0, 1.0, -1.0]

y = [PINF_DP, NINF_DP, &
    & QNAN_DP, SNAN_DP, &
    & PNORMAL_DP, NNORMAL_DP, &
    & PDENORMAL_DP, NDENORMAL_DP, &
    & PZERO_DP, NZERO_DP, &
    & huge(0.0_DP), -huge(0.0_DP), &
    & epsilon(0.0D0), -epsilon(0.0D0), &
    & tiny(0.0D0), -tiny(0.0D0), &
    & 0.0_DP, 1.0_DP, -1.0_DP]

check_finite = [.false., .false., &
    & .false., .false., &
    & .true., .true., &
    & .true., .true., &
    & .true., .true., &
    & .true., .true., &
    & .true., .true., &
    & .true., .true., &
    & .true., .true., .true.]

check_inf = [.true., .true., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., .false.]

check_posinf = [.true., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., .false.]

check_neginf = [.false., .true., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., .false.]

check_nan = [.false., .false., &
    & .true., .true., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., &
    & .false., .false., .false.]


write (*, '(/(1A))') 'Single-precision tests:'

if (.not. all(is_finite(x) .eqv. check_finite)) then
    write (*, *) 'IS_FINITE: ', is_finite(x)
    write (*, *) 'IEEE_IS_FINITE: ', ieee_is_finite(x)
end if

if (.not. all(is_inf(x) .eqv. check_inf)) then
    write (*, *) 'IS_INF: ', is_inf(x)
    write (*, *) 'IEEE_IS_INF: ', ieee_is_inf(x)
end if

if (.not. all(is_posinf(x) .eqv. check_posinf)) then
    write (*, *) 'IS_POSINF: ', is_posinf(x)
    write (*, *) 'IEEE_IS_POSINF: ', ieee_is_posinf(x)
end if

if (.not. all(is_neginf(x) .eqv. check_neginf)) then
    write (*, *) 'IS_NEGINF: ', is_neginf(x)
    write (*, *) 'IEEE_IS_NEGINF: ', ieee_is_neginf(x)
end if

if (.not. all(is_nan(x) .eqv. check_nan)) then
    write (*, *) 'IS_NAN: ', is_nan(x)
    write (*, *) 'IEEE_IS_NAN: ', ieee_is_nan(x)
end if

success_SP = all(is_finite(x) .eqv. check_finite) .and. all(is_inf(x) .eqv. check_inf) .and. &
    & all(is_posinf(x) .eqv. check_posinf) .and. all(is_neginf(x) .eqv. check_neginf) .and. &
    & all(is_nan(x) .eqv. check_nan)

if (.not. success_SP) then
    write (*, '(1A)') 'Some tests failed. The data is'
    write (*, '(1P, 20E8.0)') x
else
    write (*, '(1A)') 'All tests succeed.'
end if


write (*, '(/(1A))') 'Double-precision tests:'

if (.not. all(is_finite(y) .eqv. check_finite)) then
    write (*, *) 'IS_FINITE: ', is_finite(y)
    write (*, *) 'IEEE_IS_FINITE: ', ieee_is_finite(y)
end if

if (.not. all(is_inf(y) .eqv. check_inf)) then
    write (*, *) 'IS_INF: ', is_inf(y)
    write (*, *) 'IEEE_IS_INF: ', ieee_is_inf(y)
end if

if (.not. all(is_posinf(y) .eqv. check_posinf)) then
    write (*, *) 'IS_POSINF: ', is_posinf(y)
    write (*, *) 'IEEE_IS_POSINF: ', ieee_is_posinf(y)
end if

if (.not. all(is_neginf(y) .eqv. check_neginf)) then
    write (*, *) 'IS_NEGINF: ', is_neginf(y)
    write (*, *) 'IEEE_IS_NEGINF: ', ieee_is_neginf(y)
end if

if (.not. all(is_nan(y) .eqv. check_nan)) then
    write (*, *) 'IS_NAN: ', is_nan(y)
    write (*, *) 'IEEE_IS_NAN: ', ieee_is_nan(y)
end if

success_DP = all(is_finite(y) .eqv. check_finite) .and. all(is_inf(y) .eqv. check_inf) .and. &
    & all(is_posinf(y) .eqv. check_posinf) .and. all(is_neginf(y) .eqv. check_neginf) .and. &
    & all(is_nan(y) .eqv. check_nan)

if (.not. success_DP) then
    write (*, '(1A)') 'Some double-precision tests failed. The data is'
    write (*, '(1P, 20D8.0)') y
else
    write (*, '(1A)') 'All tests succeed.'
end if


write (*, *) ''


success = success_SP .and. success_DP


end function test_infnan


end module test_infnan_mod


program test
use, non_intrinsic :: test_infnan_mod, only : test_infnan

if (test_infnan()) then
    stop 0
else
    stop 1
end if

end program test
