module consts_mod
use, intrinsic :: iso_c_binding, only : c_int64_t

implicit none
private
public :: SP, DP, datasize

integer, parameter :: SP = kind(0.0)
integer, parameter :: DP = kind(0.0D0)
integer, parameter :: datasize = c_int64_t

end module consts_mod
