module consts_mod

implicit none
private
public :: SP, DP

integer, parameter :: SP = kind(0.0)
integer, parameter :: DP = kind(0.0D0)

end module consts_mod
