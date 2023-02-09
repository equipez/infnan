program test
use, non_intrinsic :: test_infnan_mod, only : test_infnan

if (.not. test_infnan()) then
    stop 1
end if

end program test
