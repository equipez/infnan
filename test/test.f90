program test
use, non_intrinsic :: test_infnan_mod, only : test_infnan

if (test_infnan()) then
    stop 0
else
    stop 1
end if

end program test
