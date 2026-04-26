module utils_module
    implicit none
    private
    public :: rnd_fill, check_equal_mat

    interface rnd_fill
        module procedure rnd_fill_2d
    end interface rnd_fill

contains

    subroutine rnd_fill_2d(a)
        real, intent(out) :: a(:,:)
        call random_number(a)
        a = 100.0 * a
    end subroutine rnd_fill_2d

    logical function check_equal_mat(a, b) result(equal)
        real, intent(in) :: a(:,:), b(:,:)
        equal = all(abs(a - b) <= 1.0e-4)
    end function check_equal_mat

end module utils_module
