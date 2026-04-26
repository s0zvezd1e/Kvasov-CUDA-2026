module utils_module
    implicit none
    private
    public :: rnd_fill, max_abs_diff

contains

    subroutine rnd_fill(a)
        real, intent(out) :: a(:,:)
        call random_number(a)
        a = 100.0 * a
    end subroutine

    real(8) function max_abs_diff(a, b) result(diff)
        real, intent(in) :: a(:,:), b(:,:)
        diff = real(maxval(abs(a - b)), 8)
    end function

end module utils_module
