module utils_module
    implicit none
    private
    public :: rnd_fill, max_abs_diff

contains

    subroutine rnd_fill(a)
        real(8), intent(out) :: a(:,:)
        call random_number(a)
    end subroutine rnd_fill

    real(8) function max_abs_diff(a, b) result(diff)
        real(8), intent(in) :: a(:,:), b(:,:)
        diff = maxval(abs(a - b))
    end function max_abs_diff

end module utils_module
