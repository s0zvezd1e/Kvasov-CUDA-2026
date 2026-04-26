module matrix_utils
    implicit none
    private
    public :: random_fill, check_matrices_equal

    interface random_fill
        module procedure fill_matrix_random
    end interface random_fill

contains

    subroutine fill_matrix_random(a)
        real, intent(out) :: a(:,:)
        call random_number(a)
        a = 100.0 * a
    end subroutine fill_matrix_random

    logical function check_matrices_equal(a, b) result(equal)
        real, intent(in) :: a(:,:), b(:,:)
        equal = all(abs(a - b) <= 1.0e-4)
    end function check_matrices_equal

end module matrix_utils
