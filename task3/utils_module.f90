module utils_module
    implicit none
    private
    public :: rnd_fill, max_abs_diff, print_metrics

contains

    subroutine rnd_fill(a)
        real, intent(out) :: a(:,:)
        call random_number(a)
        a = 100.0 * a
    end subroutine rnd_fill

    real(8) function max_abs_diff(a, b) result(diff)
        real, intent(in) :: a(:,:), b(:,:)
        diff = real(maxval(abs(a - b)), 8)
    end function max_abs_diff

    subroutine print_metrics(name, t_sec, bw_gbs)
        character(*), intent(in) :: name
        real(8),      intent(in) :: t_sec, bw_gbs
        write(*,'(a24,2x,f10.6,2x,f12.3)') trim(name), t_sec, bw_gbs
    end subroutine print_metrics

end module utils_module
