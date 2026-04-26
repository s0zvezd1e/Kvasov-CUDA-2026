module utils_module
    implicit none
    private
    public :: rnd_fill, max_abs_diff, print_metrics

contains

    subroutine rnd_fill(a)
        real(8), intent(out) :: a(:,:)
        call random_number(a)
    end subroutine rnd_fill

    real(8) function max_abs_diff(a, b) result(diff)
        real(8), intent(in) :: a(:,:), b(:,:)
        diff = maxval(abs(a - b))
    end function max_abs_diff

    subroutine print_metrics(name, t_sec, gflops, bytes_moved)
        character(*), intent(in) :: name
        real(8),      intent(in) :: t_sec, gflops, bytes_moved
        real(8) :: bw_gbs

        if (t_sec > 0.d0) then
            bw_gbs = bytes_moved / t_sec / 1.d9
        else
            bw_gbs = 0.d0
        end if

        write(*,'(a25,2x,f10.6,2x,f12.3,2x,f12.3)') trim(name), t_sec, gflops, bw_gbs
    end subroutine print_metrics

end module utils_module
