module cpu_transpose
    implicit none
    private
    public :: transpose_cpu

contains

    subroutine transpose_cpu(a, b)
        real, intent(in)  :: a(:,:)
        real, intent(out) :: b(:,:)
        b = transpose(a)
    end subroutine transpose_cpu

end module cpu_transpose
