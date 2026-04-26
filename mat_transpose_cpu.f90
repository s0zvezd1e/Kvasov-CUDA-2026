module mat_transpose_cpu_module
    implicit none
    private
    public :: mat_transpose_cpu

contains

    subroutine mat_transpose_cpu(a, b)
        real, intent(in)  :: a(:,:)
        real, intent(out) :: b(:,:)
        b = transpose(a)
    end subroutine mat_transpose_cpu

end module mat_transpose_cpu_module
