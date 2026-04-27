module cpu_gemm_module
    implicit none
    private
    public :: matmul_cpu

contains

    subroutine matmul_cpu(a, b, c)
        real(8), intent(in)  :: a(:,:), b(:,:)
        real(8), intent(out) :: c(:,:)
        c = matmul(a, b)
    end subroutine

end module cpu_gemm_module
