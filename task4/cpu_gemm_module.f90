module cpu_gemm_module
    implicit none
    private
    public :: matmul_cpu

contains

    ! Базовый CPU GEMM через встроенную Fortran-функцию MATMUL.
    subroutine matmul_cpu(a, b, c)
        real(8), intent(in)  :: a(:,:), b(:,:)
        real(8), intent(out) :: c(:,:)
        c = matmul(a, b)
    end subroutine matmul_cpu

    ! Простой CPU GEMM для проверки (без BLAS)
    subroutine gemm_cpu_simple(a, b, c)
        real(8), intent(in)  :: a(:,:), b(:,:)
        real(8), intent(out) :: c(:,:)
        integer :: i, j, k, m, n, p
        real(8) :: sum
        
        m = size(a, 1)
        p = size(a, 2)
        n = size(b, 2)
        
        do i = 1, m
            do j = 1, n
                sum = 0.d0
                do k = 1, p
                    sum = sum + a(i, k) * b(k, j)
                end do
                c(i, j) = sum
            end do
        end do
    end subroutine gemm_cpu_simple

end module cpu_gemm_module
