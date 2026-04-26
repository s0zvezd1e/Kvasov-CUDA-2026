program test_simple
    use cudafor
    implicit none
    
    integer, parameter :: n = 4, m = 3
    real :: a(n,m), b(m,n)
    real, device :: a_d(n,m), b_d(m,n)
    integer :: i, j
    
    ! Заполняем матрицу A
    do i = 1, n
        do j = 1, m
            a(i,j) = (i-1)*m + j
        end do
    end do
    
    print *, 'Original matrix A (', n, 'x', m, '):'
    do i = 1, n
        print '(3f8.1)', a(i,:)
    end do
    
    ! Копируем на GPU и транспонируем вручную
    a_d = a
    do i = 1, n
        do j = 1, m
            b_d(j,i) = a_d(i,j)
        end do
    end do
    b = b_d
    
    print *, 'Transposed matrix B (', m, 'x', n, '):'
    do i = 1, m
        print '(4f8.1)', b(i,:)
    end do
end program test_simple
