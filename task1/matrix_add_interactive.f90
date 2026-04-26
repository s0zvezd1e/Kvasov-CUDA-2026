! ================================================================
! МОДУЛЬ С ЯДРОМ (вынесен отдельно)
! ================================================================

module matrix_kernel_module
    use cudafor
    implicit none
    
contains

    attributes(global) subroutine add_matrix_kernel(a, b, c, n, m)
        real, device, intent(in)  :: a(:,:)
        real, device, intent(in)  :: b(:,:)
        real, device, intent(out) :: c(:,:)
        integer, value, intent(in) :: n, m
        
        integer :: i, j
        
        i = (blockIdx%y - 1) * blockDim%y + threadIdx%y
        j = (blockIdx%x - 1) * blockDim%x + threadIdx%x
        
        if (i <= n .and. j <= m) then
            c(i, j) = a(i, j) + b(i, j)
        end if
        
    end subroutine add_matrix_kernel

end module matrix_kernel_module

! ================================================================
! ОСНОВНАЯ ПРОГРАММА
! ================================================================

program matrix_add_interactive
    use cudafor
    use matrix_kernel_module
    implicit none
    
    integer :: n, m, i, j, istat
    real, allocatable :: A(:,:), B(:,:), C_cpu(:,:), C_gpu(:,:)
    real, device, allocatable :: A_d(:,:), B_d(:,:), C_d(:,:)
    
    print *, '============================================='
    print *, 'СЛОЖЕНИЕ МАТРИЦ НА CPU И GPU'
    print *, '============================================='
    print *, ''
    
    print *, 'Введите количество СТРОК матрицы:'
    read *, n
    
    print *, 'Введите количество СТОЛБЦОВ матрицы:'
    read *, m
    
    print *, ''
    print *, 'Размер матрицы:', n, 'x', m
    print *, 'Всего элементов:', n * m
    print *, ''
    
    allocate(A(n, m), B(n, m), C_cpu(n, m), C_gpu(n, m), stat=istat)
    if (istat /= 0) stop 'Ошибка памяти на CPU'
    
    allocate(A_d(n, m), B_d(n, m), C_d(n, m), stat=istat)
    if (istat /= 0) stop 'Ошибка памяти на GPU'
    
    call random_seed()
    call random_number(A)
    call random_number(B)
    
    ! CPU сложение
    do i = 1, n
        do j = 1, m
            C_cpu(i, j) = A(i, j) + B(i, j)
        end do
    end do
    
    ! GPU сложение
    A_d = A
    B_d = B
    
    call add_matrix_kernel<<<dim3((m+15)/16, (n+15)/16, 1), dim3(16,16,1)>>>(A_d, B_d, C_d, n, m)
    
    istat = cudaDeviceSynchronize()
    C_gpu = C_d
    
    ! Сравнение
    call compare_matrices(C_cpu, C_gpu, n, m)
    
    ! Вывод угла матриц
    call print_corner('Исходная матрица A (угол 4x4):', A, n, m)
    call print_corner('Исходная матрица B (угол 4x4):', B, n, m)
    call print_corner('Результат CPU (угол 4x4):', C_cpu, n, m)
    call print_corner('Результат GPU (угол 4x4):', C_gpu, n, m)
    
    deallocate(A, B, C_cpu, C_gpu, A_d, B_d, C_d)
    
    print *, ''
    print *, '============================================='
    print *, 'ПРОГРАММА ЗАВЕРШЕНА УСПЕШНО!'
    print *, '============================================='

contains

    subroutine compare_matrices(cpu_mat, gpu_mat, n, m)
        real, intent(in) :: cpu_mat(:,:), gpu_mat(:,:)
        integer, intent(in) :: n, m
        real :: max_diff
        integer :: errors, i, j
        
        max_diff = 0.0
        errors = 0
        
        do i = 1, n
            do j = 1, m
                if (abs(cpu_mat(i,j) - gpu_mat(i,j)) > 1e-5) then
                    errors = errors + 1
                    max_diff = max(max_diff, abs(cpu_mat(i,j) - gpu_mat(i,j)))
                end if
            end do
        end do
        
        print *, ''
        print *, '=== РЕЗУЛЬТАТЫ СРАВНЕНИЯ CPU vs GPU ==='
        print *, 'Несовпадающих элементов:', errors
        print *, 'Максимальное отличие:', max_diff
        
        if (errors == 0) then
            print *, '✓ РЕЗУЛЬТАТЫ СОВПАДАЮТ!'
        else
            print *, '✗ ОШИБКА! Результаты не совпадают!'
        end if
    end subroutine compare_matrices
    
    subroutine print_corner(title, mat, n, m)
        character(*), intent(in) :: title
        real, intent(in) :: mat(:,:)
        integer, intent(in) :: n, m
        integer :: i, j, nr, nc
        
        nr = min(4, n)
        nc = min(4, m)
        
        print *, ''
        print *, title
        print *, 'Первые', nr, 'строк и', nc, 'столбцов:'
        
        do i = 1, nr
            write(*, '(4f10.4)') (mat(i, j), j = 1, nc)
        end do
    end subroutine print_corner

end program matrix_add_interactive
