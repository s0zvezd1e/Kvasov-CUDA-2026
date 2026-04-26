program cuda_matrix_add_random
    use cudafor
    implicit none
    
    ! Переменные
    integer :: n
    real, allocatable :: A_h(:,:), B_h(:,:), C_h(:,:)
    real, device, allocatable :: A_d(:,:), B_d(:, :), C_d(:,:)
    type(dim3) :: grid, block
    integer :: i, j, istat
    integer :: threads_per_block = 16
    real :: sum_gpu, sum_cpu
    real :: diff_max, diff
    
    ! Интерфейс для CUDA ядра
    interface
        attributes(global) subroutine matrix_add_kernel(A, B, C, n)
            real, device :: A(n,n), B(n,n), C(n,n)
            integer, value :: n
        end subroutine matrix_add_kernel
    end interface
    
    ! Ввод размера
    print *, '====================================='
    print *, 'Программа сложения случайных матриц на GPU'
    print *, '====================================='
    print *, 'Введите размер матрицы n:'
    read *, n
    
    if (n <= 0) then
        print *, 'Ошибка: размер должен быть положительным числом!'
        stop
    end if
    
    print *, 'Выбран размер матрицы:', n, 'x', n
    print *, 'Всего элементов:', n*n
    print *, 'Памяти на матрицу:', (n*n*4.0)/(1024*1024), 'MB'
    print *, '-------------------------------------'
    
    ! Выделение памяти на CPU
    allocate(A_h(n,n), B_h(n,n), C_h(n,n))
    
    ! Генерация случайных матриц
    print *, 'Генерация случайных матриц...'
    call random_seed()
    call random_number(A_h)  ! Случайные числа от 0 до 1
    call random_number(B_h)  ! Случайные числа от 0 до 1
    
    ! Покажем несколько элементов исходных матриц
    if (n <= 5) then
        print *, 'Матрица A (первые элементы):'
        do i = 1, min(5, n)
            print '(5f8.4)', (A_h(i,j), j = 1, min(5, n))
        end do
        
        print *, 'Матрица B (первые элементы):'
        do i = 1, min(5, n)
            print '(5f8.4)', (B_h(i,j), j = 1, min(5, n))
        end do
    end if
    
    ! Выделение памяти на GPU
    allocate(A_d(n,n), B_d(n,n), C_d(n,n))
    
    ! Копирование данных на GPU
    print *, 'Копирование данных на GPU...'
    A_d = A_h
    B_d = B_h
    
    ! Настройка размеров блоков и сетки
    block = dim3(threads_per_block, threads_per_block, 1)
    grid = dim3((n + block%x - 1)/block%x, (n + block%y - 1)/block%y, 1)
    
    print *, 'Запуск ядра CUDA...'
    print *, 'Grid:  ', grid%x, 'x', grid%y, 'x', grid%z
    print *, 'Block: ', block%x, 'x', block%y, 'x', block%z
    print *, 'Всего нитей:', grid%x * grid%y * block%x * block%y
    
    ! Запуск ядра
    call matrix_add_kernel<<<grid, block>>>(A_d, B_d, C_d, n)
    
    ! Проверка ошибок
    istat = cudaDeviceSynchronize()
    if (istat /= 0) then
        print *, 'Ошибка при выполнении ядра! Код ошибки:', istat
        stop
    end if
    
    ! Копирование результата обратно
    print *, 'Копирование результата с GPU...'
    C_h = C_d
    
    ! Вывод результата
    print *, '-------------------------------------'
    print *, 'РЕЗУЛЬТАТ:'
    
    if (n <= 10) then
        print *, 'Матрица результата C = A + B:'
        do i = 1, n
            print '(10f8.4)', (C_h(i,j), j = 1, n)
        end do
    else
        print *, 'Первые 5x5 элементов матрицы результата:'
        do i = 1, 5
            print '(5f8.4)', (C_h(i,j), j = 1, 5)
        end do
    end if
    
    ! Проверка корректности на нескольких случайных элементах
    print *, '-------------------------------------'
    print *, 'ПРОВЕРКА КОРРЕКТНОСТИ:'
    print *, 'i   j   A(i,j)   +   B(i,j)   =   C(i,j) (GPU)'
    
    diff_max = 0.0
    do i = 1, min(5, n)
        do j = 1, min(5, n)
            diff = abs(C_h(i,j) - (A_h(i,j) + B_h(i,j)))
            diff_max = max(diff_max, diff)
            print '(I3, I3, 3f8.4, f12.6)', i, j, A_h(i,j), B_h(i,j), C_h(i,j), diff
        end do
    end do
    
    print *, '-------------------------------------'
    print *, 'Максимальное отклонение:', diff_max
    
    if (diff_max < 1.0E-5) then
        print *, 'РЕЗУЛЬТАТ КОРРЕКТЕН! ✓'
    else
        print *, 'ОБНАРУЖЕНЫ ОШИБКИ! ✗'
    end if
    
    ! Очистка
    deallocate(A_h, B_h, C_h)
    deallocate(A_d, B_d, C_d)
    
    print *, '====================================='
    print *, 'Программа завершена!'
    print *, '====================================='
    
end program cuda_matrix_add_random

! Определение ядра отдельно
attributes(global) subroutine matrix_add_kernel(A, B, C, n)
    use cudafor
    implicit none
    real, device :: A(n,n), B(n,n), C(n,n)
    integer, value :: n
    integer :: i, j
    
    i = (blockIdx%x - 1) * blockDim%x + threadIdx%x
    j = (blockIdx%y - 1) * blockDim%y + threadIdx%y
    
    if (i <= n .and. j <= n) then
        C(i,j) = A(i,j) + B(i,j)
    end if
end subroutine matrix_add_kernel
