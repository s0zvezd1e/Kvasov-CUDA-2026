program add_matrices_dynamic
    implicit none
    real, allocatable :: A(:, :), B(:, :), C(:, :)
    integer :: n, i, j
    
    ! Запрос размера матрицы
    print *, 'Введите размер матриц n (например, 4):'
    read *, n
    
    ! Выделение памяти
    allocate(A(n, n), B(n, n), C(n, n))
    
    ! Генерация случайных чисел
    call random_seed()
    call random_number(A)
    call random_number(B)
    
    ! Вывод матрицы A
    print *, 'Матрица A:'
    do i = 1, n
        print *, (A(i, j), j = 1, n)
    end do
    
    print *
    
    ! Вывод матрицы B
    print *, 'Матрица B:'
    do i = 1, n
        print *, (B(i, j), j = 1, n)
    end do
    
    print *
    
    ! Сложение матриц (векторная операция)
    C = A + B
    
    ! Вывод результата
    print *, 'Результат сложения C = A + B:'
    do i = 1, n
        print *, (C(i, j), j = 1, n)
    end do
    
    ! Освобождение памяти
    deallocate(A, B, C)
    
end program add_matrices_dynamic
