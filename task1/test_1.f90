program add_arrays
    implicit none
    integer, parameter :: n = 10  ! Размер массивов
    real, dimension(n) :: A, B, C
    integer :: i
    
    ! Инициализация генератора случайных чисел
    call random_seed()
    
    ! Генерация массивов A и B
    call random_number(A)
    call random_number(B)
    
    ! Вывод массива A
    print *, 'Массив A:'
    do i = 1, n
        print '(f8.4)', A(i)
    end do
    
    print *  ! Пустая строка
    
    ! Вывод массива B
    print *, 'Массив B:'
    do i = 1, n
        print '(f8.4)', B(i)
    end do
    
    print *
    
    ! Сложение массивов
    do i = 1, n
        C(i) = A(i) + B(i)
    end do
    
    ! Вывод результата
    print *, 'Результат сложения C = A + B:'
    do i = 1, n
        print '(f8.4)', C(i)
    end do
    
end program add_arrays
