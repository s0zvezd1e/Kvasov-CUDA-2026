program benchmark_full
    use cudafor
    use matrix_utils
    use cpu_transpose
    use gpu_transpose
    implicit none

    integer, parameter :: test_sizes(5) = [256, 512, 1024, 2048, 4096]
    integer :: i, n, istat
    real, allocatable :: a(:,:), b_cpu(:,:), b_gpu(:,:)
    real :: start_time, end_time, cpu_time_val, gpu_time_val
    real :: speedup
    
    print *, '=========================================================='
    print *, '        СРАВНЕНИЕ ПРОИЗВОДИТЕЛЬНОСТИ CPU vs GPU'
    print *, '=========================================================='
    print *, 'Размер    | CPU время (с) | GPU время (с) | Ускорение'
    print *, '----------|---------------|---------------|----------'
    
    do i = 1, size(test_sizes)
        n = test_sizes(i)
        
        ! Выделяем память под квадратные матрицы для простоты
        allocate(a(n, n), b_cpu(n, n), b_gpu(n, n), stat=istat)
        if (istat /= 0) then
            print *, 'Ошибка выделения памяти для n =', n
            cycle
        end if
        
        ! Заполняем случайными числами
        call random_fill(a)
        
        ! ===== CPU замер =====
        call cpu_time(start_time)
        call transpose_cpu(a, b_cpu)
        call cpu_time(end_time)
        cpu_time_val = end_time - start_time
        
        ! ===== GPU замер (с прогревочным запуском) =====
        ! Прогрев (инициализация CUDA)
        call transpose_gpu(a, b_gpu, 16)
        
        ! Основной замер
        call cpu_time(start_time)
        call transpose_gpu(a, b_gpu, 16)
        call cpu_time(end_time)
        gpu_time_val = end_time - start_time
        
        ! Проверка корректности
        if (.not. check_matrices_equal(b_gpu, b_cpu)) then
            print *, 'ОШИБКА: результаты не совпадают для n =', n
        end if
        
        speedup = cpu_time_val / gpu_time_val
        
        print '(i6, " | ", f12.6, " | ", f12.6, " | ", f8.2, "x")', n, cpu_time_val, gpu_time_val, speedup
        
        deallocate(a, b_cpu, b_gpu)
    end do
    
    print *, '=========================================================='
    print *, 'Примечание: GPU использует наивный метод транспонирования'
    print *, 'Для больших матриц ускорение должно расти'
    print *, '=========================================================='
    
end program benchmark_full
