program speed_test
    use utils_module
    use mat_transpose_cpu_module
    use gpu_transpose
    implicit none
    integer, parameter :: sizes(4) = [512, 1024, 2048, 4096]
    integer :: i, n, istat
    real, allocatable :: a(:,:), b_cpu(:,:), b_gpu(:,:)
    real :: t1, t2, time_cpu, time_gpu
    
    print *, '============================================='
    print *, '   Сравнение производительности CPU vs GPU'
    print *, '============================================='
    print *, 'Размер    | CPU время (с) | GPU время (с) | Ускорение'
    print *, '----------|---------------|---------------|----------'
    
    do i = 1, size(sizes)
        n = sizes(i)
        allocate(a(n, n), b_cpu(n, n), b_gpu(n, n), stat=istat)
        if (istat /= 0) cycle
        
        call rnd_fill(a)
        
        ! CPU замер
        call cpu_time(t1)
        call mat_transpose_cpu(a, b_cpu)
        call cpu_time(t2)
        time_cpu = t2 - t1
        
        ! GPU прогрев
        call transpose_gpu(a, b_gpu, 16)
        
        ! GPU замер
        call cpu_time(t1)
        call transpose_gpu(a, b_gpu, 16)
        call cpu_time(t2)
        time_gpu = t2 - t1
        
        print '(i6, " | ", f12.6, " | ", f12.6, " | ", f8.2, "x")', n, time_cpu, time_gpu, time_cpu/time_gpu
        
        deallocate(a, b_cpu, b_gpu)
    end do
    
    print *, '============================================='
    print *, 'Ускорение достигается на матрицах > 1024'
    print *, '============================================='
end program speed_test
