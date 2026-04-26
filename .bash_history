passwd
passwd
passwd
ls
whoami
pwd
hostname
ls -la
passwd
ls -la
ls
passwd
del test_1
rm test_1
ls
exit
ls
ls -la
nano
nano
ls
passwd
ls
dir test_1
nano test_1
nano test_1
nano test_1
ls
gfortran test_1.f90 -o matrix_mult
./matrix_mult
ls
nano test_1.90
nano test_1.f90
rm test_1.f90
ls
rm matrix_mult 
ls
nano test_1.f90
ls
nano test_1.f90
gfortran test_1.f90 -o test_1
ls
./test_1 
nano test_2
ls
rm test_2 
ls
nano test_2.f90
gfortran test_2.f90 -o test_2
./test_2
./test_2
./test_2
nano test_cuda.f90
nvfortran test_cuda.f90 -o test_cuda
module avail cuda
module avail pgi
module avail nvhpc
nvidia-smi
# Где находится nvfortran?
which nvfortran
# Какая версия?
nvfortran -V
# Поиск библиотек CUDA Fortran
find /usr -name "*cudafor*" 2>/dev/null
find /opt -name "*cudafor*" 2>/dev/null
ls
nano test_cuda.f90
ls
rm test_cuda.f90 
ls
nano cuda_matrix_add_working.f90
ls
# Компиляция с CUDA поддержкой
nvfortran -cuda cuda_matrix_add_working.f90 -o cuda_matrix_add
# Если хотите увидеть предупреждения
nvfortran -cuda -Minfo=all cuda_matrix_add_working.f90 -o cuda_matrix_add
./cuda_matrix_add
ls
ls 
nano cuda_matrix_add_working.f90 
rm cuda_matrix_add_working.f90 
nano program cuda_matrix_add_input
    use cudafor
    implicit none
    
    ! Переменные
    integer :: n  ! Размер матрицы (будет введен с клавиатуры)
    real, allocatable :: A_h(:,:), B_h(:,:), C_h(:,:)
    real, device, allocatable :: A_d(:,:), B_d(:, :), C_d(:,:)
    type(dim3) :: grid, block
    integer :: i, j, istat
    integer :: threads_per_block = 16
    real :: sum_gpu, sum_cpu
    
    ! Интерфейс для CUDA ядра
    interface
        attributes(global) subroutine matrix_add_kernel(A, B, C, n)
            real, device :: A(n,n), B(n,n), C(n,n)
            integer, value :: n
        end subroutine matrix_add_kernel
    end interface
    
    ! Ввод размера матрицы с клавиатуры
    print *, '====================================='
    print *, 'Программа сложения матриц на GPU'
    print *, '====================================='
    print *, 'Введите размер матрицы n (например, 1024):'
    read *, n
    
    ! Проверка ввода
    if (n <= 0) then         print *, 'Ошибка: размер должен быть положительным числом!';         stop;     end if         print *, 'Выбран размер матрицы:', n, 'x', n;     print *, 'Всего элементов:', n*n;     print *, 'Памяти на матрицу:', (n*n*4.0)/(1024*1024), 'MB'
    print *, '-------------------------------------'
    
    ! Выделение памяти на CPU
    allocate(A_h(n,n), B_h(n,n), C_h(n,n))
    
    ! Инициализация данных
    print *, 'Инициализация матриц...'
    do i = 1, n
        do j = 1, n
            A_h(i,j) = 1.0
            B_h(i,j) = 2.0
        end do
    end do
    
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
    if (istat /= 0) then         print *, 'Ошибка при выполнении ядра!';         stop;     end if         ! Копирование результата обратно;     print *, 'Копирование результата с GPU...';     C_h = C_d         ! Проверка результата;     print *, '-------------------------------------';     print *, 'Результат:'         ! Для маленьких матриц выводим все;     if (n <= 10) then         print *, 'Матрица результата:';         do i = 1, n
            print '(10f8.4)', (C_h(i,j), j = 1, n)
        end do
    else
        ! Для больших - только углы
        print *, 'Угловые элементы матрицы результата:'
        print *, 'C(1,1) =', C_h(1,1)
        print *, 'C(1,n) =', C_h(1,n)
        print *, 'C(n,1) =', C_h(n,1)
        print *, 'C(n,n) =', C_h(n,n)
        
        ! Покажем несколько первых элементов
        print *, 'Первые 5x5 элементов:'
        do i = 1, min(5, n)
            print '(5f8.4)', (C_h(i,j), j = 1, min(5, n))
        end do
    end if
    
    ! Проверка суммы всех элементов
    sum_gpu = sum(C_h)
    sum_cpu = n*n * 3.0  ! Должно быть 3.0 на каждый элемент
    
    print *, '-------------------------------------'
    print *, 'Проверка:'
    print *, 'Сумма всех элементов (GPU): ', sum_gpu
    print *, 'Ожидаемая сумма:           ', sum_cpu
    print *, 'Разница:                   ', abs(sum_gpu - sum_cpu)
    
    ! Очистка
    deallocate(A_h, B_h, C_h)
    deallocate(A_d, B_d, C_d)
    
    print *, '====================================='
    print *, 'Программа завершена успешно!'
    print *, '====================================='
    
end program cuda_matrix_add_input
! Определение ядра отдельно
attributes(global) subroutine matrix_add_kernel(A, B, C, n)
    use cudafor
    implicit none
    real, device :: A(n,n), B(n,n), C(n,n)
    integer, value :: n
    integer :: i, j
    
    i = (blockIdx%x - 1) * blockDim%x + threadIdx%x
    j = (blockIdx%y - 1) * blockDim%y + threadIdx%y
    
    if (i <= n .and. j <= n) then         C(i,j) = A(i,j) + B(i,j)
    end if
end subroutine matrix_add_kernel
ls
nano cuda_matrix_add_input.f90
ls
# Компиляция
nvfortran -cuda cuda_matrix_add_input.f90 -o cuda_matrix_input
# Запуск
./cuda_matrix_input
./cuda_matrix_input
./cuda_matrix_input
ls
rm cuda_matrix_add_input.f90
rm cuda_matrix_add 
rm cuda_matrix_input 
ls
nano cuda_matrix_add_random.f90
ls
nano cuda_matrix_add_random.f90
ls
ls
# Компилируем
nvfortran -cuda cuda_matrix_add_random.f90 -o cuda_matrix_random
# Запускаем
./cuda_matrix_random
./cuda_matrix_random
./cuda_matrix_random
ls
# Скачать один файл
scp st103333@titan:/home/st103333/cuda_matrix_add_random.f90 ~/Desktop/
# На сервере (вы сейчас здесь) посмотрите полный путь
pwd
ls -la cuda_matrix_add_random.f90
scp st103333@titan:/home2/st103333/cuda_matrix_add_random.f90 ~/Desktop/
mc
mc
ls
./cuda_matrix_random 
ls
cat cuda_matrix_add_random.f90 
git init
git status
git add .
ls
ls -la
git commit
git commit -m "Initial commit"
ls
mc
cd..
cd ..
git push origin main
ls
# Установите ваше имя и email (те, что на GitHub)
git config --global user.name "s0zvezd1e"
git config --global user.email "matoxa1969.3.1.6@gmail.com"
git push origin main
git branch -M main
git push -u origin main
git remote add origin https://github.com/s0zvezd1e/Kvasov-CUDA-2026.git
git remote -v
git branch -M main
git push -u origin main
