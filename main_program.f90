program main_program
    use cudafor
    use matrix_utils
    use cpu_transpose
    use gpu_transpose

    implicit none

    integer, parameter :: N_ROWS = 1024
    integer, parameter :: N_COLS = 512
    integer, parameter :: BLOCK_SIZE = 16
    integer, parameter :: PREVIEW_SIZE = 4

    real, allocatable :: matrix_a(:,:), result_gpu(:,:), result_cpu(:,:)
    integer :: istat

    write(*,'(a)') '=== Программа транспонирования матрицы ==='
    write(*,'(a,i0,a,i0)') 'Размер матрицы: ', N_ROWS, ' x ', N_COLS

    allocate(matrix_a(N_ROWS, N_COLS), result_gpu(N_COLS, N_ROWS), result_cpu(N_COLS, N_ROWS), stat=istat)
    if (istat /= 0) then
        print *, 'Ошибка выделения памяти!'
        stop 1
    end if

    call random_fill(matrix_a)
    call transpose_gpu(matrix_a, result_gpu, BLOCK_SIZE)
    call transpose_cpu(matrix_a, result_cpu)

    call print_matrix_corner('Исходная матрица A:', matrix_a, PREVIEW_SIZE, PREVIEW_SIZE)
    call print_matrix_corner('Результат (CPU):', result_cpu, PREVIEW_SIZE, PREVIEW_SIZE)
    call print_matrix_corner('Результат (GPU):', result_gpu, PREVIEW_SIZE, PREVIEW_SIZE)

    if (check_matrices_equal(result_gpu, result_cpu)) then
        print *, '✓ РЕЗУЛЬТАТЫ СОВПАДАЮТ! Транспонирование выполнено верно.'
    else
        print *, '✗ ОШИБКА! Результаты не совпадают.'
    end if

    deallocate(matrix_a, result_gpu, result_cpu)

contains

    subroutine print_matrix_corner(title, x, nrows, ncols)
        character(*), intent(in) :: title
        real,         intent(in) :: x(:,:)
        integer,      intent(in) :: nrows, ncols
        integer :: i, j

        write(*,*)
        write(*,'(a)') title
        do i = 1, nrows
            write(*,'(*(f10.4,1x))') (x(i, j), j = 1, ncols)
        end do
    end subroutine print_matrix_corner

end program main_program
