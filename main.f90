program main

    use cudafor
    use utils_module
    use mat_transpose_module
    use mat_transpose_cpu_module

    implicit none

    integer, parameter :: N_ROWS = 1024
    integer, parameter :: N_COLS = 512
    integer, parameter :: BLOCK_DIM = 16
    integer, parameter :: K_PREVIEW = 4

    real, allocatable :: a(:,:), b_gpu(:,:), b_cpu(:,:)
    integer :: ra, ca, istat

    write(*,'(a)') '=== Matrix transpose ==='

    allocate(a(N_ROWS, N_COLS), b_gpu(N_COLS, N_ROWS), b_cpu(N_COLS, N_ROWS), stat=istat)
    if (istat /= 0) then
        write(*,'(a)') 'Memory allocation error in main'
        stop 1
    end if

    call rnd_fill(a)

    call mat_transpose_gpu(a, b_gpu, BLOCK_DIM)
    call mat_transpose_cpu(a, b_cpu)

    ra = min(K_PREVIEW, N_ROWS)
    ca = min(K_PREVIEW, N_COLS)

    call print_corner('A', a, ra, ca)
    call print_corner('B (CPU)', b_cpu, ca, ra)
    call print_corner('B (GPU)', b_gpu, ca, ra)

    if (check_equal_mat(b_gpu, b_cpu)) then
        print *, 'Transpose arrays are equal'
    else
        print *, 'Transpose arrays are not equal'
    end if

    deallocate(a, b_gpu, b_cpu)

contains

    ! Печать левого верхнего угла матрицы для быстрой визуальной проверки.
    subroutine print_corner(title, x, nrows, ncols)
        character(*), intent(in) :: title
        real,         intent(in) :: x(:,:)
        integer,      intent(in) :: nrows, ncols
        integer :: i, j

        write(*,*)
        write(*,'(a)') trim(title)
        do i = 1, nrows
            write(*,'(*(f10.4,1x))') (x(i, j), j = 1, ncols)
        end do
    end subroutine print_corner

end program main
