program main
    use cudafor
    use utils_module
    use cpu_gemm_module
    use gpu_gemm_module
    implicit none

    integer, parameter :: N = 512
    integer, parameter :: NUM_REPS = 5
    integer, parameter :: NBYTES = 8

    real(8), allocatable :: a(:,:), b(:,:)
    real(8), allocatable :: c_cpu(:,:)
    real(8), allocatable :: c_bad(:,:), c_good(:,:), c_tiled(:,:), c_cublas(:,:)

    real(8) :: t0, t1, t_cpu
    real    :: t_bad_ms, t_good_ms, t_tiled_ms, t_cublas_ms
    real(8) :: flops, bytes_min, peak_bw
    type(cudaDeviceProp) :: prop
    integer :: istat

    istat = cudaGetDeviceProperties(prop, 0)
    peak_bw = 2.d0 * dble(prop%memoryClockRate) * dble(prop%memoryBusWidth) / 8.d6

    write(*,'(a)') '========================================================'
    write(*,'(a,a)') 'Device: ', trim(prop%name)
    write(*,'(a,f8.2,a)') 'Peak Bandwidth: ', peak_bw, ' GB/s'
    write(*,'(a)') '========================================================'
    write(*,'(a,i0)') 'Matrix size: ', N
    write(*,'(a,i0)') 'Repetitions: ', NUM_REPS
    write(*,*)

    allocate(a(N,N), b(N,N), c_cpu(N,N), &
             c_bad(N,N), c_good(N,N), c_tiled(N,N), c_cublas(N,N), stat=istat)
    call rnd_fill(a)
    call rnd_fill(b)

    flops = 2.d0 * dble(N) * dble(N) * dble(N)
    bytes_min = dble(N*N + N*N + N*N) * dble(NBYTES)

    ! CPU замер
    call cpu_time(t0)
    call matmul_cpu(a, b, c_cpu)
    call cpu_time(t1)
    t_cpu = (t1 - t0)

    ! GPU методы
    call gemm_bad_naive_gpu (a, b, c_bad,    t_bad_ms,    NUM_REPS)
    call gemm_good_naive_gpu(a, b, c_good,   t_good_ms,   NUM_REPS)
    call gemm_tiled_gpu     (a, b, c_tiled,  t_tiled_ms,  NUM_REPS)

    write(*,'(a)') '==================================================================='
    write(*,'(a36,2x,a10,2x,a8,2x,a10)') 'Routine', 'ms/call', 'GFLOPS', 'BW (GB/s)'
    write(*,'(a)') '-------------------------------------------------------------------'
    call print_row('CPU matmul (Fortran)',    t_cpu*1000.0, flops, bytes_min, peak_bw)
    write(*,'(a)') '-------------------------------------------------------------------'
    call print_row_ms('GPU naive (bad access)',  t_bad_ms,    NUM_REPS, flops, bytes_min, peak_bw)
    call print_row_ms('GPU naive (good access)', t_good_ms,   NUM_REPS, flops, bytes_min, peak_bw)
    call print_row_ms('GPU tiled (TILE=32)',     t_tiled_ms,  NUM_REPS, flops, bytes_min, peak_bw)
    write(*,'(a)') '==================================================================='
    write(*,*)

    write(*,'(a)') '--- Correctness (max |result - CPU| ) ---'
    write(*,'(a,es11.3)') 'GPU naive bad     : ', max_abs_diff(c_bad,     c_cpu)
    write(*,'(a,es11.3)') 'GPU naive good    : ', max_abs_diff(c_good,    c_cpu)
    write(*,'(a,es11.3)') 'GPU tiled         : ', max_abs_diff(c_tiled,   c_cpu)

    deallocate(a, b, c_cpu, c_bad, c_good, c_tiled, c_cublas)

contains

    subroutine print_row(name, t_ms, fl, byt, pk)
        character(*), intent(in) :: name
        real(8),      intent(in) :: t_ms, fl, byt, pk
        real(8) :: gflops, bw
        gflops = fl / (t_ms * 1.d-3) / 1.d9
        bw     = byt / (t_ms * 1.d-3) / 1.d9
        write(*,'(a36,2x,f10.6,2x,f8.3,2x,f10.2)') name, t_ms, gflops, bw
    end subroutine print_row

    subroutine print_row_ms(name, t_total_ms, nr, fl, byt, pk)
        character(*), intent(in) :: name
        real,         intent(in) :: t_total_ms
        integer,      intent(in) :: nr
        real(8),      intent(in) :: fl, byt, pk
        real(8) :: t_avg_ms, gflops, bw
        t_avg_ms = dble(t_total_ms) / dble(nr)
        gflops   = fl / (t_avg_ms * 1.d-3) / 1.d9
        bw       = byt / (t_avg_ms * 1.d-3) / 1.d9
        write(*,'(a36,2x,f10.6,2x,f8.3,2x,f10.2)') name, t_avg_ms, gflops, bw
    end subroutine print_row_ms

end program main
