program main
    use cudafor
    use utils_module
    use gpu_streams_module
    implicit none

    integer, parameter :: M        = 512
    integer, parameter :: N        = 512
    integer, parameter :: K_large  = 4096
    integer, parameter :: NSTREAMS = 4
    integer, parameter :: NUM_REPS = 5

    real(8), allocatable :: h_A(:,:), h_B(:,:)
    real(8), allocatable :: h_C_serial(:,:), h_C_concurrent(:,:)

    real :: t_serial_ms, t_concurrent_ms
    real(8) :: flops, diff
    integer :: istat

    write(*,'(a)') '============================================='
    write(*,'(a,i0,a,i0,a,i0,a,i0)') 'A: ', M,' x ',N,',  B: ',N,' x ',K_large
    write(*,'(a,i0)')   '  streams: ', NSTREAMS
    write(*,'(a,i0)')   '  repetitions: ', NUM_REPS
    write(*,'(a)') '============================================='

    allocate(h_A(M, N), h_B(N, K_large), stat=istat)
    if (istat /= 0) then
        write(*,'(a)') 'Memory allocation error in main (h_A, h_B)'
        stop 1
    end if

    allocate(h_C_serial(M, K_large), h_C_concurrent(M, K_large), stat=istat)
    if (istat /= 0) then
        write(*,'(a)') 'Memory allocation error in main (h_C_serial, h_C_concurrent)'
        stop 1
    end if

    call rnd_fill(h_A)
    call rnd_fill(h_B)

    write(*,'(a)', advance='no') 'Running serial...    '
    call gemm_serial(h_A, h_B, h_C_serial, NUM_REPS, t_serial_ms)
    write(*,'(a,f8.2,a)') 'done  ', t_serial_ms, ' ms'

    write(*,'(a)', advance='no') 'Running concurrent... '
    call gemm_concurrent(h_A, h_B, h_C_concurrent, NSTREAMS, NUM_REPS, t_concurrent_ms)
    write(*,'(a,f8.2,a)') 'done  ', t_concurrent_ms, ' ms'

    flops = 2.d0 * dble(M) * dble(N) * dble(K_large)

    write(*,'(a)') ''
    write(*,'(a)') '--------------------------------------------------'
    write(*,'(a25,2x,a8,2x,a10)') 'Mode', 'ms', 'GFLOPS'
    write(*,'(a)') '--------------------------------------------------'
    write(*,'(a25,2x,f8.2,2x,f8.3)') 'Serial', &
        t_serial_ms / real(NUM_REPS), flops / (dble(t_serial_ms / real(NUM_REPS))*1d-3) / 1d9
    write(*,'(a25,2x,f8.2,2x,f8.3)') 'Concurrent', &
        t_concurrent_ms / real(NUM_REPS), flops / (dble(t_concurrent_ms / real(NUM_REPS))*1d-3) / 1d9
    write(*,'(a)') '--------------------------------------------------'
    write(*,'(a,f5.2)') 'Speedup concurrent/serial: ', &
        (t_serial_ms / real(NUM_REPS)) / (t_concurrent_ms / real(NUM_REPS))

    diff = max_abs_diff(h_C_serial, h_C_concurrent)
    write(*,'(a,es10.3)') 'Max |serial - concurrent|: ', diff

    deallocate(h_A, h_B, h_C_serial, h_C_concurrent)
end program main
