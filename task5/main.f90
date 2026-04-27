program main
    use cudafor
    use utils_module
    use gpu_streams_module
    implicit none

    integer, parameter :: M = 512
    integer, parameter :: N = 512
    integer, parameter :: K = 4096
    integer, parameter :: NSTREAMS = 4

    real(8), allocatable :: h_A(:,:), h_B(:,:)
    real(8), allocatable :: h_C_serial(:,:), h_C_concurrent(:,:)

    real :: t_serial_ms, t_concurrent_ms
    real(8) :: flops, diff
    integer :: istat

    write(*,'(a)') '============================================='
    write(*,'(a,i0,a,i0,a,i0,a,i0)') 'A: ', M,' x ',N,',  B: ',N,' x ',K
    write(*,'(a,i0)')   '  streams: ', NSTREAMS
    write(*,'(a)') '============================================='

    allocate(h_A(M, N), h_B(N, K), h_C_serial(M, K), h_C_concurrent(M, K), stat=istat)
    if (istat /= 0) stop 'Memory allocation error'

    call rnd_fill(h_A)
    call rnd_fill(h_B)

    write(*,'(a)', advance='no') 'Running serial...    '
    call gemm_serial(h_A, h_B, h_C_serial, NSTREAMS, t_serial_ms)
    write(*,'(a,f8.2,a)') 'done  ', t_serial_ms, ' ms'

    write(*,'(a)', advance='no') 'Running concurrent... '
    call gemm_concurrent(h_A, h_B, h_C_concurrent, NSTREAMS, t_concurrent_ms)
    write(*,'(a,f8.2,a)') 'done  ', t_concurrent_ms, ' ms'

    flops = 2.d0 * dble(M) * dble(N) * dble(K)

    write(*,'(a)') ''
    write(*,'(a)') '--------------------------------------------------'
    write(*,'(a25,2x,a8,2x,a10)') 'Mode', 'ms', 'GFLOPS'
    write(*,'(a)') '--------------------------------------------------'
    write(*,'(a25,2x,f8.2,2x,f8.3)') 'Serial', t_serial_ms, flops / (dble(t_serial_ms)*1d-3) / 1d9
    write(*,'(a25,2x,f8.2,2x,f8.3)') 'Concurrent', t_concurrent_ms, flops / (dble(t_concurrent_ms)*1d-3) / 1d9
    write(*,'(a)') '--------------------------------------------------'
    write(*,'(a,f5.2)') 'Speedup: ', t_serial_ms / t_concurrent_ms

    diff = max_abs_diff(h_C_serial, h_C_concurrent)
    write(*,'(a,es10.3)') 'Max diff: ', diff

    deallocate(h_A, h_B, h_C_serial, h_C_concurrent)
end program main
