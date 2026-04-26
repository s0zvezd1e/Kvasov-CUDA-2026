program main
    use cudafor
    use utils_module
    use mat_transpose_cpu_module
    use mat_transpose_module
    implicit none

    integer, parameter :: N        = 4096
    integer, parameter :: NUM_REPS = 10
    integer, parameter :: NBYTES   = 4

    real, allocatable :: a(:,:), b_cpu(:,:)
    real, allocatable :: b_naive(:,:), b_tiled(:,:)

    real(8) :: t0, t1, t_cpu_sec, t_naive_sec, t_tiled_sec
    real(8) :: flops, bytes_rw
    real(8) :: bw_cpu, bw_naive, bw_tiled, peak_bw

    type(cudaDeviceProp) :: prop
    integer :: istat, i

    istat = cudaGetDeviceProperties(prop, 0)
    peak_bw = 2.d0 * dble(prop%memoryClockRate) * dble(prop%memoryBusWidth) / 8.d6

    write(*,'(a)')         '========================================================='
    write(*,'(a,a)')       'Устройство: ', trim(prop%name)
    write(*,'(a,i0,a,i0)') 'Compute Capability: ', prop%major, '.', prop%minor
    write(*,'(a,f7.3,a)')  'Memory Clock: ', prop%memoryClockRate * 1.d-6, ' GHz'
    write(*,'(a,i0,a)')    'Memory Bus: ', prop%memoryBusWidth, ' bits'
    write(*,'(a,f8.2,a)')  'Peak Bandwidth: ', peak_bw, ' GB/s'
    write(*,'(a)')         '========================================================='
    write(*,*)

    write(*,'(a,i0,a,i0)') 'Matrix size: ', N, ' x ', N
    write(*,'(a,i0)')      'Repetitions: ', NUM_REPS
    write(*,*)

    allocate(a(N,N), b_cpu(N,N), b_naive(N,N), b_tiled(N,N), stat=istat)
    if (istat /= 0) stop 'Allocation error'
    
    call rnd_fill(a)

    flops = 2.0d0 * dble(N) * dble(N)
    bytes_rw = 2.0d0 * dble(N) * dble(N) * dble(NBYTES)

    call cpu_time(t0)
    call mat_transpose_cpu(a, b_cpu)
    call cpu_time(t1)
    t_cpu_sec = t1 - t0
    bw_cpu = bytes_rw / t_cpu_sec / 1.d9

    call cpu_time(t0)
    do i = 1, NUM_REPS
        call mat_transpose_gpu_naive(a, b_naive, 16)
    end do
    call cpu_time(t1)
    t_naive_sec = (t1 - t0) / dble(NUM_REPS)
    bw_naive = bytes_rw / t_naive_sec / 1.d9

    call cpu_time(t0)
    do i = 1, NUM_REPS
        call mat_transpose_gpu_tiled(a, b_tiled)
    end do
    call cpu_time(t1)
    t_tiled_sec = (t1 - t0) / dble(NUM_REPS)
    bw_tiled = bytes_rw / t_tiled_sec / 1.d9

    write(*,'(a)') '=============================================================='
    write(*,'(a24,4x,a12,4x,a12)') 'Method', 'Time (s)', 'BW (GB/s)'
    write(*,'(a)') '--------------------------------------------------------------'
    write(*,'(a24,4x,f12.6,4x,f10.2)') 'CPU (transpose)', t_cpu_sec, bw_cpu
    write(*,'(a24,4x,f12.6,4x,f10.2)') 'GPU naive', t_naive_sec, bw_naive
    write(*,'(a24,4x,f12.6,4x,f10.2)') 'GPU tiled (shared)', t_tiled_sec, bw_tiled
    write(*,'(a)') '=============================================================='
    write(*,*)

    write(*,'(a)') '--- Correctness (max diff from CPU) ---'
    write(*,'(a,es11.3)') 'GPU naive: ', max_abs_diff(b_naive, b_cpu)
    write(*,'(a,es11.3)') 'GPU tiled: ', max_abs_diff(b_tiled, b_cpu)

    if (max_abs_diff(b_naive, b_cpu) < 1e-5 .and. max_abs_diff(b_tiled, b_cpu) < 1e-5) then
        print *, '✓ ALL RESULTS ARE CORRECT!'
    else
        print *, '✗ ERROR: Results do not match!'
    end if

    deallocate(a, b_cpu, b_naive, b_tiled)

end program main
