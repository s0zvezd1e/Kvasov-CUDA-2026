program main
    use cudafor
    use utils_module
    use mat_transpose_cpu_module
    use mat_transpose_module
    implicit none

    integer, parameter :: N        = 4096
    integer, parameter :: NUM_REPS = 100
    integer, parameter :: NBYTES   = 4

    real, allocatable :: a(:,:), b_cpu(:,:), b_copy(:,:)
    real, allocatable :: b_naive(:,:), b_bank(:,:), b_nobank(:,:)

    real    :: t_copy_ms, t_naive_ms, t_bank_ms, t_nobank_ms
    real(8) :: t0, t1, t_cpu_sec
    real(8) :: bytes_rw
    real(8) :: bw_copy, bw_naive, bw_bank, bw_nobank, bw_cpu, peak_bw

    type(cudaDeviceProp) :: prop
    integer :: istat

    ! Информация об устройстве
    istat = cudaGetDeviceProperties(prop, 0)

    peak_bw = 2.d0 * dble(prop%memoryClockRate) * dble(prop%memoryBusWidth) &
              / 8.d6

    write(*,'(a)')         '========================================================='
    write(*,'(a,a)')       'Device Name:        ', trim(prop%name)
    write(*,'(a,i0,a,i0)') 'Compute Capability: ', prop%major, '.', prop%minor
    write(*,'(a,f7.3,a)')  'Memory Clock Rate:  ', prop%memoryClockRate * 1.d-6, ' GHz'
    write(*,'(a,i0,a)')    'Memory Bus Width:   ', prop%memoryBusWidth, ' bits'
    write(*,'(a,f8.2,a)')  'Peak Bandwidth:     ', peak_bw, ' GB/s'
    write(*,'(a)')         '========================================================='
    write(*,*)

    write(*,'(a,i0,a,i0)')       'Matrix size  : ', N, ' x ', N
    write(*,'(a,i0,a,i0,a)')     'Block  (x,y) : ', TILE_DIM, ' x ', BLOCK_ROWS, &
                                 '  (= TILE_DIM x BLOCK_ROWS)'
    write(*,'(a,i0,a,i0,a)')     'Grid   (x,y) : ', N/TILE_DIM, ' x ', N/TILE_DIM, &
                                 '  blocks'
    write(*,'(a,i0,a)')          'Tile size    : ', TILE_DIM, ' x TILE_DIM (shared memory patch)'
    write(*,'(a,i0)')            'Repetitions  : ', NUM_REPS
    write(*,*)

    ! Выделение памяти
    allocate(a(N,N), b_cpu(N,N), b_copy(N,N), &
             b_naive(N,N), b_bank(N,N), b_nobank(N,N), stat=istat)
    if (istat /= 0) then
        write(*,'(a)') 'Memory allocation error in main'
        stop 1
    end if
    call rnd_fill(a)

    ! Объём данных за один вызов (чтение + запись)
    bytes_rw = 2.d0 * dble(N) * dble(N) * dble(NBYTES)

    ! CPU замер
    call cpu_time(t0)
    call mat_transpose_cpu(a, b_cpu)
    call cpu_time(t1)
    t_cpu_sec = t1 - t0
    bw_cpu = bytes_rw / t_cpu_sec / 1.d9

    ! GPU замеры
    call gpu_copy_shared_bw         (a, b_copy,   t_copy_ms,   NUM_REPS)
    call gpu_transpose_naive        (a, b_naive,  t_naive_ms,  NUM_REPS)
    call gpu_transpose_bank_conflict(a, b_bank,   t_bank_ms,   NUM_REPS)
    call gpu_transpose_no_conflict  (a, b_nobank, t_nobank_ms, NUM_REPS)

    ! Пропускная способность
    bw_copy   = bytes_rw / (dble(t_copy_ms)   / NUM_REPS * 1.d-3) / 1.d9
    bw_naive  = bytes_rw / (dble(t_naive_ms)  / NUM_REPS * 1.d-3) / 1.d9
    bw_bank   = bytes_rw / (dble(t_bank_ms)   / NUM_REPS * 1.d-3) / 1.d9
    bw_nobank = bytes_rw / (dble(t_nobank_ms) / NUM_REPS * 1.d-3) / 1.d9

    ! Вывод результатов
    write(*,'(a)') '=============================================================='
    write(*,'(a30,4x,a10,4x,a8)') 'Routine', 'BW (GB/s)', '% peak'
    write(*,'(a)') '--------------------------------------------------------------'
    call print_bw_row('shared memory copy',       bw_copy,   peak_bw)
    call print_bw_row('naive transpose',           bw_naive,  peak_bw)
    call print_bw_row('coalesced (bank conflicts)',bw_bank,   peak_bw)
    call print_bw_row('conflict-free transpose',   bw_nobank, peak_bw)
    write(*,'(a)') '--------------------------------------------------------------'
    call print_bw_row('CPU transpose',             bw_cpu,    peak_bw)
    write(*,'(a)') '=============================================================='
    write(*,*)

    ! Проверка корректности
    write(*,'(a,es11.3)') 'max|naive      - CPU| = ', max_abs_diff(b_naive,  b_cpu)
    write(*,'(a,es11.3)') 'max|bank conf  - CPU| = ', max_abs_diff(b_bank,   b_cpu)
    write(*,'(a,es11.3)') 'max|no conflict- CPU| = ', max_abs_diff(b_nobank, b_cpu)

    deallocate(a, b_cpu, b_copy, b_naive, b_bank, b_nobank)

contains

    subroutine print_bw_row(name, bw, peak)
        character(*), intent(in) :: name
        real(8),      intent(in) :: bw, peak
        write(*,'(a30,4x,f10.2,4x,f7.1,a)') name, bw, bw/peak*100.d0, '%'
    end subroutine print_bw_row

end program main
