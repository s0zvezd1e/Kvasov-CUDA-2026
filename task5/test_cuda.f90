program test_cuda
    use cudafor
    implicit none
    real, device :: a(10)
    print *, "CUDA Fortran works!"
end program test_cuda
