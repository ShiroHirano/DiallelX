module m_FFTW
    use,intrinsic :: ISO_C_binding
    implicit none
    type(C_PTR) :: plan, planR2C, planC2R	!	structure for FFTW
    include 'fftw3.f03'
end module m_FFTW
