module m_type_Similarity
    use m_type_Parameter
    use m_global_vars
    implicit none
    type Similarity
        integer :: lag
        integer :: tmp  ! ID of target template
        real(kd) :: val ! NCC value
    end type Similarity
    contains
    subroutine initialize_similarity(prm,NCC)
        type(Parameter), intent(in) :: prm
        type(Similarity),intent(out),allocatable :: NCC(:)
        integer :: io_unit
        allocate(NCC(prm%n_win))
        NCC(:) = Similarity(lag = 0, val = 0.0, tmp = 0)
        open(newunit = io_unit, file = trim(prm%iodir)//"/results/temporal_log.bin", &
            status = "replace", action = "write")
        close(io_unit)
    end subroutine initialize_similarity
end module m_type_Similarity