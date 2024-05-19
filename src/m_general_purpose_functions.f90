module m_general_purpose_functions
    use m_global_vars
    implicit none
    contains

    integer function count_lines(filename)
        character(len = *), intent(in) :: filename
        integer :: io_unit
        count_lines = 0
        open(newunit = io_unit, file = trim(filename), status = 'old', action = "read")
        do
            read(io_unit, '()', end = 999)
            count_lines = count_lines + 1
        end do
        999 close(io_unit)
    end function count_lines

    integer function count_bytes(filename)
        character(len = *), intent(in) :: filename
        integer :: io_unit
        logical(1) :: B
        count_bytes = 0
        open(newunit = io_unit, file = trim(filename), form = 'unformatted', access = 'stream', status = 'old', action = "read")
        do
            read(io_unit, end = 9999) B
            count_bytes = count_bytes + 1
        end do
        9999 close(io_unit)
    end function count_bytes

    function normalize(wave)
        real(kd), intent(in) :: wave(:)
        real(kd), allocatable :: normalize(:)
        real(kd) :: amp
        normalize = wave - sum(wave)/real(size(wave))
        amp = norm2(normalize)
        if (amp == 0e0) then
            normalize = 0e0
        else
            normalize = normalize/amp
        end if
    end function normalize
    
    function extention_of_(filename)
        character(len = *), intent(in) :: filename
        character(len = 4) :: extention_of_
        integer :: idx
        idx = index(filename, '.', back = .true.)
        extention_of_ = trim(filename(idx+1:))
    end function extention_of_

end module m_general_purpose_functions