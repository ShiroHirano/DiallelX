module m_type_Parameter
!$ use omp_lib
    implicit none
    type Parameter
        integer :: n_chn ! number of channels
        integer :: n_tmp ! number of templates
        integer :: n_rec ! number of records
        integer :: n_win ! number of windows
        integer :: l_tmp ! length of template = length of fft
        integer :: l_rec ! length of record
        integer :: l_str ! length of stride
        integer :: l_pad ! length of padding
        character(len=256) :: iodir ! directory name for IO
        character(len=3) :: ext ! directory name for IO
    end type Parameter

    contains

    subroutine read_parameters(dirname,extention,prm)
        character(len = *),intent(in) :: dirname
        character(len = 3),intent(in) :: extention
        type(Parameter) :: prm
        character(len = 256) :: line
        character(len = 256) :: input
        integer :: idx, io_unit
        prm%iodir = trim(dirname)
        prm%ext = trim(extention)
        input = trim(dirname)//"/parameters/parameters.ini"
        open(newunit = io_unit, file = input, status = "old", action = "read")
            do
                read(io_unit, '(1a)', end = 999) line
                idx = index(line, "=")
                if ( index(line, "LenCnt") > 0 ) then
                    read(line(idx+1:), *) prm%l_rec
                else if ( index(line, "NumCnt") > 0 ) then
                    read(line(idx+1:), *) prm%n_rec
                else if ( index(line, "NumChn") > 0 ) then
                    read(line(idx+1:), *) prm%n_chn
                else if ( index(line, "LenTmp") > 0 ) then
                    read(line(idx+1:), *) prm%l_tmp
                else if ( index(line, "NumTmp") > 0 ) then
                    read(line(idx+1:), *) prm%n_tmp
                else if ( index(line, "Stride") > 0 ) then
                    read(line(idx+1:), *) prm%l_str
                end if
            end do
        999 close(io_unit)
        prm%n_win = int(real(prm%l_rec-1)/real(prm%l_str)) + 1
        prm%l_pad = prm%l_tmp + (prm%n_win-1)*prm%l_str - prm%l_rec
        call print_parameters(prm)

    end subroutine read_parameters

    subroutine print_parameters(prm)
        type(Parameter) :: prm
        print '("DiallelX: IO")'
        print '("  IO directory       :  ", 1a)', trim(prm%iodir)
        print '("  output file        :  ", 1a)', trim(prm%iodir)//"/results/candidates."//trim(prm%ext)
        print '("DiallelX: fundamental parameters")'
        print '("  number of records  :", 1i12, " records")', prm%n_rec
        print '("  length of record   :", 1i12, " samples")', prm%l_rec
        print '("  number of templates:", 1i12, " templates")', prm%n_tmp
        print '("  length of template :", 1i12, " samples (= fft length)")', prm%l_tmp
        print '("  number of channels :", 1i12, " channels")', prm%n_chn
        print '("DiallelX: optional parameters")'
        print '("  accuracy           :", 1i12)', prm%l_tmp/prm%l_str
        print '("  number of threads  :", 1i12, " OMP threads")', omp_get_max_threads()
        print '("DiallelX: derived parameters")'
        print '("  number of windows  :", 1i12, " windows")', prm%n_win
        print '("  length of stride   :", 1i12, " samples")', prm%l_str
        print '("  length of padding  :", 1i12, " samples")', prm%l_pad
        print '("  required memory    :", 1f12.3, " Gbytes (infimum)")', &
            4*prm%n_chn*(1d0*prm%n_win*prm%l_tmp+prm%l_rec &
            +3*prm%n_tmp*prm%l_tmp &
            +64*prm%n_rec) &
            /real(2**30)
        print '("  computation cost   :", 1es12.1e2, " ")', (1d0*prm%n_chn*prm%n_rec*prm%n_win*prm%n_tmp*prm%l_tmp)
    end subroutine print_parameters

end module m_type_Parameter