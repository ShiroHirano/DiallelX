module m_io
    use m_general_purpose_functions, only: count_lines, extention_of_
    use m_type_Parameter
    use m_type_Similarity
    use m_global_vars, only: kd, n_bin, prm
    use m_sort
    implicit none
    contains

    subroutine read_templ(prm, tmp)
        type(Parameter), intent(in) :: prm
        real(kd), intent(out), allocatable :: tmp(:, :, :)
        character(len = 256), allocatable :: templ_file(:, :)
        integer :: io_unit, i, j, st
        character(len = 256) :: filename
        call read_filelist(trim(prm%iodir)//"/parameters/templates.csv", prm, templ_file)
        allocate(tmp(prm%l_tmp, prm%n_chn, prm%n_tmp))
        do i = 1, prm%n_tmp
            do j = 1, prm%n_chn
                filename = trim(prm%iodir)//"/templates/"//trim(templ_file(i, j))
                st = 1
                if (extention_of_(filename) == "sac") st = st + 632
                open(newunit = io_unit, file = trim(filename), &
                form = "unformatted", access = "stream", status = "old")
                    read(io_unit, pos = st, end = 999) tmp(:, j, i)
                999 close(io_unit)
            end do
        end do
    end subroutine read_templ

    subroutine read_conti(prm, conti_file, conti)
        type(Parameter), intent(in) :: prm
        character(len = *), intent(in) :: conti_file(:)
        real(kd), intent(inout) :: conti(:, :)
        integer :: io_unit, i, st
        character(len = 256) :: filename
        if (conti_file(1) == "null") then
            conti = 0e0
            return
        end if
        do i = 1, prm%n_chn
            filename = trim(prm%iodir)//"/continuous_records/"//trim(conti_file(i))
            st = 1
            if (extention_of_(filename) == "sac") st = st + 632
        open(newunit = io_unit, file = trim(filename), &
            form = "unformatted", access = "stream", status = "old")
                read(io_unit, pos = st, end = 999) conti(:, i)
            999 close(io_unit)
        end do
    end subroutine read_conti

    subroutine read_filelist(csv_filename, prm, filelist)
        character(len = *), intent(in) :: csv_filename
        type(Parameter), intent(in) :: prm
        character(len = *), intent(out), allocatable :: filelist(:, :)
        integer :: io_unit, i, n
        n = count_lines(trim(csv_filename))
        allocate(filelist(n+1, prm%n_chn))
        open(newunit = io_unit, file = trim(csv_filename))
            do i = 1, n
                read(io_unit, *) filelist(i, :)
            end do
        close(io_unit)
        filelist(n+1, :) = "null"
    end subroutine read_filelist

    subroutine output_temporal_candidates(NCC, prm, rec_num)
        type(Similarity), intent(inout) :: NCC(:)
        type(Parameter), intent(in) :: prm
        integer, intent(in) :: rec_num
        integer :: io_unit, i
        open(newunit = io_unit, file = trim(prm%iodir)//"/results/temporal_log.bin", &
            form = "unformatted", access = "stream", &
            status = "old", action = "write", position = "append")
            do i = 1, prm%n_win
                if ( NCC(i)%tmp .ne. 0 ) then
                    write(io_unit) rec_num, i, NCC(i)
                end if
!                if (NCC(i)%val .gt. 1e0) print*, "m_io.f90: 79", NCC(i)%val
            end do
        close(io_unit)
    end subroutine output_temporal_candidates

    subroutine output(histogram)
        integer(8),intent(in) :: histogram(n_bin)
        integer :: io_unit, i
        integer(8) :: total
        total = sum(histogram(:))
        open(newunit = io_unit, file = trim(prm%iodir)//"/results/histogram.csv", status = "replace", action = "write")
            do i = 1, n_bin
                write(io_unit, '(1f6.4,",",1I0,",",1I0)') real(i-1)/real(n_bin), histogram(i), total
                total = total - histogram(i)
            end do
        close(io_unit)
    end subroutine output

    subroutine progress_reports(step,filename,n)
        integer,intent(in) :: step, n
        character(len=*),intent(in) :: filename
        character(len = 20) :: RecordID
        character(len = 64) :: FmtStr
        integer :: j
        FmtStr = '("  RecordID=",1a,", progress=",1i5,"/",1i5,", ")'
        j = index(filename,"_")-1
        RecordID = filename(1:j)
        j = index(FmtStr,',"/",1i')-1
        write(FmtStr(j:j),'(1i1)') int(log10(1e0*n))+1
        FmtStr(j+8:j+8) = FmtStr(j:j)
        write(*,trim(FmtStr),advance='no') trim(RecordID), step, n
    end subroutine progress_reports

    subroutine extract_candidates(prm)
        type(Parameter), intent(in) :: prm
        character(len=256) :: log_filename
        character(len=256) :: output_filename
        type(Similarity) :: NCC(0:1)
        integer :: rec_num(0:1) = 1
        integer :: io_unit(0:1), i, j(0:1), n = 0
        type(candidates),allocatable :: line(:)
        NCC(0)%val = 0e0
        log_filename = trim(prm%iodir)//"/results/temporal_log.bin"
        output_filename = trim(prm%iodir)//"/results/candidates."//prm%ext
        open(newunit = io_unit(0), file=trim(log_filename), &
            form="unformatted", status="old", action="read", access="stream")
        open(newunit = io_unit(1), file="/tmp/DiallelX_temporal_log.bin", &
            form="unformatted", status="replace", action="write", access="stream")
            do
                read(io_unit(0),end=999) rec_num(1), j(1), NCC(1)
!                if (NCC(0)%val .gt. 1e0) print*, "m_io.f90: 129", NCC(0)%val
!                if (NCC(1)%val .gt. 1e0) print*, "m_io.f90: 130", NCC(1)%val
                if (NCC(1)%val > 1.001) then
                    print*, rec_num(1), j(1), NCC(1)
                    stop
                end if
                rec_num(0) = rec_num(1) - 1
                if (rec_num(1) > 1 .and. NCC(1)%lag < 0) then
                    rec_num(1) = rec_num(1) - 1
                    NCC(1)%lag = NCC(1)%lag + prm%l_rec
                end if
                if (j(1) .ne. prm%n_win .and. j(1) .ne. 1) then
                    write(io_unit(1)) rec_num(1), NCC(1)
                    j(0) = j(1)
                    n = n + 1
                else if (j(1) .eq. prm%n_win) then
                    j(0) = j(1)
                    NCC(0) = NCC(1)
                else if (j(0) .eq. prm%n_win .and. j(1) .ne. 1) then
                    write(io_unit(1)) rec_num(0), NCC(0)
                    write(io_unit(1)) rec_num(1), NCC(1)
                    n = n + 2
                else if (j(0) .ne. prm%n_win .and. j(1) .eq. 1) then
                    write(io_unit(1)) rec_num(1), NCC(1)
                    n = n + 1
                else if (j(0) .eq. prm%n_win .and. j(1) .eq. 1) then
                    if (NCC(0)%val > NCC(1)%val) then
                        write(io_unit(1)) rec_num(0), NCC(0)
                        n = n + 1
                    else
                        write(io_unit(1)) rec_num(1), NCC(1)
                        n = n + 1
                    end if
                    j(0) = j(1)
                end if
            end do
        999 close(io_unit(0))
        close(io_unit(1))

        allocate(line(n))

        open(newunit = io_unit(0), file="/tmp/DiallelX_temporal_log.bin", &
        form="unformatted", status="old", action="read", access="stream")
        do i = 1,n
            read(io_unit(0)) line(i)
!            if (line(i)%NCC .gt. 1e0) print*, "m_io.f90: 172", line(i)%NCC
        end do
        close(io_unit(0))

!        if (prm%ext .eq. "txt") then
!            open(newunit = io_unit(1), file=trim(output_filename), status="replace", action="write")
!            do i = 1,n
!                read(io_unit(0)) line(i)
!                write(io_unit(1),*) line(i)
!            end do
!            close(io_unit(1))
!        else if (prm%ext .eq. "csv") then
!            open(newunit = io_unit(1), file=trim(output_filename), status="replace", action="write")
!            do i = 1,n
!                read(io_unit(0)) line(i)
!                write(io_unit(1),'(*(G0.6,:,","))') line(i)
!            end do
!            close(io_unit(1))
!        else if (prm%ext .eq. "bin") then
!            open(newunit = io_unit(1), file=trim(output_filename), status="replace", action="write", &
!            form="unformatted", access="stream")
!            do i = 1,n
!                read(io_unit(0)) line(i)
!                write(io_unit(1)) line(i)
!            end do
!            close(io_unit(1))
!        end if

        output_filename = trim(prm%iodir)//"/results/candidates."//prm%ext
        call Output_candidates(prm,line,output_filename)
!        call QuickSort(line)
!        output_filename = trim(prm%iodir)//"/results/candidates_sorted."//prm%ext
!        call Output_candidates(line(n:1:-1),output_filename)

    end subroutine extract_candidates

    subroutine Output_candidates(prm,line,filename)
        type(Parameter), intent(in) :: prm
        type(candidates),intent(inout) :: line(:)
        character(len=*) :: filename
        character(len=3) :: ext
        integer :: i, idx, io_unit
        idx = index(filename,".",back = .true.)
        ext = filename(idx+1:idx+3)
        if (maxval(line%NCC) > 1.0001) then
            i = maxloc(line%NCC,1)
            print*, "NCC = ",maxval(line%NCC), " at ",i
        else if (minval(line%TemplateNo) == 0) then
            i = maxloc(line%TemplateNo,1)
            print*, "TemplateNo = 0 at",i
        else if (ext .eq. "txt") then
            open(newunit = io_unit, file=trim(filename), status="replace", action="write")
            do i=1,size(line)
                write(io_unit,*) line(i)
            end do
        else if (ext .eq. "csv") then
            open(newunit = io_unit, file=trim(filename), status="replace", action="write")
            do i=1,size(line)-1
                if (line(i)%RecordNo /= line(i+1)%RecordNo &
                    .or. abs(line(i)%SampleNo-line(i+1)%SampleNo) >= prm%l_tmp) then
                    write(io_unit,'(1f7.5,",",1G0.6,",",1G0.6,",",1G0.6)') &
                    line(i)%NCC, line(i)%RecordNo, line(i)%SampleNo, line(i)%TemplateNo
                else if (line(i)%NCC >= line(i+1)%NCC) then
                    line(i+1) = line(i)
                else
                end if
            end do
            i=size(line)
            write(io_unit,'(1f7.5,",",1G0.6,",",1G0.6,",",1G0.6)') &
            line(i)%NCC, line(i)%RecordNo, line(i)%SampleNo, line(i)%TemplateNo
        else if (ext .eq. "bin") then
            open(newunit = io_unit, file=trim(filename), status="replace", action="write", &
            form="unformatted", access="stream")
            write(io_unit) line
        end if
        close(io_unit)
    end subroutine Output_candidates

end module m_io