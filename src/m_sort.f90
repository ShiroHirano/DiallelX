module m_sort
    use m_type_Parameter
    use m_global_vars, only: kd
    use m_general_purpose_functions, only: count_lines, count_bytes
    implicit none
    type candidates
        integer :: RecordNo
        integer :: SampleNo
        integer :: TemplateNo
        real(kd) :: NCC
    end type candidates
    contains

    subroutine read(file,line)
        character(len=*),intent(in) :: file
        type(candidates),intent(out),allocatable :: line(:)
        character(len=3) :: ext
        integer :: i, n
        i = index(file,".",back=.true.)
        ext = trim(file(i+1:i+3))
        if (ext == "csv" .or. ext == "txt") then
            n = count_lines(file)
            allocate(line(n))
            open(00,file=file,status="old",action="read")
            do i = 1,n
                read(00,*) line(i)
            end do
        else
            n = count_bytes(file)/(12+kd)
            allocate(line(n))
            open(00,file=file,form="unformatted",access="stream",status="old",action="read")
            do i = 1,n
                read(00) line(i)
            end do
        end if
        close(00)
    end subroutine read

    subroutine swap(a,b)
        type(candidates),intent(inout) :: a,b
        type(candidates) :: tmp
        tmp = a
        a = b
        b = tmp
        return
    end subroutine swap

    subroutine InsertionSort(input)
        type(candidates),intent(inout) :: input(:)
        integer :: i,j,n
        n = size(input)
        do i = 2, n
            do j = i,2,-1
                if( input(j)%NCC .lt. input(j-1)%NCC ) then
                    call swap(input(j),input(j-1))
                else
                    exit
                end if
            end do
        end do
        return
    end subroutine InsertionSort

    recursive subroutine QuickSort(input)
        type(candidates),intent(inout) :: input(:)
        integer :: i, j, n, iPrevious, jPrevious
        integer,parameter :: numPivotCandidates = 5
        integer,parameter :: nThreshold = 32
        real(kd) :: pivot
        type(candidates) :: pivotCandidates(numPivotCandidates)
        n = size(input)
        if (n .le. nThreshold) then
            call InsertionSort(input)
            return
        end if
        pivotCandidates = input(n/numPivotCandidates:n:n/numPivotCandidates)
        call InsertionSort(pivotCandidates)
        pivot = pivotCandidates(numPivotCandidates/2+1)%NCC	!	the pivot is the median of pivotCandidates
        iPrevious = 0
        jPrevious = n+1
        searchBoth: do
            searchLeft: do i = iPrevious+1,n
                if ( (input(i)%NCC .ge. pivot) .or. (i .ge. jPrevious) ) then
                    iPrevious = i
                    exit searchLeft
                end if
            end do searchLeft
            searchRight: do j = jPrevious-1,1,-1
                if ( (input(j)%NCC .le. pivot) .or. (j .le. iPrevious) ) then
                    jPrevious = j
                    exit searchRight
                end if
            end do searchRight
            if ( iPrevious .ge. jPrevious ) exit searchBoth	!	exit after the crossover of searching points
            call swap(input(iPrevious),input(jPrevious))
        end do searchBoth
        call QuickSort(input(1:iPrevious-1))
        call QuickSort(input(iPrevious:n))
        return
    end subroutine QuickSort

end module m_sort
