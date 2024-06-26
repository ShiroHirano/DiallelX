module m_count_time
    integer,protected :: startTime4Count
    type :: time
        integer :: HH, MM, SS, mSS
    end type time

    contains

    subroutine tic(a,rap)
        character(*),intent(in) :: a
        integer, intent(inout) :: rap
        call system_clock(startTime4Count)
        rap = startTime4Count
        if (len_trim(a) > 0) write(*,*)
        if (len_trim(a) > 0) print '(1a)', trim(a)
        return
    end subroutine tic

    subroutine toc(rap)
        integer, intent(inout) :: rap
        real(8) :: decimalSS
        integer :: finish, rate, tMax, diff
        type(time) :: duration
        call system_clock(finish, rate, tMax)
        if ( finish .lt. startTime4Count ) then
            diff = (tMax - startTime4Count) + finish + 1
        else
            diff = finish - startTime4Count
        endif
        decimalSS = dble(diff)/dble(rate)
        call sec2hhmmss(decimalSS,duration)
        write(*, '("time=",i2.2,":",i2.2,":",i2.2,".",i3.3)',advance='no') &
            duration%HH,duration%MM,duration%SS,duration%mSS
        write(*,'("(+",1es8.2, "sec.)")') real(finish - rap)/real(rate)
        rap = finish
        return
    end subroutine toc

    subroutine sec2hhmmss(decimalSS,duration)
        real(8),intent(in) :: decimalSS
        type(time) :: duration
        duration%HH = int(decimalSS)/3600
        duration%MM = mod(int(decimalSS)/60,60)
        duration%SS = mod(int(decimalSS),60)
        duration%mSS = int(1000*(decimalSS - int(decimalSS)))
        return
    end subroutine sec2hhmmss

end module m_count_time
