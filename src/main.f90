program main
!$ use omp_lib
    use m_FFTW
    use m_count_time, only: tic, toc
    use m_type_Parameter
    use m_type_Similarity
    use m_global_vars, only: kd, threshold, n_bin, skip, prm
    use m_general_purpose_functions, only: normalize
    use m_io
    use m_synthetic
    implicit none
    type(Similarity), allocatable :: NCC(:)
    complex(kd), allocatable :: Ftmp(:, :, :), Fcnt(:, :, :)
    character(len = 256), allocatable :: conti_file(:, :)
    integer(8) :: histogram(n_bin) = 0
    integer :: i, rap
    character(len = 20) :: mode
    character(len = 256) :: iodir
    character(len = 3) :: extention

    call getarg(1,mode)
    call getarg(2,iodir)
    call getarg(3,extention)

    if (mode == "DummyDataSynthesizer") then
        print*, trim(iodir)
        call synthetic(trim(iodir))
        stop
    end if

    call read_parameters(trim(iodir),extention,prm) ! program stops here if -p option is specified.

    if (mode == "ParameterCheckerMode") stop

    call FFT_templ(prm, Ftmp)

    call read_filelist(trim(iodir)//"/parameters/continuous_records.csv", prm, conti_file)

    call initialize_similarity(prm,NCC)

    call tic("DiallelX: Main loop started", rap)

    do i = 1, prm%n_rec

        call progress_reports(i,conti_file(i,1),prm%n_rec)

        call FFT_conti(prm, conti_file(i:i+1, :), Fcnt)

        call calculate_NCC(prm, Ftmp, Fcnt, NCC, histogram)
        
        call confirm_lag(prm, conti_file(i:i+1, :), i, NCC)

        call toc(rap)
    end do

    print '(1a)', "DiallelX: Post processing"

    call extract_candidates(prm)

    call output(histogram)

stop
contains

    subroutine FFT_templ(prm, Ftmp)
        type(Parameter), intent(in) :: prm
        complex(kd), intent(out), allocatable :: Ftmp(:, :, :)
        real(kd), allocatable :: tmp(:, :, :)
        complex(kd), allocatable :: Fu(:)
        real(kd), allocatable :: u(:)
        integer :: i, j, n
        n = prm%l_tmp
        call read_templ(prm, tmp)
        allocate(Ftmp(n/2+1, prm%n_chn, prm%n_tmp))
        allocate(u(n), Fu(n/2+1))
        planR2C = fftwf_plan_dft_r2c_1d(n, u, Fu, fftw_estimate)
        !$omp parallel do private(j, u, Fu) schedule(dynamic)
        do i = 1, prm%n_tmp
            do j = 1, prm%n_chn
                u = normalize(tmp(:, j, i))/sqrt(real(n*prm%n_chn))
                call fftwf_execute_dft_r2c(planR2C, u, Fu)
                Ftmp(:, j, i) = conjg(Fu)
            end do
        end do
        !$omp end parallel do
        Ftmp(1, :, :) = 0e0
        deallocate(tmp)
    end subroutine FFT_templ

    subroutine FFT_conti(prm, filename, Fcnt)
        type(Parameter), intent(in) :: prm
        character(len = 256), intent(in) :: filename(:, :)
        complex(kd), intent(out), allocatable :: Fcnt(:, :, :)
        real(kd), allocatable :: cnt(:, :)
        real(kd), allocatable :: u(:)
        complex(kd), allocatable :: Fu(:)
        integer :: c, i, j, l, st, w
        l = prm%l_rec
        w = prm%l_tmp
        c = prm%n_chn
        allocate(cnt(l+prm%l_pad, c))
        allocate(u(w))
        allocate(Fu(w/2+1))
        allocate(Fcnt(w/2+1, c, prm%n_win))
        call read_conti(prm, filename(1, :), cnt(:l, :))
        call read_conti(prm, filename(2, :), cnt(l+1:, :)) ! reading initial part of the next data into the padding region
        planR2C = fftwf_plan_dft_r2c_1d(w, u, Fu, fftw_estimate)
        !$omp parallel do private(j, st, u, Fu) schedule(dynamic)
        do i = 1, prm%n_win
            do j = 1, prm%n_chn
                st = (i-1)*prm%l_str+1
                u = cnt(st:st+w-1, j)
                u = normalize(u)/sqrt(real(c*w))
                call fftwf_execute_dft_r2c(planR2C, u, Fu)
                Fcnt(:, j, i) = Fu
            end do
        end do
        !$omp end parallel do
        Fcnt(1, :, :) = 0e0
    end subroutine FFT_conti

    subroutine calculate_NCC(prm, Ftmp, Fcnt, NCC, histogram)
        type(Parameter), intent(in) :: prm
        complex(kd), intent(in) :: Ftmp(:, :, :), Fcnt(:, :, :)
        type(Similarity), intent(inout) :: NCC(:)
        integer(8), intent(inout) :: histogram(:)
        complex(kd), allocatable :: Fu(:)
        real(kd), allocatable :: u(:)
        integer :: i, j, k, l, ii, jj, idx
        real(kd) :: peak, current
        allocate(Fu(prm%l_tmp/2+1), u(prm%l_tmp))
        NCC%val = -1e1
        planC2R = fftwf_plan_dft_c2r_1d(prm%l_tmp, Fu, u, fftw_estimate)
        !$omp parallel do collapse(2) reduction(+:histogram) private(jj, i, j, k, Fu, u, l, peak, idx) schedule(dynamic)
        do ii = 1, prm%n_win, skip    ! outer loop for chache blocking technique
            do jj = 1, prm%n_tmp, skip    ! outer loop for chache blocking technique
                do i = ii, min(prm%n_win, ii+skip-1)    ! inner loop for chache blocking technique
                    do j = jj, min(prm%n_tmp, jj+skip-1)    ! inner loop for chache blocking technique
                        Fu = 0e0
                        do k = 1, prm%n_chn ! loop over all channel
                            Fu = Fu + Fcnt(:, k, i)*Ftmp(:, k, j)
                        end do
                        call fftwf_execute_dft_c2r(planC2R, Fu, u)
                        l = maxloc(u, 1)
                        peak = u(l)!min(u(l), 1e0)
                        idx = min(int(peak*n_bin)+1, n_bin)
                        histogram(idx) = histogram(idx) + 1
                        if (peak > NCC(i)%val) NCC(i) = Similarity(lag = l-1, val = peak, tmp = j)
                    end do
                end do
            end do
        end do
        !$omp end parallel do
    end subroutine calculate_NCC

    subroutine confirm_lag(prm, filename, rec_num, NCC)
        type(Parameter), intent(in) :: prm
        character(len = 256), intent(in) :: filename(:, :)
        integer, intent(in) :: rec_num
        type(Similarity), intent(inout) :: NCC(:)
        real(kd), allocatable :: conti(:, :)
        real(kd), allocatable :: tmp(:, :, :)
        real(kd), allocatable :: x(:), y(:), c(:)
        complex(kd), allocatable :: Fx(:), Fy(:)
        integer :: i, j, l, n, st
        call read_templ(prm, tmp)
        l = prm%l_rec
        allocate(conti(l+prm%l_pad, prm%n_chn))
        call read_conti(prm, filename(1, :), conti(:l, :))
        call read_conti(prm, filename(2, :), conti(l+1:, :))
        n = prm%l_tmp
        allocate(x(2*n), y(2*n), c(2*n))
        allocate(Fx(n+1), Fy(n+1))
        planR2C = fftwf_plan_dft_r2c_1d(2*n, x, Fx, fftw_estimate)
        planC2R = fftwf_plan_dft_c2r_1d(2*n, Fx, x, fftw_estimate)
        !$omp parallel do private(c, st, j, x, Fx, y, Fy, l) schedule(dynamic)
        do i = 1, prm%n_win
            if ( (NCC(i)%val < threshold) &
                .or. (NCC(i)%val < NCC(max(1,i-1))%val) &
                .or. (NCC(i)%val < NCC(min(prm%n_win,i+1))%val) ) then
                    NCC(i)%tmp = 0
                    cycle
            end if
            c = 0e0
            st = (i-1)*prm%l_str
            do j = 1, prm%n_chn
                x(:n) = normalize(conti(st+1:st+n, j))
                x(n+1:) = 0e0
                call fftwf_execute_dft_r2c(planR2C, x, Fx)
                y(:n) = 0e0
                y(n+1:) = normalize(tmp(:, j, NCC(i)%tmp))
                call fftwf_execute_dft_r2c(planR2C, y, Fy)
                Fx = Fx*conjg(Fy)
                call fftwf_execute_dft_c2r(planC2R, Fx, x)
                c = c + x
            end do
            l = maxloc(c, 1)
            NCC(i)%lag = st + l - n - 1
!            if (NCC(i)%lag < 0 .or. NCC(i)%lag > prm%l_rec) NCC(i)%tmp = 0
            if (NCC(i)%lag > prm%l_rec) NCC(i)%tmp = 0
        end do
        !$omp end parallel do
        call output_temporal_candidates(NCC, prm, rec_num)
    end subroutine confirm_lag

end program main