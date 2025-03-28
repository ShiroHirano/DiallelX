module m_synthetic
    use m_global_vars, only: kd
    use m_FFTW
    implicit none
    integer,parameter :: fs= 100
    integer,parameter :: NumTmp = 1000
    integer,parameter :: NumRec = 4
    integer,parameter :: LenRec = fs*60*60*24
    integer,parameter :: NumSt = 4
    integer,parameter :: NumCh = 3
    integer,parameter :: LenTmp = 2**10
    integer,parameter :: ExtLenTmp = 4*LenTmp
    type :: RandRange
        real(kd) :: max, min
    end type RandRange
    type(RandRange),parameter :: SNR = RandRange(min=1e-1, max=16e0)
    type(RandRange),parameter :: fc4Noise = RandRange(min=0.5e0, max=2e0)
    type(RandRange),parameter :: fc4Event = RandRange(min=5e0, max=30e0)
    type(RandRange),parameter :: HypoDist = RandRange(min=5e0, max=25e0)
    type(RandRange),parameter :: SeisVelo = RandRange(min=3.6e0, max=6.0e0)
    character(len=1),parameter :: comp(4) = ["E","N","U","T"]

    contains

    subroutine synthetic(iodir)
        implicit none
        character(len = *), intent(in) :: iodir
        real(kd),allocatable :: templ(:,:,:,:)
        real(kd),allocatable :: conti(:)
        integer :: idx(NumTmp,NumRec)
        integer :: i, j, k, day, id, io_unit
        real(kd) :: r(NumTmp,NumRec,2)
        real(kd) :: Amp(NumTmp,NumRec)
        character(len=40) :: filename="continuous_records/YYYYMMDD_StIDxx_C.bin"

        write(*,'(1a)') "DiallelX: synthetic data generator"
        call random_number(r)
        idx = floor(LenRec*r(:,:,1))
        Amp = SNR%min * (SNR%max / SNR%min) ** r(:,:,2)
        open(00,file=iodir//"/"//"synthetics.csv")
        do concurrent(day=1:NumRec, j=1:NumTmp)
            write(00,'(*(G0.6,:,","))') day, idx(j,day)-1, j, Amp(j,day)
        end do
        close(00)
            
        call CalcTemplates(templ,iodir)
        allocate(conti(LenRec*NumRec))

        do i=1,NumSt
            write(filename(33:34),'(1i2.2)') i
            do j=1,NumCh
                write(filename(36:36),'(1a)') comp(j)
                call CalcNoise(conti)
                do day=1,NumRec
                    write(filename(20:27),'(1i8.8)') day
                    do k=1,NumTmp
                        id = (day-1)*LenRec + idx(k,day)
                        conti(id:id+LenTmp-1) = conti(id:id+LenTmp-1) + Amp(k,day)*templ(1:LenTmp,j,i,k)
                    end do
                    open(newunit = io_unit,file=iodir//"/"//filename,form="unformatted",access="stream",status="replace")
                        write(io_unit) conti((day-1)*LenRec+1:day*LenRec)
                    close(io_unit)
                end do
            end do
            write(*,'("  synthetic records   : ",1a)') iodir//"/"//filename(1:19)//"*"//filename(28:35)//"*"//filename(37:40)
        end do
        write(*,'(1a)') "DiallelX: Done"
        write(*,'(1a)')
        write(*,'(1a)') "To calculate NCC among the synthetic data, run:"
        write(*,'("./DiallelX -d ",1a)') iodir
    end subroutine synthetic

    subroutine CalcNoise(u)
        real(kd),intent(out) :: u(:)
        real(kd),allocatable :: t(:), f(:)
        complex(kd),allocatable :: Fu(:)
        real(kd) :: fc
        integer :: n
        call random_number(fc)
!        fc = 2e0**(2e0*fc-1e0)
        fc = fc4Noise%min * (fc4Noise%max/fc4Noise%min)**fc
        n = size(u,dim=1)
        allocate(Fu(n/2+1))
        call GetVars(n,fs,t,f)
        planR2C = fftwf_plan_dft_r2c_1d(n, u, Fu, fftw_estimate)
        planC2R = fftwf_plan_dft_c2r_1d(n, Fu, u, fftw_estimate)
        u = GaussianNoise(n)
        call fftwf_execute_dft_r2c(planR2C, u, Fu)
        Fu = Fu * f / (1e0 + (f/fc)**2) * exp(-(f/(0.3*fs))**12)
        call fftwf_execute_dft_c2r(planC2R, Fu, u)
        u = u / sqrt(sum(u*u)/real(n))
    end subroutine CalcNoise

    subroutine CalcTemplates(templ,iodir)
        real(kd),allocatable :: templ(:,:,:,:)
        character(len = *), intent(in) :: iodir
        real(kd),allocatable :: t(:), f(:), u(:)
        complex(kd),allocatable :: Fu(:)
        real(kd) :: fc, r
        integer :: i, j, k
        character(len=29) :: filename="templates/000000_StIDxx_C.bin"
        call GetVars(ExtLenTmp,fs,t,f)
        allocate(templ(ExtLenTmp,NumCh,NumSt,NumTmp))
        allocate(u(ExtLenTmp), Fu(ExtLenTmp/2+1))
        planR2C = fftwf_plan_dft_r2c_1d(ExtLenTmp, u, Fu, fftw_estimate)
        planC2R = fftwf_plan_dft_c2r_1d(ExtLenTmp, Fu, u, fftw_estimate)
        do i=1,NumTmp
            write(filename(11:16),'(1i6.6)') i
            do j=1,NumSt
                write(filename(22:23),'(1i2.2)') j
                call random_number(fc)
!                fc = 5e0 + 35e0*fc
                fc = fc4Event%min * (fc4Event%max/fc4Event%min)**fc
                call random_number(r)
                r = HypoDist%min + (HypoDist%max - HypoDist%min)*r
                do k=1,NumCh
                    write(filename(25:25),'(1a)') comp(k)
                    u = GaussianNoise(ExtLenTmp)
                    call fftwf_execute_dft_r2c(planR2C, u, Fu)
                    Fu = Fu * f / (1e0 + (f/fc)**2) * exp(-(f/(0.4*fs))**12)
                    call fftwf_execute_dft_c2r(planC2R, Fu, u)
                    u = u * EnvelopePS(t,r,comp(k))
                    u = u / sqrt( sum(u*u) / real(LenTmp) ) / (r/5e0)
                    templ(:,k,j,i) = u
                    open(00,file=iodir//"/"//filename,form="unformatted",access="stream",action="write",status="replace")
                        write(00) u(1:LenTmp)
                    close(00)
                end do
            end do
        end do
        write(*,'(1a)') "  synthetic templates : "//iodir//"/templates/*.bin"
    end subroutine CalcTemplates

    function GaussianNoise(n)
        implicit none
        real(kd),parameter :: pi = acos(-1e0)
        integer,intent(in) :: n
        real(kd),allocatable :: GaussianNoise(:), rand(:,:)
        allocate(rand(n,2))
        call random_number(rand)
        rand = 1e0 - rand ! 0 <= rand < 1, but 0 < rand for the next procedure 
        GaussianNoise = sqrt(-2e0*log(rand(:,1)))*cos(2e0*pi*rand(:,2)) ! Box-Muller's method
    end function GaussianNoise

    function EnvelopePS(t,r,c)
        real(kd),intent(in) :: t(:), r
        real(kd),allocatable :: EnvelopePS(:)
        character(len=1),intent(in) :: c
        real(kd) :: PS_Ratio, Tp, Ts
        Tp = r/SeisVelo%max
        Ts = r/SeisVelo%min
        PS_Ratio = merge(1e0, 5e0, c .eq. "U") ! = c=="U" ? 1e0 : 5e0
        EnvelopePS = Envelope(4e0*(t-Tp)) + PS_Ratio*Envelope(1e1*(t-Ts))
    end function EnvelopePS

    function Envelope(t)
        real(kd),intent(in) :: t(:)
        real(kd),allocatable :: Envelope(:)
        integer :: i
        allocate(Envelope(size(t)))
        do i=1,size(t)
            Envelope(i) = merge(0e0, erf(t(i))**3 / t(i), t(i) .le. 0e0)
        end do
    end function Envelope

    subroutine GetVars(n,fs,t,f)
        integer,intent(in) :: n, fs
        real(kd),intent(out),allocatable :: t(:), f(:)
        real(kd) :: l
        integer :: i
        l = n/real(fs)
        allocate(t(n), f(n/2+1))
        do i=0,n-1
            t(i+1) = real(i)/real(fs)
        end do
        do i=0,n/2
            f(i+1) = real(i)/real(l)
        end do
!        t(:) = [(i,i=0,n-1)]/real(fs)
!        f(:) = [(i,i=0,n/2)]/real(l)
    end subroutine GetVars

end module m_synthetic
