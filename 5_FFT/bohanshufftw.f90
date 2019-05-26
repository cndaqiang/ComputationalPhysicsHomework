!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!   ___                               _            _     _                       _  !!!
!!!  / __|  ___   _ __    _ __   _  _  | |_   __ _  | |_  (_)  ___   _ _    __ _  | | !!!
!!! | (__  / _ \ | '  \  | '_ \ | || | |  _| / _` | |  _| | | / _ \ | ' \  / _` | | | !!!
!!!  \___| \___/ |_|_|_| | .__/  \_,_|  \__| \__,_|  \__| |_| \___/ |_||_| \__,_| |_| !!!
!!!  ___   _             |_|  _                                                       !!!
!!! | _ \ | |_    _  _   ___ (_)  __   ___                                            !!!
!!! |  _/ | ' \  | || | (_-< | | / _| (_-<                                            !!!
!!! |_|   |_||_|  \_, | /__/ |_| \__| /__/                                            !!!
!!!  _  _         |__/                               _                                !!!
!!! | || |  ___   _ __    ___  __ __ __  ___   _ _  | |__                             !!!
!!! | __ | / _ \ | '  \  / -_) \ V  V / / _ \ | '_| | / /                             !!!
!!! |_||_| \___/ |_|_|_| \___|  \_/\_/  \___/ |_|   |_\_\                             !!!
!!!                                                                                   !!!
!!! Author:       cndaqiang                                                           !!!
!!! ContactMe:    https://cndaqiang.github.io                                         !!! 
!!! Name:         bohanshufftw                                                          !!!
!!! Last-update:  2019-05-23                                                          !!!
!!! Build-time:   2019-05-23                                                          !!!
!!! What it is:   mpi headfile : mpi_init  mpi_end mpi's everything                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program bohanshufftw
    use,intrinsic :: iso_c_binding
    implicit none 
    include 'fftw3.f03'
    INTEGER,PARAMETER :: N=1000
    INTEGER :: i
    !波函数f(x)=c1*exp(i*p1*x)+c2*exp(i*p2*x)
    REAL,PARAMETER :: pi=3.1415926
    REAL(C_DOUBLE)::x(N),k(N),dx=0.01,p1,c1,p2,c2
    complex(C_DOUBLE_COMPLEX) :: fx(N),fk(N),ffx(N)!fx-FFT->fk-IFFT->ffx 
    REAL(C_DOUBLE) :: realf,imagef
    type(C_PTR) :: planfft,planifft
    complex(C_DOUBLE_COMPLEX),dimension(N) :: in,out
    p1=5
    c1=sqrt(2.0/3.0)
    p2=10
    c2=sqrt(1.0/3.0)
    DO i = 1,N
        x(i)=(i-1.0)*dx
        realf=c1*cos(p1*x(i))+c2*cos(p2*x(i))
        imagef=c1*sin(p1*x(i))+c2*sin(p2*x(i))
        fx(i)=cmplx(realf,imagef)
        k(i)=2*pi*(i-1.0)/N/dx
    END DO
    
    !数据结构类型
    !FFTW plans are type(C_PTR). 
    !Other C types are mapped in the obvious way via the iso_c_binding standard:
    !int turns into integer(C_INT), 
    !fftw_complex turns into complex(C_DOUBLE_COMPLEX), 
    !double turns into real(C_DOUBLE), and so on. 
    !See Section 7.3 [FFTW Fortran type reference], page 80. 
!-------------------
    !实数
    !REAL: real(C_DOUBLE), real(C_FLOAT), and real(C_LONG_DOUBLE)
!-------------------
    !复数
    !fftw_complex, fftwf_complex, and fftwl_complex 
    !complex(C_DOUBLE_COMPLEX), complex(C_FLOAT_COMPLEX), and complex(C_LONG_DOUBLE_COMPLEX)
!------fftw编译类型及调用方法
    !the FFTW subroutines and types are prefixed with 
    !‘fftw_’, fftwf_, and fftwl_ for the different precisions, 
    !and link to different libraries (-lfftw3, -lfftw3f, and -lfftw3l on Unix)
    !libfftw3.a这个应该是double的意思
    !use the same include file fftw3.f03 
    !and the same constants (all of which begin with ‘FFTW_’)
    !编译时，指定fftw时指定参数 --enable-float --enable-long-double 默认是double, 
    !--enable-avx等等是指令集 FFTW 支持 SSE、SSE2、 Altivec 和 MIPS 指令集
    !一次只能编译成一种类型
    !这就是超算上fftw有多种版本的原因吧
    !2.1.5-double  3.3.4-double-avx        3.3.4-icc-float   3.3.4-single-avx       3.3.5-double
    !3.3.4         3.3.4-double-avx-sse2   3.3.4-icc-single  3.3.4-single-avx-sse2  3.4.4
    !3.3.4-centos  3.3.4-double-fma-icc15  3.3.4-MPI         3.3.4-SSE2             mkl-14
    !3.3.4-double  3.3.4-gcc               3.3.4-MPICH2.1.5  3.3.5
    !用double就行了
    !将所有以小写"fftw_"开头的名字替换为"fftwf_"（float版本）或"fftwl_"（long double版本）。
    !比如将fftw_complex替换为fftwf_complex，将fftw_execute替换为fftwf_execute等。
    !所有以大写"FFTW_"开头的名字不变
!-------------------
    !整数
    !The C integer types int and unsigned (used for planner flags) 
    !become integer(C_ INT).
!-------------------    
    !数组
    !Numeric array pointer arguments (e.g. double *) become dimension(*),
!-------------------
!===========================================================
!------------------------
    !建立网格点
    !plan = fftw_plan_dft_2d(1000,1024,in,out,FFTW_FO RWARD,FFTW_ESTIMATE)
    !planr2c = fftw_plan_dft_r2c_1d(N,ytr,fftyc,FFTW_ESTIMATE)
    !plan = fftw_plan_dft_1d(N,ytc,ffty,FFTW_FORWARD,FFTW_ESTIMATE)
    !planc2r = fftw_plan_dft_c2r_1d(N,ytc2r,fftyc2r,FFTW_ESTIMATE)
    !其他类型的网格点，如 fftw_plan_dft_r2c_3d
    planfft=fftw_plan_dft_1d(N,in,out,FFTW_FORWARD,FFTW_ESTIMATE)
    planifft=fftw_plan_dft_1d(N,in,out,FFTW_BACKWARD,FFTW_ESTIMATE)
    !dft,dft_r2c,dft_c2r,r2r,解释见下,注r2r没有dft
    !维度1d,2d,3d
    !fftw_plan_dft_2d(N1,N2,in,out,FFTW_FORWARD,FFTW_ESTIMATE)
    !网格点N1*N2,输入输出矩阵in,out
    !sign表示是做DFT变换 FFTW_FORWARD == -1 *exp(-fn*x)
    !还是逆FFT变换 IDFT  FFTW_BACKWARD == +1 *exp(+fn*x)
    !优化方案
    !flags是策略生成方案。一般情况下为FFTW MEASURE或FFTW 
    !FFTW_MEASURE表示FFTW会先计算一些FFT并测量所用的时间，以便为大小为n的变换寻找最优的计算方法。 
    !FFTW_ESTIMATE则相反，它直接构造一个合理的但可能是次最优的方案。
    
    
!-----------------------
    !执行计算
    !call fftw_EXECUTE_dft_r2c(plan,y,ffty)
    !call fftw_EXECUTE_dft(plan,ytc,ffty)
    !call fftw_execute_dft_r2c(planr2c,ytr,fftyc)
    !call fftw_EXECUTE_dft_c2r(planc2r,ytc2r,fftyc2r)
    ! planc2r = fftw_plan_dft_c2r_1d(N,ytc,fftyc2r,FFTW_ESTIMATE)
    call fftw_EXECUTE_dft(planfft,fx,fk)
    fk=fk/N
    call fftw_EXECUTE_dft(planifft,fk,ffx)
!call fftw_execute(plan)也可以计算，但Fortran优化后经常出错，不要用
!计算网格命令要和创建网格命令匹配    
!You must use the correct type of execute function,
! matching the way the plan was created. 
!默认c2c即不写
!Complex DFT plans should use fftw_execute_dft, 
!input实数(REAL)->output复数(COMPLEX): r2c
!Real-input (r2c) DFT plans should use use fftw_execute_dft_r2c
!复数->实数：c2r
!and real-output (c2r) DFT plans should use fftw_execute_dft_c2r. 
!实数—>实数：r2r
!The various r2r plans should use fftw_execute_r2r   
    
!-----------------------    
    


    write(*,*) "                    x                   abs_fx                        k                     abs_fk                        x                  abs_iffk" 
    !"x\t","abs(fx)","k","abs(fk)","x","abs(iffk)"
 
    DO i = 1,N
        write(*,*) x(i),abs(fx(i)),k(i),abs(fk(i)),x(i),abs(ffx(i))
    END DO
    !
    !Do i = N-5,N
    !   write(*,*) fftx(i),fftyc2r(i)/N 
    !END DO
    !
    !write(*,*) "------------------------"
    !DO i = 1,6
    !    write(*,*) fftx(i),ffty(i)/N
    !END DO
    !
    !Do i = N-5,N
    !   write(*,*) fftx(i),ffty(i)/N 
    !END DO
    !释放网格点
    !call fftw_destroy_plan(planr2c)
    !call fftw_destroy_plan(plan)
    call fftw_destroy_plan(planfft)
    call fftw_destroy_plan(planifft)
    !

    


end program bohanshufftw
