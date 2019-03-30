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
!!! Name:         find_root_main                                                      !!!
!!! Last-update:  2019-03-23                                                          !!!
!!! Build-time:   2019-03-23                                                          !!!
!!! What it is:   main program for finding root                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program find_root_main
    use m_rootfunction 
    implicit none
    !varibles for bisection method 
    real :: a,b,c,fa,fb,fc !find root in [a,b], c=1/2(a+b), fx=f(x)
    real :: xdel,ydel ! if |fc|<ydel ,c is root; if |a-b|>xdel, No root 
    
    !varibles for Newton method
    real ::nx,nx0,nfx,nf1x !n is prefix of Newton, 1 is 1 jie han shu zhi
                           !nx0 is x0, nx is x1=x0-f(x0)/f'(x0)
    integer :: ni,nmax     !max times of cycle

    !logical 
    logical :: findroot ! if findroot it will be 1
   
                                              

!=======init ==============================================
    call functionname()
    xdel=1e-4
    ydel=1e-4
    
    a=1
    b=1.8
    c=0.5*(a+b)
    fa=FindYourRoot(a)
    fb=FindYourRoot(b)
    fc=FindYourRoot(c)

    nx=5.0
    nfx=FindYourRoot(nx)
    nf1x=FindYourRoot1(nx)
    ni=1      ! cycle
    nmax=1000 ! max times of cycle
!=======End init ==========================================

    write(*,*) "=========================================="
    
!=======Bisection  Method==================================
    write(*,*) "Bisection Method "
    findroot=.true. 
    Do while ( abs(fc) > ydel )
        !Stop when |a-b| < xdel
        if ( abs (a-b) < xdel ) then
            write(*,*) "Sorry, Not find root"
            findroot=.false.
            exit
        end if

        if ( fa*fc > 0 ) then
            a=c
            fa=fc
        else 
            b=c 
            fb=fc
        end if
            c=0.5*(a+b)
            fc=FindYourRoot(c)       
    End do

    if (findroot) then
        write(*,*) "The root of the function is:",c
    end if
!=======End Bisection  Method==============================
    write(*,*) "=========================================="
!=======Newton Method======================================

    write(*,*) "Newton Method"
    findroot=.true.
    Do while ( abs(nfx) > ydel )
        if ( ni > nmax ) then
            write(*,*) "Sorry, Not find root"
            findroot=.false.
            exit
        end if

        nx0=nx
        nx=nx0-nfx/nf1x
        nfx=FindYourRoot(nx)
        nf1x=FindYourRoot1(nx)
        ni=ni+1
    end DO

    if (findroot) then
        write(*,*) "The root of the function is:",nx
    end if 


    write(*,*) "=========================================="
!=======Scater Method======================================

    write(*,*) "Newton Method"
    findroot=.true.



end program find_root_main

