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
!!! Name:         fourth-order_Runge-Kutta_main                                       !!!
!!! Last-update:  2019-03-29                                                          !!!
!!! Build-time:   2019-03-29                                                          !!!
!!! What it is:   main program for  fourth-order_Runge-Kutta                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program  fourth_order_Runge_Kutta_main
    use m_runge_kutta_function
    implicit none
    !parameter
    REAL(kind=8),allocatable :: y(:,:),t(:),t0 !y is a matrix Y(n,m)
    INTEGER :: n !n is dimension of Y; y1,y2,y3,...yn
    INTEGER :: m !total time=m*delt*pi
    REAL(kind=8) :: delt !t(i+1)-ti 
    INTEGER :: i
    
    !out date file
    INTEGER ::  datafileunit=99
    character(len=20) :: datafilename

    call WhatTheFunIs()    


    !========init==========
    delt=-0.01 !
    n=4 !y1=theta, y2=Omega
    t0=100
    m=2*(t0)/abs(delt)+1 !total time = m*delt
    
    
    allocate(y(n,m),t(m))
    t=0
    y=0
    t(1)=t0 
    y(1,1)=0.9
    y(2,1)=0.1 !00001
    y(3,1)= 0.0001
    y(4,1)=-0.01
    

   

    datafileunit=99
    datafilename="danbai.dat"
    open(unit=datafileunit,file=datafilename)
    !======End init==========


    !=======Main==============
    Do i=1,m-1
        t(i)=t0+(i-1)*delt
        call rungekuta(y(:,i),y(:,i+1),t(i),delt)
        write(datafileunit,*) t(i),y(:,i)
        !if(abs(y(1,i)<yerr)) 
    end Do
    write(datafileunit,*) t(m),y(:,m)
    
    !======End main

    !====End program==========
    deallocate(y,t)
    close(unit=datafileunit)

    write(*,*) "OK, Please look at ", datafilename
end program  fourth_order_Runge_Kutta_main

