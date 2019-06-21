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
!!! Name:         fourth-order_Runge-Kutta_module_function                            !!!
!!! Last-update:  2019-03-29                                                          !!!
!!! Build-time:   2019-03-29                                                          !!!
!!! What it is:   formula of y and g                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module m_runge_kutta_function 
        implicit none
        Private
        !parameter in g
        REAL, PARAMETER,public :: pi = 3.1415927
        REAL(kind=8) :: k=0.5,L=0.5 !!actually Omega=w0/pi, 
        public :: WhatTheFunIs
        public :: rungekuta
        contains
                !write detail of g
                !subroutine runge_kutta_init()
                !        INTEGER :: IsHunDun
                !        q=0.5
                !        w0=2.0/3.0
                !        write(*,*) "周期性解请输入1,混沌解请输入非1整数："
                !        read(*,*) IsHunDun
                !        if (IsHunDun == 1)  then
                !                b=0.9
                !        else
                !                b=1.15
                !        endif
                !
                !end subroutine runge_kutta_init 
                subroutine WhatTheFunIs()
                        write(*,*) "fourth-order_Runge-Kutta"
                        write(*,*) "g1 = y2"
                        write(*,*) "g2 = -q*y2 - sin(y1) + b*cos(w0*t)"
                end subroutine WhatTheFunIs

                !y_i:yi,   y_ip1:y(i+1) , t, delt=(t+1)-t
                subroutine rungekuta(y_i,y_ip1,t,delt)
                        REAL(kind=8) :: y_i(:),y_ip1(:),t,delt
                        INTEGER :: n!num of y in rungekuta equation  
                        REAL(kind=8),allocatable :: g(:)
                        REAL(kind=8),allocatable :: c1(:),c2(:),c3(:),c4(:)
           
                        n=size(y_i,dim=1)
                        allocate(g(n),c1(n),c2(n),c3(n),c4(n))
                        !c1
                        call gfun(y_i,t,g)
                        c1=delt*g
                        !c2
                        call gfun(y_i+c1/2.0,t+delt/2.0,g)
                        c2=delt*g
                        !c3
                        call gfun(y_i+c2/2.0,t+delt/2.0,g)
                        c3=delt*g
                        !c4
                        call gfun(y_i+c3,t+delt,g)
                        c4=delt*g
                        !y(i+1)
                        y_ip1=y_i+1/6.0*(c1+2*(c2+c3)+c4)

                        deallocate(g,c1,c2,c3,c4)
                end subroutine rungekuta



                !g1(t+delt)=y2(t)
                !g2(t+delt)=-q*y2(t) - sin(y1(t)) + b*cos(w0*t)  
                subroutine gfun(y,t,g)
                        !g function
                        implicit none
                        REAL(kind=8) :: y(:),t,g(:)
                        g(1)=y(2)
                        g(2)=k*k*(y(3)*y(3)-1+y(1)*y(1))*y(1) !
                        g(3)=y(4)
                        y(4)=y(1)*y(1)*y(3)
                        
                end subroutine gfun


end module m_runge_kutta_function
                      




