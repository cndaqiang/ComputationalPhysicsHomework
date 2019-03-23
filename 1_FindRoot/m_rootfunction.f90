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
!!! Name:         find_root_module_function                                           !!!
!!! Last-update:  2019-03-23                                                          !!!
!!! Build-time:   2019-03-23                                                          !!!
!!! What it is:   the funtion that we fill find it's root                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module m_rootfunction 
      contains
                subroutine functionname()
                        !write functionname in screen
                        write(*,*) "The function is y=exp(x)*ln(x)-x^2"
                end subroutine functionname


                real function FindYourRoot(x)
                        !the formula of the function
                        implicit none
                        real :: x 
                        FindYourRoot=exp(x)*log(x)-x*x 
                end function FindYourRoot

                real function FindYourRoot1(x)
                        !Qiu 1 jie Dao
                        !For Newton method
                        implicit none
                        real :: x 
                        FindYourRoot1=exp(x)*(log(x)+1.0/x)-2.0*x
                end function FindYourRoot1

end module m_rootfunction
                      




