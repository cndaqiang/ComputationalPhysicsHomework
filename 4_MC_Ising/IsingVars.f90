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
!!! Name:         IsingVars                                                           !!!
!!! Last-update:  2019-04-26                                                          !!!
!!! Build-time:   2019-04-26                                                          !!!
!!! What it is:   IsingVars                                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module IsingVars
    implicit none
    INTEGER,PARAMETER :: dp=8 !Precision
    INTEGER :: N !Mesh is NxN
    INTEGER,allocatable :: Mesh(:,:) !Mesh(N,N)
    REAL(dp),allocatable :: M(:), E(:) !M(M,  !M(t2)=M(t1)+DM,E(t2)=E(t1)+DE 
    REAL(dp) :: DM, DE, aveM, aveE, varM, varE,Cv !OUTPUT
    INTEGER :: istep, inistep, finstep,sampleNum,sampleStart
    INTEGER :: AcceptMove
    !DM DM=M(t2)-M(t1)
    !DE DE=E(t2)-E(t1)
    !vaeM average of M
    !vaeE average of E
    !varM=<M^2>-<M>^2
    !varE=<E^2>-<E>^2
    
    REAL(dp) :: T !temperature
    INTEGER :: RandPoint
    INTEGER :: CalTimes !times of calculation
    INTEGER :: ITime
    
    
    
    



end module