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
!!! Name:         Ising                                                               !!!
!!! Last-update:  2019-04-26                                                          !!!
!!! Build-time:   2019-04-26                                                          !!!
!!! What it is:   Ising Main                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program Isingmain
    use m_mpi_my
    use IsingVars
    use IsingFuns
    use IsingInitEnd
    use m_IsingSave
    implicit none
!=====Init
    call IsingInit()
    !Down
    DO Itime=1,CalTimes
    if( node .eq. mod(Itime,np) ) then
        T=3.5
        Do while( T > 1.4_dp )
            Do istep=inistep,finstep
                if (istep .eq. 1) then
                    call getE(Mesh,E(istep))
                    call getM(Mesh,M(istep))
                    acceptMove = 0
                else
                    call IsingMc(Mesh,E,DE,M,DM,T,istep,acceptMove)
                ENDIf
            ENDDo
            call getE(Mesh,E(istep))
            call IsingAnalyze(E,M,T,sampleStart,finstep,sampleNum,aveM, aveE, varM, varE,CorrE,tao)
            call IsingSave("down")
            T=T-0.1
        EndDO
    END if
    END DO
    !up
    DO Itime=1,CalTimes
    if( node .eq. mod(Itime,np) ) then
        T=1.6
        Do while( T < 3.6_dp )
            Do istep=inistep,finstep
                if (istep .eq. 1) then
                    call getE(Mesh,E(istep))
                    call getM(Mesh,M(istep))
                    acceptMove = 0
                else
                    call IsingMc(Mesh,E,DE,M,DM,T,istep,acceptMove)
                ENDIf
            ENDDo
            call getE(Mesh,E(istep))
            call IsingAnalyze(E,M,T,sampleStart,finstep,sampleNum,aveM, aveE, varM, varE,CorrE,tao)
            call IsingSave("up")
            T=T+0.1
        EndDO
    END if
    END DO
    
    
    
!=====END
    call IsingEnd()
end
