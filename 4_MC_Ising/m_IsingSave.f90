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
!!! Name:         IsingSave                                                           !!!
!!! Last-update:  2019-04-26                                                          !!!
!!! Build-time:   2019-03-29                                                          !!!
!!! What it is:   Ising Save Data                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Module m_IsingSave
    USE IsingVars
    
    contains 
    subroutine IsingSave(prefix)
    INTEGER ::i,j
    character(len=*) :: prefix
    character(len=4) :: FileIndex
    character(len=4) :: TIndex
    FileIndex=" "
    write(FileIndex,'(i4)') ITime
    write(Tindex,'(f3.1)') T
    !prefix.T.Name.ITIME.dat
    !Matrix of Mesh trim(adjustl(Tindex))
    open(11, file = prefix // "."//trim(adjustl(Tindex))// '.Mesh.' // trim(adjustl( FileIndex )) // '.dat')
    Do i=1,size(Mesh,dim=1)
        write(11,*) Mesh(i,:)
    END DO
    close( 11 )
    !another type Matrix of Mesh
    open(12, file = prefix // "."//  trim(adjustl(Tindex))// '.MeshCoor.' // trim(adjustl( FileIndex )) // '.dat')
    write(12,*) "M","N","S"
    Do j=1,size(Mesh,dim=2)
        Do i=1,size(Mesh,dim=1)
        write(12,*) i,j,Mesh(i,j)
        END DO
    END DO
    close( 12 ) 
    !detail
    open(13, file = prefix // "."//trim(adjustl(Tindex))// '.Result.' // trim(adjustl( FileIndex )) // '.dat')
    write(13,"(a10,a10,a10,a10,a10,a10)") "T","<M>","<E>","varE","Cv","Accept"
    write(13,"(f10.3,f10.3,f10.3,f10.3,f10.3,f10.3)")  T, aveM,aveE,varE,varE/(T*T),(AcceptMove*1.0_dp)/(finstep*1.0_dp)
    close(13)

    !Energy and M
    !open(14, file = prefix // "."//trim(adjustl(Tindex))// '.EandM.' //trim(adjustl( FileIndex )) // '.dat')
    !write(14,"(a10,a10,a10)") "istep","E(istep)","M(istep)"
    !Do istep=inistep,finstep
    !    write(14,"(i10,f10.2,f10.2)") istep,E(istep),M(istep)
    !END DO
    !close(14)

    END subroutine IsingSave
    
END module m_IsingSave
