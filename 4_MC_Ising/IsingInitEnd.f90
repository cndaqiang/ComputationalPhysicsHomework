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
!!! Name:         IsingInitEnd                                                        !!!
!!! Last-update:  2019-04-26                                                          !!!
!!! Build-time:   2019-04-26                                                          !!!
!!! What it is:   Ising Init End                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module IsingInitEnd
    use IsingVars
    use m_mpi_my
    use m_Random
    contains 
    subroutine IsingInit
    call mpi_start()
    call Init_Random_Seed()
    N=60
    T=0.0
    inistep=1
    finstep=1E6*N
    sampleStart=1E5*N !1E5*N
    sampleNum=10*N
    allocate(Mesh(N,N))
    allocate(E(finstep))
    allocate(M(finstep))
    call MeshInit(Mesh)
    call Init_Random_Seed()
    AcceptMove=0
    CalTimes = 20 !
    
    ENd subroutine IsingInit
    
    subroutine IsingEnd()  
        deallocate(Mesh)
        deallocate(E)
        deallocate(M)
        call mpi_end()
    End subroutine IsingEnd
    
        !init Mesh
    subroutine MeshInit(Mesh)
        INTEGER :: Mesh(:,:)
        Mesh=1
    End subroutine Meshinit
    
End module IsingInitEnd