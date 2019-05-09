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
!!! Name:         IsingFuns                                                           !!!
!!! Last-update:  2019-04-26                                                          !!!
!!! Build-time:   2019-04-26                                                          !!!
!!! What it is:   Ising Functions                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module IsingFuns
    use IsingVars, only: dp
    implicit none
    REAL(dp) :: beta=1.0_dp !exp(-DE*beta/T)
    contains
    !calculate total energy of Mesh
    subroutine getE(Mesh,E)
        INTEGER :: Mesh(:,:)
        REAL(dp) :: E,SE
        INTEGER :: i,j
        E=0.0_dp
        Do j = 1,size(Mesh,dim=2)
            Do i = 1,size(Mesh,dim=1)
                call getSingE(Mesh,i,j,SE)
                E=E+SE
            end Do
        END DO
        E=0.5_dp*E
    End subroutine getE
    subroutine getM(Mesh,M)
        INTEGER :: Mesh(:,:)
        REAL(dp) :: M
        INTEGER :: i,j
        M=0.0_dp
        Do j = 1,size(Mesh,dim=2)
            Do i = 1,size(Mesh,dim=1)
                M=M+Mesh(i,j)
            end Do
        END DO
    End subroutine getM
    
    !Ising step
    subroutine IsingMc(Mesh,E,DE,M,DM,T,istep,acceptMove)
    INTEGER :: Mesh(:,:)
    REAL(dp) :: E(:),DE,M(:),DM
    REAL(dp) :: Eold,Enew
    REAL(dp) :: T !temperature
    INTEGER :: istep,acceptMove
    REAL(dp) :: RandCoor(2) !
    INTEGER :: MeshCoor(2)
    REAL(dp) :: RandP
    call random_number(RandCoor)
    RandCoor(1)=1.0_dp + RandCoor(1)*abs(size(Mesh,dim=1)-1)
    RandCoor(2)=1.0_dp + RandCoor(2)*abs(size(Mesh,dim=2)-1)
    MeshCoor=NINT(RandCoor)
    call random_number(RandP)
    !If 
    call getSingE(Mesh,MeshCoor(1),MeshCoor(2),Eold)
    !fliper
    Mesh(MeshCoor(1),MeshCoor(2))=-1*Mesh(MeshCoor(1),MeshCoor(2))
    call getSingE(Mesh,MeshCoor(1),MeshCoor(2),Enew)
    DE=Enew-Eold
    if ( exp(-DE*beta/T) > RandP ) then
        acceptMove=acceptMove+1
        DM=2*Mesh(MeshCoor(1),MeshCoor(2))
    else
        Mesh(MeshCoor(1),MeshCoor(2))=-1*Mesh(MeshCoor(1),MeshCoor(2))
        DE=0.0_dp
        DM=0.0_dp
    END if
    E(istep)=E(istep-1)+DE
    M(istep)=M(istep-1)+DM
    end subroutine IsingMc
    
    
    !calculate single point energy of Mesh(M,N) 
    subroutine getSingE(Mesh,M,N,E)
        INTEGER :: Mesh(:,:)
        INTEGER :: M,N
        REAL(dp) :: E
        INTEGER :: Mm,Mp,Nm,Np
        !Mm=M-1,Mp=M+1, mod for Periodic Boundary Condiction
        !0,...N-1,  
        Mm = mod(M-1,size(Mesh,dim=1))
        Mp = mod(M+1,size(Mesh,dim=1))
        Nm = mod(N-1,size(Mesh,dim=2))
        Np = mod(N+1,size(Mesh,dim=2))
        if( Mm .eq. 0 ) Mm=size(Mesh,dim=1)
        if( Nm .eq. 0 ) Nm=size(Mesh,dim=2)
        if( Mp .eq. 0 ) Mp=size(Mesh,dim=1)
        if( Np .eq. 0 ) Np=size(Mesh,dim=2)        
        E  = -1.0_dp*Mesh(M,N)*( Mesh(Mm,N) + Mesh(Mp,N) + Mesh(M,Nm) + Mesh(M,Np) )        
    end subroutine getSingE
    
    subroutine IsingAnalyze(E,M,T,sampleStart,sampleEnd,sampleNum,aveM, aveE, varM, varE,CorrE,tao)
            
        REAL(dp) :: E(:),M(:),T
        INTEGER :: sampleStart,sampleNum,sampleEnd
        REAL(dp) :: aveM, aveE, aveE2,aveM2,varM, varE,CorrE(:)
        INTEGER :: tao(:)
        REAL(dp) :: minE
        REAL(dp) :: SampleP,TotalP
        INTEGER :: Dsample,sample,i
        aveM=0.0_dp
        aveM2=0.0_dp
        aveE=0.0_dp
        aveE2=0.0_dp
        varM=0.0_dp
        varE=0.0_dp
        TotalP=0.0_dp
        minE=minval(E(sampleStart:sampleEnd))
        Dsample=(sampleEnd-sampleStart)/(sampleNum)
        Do i=0,sampleNum-1
            sample=sampleStart+i*Dsample
            SampleP=1.0_dp
            !SampleP=exp(-1.0_dp*(E(sample)-minE)*beta/T)
            TotalP=TotalP+SampleP
            aveE=E(sample)*sampleP+aveE
            aveE2=E(sample)*E(sample)*sampleP+aveE2
            aveM=M(sample)*sampleP +aveM
            aveM2=M(sample)*M(sample)*sampleP+aveM2
        END DO
        DO i=1,size(tao,dim=1)
        CorrE=( SUM(E(sampleStart:sampleEnd-tao(i))*E(sampleStart+tao(i):sampleEnd)) -&
                       SUM(E(sampleStart:sampleEnd-tao(i)))**2    )/ &
                     ( SUM(E(sampleStart:sampleEnd-tao(i))*E(sampleStart:sampleEnd-tao(i))) -&
                     SUM(E(sampleStart:sampleEnd-tao(i)))**2    )
        ENDDO
                     
        aveE=aveE/TotalP
        aveM=aveM/TotalP
        varE=aveE2/TotalP-aveE*aveE
        varM=aveM2/TotalP-aveM*aveM         
    end subroutine IsingAnalyze
    
    
End module
