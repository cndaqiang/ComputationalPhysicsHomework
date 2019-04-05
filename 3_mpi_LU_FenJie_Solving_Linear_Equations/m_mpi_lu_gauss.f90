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
!!! Name:         LU_fen_jie_module_mpi_Guass                                         !!!
!!! Last-update:  2019-04-05                                                          !!!
!!! Build-time:   2019-04-05                                                          !!!
!!! What it is:   Bing_Xing_Xiao_Yuan                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module  m_mpi_lu_gauss
        use m_mpi_my
        implicit none
        Private
        public :: mpi_gauss_up , mpi_gauss_down
        contains 

        subroutine mpi_gauss_up(A,b) !Ax=b
                REAL(kind=8) :: A(:,:),b(:)
                REAL(kind=8),allocatable :: Ab(:,:)
                INTEGER :: m
                m=size(b)
                allocate(Ab(m,m+1))
                Ab(:,1:m)=A
                Ab(:,m+1)=b
                call mpi_up_any_matrix(Ab)
                
                if ( node .eq. master_node ) then
                        A(:,1:m)=Ab(:,1:m)
                        b=Ab(:,m+1)
                endif

                deallocate(Ab)
        end subroutine mpi_gauss_up

        subroutine mpi_gauss_down(A,b)
                REAL(kind=8) :: A(:,:),b(:)
                REAL(kind=8),allocatable :: A_tmp(:,:),b_tmp(:)
                INTEGER :: m,j
                
                m=size(b)
                allocate(A_tmp(m,m),b_tmp(m))
                
                ! A(1,2,3)->A_tmp(1 2 3)=A(3,2,1) huan lie
                do j=1,m
                A_tmp(:,j)=A(:,m-j+1)
                END do
                call mpi_gauss_up(A_tmp,b)
                
                if ( node .eq. master_node ) then
                        do j=1,m
                                A(:,m-j+1)=A_tmp(:,j)
                        END do      
                        A_tmp=A
                        b_tmp=b
                        !A(1;2;3)=A_tmp(3;2;1) huan hang
                        do j=1,m
                        A(j,:)=A_tmp(m-j+1,:)
                        b(j)=b_tmp(m-j+1)
                        END do
                endif

                deallocate(A_tmp,b_tmp)
        end subroutine mpi_gauss_down

                            
        !This subroutine use guass calculate any matrix's up matrix
        subroutine mpi_up_any_matrix(A)
                REAL(kind=8) :: A(:,:),tmpA
                INTEGER :: m,n,order,j
                INTEGER :: col,l ! A(l,col)=max(A(:,col))
                INTEGER :: calculate_node
                REAL(kind=8) :: f(size(A,dim=1)) !f=a(i,col)/a(col,col) 
                !REAL,allocatable :: f(:)

                m=size(A,dim=1)
                n=size(A,dim=2)
                if ( m > n) then
                        order = n
                else
                        order = m
                endif


                
                DO col=1,order-1
                        calculate_node =  mod(col-1,np) !the node find max and BCAST

                        !exchange max
                        if (node .eq. calculate_node ) l=maxloc(abs(A(col:m,col)),dim=1)+col-1
                        call MPI_BCAST(l,1,MPI_INTEGER,calculate_node,my_COMM,mpi_ierr)
                        if ( l .ne. col ) then
                                Do j=col,n
                                        if ( node .eq. mod(j-1,np) ) then
                                                tmpA=A(col,j)
                                                A(col,j)=A(l,j)
                                                A(l,j)=tmpA
                                        END if
                                END  DO
                        END if
                        

                        !calculate xi shu f=a(i,col)/a(col,col) i=col+1,n
                        if ( node .eq. calculate_node )  f(col+1:m)=A(col+1:m,col)/A(col,col)
                        
                        call MPI_BCAST(f,size(f,dim=1),MPI_REAL8,calculate_node,my_COMM,mpi_ierr)

                        !Gauss xiao yuan
                        Do j=col,n
                                if ( node .eq. mod(j-1,np) ) A(col+1:m,j)=A(col+1:m,j)-f(col+1:m)*A(col,j)
                        END Do

                

                        !Send result
                        !Next command  error, need fix
                        !call MPI_SENDRECV(A(:,col),m,MPI_REAL,master_node,99,A(:,col),m,MPI_REAL,calculate_node,99,my_COMM,mpi_status,mpi_ierr)
                        !repalce sendrecv by this command
                        if ( node .eq. calculate_node )  call MPI_SEND(A(:,col),m,MPI_REAL8,master_node,99+col,my_COMM,mpi_ierr)
                        if ( node .eq. master_node )  call MPI_RECV(A(:,col),m,MPI_REAL8, calculate_node,99+col,my_COMM,mpi_status,mpi_ierr)
                              
                        END DO


                        Do col=order,n
                                calculate_node =  mod(col-1,np) !the node claculate
                                if ( node .eq. calculate_node )  call MPI_SEND(A(:,col),m,MPI_REAL8,master_node,99+col,my_COMM,mpi_ierr)
                                if ( node .eq. master_node )  call MPI_RECV(A(:,col),m,MPI_REAL8, calculate_node,99+col,my_COMM,mpi_status,mpi_ierr)
                        END Do
          
        End subroutine mpi_up_any_matrix


end  module m_mpi_lu_gauss
                      

