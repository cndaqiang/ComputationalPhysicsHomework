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
!!! Name:         LU_fen_jie_module_mpi_left_down_matrix_root                         !!!
!!! Last-update:  2019-04-05                                                          !!!
!!! Build-time:   2019-04-05                                                          !!!
!!! What it is:   xia_san_jiao_fang_cheng_qiu_jie                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module m_mpi_left_down_matrix_root
        use m_mpi_my
        implicit none
        contains 
        subroutine  mpi_left_down_matrix_root(L,b,x) ! Lx=b
                REAL(kind=8) :: L(:,:),b(:),x(:)
                REAL(kind=8), allocatable  :: u(:),v(:)
                INTEGER :: col,n,j 
                INTEGER :: calculate_node

                n=size(b)
                allocate(u(n))
                allocate(v(np-1)) 

                if ( node .eq. master_node ) then
                        u=b
                        v=0
                else
                        u=0
                endif

                Do col=1,n

                        calculate_node=mod(col-1,np) !Fortran array a start a(1) 
                        !================calculate x(xol) ==========================
                        if ( node .eq. calculate_node ) then
                                
                                !calculate x(col)
                                if ( col > 1 ) call  MPI_RECV(v,np-1,MPI_REAL8,front_node,99,my_COMM,mpi_status,mpi_ierr)
                                x(col)=(u(col)+v(1))/L(col,col)
                                
                                !calcualte v
                                Do j=1,np-2
                                        v(j)=v(j+1)+u(col+j)-L(col+j,col)*x(col) !v(j) -->  u ( i + j )   
                                END Do
                                v(np-1)=u(col+np-1)-L(col+np-1,col)*x(col)  !j=np-1
                                call MPI_SEND(v,np-1,MPI_REAL8,next_node,99,my_COMM,mpi_ierr)

                                !calculate u
                                Do j=col+np,n
                                        u(j)=u(j)-L(j,col)*x(col)
                                end Do
                                
                                !send x
                                call MPI_SEND(x(col),1,MPI_REAL8, master_node,200+col,my_COMM,mpi_status,mpi_ierr)

                        endif 

                        if ( node .eq. master_node )  call MPI_RECV(x(col),1,MPI_REAL8,calculate_node,200+col,my_COMM,mpi_status,mpi_ierr)
                        !============End calculate and send x(xol)=========================      
                       
                 End Do

                deallocate(u,v)
        END subroutine  mpi_left_down_matrix_root
END module m_mpi_left_down_matrix_root
