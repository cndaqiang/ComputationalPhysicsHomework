program Solving_Linear_Equations
        use m_mpi_my
        use m_mpi_lu_gauss
        use m_mpi_left_down_matrix_root
        implicit none
        REAL(kind=8),allocatable :: A(:,:),b(:),x(:)
        INTEGER :: m,i !tmp no mean
        INTEGER :: inputfile=1,outfile=2
        
        call mpi_start() 
        if ( node .eq. master_node ) then
                open(inputfile,file="input.dat")
                read(inputfile,*)
                read(inputfile,*) m
                allocate(A(m,m),b(m),x(m))
                read(inputfile,*)
                write(*,*) "A b is :"
                Do i=1,m
                        read(inputfile,*) A(i,:),b(i)
                        write(*,*)  A(i,:),b(i)
                END Do
                close(inputfile)
         endif
         
         call MPI_BCAST(m,1,MPI_INTEGER,master_node,my_COMM,mpi_ierr)
         if ( node .ne. master_node )  allocate(A(m,m),b(m),x(m))
         call MPI_BCAST(A,m*m,MPI_REAL8,master_node,my_COMM,mpi_ierr)
         call MPI_BCAST(b,m,MPI_REAL8,master_node,my_COMM,mpi_ierr)

         
         !Ax=b -> Lx=b'
         call mpi_gauss_down(A,b)
         call MPI_BCAST(A,m*m,MPI_REAL8,master_node,my_COMM,mpi_ierr)
         call MPI_BCAST(b,m,MPI_REAL8,master_node,my_COMM,mpi_ierr)

         !calculate x 
         call mpi_left_down_matrix_root(A,b,x)

         !print result
         if ( node .eq. master_node ) then 
                write(*,*) "x is :"
                write(*,*) x
                open(outfile,file="result.dat")
                write(outfile,*) x
                close(outfile)
         endif
                







        call mpi_end()





End program Solving_Linear_Equations
