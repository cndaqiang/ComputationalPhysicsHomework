program mytest
      use m_mpi_my
      use m_mpi_lu_gauss
      use m_mpi_left_down_matrix_root
      REAL(kind=8) :: Ab(4,5),A(4,4),b(4),A2(4,4),b2(4)
      REAL(kind=8) :: x(4)
      call mpi_start()
      data((Ab(i,j),i=1,4),j=1,5) /2,6,10,14,3,7,11,15,4,-8,12,16,-5,9,13,17,-6,96,312,416/

      A=Ab(:,1:4)
      b=Ab(:,5)
      A2=A
      b2=b

      if (node .eq. master_node ) then
        write(*,*) "A b"
        do i=1,4
          write(*,*) A(i,:),b(i)
        end do
      end if

      call mpi_gauss_up(A,b)

      if (node .eq. master_node ) then
        write(*,*) "UP A b"
        do i=1,4
          write(*,*) A(i,:),b(i)
        end do
      end if

      if (node .eq. master_node ) then
        write(*,*) "A2 b2"
        do i=1,4
          write(*,*) A2(i,:), b2(i)
        end do
      end if

      call mpi_gauss_down(A2,b2)

      if (node .eq. master_node ) then
        write(*,*) "=========dowm======="
        write(*,*) "DOWN A2 b2 "
        do i=1,4
          write(*,*) A2(i,:), b2(i)
        end do
      end if

      if ( node .eq. master_node ) write(*,*) "======Fine Root========"
      call MPI_BCAST(A2,16,MPI_REAL8,master_node,my_COMM,mpi_ierr)
      call MPI_BCAST(b2,1,MPI_REAL8,master_node,my_COMM,mpi_ierr)
      call mpi_left_down_matrix_root(A2,b2,x)
      if ( node .eq. master_node ) write(*,*) x


      call mpi_end()
end program mytest
