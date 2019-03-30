program test
        integer :: a(5),b(5),c(5)
        do i=1,5
        a(i)=i
        end do

        b=a*2
        c=a+2
        write(*,*) a
        write(*,*) b
        write(*,*) c
        write(*,*) a*b


end program
