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
!!! Name:         Random_Mod                                                          !!!
!!! Last-update:  2019-04-26                                                          !!!
!!! Build-time:   2019-04-26                                                          !!!
!!! What it is:   Set rand sed with Gfortran                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!而在 Fortran90 以后，语法规范引入了两个标准的函数用来产生随机数。
!它们就是 random_seed 和 random_number( 通常这两个函数需配合使用 )
!random_seed 用来获取或设置新的种子。
!不同的编译器，都会有一套“默认”的种子。
!我们可以在一开始获取它，也可以用自己的种子来覆盖它。
!这个函数在整个程序里，通常只调用一次！


!code from http://fcode.cn/guide-96-1.html
!使用方法
!Program www_fcode_cn
!  use m_Random
!  Real :: x(8)
!  call Init_Random_Seed()
!  call random_number(x)
!  write(*,*) x
!End Program www_fcode_cn

!获得a-b之间的随机数
! call random_number(x) !// 随机数种子部分忽略不写
!   a = 1.0   !// 产生从 1.0  
!   b = 100.0 !// 到 100.0 之间的随机数
!   x = abs(b-a) * x + min(a,b)
!   write(*,*) x


Module m_Random
  Implicit None
  !// 使用内部函数获得容纳10位整数的Kind值
  Integer , parameter :: int64 = Selected_Int_Kind( 10 )
contains

  Subroutine Init_Random_Seed()
    integer :: ised , i , pid
    integer(int64) :: t 
    integer , allocatable :: sed(:)
    call random_seed( size = ised ) !// 获得种子大小
    allocate( sed(ised) ) !// 分配种子
    call system_clock(t) !// 获得时间
    pid = getpid() !// 获得处理器ID
    t = ieor(t, int(pid, kind(t))) !// 用 pid 和日期做XOR运算
    do i = 1, ised
        sed(i) = lcg(t) !// 用线性同余计算种子
    end do
    call random_seed( put=sed ) !// 给定种子  
  End Subroutine Init_Random_Seed  
  
  Function lcg(s) !// 线性同余算法 Linear congruential generator
    integer :: lcg
    integer(int64) :: s
    if (s == 0) then
       s = 104729
    else
       s = mod(s, 4294967296_int64)
    end if
    s = mod(s * 279470273_int64, 4294967291_int64)
    lcg = int(mod(s, int(huge(0), int64)), kind(0))
  End Function lcg

End Module m_Random