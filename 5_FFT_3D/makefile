#########################################################################################
###   ___                               _            _     _                       _  ###
###  / __|  ___   _ __    _ __   _  _  | |_   __ _  | |_  (_)  ___   _ _    __ _  | | ###
### | (__  / _ \ | '  \  | '_ \ | || | |  _| / _` | |  _| | | / _ \ | ' \  / _` | | | ###
###  \___| \___/ |_|_|_| | .__/  \_,_|  \__| \__,_|  \__| |_| \___/ |_||_| \__,_| |_| ###
###  ___   _             |_|  _                                                       ###
### | _ \ | |_    _  _   ___ (_)  __   ___                                            ###
### |  _/ | ' \  | || | (_-< | | / _| (_-<                                            ###
### |_|   |_||_|  \_, | /__/ |_| \__| /__/                                            ###
###  _  _         |__/                               _                                ###
### | || |  ___   _ __    ___  __ __ __  ___   _ _  | |__                             ###
### | __ | / _ \ | '  \  / -_) \ V  V / / _ \ | '_| | / /                             ###
### |_||_| \___/ |_|_|_| \___|  \_/\_/  \___/ |_|   |_\_\                             ###
###                                                                                   ###
### Author:       cndaqiang                                                           ###
### ContactMe:    https://cndaqiang.github.io                                         ### 
### Name:         MakeFile                                                            ###
### Last-update:  2019-05-23                                                          ###
### Build-time:   2019-03-29                                                          ###
### What it is:   MakeFile                                                            ###
#########################################################################################

# Remove implicit rules (including for Modula-2, which just confuses matters)
.SUFFIXES:
.SUFFIXES: .f .F .o .a  .f90 .F90


FC=gfortran 
FFLAGS=-g -O2 -ffree-line-length-none  -I${FFTWDIR}/include
INCFLAGS=
FPPFLAGS=
FPPFLAGS_fixed_F= 
FPPFLAGS_free_F90=
FCFLAGS_fixed_f=
FCFLAGS_free_f90=



.F.o:
	$(FC) -c $(FFLAGS) $(INCFLAGS) $(FPPFLAGS) $(FPPFLAGS_fixed_F)  $< 
.F90.o:
	$(FC) -c $(FFLAGS) $(INCFLAGS) $(FPPFLAGS) $(FPPFLAGS_free_F90) $< 
.f.o:
	$(FC) -c $(FFLAGS) $(INCFLAGS) $(FCFLAGS_fixed_f)  $<
.f90.o:
	$(FC) -c $(FFLAGS) $(INCFLAGS) $(FCFLAGS_free_f90)  $<
    
    
default: test #run what

MAIN=

#TEST=test.o #distribute.o
TEST=bohanshufftw.o

OBJS = bohanshufftw.o #m_mpi_my.o newtest.o  #distribute.o
  
MATHDIR=/mnt/e/work/CODE/soft/OPENMPI_1.10.3-GCC-4.8.5-opensuse/scalapack/lib
BLAS_LIBS=$(MATHDIR)/librefblas.a
LAPACK_LIBS=$(MATHDIR)/libreflapack.a $(MATHDIR)/libtmg.a
BLACS_LIBS=
SCALAPACK_LIBS=$(MATHDIR)/libscalapack.a
FFTWDIR=/home/cndaqiang/soft/OPENMPI_1.10.3-GCC-4.8.5-opensuse/mathlib/fftw-3.3.4
FFTW=$(FFTWDIR)/lib/libfftw3.a
LIBS=$(FFTW) #  $(SCALAPACK_LIBS) $(BLACS_LIBS) $(LAPACK_LIBS) $(BLAS_LIBS)

what:
	@echo 
	@echo Please Read ReadMe
	@echo

run:
	./test > result.dat

test:$(TEST)
	$(FC)  $(FFLAGS) -o test $(OBJS) $(LIBS)
	./test > result.dat
	./plot.r





clean:
	@rm *.o *.mod
	@rm run mytest
    
#distribute.o : m_mpi_my.o
#test.o : m_mpi_my.o




    

