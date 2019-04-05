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
### Last-update:  2019-04-05                                                          ###
### Build-time:   2019-03-29                                                          ###
### What it is:   MakeFile                                                            ###
#########################################################################################
# Remove implicit rules (including for Modula-2, which just confuses matters)
.SUFFIXES:
.SUFFIXES: .f .F .o .a  .f90 .F90


FC=mpif90
FCFLAGE=-g -O2 -ffree-line-length-none 


default: run what

MAIN=Solving_Linear_Equations.f90

TEST=mpitest.f90

OBJ=m_mpi_my.o m_mpi_lu_gauss.o m_mpi_left_down_matrix_root.o

m_mpi_my.o : m_mpi_my.f90
	        $(FC) $(FCFLAGE) -c -o $@ $^

m_mpi_lu_gauss.o : m_mpi_lu_gauss.f90 m_mpi_my.o
	        $(FC) $(FCFLAGE) -c -o $@  m_mpi_lu_gauss.f90

m_mpi_left_down_matrix_root.o : m_mpi_left_down_matrix_root.f90 m_mpi_my.o
	$(FC) $(FCFLAGE) -c -o $@ m_mpi_left_down_matrix_root.f90


what:
	@echo 
	@echo Please Read ReadMe
	@echo

mytest:$(OBJ) $(TEST)
	$(FC)  $(FCFLAGE) -o $@  $^

run:$(OBJ) $(MAIN)
	$(FC)  $(FCFLAGE) -o $@  $^


clean:
	@rm *.o *.mod
	@rm run mytest



