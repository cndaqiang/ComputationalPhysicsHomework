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
### Last-update:  2019-04-26                                                          ###
### Build-time:   2019-03-29                                                          ###
### What it is:   MakeFile                                                            ###
#########################################################################################
# Remove implicit rules (including for Modula-2, which just confuses matters)
.SUFFIXES:
.SUFFIXES: .f .F .o .a  .f90 .F90


FC=mpif90
FFLAGS=-g -O2 -ffree-line-length-none 
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

TEST=Ising.o

OBJS = m_mpi_my.o Ising.o IsingVars.o IsingFuns.o m_Random.o \
       IsingInitEnd.o m_IsingSave.o
        

what:
	@echo 
	@echo Please Read ReadMe
	@echo

test:$(TEST)
	$(FC)  $(FFLAGS) -o test $(OBJS)

run:$(OBJS) $(MAIN)
	$(FC)  $(FFLAGS) -o $@  $^


clean:
	@rm *.o *.mod
	@rm run mytest




Ising.o:m_mpi_my.o IsingVars.o IsingFuns.o m_Random.o IsingInitEnd.o \
        m_IsingSave.o
IsingFuns.o:IsingVars.o m_Random.o
IsingInitEnd.o:IsingVars.o m_mpi_my.o m_Random.o 
m_IsingSave.o :: IsingVars.o
    

