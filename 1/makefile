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
### Last-update:  2019-03-23                                                          ###
### Build-time:   2019-03-23                                                          ###
### What it is:   MakeFile                                                            ###
#########################################################################################
# Remove implicit rules (including for Modula-2, which just confuses matters)
.SUFFIXES:
.SUFFIXES: .f .F .o .a  .f90 .F90


FC=gfortran
FCFLAGE=-g -O2

default: what root

MAIN=root.f90
OBJ=m_rootfunction.o

m_rootfunction.o:m_rootfunction.f90
	$(FC) $(FCFLAGE) -c -o m_rootfunction.o m_rootfunction.f90

what:
	@cat ReadMe
	@echo

root:$(OBJ) $(MAIN)
	$(FC)  $(FCFLAGE) -o root  $(OBJ) $(MAIN)
clean:
	@rm *.o *.mod
	@rm root



