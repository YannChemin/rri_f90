ROOTDIR  = $(shell pwd)
OPSYS    = $(shell uname -s)

SRCS_C   = $(shell ls *.f90)
SRCS_C1  = $(shell ls RRI_Mod*.f90)

OBJS_C   = $(SRCS_C:.f90=.o)
OBJS_C1  = $(SRCS_C1:.f90=.o)
TARGET   = 0_rri

ifeq ($(CC), gcc)
CFLAGS = -O3 -fopenmp -Wall -ffree-form -ffree-line-length-none
FFLAGS = -O3 -fopenmp -Wall -ffree-form -ffree-line-length-none
OUTPUTFILE=OUTPUT_my_OpenMP$(FC)
endif
 
ifeq ($(CC), icc)
CFLAGS = -O3 -openmp -Wall -ffree-form -ffree-line-length-none
FFLAGS = -O3 -openmp -Wall -ffree-form -ffree-line-length-none
OUTPUTFILE=OUTPUT_my_OpenMP$(FC)
endif

FC       = gfortran 

all: $(TARGET)

#$(OBJS_C1): $(SRCS_C1)
#	$(FC) $(FFLAGS) -c $(SRCS_C1)

#$(OBJS_C): $(SRCS_C)
#	$(FC) $(FFLAGS) -c $(SRCS_C)

#$(TARGET): $(OBJS_C)
#	$(FC) -o $@ $(FFLAGS) $(OBJS_C)

	gfortran -c -pg -O3 -fopenmp -Wall -ffree-form -ffree-line-length-none RRI_Mod.f90
	gfortran -c -pg -O3 -fopenmp -Wall -ffree-form -ffree-line-length-none RRI_Mod2.f90
	gfortran -c -pg -O3 -fopenmp -Wall -ffree-form -ffree-line-length-none RRI_Mod_Dam.f90
	gfortran -c -pg -O3 -fopenmp -Wall -ffree-form -ffree-line-length-none RRI_Mod_Tecout.f90
	gfortran -c -pg -O3 -fopenmp -ffree-form -ffree-line-length-none RRI.f90 RRI_Dam.f90 RRI_Div.f90 RRI_DT_Check.f90 RRI_Evp.f90 RRI_GW.f90 RRI_Infilt.f90 RRI_Read.f90 RRI_Riv.f90 RRI_RivSlo.f90 RRI_Slope.f90 RRI_Sub.f90 RRI_Tecout.f90 RRI_TSAS.f90 RRI_Bound.f90 RRI_Break.f90
	gfortran -pg -O3 -fopenmp -Wall -ffree-form -ffree-line-length-none -o 0_rri $(OBJS_C)
 

help:	
	@echo " "
	@echo "Operating System Detected: $(OPSYS) "
	@echo " "
	@echo "USAGE: "
	@echo "make help       To get this listing"
	@echo "make            To compile the OpenMP code in current env"
	@echo "make clean      Remove *.o and executable files"
	@echo "make list       List the compilers in current environment"
	@echo "make brun       Submit a batch job"
	@echo " "
 
list:	
	@echo
	@echo "OPSYS:       $(OPSYS)"
	@echo "ROOTDIR:     $(ROOTDIR)"
	@echo "FC Compiler: $(FC)"
	@echo "C  Compiler: $(CC)"
	@echo "CFLAGS:      $(CFLAGS)"
	@echo "FFLAGS:      $(FFLAGS)"
	@echo "OUTPUT FILE: $(OUTPUTFILE)"
	@echo "ARCH:        $(ARCH)"
	@echo " "

clean:  
	@echo "Removing *.o and executable files"
	rm -rf *.o *.mod $(TARGET) OUTPUT_my_OpenMP*
 
brun:   rm -rf $(OUTPUTFILE)
	sqsub -r 1h --mpp=2.0G  -o $(OUTPUTFILE) -f threaded -N 1 -n 16 ./$(TARGET)

