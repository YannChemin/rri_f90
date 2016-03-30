ROOTDIR  = $(shell pwd)
OPSYS    = $(shell uname -s)

SRCS_C   = $(shell ls *.f90)

OBJS_C   = $(SRCS_C:.f90=.o)
TARGET   = 0_rri

ifeq ($(CC), gcc)
CFLAGS = -03 -fopenmp -Wall -ffree-form -ffree-line-length-none
FFLAGS = -03 -fopenmp -Wall -ffree-form -ffree-line-length-none
OUTPUTFILE=OUTPUT_my_OpenMP$(FC)
endif
 
ifeq ($(CC), icc)
CFLAGS = -03 -openmp -Wall -ffree-form -ffree-line-length-none
FFLAGS = -03 -openmp -Wall -ffree-form -ffree-line-length-none
OUTPUTFILE=OUTPUT_my_OpenMP$(FC)
endif

FC       = gfortran 

all: $(TARGET)

$(TARGET): $(OBJS_C)
	$(FC) -o $@ $(FFLAGS) $(OBJS_C)
 
$(OBJS_C): $(SRCS_C)
	$(FC) $(FFLAGS) -c $(SRCS_C)

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

