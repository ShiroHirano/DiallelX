#!/bin/bash

# Argument as compiler must be ifort, ifx, or gfortran.
FC = $(compiler)

ifeq ($(FC), )
	FC      = $(shell ./compiler_finder.sh)
endif

ifeq ($(FC),ifort)
	FFLAGS  = -diag-disable=10448 -diag-disable=8291 -O3 -ipo -arch AVX2 -qopenmp
	LIBS    = -qmkl
	INCLUDE = -I/opt/intel/oneapi/2024.1/opt/compiler/include/intel64
else ifeq ($(FC),ifx)
	FFLAGS  = -diag-disable=8291 -O3 -ipo -xHost -qopenmp
	LIBS    = -qmkl
else ifeq ($(FC),gfortran)
	FFLAGS  = -Ofast -march=native -fopenmp
	LIBS    = $(shell ./lib_path_finder.sh)
	LIBS   += -lfftw3f
	INCLUDE = -I$(shell ./fftw3_path_finder.sh)
endif

PROGRAM = ../bin/DiallelX.o
OBJS = m_count_time.o \
       m_FFTW.o \
       m_type_Parameter.o \
       m_global_vars.o \
       m_general_purpose_functions.o \
       m_type_Similarity.o \
       m_sort.o \
       m_io.o \
	   m_synthetic.o \
       main.o

$(PROGRAM): $(OBJS)
	mkdir ../bin 2> /dev/null
	$(FC) -I./ $^ ${INCLUDE} ${LDFLAGS} ${LIBS} ${FFLAGS} -o ${PROGRAM}
	mv -f *.mod *.o ../bin

.SUFFIXES:
.SUFFIXES: .f90 .o

.f90.o:
	$(FC) ${INCLUDE} ${LIBS} ${FFLAGS} -c $<

clean:
	rm -fr *.o *.mod ../bin
