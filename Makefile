## MPIF90 SETTINGS
FC=mpif90
FFLAGS=-g

## FTN SETTINGS
#FC=ftn
#FFLAGS = -O3

#BG/P IBM compiler
#FC=mpixlf90
#FFLAGS=-O0 -qarch=450 -qtune=450


## Set ADIOS_DIR here or before doing make
override ADIOS_DIR:=/opt/adios/1.6.0
override ADIOS_INC:=` ${ADIOS_DIR}/bin/adios_config -c -f`
override ADIOS_FLIB:=`${ADIOS_DIR}/bin/adios_config -l -f`
override GPP = ${ADIOS_DIR}/bin/gpp.py

default: test_core_profile
all: default

ids_types.mod : ids_types.f90
	${FC} -g -c ids_types.f90  $<

ids_functions.mod : ids_functions.f90
	${FC} -g -c ids_functions.f90  $<

test_core_profile : ids_types.mod ids_functions.mod
	${FC} -g -o test_core_profile -I. ${ADIOS_INC} test_core_profile.f90 ids_functions.o ${ADIOS_FLIB} 

clean:
	rm -f *.o *.mod *.fh core.* 
	rm -f test_core_profile
        
distclean: clean
	rm -f *.bp
