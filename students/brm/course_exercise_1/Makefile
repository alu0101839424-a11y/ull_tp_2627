FC = gfortran
FFLAGS = -Wall -fcheck=all -O2
OMPFLAGS = -fopenmp

OBJS = kinds.o geometry.o particle.o

#target
all: ex1 ex1_omp

ex1: $(OBJS) ex1.f90
	$(FC) $(FFLAGS) -o $@ ex1.f90 $(OBJS)

ex1_omp: $(OBJS) ex1.f90
	$(FC) $(FFLAGS) $(OMPFLAGS) -o $@ ex1.f90 $(OBJS)

#dependencies
kinds.o: kinds.f90
	$(FC) $(FFLAGS) -c $<

geometry.o: geometry.f90 kinds.o
	$(FC) $(FFLAGS) -c $<

particle.o: particle.f90 geometry.o kinds.o
	$(FC) $(FFLAGS) -c $<

#cleanup
clean:
	rm -f *.o *.mod ex1 ex1_omp output.dat