FC = gfortran
FFLAGS = -Wall -fcheck=all -O2

OBJS = kinds.o geometry.o particle.o

#target 
ex1: $(OBJS) ex1.f90
	$(FC) $(FFLAGS) -o $@ ex1.f90 $(OBJS)

#dependencies
kinds.o: kinds.f90
	$(FC) $(FFLAGS) -c $<

geometry.o: geometry.f90 kinds.o
	$(FC) $(FFLAGS) -c $<

particle.o: particle.f90 geometry.o kinds.o
	$(FC) $(FFLAGS) -c $<

#cleanup
clean:
	rm -f *.o *.mod ex1 output.dat