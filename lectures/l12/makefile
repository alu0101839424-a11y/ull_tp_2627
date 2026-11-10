FC = mpifort
FCFLAGS = -Wall -O2
PROGRAMS = hello send_recv any_source exchange bcast trapezoid

all: $(PROGRAMS)

%: %.f90
	$(FC) $(FCFLAGS) -o $@ $<

clean:
	rm -f $(PROGRAMS)

.PHONY: all clean
