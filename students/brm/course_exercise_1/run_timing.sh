#!/bin/bash

#compiling the initial conditions
echo "Compiling random_sphere.f90..."
gfortran -Wall -O2 -o random_sphere random_sphere.f90

PARTICLES=(1000 2000 5000 10000)
THREADS=(1 2 4 8)
SEED=1126

# generaing  initial conditions and patch dt_out
echo "Generating sphere inputs..."
for N in "${PARTICLES[@]}"; do
    ./random_sphere $N $SEED > sphere_$N.dat
    
    # the exercise requires dt_out = t_end.  since
    # random_sphere defaults line 2 (dt_out) to 0.01 and line 3 (t_end) to 0.1, we can use
    # an awk command to safely replace line 2 with 0.10000.
    awk 'NR==2 {$0="0.10000"} 1' sphere_$N.dat > temp.dat && mv temp.dat sphere_$N.dat
done
echo "Inputs generated!"
echo ""

# timing study matrix
echo "Starting OpenMP Timing Study..."
echo "=================================================="

for T in "${THREADS[@]}"; do
    export OMP_NUM_THREADS=$T
    echo "Testing with OMP_NUM_THREADS = $T"
    
    for N in "${PARTICLES[@]}"; do
        echo -n "  N = $N particles: "
        
        # run and capture just the "real" elapsed time from the time command
        /usr/bin/time -p ./ex1_omp sphere_$N.dat 2>&1 | grep real
    done
    echo "--------------------------------------------------"
done

echo "Timing study complete!"