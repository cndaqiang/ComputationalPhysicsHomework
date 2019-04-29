You should use mpif90
## Change parameter
Main parameter
```
vi IsingInitEnd.f90
```
Temperature
```
vi Ising.f90
```
## run
```
make clean
make
mpirun -np N ./test
```

## ReadData
```
./Result.sh
ls Result
```
You can find the data in the dir of `Result`

## MATLAB script to plot S(i,j) in different Temperature
```
plotM.m
```