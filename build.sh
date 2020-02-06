gfortran -cpp -c src/*.f90 src/datastructures/*.f90
gfortran *.o -o aoc2019

rm *.o

time ./aoc2019 $1 $2
