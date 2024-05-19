#!/bin/bash

which ifort >& /dev/null
result=$?
if [ ${result} -eq 0 ]; then
    echo "ifort"
    exit
fi

which gfortran >& /dev/null
result=$?
if [ ${result} -eq 0 ]; then
    echo "gfortran"
    exit
fi

which ifx >& /dev/null
result=$?
if [ ${result} -eq 0 ]; then
    echo "ifx"
    exit
fi

echo "ifort, ifx, or gfortran not found"
exit
