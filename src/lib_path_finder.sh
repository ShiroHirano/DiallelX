#!/bin/bash

which ldconfig > /dev/null
IsAvail=$?
if [ ${IsAvail} = 0 ]; then
    filepath=$(ldconfig -p | grep libfftw3f.so$)
    if [ -f "${filepath#*=> }" ]; then
        dir=$(dirname ${filepath#*=> })
        echo -L${dir}
        exit
    fi
fi

filepath=($(find /usr /opt /lib -name libfftw3f.so 2> /dev/null))
if [ -f ${filepath[0]} ]; then
    echo -L$(dirname ${filepath})
    exit
fi

echo "(libfftw3f.so was not found in /usr, /opt, or /lib. Search in other directries.)"
