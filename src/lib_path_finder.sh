#!/bin/bash

ErrorMessage="not found in /usr, /opt, or /lib. Install FFTW3 as in readme.md, or modify src/lib_path_finder.sh if the FFTW3 library is installed in another directory."

OS=$(echo $(uname -s))

if [ "${OS}" == 'Linux' ]; then

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
    else
        echo "Error: libfftw3f.so "${ErrorMessage}
        exit
    fi

elif [ "${OS}" == 'Darwin' ]; then

    filepath=($(find /usr /opt /lib -name libfftw3f.a 2> /dev/null))
    if [ -f ${filepath[0]} ]; then
        echo -L$(dirname ${filepath})
        exit
    else
        echo "Error: libfftw3f.a "${ErrorMessage}
        exit
    fi

else

    echo "Error: OS ${OS} is not supported. DiallelX is only for Linux or mac."
    exit

fi
