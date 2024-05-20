#!/bin/bash

list=($(locate fftw3.f03 | grep ^/usr))

if [[ ${#list[@]} > 0 ]]; then
    echo ${list[0]%/fftw3.f03}
    exit
fi

list=($(locate fftw3.f03 | grep ^/opt))

if [[ ${#list[@]} > 0 ]]; then
    echo ${list[0]%/fftw3.f03}
    exit
fi

list=($(find /usr /opt -name fftw3.f03 2> /dev/null))

if [[ ${#list[@]} > 0 ]]; then
    echo ${list[0]%/fftw3.f03}
    exit
fi

echo "(fftw3.f03 was not found in /usr or /opt. Search in other directries.)"
