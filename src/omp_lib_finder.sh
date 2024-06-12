#!/bin/bash
path2mod=(`find /opt/intel/oneapi -name omp_lib.mod -exec dirname {} \;`)
echo -I${path2mod[0]} 
