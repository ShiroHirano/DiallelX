#!/bin/bash

files2list () {
    cd ./${iodir}/$1
    output_csv="../parameters/${1}.csv"
    output_prm="../parameters/parameters.ini"
    rm -f ${output_csv}
    touch ${output_csv}

    list=($(ls -v))
    ext=${list[0]/*./}

    sac_header_len=632
    rec_len=`cat ${list[0]} | wc -c`
    if [[ ${ext} == "sac" ]]; then
        rec_len=$(( rec_len - sac_header_len ))
    fi
    kind=4 # Numerical precision of input data (i.e., float32)
    rec_len=$(( rec_len/kind ))

    str0=${list[0]/_*/}
    num_records=1
    num_channels=0
    flag="FALSE"

    for file in ${list[@]};do
        str1=${file/_*/}
        if [[ ${str1} != ${str0} ]]; then
            if [[ ${flag} == "FALSE" ]]; then
                flag="TRUE"
            fi
            num_records=$(( num_records+1 ))
            echo >> ${output_csv}
            echo -n ${file}"," >> ${output_csv}
        else
            if [[ ${flag} == "FALSE" ]]; then
                num_channels=$(( num_channels+1 ))
            fi
            echo -n ${file}"," >> ${output_csv}
        fi
        str0=${str1}
    done
    echo >> ${output_csv}

    if [[ $1 == "templates" ]]; then
        stride=$(( rec_len/accuracy ))
        echo "NumChn="${num_channels}" # number of channels" >> ${output_prm}
        echo "LenTmp="${rec_len}" # length of template waveform per file" >> ${output_prm}
        echo "NumTmp="${num_records}" # number of templates" >> ${output_prm}
        echo "Stride="${stride}" # the smaller raises accuracy and computation time" >> ${output_prm}
    elif [[ $1 == "continuous_records" ]]; then
        echo "LenCnt="${rec_len}" # length of continuous record per file" >> ${output_prm}
        echo "NumCnt="${num_records}" # number of continuous records" >> ${output_prm}
    else
        echo 'Invalid argument!! Input "templates" or "continuous_records".'
        exit 0
    fi

    cd ../../
}

accuracy=$1 # (stride)=(rec_len)/(accuracy), hence, the larger value raises accuracy and computation time. Default value is accuracy=2.
iodir=$2

#cd `dirname $0`
mkdir -p ${iodir}/parameters
mkdir -p ${iodir}/results
prm="./${iodir}/parameters/parameters.ini"
rm -f ${prm}
touch ${prm}

files2list continuous_records
files2list templates
