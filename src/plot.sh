#!/bin/bash

NumPlt=$1 # Number of plots
IOdirname=$2 # I/O directory name
IOdirname=${IOdirname%/*} # Eliminate slash (/) if any

path2figs=${IOdirname}/results/figs
TmpDir=/tmp/DiallelX
rm -r -f ${TmpDir}
mkdir -p ${TmpDir}
mkdir -p ${path2figs}
source ${IOdirname}/parameters/parameters.ini
SortedLog="${IOdirname}/results/candidates_sorted.${Extent}"

for n in `eval echo {1..${NumPlt}}`;do

    if [ ${Extent} == "csv" ]; then
        line=`sed -n ${n}p ${SortedLog}`
        RecNum=$(echo ${line} | cut -d , -f 1)
        SmpNum=$(echo ${line} | cut -d , -f 2)
        TmpNum=$(echo ${line} | cut -d , -f 3)
        NCCval=$(echo ${line} | cut -d , -f 4)
    elif [ ${Extent} == "txt" ]; then
        line=`sed -n ${n}p ${SortedLog}`
        RecNum=$(echo ${line} | cut -d " " -f 1)
        SmpNum=$(echo ${line} | cut -d " " -f 2)
        TmpNum=$(echo ${line} | cut -d " " -f 3)
        NCCval=$(echo ${line} | cut -d " " -f 4)
    elif [ ${Extent} == "bin" ]; then
        str=($(dd if="${SortedLog}" bs=16 skip=$(( n-1 )) count=1 status=none | od -w4 -An -t d4))
        RecNum=${str[0]}
        SmpNum=${str[1]}
        TmpNum=${str[2]}
        str=($(dd if="${SortedLog}" bs=16 skip=$(( n-1 )) count=1 status=none | od -w4 -An -t f4))
        NCCval=${str[3]}
    fi

    echo "# Temporal parameter setting for gnuplot" >  ${TmpDir}/plot_Parameters.ini
    echo "NumChn="${NumChn} >>  ${TmpDir}/plot_Parameters.ini
    echo "LenTmp="${LenTmp} >> ${TmpDir}/plot_Parameters.ini

    echo "# Temporal ChannelID setting for gnuplot" > ${TmpDir}/plot_ChannelID.ini
    echo "array ChannelID[${NumChn}]" >> ${TmpDir}/plot_ChannelID.ini

    echo "# Temporal path setting for gnuplot" > ${TmpDir}/plot_FilePath.ini
    echo "array Cnt[${NumChn}]" >> ${TmpDir}/plot_FilePath.ini
    echo "array Tmp[${NumChn}]" >> ${TmpDir}/plot_FilePath.ini

    line=`sed -n ${RecNum}p ${IOdirname}/parameters/continuous_records.csv`
    for i in `eval echo {1..${NumChn}}`; do
        file=$(echo ${line} | cut -d , -f $i)
        ChannelID=${file%.*}
        ChannelID=${ChannelID#*_}
        echo ChannelID[${i}]=\"${ChannelID}\" >> ${TmpDir}/plot_ChannelID.ini
        Path2Cnt="${IOdirname}/continuous_records/${file}"
        dd if=${Path2Cnt} bs=4 skip=${SmpNum-1} count=${LenTmp-1} of="${TmpDir}/cnt-${i}.bin" >& /dev/null
        echo "Cnt[${i}]="\"${TmpDir}/cnt-${i}.bin\" >> ${TmpDir}/plot_FilePath.ini
    done
    RecordID=${line%%_*}

    NumIdentical=0
    line=`sed -n ${TmpNum}p ${IOdirname}/parameters/templates.csv`
    for i in `eval echo {1..${NumChn}}`; do
        file=$(echo ${line} | cut -d , -f $i)
        Path2Tmp="${IOdirname}/templates/${file}"
        echo "Tmp[${i}]="\"${Path2Tmp}\" >> ${TmpDir}/plot_FilePath.ini
        x=($(md5sum ${TmpDir}/cnt-${i}.bin))
        y=($(md5sum ${Path2Tmp}))
        if [[ ${x[0]} = ${y[0]} ]]; then
            NumIdentical=$(( NumIdentical+1 ))
        fi
    done
    if [ ${NumIdentical} -eq ${NumChn} ]; then
        echo "  "${n}"/"${NumPlt}": Skipping plot of identical waveforms"
        continue
    fi
    TemplateID=${line%%_*}

    echo "NCCval="${NCCval} >> ${TmpDir}/plot_Parameters.ini
    echo "RecNum="${RecNum} >> ${TmpDir}/plot_Parameters.ini
    echo "SmpNum="${SmpNum} >> ${TmpDir}/plot_Parameters.ini
    echo "TmpNum="${TmpNum} >> ${TmpDir}/plot_Parameters.ini
    echo "RcrdID="\"${RecordID}\" >> ${TmpDir}/plot_Parameters.ini
    echo "TmplID="\"${TemplateID}\" >> ${TmpDir}/plot_Parameters.ini

    gnuplot src/plot.gnuplot

    NCCval=$(echo "${NCCval} * 1000" | bc | xargs printf "%3.0f")

    mv ${TmpDir}/out.svg ${path2figs}/${NCCval}_${RecNum}-${SmpNum}_${TmpNum}.svg
    echo "  "${n}"/"${NumPlt}: ${NCCval}_${RecNum}-${SmpNum}_${TmpNum}.svg

done
rm -r -f ${TmpDir}

#svglist=`ls ${path2figs}/*.svg`
#for file in ${svglist};do
#    gzip -f -S z ${file}
#done
