unset multiplot
unset output
reset

load "/tmp/DiallelX/plot_Parameters.ini"
load "/tmp/DiallelX/plot_ChannelID.ini"
load "/tmp/DiallelX/plot_FilePath.ini"

set term svg noenhanced size 1920,40*(NumChn+2) font "Arial,Helvetica,DejaVu Sans,20"; set output "/tmp/DiallelX/out.svg"

set multiplot layout NumChn+2,1

set tmargin 0
set rmargin 0.5
set bmargin 0
set lmargin 0.5
set border 0

set border lc rgb "#ffffffff"
unset border

set xrange [0:LenTmp]
set xtics nomirror offset screen 1,screen 1
unset ytics
set grid linetype 1 lc  rgb "#666666"

set label sprintf("NCC = %5.3f",NCCval) at graph 1e-5,1.5 left tc rgb "#666666"
set key at graph 0.99,1.5 right center horizontal tc rgb "#666666"
set object rect behind from screen 0,0 to screen 1,1 fc rgb "#aaaaaa" fillstyle solid 1.0

set multiplot next

do for [i=1:NumChn]{
    if (i==NumChn){
        set xtics nomirror offset 0,0.3 tc rgb "#666666"
    }
    stats Cnt[i] binary form="%float32" u 0:1 nooutput
    Mc = STATS_mean_y
    Ac = STATS_stddev_y == 0 ? 0 : 1./STATS_stddev_y
    stats Tmp[i] binary form="%float32" u 0:1 nooutput
    Mt = STATS_mean_y
    At = 1./STATS_stddev_y
    set label ChannelID[i] at graph 1e-5,0.75 tc rgb "#666666"
    plot \
    Tmp[i] binary form="%float32" u 0:(($1-Mt)*At) w l lw 1 lc rgb "#ffffff" ti sprintf("TemplateID=%s",TmplID), \
    Cnt[i] binary form="%float32" u 0:(($1-Mc)*Ac) w l lw 1 lc rgb "#0000aa" ti sprintf("Continuous RecordID=%s(%d−)",RcrdID,SmpNum)
    unset key
    unset label
    unset object
}

unset multiplot
unset output
reset
