#!/bin/bash
#
ROOT=../../..
PAR=$*
VHDL_STD="-2008"
OPTIMIZATION=
PAR+=" "$OPTIMIZATION" "$VHDL_STD

SRC_LIST="\
common/lib/src/global.vhd \
common/lib/src/dpRam-e.vhd \
common/util/src/clkGenBhv.vhd \
common/util/src/resetGenBhv.vhd \
common/util/src/busMasterPkg.vhd \
common/util/src/busMasterBhv.vhd \
common/util/src/spRamBhv.vhd \
common/util/src/spRamBhv.vhd \
common/util/src/dpRam-bhv-a.vhd \
common/lib/src/addrDecodeRtl.vhd \
common/lib/src/binaryEncoderRtl.vhd \
common/lib/src/cntRtl.vhd \
common/lib/src/edgedetectorRtl.vhd \
common/lib/src/lutFileRtl.vhd \
common/lib/src/synchronizerRtl.vhd \
common/lib/src/registerFileRtl.vhd \
common/hostinterface/src/hostInterfacePkg.vhd \
common/hostinterface/src/dynamicBridgeRtl.vhd \
common/hostinterface/src/irqGenRtl.vhd \
common/hostinterface/src/statusControlRegRtl.vhd \
common/hostinterface/src/hostInterfaceRtl.vhd \
common/hostinterface/src/parallelInterfaceRtl.vhd vhdl \
xilinx/lib/src/dpRam-rtl-a.vhd \
xilinx/library/pcores/axi_hostinterface_vX_YY_Z/hdl/vhdl/axi_hostinterface.vhd \
xilinx/axiwrapper/src/axi_lite_master_wrapperRtl.vhd \
xilinx/axiwrapper/src/axi_lite_slave_wrapperRtl.vhd \
xilinx/axiwrapper/tb/axi_protocol_checker.vhd \
xilinx/axiwrapper/tb/tb_axi_hostinterface_ip.vhd \
"

GEN_LIST=( \
"\
gPcpStim=${ROOT}/common/hostinterface/tb/tbPCPMasterBhv_TB_stim.txt \
gHostStim=${ROOT}/common/hostinterface/tb/tbHostMasterBhv_TB_stim.txt \
" \
)

TOP_LEVEL=tb_axi_hostinterface

CNT=0
for i in "${GEN_LIST[@]}"
do
    chmod +x $ROOT/common/util/sh/msim-sim.sh
    ./$ROOT/common/util/sh/msim-sim.sh $TOP_LEVEL $PAR -s $SRC_LIST -g $i

    RET=$?

    #add cnt value to output dir
    mv _out_$TOP_LEVEL _out_${TOP_LEVEL}_$CNT

    if test $RET -ne 0
    then
        exit 1
    fi
    CNT=$(( CNT + 1 ))
done
