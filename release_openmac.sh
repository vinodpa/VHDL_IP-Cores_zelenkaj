#!/bin/bash
# ./release_openmac.sh RELEASE_DIR IP_VERSION
# e.g.: $ ./release_openmac.sh release v1_00_a

DIR_RELEASE=$1
IP_VERSION=$2
DIR_DOC=doc/openmac

if [ -z "${DIR_RELEASE}" ];
then
    DIR_RELEASE=release
fi

if [ -z "${IP_VERSION}" ];
then
    IP_VERSION=v1_00_a
fi

DIR_AXI="axi_openmac_${IP_VERSION}"

# create dir structure
echo "create release dir..."
mkdir -p ${DIR_RELEASE}

# generate docs
echo "generate docs..."
pushd $DIR_DOC
./create-this-doc --skip-doxygen
popd

# copy docs
echo "copy docs..."
cp --parents ${DIR_DOC}/images/openmac-overview.png                 ${DIR_RELEASE}
cp --parents ${DIR_DOC}/md/openmac.md                               ${DIR_RELEASE}
cp --parents ${DIR_DOC}/doxyfile                                    ${DIR_RELEASE}
cp --parents ${DIR_DOC}/mainpage.txt                                ${DIR_RELEASE}
cp --parents ${DIR_DOC}/omethlib.pdf                                ${DIR_RELEASE}

# create revision.txt
REV_FILE=${DIR_RELEASE}/${DIR_DOC}/revision.md
echo "Revision {#revision}" > $REV_FILE
echo "========" >> $REV_FILE
echo "" >> $REV_FILE
git log --format="- %s" -- */openmac/* >> $REV_FILE

# copy drivers
echo "copy drivers..."
cp --parents drivers/openmac/omethlib.c                         ${DIR_RELEASE}
cp --parents drivers/openmac/omethlib.h                         ${DIR_RELEASE}
cp --parents drivers/openmac/omethlib_phycfg_generic.c          ${DIR_RELEASE}
cp --parents drivers/openmac/omethlib_phycfg.h                  ${DIR_RELEASE}
cp --parents drivers/openmac/omethlib_target.h                  ${DIR_RELEASE}
cp --parents drivers/openmac/omethlibint.c                      ${DIR_RELEASE}
cp --parents drivers/openmac/omethlibint.h                      ${DIR_RELEASE}

# copy Altera POWERLINK
echo "copy Altera openmac ipcore..."
cp --parents altera/components/sdc/openmacTop-mii.sdc           ${DIR_RELEASE}
cp --parents altera/components/sdc/openmacTop-rmii.sdc          ${DIR_RELEASE}
cp --parents altera/components/img/br.png                       ${DIR_RELEASE}
cp --parents altera/components/openmac_hw.tcl                   ${DIR_RELEASE}
cp --parents altera/components/openmac_sw.tcl                   ${DIR_RELEASE}
cp --parents altera/components/tcl/openmac.tcl                  ${DIR_RELEASE}
cp --parents altera/components/tcl/qsysUtil.tcl                 ${DIR_RELEASE}
cp --parents common/util/tcl/ipcoreUtil.tcl                     ${DIR_RELEASE}
cp --parents common/util/tcl/writeFile.tcl                      ${DIR_RELEASE}
cp --parents common/lib/src/global.vhd                          ${DIR_RELEASE}
cp --parents common/lib/src/addrDecodeRtl.vhd                   ${DIR_RELEASE}
cp --parents common/lib/src/cntRtl.vhd                          ${DIR_RELEASE}
cp --parents common/lib/src/dpRam-e.vhd                         ${DIR_RELEASE}
cp --parents common/lib/src/dpRamSplx-e.vhd                     ${DIR_RELEASE}
cp --parents common/lib/src/edgedetectorRtl.vhd                 ${DIR_RELEASE}
cp --parents common/lib/src/synchronizerRtl.vhd                 ${DIR_RELEASE}
cp --parents common/lib/src/syncTog-rtl-ea.vhd                  ${DIR_RELEASE}
cp --parents common/fifo/src/asyncFifo-e.vhd                    ${DIR_RELEASE}
cp --parents common/openmac/tcl/openmac.tcl                     ${DIR_RELEASE}
cp --parents common/openmac/src/openmacPkg-p.vhd                ${DIR_RELEASE}
cp --parents common/openmac/src/dma_handler.vhd                 ${DIR_RELEASE}
cp --parents common/openmac/src/master_handler.vhd              ${DIR_RELEASE}
cp --parents common/openmac/src/openMAC_DMAmaster.vhd           ${DIR_RELEASE}
cp --parents common/openmac/src/openfilter-rtl-ea.vhd           ${DIR_RELEASE}
cp --parents common/openmac/src/openhub-rtl-ea.vhd              ${DIR_RELEASE}
cp --parents common/openmac/src/openmacTimer-rtl-ea.vhd         ${DIR_RELEASE}
cp --parents common/openmac/src/phyActGen-rtl-ea.vhd            ${DIR_RELEASE}
cp --parents common/openmac/src/phyMgmt-rtl-ea.vhd              ${DIR_RELEASE}
cp --parents common/openmac/src/convRmiiToMii-rtl-ea.vhd        ${DIR_RELEASE}
cp --parents common/openmac/src/openmac-rtl-ea.vhd              ${DIR_RELEASE}
cp --parents common/openmac/src/openmacTop-rtl-ea.vhd           ${DIR_RELEASE}
cp --parents altera/lib/src/dpRam-rtl-a.vhd                     ${DIR_RELEASE}
cp --parents altera/lib/src/dpRamSplx-rtl-a.vhd                 ${DIR_RELEASE}
cp --parents altera/fifo/src/asyncFifo-syn-a.vhd                ${DIR_RELEASE}
cp --parents altera/openmac/src/alteraOpenmacTop-rtl-ea.vhd     ${DIR_RELEASE}

# copy Xilinx AXI OPENMAC
echo "copy Xilinx axi openmac ipcore..."

# Copy XPS component to directory with version name
mkdir -p ${DIR_RELEASE}/xilinx/components/pcores/axi_openmac_${IP_VERSION}/data

cp xilinx/components/pcores/axi_openmac_vX_YY_Z/data/axi_openmac_v2_1_0.mdd  ${DIR_RELEASE}/xilinx/components/pcores/axi_openmac_${IP_VERSION}/data
cp xilinx/components/pcores/axi_openmac_vX_YY_Z/data/axi_openmac_v2_1_0.mpd  ${DIR_RELEASE}/xilinx/components/pcores/axi_openmac_${IP_VERSION}/data
cp xilinx/components/pcores/axi_openmac_vX_YY_Z/data/axi_openmac_v2_1_0.pao  ${DIR_RELEASE}/xilinx/components/pcores/axi_openmac_${IP_VERSION}/data
cp xilinx/components/pcores/axi_openmac_vX_YY_Z/data/axi_openmac_v2_1_0.tcl  ${DIR_RELEASE}/xilinx/components/pcores/axi_openmac_${IP_VERSION}/data
cp xilinx/components/pcores/axi_openmac_vX_YY_Z/data/axi_openmac_v2_1_0.mui  ${DIR_RELEASE}/xilinx/components/pcores/axi_openmac_${IP_VERSION}/data

cp --parents common/util/tcl/ipcoreUtil.tcl                     ${DIR_RELEASE}
cp --parents common/lib/src/global.vhd                          ${DIR_RELEASE}
cp --parents common/lib/src/addrDecodeRtl.vhd                   ${DIR_RELEASE}
cp --parents common/lib/src/clkXingRtl.vhd                      ${DIR_RELEASE}
cp --parents common/lib/src/cntRtl.vhd                          ${DIR_RELEASE}
cp --parents common/lib/src/dpRam-e.vhd                         ${DIR_RELEASE}
cp --parents common/lib/src/dpRamSplx-e.vhd                     ${DIR_RELEASE}
cp --parents common/lib/src/dpRamSplxNbe-e.vhd                  ${DIR_RELEASE}
cp --parents common/lib/src/edgedetectorRtl.vhd                 ${DIR_RELEASE}
cp --parents common/lib/src/synchronizerRtl.vhd                 ${DIR_RELEASE}
cp --parents common/lib/src/syncTog-rtl-ea.vhd                  ${DIR_RELEASE}
cp --parents common/fifo/src/asyncFifo-e.vhd                    ${DIR_RELEASE}
cp --parents common/fifo/src/asyncFifo-rtl-a.vhd                ${DIR_RELEASE}
cp --parents common/fifo/src/fifoRead-rtl-ea.vhd                ${DIR_RELEASE}
cp --parents common/fifo/src/fifoWrite-rtl-ea.vhd               ${DIR_RELEASE}
cp --parents common/openmac/tcl/openmac.tcl                     ${DIR_RELEASE}
cp --parents common/openmac/src/openmacPkg-p.vhd                ${DIR_RELEASE}
cp --parents common/openmac/src/dma_handler.vhd                 ${DIR_RELEASE}
cp --parents common/openmac/src/master_handler.vhd              ${DIR_RELEASE}
cp --parents common/openmac/src/openMAC_DMAmaster.vhd           ${DIR_RELEASE}
cp --parents common/openmac/src/openfilter-rtl-ea.vhd           ${DIR_RELEASE}
cp --parents common/openmac/src/openhub-rtl-ea.vhd              ${DIR_RELEASE}
cp --parents common/openmac/src/openmacTimer-rtl-ea.vhd         ${DIR_RELEASE}
cp --parents common/openmac/src/phyActGen-rtl-ea.vhd            ${DIR_RELEASE}
cp --parents common/openmac/src/phyMgmt-rtl-ea.vhd              ${DIR_RELEASE}
cp --parents common/openmac/src/convRmiiToMii-rtl-ea.vhd        ${DIR_RELEASE}
cp --parents common/openmac/src/mmSlaveConv-rtl-ea.vhd          ${DIR_RELEASE}
cp --parents common/openmac/src/openmac-rtl-ea.vhd              ${DIR_RELEASE}
cp --parents common/openmac/src/openmacTop-rtl-ea.vhd           ${DIR_RELEASE}
cp --parents xilinx/lib/src/dpRam-rtl-a.vhd                     ${DIR_RELEASE}
cp --parents xilinx/lib/src/dpRamSplx-rtl-a.vhd                 ${DIR_RELEASE}
cp --parents xilinx/lib/src/dpRamSplxNbe-rtl-a.vhd              ${DIR_RELEASE}
cp --parents xilinx/openmac/src/ipifMasterHandler-rtl-ea.vhd    ${DIR_RELEASE}
cp --parents xilinx/openmac/src/axi_openmac-rtl-ea.vhd          ${DIR_RELEASE}
