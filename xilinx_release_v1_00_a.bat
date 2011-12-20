@echo on

mkdir release\xilinx_xps\plb_powerlink_v1_00_a\hdl\vhdl
mkdir release\xilinx_xps\plb_powerlink_v1_00_a\hdl\vhdl\lib
mkdir release\xilinx_xps\plb_powerlink_v1_00_a\hdl\vhdl\openMAC_DMAmaster
mkdir release\xilinx_xps\plb_powerlink_v1_00_a\hdl\vhdl\openMAC_DMAFifo_Xilinx
mkdir release\xilinx_xps\plb_powerlink_v1_00_a\data
mkdir release\xilinx_xps\plb_powerlink_v1_00_a\doc


copy active_hdl\src\*.vhd				release\xilinx_xps\plb_powerlink_v1_00_a\hdl\vhdl
copy active_hdl\src\lib\*.vhd				release\xilinx_xps\plb_powerlink_v1_00_a\hdl\vhdl\lib
copy active_hdl\src\openMAC_DMAmaster\*.vhd		release\xilinx_xps\plb_powerlink_v1_00_a\hdl\vhdl\openMAC_DMAmaster
copy active_hdl\src\openMAC_DMAFifo_Xilinx\*.vhd	release\xilinx_xps\plb_powerlink_v1_00_a\hdl\vhdl\openMAC_DMAFifo_Xilinx
copy active_hdl\compile\*.vhd				release\xilinx_xps\plb_powerlink_v1_00_a\hdl\vhdl
copy active_hdl\src\xilinx_xps\*.mpd			release\xilinx_xps\plb_powerlink_v1_00_a\data
copy active_hdl\src\xilinx_xps\*.mdd			release\xilinx_xps\plb_powerlink_v1_00_a\data
copy active_hdl\src\xilinx_xps\*.pao			release\xilinx_xps\plb_powerlink_v1_00_a\data
copy active_hdl\src\xilinx_xps\*.mui			release\xilinx_xps\plb_powerlink_v1_00_a\data
copy active_hdl\src\xilinx_xps\*.tcl			release\xilinx_xps\plb_powerlink_v1_00_a\data
copy documentation\*_Generic.pdf				release\xilinx_xps\plb_powerlink_v1_00_a\doc
copy documentation\*_Xilinx.pdf				release\xilinx_xps\plb_powerlink_v1_00_a\doc
copy documentation\OpenMAC.pdf				release\xilinx_xps\plb_powerlink_v1_00_a\doc

del release\xilinx_xps\plb_powerlink_v1_00_a\hdl\vhdl\*_TB.vhd
del release\xilinx_xps\plb_powerlink_v1_00_a\hdl\vhdl\*_Altera.vhd

@echo on