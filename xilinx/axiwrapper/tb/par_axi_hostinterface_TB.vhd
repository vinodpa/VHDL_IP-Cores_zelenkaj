-------------------------------------------------------------------------------
--! @file tb_axi_hostinterface_ip.vhd
--
--! @brief Test bench for host iterface IP with AXI wrapper
--
--! @details Test bench will provide necessary stimulies to host interface IP
--! to test the functionality of host interface through stimulues.Host interface
--! IP Master is connected to Memory for Data write/read through AXI slave.
--! Host Processor can be Parallel Interface or AXI mode Host
-------------------------------------------------------------------------------
--
--    (c) B&R, 2012
--    (c) Kalycito Infotech Pvt Ltd
--
--    Redistribution and use in source and binary forms, with or without
--    modification, are permitted provided that the following conditions
--    are met:
--
--    1. Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--    2. Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--
--    3. Neither the name of B&R nor the names of its
--       contributors may be used to endorse or promote products derived
--       from this software without prior written permission. For written
--       permission, please contact office@br-automation.com
--
--    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
--    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
--    COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
--    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
--    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
--    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
--    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
--    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--    POSSIBILITY OF SUCH DAMAGE.
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_textio.all;
--! use global library
use work.global.all;
--! Bus master Pakg
use work.busMasterPkg.all;


entity tb_par_axi_hostinterface is
generic(
    gPcpStim : string := "text.txt";
    gHostStim : string := "text.txt"
    );
--port ()
end entity tb_par_axi_hostinterface ;

architecture bhv of  tb_par_axi_hostinterface is

constant C_AXI_ADDR_WIDTH : integer   := 32;
constant C_AXI_DATA_WIDTH : integer   := 32;
constant C_BASEADDR         : std_logic_vector(31 downto 0) := x"7C000000";
constant C_HIGHADDR         : std_logic_vector(31 downto 0) := x"7C0FFFFF";
constant C_SLAVE_BASEADDR         : std_logic_vector(31 downto 0) := x"00000000"; -- Respond to any master Read /Write
constant C_SLAVE_HIGHADDR         : std_logic_vector(31 downto 0) := x"FFFFFFFF"; -- Respond to any master Read /Write
signal  ACLK    : std_logic := '1';
signal  ARESETN : std_logic ;
            --Write Address
signal  AWVALID : std_logic ;
signal  AWREADY : std_logic ;
signal  AWADDR  : std_logic_vector (31 downto 0);
            --Write Data
signal  WVALID  : std_logic ;
signal  WREADY  : std_logic ;
signal  WDATA   : std_logic_vector (31 downto 0);
            --Write Response
signal  BVALID  : std_logic ;
signal  BREADY  : std_logic ;
signal  BRESP   : std_logic_vector (1 downto 0);
            --Read Address
signal  ARVALID : std_logic ;
signal  ARREADY : std_logic ;
signal  ARADDR  : std_logic_vector (31 downto 0);
            --Read Data
signal  RVALID  : std_logic ;
signal  RREADY  : std_logic ;
signal  RDATA   : std_logic_vector (31 downto 0);

signal  ARPROT  : std_logic_vector (2 downto 0);
signal  AWPROT  : std_logic_vector (2 downto 0);
signal  WSTRB   : std_logic_vector (3 downto 0);
signal  RRESP   : std_logic_vector (1 downto 0);

-- Host Interface Signals
signal  hAWVALID : std_logic ;
signal  hAWREADY : std_logic ;
signal  hAWADDR  : std_logic_vector (31 downto 0);
            --Write Data
signal  hWVALID  : std_logic ;
signal  hWREADY  : std_logic ;
signal  hWDATA   : std_logic_vector (31 downto 0);
            --Write Response
signal  hBVALID  : std_logic ;
signal  hBREADY  : std_logic ;
signal  hBRESP   : std_logic_vector (1 downto 0);
            --Read Address
signal  hARVALID : std_logic ;
signal  hARREADY : std_logic ;
signal  hARADDR  : std_logic_vector (31 downto 0);
            --Read Data
signal  hRVALID  : std_logic ;
signal  hRREADY  : std_logic ;
signal  hRDATA   : std_logic_vector (31 downto 0);

signal  hARPROT  : std_logic_vector (2 downto 0);
signal  hAWPROT  : std_logic_vector (2 downto 0);
signal  hWSTRB   : std_logic_vector (3 downto 0);
signal  hRRESP   : std_logic_vector (1 downto 0);

--Master Interface Signals
--
signal  mAWVALID : std_logic ;
signal  mAWREADY : std_logic ;
signal  mAWADDR  : std_logic_vector (31 downto 0);
            --Write Data
signal  mWVALID  : std_logic ;
signal  mWREADY  : std_logic ;
signal  mWDATA   : std_logic_vector (31 downto 0);
            --Write Response
signal  mBVALID  : std_logic ;
signal  mBREADY  : std_logic ;
signal  mBRESP   : std_logic_vector (1 downto 0);
            --Read Address
signal  mARVALID : std_logic ;
signal  mARREADY : std_logic ;
signal  mARADDR  : std_logic_vector (31 downto 0);
            --Read Data
signal  mRVALID  : std_logic ;
signal  mRREADY  : std_logic ;
signal  mRDATA   : std_logic_vector (31 downto 0);

signal  mARPROT  : std_logic_vector (2 downto 0);
signal  mAWPROT  : std_logic_vector (2 downto 0);
signal  mWSTRB   : std_logic_vector (3 downto 0);
signal  mRRESP   : std_logic_vector (1 downto 0);

--Powerlink Interface Signals
signal inr_irqSync_irq                 : std_logic;
signal ins_irqOut_irq                  : std_logic;
signal coe_ExtSync_exsync              : std_logic;
signal coe_NodeId_nodeid               : std_logic_vector(7 downto 0) := x"F0";
signal coe_PlkLed_lederr               : std_logic;
signal coe_PlkLed_ledst                : std_logic;
--Parallel Interface Signals
constant gParallelDataWidth             : natural := 16;
signal coe_parHost_chipselect          : std_logic;
signal coe_parHost_read                : std_logic;
signal coe_parHost_write               : std_logic;
signal coe_parHost_addressLatchEnable  : std_logic;
signal coe_parHost_acknowledge         : std_logic;
signal coe_parHost_byteenable          : std_logic_vector(gParallelDataWidth/8-1 downto 0);
signal coe_parHost_address             : std_logic_vector(15 downto 0);
signal coe_parHost_data_I                : std_logic_vector(gParallelDataWidth-1 downto 0);
signal coe_parHost_data_O                : std_logic_vector(gParallelDataWidth-1 downto 0);
signal coe_parHost_data_T                : std_logic ;
signal coe_parHost_addressData_I         : std_logic_vector(gParallelDataWidth-1 downto 0);
signal coe_parHost_addressData_O         : std_logic_vector(gParallelDataWidth-1 downto 0);
signal coe_parHost_addressData_T         : std_logic ;

-- Master Avalon Write Signals
signal  wAvalonRead          : std_logic                            ;
signal  wAvalonWrite         : std_logic                            ;
signal  wAvalonAddr          : std_logic_vector   (31 downto 0)     ;
signal  wAvalonBE            : std_logic_vector   (3 downto 0)      ;
signal  wAvalonWaitReq       : std_logic                            ;
signal  wAvalonReadValid     : std_logic                            ;
signal  wAvalonReadData      : std_logic_vector   (31 downto 0)     ;
signal  wAvalonWriteData     : std_logic_vector   (31 downto 0)     ;

-- Slave Avalon Read Signals
signal  memAvalonRead          : std_logic                            ;
signal  memAvalonWrite         : std_logic                            ;
signal  memAvalonAddr          : std_logic_vector   (31 downto 0)     ;
signal  memAvalonBE            : std_logic_vector   (3 downto 0)      ;
signal  memAvalonWaitReq       : std_logic                            ;
signal  memAvalonReadValid     : std_logic                            ;
signal  memAvalonReadData      : std_logic_vector   (31 downto 0)     ;
signal  memAvalonWriteData     : std_logic_vector   (31 downto 0)     ;

signal  memroyAck            : std_logic                            ;

--Bus Master Signals
signal  BusMasterEnable       : std_logic                            ;
signal  BusMasterAck          : std_logic                            ;
signal  BusMasterSelect       : std_logic                            ;
signal  BusMasterError        : std_logic                            ;
signal  BusMasterDone         : std_logic                            ;
signal  BusMasterReset        : std_logic                            ;


signal  hBusMasterDone         : std_logic                            ;--TODO: Global needed ?
-- Test case
constant cPeriode       : time := 10 ns;
-- TODO: Modelsim Simulation
--constant cStimuliFile   :string := gPcpStim ;
--constant cHostStimuliFile   : string := gHostStim ;
-- TODO: Isim Simulation
constant cStimuliFile       : string := "E:/XilinxMN/HDL_GitRepo/VHDL_IP-Cores/xilinx/axiwrapper/tb/tbPCPMasterBhv_TB_stim.txt";
constant cHostStimuliFile   : string := "E:/XilinxMN/HDL_GitRepo/VHDL_IP-Cores/xilinx/axiwrapper/tb/tbHostMasterBhv_TB_stim.txt";

constant cRamSize       : natural := 640 * 1024; --[byte]
constant cRamAddrWidth  : natural := LogDualis(cRamSize);
--type tMemory is array (natural range <>) of std_logic_vector(cMaxBitWidth-1 downto 0);
--constant cMemoryRange   : natural := 70000;
--constant cInitMemory    : tMemory(cMemoryRange-1 downto 0) := (others => (others => '0'));
--signal Memory : tMemory(cMemoryRange-1 downto 0) := cInitMemory;
--signal Memory1 : tMemory(cMemoryRange-1 downto 0) := cInitMemory;

constant gHostIfType    : integer := 1 ;
constant gVersionMajor      : integer := 255 ;
constant gVersionMinor      : integer := 255 ;
constant gVersionRevision   : integer := 255 ;
constant gVersionCount      : integer := 0 ;
constant gBaseDynBuf0       : integer := 2048; --x"00800" ;
constant gBaseDynBuf1       : integer := 4096; --x"01000" ;
constant gBaseErrCntr       : integer := 6144; --x"01800" ;
constant gBaseTxNmtQ        : integer := 10240;--x"02800" ;
constant gBaseTxGenQ        : integer := 14336;--x"03800" ;
constant gBaseTxSynQ        : integer := 18432;--x"04800" ;
constant gBaseTxVetQ        : integer := 22528;--x"05800" ;
constant gBaseRxVetQ        : integer := 26624;--x"06800" ;
constant gBaseK2UQ          : integer := 28672;--x"07000" ;
constant gBaseU2KQ          : integer := 36864;--x"09000" ;
constant gBaseTpdo          : integer := 45056;--x"0B000" ;
constant gBaseRpdo          : integer := 57344;--x"0E000" ;
constant gBaseRes           : integer := 81920;--x"14000" ;

signal BridgeAddrss : std_logic_vector (31 downto 0) ;

begin


DUT: entity work.axi_hostinterface
    generic map (
        --AXI Lite Slave PCP Interface
        C_BASEADDR         =>   C_BASEADDR,
        C_HIGHADDR         =>   C_HIGHADDR,
        C_S_AXI_ADDR_WIDTH =>   32 ,
        C_S_AXI_DATA_WIDTH =>   32 ,
        --AXI Lite Slave Host Interface
        C_HOST_BASEADDR    =>   C_BASEADDR,
        C_HOST_HIGHADDR        => C_HIGHADDR ,
        C_S_HOST_AXI_DATA_WIDTH => 32,
        C_S_HOST_AXI_ADDR_WIDTH => 32,
        ----Host Interface Parameters
        C_M_AXI_ADDR_WIDTH  => 32,
        C_M_AXI_DATA_WIDTH => 32,
        gVersionMajor      => gVersionMajor,
        gVersionMinor      => gVersionMinor ,
        gVersionRevision   => gVersionRevision ,
        gVersionCount      => gVersionCount,
        gBaseDynBuf0       => gBaseDynBuf0,
        gBaseDynBuf1       => gBaseDynBuf1 ,
        gBaseErrCntr       => gBaseErrCntr ,
        gBaseTxNmtQ        => gBaseTxNmtQ ,
        gBaseTxGenQ        => gBaseTxGenQ  ,
        gBaseTxSynQ        => gBaseTxSynQ  ,
        gBaseTxVetQ        => gBaseTxVetQ  ,
        gBaseRxVetQ        => gBaseRxVetQ  ,
        gBaseK2UQ          => gBaseK2UQ  ,
        gBaseU2KQ          => gBaseU2KQ  ,
        gBaseTpdo          => gBaseTpdo  ,
        gBaseRpdo          => gBaseRpdo  ,
        gBaseRes           => gBaseRes  ,
        gHostIfType        => gHostIfType  ,
        gParallelDataWidth  => 16   ,
        gParallelMultiplex   => 0
    )
    port map(
        --! Clock Source input
        --csi_c0_clock                    : in std_logic;
        --! Reset Source input
        --rsi_r0_reset                    : in std_logic;
        -- Avalon Memory Mapped Slave for PCP
        -- System Signals
        S_AXI_PCP_ACLK            =>  ACLK ,
        S_AXI_PCP_ARESETN         =>  ARESETN ,
        -- Slave Interface Write Address Ports
        S_AXI_PCP_AWADDR    =>  AWADDR ,
        --S_AXI_PCP_AWPROT    =>  AWPROT ,
        S_AXI_PCP_AWVALID   =>  AWVALID ,
        S_AXI_PCP_AWREADY   =>  AWREADY  ,
        -- Slave Interface Write Data Ports
        S_AXI_PCP_WDATA     =>  WDATA   ,
        S_AXI_PCP_WSTRB     =>  WSTRB   ,
        S_AXI_PCP_WVALID    =>  WVALID  ,
        S_AXI_PCP_WREADY    =>  WREADY  ,
        -- Slave Interface Write Response Ports
        S_AXI_PCP_BRESP     =>  BRESP   ,
        S_AXI_PCP_BVALID    =>  BVALID  ,
        S_AXI_PCP_BREADY    =>  BREADY  ,
        -- Slave Interface Read Address Ports
        S_AXI_PCP_ARADDR    =>  ARADDR  ,
        --S_AXI_PCP_ARPROT    =>  ARPROT  ,
        S_AXI_PCP_ARVALID   =>  ARVALID ,
        S_AXI_PCP_ARREADY   =>  ARREADY ,
        -- Slave Interface Read Data Ports
        S_AXI_PCP_RDATA     =>  RDATA   ,
        S_AXI_PCP_RRESP     =>  RRESP   ,
        S_AXI_PCP_RVALID    =>  RVALID  ,
        S_AXI_PCP_RREADY    =>  RREADY  ,

        -- Avalon Memory Mapped Slave for Host
        -- System Signals
        S_AXI_HOST_ACLK           =>  ACLK   ,
        S_AXI_HOST_ARESETN        =>  ARESETN ,
        -- Slave Interface Write Address Ports
        S_AXI_HOST_AWADDR   =>  hAWADDR ,
        --S_AXI_HOST_AWPROT   =>  hAWPROT ,
        S_AXI_HOST_AWVALID  =>  hAWVALID ,
        S_AXI_HOST_AWREADY  =>  hAWREADY ,
        -- Slave Interface Write Data Ports
        S_AXI_HOST_WDATA    =>  hWDATA  ,
        S_AXI_HOST_WSTRB    =>  hWSTRB  ,
        S_AXI_HOST_WVALID   =>  hWVALID ,
        S_AXI_HOST_WREADY   =>  hWREADY ,
        -- Slave Interface Write Response Ports
        S_AXI_HOST_BRESP    =>  hBRESP  ,
        S_AXI_HOST_BVALID   =>  hBVALID ,
        S_AXI_HOST_BREADY   =>  hBREADY ,
        -- Slave Interface Read Address Ports
        S_AXI_HOST_ARADDR   =>  hARADDR ,
        --S_AXI_HOST_ARPROT   =>  hARPROT ,
        S_AXI_HOST_ARVALID  =>  hARVALID  ,
        S_AXI_HOST_ARREADY  =>  hARREADY    ,
        -- Slave Interface Read Data Ports
        S_AXI_HOST_RDATA    =>  hRDATA  ,
        S_AXI_HOST_RRESP    =>  hRRESP  ,
        S_AXI_HOST_RVALID   =>  hRVALID  ,
        S_AXI_HOST_RREADY   =>  hRREADY ,

        -- Avalon Memory Mapped Master for Host via Magic Bridge
          -- System Signals
        M_AXI_ACLK          =>  ACLK   ,
        M_AXI_ARESETN       =>  ARESETN ,

        -- Master Interface Write Address
        M_AXI_AWADDR        =>  mAWADDR ,
        M_AXI_AWPROT        =>  mAWPROT ,
        M_AXI_AWVALID       =>  mAWVALID    ,
        M_AXI_AWREADY       =>  mAWREADY    ,

        -- Master Interface Write Data
        M_AXI_WDATA         =>  mWDATA  ,
        M_AXI_WSTRB         =>  mWSTRB  ,
        M_AXI_WVALID        =>  mWVALID ,
        M_AXI_WREADY        =>  mWREADY ,

        -- Master Interface Write Response
        M_AXI_BRESP         =>  mBRESP  ,
        M_AXI_BVALID        =>  mBVALID ,
        M_AXI_BREADY        =>  mBREADY ,

        -- Master Interface Read Address
        M_AXI_ARADDR        =>  mARADDR ,
        M_AXI_ARPROT        =>  mARPROT ,
        M_AXI_ARVALID       =>  mARVALID    ,
        M_AXI_ARREADY       =>  mARREADY    ,

        -- Master Interface Read Data
        M_AXI_RDATA         =>  mRDATA  ,
        M_AXI_RRESP         =>  mRRESP  ,
        M_AXI_RVALID        =>  mRVALID ,
        M_AXI_RREADY        =>  mRREADY ,
        --! Interrupt receiver
        inr_irqSync_irq     =>  inr_irqSync_irq ,
        --! Interrupt sender
        ins_irqOut_irq      =>  ins_irqOut_irq ,
        --! External Sync Source
        coe_ExtSync_exsync  =>  coe_ExtSync_exsync ,
        --! Node Id
        coe_NodeId_nodeid   =>  coe_NodeId_nodeid   ,
        --! POWERLINK Error LED
        coe_PlkLed_lederr   =>  coe_PlkLed_lederr   ,
        --! POWERLINK Status LED
        coe_PlkLed_ledst    =>  coe_PlkLed_ledst    ,
        -- Parallel Host Interface
        --! Chipselect
        coe_parHost_chipselect  =>  coe_parHost_chipselect ,
        --! Read strobe
        coe_parHost_read    => coe_parHost_read,
        --! Write strobe
        coe_parHost_write   => coe_parHost_write ,
        --! Address Latch enable (Multiplexed only)
        coe_parHost_addressLatchEnable  =>  coe_parHost_addressLatchEnable,
        --! High active Acknowledge
        coe_parHost_acknowledge => coe_parHost_acknowledge ,
        --! Byteenables
        coe_parHost_byteenable  => coe_parHost_byteenable ,
        --! Address bus (Demultiplexed, word-address)
        coe_parHost_address => coe_parHost_address ,
        --! Data bus (Demultiplexed)
        coe_parHost_data_I  => coe_parHost_data_I ,
        coe_parHost_data_O  => coe_parHost_data_O ,
        coe_parHost_data_T  => coe_parHost_data_T ,
        --! Address/Data bus (Multiplexed, word-address))
        coe_parHost_addressData_I   => coe_parHost_addressData_I ,
        coe_parHost_addressData_O   => coe_parHost_addressData_O ,
        coe_parHost_addressData_T   => coe_parHost_addressData_T
    );

PCP_MODEL: entity work.axi_lite_master_wrapper
generic map
    (
        C_M_AXI_ADDR_WIDTH  =>  C_AXI_ADDR_WIDTH ,
        C_M_AXI_DATA_WIDTH  =>  C_AXI_DATA_WIDTH
    )
port map
    (
        -- System Signals
        M_AXI_ACLK          =>  ACLK                ,
        M_AXI_ARESETN       =>  ARESETN             ,
        -- Master Interface Write Address
        M_AXI_AWADDR        =>  AWADDR              ,
        M_AXI_AWPROT        =>  AWPROT              ,
        M_AXI_AWVALID       =>  AWVALID             ,
        M_AXI_AWREADY       =>  AWREADY             ,
        -- Master Interface Write Data
        M_AXI_WDATA         =>  WDATA               ,
        M_AXI_WSTRB         =>  WSTRB               ,
        M_AXI_WVALID        =>  WVALID              ,
        M_AXI_WREADY        =>  WREADY              ,
        -- Master Interface Write Response
        M_AXI_BRESP         =>  BRESP               ,
        M_AXI_BVALID        =>  BVALID              ,
        M_AXI_BREADY        =>  BREADY              ,
        -- Master Interface Read Address
        M_AXI_ARADDR        =>  ARADDR              ,
        M_AXI_ARPROT        =>  ARPROT              ,
        M_AXI_ARVALID       =>  ARVALID             ,
        M_AXI_ARREADY       =>  ARREADY             ,
        -- Master Interface Read Data
        M_AXI_RDATA         =>  RDATA               ,
        M_AXI_RRESP         =>  RRESP               ,
        M_AXI_RVALID        =>  RVALID              ,
        M_AXI_RREADY        =>  RREADY              ,
        -- Avalon Interface Signals
        iAvalonRead         =>  wAvalonRead         ,
        iAvalonWrite        =>  wAvalonWrite        ,
        iAvalonAddr         =>  wAvalonAddr         ,
        iAvalonBE           =>  wAvalonBE           ,
        oAvalonWaitReq      =>  wAvalonWaitReq      ,
        oAvalonReadValid    =>  wAvalonReadValid    ,
        oAvalonReadData     =>  wAvalonReadData     ,
        iAvalonWriteData    =>  wAvalonWriteData
    );
-- Avalon Master Write/Read operations
AVALON_BUS_MASTER_PCP:entity work.busMaster
    generic map
     (
        gAddrWidth          => C_AXI_ADDR_WIDTH,
        gDataWidth          => C_AXI_DATA_WIDTH,
        gStimuliFile        => cStimuliFile
     )
    port map
     (
        iRst                =>  BusMasterReset      ,
        iClk                =>  ACLK                ,
        iEnable             =>  BusMasterEnable     ,
        iAck                =>  BusMasterAck        ,
        iReaddata           =>  wAvalonReadData     ,
        oWrite              =>  wAvalonWrite        ,
        oRead               =>  wAvalonRead         ,
        oSelect             =>  BusMasterSelect     ,
        oAddress            =>  wAvalonAddr         ,
        oByteenable         =>  wAvalonBE           ,
        oWritedata          =>  wAvalonWriteData    ,
        oError              =>  BusMasterError      ,
        oDone               =>  BusMasterDone
    );

-------------------------------------------------------------------------
-- Bridge Is connecting to Memory through AXI slave
-- DUT_BRIDGE -> AXI_SLAVE->Avalon Memory model
-------------------------------------------------------------------------
MEMORY_IF_MODEL: entity work.axi_lite_slave_wrapper
generic map
    (
        C_BASEADDR          =>  C_SLAVE_BASEADDR          ,
        C_HIGHADDR          =>  C_SLAVE_HIGHADDR          ,
        C_S_AXI_ADDR_WIDTH  =>  C_AXI_ADDR_WIDTH    ,
        C_S_AXI_DATA_WIDTH  =>  C_AXI_DATA_WIDTH
    )
port map
    (
        -- System Signals
        ACLK                =>  ACLK                ,
        ARESETN             =>  ARESETN             ,
        -- Slave Interface Write Address Ports
        S_AXI_AWADDR        =>  BridgeAddrss         , -- TODO: Check address
        S_AXI_AWPROT        =>  mAWPROT              ,
        S_AXI_AWVALID       =>  mAWVALID             ,
        S_AXI_AWREADY       =>  mAWREADY             ,
        -- Slave Interface Write Data Ports
        S_AXI_WDATA         =>  mWDATA               ,
        S_AXI_WSTRB         =>  mWSTRB               ,
        S_AXI_WVALID        =>  mWVALID              ,
        S_AXI_WREADY        =>  mWREADY              ,
        -- Slave Interface Write Response Ports
        S_AXI_BRESP         =>  mBRESP               ,
        S_AXI_BVALID        =>  mBVALID              ,
        S_AXI_BREADY        =>  mBREADY              ,
        -- Slave Interface Read Address Ports
        S_AXI_ARADDR        =>  mARADDR              ,
        S_AXI_ARPROT        =>  mARPROT              ,
        S_AXI_ARVALID       =>  mARVALID             ,
        S_AXI_ARREADY       =>  mARREADY             ,
        -- Slave Interface Read Data Ports
        S_AXI_RDATA         =>  mRDATA               ,
        S_AXI_RRESP         =>  mRRESP               ,
        S_AXI_RVALID        =>  mRVALID              ,
        S_AXI_RREADY        =>  mRREADY              ,
        --Avalon Interface
        oAvsAddress         =>  memAvalonAddr         ,
        oAvsByteenable      =>  memAvalonBE           ,
        oAvsRead            =>  memAvalonRead         ,
        oAvsWrite           =>  memAvalonWrite        ,
        oAvsWritedata       =>  memAvalonWriteData    ,
        iAvsReaddata        =>  memAvalonReadData     ,
        iAvsWaitrequest     =>  memAvalonWaitReq
    );
    BridgeAddrss <= "00" & mAWADDR(29 downto 0) ;
-- Avalon Slave Write/Read Checker
theRam : entity work.spRam
        port map (
            iClk        => ACLK,
            iWrite      => memAvalonWrite,
            iRead       => memAvalonRead,
            iAddress    => memAvalonAddr(cRamAddrWidth-1 downto 2),
            iByteenable => memAvalonBE,
            iWritedata  => memAvalonWriteData,
            oReaddata   => memAvalonReadData,
            oAck        => memroyAck
        );
            memAvalonWaitReq <= not memroyAck ;

-------------------------------------------------------------------------
-- Host AXI Interface IP master
-------------------------------------------------------------------------
genHostAXIMaster : if gHostIfType = 0 generate
-- Master Host Avalon Write Signals
signal  hAvalonRead          : std_logic                            ;
signal  hAvalonWrite         : std_logic                            ;
signal  hAvalonAddr          : std_logic_vector   (31 downto 0)     ;
signal  hAvalonBE            : std_logic_vector   (3 downto 0)      ;
signal  hAvalonWaitReq       : std_logic                            ;
signal  hAvalonReadValid     : std_logic                            ;
signal  hAvalonReadData      : std_logic_vector   (31 downto 0)     ;
signal  hAvalonWriteData     : std_logic_vector   (31 downto 0)     ;

--Bus Master Signals
signal  hBusMasterEnable       : std_logic                            ;
signal  hBusMasterAck          : std_logic                            ;
signal  hBusMasterSelect       : std_logic                            ;
signal  hBusMasterError        : std_logic                            ;
--signal  hBusMasterDone         : std_logic                            ;
signal  hBusMasterReset        : std_logic                            ;

begin
HOST_MODEL: entity work.axi_lite_master_wrapper
generic map
    (
        C_M_AXI_ADDR_WIDTH  =>  C_AXI_ADDR_WIDTH ,
        C_M_AXI_DATA_WIDTH  =>  C_AXI_DATA_WIDTH
    )
port map
    (
        -- System Signals
        M_AXI_ACLK          =>  ACLK                ,
        M_AXI_ARESETN       =>  ARESETN             ,
        -- Master Interface Write Address
        M_AXI_AWADDR        =>  hAWADDR              ,
        M_AXI_AWPROT        =>  hAWPROT              ,
        M_AXI_AWVALID       =>  hAWVALID             ,
        M_AXI_AWREADY       =>  hAWREADY             ,
        -- Master Interface Write Data
        M_AXI_WDATA         =>  hWDATA               ,
        M_AXI_WSTRB         =>  hWSTRB               ,
        M_AXI_WVALID        =>  hWVALID              ,
        M_AXI_WREADY        =>  hWREADY              ,
        -- Master Interface Write Response
        M_AXI_BRESP         =>  hBRESP               ,
        M_AXI_BVALID        =>  hBVALID              ,
        M_AXI_BREADY        =>  hBREADY              ,
        -- Master Interface Read Address
        M_AXI_ARADDR        =>  hARADDR              ,
        M_AXI_ARPROT        =>  hARPROT              ,
        M_AXI_ARVALID       =>  hARVALID             ,
        M_AXI_ARREADY       =>  hARREADY             ,
        -- Master Interface Read Data
        M_AXI_RDATA         =>  hRDATA               ,
        M_AXI_RRESP         =>  hRRESP               ,
        M_AXI_RVALID        =>  hRVALID              ,
        M_AXI_RREADY        =>  hRREADY              ,
        -- Avalon Interface Signals
        iAvalonRead         =>  hAvalonRead         ,
        iAvalonWrite        =>  hAvalonWrite        ,
        iAvalonAddr         =>  hAvalonAddr         ,
        iAvalonBE           =>  hAvalonBE           ,
        oAvalonWaitReq      =>  hAvalonWaitReq      ,
        oAvalonReadValid    =>  hAvalonReadValid    ,
        oAvalonReadData     =>  hAvalonReadData     ,
        iAvalonWriteData    =>  hAvalonWriteData
    );
AVALON_HOST_BUS_MASTER:entity work.busMaster
    generic map
     (
        gAddrWidth          => C_AXI_ADDR_WIDTH,
        gDataWidth          => C_AXI_DATA_WIDTH,
        gStimuliFile        => cHostStimuliFile
     )
    port map
     (
        iRst                =>  hBusMasterReset      ,
        iClk                =>  ACLK                ,
        iEnable             =>  hBusMasterEnable     ,
        iAck                =>  hBusMasterAck        ,
        iReaddata           =>  hAvalonReadData     ,
        oWrite              =>  hAvalonWrite        ,
        oRead               =>  hAvalonRead         ,
        oSelect             =>  hBusMasterSelect     ,
        oAddress            =>  hAvalonAddr         ,
        oByteenable         =>  hAvalonBE           ,
        oWritedata          =>  hAvalonWriteData    ,
        oError              =>  hBusMasterError      ,
        oDone               =>  hBusMasterDone
    );
    --hBusMasterReset <= not ARESETN ;
    hBusMasterReset <= BusMasterDone ;
    hBusMasterEnable <= '1' ;
    hBusMasterAck   <= not hAvalonWaitReq ;
    --TODO: Add Protocol Checker also
end generate ;
-------------------------------------------------------------------------
--  Parallel Interface for Hsot
-------------------------------------------------------------------------
genParallelMaster : if gHostIfType = 1 generate
-- Master Host Avalon Write Signals
signal  hAvalonRead          : std_logic                            ;
signal  hAvalonWrite         : std_logic                            ;
signal  hAvalonAddr          : std_logic_vector   (31 downto 0)     ;
signal  hAvalonBE            : std_logic_vector   (3 downto 0)      ;
signal  hAvalonWaitReq       : std_logic                            ;
signal  hAvalonReadValid     : std_logic                            ;
signal  hAvalonReadData      : std_logic_vector   (31 downto 0)     ;
signal  hAvalonWriteData     : std_logic_vector   (31 downto 0)     ;

--Bus Master Signals
signal  hBusMasterEnable       : std_logic                            ;
signal  hBusMasterAck          : std_logic                            ;
signal  hBusMasterSelect       : std_logic                            ;
signal  hBusMasterError        : std_logic                            ;
--signal  hBusMasterDone         : std_logic                            ;
signal  hBusMasterReset        : std_logic                            ;

begin
PARALLEL_INTERFACE_MASTER : entity work.parallel_master
        generic map (
            gDataWidth => 16,
            gMultiplex => 0
        )
        port map (
            -- Avalon Interface
            iAvalonRead         =>  hAvalonRead         ,
            iAvalonWrite        =>  hAvalonWrite        ,
            iAvalonAddr         =>  hAvalonAddr         ,
            iAvalonBE           =>  hAvalonBE           ,
            oAvalonWaitReq      =>  hAvalonWaitReq      ,
            oAvalonReadValid    =>  hAvalonReadValid    ,
            oAvalonReadData     =>  hAvalonReadData     ,
            iAvalonWriteData    =>  hAvalonWriteData    ,
            -- Parallel Interface
            oParHostChipselect => coe_parHost_chipselect,
            oParHostRead => coe_parHost_read,
            oParHostWrite => coe_parHost_write,
            oParHostAddressLatchEnable => coe_parHost_addressLatchEnable,
            iParHostAcknowledge => coe_parHost_acknowledge,
            oParHostByteenable => coe_parHost_byteenable,
            oParHostAddress => coe_parHost_address,
            iParHostData => coe_parHost_data_O,
            oParHostData => coe_parHost_data_I,
            iParHostDataEnable => coe_parHost_data_T,
            iParHostAddressData => coe_parHost_addressData_O,
            oParHostAddressData => coe_parHost_addressData_I,
            iParHostAddressDataEnable => coe_parHost_addressData_T,
            iClk => ACLK,
            iRst => BusMasterReset
        );
AVALON_HOST_BUS_MASTER:entity work.busMaster
    generic map
     (
        gAddrWidth          => C_AXI_ADDR_WIDTH,
        gDataWidth          => C_AXI_DATA_WIDTH,
        gStimuliFile        => cHostStimuliFile
     )
    port map
     (
        iRst                =>  hBusMasterReset      ,
        iClk                =>  ACLK                ,
        iEnable             =>  hBusMasterEnable     ,
        iAck                =>  hBusMasterAck        ,
        iReaddata           =>  hAvalonReadData     ,
        oWrite              =>  hAvalonWrite        ,
        oRead               =>  hAvalonRead         ,
        oSelect             =>  hBusMasterSelect     ,
        oAddress            =>  hAvalonAddr         ,
        oByteenable         =>  hAvalonBE           ,
        oWritedata          =>  hAvalonWriteData    ,
        oError              =>  hBusMasterError      ,
        oDone               =>  hBusMasterDone
    );
    --hBusMasterReset <= not ARESETN ;
    hBusMasterReset <= BusMasterDone ;
    hBusMasterEnable <= '1' ;
    hBusMasterAck   <= not hAvalonWaitReq ;
end generate;
-------------------------------------------------------------------------
-- Verification Enviornment
-------------------------------------------------------------------------
-- AXI Protocol Checket to verify the Signals flow
-------------------------------------------------------------------------
--PROTOCOL_CHECKER_PCP: entity work.axi_ProtocolChecker
--generic map
--    (
--        gAddrWidth          =>  32
--    )
--port map
--    (
--        ACLK                =>  ACLK                ,
--        ARESETN             =>  ARESETN             ,
            --Write Address
--        AWVALID             =>  AWVALID             ,
--        AWREADY             =>  AWREADY             ,
--        AWADDR              =>  AWADDR              ,
            --Write Data
--        WVALID              =>  WVALID              ,
--        WREADY              =>  WREADY              ,
--        WDATA               =>  WDATA               ,
            --Write Response
--        BVALID              =>  BVALID              ,
--        BREADY              =>  BREADY              ,
--        BRESP               =>  BRESP               ,
            --Read Address
--        ARVALID             =>  ARVALID             ,
--        ARREADY             =>  ARREADY             ,
--        ARADDR              =>  ARADDR              ,
            --Read Data
--        RVALID              =>  RVALID              ,
--        RREADY              =>  RREADY              ,
--        RDATA               =>  RDATA
--     );

-------------------------------------------------------------------------
--TODO: Add Protocol Checker for HOST Processor also
-------------------------------------------------------------------------

-------------------------------------------------------------------------
--  General Settings
-------------------------------------------------------------------------
 -- Clock & Reset
    --ACLK   <=  '0' after 0ns ;
    ARESETN <=  '0' after 0 ns,
                '1' after 100 ns;
    BusMasterReset <= not ARESETN ;
    ACLK <=   not ACLK after cPeriode/2 when BusMasterDone /= cActivated else '0' after cPeriode/2;
    BusMasterEnable <= '1' ;
    BusMasterAck      <= not wAvalonWaitReq ;

end bhv ;