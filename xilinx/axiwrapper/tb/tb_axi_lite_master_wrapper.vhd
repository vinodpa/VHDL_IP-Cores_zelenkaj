-------------------------------------------------------------------------------
--! @file tb_axi_lite_master_wrapper.vhd
--
--! @brief Test bench for AXI lite Master & AXI lite Slave
--
--! @details
--!  AXI lite master wrapper & Slave wrapper connected for verification
--!  Avalon bus master will provide stimulus for AXI lite master & Slave will
--!  Respond for this
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


entity tb_axi_lite_master_wrapper is

end entity tb_axi_lite_master_wrapper ;

architecture bhv of  tb_axi_lite_master_wrapper is

constant C_AXI_ADDR_WIDTH : integer   := 32;
constant C_AXI_DATA_WIDTH : integer   := 32;
constant C_BASEADDR         : std_logic_vector(31 downto 0) := x"7C000000";
constant C_HIGHADDR         : std_logic_vector(31 downto 0) := x"7C00ffff";

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

-- Master Write Signals
signal  wAvalonRead          : std_logic                            ;
signal  wAvalonWrite         : std_logic                            ;
signal  wAvalonAddr          : std_logic_vector   (31 downto 0)     ;
signal  wAvalonBE            : std_logic_vector   (3 downto 0)      ;
signal  wAvalonWaitReq       : std_logic                            ;
signal  wAvalonReadValid     : std_logic                            ;
signal  wAvalonReadData      : std_logic_vector   (31 downto 0)     ;
signal  wAvalonWriteData     : std_logic_vector   (31 downto 0)     ;

-- Slave Read Signals
signal  rAvalonRead          : std_logic                            ;
signal  rAvalonWrite         : std_logic                            ;
signal  rAvalonAddr          : std_logic_vector   (31 downto 0)     ;
signal  rAvalonBE            : std_logic_vector   (3 downto 0)      ;
signal  rAvalonWaitReq       : std_logic                            ;
signal  rAvalonReadValid     : std_logic                            ;
signal  rAvalonReadData      : std_logic_vector   (31 downto 0)     ;
signal  rAvalonWriteData     : std_logic_vector   (31 downto 0)     ;

--Bus Master Signals
signal  BusMasterEnable       : std_logic                            ;
signal  BusMasterAck          : std_logic                            ;
signal  BusMasterSelect       : std_logic                            ;
signal  BusMasterError        : std_logic                            ;
signal  BusMasterDone         : std_logic                            ;
signal  BusMasterReset        : std_logic                            ;

-- Test case
constant cPeriode   : time := 10 ns;
constant cStimuliFile: string := "E:/XilinxMN/HDL_GitRepo/VHDL_IP-Cores/xilinx/axiwrapper/tb/tbAvlonMasterBhv_TB_stim.txt";
type tMemory is array (natural range <>) of std_logic_vector(cMaxBitWidth-1 downto 0);
constant cMemoryRange : natural := 10;
constant cInitMemory : tMemory(cMemoryRange-1 downto 0) := (others => (others => '0'));
signal Memory : tMemory(cMemoryRange-1 downto 0) := cInitMemory;

begin

-------------------------------------------------------------------------------
--  DUT1:  AXI lite master
-- ----------------------------------------------------------------------------
AXI_MASTER_WRAPPER: entity work.axi_lite_master_wrapper
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
-------------------------------------------------------------------------------
--  DUT2:  AXI lite slave
-- ----------------------------------------------------------------------------
AXI_LITE_SLAVE: entity work.axi_lite_slave_wrapper
generic map
    (
        C_BASEADDR          =>  C_BASEADDR          ,
        C_HIGHADDR          =>  C_HIGHADDR          ,
        C_S_AXI_ADDR_WIDTH  =>  C_AXI_ADDR_WIDTH    ,
        C_S_AXI_DATA_WIDTH  =>  C_AXI_DATA_WIDTH
    )
port map
    (
        -- System Signals
        ACLK                =>  ACLK                ,
        ARESETN             =>  ARESETN             ,
        -- Slave Interface Write Address Ports
        S_AXI_AWADDR        =>  AWADDR              ,
        S_AXI_AWPROT        =>  AWPROT              ,
        S_AXI_AWVALID       =>  AWVALID             ,
        S_AXI_AWREADY       =>  AWREADY             ,
        -- Slave Interface Write Data Ports
        S_AXI_WDATA         =>  WDATA               ,
        S_AXI_WSTRB         =>  WSTRB               ,
        S_AXI_WVALID        =>  WVALID              ,
        S_AXI_WREADY        =>  WREADY              ,
        -- Slave Interface Write Response Ports
        S_AXI_BRESP         =>  BRESP               ,
        S_AXI_BVALID        =>  BVALID              ,
        S_AXI_BREADY        =>  BREADY              ,
        -- Slave Interface Read Address Ports
        S_AXI_ARADDR        =>  ARADDR              ,
        S_AXI_ARPROT        =>  ARPROT              ,
        S_AXI_ARVALID       =>  ARVALID             ,
        S_AXI_ARREADY       =>  ARREADY             ,
        -- Slave Interface Read Data Ports
        S_AXI_RDATA         =>  RDATA               ,
        S_AXI_RRESP         =>  RRESP               ,
        S_AXI_RVALID        =>  RVALID              ,
        S_AXI_RREADY        =>  RREADY              ,
        --Avalon Interface
        oAvsAddress         =>  rAvalonAddr         ,
        oAvsByteenable      =>  rAvalonBE           ,
        oAvsRead            =>  rAvalonRead         ,
        oAvsWrite           =>  rAvalonWrite        ,
        oAvsWritedata       =>  rAvalonWriteData    ,
        iAvsReaddata        =>  rAvalonReadData     ,
        iAvsWaitrequest     =>  rAvalonWaitReq
    );

-------------------------------------------------------------------------------
--  Master Stimulus
-- ----------------------------------------------------------------------------
-- Avalon Master Write/Read operations
AVALON_BUS_MASTER:entity work.busMaster
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
-------------------------------------------------------------------------------
--  Process verification
-- ----------------------------------------------------------------------------
-- Avalon Slave Write/Read Checker
-- TODO: Better to pass to a small memory.
process (ACLK,ARESETN)
begin
 if rising_edge (ACLK) then
    if(ARESETN = '0') then
    else
        if(rAvalonRead = '1') then
            report "[SLAVE] Read Address - " & integer'image(conv_integer(std_logic_vector(rAvalonAddr)));
            report "[SLAVE] Read Address - " & integer'image(conv_integer(std_logic_vector(wAvalonReadData)));
            --TODO: place Data
        elsif (rAvalonWrite = '1') then
            report "[SLAVE] Write Address - " & integer'image(conv_integer(std_logic_vector(rAvalonAddr)));
            report "[SLAVE] Write Data  - " & integer'image(conv_integer(std_logic_vector(rAvalonWriteData)));
        else
         --TODO: Nothing
        end if;
    end if;
 end if;
end process;

process (ACLK,ARESETN)
begin
 if rising_edge (ACLK) then
    if(ARESETN = '0') then
    else
        if(rAvalonRead = '1') then
  --          rAvalonReadData <= Memory(conv_integer(std_logic_vector(rAvalonAddr)));
        elsif (rAvalonWrite = '1') then
  --            Memory(conv_integer(std_logic_vector(rAvalonAddr))) <= rAvalonWriteData ;-- after cPeriode/4;
        else
         --TODO: Nothing
        end if;
    end if;
 end if;
end process ;
-------------------------------------------------------------------------------
--  Protocol Check
-- ----------------------------------------------------------------------------
PROTOCOL_CHECKER: entity work.axi_ProtocolChecker
generic map
    (
        gAddrWidth          =>  32
    )
port map
    (
        ACLK                =>  ACLK                ,
        ARESETN             =>  ARESETN             ,
            --Write Address
        AWVALID             =>  AWVALID             ,
        AWREADY             =>  AWREADY             ,
        AWADDR              =>  AWADDR              ,
            --Write Data
        WVALID              =>  WVALID              ,
        WREADY              =>  WREADY              ,
        WDATA               =>  WDATA               ,
            --Write Response
        BVALID              =>  BVALID              ,
        BREADY              =>  BREADY              ,
        BRESP               =>  BRESP               ,
            --Read Address
        ARVALID             =>  ARVALID             ,
        ARREADY             =>  ARREADY             ,
        ARADDR              =>  ARADDR              ,
            --Read Data
        RVALID              =>  RVALID              ,
        RREADY              =>  RREADY              ,
        RDATA               =>  RDATA
     );

-------------------------------------------------------------------------------
--  General Settings
-- ----------------------------------------------------------------------------

    ARESETN         <=  '0' after 0 ns,
                        '1' after 100 ns;
    BusMasterReset  <= not ARESETN ;
    ACLK            <=   not ACLK after cPeriode/2 when BusMasterDone /= cActivated else '0' after cPeriode/2;
    BusMasterEnable <= '1' ;
    BusMasterAck	<= not wAvalonWaitReq	;

end bhv ;