-------------------------------------------------------------------------------
--! @file axi_lite_slave_wrapperRtl.vhd
--
--! @brief AXI lite slave wrapper on avalon slave
--
--! @details This will convert AXI slave interface to Avalon interface
--
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

entity axi_lite_slave_wrapper is
generic
    (
    C_BASEADDR         : std_logic_vector(31 downto 0) := x"00000000";
    C_HIGHADDR         : std_logic_vector(31 downto 0) := x"0000ffff";
    C_S_AXI_ADDR_WIDTH : integer                       := 32;
    C_S_AXI_DATA_WIDTH : integer                       := 32
    );

port
    (
    --! System Signals
    ACLK            :   in  std_logic                                        ;
    ARESETN         :   in  std_logic                                        ;
    -- Slave Interface Write Address Ports
    S_AXI_AWADDR    :   in  std_logic_vector (C_S_AXI_ADDR_WIDTH -1 downto 0);
    S_AXI_AWPROT    :   in  std_logic_vector ( 2 downto 0)                   ;
    S_AXI_AWVALID   :   in  std_logic                                        ;
    S_AXI_AWREADY   :   out std_logic                                        ;
    --! Slave Interface Write Data Ports
    S_AXI_WDATA     :   in  std_logic_vector (C_S_AXI_DATA_WIDTH-1 downto 0) ;
    S_AXI_WSTRB     :   in  std_logic_vector (C_S_AXI_DATA_WIDTH/8-1 downto 0);
    S_AXI_WVALID    :   in  std_logic                                        ;
    S_AXI_WREADY    :   out std_logic                                        ;
    --! Slave Interface Write Response Ports
    S_AXI_BRESP     :   out std_logic_vector (1 downto 0)                    ;
    S_AXI_BVALID    :   out std_logic                                        ;
    S_AXI_BREADY    :   in  std_logic                                        ;
    --! Slave Interface Read Address Ports
    S_AXI_ARADDR    :   in  std_logic_vector (C_S_AXI_ADDR_WIDTH -1 downto 0);
    S_AXI_ARPROT    :   in  std_logic_vector (2 downto 0)                    ;
    S_AXI_ARVALID   :   in  std_logic                                        ;
    S_AXI_ARREADY   :   out std_logic                                        ;
    --! Slave Interface Read Data Ports
    S_AXI_RDATA     :   out std_logic_vector (C_S_AXI_DATA_WIDTH-1 downto 0) ;
    S_AXI_RRESP     :   out std_logic_vector (1 downto 0)                    ;
    S_AXI_RVALID    :   out std_logic                                        ;
    S_AXI_RREADY    :   in  std_logic                                        ;
    --! Avalon Interface
    oAvsAddress      :   out std_logic_vector    (31 downto 0)   ;
    oAvsByteenable   :   out std_logic_vector    (3 downto 0)    ;
    oAvsRead         :   out std_logic  ;
    oAvsWrite        :   out std_logic  ;
    oAvsWritedata    :   out std_logic_vector   (31 downto 0);
    iAvsReaddata     :   in  std_logic_vector   (31 downto 0);
    iAvsWaitrequest  :   in  std_logic
    );

end axi_lite_slave_wrapper;

architecture implementation of axi_lite_slave_wrapper is


type state is (sIDLE,sREAD,sREAD_DONE,sWRITE,sWRITE_DONE,sWRRES_DONE,sDELAY) ;


--Avalon Interface designs
signal  address      :   std_logic_vector(31 downto 0)   ;
signal  chip_sel     :   std_logic                       ;
signal  byte_enable  :   std_logic_vector(3 downto 0)    ;

--Signals for FSM
signal  StateCurrent   :  state     ;
signal  StateNext      :  state     ;

signal  BvalidCurrent  :   std_logic   ;
signal  AwreadyCurrent :   std_logic   ;
signal  ArreadyCurrent :   std_logic   ;
signal  WreadyCurrent  :   std_logic   ;
signal  RvalidCurrent  :   std_logic   ;

signal  BvalidNext     :   std_logic   ;
signal  AwreadyNext    :   std_logic   ;
signal  ArreadyNext    :   std_logic   ;
signal  WreadyNext     :   std_logic   ;
signal  RvalidNext     :   std_logic   ;

--Internal Signals
signal avalonRead : std_logic := '0';
signal avalonReadDataLatch : std_logic_vector (31 downto 0) := x"00000000" ;

signal axiWriteData : std_logic_vector (31 downto 0) := x"00000000" ;
signal axiDataValid : std_logic := '0' ;

signal writeStart : std_logic ;
signal readStart  : std_logic ;
signal avalonDataValid : std_logic ;
signal AvsWaitrequest_d : std_logic ;

signal read_sel : std_logic;
signal write_sel : std_logic;

begin

-- TODO: Check weather we need to add clock sync to make sure data & control signal crossing should be in same clock domains

--Avalon Slave Interface Singals
oAvsAddress      <=  address             ;
oAvsByteenable   <=  byte_enable         ;

oAvsRead           <= avalonRead ;
avalonRead         <= '1' when (readStart = '1'  and (StateCurrent = sIDLE)) else
                      --'1' when (iAvsWaitrequest = '1' and (StateCurrent = sREAD)) else
                      '1' when (StateCurrent = sREAD) else
                      '0' when (StateCurrent = sREAD_DONE) else
                      '0' ;

oAvsWrite        <=  '1' when ((StateCurrent = sWRITE) and (S_AXI_WVALID = '1')) else
                     '1' when ((StateCurrent = sIDLE) and (axiDataValid = '1')) else
                     --'1' when ((StateCurrent = sWRITE_DONE) and (iAvsWaitrequest = '1')) else
                     '1' when (StateCurrent = sWRITE_DONE) else
                     '0' ;

oAvsWritedata    <= axiWriteData ;
axiWriteData    <= S_AXI_WDATA when (axiDataValid = '1') else
                   axiWriteData;


-- AXI Lite Write Data Signals

S_AXI_BVALID     <= '1' when ((StateCurrent = sWRITE_DONE) and (iAvsWaitrequest = '0')) else
                    '1' when ((StateCurrent = sWRRES_DONE )) else
                    '0'    ;

S_AXI_AWREADY    <= '1' when ((StateCurrent = sIDLE) and (writeStart = '1')) else
                    '0' ;
S_AXI_WREADY     <= '1' when (StateCurrent = sWRITE) else
                    '1' when ((StateCurrent = sIDLE) and (axiDataValid = '1')) else
                    '0' ;

-- AXI lite Read Data Signals
S_AXI_ARREADY    <=   '1' when (StateCurrent = sIDLE and (readStart = '1')) else
                      '0' ;

S_AXI_RVALID     <=  '1' when ((iAvsWaitrequest = '0') and (StateCurrent = sREAD)) else
                     --'1' when ((S_AXI_RREADY = '0') and (StateCurrent = sREAD_DONE)) else
                     '1' when ((StateCurrent = sREAD_DONE)) else
                     '0' ;

S_AXI_RDATA         <= avalonReadDataLatch ;
avalonReadDataLatch <= iAvsReaddata when (iAvsWaitrequest = '0') else
                       avalonReadDataLatch ;

S_AXI_BRESP      <=   "00"        ;   --always OK
S_AXI_RRESP      <=   "00"        ;   --always ok

-- Address Decoder
chip_sel <= read_sel or write_sel ;

write_sel <= '1' when ( (S_AXI_AWADDR >=  C_BASEADDR ) and
                       (S_AXI_AWADDR <= C_HIGHADDR)) else
             '0' ;

read_sel <= '1' when ( (S_AXI_ARADDR >=  C_BASEADDR ) and
                       (S_AXI_ARADDR <= C_HIGHADDR)) else
            '0' ;

address <= S_AXI_ARADDR when (readStart = '1'  and (StateCurrent = sIDLE)) else
           S_AXI_AWADDR when (writeStart = '1' and (StateCurrent = sIDLE)) else
           address ;

--Combinational Logic for FSM
writeStart <= chip_sel and S_AXI_AWVALID ;
--writeDone
readStart  <= chip_sel and S_AXI_ARVALID ;
--readDone
axiDataValid <= S_AXI_WVALID ;

--byte_enable <= S_AXI_WSTRB when ((chip_sel and S_AXI_AWVALID) = '1' ) else
--               x"F"        when ((chip_sel and S_AXI_ARVALID) = '1' ) else
--               byte_enable ;

byte_enable <= x"F"  when (readStart = '1'  and (StateCurrent = sIDLE)) else
               S_AXI_WSTRB when (writeStart = '1' and (StateCurrent = sIDLE)) else
               byte_enable ;


SEQ_LOGIC_FSM:
process (ACLK)
    begin
    if(rising_edge(ACLK))  then
        if( ARESETN = '0' )   then
            StateCurrent     <= sIDLE ;
        else
            StateCurrent     <= StateNext ;
        end if;
    end if;
 end process;

COM_LOGIC_FSM:
    process (
           StateCurrent ,
           chip_sel     ,
           writeStart,
           readStart,
           S_AXI_AWVALID,
           S_AXI_ARVALID,
           S_AXI_RREADY ,
           S_AXI_WVALID,
           S_AXI_BREADY,
           iAvsWaitrequest
          --testDelayCntCurrent
        )
    begin
    --Default values
    StateNext       <=  StateCurrent    ;

    case (StateCurrent) is
            when sIDLE =>
              if(chip_sel = '1' ) then
              --Latch Address
                   --Write Operations
                  if(S_AXI_AWVALID    =   '1') then
                   if(S_AXI_WVALID = '1') then
                    if(iAvsWaitrequest = '0') then
                     StateNext <= sWRRES_DONE ;
                    else
                     StateNext <= sWRITE_DONE ;
                    end if;
                   else
                    StateNext <= sWRITE ;
                   end if;
                   --Read Operations
                  elsif (S_AXI_ARVALID    =   '1') then
                       if(iAvsWaitrequest = '0') then
                          StateNext     <= sREAD_DONE ;
                      else
                        StateNext     <= sREAD ;
                      end if;
                  else
                   StateNext  <= sIDLE ;
                  end if;
              else
                   StateNext <= sIDLE ;
              end if;

            when sREAD =>
               -- Read Valid gets assert Here
               if(iAvsWaitrequest = '0') then
                 if( S_AXI_RREADY = '1' ) then
                  StateNext     <= sIDLE ;
                 else
                  StateNext     <= sREAD_DONE ;
                 end if;
              else
                StateNext     <= sREAD ;
              end if;

             when sREAD_DONE =>
                if( S_AXI_RREADY = '1' ) then
                 StateNext     <= sIDLE ;
                else
                 StateNext     <= sREAD_DONE ;
                end if;

             when sWRITE =>
               if(S_AXI_WVALID = '1' ) then
                   if(iAvsWaitrequest = '0') then
                       if(S_AXI_BREADY = '1') then
                        StateNext     <= sIDLE ;
                       else
                        StateNext     <= sWRRES_DONE ;
                       end if;
                    else
                        StateNext     <= sWRITE_DONE ;
                    end if;
               else
                  StateNext     <= sWRITE ;
               end if;

             when sWRITE_DONE =>
                if(iAvsWaitrequest = '0') then
                   if(S_AXI_BREADY = '1') then
                    StateNext     <= sIDLE ;
                   else
                    StateNext     <= sWRRES_DONE ;
                   end if;
                else
                    StateNext     <= sWRITE_DONE ;
                end if;

             when sWRRES_DONE =>

                if(S_AXI_BREADY = '1') then
                    StateNext     <= sIDLE ;
                else
                    StateNext     <= sWRRES_DONE ;
                end if;

              when sDELAY =>
               StateNext <= sIDLE ;

            when others => null ;

        end case;
    end process;

end implementation;