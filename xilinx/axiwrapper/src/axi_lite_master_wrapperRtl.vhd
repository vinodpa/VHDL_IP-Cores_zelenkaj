-------------------------------------------------------------------------------
--! @file axi_lite_master_wrapperRtl.vhd
--
--! @brief AXI lite master wrapper on avalon master
--
--! @details This will convert avalon master interface to AXI master interface
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
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity axi_lite_master_wrapper is
generic
    (
    C_M_AXI_ADDR_WIDTH : integer                       := 32;
    C_M_AXI_DATA_WIDTH : integer                       := 32
    );

port
    (
    --! System Signals
    M_AXI_ACLK     : in  std_logic                                    ;
    M_AXI_ARESETN  : in  std_logic                                    ;

    --! Master Interface Write Address
    M_AXI_AWADDR   : out std_logic_vector  (C_M_AXI_ADDR_WIDTH-1 downto 0)   ;
    M_AXI_AWPROT   : out std_logic_vector  (3-1 downto 0)                    ;
    M_AXI_AWVALID  : out std_logic                                           ;
    M_AXI_AWREADY  : in  std_logic                                           ;

    --! Master Interface Write Data
    M_AXI_WDATA    : out std_logic_vector  (C_M_AXI_DATA_WIDTH-1 downto 0)   ;
    M_AXI_WSTRB    : out std_logic_vector  (C_M_AXI_DATA_WIDTH/8-1 downto 0) ;
    M_AXI_WVALID   : out std_logic                                           ;
    M_AXI_WREADY   : in  std_logic                                           ;
    --TODO: AXI4 Signal added Cross check this part
    M_AXI_WLAST    : out std_logic                                           ;

    --! Master Interface Write Response
    M_AXI_BRESP    : in  std_logic_vector  (2-1 downto 0)                    ;
    M_AXI_BVALID   : in  std_logic                                           ;
    M_AXI_BREADY   : out std_logic                                           ;

    --! Master Interface Read Address
    M_AXI_ARADDR   : out std_logic_vector (C_M_AXI_ADDR_WIDTH-1 downto 0)    ;
    M_AXI_ARPROT   : out std_logic_vector (3-1 downto 0)                     ;
    M_AXI_ARVALID  : out std_logic                                           ;
    M_AXI_ARREADY  : in  std_logic                                           ;

    --! Master Interface Read Data
    M_AXI_RDATA    : in  std_logic_vector (C_M_AXI_DATA_WIDTH-1 downto 0)    ;
    M_AXI_RRESP    : in  std_logic_vector (2-1 downto 0)                     ;
    M_AXI_RVALID   : in  std_logic                                           ;
    M_AXI_RREADY   : out std_logic                                           ;
    --! Avalon master bus
    iAvalonRead    : in  std_logic                                           ;
    iAvalonWrite   : in  std_logic                                           ;
    iAvalonAddr    : in  std_logic_vector (31 downto 0)                      ;
    iAvalonBE      : in  std_logic_vector (3 downto 0)                       ;
    oAvalonWaitReq : out std_logic                                           ;
    oAvalonReadValid : out std_logic                                         ;
    oAvalonReadData : out std_logic_vector   (31 downto 0)                   ;
    iAvalonWriteData : in std_logic_vector   (31 downto 0)

    --test Ports
    --test_mstate      : out std_logic_vector (3 downto 0)
    );

end axi_lite_master_wrapper;

architecture Behavioral of axi_lite_master_wrapper is

type state is (sINIT,sAWVALID,sWVALID,sBREADY,sARVALID,sRREADY,sWRITE_DONE,sREAD_DONE);
signal  StateCurrent    :    state      ;
signal  StateNext       :    state      ;

--  Handle Avalon Master
signal  start_transfer  :    std_logic  ;
signal  done_transfer   :    std_logic  ;
signal  RReady			:    std_logic  ;
signal	rd_done			:    std_logic  ;
signal  writeOp_done    :    std_logic  ;
signal  readOp_done     :    std_logic  ;

signal  readData        :   std_logic_vector (31 downto 0) := x"00000000";

begin

--test_mstate  <= x"0" when StateCurrent  = sINIT else
--                x"1" when StateCurrent  = sAWVALID else
--                x"2" when StateCurrent  = sWVALID else
--                x"3" when StateCurrent  = sBREADY else
--                x"4" when StateCurrent  = sARVALID else
--               x"5" when StateCurrent  = sRREADY else
--                x"6" when StateCurrent  = sWRITE_DONE else
--                x"7" when StateCurrent  = sREAD_DONE else
--                x"F" ;
--AXI Master Operations

    M_AXI_AWPROT    <= "000"   ;
    M_AXI_ARPROT    <= "000"    ;

    M_AXI_AWADDR    <= iAvalonAddr ;
    M_AXI_ARADDR    <= iAvalonAddr ;
    M_AXI_WDATA     <= iAvalonWriteData ;

    -- TODO: FIX Read Strobe also (against the protocol)
    M_AXI_WSTRB     <= iAvalonBE  ;

    M_AXI_AWVALID   <=  '1' when ((StateCurrent = sINIT) and (iAvalonWrite = '1')) else
                        '1' when StateCurrent = sAWVALID else
                        '0';

    M_AXI_WVALID    <=  '1' when ((StateCurrent = sINIT) and (iAvalonWrite = '1')) else
                        '1' when StateCurrent = sAWVALID else
                        '1' when StateCurrent = sWVALID else
                        '0';

    M_AXI_BREADY    <=  '1' when StateCurrent = sWRITE_DONE else
                        '1' when StateCurrent = sBREADY else
                        '0';

    M_AXI_WLAST     <=  '1' ;

    M_AXI_ARVALID   <=  '1' when iAvalonRead = '1' else
                        '1' when StateCurrent =  sARVALID else
                        '0' ;
    M_AXI_RREADY    <=  '1' when StateCurrent = sREAD_DONE else
                        '0';

    oAvalonReadValid <=  M_AXI_RVALID    ;

    oAvalonReadData  <=  readData ;
    readData         <=  M_AXI_RDATA when  M_AXI_RVALID = '1' else
                             readData ;


    oAvalonWaitReq <=   '1' when  start_transfer = '1' else
                        '0' when done_transfer = '1' else
                        '0' ;

    RReady		   <=   '1' when StateCurrent = sREAD_DONE else
                        '0' ;

    start_transfer <=   '0' when done_transfer = '1' else
                         (iAvalonRead and not RReady) or iAvalonWrite      ;

    done_transfer  <=   writeOp_done or readOp_done ;
    writeOp_done   <=   '1' when (StateCurrent = sWRITE_DONE) else
                        '0' ;
    readOp_done    <=   '1' when (StateCurrent = sREAD_DONE) else
                        '0' ;

-- Master FSM
-- Sequenctial Logics
    process (M_AXI_ACLK, M_AXI_ARESETN)
    begin
     if rising_edge (M_AXI_ACLK) then
      if(M_AXI_ARESETN = '0') then
        StateCurrent <= sINIT    ;
      else
        StateCurrent <= StateNext ;
      end if;
     end if;
    end process;
-- Combinational Logics
    process (
               StateCurrent,
               iAvalonRead,
               iAvalonWrite,
               M_AXI_AWREADY,
               M_AXI_WREADY,
               M_AXI_BVALID,
               M_AXI_ARREADY,
               M_AXI_RVALID
            )
    begin
        StateNext <= StateCurrent ;
        case (StateCurrent) is
         when sINIT =>
            if (iAvalonRead = '1') then
                StateNext   <= sARVALID ;
                --Vinod Crazy
                if(M_AXI_ARREADY = '1') then
                    if(M_AXI_RVALID = '1') then
                        StateNext   <= sREAD_DONE ;
                    else
                        StateNext   <= sRREADY ;
                    end if;
                else
                    StateNext   <= sARVALID ;
                end if;
                -- Ends here
          elsif (iAvalonWrite = '1') then
                StateNext   <= sAWVALID ;
                --Vinod Crazy
                if(M_AXI_AWREADY = '1') then
                    if (M_AXI_WREADY = '1') then
                        if (M_AXI_BVALID = '1') then
                            StateNext   <= sWRITE_DONE ;
                        else
                            StateNext   <= sBREADY ;
                        end if;
                    else
                        StateNext   <= sWVALID ;
                    end if;
                else
                    StateNext   <= sAWVALID ;
                end if;
                --Craze ends here
            else
                StateNext <= sINIT ;
            end if;
         when sAWVALID =>
            if(M_AXI_AWREADY = '1') then
                if (M_AXI_WREADY = '1') then
                    if (M_AXI_BVALID = '1') then
                        StateNext   <= sWRITE_DONE ;
                    else
                        StateNext   <= sBREADY ;
                    end if;
                else
                    StateNext   <= sWVALID ;
                end if;
            else
                StateNext   <= sAWVALID ;
            end if;

         when sWVALID  =>
            if (M_AXI_WREADY = '1') then
                if (M_AXI_BVALID = '1') then
                StateNext   <= sWRITE_DONE ;
                else
                StateNext   <= sBREADY ;
                end if;
            else
                StateNext   <= sWVALID ;
            end if;
         when sBREADY  =>
            if (M_AXI_BVALID = '1') then
                StateNext   <= sWRITE_DONE ;
            else
                StateNext   <= sBREADY ;
            end if;
         when sARVALID =>
            if(M_AXI_ARREADY = '1') then
                if(M_AXI_RVALID = '1') then
                    StateNext   <= sREAD_DONE ;
                else
                    StateNext   <= sRREADY ;
                end if;
            else
                StateNext   <= sARVALID ;
            end if;
         when sRREADY  =>
            if(M_AXI_RVALID = '1') then
                StateNext   <= sREAD_DONE ;
            else
                StateNext   <= sRREADY ;
            end if;
         when sWRITE_DONE    =>
                StateNext <= sINIT ;
         when sREAD_DONE    =>
                StateNext <= sINIT ;
         when others => null;
        end case;
    end process;

end Behavioral;