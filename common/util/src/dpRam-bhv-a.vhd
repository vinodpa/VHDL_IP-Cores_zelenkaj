-------------------------------------------------------------------------------
--! @file dpRam-bhv-a.vhd
--
--! @brief Dual Port Ram Behavioural Architecture
--
--! @details This is the DPRAM model intended for simulation only.
--!          Timing as follows [clk-cycles]: write=0 / read=1
--
-------------------------------------------------------------------------------
--
--    (c) B&R, 2013
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

--! use global library
use work.global.all;

architecture bhv of dpRam is
    --! RAM type
    type tRam is array (natural range <>) of
        std_logic_vector(gWordWidth-1 downto 0);

    --! RAM array
    shared variable vDpram : tRam(gNumberOfWords-1 downto 0) :=
        (others => (others => cInactivated));

    --! Dpram procedure - to be called within rising clock edge
    procedure dpram (
        variable vRam   : inout tRam;
        signal iAddr    : in std_logic_vector;
        signal iEn      : in std_logic;
        signal iBe      : in std_logic_vector;
        signal iWe      : in std_logic;
        signal iWrData  : in std_logic_vector;
        signal oRdData  : out std_logic_vector
    ) is
        variable vAddr : natural;
    begin
        vAddr := to_integer(unsigned(iAddr));

        -- handle too high addresses
        assert (vAddr < gNumberOfWords)
        report  "Address exceeds memory (addr=" & integer'image(vAddr) & " | max=" & integer'image(gNumberOfWords) & ")"
        severity failure;

        if vAddr >= gNumberOfWords then
            vAddr := 0;
        end if;

        if iEn = cActivated then
            -- write to dpram
            if iWe = cActivated then
                -- bytewise...
                for i in iBe'range loop
                    if iBe(i) = cActivated then
                        vRam(vAddr)((i+1)*8-1 downto i*8) := iWrData((i+1)*8-1 downto i*8);
                    end if;
                end loop;
            end if;
            -- read from dpram
            oRdData <= vRam(vAddr);
        end if;
    end procedure;
begin
    assert (gInitFile = "unused")
    report "Memory initialization is not supported in this architecture!"
    severity warning;

    portAProc : process(iClk_A)
    begin
        if rising_edge(iClk_A) then
            dpram (
                vRam    => vDpram,
                iAddr   => iAddress_A,
                iEn     => iEnable_A,
                iBe     => iByteenable_A,
                iWe     => iWriteEnable_A,
                iWrData => iWritedata_A,
                oRdData => oReaddata_A
            );
        end if;
    end process;

    portBProc : process(iClk_B)
    begin
        if rising_edge(iClk_B) then
            dpram (
                vRam    => vDpram,
                iAddr   => iAddress_B,
                iEn     => iEnable_B,
                iBe     => iByteenable_B,
                iWe     => iWriteEnable_B,
                iWrData => iWritedata_B,
                oRdData => oReaddata_B
            );
        end if;
    end process;
end bhv;
