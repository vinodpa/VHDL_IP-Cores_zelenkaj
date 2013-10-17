-------------------------------------------------------------------------------
-- RMII to MII converter
-- ex: openMAC - openHUB - RMII2MII - MII PHY
--
--       Copyright (C) 2009 B&R
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
-- Note: Used DPR is specific to Altera/Xilinx. Use one of the following files:
--       OpenMAC_DPR_Altera.vhd
--       OpenMAC_DPR_Xilinx.vhd
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity rmii2mii is
    port (
        clk50                : in     std_logic; --used by RMII as well!!!
        rst                    : in     std_logic;
        --RMII (MAC)
        rTxEn                : in    std_logic;
        rTxDat                : in     std_logic_vector(1 downto 0);
        rRxDv                : out    std_logic;
        rRxDat                : out    std_logic_vector(1 downto 0);
        rRxEr                : out    std_logic;
        --MII (PHY)
        mTxEn                : out    std_logic;
        mTxDat                : out    std_logic_vector(3 downto 0);
        mTxClk                : in    std_logic;
        mRxDv                : in    std_logic;
        mRxEr                : in    std_logic;
        mRxDat                : in    std_logic_vector(3 downto 0);
        mRxClk                : in    std_logic
    );
end rmii2mii;

architecture rtl of rmii2mii is
    constant DIBIT_SIZE : integer := 2;
    constant NIBBLE_SIZE : integer := 4;
begin

    TX_BLOCK : block
        --fifo size must not be larger than 2**5
        constant FIFO_NIBBLES_LOG2 : integer := 5;

        signal fifo_half, fifo_full, fifo_empty, fifo_valid, fifo_valid_l, fifo_wrempty : std_logic;
        signal fifo_wr, fifo_rd : std_logic;
        signal fifo_din : std_logic_vector(NIBBLE_SIZE-1 downto 0);
        signal fifo_dout, fifo_dout_l : std_logic_vector(NIBBLE_SIZE-1 downto 0);
        signal fifo_rdUsedWord : std_logic_vector (FIFO_NIBBLES_LOG2-1 downto 0);
        signal fifo_wrUsedWord : std_logic_vector (FIFO_NIBBLES_LOG2-1 downto 0);
        --necessary for clr fifo
        signal aclr, rTxEn_l : std_logic;

        --convert dibits to nibble
        signal sel_dibit : std_logic;
        signal fifo_din_reg : std_logic_vector(rTxDat'range);
    begin

        fifo_din <= rTxDat & fifo_din_reg;
        fifo_wr <= sel_dibit;

        --convert dibits to nibble (to fit to fifo)
        process(clk50, rst)
        begin
            if rst = '1' then
                sel_dibit <= '0';
                fifo_din_reg <= (others => '0');
            elsif clk50 = '1' and clk50'event then
                if rTxEn = '1' then
                    sel_dibit <= not sel_dibit;
                    if sel_dibit = '0' then
                        fifo_din_reg <= rTxDat;
                    end if;
                else
                    sel_dibit <= '0';
                end if;
            end if;
        end process;

        fifo_half <= fifo_rdUsedWord(fifo_rdUsedWord'left);

        mTxDat <= fifo_dout_l;
        mTxEn <= fifo_valid_l;

        process(mTxClk, rst)
        begin
            if rst = '1' then
                fifo_rd <= '0';
                fifo_valid <= '0';
                fifo_dout_l <= (others => '0');
                fifo_valid_l <= '0';
            elsif mTxClk = '1' and mTxClk'event then
                fifo_dout_l <= fifo_dout;
                fifo_valid_l <= fifo_valid;

                if fifo_rd = '0' and fifo_half = '1' then
                    fifo_rd <= '1';
                elsif fifo_rd = '1' and fifo_empty = '1' then
                    fifo_rd <= '0';
                end if;

                if fifo_rd = '1' and fifo_rdUsedWord > conv_std_logic_vector(1, fifo_rdUsedWord'length) then
                    fifo_valid <= '1';
                else
                    fifo_valid <= '0';
                end if;
            end if;
        end process;

        --! This is the asynchronous FIFO used to decouple RMII from MII.
        TXFIFO : entity work.asyncFifo
            generic map (
                gDataWidth  => NIBBLE_SIZE,
                gWordSize   => 2**FIFO_NIBBLES_LOG2,
                gSyncStages => 2,
                gMemRes     => "ON"
            )
            port map (
                iAclr       => aclr,
                iWrClk      => clk50,
                iWrReq      => fifo_wr,
                iWrData     => fifo_din,
                oWrEmpty    => fifo_wrempty,
                oWrFull     => fifo_full,
                oWrUsedw    => fifo_wrUsedWord,
                iRdClk      => mTxClk,
                iRdReq      => fifo_rd,
                oRdData     => fifo_dout,
                oRdEmpty    => fifo_empty,
                oRdFull     => open,
                oRdUsedw    => fifo_rdUsedWord
            );

        --sync Mii Tx En (=fifo_valid) to wr clk
        process(clk50, rst)
        begin
            if rst = '1' then
                aclr <= '1'; --reset fifo
                rTxEn_l <= '0';
            elsif clk50 = '1' and clk50'event then
                rTxEn_l <= rTxEn;

                aclr <= '0'; --default

                --clear the full fifo after TX on RMII side is done
                if fifo_full = '1' and rTxEn_l = '1' and rTxEn = '0' then
                    aclr <= '1';
                end if;
            end if;
        end process;

    end block;

    RX_BLOCK : block
        --fifo size must not be larger than 2**5
        constant FIFO_NIBBLES_LOG2 : integer := 5;

        signal fifo_half, fifo_full, fifo_empty, fifo_valid : std_logic;
        signal fifo_wr, fifo_rd : std_logic;
        signal fifo_din : std_logic_vector(NIBBLE_SIZE-1 downto 0);
        signal fifo_dout : std_logic_vector(NIBBLE_SIZE-1 downto 0);
        signal fifo_rdUsedWord : std_logic_vector(FIFO_NIBBLES_LOG2-1 downto 0);
        signal fifo_wrUsedWord : std_logic_vector(FIFO_NIBBLES_LOG2-1 downto 0);
        --convert nibble to dibits
        signal sel_dibit : std_logic;
        signal fifo_rd_s : std_logic;
    begin


        process(mRxClk, rst)
        begin
            if rst = '1' then
                fifo_din <= (others => '0');
                fifo_wr <= '0';
            elsif mRxClk = '1' and mRxClk'event then
                fifo_din <= mRxDat;
                fifo_wr <= mRxDv and not mRxEr;
            end if;
        end process;

        rRxDat <=     fifo_dout(fifo_dout'right+1 downto 0) when sel_dibit = '1' else
                    fifo_dout(fifo_dout'left downto fifo_dout'left-1);

        rRxDv <= fifo_valid;
        fifo_rd <= fifo_rd_s and not sel_dibit;

        process(clk50, rst)
        begin
            if rst = '1' then
                sel_dibit <= '0';
            elsif clk50 = '1' and clk50'event then
                if fifo_rd_s = '1' or fifo_valid = '1' then
                    sel_dibit <= not sel_dibit;
                else
                    sel_dibit <= '0';
                end if;
            end if;
        end process;

        fifo_half <= fifo_rdUsedWord(fifo_rdUsedWord'left);

        rRxEr <= '0';

        process(clk50, rst)
        begin
            if rst = '1' then
                fifo_rd_s <= '0';
                fifo_valid <= '0';
            elsif clk50 = '1' and clk50'event then
                if fifo_rd_s = '0' and fifo_half = '1' then
                    fifo_rd_s <= '1';
                elsif fifo_rd_s = '1'  and fifo_empty = '1' then
                    fifo_rd_s <= '0';
                end if;

                if fifo_rd_s = '1' then
                    fifo_valid <= '1';
                else
                    fifo_valid <= '0';
                end if;
            end if;
        end process;

        --! This is the asynchronous FIFO used to decouple RMII from MII.
        RXFIFO : entity work.asyncFifo
            generic map (
                gDataWidth  => NIBBLE_SIZE,
                gWordSize   => 2**FIFO_NIBBLES_LOG2,
                gSyncStages => 2,
                gMemRes     => "ON"
            )
            port map (
                iAclr       => rst,
                iWrClk      => mRxClk,
                iWrReq      => fifo_wr,
                iWrData     => fifo_din,
                oWrEmpty    => open,
                oWrFull     => fifo_full,
                oWrUsedw    => fifo_wrUsedWord,
                iRdClk      => clk50,
                iRdReq      => fifo_rd,
                oRdData     => fifo_dout,
                oRdEmpty    => fifo_empty,
                oRdFull     => open,
                oRdUsedw    => fifo_rdUsedWord
            );
    end block;
end rtl;
