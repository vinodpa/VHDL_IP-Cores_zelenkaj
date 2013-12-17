-------------------------------------------------------------------------------
--! @file axi_protocol_checker.vhd
--
--! @brief Monitor AXI bus functionaliy
--
--! @details Monitor AXI bus Control & data flow functionality
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
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;


entity axi_ProtocolChecker is
    generic (
      gAddrWidth      : integer := 32
      );
  port	(
      ACLK    : in std_logic ;
      ARESETN : in std_logic ;
      --! Write Address
      AWVALID : in std_logic ;
      AWREADY : in std_logic ;
      AWADDR  : in std_logic_vector (31 downto 0);
      --! Write Data
      WVALID	: in std_logic ;
      WREADY 	: in std_logic ;
      WDATA	: in std_logic_vector (31 downto 0);
      --! Write Response
      BVALID	: in std_logic ;
      BREADY	: in std_logic ;
      BRESP 	: in std_logic_vector (1 downto 0);
      --! Read Address
      ARVALID : in std_logic ;
      ARREADY	: in std_logic ;
      ARADDR	: in std_logic_vector (31 downto 0);
      --! Read Data
      RVALID	: in std_logic ;
      RREADY	: in std_logic ;
      RDATA	: in std_logic_vector (31 downto 0)

      );
end axi_ProtocolChecker ;

architecture bhv of axi_ProtocolChecker is

begin

-- Write Address Decoder
process (ACLK,ARESETN)
begin
  if rising_edge (ACLK) then
    if(ARESETN = '0') then
    elsif ( (AWVALID and AWREADY) = '1') then
     report "[PROTOCOL] Write Address - " & integer'image(conv_integer(std_logic_vector(AWADDR)));
    end if;
  end if;
end process;
-- Write Data
process (ACLK,ARESETN)
begin
  if rising_edge (ACLK) then
    if(ARESETN = '0') then
    elsif ((WVALID and WREADY) = '1') then
     report "[PROTOCOL] Write Address - " & integer'image(conv_integer(std_logic_vector(WDATA)));
    end if;
  end if;
end process;
-- Write Response
process (ACLK,ARESETN)
begin
  if rising_edge (ACLK) then
    if(ARESETN = '0') then
    elsif ((BVALID and BREADY) = '1') then
     report "[PROTOCOL] Write Address - " & integer'image(conv_integer(std_logic_vector(BRESP)));
    end if;
  end if ;
end process;
-- Read Address
process (ACLK,ARESETN)
begin
  if rising_edge (ACLK) then
    if(ARESETN = '0') then
    elsif ((ARVALID and ARREADY) = '1') then
     report "[PROTOCOL] Write Address - " & integer'image(conv_integer(std_logic_vector(ARADDR)));
    end if;
  end if ;
end process;
-- Read Data
process (ACLK,ARESETN)
begin
  if rising_edge (ACLK) then
    if(ARESETN = '0') then
    elsif ((RVALID and RREADY) = '1') then
     report "[PROTOCOL] Write Address - " & integer'image(conv_integer(std_logic_vector(RDATA)));
    end if;
  end if;
end process;

-- TODO:
-- Check the Read & Write Flow is correct or not
-- Write Flow: - valid address --> valid data --> valid response
-- Read Flow:- Valida Address --> Valid  Data with Response.
end bhv ;