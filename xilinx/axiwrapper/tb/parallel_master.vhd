library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_signed.all;
--use ieee.std_logic_unsigned.all;
--use ieee.arith.all ;
--! use global library
use work.global.all;
--! use host interface package for specific types
use work.hostInterfacePkg.all;

entity parallel_master is
    generic (
        --! Data bus width
        gDataWidth  : natural := 16;
        --! Address and Data bus are multiplexed (0 = FALSE, otherwise = TRUE)
        gMultiplex  : natural := 0
    );

port (

        -- Avalon Interface
       iAvalonAddr      :   in std_logic_vector    (31 downto 0)   ;
       iAvalonBE   :   in std_logic_vector    (3 downto 0)    ;
       iAvalonRead         :   in std_logic                         ;
       iAvalonWrite        :   in std_logic                         ;
       iAvalonWriteData    :   in std_logic_vector   (31 downto 0)  ;
       oAvalonReadData     :   out  std_logic_vector   (31 downto 0);
       oAvalonReadValid    :  out std_logic ;
       oAvalonWaitReq  :   out  std_logic                       ;
       -- Parallel Interface
       --! Chipselect
       oParHostChipselect          : out std_logic := cInactivated;
       --! Read strobe
       oParHostRead                : out std_logic := cInactivated;
       --! Write strobe
       oParHostWrite               : out std_logic := cInactivated;
       --! Address Latch enable (Multiplexed only)
       oParHostAddressLatchEnable  : out std_logic := cInactivated;
       --! High active Acknowledge
       iParHostAcknowledge         : in std_logic := cInactivated;
       --! Byteenables
       oParHostByteenable          : out std_logic_vector(gDataWidth/cByte-1 downto 0) := (others => cInactivated);
       --! Address bus (Demultiplexed, word-address)
       oParHostAddress             : out std_logic_vector(15 downto 0) := (others => cInactivated);
       --! Data bus out (Demultiplexed)
       iParHostData                : in std_logic_vector(gDataWidth-1 downto 0) := (others => cInactivated);
       --! Data bus in (Demultiplexed)
       oParHostData                : out std_logic_vector(gDataWidth-1 downto 0) := (others => cInactivated);
       --! Data bus outenable (Demultiplexed)
       iParHostDataEnable          : in std_logic;
       --! Address/Data bus out (Multiplexed, word-address))
       iParHostAddressData         : in std_logic_vector(gDataWidth-1 downto 0) := (others => cInactivated);
       --! Address/Data bus in (Multiplexed, word-address))
       oParHostAddressData         : out std_logic_vector(gDataWidth-1 downto 0) := (others => cInactivated);
       --! Address/Data bus outenable (Multiplexed, word-address))
       iParHostAddressDataEnable   : in std_logic;
       -- Clock/Reset sources
       --! Clock Source input
       iClk                        : in std_logic:= cInactivated;
       --! Reset Source input
       iRst                        : in std_logic:= cInactivated

    );
end parallel_master;

architecture rtl of parallel_master is

type state  is (sINIT,sWRITE,sREAD,sIDLE,sSTOP);
signal  StateCurrent    :    state      ;
signal  StateNext       :    state      ;
--SIGNALS
signal latchaddress :  std_logic_vector(gDataWidth-1 downto 0) ;
signal latchdata :  std_logic_vector(gDataWidth-1 downto 0) ;
signal temp,temp1 : std_logic_vector(gDataWidth-1 downto 0) ;

--signal noReadWrite : std_logic_vector (31 downto 0):= x"00000000";
signal Count	 : std_logic_vector (31 downto 0) := x"00000000";
signal Addr : std_logic_vector (15 downto 0) := x"0000" ;
signal Data : std_logic_vector (15 downto 0) := x"0000" ;

begin
latchaddress <= x"00ee";
latchdata<=x"1234";
temp <= latchaddress;
temp1 <= latchdata;

--noReadWrite <= x"00000010" ;
-- Master FSM
-- Sequenctial Logics
process (iClk,iRst)
begin
 if rising_edge (iClk) then
  if(iRst = '1') then
    StateCurrent <= sINIT    ;
  else
    StateCurrent <= StateNext ;
  end if;
 end if;
end process;
-- Combinational Logics
    process (
               iAvalonRead,
               iAvalonWrite,
               iAvalonAddr,
               iAvalonWriteData,
               StateCurrent,
               iParHostAcknowledge,
               iParHostData,
               iParHostDataEnable,
               iParHostAddressData,
               iParHostAddressDataEnable,
               temp,
               temp1

            )
    begin
        StateNext <= StateCurrent ;
        case (StateCurrent) is
         when sINIT =>
                    oParHostChipselect <= '0';
                    oParHostRead <= '0';
                    oParHostWrite <= '0';
                    oParHostAddress <= iAvalonAddr (15 downto 0);
                    oParHostByteenable <= "00";
                    oParHostData <= x"0000";
                    oParHostChipselect <= '0';
                    oParHostWrite <= '0';
                    oParHostAddressData <= iAvalonAddr (15 downto 0);
                    oParHostAddressLatchEnable <= '0';

                    if(iAvalonRead = '1') then
                    StateNext <= sREAD ;
                    elsif  (iAvalonWrite = '1') then
                    StateNext <= sWRITE ;
                    else
                    StateNext <= sINIT ;
                    end if ;

         when sWRITE =>
                    if(gMultiplex = 0) then
                        oParHostChipselect <= '1';
                        oParHostWrite <= '1';
                        oParHostAddress <= iAvalonAddr (15 downto 0);
                        oParHostByteenable <= "11";
                        oParHostData <= iAvalonWriteData;

                        if(iParHostAcknowledge = '1') then
                        StateNext <= sIDLE ;
                        else
                        StateNext <= sWRITE ;
                        end if;
                    elsif (gMultiplex = 1) then
                        oParHostChipselect <= '1';
                        oParHostWrite <= '1';
                        oParHostAddressLatchEnable <= '1';
                        if (temp = latchaddress) then
                        oParHostAddressData <= latchaddress;
                        oParHostAddressLatchEnable <= '0';
                        end if;
                        if (temp1 = latchdata) then
                        oParHostAddressData <= latchdata;
                        oParHostAddressLatchEnable <= '0';
                        end if;

                        if(iParHostAcknowledge = '1') then
                            StateNext <= sIDLE ;
                        else
                            StateNext <= sWRITE ;
                        end if;
                    end if;

        when sIDLE  =>
                    oParHostChipselect <= '0';
                    oParHostWrite <= '0';
                    oParHostAddress <= iAvalonAddr (15 downto 0);
                    oParHostByteenable <= "00";
                    oParHostData <= x"0000";
                    oParHostChipselect <= '0';
                    oParHostWrite <= '0';
                    oParHostAddressData <= iAvalonAddr (15 downto 0);
                    oParHostAddressLatchEnable <= '0';

                    StateNext <= sINIT ;

         when sREAD  =>
                    if(gMultiplex = 0) then
                        oParHostChipselect <= '1';
                        oParHostRead <= '1';
                        oParHostAddress <= iAvalonAddr (15 downto 0);
                        oParHostByteenable <= "11";

                        if(iParHostAcknowledge = '1' and iParHostDataEnable = '1') then
                        StateNext <= sSTOP ;
                        else
                        StateNext <= sREAD ;
                        end if;

                    elsif (gMultiplex = 1) then
                        oParHostChipselect <= '1' after 20 ns;
                        oParHostRead <= '1';
                        oParHostAddressData <= x"00ee";
                        oParHostAddressLatchEnable <= '1';
                        if(iParHostAcknowledge = '1' and iParHostAddressDataEnable = '1') then
                        StateNext <= sStop ;
                        else
                        StateNext <= sREAD ;
                        end if;
                    end if;

         when sSTOP =>
                    oParHostChipselect <= '0';
                    oParHostRead <= '0';
                    oParHostWrite <= '0';
                    oParHostAddress <= x"0000";
                    oParHostByteenable <= "00";
                    oParHostData <= x"0000";
                    oParHostChipselect <= '0';
                    oParHostWrite <= '0';
                    oParHostAddressData <= x"0000";
                    oParHostAddressLatchEnable <= '0';
                    Count <= Count +  x"00000001" ;
                    StateNext <= sINIT ;

         when others => null;
        end case;
    end process;


oAvalonWaitReq <= not iParHostAcknowledge ;
oAvalonReadData <= iParHostData & iParHostData;

end rtl;



