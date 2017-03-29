--------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2017 Tero Koskinen                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with STM32; use STM32;
with STM32.Device; use STM32.Device;
with STM32.GPIO; use STM32.GPIO;
with STM32.SPI; use STM32.SPI;
with HAL.SPI;
-- with Ada.Real_Time; use Ada.Real_Time;

package body SPI is
   
   
   NFC_SPI    : SPI_Port renames SPI_2;
   NFC_SPI_AF : GPIO_Alternate_Function renames GPIO_AF_SPI1_5;

   SS_Pin              : GPIO_Point renames STM32.Device.PG10;
   NFC_SPI_SCK_Pin     : GPIO_Point renames STM32.Device.PB10;
   NFC_SPI_MISO_Pin    : GPIO_Point renames STM32.Device.PC2;
   NFC_SPI_MOSI_Pin    : GPIO_Point renames STM32.Device.PC3;
   
   procedure Init_SPI is
      Config : SPI_Configuration;
   begin
      Enable_Clock (NFC_SPI);

      Config.Mode := Master;
      Config.Baud_Rate_Prescaler := BRP_128;
      Config.Clock_Polarity := Low;
      Config.Clock_Phase := P1Edge;
      Config.First_Bit := LSB;
      Config.CRC_Poly := 0;
      Config.Slave_Management := Software_Managed;  --  essential!!
      Config.Direction := D2Lines_FullDuplex;
      Config.Data_Size := HAL.SPI.Data_Size_8b;

      Disable (NFC_SPI);
      Configure (NFC_SPI, Config);
      Enable (NFC_SPI);
   end Init_SPI;
   
   procedure Init is
      Configuration : GPIO_Port_Configuration;
      
      SPI_Points : constant GPIO_Points := NFC_SPI_MOSI_Pin &
         NFC_SPI_MISO_Pin & NFC_SPI_SCK_Pin;
   begin
      Enable_Clock (SS_Pin);
      
      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_50MHz;
      Configuration.Resistors   := Floating;
      Configure_IO (SS_Pin,
                    Config => Configuration);
      
      Chip_Select (True);
      Chip_Select (False);
      Chip_Select (True);
      Chip_Select (False);
      
      Enable_Clock (SPI_Points);

      Configuration.Output_Type := Push_Pull;
      Configuration.Resistors   := Floating;
      Configuration.Speed       := Speed_50MHz;
      Configuration.Mode        := Mode_AF;

      Configure_IO (SPI_Points, Configuration);
      Configure_Alternate_Function (SPI_Points, NFC_SPI_AF);
      
      Init_SPI;
   end Init;

   procedure Send (Value : Interfaces.Unsigned_8) is
   begin
      STM32.SPI.Transmit (NFC_SPI, HAL.UInt8 (Value));
   end Send;

   function Read return Interfaces.Unsigned_8 is
      V : Interfaces.Unsigned_8;
   begin
      STM32.SPI.Receive (NFC_SPI, HAL.UInt8 (V));
      return V;
   end Read;
   
   procedure Chip_Select (Enable : Boolean) is
   begin
      if Enable then
         Clear (SS_Pin);
      else
         Set (SS_Pin);
      end if;
   end Chip_Select;
end SPI;
