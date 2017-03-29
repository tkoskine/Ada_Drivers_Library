------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2016, AdaCore                         --
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

with HAL;           use HAL;
with STM32.GPIO;    use STM32.GPIO;
with STM32.USARTs;  use STM32.USARTs;

with STM32.Device;  use STM32.Device;

package body Debug_Uart is

   TX_Pin : constant GPIO_Point := PB7;
   RX_Pin : constant GPIO_Point := PB6;

   procedure Initialize_UART_GPIO;

   procedure Await_Send_Ready (This : USART) with Inline;

   procedure Put_Blocking (This : in out USART;  Data : UInt16);

   --------------------------
   -- Initialize_UART_GPIO --
   --------------------------

   procedure Initialize_UART_GPIO is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (USART_1);
      Enable_Clock (RX_Pin & TX_Pin);

      Configuration.Mode := Mode_AF;
      Configuration.Speed := Speed_50MHz;
      Configuration.Output_Type := Push_Pull;
      Configuration.Resistors := Pull_Up;

      Configure_IO (RX_Pin & TX_Pin, Configuration);

      Configure_Alternate_Function (RX_Pin & TX_Pin, AF => GPIO_AF_USART1_7);
   end Initialize_UART_GPIO;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Initialize_UART_GPIO;

      Disable (USART_1);

      Set_Baud_Rate    (USART_1, 115_200);
      Set_Mode         (USART_1, Tx_Rx_Mode);
      Set_Stop_Bits    (USART_1, Stopbits_1);
      Set_Word_Length  (USART_1, Word_Length_8);
      Set_Parity       (USART_1, No_Parity);
      Set_Flow_Control (USART_1, No_Flow_Control);

      Enable (USART_1);
   end Initialize;

   ----------------------
   -- Await_Send_Ready --
   ----------------------

   procedure Await_Send_Ready (This : USART) is
   begin
      loop
         exit when Tx_Ready (This);
      end loop;
   end Await_Send_Ready;

   ------------------
   -- Put_Blocking --
   ------------------

   procedure Put_Blocking (This : in out USART;  Data : UInt16) is
   begin
      Await_Send_Ready (This);
      Transmit (This, UInt9 (Data));
   end Put_Blocking;

   procedure Put (Data : String) is
   begin
      for Ch of Data loop  -- arbitrary
         Put_Blocking (USART_1, Character'Pos (Ch));
      end loop;
   end Put;

   procedure Put_Line (Data : String) is
   begin
      Put (Data);
      New_Line;
   end Put_Line;

   procedure New_Line is
   begin
      Put_Blocking (USART_1, UInt16(13));
      Put_Blocking (USART_1, UInt16(10));
   end New_Line;

   Numbers : constant array (Interfaces.Unsigned_8 range 0..15) of Character :=
      ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
       'A', 'B', 'C', 'D', 'E', 'F');

   procedure Put (Data : Interfaces.Unsigned_8;
                  Base : Interfaces.Unsigned_8 := 10) is
      use type Interfaces.Unsigned_8;

      Temp : Interfaces.Unsigned_8 := Data;
      Num : Interfaces.Unsigned_8;
      Str : String (1..3) := (others => ' ');
   begin
      if Base = 10 then
         for I in reverse Str'Range loop
            exit when Temp = 0;
            Num := Temp mod 10;
            Str (I) := Numbers (Num);
            Temp := Temp / 10;
         end loop;
      elsif Base = 16 then
         for I in reverse Integer range 2..3 loop
            Num := Temp mod 16;
            Str (I) := Numbers (Num);
            Temp := Temp / 16;
         end loop;
      else
         Str := "err";
      end if;

      Put (Str);
   end Put;

   procedure Put (Data : Interfaces.Unsigned_16;
                  Base : Interfaces.Unsigned_8 := 10) is
      pragma Unreferenced (Data);
      pragma Unreferenced (Base);
   begin
      null;
   end Put;

   function Get return Character is
      D : UInt9;
   begin
      loop
         exit when Rx_Ready (USART_1);
      end loop;
      Receive (USART_1,  D);
      return Character'Val (D);
   end Get;
end Debug_Uart;


