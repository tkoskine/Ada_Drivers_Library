------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017, AdaCore                           --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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
--                                                                          --
--  This file is based on X-CUBE-53L0A1 STM32Cube expansion                 --
--                                                                          --
--   COPYRIGHT(c) 2016 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Real_Time; use Ada.Real_Time;

with HAL;           use HAL;

package body VL53L0A1 is

   procedure Update_Exp1
     (This : in out VL53L0A1_Board);

   procedure Update_Exp2
     (This : in out VL53L0A1_Board);

   --  7 segment digits

   --    -0-
   --  5|   |1
   --    -6-
   --  4|   |2
   --    -3-
   S0 : constant Digit := 2 ** 3;
   S1 : constant Digit := 2 ** 5;
   S2 : constant Digit := 2 ** 6;
   S3 : constant Digit := 2 ** 4;
   S4 : constant Digit := 2 ** 0;
   S5 : constant Digit := 2 ** 1;
   S6 : constant Digit := 2 ** 2;

   subtype Numerical is Display_Number range 0 .. 9;

   function To_Digit (C : Character) return Digit;
   function To_Digit (N : Numerical) return Digit;

   -----------------
   -- Update_Exp1 --
   -----------------

   procedure Update_Exp1
     (This : in out VL53L0A1_Board)
   is
      use STMPE1600;
      function To_Pins is new Ada.Unchecked_Conversion
        (Expander_1_Value, STMPE1600_Pins);
   begin
      Set_Pins_State (This.Exp1, To_Pins (This.Val1));
   end Update_Exp1;

   -----------------
   -- Update_Exp2 --
   -----------------

   procedure Update_Exp2
     (This : in out VL53L0A1_Board)
   is
      use STMPE1600;
      function To_Pins is new Ada.Unchecked_Conversion
        (Expander_2_Value, STMPE1600_Pins);
   begin
      Set_Pins_State (This.Exp2, To_Pins (This.Val2));
   end Update_Exp2;

   --------------
   -- To_Digit --
   --------------

   function To_Digit (C : Character) return Digit
   is
   begin
      case C is
         when ' ' => return 0;
         when '-' => return S6;
         when '_' => return S3;
         when '=' => return S3 + S6;
         when '^' => return S0;

         when '?' => return S0 + S1 + S6 + S4;
         when '*' => return S0 + S1 + S2 + S3 + S4 + S5 + S6;
         when '[' => return S0 + S3 + S4 + S5;
         when ']' => return S0 + S3 + S1 + S2;

         when '0' => return S0 + S1 + S2 + S3 + S4 + S5;
         when '1' => return S1 + S2;
         when '2' => return S0 + S1 + S6 + S4 + S3;
         when '3' => return S0 + S1 + S6 + S2 + S3;
         when '4' => return S5 + S1 + S6 + S2;
         when '5' => return S0 + S5 + S6 + S2 + S3;
         when '6' => return S0 + S2 + S3 + S4 + S5 + S6;
         when '7' => return S0 + S1 + S2;
         when '8' => return S0 + S1 + S2 + S3 + S4 + S5 + S6;
         when '9' => return S0 + S1 + S2 + S3 + S5 + S6;

         when 'A' => return S0 + S1 + S2 + S4 + S5 + S6;
         when 'B' => return S0 + S1 + S2 + S3 + S4 + S5 + S6;
         when 'C' => return S0 + S3 + S4 + S5;
         when 'D' => return S0 + S1 + S2 + S3 + S4 + S5;
         when 'E' => return S0 + S3 + S4 + S5 + S6;
         when 'F' => return S0 + S4 + S5 + S6;
         when 'G' => return S0 + S2 + S3 + S4 + S5;
         when 'H' => return S1 + S2 + S4 + S5 + S6;
         when 'I' => return S1 + S2;
         when 'J' => return S1 + S2 + S3;
         when 'K' => return S1 + S4 + S5 + S6;
         when 'L' => return S3 + S4 + S5;
         when 'M' => return S0 + S1 + S2 + S4 + S5;
         when 'N' => return S0 + S1 + S2 + S4 + S5;
         when 'O' => return S0 + S1 + S2 + S3 + S4 + S5;
         when 'P' => return S0 + S1 + S4 + S5 + S6;
         when 'Q' => return S0 + S1 + S2 + S3 + S4 + S5;
         when 'R' => return S0 + S1 + S2 + S4 + S5 + S6;
         when 'S' => return S0 + S2 + S3 + S5 + S6;
         when 'T' => return S0 + S1 + S2;
         when 'U' => return S1 + S2 + S3 + S4 + S5;
         when 'V' => return S1 + S2 + S3 + S4 + S5;
         when 'W' => return S1 + S2 + S3 + S4 + S5;
         when 'X' => return S1 + S2 + S4 + S5 + S6;
         when 'Y' => return S1 + S2 + S5 + S6;
         when 'Z' => return S0 + S1 + S3 + S4 + S6;

         when others => return S0 + S6 + S3;
      end case;
   end To_Digit;

   --------------
   -- To_Digit --
   --------------

   function To_Digit (N : Numerical) return Digit is
   begin
      case N is
         when 0 => return S0 + S1 + S2 + S3 + S4 + S5;
         when 1 => return S1 + S2;
         when 2 => return S0 + S1 + S6 + S4 + S3;
         when 3 => return S0 + S1 + S6 + S2 + S3;
         when 4 => return S5 + S1 + S6 + S2;
         when 5 => return S0 + S5 + S6 + S2 + S3;
         when 6 => return S0 + S2 + S3 + S4 + S5 + S6;
         when 7 => return S0 + S1 + S2;
         when 8 => return S0 + S1 + S2 + S3 + S4 + S5 + S6;
         when 9 => return S0 + S1 + S2 + S3 + S5 + S6;
      end case;
   end To_Digit;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This   : in out VL53L0A1_Board;
      Status : out Boolean)
   is
      Dirs : STMPE1600.STMPE1600_Pins_Direction;
   begin
      STMPE1600.Check_Id (This.Exp1, Status);
      if not Status then
         return;
      end if;

      STMPE1600.Check_Id (This.Exp2, Status);
      if not Status then
         return;
      end if;

      --  Setup all pins in output except PB1 (pin 14 of Exp1)
      Dirs := (others => STMPE1600.Output);
      STMPE1600.Set_Pins_Direction (This.Exp2, Dirs);
      Dirs (14) := STMPE1600.Input;
      STMPE1600.Set_Pins_Direction (This.Exp1, Dirs);

      --  Segments off, and sensors off
      This.Val1 := (Digit_0 => 16#7F#,
                    Digit_1 => 16#7F#,
                    XShut_L => False,
                    XShut_R => False);
      This.Val2 := (Digit_2 => 16#7F#,
                    Digit_3 => 16#7F#,
                    PB1     => False,
                    XShut_C => False);

      Update_Exp1 (This);
      Update_Exp2 (This);
   end Configure;

   -----------------
   -- Set_Display --
   -----------------

   procedure Set_Display
     (This : in out VL53L0A1_Board;
      Str  : Segment_String)
   is
   begin
      This.Val1.Digit_0 := not To_Digit (Str (1));
      This.Val1.Digit_1 := not To_Digit (Str (2));
      This.Val2.Digit_2 := not To_Digit (Str (3));
      This.Val2.Digit_3 := not To_Digit (Str (4));

      Update_Exp1 (This);
      Update_Exp2 (This);
   end Set_Display;

   -----------------
   -- Set_Display --
   -----------------

   procedure Set_Display
     (This : in out VL53L0A1_Board;
      Num  : Display_Number)
   is
      Temp : Display_Number;
      Sign : Display_Number;
      N    : Numerical;
      D    : Digit;
   begin
      if Num > 0 then
         Sign := 1;
         Temp := Num;
      else
         Sign := -1;
         Temp := -Num;
      end if;

      for J in 1 .. 4 loop
         if Temp = 0 then
            if J = 1 then
               D := To_Digit (0);
            elsif Sign = -1 then
               D := To_Digit ('-');
               Sign := 1;
            else
               D := To_Digit (' ');
            end if;
         else
            N := Temp mod 10;
            Temp := Temp / 10;
            D := To_Digit (N);
         end if;

         case J is
            when 1 => This.Val2.Digit_3 := not D;
            when 2 => This.Val2.Digit_2 := not D;
            when 3 => This.Val1.Digit_1 := not D;
            when 4 => This.Val1.Digit_0 := not D;
         end case;
      end loop;

      Update_Exp1 (This);
      Update_Exp2 (This);
   end Set_Display;

   --------------------
   -- Detect_Sensors --
   --------------------

   function Detect_Sensors
     (This : in out VL53L0A1_Board) return Sensors_Array
   is
      use HAL.I2C;
      Final_Addr : I2C_Address;
      Dev_Id     : UInt16;
      Status     : Boolean := True;
   begin
      This.Sensors (Center) := This.Center_Sensor'Unchecked_Access;
      This.Sensors (Left)   := This.Left_Sensor'Unchecked_Access;
      This.Sensors (Right)  := This.Right_Sensor'Unchecked_Access;

      for Id in This.Sensors'Range loop
         Set_Sensor_Reset_State (This, Id, False);
      end loop;

      --  Base sensor i2c address
      --  Here, we don't start with 16#52#, as this would lead to the first
      --  sensor being assigned 16#54#, which is the address of the touch
      --  panel on the STM32F469-disco board (on the same I2C bus).
      Final_Addr := 16#54#;

      for Id in This.Sensors'Range loop
         Initialize (This.Sensors (Id).all);
         Set_Sensor_Reset_State (This, Id, True);

         delay until Clock + Ada.Real_Time.Milliseconds (2);

         Final_Addr := Final_Addr + 2;

         Dev_Id := Read_Id (This.Sensors (Id).all);

         if Dev_Id = 16#EEAA# then
            --  Found a sensor: set the new I2C address
            Set_Device_Address (This.Sensors (Id).all, Final_Addr, Status);
         else
            Status := False;
         end if;

         if Status then
            --  Now check that the sensor is at the new address
            Dev_Id := Read_Id (This.Sensors (Id).all);

            if Dev_Id /= 16#EEAA# then
               Status := False;
            end if;
         end if;

         if Status then
            Status := Data_Init (This.Sensors (Id).all);
         end if;

         if not Status then
            --  Remove the sensor from the list
            This.Sensors (Id) := null;
            Set_Sensor_Reset_State (This, Id, False);
         end if;
      end loop;

      return This.Sensors;
   end Detect_Sensors;

   ---------------------
   -- Set_Reset_State --
   ---------------------

   procedure Set_Sensor_Reset_State
     (This   : in out VL53L0A1_Board;
      Sensor : Sensor_Id;
      State  : Boolean)
   is
   begin
      case Sensor is
         when Center =>
            This.Val2.XShut_C := State;
            Update_Exp2 (This);
         when Left =>
            This.Val1.XShut_L := State;
            Update_Exp1 (This);
         when Right =>
            This.Val1.XShut_R := State;
            Update_Exp1 (This);
      end case;
   end Set_Sensor_Reset_State;

end VL53L0A1;
