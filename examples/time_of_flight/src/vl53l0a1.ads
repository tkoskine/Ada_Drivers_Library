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

with HAL.I2C;
with STM32.GPIO;  use STM32.GPIO;
with STM32.Board; use STM32.Board;

with VL53L0X;      use VL53L0X;
with STMPE1600;

package VL53L0A1 is

   GPIO1_L : GPIO_Point renames ARDUINO_D8;
   GPIO1_R : GPIO_Point renames ARDUINO_D2;
   GPIO1_C : GPIO_Point renames ARDUINO_A2;

   type Sensor_Id is (Center, Left, Right);
   type Sensors_Array is array (Sensor_Id) of access VL53L0X_Ranging_Sensor;

   subtype Segment_String is String (1 .. 4);
   subtype Display_Number is Integer range -999 .. 9999;

   type VL53L0A1_Board (Port : not null HAL.I2C.Any_I2C_Port) is
   limited private;

   procedure Configure
     (This   : in out VL53L0A1_Board;
      Status : out Boolean);

   procedure Set_Display
     (This : in out VL53L0A1_Board;
      Str  : Segment_String);

   procedure Set_Display
     (This : in out VL53L0A1_Board;
      Num  : Display_Number);

   function Detect_Sensors
     (This : in out VL53L0A1_Board) return Sensors_Array;

   procedure Set_Sensor_Reset_State
     (This   : in out VL53L0A1_Board;
      Sensor : Sensor_Id;
      State  : Boolean);

private

   --  The on-board I2C Expanders are mapped as follow:
   --  Expander0
   --  Digit #1 gpio 0 to 6
   --  Digit #2 gpio 7 to 13
   --  xshut_l  gpio 14
   --  xshut_r  gpio 15

   --  Expander1
   --  Digit #3 gpio 0 to 6
   --  Digit #4 gpio 7 to 13
   --  PB1      gpio 14
   --  xshut_c  gpio 15

   --  7 segments digit
   subtype Digit is HAL.UInt7;

   type Expander_1_Value is record
      Digit_0 : Digit := 0;
      Digit_1 : Digit := 0;
      XShut_L : Boolean := False;
      XShut_R : Boolean := False;
   end record with Size => 16;

   for Expander_1_Value use record
      Digit_0 at 0 range 0 .. 6;
      Digit_1 at 0 range 7 .. 13;
      XShut_L at 0 range 14 .. 14;
      XShut_R at 0 range 15 .. 15;
   end record;

   type Expander_2_Value is record
      Digit_2 : Digit := 0;
      Digit_3 : Digit := 0;
      PB1     : Boolean := False;
      XShut_C : Boolean := False;
   end record with Size => 16;

   for Expander_2_Value use record
      Digit_2 at 0 range 0 .. 6;
      Digit_3 at 0 range 7 .. 13;
      PB1     at 0 range 14 .. 14;
      XShut_C at 0 range 15 .. 15;
   end record;

   use type HAL.I2C.I2C_Address;

   Exp1_Addr : constant HAL.I2C.I2C_Address := 16#43# * 2;
   Exp2_Addr : constant HAL.I2C.I2C_Address := 16#42# * 2;

   type VL53L0A1_Board (Port : not null HAL.I2C.Any_I2C_Port)
   is limited record
      --  IO Expanders
      Exp1          : STMPE1600.STMPE1600_Expander (Port, Exp1_Addr);
      Exp2          : STMPE1600.STMPE1600_Expander (Port, Exp2_Addr);
      --  Cached value for expander1
      Val1          : Expander_1_Value := (others => <>);
      --  Cached value for expander2
      Val2          : Expander_2_Value := (others => <>);
      --  The sensors
      Center_Sensor : aliased VL53L0X_Ranging_Sensor (Port);
      Left_Sensor   : aliased VL53L0X_Ranging_Sensor (Port);
      Right_Sensor  : aliased VL53L0X_Ranging_Sensor (Port);
      Sensors       : Sensors_Array := (others => null);
   end record;

end VL53L0A1;
