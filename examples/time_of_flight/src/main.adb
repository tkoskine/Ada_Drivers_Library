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
------------------------------------------------------------------------------

--  Demo of the Nucleo 53L0A1 expansion board
--  This board contains up to 3 Time-Of-Flight ranging sensors. This demo
--  uses the central one to measure the distance from any object in front
--  of the sensor, and displays the distance in milimeters on the on-board
--  4 * 7-segment display.

with VL53L0A1;      use VL53L0A1;
with VL53L0X;       use VL53L0X;
with HAL;

with STM32.Board;   use STM32.Board;
with STM32.Device;  use STM32.Device;
--  with STM32.EXTI;
--  with STM32.GPIO;    use STM32.GPIO;

procedure Main is
   VL53L0A1 : VL53L0A1_Board (I2C_1'Access);
   Status   : Boolean;
   Sensors  : Sensors_Array;
   Measure  : HAL.UInt16;
   use type HAL.UInt16;

begin
   STM32.Board.Initialize_I2C_GPIO (I2C_1);
   STM32.Board.Configure_I2C (I2C_1);

   --  Configure Central detector GPIO1 for interrupt handling
--     STM32.GPIO.Configure_IO
--       (GPIO1_C,
--        (Mode        => Mode_In,
--         Output_Type => Open_Drain,
--         Speed       => Speed_High,
--         Resistors   => Floating));
--     STM32.GPIO.Configure_Trigger
--       (GPIO1_C, STM32.EXTI.Interrupt_Rising_Edge);

   Configure (VL53L0A1, Status);
   if Status then
      Sensors := Detect_Sensors (VL53L0A1);
   end if;

   if Sensors (Center) /= null then
      Status := Static_Init (Sensors (Center).all);

      if Status then
         Status := Perform_Ref_Calibration (Sensors (Center).all);
      end if;

      if Status then
         --  100ms timing budget
         Status := Set_Measurement_Timing_Budget
           (Sensors (Center).all, 100_000);
      end if;
   end if;

   loop
      Measure := Read_Range_Single_Millimeters (Sensors (Center).all);
      if Measure = 8190 then
         Set_Display (VL53L0A1, "----");
      else
         Set_Display (VL53L0A1, Natural (Measure));
      end if;
   end loop;

end Main;
