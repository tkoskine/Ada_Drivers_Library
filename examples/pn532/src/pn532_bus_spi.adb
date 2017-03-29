with SPI;
with Ada.Real_Time; use Ada.Real_Time;

with Debug_Uart;

package body PN532_BUS_SPI is
   SPI_STATUS : constant := 2#10#;
   SPI_WRITE  : constant := 2#01#;
   SPI_READ   : constant := 2#11#;
   
   procedure Log (Data : String) is
   begin
      Debug_Uart.Put (Data);
      Debug_Uart.New_Line;
   end Log;

   procedure PN532_Delay is
   begin
      delay until Clock + Milliseconds (2);
   end PN532_Delay;
   pragma Inline (PN532_Delay);

   procedure PN532_SPI_Enable is
   begin
      SPI.Chip_Select (True);
   end PN532_SPI_Enable;
   pragma Inline (PN532_SPI_Enable);

   procedure PN532_SPI_Disable is
   begin
      SPI.Chip_Select (False);
   end PN532_SPI_Disable;
   pragma Inline (PN532_SPI_Disable);

   procedure PN532_Init is
   begin
      delay until Clock + Milliseconds (100);
      SPI.Init;

      PN532_SPI_Disable;
      PN532_Delay;
      PN532_SPI_Enable;

      SPI.Send(16#55#);

      delay until Clock + Milliseconds (20);
      PN532_SPI_Disable;
   end PN532_Init;

   procedure PN532_Write (Cmd : PN532_Buf)  is
      Checksum : Unsigned_8 := 16#FF#;
      Len : constant Unsigned_8 := Cmd'Length + 1;
   begin
      PN532_SPI_Enable;
      PN532_Delay;

      SPI.Send(SPI_WRITE);

      SPI.Send(0);
      SPI.Send(0);
      SPI.Send(16#FF#);

      SPI.Send (Len);
      SPI.Send ((not Len) + 1);

      SPI.Send(HOST_TO_PN532);

      Checksum := Checksum + HOST_TO_PN532;

      for I in Cmd'Range loop
         SPI.Send (Cmd (I));
         Checksum := Checksum + Cmd (I);
      end loop;

      SPI.Send(not Checksum);
      SPI.Send(0);

      PN532_SPI_Disable;
   end PN532_Write;

   function PN532_Busy return Boolean is
      Status : Unsigned_8;
   begin
      PN532_SPI_Enable;
      PN532_Delay;
      SPI.Send (SPI_STATUS);
      Status := SPI.Read;
      PN532_SPI_Disable;

      return Status /= 1;
   end PN532_Busy;

   procedure PN532_Read_Reply (Reply : out Reply_Type; Code : out Unsigned_8) is
      Buf : PN532_Buf (1..6);
      Len : Unsigned_8;
   begin
      PN532_SPI_Enable;
      PN532_Delay;
      SPI.Send (SPI_READ);
      Code := 0;

      for I in Buf'Range loop
         Buf (I) := SPI.Read;
      end loop;

      if Buf (1) = 0 and Buf (2) = 0 and Buf (3) = 16#FF# then
         if Buf (4) = 0 and Buf (5) = 16#FF# and Buf (6) = 0 then -- ACK
            Reply := REPLY_ACK;
         elsif Buf(4) = 16#FF# and Buf(5) = 0 and Buf (6) = 0 then -- NACK
            Reply := REPLY_NACK;
         else -- ERROR
            Reply := REPLY_ERROR;
            Len := Buf (4);
            if Buf (5) + Len = 0 then -- checksum ok
               Code := Buf (6);
               Buf (1) := SPI.Read;
               Buf (2) := SPI.Read;
            end if;
         end if;
      else
         Log ("Unknown reply");
         -- for I in Buf'Range loop
         --    AVR.UART.Put (Data => Buf (I), Base => 16);
         --    AVR.UART.Put (" ");
         -- end loop;
         -- AVR.UART.CRLF;
         Reply := REPLY_ERROR;
         Code := 255;
      end if;

      PN532_SPI_Disable;
   end PN532_Read_Reply;

   procedure PN532_Read_Raw (Buf : out PN532_Buf) is
   begin
      PN532_SPI_Enable;
      PN532_Delay;

      SPI.Send (SPI_READ);

      for I in Buf'Range loop
         Buf (I) := SPI.Read;
      end loop;

      PN532_SPI_Disable;
   end PN532_Read_Raw;

   procedure PN532_Read_Data
     (Buf    : out PN532_Buf;
      Len    : out Unsigned_8;
      Status : out Boolean)
   is
      Header : PN532_Buf (1..6);
      Checksum : Unsigned_8 := 16#FF#;
      Msg_Len : Unsigned_8;
   begin
      PN532_SPI_Enable;
      PN532_Delay;

      SPI.Send (SPI_READ);

      for I in Header'Range loop
         Header (I) := SPI.Read;
      end loop;

      if not (Header (2) = 0 and Header (3) = 16#FF#) then
         Log ("PN532_Read_Data: Invalid header");
         PN532_SPI_Disable;
         Status := False;
         return;
      end if;

      Msg_Len := Header (4);
      if Msg_Len + Header (5) /= 0 then
         PN532_SPI_Disable;
         Status := False;
         Log ("PN532_Read_Data: Invalid header len checksum");
         return;
      end if;

      Checksum := Checksum + Header (6);

      Msg_Len := Unsigned_8'Min (Buf'Length, Msg_Len);

      for I in Unsigned_8 range 1 .. Msg_Len loop
         Buf (I) := SPI.Read;
         Checksum := Checksum + Buf (I);
      end loop;

      Header (1) := SPI.Read;
      Header (2) := SPI.Read;
      Header (1) := not Header (1);

      if Checksum /= Header (1) then
         Log ("PN532_Read_Data: Invalid checksum");
         Status := False;
      else
         Status := True;
         Len := Msg_Len;
      end if;

      PN532_SPI_Disable;
   end PN532_Read_Data;
end PN532_BUS_SPI;
