-- PN532/NFC routines using AVR-Ada
--
-- Copyright (c) 2015 Tero Koskinen <tero.koskinen@iki.fi>
--
-- Permission to use, copy, modify, and distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--

with System;
with Interfaces; use Interfaces;
with Ada.Real_Time; use Ada.Real_Time;
with Debug_Uart;
-- with AVR.UART;
-- with AVR.Strings;
-- with AVR.Programspace;

package body Generic_PN532 is

   Data_Buffer : PN532_Buf (1..90);

   PN532_FIRMWARE_VERSION : constant := 16#02#;
   PN532_SAMCONFIG        : constant := 16#14#;
   PN532_IN_LIST_PASSIVE_TARGET : constant := 16#4A#;
   PN532_IN_DATA_EXCHANGE : constant := 16#40#;
   PN532_TG_INIT_AS_TARGET : constant := 16#8C#;
   PN532_TG_GET_DATA : constant := 16#86#;
   PN532_TG_SET_DATA : constant := 16#8E#;

   PN532_TIMEOUT_VALUE : constant := 20000;

   PN532_106_KBPS_ISOIEC_14443_A : constant := 0;

   MIFARE_READ_CARD_CMD : constant := 16#30#;
   MIFARE_WRITE_CARD_CMD : constant := 16#A2#;
   MIFARE_AUTH_KEY_A_CMD : constant := 16#60#;
   MIFARE_AUTH_KEY_B_CMD : constant := 16#61#;

   procedure My_Delay (Milli : Natural);

   procedure My_Delay (Milli : Natural) is
   begin
      delay until Clock + Milliseconds (Milli);
   end My_Delay;

   procedure PN532_Get_Data (Buf : out PN532_Buf;
                             Len : out Interfaces.Unsigned_8;
                             Status  : out Boolean);
   function PN532_Send_R_APDU (SW1 : Unsigned_8; SW2 : Unsigned_8) return Boolean;

   procedure Log (Msg : String) is
   begin
      Debug_Uart.Put_Line (Msg);
   end Log;

   procedure Log (Buf : PN532_Buf) is
   begin
      for I in Buf'Range loop
         -- AVR.UART.Put (Data => Buf (I), Base => 16);
         -- AVR.UART.Put (" ");
         null;
      end loop;
      -- AVR.UART.CRLF;
   end Log;

   function PN532_Wait_For_Ready (Timeout : Unsigned_32) return Boolean is
      Counter : Unsigned_32 := 0;
   begin
      loop
         exit when not PN532_Busy;
         Counter := Counter + 1;

         if Counter >= Timeout then
            return False;
         end if;
         delay until Clock + Milliseconds (2);
      end loop;

      return True;
   end PN532_Wait_For_Ready;

   function PN532_Send_Command (Cmd : PN532_Buf; Timeout : Unsigned_32)
     return Boolean
   is
      Status : Boolean;
      Reply : Reply_Type;
      Error_Code : Unsigned_8;
   begin
      PN532_Write (Cmd);

      Status := PN532_Wait_For_Ready (Timeout);
      if not Status then
         return False;
      end if;

      PN532_Read_Reply (Reply, Error_Code);

      if Reply = REPLY_ERROR or Reply = REPLY_NACK then
         Log ("PN532_Send_Command, invalid reply");
         return False;
      end if;

      return True;
   end PN532_Send_Command;


   function PN532_SAM_Config return Boolean is
      Cmd : constant PN532_Buf := ( PN532_SAMCONFIG,
                                    1, -- normal mode
                                    20, -- timeout (50 * 20) ms
                                    1);
      Status : Boolean;
      Reply : PN532_Buf (1..5);
      Len : Unsigned_8;
   begin
      Status := PN532_Send_Command (Cmd, PN532_TIMEOUT_VALUE);
      if not Status then
         return False;
      end if;

      Status := PN532_Wait_For_Ready (PN532_TIMEOUT_VALUE);
      if not Status then
         return False;
      end if;

      PN532_Read_Data (Reply, Len, Status);
      if Status and Reply (1) = 16#15# then
         return True;
      end if;

      return False;
   end PN532_SAM_Config;

   function PN532_Read_Firmware return Unsigned_32 is
      Cmd : constant PN532_Buf (1..1) := (1 => PN532_FIRMWARE_VERSION);
      Reply : PN532_Buf (1..12);
      Status : Boolean;
   begin
      Status := PN532_Send_Command (Cmd, PN532_TIMEOUT_VALUE);
      if not Status then
         return 0;
      end if;

      PN532_Read_Raw (Reply);

      -- Bytes are: IC(8) Ver(9) Rev(10) Support(11)
      -- See PN532 user manual section 7.2.2 GetFirmwareVersion
      return Unsigned_32 (Reply (8)) * (2**24) +
             Unsigned_32 (Reply (9)) * (2**16) +
             Unsigned_32 (Reply (10)) * (2**8) +
             Unsigned_32 (Reply (11));
   end PN532_Read_Firmware;

   procedure PN532_Detect_Tag
     (Sens_Res : out Unsigned_16;
      Sel_Res : out Unsigned_8;
      NFC_ID  : out PN532_Buf;
      NFC_ID_Len : out Unsigned_8;
      Status : out Boolean)
   is
      Cmd : constant PN532_Buf (1..3) :=
        (PN532_IN_LIST_PASSIVE_TARGET,
         1, -- Maximum amount of detected tags
         PN532_106_KBPS_ISOIEC_14443_A);

      Reply : PN532_Buf (1..40) := (others => 0);
      Len   : Unsigned_8;
      Ok    : Boolean;
   begin
      Ok := PN532_Send_Command (Cmd, PN532_TIMEOUT_VALUE);

      if not Ok then
         Status := False;
         return;
      end if;

      if not PN532_Wait_For_Ready (PN532_TIMEOUT_VALUE * 30) then
         Status := False;
         return;
      end if;

      PN532_Read_Data (Reply, Len, Ok);
      if not Ok or Len = 0 then -- Some error
         Status := False;
         return;
      end if;

      if Reply (2) = 0 then -- No tags found
         Status := False;
         return;
      end if;

      Sens_Res := Unsigned_16 (Reply (5)) * 256 or Unsigned_16 (Reply (4));
      Sel_Res := Reply (6);

      NFC_ID_Len := Reply (7);
      if NFC_ID_Len > NFC_ID'Length then
         NFC_ID_Len := NFC_ID'Length;
      end if;
      NFC_ID (NFC_ID'First .. NFC_ID'First + NFC_ID_Len - 1) :=
        Reply (8..7 + NFC_ID_Len);

      Status := Ok;
   end PN532_Detect_Tag;

   procedure PN532_Read_NFC_Forum_Type_2_Tag_Block
     (Block      : NFC_Forum_Type_2_Block;
      Buf        : out PN532_Buf;
      Byte_Count : out Unsigned_8;
      Status     : out Boolean)
   is
      Cmd : PN532_Buf (1..4) := (PN532_IN_DATA_EXCHANGE,
                                 1, -- card number
                                 MIFARE_READ_CARD_CMD,
                                 0); -- Block number
      -- Reply : PN532_Buf (1..40);
      Reply_Len : Unsigned_8;
      Len   : Unsigned_8;
      Ok    : Boolean;
   begin
      Cmd (4) := Block;

      Ok := PN532_Send_Command (Cmd, PN532_TIMEOUT_VALUE);
      if not Ok then
         Status := False;
         return;
      end if;

      if not PN532_Wait_For_Ready (PN532_TIMEOUT_VALUE) then
         Status := False;
         return;
      end if;

      PN532_Read_Data (Data_Buffer, Reply_Len, Ok);

      if (not Ok) or (Reply_Len < 3) then
         Status := False;
         return;
      end if;

      if Data_Buffer (2) /= 0 then -- status code not ok?
         Status := False;
         return;
      end if;

      Len := Unsigned_8'Min (Reply_Len - 3, Buf'Length);
      Byte_Count := Len;
      Buf (1..Len) := Data_Buffer (3 .. Len + 2);

      Status := True;
   end PN532_Read_NFC_Forum_Type_2_Tag_Block;

   procedure PN532_Write_NFC_Forum_Type_2_Tag_Block
     (Block_Number : NFC_Forum_Type_2_Block;
      Buf          : PN532_Buf_4;
      Status       : out Boolean)
   is
      Cmd : PN532_Buf (1..8) := (PN532_IN_DATA_EXCHANGE,
                                 1, -- card number
                                 MIFARE_WRITE_CARD_CMD,
                                 Block_Number, -- Block number
                                 others => 0);
      Reply : PN532_Buf (1..10);
      Reply_Len : Unsigned_8;
      Ok    : Boolean;
   begin
      Cmd (5..8) := Buf;

      Ok := PN532_Send_Command (Cmd, PN532_TIMEOUT_VALUE);
      if not Ok then
         Status := False;
         return;
      end if;

      if not PN532_Wait_For_Ready (PN532_TIMEOUT_VALUE) then
         Status := False;
         return;
      end if;

      PN532_Read_Data (Reply, Reply_Len, Ok);

      if (not Ok) or (Reply_Len < 3) then
         Status := False;
         return;
      end if;

      if Reply (2) /= 0 then -- status code not ok?
         Status := False;
         return;
      end if;

      Status := True;
   end PN532_Write_NFC_Forum_Type_2_Tag_Block;

   procedure PN532_Authenticate_Mifare_Classic_Tag_Block
      (Block_Number : Mifare_Classic_Block;
       UID          : PN532_Buf;
       Key          : Mifare_Auth;
       Key_Data     : PN532_Buf_6;
       Status       : out Boolean)
   is
      Cmd : PN532_Buf (1..20);
      Reply : PN532_Buf (1..15);
      Reply_Len : Unsigned_8;
      Ok    : Boolean;
   begin
      Cmd (1) := PN532_IN_DATA_EXCHANGE;
      Cmd (2) := 1; -- card number
      if Key = AUTH_A then
         Cmd (3) := MIFARE_AUTH_KEY_A_CMD;
      else
         Cmd (3) := MIFARE_AUTH_KEY_B_CMD;
      end if;
      Cmd (4) := Unsigned_8 (Block_Number);
      Cmd (5..4 + Key_Data'Length) := Key_Data;
      if UID'Length > 10 then
         Log ("UID too big");
         Status := False;
         return;
      end if;
      Cmd (11..10 + UID'Length) := UID;

      Log (Cmd (1..10+UID'Length));
      Ok := PN532_Send_Command (Cmd (1..10+UID'Length), PN532_TIMEOUT_VALUE);
      if not Ok then
         Log ("Send command failed");
         Status := False;
         return;
      end if;

      if not PN532_Wait_For_Ready (PN532_TIMEOUT_VALUE) then
         -- Log ("timeout");
         Status := False;
         return;
      end if;

      PN532_Read_Data (Reply, Reply_Len, Ok);

      if (not Ok) or (Reply_Len < 3) then
         Status := False;
         return;
      end if;

      if Reply (2) = 16#14# then -- Mifare authentication error
         Log ("Mifare auth error");
         Status := False;
         return;
      elsif Reply (2) /= 0 then -- status code not ok?
         Status := False;
         return;
      end if;

      Status := True;
   end PN532_Authenticate_Mifare_Classic_Tag_Block;


   -- C-APDU for select application
   -- CLA INS P1  P2  LC  Data           Le
   -- 00h A4h 04h 00h 07h D2760000850101 00
   function PN532_NFC_Forum_Type_4_Select_Application return Boolean
   is
      Data : PN532_Buf (1..20) := (PN532_IN_DATA_EXCHANGE,
                                   1, -- card number
                                   16#00#, -- class
                                   16#A4#, -- instruction
                                   16#04#, -- select by name
                                   16#00#, -- first or only occurrence
                                   16#07#,
                                   16#D2#, 16#76#, 16#00#, 16#00#, 16#85#, 16#01#, 16#01#,
                                   16#00#, -- response data field may be present
                                   others => 0);
      Reply_Len : Unsigned_8;
      Ok    : Boolean;
   begin
      Ok := PN532_Send_Command (Data (1..15), PN532_TIMEOUT_VALUE);
      if not Ok then
         return False;
      end if;

      if not PN532_Wait_For_Ready (PN532_TIMEOUT_VALUE * 30) then
         return False;
      end if;

      PN532_Read_Data (Data, Reply_Len, Ok);

      if (not Ok) or (Reply_Len < 5) then
         return False;
      end if;

      if Data (2) /= 0 then -- status code not ok?
         return False;
      end if;

      if Data (Reply_Len - 2) = 16#90# and Data (Reply_Len - 1) = 16#00# then
         -- ok
         return True;
      elsif Data (Reply_Len - 2) = 16#6A# and Data (Reply_Len - 1) = 16#82# then
         -- application not found
         null;
      else
         null; -- XXX unknown status, ignoring
      end if;

      return False;
   end PN532_NFC_Forum_Type_4_Select_Application;

   -- C-APDU for select file
   -- CLA INS P1  P2  LC  Data Le
   -- 00h A4h 00h 0Ch 02h xxyy --
   function PN532_NFC_Forum_Type_4_Select_File
     (File_ID : Interfaces.Unsigned_16) return Boolean
   is
      Cmd : constant PN532_Buf (1..9) :=
         (PN532_IN_DATA_EXCHANGE,
          1, -- card number
          16#00#, -- class
          16#A4#, -- instruction
          16#00#, -- select by identifier
          16#0C#, -- first or only occurrence
          16#02#, -- two bytes data
          Unsigned_8 (File_ID / 256), Unsigned_8 (File_ID and 16#FF#));

      Reply_Len : Unsigned_8;
      Ok    : Boolean;
   begin
      Ok := PN532_Send_Command (Cmd, PN532_TIMEOUT_VALUE);
      if not Ok then
         return False;
      end if;

      if not PN532_Wait_For_Ready (PN532_TIMEOUT_VALUE) then
         return False;
      end if;

      PN532_Read_Data (Data_Buffer, Reply_Len, Ok);

      if (not Ok) or (Reply_Len < 5) then
         return False;
      end if;

      if Data_Buffer (2) /= 0 then -- status code not ok?
         return False;
      end if;

      if Data_Buffer (Reply_Len - 2) = 16#90# and Data_Buffer (Reply_Len - 1) = 16#00# then
         -- ok
         return True;
      elsif Data_Buffer (Reply_Len - 2) = 16#6A#
        and Data_Buffer (Reply_Len - 1) = 16#82#
      then -- file not found
         return False;
      end if;

      return False;
   end PN532_NFC_Forum_Type_4_Select_File;

   -- C-APDU for read binary
   -- CLA INS P1  P2  Le
   -- 00h B0h xxh yyh zzh (xx yy = offset, zz = length)
   procedure PN532_NFC_Forum_Type_4_Read_Binary
     (Offset     : Interfaces.Unsigned_16;
      Buf        : out PN532_Buf;
      Byte_Count : out Unsigned_8;
      Status     : out Boolean)
   is

      Len : Unsigned_8;
      Reply_Len : Unsigned_8 := 0;
      Ok    : Boolean;
   begin
      Len := Unsigned_8'Min (Buf'Length, Data_Buffer'Length);

      Data_Buffer (1) := PN532_IN_DATA_EXCHANGE;
      Data_Buffer (2) := 1; -- card number
      Data_Buffer (3) := 16#00#; -- class
      Data_Buffer (4) := 16#B0#; -- instruction
      Data_Buffer (5) := Unsigned_8 (Offset and 16#FF#);
      Data_Buffer (6) := Unsigned_8 (Offset / 256);
      Data_Buffer (7) := Len;

      Ok := PN532_Send_Command (Data_Buffer (1..7), PN532_TIMEOUT_VALUE);
      if not Ok then
         Status := False;
         return;
      end if;

      if not PN532_Wait_For_Ready (PN532_TIMEOUT_VALUE * 30) then
         Status := False;
         return;
      end if;

      PN532_Read_Data (Data_Buffer, Reply_Len, Ok);

      if (not Ok) or (Reply_Len < 5) then
         Status := False;
         return;
      end if;

      if Data_Buffer (2) /= 0 then -- status code not ok?
         Status := False;
         return;
      end if;

      if Data_Buffer (Reply_Len - 2) = 16#90# and Data_Buffer (Reply_Len - 1) = 16#00# then
         -- ok, copying buffer

         Len := Reply_Len - 5;
         Buf (Buf'First .. Buf'First + Len) := Data_Buffer (3 .. 3 + Len);
         Byte_Count := Len;
         Status := True;
      else
         Status := False;
         -- AVR.UART.Put ("Read failed"); AVR.UART.CRLF;
      end if;
   end PN532_NFC_Forum_Type_4_Read_Binary;

   procedure PN532_NFC_Forum_Type_4_Update_Binary
     (Offset : Interfaces.Unsigned_16;
      Buf    : PN532_Buf;
      Status : out Boolean)
   is
      Cmd       : PN532_Buf (1..70);
      Reply     : PN532_Buf (1..10);
      Reply_Len : Unsigned_8;
      Ok        : Boolean;
   begin
      Cmd (1) := PN532_IN_DATA_EXCHANGE;
      Cmd (2) := 1; -- card number
      Cmd (3) := 16#00#; -- class
      Cmd (4) := 16#D6#; -- instruction
      Cmd (5) := Unsigned_8 (Offset / 256); -- offset p1
      Cmd (6) := Unsigned_8 (Offset and 16#FF#); -- offset p2
      Cmd (7) := Buf'Length;
      if Buf'Length > Cmd'Length - 7 then
         Status := False;
         return;
      end if;

      Cmd (8..7 + Buf'Length) := Buf;

      -- Log ("Update command:");
      -- Log (Cmd (1..7 + Buf'Length));

      -- Log (Cmd (1..7 + Buf'Length));

      Ok := PN532_Send_Command (Cmd (1..7 + Buf'Length), PN532_TIMEOUT_VALUE);
      if not Ok then
         Status := False;
         return;
      end if;

      if not PN532_Wait_For_Ready (PN532_TIMEOUT_VALUE) then
         Status := False;
         return;
      end if;

      PN532_Read_Data (Reply, Reply_Len, Ok);

      if (not Ok) or (Reply_Len < 5) then
         Status := False;
         return;
      end if;

      if Reply (2) /= 0 then -- status code not ok?
         -- Log ("PN532 error:");
         -- AVR.UART.Put (Data => Reply (2), Base => 16);
         -- AVR.UART.CRLF;
         Status := False;
         return;
      end if;

      if Reply (Reply_Len - 2) = 16#90# and Reply (Reply_Len - 1) = 16#00# then
         Status := True;
      else
         Status := False;
      end if;
   end PN532_NFC_Forum_Type_4_Update_Binary;

   function PN532_Set_Data (Buf : PN532_Buf; Buf2 : PN532_Buf)
     return Boolean
   is
      Cmd : PN532_Buf (1..70);
      Ok : Boolean;
      Reply : PN532_Buf (1..10);
      Reply_Len : Unsigned_8;
   begin
      if Buf'Length + Buf2'Length >= Cmd'Length - 2 then
         return False;
      end if;

      Cmd (1) := PN532_TG_SET_DATA;
      Cmd (2 .. 1 + Buf'Length) := Buf;
      if Buf2'Length > 0 then
         Cmd (2 + Buf'Length .. 1 + Buf'Length + Buf2'Length) := Buf2;
      end if;

      Ok := PN532_Send_Command
        (Cmd (1 .. 1 + Buf'Length + Buf2'Length), PN532_TIMEOUT_VALUE);

      if not Ok then
         return False;
      end if;

      if not PN532_Wait_For_Ready (PN532_TIMEOUT_VALUE * 30) then
         return False;
      end if;

      PN532_Read_Data (Reply, Reply_Len, Ok);
      if not Ok or Reply_Len = 0 then -- Some error
         return False;
      end if;

      if Reply (2) /= 0 then
         -- AVR.UART.Put ("PN532 error code: ");
         -- AVR.UART.Put (Data => Reply (2), Base => 16);
         -- AVR.UART.CRLF;
         return False;
      end if;

      return True;
   end PN532_Set_Data;

   Null_Buf : constant PN532_Buf (1..0) := (others => 0);

   function PN532_Set_Data (Buf : PN532_Buf) return Boolean is
   begin
      return PN532_Set_Data(Buf => Buf, Buf2 => Null_Buf);
   end PN532_Set_Data;

   procedure PN532_Get_Data (Buf : out PN532_Buf;
                             Len : out Unsigned_8;
                             Status  : out Boolean)
   is
      Cmd : constant PN532_Buf (1..1) := (1 => PN532_TG_GET_DATA);
      Ok : Boolean;
      Reply_Len : Unsigned_8;
   begin
      Ok := PN532_Send_Command (Cmd, PN532_TIMEOUT_VALUE);

      if not Ok then
         Status := False;
         return;
      end if;

      if not PN532_Wait_For_Ready (PN532_TIMEOUT_VALUE * 20) then
         Status := False;
         return;
      end if;

      PN532_Read_Data (Data_Buffer, Reply_Len, Ok);
      if not Ok or Reply_Len = 0 then -- Some error
         Status := False;
         return;
      end if;

      if Data_Buffer (2) /= 0 then
         -- AVR.UART.Put ("PN532 error code: ");
         -- AVR.UART.Put (Data => Reply (2), Base => 16);
         -- AVR.UART.CRLF;
         Status := False;
         return;
      end if;

      Len := Reply_Len - 3;

      Buf (1 .. Len) := Data_Buffer (3 .. 3 + Len);
      Status := True;
   end PN532_Get_Data;

   Init_Cmd : constant PN532_Buf :=
     (PN532_TG_INIT_AS_TARGET,
      16#05#,         -- 4h = ISO/IEC 14443-4A only
      16#04#, 16#00#, -- sens res
      16#12#, 16#34#, 16#46#, -- NFCID1
      16#20#,         -- sel res
      16#01#, 16#FE#,
      16#A2#, 16#A3#, 16#A4#,
      16#A6#, 16#A7#, 16#A8#,
      16#C0#, 16#C1#, 16#C2#,
      16#C3#, 16#C4#, 16#C5#,
      16#C6#, 16#C7#, 16#FF#,
      16#FF#,
      16#AA#, 16#99#, 16#88#, -- NFCID3t
      16#77#, 16#66#, 16#55#, 16#44#,
      16#33#, 16#22#, 16#11#,
      16#00#, -- Length of historical bytes
      16#00#  -- Length of general bytes
      );

   procedure Init is
   begin
      PN532_Init;
   end Init;

   function PN532_Init_As_Target return Boolean is
      use type System.Address;

      Len   : Unsigned_8;
      Ok    : Boolean;
   begin
      Ok := PN532_Send_Command (Init_Cmd, PN532_TIMEOUT_VALUE);

      if not Ok then
         return False;
      end if;

      if not PN532_Wait_For_Ready (PN532_TIMEOUT_VALUE * 30) then
         return False;
      end if;

      PN532_Read_Data (Data_Buffer, Len, Ok);
      if not Ok or Len = 0 then -- Some error
         return False;
      end if;

      if Data_Buffer (2) /= 16#08# then
         -- AVR.UART.Put ("Unsupported mode ");
         -- AVR.UART.Put (Data => Reply (2), Base => 16);
         -- AVR.UART.CRLF;
         return False;
      end if;

      return True;
   end PN532_Init_As_Target;

   function PN532_Send_R_APDU (SW1 : Unsigned_8; SW2 : Unsigned_8) return Boolean is
      Cmd : constant PN532_Buf (1..2) := (SW1, SW2);
   begin
      return PN532_Set_Data (Cmd);
   end PN532_Send_R_APDU;

   APDU_SELECT_FILE : constant := 16#A4#;
   APDU_READ_BINARY : constant := 16#B0#;

   type Current_File_Type is (FILE_NONE, FILE_CC, FILE_NDEF);
   Current_File : Current_File_Type := FILE_NONE;

   CC_Reply : constant PN532_Buf (1..17) :=
     (16#00#, 16#0F#, 16#20#, 16#00#,
      16#3B#, 16#00#, 16#34#, 16#04#,
      16#06#, 16#E1#, 16#04#, 16#00#,
      16#32#, 16#00#, 16#00#,
      16#90#, 16#00#);

   procedure Handle_Select_File (P1 : Unsigned_8; P2 : Unsigned_8;Rest : PN532_Buf) is
      Ok : Boolean;
   begin
      if P1 = 4 and P2 = 0 then -- select by name
         if Rest'Length > 3 then
            if Rest (Rest'First) = 7 and Rest (Rest'First + 1 .. Rest'Last - 1) =
              (16#D2#, 16#76#,16#00#,16#00#,16#85#,16#01#,16#01#)
            then
               if not PN532_Send_R_APDU (16#90#, 16#00#) then
                  null; -- ignore the error for now
               end if;
               return;
            end if;
         end if;

         if not PN532_Send_R_APDU (16#6D#, 16#00#) then
            null; -- ignore the error
         end if;
         return;
      elsif P1 = 0 then -- select by id
         if P2 = 16#0C# and Rest = (16#02#, 16#E1#, 16#03#) then
            Ok := PN532_Send_R_APDU (16#90#, 16#00#);
            Current_File := FILE_CC;
            return;
         elsif P2 = 16#0C# and Rest = (16#02#, 16#E1#, 16#04#) then
            Ok := PN532_Send_R_APDU (16#90#, 16#00#);
            Current_File := FILE_NDEF;
            return;
         else
            Ok := PN532_Send_R_APDU (16#90#, 16#00#);
            return;
         end if;
      end if;

      if not PN532_Send_R_APDU (16#6D#, 16#00#) then
         null;
      end if;
      if not Ok then
         Log ("failed");
      end if;
   end Handle_Select_File;

   procedure PN532_NFC_Forum_Type_4_Emulate (NDEF_Message : PN532_Buf;
                                             Status : out Boolean)
   is
      Data : PN532_Buf (1..32);
      Data_Len : Unsigned_8;
      Offset : Unsigned_16;
      Ok : Boolean;
      NDEF_Read_Binary_Done : Boolean := False;
   begin
      if not PN532_Init_As_Target then
         Status := False;
         return;
      end if;

      My_Delay (200);

      loop
         PN532_Get_Data (Data, Data_Len, Ok);
         if not Ok then
            Status := NDEF_Read_Binary_Done;
            return;
         end if;

         if Data (1) = 0 then
            case Data (2) is
               when APDU_SELECT_FILE =>
                  if Data_Len > 5 then
                     Handle_Select_File (Data (3), Data (4), Data (5..Data_Len));
                  else
                     -- Log ("short apdu");
                     Ok := PN532_Send_R_APDU (16#6D#, 16#00#);
                  end if;
               when APDU_READ_BINARY =>
                  if Current_File = FILE_CC then
                     Ok := PN532_Set_Data (CC_Reply);
                  elsif Current_File = FILE_NDEF and Data_Len >= 5 then
                     Offset := Unsigned_16 (Data (3)) * 256 + Unsigned_16 (Data (4));
                     if NDEF_Message'First + Unsigned_8 (Offset)
                       + Data (5) - 1 <= NDEF_Message'Last
                     then
                        Ok := PN532_Set_Data
                             (Buf  => NDEF_Message
                                      (NDEF_Message'First + Unsigned_8 (Offset) ..
                                        NDEF_Message'First + Unsigned_8 (Offset) + Data (5) - 1),
                              Buf2 => (16#90#, 16#00#));
                     else
                        Ok := PN532_Send_R_APDU (16#6D#, 16#00#);
                     end if;
                     NDEF_Read_Binary_Done := True;
                  else
                     Ok := PN532_Send_R_APDU (16#6D#, 16#00#);
                  end if;
               when others =>
                  -- Log ("Unsupp. ins");
                  -- instruction not supported, or invalid
                  Ok := PN532_Send_R_APDU (16#6D#, 16#00#);
                  Status := False;
                  return;
            end case;
         else
            if not PN532_Send_R_APDU (16#6E#, 16#00#) then -- class not supported
               -- Log ("PN532_Send_R_APDU failed");
               Status := False;
               return;
            end if;
         end if;
      end loop;
   end PN532_NFC_Forum_Type_4_Emulate;

end Generic_PN532;
