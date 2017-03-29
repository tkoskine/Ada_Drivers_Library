-- PN532/NFC routines for GNAT for ARM Cortex-Mx
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

with Debug_Uart;
with Ada.Real_Time;      use Ada.Real_Time;
-- with System;
with NDEF;
with PN532_BUS_SPI;
with Generic_PN532;
with Interfaces;
with PN532_Types;

package body NFC_Tags is
   use Interfaces;
   use PN532_Types;
   
   procedure My_Delay (Milli : Natural);

   procedure My_Delay (Milli : Natural) is
   begin
      delay until Clock + Milliseconds (Milli);
   end My_Delay;

   package PN532 is new Generic_PN532
     (PN532_Init => PN532_BUS_SPI.PN532_Init,
      PN532_Busy => PN532_BUS_SPI.PN532_Busy,
      PN532_Write => PN532_BUS_SPI.PN532_Write,
      PN532_Read_Reply => PN532_BUS_SPI.PN532_Read_Reply,
      PN532_Read_Raw => PN532_BUS_SPI.PN532_Read_Raw,
      PN532_Read_Data => PN532_BUS_SPI.PN532_Read_Data);

   procedure Print_Str (Str : String);

   procedure Print_NDEF_Record (NFC_Data    : PN532_Buf;
                                NDEF_Header : NDEF.NDEF_Record)
   is
      use type NDEF.Type_Name_Format;

      Data_End : Unsigned_8;
   begin
      Data_End := Unsigned_8'Min
        (NFC_Data'Last,
         NDEF_Header.Payload_Offset + NDEF_Header.Payload_Length - 1);
      if Data_End < NDEF_Header.Payload_Offset + NDEF_Header.Payload_Length - 1 then
         Debug_Uart.Put ("Note: printing truncated");
         Debug_Uart.New_Line;
         Debug_Uart.Put (Data_End, 10);
         Debug_Uart.New_Line;
         Debug_Uart.Put (NDEF_Header.Payload_Offset + NDEF_Header.Payload_Length - 1, 10);
         Debug_Uart.New_Line;
      end if;

      Debug_Uart.Put ("TNF: ");
      Debug_Uart.Put (Data => NDEF.TNF_To_Unsigned_8 (NDEF_Header.TNF), Base => 16);
      Debug_Uart.New_Line;
      Debug_Uart.Put ("NDEF record at ");
      Debug_Uart.Put (Data => NDEF_Header.Payload_Offset, Base => 10);
      Debug_Uart.New_Line;
      Debug_Uart.Put ("NDEF type: ");
      Debug_Uart.Put (Data => NDEF_Header.NDEF_Type, Base => 16);
      Debug_Uart.New_Line;
      Debug_Uart.Put ("NDEF Payload length: ");
      Debug_Uart.Put (Data => NDEF_Header.Payload_Length, Base => 10);
      Debug_Uart.New_Line;

      if NDEF_Header.TNF = NDEF.TNF_WELL_KNOWN then
         if NDEF_Header.NDEF_Type = Character'Pos ('U') then
            if NFC_Data (NDEF_Header.Payload_Offset) = 1 then -- https
               Debug_Uart.Put ("http://");
            elsif NFC_Data (NDEF_Header.Payload_Offset) = 4 then -- https
               Debug_Uart.Put ("https://");
            end if;
            for I in NDEF_Header.Payload_Offset + 1 .. Data_End loop
               Debug_Uart.Put (Character'Val (NFC_Data (I)) & "");
            end loop;
            Debug_Uart.New_Line;
         end if;
      end if;
   end Print_NDEF_Record;

   procedure Read_Mifare_Classic_Tag (NFC_ID : PN532_Buf) is
      Status : Boolean;
      -- Key_A : PN532_Buf_6 := (255, 255, 255, 255, 255, 255);
      MAD_Key_A : constant PN532_Buf_6 :=
        (16#A0#, 16#A1#,  16#A2#, 16#A3#, 16#A4#, 16#A5#);
      Key_B : constant PN532_Buf_6 := (255, 255, 255, 255, 255, 255);
      Data : PN532_Buf (1 .. 16);
      NFC_Data : PN532_Buf (1 .. 48);
      Len : Unsigned_8;
   begin
--      Debug_Uart.Put ("UID len: ");
--      Debug_Uart.Put (Unsigned_8 (NFC_ID'Length), 10);
--      Debug_Uart.New_Line;
      PN532.PN532_Authenticate_Mifare_Classic_Tag_Block
        (Block_Number => 0,
         UID          => NFC_ID,
         Key          => PN532.AUTH_A,
         Key_Data     => MAD_Key_A,
         Status       => Status);
      if Status then
         Debug_Uart.Put ("Classic tag auth OK");
         Debug_Uart.New_Line;
      else
         Debug_Uart.Put ("Classic tag auth FAILED");
         Debug_Uart.New_Line;
      end if;

      if False then
         PN532.PN532_Read_NFC_Forum_Type_2_Tag_Block
          (Block => 0,
           Buf   => Data,
           Byte_Count => Len,
           Status => Status);
         if Status then
            Debug_Uart.Put ("BLOCK 00: ");
            for I in Unsigned_8 range 1 .. Len loop
               Debug_Uart.Put (Data => Data (I), Base => 16);
               Debug_Uart.Put (" ");
            end loop;
            Debug_Uart.New_Line;
         else
            Debug_Uart.Put ("Read failed");
            Debug_Uart.New_Line;
         end if;
      end if;
      for B in Unsigned_8 range 0 .. 7 loop

         if B mod 4 = 0 and B > 0 then
            PN532.PN532_Authenticate_Mifare_Classic_Tag_Block
              (Block_Number => B,
               UID          => NFC_ID,
               Key          => PN532.AUTH_B,
               Key_Data     => Key_B,
               Status       => Status);
         end if;
         PN532.PN532_Read_NFC_Forum_Type_2_Tag_Block
           (Block => B, -- (B + 1)*4,
            Buf   => Data,
            Byte_Count => Len,
            Status => Status);
         if not Status then
            Debug_Uart.Put ("Failed to read block ");
            Debug_Uart.Put (Data => B, Base => 16);
            Debug_Uart.New_Line;
            exit;
         end if;
         if B*16 + 16 < NFC_Data'Last then
            NFC_Data (B*16 + 1 .. B*16 +16) := Data;
         end if;

         Print_Str("Block:");
         Debug_Uart.Put (Data => B, Base => 16);
         Debug_Uart.Put (" from "); Debug_Uart.Put (B*16 +1, Base => 10);
         Debug_Uart.Put (" to "); Debug_Uart.Put (B*16 + 16, Base => 10);
         Debug_Uart.Put (": ");
         for I in Unsigned_8 range 1 .. Len loop
            Debug_Uart.Put (Data => Data (I), Base => 16);
            Debug_Uart.Put (" ");
         end loop;
         Debug_Uart.New_Line;
      end loop;
   end Read_Mifare_Classic_Tag;

   -- Read NFC Forum type 2 tag (like NXP Mifare Ultralight or NXP NTAG)
   --
   -- First 4 blocks are special */
   -- Format of the blocks is following:
   --                 00 11 22 33 Block
   -- UID:            xx xx xx xx  0
   -- Serial:         xx xx xx xx  1
   -- Internal/Lock:  xx xx xx LL  2
   -- Capabilities:   CC CC CC CC  3
   procedure Read_Forum_Type_2_Tag is
      Len : Unsigned_8;
      Card_Block_Count : Unsigned_8;
      Card_Size : Unsigned_16;
      Status : Boolean;
      Data : PN532_Buf (1 .. 16);
      NFC_Data : PN532_Buf (1 .. 48);
      NDEF_Loc : Unsigned_8 := 0;
      NDEF_Header : NDEF.NDEF_Record;
   begin
      Print_Str ("Type 2 tag"); Debug_Uart.New_Line;
      PN532.PN532_Read_NFC_Forum_Type_2_Tag_Block
       (Block => 0,
        Buf   => Data,
        Byte_Count => Len,
        Status => Status);
      if Status then
         Debug_Uart.Put ("BLOCK 00: ");
         for I in Unsigned_8 range 1 .. Len loop
            Debug_Uart.Put (Data => Data (I), Base => 16);
            Debug_Uart.Put (" ");
         end loop;
         Debug_Uart.New_Line;

         Card_Size := Unsigned_16 (Data (13 + 2)) * 8;
         Card_Block_Count := Unsigned_8 (Card_Size / 4);

         Debug_Uart.Put ("NFC tag size: ");
         Debug_Uart.Put (Data => Card_Size, Base => 10);
         Debug_Uart.Put (" bytes"); Debug_Uart.New_Line;

         if Card_Block_Count > 50 then
            -- Reading only the first 50 blocks
            Card_Block_Count := 50;
         end if;

         -- Read 4 blocks at time, first block is not in Card_Block_Count
         for B in Unsigned_8 range 0 .. (Card_Block_Count / 4) - 1 loop
            PN532.PN532_Read_NFC_Forum_Type_2_Tag_Block
              (Block => (B + 1)*4,
               Buf   => Data,
               Byte_Count => Len,
               Status => Status);
            if not Status then
               Debug_Uart.Put ("Failed to read block ");
               Debug_Uart.Put (Data => B*4, Base => 16);
               Debug_Uart.New_Line;
               exit;
            end if;
            if B*16 + 16 < NFC_Data'Last then
               NFC_Data (B*16 + 1 .. B*16 +16) := Data;
            end if;

            Print_Str ("Block");
            Debug_Uart.Put (Data => (B + 1)*4, Base => 16);
            Debug_Uart.Put (" from "); Debug_Uart.Put (B*16 +1, Base => 10);
            Debug_Uart.Put (" to "); Debug_Uart.Put (B*16 + 16, Base => 10);
            Debug_Uart.Put (": ");
            for I in Unsigned_8 range 1 .. Len loop
               Debug_Uart.Put (Data => Data (I), Base => 16);
               Debug_Uart.Put (" ");
            end loop;
            Debug_Uart.New_Line;
         end loop;
         NDEF.Find_NDEF_TLV (Data => NDEF.NDEF_Array (NFC_Data),
                             Start => NFC_Data'First,
                             Location => NDEF_Loc,
                             TLV_Len => Len,
                             Found => Status);
         if Status then
            Print_Str ("NDEF message:");
            Debug_Uart.Put (Data => NDEF_Loc, Base => 10);
            Debug_Uart.New_Line;

            NDEF.Read_NDEF_Record (NDEF.NDEF_Array (NFC_Data), NDEF_Loc, NDEF_Header, Status);
            if Status then
               Print_NDEF_Record (NFC_Data, NDEF_Header);
            end if;
         else
            Print_Str ("No NDEF TLV found");
            Debug_Uart.New_Line;
         end if;
      else
         Print_Str ("Failed to read block");
         Debug_Uart.New_Line;
      end if;
   end Read_Forum_Type_2_Tag;

   procedure Write_Forum_Type_2_Tag (Data : PN532_Buf) is
      -- use type PN532.NFC_Forum_Type_2_Block;

      Status : Boolean;
      Block : PN532_Buf (1..4);
      I : Unsigned_8 := Data'First;
      Len : Unsigned_8;
      Current_Block : PN532.NFC_Forum_Type_2_Block := 4;
   begin
      loop
         exit when I > Data'Length;
         Len := Data'Length - I + 1;
         Len := Unsigned_8'Min (4, Len);

         exit when Len = 0;

         Block (1..Len) := Data (I..I + Len);
         if Len < 4 then
            Block (Len + 1..4) := (others => 0);
         end if;

         PN532.PN532_Write_NFC_Forum_Type_2_Tag_Block
           (Block_Number => Current_Block,
            Buf          => Block,
            Status       => Status);
         if not Status then
            Print_Str ("Failed to write block");
            Debug_Uart.New_Line;
            exit;
         else
            Debug_Uart.Put ("Wrote block ");
            Debug_Uart.Put (Data => Current_Block, Base => 10);
            Debug_Uart.Put (", ");
            Debug_Uart.Put (Data => Len, Base => 10);
            Debug_Uart.Put ("bytes");
            Debug_Uart.New_Line;
         end if;
         Current_Block := Current_Block + 1;
         I := I + Len;
      end loop;
   end Write_Forum_Type_2_Tag;

   procedure Read_Forum_Type_4_Tag is
      use type NDEF.Type_Name_Format;

      Status : Boolean;
      Data : PN532_Buf (1 .. 16);
      NFC_Data : PN532_Buf (1 .. 80);
      Len : Unsigned_8;
      NDEF_ID : Unsigned_16;
      NDEF_Len : Unsigned_16;
      NDEF_Header : NDEF.NDEF_Record;
   begin
      if not PN532.PN532_NFC_Forum_Type_4_Select_Application then
         Print_Str ("Failed to select type 4 tag"); Debug_Uart.New_Line;
         return;
      end if;

      if not PN532.PN532_NFC_Forum_Type_4_Select_File
        (PN532.CAPABILITY_CONTAINER_FILE)
      then
         Debug_Uart.Put ("Failed to select CC file"); Debug_Uart.New_Line;
         return;
      end if;

      PN532.PN532_NFC_Forum_Type_4_Read_Binary
        (Offset => 0,
         Buf => Data,
         Byte_Count => Len,
         Status => Status);
      if not Status then
         Print_Str ("Failed to read CC file"); Debug_Uart.New_Line;
         return;
      end if;

      NDEF_ID := Unsigned_16 (Data (10)) * 256 + Unsigned_16 (Data (11));

      if not PN532.PN532_NFC_Forum_Type_4_Select_File (NDEF_ID) then
         Print_Str ("NDEF Select failed"); Debug_Uart.New_Line;
         return;
      end if;

      PN532.PN532_NFC_Forum_Type_4_Read_Binary
        (Offset => 0,
         Buf => NFC_Data,
         Byte_Count => Len,
         Status => Status);
      if not Status then
         Print_Str ("Failed to read NDEF file");
         Debug_Uart.New_Line;
         return;
      end if;

      NDEF_Len := Unsigned_16 (NFC_Data (1)) * 256 +
        Unsigned_16 (NFC_Data (2));
      Debug_Uart.Put ("NDEF data is "); Debug_Uart.Put (NDEF_Len, 10);
      Debug_Uart.Put (" bytes:");
      Debug_Uart.New_Line;

      if NDEF_Len > 255 then
         NDEF_Len := 255;
      end if;

      for I in Unsigned_8 range 3 .. Unsigned_8 (NDEF_Len) + 2 loop
         Debug_Uart.Put (Data => NFC_Data (I), Base => 16);
         Debug_Uart.Put (" ");
      end loop;
      Debug_Uart.New_Line;

      -- Read first record
      NDEF.Read_NDEF_Record (NDEF.NDEF_Array (NFC_Data), 3, NDEF_Header, Status);
      if Status then
         Print_NDEF_Record (NFC_Data, NDEF_Header);
--         Debug_Uart.Put ("TNF: ");
--         Debug_Uart.Put (Data => NDEF.TNF_To_Unsigned_8 (NDEF_Header.TNF), Base => 16);
--         Debug_Uart.New_Line;
--         Debug_Uart.Put ("NDEF record at ");
--         Debug_Uart.Put (Data => NDEF_Header.Payload_Offset, Base => 10);
--         Debug_Uart.New_Line;
--         Debug_Uart.Put ("NDEF type: ");
--         Debug_Uart.Put (Data => NDEF_Header.NDEF_Type, Base => 16);
--         Debug_Uart.New_Line;
--         Debug_Uart.Put ("NDEF Payload length: ");
--         Debug_Uart.Put (Data => NDEF_Header.Payload_Length, Base => 10);
--         Debug_Uart.New_Line;
--
--         if NDEF_Header.TNF = NDEF.TNF_WELL_KNOWN then
--            if NDEF_Header.NDEF_Type = Character'Pos ('U') then
--               if NFC_Data (NDEF_Header.Payload_Offset) = 4 then -- https
--                  Debug_Uart.Put ("https://");
--               end if;
--               for I in NDEF_Header.Payload_Offset + 1 .. (NDEF_Header.Payload_Offset + NDEF_Header.Payload_Length - 1) loop
--                  Debug_Uart.Put (Character'Val (NFC_Data (I)));
--               end loop;
--               Debug_Uart.New_Line;
--            end if;
--         end if;
      else
         Debug_Uart.Put ("Failed to parse NDEF record");
         Debug_Uart.New_Line;
      end if;
   end Read_Forum_Type_4_Tag;

   procedure Update_Forum_Type_4_Tag (New_Content : PN532_Buf)
   is
      Status   : Boolean;
      Data     : PN532_Buf (1 .. 16);
      NFC_Data : PN532_Buf (1 .. 2);
      Len      : Unsigned_8;
      NDEF_ID  : Unsigned_16;
   begin
      if not PN532.PN532_NFC_Forum_Type_4_Select_Application then
         Print_Str ("Failed to select type 4 tag"); Debug_Uart.New_Line;
         return;
      else
         Debug_Uart.Put ("Type 4 tag selected");
         Debug_Uart.New_Line;
      end if;

      if not PN532.PN532_NFC_Forum_Type_4_Select_File
        (PN532.CAPABILITY_CONTAINER_FILE)
      then
         Debug_Uart.Put ("Failed to select CC file"); Debug_Uart.New_Line;
         return;
      end if;

      PN532.PN532_NFC_Forum_Type_4_Read_Binary
        (Offset => 0,
         Buf => Data,
         Byte_Count => Len,
         Status => Status);
      if not Status then
         Print_Str ("Failed to read CC file from tag");
         Debug_Uart.New_Line;
         return;
      end if;

      NDEF_ID := Unsigned_16 (Data (10)) * 256 + Unsigned_16 (Data (11));
      Debug_Uart.Put ("NDEF file ID: ");
      Debug_Uart.Put (Data => NDEF_ID, Base => 16); Debug_Uart.New_Line;

      if not PN532.PN532_NFC_Forum_Type_4_Select_File (NDEF_ID) then
         Print_Str ("Failed to select NDEF file"); Debug_Uart.New_Line;
         return;
      else
         Debug_Uart.Put ("NDEF file selected"); Debug_Uart.New_Line;
      end if;

      PN532.PN532_NFC_Forum_Type_4_Read_Binary
        (Offset => 0,
         Buf => NFC_Data,
         Byte_Count => Len,
         Status => Status);
      if not Status then
         Print_Str ("Failed to read NDEF file");
         Debug_Uart.New_Line;
         return;
      end if;

      PN532.PN532_NFC_Forum_Type_4_Update_Binary
        (Offset => 0,
         Buf    => New_Content,
         Status => Status);
      if not Status then
         Debug_Uart.Put ("Failed to update NDEF file"); Debug_Uart.New_Line;
         return;
      else
         Debug_Uart.Put ("Updated the NDEF file");
         Debug_Uart.New_Line;
      end if;
   end Update_Forum_Type_4_Tag;

   procedure Read_Tag is
      Sel_Res  : Unsigned_8;
      Sens_Res : Unsigned_16;
      NFC_ID   : PN532_Buf (1..16);
      NFC_ID_Len : Unsigned_8;
      Status   : Boolean;
   begin
      Debug_Uart.Put ("Reading NFC tag");
      Debug_Uart.New_Line;
      Print_Str ("Bring tag nearby");
      Debug_Uart.New_Line;

      loop
         PN532.PN532_Detect_Tag (Sens_Res => Sens_Res,
                                 Sel_Res => Sel_Res,
                                 NFC_ID => NFC_ID,
                                 NFC_ID_Len => NFC_ID_Len,
                                 Status => Status);
         if Status then
            -- NFC Forum type 2 tag
            if Sel_Res = 16#00# and Sens_Res = 16#4400# then
               Read_Forum_Type_2_Tag;
            -- NFC Forum type 4 tag
            elsif Sel_Res = 16#08# and Sens_Res = 16#0400# then
               Debug_Uart.Put ("Mifare classic tag MF1S503");
               Debug_Uart.New_Line;
               Read_Mifare_Classic_Tag
                 (NFC_ID (NFC_ID'First..NFC_ID'First + NFC_ID_Len - 1));
            elsif Sel_Res = 16#08# and Sens_Res = 16#4400# then
               Debug_Uart.Put ("Mifare classic tag MF1S500");
               Debug_Uart.New_Line;
               Read_Mifare_Classic_Tag
                 (NFC_ID (NFC_ID'First..NFC_ID'First + NFC_ID_Len - 1));
            elsif Sel_Res = 16#20# and Sens_Res = 16#4403# then
               Read_Forum_Type_4_Tag;
            elsif Sel_Res = 16#28# and Sens_Res = 16#4400# then -- Yubikey NEO
               -- Read_Mifare_Classic_Tag
               --   (NFC_ID (NFC_ID'First..NFC_ID'First + NFC_ID_Len - 1));
               Read_Forum_Type_4_Tag;
            else
               Debug_Uart.Put ("Unknown tag detected: ");
               Debug_Uart.Put (Sel_Res, 16);
               Debug_Uart.Put (" ");
               Debug_Uart.Put (Sens_Res, 16);
               Debug_Uart.New_Line;
            end if;

            return;
         end if;
      end loop;
   end Read_Tag;

   procedure Write_Tag is
      use Debug_Uart;

      NDEF_Data : PN532_Buf (1..18) :=
        (03, 15, -- len
         16#D1#, 16#01#, 16#09#, 16#54#,
         16#02#, -- plain text
         16#65#, 16#6E#, -- 'en'
         Character'Pos ('1'),
         Character'Pos ('2'),
         Character'Pos ('3'),
         Character'Pos ('4'),
         Character'Pos ('5'),
         Character'Pos ('6'),
         Character'Pos ('7'),
         Character'Pos ('8'),
         16#FE#
        );

      Ch : Character;
      Len : Unsigned_8 := 0;
      My_String : PN532_Buf (1..8);
      NFC_ID : PN532_Buf (1..7);
      NFC_ID_Len : Unsigned_8;
      Sel_Res  : Unsigned_8;
      Sens_Res : Unsigned_16;
      Status   : Boolean;
   begin
      Print_Str ("Enter 8 characters");
      Debug_Uart.New_Line;
      loop
         Ch := Get;
         if Ch = Character'Val (10)
           or Ch = Character'Val (13)
         then
            Debug_Uart.New_Line;
            exit;
         else
            Debug_Uart.Put (Ch & "");
         end if;

         Len := Len + 1;
         My_String (Len) := Character'Pos (Ch);
         if Len = 8 then
            Debug_Uart.New_Line;
            exit;
         end if;
      end loop;

      if Len = 0 then
         return;
      end if;

      Print_Str ("Bring tag nearby");
      Debug_Uart.New_Line;

      loop
         PN532.PN532_Detect_Tag (Sens_Res => Sens_Res,
                                 Sel_Res => Sel_Res,
                                 NFC_ID => NFC_ID,
                                 NFC_ID_Len => NFC_ID_Len,
                                 Status => Status);
         if Status then
            -- NFC Forum type 2 tag
            if Sel_Res = 16#00# and Sens_Res = 16#4400# then
               Debug_Uart.Put ("Writing type 2 tag"); Debug_Uart.New_Line;
               NDEF_Data (10..9 + Len) := My_String (1..Len);
               NDEF_Data (2) := Len + 7;
               NDEF_Data (5) := Len + 3;
               NDEF_Data (10 + Len) := 16#FE#; -- END of  data
               Write_Forum_Type_2_Tag (NDEF_Data (1..10+Len));

            -- NFC Forum type 4 tag
            elsif Sel_Res = 16#20# and Sens_Res = 16#4403# then
               Debug_Uart.Put ("Writing type 4 tag"); Debug_Uart.New_Line;

               NDEF_Data (10..9 + Len) := My_String (1..Len);
               NDEF_Data (1) := 0;
               NDEF_Data (2) := Len + 7;
               NDEF_Data (5) := Len + 3;
               if 9 + Len < NDEF_Data'Last then
                  NDEF_Data (10 + Len .. NDEF_Data'Last) := (others => 0);
               end if;

               Update_Forum_Type_4_Tag (NDEF_Data);
            else
               Debug_Uart.Put ("Unknown tag detected");
               Debug_Uart.New_Line;
            end if;
            exit;
         end if;
      end loop;

      Debug_Uart.Put ("Tag data written.");
      Debug_Uart.New_Line;
   end Write_Tag;


   NDEF_Hello : constant PN532_Buf (1..14) :=
     (00, 12, -- len
      16#D1#, 16#01#, 16#08#, 16#54#,
      16#02#, -- plain text
      16#65#, 16#6E#, -- 'en'
      Character'Pos ('H'),
      Character'Pos ('e'),
      Character'Pos ('l'),
      Character'Pos ('l'),
      Character'Pos ('o')
     );

   procedure Emulate_Tag is
      Status : Boolean;
      Counter : Unsigned_8 := 0;
   begin
      Print_Str ("Bring NFC reader nearby"); Debug_Uart.New_Line;
      loop
         PN532.PN532_NFC_Forum_Type_4_Emulate (NDEF_Hello, Status);
         exit when Status;
         Debug_Uart.Put (".");
         delay until Clock + Milliseconds (100);
         Counter := Counter + 1;
         if Counter = 15 then
            Debug_Uart.New_Line;
            -- Debug_Uart.Put ("No reader detected, exiting");
            -- Debug_Uart.New_Line;
            Counter := 0;
         end if;
      end loop;
      Print_Str ("Emulation over");
      Debug_Uart.New_Line;
   end Emulate_Tag;

   procedure Print_Str (Str : String) is
   begin
      Debug_Uart.Put (Str);
   end Print_Str;
   
   procedure Main_Menu is
      use Debug_Uart;

      Ch : Character;

   begin
      loop
         New_Line;
         Print_Str ("Select action"); New_Line;
         Print_Str ("1 - read tag"); New_Line;
         Print_Str ("2 - write tag"); New_Line;
         Print_Str ("3 - emulate tag"); New_Line;

         Ch := Get;

         if Ch = '1' then
            Read_Tag;
         elsif Ch = '2' then
            Write_Tag;
         elsif Ch = '3' then
            Emulate_Tag;
         else
            Print_Str ("Invalid option"); New_Line;
         end if;
      end loop;
   end Main_Menu;


   procedure Main is
   begin
      -- AVR.Interrupts.Enable;
      Debug_Uart.Initialize;
      Debug_Uart.Put_Line ("PN532 Example");
      My_Delay (300);
      PN532.Init;

      -- Debug_Uart.Put ("Firmware: ");
      -- Debug_Uart.Put (Data => PN532.PN532_Read_Firmware, Base => 16);
      -- Debug_Uart.New_Line;

      if not PN532.PN532_SAM_Config then
         Debug_Uart.Put ("SAM config failed");
         Debug_Uart.New_Line;
         loop
            null;
         end loop;
      end if;

      Main_Menu;
   end Main;
end NFC_Tags;