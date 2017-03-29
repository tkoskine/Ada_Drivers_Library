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

with Interfaces;
with PN532_Types;

use PN532_Types;

generic
   with procedure PN532_Init;

   with function PN532_Busy return Boolean;

   with procedure PN532_Write (Cmd : PN532_Buf);

   with procedure PN532_Read_Reply
     (Reply : out Reply_Type; Code : out Interfaces.Unsigned_8);

   with procedure PN532_Read_Raw (Buf : out PN532_Buf);

   with procedure PN532_Read_Data
     (Buf    : out PN532_Buf;
      Len    : out Interfaces.Unsigned_8;
      Status : out Boolean);
package Generic_PN532 is

   subtype PN532_Buf_4 is PN532_Buf (1..4);
   subtype PN532_Buf_6 is PN532_Buf (1..6);

   subtype NFC_Forum_Type_2_Block is Interfaces.Unsigned_8 range 0 .. 63;
   subtype Mifare_Classic_Block is Interfaces.Unsigned_8 range 0 .. 255;

   type Mifare_Auth is (AUTH_A, AUTH_B);

   CAPABILITY_CONTAINER_FILE : constant := 16#E103#;

   procedure Init;

   function PN532_SAM_Config return Boolean;
   function PN532_Read_Firmware return Interfaces.Unsigned_32;
   procedure PN532_Detect_Tag
     (Sens_Res : out Interfaces.Unsigned_16;
      Sel_Res  : out Interfaces.Unsigned_8;
      NFC_ID  : out PN532_Buf;
      NFC_ID_Len : out Interfaces.Unsigned_8;
      Status   : out Boolean);

   procedure PN532_Authenticate_Mifare_Classic_Tag_Block
      (Block_Number : Mifare_Classic_Block;
       UID          : PN532_Buf;
       Key          : Mifare_Auth;
       Key_Data     : PN532_Buf_6;
       Status       : out Boolean);

   procedure PN532_Read_NFC_Forum_Type_2_Tag_Block
     (Block      : NFC_Forum_Type_2_Block;
      Buf        : out PN532_Buf;
      Byte_Count : out Interfaces.Unsigned_8;
      Status     : out Boolean);

   procedure PN532_Write_NFC_Forum_Type_2_Tag_Block
     (Block_Number : NFC_Forum_Type_2_Block;
      Buf          : PN532_Buf_4;
      Status       : out Boolean);

   function PN532_NFC_Forum_Type_4_Select_Application return Boolean;

   function PN532_NFC_Forum_Type_4_Select_File
     (File_ID : Interfaces.Unsigned_16) return Boolean;

   procedure PN532_NFC_Forum_Type_4_Read_Binary
     (Offset     : Interfaces.Unsigned_16;
      Buf        : out PN532_Buf;
      Byte_Count : out Interfaces.Unsigned_8;
      Status     : out Boolean);

   procedure PN532_NFC_Forum_Type_4_Emulate (NDEF_Message : PN532_Buf;
                                             Status : out Boolean);

   procedure PN532_NFC_Forum_Type_4_Update_Binary
     (Offset : Interfaces.Unsigned_16;
      Buf    : PN532_Buf;
      Status : out Boolean);
end Generic_PN532;
