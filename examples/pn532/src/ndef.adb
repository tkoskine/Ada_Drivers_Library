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

with Debug_Uart;
use Debug_Uart;

package body NDEF is
   use Interfaces;

   NULL_TLV       : constant := 16#00#;
   LOCK_TLV       : constant := 16#01#;
   MEMORY_TLV     : constant := 16#02#;
   NDEF_TLV       : constant := 16#03#;
   TERMINATOR_TLV : constant := 16#FE#;

   procedure Find_NDEF_TLV (Data     : NDEF_Array;
                            Start    : Interfaces.Unsigned_8;
                            Location : out Interfaces.Unsigned_8;
                            TLV_Len  : out Interfaces.Unsigned_8;
                            Found    : out Boolean)
   is
      Pos : Unsigned_8 := Start;
      Len : Unsigned_8 := 0;
   begin
      loop
         exit when Pos > Data'Last;

         case Data (Pos) is
            when NULL_TLV =>
               Pos := Pos + 1;
            when LOCK_TLV | MEMORY_TLV =>
               Put ("Lock or memory TLV at ");
               Put (Pos, 10);
               New_Line;
               Pos := Pos + 1;
               Len := Data (Pos);
               Pos := Pos + 1;
               Pos := Pos + Len;

               Put ("new pos ");
               Put (Pos, 10);
               New_Line;
            when TERMINATOR_TLV =>
               Put ("TERMINATOR TLV at "); Put (Pos, 10); New_Line;
               Found := False;
               Location := Pos + 1;
               exit;
            when NDEF_TLV =>
               Pos := Pos + 1;
               TLV_Len := Data (Pos);
               Pos := Pos + 1;
               Location := Pos;
               Found := True;
               return;
            when others =>
               Put ("Unknown TLV "); Put (Data (Pos), 10); Put (" at "); Put (Pos, 10); New_Line;
               Pos := Pos + 1;
               Len := Data (Pos);
               Pos := Pos + 1;
               Pos := Pos + Len;
         end case;
      end loop;
      Found    := False;
      Location := Pos;
   end Find_NDEF_TLV;

--   type NDEF_Record is record
--      Message_Location : NDEF_Message_Location;
--      Chunked : Boolean;
--      Short_Record : Boolean;
--      ID_Length_Present : Boolean;
--      TNF : Type_Name_Format;
--      Payload_Offset : Interfaces.Unsigned_8;
--      Payload_Length : Interfaces.Unsigned_8;
--   end record;

   -- Parse the NDEF record from the data
   -- Header format:
   --    MB|ME|CF|SR|IL|TNF
   --    Type Length
   --    Payload len 4
   --    Payload len 3
   --    Payload len 2
   --    Payload len 1
   --    ID Len
   --    Type
   --    ID
   --    Payload
   procedure Read_NDEF_Record (Data        : NDEF_Array;
                               Start       : Interfaces.Unsigned_8;
                               NDEF_Header : out NDEF_Record;
                               Status      : out Boolean)
   is
      CF_Mask     : constant Unsigned_8 := 2#0010_0000#;
      SR_Mask     : constant Unsigned_8 := 2#0001_0000#;
      ID_Len_Mask : constant Unsigned_8 := 2#0000_1000#;
      TNF_Mask    : constant Unsigned_8 := 2#0000_0111#;

      Current_Location : Unsigned_8 := Start;
      Type_Len : Unsigned_8;
      ID_Len : Unsigned_8 := 0;
   begin
      if Start > Data'Last then
         Status := False;
         return;
      end if;

      declare
         Header_Bits : constant Unsigned_8 := Data (Current_Location);
      begin
         NDEF_Header.Chunked := (Header_Bits and CF_Mask) = CF_Mask;
         NDEF_Header.Short_Record := (Header_Bits and SR_Mask) = SR_Mask;
         NDEF_Header.ID_Length_Present := (Header_Bits and ID_Len_Mask) = ID_Len_Mask;
         NDEF_Header.TNF := To_TNF (Header_Bits and TNF_Mask);
      end;
      Current_Location := Current_Location + 1;

      -- Read Type length
      Type_Len := Data (Current_Location);
      Current_Location := Current_Location + 1;

      -- Read Payload length
      if NDEF_Header.Short_Record then
         NDEF_Header.Payload_Length := Data (Current_Location);
      else
         -- Long payload length not supported (not enough memory)
         Status := False;
         return;
      end if;
      Current_Location := Current_Location + 1;

      -- ID Length
      if NDEF_Header.ID_Length_Present then
         ID_Len := Data (Current_Location);
         Current_Location := Current_Location + 1;
      else
         ID_Len := 0;
      end if;

      -- Type
      if Type_Len > 1 then -- types longer than 1 byte not supported
         Status := False;
         return;
      else
         NDEF_Header.NDEF_Type := Data (Current_Location);
      end if;
      Current_Location := Current_Location + 1;

      -- ID, skip it
      Current_Location := Current_Location + ID_Len;

      NDEF_Header.Payload_Offset := Current_Location;

      Status := True;
      return;
   end Read_NDEF_Record;
end NDEF;
