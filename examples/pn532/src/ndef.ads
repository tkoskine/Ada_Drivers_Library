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
with Ada.Unchecked_Conversion;

package NDEF is
   type NDEF_Array is array
     (Interfaces.Unsigned_8 range <>) of Interfaces.Unsigned_8;

   type NDEF_Message_Location is (MESSAGE_BEGIN, MESSAGE_MIDDLE, MESSAGE_END);

   type Type_Name_Format is
     (TNF_EMPTY,
      TNF_WELL_KNOWN,
      TNF_MEDIA,
      TNF_ABSOLUTE_URI,
      TNF_EXTERNAL,
      TNF_UNKNOWN,
      TNF_UNCHANGED,
      TNF_RESERVED);
   for Type_Name_Format'Size use 8;
   for Type_Name_Format use
     (TNF_EMPTY        => 0,
      TNF_WELL_KNOWN   => 1,
      TNF_MEDIA        => 2,
      TNF_ABSOLUTE_URI => 3,
      TNF_EXTERNAL     => 4,
      TNF_UNKNOWN      => 5,
      TNF_UNCHANGED    => 6,
      TNF_RESERVED     => 7);
   function To_TNF is
     new Ada.Unchecked_Conversion (Interfaces.Unsigned_8, Type_Name_Format);
   function TNF_To_Unsigned_8 is
     new Ada.Unchecked_Conversion (Type_Name_Format, Interfaces.Unsigned_8);

   type NDEF_Record is record
      Message_Location : NDEF_Message_Location;
      Chunked : Boolean;
      Short_Record : Boolean;
      ID_Length_Present : Boolean;
      TNF : Type_Name_Format;
      NDEF_Type : Interfaces.Unsigned_8;
      Payload_Offset : Interfaces.Unsigned_8;
      Payload_Length : Interfaces.Unsigned_8;
   end record;

   procedure Find_NDEF_TLV (Data     : NDEF_Array;
                            Start    : Interfaces.Unsigned_8;
                            Location : out Interfaces.Unsigned_8;
                            TLV_Len  : out Interfaces.Unsigned_8;
                            Found    : out Boolean);

   procedure Read_NDEF_Record (Data        : NDEF_Array;
                               Start       : Interfaces.Unsigned_8;
                               NDEF_Header : out NDEF_Record;
                               Status      : out Boolean);
end NDEF;
