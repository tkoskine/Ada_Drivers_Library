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

with Interfaces;

package PN532_Types is
   type PN532_Buf is array
     (Interfaces.Unsigned_8 range <>) of Interfaces.Unsigned_8;
     
   type Reply_Type is (REPLY_ACK, REPLY_NACK, REPLY_ERROR);
     
   subtype PN532_Buf_4 is PN532_Buf (1..4);
   subtype PN532_Buf_6 is PN532_Buf (1..6);
   
   HOST_TO_PN532 : constant := 16#D4#;
end PN532_Types;