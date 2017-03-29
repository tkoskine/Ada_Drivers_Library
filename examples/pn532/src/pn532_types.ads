with Interfaces;

package PN532_Types is
   type PN532_Buf is array
     (Interfaces.Unsigned_8 range <>) of Interfaces.Unsigned_8;
     
   type Reply_Type is (REPLY_ACK, REPLY_NACK, REPLY_ERROR);
     
   subtype PN532_Buf_4 is PN532_Buf (1..4);
   subtype PN532_Buf_6 is PN532_Buf (1..6);
   
   HOST_TO_PN532 : constant := 16#D4#;
end PN532_Types;