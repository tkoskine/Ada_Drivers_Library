with Interfaces;
with PN532_Types;

use PN532_Types;
use Interfaces;

package PN532_BUS_SPI is
   procedure PN532_Init;

   function PN532_Busy return Boolean;

   procedure PN532_Write (Cmd : PN532_Buf);

   procedure PN532_Read_Reply (Reply : out Reply_Type; Code : out Unsigned_8);

   procedure PN532_Read_Raw (Buf : out PN532_Buf);

   procedure PN532_Read_Data
     (Buf    : out PN532_Buf;
      Len    : out Unsigned_8;
      Status : out Boolean);

end PN532_BUS_SPI;