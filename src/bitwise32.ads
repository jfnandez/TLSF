with Interfaces;
use  Interfaces;

package Bitwise32 is

   pragma Pure;

   -- Bit Position in an 32 bit word.
   type Position is range 0 .. 31;

   -- Shift Left : Shifts bits in "Value" "Bits" positions to the left.
   function SHL
     (Value : in Unsigned_32;
      NBits : in Position) return Unsigned_32;

   -- Shift Right: Shifts bits in "Value" "Bits" positions to the right.
   function SHR
     (Value : in Unsigned_32;
      NBits : in Position) return Unsigned_32;


   -- Bit Scan Forward : Returns the position of the least signif. bit set.
   function BSF
     (Value : in Unsigned_32) return Position;

   -- Bit Scan Reverse : Returns the position of the most signif. bit set.
   function BSR
     (Value : in Unsigned_32) return Position;


   -- Bit Test and Set : Sets the "Bit"'th bit in "Value".
   procedure BTS
     (Value : in out Unsigned_32;
      Bit   : in     Position);

   -- Bit Test and Reset : Resets the "Bit"'th bit in "Value".
   procedure BTR
     (Value : in out Unsigned_32;
      Bit   : in     Position);

   -- Bit Test and Complement : Complements the "Bit"'th bit in "Value".
   procedure BTC
     (Value : in out Unsigned_32;
      Bit   : in     Position);

private

   for Position'Size use 8;

end Bitwise32;
