with System.Machine_Code;
use  System.Machine_Code;

package body Bitwise32 is


   ---------
   -- BSF --
   ---------

   function BSF
     (Value : Unsigned_32) return Position
   is
      Result : Position := 0;
   begin
      pragma Assert (Value /= 0, "Bitwise_OPS BSF Value = 0.");
      Asm ("bsfl %%ebx, %%eax",
           Outputs => Position'Asm_Output ("=a", Result),
           Inputs  => Unsigned_32'Asm_Input ("b", Value));
      return Result;
   end BSF;
   pragma Inline (BSF);


   ---------
   -- BSR --
   ---------

   function BSR
     (Value : Unsigned_32) return Position
   is
      Result : Position := 0;
   begin
      pragma Assert (Value /= 0, "Bitwise_OPS BSR Value = 0.");
      Asm ("bsrl %%ebx, %%eax",
           Outputs => Position'Asm_Output ("=a", Result),
           Inputs  => Unsigned_32'Asm_Input ("b", Value));
      return Result;
   end BSR;
   pragma Inline (BSR);


   ---------
   -- BTS --
   ---------

   procedure BTS
     (Value : in out Unsigned_32;
      Bit   : in     Position)
   is
   begin
      Asm ("btsl %%ebx, %%eax",
        Outputs => Unsigned_32'Asm_Output ("=a", Value),
        Inputs  => (Unsigned_32'Asm_Input ("a", Value),
                    Position'Asm_Input ("b", Bit)));
   end BTS;
   pragma Inline (BTS);


   ---------
   -- BTR --
   ---------

   procedure BTR
     (Value : in out Unsigned_32;
      Bit   : in     Position)
   is
   begin
      Asm ("btrl %%ebx, %%eax",
        Outputs => Unsigned_32'Asm_Output ("=a", Value),
        Inputs  => (Unsigned_32'Asm_Input ("a", Value),
                    Position'Asm_Input ("b", Bit)));
   end BTR;
   pragma Inline (BTR);


   ---------
   -- BTC --
   ---------

   procedure BTC
     (Value : in out Unsigned_32;
      Bit   : in     Position)
   is
   begin
      Asm ("btcl %%ebx, %%eax",
        Outputs => Unsigned_32'Asm_Output ("=a", Value),
        Inputs  => (Unsigned_32'Asm_Input ("a", Value),
                    Position'Asm_Input ("b", Bit)));
   end BTC;
   pragma Inline (BTC);


   ---------
   -- SHL --
   ---------

   function SHL
     (Value : in Unsigned_32;
      NBits : in Position) return Unsigned_32
   is
      Result : Unsigned_32;
   begin
      Asm ("shll %%cl, %%eax",
        Outputs => Unsigned_32'Asm_Output ("=a", Result),
        Inputs  => (Unsigned_32'Asm_Input ("a", Value),
                    Position'Asm_Input ("c", NBits)));
      return Result;
   end SHL;
   pragma Inline (SHL);


   ---------
   -- SHR --
   ---------

   function SHR
     (Value : in Unsigned_32;
      NBits : in Position)
      return Unsigned_32
   is
      Result : Unsigned_32;
   begin
      Asm ("shrl %%cl, %%eax",
        Outputs => Unsigned_32'Asm_Output ("=a", Result),
        Inputs  => (Unsigned_32'Asm_Input ("a", Value),
                    Position'Asm_Input ("c", NBits)));
      return Result;
   end SHR;
   pragma Inline (SHR);

end Bitwise32;
