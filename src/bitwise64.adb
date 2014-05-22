with System.Machine_Code;
use  System.Machine_Code;

package body Bitwise64 is


   ---------
   -- BSF --
   ---------

   function BSF
     (Value : Unsigned_64) return Position
   is
      Result : Position := 0;
   begin
      pragma Assert (Value /= 0, "Bitwise_OPS BSF Value = 0.");
      Asm ("bsfq %%rbx, %%rax",
           Outputs => Position'Asm_Output ("=a", Result),
           Inputs  => Unsigned_64'Asm_Input ("b", Value));
      return Result;
   end BSF;
   pragma Inline (BSF);


   ---------
   -- BSR --
   ---------

   function BSR
     (Value : Unsigned_64) return Position
   is
      Result : Position := 0;
   begin
      pragma Assert (Value /= 0, "Bitwise_OPS BSR Value = 0.");
      Asm ("bsrq %%rbx, %%rax",
           Outputs => Position'Asm_Output ("=a", Result),
           Inputs  => Unsigned_64'Asm_Input ("b", Value));
      return Result;
   end BSR;
   pragma Inline (BSR);


   ---------
   -- BTS --
   ---------

   procedure BTS
     (Value : in out Unsigned_64;
      Bit   : in     Position)
   is
   begin
      Asm ("btsq %%rbx, %%rax",
        Outputs => Unsigned_64'Asm_Output ("=a", Value),
        Inputs  => (Unsigned_64'Asm_Input ("a", Value),
                    Position'Asm_Input ("b", Bit)));
   end BTS;
   pragma Inline (BTS);


   ---------
   -- BTR --
   ---------

   procedure BTR
     (Value : in out Unsigned_64;
      Bit   : in     Position)
   is
   begin
      Asm ("btrq %%rbx, %%rax",
        Outputs => Unsigned_64'Asm_Output ("=a", Value),
        Inputs  => (Unsigned_64'Asm_Input ("a", Value),
                    Position'Asm_Input ("b", Bit)));
   end BTR;
   pragma Inline (BTR);


   ---------
   -- BTC --
   ---------

   procedure BTC
     (Value : in out Unsigned_64;
      Bit   : in     Position)
   is
   begin
      Asm ("btcq %%rbx, %%rax",
        Outputs => Unsigned_64'Asm_Output ("=a", Value),
        Inputs  => (Unsigned_64'Asm_Input ("a", Value),
                    Position'Asm_Input ("b", Bit)));
   end BTC;
   pragma Inline (BTC);


   ---------
   -- SHL --
   ---------

   function SHL
     (Value : in Unsigned_64;
      NBits : in Position) return Unsigned_64
   is
      Result : Unsigned_64;
   begin
      Asm ("shlq %%cl, %%rax",
        Outputs => Unsigned_64'Asm_Output ("=a", Result),
        Inputs  => (Unsigned_64'Asm_Input ("a", Value),
                    Position'Asm_Input ("c", NBits)));
      return Result;
   end SHL;
   pragma Inline (SHL);


   ---------
   -- SHR --
   ---------

   function SHR
     (Value : in Unsigned_64;
      NBits : in Position)
      return Unsigned_64
   is
      Result : Unsigned_64;
   begin
      Asm ("shrq %%cl, %%rax",
        Outputs => Unsigned_64'Asm_Output ("=a", Result),
        Inputs  => (Unsigned_64'Asm_Input ("a", Value),
                    Position'Asm_Input ("c", NBits)));
      return Result;
   end SHR;
   pragma Inline (SHR);

end Bitwise64;
