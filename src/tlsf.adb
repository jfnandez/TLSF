with System.Address_To_Access_Conversions;

package body TLSF is


   ---------------------
   -- Alignment Mask ---
   ---------------------

   Align_Mask : constant Unsigned_Sys := 2#0011#; -- 4 bytes alignment


   -----------
   -- Align --
   -----------

   function Align (Size : in Storage_Count) return Storage_Count is
      Size_64 : constant Unsigned_Sys :=
        (Unsigned_Sys(Size) + Align_Mask) and (not Align_Mask);
   begin
      return Storage_Count(Size_64);
   end Align;
   pragma Inline (Align);


   ----------------
   -- Is_Aligned --
   ----------------

   function Is_Aligned (Size : in Storage_Count) return Boolean is
      Size_64 : constant Unsigned_Sys := Unsigned_Sys(Size);
   begin
      return (Size_64 and Align_Mask) = 0;
   end Is_Aligned;
   pragma Inline (Is_Aligned);


   -------------------------
   -- Address Conversions --
   -------------------------

   package Convert is new Address_To_Access_Conversions(Block);


   -----------------
   -- Block_Start --
   -----------------

   function Block_Start (Addr : in Address) return Block_Ptr is
   begin
      pragma Assert (Addr /= Null_Address);
      return Block_Ptr(Convert.To_Pointer(Addr));
   end Block_Start;
   pragma Inline (Block_Start);


   -----------------
   -- Block_Start --
   -----------------

   function Block_Start (Hdr_Ptr : in Block_Ptr) return Address is
   begin
      pragma Assert (Hdr_Ptr /= null);
      return Convert.To_Address (Convert.Object_Pointer(Hdr_Ptr));
   end Block_Start;
   pragma Inline (Block_Start);


   ----------------
   -- Data_Start --
   ----------------

   function Data_Start (Addr : in Address) return Block_Ptr is
   begin
      pragma Assert (Addr /= Null_Address);
      return Block_Start(Addr - Used_Header_Size);
   end Data_Start;
   pragma Inline (Data_Start);


   ----------------
   -- Data_Start --
   ----------------

   function Data_Start (Hdr_Ptr : in Block_Ptr) return Address is
   begin
      pragma Assert (Hdr_Ptr /= null);
      return Block_Start(Hdr_Ptr) + Used_Header_Size;
   end Data_Start;
   pragma Inline (Data_Start);


   ------------------
   -- Label Masks ---
   ------------------

   Last_Mask : constant Unsigned_Sys :=     2#0001#;
   Used_Mask : constant Unsigned_Sys :=     2#0010#;
   Size_Mask : constant Unsigned_Sys := not 2#0011#;


   --------------
   -- Get_Size --
   --------------

   function Get_Size (Hdr_Ptr : in Block_Ptr) return Storage_Count is
      Size : Storage_Count;
   begin
      pragma Assert (Hdr_Ptr /= null);
      pragma Assert (Is_Free(Hdr_Ptr));
      Size := Storage_Count(Hdr_Ptr.Label and Size_Mask);
      pragma Assert(Is_Aligned(Size));
      pragma Assert(Size >= Min_Block_Size);
      return Size;
   end Get_Size;
   pragma Inline (Get_Size);


   --------------
   -- Set_Size --
   --------------

   procedure Set_Size (Hdr_Ptr : in Block_Ptr; Size : in Storage_Count) is
      Mask_Data : constant Unsigned_Sys := Hdr_Ptr.Label and (not Size_Mask);
      Mask_Size : constant Unsigned_Sys := Unsigned_Sys(Size) and Size_Mask;
   begin
      pragma Assert (Hdr_Ptr /= null);
      pragma Assert (Is_Free(Hdr_Ptr));
      pragma Assert (Is_Aligned(Size));
      pragma Assert (Size >= Min_Block_Size);
      Hdr_Ptr.Label := Mask_Data xor Mask_Size;
   end Set_Size;
   pragma Inline (Set_Size);


   -------------
   -- Is_Used --
   -------------

   function Is_Used (Hdr_Ptr : in Block_Ptr) return Boolean is
   begin
      pragma Assert (Hdr_Ptr /= null);
      return (Hdr_Ptr.Label and Used_Mask) = Used_Mask;
   end Is_Used;
   pragma Inline (Is_Used);


   -------------
   -- Is_Free --
   -------------

   function Is_Free (Hdr_Ptr : in Block_Ptr) return Boolean is
   begin
      pragma Assert (Hdr_Ptr /= null);
      return (Hdr_Ptr.Label and Used_Mask) = 0;
   end Is_Free;
   pragma Inline (Is_Free);


   --------------
   -- Set_Used --
   --------------

   procedure Set_Used (Hdr_Ptr : in Block_Ptr) is
   begin
      pragma Assert (Hdr_Ptr /= null);
      pragma Assert (Is_Free(Hdr_Ptr));
      Hdr_Ptr.Label := Hdr_Ptr.Label or Used_Mask;
   end Set_Used;
   pragma Inline (Set_Used);


   --------------
   -- Set_Free --
   --------------

   procedure Set_Free (Hdr_Ptr : in Block_Ptr) is
   begin
      pragma Assert (Hdr_Ptr /= null);
      pragma Assert (Is_Used(Hdr_Ptr));
      Hdr_Ptr.Label := Hdr_Ptr.Label and (not Used_Mask);
   end Set_Free;
   pragma Inline (Set_Free);


   -------------
   -- Is_Last --
   -------------

   function Is_Last (Hdr_Ptr : in Block_Ptr) return Boolean is
   begin
      pragma Assert (Hdr_Ptr /= null);
      pragma Assert (Is_Free(Hdr_Ptr));
      return (Hdr_Ptr.Label and Last_Mask) = Last_Mask;
   end Is_Last;
   pragma Inline (Is_Last);


   --------------
   -- Is_First --
   --------------

   function Is_First (Hdr_Ptr : in Block_Ptr) return Boolean is
   begin
      pragma Assert (Hdr_Ptr /= null);
      pragma Assert (Is_Free(Hdr_Ptr));
      return Hdr_Ptr.Prev_Phys_Bk = null;
   end Is_First;
   pragma Inline (Is_First);


   --------------
   -- Set_Last --
   --------------

   procedure Set_Last (Hdr_Ptr : in Block_Ptr) is
   begin
      pragma Assert (Hdr_Ptr /= null);
      pragma Assert (Is_Free(Hdr_Ptr));
      pragma Assert (not Is_Last(Hdr_Ptr));
      Hdr_Ptr.Label := Hdr_Ptr.Label or Last_Mask;
   end Set_Last;
   pragma Inline (Set_Last);


   ----------------
   -- Reset_Last --
   ----------------

   procedure Reset_Last (Hdr_Ptr : in Block_Ptr)  is
   begin
      pragma Assert (Hdr_Ptr /= null);
      pragma Assert (Is_Free(Hdr_Ptr));
      pragma Assert (Is_Last(Hdr_Ptr));
      Hdr_Ptr.Label := Hdr_Ptr.Label and (not Last_Mask);
   end Reset_Last;
   pragma Inline (Reset_Last);


   -------------------------------
   -- Physical Block Operations --
   -------------------------------


   ---------------------
   -- Next_Phys_Block --
   ---------------------

   function Next_Phys_Block (Hdr_Ptr : in Block_Ptr) return Block_Ptr is
      Next_Addr  : Address;
      Next_Block : Block_Ptr;
   begin
      pragma Assert (Hdr_Ptr /= null);
      pragma Assert (Is_Free(Hdr_Ptr));
      pragma Assert (not Is_Last(Hdr_Ptr));
      Next_Addr  := Block_Start(Hdr_Ptr) + Get_Size(Hdr_Ptr);
      Next_Block := Block_Start(Next_Addr);
      pragma Assert (Next_Block /= null);
      return Next_Block;
   end Next_Phys_Block;
   pragma Inline (Next_Phys_Block);


   ---------------------
   -- Prev_Phys_Block --
   ---------------------

   function Prev_Phys_Block (Hdr_Ptr : in Block_Ptr) return Block_Ptr is
      Prev_Block : Block_Ptr;
   begin
      pragma Assert (Hdr_Ptr /= null);
      pragma Assert (Is_Free(Hdr_Ptr));
      pragma Assert (not Is_First(Hdr_Ptr));
      Prev_Block := Hdr_Ptr.Prev_Phys_Bk;
      pragma Assert (Prev_Block /= null);
      return Prev_Block;
   end Prev_Phys_Block;
   pragma Inline (Prev_Phys_Block);


   -----------------
   -- Merge_Block --
   -----------------

   procedure Merge_Block
     (Main_Block : in     Block_Ptr;
      Next_Block : in out Block_Ptr;
      New_Size   :    out Storage_Count)
   is
      ----------------------------------
      --  Main_Block  --  Next_Block  --
      ----------------------------------

      ----------------------------------
      --          Main_Block          --
      ----------------------------------
   begin

      pragma Assert (Main_Block /= null);
      pragma Assert (Next_Block /= null);
      pragma Assert (Is_Free(Main_Block));
      pragma Assert (Is_Free(Next_Block));
      pragma Assert (Main_Block = Next_Block.Prev_Phys_Bk);

      if not Is_Last(Next_Block) then
         -- The block followign Next_Block will point to the merged block.
         Next_Phys_Block(Next_Block).Prev_Phys_Bk := Main_Block;
      end if;

      -- Both Free, If last is final, the new block is final also.
      Main_Block.Label := Main_Block.Label + Next_Block.Label;

      -- Next_Block_Header info destroyed
      Next_Block.Label        := 0;
      Next_Block.Next_Free_Bk := null;
      Next_Block.Prev_Free_Bk := null;
      Next_Block.Prev_Phys_Bk := null;
      -- Next_Block_Header destroyed
      Next_Block              := null;

      New_Size := Get_Size(Main_Block);

      pragma Assert (Next_Block = null);
      pragma Assert (New_Size >= Min_Block_Size);

   end Merge_Block;
   pragma Inline (Merge_Block);


   -----------------
   -- Split_Block --
   -----------------

   procedure Split_Block
     (Old_Block : in     Block_Ptr;
      New_Block :    out Block_Ptr;
      New_Size  : in     Storage_Count)
   is
      --------------------------------
      --         Old_Block          --
      --------------------------------

      --------------------------------
      --  Old_Block  --  New_Block  --
      --------------------------------
   begin

      pragma Assert (Old_Block /= null);
      pragma Assert (Is_Free(Old_Block));
      pragma Assert (Is_Aligned(New_Size));
      pragma Assert (New_Size >= Min_Block_Size);
      pragma Assert ((New_Size + Min_Block_Size) <= Get_Size(Old_Block));

      New_Block := Block_Start(Block_Start(Old_Block) + New_Size);

      New_Block.Prev_Phys_Bk := Old_Block;
      New_Block.Label        := Old_Block.Label; -- Flags propagation

      Set_Size(New_Block, Get_Size(New_Block) - New_Size);
      Set_Size(Old_Block, New_Size);

      if not Is_Last(New_Block) then
         Next_Phys_Block(New_Block).Prev_Phys_Bk := New_Block;
      else
         Reset_Last(Old_Block);
      end if;

      pragma Assert (New_Block /= null);
      pragma Assert (Is_Free(New_Block));
      pragma Assert (Get_Size(Old_Block) = New_Size);
      pragma Assert (Get_Size(New_Block) >= Min_Block_Size);

   end Split_Block;
   pragma Inline (Split_Block);

end TLSF;
