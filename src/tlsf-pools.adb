package body TLSF.Pools is

   --------------------
   -- Mapping_Insert --
   --------------------

   -- Given "Block_Size", returns the L1 and L2 values where the
   -- block should be inserted in the free node list matrix.
   procedure Mapping_Insert
     (Block_Size : in     Storage_Count;
      Level_One  :    out L1_Range;
      Level_Two  :    out L2_Range)
   is
      use type Bitwise_Ops.Position;
      L1  : L1_Range renames Level_One;
      L2  : L2_Range renames Level_Two;
      M64 : constant Unsigned_Sys := Unsigned_Sys(Block_Size);
   begin

      if Block_Size >= Max_Block_Size then
         -- The block's size is larger than normal,
         -- therefore it is appended at the end.
         L1 := L1_Range'Last;
         L2 := L2_Range'Last;
      elsif Block_Size >= Min_Block_Size then
         L1 := Bitwise_Ops.BSR(M64);
         L2 := L2_Range(Bitwise_Ops.SHR(M64, L1 - L2_Log2) and L2_Mask);
      else
         -- The block's size is too small to be inserted into the list.
         -- Its respectives L1 and L2 values are out of range.
         raise Program_Error;
      end if;

   end Mapping_Insert;
   pragma Inline_Always (Mapping_Insert);


   --------------------
   -- Mapping_Search --
   --------------------

   -- Given "Block_Size", returns the least L1 and L2 possible values
   -- where to find a free block in the free block list matrix.
   -- If the requested block size is smaller than the minimum block size,
   -- it also updates the requested value to the minumum block size.
   procedure Mapping_Search
     (Block_Size : in out Storage_Count;
      Level_One  :    out L1_Range;
      Level_Two  :    out L2_Range)
   is
      use type Bitwise_Ops.Position;
      L1   : L1_Range renames Level_One;
      L2   : L2_Range renames Level_Two;
      M64  : Unsigned_Sys := Unsigned_Sys(Block_Size);
   begin

      if Block_Size <= Min_Block_Size then
         L1 := L1_Range'First;
         L2 := L2_Range'First;
         Block_Size := Min_Block_Size;
      elsif  Block_Size <= Max_Block_Size  then
         M64 := M64 + Bitwise_Ops.SHL(1, Bitwise_Ops.BSR(M64) - L2_Log2) - 1;
         L1  := Bitwise_Ops.BSR(M64);
         L2  := L2_Range(Bitwise_Ops.SHR (M64, L1 - L2_Log2) and L2_Mask);
      else
         raise Program_Error;
      end if;

   end Mapping_Search;
   pragma Inline_Always (Mapping_Search);


   ---------------------------
   -- Search_Suitable_Block --
   ---------------------------

   -- Given L1 and L2 values, returns the smallest free block found
   -- in the free node list and its associated new L1 and L2 values.
   procedure Search_Suitable_Block
     (This      : in     Storage_Pool;
      The_Block :    out Block_Ptr;
      Level_One : in out L1_Range;
      Level_Two : in out L2_Range)
   is
      use type Bitwise_Ops.Position;
      L1  : L1_Range    renames Level_One;
      L2  : L2_Range    renames Level_Two;
      MAT : L1L2_Matrix renames This.Free_Matrix;
      L1B : Unsigned_Sys renames This.L1_Bitmap;
      L2B : L1_Array    renames This.L2_Bitmap;

      BMP : Unsigned_Sys;
   begin

      BMP := L2B(L1) and Bitwise_Ops.SHL(-1, L2);

      if (BMP /= 0) then
         L2 := Bitwise_Ops.BSF(BMP);
      else
         BMP := L1B and Bitwise_Ops.SHL(-1, L1 + 1);
         if (BMP /= 0) then
            L1 := Bitwise_Ops.BSF(BMP);
            L2 := Bitwise_Ops.BSF(L2B(L1));
         else
            raise Storage_Error;
         end if;
      end if;

      The_Block := MAT(L1, L2);

      pragma Assert (The_Block /= null, "Search_Suitable_Block Error : Null block found.");

      L1 := The_Block.Level_One;
      L2 := The_Block.Level_Two;

      pragma Assert (Level_One'Valid);
      pragma Assert (Level_Two'Valid);
      pragma Assert (The_Block /= null);

   end Search_Suitable_Block;
   pragma Inline_Always (Search_Suitable_Block);


   ------------------
   -- Insert_Block --
   ------------------

   procedure Insert_Block
     (This      : in out Storage_Pool;
      The_Block : in     Block_Ptr)
   is
      L1  : L1_Range    renames The_Block.Level_One;
      L2  : L2_Range    renames The_Block.Level_Two;
      L1B : Unsigned_Sys renames This.L1_Bitmap;
      L2B : L1_Array    renames This.L2_Bitmap;
      MAT : L1L2_Matrix renames This.Free_Matrix;

      List_Head  : constant Block_Ptr := MAT(L1, L2);
   begin

      pragma Assert (The_Block /= null, "Insert_Block Error : Null block provided.");
      pragma Assert (Is_Free(The_Block), "Insert_Block Error : Non free block provided.");

      if List_Head /= null then
         -- Non empty list.
         List_Head.Prev_Free_Bk := The_Block;
      else
         -- Empty list.
         Bitwise_Ops.BTS(L1B, L1);
         Bitwise_Ops.BTS(L2B(L1), L2);
      end if;

      The_Block.Prev_Free_Bk := null;
      The_Block.Next_Free_Bk := List_Head;

      MAT(L1, L2) := The_Block;

   end Insert_Block;
   pragma Inline_Always (Insert_Block);


   ------------------
   -- Remove_Block --
   ------------------

   procedure Remove_Block
     (This      : in out Storage_Pool;
      The_Block : in     Block_Ptr)
   is
      L1  : L1_Range    renames The_Block.Level_One;
      L2  : L2_Range    renames The_Block.Level_Two;
      L1B : Unsigned_Sys renames This.L1_Bitmap;
      L2B : L1_Array    renames This.L2_Bitmap;
      MAT : L1L2_Matrix renames This.Free_Matrix;

      Prev_Block : constant Block_Ptr := The_Block.Prev_Free_Bk;
      Next_Block : constant Block_Ptr := The_Block.Next_Free_Bk;

   begin

      pragma Assert (The_Block /= null, "Remove_Block Error : Null block provided.");
      pragma Assert (Is_Free(The_Block), "Remove_Block Error : Non free block provided.");

      -- The Block position in the Free Block List.
      case (The_Block.Prev_Free_Bk = null) is
         when True =>
            case (The_Block.Next_Free_Bk = null) is
            when True =>
               -- The Block is alone.
               MAT(L1, L2) := null;
               Bitwise_Ops.BTR(L1B,  L1);
               Bitwise_Ops.BTR(L2B(L1), L2);
            when False =>
               -- The Block is the the first node.
               MAT(L1, L2) := Next_Block;
               Next_Block.Prev_Free_Bk := null;
            end case;
         when False =>
            case (The_Block.Next_Free_Bk = null) is
            when True =>
               -- The Block is the the last node.
               Prev_Block.Next_Free_Bk := null;
            when False =>
               -- The Block is in bewteen.
               Prev_Block.Next_Free_Bk := Next_Block;
               Next_Block.Prev_Free_Bk := Prev_Block;
            end case;
      end case;

      The_Block.Prev_Free_Bk := null;
      The_Block.Next_Free_Bk := null;

   end Remove_Block;
   pragma Inline(Remove_Block);


   -------------------------------------------------
   -------------------------------------------------
   ---- Pool as Root_Storage_Pool  procedures : ----
   -------------------------------------------------
   -------------------------------------------------


   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (This                     : in out Storage_Pool;
      Storage_Address          :    out Address;
      Size_In_Storage_Elements : in     Storage_Count;
      Alignment                : in     Storage_Count)
   is

      Free_Block : Block_Ptr;
      New_Block  : Block_Ptr;
      Req_Size   : Storage_Count; -- Requested size
      Deq_Size   : Storage_Count; -- Dequeued block size
      L1, L1_New : L1_Range;
      L2, L2_New : L2_Range;
   begin

      pragma Assert (Size_In_Storage_Elements <= This.Storage_Size);
      pragma Assert (Is_Aligned(Alignment));

      Req_Size := Align(Size_In_Storage_Elements) + Used_Header_Size;

      Mapping_Search
        (Block_Size => Req_Size,
         Level_One  => L1,
         Level_Two  => L2);

      Search_Suitable_Block
        (This      => This,
         The_Block => Free_Block,
         Level_One => L1,
         Level_Two => L2);

      Remove_Block
        (This      => This,
         The_Block => Free_Block);

      Deq_Size := Get_Size(Free_Block);

      if (Deq_Size - Req_Size) >= Min_Block_Size then
         -- Dequeued size is larger than necessary and can be splitted.
         -- therefore it is splitted and the new resulting
         -- block is inserted into the free node list matrix.

         Split_Block
           (Old_Block => Free_Block,
            New_Block => New_Block,
            New_Size  => Req_Size);

         Mapping_Insert
           (Block_Size => Get_Size(New_Block),
            Level_One  => L1_New,
            Level_Two  => L2_New);

         New_Block.Level_One := L1_New;
         New_Block.Level_Two := L2_New;

         Insert_Block
           (This      => This,
            The_Block => New_Block);

      end if;


      This.Memory_Free := This.Memory_Free - Get_Size(Free_Block);
      Storage_Address  := Data_Start(Free_Block);

      pragma Assert (Is_Free(Free_Block));
      Set_Used(Free_Block);

   end Allocate;


   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (This                     : in out Storage_Pool;
      Storage_Address          : in     Address;
      Size_In_Storage_Elements : in     Storage_Count;
      Alignment                : in     Storage_Count)
   is
      Free_Block : Block_Ptr := Data_Start(Storage_Address);
      Next_Block : Block_Ptr;
      Prev_Block : Block_Ptr;
      New_Size   : Storage_Count;
      L1         : L1_Range;
      L2         : L2_Range;
   begin

      pragma Assert (Free_Block /= null);
      pragma Assert (Is_Used(Free_Block));
      pragma Assert (Is_Aligned(Alignment));

      Set_Free(Free_Block);

      pragma Assert (Size_In_Storage_Elements <= Get_Size(Free_Block));

      This.Memory_Free := This.Memory_Free + Get_Size(Free_Block);

      -- Block coalescences section ---

      if not Is_Last(Free_Block) then
         -- Block coalescence to the right.

         Next_Block := Next_Phys_Block(Free_Block);
         if Is_Free(Next_Block) then

            Remove_Block
              (This      => This,
               The_Block => Next_Block);

            Merge_Block
              (Main_Block => Free_Block,
               Next_Block => Next_Block,
               New_Size   => New_Size);

         end if;
      end if;

      if not Is_First(Free_Block) then
         -- Block coalescence to the left.

         Prev_Block := Prev_Phys_Block(Free_Block);
         if Is_Free (Prev_Block) then

            Remove_Block
              (This      => This,
               The_Block => Prev_Block);

            Merge_Block
              (Main_Block => Prev_Block,
               Next_Block => Free_Block,
               New_Size   => New_Size);

            Free_Block := Prev_Block; -- Block shift.
         end if;
      end if;

      -- End of block coalescences section ---

      New_Size := Get_Size(Free_Block);

      Mapping_Insert
        (Block_Size => New_Size,
         Level_One  => L1,
         Level_Two  => L2);

      Free_Block.Level_One := L1;
      Free_Block.Level_Two := L2;

      Insert_Block
        (This      => This,
         The_Block => Free_Block);

   end Deallocate;


   ------------------
   -- Storage_Size --
   ------------------

   function Storage_Size (This : in Storage_Pool) return Storage_Count is
      Biggest_L1 : L1_Range;
      Biggest_L2 : L2_Range;
      Biggest_BL : Block_Ptr;
      Biggest_S  : Storage_Count := 0;
   begin

      if (This.L1_Bitmap > 0) then
         Biggest_L1 := Bitwise_Ops.BSR(This.L1_Bitmap);
         Biggest_L2 := Bitwise_Ops.BSR(This.L2_Bitmap(Biggest_L1));
         Biggest_BL := This.Free_Matrix(Biggest_L1, Biggest_L2);
         Biggest_S  := Get_Size(Biggest_BL);
      end if;

      return Biggest_S;

   end Storage_Size;


   -----------------
   -- Initialize --
   -----------------

   overriding procedure Initialize (This : in out Storage_Pool) is
      Mem_Space : Storage_Array renames This.Memory_Space;
      Mem_Size  : Storage_Count renames This.Size;

      Gen_Addrs : constant Address   := Mem_Space'Address;
      Gen_Block : constant Block_Ptr := Block_Start(Gen_Addrs);

      L1 : L1_Range;
      L2 : L2_Range;
   begin

      Gen_Block.Label := 1;
      Set_Size(Gen_Block, Mem_Size);

      Gen_Block.Next_Free_Bk := null;
      Gen_Block.Prev_Free_Bk := null;
      Gen_Block.Prev_Phys_Bk := null;

      Mapping_Insert
        (Block_Size => Mem_Size,
         Level_One  => L1,
         Level_Two  => L2);

      Gen_Block.Level_One := L1;
      Gen_Block.Level_Two := L2;

      Insert_Block
        (This      => This,
         The_Block => Gen_Block);

   end Initialize;


   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Storage_Pool) is
   begin
      -- Memory erasing for safety purposes.
      This.Memory_Space := (others => 0);
   end Finalize;

end TLSF.Pools;
