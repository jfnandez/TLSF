with System.Storage_Pools;
use  System.Storage_Pools;

package TLSF.Pools is

   pragma Preelaborate;

   type Storage_Pool (Size : Storage_Count)
     is new Root_Storage_Pool with private;

   overriding procedure Allocate
     (This                     : in out Storage_Pool;
      Storage_Address          :    out Address;
      Size_In_Storage_Elements : in     Storage_Count;
      Alignment                : in     Storage_Count);

   overriding procedure Deallocate
     (This                     : in out Storage_Pool;
      Storage_Address          : in     Address;
      Size_In_Storage_Elements : in     Storage_Count;
      Alignment                : in     Storage_Count);

   overriding function Storage_Size
     (This : in Storage_Pool) return Storage_Count;

private

   overriding procedure Initialize
     (This : in out Storage_Pool);

   overriding procedure Finalize
     (This : in out Storage_Pool);

   type L1_Array    is array (L1_Range) of Unsigned_Sys;
   type L1L2_Matrix is array (L1_Range, L2_Range) of Block_Ptr;

   type Storage_Pool (Size : Storage_Count) is new Root_Storage_Pool with
      record
         L1_Bitmap    : Unsigned_Sys   := 0;
         L2_Bitmap    : L1_Array      := (others => 0);
         Free_Matrix  : L1L2_Matrix   := (others => (others => null));
         Memory_Free  : Storage_Count := Size;
         Memory_Space : Storage_Array (1 .. Size);
      end record;

end TLSF.Pools;
