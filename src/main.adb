with Ada.Text_IO; use Ada.Text_IO;

with Ada.Unchecked_Deallocation;

with TLSF.Pools;

with Ada.Execution_Time;
with Ada.Real_Time;


procedure Main is

   type Parameterless_Procedure is access procedure;

   procedure Bench (Test : in Parameterless_Procedure) is

      use Ada.Execution_Time;
      use Ada.Real_Time;

      CPU_Start : CPU_Time;
      CPU_End   : CPU_Time;

   begin

      CPU_Start := Clock;
      Test.all;
      CPU_End   := Clock;

      Put_Line( "CPU use (s) :" & To_Duration(CPU_End - CPU_Start)'Img);

   end Bench;

   S : constant := 2**17;-- - 2**10;

   TLSF_Pool : TLSF.Pools.Storage_Pool(S);

   type Integer_Arr is array (Integer range <>) of Integer;

   type Integer_Ptr     is access all Integer;
   type Integer_Arr_Ptr is access all Integer_Arr;

   for Integer_Ptr'Storage_Pool     use TLSF_Pool;
   for Integer_Arr_Ptr'Storage_Pool use TLSF_Pool;

   procedure Free_Integer_Ptr is new
     Ada.Unchecked_Deallocation(Object => Integer, Name => Integer_Ptr);
   procedure Free_Integer_Arr_Ptr is new
     Ada.Unchecked_Deallocation(Object => Integer_Arr, Name => Integer_Arr_Ptr);

   --pragma Inline_Always(Free_Integer_Ptr);
   --pragma Inline_Always(Free_Integer_Arr_Ptr);

   procedure Test_Fun is
      Int_1, Int_2, Int_3, Int_4 : Integer_Ptr;
      Arr_1, Arr_2, Arr_3, Arr_4 : Integer_Arr_Ptr;


   begin

      for I in 1 .. 1000 loop

         Int_1 := new Integer; --Put_Line(TLSF_Pool.Storage_Size'Img);
         Int_2 := new Integer; --Put_Line(TLSF_Pool.Storage_Size'Img);
         Int_3 := new Integer; --Put_Line(TLSF_Pool.Storage_Size'Img);
         Int_4 := new Integer; --Put_Line(TLSF_Pool.Storage_Size'Img);

         Arr_1 := new Integer_Arr(1 .. 1000); --Put_Line(TLSF_Pool.Storage_Size'Img);
         Arr_2 := new Integer_Arr(1 .. 1001); --Put_Line(TLSF_Pool.Storage_Size'Img);
         Arr_3 := new Integer_Arr(1 .. 1002); --Put_Line(TLSF_Pool.Storage_Size'Img);
         Arr_4 := new Integer_Arr(1 .. 1003); --Put_Line(TLSF_Pool.Storage_Size'Img);

         Free_Integer_Ptr(Int_1);
         Free_Integer_Ptr(Int_3);
         Free_Integer_Ptr(Int_2);
         Free_Integer_Ptr(Int_4);


         Free_Integer_Arr_Ptr(Arr_1);
         Free_Integer_Arr_Ptr(Arr_2);
         Free_Integer_Arr_Ptr(Arr_3);
         Free_Integer_Arr_Ptr(Arr_4);

      end loop;

       while Natural(TLSF_Pool.Storage_Size) > 0 loop
         Int_1 := new Integer;
         Put_Line(TLSF_Pool.Storage_Size'Img);
       end loop;


   end;

begin

   Bench(Test_Fun'Access);
   TLSF_Pool.Finalize;

end Main;
