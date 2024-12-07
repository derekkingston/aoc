with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Generic_Array_Sort;

procedure day01 with SPARK_Mode is
   type Integer_Array is array (Natural range <>) of Integer;

   File_Name  : constant String := "input.txt";
   File       : File_Type;
   Line       : String (1 .. 256);
   Line_Last  : Natural;
   Space      : Natural;
   Num_Last   : Natural;
   Left_List  : Integer_Array (1 .. 1000);
   Right_List : Integer_Array (1 .. 1000);
   List_Len   : Natural := 0;
   List_Dist  : Integer := 0;
   List_Sim   : Integer := 0;

   procedure Sort is new Ada.Containers.Generic_Array_Sort
      (Index_Type  => Natural,
      Element_Type => Integer,
      Array_Type   => Integer_Array);

   function Count_Array (A: in Integer_Array; V: in Integer) return Natural is
      C : Natural := 0;
   begin
      for I in A'Range loop
         if A (I) = V then
            C := C + 1;
         end if;
      end loop;
      return C;
   end Count_Array;
begin
   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) and then List_Len < Left_List'Length loop
      Get_Line (File, Line, Line_Last);

      --  Split the line by space
      Space := Index (Source => Line (1 .. Line_Last), Pattern => " ",
         From => 1);
      exit when Space = 0;

      --  Assume thet either side of the space is a number for each list
      List_Len := List_Len + 1;
      Get (Line (1 .. Space - 1), Left_List (List_Len), Num_Last);
      Get (Line (Space + 1 .. Line_Last), Right_List (List_Len), Num_Last);

   end loop;

   --  Close the file
   Close (File);

   --  Part B: Compute similarity
   for I in 1 .. List_Len loop
      List_Sim := List_Sim + Count_Array (
         Right_List (1 .. List_Len), Left_List (I)) * Left_List (I);
   end loop;

   --  Part A: Sort both lists
   Sort (Left_List (1 .. List_Len));
   Sort (Right_List (1 .. List_Len));

   --  Part A: Compute distance between left and right lists
   for I in 1 .. List_Len loop
      List_Dist := List_Dist + ABS (Left_List (I) - Right_List (I));
   end loop;

   --  print the results
   Put_Line ("Part A: " & Integer'Image (List_Dist));
   Put_Line ("Part B: " & Integer'Image (List_Sim));

end day01;
