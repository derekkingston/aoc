with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

procedure Day19 is
   type UInt is mod 2**64;
   package String_Vec is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Unbounded_String);
   
   function Unbounded_String_Hash (Key : Unbounded_String)
      return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (To_String (Key));
   end Unbounded_String_Hash;

   package Completed_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Unbounded_String,
      Hash => Unbounded_String_Hash,
      Equivalent_Keys => "=",
      Element_Type => UInt);

   File_Name     : constant String := "input.txt";
   File          : File_Type;
   Line          : String (1 .. 4096);
   Line_Last     : Natural;
   Comma_Idx     : Natural;
   Comma_Idx_prv : Natural;
   Towels        : String_Vec.Vector := String_Vec.Empty_Vector;
   Designs       : String_Vec.Vector := String_Vec.Empty_Vector;
   Possible      : UInt := 0;
   Memo          : Completed_Maps.Map;
   Arrange_Sum   : UInt := 0;

   function Count_Match (S : Unbounded_String) return UInt is
      Count : UInt := 0;
   begin
      --  base case: already in completed map (memoization)
      if Memo.Contains (S) then
         return Memo.Element (S);
      end if;

      --  base case: all characters removed from S, trivially matches
      if Length (S) < 1 then
         return 1;
      end if;

      --  otherwise, try to match each design
      for T of Towels loop
         --  if the candidate towel, T, could fit at the start of the design
         --  then try to continue completing the design after removing it
         if Length (T) <= Length (S) and then Slice (S, 1, Length (T)) = T then
            --  make sure the rest of the design can be satisfied recursively
            Count := Count + Count_Match (To_Unbounded_String (
               Slice (S, Length (T) + 1, Length (S))));
         end if;
      end loop;

      Memo.Include (S, Count);
      return Count;
   end Count_Match;

begin
   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) loop
      Get_Line (File, Line, Line_Last);
      Comma_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => ", ", From => 1);

      --  process the line as ccccccccc
      if Line_Last >= 1 and then Comma_Idx = 0 then
         Designs.Append (To_Unbounded_String (
            Line (1 .. Line_Last)));
      end if;

      --  process the line as cccc, cc, ..., ccc
      if Comma_Idx /= 0 then
         Comma_Idx_prv := 1;
         while Comma_Idx /= 0 loop
            Towels.Append (To_Unbounded_String (
               Line (Comma_Idx_prv .. Comma_Idx - 1)));
            Comma_Idx_prv := Comma_Idx + 2;
            Comma_Idx := Index (Source => Line (Comma_Idx + 2 .. Line_Last),
               Pattern => ", ", From => Comma_Idx + 2);
         end loop;
         Towels.Append (To_Unbounded_String (
            Line (Comma_Idx_prv .. Line_Last)));
      end if;
   end loop;

   --  Close the file
   Close (File);

   --  Part A
   for D of Designs loop
      if Count_Match (D) > 0 then
         Possible := Possible + 1;
      end if;
   end loop;
   Put_Line ("Part A: " & Possible'Image);

   --  Part B
   for D of Designs loop
      if Memo.Contains (D) then
         Arrange_Sum := Arrange_Sum + Memo.Element (D);
      end if;
   end loop;
   Put_Line ("Part B: " & Arrange_Sum'Image);

end Day19;
