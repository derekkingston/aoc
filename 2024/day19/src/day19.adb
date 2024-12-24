with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

procedure Day19 is
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
      Element_Type => Boolean);

   File_Name     : constant String := "input.txt";
   File          : File_Type;
   Line          : String (1 .. 4096);
   Line_Last     : Natural;
   Comma_Idx     : Natural;
   Comma_Idx_prv : Natural;
   Towels        : String_Vec.Vector := String_Vec.Empty_Vector;
   Designs       : String_Vec.Vector := String_Vec.Empty_Vector;
   Possible      : Natural := 0;
   Memo          : Completed_Maps.Map;

   function Check_Match (S : Unbounded_String) return Boolean is
   begin
      --  base case: already in completed map
      if Memo.Contains (S) then
         return Memo.Element (S);
      end if;
      --  base case: all characters removed from S, trivially matches
      if Length (S) < 1 then
         return True;
      end if;
      --  otherwise, try to match each design
      for T of Towels loop
         --  if the candidate towel, T, could fit at the start of the design
         --  then try to continue completing the design after removing it
         if Length (T) <= Length (S) and then Slice (S, 1, Length (T)) = T then
            --  make sure the rest of the design can be satisfied recursively
            if Check_Match (To_Unbounded_String (
               Slice (S, Length (T) + 1, Length (S))))
            then
               --  if the design is satisfied, add it to the map and return
               Memo.Include (S, True);
               return True;
            end if;
         end if;
      end loop;
      --  no design could match, mark in the map as unsatisfiable
      Memo.Include (S, False);
      return False;
   end Check_Match;

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
      if Check_Match (D) then
         Possible := Possible + 1;
      end if;
   end loop;
   Put_Line ("Feasible designs: " & Possible'Image);

end Day19;
