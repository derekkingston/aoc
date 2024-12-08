with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Bounded_Ordered_Sets;

procedure day08 with SPARK_Mode is
   type Location is record
      Row : Integer;
      Col : Integer;
   end record;

   function Location_Less (Left, Right : Location) return Boolean is
   begin
      --  lexicographical ordering
      if Left.Row < Right.Row then
         return True;
      end if;
      if Left.Row > Right.Row then
         return False;
      end if;
      return Left.Col < Right.Col;
   end Location_Less;

   function Location_Eq (Left, Right : Location) return Boolean is
   begin
      if Left.Row = Right.Row and then Left.Col = Right.Col then
         return True;
      end if;
      return False;
   end Location_Eq;

   package Location_Sets is new Ada.Containers.Bounded_Ordered_Sets
     (Element_Type => Location,
      "<" => Location_Less,
      "=" => Location_Eq);

   type String_List is array (Positive range <>) of String (1 .. 131);
   File_Name    : constant String := "input.txt";
   File         : File_Type;
   Line         : String (1 .. 131);
   Line_Last    : Natural;
   Puzzle       : String_List (1 .. 131);
   Row_Len      : Natural := 0;
   Col_Len      : Natural := 0;
   Anti_Nodes   : Location_Sets.Set (16900);

   function Is_On_Map (L : Location) return Boolean is
   begin
      if L.Row > 0
        and then L.Row <= Row_Len
        and then L.Col > 0
        and then L.Col <= Col_Len
      then
         return True;
      end if;
      return False;
   end Is_On_Map;

   procedure Add_Anti_Nodes (R1, C1, R2, C2 : Positive; H : Boolean) is
      dR : constant Integer := R2 - R1;
      dC : constant Integer := C2 - C1;
      L : Location;
      L1 : Location;
      L2 : Location;
   begin
      --  along line back from (R1, C1)
      L1.Row := R1 - dR;
      L1.Col := C1 - dC;
      --  along line from (R2, C2)
      L2.Row := R2 + dR;
      L2.Col := C2 + dC;
      --  add locations to set if they are on the map
      if Is_On_Map (L1) then
         Anti_Nodes.Include (L1);
      end if;
      if Is_On_Map (L2) then
         Anti_Nodes.Include (L2);
      end if;
      --  if harmonics are enabled, add them
      if H then
         L.Row := R1;
         L.Col := C1;
         --  harmonics from (R1,C1) and backwards
         while Is_On_Map (L) loop
            Anti_Nodes.Include (L);
            L.Row := L.Row - dR;
            L.Col := L.Col - dC;
         end loop;
         L.Row := R2;
         L.Col := C2;
         --  harmonics from (R2,C2) and forwards
         while Is_On_Map (L) loop
            Anti_Nodes.Include (L);
            L.Row := L.Row + dR;
            L.Col := L.Col + dC;
         end loop;
      end if;
   end Add_Anti_Nodes;

   procedure Determine_Anti_Nodes (H : Boolean) is
      Ch : Character;
   begin
      for R_A in 1 .. Row_Len loop
         for C_A in 1 .. Col_Len loop
            Ch := Puzzle (R_A) (C_A);
            if Ch /= '.' then
               --  clear remainder of row
               for C_T in C_A + 1 .. Col_Len loop
                  if Puzzle (R_A) (C_T) = Ch then
                     Add_Anti_Nodes (R_A, C_A, R_A, C_T, H);
                  end if;
               end loop;
               --  search through remainder of puzzle
               for R_B in R_A + 1 .. Row_Len loop
                  for C_B in 1 .. Col_Len loop
                     if Puzzle (R_B) (C_B) = Ch then
                        Add_Anti_Nodes (R_A, C_A, R_B, C_B, H);
                     end if;
                  end loop;
               end loop;
            end if;
         end loop;
      end loop;
   end Determine_Anti_Nodes;

begin
   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) and then Row_Len < Puzzle'Length loop
      Get_Line (File, Line, Line_Last);
      if Col_Len = 0 then
         Col_Len := Line_Last;
      end if;
      if Col_Len /= Line_Last then
         Put_Line ("All rows must be of the same length");
         exit;
      end if;
      Row_Len := Row_Len + 1;
      Puzzle (Row_Len) := Line;

   end loop;

   --  Close the file
   Close (File);

   --  Part A: count unique anti nodes (no harmonics)
   Anti_Nodes.Clear;
   Determine_Anti_Nodes (False);
   Put_Line ("Part A: " & Anti_Nodes.Length'Image);

   --  Part B: count unique anti nodes (with harmonics)
   Anti_Nodes.Clear;
   Determine_Anti_Nodes (True);
   Put_Line ("Part B: " & Anti_Nodes.Length'Image);

end day08;
