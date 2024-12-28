with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

procedure day21 is
   type UInt is mod 2**64;

   type Location is record
      Row : Natural;
      Col : Natural;
   end record;

   function Image (L : Location) return String is
   begin
      return "(Row =>" & L.Row'Image & ", Col =>" & L.Col'Image & ")";
   end Image;

   package String_Vec is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Unbounded_String);

   function UInt_Hash (Key : UInt) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key mod 32);
   end UInt_Hash;

   package UInt_UInt_Map is new Ada.Containers.Hashed_Maps
      (Key_Type => UInt, Element_Type => UInt,
      Hash => UInt_Hash, Equivalent_Keys => "=");
   use UInt_UInt_Map;

   --  Create 'extended' key pad as follows
   --  note: numeric pad 'A' is now 'B'
   --
   --         C1  C2  C3
   --       +---+---+---+
   --  R1   | 7 | 8 | 9 |
   --       +---+---+---+
   --  R2   | 4 | 5 | 6 |
   --       +---+---+---+
   --  R3   | 1 | 2 | 3 |
   --       +---+---+---+
   --  R4       | 0 | B |
   --           +---+---+
   --  R5       | ^ | A |
   --       +---+---+---+
   --  R6   | < | v | > |
   --       +---+---+---+

   function Pad_Coord (V : Character) return Location is
   begin
      case V is
         when '7' => return (Row => 1, Col => 1);
         when '8' => return (Row => 1, Col => 2);
         when '9' => return (Row => 1, Col => 3);
         when '4' => return (Row => 2, Col => 1);
         when '5' => return (Row => 2, Col => 2);
         when '6' => return (Row => 2, Col => 3);
         when '1' => return (Row => 3, Col => 1);
         when '2' => return (Row => 3, Col => 2);
         when '3' => return (Row => 3, Col => 3);
         when '0' => return (Row => 4, Col => 2);
         when 'B' => return (Row => 4, Col => 3);
         when '^' => return (Row => 5, Col => 2);
         when 'A' => return (Row => 5, Col => 3);
         when '<' => return (Row => 6, Col => 1);
         when 'v' => return (Row => 6, Col => 2);
         when '>' => return (Row => 6, Col => 3);
         when others => Put_Line ("Pad error");
      end case;
      return (Row => 0, Col => 0);
   end Pad_Coord;
   Blank_Num_Pad : constant Location := (Row => 4, Col => 1);
   Blank_Dir_Pad : constant Location := (Row => 5, Col => 1);

   function Valid (Loc : Location) return Boolean is
   begin
      --  over the blank on the dir part of the pad is invalid
      if Loc.Row = Blank_Dir_Pad.Row and then Loc.Col = Blank_Dir_Pad.Col then
         return False;
      end if;
      --  over the blank on the num part of the pad is invalid
      if Loc.Row = Blank_Num_Pad.Row and then Loc.Col = Blank_Num_Pad.Col then
         return False;
      end if;
      --  off the grid is invalid
      if Loc.Row = 0 or else Loc.Col = 0 or else
         Loc.Row > 6 or else Loc.Col > 3
      then
         return False;
      end if;
      --  no invalid case, so must be valid
      return True;
   end Valid;

   --  unique key for mapping from state: (Source, Destination, robot ID)
   function Memo_Key (A, B : Location; N : Positive) return UInt is
   begin
      return UInt (N     * 6 * 3 * 6 * 3 +
                   A.Row     * 3 * 6 * 3 +
                   A.Col         * 6 * 3 +
                   B.Row             * 3 +
                   B.Col);
   end Memo_Key;

   Example       : String_Vec.Vector := String_Vec.Empty_Vector;
   Input         : String_Vec.Vector := String_Vec.Empty_Vector;
   Code_Val      : UInt;
   Code_Length   : UInt;
   Code_Sum      : UInt := 0;
   Memo          : UInt_UInt_Map.Map;

   function Pattern_Length (A, B : Location; N : Positive) return UInt is
      M_Key : constant UInt := Memo_Key (A, B, N);
      L : UInt := 0;
   begin
      Put_Line ("From " & Image (A) & " to " & Image (B) & " ID: " & N'Image);
      --  memoization: if already in memo, return
      if Memo.Contains (M_Key) then
         return Memo.Element (M_Key);
      end if;
      --  base case: only one robot remaining
      if N = 1 then
         --  the number of presses is just the manhattan distance
         --  plus one to press 'A'
         L := UInt (abs (A.Row - B.Row) + abs (A.Col - B.Col)) + 1;
         Memo.Include (M_Key, L);
         return L;
      end if;
      Memo.Include (M_Key, L);
      return L;
   end Pattern_Length;

   function Solve_Code (Code : Unbounded_String; N : Positive) return UInt is
      Combo : constant String := To_String (Code);
      LocA, LocB : Location;
      Pushes : UInt := 0;
   begin
      for I in 2 .. Combo'Length loop
         LocA := Pad_Coord (Combo (I - 1));
         LocB := Pad_Coord (Combo (I));
         Pushes := Pushes + Pattern_Length (LocA, LocB, N);
      end loop;
      return Pushes;
   end Solve_Code;

begin
   --  recall: for extended pad, numeric pad 'A' is now 'B'
   --  Example.Append (To_Unbounded_String ("B029B"));
   --  Example.Append (To_Unbounded_String ("B980B"));
   --  Example.Append (To_Unbounded_String ("B179B"));
   --  Example.Append (To_Unbounded_String ("B456B"));
   --  Example.Append (To_Unbounded_String ("B379B"));
   Example.Append (To_Unbounded_String ("B379B"));

   Input.Append (To_Unbounded_String ("B805A"));
   Input.Append (To_Unbounded_String ("B170B"));
   Input.Append (To_Unbounded_String ("B129B"));
   Input.Append (To_Unbounded_String ("B283B"));
   Input.Append (To_Unbounded_String ("B540B"));

   --  Example access:
   --  LocA := Pad_Coord (Start_Character);
   --  LocB := Pad_Coord (End_Character);

   --  Part A: intermediate robots = 2
   Code_Sum := 0;
   Memo.Clear;
   for Code of Example loop
      Code_Val := UInt'Value (Slice (Code, 2, Length (Code) - 1));
      Code_Length := Solve_Code (Code, 2);
      Code_Sum := Code_Sum + Code_Val * Code_Length;
   end loop;
   Put_Line ("Part A: " & Code_Sum'Image);

   --  Part B: intermediate robots = 25
   Code_Sum := 0;
   Memo.Clear;
   for Code of Example loop
      Code_Val := UInt'Value (Slice (Code, 2, Length (Code) - 1));
      Code_Length := Solve_Code (Code, 25);
      Code_Sum := Code_Sum + Code_Val * Code_Length;
   end loop;
   Put_Line ("Part B: " & Code_Sum'Image);
   --  193369129748870 too high

end day21;
