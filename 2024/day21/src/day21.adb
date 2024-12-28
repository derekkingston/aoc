with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Hash;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

procedure day21 is
   type UInt is mod 2**64;

   type Location is record
      Row : Natural;
      Col : Natural;
   end record;

   package String_Vec is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Unbounded_String);

   function Unbounded_String_Hash (Key : Unbounded_String)
      return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (To_String (Key));
   end Unbounded_String_Hash;

   package Transition_Mapping is new Ada.Containers.Hashed_Maps
     (Key_Type => Unbounded_String,
      Hash => Unbounded_String_Hash,
      Equivalent_Keys => "=",
      Element_Type => Unbounded_String);
   use Transition_Mapping;

   package Completed_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Unbounded_String,
      Hash => Unbounded_String_Hash,
      Equivalent_Keys => "=",
      Element_Type => UInt);

   Example       : String_Vec.Vector := String_Vec.Empty_Vector;
   Input         : String_Vec.Vector := String_Vec.Empty_Vector;
   Num_To_Dir    : Transition_Mapping.Map;
   Dir_To_Dir    : Transition_Mapping.Map;
   Num_Pad_Vals  : constant String := "0123456789A";
   Dir_Pad_Vals  : constant String := "^>v<A";
   To_From       : Unbounded_String;
   Combo         : Unbounded_String;
   LocA, LocB    : Location;
   Blank_Num_Pad : constant Location := (Row => 4, Col => 1);
   Blank_Dir_Pad : constant Location := (Row => 1, Col => 1);
   Row_Diff      : Integer;
   Col_Diff      : Integer;
   Shuffle       : String_Vec.Vector := String_Vec.Empty_Vector;
   Code_Val      : UInt;
   Code_Length   : UInt;
   Code_Sum      : UInt := 0;
   Memo          : Completed_Maps.Map;

   function Num_Pad_Coord (V : Character) return Location is
   begin
      case V is
         when '0' => return (Row => 4, Col => 2);
         when '1' => return (Row => 3, Col => 1);
         when '2' => return (Row => 3, Col => 2);
         when '3' => return (Row => 3, Col => 3);
         when '4' => return (Row => 2, Col => 1);
         when '5' => return (Row => 2, Col => 2);
         when '6' => return (Row => 2, Col => 3);
         when '7' => return (Row => 1, Col => 1);
         when '8' => return (Row => 1, Col => 2);
         when '9' => return (Row => 1, Col => 3);
         when 'A' => return (Row => 4, Col => 3);
         when others => Put_Line ("Num pad error");
      end case;
      return (Row => 0, Col => 0);
   end Num_Pad_Coord;

   function Dir_Pad_Coord (V : Character) return Location is
   begin
      case V is
         when '^' => return (Row => 1, Col => 2);
         when '>' => return (Row => 2, Col => 3);
         when 'v' => return (Row => 2, Col => 2);
         when '<' => return (Row => 2, Col => 1);
         when 'A' => return (Row => 1, Col => 3);
         when others => Put_Line ("Dir pad error");
      end case;
      return (Row => 0, Col => 0);
   end Dir_Pad_Coord;

   function Add_Rows (R0 : Integer) return Unbounded_String is
      R : Integer := R0;
      S : Unbounded_String := Null_Unbounded_String;
   begin
      while R > 0 loop
         S := S & 'v';
         R := R - 1;
      end loop;
      while R < 0 loop
         S := S & '^';
         R := R + 1;
      end loop;
      return S;
   end Add_Rows;

   function Add_Cols (C0 : Integer) return Unbounded_String is
      C : Integer := C0;
      S : Unbounded_String := Null_Unbounded_String;
   begin
      while C > 0 loop
         S := S & '>';
         C := C - 1;
      end loop;
      while C < 0 loop
         S := S & '<';
         C := C + 1;
      end loop;
      return S;
   end Add_Cols;

   function Row_First (R, C : Integer) return Unbounded_String is
      S : Unbounded_String;
   begin
      S := Add_Rows (R);
      S := S & Add_Cols (C);
      return S;
   end Row_First;

   function Col_First (R, C : Integer) return Unbounded_String is
      S : Unbounded_String;
   begin
      S := Add_Cols (C);
      S := S & Add_Rows (R);
      return S;
   end Col_First;

   function Process_Combo (S0 : Unbounded_String) return Unbounded_String is
      Code : Unbounded_String := Null_Unbounded_String;
      S : constant Unbounded_String := 'A' & S0; --  always start from 'A'
   begin
      for I in 2 .. Length (S) loop
         Code := Code & Dir_To_Dir.Element (
            To_Unbounded_String (Slice (S, I - 1, I)));
      end loop;
      return Code;
   end Process_Combo;

   function Pattern_Length (Code : Unbounded_String; N : Positive)
         return UInt is
      S : constant Unbounded_String := 'A' & Code;
      L : UInt := 0;
      M_Key : constant Unbounded_String := S & To_Unbounded_String (N'Image);
   begin
      --  base case: already in completed map (memoization)
      if Memo.Contains (M_Key) then
         return Memo.Element (M_Key);
      end if;
      --  base case: only one robot remaining
      if N = 1 then
         for I in 2 .. Length (S) loop
            L := L + UInt (Length (Dir_To_Dir.Element (
               To_Unbounded_String (Slice (S, I - 1, I)))));
         end loop;
         Memo.Include (M_Key, L);
         return L;
      end if;
      --  recusion: add length of all transitions for fewer robots
      for I in 2 .. Length (S) loop
         L := L + Pattern_Length (Dir_To_Dir.Element (
            To_Unbounded_String (Slice (S, I - 1, I))), N - 1);
      end loop;
      Memo.Include (M_Key, L);
      return L;
   end Pattern_Length;

   function Solve_Code (Code : Unbounded_String; N : Positive) return UInt is
      S : Unbounded_String;
   begin
      --  map from the number pad to the direction pad
      for I in 2 .. Length (Code) loop
         S := S & Num_To_Dir.Element (
            To_Unbounded_String (Slice (Code, I - 1, I)));
      end loop;
      return Pattern_Length (S, N);
   end Solve_Code;

   --  note: generally, very large number of permutations (n!) result
   --  here, the longest possible combo is 4 moves, which has 24 permutations
   --  This procedure adds each permutation to a vector; modification of alg:
   --  https://www.geeksforgeeks.org/write-a-c-program-to-print-all-permutations-of-a-given-string/
   procedure Permutations (S : in out Unbounded_String;
      Idx : Positive; P : in out String_Vec.Vector) is
      Tmp : Character;
   begin
      --  base case: reached end of string
      if Idx = Length (S) then
         P.Append (S);
         return;
      end if;

      for I in Idx .. Length (S) loop
         --  swap character at Idx with character at I
         Tmp := Element (S, Idx);
         Replace_Element (S, Idx, Element (S, I));
         Replace_Element (S, I, Tmp);
         --  recurse
         Permutations (S, Idx + 1, P);
         --  backtrack, undo swap
         Tmp := Element (S, Idx);
         Replace_Element (S, Idx, Element (S, I));
         Replace_Element (S, I, Tmp);
      end loop;
   end Permutations;

   function Optimize_Combo (S : Unbounded_String; A, X : Location)
         return Unbounded_String is
      S_Tmp : Unbounded_String := S;
      L : UInt;
      Min_Len : UInt := UInt'Last;
      Resulting_Presses : Unbounded_String;
      Best_S : Unbounded_String := S;
      Loc_Tmp : Location;
      Valid_P : Boolean;
   begin
      Shuffle.Clear;
      Permutations (S_Tmp, 1, Shuffle);
      for P of Shuffle loop
         --  make sure this permutation is valid, e.g. doesn't enter blank
         Loc_Tmp := A;
         Valid_P := True;
         for I in 1 .. Length (P) loop
            case Element (P, I) is
               when '^' => Loc_Tmp.Row := Loc_Tmp.Row - 1;
               when '>' => Loc_Tmp.Col := Loc_Tmp.Col + 1;
               when 'v' => Loc_Tmp.Row := Loc_Tmp.Row + 1;
               when '<' => Loc_Tmp.Col := Loc_Tmp.Col - 1;
               when others => null;
            end case;
            if Loc_Tmp.Row = X.Row and then Loc_Tmp.Col = X.Col then
               Valid_P := False;
               exit;
            end if;
         end loop;
         --  check length of processed result
         if Valid_P then
            --  get length of sequence via two intermediates
            --  note: must be at least two to capture dependence
            --  on button ordering
            Resulting_Presses := P & 'A';
            L := Pattern_Length (Resulting_Presses, 25);
            if L < Min_Len then
               Min_Len := L;
               Best_S := P;
            end if;
         end if;
      end loop;
      return Best_S;
   end Optimize_Combo;

begin
   Example.Append (To_Unbounded_String ("A029A"));
   Example.Append (To_Unbounded_String ("A980A"));
   Example.Append (To_Unbounded_String ("A179A"));
   Example.Append (To_Unbounded_String ("A456A"));
   Example.Append (To_Unbounded_String ("A379A"));

   Input.Append (To_Unbounded_String ("A805A"));
   Input.Append (To_Unbounded_String ("A170A"));
   Input.Append (To_Unbounded_String ("A129A"));
   Input.Append (To_Unbounded_String ("A283A"));
   Input.Append (To_Unbounded_String ("A540A"));

   --  attempt to create mapping by just avoiding the blank
   for I in Dir_Pad_Vals'Range loop
      for J in Dir_Pad_Vals'Range loop
         To_From := To_Unbounded_String (Dir_Pad_Vals (I) & Dir_Pad_Vals (J));
         LocA := Dir_Pad_Coord (Dir_Pad_Vals (I));
         LocB := Dir_Pad_Coord (Dir_Pad_Vals (J));
         Row_Diff := LocB.Row - LocA.Row;
         Col_Diff := LocB.Col - LocA.Col;
         --  to avoid blank spot: if going down, go row first
         if Row_Diff > 0 then
            Combo := Row_First (Row_Diff, Col_Diff);
         else
            Combo := Col_First (Row_Diff, Col_Diff);
         end if;
         Combo := Combo & 'A';
         Dir_To_Dir.Insert (To_From, Combo);
      end loop;
   end loop;
   --  Put_Line ("Dir_To_Dir");
   --  for Csr in Dir_To_Dir.Iterate loop
   --     Put_Line (To_String (Key (Csr)) & ": "
   --        & To_String (Dir_To_Dir (Csr)));
   --  end loop;
   --  New_Line;

   --  FIX BY FIAT to prioritize left and down before up before right
   Dir_To_Dir.Include (To_Unbounded_String ("A<"), To_Unbounded_String ("v<<A"));
   Dir_To_Dir.Include (To_Unbounded_String ("Av"), To_Unbounded_String ("<vA"));
   Dir_To_Dir.Include (To_Unbounded_String ("^>"), To_Unbounded_String ("v>A"));
   Dir_To_Dir.Include (To_Unbounded_String (">^"), To_Unbounded_String ("<^A"));
   Dir_To_Dir.Include (To_Unbounded_String ("vA"), To_Unbounded_String ("^>A"));

   --  build shortest path mappings
   for I in Num_Pad_Vals'Range loop
      for J in Num_Pad_Vals'Range loop
         To_From := To_Unbounded_String (Num_Pad_Vals (I) & Num_Pad_Vals (J));
         LocA := Num_Pad_Coord (Num_Pad_Vals (I));
         LocB := Num_Pad_Coord (Num_Pad_Vals (J));
         Row_Diff := LocB.Row - LocA.Row;
         Col_Diff := LocB.Col - LocA.Col;
         --  to avoid blank spot: if going up, go row first
         if Row_Diff < 0 then
            Combo := Row_First (Row_Diff, Col_Diff);
         else
            Combo := Col_First (Row_Diff, Col_Diff);
         end if;
         Combo := Optimize_Combo (Combo, LocA, Blank_Num_Pad);
         Combo := Combo & 'A';
         Num_To_Dir.Insert (To_From, Combo);
      end loop;
   end loop;
   --  Put_Line ("Num_To_Dir");
   --  for Csr in Num_To_Dir.Iterate loop
   --     Put_Line (To_String (Key (Csr)) & ": "
   --        & To_String (Num_To_Dir (Csr)));
   --  end loop;
   --  New_Line;

   --  Part A: intermediate robots = 2
   Code_Sum := 0;
   for Code of Input loop
      Code_Val := UInt'Value (Slice (Code, 2, Length (Code) - 1));
      Code_Length := Solve_Code (Code, 2);
      Code_Sum := Code_Sum + Code_Val * Code_Length;
   end loop;
   Put_Line ("Part A: " & Code_Sum'Image);

   --  Part B: intermediate robots = 25
   Code_Sum := 0;
   for Code of Input loop
      Code_Val := UInt'Value (Slice (Code, 2, Length (Code) - 1));
      Code_Length := Solve_Code (Code, 25);
      Code_Sum := Code_Sum + Code_Val * Code_Length;
   end loop;
   Put_Line ("Part B: " & Code_Sum'Image);
   --  193369129748870 too high

end day21;
