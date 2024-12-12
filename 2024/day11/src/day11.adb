with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;

procedure day11 is
   type UInt is mod 2**64;

   function UInt_Hash (Key : UInt) return Ada.Containers.Hash_Type is
   begin
      --  Ada.Containers.Hash_Type is defined as mod 2**32 but keys are
      --  mod 2**64, therefore making a string and hashing that instead
      return Ada.Strings.Hash (UInt'Image (Key));
   end UInt_Hash;

   package UInt_UInt_Map is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type => UInt, Element_Type => UInt,
      Hash => UInt_Hash, Equivalent_Keys => "=");
   use UInt_UInt_Map;

   Example     : UInt_UInt_Map.Map;
   Input       : UInt_UInt_Map.Map;
   Blinks      : constant Natural := 75;
   Puzzle      : UInt_UInt_Map.Map;
   Next_Puzzle : UInt_UInt_Map.Map;
   Value       : UInt;
   TopV        : UInt;
   RemV        : UInt;
   NDigits     : Natural;
   Num_Stones  : UInt := 0;

   function Count_Digits (V0 : UInt) return Natural is
      Num_Digits : Natural := 0;
      V : UInt := V0;
   begin
      if V = 0 then
         return 1;
      end if;
      while V > 0 loop
         Num_Digits := Num_Digits + 1;
         V := V / 10;
      end loop;
      return Num_Digits;
   end Count_Digits;

   procedure Update_Next (K, V : UInt) is
   begin
      if Next_Puzzle.Contains (K) then
         Next_Puzzle.Include (K, Next_Puzzle.Element (K) + V);
      else
         Next_Puzzle.Insert (K, V);
      end if;
   end Update_Next;

   procedure Process_Blink (C : UInt_UInt_Map.Cursor) is
   begin
      Value := Key (C);
      if Value = 0 then
         Update_Next (1, Element (C));
      else
         NDigits := Count_Digits (Value);
         if NDigits mod 2 = 0 then
            TopV := Value / (10 ** (NDigits / 2));
            RemV := Value mod (10 ** (NDigits / 2));
            Update_Next (TopV, Element (C));
            Update_Next (RemV, Element (C));
         else
            Update_Next (Value * 2024, Element (C));
         end if;
      end if;
   end Process_Blink;

   procedure Count_Stones (C : UInt_UInt_Map.Cursor) is
   begin
      Num_Stones := Num_Stones + Element (C);
   end Count_Stones;

begin
   Example.Insert (125, 1);
   Example.Insert (17, 1);

   Input.Insert (1117, 1);
   Input.Insert (0, 1);
   Input.Insert (8, 1);
   Input.Insert (21078, 1);
   Input.Insert (2389032, 1);
   Input.Insert (142881, 1);
   Input.Insert (93, 1);
   Input.Insert (385, 1);

   Puzzle := Input;

   for I in 1 .. Blinks loop
      Next_Puzzle.Clear;
      Puzzle.Iterate (Process_Blink'Access);
      Puzzle := Next_Puzzle;
   end loop;

   Puzzle.Iterate (Count_Stones'Access);
   Put_Line ("# Stones: " & UInt'Image (Num_Stones));
end day11;
