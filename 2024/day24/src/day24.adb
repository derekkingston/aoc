with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
use Ada.Containers;

procedure Day24 is
   type UInt is mod 2**64;
   type Wire_State is (ON, OFF, UNK);
   type Gate_Type is (AND_GATE, OR_GATE, XOR_GATE);

   package Wire_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Unbounded_String, "<" => "<", "=" => "=",
      Element_Type => Wire_State);
   use Wire_Maps;

   type Gate is record
      In_Wire_A : Unbounded_String;
      In_Wire_B : Unbounded_String;
      Out_Wire  : Unbounded_String;
      G_Type    : Gate_Type;
   end record;

   function Gate_Less (Left, Right : Gate) return Boolean is
   begin
      --  lexicographical ordering
      if Left.In_Wire_A < Right.In_Wire_A then
         return True;
      end if;
      if Left.In_Wire_A > Right.In_Wire_A then
         return False;
      end if;
      if Left.In_Wire_B < Right.In_Wire_B then
         return True;
      end if;
      if Left.In_Wire_B > Right.In_Wire_B then
         return False;
      end if;
      if Left.Out_Wire < Right.Out_Wire then
         return True;
      end if;
      if Left.Out_Wire > Right.Out_Wire then
         return False;
      end if;
      return Gate_Type'Pos (Left.G_Type) < Gate_Type'Pos (Right.G_Type);
   end Gate_Less;

   package Gate_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Gate, "<" => Gate_Less, "=" => "=");
   use Gate_Sets;

   package Natural_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Natural, "<" => "<", "=" => "=");
   use Natural_Sets;

   File_Name     : constant String := "input.txt";
   File          : File_Type;
   Line          : String (1 .. 256);
   Line_Last     : Natural;
   Colon_Idx     : Natural;
   Arrow_Idx     : Natural;
   And_Idx       : Natural;
   Or_Idx        : Natural;
   Xor_Idx       : Natural;
   Wire_Tmp      : Unbounded_String;
   Gate_Tmp      : Gate;
   Wires         : Wire_Maps.Map;
   Gates         : Gate_Sets.Set;
   Final_Val     : UInt := 0;
   Save_Val      : UInt;
   V             : UInt;
   Error_Bits    : Natural_Sets.Set;
   OG_Gates      : Gate_Sets.Set;

   function All_Z_Wires_Set return Boolean is
   begin
      for Csr in Wires.Iterate loop
         if To_String (Key (Csr)) (1) = 'z' and then Element (Csr) = UNK then
            return False;
         end if;
      end loop;
      return True;
   end All_Z_Wires_Set;

   procedure Simulate_Sum (X0, Y0 : UInt) is
      X : UInt := X0;
      Y : UInt := Y0;
      Bit_Idx : Natural;
      Wire_Str : Unbounded_String;
   begin
      --  reset wires
      Final_Val := 0;
      for Csr in Wires.Iterate loop
         if To_String (Key (Csr)) (1) = 'x' then
            Wires.Replace_Element (Csr, OFF);
         elsif To_String (Key (Csr)) (1) = 'y' then
            Wires.Replace_Element (Csr, OFF);
         else
            Wires.Replace_Element (Csr, UNK);
         end if;
      end loop;
      --  set the wires for X
      Bit_Idx := 0;
      while X > 0 loop
         Wire_Str := To_Unbounded_String (Ada.Strings.Fixed.Trim
            (Natural'Image (Bit_Idx), Ada.Strings.Left));
         if Bit_Idx < 10 then
            Wire_Str := To_Unbounded_String ("x0" & To_String (Wire_Str));
         else
            Wire_Str := To_Unbounded_String ("x" & To_String (Wire_Str));
         end if;
         if X mod 2 = 1 then
            Wires.Include (Wire_Str, ON);
         end if;
         X := X / 2;
         Bit_Idx := Bit_Idx + 1;
      end loop;
      --  set the wires for Y
      Bit_Idx := 0;
      while Y > 0 loop
         Wire_Str := To_Unbounded_String (Ada.Strings.Fixed.Trim
            (Natural'Image (Bit_Idx), Ada.Strings.Left));
         if Bit_Idx < 10 then
            Wire_Str := To_Unbounded_String ("y0" & To_String (Wire_Str));
         else
            Wire_Str := To_Unbounded_String ("y" & To_String (Wire_Str));
         end if;
         if Y mod 2 = 1 then
            Wires.Include (Wire_Str, ON);
         end if;
         Y := Y / 2;
         Bit_Idx := Bit_Idx + 1;
      end loop;

      --  --  ----- DEBUG -----
      --  for Csr in reverse Wires.Iterate loop
      --     if To_String (Key (Csr)) (1) = 'x' or else
      --        To_String (Key (Csr)) (1) = 'y'
      --     then
      --        Put_Line (To_String (Key (Csr)) & ": " & Element (Csr)'Image);
      --     end if;
      --  end loop;
      --  --  ----- DEBUG -----

      --  simulate the full circuit
      while not All_Z_Wires_Set loop
         for G of Gates loop
            declare
               A : constant Wire_State := Wires.Element (G.In_Wire_A);
               B : constant Wire_State := Wires.Element (G.In_Wire_B);
               O : constant Wire_State := Wires.Element (G.Out_Wire);
            begin
               if O = UNK and then A /= UNK and then B /= UNK then
                  case G.G_Type is
                     when AND_GATE =>
                        if A = ON and then B = ON then
                           Wires.Include (G.Out_Wire, ON);
                        else
                           Wires.Include (G.Out_Wire, OFF);
                        end if;
                     when OR_GATE =>
                        if A = ON or else B = ON then
                           Wires.Include (G.Out_Wire, ON);
                        else
                           Wires.Include (G.Out_Wire, OFF);
                        end if;
                     when XOR_GATE =>
                        if A /= B then
                           Wires.Include (G.Out_Wire, ON);
                        else
                           Wires.Include (G.Out_Wire, OFF);
                        end if;
                  end case;
               end if;
            end;
         end loop;
      end loop;
      --  convert the wire states back to a number
      for Csr in reverse Wires.Iterate loop
         if To_String (Key (Csr)) (1) = 'z' then
            Final_Val := Final_Val * 2;
            if Element (Csr) = ON then
               Final_Val := Final_Val + 1;
            end if;
         end if;
      end loop;
   end Simulate_Sum;

   function Roll_Over_Test return Natural_Sets.Set is
      X, Y, V : UInt;
      Wrong_Bits : Natural_Sets.Set;
   begin
      for I in 0 .. 45 loop
         X := 2**I - 1;
         Y := 1;
         Simulate_Sum (X, Y);
         V := Final_Val;
         for J in 0 .. I - 1 loop
            if V mod 2 = 1 then
               --  Put_Line (I'Image & ": " & J'Image);
               Wrong_Bits.Include (J);
            end if;
            V := V / 2;
         end loop;
      end loop;
      return Wrong_Bits;
   end Roll_Over_Test;

begin
   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) loop
      Get_Line (File, Line, Line_Last);
      Colon_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => ": ", From => 1);
      Arrow_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => " -> ", From => 1);
      And_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => " AND ", From => 1);
      Or_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => " OR ", From => 1);
      Xor_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => " XOR ", From => 1);

      --  read initial wire states
      if Colon_Idx /= 0 then
         Wire_Tmp := To_Unbounded_String (Line (1 .. Colon_Idx - 1));
         if "1" = Line (Colon_Idx + 2 .. Line_Last) then
            Wires.Include (Wire_Tmp, ON);
         else
            Wires.Include (Wire_Tmp, OFF);
         end if;
      end if;

      --  read gates
      if Arrow_Idx /= 0 then
         if And_Idx /= 0 then
            Gate_Tmp.G_Type := AND_GATE;
            Gate_Tmp.In_Wire_A := To_Unbounded_String
               (Line (1 .. And_Idx - 1));
            Gate_Tmp.In_Wire_B := To_Unbounded_String
               (Line (And_Idx + 5 .. Arrow_Idx - 1));
            Gate_Tmp.Out_Wire := To_Unbounded_String
               (Line (Arrow_Idx + 4 .. Line_Last));
         elsif Or_Idx /= 0 then
            Gate_Tmp.G_Type := OR_GATE;
            Gate_Tmp.In_Wire_A := To_Unbounded_String
               (Line (1 .. Or_Idx - 1));
            Gate_Tmp.In_Wire_B := To_Unbounded_String
               (Line (Or_Idx + 4 .. Arrow_Idx - 1));
            Gate_Tmp.Out_Wire := To_Unbounded_String
               (Line (Arrow_Idx + 4 .. Line_Last));
         elsif Xor_Idx /= 0 then
            Gate_Tmp.G_Type := XOR_GATE;
            Gate_Tmp.In_Wire_A := To_Unbounded_String
               (Line (1 .. Xor_Idx - 1));
            Gate_Tmp.In_Wire_B := To_Unbounded_String
               (Line (Xor_Idx + 5 .. Arrow_Idx - 1));
            Gate_Tmp.Out_Wire := To_Unbounded_String
               (Line (Arrow_Idx + 4 .. Line_Last));
         else
            Put_Line ("Could not parse: " & Line (1 .. Line_Last));
         end if;
         if not Wires.Contains (Gate_Tmp.In_Wire_A) then
            Wires.Include (Gate_Tmp.In_Wire_A, UNK);
         end if;
         if not Wires.Contains (Gate_Tmp.In_Wire_B) then
            Wires.Include (Gate_Tmp.In_Wire_B, UNK);
         end if;
         if not Wires.Contains (Gate_Tmp.Out_Wire) then
            Wires.Include (Gate_Tmp.Out_Wire, UNK);
         end if;
         Gates.Include (Gate_Tmp);
      end if;
   end loop;

   --  Close the file
   Close (File);

   --  Part A: simulate the circuit
   while not All_Z_Wires_Set loop
      for G of Gates loop
         declare
            A : constant Wire_State := Wires.Element (G.In_Wire_A);
            B : constant Wire_State := Wires.Element (G.In_Wire_B);
            O : constant Wire_State := Wires.Element (G.Out_Wire);
         begin
            if O = UNK and then A /= UNK and then B /= UNK then
               case G.G_Type is
                  when AND_GATE =>
                     if A = ON and then B = ON then
                        Wires.Include (G.Out_Wire, ON);
                     else
                        Wires.Include (G.Out_Wire, OFF);
                     end if;
                  when OR_GATE =>
                     if A = ON or else B = ON then
                        Wires.Include (G.Out_Wire, ON);
                     else
                        Wires.Include (G.Out_Wire, OFF);
                     end if;
                  when XOR_GATE =>
                     if A /= B then
                        Wires.Include (G.Out_Wire, ON);
                     else
                        Wires.Include (G.Out_Wire, OFF);
                     end if;
               end case;
            end if;
         end;
      end loop;
   end loop;

   Put_Line ("Part A: ");
   for Csr in reverse Wires.Iterate loop
      if To_String (Key (Csr)) (1) = 'z' then
         Put ((if Element (Csr) = ON then "1" else "0"));
         Final_Val := Final_Val * 2;
         if Element (Csr) = ON then
            Final_Val := Final_Val + 1;
         end if;
      end if;
   end loop;
   New_Line;
   Put_Line (Final_Val'Image);

   --  Part B: find proper gate swap
   --  roll over test
   Error_Bits := Roll_Over_Test;
   Put ("Roll over test showed errors with: ");
   for EB of Error_Bits loop
      Put (EB'Image);
   end loop;
   New_Line;
   --  appears to have errors on bits 20, 21, 22, 23, 31

   --  try pair swaps to fix
   OG_Gates := Gates;
   for G1 in OG_Gates.Iterate loop
      declare
         Gate_1, Gate_2 : Gate;
         Tmp_Str : Unbounded_String;
         G2: Gate_Sets.Cursor := Next (G1);
      begin
         Gate_1 := Element (G1);
         while Has_Element (G2) loop
            Put_Line ("swapping ...");
            --  UGHHHH naive swapping can cause infinite loop in circuit sim
            Gate_2 := Element (G2);
            Gates.Exclude (Gate_1);
            Gates.Exclude (Gate_2);
            Tmp_Str := Gate_1.Out_Wire;
            Gate_1.Out_Wire := Gate_2.Out_Wire;
            Gate_2.Out_Wire := Tmp_Str;
            Gates.Include (Gate_1);
            Gates.Include (Gate_2);
            Error_Bits := Roll_Over_Test;
            if Error_Bits.Length < 5 then
               Put_Line ("Found a swap: " & To_String (Gate_1.Out_Wire) &
                  ", " & To_String (Gate_2.Out_Wire));
               exit;
            else
               Gates.Exclude (Gate_1);
               Gates.Exclude (Gate_2);
               Gates.Include (Element (G1));
               Gates.Include (Element (G2));
            end if;
            G2 := Next (G2);
         end loop;
      end;
   end loop;

end Day24;
