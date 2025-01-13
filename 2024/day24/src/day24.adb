with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
use Ada.Containers;

procedure Day24 is
   type UInt is mod 2**64;
   type Wire_State is (ON, OFF, UNK);
   type Gate_Type is (AND_GATE, OR_GATE, XOR_GATE);

   --  key: wire name, value: wire state
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

   type Gate_Sets_Access is access all Gate_Sets.Set;

   --  key: wire name, value: set of Gates for which wire is input
   package Wire_Input_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Unbounded_String, "<" => "<", "=" => "=",
      Element_Type => Gate_Sets_Access);
   use Wire_Input_Maps;

   package Gate_Vec is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Gate);
   use Gate_Vec;

   package Natural_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Natural, "<" => "<", "=" => "=");
   use Natural_Sets;

   package String_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Unbounded_String, "<" => "<", "=" => "=");
   use String_Sets;

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
   Wire_In_Map   : Wire_Input_Maps.Map;
   Circuit       : Gate_Vec.Vector := Gate_Vec.Empty_Vector;
   Final_Val     : UInt := 0;
   Error_Bits    : Natural_Sets.Set;
   Wrong_Wires   : String_Sets.Set;

   function All_Z_Wires_Set return Boolean is
   begin
      for Csr in Wires.Iterate loop
         if To_String (Key (Csr)) (1) = 'z' and then Element (Csr) = UNK then
            return False;
         end if;
      end loop;
      return True;
   end All_Z_Wires_Set;

   function Wire_Is_Input (Wire_Str : Unbounded_String) return Boolean is
   begin
      if To_String (Wire_Str) (1) = 'x' or else
         To_String (Wire_Str) (1) = 'y'
      then
         return True;
      end if;
      return False;
   end Wire_Is_Input;

   function Wire_Is_Output (Wire_Str : Unbounded_String) return Boolean is
   begin
      if To_String (Wire_Str) (1) = 'z' then
         return True;
      end if;
      return False;
   end Wire_Is_Output;

   --  careful, this function assumes the form 'z00'
   function Wire_Output_Idx (Wire_Str : Unbounded_String) return Natural is
   begin
      return Natural'Value (To_String (Wire_Str) (2 .. 3));
   end Wire_Output_Idx;

   procedure Simulate_Sum (X0, Y0 : UInt) is
      X : UInt := X0;
      Y : UInt := Y0;
      Bit_Idx : Natural;
      Wire_Str : Unbounded_String;
   begin
      --  reset wires
      Final_Val := 0;
      for Csr in Wires.Iterate loop
         if Wire_Is_Input (Key (Csr)) then
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
         for G of Circuit loop
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

   function Next_Gate_Is_Or (Wire_Str : Unbounded_String)
         return Boolean is
      First_Gate_Csr : Gate_Sets.Cursor;
      Second_Gate_Csr : Gate_Sets.Cursor;
      First_Gate : Gate;
   begin
      --  must actually have a set of defined next gates
      if not Wire_In_Map.Contains (Wire_Str) then
         return False;
      end if;
      First_Gate_Csr := First (Wire_In_Map.Element (Wire_Str).all);
      --  must contain at least one elements in the set
      if not Has_Element (First_Gate_Csr) then
         return False;
      end if;
      First_Gate := Element (First_Gate_Csr);
      --  next gate must be OR
      if First_Gate.G_Type /= OR_GATE then
         return False;
      end if;
      --  must not have 2 or more follow on gates
      Second_Gate_Csr := Next (First_Gate_Csr);
      if Has_Element (Second_Gate_Csr) then
         return False;
      end if;
      return True;
   end Next_Gate_Is_Or;

   function Next_Gates_Are_Xor_And (Wire_Str : Unbounded_String)
         return Boolean is
      First_Gate_Csr : Gate_Sets.Cursor;
      Second_Gate_Csr : Gate_Sets.Cursor;
      Third_Gate_Csr : Gate_Sets.Cursor;
      First_Gate : Gate;
      Second_Gate : Gate;
   begin
      --  must actually have a set of defined next gates
      if not Wire_In_Map.Contains (Wire_Str) then
         return False;
      end if;
      First_Gate_Csr := First (Wire_In_Map.Element (Wire_Str).all);
      --  must contain at least two elements in the set
      if not Has_Element (First_Gate_Csr) then
         return False;
      end if;
      First_Gate := Element (First_Gate_Csr);
      Second_Gate_Csr := Next (First_Gate_Csr);
      if not Has_Element (Second_Gate_Csr) then
         return False;
      end if;
      Second_Gate := Element (Second_Gate_Csr);
      --  must not have 3 or more follow on gates
      Third_Gate_Csr := Next (Second_Gate_Csr);
      if Has_Element (Third_Gate_Csr) then
         return False;
      end if;
      --  next gates are AND and XOR
      if First_Gate.G_Type = AND_GATE and then
         Second_Gate.G_Type = XOR_GATE
      then
         return True;
      end if;
      if First_Gate.G_Type = XOR_GATE and then
         Second_Gate.G_Type = AND_GATE
      then
         return True;
      end if;
      return False;
   end Next_Gates_Are_Xor_And;

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
         --  parse the wire definition
         Wire_Tmp := To_Unbounded_String (Line (1 .. Colon_Idx - 1));
         if "1" = Line (Colon_Idx + 2 .. Line_Last) then
            Wires.Include (Wire_Tmp, ON);
         else
            Wires.Include (Wire_Tmp, OFF);
         end if;
      end if;

      --  read gates
      if Arrow_Idx /= 0 then
         --  parse the gate definition
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

         --  make sure that a wire referenced in a gate is included
         --  in the complete wire map
         if not Wires.Contains (Gate_Tmp.In_Wire_A) then
            Wires.Include (Gate_Tmp.In_Wire_A, UNK);
         end if;
         if not Wires.Contains (Gate_Tmp.In_Wire_B) then
            Wires.Include (Gate_Tmp.In_Wire_B, UNK);
         end if;
         if not Wires.Contains (Gate_Tmp.Out_Wire) then
            Wires.Include (Gate_Tmp.Out_Wire, UNK);
         end if;

         --  add the gate to the total circuit
         Circuit.Append (Gate_Tmp);

         --  for both input wires, mark that this gate uses it as input
         if not Wire_In_Map.Contains (Gate_Tmp.In_Wire_A) then
            Wire_In_Map.Include (Gate_Tmp.In_Wire_A, new Gate_Sets.Set);
         end if;
         Wire_In_Map.Element (Gate_Tmp.In_Wire_A).Include (Gate_Tmp);
         if not Wire_In_Map.Contains (Gate_Tmp.In_Wire_B) then
            Wire_In_Map.Include (Gate_Tmp.In_Wire_B, new Gate_Sets.Set);
         end if;
         Wire_In_Map.Element (Gate_Tmp.In_Wire_B).Include (Gate_Tmp);
      end if;
   end loop;

   --  Close the file
   Close (File);

   --  Part A: simulate the circuit
   while not All_Z_Wires_Set loop
      for G of Circuit loop
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
   --  45923082839246

   --  Part B: find proper gate swap
   --  roll over test
   Error_Bits := Roll_Over_Test;
   Put ("Roll over test showed errors with: ");
   for EB of Error_Bits loop
      Put (EB'Image);
   end loop;
   New_Line;
   --  appears to have errors on bits 20, 21, 22, 23, 31

   --  ASSUME that the circuit is a standard Adder and follows:
   --  https://en.wikipedia.org/wiki/Adder_(electronics)#/media/File:Full-adder_logic_diagram.svg
   --  then the following loop inspects each gate to check which
   --  gate output wires do not conform with the wiki diagram above
   for G of Circuit loop
      --  process all gates except the ones that drive the first and last 'z'
      --  NOTE: there are z00 .. z45 circuit outputs in the 'input.txt' file
      if not (Wire_Is_Output (G.Out_Wire) and then
         (Wire_Output_Idx (G.Out_Wire) = 0 or else
         Wire_Output_Idx (G.Out_Wire) = 45))
      then
         --  A, B are never paired with an intermediate wire
         if Wire_Is_Input (G.In_Wire_A) and then
            not Wire_Is_Input (G.In_Wire_B)
         then
            Wrong_Wires.Include (G.Out_Wire);
         else
            case G.G_Type is
               when AND_GATE =>
                  --  AND gates must lead to an OR gate
                  if not Next_Gate_Is_Or (G.Out_Wire) then
                     Wrong_Wires.Include (G.Out_Wire);
                  end if;
               when OR_GATE =>
                  --  OR only of intermediate wires
                  if Wire_Is_Input (G.In_Wire_A) or else
                     Wire_Is_Input (G.In_Wire_B) or else
                     Wire_Is_Output (G.In_Wire_A) or else
                     Wire_Is_Output (G.In_Wire_B)
                  then
                     Wrong_Wires.Include (G.Out_Wire);
                  elsif not Next_Gates_Are_Xor_And (G.Out_Wire) then
                     --  OR gates always lead to a carry value which must go
                     --  to exactly XOR and AND gates
                     Wrong_Wires.Include (G.Out_Wire);
                  end if;
               when XOR_GATE =>
                  --  XOR that drives an output is composed entirely from
                  --  intermediate wires
                  if Wire_Is_Output (G.Out_Wire) and then (
                     Wire_Is_Input (G.In_Wire_A) or else
                     Wire_Is_Input (G.In_Wire_B) or else
                     Wire_Is_Output (G.In_Wire_A) or else
                     Wire_Is_Output (G.In_Wire_B))
                  then
                     Wrong_Wires.Include (G.Out_Wire);
                  end if;

                  --  XOR that drives an intermediate wire must have input
                  --  wires that are BOTH direct inputs
                  if not Wire_Is_Output (G.Out_Wire) and then
                     not Wire_Is_Input (G.In_Wire_A) and then
                     not Wire_Is_Input (G.In_Wire_B)
                  then
                     Wrong_Wires.Include (G.Out_Wire);
                  end if;

                  --  A xor B always leads to XOR and AND gate
                  if Wire_Is_Input (G.In_Wire_A) and then
                     Wire_Is_Input (G.In_Wire_B) and then
                     not Wire_Is_Output (G.Out_Wire) and then
                     not Next_Gates_Are_Xor_And (G.Out_Wire)
                  then
                     Wrong_Wires.Include (G.Out_Wire);
                  end if;
            end case;
         end if;
      end if;
   end loop;

   --  exclude any gates that were attached to inputs 00
   --  since there is no carry bit for z00
   for G of Circuit loop
      if To_String (G.In_Wire_A) = "x00" or else
         To_String (G.In_Wire_A) = "y00" or else
         To_String (G.In_Wire_B) = "x00" or else
         To_String (G.In_Wire_B) = "y00"
      then
         Wrong_Wires.Exclude (G.Out_Wire);
      end if;
   end loop;

   --  print all wrong wires in sorted order
   Put ("Part B: ");
   for Wire of Wrong_Wires loop
      Put (To_String (Wire) & ",");
   end loop;
   New_Line;
   --  jgb,rkf,rrs,rvc,vcg,z09,z20,z24

end Day24;
