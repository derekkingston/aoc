with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure day17 is
   type UInt is mod 2**64;
   package UInt_Vec is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => UInt);
   use UInt_Vec;

   --  Example
   --  RegisterA : UInt := 729;
   --  Program   : constant UInt_Vec.Vector :=
   --  0 & 1   & 5 & 4   3 & 0;

   --  Example
   --  RegisterA : UInt := 117440;
   --  Program : constant UInt_Vec.Vector :=
   --  0 & 3   & 5 & 4   & 3 & 0;
   --  note: the only jump is the last instruction: opcode 3, operand 0
   --  therefore, that the program will return to start if A /= 0
   --  note: only a single output instruction: opcode 5, operand 4
   --  therefore, that the only output is ever [A mod 8]
   --  note: each time through the program the first instruction
   --  results in A := A / 8
   --  so, in backwards order A should be: 0, 24, 224, 1832, 14680, 117440
   --  from satisfying at each iteration: A_n / 8 = A and A_n mod 8 = output_n
   --  or equivalently that A_n = (A + output_n) * 8

   --  Input
   RegisterA : UInt := 53437164;
   Program   : constant UInt_Vec.Vector :=
   2 & 4   & 1 & 7   & 7 & 5   & 4 & 1   & 1 & 4   & 5 & 5   & 0 & 3   & 3 & 0;
   --  note: the only jump is the last instruction: opcode 3, operand 0
   --  therefore, the program will return to start if A /= 0
   --  note: only a single output instruction: opcode 5, operand 5
   --  therefore, the only output is ever [B mod 8]
   --  last three instructions are:
   --    output [B mod 8]
   --    A := A / 8
   --    return to start (or exit if A = 0)
   --  need to understand what value of A at start of program results
   --  in the proper [B mod 8] = [program output value]

   Output : UInt_Vec.Vector := UInt_Vec.Empty_Vector;

   procedure Print_Output (V : UInt_Vec.Vector) is
   begin
      for I in V.First_Index .. V.Last_Index loop
         if I /= V.First_Index then
            Put (",");
         end if;
         Put (Integer (V.Element (I)), Width => 0);
      end loop;
   end Print_Output;

   function Compare_Program (V : UInt_Vec.Vector; I : Integer)
      return Boolean is
   begin
      for J in V.First_Index .. V.Last_Index loop
         if V.Element (J) /= Program.Element (I + J) then
            return False;
         end if;
      end loop;
      return True;
   end Compare_Program;

   function Get_Combo (X : Natural; A, B, C : UInt) return UInt is
   begin
      case X is
         when 0 => return 0;
         when 1 => return 1;
         when 2 => return 2;
         when 3 => return 3;
         when 4 => return A;
         when 5 => return B;
         when 6 => return C;
         when others => return 0;
      end case;
   end Get_Combo;

   procedure Run_Program (A0 : UInt; V : in out UInt_Vec.Vector) is
      A : UInt := A0; --  internal register A, set by input
      B : UInt := 0;  --  register B: initialized to zero
      C : UInt := 0;  --  register C: initialized to zero
      Inst_Ptr : Natural := Program.First_Index;  --  start of program
      Opcode, Operand : Natural;
      X, Z : UInt;
      Y : Natural;
   begin
      --  clear the in/out vector containing the output values
      V := UInt_Vec.Empty_Vector;
      --  continue the program until the instruction pointer is out of bounds
      while Inst_Ptr < Program.Last_Index loop
         --  program is processed in pairs: opcode, operand
         Opcode := Natural (Program.Element (Inst_Ptr));
         Operand := Natural (Program.Element (Inst_Ptr + 1));
         case Opcode is
            when 0 =>
               X := A;
               Y := Natural (Get_Combo (Operand, A, B, C));
               Z := 2 ** Y;
               A := X / Z;
               Inst_Ptr := Inst_Ptr + 2;
            when 1 =>
               B := B xor UInt (Operand);
               Inst_Ptr := Inst_Ptr + 2;
            when 2 =>
               B := Get_Combo (Operand, A, B, C) mod 8;
               Inst_Ptr := Inst_Ptr + 2;
            when 3 =>
               if A /= 0 then
                  Inst_Ptr := Operand;
               else
                  Inst_Ptr := Inst_Ptr + 2;
               end if;
            when 4 =>
               B := B xor C;
               Inst_Ptr := Inst_Ptr + 2;
            when 5 =>
               V.Append (Get_Combo (Operand, A, B, C) mod 8);
               Inst_Ptr := Inst_Ptr + 2;
            when 6 =>
               X := A;
               Y := Natural (Get_Combo (Operand, A, B, C));
               Z := 2 ** Y;
               B := X / Z;
               Inst_Ptr := Inst_Ptr + 2;
            when 7 =>
               X := A;
               Y := Natural (Get_Combo (Operand, A, B, C));
               Z := 2 ** Y;
               C := X / Z;
               Inst_Ptr := Inst_Ptr + 2;
            when others =>
               Put_Line ("Not a valid opcode: " & Opcode'Image);
               Inst_Ptr := Inst_Ptr + 2;
         end case;
      end loop;
   end Run_Program;

begin
   --  part A
   New_Line;
   Run_Program (RegisterA, Output);
   Put_Line ("Part A:");
   Print_Output (Output);
   New_Line;
   New_Line;

   --  part B
   --  Idea: run the program matching from the back; multiply by 8 after match
   --  since each time through the program, A is divided by 8 before next iter
   RegisterA := 0;
   for I in reverse Program.First_Index .. Program.Last_Index loop
      --  matched output from I .. Program.Last_Index
      --  multiple by 8 since each time through the program divides by 8
      RegisterA := RegisterA * 8;
      Run_Program (RegisterA, Output);
      --  check the last values of 'Output' to 'Program' until they match
      while not Compare_Program (Output, I) loop
         --  increase register A by 1 (correct magnitude, but offset)
         RegisterA := RegisterA + 1;
         --  re-run program with updated register A
         Run_Program (RegisterA, Output);
      end loop;
   end loop;
   Put_Line ("Part B:");
   Put_Line (RegisterA'Image);
   New_Line;

   Run_Program (RegisterA, Output);
   Put ("Output:  ");
   Print_Output (Output);
   Put_Line ("  vs");
   Put ("Program: ");
   Print_Output (Program);
   New_Line;

end day17;
