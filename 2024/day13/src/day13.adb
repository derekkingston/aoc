with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure day13 with SPARK_Mode is
   type Int is new Long_Long_Integer;
   type Double is new Long_Long_Float;

   File_Name    : constant String := "input.txt";
   File         : File_Type;
   Line         : String (1 .. 256);
   Line_Last    : Natural;
   Preamble     : Natural;
   X_idx, Y_idx : Natural;
   Comma        : Natural;
   A_11, A_12   : Int;
   A_21, A_22   : Int;
   Determinant  : Int;
   Ai_11, Ai_12 : Double;
   Ai_21, Ai_22 : Double;
   X_1, X_2     : Double;
   Y_1, Y_2     : Int;
   A_presses    : Int;
   B_presses    : Int;
   Z_1, Z_2     : Int;
   CostA        : Int := 0;
   CostB        : Int := 0;

begin
   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) loop
      Get_Line (File, Line, Line_Last);

      --  check for start of individual puzzle
      Preamble := Index (Source => Line (1 .. Line_Last),
         Pattern => "Button A: ", From => 1);
      if Preamble /= 0 then
         --  Treat as a 2x2 matrix, invert, ensure integer solution
         --  y = Ax  -->  x = A^-1 y

         --  read first column of A matrix
         X_idx := Index (Source => Line (1 .. Line_Last),
            Pattern => "X+", From => 1);
         Comma := Index (Source => Line (1 .. Line_Last),
            Pattern => ",", From => 1);
         Y_idx := Index (Source => Line (1 .. Line_Last),
            Pattern => "Y+", From => 1);
         A_11 := Int'Value (Line (X_idx + 2 .. Comma - 1));
         A_21 := Int'Value (Line (Y_idx + 2 .. Line_Last));

         --  read second column of A matrix
         Get_Line (File, Line, Line_Last);
         X_idx := Index (Source => Line (1 .. Line_Last),
            Pattern => "X+", From => 1);
         Comma := Index (Source => Line (1 .. Line_Last),
            Pattern => ",", From => 1);
         Y_idx := Index (Source => Line (1 .. Line_Last),
            Pattern => "Y+", From => 1);
         A_12 := Int'Value (Line (X_idx + 2 .. Comma - 1));
         A_22 := Int'Value (Line (Y_idx + 2 .. Line_Last));

         --  read Y vector
         Get_Line (File, Line, Line_Last);
         X_idx := Index (Source => Line (1 .. Line_Last),
            Pattern => "X=", From => 1);
         Comma := Index (Source => Line (1 .. Line_Last),
            Pattern => ",", From => 1);
         Y_idx := Index (Source => Line (1 .. Line_Last),
            Pattern => "Y=", From => 1);
         Y_1 := Int'Value (Line (X_idx + 2 .. Comma - 1));
         Y_2 := Int'Value (Line (Y_idx + 2 .. Line_Last));

         --  invert the A matrix
         Determinant := A_11 * A_22 - A_12 * A_21;
         if abs (Determinant) > 0 then
            Ai_11 := Double (A_22) / Double (Determinant);
            Ai_12 := Double (-A_12) / Double (Determinant);
            Ai_21 := Double (-A_21) / Double (Determinant);
            Ai_22 := Double (A_11) / Double (Determinant);

            --  PART A, recall x = A^-1 y
            X_1 := Ai_11 * Double (Y_1) + Ai_12 * Double (Y_2);
            X_2 := Ai_21 * Double (Y_1) + Ai_22 * Double (Y_2);
            --  round to integers and check if math works out
            A_presses := Int (Double'Rounding (X_1));
            B_presses := Int (Double'Rounding (X_2));
            Z_1 := A_11 * A_presses + A_12 * B_presses;
            Z_2 := A_21 * A_presses + A_22 * B_presses;
            if abs (Z_1 - Y_1) < 1 and then abs (Z_2 - Y_2) < 1 then
               --  feasible integer solution
               CostA := CostA + 3 * A_presses + B_presses;
            end if;

            --  PART B, recall x = A^-1 y
            Y_1 := Y_1 + 10000000000000;
            Y_2 := Y_2 + 10000000000000;
            X_1 := Ai_11 * Double (Y_1) + Ai_12 * Double (Y_2);
            X_2 := Ai_21 * Double (Y_1) + Ai_22 * Double (Y_2);
            --  round to integers and check if math works out
            A_presses := Int (Double'Rounding (X_1));
            B_presses := Int (Double'Rounding (X_2));
            Z_1 := A_11 * A_presses + A_12 * B_presses;
            Z_2 := A_21 * A_presses + A_22 * B_presses;
            if abs (Z_1 - Y_1) < 1 and then abs (Z_2 - Y_2) < 1 then
               --  feasible integer solution
               CostB := CostB + 3 * A_presses + B_presses;
            end if;
         end if;
      end if;
   end loop;

   --  Close the file
   Close (File);

   Put_Line ("Part A: " & CostA'Image);
   Put_Line ("Part B: " & CostB'Image);

end day13;
