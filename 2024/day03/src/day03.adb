with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure day03 with SPARK_Mode is
   File_Name   : constant String := "input.txt";
   File        : File_Type;
   Line        : String (1 .. 20000);
   Line_Last   : Natural;
   Mul_Idx     : Natural;
   Do_Idx      : Natural;
   Dont_Idx    : Natural;
   Work_Idx    : Natural;
   X           : Integer;
   Y           : Integer;
   Num_Last    : Natural;
   Comma       : Natural;
   Close_Paren : Natural;
   AResult     : Integer := 0;
   BResult     : Integer := 0;
   Is_Enabled  : Boolean := True;

   function Is_Number (A : String) return Boolean is
      Num_Str  : constant String := "0123456789";
      Find_Idx : Natural;
   begin
      --  Put_Line ("Checking: '" & A & "'");
      if A'Length < 1 then
         return False;
      end if;
      if A'Length > 3 then
         return False;
      end if;
      for I in A'Range loop
         Find_Idx := Index (Source => Num_Str,
            Pattern => A (I .. I), From => 1);
         if Find_Idx = 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Number;

   function Min_Positive (A, B : Natural) return Natural is
   begin
      if A = 0 then
         return B;
      end if;
      if B = 0 then
         return A;
      end if;
      if A < B then
         return A;
      end if;
      return B;
   end Min_Positive;

   function Min_Positive (A, B, C : Natural) return Natural is
   begin
      return Min_Positive (Min_Positive (A, B), C);
   end Min_Positive;

begin
   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) loop
      Get_Line (File, Line, Line_Last);
      Mul_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => "mul(", From => 1);
      Do_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => "do()", From => 1);
      Dont_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => "don't()", From => 1);
      Work_Idx := Min_Positive (Mul_Idx, Do_Idx, Dont_Idx);

      while Work_Idx > 0 loop
         if Work_Idx = Do_Idx then
            Is_Enabled := True;
            Do_Idx := Index (Source => Line (Do_Idx + 4 .. Line_Last),
               Pattern => "do()", From => Do_Idx + 4);
            Work_Idx := Min_Positive (Mul_Idx, Do_Idx, Dont_Idx);
         elsif Work_Idx = Dont_Idx then
            Is_Enabled := False;
            Dont_Idx := Index (Source => Line (Dont_Idx + 7 .. Line_Last),
               Pattern => "don't()", From => Dont_Idx + 7);
            Work_Idx := Min_Positive (Mul_Idx, Do_Idx, Dont_Idx);
         else
            Close_Paren := Index (Source => Line (Mul_Idx .. Line_Last),
               Pattern => ")", From => Mul_Idx);
            if Close_Paren /= 0 then
               Comma := Index (Source => Line (Mul_Idx .. Close_Paren),
                  Pattern => ",", From => Mul_Idx);
               if Comma /= 0 then
                  if Is_Number (Line (Mul_Idx + 4 .. Comma - 1)) then
                     Get (Line (Mul_Idx + 4 .. Comma - 1), X, Num_Last);
                     if Is_Number (Line (Comma + 1 .. Close_Paren - 1)) then
                        Get (Line (Comma + 1 .. Close_Paren - 1), Y, Num_Last);
                        --  Put_Line ("X: " & Integer'Image (X) &
                        --     " Y: " & Integer'Image (Y));
                        AResult := AResult + X * Y;
                        if Is_Enabled then
                           BResult := BResult + X * Y;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
            Mul_Idx := Index (Source => Line (Mul_Idx + 4 .. Line_Last),
               Pattern => "mul(", From => Mul_Idx + 4);
            Work_Idx := Min_Positive (Mul_Idx, Do_Idx, Dont_Idx);
         end if;
      end loop;
   end loop;

   --  Close the file
   Close (File);

   --  print the results
   Put_Line ("Part A: " & Integer'Image (AResult));
   Put_Line ("Part B: " & Integer'Image (BResult));

end day03;
