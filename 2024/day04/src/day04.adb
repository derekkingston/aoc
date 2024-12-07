with Ada.Text_IO; use Ada.Text_IO;

procedure day04 with SPARK_Mode is
   type String_List is array (Positive range <>) of String (1 .. 256);
   File_Name    : constant String := "input.txt";
   File         : File_Type;
   Line         : String (1 .. 256);
   Line_Last    : Natural;
   Puzzle       : String_List (1 .. 256);
   Row_Len      : Natural := 0;
   Col_Len      : Natural := 0;
   Xmas_Count   : Natural := 0;
   X_mas_Count  : Natural := 0;

   function Puzzle_Search (X, Y, DX, DY : Integer)
      return Natural is
   begin
      if X + 3 * DX < 1 or else X + 3 * DX > day04.Col_Len then
         return 0;
      end if;
      if Y + 3 * DY < 1 or else Y + 3 * DY > day04.Row_Len then
         return 0;
      end if;
      if day04.Puzzle (X + DX) (Y + DY .. Y + DY) /= "M" then
         return 0;
      end if;
      if day04.Puzzle (X + 2 * DX) (Y + 2 * DY .. Y + 2 * DY) /= "A" then
         return 0;
      end if;
      if day04.Puzzle (X + 3 * DX) (Y + 3 * DY .. Y + 3 * DY) /= "S" then
         return 0;
      end if;
      return 1;
   end Puzzle_Search;

   function Is_MAS_As_X (X, Y : Integer) return Natural is
   begin
      if X - 1 < 1 or else X + 1 > day04.Col_Len then
         return 0;
      end if;
      if Y - 1 < 1 or else Y + 1 > day04.Row_Len then
         return 0;
      end if;
      if day04.Puzzle (X + 1) (Y + 1 .. Y + 1) = "M" and then
            day04.Puzzle (X - 1) (Y + 1 .. Y + 1) = "M" and then
            day04.Puzzle (X - 1) (Y - 1 .. Y - 1) = "S" and then
            day04.Puzzle (X + 1) (Y - 1 .. Y - 1) = "S"
      then
         return 1;
      end if;
      if day04.Puzzle (X + 1) (Y + 1 .. Y + 1) = "S" and then
            day04.Puzzle (X - 1) (Y + 1 .. Y + 1) = "M" and then
            day04.Puzzle (X - 1) (Y - 1 .. Y - 1) = "M" and then
            day04.Puzzle (X + 1) (Y - 1 .. Y - 1) = "S"
      then
         return 1;
      end if;
      if day04.Puzzle (X + 1) (Y + 1 .. Y + 1) = "S" and then
            day04.Puzzle (X - 1) (Y + 1 .. Y + 1) = "S" and then
            day04.Puzzle (X - 1) (Y - 1 .. Y - 1) = "M" and then
            day04.Puzzle (X + 1) (Y - 1 .. Y - 1) = "M"
      then
         return 1;
      end if;
      if day04.Puzzle (X + 1) (Y + 1 .. Y + 1) = "M" and then
            day04.Puzzle (X - 1) (Y + 1 .. Y + 1) = "S" and then
            day04.Puzzle (X - 1) (Y - 1 .. Y - 1) = "S" and then
            day04.Puzzle (X + 1) (Y - 1 .. Y - 1) = "M"
      then
         return 1;
      end if;
      return 0;
   end Is_MAS_As_X;
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

   --  Part A: count occurrances of XMAS in any direction
   for I in 1 .. Col_Len loop
      for J in 1 .. Row_Len loop
         if Puzzle (I) (J .. J) = "X" then
            Xmas_Count := Xmas_Count + Puzzle_Search (I, J, -1, -1);
            Xmas_Count := Xmas_Count + Puzzle_Search (I, J, -1,  0);
            Xmas_Count := Xmas_Count + Puzzle_Search (I, J, -1,  1);
            Xmas_Count := Xmas_Count + Puzzle_Search (I, J,  0,  1);
            Xmas_Count := Xmas_Count + Puzzle_Search (I, J,  1,  1);
            Xmas_Count := Xmas_Count + Puzzle_Search (I, J,  1,  0);
            Xmas_Count := Xmas_Count + Puzzle_Search (I, J,  1, -1);
            Xmas_Count := Xmas_Count + Puzzle_Search (I, J,  0, -1);
         end if;
      end loop;
   end loop;
   Put_Line ("Part A: " & Integer'Image (Xmas_Count));

   --  Part B: count x patterns of MAS
   for I in 1 .. Col_Len loop
      for J in 1 .. Row_Len loop
         if Puzzle (I) (J .. J) = "A" then
            X_mas_Count := X_mas_Count + Is_MAS_As_X (I, J);
         end if;
      end loop;
   end loop;
   Put_Line ("Part B: " & Integer'Image (X_mas_Count));

end day04;
