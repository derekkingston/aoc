with Ada.Text_IO; use Ada.Text_IO;

procedure day10 with SPARK_Mode is
   type Integer_Array is array (Positive range <>) of Integer;
   type Integer_Matrix is array (Positive range <>)
      of Integer_Array (1 .. 46);
   File_Name    : constant String := "input.txt";
   File         : File_Type;
   Line         : String (1 .. 46);
   Line_Last    : Natural;
   Puzzle       : Integer_Matrix (1 .. 46);
   Visited      : Integer_Matrix (1 .. 46);
   Row_Len      : Natural := 0;
   Col_Len      : Natural := 0;
   Score        : Natural := 0;
   Total_ScoreA : Natural := 0;
   Total_ScoreB : Natural := 0;

   procedure Clear_Visited is
   begin
      for I in 1 .. Row_Len loop
         for J in 1 .. Col_Len loop
            Visited (I) (J) := 0;
         end loop;
      end loop;
   end Clear_Visited;

   function Sum_Visited return Natural is
      Sum : Natural := 0;
   begin
      for I in 1 .. Row_Len loop
         for J in 1 .. Col_Len loop
            Sum := Sum + Visited (I) (J);
         end loop;
      end loop;
      return Sum;
   end Sum_Visited;

   function Count_Paths (R, C : Natural) return Natural is
      V : constant Integer := Puzzle (R) (C) + 1;
      Count : Natural := 0;
   begin
      --  Put_Line ("(" & R'Image & ", " & C'Image & "): " & V'Image);
      --  base case (R, C) was a 9
      if V = 10 then
         Visited (R) (C) := Visited (R) (C) + 1;
         if Visited (R) (C) > 1 then
            return 0;
         end if;
         return 1;
      end if;
      --  north
      if R > 1 and then Puzzle (R - 1) (C) = V then
         Count := Count + Count_Paths (R - 1, C);
      end if;
      --  east
      if C < Col_Len and then Puzzle (R) (C + 1) = V then
         Count := Count + Count_Paths (R, C + 1);
      end if;
      --  south
      if R < Row_Len and then Puzzle (R + 1) (C) = V then
         Count := Count + Count_Paths (R + 1, C);
      end if;
      --  west
      if C > 1 and then Puzzle (R) (C - 1) = V then
         Count := Count + Count_Paths (R, C - 1);
      end if;
      return Count;
   end Count_Paths;

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

      for I in 1 .. Col_Len loop
         Puzzle (Row_Len) (I) := Integer'Value (Line (I .. I));
      end loop;

   end loop;

   --  Close the file
   Close (File);

   for I in 1 .. Row_Len loop
      for J in 1 .. Col_Len loop
         if Puzzle (I) (J) = 0 then
            Clear_Visited;
            Score := Count_Paths (I, J);
            Put_Line ("(" & I'Image & ", " & J'Image & "): " & Score'Image);
            Total_ScoreA := Total_ScoreA + Score;
            Total_ScoreB := Total_ScoreB + Sum_Visited;
         end if;
      end loop;
   end loop;

   Put_Line ("Part A: " & Total_ScoreA'Image);
   Put_Line ("Part B: " & Total_ScoreB'Image);

end day10;
