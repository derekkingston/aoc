with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Bounded_Ordered_Sets;

procedure day15 is
   type String_List is array (Positive range <>) of String (1 .. 1024);
   type Direction is (North, East, South, West);
   type Direction_Array is array (Positive range <>) of Direction;
   type Location is record
      Row : Integer;
      Col : Integer;
   end record;

   function Location_Less (Left, Right : Location) return Boolean is
   begin
      --  lexicographical ordering
      if Left.Row < Right.Row then
         return True;
      end if;
      if Left.Row > Right.Row then
         return False;
      end if;
      return Left.Col < Right.Col;
   end Location_Less;

   function Location_Eq (Left, Right : Location) return Boolean is
   begin
      if Left.Row = Right.Row and then Left.Col = Right.Col then
         return True;
      end if;
      return False;
   end Location_Eq;

   package Location_Sets is new Ada.Containers.Bounded_Ordered_Sets
     (Element_Type => Location,
      "<" => Location_Less,
      "=" => Location_Eq);

   File_Name     : constant String := "input.txt";
   File          : File_Type;
   Line          : String (1 .. 1024);
   Line_Last     : Natural;
   Wall_Idx      : Natural;
   PuzzleA       : String_List (1 .. 1024);
   PuzzleB       : String_List (1 .. 1024);
   Boxes         : Location_Sets.Set (2500);
   Row_Len       : Natural := 0;
   Col_Len       : Natural := 0;
   Robot_Idx     : Natural;
   Commands      : Direction_Array (1 .. 21000);
   Cmd_Len       : Natural := 0;
   RobotA        : Location;
   RobotB        : Location;
   Map_Score     : Natural;

   Puzzle_Save   : String_List (1 .. 1024);
   Num_Wall      : Natural;
   Num_Left_Box  : Natural;
   Num_Right_Box : Natural;
   Robot_Prev    : Location;
   Cmd_Prev      : Direction;

   procedure Copy_Puzzle (A : in out String_List; B : String_List) is
   begin
      for R in 1 .. Row_Len loop
         for C in 1 .. 2 * Col_Len loop
            A (R) (C) := B (R) (C);
         end loop;
      end loop;
   end Copy_Puzzle;

   procedure Show_Puzzle (A : String_List) is
      S : String (1 .. 2 * Col_Len);
   begin
      for R in 1 .. Row_Len loop
         S := A (R) (1 .. 2 * Col_Len);
         if R = RobotB.Row then
            S (RobotB.Col) := '@';
         end if;
         Put_Line (S);
      end loop;
      New_Line;
   end Show_Puzzle;

   function Count_Character (Ch : Character) return Natural is
      Count : Natural := 0;
   begin
      for R in 1 .. Row_Len loop
         for C in 1 .. 2 * Col_Len loop
            if PuzzleB (R) (C) = Ch then
               Count := Count + 1;
            end if;
         end loop;
      end loop;
      return Count;
   end Count_Character;

   procedure Save_Point (Dir : Direction) is
   begin
      Copy_Puzzle (Puzzle_Save, PuzzleB);
      Num_Wall := Count_Character ('#');
      Num_Left_Box := Count_Character ('[');
      Num_Right_Box := Count_Character (']');
      Robot_Prev := RobotB;
      Cmd_Prev := Dir;
   end Save_Point;

   procedure Check_Point is
      W, L, R : Natural;
      Sv_Robot : Location;
   begin
      W := Count_Character ('#');
      L := Count_Character ('[');
      R := Count_Character (']');
      if W /= Num_Wall or else
         L /= Num_Left_Box or else
         R /= Num_Right_Box
      then
         Sv_Robot := RobotB;
         Put_Line ("Checkpoint failed going " & Cmd_Prev'Image);
         RobotB := Robot_Prev;
         Show_Puzzle (Puzzle_Save);
         New_Line;
         RobotB := Sv_Robot;
         Show_Puzzle (PuzzleB);
         Get_Line (Line, Line_Last);
      end if;
   end Check_Point;

   procedure Move_Boxes (Dir : Direction) is
      DR : Integer := 0;
      DC : Integer := 0;
   begin
      case Dir is
         when North => DR := -1;
         when East =>  DC :=  1;
         when South => DR :=  1;
         when West =>  DC := -1;
      end case;
      for B in Boxes.Iterate loop
         PuzzleB (Boxes.Element (B).Row) (Boxes.Element (B).Col) := '.';
         PuzzleB (Boxes.Element (B).Row) (Boxes.Element (B).Col + 1) := '.';
      end loop;
      for B in Boxes.Iterate loop
         PuzzleB (Boxes.Element (B).Row + DR) (Boxes.Element (B).Col + DC) := '[';
         PuzzleB (Boxes.Element (B).Row + DR) (Boxes.Element (B).Col + DC + 1) := ']';
      end loop;
   end Move_Boxes;

   procedure Move_RobotA (Dir : Direction) is
      X : Natural;
   begin
      case Dir is
         when North =>
            if PuzzleA (RobotA.Row - 1) (RobotA.Col) = '#' then
               return;
            end if;
            if PuzzleA (RobotA.Row - 1) (RobotA.Col) = '.' then
               RobotA.Row := RobotA.Row - 1;
            else
               --  see if box can be pushed
               X := RobotA.Row - 1;
               while X > 0 loop
                  if PuzzleA (X) (RobotA.Col) = '#' then
                     exit;
                  end if;
                  if PuzzleA (X) (RobotA.Col) = '.' then
                     PuzzleA (X) (RobotA.Col) := 'O';
                     PuzzleA (RobotA.Row - 1) (RobotA.Col) := '.';
                     RobotA.Row := RobotA.Row - 1;
                     exit;
                  end if;
                  X := X - 1;
               end loop;
            end if;
         when East =>
            if PuzzleA (RobotA.Row) (RobotA.Col + 1) = '#' then
               return;
            end if;
            if PuzzleA (RobotA.Row) (RobotA.Col + 1) = '.' then
               RobotA.Col := RobotA.Col + 1;
            else
               --  see if box can be pushed
               X := RobotA.Col + 1;
               while X <= Col_Len loop
                  if PuzzleA (RobotA.Row) (X) = '#' then
                     exit;
                  end if;
                  if PuzzleA (RobotA.Row) (X) = '.' then
                     PuzzleA (RobotA.Row) (X) := 'O';
                     PuzzleA (RobotA.Row) (RobotA.Col + 1) := '.';
                     RobotA.Col := RobotA.Col + 1;
                     exit;
                  end if;
                  X := X + 1;
               end loop;
            end if;
         when South =>
            if PuzzleA (RobotA.Row + 1) (RobotA.Col) = '#' then
               return;
            end if;
            if PuzzleA (RobotA.Row + 1) (RobotA.Col) = '.' then
               RobotA.Row := RobotA.Row + 1;
            else
               --  see if box can be pushed
               X := RobotA.Row + 1;
               while X <= Row_Len loop
                  if PuzzleA (X) (RobotA.Col) = '#' then
                     exit;
                  end if;
                  if PuzzleA (X) (RobotA.Col) = '.' then
                     PuzzleA (X) (RobotA.Col) := 'O';
                     PuzzleA (RobotA.Row + 1) (RobotA.Col) := '.';
                     RobotA.Row := RobotA.Row + 1;
                     exit;
                  end if;
                  X := X + 1;
               end loop;
            end if;
         when West =>
            if PuzzleA (RobotA.Row) (RobotA.Col - 1) = '#' then
               return;
            end if;
            if PuzzleA (RobotA.Row) (RobotA.Col - 1) = '.' then
               RobotA.Col := RobotA.Col - 1;
            else
               --  see if box can be pushed
               X := RobotA.Col - 1;
               while X > 0 loop
                  if PuzzleA (RobotA.Row) (X) = '#' then
                     exit;
                  end if;
                  if PuzzleA (RobotA.Row) (X) = '.' then
                     PuzzleA (RobotA.Row) (X) := 'O';
                     PuzzleA (RobotA.Row) (RobotA.Col - 1) := '.';
                     RobotA.Col := RobotA.Col - 1;
                     exit;
                  end if;
                  X := X - 1;
               end loop;
            end if;
      end case;
   end Move_RobotA;

   function GPS_Sum_A return Natural is
      Score : Natural := 0;
   begin
      for R in 1 .. Row_Len loop
         for C in 1 .. Col_Len loop
            if PuzzleA (R) (C) = 'O' then
               Score := Score + 100 * (R - 1) + (C - 1);
            end if;
         end loop;
      end loop;
      return Score;
   end GPS_Sum_A;

   function sign (X : Integer) return Integer is
   begin
      if X < 0 then
         return -1;
      end if;
      if X > 0 then
         return 1;
      end if;
      return 0;
   end sign;

   function Try_Push_Vertical (DR, DC : Integer) return Boolean is
   begin
      --  base case: immediately below is '#'
      if PuzzleB (RobotB.Row + DR) (RobotB.Col + DC) = '#' then
         return False;
      end if;
      --  base case: immediately below is '.'
      if PuzzleB (RobotB.Row + DR) (RobotB.Col + DC) = '.' then
         return True;
      end if;
      --  recursively add lower boxes
      if PuzzleB (RobotB.Row + DR) (RobotB.Col + DC) = '[' then
         Boxes.Include ((Row => RobotB.Row + DR, Col => RobotB.Col + DC));
         return Try_Push_Vertical (DR + sign (DR), DC) and then
            Try_Push_Vertical (DR + sign (DR), DC + 1);
      else
         Boxes.Include ((Row => RobotB.Row + DR, Col => RobotB.Col + DC - 1));
         return Try_Push_Vertical (DR + sign (DR), DC - 1) and then
            Try_Push_Vertical (DR + sign (DR), DC);
      end if;
   end Try_Push_Vertical;

   function Try_Push (Dir : Direction) return Boolean is
      X : Natural;
   begin
      case Dir is
         when North =>
            return Try_Push_Vertical (-1, 0);
         when East =>
            X := RobotB.Col + 1;
            while X < 2 * Col_Len loop
               if PuzzleB (RobotB.Row) (X) = '[' then
                  Boxes.Include ((Row => RobotB.Row, Col => X));
               end if;
               if PuzzleB (RobotB.Row) (X) = '#' then
                  return False;
               end if;
               if PuzzleB (RobotB.Row) (X) = '.' then
                  return True;
               end if;
               X := X + 1;
            end loop;
            return False;
         when South =>
            return Try_Push_Vertical (1, 0);
         when West =>
            X := RobotB.Col - 1;
            while X > 1 loop
               if PuzzleB (RobotB.Row) (X) = '[' then
                  Boxes.Include ((Row => RobotB.Row, Col => X));
               end if;
               if PuzzleB (RobotB.Row) (X) = '#' then
                  return False;
               end if;
               if PuzzleB (RobotB.Row) (X) = '.' then
                  return True;
               end if;
               X := X - 1;
            end loop;
            return False;
      end case;
   end Try_Push;

   procedure Move_RobotB (Dir : Direction) is
      DR, DC : Integer;
   begin
      case Dir is
         when North =>
            DR := -1; DC :=  0;
         when East =>
            DR :=  0; DC :=  1;
         when South =>
            DR :=  1; DC :=  0;
         when West =>
            DR :=  0; DC := -1;
      end case;
      --  case for robot pushing against wall
      if PuzzleB (RobotB.Row + DR) (RobotB.Col + DC) = '#' then
         return;
      end if;
      --  case for robot moving to open space
      if PuzzleB (RobotB.Row + DR) (RobotB.Col + DC) = '.' then
         RobotB.Row := RobotB.Row + DR;
         RobotB.Col := RobotB.Col + DC;
         return;
      end if;
      --  need to check if robot can push box(es)
      Boxes.Clear;
      if Try_Push (Dir) then
         RobotB.Row := RobotB.Row + DR;
         RobotB.Col := RobotB.Col + DC;
         Move_Boxes (Dir);
      end if;
   end Move_RobotB;

   function GPS_Sum_B return Natural is
      Score : Natural := 0;
   begin
      for R in 1 .. Row_Len loop
         for C in 1 .. 2 * Col_Len loop
            if PuzzleB (R) (C) = '[' then
               Score := Score + 100 * (R - 1) + (C - 1);
            end if;
         end loop;
      end loop;
      return Score;
   end GPS_Sum_B;

begin
   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) loop
      Get_Line (File, Line, Line_Last);
      Wall_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => "#", From => 1);
      if Wall_Idx /= 0 then
         --  must be part of the map
         if Col_Len = 0 then
            Col_Len := Line_Last;
         end if;
         if Col_Len /= Line_Last then
            Put_Line ("All rows must be of the same length");
            exit;
         end if;
         Row_Len := Row_Len + 1;

         PuzzleA (Row_Len) := Line;
         Robot_Idx := Index (Source => Line (1 .. Line_Last),
            Pattern => "@", From => 1);
         if Robot_Idx /= 0 then
            RobotA.Row := Row_Len;
            RobotA.Col := Robot_Idx;
            PuzzleA (RobotA.Row) (RobotA.Col) := '.';
         end if;

         for I in 1 .. Line_Last loop
            if Line (I) = '#' then
               PuzzleB (Row_Len) (2 * I - 1 .. 2 * I) := "##";
            elsif Line (I) = '.' then
               PuzzleB (Row_Len) (2 * I - 1 .. 2 * I) := "..";
            elsif Line (I) = 'O' then
               PuzzleB (Row_Len) (2 * I - 1 .. 2 * I) := "[]";
            elsif Line (I) = '@' then
               PuzzleB (Row_Len) (2 * I - 1 .. 2 * I) := "..";
               RobotB.Row := Row_Len;
               RobotB.Col := 2 * I - 1;
            else
               Put_Line ("Could not process character: " & Line (I .. I));
            end if;
         end loop;
      else
         --  build up list of instructions
         for I in 1 .. Line_Last loop
            Cmd_Len := Cmd_Len + 1;
            if Line (I) = '^' then
               Commands (Cmd_Len) := North;
            elsif Line (I) = '>' then
               Commands (Cmd_Len) := East;
            elsif Line (I) = 'v' then
               Commands (Cmd_Len) := South;
            elsif Line (I) = '<' then
               Commands (Cmd_Len) := West;
            else
               Cmd_Len := Cmd_Len - 1;
               Put_Line ("Cound not process: " & Line (I .. I));
            end if;
         end loop;
      end if;
   end loop;

   --  Close the file
   Close (File);

   --  simulate each of the robot movements
   for I in 1 .. Cmd_Len loop
      Move_RobotA (Commands (I));
   end loop;
   Map_Score := GPS_Sum_A;
   Put_Line ("Part A: " & Map_Score'Image);

   --  count boxes and obstacles in PuzzleB
   --  Show_Puzzle (PuzzleB);
   for I in 1 .. Cmd_Len loop
      --  Show_Puzzle (PuzzleB);
      Save_Point (Commands (I));
      Move_RobotB (Commands (I));
      Check_Point;
   end loop;
   --  Show_Puzzle (PuzzleB);
   Map_Score := GPS_Sum_B;
   Put_Line ("Part B: " & Map_Score'Image);

end day15;
