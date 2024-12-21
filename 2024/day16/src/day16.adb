with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Bounded_Ordered_Sets;

procedure day16 with SPARK_Mode is
   PUZZLE_SIZE : constant Natural := 142;
   type String_List is array (Positive range <>) of String (1 .. PUZZLE_SIZE);
   type Direction is (North, East, South, West);
   type Integer_Array is array (Direction) of Integer;
   type Integer_Matrix is array (Positive range <>) of Integer_Array;
   type Integer_Matrix3 is array (Positive range <>)
      of Integer_Matrix (1 .. PUZZLE_SIZE);
   type Pose is record
      Row : Integer;
      Col : Integer;
      Dir : Direction;
   end record;
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

   File_Name    : constant String := "input.txt";
   File         : File_Type;
   Line         : String (1 .. PUZZLE_SIZE);
   Line_Last    : Natural;
   Puzzle       : String_List (1 .. PUZZLE_SIZE);
   Cost_Map     : Integer_Matrix3 (1 .. PUZZLE_SIZE);
   Row_Len      : Natural := 0;
   Col_Len      : Natural := 0;
   Start_Idx    : Natural := 0;
   Reindeer     : Pose;
   Min_Cost     : Natural := Natural'Last;
   Good_Seat    : Boolean;
   Seats        : Location_Sets.Set (2500);

   procedure Clear_Cost_Map is
   begin
      for I in 1 .. Row_Len loop
         for J in 1 .. Col_Len loop
            for D in Direction loop
               Cost_Map (I) (J) (D) := Natural'Last;
            end loop;
         end loop;
      end loop;
   end Clear_Cost_Map;

   --  full depth first search
   function FindMinPath (P : Pose; Cost : Natural) return Boolean is
      R : Pose;
      Good : Boolean := False;
   begin
      --  Put_Line ("At (" & P.Row'Image & ", " & P.Col'Image
      --     & ", " & P.Dir'Image & ")");
      --  base case: already has lower cost
      if Cost_Map (P.Row) (P.Col) (P.Dir) < Cost then
         return False;
      end if;

      --  base case: reached end, update min cost
      if Puzzle (P.Row) (P.Col) = 'E' then
         --  Put_Line ("### FOUND IT ###");
         if Cost < Min_Cost then
            Min_Cost := Cost;
         end if;
         --  for part B, Min_Cost already set, check if this path matches
         if Cost = Min_Cost then
            Seats.Include ((Row => P.Row, Col => P.Col));
            return True;
         end if;
         return False;
      end if;

      --  base case: hit obstacle
      if Puzzle (P.Row) (P.Col) = '#' then
         return False;
      end if;

      --  mark cost along this path
      Cost_Map (P.Row) (P.Col) (P.Dir) := Cost;

      --  recurse each direction (North)
      R := P;
      R.Row := R.Row - 1;
      R.Dir := North;
      case P.Dir is
         when North =>
            Good := FindMinPath (R, Cost + 1) or else Good;
         when East =>
            Good := FindMinPath (R, Cost + 1001) or else Good;
         when South =>
            null;  --  don't go backwards
         when West =>
            Good := FindMinPath (R, Cost + 1001) or else Good;
      end case;

      --  recurse each direction (East)
      R := P;
      R.Col := R.Col + 1;
      R.Dir := East;
      case P.Dir is
         when North =>
            Good := FindMinPath (R, Cost + 1001) or else Good;
         when East =>
            Good := FindMinPath (R, Cost + 1) or else Good;
         when South =>
            Good := FindMinPath (R, Cost + 1001) or else Good;
         when West =>
            null;  --  don't go backwards
      end case;

      --  recurse each direction (South)
      R := P;
      R.Row := R.Row + 1;
      R.Dir := South;
      case P.Dir is
         when North =>
            null;  --  don't go backwards
         when East =>
            Good := FindMinPath (R, Cost + 1001) or else Good;
         when South =>
            Good := FindMinPath (R, Cost + 1) or else Good;
         when West =>
            Good := FindMinPath (R, Cost + 1001) or else Good;
      end case;

      --  recurse each direction (West)
      R := P;
      R.Col := R.Col - 1;
      R.Dir := West;
      case P.Dir is
         when North =>
            Good := FindMinPath (R, Cost + 1001) or else Good;
         when East =>
            null;  --  don't go backwards
         when South =>
            Good := FindMinPath (R, Cost + 1001) or else Good;
         when West =>
            Good := FindMinPath (R, Cost + 1) or else Good;
      end case;

      if Good then
         Seats.Include ((Row => P.Row, Col => P.Col));
      end if;
      return Good;
   end FindMinPath;

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

      Start_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => "S", From => 1);
      if Start_Idx /= 0 then
         Reindeer.Row := Row_Len;
         Reindeer.Col := Start_Idx;
         Reindeer.Dir := East;
      end if;
   end loop;

   --  Close the file
   Close (File);

   Seats.Clear;
   Clear_Cost_Map;
   Good_Seat := FindMinPath (Reindeer, 0);
   Put_Line ("Part A: " & Min_Cost'Image);

   --  run again, notice that this time min_cost remains as from part A
   Seats.Clear;
   Clear_Cost_Map;
   Good_Seat := FindMinPath (Reindeer, 0);
   Put_Line ("Part B: " & Seats.Length'Image);

end day16;
