with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Bounded_Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

procedure day18 is

   File_Name     : constant String := "input.txt";
   PUZZLE_SIZE   : constant Natural := 71;
   Block_Limit   : constant Natural := 1024;

   type Location is record
      Row : Integer;
      Col : Integer;
   end record;

   package Location_Vec is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Location);
   use Location_Vec;

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

   package Location_Sets is new Ada.Containers.Bounded_Ordered_Sets
     (Element_Type => Location,
      "<" => Location_Less,
      "=" => "=");

   function Location_Hash (Key : Location) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key.Row * PUZZLE_SIZE + Key.Col);
   end Location_Hash;

   package Location_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Location,
      Hash => Location_Hash,
      Equivalent_Keys => "=",
      Element_Type => Location);

   package Cost_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Location,
      Hash => Location_Hash,
      Equivalent_Keys => "=",
      Element_Type => Integer);

   type String_List is array (Positive range <>) of String (1 .. PUZZLE_SIZE);
   type Integer_Array is array (Positive range <>) of Integer;
   type Integer_Matrix is array (Positive range <>)
      of Integer_Array (1 .. PUZZLE_SIZE);

   File          : File_Type;
   Line          : String (1 .. 1024);
   Line_Last     : Natural;
   Comma_Idx     : Natural;
   Num_Last      : Natural;

   Puzzle        : String_List (1 .. PUZZLE_SIZE);
   Blocks_First  : Location_Vec.Vector := Location_Vec.Empty_Vector;
   Blocks_Rem    : Location_Vec.Vector := Location_Vec.Empty_Vector;
   Block_Count   : Natural := 0;
   Loc           : Location;

   --  state for depth first search
   Cost_Map      : Integer_Matrix (1 .. PUZZLE_SIZE);
   Min_Cost      : Integer := Integer'Last;
   Path          : Location_Vec.Vector := Location_Vec.Empty_Vector;
   Found_Path    : Boolean;

   procedure Clear_Puzzle is
   begin
      for R in 1 .. PUZZLE_SIZE loop
         for C in 1 .. PUZZLE_SIZE loop
            Puzzle (R) (C) := '.';
            Cost_Map (R) (C) := Integer'Last;
         end loop;
      end loop;
   end Clear_Puzzle;

   procedure Show_Puzzle (P : Location_Vec.Vector) is
   begin
      for I in P.First_Index .. P.Last_Index loop
         Puzzle (P.Element (I).Row) (P.Element (I).Col) := 'O';
      end loop;
      for R in 1 .. PUZZLE_SIZE loop
         Put_Line (Puzzle (R));
      end loop;
      New_Line;
   end Show_Puzzle;

   --  full depth first search TOO SLOW
   function DepthFirstSearch (L : Location;
                         Cost : Integer;
                         P : in out Location_Vec.Vector)
      return Boolean is
      Good : Boolean := False;
   begin
      --  base case: off map
      if L.Row < 1 or else L.Row > PUZZLE_SIZE or else
         L.Col < 1 or else L.Col > PUZZLE_SIZE
      then
         return False;
      end if;

      --  base case: hit obstacle
      if Puzzle (L.Row) (L.Col) = '#' then
         return False;
      end if;

      --  base case: already has lower cost
      if Cost_Map (L.Row) (L.Col) < Cost then
         return False;
      end if;

      --  base case: reached end, update min cost
      if L.Row = PUZZLE_SIZE and then L.Col = PUZZLE_SIZE then
         if Cost < Min_Cost then
            Min_Cost := Cost;
            --  clear the path and recurse out from the exit
            P.Clear;
            P.Append (L);
            return True;
         end if;
         return False;
      end if;

      --  mark cost along this path
      Cost_Map (L.Row) (L.Col) := Cost;

      --  recurse each direction
      Good := DepthFirstSearch ((Row => L.Row - 1, Col => L.Col), Cost + 1, P)
         or else Good;
      Good := DepthFirstSearch ((Row => L.Row, Col => L.Col + 1), Cost + 1, P)
         or else Good;
      Good := DepthFirstSearch ((Row => L.Row + 1, Col => L.Col), Cost + 1, P)
         or else Good;
      Good := DepthFirstSearch ((Row => L.Row, Col => L.Col - 1), Cost + 1, P)
         or else Good;

      if Good then
         --  note: path is in reverse order
         P.Append (L);
      end if;
      return Good;
   end DepthFirstSearch;

   --  following pseudo-code from wikipedia
   --  https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode
   function H (Loc : Location) return Integer is
   begin
      --  Manhattan distance to bottom corner
      return (PUZZLE_SIZE - Loc.Row) + (PUZZLE_SIZE - Loc.Col);
   end H;

   function Reconstruct_Path (Came_From : Location_Maps.Map;
         Current : Location) return Location_Vec.Vector is
      Total_Path : Location_Vec.Vector := Location_Vec.Empty_Vector;
      Loc        : Location := Current;
   begin
      --  keeping reverse order of path to match DFS
      Total_Path.Append (Loc);
      while Came_From.Contains (Loc) loop
         Loc := Came_From.Element (Loc);
         Total_Path.Append (Loc);
      end loop;
      return Total_Path;
   end Reconstruct_Path;

   function A_Star (Start_Loc : Location) return Location_Vec.Vector is
      Open_Set : Location_Sets.Set (5042); --  note: 71*71 + 1
      Neighbors : Location_Sets.Set (4);
      Came_From : Location_Maps.Map;
      G_Score : Cost_Maps.Map;
      F_Score : Cost_Maps.Map;
      CursorS : Location_Sets.Cursor;
      Min_Loc : Location;
      Min_F   : Integer;
      Loc     : Location;
      Score   : Integer;
   begin
      --  initialize score maps to inifity
      for I in 1 .. PUZZLE_SIZE loop
         for J in 1 .. PUZZLE_SIZE loop
            Loc.Row := I;
            Loc.Col := J;
            G_Score.Include (Loc, Integer'Last);
            F_Score.Include (Loc, Integer'Last);
         end loop;
      end loop;
      --  initialize open set to just the starting location
      Open_Set.Include (Start_Loc);
      --  set g_score to zero at start and f_score to heuristic
      G_Score.Include (Start_Loc, 0);
      F_Score.Include (Start_Loc, H (Start_Loc));

      while not Open_Set.Is_Empty loop
         --  find element of set that has smallest F score
         CursorS := Open_Set.First;
         Min_Loc := Open_Set.Element (CursorS);
         Min_F := F_Score.Element (Min_Loc);
         for E of Open_Set loop
            Score := F_Score.Element (E);
            if Score < Min_F then
               Min_F := Score;
               Min_Loc := E;
            end if;
         end loop;

         --  if reached the goal point, return
         if Min_Loc.Row = PUZZLE_SIZE and then Min_Loc.Col = PUZZLE_SIZE then
            return Reconstruct_Path (Came_From, Min_Loc);
         end if;

         --  remove from the open set
         Open_Set.Exclude (Min_Loc);

         --  collect neighbors of Min_Loc
         Neighbors.Clear;
         --  north
         Loc.Row := Min_Loc.Row - 1; Loc.Col := Min_Loc.Col;
         if Loc.Row > 0 and then Puzzle (Loc.Row) (Loc.Col) /= '#' then
            Neighbors.Include (Loc);
         end if;
         --  east
         Loc.Row := Min_Loc.Row; Loc.Col := Min_Loc.Col + 1;
         if Loc.Col <= PUZZLE_SIZE and then Puzzle (Loc.Row) (Loc.Col) /= '#' then
            Neighbors.Include (Loc);
         end if;
         --  south
         Loc.Row := Min_Loc.Row + 1; Loc.Col := Min_Loc.Col;
         if Loc.Row <= PUZZLE_SIZE and then Puzzle (Loc.Row) (Loc.Col) /= '#' then
            Neighbors.Include (Loc);
         end if;
         --  west
         Loc.Row := Min_Loc.Row; Loc.Col := Min_Loc.Col - 1;
         if Loc.Col > 0 and then Puzzle (Loc.Row) (Loc.Col) /= '#' then
            Neighbors.Include (Loc);
         end if;

         for E of Neighbors loop
            Score := G_Score.Element (Min_Loc) + 1;
            if Score < G_Score.Element (E) then
               Came_From.Include (E, Min_Loc);
               G_Score.Include (E, Score);
               F_Score.Include (E, Score + H (Loc));
               Open_Set.Include (E);
            end if;
         end loop;
      end loop;
      --  error condition, no path
      return Location_Vec.Empty_Vector;
   end A_Star;

begin
   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) loop
      Get_Line (File, Line, Line_Last);
      Comma_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => ",", From => 1);

      Get (Line (1 .. Comma_Idx - 1), Loc.Col, Num_Last);
      Get (Line (Comma_Idx + 1 .. Line_Last), Loc.Row, Num_Last);
      if Block_Count < Block_Limit then
         Block_Count := Block_Count + 1;
         Blocks_First.Append (Loc);
      else
         Blocks_Rem.Append (Loc);
      end if;
   end loop;

   --  Close the file
   Close (File);

   --  file the puzzle with the corrupted blocks
   Clear_Puzzle;
   for B of Blocks_First loop
      --  switch to 1-based indexing
      Puzzle (B.Row + 1) (B.Col + 1) := '#';
   end loop;
   Show_Puzzle (Path);

   --  --  solve with dfs
   --  Loc.Row := 1;
   --  Loc.Col := 1;
   --  Found_Path := DepthFirstSearch (Loc, 0, Path);
   --  Show_Puzzle (Path);
   --  if Found_Path then
   --     Put_Line (Min_Cost'Image);
   --  else
   --     Put_Line ("No path found");
   --  end if;

   --  solve with a-star
   Path := A_Star ((1, 1));
   Show_Puzzle (Path);
   Min_Cost := Integer (Path.Length) - 1;
   Put_Line (Min_Cost'Image);

   --  Part B: continue adding the remaining blocks until no path found
   --  check to see if the block is on the current shortest path
   for B of Blocks_Rem loop
      --  if this is just a repeat of a block already on the map, skip
      if Puzzle (B.Row + 1) (B.Col + 1) /= '#' then
         --  add the new block to the map
         Puzzle (B.Row + 1) (B.Col + 1) := '#';
         Loc.Row := B.Row + 1;
         Loc.Col := B.Col + 1;
         --  check to see if the new block was on shortest path
         --  if so, recompute shortest path
         if Path.Contains (Loc) then
            Path := A_Star ((1, 1));
            --  if the path was empty, then no path available, return
            if Path.Is_Empty then
               Put_Line (B.Col'Image & ", " & B.Row'Image);
               exit;
            end if;
         end if;
      end if;
   end loop;

end day18;
