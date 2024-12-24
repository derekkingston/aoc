with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Bounded_Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Calendar; use Ada.Calendar;

procedure day20 is

   File_Name     : constant String := "input.txt";
   MAX_SIZE      : constant Natural := 142;

   type String_List is array (Positive range <>) of String (1 .. MAX_SIZE);
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
      return Ada.Containers.Hash_Type (Key.Row * MAX_SIZE + Key.Col);
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

   File          : File_Type;
   Line          : String (1 .. MAX_SIZE);
   Line_Last     : Natural;
   Puzzle        : String_List (1 .. MAX_SIZE);
   G_Score       : Cost_Maps.Map;
   Row_Len       : Natural := 0;
   Col_Len       : Natural := 0;
   Start_Idx     : Natural;
   Goal_Idx      : Natural;
   Start_Loc     : Location;
   Goal          : Location;
   Path          : Location_Vec.Vector := Location_Vec.Empty_Vector;
   Start_Time    : Time;
   End_Time      : Time;
   Elapsed_Time  : Duration;
   LocA          : Location;
   LocB          : Location;
   Count         : Natural := 0;
   Cheat         : array (1 .. 20) of Natural := (others => 0);
   Loc_Dist      : Integer;
   Savings       : Integer;

   procedure Show_Puzzle (P : Location_Vec.Vector) is
   begin
      for I in P.First_Index .. P.Last_Index loop
         Puzzle (P.Element (I).Row) (P.Element (I).Col) := 'O';
      end loop;
      for R in 1 .. Row_Len loop
         Put_Line (Puzzle (R) (1 .. Col_Len));
      end loop;
      New_Line;
   end Show_Puzzle;

   --  following pseudo-code from wikipedia
   --  https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode
   function H (Loc, Goal : Location) return Integer is
   begin
      --  Manhattan distance to goal
      return abs (Goal.Row - Loc.Row) + abs (Goal.Col - Loc.Col);
   end H;

   function Reconstruct_Path (Came_From : Location_Maps.Map;
         Current : Location) return Location_Vec.Vector is
      Total_Path : Location_Vec.Vector := Location_Vec.Empty_Vector;
      Loc        : Location := Current;
   begin
      Total_Path.Append (Loc);
      while Came_From.Contains (Loc) loop
         Loc := Came_From.Element (Loc);
         Total_Path.Prepend (Loc);
      end loop;
      return Total_Path;
   end Reconstruct_Path;

   function A_Star (Start_Loc, Goal : Location;
         G_Score : in out Cost_Maps.Map) return Location_Vec.Vector is
      Open_Set : Location_Sets.Set (19882); --  note: 141*141 + 1
      Neighbors : Location_Sets.Set (4);
      Came_From : Location_Maps.Map;
      F_Score : Cost_Maps.Map;
      Min_Loc : Location;
      Min_F   : Integer;
      Loc     : Location;
      Score   : Integer;
   begin
      --  initialize score maps to inifity
      for I in 1 .. Row_Len loop
         for J in 1 .. Col_Len loop
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
      F_Score.Include (Start_Loc, H (Start_Loc, Goal));

      while not Open_Set.Is_Empty loop
         --  find element of set that has smallest F score
         Min_F := Integer'Last;
         for E of Open_Set loop
            Score := F_Score.Element (E);
            if Score < Min_F then
               Min_F := Score;
               Min_Loc := E;
            end if;
         end loop;

         --  if reached the goal point, return
         if Min_Loc.Row = Goal.Row and then Min_Loc.Col = Goal.Col then
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
         if Loc.Col <= Col_Len and then Puzzle (Loc.Row) (Loc.Col) /= '#' then
            Neighbors.Include (Loc);
         end if;
         --  south
         Loc.Row := Min_Loc.Row + 1; Loc.Col := Min_Loc.Col;
         if Loc.Row <= Row_Len and then Puzzle (Loc.Row) (Loc.Col) /= '#' then
            Neighbors.Include (Loc);
         end if;
         --  west
         Loc.Row := Min_Loc.Row; Loc.Col := Min_Loc.Col - 1;
         if Loc.Col > 0 and then Puzzle (Loc.Row) (Loc.Col) /= '#' then
            Neighbors.Include (Loc);
         end if;

         for E of Neighbors loop
            --  tentative score is g-score + [weight of edge], here just 1
            Score := G_Score.Element (Min_Loc) + 1;
            if Score < G_Score.Element (E) then
               Came_From.Include (E, Min_Loc);
               G_Score.Include (E, Score);
               F_Score.Include (E, Score + H (Loc, Goal));
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
      if Col_Len = 0 then
         Col_Len := Line_Last;
      end if;
      if Col_Len /= Line_Last then
         Put_Line ("All rows must be of the same length");
         exit;
      end if;
      Row_Len := Row_Len + 1;
      Puzzle (Row_Len) := Line;

      --  read starting location
      Start_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => "S", From => 1);
      if Start_Idx /= 0 then
         Start_Loc.Row := Row_Len;
         Start_Loc.Col := Start_Idx;
      end if;

      --  read goal location
      Goal_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => "E", From => 1);
      if Goal_Idx /= 0 then
         Goal.Row := Row_Len;
         Goal.Col := Goal_Idx;
      end if;
   end loop;

   --  Close the file
   Close (File);

   --  solve with a-star
   --  Show_Puzzle (Path);
   Start_Time := Clock;
   Path := A_Star (Start_Loc, Goal, G_Score);
   End_Time := Clock;
   Elapsed_Time := End_Time - Start_Time;
   --  Show_Puzzle (Path);
   Put_Line ("A-star run time (sec): " & Elapsed_Time'Image);

   --  note: after analyzing the input file - there are no choices, the
   --  shortest path is, continuously, one long path from start to end
   --  therefore, removing an obstacle results in a time savings of
   --  the difference between the points on the path that would be
   --  joined by removing that obstacle
   for R in 2 .. Row_Len - 1 loop
      for C in 2 .. Col_Len - 1 loop
         if Puzzle (R) (C) = '#' then
            --  check that can be joined North-South by removal
            if Puzzle (R - 1) (C) /= '#' and then
               Puzzle (R + 1) (C) /= '#'
            then
               LocA.Row := R - 1; LocA.Col := C;
               LocB.Row := R + 1; LocB.Col := C;
               if abs (G_Score.Element (LocA) - G_Score.Element (LocB)) > 100
               then
                  Count := Count + 1;
               end if;
            end if;
            --  check that can be joined East-West by removal
            if Puzzle (R) (C - 1) /= '#' and then
               Puzzle (R) (C + 1) /= '#'
            then
               LocA.Row := R; LocA.Col := C - 1;
               LocB.Row := R; LocB.Col := C + 1;
               if abs (G_Score.Element (LocA) - G_Score.Element (LocB)) > 100
               then
                  Count := Count + 1;
               end if;
            end if;
         end if;
      end loop;
   end loop;
   Put_Line ("Part A: " & Count'Image);

   --  Part B: look for start/end positions on the path; check distance less
   --  than the 20 seconds allowed; then compute if cheat saves 100
   for I in Path.First_Index .. Path.Last_Index loop
      --  note: the destination must be at least 100 steps further up the path
      for J in I + 100 .. Path.Last_Index loop
         LocA := Path.Element (I);
         LocB := Path.Element (J);
         Loc_Dist := H (LocA, LocB);
         Savings := abs (G_Score.Element (LocA) - G_Score.Element (LocB));
         if Loc_Dist <= 20 and then Savings >= 100 + Loc_Dist then
            Cheat (Loc_Dist) := Cheat (Loc_Dist) + 1;
         end if;
      end loop;
   end loop;

   Count := 0;
   for I in 1 .. 20 loop
      Put_Line (I'Image & ": " & Cheat (I)'Image);
      Count := Count + Cheat (I);
   end loop;
   Put_Line ("Part B: " & Count'Image);

end day20;
