with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure day06 with SPARK_Mode is
   type Direction is (North, East, South, West);
   type Location is record
      Row : Natural;
      Col : Natural;
   end record;
   type Pose is record
      Loc : Location;
      Dir : Direction;
   end record;
   type Location_Array is array (Positive range <>) of Location;
   type Pose_Array is array (Positive range <>) of Pose;
   type String_List is array (Positive range <>) of String (1 .. 131);
   File_Name    : constant String := "input.txt";
   File         : File_Type;
   Line         : String (1 .. 131);
   Line_Last    : Natural;
   Puzzle       : String_List (1 .. 131);
   Row_Len      : Natural := 0;
   Col_Len      : Natural := 0;
   Start_Idx    : Natural;
   Start_Pose   : Pose;
   --  maximum visit length is all locations
   --  so 130 * 130 = 16900
   Visited      : Location_Array (1 .. 16900);
   Visited_Len  : Natural := 0;
   --  maximum trace length is pointing in all 4 directions from each location
   --  so 4 * 130 * 130 = 67600
   Trace        : Pose_Array (1 .. 67600);
   Trace_Len    : Natural := 0;
   Num_Loops    : Natural := 0;

   function Index_Of (A : Location_Array; Value : Location) return Natural is
   begin
      for I in A'Range loop
         if A (I) = Value then
            return I;
         end if;
      end loop;
      return 0;
   end Index_Of;

   function Index_Of (A : Pose_Array; Value : Pose) return Natural is
   begin
      for I in A'Range loop
         if A (I) = Value then
            return I;
         end if;
      end loop;
      return 0;
   end Index_Of;

   function Is_On_Map (R, C : Natural) return Boolean is
   begin
      if R > 0 and then C > 0 and then R <= Row_Len and then C <= Col_Len then
         return True;
      end if;
      return False;
   end Is_On_Map;

   function Is_On_Map (L : Location) return Boolean is
   begin
      return Is_On_Map (L.Row, L.Col);
   end Is_On_Map;

   function Is_On_Map (P : Pose) return Boolean is
   begin
      return Is_On_Map (P.Loc.Row, P.Loc.Col);
   end Is_On_Map;

   function Is_Facing_Obstacle (P : Pose) return Boolean is
   begin
      case P.Dir is
         when North =>
            if Is_On_Map (P.Loc.Row - 1, P.Loc.Col) and then
               Puzzle (P.Loc.Row - 1) (P.Loc.Col) = '#'
            then
               return True;
            end if;
         when East =>
            if Is_On_Map (P.Loc.Row, P.Loc.Col + 1) and then
               Puzzle (P.Loc.Row) (P.Loc.Col + 1) = '#'
            then
               return True;
            end if;
         when South =>
            if Is_On_Map (P.Loc.Row + 1, P.Loc.Col) and then
               Puzzle (P.Loc.Row + 1) (P.Loc.Col) = '#'
            then
               return True;
            end if;
         when West =>
            if Is_On_Map (P.Loc.Row, P.Loc.Col - 1) and then
               Puzzle (P.Loc.Row) (P.Loc.Col - 1) = '#'
            then
               return True;
            end if;
      end case;
      return False;
   end Is_Facing_Obstacle;

   function Walk_Map (P0 : Pose) return Boolean is
      P : Pose := P0;
   begin
      --  initialize
      Visited_Len := 0;
      Trace_Len := 0;
      --  loop until walked of the map or returned to a pose
      while Is_On_Map (P.Loc) loop
         --  update visited list if not already visited
         if Visited_Len = 0 or else
            Index_Of (Visited (1 .. Visited_Len), P.Loc) = 0
         then
            Visited_Len := Visited_Len + 1;
            Visited (Visited_Len) := P.Loc;
         end if;
         --  update trace list, checking for returning to a pose from earlier
         if Trace_Len = 0 or else
            Index_Of (Trace (1 .. Trace_Len), P) = 0
         then
            Trace_Len := Trace_Len + 1;
            Trace (Trace_Len) := P;
         else
            return True;
         end if;

         if Is_Facing_Obstacle (P) then
            --  change direction if facing an obstacle
            case P.Dir is
               when North =>
                  P.Dir := East;
               when East =>
                  P.Dir := South;
               when South =>
                  P.Dir := West;
               when West =>
                  P.Dir := North;
            end case;
         else
            --  step in the direction facing
            case P.Dir is
               when North =>
                  P.Loc.Row := P.Loc.Row - 1;
               when East =>
                  P.Loc.Col := P.Loc.Col + 1;
               when South =>
                  P.Loc.Row := P.Loc.Row + 1;
               when West =>
                  P.Loc.Col := P.Loc.Col - 1;
            end case;
         end if;
      end loop;
      return False;
   end Walk_Map;

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
         Pattern => "^", From => 1);
      if Start_Idx /= 0 then
         Start_Pose.Dir := North;
         Start_Pose.Loc.Row := Row_Len;
         Start_Pose.Loc.Col := Start_Idx;
      end if;
   end loop;

   --  Close the file
   Close (File);

   --  Part A: count locations visited
   if not Walk_Map (Start_Pose) then
      Put_Line ("Part A: " & Natural'Image (Visited_Len));
   end if;

   --  Part B: count obstacles placed to get loop
   --  This is pure brute force, could likely get it much
   --  faster if we only placed obstacles on locations
   --  visited in the original walk
   for I in 1 .. Row_Len loop
      -- Put_Line ("Working row: " & Natural'Image (I));
      for J in 1 .. Col_Len loop
         if Puzzle (I) (J) = '.' then
            Puzzle (I) (J) := '#';
            if Walk_Map (Start_Pose) then
               Num_Loops := Num_Loops + 1;
            end if;
            Puzzle (I) (J) := '.';
         end if;
      end loop;
   end loop;
   Put_Line ("Part B: " & Natural'Image (Num_Loops));

end day06;
