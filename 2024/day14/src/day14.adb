with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure day14 with SPARK_Mode is
   type UInt is mod 2**64;
   type UInt_Array is array (Positive range <>) of UInt;
   type Natural_Array is array (Positive range <>) of Natural;

   --  File_Name    : constant String := "example.txt";
   --  Row_Len      : constant Natural := 7;
   --  Col_Len      : constant Natural := 11;
   File_Name    : constant String := "input.txt";
   Row_Len      : constant Natural := 103;
   Col_Len      : constant Natural := 101;
   Sim_Time     : constant Natural := 100;
   type String_List is array (Positive range <>) of String (1 .. Col_Len);

   type Location is record
      Row : Integer;
      Col : Integer;
   end record;
   type Velocity is record
      DR : Integer;
      DC : Integer;
   end record;
   type Robot is record
      Pos : Location;
      Vel : Velocity;
   end record;
   type Robot_Array is array (Positive range <>) of Robot;

   File         : File_Type;
   Line         : String (1 .. 256);
   Line_Last    : Natural;
   Start_Idx    : Natural;
   Comma_Idx    : Natural;
   Space_Idx    : Natural;
   Robots       : Robot_Array (1 .. 500);
   N            : Natural := 0;  --  number of robots

   End_Row      : Integer;
   End_Col      : Integer;
   Quadrant     : Natural_Array (1 .. 4) := (others => 0);
   Safe_Factor  : Natural;

   Display      : String_List (1 .. Row_Len);
   Cycle_Len    : UInt_Array (1 .. 500) := (others => 0);
   Cycle_Lcm    : UInt;

   procedure Clear_Display is
   begin
      for R in 1 .. Row_Len loop
         Display (R) := (others => ' ');
      end loop;
   end Clear_Display;

   procedure Show_Display is
   begin
      for R in 1 .. Row_Len loop
         Put_Line (Display (R));
      end loop;
      New_Line;
   end Show_Display;

   procedure Set_Display is
   begin
      for R in 1 .. N loop
         Display (Robots (R).Pos.Row + 1) (Robots (R).Pos.Col + 1) := '*';
      end loop;
   end Set_Display;

   function Find_Cluster return Boolean is
      --  look for 3x3 cluster (radius 1)
      Radius  : constant Natural := 1;
      Cluster : Boolean;
   begin
      for R in Radius + 1 .. Row_Len - Radius loop
         for C in Radius + 1 .. Col_Len - Radius loop
            if Display (R) (C) = '*' then
               Cluster := True;
               for DR in -Radius .. Radius loop
                  for DC in -Radius .. Radius loop
                     if Display (R + DR) (C + DC) /= '*' then
                        Cluster := False;
                        exit;
                     end if;
                  end loop;
                  exit when Cluster = False;
               end loop;
               if Cluster then
                  return True;
               end if;
            end if;
         end loop;
      end loop;
      return False;
   end Find_Cluster;

   function Compute_Cycle_Len (R : Natural) return UInt is
      T : UInt := 1;
   begin
      End_Row := (Robots (R).Pos.Row + Robots (R).Vel.DR) mod Row_Len;
      End_Col := (Robots (R).Pos.Col + Robots (R).Vel.DC) mod Col_Len;
      while End_Row /= Robots (R).Pos.Row or else End_Col /= Robots (R).Pos.Col loop
         T := T + 1;
         End_Row := (End_Row + Robots (R).Vel.DR) mod Row_Len;
         End_Col := (End_Col + Robots (R).Vel.DC) mod Col_Len;
      end loop;
      return T;
   end Compute_Cycle_Len;

   function LCM (A, B : UInt) return UInt is
      function GCD (A0, B0 : UInt) return UInt is
         A : UInt := A0;
         B : UInt := B0;
         T : UInt;
      begin
         while B /= 0 loop
            T := B;
            B := A mod B;
            A := T;
         end loop;
         return A;
      end GCD;
   begin
      return (A * B) / GCD (A, B);
   end LCM;

begin
   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) loop
      Get_Line (File, Line, Line_Last);

      --  each line is a new robot
      N := N + 1;

      --  extract the starting position as: 'p=col,row '
      Start_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => "p=", From => 1);
      Comma_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => ",", From => Start_Idx);
      Space_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => " ", From => Start_Idx);
      Robots (N).Pos.Col := Integer'Value (Line (Start_Idx + 2 .. Comma_Idx - 1));
      Robots (N).Pos.Row := Integer'Value (Line (Comma_Idx + 1 .. Space_Idx - 1));

      --  extract the velocity as 'v=dc,dr'
      Start_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => "v=", From => 1);
      Comma_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => ",", From => Start_Idx);
      Robots (N).Vel.DC := Integer'Value (Line (Start_Idx + 2 .. Comma_Idx - 1));
      Robots (N).Vel.DR := Integer'Value (Line (Comma_Idx + 1 .. Line_Last));
   end loop;

   --  Close the file
   Close (File);

   --  Part A
   for R in 1 .. N loop
      End_Row := (Robots (R).Pos.Row + Sim_Time * Robots (R).Vel.DR) mod Row_Len;
      End_Col := (Robots (R).Pos.Col + Sim_Time * Robots (R).Vel.DC) mod Col_Len;

      --  Quadrant 1
      if End_Col < Col_Len / 2 and then End_Row < Row_Len / 2 then
         Quadrant (1) := Quadrant (1) + 1;
      end if;
      --  Quadrant 2
      if End_Col > Col_Len / 2 and then End_Row < Row_Len / 2 then
         Quadrant (2) := Quadrant (2) + 1;
      end if;
      --  Quadrant 3
      if End_Col < Col_Len / 2 and then End_Row > Row_Len / 2 then
         Quadrant (3) := Quadrant (3) + 1;
      end if;
      --  Quadrant 4
      if End_Col > Col_Len / 2 and then End_Row > Row_Len / 2 then
         Quadrant (4) := Quadrant (4) + 1;
      end if;
   end loop;

   Safe_Factor := Quadrant (1) * Quadrant (2) * Quadrant (3) * Quadrant (4);
   Put_Line ("Part A: " & Safe_Factor'Image);

   --  Part B
   for R in 1 .. N loop
      Cycle_Len (R) := Compute_Cycle_Len (R);
   end loop;
   Cycle_Lcm := Cycle_Len (1);
   for R in 2 .. N loop
      Cycle_Lcm := LCM (Cycle_Lcm, Cycle_Len (R));
   end loop;
   --  Note: every cycle for every robot is 10403
   --  Therefore, the LCM is also 10403
   --  Also note 10403 = 101 * 103 which is
   --  Col_Len * Row_Len, the total number of
   --  positions on the map

   --  The whole pattern repeats every 10403 steps, so
   --  then answer must be smaller than that
   for DT in 1 .. Cycle_Lcm loop
      for R in 1 .. N loop
         Robots (R).Pos.Row := (Robots (R).Pos.Row + Robots (R).Vel.DR) mod Row_Len;
         Robots (R).Pos.Col := (Robots (R).Pos.Col + Robots (R).Vel.DC) mod Col_Len;
      end loop;
      Clear_Display;
      Set_Display;
      if Find_Cluster then
         Show_Display;
         Put_Line ("T: " & DT'Image);
         Get_Line (Line, Line_Last);
      end if;
   end loop;

end day14;
