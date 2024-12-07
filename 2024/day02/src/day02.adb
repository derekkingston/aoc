with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure day02 with SPARK_Mode is
   type Natural_Array is array (Natural range <>) of Natural;
   type Integer_Array is array (Natural range <>) of Integer;
   type Integer_Matrix is array (Natural range <>)
      of Integer_Array (1 .. 1000);

   File_Name    : constant String := "input.txt";
   File         : File_Type;
   Line         : String (1 .. 256);
   Line_Last    : Natural;
   Space        : Natural;
   Space_Prev   : Natural;
   Num_Last     : Natural;
   Reports      : Integer_Matrix (1 .. 1000);
   Level_Len    : Natural_Array (1 .. 1000);
   Report_Len   : Natural := 0;
   Safe_Reports : Natural := 0;
   Read_Num     : Integer;
   Working_Idx  : Natural := 0;
   Is_Safe      : Boolean;

   function Is_Safe_Report (A : Integer_Array)
      return Boolean is
      Ascending : Boolean := False;
      Step_Size : Integer;
   begin
      if A'Length < 2 then
         --  Assume a report of a single level is by definition safe
         Put_Line ("Found a report with only 0/1 levels");
         return True;
      end if;
      if A (A'First) = A (A'First + 1) then
         --  Put_Line ("No direction");
         return False;
      end if;
      if A (A'First) < A (A'First + 1) then
         Ascending := True;
      end if;
      for I in A'First + 1 .. A'Last loop
         Step_Size := A (I) - A (I - 1);
         if Ascending and then Step_Size <= 0 then
            --  Put_Line ("Ascending, but went down");
            return False;
         end if;
         if not Ascending and then Step_Size >= 0 then
            --  Put_Line ("Descending, but went up");
            return False;
         end if;
         if abs (Step_Size) > 3 then
            --  Put_Line ("Large step size");
            return False;
         end if;
      end loop;
      --  Put_Line ("Found safe report");
      return True;
   end Is_Safe_Report;

begin
   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) and then Report_Len < Reports'Length loop
      Get_Line (File, Line, Line_Last);

      --  Ensure at least one space on a line, otherwise blank and we exit
      Space := Index (Source => Line (1 .. Line_Last), Pattern => " ",
         From => 1);
      exit when Space = 0;

      --  Read each report as a list of space separated integers
      Working_Idx := 1;
      Report_Len := Report_Len + 1;
      Level_Len (Report_Len) := 0;
      while Working_Idx < Space loop
         Space_Prev := Space;
         Level_Len (Report_Len) := Level_Len (Report_Len) + 1;
         Get (Line (Working_Idx .. Space - 1), Read_Num, Num_Last);
         Reports (Report_Len) (Level_Len (Report_Len)) := Read_Num;
         Working_Idx := Space_Prev + 1;
         Space := Index (Source => Line (Working_Idx .. Line_Last),
            Pattern => " ", From => Working_Idx);

         if Space = 0 then
            --  Read the final value of the line
            Level_Len (Report_Len) := Level_Len (Report_Len) + 1;
            Get (Line (Working_Idx .. Line_Last), Read_Num, Num_Last);
            Reports (Report_Len) (Level_Len (Report_Len)) := Read_Num;
            exit;
         end if;
      end loop;
   end loop;

   --  Close the file
   Close (File);

   --  Part A: count reports that are 'safe'
   Safe_Reports := 0;
   for I in 1 .. Report_Len loop
      Is_Safe := Is_Safe_Report (Reports (I) (1 .. Level_Len (I)));
      if Is_Safe then
         Safe_Reports := Safe_Reports + 1;
      end if;
   end loop;
   Put_Line ("Part A: " & Integer'Image (Safe_Reports));

   --  Part B: eliminate one of the values and check for 'safe'
   Safe_Reports := 0;
   for I in 1 .. Report_Len loop
      Is_Safe := Is_Safe_Report (Reports (I) (1 .. Level_Len (I)));
      Is_Safe := Is_Safe or Is_Safe_Report
         (Reports (I) (2 .. Level_Len (I)));
      Is_Safe := Is_Safe or Is_Safe_Report
         (Reports (I) (1 .. Level_Len (I) - 1));
      for J in 2 .. Level_Len (I) - 1 loop
         exit when Is_Safe;
         Is_Safe := Is_Safe_Report
            (Reports (I) (1 .. J-1) & Reports (I) (J + 1 .. Level_Len (I)));
      end loop;
      if Is_Safe then
         Safe_Reports := Safe_Reports + 1;
      end if;
   end loop;
   Put_Line ("Part B: " & Integer'Image (Safe_Reports));

end day02;
