with Ada.Text_IO; use Ada.Text_IO;

procedure day12 is
   type String_List is array (Positive range <>) of String (1 .. 142);
   type Plot_List is array (Positive range <>) of String_List (1 .. 142);

   File_Name     : constant String := "input.txt";
   File          : File_Type;
   Line          : String (1 .. 142);
   Line_Last     : Natural;
   Puzzle        : String_List (1 .. 142);
   Visited       : String_List (1 .. 142);
   Row_Len       : Natural := 0;
   Col_Len       : Natural := 0;
   Occupancy     : Plot_List (1 .. 1000);
   Num_Plots     : Natural := 0;
   Total_Cost    : Natural := 0;
   Discount_Cost : Natural := 0;

   procedure Clear_Plot (I : Natural) is
   begin
      for R in 1 .. Row_Len loop
         Occupancy (I) (R) := (others => ' ');
      end loop;
   end Clear_Plot;

   procedure Fill_Plot (I, R, C : Natural) is
      V : constant Character := Puzzle (R) (C);
   begin
      --  mark as visited
      Visited (R) (C) := V;
      Occupancy (I) (R) (C) := V;
      --  north
      if R > 1 and then Puzzle (R - 1) (C) = V and then
         Visited (R - 1) (C) = ' '
      then
         Fill_Plot (I, R - 1, C);
      end if;
      --  east
      if C < Col_Len and then Puzzle (R) (C + 1) = V and then
         Visited (R) (C + 1) = ' '
      then
         Fill_Plot (I, R, C + 1);
      end if;
      --  south
      if R < Row_Len and then Puzzle (R + 1) (C) = V and then
         Visited (R + 1) (C) = ' '
      then
         Fill_Plot (I, R + 1, C);
      end if;
      --  west
      if C > 1 and then Puzzle (R) (C - 1) = V and then
         Visited (R) (C - 1) = ' '
      then
         Fill_Plot (I, R, C - 1);
      end if;
   end Fill_Plot;

   function Fence_Cost (I : Natural) return Natural is
      Area : Natural := 0;
      Sides : Natural := 0;
   begin
      for R in 1 .. Row_Len loop
         for C in 1 .. Col_Len loop
            if Occupancy (I) (R) (C) /= ' ' then
               Area := Area + 1;
               --  north
               if R = 1 or else Occupancy (I) (R - 1) (C) = ' ' then
                  Sides := Sides + 1;
               end if;
               --  east
               if C = Col_Len or else Occupancy (I) (R) (C + 1) = ' ' then
                  Sides := Sides + 1;
               end if;
               --  south
               if R = Row_Len or else Occupancy (I) (R + 1) (C) = ' ' then
                  Sides := Sides + 1;
               end if;
               --  west
               if C = 1 or else Occupancy (I) (R) (C - 1) = ' ' then
                  Sides := Sides + 1;
               end if;
            end if;
         end loop;
      end loop;
      return Area * Sides;
   end Fence_Cost;

   function Discount_Fence_Cost (I : Natural) return Natural is
      Area : Natural := 0;
      Sides : Natural := 0;
   begin
      for R in 1 .. Row_Len loop
         for C in 1 .. Col_Len loop
            if Occupancy (I) (R) (C) /= ' ' then
               Area := Area + 1;
               --  north
               if R = 1 or else Occupancy (I) (R - 1) (C) = ' ' then
                  Sides := Sides + 1;
                  --  discount: cell to left is part of plot AND has
                  --  a northern edge --> straight edge on north, discount
                  if C > 1 and then Occupancy (I) (R) (C - 1) /= ' ' then
                     if R = 1 or else Occupancy (I) (R - 1) (C - 1) = ' ' then
                        Sides := Sides - 1;
                     end if;
                  end if;
               end if;
               --  east
               if C = Col_Len or else Occupancy (I) (R) (C + 1) = ' ' then
                  Sides := Sides + 1;
                  --  discount: cell above is part of plot AND has
                  --  an eastern edge --> straight edge on east, discount
                  if R > 1 and then Occupancy (I) (R - 1) (C) /= ' ' then
                     if C = Col_Len or else Occupancy (I) (R - 1) (C + 1) = ' ' then
                        Sides := Sides - 1;
                     end if;
                  end if;
               end if;
               --  south
               if R = Row_Len or else Occupancy (I) (R + 1) (C) = ' ' then
                  Sides := Sides + 1;
                  --  discount: cell to left is part of plot AND has
                  --  a southern edge --> straight edge on south, discount
                  if C > 1 and then Occupancy (I) (R) (C - 1) /= ' ' then
                     if R = Row_Len or else Occupancy (I) (R + 1) (C - 1) = ' ' then
                        Sides := Sides - 1;
                     end if;
                  end if;
               end if;
               --  west
               if C = 1 or else Occupancy (I) (R) (C - 1) = ' ' then
                  Sides := Sides + 1;
                  --  discount: cell above is part of plot AND has
                  --  a western edge --> straight edge on west, discount
                  if R > 1 and then Occupancy (I) (R - 1) (C) /= ' ' then
                     if C = 1 or else Occupancy (I) (R - 1) (C - 1) = ' ' then
                        Sides := Sides - 1;
                     end if;
                  end if;
               end if;
            end if;
         end loop;
      end loop;
      return Area * Sides;
   end Discount_Fence_Cost;

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
      Visited (Row_Len) := (others => ' ');
   end loop;

   --  Close the file
   Close (File);

   --  Perform fill with each character not already in Visited
   for R in 1 .. Row_Len loop
      for C in 1 .. Col_Len loop
         --  if this parcerl is  not yet visited
         if Visited (R) (C) = ' ' then
            --  Put_Line ("Creating plot " & Puzzle (R) (C)
            --     & " from (" & R'Image & ", " & C'Image & ")");
            --  create a new empty plot map
            Num_Plots := Num_Plots + 1;
            Clear_Plot (Num_Plots);
            --  fill in the plot map starting from (R, C)
            Fill_Plot (Num_Plots, R, C);
         end if;
      end loop;
   end loop;

   for I in 1 .. Num_Plots loop
      Total_Cost := Total_Cost + Fence_Cost (I);
   end loop;
   Put_Line ("Part A: " & Total_Cost'Image);

   for I in 1 .. Num_Plots loop
      Discount_Cost := Discount_Cost + Discount_Fence_Cost (I);
   end loop;
   Put_Line ("Part B: " & Discount_Cost'Image);
end day12;
