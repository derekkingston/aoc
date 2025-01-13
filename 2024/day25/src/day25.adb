with Ada.Text_IO; use Ada.Text_IO;

procedure Day25 is
   File_Name     : constant String := "input.txt";
   File          : File_Type;
   Line          : String (1 .. 256);
   Line_Last     : Natural;
   Locks         : array (1 .. 500, 1 .. 5) of Natural
                 := (others => (others => 0));
   Num_Locks     : Natural := 0;
   Keys          : array (1 .. 500, 1 .. 5) of Natural
                 := (others => (others => 5));
   Num_Keys      : Natural := 0;
   Is_Lock       : Boolean := False;
   Is_Key        : Boolean := False;
   Is_Valid      : Boolean := False;
   Combos        : Natural := 0;
begin

   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) loop
      Get_Line (File, Line, Line_Last);

      if Line_Last /= 5 then
         Is_Lock := False;
         Is_Key := False;
      else
         if not Is_Key and then not Is_Lock and then
            Line (1 .. 5) = "#####"
         then
            Is_Lock := True;
            Num_Locks := Num_Locks + 1;
         elsif not Is_Key and then not Is_Lock and then
            Line (1 .. 5) = "....."
         then
            Is_Key := True;
            Num_Keys := Num_Keys + 1;
         else
            for I in 1 .. 5 loop
               if Is_Lock and then Line (I) = '#' then
                  Locks (Num_Locks, I) := Locks (Num_Locks, I) + 1;
               end if;
               if Is_Key and then Line (I) = '.' then
                  Keys (Num_Keys, I) := Keys (Num_Keys, I) - 1;
               end if;
            end loop;
         end if;
      end if;
   end loop;

   --  Close the file
   Close (File);

   --  Part A: for each lock check if each key is compatible
   for L in 1 .. Num_Locks loop
      for K in 1 .. Num_Keys loop
         Is_Valid := True;
         for I in 1 .. 5 loop
            if Locks (L, I) + Keys (K, I) > 5 then
               Is_Valid := False;
               exit;
            end if;
         end loop;
         if Is_Valid then
            Combos := Combos + 1;
         end if;
      end loop;
   end loop;
   Put_Line ("Part A: " & Combos'Image);
end Day25;
