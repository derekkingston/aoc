with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

procedure day09 with SPARK_Mode is
   type UInt is mod 2**64;
   type File_Record is record
      File_Block : Natural;
      Free_Block : Natural;
      File_ID    : Natural;
   end record;

   procedure Print_File_Record (F : File_Record) is
   begin
      Put_Line ("Size: " & F.File_Block'Image
         & " Free: " & F.Free_Block'Image
         & " ID: " & F.File_ID'Image);
   end Print_File_Record;

   package File_Record_Vec is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => File_Record);
   use File_Record_Vec;

   File_Name    : constant String := "input.txt";
   File         : File_Type;
   Line         : String (1 .. 20001);
   Line_Start   : Natural := 1;
   Line_Last    : Natural;
   FilesA       : File_Record_Vec.Vector := File_Record_Vec.Empty_Vector;
   CompressedA  : File_Record_Vec.Vector := File_Record_Vec.Empty_Vector;
   FilesB       : File_Record_Vec.Vector := File_Record_Vec.Empty_Vector;
   CompressedB  : File_Record_Vec.Vector := File_Record_Vec.Empty_Vector;
   Id           : Natural := 0;
   X            : Natural;
   Y            : Natural;
   DX           : Natural;
   R            : File_Record;
   Count        : UInt := 0;
   Chk_Sum      : UInt := 0;

   procedure CompressA is
   begin
      --  move file blocks from the end to the open space at the front
      X := FilesA.First_Index;
      Y := FilesA.Last_Index;
      --  add the first file block to the compressed list
      CompressedA.Append ((File_Block => FilesA.Element (X).File_Block,
         Free_Block => 0, File_ID => FilesA.Element (X).File_ID));
      while X /= Y loop
         DX := Natural'Min (FilesA.Element (X).Free_Block,
            FilesA.Element (Y).File_Block);
         if DX > 0 then
            --  add a 'DX' size file block from the back to the front
            R := FilesA.Element (X);
            R.Free_Block := R.Free_Block - DX;
            FilesA.Replace_Element (X, R);
            R := FilesA.Element (Y);
            R.File_Block := R.File_Block - DX;
            FilesA.Replace_Element (Y, R);
            CompressedA.Append ((File_Block => DX, Free_Block => 0,
               File_ID => FilesA.Element (Y).File_ID));
         end if;
         if FilesA.Element (X).Free_Block < 1 then
            --  if all of the free space is used up at the front, then
            --  add the next file block from the front
            X := X + 1;
            CompressedA.Append ((File_Block => FilesA.Element (X).File_Block,
               Free_Block => 0, File_ID => FilesA.Element (X).File_ID));
         end if;
         if FilesA.Element (Y).File_Block < 1 then
            --  all of the files have been moved from this block, so
            --  it is no longer relevant
            Y := Y - 1;
         end if;
      end loop;

      --  New_Line;
      --  for I in CompressedA.First_Index .. CompressedA.Last_Index loop
      --     Print_File_Record (CompressedA.Element (I));
      --  end loop;
   end CompressA;

   procedure CompressB is
   begin
      --  copy files
      for I in FilesB.First_Index .. FilesB.Last_Index loop
         CompressedB.Append (FilesB.Element (I));
      end loop;
      Y := FilesB.Last_Index;
      while Y > FilesB.First_Index loop
         --  find the corresponding ID in the compressed list
         X := Y;
         for K in CompressedB.First_Index .. CompressedB.Last_Index loop
            if CompressedB.Element (K).File_ID =
               FilesB.Element (Y).File_ID
            then
               X := K;
               exit;
            end if;
         end loop;

         -- search from the start to the current position of the Y record
         for J in CompressedB.First_Index .. X - 1 loop
            if CompressedB.Element (J).Free_Block >=
               FilesB.Element (Y).File_Block
            then
               --  put Y record into J's free space
               DX := CompressedB.Element (J).Free_Block -
                        FilesB.Element (Y).File_Block;

               --  reduce free space of J
               R := CompressedB.Element (J);
               R.Free_Block := 0;
               CompressedB.Replace_Element (J, R);

               --  free up record Y
               R := CompressedB.Element (X);
               R.Free_Block := R.Free_Block + R.File_Block;
               R.File_Block := 0;
               R.File_ID := 0;
               CompressedB.Replace_Element (X, R);

               --  insert J+1 compressed record
               R := FilesB.Element (Y);
               R.Free_Block := DX;
               CompressedB.Insert (J + 1, R);
               exit;
            end if;
         end loop;
         Y := Y - 1;
      end loop;
   end CompressB;

begin
   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) loop
      Get_Line (File, Line, Line_Last);

      while Line_Start < Line_Last loop
         R := (File_Block => Natural'Value (
                  Line (Line_Start .. Line_Start)),
               Free_Block => Natural'Value (Line (
                  Line_Start + 1 .. Line_Start + 1)),
               File_ID => Id);
         FilesA.Append (R);
         FilesB.Append (R);
         Id := Id + 1;
         Line_Start := Line_Start + 2;
      end loop;
      if Line_Start = Line_Last then
         R := (File_Block => Natural'Value (
                  Line (Line_Start .. Line_Start)),
               Free_Block => 0, File_ID => Id);
         FilesA.Append (R);
         FilesB.Append (R);
      end if;

      --  for I in FilesA.First_Index .. FilesA.Last_Index loop
      --     Print_File_Record (FilesA.Element (I));
      --  end loop;

      CompressA;
      --  Part A: checksum of compressed list
      for I in CompressedA.First_Index .. CompressedA.Last_Index loop
         for J in 1 .. CompressedA.Element (I).File_Block loop
            Chk_Sum := Chk_Sum +
               Count * UInt (CompressedA.Element (I).File_ID);
            Count := Count + 1;
         end loop;
      end loop;
      Put_Line ("Part A: " & Chk_Sum'Image);

      CompressB;
      --  Part B: checksum of compressed list
      Chk_Sum := 0;
      Count := 0;
      for I in CompressedB.First_Index .. CompressedB.Last_Index loop
         for J in 1 .. CompressedB.Element (I).File_Block loop
            Chk_Sum := Chk_Sum +
               Count * UInt (CompressedB.Element (I).File_ID);
            Count := Count + 1;
         end loop;
         Count := Count + UInt (CompressedB.Element (I).Free_Block);
      end loop;
      Put_Line ("Part B: " & Chk_Sum'Image);

   end loop;

   --  Close the file
   Close (File);

end day09;
