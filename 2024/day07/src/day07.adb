with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Long_Integer_Text_IO; use Ada.Long_Long_Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure day07 with SPARK_Mode is
   type UInt is mod 2**64;
   type Int is new Long_Long_Integer;
   type Int_Array is array (Positive range <>) of Int;

   File_Name     : constant String := "input.txt";
   File          : File_Type;
   Line          : String (1 .. 256);
   Line_Last     : Natural;
   Colon_Idx     : Natural;
   Space_Idx     : Natural;
   Read_Num      : Long_Long_Integer;
   Num_Last      : Natural;
   Result        : Int;
   ResultA_Sum   : Int := 0;
   ResultB_Sum   : Int := 0;
   Values        : Int_Array (1 .. 32);
   Values_Len    : Natural := 0;

   function Can_Chop (R0, V0 : Int) return Boolean is
      R : Int := R0;
      V : Int := V0;
   begin
      while V > 0 loop
         if R mod 10 /= V mod 10 then
            return False;
         end if;
         R := R / 10;
         V := V / 10;
      end loop;
      return True;
   end Can_Chop;

   function Chop (R0, V0 : Int) return Int is
      R : Int := R0;
      V : Int := V0;
   begin
      while V > 0 loop
         R := R / 10;
         V := V / 10;
      end loop;
      return R;
   end Chop;

   function Is_Feasible (R : Int; A : Int_Array) return Boolean is
   begin
      --  base case: empty array (note: does not occur)
      if A'Length < 1 then
         if R = 0 then
            return True;
         end if;
         return False;
      end if;
      --  base case: array of length 1
      if A'Length = 1 then
         if R = A (A'First) then
            return True;
         end if;
         return False;
      end if;
      --  recursion from the back
      --  try last operator as '*' --> result changes by dividing
      --  assuming integers require that the division have no remainder
      if R mod A (A'Last) = 0 and then
         Is_Feasible (R / A (A'Last), A (A'First .. A'Last - 1))
      then
         return True;
      end if;
      --  try last operator as '+' --> result changes by sutracting
      if Is_Feasible (R - A (A'Last), A (A'First .. A'Last - 1)) then
         return True;
      end if;
      return False;
   end Is_Feasible;

   function Is_Feasible_Or (R : Int; A : Int_Array) return Boolean is
   begin
      --  base case: empty array (note: does not occur)
      if A'Length < 1 then
         if R = 0 then
            return True;
         end if;
         return False;
      end if;
      --  base case: array of length 1
      if A'Length = 1 then
         if R = A (A'First) then
            return True;
         end if;
         return False;
      end if;
      --  recursion from the back
      --  try last operator as '*' --> result changes by dividing
      --  assuming integers require that the division have no remainder
      if R mod A (A'Last) = 0 and then
         Is_Feasible_Or (R / A (A'Last), A (A'First .. A'Last - 1))
      then
         return True;
      end if;
      --  try last operator as '+' --> result changes by sutracting
      if Is_Feasible_Or (R - A (A'Last), A (A'First .. A'Last - 1)) then
         return True;
      end if;
      --  try last operator as '||' --> result changes by chopping
      if Can_Chop (R, A (A'Last)) then
         if Is_Feasible_Or (Chop (R, A (A'Last)), A (A'First .. A'Last - 1))
         then
            return True;
         end if;
      end if;
      return False;
   end Is_Feasible_Or;

begin
   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) loop
      --  pattern is [Result]: [Value] [Value] ... [Value]
      Get_Line (File, Line, Line_Last);

      --  extract the result by finding the ':'
      Colon_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => ":", From => 1);
      if Colon_Idx = 0 then
         Put ("Missing ':' separator");
         exit;
      end if;
      Get (Line (1 .. Colon_Idx - 1), Read_Num, Num_Last);
      Result := Int (Read_Num);

      --  put all space separated values into the 'Values' array
      Colon_Idx := Colon_Idx + 2;
      Space_Idx := Index (Source => Line (Colon_Idx .. Line_Last),
         Pattern => " ", From => Colon_Idx);
      Values_Len := 0;
      while Space_Idx /= 0 and then Values_Len < Values'Length loop
         Values_Len := Values_Len + 1;
         Get (Line (Colon_Idx .. Space_Idx - 1), Read_Num, Num_Last);
         Values (Values_Len) := Int (Read_Num);
         Colon_Idx := Space_Idx + 1;
         Space_Idx := Index (Source => Line (Colon_Idx .. Line_Last),
            Pattern => " ", From => Colon_Idx);
      end loop;
      --  don't forget the last value on the line
      Values_Len := Values_Len + 1;
      Get (Line (Colon_Idx .. Line_Last), Read_Num, Num_Last);
      Values (Values_Len) := Int (Read_Num);

      --  check feasible result against possible operators
      if Is_Feasible (Result, Values (1 .. Values_Len)) then
         ResultA_Sum := ResultA_Sum + Result;
         ResultB_Sum := ResultB_Sum + Result;
      elsif Is_Feasible_Or (Result, Values (1 .. Values_Len)) then
         --  Put_Line ("With ||: " & Line (1 .. Line_Last));
         ResultB_Sum := ResultB_Sum + Result;
      end if;

   end loop;

   --  Close the file
   Close (File);

   --  Part A: display the sum of all values in valid operations
   Put_Line ("Part A: " & Int'Image (ResultA_Sum)); --  12839601725877
   Put_Line ("Part B: " & Int'Image (ResultB_Sum)); --  149956401519484

end day07;
