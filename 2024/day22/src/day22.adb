with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Day22 is
   STEPS         : constant Natural := 2000;
   MAX_INPUTS    : constant Natural := 1536;
   SEQ_SIZE      : constant Natural := 4;
   type UInt is mod 2**64;
   type Byte is range -128 .. 127;
   type UInt_Matrix is array (1 .. MAX_INPUTS, 1 .. STEPS + 1) of UInt;
   type Int_Matrix is array (1 .. MAX_INPUTS, 1 .. STEPS + 1) of Byte;
   type Delta_Seq is array (1 .. SEQ_SIZE) of Byte;

   File_Name     : constant String := "input.txt";
   File          : File_Type;
   Line          : String (1 .. 256);
   Line_Last     : Natural;
   Read_Num      : Integer;
   Num_Last      : Natural;
   Secrets       : UInt_Matrix := (others => (others => 0));
   Prices        : Int_Matrix := (others => (others => 0));
   Deltas        : Int_Matrix := (others => (others => 0));
   Num_Secrets   : Natural := 0;
   Secret_Sum    : UInt := 0;
   Max_Bananas   : Integer := 0;
   Candidate     : Integer;
   Try_Seq       : Delta_Seq := (others => 0);
   Best_Seq      : Delta_Seq := (others => 0);

   procedure Simulate_Secret (V0 : UInt; Secret_Idx : Natural) is
      V : UInt := V0;
   begin
      for I in 1 .. STEPS loop
         Secrets (Secret_Idx, I) := V;
         --  mult 64, mix, prune
         V := (V xor (V * 64)) mod 16777216;
         --  divide 32, mix, prune
         V := (V xor (V / 32)) mod 16777216;
         --  mult 2048, mix, prune
         V := (V xor (V * 2048)) mod 16777216;
      end loop;
      Secrets (Secret_Idx, STEPS + 1) := V;
   end Simulate_Secret;

   procedure Calculate_Prices is
   begin
      for I in 1 .. Num_Secrets loop
         for J in 1 .. STEPS + 1 loop
            Prices (I, J) := Byte (Secrets (I, J) mod 10);
         end loop;
      end loop;
   end Calculate_Prices;

   procedure Calculate_Deltas is
   begin
      for I in 1 .. Num_Secrets loop
         for J in 1 .. STEPS loop
            Deltas (I, J) := Prices (I, J + 1) - Prices (I, J);
         end loop;
      end loop;
   end Calculate_Deltas;

   function Buy_Bananas (Seq : Delta_Seq) return Integer is
      Bananas : Integer := 0;
      Buy : Boolean;
   begin
      for I in 1 .. Num_Secrets loop
         for J in 1 .. STEPS - SEQ_SIZE loop
            Buy := True;
            for K in 1 .. SEQ_SIZE loop
               if Deltas (I, J + K) /= Seq (K) then
                  Buy := False;
                  exit;
               end if;
            end loop;
            if Buy then
               Bananas := Bananas + Integer (Prices (I, J + SEQ_SIZE + 1));
               exit;
            end if;
         end loop;
      end loop;
      return Bananas;
   end Buy_Bananas;

begin

   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) loop
      Get_Line (File, Line, Line_Last);
      --  each line is a single secret number
      Get (Line (1 .. Line_Last), Read_Num, Num_Last);
      Num_Secrets := Num_Secrets + 1;
      Simulate_Secret (UInt (Read_Num), Num_Secrets);
   end loop;

   --  Close the file
   Close (File);

   --  Part A: display the sum of evolved secrets
   for I in 1 .. Num_Secrets loop
      Secret_Sum := Secret_Sum + Secrets (I, STEPS + 1);
   end loop;
   Put_Line ("Part A: " & Secret_Sum'Image);

   --  Part B: find the 4 digit change sequence that maximizes bananas
   Calculate_Prices;
   Calculate_Deltas;
   --  pure brute force; takes about 15 minutes; probably could be solved
   --  by looking for price sequences of smaller length first or looking
   --  for length 4 codes that occur in some significant subset of price
   --  delta histories or starting with codes that maximize some history
   for D1 in -9 .. 9 loop
      for D2 in -9 .. 9 loop
         for D3 in -9 .. 9 loop
            for D4 in -9 .. 9 loop
               Try_Seq (1) := Byte (D1);
               Try_Seq (2) := Byte (D2);
               Try_Seq (3) := Byte (D3);
               Try_Seq (4) := Byte (D4);
               if D3 mod 10 = 0 and then D4 mod 10 = 0 then
                  Put ("seq: (");
                  for K in 1 .. SEQ_SIZE loop
                     Put (Try_Seq (K)'Image & " ");
                  end loop;
                  Put (")"); New_Line;
               end if;
               Candidate := Buy_Bananas (Try_Seq);
               if Candidate > Max_Bananas then
                  Max_Bananas := Candidate;
                  Best_Seq := Try_Seq;
               end if;
            end loop;
         end loop;
      end loop;
   end loop;
   Put ("Best seq: (");
   for K in 1 .. SEQ_SIZE loop
      Put (Best_Seq (K)'Image & " ");
   end loop;
   Put (")"); New_Line;
   Put_Line ("Part B: " & Max_Bananas'Image);
   --  (-1, 2, 0, 0) --> 1501
end Day22;
