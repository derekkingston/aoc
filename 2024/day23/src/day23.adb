with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

procedure Day23 is
   type UShort is mod 2**16;
   type UInt is mod 2**64;
   Zero          : constant UShort := UShort (Character'Pos ('a'));
   T_Id          : constant UShort := UShort (Character'Pos ('t')) - Zero;
   Width         : constant UShort := UShort (Character'Pos ('z')) - Zero + 1;
   ID_SIZE       : constant UShort := Width * Width; --  should be 676
   type UShort_Array is array (0 .. ID_SIZE) of UShort;
   type Bool_Matrix is array (0 .. ID_SIZE, 0 .. ID_SIZE) of Boolean;

   package UInt_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => UInt, "<" => "<", "=" => "=");
   use UInt_Sets;
   package UShort_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => UShort, "<" => "<", "=" => "=");
   use UShort_Sets;

   File_Name     : constant String := "input.txt";
   File          : File_Type;
   Line          : String (1 .. 256);
   Line_Last     : Natural;
   Dash_Idx      : Natural;
   CompA, CompB  : UShort;
   Computers     : UShort_Sets.Set;
   Primes        : UShort_Array := (others => 0);
   Connections   : Bool_Matrix := (others => (others => False));
   Triples       : UInt_Sets.Set;

   --  create a mapping from two character lower-case string to UShort
   --  treat first character as the least significant bits
   function To_Id (S : String) return UShort is
      A : constant UShort := UShort (Character'Pos (
            S (S'First))) - Zero;
      B : constant UShort := UShort (Character'Pos (
            S (S'First + 1))) - Zero;
   begin
      return B * Width + A;
   end To_Id;

   function To_Name (Id : UShort) return String is
      A : constant Character := Character'Val (Id mod Width + Zero);
      B : constant Character := Character'Val (Id / Width + Zero);
   begin
      return A & B;
   end To_Name;

   function Starts_With_T (Id : UShort) return Boolean is
   begin
      if Id mod Width = T_Id then
         return True;
      end if;
      return False;
   end Starts_With_T;

   --  sieve of eratosthenes
   procedure Compute_Prime_Mapping is
      --  note: 5059 is the 677th prime number
      MAX_PRIME : constant UShort := 5059;
      Is_Prime : array (0 .. MAX_PRIME) of Boolean := (others => True);
      V, W : UShort;
   begin
      Is_Prime (0) := False;
      Is_Prime (1) := False;
      V := 2;
      while V * V <= MAX_PRIME loop
         if Is_Prime (V) then
            --  mark all multiples of V as not prime
            W := V * V;
            while W <= MAX_PRIME loop
               Is_Prime (W) := False;
               W := W + V;
            end loop;
         end if;
         V := V + 1;
      end loop;
      V := 2;
      for I in 0 .. ID_SIZE loop
         for J in V .. MAX_PRIME loop
            if Is_Prime (J) then
               Primes (I) := J;
               V := J + 1;
               exit;
            end if;
         end loop;
      end loop;
   end Compute_Prime_Mapping;

   function Triple_Key (A, B, C : UShort) return UInt is
   begin
      return UInt (Primes (A)) * UInt (Primes (B)) * UInt (Primes (C));
   end Triple_Key;

   function Get_Neighbors (Node : UShort) return UShort_Sets.Set is
      Neighbors : UShort_Sets.Set;
   begin
      for I in 0 .. ID_SIZE loop
         if Connections (Node, I) and then I /= Node then
            Neighbors.Include (I);
         end if;
      end loop;
      return Neighbors;
   end Get_Neighbors;

   function Shared_Neighbors (A, B : UShort) return UShort_Sets.Set is
      Shared : UShort_Sets.Set;
      ANeighborhood : constant UShort_Sets.Set := Get_Neighbors (A);
      BNeighborhood : constant UShort_Sets.Set := Get_Neighbors (B);
   begin
      for Neighbor of ANeighborhood loop
         if BNeighborhood.Contains (Neighbor) and then Neighbor /= B then
            Shared.Include (Neighbor);
         end if;
      end loop;
      return Shared;
   end Shared_Neighbors;

begin
   Compute_Prime_Mapping;

   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) loop
      Get_Line (File, Line, Line_Last);
      Dash_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => "-", From => 1);
      --  create the IDs of the connected computers
      CompA := To_Id (Line (1 .. Dash_Idx - 1));
      CompB := To_Id (Line (Dash_Idx + 1 .. Line_Last));
      --  Put_Line (To_Name (CompA) & "-" & To_Name (CompB));
      --  Put_Line (CompA'Image & "-" & CompB'Image);
      --  indicate the connection in the connection matrix, bi-directional
      Connections (CompA, CompB) := True;
      Connections (CompB, CompA) := True;
      --  update set of all computers involved
      Computers.Include (CompA);
      Computers.Include (CompB);
   end loop;

   --  Close the file
   Close (File);

   --  Part A: find all immediate neighborhoods of size 3
   for Comp of Computers loop
      if Starts_With_T (Comp) then
         declare
            Neighborhood : UShort_Sets.Set;
            Shared_Neighborhood : UShort_Sets.Set;
         begin
            Neighborhood := Get_Neighbors (Comp);
            for Neighbor of Neighborhood loop
               Shared_Neighborhood := Shared_Neighbors (Comp, Neighbor);
               for SNeighbor of Shared_Neighborhood loop
                  Triples.Include (Triple_Key (Comp, Neighbor, SNeighbor));
               end loop;
            end loop;
         end;
      end if;
   end loop;
   Put_Line ("Part A: " & Triples.Length'Image);

end Day23;
