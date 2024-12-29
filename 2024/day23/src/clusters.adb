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
   type Bool_Array is array (0 .. ID_SIZE) of Boolean;
   type Bool_Matrix is array (0 .. ID_SIZE, 0 .. ID_SIZE) of Boolean;

   package UInt_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => UInt, "<" => "<", "=" => "=");
   use UInt_Sets;
   package UShort_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => UShort, "<" => "<", "=" => "=");
   use UShort_Sets;
   package UShort_Clusters is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => UShort_Sets.Set);
   use UShort_Clusters;

   File_Name     : constant String := "input.txt";
   File          : File_Type;
   Line          : String (1 .. 256);
   Line_Last     : Natural;
   Dash_Idx      : Natural;
   CompA, CompB  : UShort;
   Computers     : UShort_Sets.Set;
   Primes        : UShort_Array := (others => 0);
   Connections   : Bool_Matrix := (others => (others => False));
   Visited       : Bool_Array := (others => False);
   Clusters      : UShort_Clusters.Vector;
   Three_Size    : UInt := 0;
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

   --  Factorial over UInt will overflow for X >= 20
   function Factorial (X : UInt) return UInt with Pre => X < 20 is
      V : UInt := 1;
   begin
      if X < 2 then
         return 1;
      end if;
      for I in 2 .. X loop
         V := V * UInt (I);
      end loop;
      return V;
   end Factorial;

   function N_Choose_K (N, K : UInt) return UInt with
         Pre => K <= N and then K >= 1 is
      Num : UInt := 1;
      Den : UInt := 1;
      Count : UInt := 0;
      Start_Den : UInt := 2;
   begin
      --  base case: K = 1
      if K = 1 then
         return N;
      end if;
      --  base case K = N
      if K = N then
         return 1;
      end if;

      --  Definition of n choose k:
      --  (n)      n (n-1) (n-2) ... (n-k+1)
      --      :=  ---------------------------
      --  (k)         k (k-1) (k-2) ... 1

      --  attempt to group factors so as to divide as early as possible
      for I in 0 .. N - K + 1 loop
         --  CAREFUL: overflow not detected for mod types
         Num := Num * (N - UInt (I));
         Count := Count + 1;
         --  note: Y consecutive numbers have at least one divisible by Y
         --  so the product of Y consecutive numbers is also divisible by Y
         if Count = Start_Den then
            Num := Num / Start_Den;
            Start_Den := Start_Den + 1;
            Count := 0;
         end if;
      end loop;
      --  compute the remaining denominator factors
      for I in Start_Den .. K loop
         Den := Den * UInt (I);
      end loop;
      return Num / Den;
   end N_Choose_K;

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

   --  depth first search
   procedure DepthFirstSearch (Node : UShort; P : in out UShort_Sets.Set) is
      Neighbors : UShort_Sets.Set;
   begin
      --  base case: node already in tree
      if P.Contains (Node) then
         return;
      end if;

      --  otherwise, add node to the connected list
      P.Include (Node);

      --  build neighbor set from connections matrix
      Neighbors := Get_Neighbors (Node);

      --  recurse each neighbor
      for Neighbor of Neighbors loop
         DepthFirstSearch (Neighbor, P);
      end loop;
   end DepthFirstSearch;

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

   --  Extra: find all clusters of connected computers
   Visited := (others => False);
   Three_Size := 0;
   for Comp of Computers loop
      if not Visited (Comp) then
         declare
            Cluster : UShort_Sets.Set;
         begin
            Cluster.Clear;
            DepthFirstSearch (Comp, Cluster);
            Clusters.Append (Cluster);
            for C of Cluster loop
               Visited (C) := True;
            end loop;
         end;
      end if;
   end loop;

   --  all clusters of size 3 with a computer that starts with 't'
   for C in Clusters.First_Index .. Clusters.Last_Index loop
      declare
         Scratch : UShort_Sets.Set := Clusters (C);
         --  WHY? cannot use Scratch.Length in `if` statement
         --  compiler gives errors:
         --  1. `operator for type "Ada.Containers.Count_Type" is not directly visible`
         --  2. `use clause would make operation legal`
         Len : UShort := UShort (Scratch.Length);
      begin
         --  Put_Line ("Cluster [" & C'Image & "]: " & Len'Image);
         for Comp of Clusters (C) loop
            if Len > 2 and then Starts_With_T (Comp) then
               Scratch.Exclude (Comp);
               Len := Len - 1;
               Three_Size := Three_Size + N_Choose_K (UInt (Len), 2);
            end if;
         end loop;
      end;
   end loop;
   Put_Line ("Extra: " & Three_Size'Image);

end Day23;
