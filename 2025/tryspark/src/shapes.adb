package body Shapes is

   function Area (R : Rectangle) return UInt64 is
      -- Note: that to get the proper 'abs' function to be called
      -- and thus avoid possible overflow, casting to Int32 before
      -- subtraction is required. Interestingly, without the cast in
      -- place, SPARK will still prove absence of run-time errors
      -- even though a run-time constraint error is raised in actual
      -- execution at (no cast version):
      --  Width  : constant UInt64 := UInt64 (abs (R.Top_Left.X - R.Bottom_Right.X));
      --  Height : constant UInt64 := UInt64 (abs (R.Bottom_Right.Y - R.Top_Left.Y));
      Width  : constant UInt64 := UInt64 (abs ( Int32(R.Top_Left.X) - Int32(R.Bottom_Right.X)));
      Height : constant UInt64 := UInt64 (abs ( Int32(R.Bottom_Right.Y) - Int32(R.Top_Left.Y)));
   begin
      return Width * Height;
   end Area;

end Shapes;
