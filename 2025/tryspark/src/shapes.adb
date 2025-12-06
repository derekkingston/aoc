package body Shapes is

   function Area (R : Rectangle) return UInt64 is
      Width  : constant UInt32 := UInt32 (
         abs (R.Top_Left.X - R.Bottom_Right.X));
      Height : constant UInt32 := UInt32 (
         abs (R.Bottom_Right.Y - R.Top_Left.Y));
   begin
      return UInt64 (Width * Height);
   end Area;

end Shapes;
