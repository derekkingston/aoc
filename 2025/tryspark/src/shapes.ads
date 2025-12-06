package Shapes 
   with SPARK_Mode => On
is
   type Int16 is range -2**15 .. 2**15 - 1;
   type Int32 is range -2**31 .. 2**31 - 1;
   type Int64 is range -2**63 .. 2**63 - 1;
   type UInt16 is mod 2**16;
   type UInt32 is mod 2**32;
   type UInt64 is mod 2**64;

   type Point is record
      X : Int16;
      Y : Int16;
   end record;

   type Rectangle is record
      Top_Left     : Point;
      Bottom_Right : Point;
   end record;

   function Area (R : Rectangle) return UInt64;

end Shapes;
