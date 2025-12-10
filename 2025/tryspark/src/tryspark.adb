with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Shapes;

procedure TrySpark with
      SPARK_Mode => On
   is

   procedure Increment (X : in out Integer) with
      Global => null,
      Depends => (X => X),
      Pre => X < Integer'Last,
      Post => X = X'Old + 1
   is
   begin
      X := X + 1;
   end Increment;

   procedure Put_Val (X : Integer)
   is
   begin
      Put (X);
   exception
      when others =>
         Put_Line ("Cannot write value!");
   end Put_Val;

   P1c : constant Shapes.Point := (X => Shapes.Int16'First, Y => Shapes.Int16'First);
   P2c : constant Shapes.Point := (X => Shapes.Int16'Last, Y => Shapes.Int16'Last);
   Rtc : constant Shapes.Rectangle := (Top_Left => P1c, Bottom_Right => P2c);

   P1 : Shapes.Point;
   P2 : Shapes.Point;
   Rt : Shapes.Rectangle;
begin
   ExpectOne : Integer := 0;
   Increment (ExpectOne);
   Increment (ExpectOne);
   Put_Line ("Incrementing safely!");
   Put_Val (ExpectOne);
   Put_Line ("");

   Put_Line ("Rectangle (c) Area: " & Shapes.UInt64'Image (Shapes.Area (Rtc)));

   P1 := (X => 0, Y => 0);
   P2 := (X => 10, Y => 10);
   -- strangely, remove this print statments allows the proof of the final print statment
   --  Put_Line ("P1: (" & Shapes.Int16'Image (P1.X) & "," & Shapes.Int16'Image (P1.Y) & ")");
   --  Put_Line ("P2: (" & Shapes.Int16'Image (P2.X) & "," & Shapes.Int16'Image (P2.Y) & ")");

   Rt := (Top_Left => P1, Bottom_Right => P2);
   Put_Line ("Rectangle Area: " & Shapes.UInt64'Image (Shapes.Area (Rt)));
end TrySpark;
