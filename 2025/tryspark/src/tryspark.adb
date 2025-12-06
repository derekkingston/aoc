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

   P1 : constant Shapes.Point := (X => 0, Y => 10);
   P2 : constant Shapes.Point := (X => 5, Y => 0);
   Rt : constant Shapes.Rectangle := (Top_Left => P1, Bottom_Right => P2);
begin
   ExpectOne : Integer := 0;
   Increment (ExpectOne);
   Increment (ExpectOne);
   Put_Line ("Incrementing safely!");
   Put_Val (ExpectOne);
   Put_Line ("");
   Put_Line ("Rectangle Area: " & Shapes.UInt64'Image (Shapes.Area (Rt)));
end TrySpark;
