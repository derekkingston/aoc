with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Ordered_Sets;
use Ada.Containers;

procedure Broken_Cursor is

   package Integer_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Integer, "<" => "<", "=" => "=");
   use Integer_Sets;

   My_Ints : Integer_Sets.Set;
begin
   My_Ints.Insert (1);
   My_Ints.Insert (-2);
   My_Ints.Insert (10);
   My_Ints.Insert (-100);

   for I in My_Ints.First .. My_Ints.Last loop
      for J in I + 1 .. My_Ints.Last loop
         Put_Line (I'Image & ", " & J'Image);
      end loop;
   end loop;

end Broken_Cursor;
