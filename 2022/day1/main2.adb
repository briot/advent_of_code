with Ada.Containers.Vectors;
with Ada.Text_IO;  use Ada.Text_IO;
procedure Main2 is
   type Calory_Type is new Natural;
   type Index_Type is new Positive;

   package Calories_Vectors is new Ada.Containers.Vectors
      (Index_Type, Calory_Type);
   use Calories_Vectors;

   package Sorting is new Calories_Vectors.Generic_Sorting (">");

   F         : File_Type;
   Elf_Total : Calory_Type := 0;
   Sum       : Calory_Type;
   Total     : Calories_Vectors.Vector;

   procedure Add_Line (Line : String) is
   begin
      if Line = "" then
         Total.Append (Elf_Total);
         Elf_Total := 0;
      else
         Elf_Total := Elf_Total + Calory_Type'Value (Line);
      end if;
   end Add_Line;

begin
   Open (F, In_File, "input_full");
   while not End_Of_File (F) loop
      Add_Line (Get_Line (F));
   end loop;
   Add_Line ("");
   Close (F);

   Sorting.Sort (Total);

   Sum := 0;
   for Cal in Index_Type'First .. Index_Type'First + 2 loop
      Sum := Sum + Total (Cal);
      Put_Line (Calory_Type'Image (Total (Cal)));
   end loop;
   Put_Line ("Sum of best 3 =" & Sum'Image);
end Main2;
