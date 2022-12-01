with Ada.Text_IO;  use Ada.Text_IO;
procedure Main1 is
   type Elf_Type is new Positive;
   type Calory_Type is new Natural;
   F     : File_Type;
   Elf_Index  : Elf_Type := 1;
   Elf_Total  : Calory_Type := 0;
   Best_Index : Elf_Type;
   Best_Total : Calory_Type := 0;

   procedure Add_Line (Line : String) is
   begin
      if Line = "" then
         if Best_Total < Elf_Total then
            Best_Total := Elf_Total;
            Best_Index := Elf_Index;
         end if;
         Elf_Total := 0;
         Elf_Index := Elf_Index + 1;

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

   Put_Line
      ("Best Elf is" & Best_Index'Image & " with total" & Best_Total'Image);
end Main1;
