with Ada.Containers.Vectors;
with Ada.Text_IO;  use Ada.Text_IO;
procedure Main is

   type Item_Type is new Character;
   type Priority_Type is new Long_Integer;

   function Priority (Item : Item_Type) return Priority_Type
      is (case Item is
          when 'a' .. 'z' => Priority_Type
             (Item_Type'Pos (Item) - Item_Type'Pos ('a') + 1),
          when 'A' .. 'Z' => Priority_Type
             (Item_Type'Pos (Item) - Item_Type'Pos ('A') + 27),
          when others => raise Constraint_Error);

   type Rucksack_Half is array (Item_Type) of Boolean;

   function Value (S : String) return Rucksack_Half is
   begin
      return R : Rucksack_Half := (others => False) do
         for C of S loop
            R (Item_Type (C)) := True;
         end loop;
      end return;
   end Value;

   function Part1 (Filename: String) return Priority_Type is
      F          : File_Type;
      Total      : Priority_Type := 0;
   begin
      Open (F, In_File, Filename);
      while not End_Of_File (F) loop
         declare
            Line  : constant String := Get_Line (F);
            First : constant Rucksack_Half :=
               Value (Line (Line'First .. Line'First + Line'Length / 2 - 1));
            Second : constant Rucksack_Half :=
               Value (Line (Line'First + Line'Length / 2 .. Line'Last));
         begin
            for S in First'Range loop
               if First (S) and Second (S) then
                  Total := Total + Priority (S);
                  exit;
               end if;
            end loop;
         end;
      end loop;
      Close (F);
      return Total;
   end Part1;

   function Part2 (Filename: String) return Priority_Type is
      F          : File_Type;
      Total      : Priority_Type := 0;
   begin
      Open (F, In_File, Filename);
      while not End_Of_File (F) loop
         declare
            Line1    : constant String := Get_Line (F);
            Line2    : constant String := Get_Line (F);
            Line3    : constant String := Get_Line (F);
            R1 : Rucksack_Half := Value (line1);
            R2 : constant Rucksack_Half := Value (line2);
            R3 : constant Rucksack_Half := Value (line3);
         begin
            for Item in R2'Range loop
               if not R2 (Item) then
                  R1 (Item) := False;
               end if;
            end loop;

            for Item in R3'Range loop
               if not R3 (Item) then
                  R1 (Item) := False;
               end if;
            end loop;

            for J in R1'Range loop
               if R1 (J) then
                  Total := Total + Priority (J);
                  exit;
               end if;
            end loop;
         end;
      end loop;
      Close (F);
      return Total;
   end Part2;

begin
   Put_Line ("==== Part 1 ====");
   Put_Line ("Short Priority=" & Part1 ("input_short")'Image);
   Put_Line ("Full Priority=" & Part1 ("input_full")'Image);
   Put_Line ("==== Part 2 ====");
   Put_Line ("Short Priority=" & Part2 ("input_short")'Image);
   Put_Line ("Full Priority=" & Part2 ("input_full")'Image);
end Main;
