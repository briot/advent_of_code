with Ada.Containers.Vectors;
with Ada.Text_IO;  use Ada.Text_IO;
procedure Main is
   type RPC is (Rock, Paper, Cissors);
   type Output is (Win, Draw, Loss);

   --  First dimension is opponent, Second dimension is us
   Result : constant array (RPC, RPC) of Output :=
      (Rock    => (Rock => Draw, Paper => Win,  Cissors => Loss),
       Paper   => (Rock => Loss, Paper => Draw, Cissors => Win),
       Cissors => (Rock => Win,  Paper => Loss, Cissors => Draw));

   function Value (S : Character) return RPC is
      (case S is
       when 'A' | 'X' => Rock,
       when 'B' | 'Y' => Paper,
       when 'C' | 'Z' => Cissors,
       when others => raise Constraint_Error);

   What_To_Play : constant array (RPC, Output) of RPC :=
      (Rock    => (Draw => Rock, Win  => Paper,  Loss => Cissors),
       Paper   => (Loss => Rock, Draw => Paper,  Win  => Cissors),
       Cissors => (Win  => Rock, Loss => Paper,  Draw => Cissors));

   function Value (S : Character) return Output is
      (case S is
       when 'X' => Loss,
       when 'Y' => Draw,
       when 'Z' => Win,
       when others => raise Constraint_Error);

   Score_From_Output : constant array (Output) of Natural :=
      (Win => 6,  Draw => 3, Loss => 0);
   Score_From_RPC    : constant array (RPC) of Natural :=
      (Rock => 1, Paper => 2, Cissors => 3);

   function Part1 (Filename: String) return Natural is
      F         : File_Type;
      Total     : Natural := 0;
   begin
      Open (F, In_File, Filename);
      while not End_Of_File (F) loop
         declare
            Line     : constant String := Get_Line (F);
            Opponent : constant RPC := Value (Line (Line'First));
            Me       : constant RPC := Value (Line (Line'First + 2));
         begin
            Total := Total
               + Score_From_Output (Result (Opponent, Me))
               + Score_From_RPC (Me);
         end;
      end loop;
      Close (F);
      return Total;
   end Part1;

   function Part2 (Filename: String) return Natural is
      F         : File_Type;
      Total     : Natural := 0;
   begin
      Open (F, In_File, Filename);
      while not End_Of_File (F) loop
         declare
            Line     : constant String := Get_Line (F);
            Opponent : constant RPC := Value (Line (Line'First));
            Expected : constant Output := Value (Line (Line'First + 2));
            Me       : constant RPC := What_To_Play (Opponent, Expected);
         begin
            Total := Total
               + Score_From_Output (Result (Opponent, Me))
               + Score_From_RPC (Me);
         end;
      end loop;
      Close (F);
      return Total;
   end Part2;

begin
   Put_Line ("==== Part 1 ====");
   Put_Line ("Short Score=" & Part1 ("input_short")'Image);
   Put_Line ("Full Score=" & Part1 ("input_full")'Image);
   Put_Line ("==== Part 2 ====");
   Put_Line ("Short Score=" & Part2 ("input_short")'Image);
   Put_Line ("Full Score=" & Part2 ("input_full")'Image);
end Main;
