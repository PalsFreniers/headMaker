with Ada.Text_IO;
with Ada.Directories;
with process;

procedure Headmaker is
        package IO renames Ada.Text_IO;
        package DIR renames Ada.Directories;
        search: DIR.Search_Type;
        dirEntry: DIR.Directory_Entry_Type;
        curDir: constant String := DIR.Current_Directory;
begin
        IO.Put_line("Treating directory: " & curDir);
        DIR.Start_Search(search, curDir, "*.c");
        loop
                exit when not DIR.More_Entries(search);
                DIR.Get_Next_Entry(search, dirEntry);
                case dirEntry.Kind is
                        when DIR.Ordinary_File => process.processFile(dirEntry.Full_Name);
                        when others => null;
                end case;
                pos := pos + 1;
        end loop;
        DIR.End_Search(search);
        IO.Put_line("Closing directory: " & curDir);
end Headmaker;
