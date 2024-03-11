with Ada.Text_IO;
use Ada;

package body Process is
        package IO renames Text_IO;

        procedure processFile(path: String) is
                file: IO.File_Type;
                data: String(1 .. 200);
                size: Natural;
        begin
                IO.Put_line("treating file: " & path);
                file.Open(IO.In_File, path);
                loop
                        file.Get_Line(data, size);
                        IO.Put_line(data(1 .. size));
                        exit when file.End_Of_File;
                end loop;
                file.Close;
                IO.Put_line("closing file: " & path);
        end processFile;
end Process;
