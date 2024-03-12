with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.RegPat;
use GNAT.Regpat;
use GNAT;
use Ada;

package body Process is
        package IO renames Text_IO;
        package CH renames Characters.Handling;
        package DIR renames Directories;

        procedure printHelp(name: String) is
        begin
                IO.Put_Line("USAGE: " & name & " [OPTIONS]");
                IO.Put_Line("    -nw | --no-warn         remove warning messages");
                IO.Put_Line("    -w  | --warn            prints warning messages");
                IO.Put_Line("    -ni | --no-info         remove info messages");
                IO.Put_Line("    -i  | --info            prints info messages");
                IO.Put_Line("    -h  | --no-warn         prints this help");
        end printHelp;

        procedure processFile(self: in out sourceFile; path: String; info: Boolean) is
                file: IO.File_Type;
        begin
                file.Open(IO.In_File, path);
                loop
                        data: constant Unbounded_String := To_Unbounded_String(file.Get_Line);
                        exit when self.headName(data.To_String);
                        exit when file.End_Of_File;
                end loop;
                if self.has_header then
                        data: constant String := self.header.To_String;
                        if data(data'Last - 1 .. data'Last) /= ".h" then
                                IO.Put_Line("Founded Error in auto header '" & data & "' : header extension is not .h");
                                self.has_header := False;
                                return;
                        elsif info then
                                IO.Put_Line("Founded Auto-Header: " & self.header.To_String);
                        end if;
                end if;
                file.Reset;
                loop
                        data: constant Unbounded_String := To_Unbounded_String(file.Get_Line);
                        self.getPrototype(data.To_String);
                        exit when file.End_Of_File;
                end loop;
                file.Close;
        end processFile;

        function headName(self: in out sourceFile; line: String) return Boolean is
                i, k: Integer := 0;
        begin
                return False when line'Length <= 2;
                return False when line(Line'First .. Line'First + 1) /= "//";
                for j in line'First .. line'Last loop
                        i := j;
                        exit when line(j) /= '/' and line(j) /= ' ';
                end loop;
                return False when line'Last - i < 14;
                return False when line(i .. i + 11) /= "auto-header ";
                for j in i + 12 .. line'Last loop
                        k := j;
                        exit when line(j) /= ' ';
                end loop;
                return False when line'Last - k < 3;
                self.has_header := True;
                self.header := To_Unbounded_String(line(k .. line'Last));
                return True;
        end headName;

        procedure getPrototype(self: in out sourceFile; line: String) is
                engine: constant Regpat.Pattern_Matcher := Regpat.Compile("^(((struct|enum|union|unsigned|signed)\s)?(long\s)?(long\s)?[a-zA-Z0-9_]+\s*(\s+|[*]*)\s*[a-zA-Z0-9_]+\s*[(]\s*(\s*(((struct|enum|union|unsigned|signed)\s)?(long\s)?(long\s)?\s*[a-zA-Z0-9_]+\s*(\s+|[*]*)\s*[a-zA-Z0-9_]+\s*([[]\s*[0-9]*\s*[]]\s*)?\s*[,]?\s*)*|(\s*void\s*)?)\s*[)])");
                match: Regpat.Match_Array(0 .. 1);
        begin
                Regpat.Match(engine, line, match);
                return when match(0) = Regpat.No_Match;
                self.prototypes := self.prototypes & line(match(1).First .. match(1).Last) & ";" & ASCII.LF;
        end getPrototype;

        function has_symbol(self: sourceFile; symbol: String) return Boolean is
                engine: constant Regpat.Pattern_Matcher := Regpat.Compile("([a-zA-Z0-9_]+)\s*[(]");
                match: Regpat.Match_Array(0 .. 1);
                line: constant String := self.prototypes.To_String;
                start: Integer := line'First;
        begin
                loop
                        Regpat.Match(engine, line(start .. line'Last), match);
                        exit when match(0) = Regpat.No_Match;
                        return True when line(match(1).First .. match(1).Last) = symbol;
                        start := match(1).Last + 1;
                end loop;
                return False;
        end has_symbol;

        procedure append(self: sourceFile; warn: Boolean) is
                f: IO.File_Type;
                name: String := self.header.To_String;
                nameCpy: constant String := name;
                engine: constant Regpat.Pattern_Matcher := Regpat.Compile("([a-zA-Z0-9_]+)\s*[(]");
                match: Regpat.Match_Array(0 .. 1);
                data: Unbounded_String;
                newProto: Unbounded_String;
        begin
                f.Open(IO.In_File, name);
                for i in name'First .. name'Last loop
                        if name(i) /= '.' then
                                name(i) := CH.To_Upper(name(i));
                        else
                                name(i) := '_';
                        end if;
                end loop;
                loop
                        exit when f.End_Of_File;
                        data := To_Unbounded_String(f.Get_Line);
                        declare
                                line: constant String := data.To_String;
                        begin
                                if line'Length > 6 then
                                        exit when line(line'First .. line'First + 6) = "#endif ";
                                end if;
                                Regpat.Match(engine, line, match);
                                if line'Length <= 1 then
                                        null;
                                elsif line = ("#define " & name) then
                                        newProto := newProto & line & ASCII.LF & ASCII.LF;
                                elsif match(0) = Regpat.No_Match then
                                        newProto := newProto & line & ASCII.LF;
                                elsif (line'Length > 7) and (line(line'First .. line'First + 7) = "typedef ") then
                                        newProto := newProto & line & ASCII.LF;
                                elsif not self.has_symbol(line(match(1).First .. match(1).Last)) then
                                        newProto := newProto & line & ASCII.LF;
                                else
                                        if warn then
                                                IO.Put_Line ("warning: overriting signature of symbol '" & line(match(1).First .. match(1).Last) & "'");
                                        end if;
                                end if;
                        end;
                end loop;
                f.Close;
                newProto := newProto & self.prototypes & ASCII.LF & "#endif // " & name & ASCII.LF;
                DIR.Delete_File(nameCpy);
                f.Create(Name => nameCpy);
                f.Put(newProto.To_String);
                f.Close;
        end append;

        procedure write(self: sourceFile) is
                f: IO.File_Type;
                name: String := self.header.To_String;
        begin
                f.Create(Name => name);
                for i in name'First .. name'Last loop
                        if name(i) /= '.' then
                                name(i) := CH.To_Upper(name(i));
                        else
                                name(i) := '_';
                        end if;
                end loop;
                f.Put_Line("#ifndef " & name);
                f.Put_Line("#define " & name);
                f.Put(ASCII.LF & self.prototypes.To_String & ASCII.LF);
                f.Put_Line("#endif // " & name);
                f.Close;
        end write;
end Process;
