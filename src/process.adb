with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Streams;
with Ada.Text_IO;
with GNAT.RegPat;
with Spawn;
with Spawn.Processes;
with Spawn.String_Vectors;
with Spawn.Processes.Monitor_Loop;
use Ada.Streams;
use GNAT.Regpat;
use Spawn;
use GNAT;
use Ada;

package body Process is
        use stringList;
        package IO renames Text_IO;
        package CH renames Characters.Handling;
        package DIR renames Directories;
        package CLI renames Command_Line;
        package Listeners is
                type Listener is limited new Spawn.Processes.Process_Listener with record
                        proc: Spawn.Processes.Process;
                        Started : Boolean := False;
                        Stopped : Boolean := False;
                        Error   : Integer := 0;
                end record;

                overriding procedure Started (Self: in out Listener);
                overriding procedure Finished(Self: in out Listener; Exit_Status: Spawn.Processes.Process_Exit_Status; Exit_Code: Spawn.Processes.Process_Exit_Code);
                overriding procedure Error_Occurred(Self: in out Listener; Process_Error : Integer);

        end Listeners;

        package body Listeners is
                overriding procedure Started (Self : in out Listener) is
                begin
                        Self.Started := True;
                end Started;

                overriding procedure Finished(Self: in out Listener; Exit_Status: Spawn.Processes.Process_Exit_Status; Exit_Code: Spawn.Processes.Process_Exit_Code) is
                begin
                        Self.Stopped := True;
                end Finished;

                overriding procedure Error_Occurred(Self: in out Listener; Process_Error : Integer) is
                begin
                        Ada.Text_IO.Put_Line ("Error_Occurred:" & (Process_Error'Img));
                        Self.Error := Process_Error;
                        Self.Stopped := True;
                end Error_Occurred;
        end Listeners;

        procedure printHelp(name: String) is
        begin
                IO.Put_Line("USAGE: " & name & " [OPTIONS]");
                IO.Put_Line("    -p  | --project (NAME)  find and compiles project (NAME)");
                IO.Put_Line("    -nw | --no-warn         remove warning messages");
                IO.Put_Line("    -w  | --warn            prints warning messages");
                IO.Put_Line("    -ni | --no-info         remove info messages");
                IO.Put_Line("    -i  | --info            prints info messages");
                IO.Put_Line("    -ne | --no-error        remove error messages");
                IO.Put_Line("    -e  | --error           prints error messages");
                IO.Put_Line("    -h  | --help            prints this help");
        end printHelp;
        
        procedure processFile(self: in out sourceFile; path: String; info: Boolean; error: Boolean) is
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
                                if error then
                                        IO.Put_Line("Founded Error in auto header '" & data & "' : header extension is not .h");
                                end if;
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
        
        function is_signature(self: sourceFile; signature: String) return Boolean is
                engine: constant Regpat.Pattern_Matcher := Regpat.Compile("^(((struct|enum|union|unsigned|signed)\s)?(long\s)?(long\s)?[a-zA-Z0-9_]+\s*(\s+|[*]*)\s*[a-zA-Z0-9_]+\s*[(]\s*(\s*(((struct|enum|union|unsigned|signed)\s)?(long\s)?(long\s)?\s*[a-zA-Z0-9_]+\s*(\s+|[*]*)\s*[a-zA-Z0-9_]+\s*([[]\s*[0-9]*\s*[]]\s*)?\s*[,]?\s*)*|(\s*void\s*)?)\s*[)])");
                match: Regpat.Match_Array(0 .. 1);
                line: constant String := self.prototypes.To_String;
                start: Integer := line'First;
        begin
                loop
                        Regpat.Match(engine, line(start .. line'Last), match);
                        exit when match(0) = Regpat.No_Match;
                        return True when line(match(1).First .. match(1).Last) = signature;
                        start := match(1).Last + 3;
                end loop;
                return False;
        end is_signature;
        
        procedure append(self: sourceFile; warn: Boolean) is
                f: IO.File_Type;
                name: String := self.header.To_String;
                nameCpy: constant String := name;
                engine: constant Regpat.Pattern_Matcher := Regpat.Compile("([a-zA-Z0-9_]+)\s*[(]");
                sigEngine: constant Regpat.Pattern_Matcher := Regpat.Compile("^(((struct|enum|union|unsigned|signed)\s)?(long\s)?(long\s)?[a-zA-Z0-9_]+\s*(\s+|[*]*)\s*[a-zA-Z0-9_]+\s*[(]\s*(\s*(((struct|enum|union|unsigned|signed)\s)?(long\s)?(long\s)?\s*[a-zA-Z0-9_]+\s*(\s+|[*]*)\s*[a-zA-Z0-9_]+\s*([[]\s*[0-9]*\s*[]]\s*)?\s*[,]?\s*)*|(\s*void\s*)?)\s*[)])");
                match: Regpat.Match_Array(0 .. 1);
                sigMatch: Regpat.Match_Array(0 .. 1);
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
                                                Regpat.Match(sigEngine, line, sigMatch);
                                                if sigMatch(0) = Regpat.No_Match then
                                                        IO.Put_Line ("warning: unknown signature of symbol '" & line(match(1).First .. match(1).Last) & "'");
                                                elsif not self.is_signature(line(sigMatch(1).First .. sigMatch(1).Last)) then
                                                        IO.Put_Line ("warning: overriting signature of symbol '" & line(match(1).First .. match(1).Last) & "'");
                                                end if;
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
        
        function addFile(self: in out Project; path: String) return Boolean is
                file: IO.File_Type;
                correct: Boolean := False;
        begin
                file.Open(IO.In_File, path);
                loop
                        line: constant Unbounded_String := To_Unbounded_String(file.Get_Line);
                        if self.projectName(line.To_String) then
                                correct := True;
                                exit;
                        end if;
                        exit when file.End_Of_File;
                end loop;
                file.Close;
                return False when not correct;
                self.files.Append(To_Unbounded_String(path));
                return True;
        end addFile;
        
        function projectName(self: in out Project; line: String) return Boolean is
                engine: constant Regpat.Pattern_Matcher := Regpat.Compile("[\/][\/]\s*project\s*([a-zA-Z0-9_]+)");
                match: Regpat.Match_Array(0 .. 1);
        begin
                Regpat.Match(engine, line, match);
                return False when match(0) = Regpat.No_Match;
                return True when line(match(1).First .. match(1).Last) = self.name.To_String;
                return False;
        end projectName;
        
        function Compile(self: Project; info: Boolean; err: Boolean) return CLI.Exit_Status is
                cursor: stringList.Cursor;
                args: Spawn.String_Vectors.UTF_8_String_Vector;
                L: aliased Listeners.Listener;
                error: Unbounded_String;
                data: Ada.Streams.Stream_Element_Array(1 .. 5);
                last: Ada.Streams.Stream_Element_Count;
                sucess: Boolean := True;
        begin
                if info then
                        IO.Put_Line("starting compiling project : '" & self.name.To_String & "'");
                end if;
                args.Append("-o");
                args.Append(self.name.To_String);
                cursor := self.files.First;
                loop
                        exit when cursor = stringList.No_Element;
                        if info then
                                IO.Put_Line("adding file '" & cursor.Element.To_String & "'to compilation of  project : '" & self.name.To_String & "'");
                        end if;
                        args.Append(cursor.Element.To_String);
                        cursor.Next;
                end loop;
                L.proc.Set_Program("/bin/gcc");
                L.proc.Set_Arguments(args);
                L.proc.Set_Working_Directory(DIR.Current_Directory);
                L.proc.Set_Listener(L'Unchecked_Access);
                L.proc.Start;
                while not L.Stopped loop
                        L.proc.Read_Standard_Error(data, last, sucess);
                        if last >= data'First then
                                for char of data loop
                                        Ada.Strings.Unbounded.Append(error, Character'Val(Char));
                                end loop;
                        end if;
                        L.proc.Read_Standard_Output(data, last, sucess);
                        Spawn.Processes.Monitor_Loop(0.001);
                end loop;
                if L.proc.Exit_Code /= 0 then
                        if err then
                                IO.Put_Line(error.To_String);
                                IO.Put_Line("error: compilation of project '" & self.name.To_String & "' finished with error");
                        end if;
                        return CLI.Failure;
                end if;
                if info then
                        IO.Put_Line("finished compiling project: '" & self.name.To_String & "'");
                end if;
                return CLI.Success;
        end Compile;
end Process;
