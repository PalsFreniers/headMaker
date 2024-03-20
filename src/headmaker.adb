with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with process;
use Ada.Strings.Unbounded;
use process.sourceList;


procedure Headmaker is
        package IO renames Ada.Text_IO;
        package DIR renames Ada.Directories;
        package CLI renames Ada.Command_Line;

        search: DIR.Search_Type;
        dirEntry: DIR.Directory_Entry_Type;
        curDir: constant String := DIR.Current_Directory;
        sources: process.sourceList.List;
        sourcesCursor: process.sourceList.Cursor;
        headers: process.sourceList.List;
        headersCursor: process.sourceList.Cursor;
        project: process.Project;
        info: Boolean := False;
        warn: Boolean := True;
        error: Boolean := True;
        computeProject: Boolean := False;
begin
        for i in 1 .. CLI.Argument_Count loop
                if (i = 1) then
                        if CLI.Argument(i) = "--no-warn" then
                                warn := False;
                        elsif CLI.Argument(i) = "--warn" then
                                warn := True;
                        elsif CLI.Argument(i) = "-nw" then
                                warn := False;
                        elsif CLI.Argument(i) = "-w" then
                                warn := True;
                        elsif CLI.Argument(i) = "--no-info" then
                                info := False;
                        elsif CLI.Argument(i) = "--info" then
                                info := True;
                        elsif CLI.Argument(i) = "-ni" then
                                info := False;
                        elsif CLI.Argument(i) = "-i" then
                                info := True;
                        elsif CLI.Argument(i) = "--no-error" then
                                error := False;
                        elsif CLI.Argument(i) = "--error" then
                                error := True;
                        elsif CLI.Argument(i) = "-ne" then
                                error := False;
                        elsif CLI.Argument(i) = "-e" then
                                error := True;
                        elsif CLI.Argument(i) = "-p" then
                                if CLI.Argument_Count < i + 1 then
                                        process.printHelp(CLI.Command_Name);
                                        IO.Put_Line ("Unknown argument: '" & CLI.Argument(i) & "'");
                                        return;
                                end if;
                                project.name := To_Unbounded_String(CLI.Argument(i + 1));
                                computeProject := True;
                        elsif CLI.Argument(i) = "--project" then
                                if CLI.Argument_Count < i + 1 then
                                        process.printHelp(CLI.Command_Name);
                                        IO.Put_Line ("Unknown argument: '" & CLI.Argument(i) & "'");
                                        return;
                                end if;
                                project.name := To_Unbounded_String(CLI.Argument(i + 1));
                                computeProject := True;
                        elsif (CLI.Argument(i) = "-h") or (CLI.Argument(i) = "--help") then
                                process.printHelp(CLI.Command_Name);
                                return;
                        else
                                process.printHelp(CLI.Command_Name);
                                IO.Put_Line ("Unknown argument: '" & CLI.Argument(i) & "'");
                                return;
                        end if;
                elsif ((CLI.Argument(i - 1) /= "-p") and (CLI.Argument(i - 1) /= "--project")) then
                        if CLI.Argument(i) = "--no-warn" then
                                warn := False;
                        elsif CLI.Argument(i) = "--warn" then
                                warn := True;
                        elsif CLI.Argument(i) = "-nw" then
                                warn := False;
                        elsif CLI.Argument(i) = "-w" then
                                warn := True;
                        elsif CLI.Argument(i) = "--no-info" then
                                info := False;
                        elsif CLI.Argument(i) = "--info" then
                                info := True;
                        elsif CLI.Argument(i) = "-ni" then
                                info := False;
                        elsif CLI.Argument(i) = "-i" then
                                info := True;
                        elsif CLI.Argument(i) = "--no-error" then
                                error := False;
                        elsif CLI.Argument(i) = "--error" then
                                error := True;
                        elsif CLI.Argument(i) = "-ne" then
                                error := False;
                        elsif CLI.Argument(i) = "-e" then
                                error := True;
                        elsif CLI.Argument(i) = "-p" then
                                if CLI.Argument_Count < i + 1 then
                                        process.printHelp(CLI.Command_Name);
                                        IO.Put_Line ("Unknown argument: '" & CLI.Argument(i) & "'");
                                        return;
                                end if;
                                project.name := To_Unbounded_String(CLI.Argument(i + 1));
                                computeProject := True;
                        elsif CLI.Argument(i) = "--project" then
                                if CLI.Argument_Count < i + 1 then
                                        process.printHelp(CLI.Command_Name);
                                        IO.Put_Line ("Unknown argument: '" & CLI.Argument(i) & "'");
                                        return;
                                end if;
                                project.name := To_Unbounded_String(CLI.Argument(i + 1));
                                computeProject := True;
                        elsif (CLI.Argument(i) = "-h") or (CLI.Argument(i) = "--help") then
                                process.printHelp(CLI.Command_Name);
                                return;
                        else
                                process.printHelp(CLI.Command_Name);
                                IO.Put_Line ("Unknown argument: '" & CLI.Argument(i) & "'");
                                return;
                        end if;
                end if;
        end loop;
        DIR.Start_Search(search, curDir, "*.c");
        loop
                exit when not DIR.More_Entries(search);
                DIR.Get_Next_Entry(search, dirEntry);
                case dirEntry.Kind is
                        when DIR.Ordinary_File =>
                                if not computeProject then
                                        file: process.sourceFile;
                                        file.processFile(dirEntry.Full_Name, info, error);
                                        sources.Append(file);
                                else
                                        if project.addFile(dirEntry.Full_Name) then
                                                file: process.sourceFile;
                                                file.processFile(dirEntry.Full_Name, info, error);
                                                sources.Append(file);
                                        end if;
                                end if;
                        when others => null;
                end case;
        end loop;
        DIR.End_Search(search);
        if sources.First = process.sourceList.No_Element then
                if warn then
                        IO.Put_Line("No .c Files founded");
                end if;
                return;
        end if;
        sourcesCursor := sources.First;
        loop
                exit when sourcesCursor = process.sourceList.No_Element;
                if not sourcesCursor.Element.has_header then
                        sources.Delete(sourcesCursor);
                end if;
                sourcesCursor.Next;
        end loop;
        sourcesCursor := sources.First;
        headersCursor := headers.First;
        if headersCursor = process.sourceList.No_Element then
                headers.Append(sourcesCursor.Element);
                sourcesCursor.Next;
        end if;
        loop
                exit when sourcesCursor = process.sourceList.No_Element;
                if sourcesCursor.Element.has_header then
                        headersCursor := headers.First;
                        loop
                                exit when headersCursor = process.sourceList.No_Element;
                                exit when headersCursor.Element.header = sourcesCursor.Element.header;
                                headersCursor.Next;
                        end loop;
                        if headersCursor = process.sourceList.No_Element then
                                headers.Append(sourcesCursor.Element);
                        else
                                header: process.sourceFile := headersCursor.Element;
                                header.prototypes := headersCursor.Element.prototypes & sourcesCursor.Element.prototypes;
                                headers.Replace_Element(headersCursor, header);
                        end if;
                end if;
                sourcesCursor.Next;
        end loop;
        headersCursor := headers.First;
        loop
                exit when headersCursor = process.sourceList.No_Element;
                head: constant process.sourceFile := headersCursor.Element;
                if DIR.Exists(head.header.To_String) then
                        head.append(warn);
                else
                        head.write;
                end if;
                headersCursor.Next;
        end loop;
        if info then
                IO.Put_Line("Finished writing all headers");
        end if;
        if computeProject then
                CLI.Set_Exit_Status(project.Compile(info, error));
        end if;
end Headmaker;
