package Process is
        procedure processFile(self: in out sourceFile; path: String);
        function headName(line: String) return String;
end Process;
