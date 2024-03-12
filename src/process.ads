with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Process is
        type sourceFile is tagged record
                prototypes: Unbounded_String;
                header: Unbounded_String;
                has_header: Boolean := False;
        end record;

        function headName(self: in out sourceFile; line: String) return Boolean;
        function has_symbol(self: sourceFile; symbol: String) return Boolean;
        procedure getPrototype(self: in out sourceFile; line: String);
        procedure processFile(self: in out sourceFile; path: String; info: Boolean);
        procedure append(self: sourceFile; warn: Boolean);
        procedure write(self: sourceFile);
        procedure printHelp(name: String);

        package sourceList is new Ada.Containers.Doubly_Linked_Lists(process.sourceFile);
end Process;
