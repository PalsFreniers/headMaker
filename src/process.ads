with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Process is
        type sourceFile is record
                prototypes: Unbounded_String;
                header: Unbounded_String;
                has_header: Boolean := False;
        end record;

        function is_signature(self: sourceFile; signature: String) return Boolean;
        function headName(self: in out sourceFile; line: String) return Boolean;
        function has_symbol(self: sourceFile; symbol: String) return Boolean;
        procedure processFile(self: in out sourceFile; path: String; info: Boolean);
        procedure getPrototype(self: in out sourceFile; line: String);
        procedure append(self: sourceFile; warn: Boolean);
        procedure write(self: sourceFile);
        procedure printHelp(name: String);

        package sourceList is new Ada.Containers.Doubly_Linked_Lists(process.sourceFile);
        package stringList is new Ada.Containers.Doubly_Linked_Lists(Unbounded_String);

        type Project is record
               files: stringList.List; 
               name: Unbounded_String;
        end record;

        function addFile(self: in out Project; path: String) return Boolean;
        procedure compile(self: Project);
end Process;
