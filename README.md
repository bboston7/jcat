jcat
====

jcat takes multiple Java files and prints a large, legal Java file to stdout.
If separate compilation is in your way, this tool will help you get around it.

What does jcat do?
------------------
Jcat does three things when concatenating files.
1. Checks that all files belong to the same package, then creates a single
   package declaration for them.
2. Moves all import statements to the top of the output
3. Strips the public qualifier from class declarations
By performing these three actions, a legal output file is produced with all the
code from the input files!
