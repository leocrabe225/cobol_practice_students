# Cobol Practice Student
This COBOL project is an exercise to practice reading, storing, and writing sequential files.
The goal is to : 
- Read a file, which contains a record for each student and another record for each of his grade in every course.
- Store the content of that file in a structured way.
- Process the data to calculate the average per student, per course, and the class average.
- Write the newly obtained data in a properly formatted way to an output file.

Some attention was put towards input verification, such as UTF-8 characters, duplicate, or missing data.

## How to use
The attached Makefile has 3 main rules :
- `make run F=<n>` to run the program with a specific file. Example : `make run F=2` to run with `data/input-1.dat`. It will output `output/output-2.dat`.
- `make run-all` to run the program with all `data/input-*.dat` files. It will output a different output file for each input.
- `make clean` to remove the temporary input file, the binary, and the output folder and files.

## The design
As courses names and coefficient are redundant, they are stored only once in a separate table from the students.\
The grade table size isn't dynamic, as you can't nest dynamic tables in COBOL.\
100% of the file output goes through groups in the WORKING-STORAGE SECTION to allow use of fillers, which means there is no output format in the file descriptor declaration.\
Student and course averages aren't processed concurrently, as it would have needed a buffer table, which I dislike.\
The approach to handle the "Ç" is quite simple, I replace it with "C" and move the rest of the input a few characters left.\
Missing grades are replaced with "MISS" in the output, and are not counted towards the averages, so missing a grade doesn't hurt average.

## Documentation
### [Code internal structure](docs/Code_Internal_Structure.md)
### [Input / Output](docs/Input_Output.md)