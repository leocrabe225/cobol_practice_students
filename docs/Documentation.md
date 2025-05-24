[Go back](..)
## Code internal structure
### IDENTIFICATION DIVISION.
```cobol
       AUTHOR. Leocrabe225.
```

### ENVIRONMENT DIVISION.
```cobol
      * Tells the program that it must read the commas as decimal
      * point in numbers.
       DECIMAL-POINT IS COMMA.
```
#### FILE SECTION.
```cobol
           SELECT F-INPUT.

           SELECT F-OUTPUT.
```

### DATA DIVISION.
#### FILE SECTION.
```cobol
       FD F-INPUT

      * Stores 1 numeric of size 2, it allows to read the line key
      * before actually looking at the fields.
       01 REC-F-INPUT-2.

      * Stores 1 alphanumeric of size 1000, it allows to process
      * the input as a whole.
       01 REC-F-INPUT-1000.

      * Dictates the stucture of the record when it needs to be
      * interpreted as a student.
       01 REC-STUDENT.

      * Dictates the stucture of the record when it needs to be
      * interpreted as a course.
       01 REC-COURSE.
```