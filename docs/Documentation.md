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

       FD F-OUTPUT.

      * Simple alphanumeric of size 250 for writing the output.
       01 REC-F-OUTPUT.
```
#### WORKING-STORAGE
```cobol
      * Flag group to handle input file status (OK / EOF).
       01  F-INPUT-STATUS.

      * Main structure, contains all student/courses related data.
       01  WS-DATA-STUDENT.
      * First table, contains courses names and coefficients.
           05 WS-COURSE.
      * Second table, dynamically sized, contains the students.
           05 WS-STUDENT.
      * Nested table, contains the grades.
               10 WS-S-GRADES. 

      * Multiple variables and groups used for output formatting.
       01 WS-OUTPUT-SPACE-LINE.
       01 WS-OUTPUT-STAR-LINE.
       01 WS-OUTPUT-NOTE-REPORT.
       01 WS-OUTPUT-END-NOTE-REPORT.
       01 WS-OUTPUT-HEADER.
       01 WS-STUDENT-OUTPUT-GRADE.
       01 WS-FOOTER-OUTPUT-1.
       01 WS-COURSE-OUTPUT.
       01 WS-STUD-AMT-OUTPUT.
       01 WS-CRS-AMT-OUTPUT.
       01 WS-GRADE-AMT-OUTPUT.
```