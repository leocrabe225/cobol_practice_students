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
#### FILE-CONTROL.
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
#### WORKING-STORAGE SECTION.
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

### PROCEDURE DIVISION.
```cobol
       0100-READ-INPUT-FILE.

       0200-COMPUTE-AVERAGES.

       1100-INITIALIZE-OUTPUT-LINES.

       0300-WRITE-OUTPUT-FILE.

       0400-DISPLAY-TABLE.

       STOP RUN.
```
#### 0100-READ-INPUT-FILE.
```cobol
      * Reads the input file, and stores its content in a
      * data structure
       0100-READ-INPUT-FILE-BEGIN.
           PERFORM UNTIL EOF
               READ
                   EVALUATE KEY
                       WHEN 1
                           STORE STUDENT
                       WHEN 2
                           STORE GRADE
                           STORE COURSE
                   END-EVALUATE
           END-PERFORM.
       0100-READ-INPUT-FILE-END.
```
#### 0200-COMPUTE-AVERAGES.
```cobol
      * Computes the averages, by starting two paragraphs.
       0200-COMPUTE-AVERAGES-BEGIN.
           PERFORM 0210-COMPUTE-COURSE-AVERAGE.

           PERFORM 0220-COMPUTE-STUDENT-AVERAGE.
       0200-COMPUTE-AVERAGES-END.
```
##### 0210-COMPUTE-COURSE-AVERAGE.
```cobol
      * Computes the average grade per course.
```

##### 0210-COMPUTE-STUDENT-AVERAGE.
```cobol
      * Computes the average grade per student, and the 
      * classe's average too.
```

#### 0300-WRITE-OUTPUT-FILE.
```cobol
      * Writes all the previously stored and computed data into the
      * output file.
       0300-WRITE-OUTPUT-FILE-BEGIN.
           OPEN OUTPUT F-OUTPUT.
           
           PERFORM 0500-SETUP-OUTPUT-HEADER.
           WRITE.

           PERFORM UNTIL IDX > STUDENT
               PERFORM 0600-SETUP-OUTPUT-GRADE
               WRITE.
           END-PERFORM.

           PERFORM 0700-SETUP-OUTPUT-FOOTER-1.
           WRITE.

           PERFORM UNTIL IDX > COURSE
               PERFORM 0800-SETUP-OUTPUT-COURSE
               WRITE.
           END-PERFORM.

           CLOSE F-OUTPUT.
       0300-WRITE-OUTPUT-FILE-END.
```

#### 0400-DISPLAY-TABLE.
```cobol
      * Displays all the previously stored and computed data to
      * the terminal.
```

#### 0500-SETUP-OUTPUT-HEADER.
```cobol
      * Prepares the header of the file output, which contains the
      * courses indices and column names.
```

#### 0600-SETUP-OUTPUT-GRADE.
```cobol
      * Prepares a student's output line with his name, all his
      * grades in all courses, and his average.
```

#### 0700-SETUP-OUTPUT-FOOTER-1.
```cobol
      * Prepares the first footer of the file output, which contains
      * the courses average and the class average.
```

#### 0800-SETUP-OUTPUT-COURSE.
```cobol
      * Prepares the course description output, with its index, name
      * and coefficient.
```

#### 1100-INITIALIZE-OUTPUT-LINES.
```cobol
      * Computes the output width, and centers the output texts.
       1100-INITIALIZE-OUTPUT-LINES-BEGIN.
           COMPUTE WS-OUTPUT-SIZE.

           PERFORM 1200-CENTER-TEXT(WS-OUTPUT-NOTE-REPORT)

           PERFORM 1200-CENTER-TEXT-BEGIN(WS-OUTPUT-END-NOTE-REPORT)

       1100-INITIALIZE-OUTPUT-LINES-END.
```

#### 1200-CENTER-TEXT.
```cobol
      * centers the given output texts based on the maximum line width.
```