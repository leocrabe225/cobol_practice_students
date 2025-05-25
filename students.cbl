      * Parses a file which contains students, courses and grades,
      * processes the data into students, courses and class averages,
      * and then output a properly formatted report to a file.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. students.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 21-05-2025.
       DATE-COMPILED. null.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
      * Makes the program read commas as the decimal point in numbers
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Select input file, line sequential as we read line by line,
      * file read status is handled here.
           SELECT F-INPUT
               ASSIGN TO 'data/input.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-INPUT-STATUS. 

      * Select output file, line sequential too.
           SELECT F-OUTPUT
               ASSIGN TO 'output/output.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL.   

       DATA DIVISION.
       FILE SECTION.
      * Input file descriptor, contains 2 to 1000 characters, depending
      * on whether you read only the key, or the input as a whole.
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.

      * Allows to read only the record key, to identify the type of
      * record.
       01  REC-F-INPUT-2       PIC 9(02).
      * Allows to manipulate the input as a whole, for example for UTF-8
      * handling.
       01  REC-F-INPUT-1000    PIC X(1000).

      * Describes the student record, the key is always "01".
       01  REC-STUDENT.
           03 R-S-KEY          PIC 9(02).       
           03 R-LASTNAME       PIC X(07).       
           03 R-FIRSTNAME      PIC X(06).       
           03 R-AGE            PIC 9(02).       

      * Describes the course record, the key is always "02".
       01  REC-COURSE.
           03 R-C-KEY          PIC 9(02).       
           03 R-LABEL          PIC X(21).       
           03 R-COEF           PIC 9,9.       
           03 R-GRADE          PIC X(5).       

      * Output file descriptor.
       FD  F-OUTPUT
           RECORD CONTAINS 250 CHARACTERS
           RECORDING MODE IS F.

      * Simple X(250) variable, the formatting will be done in the
      * output groups of the WORKING-STORAGE.
       01  REC-F-OUTPUT        PIC X(250).

       WORKING-STORAGE SECTION.
      * Flags for read status check, allows to stop when the file is
      * completely read.
       01  F-INPUT-STATUS      PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

      * Main program structure, it holds pretty much everything
      * important.
       01  WS-DATA-STUDENT.
      * Size of the Student table.
           05 WS-STUDENT-LGHT PIC 9(03).
      * Amount of courses.
           05 WS-COURSE-LGHT  PIC 9(03).
      * Course table, holds a name, a coef, and the course average.
           05 WS-COURSE OCCURS 10.
               10 WS-C-NAME       PIC X(21).
               10 WS-C-COEF       PIC 9(01)V9(01).
               10 WS-C-AVERAGE    PIC 9(02)V9(02).
      * Class average, unique.
           05 WS-CLASS-AVERAGE    PIC 9(02)V9(02).
      * Student table, depends on WS-STUDENT-LGHT, it holds name, age,
      * average, grade amount and a table for the grades themselves.
           05 WS-STUDENT OCCURS 1 TO 999 TIMES 
                         DEPENDING ON WS-STUDENT-LGHT.
               10 WS-S-FIRSTNAME  PIC X(06).
               10 WS-S-LASTNAME   PIC X(07).
               10 WS-S-AGE        PIC 9(02).
               10 WS-S-AVERAGE    PIC 9(02)V9(02).
               10 WS-S-GRADE-AMT  PIC 9(02).
      * The grades table.
               10 WS-S-GRADES OCCURS 10.
      * The grade itself.
                   15 WS-S-GRADE  PIC 9(02)V9(02).  
      * Flag to indicate whether the grade is already fullfilled
      * or not. Allows for special output when a grade is missing, as
      * you cannot just compare with 0 (it's filthy).
                   15 WS-S-GRADE-STATUS PIC 9(01) VALUE 1.
                       88 WS-S-GRADE-OK           VALUE 0.
                       88 WS-S-GRADE-MISSING      VALUE 1.

      * Width of the output report.
       01 WS-OUTPUT-SIZE             PIC 9(03).

      * Multiple lines for output formatting.
       01 WS-OUTPUT-SPACE-LINE       PIC X(250) VALUE ALL SPACE.
       01 WS-OUTPUT-STAR-LINE        PIC X(250) VALUE ALL "*".
       01 WS-OUTPUT-NOTE-REPORT      PIC X(250) VALUE "NOTES REPORT".
       01 WS-OUTPUT-END-NOTE-REPORT  PIC X(250) VALUE "END REPORT".

      * The output header, it holds the columns names and the courses
      * indices.
       01 WS-OUTPUT-HEADER.
           05 FILLER                 PIC X(07) VALUE "NAME".
           05 FILLER                 PIC X(01) VALUE SPACE.
           05 FILLER                 PIC X(06) VALUE "FNAME".
           05 FILLER                 PIC X(01) VALUE SPACE.
           05 FILLER                 PIC X(07) VALUE "AVERAGE".
           05 FILLER                 PIC X(01) VALUE SPACE.
      * The size is dynamic depending on the amount of courses.
           05 WS-HDR-CRS-OUTPUT OCCURS 1 TO 10 TIMES 
                                DEPENDING ON WS-COURSE-LGHT.
               10 FILLER             PIC X(01) VALUE "C".
               10 WS-HDR-CRS-OUT-NBR PIC 9(02).
               10 FILLER             PIC X(03) VALUE SPACE.
       
      * The actual output body, with the grades and names of the
      * students.
       01 WS-STUDENT-OUTPUT-GRADE.
           05 WS-STUD-OUT-NAME       PIC X(07).
           05 FILLER                 PIC X(01) VALUE SPACE.
           05 WS-STUD-OUT-FNAME      PIC X(06).
           05 FILLER                 PIC X(01) VALUE SPACE.
           05 WS-STUD-OUT-AVG        PIC X(05).
           05 FILLER                 PIC X(03) VALUE SPACE.
      * The size is dynamic depending on the amount of courses.
           05 WS-STUD-OUT OCCURS 1 TO 10 TIMES 
                                DEPENDING ON WS-COURSE-LGHT.
               10 WS-STUD-OUT-GRADE  PIC X(05).
               10 FILLER             PIC X(01) VALUE SPACE.

      * The output footer, it holds the class average and the average
      * of each student.
       01 WS-FOOTER-OUTPUT-1.
           05 FILLER                 PIC X(07) VALUE "CLASS".
           05 FILLER                 PIC X(01) VALUE SPACE.
           05 FILLER                 PIC X(06) VALUE SPACE.
           05 FILLER                 PIC X(01) VALUE SPACE.
           05 WS-OUT-CLASS-AVG       PIC X(05).
           05 FILLER                 PIC X(03) VALUE SPACE.
      * The size is dynamic depending on the amount of courses.
           05 WS-COURSE-OUT OCCURS 1 TO 10 TIMES 
                                DEPENDING ON WS-COURSE-LGHT.
               10 WS-COURSE-OUT-AVG  PIC 9(02),9(02).
               10 FILLER             PIC X(01) VALUE SPACE.

      * Course output, that holds its index, coef and name.
       01 WS-COURSE-OUTPUT.
           05 FILLER                 PIC X(01) VALUE "C".
           05 WS-CRS-OUT-NBR         PIC 9(02).
           05 FILLER                 PIC X(04) VALUE " => ".
           05 FILLER                 PIC X(06) VALUE "COEF: ".
           05 WS-CRS-OUT-COEF        PIC 9,9.
           05 FILLER                 PIC X(08) VALUE " LABEL: ".
           05 WS-CRS-OUT-NAME        PIC X(21).

      * Student count output.
       01 WS-STUD-AMT-OUTPUT.
           05 FILLER                 PIC X(08) VALUE "STUDENTS".
           05 FILLER                 PIC X(04) VALUE " => ".
           05 WS-STUD-AMT-OUT        PIC 9(02).

      * Course count output.
       01 WS-CRS-AMT-OUTPUT.
           05 FILLER                 PIC X(08) VALUE "COURSES".
           05 FILLER                 PIC X(04) VALUE " => ".
           05 WS-CRS-AMT-OUT         PIC 9(02).

      * Grade count output.
       01 WS-GRADE-AMT-OUTPUT.
           05 FILLER                 PIC X(08) VALUE "GRADES".
           05 FILLER                 PIC X(04) VALUE " => ".
           05 WS-GRADE-AMT-OUT       PIC 9(02).
           
      * Indices.
       77 WS-IDX-1                PIC 9(04).
       77 WS-IDX-2                PIC 9(02).

      * Special index for UTF removal.
       77 WS-IDX-UTF              PIC 9(04).

       77 WS-STUDENT-ID           PIC 9(04).

       77 WS-COURSE-ID            PIC 9(02).

      * 1400P = 1400 - Parameter.
       77 WS-1400P-COURSE-NAME    PIC X(21).
       77 WS-1400P-COURSE-COEF    PIC 9(01)V9(01).
       77 WS-IDX-1400             PIC 9(02).

      * 1500P = 1500 - Parameter.
      * 1500R = 1500 - Return value.
       77 WS-1500P-STUDENT-NAME    PIC X(07).
       77 WS-1500P-STUDENT-FNAME   PIC X(06).
       01 WS-1500R-IS-DUPLICATA    PIC 9(01).
           88 WS-1500R-DUPLICATA-YES         VALUE 1.
           88 WS-1500R-DUPLICATA-NO          VALUE 0.
       77 WS-IDX-1500             PIC 9(02).

      * Amount of present student in a course, useful for calculating
      * the course average.
       77 WS-PRESENT-STUDENT      PIC 9(04).

      * Used to calculate individual student and course averages.
       77 WS-MATH-BUFFER          PIC 9(05)V9(02).
      * Used to count the total coefficient.
       77 WS-MATH-BUFFER-2        PIC 9(02)V9(01).
      * Used to calculate class average.
       77 WS-MATH-BUFFER-3        PIC 9(05)V9(02).

      * Buffers used to center output lines.
       77 WS-CENTER-BUFFER        PIC X(250).
       77 WS-CENTER-BUFFER-2      PIC X(250).

      * Math buffers used to calculate line center positions.
       77 WS-INT-MATH-BUFFER      PIC 9(03).
       77 WS-INT-MATH-BUFFER-2    PIC 9(03).

      * Buffer used to go from numeric to alphanumeric with decimal
      * points numbers.
       77 WS-WRITE-NUM-BUFFER     PIC 99,99.

      * Buffer used to compute the size of the move after removing
      * a utf character.
       77 WS-UTF-SIZE             PIC 9(04).

       PROCEDURE DIVISION.
           
      * Read.
           PERFORM 0100-READ-INPUT-FILE-BEGIN
              THRU 0100-READ-INPUT-FILE-END.

      * Compute.
           PERFORM 0200-COMPUTE-AVERAGES-BEGIN
              THRU 0200-COMPUTE-AVERAGES-END.

      * Initialize output.
           PERFORM 1100-INITIALIZE-OUTPUT-LINES-BEGIN
              THRU 1100-INITIALIZE-OUTPUT-LINES-END.

      * Write output.
           PERFORM 0300-WRITE-OUTPUT-FILE-BEGIN
              THRU 0300-WRITE-OUTPUT-FILE-END.

      * Display to terminal.
           PERFORM 0400-DISPLAY-TABLE-BEGIN
              THRU 0400-DISPLAY-TABLE-END.
           
           STOP RUN.

      * Reads the input file, and stores its content in a
      * data structure
       0100-READ-INPUT-FILE-BEGIN.
      * Initializing student and course size
           MOVE 0 TO WS-STUDENT-LGHT.
           MOVE 0 TO WS-COURSE-LGHT.
      * Input file opening.
           OPEN INPUT F-INPUT.
      * Read until EOF file is set by FILE STATUS.
           PERFORM UNTIL F-INPUT-STATUS-EOF
               READ F-INPUT
                   NOT AT END
      * Starts the UTF-8 Replacing paragraph.
                       PERFORM 1300-REPLACE-UTF-8-BEGIN
                          THRU 1300-REPLACE-UTF-8-END
      * Evaluating the key
                       EVALUATE REC-F-INPUT-2
                           WHEN 1
      * Move values to paragraph 1500 parameters.
                               MOVE R-LASTNAME 
                                   TO WS-1500P-STUDENT-NAME
                               MOVE R-FIRSTNAME 
                                   TO WS-1500P-STUDENT-FNAME
      * Getting current student index, and making the table bigger if
      * need be.
                               PERFORM 1500-GET-STUDENT-INDEX-BEGIN
                                  THRU 1500-GET-STUDENT-INDEX-END
                               IF WS-1500R-DUPLICATA-NO THEN
      * If no duplicate, just store the names, age and initialize the
      * amount of grades.
                                   MOVE 0 TO WS-S-GRADE-AMT
                                             (WS-STUDENT-ID)
                                   MOVE R-LASTNAME 
                                       TO WS-S-LASTNAME(WS-STUDENT-ID)
                                   MOVE R-FIRSTNAME 
                                       TO WS-S-FIRSTNAME(WS-STUDENT-ID)
                                   MOVE R-AGE 
                                       TO WS-S-AGE(WS-STUDENT-ID)
                               ELSE
      * If duplicate student, let the user know.
                                   DISPLAY "Duplicate student "
                                       R-LASTNAME
                               END-IF
                           WHEN 2
      * Move values to paragraph 1400 parameters.
                               MOVE R-LABEL TO WS-1400P-COURSE-NAME
                               MOVE R-COEF TO  WS-1400P-COURSE-COEF
      * Getting current course index, adding 1 to the course-lgth if
      * needed.
                               PERFORM 1400-GET-COURSE-INDEX-BEGIN
                                  THRU 1400-GET-COURSE-INDEX-END
                               IF WS-S-GRADE-MISSING
                                  (WS-STUDENT-ID, WS-COURSE-ID) THEN
                                   IF R-GRADE NOT EQUAL SPACE THEN
      * If the grade is not fullfilled, store the value, and set the
      * grade-ok flag (and add 1 to grade-amt).
                                       MOVE R-GRADE 
                                           TO WS-S-GRADE
                                           (WS-STUDENT-ID, WS-COURSE-ID)
                                       SET WS-S-GRADE-OK
                                           (WS-STUDENT-ID, WS-COURSE-ID)
                                           TO TRUE
                                       ADD 1 TO WS-S-GRADE-AMT
                                                (WS-STUDENT-ID)
                                   ELSE
      * If the grade field is empty, not storing the grade, letting
      * the user know
                                       DISPLAY "Missing grade number "
                                           "for " 
                                           WS-S-LASTNAME(WS-STUDENT-ID)
                                           " in " R-LABEL
                                   END-IF
                               ELSE
      * If the grade of that course for that student is not missing
      * it is a duplicata, letting the user know.
                                   DISPLAY "Duplicate grade for "
                                           WS-S-LASTNAME(WS-STUDENT-ID)
                                           " in " R-LABEL
                               END-IF
                       END-EVALUATE
               END-READ
           END-PERFORM.
      * CLOSE THE FILE.
           CLOSE F-INPUT.
       0100-READ-INPUT-FILE-END.

      * Computes the averages, by starting two paragraphs.
       0200-COMPUTE-AVERAGES-BEGIN.
           PERFORM 0210-COMPUTE-COURSE-AVERAGE-BEGIN
              THRU 0210-COMPUTE-COURSE-AVERAGE-END.

           PERFORM 0220-COMPUTE-STUDENT-AVERAGE-BEGIN
              THRU 0220-COMPUTE-STUDENT-AVERAGE-END.
       0200-COMPUTE-AVERAGES-END.

      * Computes the average grade per course.
       0210-COMPUTE-COURSE-AVERAGE-BEGIN.
      * Simply iterates over the bi-dimensional table with 2 loops.
           PERFORM VARYING WS-IDX-2 FROM 1 BY 1 
                   UNTIL WS-IDX-2 > WS-COURSE-LGHT
      * Re-initialize the math buffer after (before) each calculation.
               MOVE 0 TO WS-MATH-BUFFER
               MOVE 0 TO WS-PRESENT-STUDENT
               PERFORM VARYING WS-IDX-1 FROM 1 BY 1 
                       UNTIL WS-IDX-1 > WS-STUDENT-LGHT
      * Addition to math-buffer and present-student ONLY if there is a
      * grade, so absent grades aren't counted for the average
                   IF WS-S-GRADE-OK(WS-IDX-1, WS-IDX-2) THEN
                    ADD WS-S-GRADE(WS-IDX-1, WS-IDX-2) TO WS-MATH-BUFFER
                    ADD 1 TO WS-PRESENT-STUDENT
                   END-IF
               END-PERFORM
      * Actual average calculation, SUM / AMOUNT.
               COMPUTE WS-MATH-BUFFER ROUNDED = 
                   WS-MATH-BUFFER / WS-PRESENT-STUDENT
               MOVE WS-MATH-BUFFER TO WS-C-AVERAGE(WS-IDX-2)
           END-PERFORM.
       0210-COMPUTE-COURSE-AVERAGE-END.

      * Computes the average grade per student, and the 
      * classe's average too.
       0220-COMPUTE-STUDENT-AVERAGE-BEGIN.
      * Initialize class average buffers.
           MOVE 0 TO WS-MATH-BUFFER-3.
           MOVE 0 TO WS-PRESENT-STUDENT
           PERFORM VARYING WS-IDX-1 FROM 1 BY 1 
                   UNTIL WS-IDX-1 > WS-STUDENT-LGHT
      * Re-initialize the student math buffer after (before) 
      * each calculation.
               MOVE 0 TO WS-MATH-BUFFER
               MOVE 0 TO WS-MATH-BUFFER-2
               PERFORM VARYING WS-IDX-2 FROM 1 BY 1 
                   UNTIL WS-IDX-2 > WS-COURSE-LGHT
      * Addition only if grade is there again.
                   IF WS-S-GRADE-OK(WS-IDX-1, WS-IDX-2) THEN
                       COMPUTE WS-MATH-BUFFER = WS-MATH-BUFFER + 
                               WS-S-GRADE(WS-IDX-1, WS-IDX-2) *
                               WS-C-COEF(WS-IDX-2)
                       ADD WS-C-COEF(WS-IDX-2) TO WS-MATH-BUFFER-2
                   END-IF
               END-PERFORM
      * Avoiding division per 0, and not counting students without
      * grades for the class average.
      * Also computes the student average.
               IF WS-S-GRADE-AMT(WS-IDX-1) NOT EQUAL 0 THEN 
                   COMPUTE WS-MATH-BUFFER ROUNDED = 
                       WS-MATH-BUFFER / WS-MATH-BUFFER-2
                   MOVE WS-MATH-BUFFER TO WS-S-AVERAGE(WS-IDX-1)
                   ADD WS-MATH-BUFFER TO WS-MATH-BUFFER-3
                   ADD 1 TO WS-PRESENT-STUDENT
               END-IF
           END-PERFORM.
      * Actual class average computation, SUM(AVERAGES) / STUD-AMT.
           COMPUTE WS-MATH-BUFFER-3 ROUNDED = 
                   WS-MATH-BUFFER-3 / WS-PRESENT-STUDENT.
           MOVE WS-MATH-BUFFER-3 TO WS-CLASS-AVERAGE.
       0220-COMPUTE-STUDENT-AVERAGE-END.

      * Writes all the previously stored and computed data into the
      * output file.
       0300-WRITE-OUTPUT-FILE-BEGIN.
      * Open output file.
           OPEN OUTPUT F-OUTPUT.
           
      * Title surrounded by star lines.
           MOVE WS-OUTPUT-STAR-LINE(1:WS-OUTPUT-SIZE) TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.
           MOVE WS-OUTPUT-NOTE-REPORT TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.
           MOVE WS-OUTPUT-STAR-LINE(1:WS-OUTPUT-SIZE) TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

      * Header followed by an empty line.
           PERFORM 0500-SETUP-OUTPUT-HEADER-BEGIN
              THRU 0500-SETUP-OUTPUT-HEADER-END.
           MOVE WS-OUTPUT-HEADER TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

           MOVE WS-OUTPUT-SPACE-LINE(1:WS-OUTPUT-SIZE) TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

      * Loop to write all the students, followed by an empty line.
           PERFORM VARYING WS-IDX-1 FROM 1 BY 1
                   UNTIL WS-IDX-1 > WS-STUDENT-LGHT
               MOVE WS-IDX-1 TO WS-STUDENT-ID
               PERFORM 0600-SETUP-OUTPUT-GRADE-BEGIN
                  THRU 0600-SETUP-OUTPUT-GRADE-END
               MOVE WS-STUDENT-OUTPUT-GRADE TO REC-F-OUTPUT
               WRITE REC-F-OUTPUT
           END-PERFORM.

           MOVE WS-OUTPUT-SPACE-LINE(1:WS-OUTPUT-SIZE) TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

      * Footer followed by a star line.
           PERFORM 0700-SETUP-OUTPUT-FOOTER-1-BEGIN
              THRU 0700-SETUP-OUTPUT-FOOTER-1-END.
           MOVE WS-FOOTER-OUTPUT-1 TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT.

           MOVE WS-OUTPUT-STAR-LINE(1:WS-OUTPUT-SIZE) TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

      * Loop to write all the courses, followed by a star line.
           PERFORM VARYING WS-IDX-1 FROM 1 BY 1
                   UNTIL WS-IDX-1 > WS-COURSE-LGHT
               MOVE WS-IDX-1 TO WS-COURSE-ID
               PERFORM 0800-SETUP-OUTPUT-COURSE-BEGIN
                  THRU 0800-SETUP-OUTPUT-COURSE-END
               MOVE WS-COURSE-OUTPUT TO REC-F-OUTPUT
               WRITE REC-F-OUTPUT
           END-PERFORM.

           MOVE WS-OUTPUT-STAR-LINE(1:WS-OUTPUT-SIZE) TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

      * Writes student amount.
           MOVE WS-STUD-AMT-OUTPUT TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

      * Writes courses amount.
           MOVE WS-CRS-AMT-OUTPUT TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

      * Writes grades amount.
           MOVE WS-GRADE-AMT-OUTPUT TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

      * End report note,surroundded by star lines.
           MOVE WS-OUTPUT-STAR-LINE(1:WS-OUTPUT-SIZE) TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

           MOVE WS-OUTPUT-END-NOTE-REPORT TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

           MOVE WS-OUTPUT-STAR-LINE(1:WS-OUTPUT-SIZE) TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

      * CLOSE THE FILE.
           CLOSE F-OUTPUT.
       0300-WRITE-OUTPUT-FILE-END.

      * Displays all the previously stored and computed data to
      * the terminal.
       0400-DISPLAY-TABLE-BEGIN.
      * Again, two loops for two dimensions. You will note that the
      * first one starts at 0 and ends at + 1, it is for the header
      * and the footer, which also need to iterate on the students.
           PERFORM VARYING WS-IDX-2 FROM 0 BY 1 
                   UNTIL WS-IDX-2 > WS-COURSE-LGHT + 1
               EVALUATE WS-IDX-2
      * When 0, header.
                   WHEN 0
                       DISPLAY "                      COEF AVERAGE "
                           WITH NO ADVANCING
      * When +1, footer.
                   WHEN WS-COURSE-LGHT + 1
                       IF WS-COURSE-LGHT NOT EQUAL 0 THEN
                           DISPLAY "             AVERAGES      "
                               WS-CLASS-AVERAGE "   "
                               WITH NO ADVANCING
                       ELSE
                           DISPLAY "             AVERAGES      "
                           "MISS" "    "
                           WITH NO ADVANCING
                       END-IF
      * When other, body.
                   WHEN OTHER
                       DISPLAY WS-C-NAME(WS-IDX-2) SPACE 
                           WS-C-COEF(WS-IDX-2) "  "
                           WS-C-AVERAGE(WS-IDX-2) "   "
                           WITH NO ADVANCING
               END-EVALUATE
      * Loop for writing the grades, or the names if it's the header.
               PERFORM VARYING WS-IDX-1 FROM 1 BY 1
                       UNTIL WS-IDX-1 > WS-STUDENT-LGHT
                   EVALUATE WS-IDX-2
      * When 0, header.
                       WHEN 0
                           DISPLAY WS-S-LASTNAME(WS-IDX-1) SPACE
                               WITH NO ADVANCING
      * When +1, footer.
                       WHEN WS-COURSE-LGHT + 1
                           IF WS-S-GRADE-AMT(WS-IDX-1) NOT EQUAL 0 THEN
                               DISPLAY WS-S-AVERAGE(WS-IDX-1) "   "
                                   WITH NO ADVANCING
                           ELSE
                               DISPLAY "MISS    "
                                      WITH NO ADVANCING
                           END-IF
      * When other, body.
                       WHEN OTHER
                           IF WS-S-GRADE-OK(WS-IDX-1, WS-IDX-2) THEN
                               DISPLAY WS-S-GRADE(WS-IDX-1, WS-IDX-2)
                                      "   " 
                                      WITH NO ADVANCING
                           ELSE
                               DISPLAY "MISS    "
                                      WITH NO ADVANCING
                           END-IF
                   END-EVALUATE
               END-PERFORM
      * Line break.
               DISPLAY SPACE
           END-PERFORM.
       0400-DISPLAY-TABLE-END.

      * Prepares the header of the file output, which contains the
      * courses indices and column names.
       0500-SETUP-OUTPUT-HEADER-BEGIN.
      * Loops until course-lgth to write C01, C02, C03... as indices.
           PERFORM VARYING WS-IDX-1 FROM 1 BY 1
                   UNTIL WS-IDX-1 > WS-COURSE-LGHT
               MOVE WS-IDX-1 TO WS-HDR-CRS-OUT-NBR(WS-IDX-1)
           END-PERFORM.
       0500-SETUP-OUTPUT-HEADER-END.

      * Prepares a student's output line with his name, all his
      * grades in all courses, and his average.
       0600-SETUP-OUTPUT-GRADE-BEGIN.
      * Moves names to output.
           MOVE WS-S-LASTNAME(WS-STUDENT-ID) TO WS-STUD-OUT-NAME.
           MOVE WS-S-FIRSTNAME(WS-STUDENT-ID) TO WS-STUD-OUT-FNAME.
      * Checks whether there is at least 1 grade, if not, writes "MISS"
      * instead.
           IF WS-S-GRADE-AMT(WS-STUDENT-ID) NOT EQUAL 0 THEN
               MOVE WS-S-AVERAGE(WS-STUDENT-ID) TO WS-WRITE-NUM-BUFFER
               MOVE WS-WRITE-NUM-BUFFER TO WS-STUD-OUT-AVG
           ELSE
               MOVE "MISS" TO WS-STUD-OUT-AVG
           END-IF.

      * Then loops on all grades for a student, writes "MISS" if one is 
      * missing.
           PERFORM VARYING WS-IDX-2 FROM 1 BY 1
                   UNTIL WS-IDX-2 > WS-COURSE-LGHT
               IF WS-S-GRADE-OK(WS-STUDENT-ID, WS-IDX-2) THEN
                   MOVE WS-S-GRADE(WS-STUDENT-ID, WS-IDX-2)
                       TO WS-WRITE-NUM-BUFFER
                   MOVE WS-WRITE-NUM-BUFFER
                       TO WS-STUD-OUT-GRADE(WS-IDX-2)
               ELSE
                   MOVE "MISS" TO WS-STUD-OUT-GRADE(WS-IDX-2)
               END-IF
           END-PERFORM.
       0600-SETUP-OUTPUT-GRADE-END.

      * Prepares the first footer of the file output, which contains
      * the courses average and the class average.
       0700-SETUP-OUTPUT-FOOTER-1-BEGIN.
      * If there are no courses, the class average is replaced by
      * "MISS".
           IF WS-COURSE-LGHT NOT EQUAL 0 THEN
               MOVE WS-CLASS-AVERAGE TO WS-WRITE-NUM-BUFFER
               MOVE WS-WRITE-NUM-BUFFER TO WS-OUT-CLASS-AVG
           ELSE
               MOVE "MISS" TO WS-OUT-CLASS-AVG
           END-IF

      * Moves all the course averages to the output line.
           PERFORM VARYING WS-IDX-2 FROM 1 BY 1
                   UNTIL WS-IDX-2 > WS-COURSE-LGHT
               MOVE WS-C-AVERAGE(WS-IDX-2)
                   TO WS-COURSE-OUT-AVG(WS-IDX-2)
           END-PERFORM.
       0700-SETUP-OUTPUT-FOOTER-1-END.

      * Prepares the course description output, with its index, name
      * and coefficient.
       0800-SETUP-OUTPUT-COURSE-BEGIN.
           MOVE WS-COURSE-ID TO WS-CRS-OUT-NBR.
           MOVE WS-C-COEF(WS-COURSE-ID) TO WS-CRS-OUT-COEF.
           MOVE WS-C-NAME(WS-COURSE-ID) TO WS-CRS-OUT-NAME.
       0800-SETUP-OUTPUT-COURSE-END.

      * Computes the output width, and centers the output texts.
       1100-INITIALIZE-OUTPUT-LINES-BEGIN.
      * Calculates the output width, depending on the biggest line
      * between the course line, and the grades lines.
      * If there are not many courses, the course lines will be wider.
           IF LENGTH OF WS-STUDENT-OUTPUT-GRADE - 1 > 
              LENGTH OF WS-OUTPUT-SIZE THEN
               COMPUTE WS-OUTPUT-SIZE = 
                       LENGTH OF WS-STUDENT-OUTPUT-GRADE - 1
           ELSE
               MOVE LENGTH OF WS-COURSE-OUTPUT TO WS-OUTPUT-SIZE
           END-IF.

           MOVE WS-STUDENT-LGHT TO WS-STUD-AMT-OUT.
           MOVE WS-COURSE-LGHT TO WS-CRS-AMT-OUT.

      * Counting actual grades.
           MOVE 0 TO WS-GRADE-AMT-OUT.
           PERFORM VARYING WS-IDX-1 FROM 1 BY 1
                   UNTIL WS-IDX-1 > WS-STUDENT-LGHT
               ADD WS-S-GRADE-AMT(WS-IDX-1) TO WS-GRADE-AMT-OUT
           END-PERFORM.

      * Centering title.
           MOVE WS-OUTPUT-NOTE-REPORT TO WS-CENTER-BUFFER.
           PERFORM 1200-CENTER-TEXT-BEGIN
              THRU 1200-CENTER-TEXT-END.
           MOVE WS-CENTER-BUFFER TO WS-OUTPUT-NOTE-REPORT.

      * Centering end text.
           MOVE WS-OUTPUT-END-NOTE-REPORT TO WS-CENTER-BUFFER.
           PERFORM 1200-CENTER-TEXT-BEGIN
              THRU 1200-CENTER-TEXT-END.
           MOVE WS-CENTER-BUFFER TO WS-OUTPUT-END-NOTE-REPORT.
       1100-INITIALIZE-OUTPUT-LINES-END.

      * centers the given output texts based on the maximum line width.
       1200-CENTER-TEXT-BEGIN.
      * Checks the actual width of the text by starting from the end
      * and stopping when a character isn't a space .
           MOVE WS-OUTPUT-SIZE TO WS-IDX-1.
           PERFORM UNTIL WS-IDX-1 EQUAL 0
                   OR WS-CENTER-BUFFER(WS-IDX-1:1) NOT EQUAL SPACE
               SUBTRACT 1 FROM WS-IDX-1
           END-PERFORM.
           MOVE WS-IDX-1 TO WS-INT-MATH-BUFFER.
      * Computing text position based on line and text width.
           COMPUTE WS-INT-MATH-BUFFER-2 = (-WS-INT-MATH-BUFFER / 2) + 
                   (WS-OUTPUT-SIZE / 2) + 1.
           MOVE SPACE TO WS-CENTER-BUFFER-2.
           MOVE WS-CENTER-BUFFER(1:WS-IDX-1) 
               TO WS-CENTER-BUFFER-2(WS-INT-MATH-BUFFER-2:WS-IDX-1).
           MOVE WS-CENTER-BUFFER-2 TO WS-CENTER-BUFFER.
       1200-CENTER-TEXT-END.

      * Replaces all the "Ç" by "C", and then moves the rest of the
      * string, as "Ç" is not the same size as "C".
       1300-REPLACE-UTF-8-BEGIN.
           PERFORM VARYING WS-IDX-UTF FROM 1 BY 1
                   UNTIL WS-IDX-UTF > LENGTH OF REC-F-INPUT-1000
      * Apparently, only 2 works for the size, which I find odd (funny).
               IF REC-F-INPUT-1000(WS-IDX-UTF:2) EQUAL "Ç"
                   MOVE "C" TO REC-F-INPUT-1000(WS-IDX-UTF:1)
      * Computes the size of the part that needs moving.
                   COMPUTE WS-UTF-SIZE = LENGTH OF REC-F-INPUT-1000 -
                           WS-IDX-UTF - 2
                   MOVE REC-F-INPUT-1000(WS-IDX-UTF + 2:WS-UTF-SIZE)
                       TO REC-F-INPUT-1000(WS-IDX-UTF + 1: WS-UTF-SIZE)
               END-IF
           END-PERFORM.
       1300-REPLACE-UTF-8-END.

      * Looks for the course name in the course table, if it is already
      * present, just "returns" its index, if it isn't, it makes room
      * for the new course in the table, and return the last index.
       1400-GET-COURSE-INDEX-BEGIN.
           MOVE 0 TO WS-COURSE-ID.
           PERFORM VARYING WS-IDX-1400 FROM 1 BY 1
                   UNTIL WS-IDX-1400 > WS-COURSE-LGHT
               IF WS-C-NAME(WS-IDX-1400) EQUAL WS-1400P-COURSE-NAME
                   MOVE WS-IDX-1400 TO WS-COURSE-ID
               END-IF
           END-PERFORM.
           IF WS-COURSE-ID EQUAL 0 THEN
               ADD 1 TO WS-COURSE-LGHT
               MOVE WS-COURSE-LGHT TO WS-COURSE-ID
               MOVE WS-1400P-COURSE-NAME TO WS-C-NAME(WS-COURSE-ID)
               MOVE WS-1400P-COURSE-COEF TO WS-C-COEF(WS-COURSE-ID)
           END-IF.
       1400-GET-COURSE-INDEX-END.

      * Looks for the student name/fname pair in the student table, if
      * it is already present, it "returns" its index, while setting
      * the WS-1500R-DUPLICATA-YES flag to TRUE, if it isn't, it makes
      * room for the new student in the table, and returns the last
      * index.
       1500-GET-STUDENT-INDEX-BEGIN.
           SET WS-1500R-DUPLICATA-NO TO TRUE.
           MOVE 0 TO WS-STUDENT-ID.
           PERFORM VARYING WS-IDX-1500 FROM 1 BY 1
                   UNTIL WS-IDX-1500 > WS-STUDENT-LGHT
               IF WS-S-LASTNAME(WS-IDX-1500)
                  EQUAL WS-1500P-STUDENT-NAME AND
                  WS-S-FIRSTNAME(WS-IDX-1500) 
                  EQUAL WS-1500P-STUDENT-FNAME
                   MOVE WS-IDX-1500 TO WS-STUDENT-ID
                   SET WS-1500R-DUPLICATA-YES TO TRUE
               END-IF
           END-PERFORM
           IF WS-STUDENT-ID EQUAL 0 THEN
               ADD 1 TO WS-STUDENT-LGHT
               MOVE WS-STUDENT-LGHT TO WS-STUDENT-ID
           END-IF.
       1500-GET-STUDENT-INDEX-END.
