       IDENTIFICATION DIVISION.
       PROGRAM-ID. students.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 21-05-2025.
       DATE-COMPILED. null.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT
               ASSIGN TO 'data/input.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-INPUT-STATUS. 

           SELECT F-OUTPUT
               ASSIGN TO 'output.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-OUTPUT-STATUS.   

       DATA DIVISION.
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.

       01  REC-F-INPUT-2       PIC 9(02).
       01  REC-F-INPUT-10      PIC X(10).
       01  REC-F-INPUT-100     PIC X(100).
       01  REC-F-INPUT-1000    PIC X(1000).

       01  REC-STUDENT.
           03 R-S-KEY          PIC 9(02).       
           03 R-LASTNAME       PIC X(07).       
           03 R-FIRSTNAME      PIC X(06).       
           03 R-AGE            PIC 9(02).       

       01  REC-COURSE.
           03 R-C-KEY          PIC 9(02).       
           03 R-LABEL          PIC X(21).       
           03 R-COEF           PIC 9,9.       
           03 R-GRADE          PIC 99,99.       

       FD  F-OUTPUT
           RECORD CONTAINS 250 CHARACTERS
           RECORDING MODE IS F.

       01  REC-F-OUTPUT        PIC X(250).

       WORKING-STORAGE SECTION.
       01  F-INPUT-STATUS      PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       01  F-OUTPUT-STATUS     PIC X(02) VALUE SPACE.
           88 F-OUTPUT-STATUS-OK    VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF   VALUE '10'.

       01  WS-DATA-STUDENT.
           05 WS-STUDENT-LGHT PIC 9(03).
           05 WS-COURSE-LGHT  PIC 9(03).
           05 WS-COURSE OCCURS 10.
               10 WS-C-NAME       PIC X(21).
               10 WS-C-COEF       PIC 9(01)V9(01).
               10 WS-C-AVERAGE    PIC 9(02)V9(02).
           05 WS-CLASS-AVERAGE    PIC 9(02)V9(02).
           05 WS-STUDENT OCCURS 1 TO 999 TIMES 
                         DEPENDING ON WS-STUDENT-LGHT.
               10 WS-S-FIRSTNAME  PIC X(06).
               10 WS-S-LASTNAME   PIC X(07).
               10 WS-S-AGE        PIC 9(02).
               10 WS-S-AVERAGE    PIC 9(02)V9(02).
               10 WS-S-GRADES OCCURS 10.
                   15 WS-S-GRADE  PIC 9(02)V9(02).  

       77 WS-IDX-1                PIC 9(04).
       77 WS-IDX-2                PIC 9(02).

      * Used to calculate individual student and course averages.
       77 WS-MATH-BUFFER          PIC 9(05)V9(02).
      * Used to count the total coefficient.
       77 WS-MATH-BUFFER-2        PIC 9(02)V9(01).
      * Used to calculate class average.
       77 WS-MATH-BUFFER-3        PIC 9(05)V9(02).

       PROCEDURE DIVISION.
           
           PERFORM 0100-READ-INPUT-FILE-BEGIN
              THRU 0100-READ-INPUT-FILE-END.

           PERFORM 0200-COMPUTE-AVERAGES-BEGIN
              THRU 0200-COMPUTE-AVERAGES-END.

           PERFORM 0400-DISPLAY-TABLE-BEGIN
              THRU 0400-DISPLAY-TABLE-END.
           
           STOP RUN.

       0100-READ-INPUT-FILE-BEGIN.
           MOVE 0 TO WS-IDX-1.
           MOVE 0 TO WS-STUDENT-LGHT.
           MOVE 0 TO WS-COURSE-LGHT.
           OPEN INPUT F-INPUT.
           PERFORM UNTIL F-INPUT-STATUS-EOF
               READ F-INPUT
                   NOT AT END
                       EVALUATE REC-F-INPUT-2
                           WHEN 1
                               ADD 1 TO WS-IDX-1
                               ADD 1 TO WS-STUDENT-LGHT
                               IF WS-IDX-2 >
                                  WS-COURSE-LGHT THEN
                                   MOVE WS-IDX-2 TO WS-COURSE-LGHT
                               END-IF
                               MOVE 0 TO WS-IDX-2
                               MOVE R-LASTNAME 
                                   TO WS-S-LASTNAME(WS-IDX-1)
                               MOVE R-FIRSTNAME 
                                   TO WS-S-FIRSTNAME(WS-IDX-1)
                               MOVE R-AGE 
                                   TO WS-S-AGE(WS-IDX-1)
                           WHEN 2
                               ADD 1 TO WS-IDX-2
                               MOVE R-LABEL TO WS-C-NAME(WS-IDX-2)
                               MOVE R-COEF TO WS-C-COEF(WS-IDX-2)
                               MOVE R-GRADE 
                                   TO WS-S-GRADE(WS-IDX-1, WS-IDX-2)
                       END-EVALUATE
               END-READ
           END-PERFORM.
           CLOSE F-INPUT.
       0100-READ-INPUT-FILE-END.

       0200-COMPUTE-AVERAGES-BEGIN.
           PERFORM 0210-COMPUTE-COURSE-AVERAGE-BEGIN
              THRU 0210-COMPUTE-COURSE-AVERAGE-END.

           PERFORM 0220-COMPUTE-STUDENT-AVERAGE-BEGIN
              THRU 0220-COMPUTE-STUDENT-AVERAGE-END.
       0200-COMPUTE-AVERAGES-END.

       0210-COMPUTE-COURSE-AVERAGE-BEGIN.
           PERFORM VARYING WS-IDX-2 FROM 1 BY 1 
                   UNTIL WS-IDX-2 > WS-COURSE-LGHT
                   MOVE 0 TO WS-MATH-BUFFER
               PERFORM VARYING WS-IDX-1 FROM 1 BY 1 
                       UNTIL WS-IDX-1 > WS-STUDENT-LGHT
                   ADD WS-S-GRADE(WS-IDX-1, WS-IDX-2) TO WS-MATH-BUFFER
               END-PERFORM
               COMPUTE WS-MATH-BUFFER ROUNDED = 
                   WS-MATH-BUFFER / WS-STUDENT-LGHT
               MOVE WS-MATH-BUFFER TO WS-C-AVERAGE(WS-IDX-2)
           END-PERFORM.
       0210-COMPUTE-COURSE-AVERAGE-END.

       0220-COMPUTE-STUDENT-AVERAGE-BEGIN.
           MOVE 0 TO WS-MATH-BUFFER-3.
           PERFORM VARYING WS-IDX-1 FROM 1 BY 1 
                       UNTIL WS-IDX-1 > WS-STUDENT-LGHT
                   MOVE 0 TO WS-MATH-BUFFER
                   MOVE 0 TO WS-MATH-BUFFER-2
               PERFORM VARYING WS-IDX-2 FROM 1 BY 1 
                   UNTIL WS-IDX-2 > WS-COURSE-LGHT
                   COMPUTE WS-MATH-BUFFER = WS-MATH-BUFFER + 
                           WS-S-GRADE(WS-IDX-1, WS-IDX-2) *
                           WS-C-COEF(WS-IDX-2)
                   ADD WS-C-COEF(WS-IDX-2) TO WS-MATH-BUFFER-2
               END-PERFORM
               COMPUTE WS-MATH-BUFFER ROUNDED = 
                   WS-MATH-BUFFER / WS-MATH-BUFFER-2
               MOVE WS-MATH-BUFFER TO WS-S-AVERAGE(WS-IDX-1)
               ADD WS-MATH-BUFFER TO WS-MATH-BUFFER-3
           END-PERFORM.
           COMPUTE WS-MATH-BUFFER-3 ROUNDED = 
                   WS-MATH-BUFFER-3 / WS-STUDENT-LGHT
           MOVE WS-MATH-BUFFER-3 TO WS-CLASS-AVERAGE.
       0220-COMPUTE-STUDENT-AVERAGE-END.

       0400-DISPLAY-TABLE-BEGIN.
           PERFORM VARYING WS-IDX-2 FROM 0 BY 1 
                   UNTIL WS-IDX-2 > WS-COURSE-LGHT + 1
               EVALUATE WS-IDX-2
                   WHEN 0
                       DISPLAY "                      COEF AVERAGE "
                           WITH NO ADVANCING
                   WHEN WS-COURSE-LGHT + 1
                       DISPLAY "             AVERAGES      "
                           WS-CLASS-AVERAGE "   "
                           WITH NO ADVANCING
                   WHEN OTHER
                       DISPLAY WS-C-NAME(WS-IDX-2) SPACE 
                           WS-C-COEF(WS-IDX-2) "  "
                           WS-C-AVERAGE(WS-IDX-2) "   "
                           WITH NO ADVANCING
               END-EVALUATE
               PERFORM VARYING WS-IDX-1 FROM 1 BY 1
                       UNTIL WS-IDX-1 > WS-STUDENT-LGHT
                   EVALUATE WS-IDX-2
                       WHEN 0
                           DISPLAY WS-S-LASTNAME(WS-IDX-1) SPACE
                               WITH NO ADVANCING
                       WHEN WS-COURSE-LGHT + 1
                           DISPLAY WS-S-AVERAGE(WS-IDX-1) "   "
                               WITH NO ADVANCING
                       WHEN OTHER
                           DISPLAY WS-S-GRADE(WS-IDX-1, WS-IDX-2) "   "
                               WITH NO ADVANCING
                   END-EVALUATE
               END-PERFORM
               DISPLAY SPACE
           END-PERFORM.
       0400-DISPLAY-TABLE-END.
