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
               ASSIGN TO 'output/output.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL.   

       DATA DIVISION.
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.

       01  REC-F-INPUT-2       PIC 9(02).
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

       01 WS-OUTPUT-SIZE             PIC 9(03).

       01 WS-OUTPUT-SPACE-LINE       PIC X(250) VALUE ALL SPACE.
       01 WS-OUTPUT-STAR-LINE        PIC X(250) VALUE ALL "*".
       01 WS-OUTPUT-NOTE-REPORT      PIC X(250) VALUE "NOTES REPORT".
       01 WS-OUTPUT-END-NOTE-REPORT  PIC X(250) VALUE "END REPORT".

       01 WS-OUTPUT-HEADER.
           05 FILLER                 PIC X(07) VALUE "NAME".
           05 FILLER                 PIC X(01) VALUE SPACE.
           05 FILLER                 PIC X(06) VALUE "FNAME".
           05 FILLER                 PIC X(01) VALUE SPACE.
           05 FILLER                 PIC X(07) VALUE "AVERAGE".
           05 FILLER                 PIC X(01) VALUE SPACE.
           05 WS-HDR-CRS-OUTPUT OCCURS 1 TO 10 TIMES 
                                DEPENDING ON WS-COURSE-LGHT.
               10 FILLER             PIC X(01) VALUE "C".
               10 WS-HDR-CRS-OUT-NBR PIC 9(02).
               10 FILLER             PIC X(03) VALUE SPACE.
       
       01 WS-STUDENT-OUTPUT-GRADE.
           05 WS-STUD-OUT-NAME       PIC X(07).
           05 FILLER                 PIC X(01) VALUE SPACE.
           05 WS-STUD-OUT-FNAME      PIC X(06).
           05 FILLER                 PIC X(01) VALUE SPACE.
           05 WS-STUD-OUT-AVG        PIC 9(02),9(02).
           05 FILLER                 PIC X(03) VALUE SPACE.
           05 WS-STUD-OUT OCCURS 1 TO 10 TIMES 
                                DEPENDING ON WS-COURSE-LGHT.
               10 WS-STUD-OUT-GRADE  PIC 9(02),9(02).
               10 FILLER             PIC X(01) VALUE SPACE.

       01 WS-FOOTER-OUTPUT-1.
           05 FILLER                 PIC X(07) VALUE "CLASS".
           05 FILLER                 PIC X(01) VALUE SPACE.
           05 FILLER                 PIC X(06) VALUE SPACE.
           05 FILLER                 PIC X(01) VALUE SPACE.
           05 WS-OUT-CLASS-AVG       PIC 9(02),9(02).
           05 FILLER                 PIC X(03) VALUE SPACE.
           05 WS-COURSE-OUT OCCURS 1 TO 10 TIMES 
                                DEPENDING ON WS-COURSE-LGHT.
               10 WS-COURSE-OUT-AVG  PIC 9(02),9(02).
               10 FILLER             PIC X(01) VALUE SPACE.

       01 WS-COURSE-OUTPUT.
           05 FILLER                 PIC X(01) VALUE "C".
           05 WS-CRS-OUT-NBR         PIC 9(02).
           05 FILLER                 PIC X(04) VALUE " => ".
           05 FILLER                 PIC X(06) VALUE "COEF: ".
           05 WS-CRS-OUT-COEF        PIC 9,9.
           05 FILLER                 PIC X(08) VALUE " LABEL: ".
           05 WS-CRS-OUT-NAME        PIC X(21).

       01 WS-STUD-AMT-OUTPUT.
           05 FILLER                 PIC X(08) VALUE "STUDENTS".
           05 FILLER                 PIC X(04) VALUE " => ".
           05 WS-STUD-AMT-OUT        PIC 9(02).

       01 WS-CRS-AMT-OUTPUT.
           05 FILLER                 PIC X(08) VALUE "COURSES".
           05 FILLER                 PIC X(04) VALUE " => ".
           05 WS-CRS-AMT-OUT         PIC 9(02).

       01 WS-GRADE-AMT-OUTPUT.
           05 FILLER                 PIC X(08) VALUE "GRADES".
           05 FILLER                 PIC X(04) VALUE " => ".
           05 WS-GRADE-AMT-OUT       PIC 9(02).
           

       77 WS-IDX-1                PIC 9(04).
       77 WS-IDX-2                PIC 9(02).

       77 WS-STUDENT-ID           PIC 9(04).

       77 WS-COURSE-ID            PIC 9(02).

      * Used to calculate individual student and course averages.
       77 WS-MATH-BUFFER          PIC 9(05)V9(02).
      * Used to count the total coefficient.
       77 WS-MATH-BUFFER-2        PIC 9(02)V9(01).
      * Used to calculate class average.
       77 WS-MATH-BUFFER-3        PIC 9(05)V9(02).

       77 WS-CENTER-BUFFER        PIC X(80).
       77 WS-CENTER-BUFFER-2      PIC X(80).

       77 WS-INT-MATH-BUFFER      PIC 9(03).
       77 WS-INT-MATH-BUFFER-2    PIC 9(03).

       PROCEDURE DIVISION.
           
           PERFORM 0100-READ-INPUT-FILE-BEGIN
              THRU 0100-READ-INPUT-FILE-END.

           PERFORM 0200-COMPUTE-AVERAGES-BEGIN
              THRU 0200-COMPUTE-AVERAGES-END.

           PERFORM 1100-INITIALIZE-OUTPUT-LINES-BEGIN
              THRU 1100-INITIALIZE-OUTPUT-LINES-END.

           PERFORM 0300-WRITE-OUTPUT-FILE-BEGIN
              THRU 0300-WRITE-OUTPUT-FILE-END.

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

       0300-WRITE-OUTPUT-FILE-BEGIN.
           OPEN OUTPUT F-OUTPUT.
           
           MOVE WS-OUTPUT-STAR-LINE(1:WS-OUTPUT-SIZE) TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.
           MOVE WS-OUTPUT-NOTE-REPORT TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.
           MOVE WS-OUTPUT-STAR-LINE(1:WS-OUTPUT-SIZE) TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

           PERFORM 0500-SETUP-OUTPUT-HEADER-BEGIN
              THRU 0500-SETUP-OUTPUT-HEADER-END.
           MOVE WS-OUTPUT-HEADER TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

           MOVE WS-OUTPUT-SPACE-LINE(1:WS-OUTPUT-SIZE) TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

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

           PERFORM 0700-SETUP-OUTPUT-FOOTER-1-BEGIN
              THRU 0700-SETUP-OUTPUT-FOOTER-1-END.
           MOVE WS-FOOTER-OUTPUT-1 TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT.

           MOVE WS-OUTPUT-STAR-LINE(1:WS-OUTPUT-SIZE) TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

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

           MOVE WS-STUD-AMT-OUTPUT TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

           MOVE WS-CRS-AMT-OUTPUT TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

           MOVE WS-GRADE-AMT-OUTPUT TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

           MOVE WS-OUTPUT-STAR-LINE(1:WS-OUTPUT-SIZE) TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

           MOVE WS-OUTPUT-END-NOTE-REPORT TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

           CLOSE F-OUTPUT.
       0300-WRITE-OUTPUT-FILE-END.

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

       0500-SETUP-OUTPUT-HEADER-BEGIN.
           PERFORM VARYING WS-IDX-1 FROM 1 BY 1
                   UNTIL WS-IDX-1 > WS-COURSE-LGHT
               MOVE WS-IDX-1 TO WS-HDR-CRS-OUT-NBR(WS-IDX-1)
           END-PERFORM.
       0500-SETUP-OUTPUT-HEADER-END.

       0600-SETUP-OUTPUT-GRADE-BEGIN.
           MOVE WS-S-LASTNAME(WS-STUDENT-ID) TO WS-STUD-OUT-NAME.
           MOVE WS-S-FIRSTNAME(WS-STUDENT-ID) TO WS-STUD-OUT-FNAME.
           MOVE WS-S-AVERAGE(WS-STUDENT-ID) TO WS-STUD-OUT-AVG.

           PERFORM VARYING WS-IDX-2 FROM 1 BY 1
                   UNTIL WS-IDX-2 > WS-COURSE-LGHT
               MOVE WS-S-GRADE(WS-STUDENT-ID, WS-IDX-2)
                   TO WS-STUD-OUT-GRADE(WS-IDX-2)
           END-PERFORM.
       0600-SETUP-OUTPUT-GRADE-END.

       0700-SETUP-OUTPUT-FOOTER-1-BEGIN.
           MOVE WS-CLASS-AVERAGE TO WS-OUT-CLASS-AVG.

           PERFORM VARYING WS-IDX-2 FROM 1 BY 1
                   UNTIL WS-IDX-2 > WS-COURSE-LGHT
               MOVE WS-C-AVERAGE(WS-IDX-2)
                   TO WS-COURSE-OUT-AVG(WS-IDX-2)
           END-PERFORM.
       0700-SETUP-OUTPUT-FOOTER-1-END.

       0800-SETUP-OUTPUT-COURSE-BEGIN.
           MOVE WS-COURSE-ID TO WS-CRS-OUT-NBR.
           MOVE WS-C-COEF(WS-COURSE-ID) TO WS-CRS-OUT-COEF.
           MOVE WS-C-NAME(WS-COURSE-ID) TO WS-CRS-OUT-NAME.
       0800-SETUP-OUTPUT-COURSE-END.

       1100-INITIALIZE-OUTPUT-LINES-BEGIN.
           IF LENGTH OF WS-STUDENT-OUTPUT-GRADE - 1 > 
              LENGTH OF WS-OUTPUT-SIZE THEN
               COMPUTE WS-OUTPUT-SIZE = 
                       LENGTH OF WS-STUDENT-OUTPUT-GRADE - 1
           ELSE
               MOVE LENGTH OF WS-COURSE-OUTPUT TO WS-OUTPUT-SIZE
           END-IF.

           MOVE WS-STUDENT-LGHT TO WS-STUD-AMT-OUT.
           MOVE WS-COURSE-LGHT TO WS-CRS-AMT-OUT.
           COMPUTE WS-GRADE-AMT-OUT = WS-COURSE-LGHT * WS-STUDENT-LGHT.

           MOVE WS-OUTPUT-NOTE-REPORT TO WS-CENTER-BUFFER.
           PERFORM 1200-CENTER-TEXT-BEGIN
              THRU 1200-CENTER-TEXT-END.
           MOVE WS-CENTER-BUFFER TO WS-OUTPUT-NOTE-REPORT.

           MOVE WS-OUTPUT-END-NOTE-REPORT TO WS-CENTER-BUFFER.
           PERFORM 1200-CENTER-TEXT-BEGIN
              THRU 1200-CENTER-TEXT-END.
           MOVE WS-CENTER-BUFFER TO WS-OUTPUT-END-NOTE-REPORT.
       1100-INITIALIZE-OUTPUT-LINES-END.

       1200-CENTER-TEXT-BEGIN.
           MOVE WS-OUTPUT-SIZE TO WS-IDX-1.
           PERFORM UNTIL WS-IDX-1 EQUAL 0
                   OR WS-CENTER-BUFFER(WS-IDX-1:1) NOT EQUAL SPACE
               SUBTRACT 1 FROM WS-IDX-1
           END-PERFORM.
           MOVE WS-IDX-1 TO WS-INT-MATH-BUFFER.
           COMPUTE WS-INT-MATH-BUFFER-2 = (-WS-INT-MATH-BUFFER / 2) + 
                   (WS-OUTPUT-SIZE / 2) + 1.
           MOVE SPACE TO WS-CENTER-BUFFER-2.
           MOVE WS-CENTER-BUFFER(1:WS-IDX-1) 
               TO WS-CENTER-BUFFER-2(WS-INT-MATH-BUFFER-2:WS-IDX-1).
           MOVE WS-CENTER-BUFFER-2 TO WS-CENTER-BUFFER.
       1200-CENTER-TEXT-END.
