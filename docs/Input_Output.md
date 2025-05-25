[Go back](..)
## Input / Output
### Input
The input is a file, formatted the following way :
```dat
01[STUDENT-NAME][AGE]
02[COURSE-NAME]        [COEF][GRADE]
02[COURSE-NAME]        [COEF][GRADE]
02[COURSE-NAME]        [COEF][GRADE]
01[STUDENT-NAME][AGE]
02[COURSE-NAME]        [COEF][GRADE]
02[COURSE-NAME]        [COEF][GRADE]
02[COURSE-NAME]        [COEF][GRADE]
```
As you can see, there are 2 types of records :
- The first one starts by the key "01", it holds the student's name, first name, and his age.
- The second one starts by the key "02", it holds the course's name, its coefficient, and the corresponding grade.

Every course line refers to the previous student, which mean that each time you read a new student, the following grades will be his.

### Output
The output is stored in a file, formatted the following way :
```dat
**********************************************************
                       NOTES REPORT
**********************************************************
NAME    FNAME  AVERAGE C01   C02   C03   C04   C05   C06

ASSIN   MARC   09,65   10,00 07,25 06,75 13,00 11,00 09,50
BONOT   JEAN   12,18   11,50 13,00 16,00 10,00 12,00 10,00
INGALE  MARTHE 10,71   15,00 09,00 08,75 14,00 06,50 10,00
INNIC   MARTHE 10,71   15,00 09,00 08,75 14,00 06,50 10,00
HOMALY  ANNE   13,82   10,00 11,00 15,50 18,00 14,50 10,00
MANTORT GERARD 14,47   10,00 11,00 18,75 17,75 12,50 11,00

CLASS          11,92   11,92 10,04 12,42 14,46 10,50 10,08
**********************************************************
C01 => COEF: 1,0 LABEL: SYSTEME D INFORMATION
C02 => COEF: 1,0 LABEL: PROGRAMMATION VBA
C03 => COEF: 2,0 LABEL: SQL
C04 => COEF: 2,0 LABEL: SCHEMA RELATIONNEL
C05 => COEF: 1,0 LABEL: RESEAUX INFORMATIQUES
C06 => COEF: 1,5 LABEL: MODELISATION
**********************************************************
STUDENTS => 06
COURSES  => 06
GRADES   => 36
**********************************************************
                        END REPORT

```
There is :
- A title.
- A header with columns names and courses indices.
- Each student has his own line, with his average and his grade per course.
- A footer with the whole class average and the average per course.
- Each course has its own line, with its index, coefficient and name.
- A small recap, with a count of the students, courses, and grades.

You can note that the "*" separating lines fit the size of the report exactly, that's by design.\
The titles are centered too.