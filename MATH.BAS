DEFINT A-Z
CONST FALSE = 0
CONST TRUE = -1
CONST TURNS = 25

DECLARE FUNCTION Choose$ (x, y, Prompt$, Choices$)
DECLARE FUNCTION GetLevel ()
DECLARE FUNCTION GetName$ ()
DECLARE FUNCTION GetQuestion$ (Subject, Level, Operator$)
DECLARE FUNCTION GetRandom (AllowZero)
DECLARE FUNCTION GetSubject ()
DECLARE FUNCTION GetSubjectName$ (Subject)
DECLARE SUB Center (x, Text$)
DECLARE SUB DisplayReport ()
DECLARE SUB GetSettings ()
DECLARE SUB Menu ()
DECLARE SUB MenuBackground ()
DECLARE SUB Practice ()
DECLARE SUB SaveReport (Correct, Incorrect, Subject, Level, Name$)
DECLARE FUNCTION Trim$ (Text$)
OPTION BASE 1
ON ERROR GOTO ErrorTrap
DIM Key$
 SCREEN 0: WIDTH 80, 25: COLOR 7, 0: CLS : RANDOMIZE TIMER
 CALL Menu
 END

ErrorTrap:
 COLOR 0, 7: LOCATE 5
 SELECT CASE ERR
  CASE 7, 14
   Center 0, " Not enough memory. "
  CASE 53
   Center 0, " Cannot find Report.dat. "
  CASE 61
   Center 0, " The disk is full. "
  CASE 70
   Center 0, " Disk or file is  "
   Center 0, " write protected. "
  CASE 71
   Center 0, " No disk in diskdrive. "
  CASE ELSE
   Center 0, " Unexpected error. "
 END SELECT
 COLOR 7, 0: Center 0, " Escape = Quit   Enter = Try Again "
 DO
  Key$ = INPUT$(1)
  IF Key$ = CHR$(13) THEN RESUME
  IF Key$ = CHR$(27) THEN COLOR 7, 0: CLS : SYSTEM
 LOOP

SUB Center (Row, Text$)
 LOCATE , INT(40 - (LEN(Text$) / 2))
 IF Row > 0 THEN LOCATE Row
 PRINT Text$
END SUB

FUNCTION Choose$ (x, y, Prompt$, Choices$)
DIM Choice$

 LOCATE x, y: PRINT Prompt$
 DO
  Choice$ = UCASE$(INPUT$(1))
 LOOP UNTIL INSTR(Choices$, Choice$) > 0
 LOCATE x, y: PRINT SPACE$(LEN(Prompt$))

 Choose$ = Choice$
END FUNCTION

SUB DisplayReport ()
DIM Cell$
DIM DisplayPage
DIM Item
DIM Item$(5)
DIM Key$

 Item$(1) = "Correct:"
 Item$(2) = "Incorrect:"
 Item$(3) = "Subject:"
 Item$(4) = "Level:"
 Item$(5) = "Name:"

 OPEN "Report.dat" FOR APPEND AS 1: CLOSE 1
 OPEN "Report.dat" FOR INPUT AS 1
  DisplayPage = TRUE
  DO
   IF DisplayPage THEN
    COLOR 7, 0: CLS : LOCATE 2, 1
    DO UNTIL EOF(1) OR CSRLIN >= 23
     FOR Item = 1 TO 5
      INPUT #1, Cell$
      Cell$ = Trim$(Cell$)
      PRINT " "; Cell$; SPACE$(15 - LEN(Cell$));
     NEXT Item
     PRINT
    LOOP

    DisplayPage = FALSE
   END IF

   COLOR 0, 7: LOCATE 1, 1
   FOR Item = 1 TO 5
    PRINT " "; Item$(Item); SPACE$(15 - LEN(Item$(Item)));
   NEXT Item

   LOCATE 23, 1: PRINT "   Enter = Next Page  Home = Top  Delete = Erase report "; SPACE$(23);

   DO
    Key$ = INKEY$
   LOOP WHILE Key$ = ""
 
   SELECT CASE Key$
    CASE CHR$(13)
     IF NOT EOF(1) THEN DisplayPage = TRUE
    CASE CHR$(27)
     EXIT DO
    CASE CHR$(0) + "G"
     SEEK #1, 1
     DisplayPage = TRUE
    CASE CHR$(0) + "S"
     COLOR 7, 0
     IF Choose$(23, 3, " Delete report Y/N? ", "YN") = "Y" THEN
      CLOSE 1
      OPEN "Report.dat" FOR OUTPUT AS 1: CLOSE 1
      OPEN "Report.dat" FOR INPUT AS 1
      DisplayPage = TRUE
     END IF
   END SELECT
  LOOP
 CLOSE 1
END SUB

FUNCTION GetLevel ()
DIM Key$
DIM Level

 PRINT
 PRINT "   Level (1-9):"

 Level = 0
 DO
  Key$ = INPUT$(1)
  IF Key$ = CHR$(27) THEN
   Level = 0
   EXIT DO
  ELSEIF INSTR("123456789", Key$) > 0 THEN
   Level = VAL(Key$)
   EXIT DO
  END IF
 LOOP

 GetLevel = Level
END FUNCTION

FUNCTION GetName$ ()
DIM Key$
DIM Length
DIM Name$

 CLS
 DO
  Length = LEN(Name$)
  COLOR 7: LOCATE 2, 3: PRINT "Name: "; Name$;
  COLOR 23: PRINT "_ ";
  DO
   Key$ = INKEY$
  LOOP WHILE Key$ = ""
  SELECT CASE Key$
   CASE CHR$(8)
    IF Length > 0 THEN Name$ = LEFT$(Name$, Length - 1)
   CASE CHR$(13)
    LOCATE 2, Length + 9: PRINT " "
    EXIT DO
   CASE CHR$(27)
    Name$ = ""
    EXIT DO
   CASE ELSE
    IF Length < 10 THEN
     IF Key$ = " " OR (Key$ >= "A" AND Key$ <= "Z") OR (Key$ >= "a" AND Key$ <= "z") THEN
      Name$ = Name$ + Key$
     END IF
    END IF
   END SELECT
 LOOP

 GetName$ = Name$
END FUNCTION

FUNCTION GetQuestion$ (Subject, Level, Operator$)
DIM Answer
DIM Number1
DIM Number2

 SELECT CASE Subject
  CASE 1: Operator$ = "+"
  CASE 2: Operator$ = "-"
  CASE 3: Operator$ = "x"
  CASE 4: Operator$ = ":"
  CASE 5, 6: Operator$ = ""
 END SELECT

 DO
  SELECT CASE Subject
   CASE 1, 2, 3
    Number1 = GetRandom(TRUE) * Level
    Number2 = GetRandom(TRUE) * Level

    IF Subject = 2 AND Number1 - Number2 >= 0 THEN
     EXIT DO
    ELSEIF NOT Subject = 2 THEN
     EXIT DO
    END IF
   CASE 4, 5, 6
    Number1 = GetRandom(FALSE) * (Level * 2)
    Number2 = GetRandom(FALSE) * (Level * 2)

    IF Subject = 4 AND Number1 MOD Number2 = 0 THEN
     EXIT DO
    ELSEIF NOT Subject = 4 THEN
     EXIT DO
    END IF
  END SELECT
 LOOP

 SELECT CASE Subject
  CASE 1: Answer = Number1 + Number2
  CASE 2: Answer = Number1 - Number2
  CASE 3: Answer = Number1 * Number2
  CASE 4: Answer = Number1 / Number2
  CASE 5: Answer = Number1 * Number2
  CASE 6: Answer = (Number1 + Number2) * 2
 END SELECT

 GetQuestion$ = STR$(Number1) + STR$(Number2) + STR$(Answer)
END FUNCTION

FUNCTION GetRandom (AllowZero)
 IF AllowZero THEN GetRandom = INT(RND * 11) ELSE GetRandom = INT(RND * 10) + 1
END FUNCTION

FUNCTION GetSubject ()
DIM Key$
DIM Subject

 COLOR 7, 0: LOCATE 4, 3: PRINT "Choice (1-6):"
 FOR Subject = 1 TO 6
  LOCATE , 4: PRINT Subject; "= "; GetSubjectName$(Subject)
 NEXT Subject

 DO
  Key$ = INPUT$(1)
  IF Key$ = CHR$(27) THEN
   Subject = 0
   EXIT DO
  ELSEIF INSTR("123456", Key$) > 0 THEN
   Subject = VAL(Key$)
   EXIT DO
  END IF
 LOOP

 GetSubject = Subject
END FUNCTION

FUNCTION GetSubjectName$ (Subject)
DIM Name$

 SELECT CASE Subject
  CASE 1: Name$ = "Addition"
  CASE 2: Name$ = "Subtraction"
  CASE 3: Name$ = "Multiplication"
  CASE 4: Name$ = "Division"
  CASE 5: Name$ = "Surface area"
  CASE 6: Name$ = "Circumference"
 END SELECT

GetSubjectName$ = Name$
END FUNCTION

SUB Menu ()
DIM CursorX
DIM Item
DIM Item$(3)
DIM Key$

 CursorX = 1
 Item$(1) = " Practice "
 Item$(2) = " Report   "
 Item$(3) = " Quit     "
 
 MenuBackground

 DO
  FOR Item = 1 TO 3
   IF Item = CursorX THEN COLOR 0, 7 ELSE COLOR 7, 0
   LOCATE (Item * 2) + 3, 10: PRINT Item$(Item)
  NEXT Item

  DO
   Key$ = INKEY$
  LOOP WHILE Key$ = ""

  SELECT CASE Key$
   CASE CHR$(0) + "H"
    IF CursorX = 1 THEN CursorX = 3 ELSE CursorX = CursorX - 1
   CASE CHR$(0) + "P"
    IF CursorX = 3 THEN CursorX = 1 ELSE CursorX = CursorX + 1
   CASE CHR$(13)
    IF CursorX = 1 THEN Practice
    IF CursorX = 2 THEN DisplayReport
    IF CursorX = 3 THEN COLOR 7, 0: CLS : SYSTEM
    MenuBackground
  END SELECT
 LOOP
END SUB

SUB MenuBackground ()
DIM Row

 LOCATE 1, 1
 COLOR 7, 0: PRINT "Ú"; STRING$(31, "Ä"); "´";
 COLOR 0, 7: PRINT " Mathematics  ";
 COLOR 7, 0: PRINT "Ã"; STRING$(31, "Ä"); "¿"
 FOR Row = 1 TO 21
  PRINT "³"; SPC(78); "³"
 NEXT Row
 PRINT "À"; STRING$(78, "Ä"); "Ù"
 LOCATE 20, 5: PRINT "Mathematics v1.01, by: Peter Swinkels, 1996"
END SUB

SUB Practice ()
DIM Answer$
DIM Correct
DIM GivenAnswer$
DIM Incorrect
DIM Key$
DIM Length
DIM Level
DIM Name$
DIM Number1$
DIM Number2$
DIM Operator$
DIM Question$
DIM Subject
DIM Turn

 Name$ = GetName$
 IF Name$ = "" THEN EXIT SUB
 Subject = GetSubject
 IF Subject = 0 THEN EXIT SUB
 Level = GetLevel
 IF Level = 0 THEN EXIT SUB

 CLS
 Length = 38 - (LEN(GetSubjectName$(Subject)) + 2) \ 2
 LOCATE 1, 1
 COLOR 7, 0: PRINT STRING$(Length, "Ä") + "´";
 COLOR 0, 7: PRINT " "; GetSubjectName$(Subject); " ";
 COLOR 7, 0: PRINT "Ã"; STRING$(Length, "Ä")

 Question$ = Trim$(GetQuestion$(Subject, Level, Operator$))
 Number1$ = LEFT$(Question$, INSTR(Question$, " "))
 Number2$ = MID$(Question$, LEN(Number1$), INSTR(LEN(Number1$), Question$, " "))
 Answer$ = Trim$(MID$(Question$, LEN(Number1$ + Number2$)))
 DO
  Number1$ = Trim$(Number1$)
  Number2$ = Trim$(Number2$)
  Length = LEN(GivenAnswer$)
  LOCATE 2, 1
  PRINT USING "  Correct: ###  Incorrect: ###  Turn: ###/###"; Correct; Incorrect; Turn; TURNS
  PRINT STRING$(80, "Ä")
  LOCATE 21, 1
  PRINT STRING$(80, "Ä")
  PRINT USING "  Name: \          \   Level: ###"; Name$; Level
  PRINT STRING$(80, "Ä")
  LOCATE 5, 5
  IF Subject < 5 THEN
   PRINT Number1$; " "; Operator$; " "; Number2$; " = "; GivenAnswer$;
  ELSEIF Subject = 5 THEN
   PRINT "An area of "; Number1$; " by "; Number2$; " metres. Surface area? "; GivenAnswer$;
  ELSEIF Subject = 6 THEN
   PRINT "A surface of "; Number1$; " by "; Number2$; " metres. The circumference? "; GivenAnswer$;
  END IF
  PRINT SPC(15);
  DO
   Key$ = INKEY$
  LOOP WHILE Key$ = ""
  SELECT CASE Key$
   CASE CHR$(8)
    IF LEN(GivenAnswer$) > 0 THEN GivenAnswer$ = LEFT$(GivenAnswer$, Length - 1)
   CASE CHR$(13)
    IF Turn = TURNS THEN
     SaveReport Correct, Incorrect, Subject, Level, Name$
     EXIT DO
    ELSE
     IF GivenAnswer$ = Answer$ THEN
      Correct = Correct + 1
     ELSE
      SOUND 37, 5: Incorrect = Incorrect + 1
     END IF
     GivenAnswer$ = ""
     Turn = Turn + 1
     Question$ = LTRIM$(GetQuestion$(Subject, Level, Operator$))
     Number1$ = LEFT$(Question$, INSTR(Question$, " "))
     Number2$ = MID$(Question$, LEN(Number1$), INSTR(LEN(Number1$), Question$, " "))
     Answer$ = Trim$(MID$(Question$, LEN(Number1$ + Number2$)))
    END IF
   CASE CHR$(27)
    EXIT DO
   CASE ELSE
    IF Trim$(STR$(VAL(Key$))) = Key$ THEN
     IF Length < 11 THEN GivenAnswer$ = GivenAnswer$ + Key$
    END IF
  END SELECT
 LOOP
END SUB

SUB SaveReport (Correct, Incorrect, Subject, Level, Name$)
 OPEN "Report.dat" FOR APPEND AS 1
  PRINT #1, Correct; ",";
  PRINT #1, Incorrect; ",";
  PRINT #1, GetSubjectName$(Subject); ",";
  PRINT #1, Level; ",";
  PRINT #1, Name$
 CLOSE 1
END SUB

FUNCTION Trim$ (Text$)
 TRIM$ = LTRIM$(RTRIM$(Text$))
END FUNCTION

