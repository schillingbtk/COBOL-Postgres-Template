       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBPG.
       AUTHOR. THOMAS SCHILLING.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * cobc -Xref -P -Tcobpg.txt --tlines=0
      * --tsymbols -lpq -x cobpg.cbl 
       77 WERT1 PIC X(12).
       77 WERT2 PIC X(25).
       77 WERT3 PIC X(20).
       77 neuezeile pic x value x'00'.
       01 pgconn USAGE POINTER.
       01 pgres  USAGE POINTER.
       01 resptr USAGE POINTER.
       01 resstr PICTURE x(80) based.
       01 tupl PIC 9(6).
       01 nfield PIC 9(6).
       01 sqlstr PIC x(250).
       01 zeile PIC 9(6) VALUE 0.
       01 spalte PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
       CALL "PQconnectdb" USING
       BY REFERENCE "user = xxx" & x"00"
       BY REFERENCE "password = yyy" & x"00"
       BY REFERENCE "dbname = zzz" & x"00"
       RETURNING pgconn
       ON EXCEPTION
           DISPLAY
           "Fehler. lpq fehlt?"
           UPON syserr
           END-DISPLAY
           STOP RUN RETURNING 1
       END-CALL.
       IF pgconn EQUAL NULL THEN
           DISPLAY
           "Fehler. DB nicht erreichbar" 
           UPON syserr 
           END-DISPLAY
           STOP RUN RETURNING 1
       END-IF.
       STRING
           "SELECT a,b,c " DELIMITED BY SIZE
           "FROM tabelle;" DELIMITED BY SIZE
           x"00"
       INTO SQLSTR
       END-STRING.
       CALL "PQexec" USING
           BY VALUE pgconn
           BY REFERENCE SQLSTR
           RETURNING pgres
       END-CALL.
       CALL "PQntuples" USING
           BY VALUE pgres
           RETURNING tupl
       END-CALL
       CALL "PQnfields" USING
           BY VALUE pgres
           RETURNING nfield
       END-CALL
       DISPLAY "Wir haben ",tupl," Zeilen"
       DISPLAY neuezeile
       DISPLAY "Zeilen haben ",nfield," Spalten"
       DISPLAY neuezeile
       PERFORM VARYING zeile FROM 0 BY 1
       UNTIL zeile = tupl
           PERFORM VARYING spalte FROM 0 BY 1
           UNTIL spalte = nfield
           CALL "PQgetvalue" USING
              BY VALUE pgres
              BY VALUE zeile
              BY VALUE spalte
              RETURNING resptr
           END-CALL
           IF resptr NOT EQUAL NULL THEN
           SET ADDRESS OF resstr TO resptr
           IF spalte = 0   THEN
              STRING resstr DELIMITED BY x"00" INTO WERT1 END-STRING
              DISPLAY
              "Zeile ",zeile
              " Spalte ",spalte," Wert= ", WERT1
              neuezeile
              END-DISPLAY
              ELSE IF spalte = 1 THEN
              STRING resstr DELIMITED BY x"00" INTO WERT2 END-STRING
              DISPLAY
              "Zeile ",zeile
              " Spalte ", spalte," Wert= ",WERT2
              neuezeile
              END-DISPLAY
              ELSE IF spalte = 2 THEN
              STRING resstr DELIMITED BY x"00" INTO WERT3 END-STRING
              DISPLAY
              "Zeile ",zeile
              " Spalte ", spalte," Wert= ",WERT3
              neuezeile
              END-DISPLAY
           END-IF
           END-IF
           END-PERFORM
           MOVE 0 to spalte
       END-PERFORM.
       CALL "PQclear"  USING BY VALUE pgres  END-CALL.
       CALL "PQfinish" USING BY VALUE pgconn END-CALL.
       SET pgconn to NULL.
       STOP RUN.
