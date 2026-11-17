      *----------------------------------------------------------------*
      *  Licensed Materials - Property of IBM                          *
      *  SAMPLE                                                        *
      *  (c) Copyright IBM Corp. 2016 All Rights Reserved              *
      *  US Government Users Restricted Rights - Use, duplication or   *
      *  disclosure restricted by GSA ADP Schedule Contract with       *
      *  IBM Corp                                                      *
      *----------------------------------------------------------------*
      *       AUTHOR  :  Giovanni Creato/Italy/IBM                     *
      *               :  Tony Fitztgerald/UK/IBM
      *       DATE    :  15 / 10 / 2026                                *
      *       VERSION :  2.0.0                                         *
      *       HISTORY :                                                *
      *        16/06/16  Moved 01 level DFHCOMMAREA to EDUCPY          *
      *        28/04/26  Removed 01 level DFHCOMMAREA for containers   *
      *        28/04/26  Converted from COMMAREA to channel/container  *
      *----------------------------------------------------------------*
      * Description                                                    *
      *                                                                *
      * This program defines different types of COBOL data and can be  *
      * used to understand different COBOL data types                  *
      *                                                                *
      * The data is received in, and returned in, the container named  *
      * EDUCONTAINER on the channel the program was invoked with.      *
      *                                                                *
      *----------------------------------------------------------------*
       TITLE 'Sample program that treats different types of COBOL data'
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    "EDUPGM".
       Author.        "Tony Fitzgerald/UK/IBM".
       DATE-WRITTEN.   15/10/2026.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-zSeries WITH DEBUGGING MODE.
      *SOURCE-COMPUTER.  IBM-zSeries.
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01 Program-Description.
           03 WS-PGM-NAME.
              05 pic x(10)   value 'EDUPGM  : '.

       01 WS-VARIABLES.
           03 DISPLAYABLE-NUMERIC             PIC -z(3)9.
           03 REQUIRED-CONT-LEN               PIC S9(4)      VALUE +0.
           03  VALID-INPUT-PARAMETERS         PIC X.
              88 VALID-INPUT-PARAMETERS-FALSE value X'00'.
              88 VALID-INPUT-PARAMETERS-TRUE  value X'01' through X'FF'.
           03  Switches.
              05 Switch-condition       Pic X value space.
                 88 Switch-condition-1 value "A".
                 88 Switch-condition-2 value "B".

      * Channel and container fields
       01 WS-CHANNEL-CONTAINER.
           03 WS-CHANNEL-NAME                 PIC X(16)  VALUE SPACES.
           03 WS-CONTAINER-NAME               PIC X(16)
                                              VALUE 'EDUCONTAINER'.
           03 WS-FLENGTH                      PIC S9(8)  COMP VALUE +0.
           03 WS-RESP                         PIC S9(8)  COMP VALUE +0.

      * COPY BOOK - for the data received from , and returned
      *   in the container
       01 WS-CONTAINER-DATA.
            COPY EDUCPY.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       MAIN-PROGRAM          section.
      D    DISPLAY WS-PGM-NAME 'Performing MAIN-PROGRAM'.

           PERFORM INPUT-PARAMETER-CHECK.

           if VALID-INPUT-PARAMETERS-TRUE  then
               PERFORM BUSINESS-LOGIC
           else
               MOVE -1 TO RESULT-CODE
               MOVE 'INVALID INPUT PARAMETERS'
                    TO RESULT-TEXT
           end-if.

      D    MOVE RESULT-CODE  TO DISPLAYABLE-NUMERIC.
      D    DISPLAY WS-PGM-NAME '  Returning with RESULT-CODE : '
      D                               DISPLAYABLE-NUMERIC.

           PERFORM PUT-OUTPUT-CONTAINER.

           PERFORM END-PROGRAM.

       INPUT-PARAMETER-CHECK section.

      * Find the name of the channel the program was invoked with.
           EXEC CICS ASSIGN CHANNEL(WS-CHANNEL-NAME)
                     RESP(WS-RESP)
           END-EXEC
      *    Check if the ASSIGN CHANNEL worked. If NO channel received
      *    issue an ABEND
           IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)
              OR WS-CHANNEL-NAME IS EQUAL TO SPACES
               DISPLAY WS-PGM-NAME
                  '  NO CHANNEL RECEIVED. ABENDING THE TASK'
               EXEC CICS ABEND ABCODE('LENG') NODUMP END-EXEC
           END-IF

      *    Set the maximum length of the data that the program accepts
           MOVE LENGTH OF WS-CONTAINER-DATA TO WS-FLENGTH

      *    Read the input container.
      *      LENGERR means the data received is bigger than the value
      *        set in WS-FLENGTH in which case the data received will be
      *        truncated
      *      On completion of the GET CONTAINER WS-FLENGTH will contain
      *        the actual length of the data in the container
           EXEC CICS GET CONTAINER(WS-CONTAINER-NAME)
                     CHANNEL(WS-CHANNEL-NAME)
                     INTO(WS-CONTAINER-DATA)
                     FLENGTH(WS-FLENGTH)
                     RESP(WS-RESP)
           END-EXEC

      *    Check the result of the GET container
      *    If it does not exist issue an ABEND.
           IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)
              AND WS-RESP IS NOT EQUAL TO DFHRESP(LENGERR)
               DISPLAY WS-PGM-NAME
                 '  NO CONTAINER RECEIVED. ABENDING THE TASK'
               EXEC CICS ABEND ABCODE('NOCO') NODUMP END-EXEC
           END-IF

      *    Do something with the data received
      D    MOVE ZERO                       TO REQUIRED-CONT-LEN
      D    ADD LENGTH OF BINARY-DIGIT      TO REQUIRED-CONT-LEN
      D    ADD LENGTH OF CHARACTER-STRING  TO REQUIRED-CONT-LEN
      D    ADD LENGTH OF NUMERIC-STRING    TO REQUIRED-CONT-LEN
      D    ADD LENGTH OF PACKED-DIGIT      TO REQUIRED-CONT-LEN

      D    MOVE REQUIRED-CONT-LEN          TO DISPLAYABLE-NUMERIC
      D    DISPLAY WS-PGM-NAME '  PARTIAL SUM IS  : '
      D            DISPLAYABLE-NUMERIC

      D    ADD LENGTH OF SIGNED-PACKED     TO REQUIRED-CONT-LEN
      D    MOVE REQUIRED-CONT-LEN          TO DISPLAYABLE-NUMERIC
      D    DISPLAY WS-PGM-NAME '  PARTIAL SUM IS  : '
      D            DISPLAYABLE-NUMERIC

      D    ADD LENGTH OF BOOL              TO REQUIRED-CONT-LEN
      D    ADD LENGTH OF RESULT-CODE       TO REQUIRED-CONT-LEN
      D    MOVE REQUIRED-CONT-LEN          TO DISPLAYABLE-NUMERIC
      D    DISPLAY WS-PGM-NAME '  PARTIAL SUM IS  : '
      D            DISPLAYABLE-NUMERIC

      D    ADD LENGTH OF RESULT-TEXT       TO REQUIRED-CONT-LEN
      D    MOVE REQUIRED-CONT-LEN          TO DISPLAYABLE-NUMERIC
      D    DISPLAY WS-PGM-NAME '  PARTIAL SUM IS  : '
      D            DISPLAYABLE-NUMERIC

      *    If container is less then required issue an ABEND
           MOVE LENGTH OF DATA-PAYLOAD     TO REQUIRED-CONT-LEN
      D    MOVE REQUIRED-CONT-LEN          TO DISPLAYABLE-NUMERIC
      D    DISPLAY WS-PGM-NAME '  MINIMUM CONTAINER LENGTH IS : '
                                              DISPLAYABLE-NUMERIC
           IF WS-FLENGTH IS LESS THAN REQUIRED-CONT-LEN
               MOVE WS-FLENGTH             TO DISPLAYABLE-NUMERIC
               DISPLAY WS-PGM-NAME '  CONTAINER SHORTER THAN : '
                          DISPLAYABLE-NUMERIC '. ABEND THE TASK'
               EXEC CICS ABEND ABCODE('GCCO') NODUMP END-EXEC
           END-IF

      *    Initialise result variables
           MOVE ZERO   TO RESULT-CODE
           MOVE SPACES TO RESULT-TEXT

           Set VALID-INPUT-PARAMETERS-TRUE to True.

      * NOTE BINARY-DIGIT cannot be controlled

           if CHARACTER-STRING IS NOT ALPHABETIC then
      D         DISPLAY WS-PGM-NAME
      D            '  CHARACTER-STRING IS NOT ALPHABETIC'
                Set VALID-INPUT-PARAMETERS-FALSE to TRUE.

           if NUMERIC-STRING IS NOT NUMERIC then
      D         DISPLAY WS-PGM-NAME '  NUMERIC-STRING IS NOT NUMERIC'
                Set VALID-INPUT-PARAMETERS-FALSE to TRUE.

           if PACKED-DIGIT IS NOT NUMERIC then
      D         DISPLAY WS-PGM-NAME '  PACKED-DIGIT IS NOT NUMERIC'
                Set VALID-INPUT-PARAMETERS-FALSE to TRUE.

           if SIGNED-PACKED IS NOT NUMERIC then
      D         DISPLAY WS-PGM-NAME '  SIGNED-PACKED IS NOT NUMERIC'
                Set VALID-INPUT-PARAMETERS-FALSE to TRUE.

           if BOOL IS NOT NUMERIC then
      D         DISPLAY WS-PGM-NAME '  BOOL IS NOT NUMERIC'
                Set VALID-INPUT-PARAMETERS-FALSE to TRUE.


       BUSINESS-LOGIC        section.
      D         DISPLAY WS-PGM-NAME '  Performing Business Logic'.
                MOVE ZERO TO RESULT-CODE.
                MOVE 'PARAMETERS ARE ALL OK'
                     TO RESULT-TEXT.


       PUT-OUTPUT-CONTAINER  section.
      * Return the updated data to the caller by replacing the
      * contents of the input container. DATATYPE(BIT) is required
      * because the data holds binary and packed decimal fields that
      * must not be code page converted.
      D         DISPLAY WS-PGM-NAME
      D          '  Writing updated data to container'.

      * Set the length of the data we are sending back
                MOVE LENGTH OF DATA-PAYLOAD TO WS-FLENGTH

      * Put the data to be returned into the container
                EXEC CICS PUT CONTAINER(WS-CONTAINER-NAME)
                          CHANNEL(WS-CHANNEL-NAME)
                          FROM(DATA-PAYLOAD)
                          FLENGTH(WS-FLENGTH)
                          BIT
                          RESP(WS-RESP)
                END-EXEC

      * Check the PUT CONTAINER worked
                IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)
                    DISPLAY WS-PGM-NAME
                      '  PUT CONTAINER FAILED. ABENDING THE TASK'
                    EXEC CICS ABEND ABCODE('PCNT') NODUMP END-EXEC
                END-IF.


       END-PROGRAM           section.
      D         DISPLAY WS-PGM-NAME 'Performing END-PROGRAM'.
      D         DISPLAY '   '.
                EXEC CICS RETURN END-EXEC.
