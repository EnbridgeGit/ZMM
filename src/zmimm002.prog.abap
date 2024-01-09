REPORT ZMIMM002 MESSAGE-ID ZZ
                LINE-SIZE  79
                LINE-COUNT 59.

*-----------------------------------------------------------------------
* Program      :  ZMIMM002
* Author       :  J. LEE  OMNILOGIC SYSTEMS GROUP
* Date         :  October 21, 1996
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
* data declaration
*-----------------------------------------------------------------------
TABLES:  RESB.

* internal table storing reservation no. / item no. of those
* for which flag is to be turned on.
DATA: BEGIN OF TAB OCCURS 50,
        RSNUM(10) TYPE C,
        RSPOS(4)  TYPE C,
      END OF TAB.

* misc variable(s) used
DATA: STATUS(5)      TYPE C VALUE '     '.  "status of update

* internal table storing BDC entries
DATA: BEGIN OF BDCDATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

* internal table storing error messages
DATA: BEGIN OF ERRDATA OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF ERRDATA.

*-----------------------------------------------------------------------
* selection screen
*-----------------------------------------------------------------------
START-OF-SELECTION.
SELECT-OPTIONS: ZRSNUM FOR RESB-RSNUM.
PARAMETERS: SBDC AS CHECKBOX DEFAULT ' '.  "batch input / report

END-OF-SELECTION.

*-----------------------------------------------------------------------
* figure out which reservation(s) to change
*-----------------------------------------------------------------------
SELECT * FROM RESB WHERE RSNUM IN ZRSNUM
                   AND   BDART = 'MR'.
   "set the flag on for those reservation items having
   "qty withdraw >= qty reserved.
   IF RESB-ENMNG >= RESB-BDMNG AND
      RESB-XLOEK <> 'X'.
      MOVE RESB-RSNUM TO TAB-RSNUM.
      MOVE RESB-RSPOS TO TAB-RSPOS.
      APPEND TAB.
   ENDIF.
ENDSELECT.

*-----------------------------------------------------------------------
* build BDC table
*-----------------------------------------------------------------------
SORT TAB BY RSNUM RSPOS.
PERFORM FILL_BDCDATA.

*-----------------------------------------------------------------------
* form fill_bdcdata .. create the BDC entries required
*-----------------------------------------------------------------------
FORM FILL_BDCDATA.
   DESCRIBE TABLE TAB.
   IF SY-TFILL <> 0.
      WRITE: /3 'Reservation No.', 25 'Item No.', 38 'Status'.
      WRITE: /3 '---------------', 25 '--------', 38 '------'.
      LOOP AT TAB.
         IF SBDC = 'X'.
            PERFORM SCREEN_HEADER USING 'SAPMM07R'    '0560' 'X'.
            PERFORM SCREEN_FIELD  USING 'RM07M-RSNUM' TAB-RSNUM.
            PERFORM SCREEN_FIELD  USING 'RM07M-RSPOS' TAB-RSPOS.
            PERFORM SCREEN_FIELD  USING 'BDC_OKCODE' '/0'.

            PERFORM SCREEN_HEADER USING 'SAPMM07R'    '0510' 'X'.
            PERFORM SCREEN_FIELD  USING 'RESB-XLOEK'  'X'.

            PERFORM SCREEN_FIELD  USING 'BDC_OKCODE'  'BU'.

            PERFORM SCREEN_HEADER USING 'SAPLKACB'   '0002' 'X'.
            PERFORM SCREEN_FIELD  USING 'BDC_OKCODE' 'ENTE'.

            CALL TRANSACTION 'MB22' USING BDCDATA
                 MODE 'N' UPDATE 'S' MESSAGES INTO ERRDATA.

            IF SY-SUBRC = 0.
               MOVE 'OK ' TO STATUS.
            ELSE.
               MOVE 'Error' TO STATUS.
            ENDIF.

            REFRESH BDCDATA.
            REFRESH ERRDATA.
         ENDIF.
         WRITE: /3 TAB-RSNUM, 25 TAB-RSPOS, 38 STATUS.
      ENDLOOP.
   ELSE.
      WRITE:/ '0 item selected'.
   ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
* form screen_header
*-----------------------------------------------------------------------
FORM SCREEN_HEADER USING VALUE(PROGRAM)
                         VALUE(DYNPRO)
                         VALUE(DYNBEGIN).
   CLEAR BDCDATA.
   MOVE PROGRAM  TO BDCDATA-PROGRAM.
   MOVE DYNPRO   TO BDCDATA-DYNPRO.
   MOVE DYNBEGIN TO BDCDATA-DYNBEGIN.
   APPEND BDCDATA.
ENDFORM.

*-----------------------------------------------------------------------
* form screen_field
*-----------------------------------------------------------------------
FORM SCREEN_FIELD USING VALUE(FNAM)
                        VALUE(FVAL).
   CLEAR BDCDATA.
   MOVE FNAM TO BDCDATA-FNAM.
   MOVE FVAL TO BDCDATA-FVAL.
   APPEND BDCDATA.
ENDFORM.
