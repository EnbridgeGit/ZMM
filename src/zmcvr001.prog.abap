REPORT ZMCVR001 LINE-SIZE 132 LINE-COUNT 65.
************************************************************************
*    Program       :  ZMCVR001
*    Programmer    :  M. Moore
*    Date          :  Nov, 1996.
*
* cross reference report of legacy stock numbers to SAP Material Numbers
* fields in report - legacy stock#, SAP material#, material description
***********************************************************************
TABLES:
       MARA, MAKT.

SELECT-OPTIONS: SMATNR FOR MARA-MATNR,
                SMTART FOR MARA-MTART,
                SMATKL FOR MARA-MATKL.

DATA: UNION_NUM(8).

SELECT * FROM MARA WHERE MATNR IN SMATNR AND
                         MTART IN SMTART AND
                         MATKL IN SMATKL
                         ORDER BY BISMT.
    SELECT SINGLE * FROM MAKT
       WHERE MATNR = MARA-MATNR
         AND SPRAS = SY-LANGU.
       MOVE MARA-BISMT TO UNION_NUM.
       IF UNION_NUM <> SPACE.
          WRITE: / UNION_NUM,
                   MARA-MATNR,
                   MAKT-MAKTX.
       ENDIF.
ENDSELECT.

TOP-OF-PAGE.
IF SY-MANDT <> '050'.
    WRITE: '*** test data only - not final bis xref ***', SY-DATUM, /.
ENDIF.
