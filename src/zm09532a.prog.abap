REPORT ZM09532A.
TABLES: A003, KONH, KONP.
SELECT * FROM A003.
  WRITE: / A003-KAPPL,
           A003-KSCHL,
           A003-ALAND,
           A003-MWSKZ,
           A003-KNUMH.
SELECT SINGLE * FROM KONH WHERE KNUMH = A003-KNUMH.
IF SY-SUBRC = 0.
   IF KONH-KOTABNR = '003' OR KONH-KOTABNR = '053'.
      WRITE 'KONH'.
   ELSE.
      WRITE KONH-KOTABNR.
*     DELETE A003.
   ENDIF.
ELSE.
    WRITE '---'.
ENDIF.
SELECT * FROM KONP WHERE KNUMH = A003-KNUMH.
  WRITE: 'KONP',KONP-KBETR.
ENDSELECT.
IF SY-SUBRC <> 0.
   WRITE: '---'.
ENDIF.
ENDSELECT.