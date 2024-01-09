*-------------------------------------------------------------------
***INCLUDE LMGXXF01 .   Blätterroutinen für Materialstamm
*-------------------------------------------------------------------
*  Bedeutungen der Parameter :
*
*  ERSTE_ZEILE  : Position des ersten sichtbaren Eintrages - 1 (!!),
*     d.h. wenn der erste Eintrag der Tabelle am Anfang zu sehen ist,
*     ist ERSTE_ZEILE = 0
*  ZLE_PROSEITE : Anzahl der Step-Loop - Zeilen im Dynpro
*  LINES        : Anzahl der Einträge in der internen Tabelle
*
*-------------------------------------------------------------------

  INCLUDE LMGD1F22 .  " FIRST_PAGE

  INCLUDE LMGD1F21 .  " PREV_PAGE

  INCLUDE LMGD1F20 .  " NEXT_PAGE


  INCLUDE LMGD1F1Z .  " NEXT_PAGE_VW
* AHE: 02.06.98 - E


  INCLUDE LMGD1F1Y .  " LAST_PAGE

  INCLUDE LMGD1F1X .  " PARAM_SET

  INCLUDE LMGD1F2H.   "TC_LONGTEXT_INIT
