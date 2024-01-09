
*------------------------------------------------------------------
*           D0079_PERIV.
* Beim Pop-Up-Window 0079 ist eine Periodenversion eingebbar, für diese
* gilt:
* Periodenversion nur sinnvoll mit Periodenkz. 'Buchhaltungsperiode'
* mk/18.04.95: wird auch verwendet auf Dispobildern
*------------------------------------------------------------------
MODULE D0079_PERIV.

  CHECK BILDFLAG IS INITIAL.           "mk/18.04.95
  CHECK T130M-AKTYP NE AKTYPA AND T130M-AKTYP NE AKTYPZ.  "mk/18.04.95

  CALL FUNCTION 'D_0079_PERIV'
       EXPORTING
            P_PERIV      = MARC-PERIV
            P_PERKZ      = MARC-PERKZ
            P_DISLS      = MARC-DISLS           "JB/331601
            P_KZ_NO_WARN = ' '
       IMPORTING
            P_PERIV      = MARC-PERIV.
*    EXCEPTIONS
*         P_ERR_D_0079_PERIV = 01.
ENDMODULE.
