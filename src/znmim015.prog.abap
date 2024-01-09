FORM CD_CALL_EINKBELEG .
   IF   ( UPD_EKAN       NE SPACE )
*    or ( upd_ekek       ne space )             "1999/06/01  mdemeest
     OR ( UPD_EKES       NE SPACE )
     OR ( UPD_EKET       NE SPACE )
     OR ( UPD_EKKN       NE SPACE )
     OR ( UPD_EKKO       NE SPACE )
     OR ( UPD_EKPO       NE SPACE )
     OR ( UPD_SADR       NE SPACE )
     OR ( UPD_ICDTXT_EINKBELEG  NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'EINKBELEG_WRITE_DOCUMENT      ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  O_EKAN                = *EKAN
                  N_EKAN                = EKAN
                  UPD_EKAN              = UPD_EKAN
*                 upd_ekek              = upd_ekek "1999/06/01 mdemeest
                  UPD_EKEK              = ' '      "1999/06/01 mdemeest
                  UPD_EKES              = UPD_EKES
                  UPD_EKET              = UPD_EKET
                  UPD_EKKN              = UPD_EKKN
                  O_EKKO                = *EKKO
                  N_EKKO                = EKKO
                  UPD_EKKO              = UPD_EKKO
                  UPD_EKPO              = UPD_EKPO
                  UPD_SADR              = UPD_SADR
                  UPD_ICDTXT_EINKBELEG  = UPD_ICDTXT_EINKBELEG
          TABLES  ICDTXT_EINKBELEG      = ICDTXT_EINKBELEG
                  XEKEK                 = XEKEK
                  YEKEK                 = YEKEK
                  XEKES                 = XEKES
                  YEKES                 = YEKES
                  XEKET                 = XEKET
                  YEKET                 = YEKET
                  XEKKN                 = XEKKN
                  YEKKN                 = YEKKN
                  XEKPO                 = XEKPO
                  YEKPO                 = YEKPO
                  XSADR                 = XSADR
                  YSADR                 = YSADR
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
