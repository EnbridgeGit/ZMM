*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZEXEC_DATE......................................*
DATA:  BEGIN OF STATUS_ZEXEC_DATE                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEXEC_DATE                    .
CONTROLS: TCTRL_ZEXEC_DATE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZEXEC_DATE                    .
TABLES: ZEXEC_DATE                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
