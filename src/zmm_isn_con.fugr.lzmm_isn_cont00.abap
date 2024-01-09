*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_ISN_CON.....................................*
DATA:  BEGIN OF STATUS_ZMM_ISN_CON                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_ISN_CON                   .
CONTROLS: TCTRL_ZMM_ISN_CON
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZMM_ISN_CON                   .
TABLES: ZMM_ISN_CON                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
