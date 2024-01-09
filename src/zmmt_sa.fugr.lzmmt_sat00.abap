*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT_SA.........................................*
DATA:  BEGIN OF STATUS_ZMMT_SA                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT_SA                       .
CONTROLS: TCTRL_ZMMT_SA
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT_SA                       .
TABLES: ZMMT_SA                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
