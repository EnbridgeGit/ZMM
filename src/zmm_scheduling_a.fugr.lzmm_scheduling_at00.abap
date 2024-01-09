*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT_TVARVC.....................................*
DATA:  BEGIN OF STATUS_ZMMT_TVARVC                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT_TVARVC                   .
CONTROLS: TCTRL_ZMMT_TVARVC
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT_TVARVC                   .
TABLES: ZMMT_TVARVC                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
