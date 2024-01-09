*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT_LOCMAST....................................*
DATA:  BEGIN OF STATUS_ZMMT_LOCMAST                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT_LOCMAST                  .
CONTROLS: TCTRL_ZMMT_LOCMAST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT_LOCMAST                  .
TABLES: ZMMT_LOCMAST                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
