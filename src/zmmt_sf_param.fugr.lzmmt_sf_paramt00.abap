*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT_SF_PARAM...................................*
DATA:  BEGIN OF STATUS_ZMMT_SF_PARAM                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT_SF_PARAM                 .
CONTROLS: TCTRL_ZMMT_SF_PARAM
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT_SF_PARAM                 .
TABLES: ZMMT_SF_PARAM                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
