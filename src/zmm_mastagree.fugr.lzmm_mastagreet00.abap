*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT_MASTAGREE..................................*
DATA:  BEGIN OF STATUS_ZMMT_MASTAGREE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT_MASTAGREE                .
CONTROLS: TCTRL_ZMMT_MASTAGREE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT_MASTAGREE                .
TABLES: ZMMT_MASTAGREE                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
