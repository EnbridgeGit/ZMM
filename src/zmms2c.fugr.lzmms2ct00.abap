*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZISNQUAL........................................*
DATA:  BEGIN OF STATUS_ZISNQUAL                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZISNQUAL                      .
CONTROLS: TCTRL_ZISNQUAL
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZISNQUAL                      .
TABLES: ZISNQUAL                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
