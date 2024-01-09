*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLSDCIS03.......................................*
DATA:  BEGIN OF STATUS_ZLSDCIS03                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLSDCIS03                     .
CONTROLS: TCTRL_ZLSDCIS03
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLSDCIS03                     .
TABLES: ZLSDCIS03                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
