*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZARIBAAPRV......................................*
DATA:  BEGIN OF STATUS_ZARIBAAPRV                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZARIBAAPRV                    .
CONTROLS: TCTRL_ZARIBAAPRV
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZARIBAAPRV                    .
TABLES: ZARIBAAPRV                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
