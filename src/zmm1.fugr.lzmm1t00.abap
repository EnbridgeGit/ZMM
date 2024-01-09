*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_LAST_MSEG...................................*
DATA:  BEGIN OF STATUS_ZMM_LAST_MSEG                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_LAST_MSEG                 .
CONTROLS: TCTRL_ZMM_LAST_MSEG
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZSAP_MAP_ZEMA...................................*
DATA:  BEGIN OF STATUS_ZSAP_MAP_ZEMA                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSAP_MAP_ZEMA                 .
CONTROLS: TCTRL_ZSAP_MAP_ZEMA
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZMM_LAST_MSEG                 .
TABLES: *ZSAP_MAP_ZEMA                 .
TABLES: ZMM_LAST_MSEG                  .
TABLES: ZSAP_MAP_ZEMA                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
