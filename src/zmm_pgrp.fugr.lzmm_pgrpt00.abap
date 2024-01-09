*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT_PGRP.......................................*
DATA:  BEGIN OF STATUS_ZMMT_PGRP                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT_PGRP                     .
CONTROLS: TCTRL_ZMMT_PGRP
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZMMT_PGRP                     .
TABLES: ZMMT_PGRP                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
