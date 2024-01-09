*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMMV_T001W.....................................*
TABLES: ZMMMV_T001W, *ZMMMV_T001W. "view work areas
CONTROLS: TCTRL_ZMMMV_T001W
TYPE TABLEVIEW USING SCREEN '9000'.
DATA: BEGIN OF STATUS_ZMMMV_T001W. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMMMV_T001W.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMMMV_T001W_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMMMV_T001W.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMMMV_T001W_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMMMV_T001W_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMMMV_T001W.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMMMV_T001W_TOTAL.

*.........table declarations:.................................*
TABLES: T001W                          .
TABLES: T002                           .
TABLES: T002T                          .
TABLES: T005                           .
TABLES: T005E                          .
TABLES: T005F                          .
TABLES: T005G                          .
TABLES: T005H                          .
TABLES: T005S                          .
TABLES: T005T                          .
TABLES: T005U                          .
TABLES: TFACD                          .
TABLES: TFACT                          .
