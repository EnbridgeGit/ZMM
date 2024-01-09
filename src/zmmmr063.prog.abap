REPORT ZMMMR063 no standard page heading LINE-Size 256 line-count 65
       message-id zm.



******************************************************************
*       Owner: Union                                             *
*  Programmer: M DeMeester                                       *
*        Date: September 19,2008                                 *
*  Request ID: TR005                                             *
*                                                                *
* This report downloads a file from unix to the network.  It     *
* must be run in foreground                                      *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YY/MM/DD - USERID - MOD# - DESCRIPTION                         *
* -------------------------------------------------------------- *
*  2008/09/19 mdemeester TR005 - New report                      *
*       *
******************************************************************
TYPE-POOLS:  SLIS.               "For EXCEL SPREADSHEET

data:  begin of outtab occurs 0,
       out_file(360) type c,
       end of outtab.

* This first block produces a border like frame around the following: *

SELECTION-SCREEN BEGIN OF BLOCK GENERAL WITH FRAME title text-001.


SELECTION-SCREEN END OF BLOCK GENERAL.

SELECTION-SCREEN SKIP.

* The following second block produces a frame within the first block. *

* The following is the additional search parameters.

.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK reportoptions with frame title text-100.
SELECTION-SCREEN SKIP.

selection-screen begin of line.
selection-screen comment 10(27) text-252.        "Unix file
parameter:  P_FILE        LIKE RFPDO-RFBIFILE
           default '/usr/sap/interfaces/P01/CFMM001/file.txt'.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 10(27) text-251.        "Network file
PARAMETERS: p_out   TYPE RLGRAP-FILENAME DEFAULT
                            'C:\saptemp\catalog.csv'.
selection-screen end of line.


SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK reportoptions.


SELECTION-SCREEN SKIP.




********************* INITIALIZATION *******************************
INITIALIZATION.
move sy-sysid to p_file+20(3).

************************************************************************

*  This routine retains only those CLASSES with the corresponding object
*  number requested by the user.


************************************************************************



START-OF-SELECTION.

  perform open_file.
* p_file  '/usr/sap/interfaces/P01/CFMM001/zmmmr062.txt'.


form open_file.
  open dataset p_file for input in text mode.
  do.
    READ DATASET p_file INTO outtab.
    if sy-subrc <> '0'.
       exit.
    endif.
    append outtab.
  enddo.

  perform write_file.

  delete dataset p_file.

  endform.









FORM WRITE_FILE.

   call function 'WS_DOWNLOAD'
     exporting
         filename            = p_out
         filetype            = 'DAT'
     tables
         data_tab            = outtab
     exceptions
         file_open_error     = 1
         file_write_error    = 2
         invalid_filesize    = 3
         invalid_table_width = 4
         invalid_type        = 5
         no_batch            = 6
         unknown_error       = 7
         others              = 8.

  write: / 'ws_download done. returncode =',  sy-subrc.

ENDFORM.
