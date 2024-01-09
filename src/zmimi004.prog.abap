report ZMIMI004 message-id zm.
*----------------------------------------------------------------------
*  Goods Receipts from WIS
*  Although there are several transactions on the incoming file,
*  only "MB01" are accepted by this program.
*  Also be aware, that the transaction code changed from 4 bytes in 3.1i
*  to 20 bytes in 4.6b.  To avoid a file conversion in the WIS
*  application, this change is handled thru a windowing process in
*  the "LOAD_FILE" routine.
*----------------------------------------------------------------------
* 2000/05/26 mdemeest - Input WIS file and call SAP BAPI Goods Receipt
*                       to post info immediately.
*----------------------------------------------------------------------
TABLES:  T158G.
*         BAPI2017_GM_HEAD_RET_MAT_DOC.

*----------------------------------------------------------------------
* INREC temporarily hold the data from the WIS world and then the
* data is moved to the correct locations on ZBMSEG.  This is
* necessary because the TRANSACTION CODE changed from (4) to (20)
*----------------------------------------------------------------------
data: inrec(1000) type c.

data: begin of zbmseg occurs 100.
        include structure zbmseg.
data: end of zbmseg.

********************* LOGICAL PROCESSING GRID **************************
*-----------------------------------------------------------------------
* - The following grid describes how the program handles different
* combinations of action code(ZBMSEG-ACTION), control break(ZMBSEG-
* CNTRL) , and transaction code(ZBMSEG-TCODE) contained in the input
* file.  Please read the special note in the FORM CHG_LINE_ITEM.
*-----------------------------------------------------------------------
* ACTION = 'C' and CNTRL = 'X' and TCODE = MB21 ==> Create Reservation
* ACTION = 'C' and CNTRL = 'X' and TCODE = MB22 ==> Change Reservation
* ACTION = 'C' and CNTRL = ' ' and TCODE = MB22 ==> Change Reservation
*                                                   (add line item)
* ACTION = 'D' and CNTRL = ' ' and TCODE = MB22 ==> Change Reservation
*                                                   (delete line item)
* ACTION = 'P' and CNTRL = 'X' and TCODE = MB22 ==> "Purge" (delete)
*                                                   reservation
*-----------------------------------------------------------------------
* physical file name
data:  infile(70).
*-------------------------  SELECTION SCREEN  --------------------------
* logical file name
parameters:  lgclfile like filename-fileintern obligatory
                                               default 'ZMIN002_?',
             p_test(1) type c.

*-----------------------   SELECTION SCREEN PROCESSING  ----------------
at selection-screen.
  perform check_file.

*------------------------  START-of-SELECTION  -------------------------
start-of-selection.
  perform load_file.



  loop at zbmseg.
    perform post_goods_receipt.
  endloop.


*-----------------------------  CHECK_FILE  ----------------------------
*  This routine checks the logical file and converts it to a physical
*  physical file name.  It then attempts to open the physical file to
*  determine if there are any errors reading it.
*-----------------------------------------------------------------------
form check_file.

 DATA: MSG(100).                      "open file - system message

 CALL FUNCTION 'FILE_GET_NAME'
      EXPORTING
           CLIENT           = SY-MANDT
           LOGICAL_FILENAME = LGCLFILE
           OPERATING_SYSTEM = SY-OPSYS
      IMPORTING
           FILE_NAME        = INFILE
      EXCEPTIONS
           FILE_NOT_FOUND   = 01.

 IF ( SY-SUBRC = 1 ).
   MESSAGE E001 WITH LGCLFILE.
 ENDIF.

  OPEN DATASET INFILE FOR INPUT IN TEXT MODE MESSAGE MSG.
  IF ( SY-SUBRC <> 0 ).
   MESSAGE I002 WITH INFILE MSG. "Message - Error opening physical file"
    STOP.
  ENDIF.
*  CLOSE DATASET INFILE.

endform.


*-----------------------------------------------------------------------
*     FORM LOAD_FILE
*-----------------------------------------------------------------------
* - Loads data records from input file into internal table
*-----------------------------------------------------------------------
form load_file.

* do until we have read in the complete file processing only 'MIGO'
* transaction codes (Goods Receipts).

  do.
    clear:  zbmseg,  inrec.
    read dataset infile into inrec.
    if sy-subrc <> 0.
      exit.
    endif.

    move inrec(18) to zbmseg(18).
    move inrec+18(674) to zbmseg+34(674).
    if zbmseg-tcode = 'MB01'.
       append zbmseg.
    endif.

  enddo.

  close dataset infile.

endform.

form post_goods_receipt.


data:  GOODSMVT_HEADRET      like BAPI2017_GM_HEAD_RET,
       MATERIALDOCUMENT      like BAPI2017_GM_HEAD_RET-MAT_DOC,
       MATDOCUMENTYEAR       like BAPI2017_GM_HEAD_RET-DOC_YEAR,
       GOODSMVT_HEADER       like BAPI2017_GM_HEAD_01,
       GOODSMVT_CODE         like BAPI2017_GM_CODE,
       TESTRUN               like BAPI2017_GM_GEN-TESTRUN,

       GOODSMVT_ITEM         like BAPI2017_GM_ITEM_CREATE
                                          occurs 0 with header line,
       GOODSMVT_SERIALNUMBER like BAPI2017_GM_SERIALNUMBER
                                          occurs 0 with header line,
       RETURN                like BAPIRET2
                                          occurs 0 with header line.


*----------------------------------------------------------------------
* move incoming info to GOODSMVT_HEADER
*----------------------------------------------------------------------
  move zbmseg-budat to GOODSMVT_HEADER-pstng_date.       "Posting Date
  move zbmseg-bldat to GOODSMVT_HEADER-doc_date.         "Document Date
  move zbmseg-xblnr to GOODSMVT_HEADER-ref_doc_no.       "Ref document
  move zbmseg-frbnr to GOODSMVT_HEADER-bill_of_lading.   "Bill of Lading
* move zbmseg-***** to GOODSMVT-pr_uname.                "User Name
  move zbmseg-bktxt to GOODSMVT_HEADER-header_txt.       "Header Text
*----------------------------------------------------------------------
* move incoming info to GOODSMVT_CODE
*----------------------------------------------------------------------
  select single * from T158G
     where tcode = zbmseg-tcode.
  if sy-subrc = '0'.
     move t158g-gmcode to GOODSMVT_CODE-gm_code.
  else.
     write: 'Purchase Order = ', zbmseg-xblnr,
                                        'has problem with movement cd'.
  endif.
*----------------------------------------------------------------------
* move incoming info to GOODMVT_ITEM
*----------------------------------------------------------------------
  move zbmseg.
  move p_test       to testrun.

  call function 'BAPI_GOODSMVT_CREATE'
    exporting
        GOODSMVT_HEADER               = GOODSMVT_HEADER
        GOODSMVT_CODE                 = GOODSMVT_CODE
        TESTRUN                       = TESTRUN
    importing
        GOODSMVT_HEADRET              = GOODSMVT_HEADRET
        MATERIALDOCUMENT              = MATERIALDOCUMENT
        MATDOCUMENTYEAR               = MATDOCUMENTYEAR
    tables
        GOODSMVT_ITEM                 = GOODSMVT_ITEM
        GOODSMVT_SERIALNUMBER         = GOODSMVT_SERIALNUMBER
        RETURN                        = RETURN
    exceptions
        OTHERS                        = 1.


*----------------------------------------------------------------------
* commit work only if valid return code (sy-subrc = 0)
*----------------------------------------------------------------------
  if sy-subrc = '0'.
     commit work.
     write: /.
     write: /1 MATERIALDOCUMENT, ' WAS POSTED'.
  else.
     write: /1 zbmseg-frbnr, ' WAS NOT POSTED'.
  endif.


endform.






