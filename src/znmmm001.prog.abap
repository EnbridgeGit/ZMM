************************************************************************
*    Program     :  ZNMMM001
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  September 30, 1996
*
*    This ABAP will be used as an #INCLUDE to store the data structures
*    necessary for the sub-routines in the ABAP ZNMMM002 and all
*    catalogues.
************************************************************************
TABLES: MARA, KLAH, KSSK, CABN, AUSP, SWOR.
DATA:  INCLFLAG TYPE I VALUE '0'.
DATA   : BEGIN OF TABLE1 OCCURS 10000,
             MATNR      LIKE MARA-MATNR,   "MASTER is the class that the
             MASTER     LIKE KLAH-CLINT,   "MATERIAL NUMBER is directly
             CLASS1     LIKE KLAH-CLINT,   "associated with.  CLASS1 is
             CLASS2     LIKE KLAH-CLINT,   "the class that MASTER is
             CLASS3     LIKE KLAH-CLINT,   "associated with and so on.
             CLASS4     LIKE KLAH-CLINT,   "The TEXT field is used in
             CLASS5     LIKE KLAH-CLINT,   "the case where the hiearachy
             CLASS6     LIKE KLAH-CLINT,   "structure has exceeded the
             CLASS7     LIKE KLAH-CLINT,   "program's scoop. 'Nothing'
             TEXT(20)   TYPE C,        "will appear in that case.
         END OF TABLE1.

* Table3 is used by the Function Module 'CLFM_SELECT_AUSP'.  it is
* also used a temporary storage location and nothing is ever APPENDED to
* it.
DATA   :  BEGIN OF TABLE3 OCCURS 10000.
        INCLUDE STRUCTURE AUSP.
DATA   :  END OF TABLE3.

* Nothing is ever APPENDED to this table.  It is used to store the
* values of the first line to be displayed for each record.
DATA   : BEGIN OF TABLE4 OCCURS 10000,
            FIELD1      LIKE AUSP-ATWRT,
            FIELD2      LIKE AUSP-ATWRT,
            FIELD3      LIKE AUSP-ATWRT,
            FIELD4      LIKE AUSP-ATWRT,
            FIELD5      LIKE AUSP-ATWRT,
            FIELD6      LIKE AUSP-ATWRT,
            FIELD7      LIKE AUSP-ATWRT,
            FIELD8      LIKE AUSP-ATWRT,
            FIELD9      LIKE AUSP-ATWRT,
             MATNR      LIKE MARA-MATNR,
** Nesh N. Laurencic added this:
          ALTMATNR(6)   TYPE C,   " This is added later
                                  " I'll use this field just for output
                                  " See perform get_alternate_material.
                                  " See also indicator.
          END OF TABLE4.
**
* Nesh N. Laurencic added this:
DATA: INDICATOR TYPE C,         " To determine if there is more
                                " alternate material,... output related
      C1 TYPE CURSOR,           " See cursor command
      WA LIKE AUSP.             " Working area
**
* TABLE2 is used to store the MULTIPLE values for a record.  This table
* is refreshed when a NEW record is introduced.
DATA   : BEGIN OF TABLE2 OCCURS 10000.
        INCLUDE STRUCTURE TABLE4.
DATA   : END OF TABLE2.

DATA   : BEGIN OF TABLE5 OCCURS 10000,
             FIELD1     LIKE CABNT-ATBEZ,
             FIELD2     LIKE SY-PAGNO,
             CLASS      LIKE KLAH-CLINT,
             CLASS2     LIKE KLAH-CLINT,
         END OF TABLE5.

DATA   : SEARCH_CLASS LIKE KLAH-CLINT,
          LINK1   LIKE KSSK-OBJEK,
          LINK2   LIKE LINK1,
          LINK3   LIKE LINK1,
          LINK4   LIKE LINK1,
          LINK5   LIKE LINK1,
          LINK6   LIKE LINK1.

* These variables "*-LINE"  are used to record the line number for each
* column.
DATA   : ORIG-LINE    TYPE I,
         FIELD2-LINE   LIKE ORIG-LINE,
         FIELD3-LINE   LIKE ORIG-LINE,
         FIELD4-LINE   LIKE ORIG-LINE,
         FIELD5-LINE   LIKE ORIG-LINE,
         FIELD6-LINE   LIKE ORIG-LINE,
         FIELD7-LINE   LIKE ORIG-LINE,
         FIELD8-LINE   LIKE ORIG-LINE,
         FIELD9-LINE   LIKE ORIG-LINE,
         NESH-LINE     LIKE ORIG-LINE,
         NEXT-LINE    LIKE ORIG-LINE.

* These variables are used to control the execution of certain
* procedures
DATA   : TEMP_MATNR   LIKE AUSP-OBJEK,
         DESCRIPTION  LIKE CABNT-ATBEZ,
         ABBRE_CODE   LIKE CABN-ATNAM,
         MULTI_VALUE_IND(1)    TYPE  C  VALUE 'N',
         CHECKPOINT1           TYPE  C  VALUE 'N',
         CHECKPOINT2           TYPE  C  VALUE 'N',
         CHECKPOINT3           TYPE  C  VALUE 'N',
         CHECKPOINT4           TYPE  C  VALUE 'N',
         CHECKPOINT5           TYPE  C  VALUE 'N',
         CHECKPOINT6           TYPE  C  VALUE 'N',
         CHECKPOINT7           TYPE  C  VALUE 'N',
         ALPHABIT(1)           TYPE  C VALUE SPACE,
         DISPLAY_CLASS         LIKE  SWOR-KSCHL,
         DISPLAY_SUBCLASS      LIKE  DISPLAY_CLASS,
         DISPLAY_SUPERIOR      LIKE  DISPLAY_CLASS,
         DISPLAY_REPCLASS      LIKE  DISPLAY_CLASS,
         DISPLAY_REPSUBCLASS   LIKE  DISPLAY_CLASS,
         HEADER1(1)            TYPE C VALUE 'Y',
         HEADER2(1)            TYPE C VALUE 'N',
         APPL_ATINN            LIKE AUSP-ATINN,
         MANU_ATINN            LIKE AUSP-ATINN,
         KEYW_ATINN            LIKE AUSP-ATINN,
         PRID_ATINN            LIKE AUSP-ATINN,
         SECD_ATINN            LIKE AUSP-ATINN,
         MODL_ATINN            LIKE AUSP-ATINN,
         MATL_ATINN            LIKE AUSP-ATINN,
         MANN_ATINN            LIKE AUSP-ATINN.

* This table will store the final records to be processed for output.
DATA   : BEGIN OF TEMPTABLE OCCURS 10000,
             SORT1      LIKE AUSP-ATWRT,
             SORT2      LIKE AUSP-ATWRT,
             SORT3      LIKE AUSP-ATWRT,
             SORT4      LIKE AUSP-ATWRT,
             SORT5      LIKE AUSP-ATWRT,
             SORT6      LIKE AUSP-ATWRT,
             SORT7      LIKE AUSP-ATWRT,
             SORT8      LIKE AUSP-ATWRT,
             SORT9      LIKE AUSP-ATWRT,
             SORT10     LIKE AUSP-ATWRT,
             SORT11     LIKE AUSP-ATWRT,
             MATNR      LIKE MARA-MATNR,     "MATERIAL NUMBER
             CLASS      LIKE KLAH-CLINT,
             CLASS2     LIKE KLAH-CLINT,
         END OF TEMPTABLE.
