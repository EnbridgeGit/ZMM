"Name: \PR:SAPLMR1M\EX:TRANSAKTIONS_INIT_01\EI
ENHANCEMENT 0 ZMIR4_CHANGE_AUTHORITY_CHECK.
*
  IF mir4_change = 'X'.
   DATA: vari_tcode LIKE shdtv-tcode.

   CALL FUNCTION 'RS_HDSYS_GET_TCODE'
     IMPORTING
       param_tcode           = vari_tcode.
   IF vari_tcode NE 'ZMIR4'.
     AUTHORITY-CHECK OBJECT 'Z_SRCINV'
              ID 'ACTVT' FIELD '02'.
     IF SY-SUBRC NE 0.
       MESSAGE E000(ZFI_WORKFLOW) WITH 'Not Authorized to Change Outside Workflow'.
     ENDIF.
   ENDIF.
  ENDIF.
ENDENHANCEMENT.
