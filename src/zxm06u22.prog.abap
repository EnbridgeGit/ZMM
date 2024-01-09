*----------------------------------------------------------------------*
*   INCLUDE ZXM06U22                                                   *
*----------------------------------------------------------------------*
*& 2021/04/17 birudurd COG changes to capture ME32L change for reset of
*                      release indicator
*----------------------------------------------------------------------*

DATA: lv_ebeln LIKE ekko-ebeln,
      lv_beket LIKE beket,
      wa_ekpo like bekpo.

* ES Integration specific RFQ user exit
* Please maintain the corresponding messages via transaction SE80
IF i_cekko-lifnr = 'ESOURCING'.
  CASE sy-tcode.
    WHEN 'ME41'.                                "Create RFQ
      LOOP AT it_beket INTO lv_beket WHERE NOT banfn IS initial.
        IF lv_beket-eindt < sy-datum.
* ES RFQ does not allow delivery date in the past.
          MESSAGE e002(zesi).
        ENDIF.

        SELECT SINGLE a~ebeln INTO lv_ebeln
        FROM eket AS a INNER JOIN
        ekko AS b ON ( a~ebeln = b~ebeln )
        WHERE a~banfn = lv_beket-banfn AND
        a~bnfpo = lv_beket-bnfpo AND
        b~bstyp = 'A' AND
        b~lifnr = 'ESOURCING'.

        IF sy-subrc = 0.
* Purchase requisition & & has already been included in another ES RFQ.
          MESSAGE e000(zesi) WITH lv_beket-banfn lv_beket-bnfpo.
        ENDIF.
      ENDLOOP.
    WHEN 'ME42'.                               "Change RFQ
      READ TABLE it_beket INTO lv_beket INDEX 1.
      SELECT SINGLE objky INTO lv_ebeln FROM nast WHERE
      kappl = 'EA' AND
      objky = lv_beket-ebeln AND
      kschl = 'NEU' AND
      nacha = '6' AND
      vstat = '1'.
      IF sy-subrc = 0.
* RFQ & has already been sent to e-Sourcing system. Changes not allowed.
        MESSAGE w001(zesi) WITH lv_beket-ebeln.
        LEAVE TO TRANSACTION 'ME43'.
      ENDIF.
  ENDCASE.
ENDIF.

* Start of changes COG to reset Release strategy
IF ( sy-tcode = 'ME32L' AND SY-UCOMM = 'BU').
*  I_CEKKO-USRC1 = 'X'.
  clear wa_ekpo.
  read table it_bekpo into wa_ekpo with key stapo = ' '.
  if sy-subrc ne 0.
    read table it_bekpo into wa_ekpo index 1.
    if sy-subrc = 0.
      i_cekko-usrc1 = 'Y'.
    endif.
  else.
   I_CEKKO-USRC1 = 'X'.
  endif.
ELSEIF  ( SY-TCODE = 'ME32K' AND SY-UCOMM = 'BU' ) .
  IF I_CEKKO-BSART = 'ZK' OR I_CEKKO-BSART = 'ZJ'.
    I_CEKKO-USRC1 = 'X'.
  ENDIF.
ENDIF.
* End of changes COG to reset Release strategy
E_CEKKO = I_CEKKO.
