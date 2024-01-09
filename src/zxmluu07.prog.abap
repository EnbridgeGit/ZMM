*&---------------------------------------------------------------------*
*&  Include           ZXMLUU07
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZXMLUU07                                       *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 15-Apr-2014                                    *
*& Object ID          : SDP57088                                       *
*& Application Area   : MM                                             *
*& Description        : Expected value should not exceed Overall limit *
*&---------------------------------------------------------------------*
*Expected value exceeds Overall limit
IF i_esuh-commitment > i_esuh-sumlimit.
  MESSAGE e303(me) WITH text-001 text-002.
ENDIF.
