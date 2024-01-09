************************************************************************
*             Modulpool  SAPFM06L                                      *
*                                                                      *
*        Allgemeine Routinen für Reports                               *
*        und Listen im Einkauf                                         *
************************************************************************

*------- Datenteil ----------------------------------------------------*
*include fm06ltop.
INCLUDE ZNMIM009.

*------- Funktionsbezogene Performroutinen ----------------------------*
INCLUDE FM06LFBE.            "Bestellentwicklung
INCLUDE FM06LFAD.            "Abrufdokumentation
INCLUDE FM06LFCD.            "Änderungsdokumentation.
INCLUDE FM06LFEK.            "Einkaufsbelegzeilen
*include fm06lfsl.           "Selektionsparameter
INCLUDE ZNMIM006.            "Selektionsparameter  "JLEE 11/18/1996
INCLUDE FM06LFVA.            "Variantendaten
*------------------ upgrade 1999/06/01 mdemeest ------------------------
*include fm06lffr.            "Freigabeverfahren
INCLUDE ZNMIM014.            "Freigabeverfahren
*-----------------------------------------------------------------------
INCLUDE FM06LFMS.            "Memory-Schnittstellenfunktion
INCLUDE FM06LFWE.            "WE-Vorschau

*------- Allgemeine Performroutinen -----------------------------------*
INCLUDE FM06LF01.
