*&---------------------------------------------------------------------*
*& Include          ZINFI_IVA_PAGADO_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_MAIN
*&---------------------------------------------------------------------*
*& Form principal del reporte
*&---------------------------------------------------------------------*
FORM f_main .
  PERFORM f_authority.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_AUTHORITY
*&---------------------------------------------------------------------*
*& Revicion de autorizacion
*&---------------------------------------------------------------------*
FORM f_authority .

  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
  FOR USER sy-uname
  ID 'BUKRS' FIELD p_bukrs
    ID 'ACTVT' FIELD '03'.


  IF sy-subrc = 0.

    PERFORM: f_get_data,
             f_build_alv.

  ELSE.
    MESSAGE TEXT-e01 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
*& Obtencion de los datos para ALV
*&---------------------------------------------------------------------*
FORM f_get_data .

*Type Locales
  TYPES: BEGIN OF ty_bkpf,
           bukrs     TYPE bukrs,
           belnr     TYPE belnr_d,
           gjahr     TYPE gjahr,
           blart     TYPE blart,
           bldat     TYPE bldat,
           budat     TYPE budat,
           monat     TYPE monat,
           xblnr     TYPE xblnr1,
           waers     TYPE waers,
           kursf     TYPE kursf,
           xreversed TYPE co_stokz,
           belnr2    TYPE belnr_d,
           fact      TYPE string,
         END OF ty_bkpf.

  TYPES: BEGIN OF ty_bseg,
           bukrs   TYPE bukrs,
           belnr   TYPE belnr_d,
           gjahr   TYPE gjahr,
           buzei   TYPE buzei,
           augbl   TYPE augbl,
           koart   TYPE koart,
           shkzg   TYPE shkzg,
           mwskz   TYPE mwskz,
           dmbtr   TYPE dmbtr,
           wrbtr   TYPE wrbtr,
           ktosl   TYPE ktosl,
           zuonr   TYPE dzuonr,
           sgtxt   TYPE sgtxt,
           fdlev   TYPE fdlev,
           saknr   TYPE saknr,
           hkont   TYPE hkont,
           lifnr   TYPE lifnr,
           zlsch   TYPE schzw_bseg,
           nebtr   TYPE nebtr,
           rebzg   TYPE rebzg,
           qbshb   TYPE qbshb,
           dmbe2   TYPE dmbe2,
           fwbas   TYPE fwbas,
           hwbas   TYPE hwbas,
           h_waers TYPE waers,
           h_blart TYPE blart,
           zzuuid  TYPE logmx_uuid,
           docpag  TYPE belnr_d,
           gjahrp  TYPE gjahr,
         END OF ty_bseg.

  TYPES: BEGIN OF ty_but000,
           partner    TYPE bu_partner,
           name_org1  TYPE bu_nameor1,
           name_org2  TYPE bu_nameor2,
           name_org3  TYPE bu_nameor3,
           name_org4  TYPE bu_nameor4,
           name_last  TYPE bu_namep_l,
           name_first TYPE bu_namep_f,
         END OF ty_but000.

  TYPES: BEGIN OF ty_dfkkbptaxnum,
           partner TYPE bu_partner,
           taxtype TYPE bptaxtype,
           taxnum  TYPE bptaxnum,
         END OF ty_dfkkbptaxnum.

  TYPES: BEGIN OF ty_payr,
           zbukr TYPE dzbukr,
           hbkid TYPE hbkid,
           hktid TYPE hktid,
           rzawe TYPE dzlsch,
           chect TYPE chect,
           lifnr TYPE lifnr,
           gjahr TYPE gjahr,
           rwbtr TYPE rwbtr,
         END OF ty_payr.

  TYPES: BEGIN OF ty_skat,
           spras TYPE spras,
           ktopl TYPE ktopl,
           saknr TYPE saknr,
           txt20 TYPE txt20_skat,
           txt50 TYPE txt50_skat,
         END OF ty_skat.

  TYPES: BEGIN OF ty_t001,
           bukrs TYPE bukrs,
           adrnr TYPE adrnr,
         END OF ty_t001.

  TYPES: BEGIN OF ty_adrc,
           addrnumber TYPE ad_addrnum,
           date_from  TYPE ad_date_fr,
           nation     TYPE ad_nation,
           name1      TYPE ad_name1,
           name2      TYPE ad_name2,
           name3      TYPE ad_name3,
           name4      TYPE ad_name4,
         END OF ty_adrc.

  TYPES: BEGIN OF ty_acdoca,
           rldnr  TYPE fins_ledger,
           rbukrs TYPE bukrs,
           gjahr  TYPE gjahr,
           belnr  TYPE belnr_d,
           docln  TYPE docln6,
           lifnr  TYPE lifnr,
         END OF ty_acdoca.

  TYPES: BEGIN OF ty_gastos,
           sociedad   TYPE bukrs,
           rfc_emisor TYPE zed_rfc_emisor,
           folio      TYPE xblnr1,
           cfdi_uuid  TYPE logmx_uuid_fin,
           documento  TYPE belnr_d,
           ejercicio  TYPE gjahr,
           met_pago   TYPE paym_methd,
         END OF ty_gastos.

*Tablas Locales
  DATA: lt_bkpf_doc     TYPE STANDARD TABLE OF ty_bkpf,
        lt_bkpf_fac     TYPE STANDARD TABLE OF ty_bkpf,
        lt_bkpf_aux     TYPE STANDARD TABLE OF ty_bkpf,
        lt_bseg_doc     TYPE STANDARD TABLE OF ty_bseg,
        lt_bseg_bnk     TYPE STANDARD TABLE OF ty_bseg,
        lt_bseg_aux     TYPE STANDARD TABLE OF ty_bseg,
        lt_bseg_loop    TYPE STANDARD TABLE OF ty_bseg,
        lt_bseg_fac     TYPE STANDARD TABLE OF ty_bseg,
        lt_bseg_full    TYPE STANDARD TABLE OF ty_bseg,
        lt_but000       TYPE STANDARD TABLE OF ty_but000,
        lt_dfkkbptaxnum TYPE STANDARD TABLE OF ty_dfkkbptaxnum,
        lt_payr         TYPE STANDARD TABLE OF ty_payr,
        lt_skat         TYPE STANDARD TABLE OF ty_skat,
        lt_meses        TYPE STANDARD TABLE OF t247,
        lt_gastos       TYPE STANDARD TABLE OF ty_gastos,
        lt_t030k        TYPE STANDARD TABLE OF t030k.

*Estructuras Locales
  DATA: ls_bkpf         TYPE ty_bkpf,
        ls_bkpf_aux     TYPE ty_bkpf,
        ls_bkpf_fac     TYPE ty_bkpf,
        ls_bseg         TYPE ty_bseg,
        ls_bseg_fac     TYPE ty_bseg,
        ls_bseg_bnk     TYPE ty_bseg,
        ls_bseg_doc     TYPE ty_bseg,
        ls_bseg_full    TYPE ty_bseg,
        ls_bseg_doc_aux TYPE ty_bseg,
        ls_bseg_aux     TYPE ty_bseg,
        ls_bseg_aux1    TYPE ty_bseg,
        ls_bseg_aux2    TYPE ty_bseg,
        ls_bseg_aux3    TYPE ty_bseg,
        ls_bseg_iv      TYPE ty_bseg,
        ls_but000       TYPE ty_but000,
        ls_dfkkbptaxnum TYPE ty_dfkkbptaxnum,
        ls_payr         TYPE ty_payr,
        ls_skat         TYPE ty_skat,
        ls_t001         TYPE ty_t001,
        ls_adrc         TYPE ty_adrc,
        ls_meses        TYPE t247,
        ls_outtab       TYPE zsfi_012_iva_pagado,
        ls_gastos       TYPE ty_gastos,
        ls_cambio       LIKE  bapi1093_0,

        lt_iva          TYPE STANDARD TABLE OF ftaxp,
        ls_iva          TYPE ftaxp,
        ls_t030k        TYPE t030k.

  DATA: lv_tabix     TYPE i VALUE 1,
        lv_flag,
        vl_parcial,
        lv_doc       TYPE belnr,
        lv_aux_lines TYPE i,
        lv_lines     TYPE i,
        lv_vini,
        lv_full,
        vl_int       TYPE i,
        lv_especial,
        lv_augbl,
        lv_doc_lines TYPE i,
        lv_nota.

  DATA: lv_aland    LIKE rf82t-land1,
        lv_datab    LIKE rf82t-datab,
        lv_mwskz    LIKE rf82t-mwskz,
        lv_txjcd    LIKE rf82t-txjcd,
        lv_unid(12) TYPE p DECIMALS 5.

*Constantes Locales
  CONSTANTS: co_text TYPE string VALUE 'Operaciones Realizadas Proveedores y/o Prestadores de Servicios'.

*------------------------------------------------------------Seleccion Cabecera*
  SELECT SINGLE bukrs "*
                adrnr
  FROM t001
  INTO ls_t001
  WHERE bukrs EQ p_bukrs.

  IF ls_t001 IS NOT INITIAL.

    SELECT SINGLE addrnumber
                  date_from
                  nation
                  name1
                  name2
                  name3
                  name4
    FROM adrc
    INTO ls_adrc
    WHERE addrnumber EQ ls_t001-adrnr.

  ENDIF.

*-----------------------------------------------------------------Seleccion ALV*

  PERFORM f_tvarv.

  SELECT *
    FROM t030k
    INTO TABLE lt_t030k
    WHERE ktopl EQ 'GVAS'
    AND   ktosl EQ 'VST'
    AND   mwskz IN ( 'PG', 'PI' )
    AND   land1 EQ 'MX'.


  SELECT bukrs "* Sociedad
         belnr "* No. Documento
         gjahr "* Ejercicio
         blart "  Clases de documento
         bldat
         budat
         monat
         xblnr
         waers " Moneda
         kursf " Tipo de cambio
         xreversed
  FROM bkpf
  INTO TABLE lt_bkpf_doc
  WHERE bukrs EQ p_bukrs  "Sociedad
  AND belnr IN s_belnr  "No. Documentos
  AND gjahr EQ p_gjahr  "Ejercicio
  AND budat IN s_budat  "Fecha contabilización
  AND monat EQ p_monat  "Periodo contable
  AND blart IN r_blart "( 'KZ','ZP','ZS' )
  AND ( xreversed EQ abap_false "NO acepta pagos anulados
  AND   stblg EQ space ).

  IF lt_bkpf_doc IS NOT INITIAL.

    SELECT bukrs "#EC CI_NO_TRANSFORM                      "#EC CI_NO_TRANSFORM "* Sociedad
           belnr "* No. Documentos
           gjahr "* Ejercicio
           buzei
           augbl " Doc. Comprobacion
           koart
           shkzg
           mwskz
           dmbtr
           wrbtr
           ktosl
           zuonr
           sgtxt
           fdlev
           saknr
           hkont
           lifnr
           zlsch " Via de pago
           nebtr
           rebzg
           qbshb
           dmbe2
           fwbas
           hwbas
           h_waers
           h_blart" Clases de documento
           zzuuid
    FROM bseg
    INTO TABLE lt_bseg_doc
    FOR ALL ENTRIES IN lt_bkpf_doc
    WHERE bukrs EQ lt_bkpf_doc-bukrs
    AND belnr EQ lt_bkpf_doc-belnr
    AND gjahr EQ lt_bkpf_doc-gjahr
    AND koart EQ 'K'
    AND shkzg EQ 'S'
    AND lifnr IN s_lifnr.
    "AND h_blart EQ lt_bkpf_doc-blart.

    SELECT bukrs                       "#EC CI_NO_TRANSFORM "* Sociedad
           belnr "* No. Documentos
           gjahr "* Ejercicio
           buzei
           augbl " Doc. Comprobacion
           koart
           shkzg
           mwskz
           dmbtr
           wrbtr
           ktosl
           zuonr
           sgtxt
           fdlev
           saknr
           hkont
           lifnr
           zlsch " Via de pago
           nebtr
           rebzg
           qbshb
           dmbe2
           fwbas
           hwbas
           h_waers
           h_blart" Clases de documento
           zzuuid
    FROM bseg
    INTO TABLE lt_bseg_bnk
    FOR ALL ENTRIES IN lt_bkpf_doc
    WHERE bukrs EQ lt_bkpf_doc-bukrs
    AND belnr EQ lt_bkpf_doc-belnr
    AND gjahr EQ lt_bkpf_doc-gjahr
    AND fdlev NE abap_false.

  ENDIF.

  IF lt_bseg_doc IS NOT INITIAL.

    SELECT partner                                 "#EC CI_NO_TRANSFORM
           name_org1
           name_org2
           name_org3
           name_org4
           name_last
           name_first
    FROM but000
    INTO TABLE lt_but000
    FOR ALL ENTRIES IN lt_bseg_doc
    WHERE partner EQ lt_bseg_doc-lifnr.

    SELECT partner                                 "#EC CI_NO_TRANSFORM
           taxtype
           taxnum
    FROM dfkkbptaxnum
    INTO TABLE lt_dfkkbptaxnum
    FOR ALL ENTRIES IN lt_bseg_doc
    WHERE partner EQ lt_bseg_doc-lifnr.



    SELECT bukrs                                   "#EC CI_NO_TRANSFORM
           belnr "* No. Documentos
           gjahr "* Ejercicio
           buzei
           augbl " Doc. Comprobacion
           koart
           shkzg
           mwskz
           dmbtr
           wrbtr
           ktosl
           zuonr
           sgtxt
           fdlev
           saknr
           hkont
           lifnr
           zlsch " Via de pago
           nebtr
           rebzg
           qbshb
           dmbe2
           fwbas
           hwbas
           h_waers
           h_blart" Clases de documento
           zzuuid
    FROM bseg
    INTO TABLE lt_bseg_aux
    FOR ALL ENTRIES IN lt_bseg_doc
    WHERE bukrs EQ lt_bseg_doc-bukrs
 ""   AND gjahr EQ lt_bseg_doc-gjahr
    AND augbl EQ lt_bseg_doc-belnr.
*    AND shkzg EQ 'H'.

  ENDIF.

  IF lt_bseg_aux IS NOT INITIAL.

    SELECT bukrs                      "#EC CI_NO_TRANSFORM  "* Sociedad
           belnr  "* No. Documentos
           gjahr  "* Ejercicio
           buzei
           augbl  " Doc. Comprobacion
           koart
           shkzg
           mwskz
           dmbtr
           wrbtr
           ktosl
           zuonr
           sgtxt
           fdlev
           saknr
           hkont
           lifnr
           zlsch  " Via de pago
           nebtr
           rebzg
           qbshb
           dmbe2
           fwbas
           hwbas
           h_waers
           h_blart" Clases de documento
           zzuuid
    FROM bseg
    INTO TABLE lt_bseg_fac
    FOR ALL ENTRIES IN lt_bseg_aux
    WHERE bukrs EQ lt_bseg_aux-bukrs
    AND belnr EQ lt_bseg_aux-belnr
    AND gjahr EQ lt_bseg_aux-gjahr.

    SELECT bukrs                      "#EC CI_NO_TRANSFORM  "* Sociedad
           belnr "* No. Documento
           gjahr "* Ejercicio
           blart "  Clases de documento
           bldat
           budat
           monat
           xblnr
           waers " Moneda
           kursf " Tipo de cambio
           xreversed
    FROM bkpf
    INTO TABLE lt_bkpf_fac
    FOR ALL ENTRIES IN lt_bseg_aux
    WHERE bukrs EQ lt_bseg_aux-bukrs  "Sociedad
    AND belnr EQ lt_bseg_aux-belnr  "No. Documentos
    AND gjahr EQ lt_bseg_aux-gjahr  "Ejercicio
    AND ( xreversed EQ abap_false "No acepta pagos anulados
    OR    stblg NE space ).

  ENDIF.

  IF lt_bseg_doc IS NOT INITIAL.

    SELECT bukrs                      "#EC CI_NO_TRANSFORM  "* Sociedad
           belnr  "* No. Documentos
           gjahr  "* Ejercicio
           buzei
           augbl  " Doc. Comprobacion
           koart
           shkzg
           mwskz
           dmbtr
           wrbtr
           ktosl
           zuonr
           sgtxt
           fdlev
           saknr
           hkont
           lifnr
           zlsch  " Via de pago
           nebtr
           rebzg
           qbshb
           dmbe2
           fwbas
           hwbas
           h_waers
           h_blart" Clases de documento
           zzuuid
    FROM bseg
    APPENDING TABLE lt_bseg_fac
    FOR ALL ENTRIES IN lt_bseg_doc
    WHERE bukrs EQ lt_bseg_doc-bukrs
    AND belnr EQ lt_bseg_doc-rebzg.
    "AND gjahr EQ lt_bseg_doc-gjahr.

    IF  sy-subrc NE 0.
      lv_flag = abap_true.
    ENDIF.

*GASTOS
    SELECT sociedad
           rfc_emisor
           folio
           cfdi_uuid
           documento
           ejercicio
           met_pago
    FROM zuuid_gastos
    INTO TABLE lt_gastos
    FOR ALL ENTRIES IN lt_bseg_doc
    WHERE sociedad  EQ lt_bseg_doc-bukrs
    AND documento EQ lt_bseg_doc-belnr
    AND ejercicio EQ lt_bseg_doc-gjahr.

  ENDIF.

  IF lt_bseg_bnk IS NOT INITIAL.

    SELECT zbukr                                   "#EC CI_NO_TRANSFORM
           hbkid
           hktid
           rzawe
           chect
           lifnr
           gjahr
           rwbtr
    FROM payr
    INTO TABLE lt_payr
    FOR ALL ENTRIES IN lt_bseg_bnk
    WHERE zbukr EQ lt_bseg_bnk-bukrs
    AND lifnr IN s_lifnr
    AND vblnr EQ lt_bseg_bnk-belnr
    AND gjahr EQ lt_bseg_bnk-gjahr.

    SELECT spras                                   "#EC CI_NO_TRANSFORM
           ktopl
           saknr
           txt20
           txt50
    FROM skat                                          "#EC CI_GENBUFF.
    INTO TABLE lt_skat
    FOR ALL ENTRIES IN lt_bseg_bnk
    WHERE spras EQ sy-langu
    AND saknr EQ lt_bseg_bnk-hkont.

  ENDIF.

  SORT lt_bkpf_doc
  BY belnr
     bukrs
     gjahr.

*------------------------------------------------------------Armado de Cabecera*

*Drill down
  gv_buk = p_bukrs.
  gv_gjr = p_gjahr.

  "*Razon social (Cabecera)
  IF ls_adrc IS NOT INITIAL.
    CONCATENATE ls_adrc-name1 ls_adrc-name2 ls_adrc-name3 ls_adrc-name4
    INTO gv_r_social SEPARATED BY space.
  ENDIF.

*Nombre del reporte (Cabecera)
  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      language    = sy-langu
    TABLES
      month_names = lt_meses.

  READ TABLE lt_meses INTO ls_meses WITH KEY mnr = p_monat BINARY SEARCH.

  IF ls_meses IS NOT INITIAL.
    CONCATENATE co_text ls_meses-ltx p_gjahr INTO gv_nombre_re SEPARATED BY space.
  ENDIF.

*Nombre del proveedor (Cabecera)
  IF lt_but000 IS NOT INITIAL
  AND s_lifnr IS NOT INITIAL.

    SORT: lt_but000 BY partner,
    lt_dfkkbptaxnum BY partner.

    LOOP AT lt_but000 INTO ls_but000.

      IF ls_but000-name_org1 IS NOT INITIAL.
        CONCATENATE ls_but000-name_org1 ls_but000-name_org2 ls_but000-name_org3 ls_but000-name_org4
        INTO gs_cabecera-nombre_pro SEPARATED BY space.
      ELSE.
        CONCATENATE ls_but000-name_first ls_but000-name_last
        INTO gs_cabecera-nombre_pro SEPARATED BY space.
      ENDIF.

*RFC del proveedor (Cabecera)
      READ TABLE lt_dfkkbptaxnum INTO ls_dfkkbptaxnum WITH KEY partner = ls_but000-partner BINARY SEARCH.

      IF ls_dfkkbptaxnum IS NOT INITIAL.
        gs_cabecera-rfc_pro = ls_dfkkbptaxnum-taxnum.
      ENDIF.

      APPEND gs_cabecera TO gt_cabecera.

      CLEAR: gs_cabecera,
      ls_but000,
      ls_dfkkbptaxnum.

    ENDLOOP.

  ENDIF.

  LOOP AT lt_bseg_aux ASSIGNING FIELD-SYMBOL(<fs_aa>).
    <fs_aa>-docpag = <fs_aa>-augbl.
    <fs_aa>-gjahrp = <fs_aa>-gjahr.
  ENDLOOP.


*------------------------------Llenado de la OUTTAB----------------------------*
  SORT: lt_bkpf_doc BY bukrs belnr gjahr,
  lt_bkpf_fac BY bukrs belnr gjahr,
  lt_bseg_aux BY bukrs belnr gjahr,
  lt_bseg_doc BY bukrs belnr gjahr,
  lt_bseg_fac BY belnr buzei augbl.

*Agregando Pagos parciales a la tabla
  LOOP AT lt_bseg_doc INTO ls_bseg_doc.

    LOOP AT lt_bseg_fac INTO ls_bseg_aux WHERE belnr EQ ls_bseg_doc-rebzg
    AND buzei EQ 001.

      READ TABLE lt_bseg_aux TRANSPORTING NO FIELDS WITH KEY belnr = ls_bseg_aux-belnr BINARY SEARCH.

      "IF sy-subrc NE 0.
      ls_bseg_aux-docpag = ls_bseg_doc-belnr.
      ls_bseg_aux-gjahrp = ls_bseg_doc-gjahr.
      APPEND ls_bseg_aux TO lt_bseg_aux.
      "ENDIF.

      CLEAR ls_bseg_aux.

    ENDLOOP.

    CLEAR ls_bseg_aux.

    SORT: lt_bseg_aux BY bukrs belnr gjahr.


    ls_bkpf_aux-belnr2 = ls_bseg_doc-belnr.

    IF ls_bseg_doc-augbl IS NOT INITIAL.
      ls_bkpf_aux-fact = ls_bseg_doc-augbl.
    ELSE.
      ls_bkpf_aux-fact = ls_bseg_doc-rebzg.
    ENDIF.

    APPEND ls_bkpf_aux TO lt_bkpf_aux.

    CLEAR ls_bseg_doc.

  ENDLOOP.

  SELECT bukrs "* Sociedad
         belnr "* No. Documento
         gjahr "* Ejercicio
         blart "  Clases de documento
         bldat
         budat
         monat
         xblnr
         waers " Moneda
         kursf " Tipo de cambio
         xreversed
  FROM bkpf
  APPENDING TABLE lt_bkpf_fac
  FOR ALL ENTRIES IN lt_bseg_aux
  WHERE bukrs EQ lt_bseg_aux-bukrs  "Sociedad
  AND belnr EQ lt_bseg_aux-belnr  "No. Documentos
  AND gjahr EQ lt_bseg_aux-gjahr "Ejercicio
  AND xreversed EQ abap_false. "No acepta pagos anulados

  SORT: lt_bkpf_fac BY belnr.


*CAMPOS DEL ALV----------------------------------------------------------------*
  DELETE lt_bseg_aux WHERE h_blart EQ 'KZ' AND shkzg EQ 'S'.

  "DELETE lt_bkpf_doc WHERE blart eq 'KZ'.

  LOOP AT lt_bkpf_doc
  ASSIGNING FIELD-SYMBOL(<lfs_bkpf>).

    ls_outtab-doc_pago_sap = <lfs_bkpf>-belnr.

    SORT: lt_bseg_doc BY belnr, "t_bseg
          lt_bseg_fac BY belnr buzei.

    READ TABLE lt_bseg_doc "t_bseg
    ASSIGNING FIELD-SYMBOL(<lfs_bseg>)
    WITH KEY bukrs = <lfs_bkpf>-bukrs
             belnr = <lfs_bkpf>-belnr
             gjahr = <lfs_bkpf>-gjahr
    BINARY SEARCH.

    IF <lfs_bseg> IS ASSIGNED. "DOCUMENTOS DE PAGO

      READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = <lfs_bseg>-lifnr.

      IF ls_but000-name_org1 IS NOT INITIAL.
        CONCATENATE ls_but000-name_org1 ls_but000-name_org2 ls_but000-name_org3 ls_but000-name_org4
        INTO ls_outtab-nombre_prov SEPARATED BY space. "*Nombre del proveedor
      ELSE.
        CONCATENATE ls_but000-name_first ls_but000-name_last
        INTO ls_outtab-nombre_prov SEPARATED BY space. "*Nombre del proveedor
      ENDIF.

      IF ls_but000 IS NOT INITIAL.
        READ TABLE lt_dfkkbptaxnum INTO ls_dfkkbptaxnum WITH KEY partner = ls_but000-partner.
        CLEAR: ls_but000.
      ENDIF.

      IF ls_dfkkbptaxnum IS NOT INITIAL.
        ls_outtab-rfc_prov = ls_dfkkbptaxnum-taxnum. "*RFC del proveedor
        CLEAR: ls_dfkkbptaxnum.
      ENDIF.

*************************************************************************FACTURA
      DESCRIBE TABLE lt_bseg_aux LINES lv_lines.

      IF lv_lines GT 1 AND lv_vini EQ abap_true.
*        DELETE lt_bseg_aux INDEX 1.
      ENDIF.

      lt_bseg_loop = lt_bseg_aux.

      lv_vini = abap_true.

      LOOP AT lt_bseg_loop INTO ls_bseg_aux
        WHERE docpag EQ <lfs_bkpf>-belnr .""AND gjahrp EQ <lfs_bkpf>-gjahr. """

        CHECK <lfs_bkpf>-belnr NE ls_bseg_aux-belnr.

        CHECK ls_bseg_aux-h_blart NE 'GA' AND ls_bseg_aux-h_blart NE 'KZ'.

        lv_tabix = sy-tabix.

        "READ TABLE lt_bseg_loop INTO ls_bseg_aux WITH KEY belnr = <lfs_bseg>-rebzg.

        IF ls_bseg_aux IS INITIAL.
          "READ TABLE lt_bseg_loop INTO ls_bseg_aux WITH KEY augbl = <lfs_bkpf>-belnr.
        ENDIF.

        DESCRIBE TABLE lt_bseg_loop LINES lv_lines.

***        IF lv_lines GT 1. """
***          DELETE lt_bseg_loop INDEX lv_tabix. """
***        ENDIF. """

        ls_outtab-doc_sap_factura = ls_bseg_aux-belnr. "*Documento SAP fatura
        ls_outtab-gjahr = ls_bseg_aux-gjahr. "*Documento SAP fatura

        IF ls_bseg_aux IS NOT INITIAL.

          READ TABLE lt_bseg_fac INTO ls_bseg_aux1 WITH KEY belnr = ls_bseg_aux-belnr
          buzei = 001 BINARY SEARCH.

          READ TABLE lt_bseg_fac INTO ls_bseg_aux2 WITH KEY belnr = ls_bseg_aux-belnr
          buzei = 002 BINARY SEARCH.

          IF <lfs_bkpf>-waers EQ 'MXN'.

            SORT: lt_bseg_fac BY belnr ktosl.

            READ TABLE lt_bseg_fac INTO ls_bseg_aux3 WITH KEY belnr = ls_bseg_aux-belnr
            ktosl = 'VST' BINARY SEARCH.

            IF ls_bseg_aux3-dmbtr EQ 0.

              SORT: lt_bseg_fac BY belnr buzei.
              READ TABLE lt_bseg_fac INTO ls_bseg_aux3 WITH KEY belnr = ls_bseg_aux-belnr
              buzei = 003 BINARY SEARCH.

            ENDIF.

          ELSE.

            SORT: lt_bseg_fac BY belnr ktosl.

            READ TABLE lt_bseg_fac INTO ls_bseg_aux3 WITH KEY belnr = ls_bseg_aux-belnr
            ktosl = 'VST' BINARY SEARCH.

          ENDIF.

          CLEAR lv_nota.
          IF  ls_bseg_aux-koart = 'K'
                AND ls_bseg_aux-shkzg = 'S'.

            lv_nota = abap_true.

          ENDIF.


          SORT: lt_bseg_fac BY belnr ktosl.

          READ TABLE lt_bseg_fac INTO ls_bseg_iv WITH KEY belnr = ls_bseg_aux-belnr
          ktosl = 'WIT' BINARY SEARCH.

          SORT: lt_bseg_fac BY belnr buzei augbl.

          CLEAR: ls_bseg_aux.
        ENDIF.

        READ TABLE lt_bseg_fac INTO ls_bseg_aux WITH KEY belnr = ls_outtab-doc_sap_factura BINARY SEARCH.

        IF  <lfs_bkpf>-blart EQ 'CI'
        AND ls_bseg_aux-zzuuid IS INITIAL.
          ls_outtab-no_folio = ls_bseg_aux1-sgtxt.  "*No. folio fiscal CFDI
        ELSE.
          ls_outtab-no_folio = ls_bseg_aux-zzuuid. "*No. folio fiscal CFDI
        ENDIF.
        ls_outtab-no_folio = ls_bseg_aux-zzuuid. "UUID

        CLEAR ls_outtab-no_folio.

        PERFORM f_read_text USING ls_bseg_aux-bukrs ls_bseg_aux-belnr
                ls_bseg_aux-gjahr 'BELEG' 'YUUD' CHANGING ls_outtab-no_folio.

      "  TRANSLATE ls_outtab-no_folio TO UPPER CASE.


        CLEAR: ls_bseg_aux.


        IF ls_bseg_aux1-sgtxt IS NOT INITIAL.
          ls_outtab-concepto = ls_bseg_aux1-sgtxt. "*Concepto
        ENDIF.


        ls_outtab-shkzg = ls_bseg_aux1-shkzg.

        READ TABLE lt_bkpf_doc INTO ls_bkpf_aux WITH KEY belnr = <lfs_bseg>-belnr.

        IF ls_bkpf_aux IS NOT INITIAL.

          ls_outtab-fecha_pago = ls_bkpf_aux-bldat." ls_bkpf-bldat. "*Fecha de pago (contabilización)

          CLEAR: ls_cambio-exch_rate_v.

          CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
            EXPORTING
              rate_type  = 'M'
              from_curr  = 'MXN'
              to_currncy = 'USD'
              date       = ls_bkpf_aux-bldat "budat
            IMPORTING
              exch_rate  = ls_cambio.

          ls_outtab-fecha_pago_fiscal = ls_bkpf_aux-bldat."budat. "*Fecha de pago comprobante fiscal

          ls_outtab-doc_pago_sap = ls_bkpf_aux-belnr. "*Documento de pago SAP

          ls_outtab-zno = ls_bkpf_aux-xblnr. "*No.

          ls_outtab-fecha = ls_bkpf_aux-budat. "*Fecha



          READ TABLE lt_bkpf_fac INTO ls_bkpf_fac WITH KEY belnr = ls_outtab-doc_sap_factura
          blart = 'KR'.


          IF ls_bkpf_fac IS NOT INITIAL.
            ls_outtab-no_factura = ls_bkpf_fac-xblnr. "*No. Factura
            ls_outtab-fecha_comprobante = ls_bkpf_fac-bldat."budat. "*Fecha del comprobante fiscal
          ELSE.
            READ TABLE lt_bkpf_fac INTO ls_bkpf_fac WITH KEY belnr = ls_outtab-doc_sap_factura.
            ls_outtab-no_factura = ls_bkpf_fac-xblnr. "*No. Factura
            ls_outtab-fecha_comprobante = ls_bkpf_fac-bldat."budat. "*Fecha del comprobante fiscal
          ENDIF.

          ls_outtab-moneda_pago = ls_bkpf_fac-waers.
          ls_outtab-tc_historico = ls_bkpf_fac-kursf.

          CLEAR: ls_cambio-exch_rate_v, ls_outtab-tc_historico.

          CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
            EXPORTING
              rate_type  = 'M'
              from_curr  = 'MXN'
              to_currncy = 'USD'
              date       = ls_bkpf_fac-bldat "budat
            IMPORTING
              exch_rate  = ls_cambio.

          IF  ls_outtab-moneda_pago EQ 'USD'.
            ls_outtab-tc_historico = ls_cambio-exch_rate_v.
          ENDIF.

          CLEAR: ls_bkpf_fac.

        ENDIF.


        IF ls_outtab-moneda_pago EQ 'USD'. "*Moneda del documento (Dolares)

          CLEAR: ls_cambio-exch_rate_v, ls_outtab-tc_pago.

          CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
            EXPORTING
              rate_type  = 'M'
              from_curr  = 'MXN'
              to_currncy = 'USD'
              date       = ls_bkpf_aux-bldat "budat
            IMPORTING
              exch_rate  = ls_cambio.

          ls_outtab-tc_pago = ls_cambio-exch_rate_v.

          IF ls_bseg_aux3 IS NOT INITIAL.

            ls_outtab-iva_dlls = ls_bseg_aux3-dmbe2. "*IVA DLLS (IVA al tipo de cambio)

            ls_outtab-iva_mxn_tc = ( ls_bseg_aux3-dmbe2 * ls_outtab-tc_historico ). "*IVA en MXN a t.c. histórico (IVA al tipo de cambio)

            ls_outtab-iva_mn = ( ls_bseg_aux3-dmbe2 * ls_outtab-tc_pago ). "*IVA M.N.
            ls_outtab-id_iva = ls_bseg_aux3-mwskz. "IVA ID

            "ls_outtab-total_dlls = ls_bseg_aux3-hwbas. "*Total DLLS (Importe total al tipo de cambio)
            "ls_outtab-total_mxn_hist = ls_bseg_aux3-fwbas. "*Total en MXN a t.c. histórico

            LOOP AT lt_bseg_fac ASSIGNING FIELD-SYMBOL(<fs_lll>) WHERE belnr = ls_bseg_aux3-belnr.

              IF <fs_lll>-mwskz EQ 'PI' OR <fs_lll>-mwskz EQ 'PG'.
                ls_outtab-id_iva = <fs_lll>-mwskz. "IVA ID
              ENDIF.

            ENDLOOP.

            CONDENSE ls_outtab-id_iva.

            CLEAR: ls_bseg_aux3.
          ENDIF.

          IF ls_bseg_aux1 IS NOT INITIAL.
            ls_outtab-total_dlls = ls_bseg_aux1-dmbe2. "*Total DLLS (Importe total al tipo de cambio)
            ls_outtab-total_mxn_hist = ls_bseg_aux1-dmbtr. "*Total en MXN a t.c. histórico
            CLEAR: ls_bseg_aux1.
          ENDIF.
"BREAK apalmar.
          IF ls_bseg_iv IS NOT INITIAL.
            ls_outtab-retencion_iv = ls_bseg_iv-qbshb. "*IVA Retenido en MXN
            CLEAR: ls_bseg_iv.
          ENDIF.

*        ls_outtab-tc_historico = <lfs_bkpf>-kursf. "*T.C. Historico de la factura

          IF ls_bseg_aux2 IS NOT INITIAL.
            ls_outtab-importe_mn = ( ls_bseg_aux2-dmbe2 * ls_outtab-tc_pago ). "*Importe M.N. (Importe sin IVA al tipo de cambio)
            ls_outtab-importge_dlls  = ( ls_outtab-total_dlls - ls_outtab-iva_dlls ). "*Importe DLLS (Sin IVA al tipo de cambio)
            ls_outtab-importe_mxn_tc = ( ls_outtab-total_mxn_hist - ls_outtab-iva_mxn_tc ). "*Importe en MXN a t.c. histórico (Importe sin IVA al tipo de cambio)

            CLEAR: ls_bseg_aux2.
          ENDIF.

          ls_outtab-total_mn = ( ls_outtab-importe_mn + ls_outtab-iva_mn ). " + ls_outtab-iva_retenido_mxn ). "*Total  M.N.

        ELSEIF ls_outtab-moneda_pago EQ 'MXN'. "*Moneda del documento (Pesos)

          IF ls_bseg_aux3 IS NOT INITIAL.
            ls_outtab-iva_mxn_tc = ls_bseg_aux3-dmbtr. "*IVA en MXN a t.c. histórico (IVA al tipo de cambio)
            ls_outtab-iva_mn = ls_bseg_aux3-dmbtr. "*IVA M.N.
            ls_outtab-id_iva = ls_bseg_aux3-mwskz. "IVA ID

            LOOP AT lt_bseg_fac ASSIGNING FIELD-SYMBOL(<fs_ll2>) WHERE belnr = ls_bseg_aux3-belnr.

              IF <fs_ll2>-mwskz EQ 'PI' OR <fs_ll2>-mwskz EQ 'PG'.
                ls_outtab-id_iva = <fs_ll2>-mwskz. "IVA ID
              ENDIF.

            ENDLOOP.

            CONDENSE ls_outtab-id_iva.

            IF ls_outtab-id_iva IS INITIAL.
              CLEAR: ls_outtab-iva_mxn_tc, ls_outtab-iva_mn.
            ENDIF.

            CLEAR: ls_bseg_aux3.
          ENDIF.

          IF ls_bseg_aux1 IS NOT INITIAL.
            ls_outtab-total_mxn_hist = ls_bseg_aux1-dmbtr. "*Total en MXN a t.c. histórico ********************* checar este
            CLEAR: ls_bseg_aux1.
          ENDIF.

          IF ls_bseg_iv IS NOT INITIAL.
            ls_outtab-retencion_iv = ls_bseg_iv-qbshb. "*IVA Retenido en MXN
            CLEAR: ls_bseg_iv.
          ENDIF.

          IF ls_bseg_aux2 IS NOT INITIAL.
            ls_outtab-importe_mn = ls_bseg_aux2-dmbtr. "*Importe M.N. (Importe sin IVA al tipo de cambio)
            ls_outtab-importe_mxn_tc = ( ls_outtab-total_mxn_hist - ls_outtab-iva_mxn_tc ). "*Importe en MXN a t.c. histórico (Importe sin IVA al tipo de cambio)
            CLEAR: ls_bseg_aux2.
          ENDIF.

          ls_outtab-total_mn = ( ls_outtab-importe_mn + ls_outtab-iva_mn )." + ls_outtab-iva_retenido_mxn ). "*Total  M.N.

        ENDIF.

        READ TABLE lt_gastos INTO ls_gastos WITH KEY sociedad = <lfs_bseg>-bukrs "DOCUMENTO
        documento = <lfs_bseg>-belnr
        ejercicio = <lfs_bseg>-gjahr.

        IF sy-subrc EQ 0.
          ls_outtab-forma_pago_fiscal = ls_gastos-met_pago. "Forma pago comprobante
        ENDIF.


        IF <lfs_bseg>-zlsch EQ 'C'. " Via de pago

          READ TABLE lt_payr INTO ls_payr WITH KEY zbukr = <lfs_bseg>-bukrs
          rzawe = <lfs_bseg>-zlsch.

          IF sy-subrc EQ 0.

            ls_outtab-no_cheque  = ls_payr-chect. "*No. de cheque y/o transf.
            ls_outtab-importe_ch = abs( ls_payr-rwbtr ). "*Importe  ch. y/o transfer.

          ENDIF.

        ELSE.

          ls_outtab-no_cheque  = <lfs_bseg>-belnr. "*No. de cheque y/o transf.

          IF <lfs_bseg>-nebtr IS NOT INITIAL.
            ls_outtab-importe_ch = <lfs_bseg>-nebtr. "*Importe  ch. y/o transfer.
          ELSE.
            ls_outtab-importe_ch = ( <lfs_bseg>-wrbtr * -1 ). "*Importe  ch. y/o transfer.
          ENDIF.

        ENDIF.

        READ TABLE lt_bseg_bnk INTO ls_bseg_bnk WITH KEY belnr = <lfs_bseg>-belnr.

        IF ls_bseg_bnk IS NOT INITIAL.

          READ TABLE lt_skat INTO ls_skat WITH KEY spras = sy-langu
          saknr = ls_bseg_bnk-hkont.

          IF sy-subrc EQ 0.
            CONCATENATE ls_skat-txt50 ls_bkpf-monat <lfs_bkpf>-gjahr
            INTO ls_outtab-inst_bancaria SEPARATED BY space. "*Institucion bancaria no. de cuenta y periodo del Edo. de Cta.
          ENDIF.

        ENDIF.

        lv_aland = 'MX'.
        lv_datab = space.
        lv_mwskz = ls_outtab-id_iva.
        lv_txjcd = space.

        CALL FUNCTION 'GET_TAX_PERCENTAGE'
          EXPORTING
            aland   = lv_aland
            datab   = lv_datab
            mwskz   = lv_mwskz
            txjcd   = lv_txjcd
          TABLES
            t_ftaxp = lt_iva.

        READ TABLE lt_iva INTO ls_iva INDEX 1.
        ls_iva-kbetr = ls_iva-kbetr / 1000.
        lv_unid = ls_iva-kbetr + 1.

        CLEAR: ls_outtab-importe_mn,
        ls_outtab-iva_mn,
        ls_outtab-total_mn.

********************************************************************************************************NUEVO
        IF ls_outtab-id_iva EQ 'PG'
        OR ls_outtab-id_iva EQ 'PI'.

          READ TABLE lt_t030k INTO ls_t030k WITH KEY ktosl = 'VST'
          mwskz = ls_outtab-id_iva
          land1 = 'MX'.
          IF ls_t030k IS NOT INITIAL.
            SORT lt_bseg_fac BY belnr hkont.
            READ TABLE lt_bseg_fac INTO ls_bseg_aux WITH KEY belnr = ls_outtab-doc_sap_factura
            hkont = ls_t030k-konts.

            CLEAR: ls_outtab-iva_mxn_tc,
            ls_outtab-importe_mxn_tc.

            IF ls_outtab-moneda_pago EQ 'MXN'.
              ls_outtab-iva_mxn_tc = ls_bseg_aux-dmbtr.
            ELSE."Dolares
              ls_outtab-iva_dlls   = ls_bseg_aux-dmbe2. "*IVA DLLS (IVA al tipo de cambio)
              ls_outtab-iva_mxn_tc = ( ls_outtab-iva_dlls * ls_outtab-tc_historico ). "*IVA en MXN a t.c. histórico (IVA al tipo de cambio)

              ls_outtab-importge_dlls  = ( ls_outtab-total_dlls - ls_outtab-iva_dlls ).
            ENDIF.

            ls_outtab-importe_mxn_tc = ( ls_outtab-total_mxn_hist - ls_outtab-iva_mxn_tc ).

          ENDIF.

        ENDIF.
********************************************************************************************************NUEVO

        IF lv_nota NE abap_true. "Nota de credito

          IF ls_outtab-moneda_pago EQ 'MXN'.
            ls_outtab-total_mn = ls_outtab-importe_ch.
          ELSE.
            ls_outtab-total_mn = ( ls_outtab-importe_ch * ls_outtab-tc_pago ).
          ENDIF.

          ls_outtab-importe_mn = ls_outtab-total_mn / lv_unid.
          ls_outtab-iva_mn     = ls_outtab-importe_mn * ls_iva-kbetr.


          IF ls_outtab-id_iva EQ 'PG'
          OR ls_outtab-id_iva EQ 'PI'.

            CLEAR: lv_unid.

            lv_unid = ls_outtab-total_mn / ls_outtab-importe_mxn_tc.

            ls_outtab-importe_mn = ls_outtab-total_mn / lv_unid.
            ls_outtab-iva_mn     = ls_outtab-importe_mn * ( lv_unid - 1 ).


          ELSE.

            ls_outtab-importe_mn = ls_outtab-total_mn / lv_unid.
            ls_outtab-iva_mn     = ls_outtab-importe_mn * ls_iva-kbetr.

          ENDIF.


        ELSE.

          ls_outtab-importge_dlls   = ( ls_outtab-importge_dlls * -1 ).
          ls_outtab-iva_dlls        = ( ls_outtab-iva_dlls * -1 ).
          ls_outtab-total_dlls      = ( ls_outtab-total_dlls * -1 ).
          ls_outtab-importe_mxn_tc  = ( ls_outtab-importe_mxn_tc * -1 ).
          ls_outtab-iva_mxn_tc      = ( ls_outtab-iva_mxn_tc * -1 ).
          ls_outtab-total_mxn_hist  = ( ls_outtab-total_mxn_hist * -1 ).

          CLEAR: ls_outtab-retencion_iv,
          ls_outtab-retencion_is,
          ls_outtab-retencion_no,
          ls_outtab-importe_ch.

        ENDIF.

        IF ( ls_outtab-importe_ch > ls_outtab-total_mxn_hist ) AND
          ( ls_outtab-importge_dlls IS INITIAL AND ls_outtab-moneda_pago EQ 'MXN' ).
          ls_outtab-iva_mn = ls_outtab-iva_mxn_tc.
          ls_outtab-importe_mn = ls_outtab-importe_mxn_tc.
          ls_outtab-total_mn = ( ls_outtab-iva_mn + ls_outtab-importe_mn ) .
          "  ls_outtab-importe_ch = ls_outtab-total_mn.
        ENDIF.

        IF ( ls_outtab-importe_ch > ls_outtab-importge_dlls ) AND
         ( ls_outtab-importge_dlls IS NOT INITIAL AND ls_outtab-moneda_pago EQ 'USD' ).
          ls_outtab-iva_mn = ls_outtab-iva_dlls * ls_outtab-tc_pago.
          ls_outtab-importe_mn = ls_outtab-importge_dlls * ls_outtab-tc_pago.
          ls_outtab-total_mn = ( ls_outtab-iva_mn + ls_outtab-importe_mn ) .
          " ls_outtab-importe_ch = ls_outtab-importge_dlls.
        ENDIF.


        APPEND ls_outtab TO gt_outtab.

      ENDLOOP. """

    ENDIF.

    CLEAR: ls_bkpf,
    ls_bseg,
    ls_outtab,
    lv_nota.

  ENDLOOP.

  PERFORM: f_get_imp_ch.

  "IF sy-uname eq 'APALMAR'.
    PERFORM f_set_montos.
  "ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_BUILD_ALV
*&---------------------------------------------------------------------*
*& Construccion del ALV
*&---------------------------------------------------------------------*
FORM f_build_alv.

  PERFORM:
  f_create_container,
  f_build_fc,
  f_build_h,
  f_build_layout,
  f_create_alv,
  f_set_handlers,
  f_show_alv.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CREATE_CONTAINER
*&---------------------------------------------------------------------*
*& Creacion del contenedor ALV
*&---------------------------------------------------------------------*
FORM f_create_container.

  CREATE OBJECT gr_custom_container
    EXPORTING
      container_name              = 'ALV_CONT'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_BUILD_FC
*&---------------------------------------------------------------------*
*& Creacion del Catalogo de campos
*&---------------------------------------------------------------------*
FORM f_build_fc.

  DATA: ls_fcat TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSFI_012_IVA_PAGADO'
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT gt_fcat INTO ls_fcat.

*    IF ls_fcat-fieldname EQ 'ZUUID_DOC' "NO MOSTRAR CAMPOS
*    OR ls_fcat-fieldname EQ 'SHKZG'.

    IF ls_fcat-fieldname EQ 'SHKZG'."NO MOSTRAR CAMPOS
      ls_fcat-no_out = abap_true.
    ENDIF.

    ls_fcat-col_opt = abap_true.
    MODIFY gt_fcat FROM ls_fcat TRANSPORTING col_opt no_out.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_BUILD_H
*&---------------------------------------------------------------------*
*& Construccion de la Cabecera
*&---------------------------------------------------------------------*
FORM f_build_h.

  CREATE OBJECT gr_header
    EXPORTING
      container_name = 'HEADER'.

  CREATE OBJECT gr_top_viewer
    EXPORTING
      parent             = gr_header
    EXCEPTIONS
      cntl_error         = 1
      cntl_install_error = 2
      dp_install_error   = 3
      dp_error           = 4
      OTHERS             = 5.

  CREATE OBJECT gr_top_document
    EXPORTING
      style = 'ALV_GRID'.

  PERFORM f_get_h.

*Display General*******************************

  gr_top_document->html_control = gr_top_viewer.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      document = gr_top_document
      bottom   = space.

  CALL METHOD gr_top_document->merge_document.

  CALL METHOD gr_top_document->display_document
    EXPORTING
      reuse_control      = abap_true
      parent             = gr_header
    EXCEPTIONS
      html_display_error = 1
      OTHERS             = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_H
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_get_h.

  DATA: c_area1 TYPE REF TO cl_dd_area,
        c_area2 TYPE REF TO cl_dd_area.

*AREA
  CALL METHOD gr_top_document->vertical_split
    EXPORTING
      split_area               = gr_top_document
      split_width              = '50%'
    IMPORTING
      right_area               = c_area1
    EXCEPTIONS
      invalid_split_area       = 1
      split_area_eq_right_area = 2
      right_area_already_used  = 3
      OTHERS                   = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD gr_top_document->vertical_split
    EXPORTING
      split_area               = gr_top_document
      split_width              = '0%'
    IMPORTING
      right_area               = c_area2
    EXCEPTIONS
      invalid_split_area       = 1
      split_area_eq_right_area = 2
      right_area_already_used  = 3
      OTHERS                   = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*Razon social
  CALL METHOD c_area2->add_text
    EXPORTING
      text         = gv_r_social
      sap_fontsize = cl_dd_document=>medium.

*Nombre del reporte
  CALL METHOD c_area2->add_text
    EXPORTING
      text         = gv_nombre_re
      sap_fontsize = cl_dd_document=>medium.

*Nombre del proveedor
  LOOP AT gt_cabecera INTO gs_cabecera.

    gv_nombre_pro = gs_cabecera-nombre_pro.
    gv_rfc_pro    = gs_cabecera-rfc_pro.

    CALL METHOD c_area1->new_line.

    IF gv_nombre_pro IS NOT INITIAL.

      CALL METHOD c_area2->new_line.

      CALL METHOD c_area2->add_text
        EXPORTING
          text         = gv_nombre_pro
          sap_fontsize = cl_dd_document=>medium.

    ENDIF.

*RFC del proveedor
    IF gv_rfc_pro IS NOT INITIAL.

      CALL METHOD c_area1->new_line.
      CALL METHOD c_area1->add_text
        EXPORTING
          text         = gv_rfc_pro
          sap_fontsize = cl_dd_document=>medium.

    ENDIF.

    CLEAR: gs_cabecera,
    gv_nombre_pro,
    gv_rfc_pro.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_BUILD_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_build_layout.

  gs_layout-zebra   = abap_true.
  gs_layout-col_opt = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CREATE_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_create_alv.

  CREATE OBJECT gr_alv_grid
    EXPORTING
      i_parent          = gr_custom_container "Contenedor Padre
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_HANDLERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_set_handlers.

  CREATE OBJECT gr_eveny_receiver.

  SET HANDLER:
  gr_eveny_receiver->mi_handle_double_click FOR gr_alv_grid.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SHOW_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_show_alv .

  SORT: gt_outtab BY doc_pago_sap.

  DELETE ADJACENT DUPLICATES FROM gt_outtab.

  DELETE gt_outtab WHERE doc_sap_factura IS INITIAL.

  CALL METHOD gr_alv_grid->set_table_for_first_display
    EXPORTING
      is_layout                     = gs_layout
    CHANGING
      it_outtab                     = gt_outtab
      it_fieldcatalog               = gt_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL SCREEN 2000.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_IMP_CH
*&---------------------------------------------------------------------*
*& Con la tabla completa leemos bseg para sacar el importe del cheque
*&---------------------------------------------------------------------*
FORM f_get_imp_ch .

  TYPES: BEGIN OF ty_bseg,
           bukrs TYPE bukrs,
           belnr TYPE belnr_d,
           gjahr TYPE gjahr,
           buzei TYPE buzei,
           qsskz TYPE qsskz,
           dmbrt TYPE dmbrt,
           webtr TYPE webtr,
           ktosl TYPE ktosl,
         END OF ty_bseg.

  DATA: lt_bseg_ch    TYPE STANDARD TABLE OF ty_bseg,
        ls_bseg_ch    TYPE ty_bseg,
        ls_outtab     TYPE zsfi_012_iva_pagado,
        ls_outtab_aux TYPE zsfi_012_iva_pagado.

  DATA: lv_aland LIKE rf82t-land1,
        lv_datab LIKE rf82t-datab,
        lv_mwskz LIKE rf82t-mwskz,
        lv_txjcd LIKE rf82t-txjcd,
        lv_unid  TYPE kbetr,
        lt_iva   TYPE STANDARD TABLE OF ftaxp,
        ls_iva   TYPE ftaxp.

  SELECT bukrs
  belnr
  gjahr
  buzei
  qsskz
  dmbtr
  wrbtr
  ktosl
  FROM bseg
  INTO TABLE lt_bseg_ch
  FOR ALL ENTRIES IN gt_outtab
  WHERE bukrs EQ p_bukrs
  AND belnr EQ gt_outtab-doc_pago_sap
  AND gjahr EQ p_gjahr
  AND qsskz IN ( 'IV', 'IS', 'NO' ).
*    AND KTOSL EQ 'WIT'.

  LOOP AT lt_bseg_ch INTO ls_bseg_ch.

    CASE ls_bseg_ch-qsskz.
      WHEN 'IV'.
        ls_outtab-retencion_iv = ls_bseg_ch-webtr.
      WHEN 'IS'.
        ls_outtab-retencion_is = ls_bseg_ch-webtr.
      WHEN 'NO'.
        ls_outtab-retencion_no = ls_bseg_ch-webtr.
    ENDCASE.

    AT END OF belnr.

      READ TABLE gt_outtab INTO ls_outtab_aux WITH KEY doc_pago_sap = ls_bseg_ch-belnr.

      IF ls_outtab_aux IS NOT INITIAL.

        lv_aland = 'MX'.
        lv_datab = space.
        lv_mwskz = ls_outtab_aux-id_iva.
        lv_txjcd = space.

        CALL FUNCTION 'GET_TAX_PERCENTAGE'
          EXPORTING
            aland   = lv_aland
            datab   = lv_datab
            mwskz   = lv_mwskz
            txjcd   = lv_txjcd
          TABLES
            t_ftaxp = lt_iva.

        READ TABLE lt_iva INTO ls_iva INDEX 1.
        ls_iva-kbetr = ls_iva-kbetr / 1000.
        lv_unid = ls_iva-kbetr + 1.


        IF ls_outtab_aux-moneda_pago EQ 'USD'.

          ls_outtab-retencion_iv = ( ls_outtab-retencion_iv * ls_outtab_aux-tc_pago ).
          ls_outtab-retencion_is = ( ls_outtab-retencion_is * ls_outtab_aux-tc_pago ).
          ls_outtab-retencion_no = ( ls_outtab-retencion_no * ls_outtab_aux-tc_pago ).

        ENDIF.



        ls_outtab-total_mn = ls_outtab_aux-total_mn + ls_outtab-retencion_iv + ls_outtab-retencion_is + ls_outtab-retencion_no.
        ls_outtab-importe_mn = ls_outtab-total_mn / lv_unid.
        ls_outtab-iva_mn     = ls_outtab-importe_mn * ls_iva-kbetr.

        IF ( ls_outtab-importe_mn > ls_outtab-total_mxn_hist ) AND
         ( ls_outtab-importge_dlls IS INITIAL AND ls_outtab-moneda_pago EQ 'MXN' ).
          ls_outtab-iva_mn = ls_outtab-iva_mxn_tc.
          ls_outtab-importe_mn = ls_outtab-importe_mxn_tc.
          ls_outtab-total_mn = ( ls_outtab-iva_mn + ls_outtab-importe_mn ) .
        ENDIF.

        CLEAR ls_outtab_aux.
      ENDIF.
      "importe_mn iva_mn total_mn

      MODIFY gt_outtab FROM ls_outtab TRANSPORTING retencion_iv retencion_is retencion_no  WHERE doc_pago_sap EQ ls_bseg_ch-belnr .
      CLEAR ls_outtab.

    ENDAT.

  ENDLOOP.

ENDFORM.

FORM f_tvarv.

  CLEAR r_blart[].

  CONSTANTS:lco_type_s TYPE c VALUE 'S'.

  DATA:lv_program TYPE sycprog    VALUE 'ZFI012',
       lv_field   TYPE rvari_vnam VALUE 'BLART_PAGO'.

*Obtiene Tipo Documento
  CALL FUNCTION 'ZMF_READ_TVARV'
    EXPORTING
      pi_type       = lco_type_s
      pi_programid  = lv_program
      pi_fieldname  = lv_field
    TABLES
      i_seloptions  = r_blart
    EXCEPTIONS
      ee_not_found  = 1
      ee_wrong_type = 2
      OTHERS        = 3.

  IF sy-subrc NE 0.
    REFRESH r_blart[].
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_MONTOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_montos .

  LOOP AT gt_outtab ASSIGNING FIELD-SYMBOL(<fs_out>).

    IF <fs_out>-total_mxn_hist EQ <fs_out>-total_mn.
      <fs_out>-IMPORTE_MN = <fs_out>-importe_mxn_tc.
      <fs_out>-iva_mn = <fs_out>-iva_mxn_tc.
    ELSEIF <fs_out>-total_mxn_hist > <fs_out>-total_mn.
      CHECK <fs_out>-total_mxn_hist NE 0.
      <fs_out>-iva_mn = ( <fs_out>-total_mn * <fs_out>-iva_mxn_tc ) / <fs_out>-total_mxn_hist.
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM f_read_text USING p_bukrs p_belnr p_gjahr p_obj p_id CHANGING c_uuid.

   DATA: lv_tdid   LIKE          thead-tdid,
         lv_spras  LIKE          thead-tdspras VALUE 'S',
         lv_tdname LIKE          thead-tdname,
         lv_obj    LIKE          thead-tdobject,
         tl_lines  TYPE TABLE OF tline.

   CLEAR c_uuid.

   lv_obj = p_obj.
   lv_tdid = p_id.

   CONCATENATE p_bukrs p_belnr p_gjahr INTO lv_tdname.

   CLEAR c_uuid.

   CALL FUNCTION 'READ_TEXT'
     EXPORTING
       id                      = lv_tdid
       language                = lv_spras
       name                    = lv_tdname
       object                  = lv_obj
     TABLES
       lines                   = tl_lines
     EXCEPTIONS
       id                      = 1
       language                = 2
       name                    = 3
       not_found               = 4
       object                  = 5
       reference_check         = 6
       wrong_access_to_archive = 7
       OTHERS                  = 8.

   IF sy-subrc EQ 0.

     READ TABLE tl_lines ASSIGNING FIELD-SYMBOL(<fs_line>) INDEX 1.

     CHECK sy-subrc EQ 0.

     c_uuid = <fs_line>-tdline.

   ENDIF.

 ENDFORM.
