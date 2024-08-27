*&---------------------------------------------------------------------*
*& Report ZCREATE_ASSET                                                *
*&---------------------------------------------------------------------*
*& Date:   01.08.2024                                                  *
*& Author: Hannes Maisch (HANNESM)                                     *
*& Company: ponturo consulting AG                                      *
*& Requested from:                                                     *
*& Description: Copy asset master data per asset class from one company*
*&              code to the other company code with the possibility to *
*&              change the cost center assignment.                     *
*&---------------------------------------------------------------------*
*& Change History                                                      *
*& Date        | Author   | CR &  Description                          *
*&---------------------------------------------------------------------*
REPORT zcreate_asset.
DATA ls_key       TYPE bapi1022_key.
DATA ls_reference TYPE bapi1022_reference.
DATA ls_alv_key   TYPE salv_s_layout_key.
DATA gs_anl1      TYPE anla-anln1.
DATA ls_time      TYPE bapi1022_feglg003.
DATA ls_time_x    TYPE bapi1022_feglg003x.
DATA lt_return    TYPE STANDARD TABLE OF bapiret2.
DATA gt_return    TYPE STANDARD TABLE OF bapiret2.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  " --- Source
  PARAMETERS: pv_bukrs TYPE bukrs DEFAULT 'PON1' MODIF ID z01,
              p_ankl   TYPE anla-anlkl.
  SELECT-OPTIONS so_aln1 FOR gs_anl1.
  " --- Destination
  SELECTION-SCREEN SKIP 1.
  PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY,
              p_kostl TYPE csks-kostl.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS p_test TYPE bapi1022_misc-testrun DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b01.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'Z01'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

INITIALIZATION.

START-OF-SELECTION.
  SELECT * FROM anla
    WHERE bukrs  = @pv_bukrs
      AND anln1 IN @so_aln1
      AND anlkl  = @p_ankl
    INTO TABLE @DATA(lt_anla).
  IF sy-subrc <> 0.
    MESSAGE 'Keine Daten zur Selektion gefunden!' TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  LOOP AT lt_anla ASSIGNING FIELD-SYMBOL(<ls_anla>).
    CLEAR: ls_key,
           ls_reference,
           ls_time,
           ls_time_x,
           lt_return.

    ls_key-CompanyCode = p_bukrs.
    ls_key-asset       = <ls_anla>-anln1.
    ls_key-subnumber   = <ls_anla>-anln2.

    ls_reference-CompanyCode = <ls_anla>-bukrs.
    ls_reference-asset       = <ls_anla>-anln1.
    ls_reference-subnumber   = <ls_anla>-anln2.

    ls_time-costcenter   = p_kostl.
    ls_time_x-costcenter = abap_true.

    CALL FUNCTION 'BAPI_FIXEDASSET_CREATE1'
      EXPORTING key                = ls_key
                reference          = ls_reference
                testrun            = p_test
                timedependentdata  = ls_time
                timedependentdatax = ls_time_x
      IMPORTING return             = lt_return.

    APPEND LINES OF lt_return[] TO gt_return[].
  ENDLOOP.

  CHECK gt_return[] IS NOT INITIAL.

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lr_out)
                              CHANGING  t_table      = gt_return[]      ).
    CATCH cx_salv_msg.
  ENDTRY.

  DATA(lr_display_settings) = lr_out->get_display_settings( ).
  lr_display_settings->set_list_header( sy-title ).
  " --- Funktionen (Toolbar)
  lr_out->get_functions( )->set_all( abap_true ).

  " --- Optimale Spalatenbreite
  DATA(lr_columns) = lr_out->get_columns( ).

  lr_columns->set_optimize( abap_true ).

  DATA(lr_disp) = lr_out->get_display_settings( ).
  lr_disp->set_striped_pattern( cl_salv_display_settings=>true ).

  " --- Layout (Layout-Ääderungen speicherbar)
  DATA(lr_layout)   = lr_out->get_layout( ).
  ls_alv_key-report = sy-repid.
  lr_layout->set_key( ls_alv_key ).
  lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).

**  lr_layout->set_initial_layout( value = iv_vari ).

  lr_out->display( ).
