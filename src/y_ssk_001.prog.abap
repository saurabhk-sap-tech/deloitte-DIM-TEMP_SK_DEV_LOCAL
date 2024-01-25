*&---------------------------------------------------------------------*
*& Report Y_SSK_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_ssk_001.

CLASS lcl_app DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      select_file
        RETURNING
          VALUE(rv_file) TYPE if_mass_spreadsheet_types=>file_name.

    METHODS:
      run.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS:
      mc_insp_lot TYPE string VALUE 'PRUEFLOS',
      mc_insp_op  TYPE string VALUE 'VORNR',
      mc_comma    TYPE string VALUE ','.

    DATA:
      mt_data        TYPE if_mass_spreadsheet_types=>t_spreadsheet,
      mt_desc_header LIKE mt_data,
      mt_tech_header LIKE mt_data,

      mv_rows        TYPE i,
      mv_cols        TYPE i,

      BEGIN OF ms_log,
        status      TYPE icon_d,
        insp_lot    TYPE qplos,
        insp_op     TYPE vornr,
        plant       TYPE werks_d,
        material    TYPE matnr,
        batch       TYPE charg_d,
        insp_plan   TYPE plnnr,
        grp_counter TYPE plnal,
        message     TYPE bapi_msg,
      END OF ms_log,
      mt_log LIKE STANDARD TABLE OF ms_log WITH DEFAULT KEY.

    METHODS:
      read_file
        IMPORTING
          iv_file TYPE if_mass_spreadsheet_types=>file_name,

      extract_headers,

*      validate_inspection_lots,
      update_inspections_chars,

      read_cell_value
        IMPORTING
          is_sheet_cell   TYPE if_mass_spreadsheet_types=>s_spreadsheet_cell
        RETURNING
          VALUE(rv_value) TYPE string,

      get_col_num
        IMPORTING
          VALUE(iv_tech_name) TYPE string
        RETURNING
          VALUE(rv_col_num)   TYPE i,

      is_numeric
        IMPORTING
          VALUE(iv_value)   TYPE any
        RETURNING
          VALUE(rv_numeric) TYPE abap_bool,

      display_log.
ENDCLASS.

CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS start.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    p_file TYPE if_mass_spreadsheet_types=>file_name OBLIGATORY.

  SELECTION-SCREEN SKIP.

  PARAMETERS:
    p_test AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_app IMPLEMENTATION.
  METHOD select_file.
    " Local data
    DATA:
      lt_file TYPE filetable,
      lv_rc   TYPE i,
      lv_ua   TYPE i.

    CLEAR rv_file.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        file_filter             = cl_gui_frontend_services=>filetype_excel    " File Extension Filter String
      CHANGING
        file_table              = lt_file                                     " Table Holding Selected Files
        rc                      = lv_rc                                       " Return Code, Number of Files or -1 If Error Occurred
        user_action             = lv_ua                                       " User Action (See Class Constants ACTION_OK, ACTION_CANCEL)
      EXCEPTIONS
        file_open_dialog_failed = 1                                           " "Open File" dialog failed
        cntl_error              = 2                                           " Control error
        error_no_gui            = 3                                           " No GUI available
        not_supported_by_gui    = 4                                           " GUI does not support this
        OTHERS                  = 5 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF lv_rc = 1 AND lv_ua = cl_gui_frontend_services=>action_ok.
      rv_file = VALUE #( lt_file[ 1 ]-filename ).
    ENDIF.
  ENDMETHOD.

  METHOD run.
    read_file( EXPORTING iv_file = p_file ).

*    validate_inspection_lots( ).

    update_inspections_chars( ).
  ENDMETHOD.

  METHOD read_file.
    " Read excel data using SOI - SAP Office Integration
    TRY.
        DATA(lo_imp_excel) = NEW cl_mass_spsh_file_imp_excel( ).

        IF lo_imp_excel IS BOUND.
          lo_imp_excel->set_file( EXPORTING iv_file = iv_file ).

          DATA(lo_excel) =
            CAST cl_mass_spreadsheet(
              lo_imp_excel->if_mass_spreadsheet_import~import(
                EXPORTING
                  io_imp_conf = cl_mass_spsh_imp_conf_excel=>create_default_config( ) ) ).

          IF lo_excel IS BOUND.
            lo_excel->if_mass_spreadsheet~get_data(
              IMPORTING
                et_data = DATA(lt_excel) ).

            IF lt_excel IS NOT INITIAL.
              DATA(ls_spreadsheet) = VALUE #( lt_excel[ 1 ] OPTIONAL ).
              CLEAR mt_data.
              mt_data = ls_spreadsheet-spreadsheet.
              mv_rows = ls_spreadsheet-num_rows.
              mv_cols = ls_spreadsheet-num_cols.
            ENDIF.
          ENDIF.
        ENDIF.
      CATCH cx_mass_spreadsheet INTO DATA(lox_mass_spreadsheet).
        MESSAGE i398(00) WITH lox_mass_spreadsheet->get_longtext( ) DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

*  METHOD validate_inspection_lots.
*  ENDMETHOD.

  METHOD extract_headers.
    CLEAR:
      mt_desc_header,
      mt_tech_header.

    IF mt_data IS NOT INITIAL.
      mt_desc_header = FILTER #( mt_data USING KEY primary_key WHERE row = 1 ).
      mt_tech_header = VALUE #(
                         FOR ls_data_int IN mt_data
                           USING KEY primary_key
                             WHERE ( row = 2 )
                             ( row    = ls_data_int-row
                               column = ls_data_int-column
                               value  = to_upper( ls_data_int-value ) ) ).

      DELETE mt_data USING KEY primary_key WHERE row = 1 OR row = 2.
    ENDIF.
  ENDMETHOD.

  METHOD update_inspections_chars.
    DATA:
      ls_return     TYPE bapiret2,
      lt_return     TYPE STANDARD TABLE OF bapiret2,
      lt_char_req   TYPE STANDARD TABLE OF bapi2045d1,
      lt_char_res   TYPE STANDARD TABLE OF bapi2045d2,
      lt_sample_res TYPE STANDARD TABLE OF bapi2045d3,
      lt_single_res TYPE STANDARD TABLE OF bapi2045d4.

    IF mt_data IS INITIAL.
      RETURN.
    ENDIF.

    DO ( mv_rows - 2 ) TIMES.
      DATA(lv_row) = sy-index + 2.

      APPEND INITIAL LINE TO mt_log ASSIGNING FIELD-SYMBOL(<ls_log>).
      IF <ls_log> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      <ls_log>-insp_lot = read_cell_value( VALUE #( row = lv_row value = mc_insp_lot ) ).
      <ls_log>-insp_op  = read_cell_value( VALUE #( row = lv_row value = mc_insp_op ) ).

      <ls_log>-insp_lot = |{ <ls_log>-insp_lot ALPHA = IN }|.
      <ls_log>-insp_op  = |{ <ls_log>-insp_op ALPHA = IN }|.

      SELECT SINGLE werk,
                    matnr,
                    charg,
                    plnnr,
                    plnal
        FROM qals
        WHERE prueflos = @<ls_log>-insp_lot
        INTO ( @<ls_log>-plant,
               @<ls_log>-material,
               @<ls_log>-batch,
               @<ls_log>-insp_plan,
               @<ls_log>-grp_counter ).

      CALL FUNCTION 'BAPI_INSPOPER_GETDETAIL'
        EXPORTING
          insplot                = <ls_log>-insp_lot         " Inspection Lot Number
          inspoper               = <ls_log>-insp_op          " Inspection Lot Operation Number
          read_char_requirements = abap_true                 " Read Table CHAR_REQUIREMENTS
          read_char_results      = abap_true                 " Read Table CHAR_RESULTS
          read_sample_results    = abap_true                 " Read Table SAMPLE_RESULTS
          read_single_results    = abap_true                 " Read Table SINGLE_RESULTS
        IMPORTING
          return                 = ls_return                 " Return Parameter
        TABLES
          char_requirements      = lt_char_req               " Inspection Specifications - Inspection Lot Characteristics
          char_results           = lt_char_res               " Inspection Results Characteristic Level
          sample_results         = lt_sample_res             " Inspection Results: Sample Level
          single_results         = lt_single_res.            " Inspection Results: Single Value Level

      IF ls_return-type = 'E'.
        <ls_log>-status  = icon_red_light.
        <ls_log>-message = ls_return-message.
        CONTINUE.
      ENDIF.

      IF lt_char_req IS INITIAL.
        CONTINUE.
      ENDIF.
      " Check mandatory chars

      LOOP AT lt_char_req INTO DATA(ls_char_req).
        DATA(lv_char_val) = read_cell_value( VALUE #( row = lv_row value = ls_char_req-mstr_char ) ).

        " Val initial = either column for char is missing or value is not maintained in the column for this row
        IF lv_char_val IS NOT INITIAL.
          CONDENSE lv_char_val NO-GAPS.

          IF lv_char_val CA mc_comma.
            SPLIT lv_char_val AT mc_comma INTO TABLE DATA(lt_val).
          ENDIF.

          " check type of char
          IF ls_char_req-single_res = abap_true.
            " check value
            LOOP AT lt_val INTO DATA(lv_val) WHERE table_line IS NOT INITIAL.
              IF NOT is_numeric( lv_val ).
                <ls_log>-status  = icon_red_light.
                <ls_log>-message = |{ ls_char_req-mstr_char } - { lv_val }| && ` ` && 'is not numeric'(003).
                EXIT.
              ENDIF.
              CLEAR lv_val.
            ENDLOOP.
          ENDIF.

          IF ls_char_req-sample_res = abap_true.
            " check value
            IF lines( lt_val ) > 1.
              " Error - Continue
              <ls_log>-status  = icon_red_light.
              <ls_log>-message = |{ ls_char_req-mstr_char }| && ` ` && 'does not allow mulitple results'(002).
              EXIT.
            ENDIF.
            IF NOT is_numeric( lv_val ).
              <ls_log>-status  = icon_red_light.
              <ls_log>-message = |{ ls_char_req-mstr_char } - { lv_val }| && ` ` && 'is not numeric'(003).
              EXIT.
            ENDIF.
          ENDIF.

          IF ls_char_req-char_res = abap_true.
            " check value
            IF lines( lt_val ) > 1.
              " Error - Continue
              <ls_log>-status  = icon_red_light.
              <ls_log>-message = |{ ls_char_req-mstr_char }| && ` ` && 'does not allow mulitple results'(002).
              EXIT.
            ENDIF.
            IF NOT is_numeric( lv_val ).
              <ls_log>-status  = icon_red_light.
              <ls_log>-message = |{ ls_char_req-mstr_char } - { lv_val }| && ` ` && 'is not numeric'(003).
              EXIT.
            ENDIF.
          ENDIF.

          " Build BAPI tables
        ENDIF.
        CLEAR ls_char_req.
      ENDLOOP.

      IF <ls_log>-status IS INITIAL.
        " Call BAPI
        CLEAR ls_return.
        CALL FUNCTION 'BAPI_INSPOPER_RECORDRESULTS'
          EXPORTING
            insplot        = <ls_log>-insp_lot          " Inspection Lot Number
            inspoper       = <ls_log>-insp_op           " Inspection Operation Number
          IMPORTING
            return         = ls_return                  " Return Value
          TABLES
            char_results   = lt_char_res                " Inspection Result - Characteristic Level
            sample_results = lt_sample_res              " Inspection Result: Sample Level
            single_results = lt_single_res              " Insp. result-single value level
            returntable    = lt_return.                 " Error Messages.

        IF ls_return-type = 'S'.  " To Do - Check specific message id and num
          IF p_test = abap_false.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_true.                 " Use of Command `COMMIT AND WAIT`
          ENDIF.

          <ls_log>-status  = icon_green_light.
          <ls_log>-message = ls_return-message.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD read_cell_value.
    rv_value = VALUE #( mt_data[ KEY primary_key
                                   row    = is_sheet_cell-row
                                   column = get_col_num( is_sheet_cell-value ) ]-value OPTIONAL ).
  ENDMETHOD.

  METHOD get_col_num.
    CLEAR rv_col_num.
    rv_col_num = VALUE #( mt_tech_header[ value = iv_tech_name ]-column OPTIONAL ).
  ENDMETHOD.

  METHOD is_numeric.
    IF iv_value IS NOT INITIAL.
      CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
        EXPORTING
          input      = iv_value
        EXCEPTIONS
          no_numeric = 1
          OTHERS     = 2.
      IF sy-subrc = 0.
        rv_numeric = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD display_log.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD start.
    TRY.
        NEW lcl_app( )->run( ).
      CATCH cx_root INTO DATA(lox_root).
        MESSAGE s398(00) WITH lox_root->get_text( ) DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  p_file = lcl_app=>select_file( ).

START-OF-SELECTION.
  lcl_main=>start( ).
