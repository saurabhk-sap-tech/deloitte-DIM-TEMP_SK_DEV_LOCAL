
report zsdb0010_output_conditions
no standard page heading
line-size 132
line-count 65(0)
message-id zz.
tables: t685, "Conditions: Types
        t682i, "Access Sequences (Generated Form)
        t682z, "Access Sequences (Fields)
        dd03l.
*Work area for the inbound file for unix
data: begin of x_inrec,
        line(1024) type c,
      end of x_inrec.
*Work area for the inbound file from PC
data: begin of it_xinrec occurs 0,
        line(1024) type c,
      end of it_xinrec.
* Table for getting the field name and lenght.
data: begin of it_vakey_fields occurs 0,
        fieldname   like t682z-zifna, "Field for condition table
        fieldlen(3) type n, "Field lenght
        offset(3)   type n, "Field Offset
      end of it_vakey_fields.
*processing table for inbound file.
data: begin of x_comm,
        parnr like nach-parnr, "Message partner
      end of x_comm.

* final BDC table.
data: begin of it_data occurs 0,
        vakey type vakey, "Variable key 100 bytes
        parnr like nach-parnr, ""Message partner
      end of it_data.
*populating t682z values.
data: it_t682z like t682z occurs 0 with header line.
*getting value of the radio button
data: v_access_step(2) type n.
* check for the value
data: v_step_found type c.
* Get field length
data: v_vakey_length type i.
*get number of messages.
data: g_lines    type i,
* Message
      g_msg(100).
* File name
data: v_filename type string.
* Get the condition table
data: v_cond_table(4) type c.
* Error for subrc checking
data: v_error like sy-subrc.
*----------------------------------------------------------------------*
* data definition
*----------------------------------------------------------------------*
* Batchinputdata of single transaction
data: bdcdata like bdcdata occurs 0 with header line.
* messages of call transaction
data: messtab like bdcmsgcoll occurs 0 with header line.

* Error table
data: begin of it_error occurs 0,
        vakey        type vakey, "Variable key 100 bytes
        parnr        like nach-parnr, "Message partner
        message(100), "Message
      end of it_error.
constants: c_usage   like t685-kvewe value 'B', "Usage of the
           "condition table
           c_true(1) type c value 'X', "Check for a record
           c_n(1)    type c value 'N', "No screen mode
           c_s(1)    type c value 'S'. "Update mode
************************************************************************
*selection-screen
************************************************************************
selection-screen begin of block a01 with frame title text-001.
  parameters: p_kappl like t685-kappl obligatory, "Application
              p_kschl like nach-kschl obligatory, "Condition type
              p_kotab like t682i-kotabnr obligatory, "Condition table
              p_parvw like nach-parvw. "Partner function
  parameters: pr_pc   radiobutton group g1, "PC upload
              pr_unix radiobutton group g1. "Unix Upload
  parameters: p_infile like rlgrap-filename obligatory. "Input file path
selection-screen end of block a01.
* Note: for the Print out Block Validation Not required.
selection-screen begin of block a02 with frame title text-002.
  parameters: p_comm as checkbox. "Communication
  selection-screen begin of line.
    selection-screen comment 1(31) text-s01 for field p_ldest.
    parameters: p_ldest like nach-ldest. "Output Device
    selection-screen position 75.
    parameters: p_dimme as checkbox. "Print immediate
    selection-screen comment 78(20) text-s08 for field p_dimme.
  selection-screen end of line.
  selection-screen begin of line.
    selection-screen comment 1(31) text-s02 for field p_anzal.
    parameters: p_anzal(2) type n. "Number of
    "Messages
    selection-screen position 75.
    parameters: p_delet as checkbox. "Release after
    "output
    selection-screen comment 78(20) text-s03 for field p_delet.
  selection-screen end of line.
  parameters: p_dsnam like nach-dsnam, "Spool request
              "name
              p_dsuf1 like nach-dsuf1, "Suffix1
              p_dsuf2 like nach-dsuf2, "Suffix1
              p_tdoco like nach-tdocover, "SAP Cover page
              p_tdrec like nach-tdreceiver, "Recipient
              p_tddiv like nach-tddivision, "Department on
              "cover page
              p_tdcov like nach-tdcovtitle, "Cover Page Text
              p_tdaut like nach-tdautority, "Authorization
              p_tdarm like nach-tdarmod. "Storage Mode
selection-screen end of block a02.
************************************************************************
*at selection-screen on field.
***********************************************************************

at selection-screen on value-request for p_infile.
*--- Value Request Functionality for PC File
  perform value_request_for_pc_file.

at selection-screen.
* check for the records in the database
  perform read_access_tables.
* Screen validations
  perform addl_screen_validations.
***********************************************************************
* START-OF-SELECTION.
***********************************************************************
start-of-selection.
* Get the field name and field value.
  perform determine_variable_key.
* Get the fill from PC or Unix
  perform read_input_file.
* Upload records to the database
  perform upload_conditions.
************************************************************************
* End of selection.
************************************************************************
end-of-selection.
* Error check
  if v_error <> 0.
    stop.
  endif.
* Get out put of the report

  perform write_reoprt.
************************************************************************
* Top-of-page.
************************************************************************
top-of-page.
* Standard header.
*  call function 'Z_STEELCASE_HEADER'
* EXPORTING
* HEADING_1 =
* HEADING_2 =
  .
*&---------------------------------------------------------------------*
*& Form READ_ACCESS_TABLES
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
form read_access_tables .
* Get Data from t685
  select single * from t685
  where kvewe = c_usage "B
  and kappl = p_kappl "Application
  and kschl = p_kschl. "Condition type
  if sy-subrc <> 0.
    message e002 with 'Incorrect application or output type'(004).
  endif.
  clear v_access_step.
  clear v_step_found.
* Get value of kolnr

  select * from t682i
  where kvewe = c_usage "Usage of the condition table
  and kappl = p_kappl "Application
  and kozgf = t685-kozgf. "Access sequence
    add 1 to v_access_step.
* IF t682i-kolnr = p_kolnr.
    if t682i-kotabnr = p_kotab.
      v_step_found = c_true.
      exit.
    endif.
  endselect.
  if sy-subrc <> 0 or
  v_step_found is initial.
    message e002 with 'Incorrect condition table'(005).
  endif.
  v_cond_table+0(1) = 'B'.
  v_cond_table+1(3) = t682i-kotabnr.
*
  select * from t682z
  into table it_t682z
  where kvewe = c_usage "Usage of the condition table
  and kappl = p_kappl "Application
  and kozgf = t685-kozgf "Access sequence
* AND kolnr = p_kolnr.
  and kolnr = t682i-kolnr. "Access number
  if sy-subrc <> 0.
    message e002 with 'Unable to find condition table'(006).
  endif.
endform. " READ_ACCESS_TABLES

*&---------------------------------------------------------------------*
*& Form DETERMINE_VARIABLE_KEY
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
form determine_variable_key .
  data: offset(3) type n.
* Get the name of the field and the Length
  clear offset.
  loop at it_t682z into t682z.
    select single * from dd03l
    where tabname = t682z-qustr " Table name
    and fieldname = t682z-qufna. " Table field
    if sy-subrc <> 0.
      exit.
    endif.
* Checking the lenght of the field and name
    v_vakey_length = v_vakey_length + dd03l-leng. "Internal Length
    it_vakey_fields-fieldname = t682z-zifna. " Table name
    it_vakey_fields-fieldlen = dd03l-leng. " Table field
    it_vakey_fields-offset = offset. " Off set
    append it_vakey_fields.
    offset = offset + dd03l-leng.
  endloop.
endform. " DETERMINE_VARIABLE_KEY
*&---------------------------------------------------------------------*
*& Form READ_INPUT_FILE
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*

form read_input_file .
* From unix
  if pr_unix = 'X'.
    perform open_dataset.
  else.
* From PC
    perform gui_upload.
  endif.
endform. " READ_INPUT_FILE
************************************************************************
* Subroutines
************************************************************************
*----------------------------------------------------------------------*
* Start new screen *
*----------------------------------------------------------------------*
form bdc_dynpro using program dynpro.
  clear bdcdata.
  bdcdata-program = program. "Program name
  bdcdata-dynpro = dynpro. "Screen number
  bdcdata-dynbegin = 'X'. "New Screen
  append bdcdata.
endform. "BDC_DYNPRO
*----------------------------------------------------------------------*
* Insert field *
*----------------------------------------------------------------------*
form bdc_field using fnam fval.
  clear bdcdata.
  bdcdata-fnam = fnam. "Field name

  bdcdata-fval = fval. "Field Value
  append bdcdata.
endform. "BDC_FIELD
*&--------------------------------------------------------------------*
*& Form BDC_TRANSACTION
*&--------------------------------------------------------------------*
* text
*---------------------------------------------------------------------*
* -->TCODE text
*---------------------------------------------------------------------*
form bdc_transaction using tcode.
  data: l_mstring(480).
  refresh messtab.

  data(ls_options) = value ctu_params( dismode = 'A' updmode = c_s racommit = abap_true ).

  call transaction tcode using bdcdata options from ls_options
*  mode 'A'" c_n "No screen mode
*  update c_s "Synchronous update
  messages into messtab.
  loop at messtab.
* If it says the condition record doesn't exist, that is ok.
* It is going to create a new record.
    if messtab-msgid = 'VK' and
    messtab-msgnr = '021'.
      continue.
    endif.
* Populate error records
    clear g_lines.

    describe table messtab lines g_lines.
    read table messtab index g_lines transporting all fields.
* Get the error message
    perform format_message.
* Get record.
    if not g_msg is initial.
      it_error-message = g_msg.
      it_error-vakey = it_data-vakey.
      it_error-parnr = it_data-parnr.
      append : it_error.
      clear : it_error.
    endif.
* ENDIF.
  endloop.
  clear bdcdata.
  refresh bdcdata.
endform. "BDC_TRANSACTION
*&---------------------------------------------------------------------*
*& Form upload_conditions
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
form upload_conditions .
  loop at it_data.
* Checking if the Vakey is empty
    check not it_data-vakey is initial.
* Process BDC

    perform process_bdc.
  endloop.
endform. " upload_conditions
*&---------------------------------------------------------------------*
*& Form process_bdc
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
form process_bdc .
  data: bdc_field_name(20).
  data: vakey_fieldname(20).
  data: vakey_fieldval(100).
  data: field_count(3) type n.
  constants: single_quote type c value ''''.
  data: cond_rec_exists.
  data: where_clause type string.
  data: kappl_temp like nach-kappl. "Application
  data: text1 type vakey. "variable key
  data: text2 type string. "variable key

  select
    from t681e
    fields *
    where kvewe   = @c_usage
      and kotabnr = @p_kotab
    into table @data(it_t681e).

* Get whear clause
  clear where_clause.
  concatenate 'KAPPL = P_KAPPL AND'
  'KSCHL = P_KSCHL'
  into where_clause
  separated by space.
  loop at it_vakey_fields.
*Get whear clause
    concatenate where_clause 'AND'
    into where_clause
    separated by space.
* Getting the table and field name.
    text1+0(it_vakey_fields-fieldlen) =
    it_data-vakey+it_vakey_fields-offset(it_vakey_fields-fieldlen).
* Getting the table and field name.
    concatenate single_quote
    text1+0(it_vakey_fields-fieldlen)
    single_quote
    into text2.
* Get where clause
    concatenate where_clause
    it_vakey_fields-fieldname
    '='
    text2
    into where_clause
    separated by space.
  endloop.
* Dynamic select with dynamic where clause
  clear cond_rec_exists.
  select single kappl
  into kappl_temp
  from (v_cond_table)
  where (where_clause).
  if sy-subrc = 0.
    cond_rec_exists = 'X'.
  endif.
* Passing value to the BDC table
  perform bdc_dynpro using 'SAPMV13B' '0100'.

  perform bdc_field using: 'BDC_OKCODE' '=ANTA',
  'RV130-KAPPL' p_kappl,
  'RV13B-KSCHL' p_kschl.
  perform bdc_dynpro using 'SAPLV14A' '0100'.
  concatenate 'RV130-SELKZ(' v_access_step ')'
  into bdc_field_name.
  perform bdc_field using: 'BDC_CURSOR' bdc_field_name,
  bdc_field_name c_true,
  'BDC_OKCODE' '=WEIT'.
  concatenate 'RV13B' t682i-kotabnr
  into bdc_field_name.
* passing value to the new screen
  perform bdc_dynpro using bdc_field_name '1000'.
* Passing filed name field value
  perform bdc_field using: 'BDC_OKCODE' '=HIZ1'. " '=ONLI'.
  clear field_count.
  loop at it_vakey_fields.
    add 1 to field_count.

    if line_exists( it_t681e[ sefeld = it_vakey_fields-fieldname fsetyp = 'B' ] ).
      concatenate 'F' field_count '-LOW'
      into vakey_fieldname.
    else.
      concatenate 'F' field_count
    into vakey_fieldname.
    endif.
    vakey_fieldval =
    it_data-vakey+it_vakey_fields-offset(it_vakey_fields-fieldlen).
    perform bdc_field using: vakey_fieldname vakey_fieldval.

  endloop.
  concatenate '1' t682i-kotabnr
  into bdc_field_name.
* PERFORM bdc_dynpro USING 'SAPMV13B' bdc_field_name.
** Go to a new page
* PERFORM bdc_field USING: 'BDC_OKCODE' '=NEWP'.
* In the new page we will always populate in the second line
  perform bdc_dynpro using 'SAPMV13B' bdc_field_name.
  perform bdc_field using: 'BDC_OKCODE' '/00'.
  if not p_parvw is initial.
    perform bdc_field using: 'NACH-PARVW(01)' p_parvw.
  endif.
  if not it_data-parnr is initial.
    perform bdc_field using: 'RV13B-PARNR(01)' it_data-parnr.
  endif.
  perform bdc_field using: 'NACH-SPRAS(01)' 'EN'.
  if cond_rec_exists is initial.
    loop at it_vakey_fields.
      if line_exists( it_t681e[ sefeld = it_vakey_fields-fieldname fsetyp = 'B' ] ).
        concatenate 'KOMB-' it_vakey_fields-fieldname '(01)'
        into vakey_fieldname.
      else.
        concatenate 'KOMB-' it_vakey_fields-fieldname
      into vakey_fieldname.
      endif.
      vakey_fieldval =
      it_data-vakey+it_vakey_fields-offset(it_vakey_fields-fieldlen).
      perform bdc_field using: vakey_fieldname vakey_fieldval.
    endloop.
  endif.
* Communication required is checked.
  if p_comm = 'X'.

    perform bdc_dynpro using 'SAPMV13B' bdc_field_name.
    perform bdc_field using: 'BDC_OKCODE' '=KOMM',
    'RV130-SELKZ(01)' 'X'.
    perform bdc_dynpro using 'SAPMV13B' '0211'.
    perform bdc_field using: 'BDC_OKCODE' '=SICH',
    'NACH-LDEST' p_ldest,
    'NACH-DIMME' p_dimme,
    'NACH-ANZAL' p_anzal,
    'NACH-DELET' p_delet,
    'NACH-DSNAM' p_dsnam,
    'NACH-DSUF1' p_dsuf1,
    'NACH-DSUF2' p_dsuf2,
    'NACH-TDOCOVER' p_tdoco,
    'NACH-TDRECEIVER' p_tdrec,
    'NACH-TDDIVISION' p_tddiv,
    'NACH-TDCOVTITLE' p_tdcov,
    'NACH-TDAUTORITY' p_tdaut.
    if not p_tdarm is initial.
      perform bdc_field using: 'NACH-TDARMOD' p_tdarm.
    endif.
  else.
    perform bdc_field using: 'BDC_OKCODE' '=SICH'.
  endif.
* Call tranastction
  perform bdc_transaction using 'NACR'.
endform. " process_bdc
*&---------------------------------------------------------------------*

*& Form format_message
*&---------------------------------------------------------------------*
* Get the error message
*----------------------------------------------------------------------*
form format_message.
* Calling format_message to format the message
  clear g_msg.
  call function 'FORMAT_MESSAGE'
    exporting
      id        = messtab-msgid
      lang      = sy-langu
      no        = messtab-msgnr
      v1        = messtab-msgv1
      v2        = messtab-msgv2
      v3        = messtab-msgv3
      v4        = messtab-msgv4
    importing
      msg       = g_msg
    exceptions
      not_found = 1
      others    = 2.
  if sy-subrc <> 0.
    g_msg = space.
  endif.
endform. " format_message
*&---------------------------------------------------------------------*
*& Form value_request_for_pc_file
*&---------------------------------------------------------------------*
* Description : Value Request Functionality for PC File
*----------------------------------------------------------------------*

form value_request_for_pc_file.
  call function 'F4_FILENAME'
    exporting
      program_name  = sy-cprog
      dynpro_number = sy-dynnr
      field_name    = ' '
    importing
      file_name     = p_infile.
endform. " value_request_for_pc_file
*&---------------------------------------------------------------------*
*& Form gui_upload
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
form gui_upload .
  v_filename = p_infile.
  call function 'GUI_UPLOAD'
    exporting
      filename                = v_filename
    tables
      data_tab                = it_xinrec
    exceptions
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      others                  = 17.
  if sy-subrc <> 0.
    v_error = 4.
    message i002 with 'Cannot open input file'(007).
    stop.
  else.
    loop at it_xinrec into x_inrec.
      if sy-subrc <> 0.
        exit.
      endif.
      it_data-vakey = x_inrec+0(v_vakey_length).
      x_comm = x_inrec+v_vakey_length.
      it_data-parnr = x_comm-parnr.
      append it_data.
    endloop.
  endif.
endform. " gui_upload
*&---------------------------------------------------------------------*
*& Form open_dataset
*&---------------------------------------------------------------------*

* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
form open_dataset .
  open dataset p_infile for input in text mode encoding default.
  if sy-subrc <> 0.
    v_error = 4.
    message i002 with 'Cannot open input file'(003).
    stop.
  endif.
  do.
    read dataset p_infile into x_inrec.
    if sy-subrc <> 0.
      exit.
    endif.
    it_data-vakey = x_inrec+0(v_vakey_length).
    x_comm = x_inrec+v_vakey_length.
    it_data-parnr = x_comm-parnr.
    append it_data.
  enddo.
  close dataset p_infile.
endform. " open_dataset
*&---------------------------------------------------------------------*
*& Form write_reoprt
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
form write_reoprt .
  if not it_error[] is initial.
    write:/ sy-uline.
    write:/ sy-vline.
    write: 02 'Details of Data'(008),
    22 sy-vline,
    23 'Message partner'(009),
    38 sy-vline,
    39 'Partner function'(010),
    56 sy-vline,
    57 'Message'(015) ,
    132 sy-vline.
    write:/ sy-uline.
    loop at it_error.
      write:/ sy-vline.
      write: 02 it_error-vakey,
      22 sy-vline,
      39 it_error-parnr,
      56 sy-vline,
      57 it_error-message ,
      132 sy-vline.
    endloop.
    write:/ sy-uline.
  else.
    write:/ 'All condition records successfully updated'(011).

  endif.
endform. " write_reoprt
*&---------------------------------------------------------------------*
*& Form addl_screen_validations
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
form addl_screen_validations .
  data: parvw like tpar-parvw.
  if not p_parvw is initial.
    select single parvw into parvw
    from tpar
    where parvw = p_parvw.
    if sy-subrc <> 0.
      message e002 with 'Invalid partner function'(012).
    endif.
  endif.
  if p_tdoco is initial or
  p_tdoco = 'X' or
  p_tdoco = 'D'.
  else.
    message e002 with 'Invalid coverpage flag'(013).
  endif.
  if not p_tdarm is initial and
  p_tdarm > '3'.
    message e002 with 'Invalid archiving mode'(014).
  endif.

endform. " addl_screen_validations
