*&---------------------------------------------------------------------*
*& Report Y_SSK_TEST2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report y_ssk_002.

class lcl_app definition final.
  public section.
    methods display_metadata.
endclass.

class lcl_app implementation.
  method display_metadata.

    data lo_metadata      type ref to /iwcor/if_rest_entity_provider.
    data lo_ep_binary     type ref to /iwcor/cl_ds_ep_binary.
    data lv_svc_namespace type /iwfnd/med_mdl_namespace value '/IWBEP/'.
    data lv_svc_name      type /iwfnd/med_mdl_srg_name  value 'GWSAMPLE_BASIC'.
    data lv_svc_version   type /iwfnd/med_mdl_version   value '0001'.

    try.
        data(lv_svc_id) = /iwfnd/cl_med_exploration=>get_service_id_by_parts(
          exporting
            iv_namespace     = lv_svc_namespace                 " Namespace
            iv_external_name = conv #( lv_svc_name )            " External Service Group Name
            iv_version       = lv_svc_version ).                " version of meta model entity

        data(lo_transaction_handler) = /iwfnd/cl_transaction_handler=>get_transaction_handler( ).
        lo_transaction_handler->set_service_identifier( lv_svc_id ).
        lo_transaction_handler->set_service_name( conv #( lv_svc_name ) ).
        lo_transaction_handler->set_service_namespace( lv_svc_namespace ).
        lo_transaction_handler->set_service_version( lv_svc_version ).

        data(lo_context) = new /iwcor/cl_ds_cntxt( ).

        if lo_context is bound.
          try.
              lo_context->/iwcor/if_ds_cntxt~set_object(
                iv_name   = /iwcor/cl_ds_cntxt=>gc_object_service
                io_object = /iwfnd/cl_sodata_svc_factory=>get_svc_factory( )->create_service( conv #( lv_svc_name ) ) ).


*              data(lo_svc_grp) = lo_transaction_handler->get_service_group_metadata( ).
*
*              if lo_svc_grp is bound.
*                data(lo_odata_proc) = /iwfnd/cl_sodata_processor=>get_processor( lo_svc_grp ).
*
*                if lo_odata_proc is bound.
*                  lo_odata_proc->set_context( lo_context ).
*
*                  data(lo_meta) = cast /iwcor/cl_ds_ext_ep_metadata( lo_odata_proc->/iwcor/if_ds_proc_metadata~read( ) ).
*
*                  if lo_meta is bound.
*                    data(lv_bin) = lo_meta->to_xstring( ).
*                  endif.
*                endif.
*              endif.

              data(lo_proc_single) = new /iwcor/cl_ds_proc_single( ).

              if lo_proc_single is bound.
                lo_proc_single->set_context( lo_context ).

                lo_metadata = lo_proc_single->/iwcor/if_ds_proc_metadata~read( ).

                if lo_metadata is bound.
                  try.
                      lo_ep_binary ?= lo_metadata.

                      data(lv_metadata_bin) = lo_ep_binary->get_binary_data( ).
                    catch cx_sy_move_cast_error into data(lox_move_cast_error).
                      message lox_move_cast_error type 'E'.
                  endtry.

                  data(lv_metadata) = cl_abap_codepage=>convert_from( lv_metadata_bin ).

                  data(lo_xml) = new cl_xml_document( ).

                  if lo_xml is bound.
                    lo_xml->parse_xstring( stream = lv_metadata_bin ).

                    lo_xml->display( ).
                  endif.
                endif.
              endif.
            catch /iwcor/cx_ds_error into data(lox_ds_error).
              message lox_ds_error type 'E'.
          endtry.
        endif.

      catch /iwfnd/cx_med_mdl_access into data(lox_med_mdl_access).
        message lox_med_mdl_access type 'E'.
    endtry.
  endmethod.
endclass.

start-of-selection.
  new lcl_app( )->display_metadata( ).
