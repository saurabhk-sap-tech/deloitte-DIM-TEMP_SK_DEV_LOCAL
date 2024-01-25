CLASS zcl_sk_test_http_service DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_service_extension.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_sk_test_http_service IMPLEMENTATION.
  METHOD if_http_service_extension~handle_request.
    TRY.
        DATA(lv_json) = '{ "ThisIs": "A Placeholder" }'.
        response->set_header_field( i_name  = if_web_http_header=>content_type
                                    i_value = if_web_http_header=>accept_application_json
                  )->set_text( CONV #( lv_json ) ).

      CATCH cx_web_message_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
