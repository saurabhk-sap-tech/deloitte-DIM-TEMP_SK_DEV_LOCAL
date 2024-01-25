CLASS zcl_test_http_service DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_service_extension.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_test_http_service IMPLEMENTATION.
  METHOD if_http_service_extension~handle_request.
    TRY.
        response->set_text( 'HelloWorld!' ).
        response->set_status( 200 ).
      CATCH cx_web_message_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
