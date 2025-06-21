CLASS zcx_ctx_assert DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM cx_no_check .



  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL.

    CLASS-METHODS assert_equals
      IMPORTING
        iv_act     TYPE any
        iv_exp     TYPE any
        iv_message TYPE string OPTIONAL
        iv_context TYPE REF TO zif_ctx_assert_context OPTIONAL.

    CLASS-METHODS assert_bound
      IMPORTING
        iv_act     TYPE any
        iv_message TYPE string OPTIONAL
        iv_context TYPE REF TO zif_ctx_assert_context OPTIONAL.

    CLASS-METHODS assert_not_bound
      IMPORTING
        iv_act     TYPE any
        iv_message TYPE string OPTIONAL
        iv_context TYPE REF TO zif_ctx_assert_context OPTIONAL.

    CLASS-METHODS assert_initial
      IMPORTING
        iv_act     TYPE any
        iv_message TYPE string OPTIONAL
        iv_context TYPE REF TO zif_ctx_assert_context OPTIONAL.

    CLASS-METHODS assert_not_initial
      IMPORTING
        iv_act     TYPE any
        iv_message TYPE string OPTIONAL
        iv_context TYPE REF TO zif_ctx_assert_context OPTIONAL.

    CLASS-METHODS assert_subrc
      IMPORTING
        iv_act     TYPE int4
        iv_message TYPE string OPTIONAL
        iv_context TYPE REF TO zif_ctx_assert_context OPTIONAL.

    CLASS-METHODS assert_true
      IMPORTING
        iv_act     TYPE any
        iv_message TYPE string OPTIONAL
        iv_context TYPE REF TO zif_ctx_assert_context OPTIONAL.

    CLASS-METHODS assert_false
      IMPORTING
        iv_act     TYPE any
        iv_message TYPE string OPTIONAL
        iv_context TYPE REF TO zif_ctx_assert_context OPTIONAL.

    CLASS-METHODS assert_differs
      IMPORTING
        iv_act     TYPE any
        iv_exp     TYPE any
        iv_message TYPE string OPTIONAL
        iv_context TYPE REF TO zif_ctx_assert_context OPTIONAL.

    CLASS-METHODS fail
      IMPORTING
        iv_message TYPE string OPTIONAL
        iv_context TYPE REF TO zif_ctx_assert_context OPTIONAL.


  PRIVATE SECTION.
    CLASS-METHODS handle_messages
      IMPORTING
        iv_message        TYPE string
        iv_exception_uuid TYPE zctx_assert_uuid_char
        iv_passed_context TYPE REF TO zif_ctx_assert_context OPTIONAL.

    CLASS-METHODS are_asserts_on
      RETURNING VALUE(rv_asserts_on) TYPE abap_bool.

    CLASS-METHODS genereate_exception_uuid
      RETURNING
        VALUE(rv_uuid) TYPE zctx_assert_uuid_char.
ENDCLASS.



CLASS zcx_ctx_assert IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
  ENDMETHOD.

  METHOD handle_messages.

    TRY.
        DATA(lo_log_handler) = cl_bali_log=>create(  ).


        DATA(lv_header) = cl_bali_header_setter=>create( object = 'ZCXT_ASSERT'
                                       subobject = 'ZASSERT'
                                       external_id = CONV #( iv_exception_uuid ) ).
       lo_log_handler->set_header( lv_header ).

       lo_log_handler->add_item(  CL_BALI_FREE_TEXT_SETTER=>create( text = 'Test' ) ).

        cl_bali_log_db=>get_instance( )->save_log( log = lo_log_handler use_2nd_db_connection = abap_true ).


      CATCH cx_bali_runtime.
        "handle exception
    ENDTRY.
  ENDMETHOD.

  METHOD are_asserts_on.

    rv_asserts_on = abap_true.

  ENDMETHOD.


  METHOD assert_equals.
    DATA(lv_uuid) = genereate_exception_uuid( ).
    IF  iv_act = iv_exp.
      RETURN.
    ENDIF.

    handle_messages( iv_message = iv_message iv_passed_context = iv_context iv_exception_uuid = lv_uuid ).

    IF are_asserts_on(  ).
      RAISE EXCEPTION TYPE zcx_ctx_assert  MESSAGE  e000(zcx_ctx_assert) WITH lv_uuid.
    ENDIF.

  ENDMETHOD.


  METHOD genereate_exception_uuid.

    rv_uuid = xco_cp=>uuid(  )->as( io_format = xco_cp_uuid=>format->c22 )->value  .


  ENDMETHOD.

  METHOD assert_bound.

    DATA(lv_uuid) = genereate_exception_uuid( ).
    IF   iv_act IS BOUND .
      RETURN.
    ENDIF.

    handle_messages( iv_message = iv_message iv_passed_context = iv_context iv_exception_uuid = lv_uuid ).

    IF are_asserts_on(  ).
      RAISE EXCEPTION TYPE zcx_ctx_assert  MESSAGE  e000(zcx_ctx_assert) WITH lv_uuid.
    ENDIF.
  ENDMETHOD.

  METHOD assert_not_bound.
    DATA(lv_uuid) = genereate_exception_uuid( ).
    IF  iv_act IS NOT BOUND.
      RETURN.
    ENDIF.

    handle_messages( iv_message = iv_message iv_passed_context = iv_context iv_exception_uuid = lv_uuid ).

    IF are_asserts_on(  ).
      RAISE EXCEPTION TYPE zcx_ctx_assert  MESSAGE  e000(zcx_ctx_assert) WITH lv_uuid.
    ENDIF.
  ENDMETHOD.

  METHOD assert_initial.
    DATA(lv_uuid) = genereate_exception_uuid( ).
    IF iv_act IS INITIAL.
      RETURN.
    ENDIF.

    handle_messages( iv_message = iv_message iv_passed_context = iv_context iv_exception_uuid = lv_uuid ).

    IF are_asserts_on(  ).
      RAISE EXCEPTION TYPE zcx_ctx_assert  MESSAGE  e000(zcx_ctx_assert) WITH lv_uuid.
    ENDIF.
  ENDMETHOD.

  METHOD assert_not_initial.
    DATA(lv_uuid) = genereate_exception_uuid( ).
    IF iv_act IS NOT INITIAL.
      RETURN.
    ENDIF.

    handle_messages( iv_message = iv_message iv_passed_context = iv_context iv_exception_uuid = lv_uuid ).

    IF are_asserts_on(  ).
      RAISE EXCEPTION TYPE zcx_ctx_assert  MESSAGE  e000(zcx_ctx_assert) WITH lv_uuid.
    ENDIF.
  ENDMETHOD.

  METHOD assert_subrc.
    DATA lv_expected TYPE i.
    DATA(lv_uuid) = genereate_exception_uuid( ).
    " Default expected value is 0
    IF iv_act = 0.
      RETURN.
    ENDIF.

    handle_messages( iv_message = iv_message iv_passed_context = iv_context iv_exception_uuid = lv_uuid ).

    IF are_asserts_on(  ).
      RAISE EXCEPTION TYPE zcx_ctx_assert  MESSAGE  e000(zcx_ctx_assert) WITH lv_uuid.
    ENDIF.
  ENDMETHOD.

  METHOD assert_true.
    DATA(lv_uuid) = genereate_exception_uuid( ).
    IF iv_act = abap_true.
      RETURN.
    ENDIF.

    handle_messages( iv_message = iv_message iv_passed_context = iv_context iv_exception_uuid = lv_uuid ).

    IF are_asserts_on(  ).
      RAISE EXCEPTION TYPE zcx_ctx_assert  MESSAGE  e000(zcx_ctx_assert) WITH lv_uuid.
    ENDIF.
  ENDMETHOD.

  METHOD assert_false.
    DATA(lv_uuid) = genereate_exception_uuid( ).
    IF iv_act = abap_false.
      RETURN.
    ENDIF.

    handle_messages( iv_message = iv_message iv_passed_context = iv_context iv_exception_uuid = lv_uuid ).

    IF are_asserts_on(  ).
      RAISE EXCEPTION TYPE zcx_ctx_assert  MESSAGE  e000(zcx_ctx_assert) WITH lv_uuid.
    ENDIF.
  ENDMETHOD.

  METHOD assert_differs.
    DATA(lv_uuid) = genereate_exception_uuid( ).
    IF iv_act <> iv_exp.
      RETURN.
    ENDIF.

    handle_messages( iv_message = iv_message iv_passed_context = iv_context iv_exception_uuid = lv_uuid ).

    IF are_asserts_on(  ).
      RAISE EXCEPTION TYPE zcx_ctx_assert  MESSAGE  e000(zcx_ctx_assert) WITH lv_uuid.
    ENDIF.
  ENDMETHOD.

  METHOD fail.
    DATA(lv_uuid) = genereate_exception_uuid( ).
    handle_messages( iv_message = iv_message iv_passed_context = iv_context iv_exception_uuid = lv_uuid ).

    IF are_asserts_on(  ).
      RAISE EXCEPTION TYPE zcx_ctx_assert  MESSAGE  e000(zcx_ctx_assert) WITH lv_uuid.
    ENDIF.
  ENDMETHOD.



ENDCLASS.

