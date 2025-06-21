CLASS zcl_assert_context_registry DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_assert_context_register.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gt_regestry TYPE zctx_assert_regestry.


ENDCLASS.



CLASS zcl_assert_context_registry IMPLEMENTATION.


  METHOD zif_assert_context_register~register_context.

    IF if_context IS NOT BOUND.

      RAISE EXCEPTION TYPE zcx_assert_context_error
        EXPORTING
          textid = zcx_assert_context_error=>context_not_bound.
    ENDIF.

    DATA(lv_context_uuid) = if_context->get_context_uuid( ).

    IF lv_context_uuid IS INITIAL.
      RAISE EXCEPTION TYPE zcx_assert_context_error
        EXPORTING
          textid = zcx_assert_context_error=>context_uuid_not_set.
    ENDIF.

    INSERT VALUE #( uuid = lv_context_uuid context = if_context ) INTO TABLE gt_regestry.
    IF sy-subrc <> 0.
      "TODO I need to thinks if i need error handling here
    ENDIF.

    rf_context = if_context.


  ENDMETHOD.


  METHOD zif_assert_context_register~unregister_context.

    IF if_context IS NOT BOUND.

      RAISE EXCEPTION TYPE zcx_assert_context_error
        EXPORTING
          textid = zcx_assert_context_error=>context_not_bound.
    ENDIF.

    DATA(lv_context_uuid) = if_context->get_context_uuid( ).

    IF lv_context_uuid IS INITIAL.
      RAISE EXCEPTION TYPE zcx_assert_context_error
        EXPORTING
          textid = zcx_assert_context_error=>context_uuid_not_set.
    ENDIF.

    DELETE  gt_regestry WHERE uuid = lv_context_uuid.
    IF sy-subrc <> 0.
      "TODO I need to thinks if i need error handling here
    ENDIF.

    rf_context = if_context.



  ENDMETHOD.

  METHOD zif_assert_context_register~get_all_registerd_contexts.

    rt_contexts = gt_regestry.

  ENDMETHOD.

ENDCLASS.
