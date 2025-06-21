INTERFACE zif_assert_context_register
  PUBLIC .
  METHODS unregister_context
    IMPORTING if_context        TYPE REF TO zif_ctx_assert_context
    RETURNING VALUE(rf_context) TYPE REF TO zif_ctx_assert_context
    RAISING   zcx_assert_context_error.

  METHODS register_context
    IMPORTING if_context        TYPE REF TO zif_ctx_assert_context
    RETURNING VALUE(rf_context) TYPE REF TO zif_ctx_assert_context
    RAISING   zcx_assert_context_error.

  METHODS get_all_registerd_contexts
    RETURNING VALUE(rt_contexts) TYPE zctx_assert_regestry.

ENDINTERFACE.
