INTERFACE zif_ctx_assert_context
  PUBLIC .


  METHODS get_context_uuid
    RETURNING VALUE(rv_uuid) TYPE uuid.

  METHODS get_context_as_table
    EXPORTING et_context_table TYPE ANY TABLE.

ENDINTERFACE.
