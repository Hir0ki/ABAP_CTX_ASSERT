CLASS zcx_assert_context_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL .


CONSTANTS: gcv_message_class type symsgid value 'ZCX_CTX_ASSERT'.

    CONSTANTS: BEGIN OF context_not_bound,
                  textid TYPE symsgid value gcv_message_class,
                  msgno  TYPE symsgno value '101',
                  attr1  TYPE scx_attrname value '',
                  attr2  TYPE scx_attrname value '',
                  attr3  TYPE scx_attrname value '',
                  attr4  TYPE scx_attrname value '',
                END OF context_not_bound.
     CONSTANTS: BEGIN OF context_uuid_not_set,
                  textid TYPE symsgid value gcv_message_class,
                  msgno  TYPE symsgno value '102',
                  attr1  TYPE scx_attrname value '',
                  attr2  TYPE scx_attrname value '',
                  attr3  TYPE scx_attrname value '',
                  attr4  TYPE scx_attrname value '',
                END OF context_uuid_not_set.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_assert_context_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
    previous = previous
    ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
