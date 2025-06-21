CLASS ltcl_assert_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    " Test methods for assert_equals
    METHODS test_assert_equals_success FOR TESTING.
    METHODS test_assert_equals_fail FOR TESTING.

    " Test methods for assert_bound
    METHODS test_assert_bound_success FOR TESTING.
    METHODS test_assert_bound_fail FOR TESTING.

    " Test methods for assert_not_bound
    METHODS test_assert_not_bound_success FOR TESTING.
    METHODS test_assert_not_bound_fail FOR TESTING.

    " Test methods for assert_initial
    METHODS test_assert_initial_success FOR TESTING.
    METHODS test_assert_initial_fail FOR TESTING.

    " Test methods for assert_not_initial
    METHODS test_assert_not_initial_succes FOR TESTING.
    METHODS test_assert_not_initial_fail FOR TESTING.

    " Test methods for assert_subrc
    METHODS test_assert_subrc_success FOR TESTING.
    METHODS test_assert_subrc_fail FOR TESTING.

    " Test methods for assert_true
    METHODS test_assert_true_success FOR TESTING.
    METHODS test_assert_true_fail FOR TESTING.

    " Test methods for assert_false
    METHODS test_assert_false_success FOR TESTING.
    METHODS test_assert_false_fail FOR TESTING.

    " Test methods for assert_differs
    METHODS test_assert_differs_success FOR TESTING.
    METHODS test_assert_differs_fail FOR TESTING.

    " Test method for fail
    METHODS test_fail FOR TESTING.

    " Helper method for testing exceptions
    METHODS expect_exception
      IMPORTING
        iv_block TYPE any.
ENDCLASS.

CLASS ltcl_assert_test IMPLEMENTATION.

  METHOD test_assert_equals_success.
    " Should not raise exception when values are equal
    TRY.
        zcx_ctx_assert=>assert_equals( iv_act = 'Test' iv_exp = 'Test' ).
        " Also test with numbers
        zcx_ctx_assert=>assert_equals( iv_act = 42 iv_exp = 42 ).
        " Test passes if no exception is raised
      CATCH zcx_ctx_assert.
        cl_abap_unit_assert=>fail( 'assert_equals raised exception for equal values' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_equals_fail.
    " Should raise exception when values are different
    TRY.
        zcx_ctx_assert=>assert_equals( iv_act = 'Test' iv_exp = 'Different' ).
        cl_abap_unit_assert=>fail( 'assert_equals did not raise exception for different values' ).
      CATCH zcx_ctx_assert.
        " Expected exception - test passes
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_bound_success.
    " Should not raise exception when object is bound
    DATA lo_object TYPE REF TO zcx_ctx_assert.
    CREATE OBJECT lo_object.

    TRY.
        zcx_ctx_assert=>assert_bound( iv_act = lo_object ).
        " Test passes if no exception is raised
      CATCH zcx_ctx_assert.
        cl_abap_unit_assert=>fail( 'assert_bound raised exception for bound object' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_bound_fail.
    " Should raise exception when object is not bound
    DATA lo_object TYPE REF TO zcx_ctx_assert.

    TRY.
        zcx_ctx_assert=>assert_bound( iv_act = lo_object ).
        cl_abap_unit_assert=>fail( 'assert_bound did not raise exception for unbound object' ).
      CATCH zcx_ctx_assert.
        " Expected exception - test passes
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_not_bound_success.
    " Should not raise exception when object is not bound
    DATA lo_object TYPE REF TO zcx_ctx_assert.

    TRY.
        zcx_ctx_assert=>assert_not_bound( iv_act = lo_object ).
        " Test passes if no exception is raised
      CATCH zcx_ctx_assert.
        cl_abap_unit_assert=>fail( 'assert_not_bound raised exception for unbound object' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_not_bound_fail.
    " Should raise exception when object is bound
    DATA lo_object TYPE REF TO zcx_ctx_assert.
    CREATE OBJECT lo_object.

    TRY.
        zcx_ctx_assert=>assert_not_bound( iv_act = lo_object ).
        cl_abap_unit_assert=>fail( 'assert_not_bound did not raise exception for bound object' ).
      CATCH zcx_ctx_assert.
        " Expected exception - test passes
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_initial_success.
    " Should not raise exception when value is initial
    DATA lv_value TYPE string.

    TRY.
        zcx_ctx_assert=>assert_initial( iv_act = lv_value ).
        " Test passes if no exception is raised
      CATCH zcx_ctx_assert.
        cl_abap_unit_assert=>fail( 'assert_initial raised exception for initial value' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_initial_fail.
    " Should raise exception when value is not initial
    DATA lv_value TYPE string VALUE 'not initial'.

    TRY.
        zcx_ctx_assert=>assert_initial( iv_act = lv_value ).
        cl_abap_unit_assert=>fail( 'assert_initial did not raise exception for non-initial value' ).
      CATCH zcx_ctx_assert.
        " Expected exception - test passes
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_not_initial_succes.
    " Should not raise exception when value is not initial
    DATA lv_value TYPE string VALUE 'not initial'.

    TRY.
        zcx_ctx_assert=>assert_not_initial( iv_act = lv_value ).
        " Test passes if no exception is raised
      CATCH zcx_ctx_assert.
        cl_abap_unit_assert=>fail( 'assert_not_initial raised exception for non-initial value' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_not_initial_fail.
    " Should raise exception when value is initial
    DATA lv_value TYPE string.

    TRY.
        zcx_ctx_assert=>assert_not_initial( iv_act = lv_value ).
        cl_abap_unit_assert=>fail( 'assert_not_initial did not raise exception for initial value' ).
      CATCH zcx_ctx_assert.
        " Expected exception - test passes
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_subrc_success.
    " Should not raise exception when subrc is 0
    DATA lv_subrc TYPE i VALUE 0.

    TRY.
        zcx_ctx_assert=>assert_subrc( iv_act = lv_subrc ).
        " Test passes if no exception is raised
      CATCH zcx_ctx_assert.
        cl_abap_unit_assert=>fail( 'assert_subrc raised exception for subrc = 0' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_subrc_fail.
    " Should raise exception when subrc is not 0
    DATA lv_subrc TYPE i VALUE 4.

    TRY.
        zcx_ctx_assert=>assert_subrc( iv_act = lv_subrc ).
        cl_abap_unit_assert=>fail( 'assert_subrc did not raise exception for subrc <> 0' ).
      CATCH zcx_ctx_assert.
        " Expected exception - test passes
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_true_success.
    " Should not raise exception when value is true
    DATA lv_true TYPE abap_bool VALUE abap_true.

    TRY.
        zcx_ctx_assert=>assert_true( iv_act = lv_true ).
        " Test passes if no exception is raised
      CATCH zcx_ctx_assert.
        cl_abap_unit_assert=>fail( 'assert_true raised exception for true value' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_true_fail.
    " Should raise exception when value is false
    DATA lv_false TYPE abap_bool VALUE abap_false.

    TRY.
        zcx_ctx_assert=>assert_true( iv_act = lv_false ).
        cl_abap_unit_assert=>fail( 'assert_true did not raise exception for false value' ).
      CATCH zcx_ctx_assert.
        " Expected exception - test passes
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_false_success.
    " Should not raise exception when value is false
    DATA lv_false TYPE abap_bool VALUE abap_false.

    TRY.
        zcx_ctx_assert=>assert_false( iv_act = lv_false ).
        " Test passes if no exception is raised
      CATCH zcx_ctx_assert.
        cl_abap_unit_assert=>fail( 'assert_false raised exception for false value' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_false_fail.
    " Should raise exception when value is true
    DATA lv_true TYPE abap_bool VALUE abap_true.

    TRY.
        zcx_ctx_assert=>assert_false( iv_act = lv_true ).
        cl_abap_unit_assert=>fail( 'assert_false did not raise exception for true value' ).
      CATCH zcx_ctx_assert.
        " Expected exception - test passes
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_differs_success.
    " Should not raise exception when values are different
    TRY.
        zcx_ctx_assert=>assert_differs( iv_act = 'Value1' iv_exp = 'Value2' ).
        " Test passes if no exception is raised
      CATCH zcx_ctx_assert.
        cl_abap_unit_assert=>fail( 'assert_differs raised exception for different values' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_assert_differs_fail.
    " Should raise exception when values are the same
    TRY.
        zcx_ctx_assert=>assert_differs( iv_act = 'SameValue' iv_exp = 'SameValue' ).
        cl_abap_unit_assert=>fail( 'assert_differs did not raise exception for identical values' ).
      CATCH zcx_ctx_assert.
        " Expected exception - test passes
    ENDTRY.
  ENDMETHOD.

  METHOD test_fail.
    " Should always raise exception
    TRY.
        zcx_ctx_assert=>fail( iv_message = 'Forced failure' ).
        cl_abap_unit_assert=>fail( 'fail method did not raise exception' ).
      CATCH zcx_ctx_assert.
        " Expected exception - test passes
    ENDTRY.
  ENDMETHOD.

  METHOD expect_exception.
    " Helper method to expect exceptions (not used in this implementation)
  ENDMETHOD.

ENDCLASS.
