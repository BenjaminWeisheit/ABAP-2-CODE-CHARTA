*"* use this source file for your ABAP unit test classes
CLASS ltcl_test_provider DEFINITION DEFERRED.
CLASS zcl_i_a2cc_where_used_provider DEFINITION LOCAL FRIENDS ltcl_test_provider.

CLASS ltcl_test_provider DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zif_i_A2CC_where_used_provider.
    METHODS:
      setup,
      fail_with_xxxx FOR TESTING RAISING cx_static_check,
      succeed_with_meth FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS lth_dummy_where_used DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_i_A2CC_where_used.
ENDCLASS.

CLASS lth_dummy_where_used IMPLEMENTATION.

  METHOD zif_i_A2CC_where_used~get_cross_references.

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_test_provider IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_i_a2cc_where_used_provider( ).
    cast zcl_i_a2cc_where_used_provider( cut )->set_configuration( VALUE #( ( objecttype = 'CLAS' subtype = 'METH' instance = NEW lth_dummy_where_used( ) )   )
                       ).

  ENDMETHOD.

  METHOD succeed_with_meth.
    TRY.
        "given configuration with METH
        "when called with  subtype Meth
        cut->get( objecttype = 'CLAS' subtype = 'METH' ).
        "then expext implementation
      CATCH zcx_i_A2CC_metrics_impl_error.
        cl_abap_unit_assert=>fail( 'Expect implementation' ).

    ENDTRY.

  ENDMETHOD.

  METHOD fail_with_xxxx.
    CONSTANTS any_non_existant_value TYPE zif_i_A2CC_where_used_provider=>subtype VALUE 'XXXX' ##NO_TEXT.
    TRY.
        "given instance and
        "when called with nonsense subtype
         cut->get( objecttype = 'CLAS' subtype = any_non_existant_value ).
        "then expext exception
        cl_abap_unit_assert=>fail( 'Expect exception zcx_i_metric_implement_error' ).
      CATCH zcx_i_A2CC_metrics_impl_error.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
