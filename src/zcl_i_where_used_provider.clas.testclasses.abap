*"* use this source file for your ABAP unit test classes
class ltcl_test_provider definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      fail_with_xxxx for testing raising cx_static_check,
      succeed_with_meth FOR TESTING RAISING cx_static_check.
endclass.


class ltcl_test_provider implementation.
  method succeed_with_METH.
       try.
       "given instance and
       "when called with  subtype Meth
        NEW zcl_i_where_used_provider(  )->get( 'METH' ).
        "then expext implementation
      CATCH zcx_i_metrics_implement_error.
        cl_abap_unit_assert=>fail( 'Expect implementation' ).

    ENDTRY.

  endmethod.

  METHOD fail_with_xxxx.
    TRY.
       "given instance and
       "when called with nonsense subtype
        NEW zcl_i_where_used_provider(  )->get( 'XXXX' ).
        "then expext exception
        cl_abap_unit_assert=>fail( 'Expect exception zcx_i_metric_implement_error' ).
      CATCH zcx_i_metrics_implement_error.
    ENDTRY.
  ENDMETHOD.

endclass.
