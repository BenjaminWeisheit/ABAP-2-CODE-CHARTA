CLASS zcl_i_where_used_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
   types subtype type c length 4.
    methods  get importing subtype type subtype RETURNING VALUE(result) type ref to zif_i_where_used
    RAISING zcx_i_metrics_implement_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_i_where_used_provider IMPLEMENTATION.

  METHOD get.
    result = SWITCH #( subtype WHEN 'METH' THEN NEW zcl_i_where_used_class( )
                               ELSE THROW zcx_i_metrics_implement_error( ) ).
  ENDMETHOD.

ENDCLASS.
