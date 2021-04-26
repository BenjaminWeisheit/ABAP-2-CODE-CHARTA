CLASS zcl_i_a2cc_where_used_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_i_A2CC_where_used_provider.
    METHODS constructor.
  PRIVATE SECTION.

    TYPES: BEGIN OF object_where_used_provider,
             objecttype TYPE zif_i_A2CC_where_used_provider=>objecttype,
             subtype    TYPE zif_i_A2CC_where_used_provider=>subtype,
             instance   TYPE REF TO zif_i_A2CC_where_used,
           END OF object_where_used_provider.
    TYPES object_whereused_prov_config TYPE HASHED TABLE OF object_where_used_provider WITH UNIQUE KEY objecttype subtype.
    DATA object_provider_config TYPE object_whereused_prov_config.

    METHODS get_instance_by_type IMPORTING
                                   objecttype    TYPE zif_i_A2CC_where_used_provider=>objecttype
                                   subtype       TYPE zif_i_A2CC_where_used_provider=>subtype
                                 RETURNING
                                   VALUE(result) TYPE REF TO zif_i_A2CC_where_used
                                 RAISING
                                   zcx_i_a2cc_metrics_impl_error.

    METHODS set_configuration IMPORTING object_provider_config TYPE object_whereused_prov_config.
ENDCLASS.

CLASS zcl_i_a2cc_where_used_provider IMPLEMENTATION.
  METHOD constructor.
    set_configuration( VALUE #( ( objecttype = 'CLAS' subtype = 'METH' instance = NEW zcl_i_a2cc_where_used_clas( 'CLAS' ) )
                                ( objecttype = 'FUGR' subtype = 'FUNC' instance =  NEW zcl_i_a2cc_where_used_func( 'FUNC' ) ) ) ).
  ENDMETHOD.

  METHOD zif_i_A2CC_where_used_provider~get.
    result = get_instance_by_type( objecttype = objecttype subtype = subtype ).
  ENDMETHOD.

  METHOD get_instance_by_type.
    TRY.
        result = object_provider_config[ objecttype = objecttype subtype = subtype ]-instance.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_i_A2CC_metrics_impl_error.
    ENDTRY.
  ENDMETHOD.

  METHOD set_configuration.
    me->object_provider_config = object_provider_config.
  ENDMETHOD.
ENDCLASS.
