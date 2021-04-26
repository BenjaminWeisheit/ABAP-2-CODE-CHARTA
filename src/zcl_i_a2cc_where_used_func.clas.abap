CLASS zcl_i_a2cc_where_used_func DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_i_A2CC_where_used_abstract.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING objecttype TYPE zif_i_A2CC_where_used_provider=>objecttype.
    METHODS zif_i_A2CC_where_used~get_cross_references REDEFINITION.
ENDCLASS.

CLASS zcl_i_a2cc_where_used_func IMPLEMENTATION.
  METHOD zif_i_A2CC_where_used~get_cross_references.
    result = get_where_used_list_from_cross( conv #( object-subobject ) ) .
  ENDMETHOD.
  METHOD constructor.
   SUper->constructor( objecttype ).
  ENDMETHOD.
ENDCLASS.
