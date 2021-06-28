CLASS zcl_i_a2cc_where_used_clas DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_i_a2cc_where_used_abstract.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING objecttype TYPE zif_i_a2cc_where_used_provider=>objecttype.
    METHODS zif_i_a2cc_where_used~get_cross_references REDEFINITION.
ENDCLASS.

CLASS zcl_i_a2cc_where_used_clas IMPLEMENTATION.
  METHOD zif_i_a2cc_where_used~get_cross_references.
    result = get_where_used_list_from_cross( CONV #( object-object ) ) .
  ENDMETHOD.

  METHOD constructor.
    super->constructor( objecttype ).
  ENDMETHOD.
ENDCLASS.
