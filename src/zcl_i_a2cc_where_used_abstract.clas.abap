CLASS zcl_i_a2cc_where_used_abstract DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    INTERFACES zif_i_a2cc_where_used.
    METHODS constructor
      IMPORTING objecttype TYPE zif_i_a2cc_where_used_provider=>objecttype.
  PROTECTED SECTION.
    DATA objecttype TYPE zif_i_a2cc_where_used_provider=>objecttype.
    METHODS get_where_used_list_from_cross
      IMPORTING
        object        TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zif_i_a2cc_where_used~object_list.
  PRIVATE SECTION.
    TYPES: BEGIN OF internal_object,
             type TYPE euobj-type,
             id   TYPE euobj-id,
           END OF internal_object.

    CLASS-DATA internal_objects TYPE SORTED TABLE OF internal_object WITH NON-UNIQUE KEY type.

    METHODS get_enclosing_object
      IMPORTING
        found_object     TYPE REF TO rsfindlst
      EXPORTING
        enclosing_object TYPE rsobject
        enclosing_type   TYPE trobjtype .
    METHODS get_parent_object_type
      IMPORTING
        found_object  TYPE REF TO rsfindlst
      RETURNING
        VALUE(result) TYPE seu_obj.
ENDCLASS.



CLASS zcl_i_a2cc_where_used_abstract IMPLEMENTATION.
  METHOD class_constructor.
    SELECT * FROM euobj INTO CORRESPONDING FIELDS OF TABLE internal_objects WHERE internal = abap_false.
  ENDMETHOD.

  METHOD constructor.
    me->objecttype = objecttype.
  ENDMETHOD.


  METHOD get_enclosing_object.
    DATA group  TYPE rs38l_area.
    DATA include  TYPE progname.

    include = found_object->object.
    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      IMPORTING
        group   = group
      CHANGING
        include = include
      EXCEPTIONS
        OTHERS  = 6.
    IF sy-subrc = 0 AND group IS NOT INITIAL.
      enclosing_object = |SAPL{ group }|.
      enclosing_type = 'FUGR'.
    ELSE.
      enclosing_type = 'PROG'.
      SELECT SINGLE master FROM d010inc INTO enclosing_object WHERE include = include.
      IF sy-subrc <> 0.
        enclosing_object = include.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_where_used_list_from_cross.
    DATA: findstrings    TYPE TABLE OF rsfind,
          found_objects  TYPE STANDARD TABLE OF rsfindlst,
          scope          TYPE STANDARD TABLE OF seu_obj,
          find_obj_class TYPE euobj-id.
    DATA enclosing_type TYPE trobjtype.
    DATA enclosing_object TYPE rsobject.

    findstrings = VALUE #( ( CONV #( object ) )  ).
    find_obj_class = me->objecttype.

    CALL FUNCTION 'RS_EU_CROSSREF'
      EXPORTING
        i_find_obj_cls           = find_obj_class
        no_dialog                = abap_true
      TABLES
        i_findstrings            = findstrings
        o_founds                 = found_objects
        i_scope_object_cls       = scope
      EXCEPTIONS
        not_executed             = 1
        not_found                = 2
        illegal_object           = 3
        no_cross_for_this_object = 4
        batch                    = 5
        batchjob_error           = 6
        wrong_type               = 7
        object_not_exist         = 8
        OTHERS                   = 9.
    IF sy-subrc IS INITIAL.
      result = NEW #( ).
      LOOP AT found_objects REFERENCE INTO DATA(found_object).

        IF found_object->encl_objec IS NOT INITIAL.
          enclosing_object = found_object->encl_objec.
          enclosing_type = get_parent_object_type( found_object ).
        ELSE.
          get_enclosing_object( EXPORTING found_object =  found_object
                                IMPORTING enclosing_object = enclosing_object
                                          enclosing_type   = enclosing_type ).
        ENDIF.

        IF line_exists( result->*[ depending_object = enclosing_object depending_type = enclosing_type(4) ] ).
          ASSIGN result->*[ depending_object = enclosing_object ] TO FIELD-SYMBOL(<reference>).
          <reference>-number_of_usages = <reference>-number_of_usages + 1.
        ELSE.
          INSERT VALUE #( depending_object = enclosing_object depending_type = enclosing_type(4) number_of_usages = 1 ) INTO TABLE result->*.
        ENDIF.      .
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_parent_object_type.
    result  = VALUE seu_obj( internal_objects[ type = found_object->object_cls ]-id OPTIONAL ).

    result = COND #( WHEN result(4) = 'PROG' AND found_object->*(4) = 'SAPL' THEN 'FUGR'
                     WHEN result = 'METH' THEN 'CLAS'
                     ELSE result(4) ).
  ENDMETHOD.

  METHOD zif_i_a2cc_where_used~get_cross_references.

  ENDMETHOD.
ENDCLASS.
