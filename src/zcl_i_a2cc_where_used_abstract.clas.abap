CLASS zcl_i_a2cc_where_used_abstract DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
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

    METHODS get_enclosing_object
      IMPORTING
        found_object  TYPE REF TO rsfindlst
      RETURNING
        VALUE(result) TYPE rsobject.
ENDCLASS.



CLASS ZCL_I_A2CC_WHERE_USED_ABSTRACT IMPLEMENTATION.


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
      result = |SAPL{ group }|.
    ELSE.
      SELECT SINGLE master FROM d010inc INTO result WHERE include = include.
      IF sy-subrc <> 0.
        result = include.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_where_used_list_from_cross.
    DATA: findstrings    TYPE TABLE OF rsfind,
          found_objects  TYPE STANDARD TABLE OF rsfindlst,
          scope          TYPE STANDARD TABLE OF seu_obj,
          find_obj_class TYPE euobj-id.

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
        DATA(where_used_reference) = COND rsobject( WHEN found_object->encl_objec IS NOT INITIAL THEN found_object->encl_objec
                                                    ELSE get_enclosing_object( found_object ) ).
        IF line_exists( result->*[ depending_object = where_used_reference ] ).
          ASSIGN result->*[ depending_object = where_used_reference ] TO FIELD-SYMBOL(<reference>).
          <reference>-number_of_usages = <reference>-number_of_usages + 1.
        ELSE.
          INSERT VALUE #( depending_object = where_used_reference number_of_usages = 1 ) INTO TABLE result->*.
        ENDIF.      .
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD zif_i_a2cc_where_used~get_cross_references.

  ENDMETHOD.
ENDCLASS.
