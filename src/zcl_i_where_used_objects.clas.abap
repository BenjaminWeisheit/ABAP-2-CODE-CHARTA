CLASS zcl_i_where_used_objects DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
   INTERFACES zif_i_where_used.
  PROTECTED SECTION.
  PRIVATE SECTION.
     METHODS get_where_used_list_from_Cross
      IMPORTING
        method        TYPE string
        class         TYPE string
      RETURNING
        VALUE(result) TYPE ref to zif_i_where_used~object_list.

    METHODS get_classname_from_include
      IMPORTING
        include       TYPE  programm
      RETURNING
        VALUE(result) TYPE classname.
ENDCLASS.



CLASS zcl_i_where_used_objects IMPLEMENTATION.
  METHOD zif_i_where_used~get_cross_references.
      result = get_where_used_list_from_Cross( class =  object  method = subobject  ) .
  ENDMETHOD.


  METHOD get_where_used_list_from_Cross.
    DATA name TYPE eu_lname.
    name = class && '\ME:' && method.

    SELECT DISTINCT name, include FROM wbcrossgt
      INTO TABLE @DATA(cross_references)
       WHERE otype = 'ME'
         AND name  = @name.
    IF sy-subrc IS INITIAL.
      result = NEW #( ).

      LOOP AT cross_references REFERENCE INTO DATA(cross_reference).
        DATA(usage) = get_classname_from_include( cross_reference->include ).
        CHECK usage <> class.
        INSERT  conv #( usage )  INTO TABLE result->*.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


   METHOD get_classname_from_include.
    result = cl_oo_classname_service=>get_clsname_by_include( include ).
  ENDMETHOD.

ENDCLASS.
