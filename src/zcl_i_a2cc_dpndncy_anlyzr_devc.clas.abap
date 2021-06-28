CLASS zcl_i_a2cc_dpndncy_anlyzr_devc DEFINITION
  PUBLIC INHERITING FROM zcl_i_a2cc_dpndncy_anlyzr_clas
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS collect_object_where_used_list REDEFINITION.
    METHODS where_used_analysis REDEFINITION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF object_data,
        object_id   TYPE zei_a2cc_object_id,
        object_type TYPE trobjtype,
        package     TYPE char30,
      END OF object_data .
    CONSTANTS package_obj TYPE trobjtype VALUE 'DEVC'.

    DATA:
      packages TYPE HASHED TABLE OF object_data WITH UNIQUE KEY object_id object_type.

    METHODS aggregate_to_package_level
      IMPORTING
        !depending_objects TYPE zif_i_a2cc_where_used=>object_list
      RETURNING
        VALUE(result)      TYPE zif_i_a2cc_where_used=>object_list .
ENDCLASS.



CLASS zcl_i_a2cc_dpndncy_anlyzr_devc IMPLEMENTATION.

  METHOD aggregate_to_package_level.
    LOOP AT depending_objects REFERENCE INTO DATA(depending_object).
      DATA(package) = COND string( WHEN line_exists( packages[ object_id = depending_object->depending_object object_type = depending_object->depending_type ] )
                                   THEN packages[ object_id = depending_object->depending_object object_type = depending_object->depending_type ]-package
                                   ELSE |{ depending_object->depending_object }.{ depending_object->depending_type }| ).

      INSERT VALUE #( depending_object = package depending_type = package_obj ) INTO TABLE result.
    ENDLOOP.
    SORT result.
    DELETE ADJACENT DUPLICATES FROM result COMPARING depending_object depending_type.
  ENDMETHOD.


  METHOD collect_object_where_used_list.
    TRY.
        DATA(references) = REF #( me->object_where_used_list_by[ object_identifier = metric->package object_type = package_obj ]-references ).
        LOOP AT depending_objects REFERENCE INTO DATA(depending_object).
          IF line_exists( references->*[ depending_object = depending_object->depending_object depending_type = depending_object->depending_type ] ).
            ASSIGN references->*[ depending_object = depending_object->depending_object depending_type = depending_object->depending_type ] TO FIELD-SYMBOL(<existing_reference>).
            <existing_reference>-number_of_usages = <existing_reference>-number_of_usages + 1.
          ELSE.
            INSERT depending_object->* INTO TABLE references->*.
          ENDIF.
        ENDLOOP.
      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( object_identifier = metric->package object_type = package_obj references = depending_objects ) INTO TABLE object_where_used_list_by.
    ENDTRY.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.

  METHOD where_used_analysis.
    LOOP AT metrics REFERENCE INTO DATA(metric).
      INSERT VALUE #( object_id = metric->modu_unit_1 object_type = metric->category package = metric->package ) INTO TABLE packages.
    ENDLOOP.

    LOOP AT metrics REFERENCE INTO metric.
      TRY.
          DATA(depending_objects) = where_used_provider->get( objecttype = metric->category subtype = metric->sub_type )->get_cross_references( VALUE #( object    = metric->modu_unit_1
                                                                                                                                                         subobject = metric->modu_unit_2 ) ).
          IF has_where_used_entries( depending_objects ).
            collect_object_where_used_list( metric = metric depending_objects = aggregate_to_package_level( depending_objects->* ) ).
          ENDIF.
        CATCH zcx_i_a2cc_metrics_impl_error.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
