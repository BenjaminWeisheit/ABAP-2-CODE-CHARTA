CLASS zcl_i_a2cc_package_analyzer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES zif_i_a2cc_package_analyzer.
  PRIVATE SECTION.
    METHODS get_parent_packages
      IMPORTING
        package       TYPE zsi_a2cc_code_metrics-package
        path          TYPE string
      RETURNING
        VALUE(result) TYPE string.
    DATA packages            TYPE ztti_a2cc_packages.

ENDCLASS.
CLASS zcl_i_a2cc_package_analyzer IMPLEMENTATION.
  METHOD zif_i_a2cc_package_analyzer~aggregate_packages.
    DATA devclasses TYPE RANGE OF devclass.

    devclasses = VALUE #( FOR package IN aggregated_metrics ( sign = 'I' option = 'EQ' low = package-package ) ).
    SORT devclasses.
    DELETE ADJACENT DUPLICATES FROM devclasses.

    SELECT devclass, parentcl FROM tdevc INTO TABLE @DATA(package_hierarchy) WHERE devclass IN @devclasses AND parentcl IN @devclasses.

    LOOP AT aggregated_metrics REFERENCE INTO DATA(metric).
      AT NEW package.
        TRY.
            APPEND VALUE #( package  = metric->package
                            parent   = package_hierarchy[ devclass = metric->package ]-parentcl ) TO packages.
          CATCH cx_sy_itab_line_not_found.
            APPEND VALUE #( package  = metric->package ) TO packages.
        ENDTRY.
      ENDAT.
    ENDLOOP.
    result = packages.
  ENDMETHOD.

  METHOD zif_i_a2cc_package_analyzer~get_parent_packages.
    result = get_parent_packages( package = package
                                  path    = CONV #( package ) ).
  ENDMETHOD.

  METHOD get_parent_packages.
    TRY.
        DATA(parent_package) = packages[ package = package ]-parent.
        IF parent_package IS INITIAL.
          result = path.
        ELSE.
          result = get_parent_packages( package = parent_package
                                        path    = |{ parent_package }/{ path }| ).
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
        result = path.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
