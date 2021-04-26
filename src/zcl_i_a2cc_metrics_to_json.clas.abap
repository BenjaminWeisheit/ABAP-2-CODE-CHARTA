*
* This is version 0.1.0
*
*The MIT License (MIT)
*
*Copyright (c) 2021 Benjamin Weisheit
*
*Permission is hereby granted, free of charge, to any person obtaining a copy
*of this software and associated documentation files (the "Software"), to deal
*in the Software without restriction, including without limitation the rights
*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*copies of the Software, and to permit persons to whom the Software is
*furnished to do so, subject to the following conditions:
*
*The above copyright notice and this permission notice shall be included in all
*copies or substantial portions of the Software.
*
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.CLASS zcl_i_metrics_to_json DEFINITION
CLASS zcl_i_a2cc_metrics_to_json DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
    METHODS
      to_json
        IMPORTING
          VALUE(it_metrics) TYPE ztti_a2cc_code_metrics
        RETURNING
          VALUE(result)     TYPE string.

  PRIVATE SECTION.
    CLASS-DATA json_writer TYPE REF TO if_sxml_writer.
    METHODS aggregate_metrics
      IMPORTING
        it_metrics TYPE ztti_a2cc_code_metrics.

    METHODS aggregate_packages.
    METHODS write_package
      IMPORTING
        package TYPE zsi_a2cc_packages
      RAISING
        cx_sxml_state_error.
    METHODS convert2json
      RETURNING
        VALUE(result) TYPE string.
    CLASS-METHODS write_element IMPORTING name  TYPE string
                                          attr  TYPE string OPTIONAL
                                          value TYPE string OPTIONAL
                                RAISING   cx_sxml_state_error.
    DATA: aggregated_metrics TYPE ztti_a2cc_code_metrics,
          packages           TYPE ztti_a2cc_packages.
ENDCLASS.

CLASS zcl_i_a2cc_metrics_to_json IMPLEMENTATION.
  METHOD constructor.
    json_writer =
      CAST if_sxml_writer(
             cl_sxml_string_writer=>create(
               type = if_sxml=>co_xt_json  ) ).
  ENDMETHOD.
  METHOD to_json.
    SORT it_metrics.

    aggregate_metrics( it_metrics ).
    aggregate_packages( ).
    result = convert2json( ).
  ENDMETHOD.

  METHOD aggregate_metrics.
    DATA aggregated_metric TYPE zsi_a2cc_code_metrics.

    LOOP AT it_metrics REFERENCE INTO DATA(metric).
      aggregated_metric-component_to_be_changed =  metric->component_to_be_changed.
      aggregated_metric-package     =  metric->package.
      aggregated_metric-category    =  metric->category.
      aggregated_metric-modu_unit_1 =  metric->modu_unit_1.
      aggregated_metric-loc        = aggregated_metric-loc        + metric->loc       .
      aggregated_metric-nos        = aggregated_metric-nos        + metric->nos - 2   . "DO not Consider METHOD/ENDMETHOD
      aggregated_metric-noc        = aggregated_metric-noc        + metric->noc       .
      aggregated_metric-nop        = aggregated_metric-nop        + metric->nop       .
      aggregated_metric-mcc_com    = aggregated_metric-mcc_com    + metric->mcc_com   .
      aggregated_metric-vrsd_chan  = aggregated_metric-vrsd_chan  + metric->vrsd_chan .
      aggregated_metric-dd_total   = aggregated_metric-dd_total   + metric->dd_total  .
      aggregated_metric-stmts_c    = aggregated_metric-stmts_c    + metric->stmts_c   .
      aggregated_metric-stmts_p    = aggregated_metric-stmts_p    + metric->stmts_p   .
      aggregated_metric-stmts_i    = aggregated_metric-stmts_i    + metric->stmts_i   .
      aggregated_metric-stmts_e    = aggregated_metric-stmts_e    + metric->stmts_e   .
      aggregated_metric-stmts_ei   = aggregated_metric-stmts_ei   + metric->stmts_ei  .
      aggregated_metric-stmts_d    = aggregated_metric-stmts_d    + metric->stmts_d   .
      aggregated_metric-stmts_w    = aggregated_metric-stmts_w    + metric->stmts_w   .
      aggregated_metric-stmts_l    = aggregated_metric-stmts_l    + metric->stmts_l   .
      aggregated_metric-stmts_ca   = aggregated_metric-stmts_ca   + metric->stmts_ca  .
      aggregated_metric-stmts_wh   = aggregated_metric-stmts_wh   + metric->stmts_wh  .
      aggregated_metric-stmts_sl   = aggregated_metric-stmts_sl   + metric->stmts_sl  .
      aggregated_metric-stmts_ins  = aggregated_metric-stmts_ins  + metric->stmts_ins .
      aggregated_metric-stmts_upd  = aggregated_metric-stmts_upd  + metric->stmts_upd .
      aggregated_metric-stmts_del  = aggregated_metric-stmts_del  + metric->stmts_del .
      aggregated_metric-stmts_mod  = aggregated_metric-stmts_mod  + metric->stmts_mod .
      aggregated_metric-diff_vers  = aggregated_metric-diff_vers  + metric->diff_vers .
      aggregated_metric-diff_mod_l = aggregated_metric-diff_mod_l + metric->diff_mod_l.
      aggregated_metric-diff_new_l = aggregated_metric-diff_new_l + metric->diff_new_l.
      aggregated_metric-diff_sap   = aggregated_metric-diff_sap   + metric->diff_sap  .
      aggregated_metric-mod_db     = aggregated_metric-mod_db     + metric->mod_db    .
      aggregated_metric-mod_call   = aggregated_metric-mod_call   + metric->mod_call  .
      aggregated_metric-mod_bra    = aggregated_metric-mod_bra    + metric->mod_bra   .
      aggregated_metric-number_of_methods = aggregated_metric-number_of_methods + 1.
      AT END OF modu_unit_1.
        aggregated_metric-average_nos_per_method = aggregated_metric-nos / aggregated_metric-number_of_methods.
        aggregated_metric-complexity_of_conditions = aggregated_metric-mcc_com / aggregated_metric-number_of_methods.
        aggregated_metric-decission_depht = aggregated_metric-dd_total / aggregated_metric-number_of_methods.
        APPEND aggregated_metric TO aggregated_metrics.
        CLEAR aggregated_metric.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert2json.
    write_element( name  = 'object').
    write_element( name  = 'str'  attr = 'projectName'  value = 'ABAP' ).
    json_writer->close_element( ).
    write_element( name  = 'str'  attr = 'apiVersion'  value = '1.1' ).
    json_writer->close_element( ).
    write_element( name  = 'array' attr = 'nodes' ).
    write_element( name  = 'object' ).
    write_element( name  = 'str'  attr = 'name'  value = 'root' ).
    json_writer->close_element( ).
    write_element( name  = 'str'  attr = 'type'  value = 'Folder' ).
    json_writer->close_element( ).
    write_element( name  = 'object' attr = 'attributes' ).
    json_writer->close_element( ).
    write_element( name  = 'str'  attr = 'link' ).
    json_writer->close_element( ).
    write_element( name  = 'array' attr = 'children' ).

    LOOP AT packages REFERENCE INTO DATA(package)
      WHERE parent IS INITIAL.
      write_package( package->* ).
    ENDLOOP.
    json_writer->close_element( ).
    json_writer->close_element( ).
    json_writer->close_element( ).
    json_writer->close_element( ).

    DATA(xjson) = CAST cl_sxml_string_writer( json_writer )->get_output(  ).
    DATA(reader) = cl_sxml_string_reader=>create( xjson ).
    DATA(writer) = CAST if_sxml_writer(
                                 cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).
    writer->set_option( option = if_sxml_writer=>co_opt_linebreaks ).
    writer->set_option( option = if_sxml_writer=>co_opt_indent ).
    reader->next_node( ).
    reader->skip_node( writer ).
    result = cl_abap_codepage=>convert_from( CAST cl_sxml_string_writer( writer )->get_output( ) ).
  ENDMETHOD.

  METHOD write_package.
    write_element( name  = 'object' ).
    write_element( name  = 'str'  attr = 'name'  value = CONV #( package-package ) ).
    json_writer->close_element( ).
    write_element( name  = 'str'  attr = 'type'  value = 'Folder' ).
    json_writer->close_element( ).
    write_element( name  = 'object' attr = 'attributes' ).
    json_writer->close_element( ).
    write_element( name  = 'str'  attr = 'link' ).
    json_writer->close_element( ).
    write_element( name  = 'array' attr = 'children' ).
    LOOP AT aggregated_metrics REFERENCE INTO DATA(class)
      WHERE package = package-package.
      write_element( name  = 'object' ).
      write_element( name  = 'str'  attr = 'name'  value = |{ class->modu_unit_1 }.{ class->category }| ) .
      json_writer->close_element( ).
      write_element( name  = 'str'  attr = 'type'  value = 'File' ).
      json_writer->close_element( ).
      write_element( name  = 'object' attr = 'attributes' ).


      write_element( name  = 'num'  attr = 'LinesOfCode'  value = CONV #( class->loc ) ).
      json_writer->close_element( ).
      write_element( name  = 'num'  attr = 'Statements'  value = CONV #( class->nos ) ).
      json_writer->close_element( ).
      write_element( name  = 'num'  attr = 'AvgStatementsPerMethod'  value = CONV #( class->average_nos_per_method ) ).
      json_writer->close_element( ).
      write_element( name  = 'num'  attr = 'NumberOfChanges'  value = CONV #( class->diff_vers ) ).
      json_writer->close_element( ).
      write_element( name  = 'num'  attr = 'ComplexityOfConditions'  value = CONV #( class->complexity_of_conditions ) ).
      json_writer->close_element( ).
      write_element( name  = 'num'  attr = 'DecissionDepth'  value = CONV #( class->decission_depht ) ).
      json_writer->close_element( ).

      json_writer->close_element( ).
      write_element( name  = 'str'  attr = 'link'  value = VALUE #(  ) ).
      json_writer->close_element( ).
      write_element( name  = 'array' attr = 'children' ).
      json_writer->close_element( ).
      json_writer->close_element( ).
    ENDLOOP.
    LOOP AT packages REFERENCE INTO DATA(child_package)
    WHERE parent = package-package.
      write_package( child_package->* ).
    ENDLOOP.
    json_writer->close_element( ).
    json_writer->close_element( ).
  ENDMETHOD.

  METHOD write_element.
    json_writer->open_element( name ).
    IF attr IS NOT INITIAL.
      json_writer->write_attribute( name = 'name' value = attr ).
    ENDIF.
    IF value IS NOT INITIAL.
      json_writer->write_value( value ).
    ENDIF.
  ENDMETHOD.

  METHOD aggregate_packages.
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
  ENDMETHOD.

ENDCLASS.
