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
*SOFTWARE.

REPORT zi_metrics_to_codecharta.

CLASS lcl_code_metrics DEFINITION.
  PUBLIC SECTION.
    METHODS run
      IMPORTING
        selection_variant TYPE variant
      RETURNING
        VALUE(result)     TYPE ztti_code_metrics
      RAISING
        cx_salv_bs_sc_runtime_info.
  PRIVATE SECTION.
    METHODS set_alv_runtime_info.
    METHODS submit_code_metrics_report
      IMPORTING
        i_selection_variant TYPE variant.
    METHODS get_alv_list_from_report
      RETURNING
        VALUE(r_result) TYPE ztti_code_metrics
      RAISING
        cx_salv_bs_sc_runtime_info.
ENDCLASS.

CLASS lcl_code_metrics IMPLEMENTATION.
  METHOD run.
    set_alv_runtime_info( ).
    submit_code_metrics_report( selection_variant ).
    result = get_alv_list_from_report( ).
  ENDMETHOD.

  METHOD get_alv_list_from_report.
    FIELD-SYMBOLS <alv_list>   TYPE STANDARD TABLE.
    DATA alv_list              TYPE REF TO data.
    cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = alv_list ).
    ASSIGN alv_list->* TO <alv_list>.
    MOVE-CORRESPONDING <alv_list> TO r_result.
  ENDMETHOD.

  METHOD submit_code_metrics_report.
    SUBMIT /sdf/cd_custom_code_metric
    USING SELECTION-SET i_selection_variant
    EXPORTING LIST TO MEMORY
    AND RETURN.
  ENDMETHOD.

  METHOD set_alv_runtime_info.
    cl_salv_bs_runtime_info=>set(
      EXPORTING display  = abap_false
                metadata = abap_false
                data     = abap_true ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_file_output DEFINITION.
  PUBLIC SECTION.
    METHODS write_file
      IMPORTING
        file_name TYPE localfile
        json      TYPE string.
  PRIVATE SECTION.
    METHODS convert_string_to_xstring
      IMPORTING
        string        TYPE string
      RETURNING
        VALUE(result) TYPE xstring.
    METHODS write_xstring_to_file
      IMPORTING
        file_name TYPE localfile
        xstring   TYPE xstring.
ENDCLASS.

CLASS lcl_file_output IMPLEMENTATION.
  METHOD write_file.
    DATA(json_as_xstring) = convert_string_to_xstring( json ).
    write_xstring_to_file(
        file_name = file_name
        xstring   = json_as_xstring ).
  ENDMETHOD.
  METHOD write_xstring_to_file.
    OPEN DATASET file_name FOR OUTPUT IN BINARY MODE.
    IF sy-subrc NE 0.
      WRITE : / 'Error Opening the Server Filepath :', file_name.
    ELSE.
      WRITE : / 'File Downloaded to the Server at :', file_name.
    ENDIF.

    TRANSFER xstring TO file_name.
    CLOSE DATASET file_name.
  ENDMETHOD.
  METHOD convert_string_to_xstring.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = string
      IMPORTING
        buffer = result.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  PARAMETERS p_varnt TYPE variant.
  PARAMETERS p_file TYPE localfile.

  DATA(code_metrics) = NEW lcl_code_metrics( )->run( p_varnt ).
  DATA(json) = NEW zcl_i_metrics_to_json( )->to_json( code_metrics ).
  new lcl_file_output( )->write_file( file_name = p_file
                                      json      = json ).
