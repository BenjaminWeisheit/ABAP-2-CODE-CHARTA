CLASS zcl_i_a2cc_metrics_file_writer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS write_json_2_file
      IMPORTING
        file_name TYPE localfile
        json      TYPE string.
  PRIVATE SECTION.
    CONSTANTS utf_8 TYPE string VALUE `UTF-8`.
    METHODS convert_string_to_xstring
      IMPORTING
        string        TYPE string
      RETURNING
        VALUE(result) TYPE xstring.
    METHODS write_xstring_to_file
      IMPORTING
        file_name TYPE localfile
        xstring   TYPE xstring.
    METHODS rplce_placeholders_in_filename
      IMPORTING
        file_name     TYPE localfile
      RETURNING
        VALUE(result) TYPE localfile.
ENDCLASS.

CLASS zcl_i_a2cc_metrics_file_writer IMPLEMENTATION.
  METHOD write_json_2_file.
    write_xstring_to_file(
        file_name = rplce_placeholders_in_filename( file_name )
        xstring   = convert_string_to_xstring( json ) ).
  ENDMETHOD.

  METHOD write_xstring_to_file.
    OPEN DATASET file_name FOR OUTPUT IN BINARY MODE.
    IF sy-subrc NE 0.
      WRITE : |Error opening the server filepath: { file_name }|.
    ELSE.
      TRANSFER xstring TO file_name.
      CLOSE DATASET file_name.
      WRITE : |File downloaded to the server at: { file_name }|.
    ENDIF.
  ENDMETHOD.

  METHOD convert_string_to_xstring.
    result = cl_abap_codepage=>convert_to( source = string codepage = utf_8 ).
  ENDMETHOD.

  METHOD rplce_placeholders_in_filename.
    result = file_name.
    REPLACE ALL OCCURRENCES OF '<DATE>' IN result WITH sy-datum.
  ENDMETHOD.
ENDCLASS.
