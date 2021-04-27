interface ZIF_I_A2CC_PACKAGE_ANALYZER
  public .

    METHODS aggregate_packages
      IMPORTING
        aggregated_metrics  type ztti_a2cc_code_metrics
      RETURNING
        VALUE(result) type ztti_a2cc_packages.

    METHODS get_parent_packages
      IMPORTING
        package       TYPE zsi_a2cc_code_metrics-package
      RETURNING
        VALUE(result) TYPE string.
endinterface.
