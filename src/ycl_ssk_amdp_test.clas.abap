CLASS ycl_ssk_amdp_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF msty_order,
        customer   TYPE kunnr,
        order_num  TYPE vbeln,
        order_date TYPE erdat,
        order_time TYPE erzet,
      END OF msty_order,
      mtty_order TYPE SORTED TABLE OF msty_order
        WITH UNIQUE KEY primary_key
          COMPONENTS
            customer
            order_num.

    INTERFACES if_amdp_marker_hdb.

    CLASS-METHODS:
      get_latest_n_orders_per_cust
        IMPORTING
          VALUE(iv_where) TYPE string
          VALUE(iv_num)   TYPE i
        EXPORTING
          VALUE(et_order) TYPE mtty_order.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ycl_ssk_amdp_test IMPLEMENTATION.
  METHOD get_latest_n_orders_per_cust
    BY DATABASE PROCEDURE
    FOR HDB
    LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY
    USING vbak.

    --Apply the selection criteria on the DB table
    lt_order = APPLY_FILTER ( vbak, :iv_where );

    --Split/partition the result(lt_order) by customer sorted by creation date & time in descending order
    --and assign row numbers per section. Row numbers will be reset after end of each section/customer
    --Then select only the specified row numbers per section/customer from the result
    et_order = SELECT customer,
                      order_num,
                      order_date,
                      order_time
                 FROM ( SELECT kunnr AS customer,
                               vbeln AS order_num,
                               erdat AS order_date,
                               erzet AS order_time,
                               ROW_NUMBER() OVER ( PARTITION BY kunnr
                                                   ORDER BY erdat, erzet DESC ) AS row_num
                          FROM :lt_order )
                 WHERE row_num <= :iv_num;
  ENDMETHOD.
ENDCLASS.
