class ZCL_ZSO_MANAGE_DPC_EXT definition
  public
  inheriting from ZCL_ZSO_MANAGE_DPC
  create public .

public section.
protected section.

  methods SOHEADERSET_CREATE_ENTITY
    redefinition .
  methods SOHEADERSET_DELETE_ENTITY
    redefinition .
  methods SOHEADERSET_GET_ENTITY
    redefinition .
  methods SOHEADERSET_GET_ENTITYSET
    redefinition .
  methods SOHEADERSET_UPDATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZSO_MANAGE_DPC_EXT IMPLEMENTATION.


METHOD soheaderset_create_entity.

  DATA ls_soheader TYPE zsoheader.

  TRY.
      io_data_provider->read_entry_data(
        IMPORTING
          es_data = ls_soheader
      ).
    CATCH /iwbep/cx_mgw_tech_exception /iwbep/cx_mgw_busi_exception INTO DATA(cx_error).
      cx_error->get_longtext( ).
  ENDTRY.

  SELECT SINGLE MAX( salesorderid )
  FROM zsoheader
  INTO @DATA(lv_max_id).

  DATA(lv_last_id) = CONV int4( lv_max_id ).
  lv_last_id += 1.
  ls_soheader-salesorderid = lv_last_id.
  ls_soheader-salesorderid = |{ ls_soheader-salesorderid ALPHA = IN }|.
  ls_soheader-createdby = cl_abap_context_info=>get_user_technical_name( ).
  ls_soheader-totalorder = ls_soheader-totalitems + ls_soheader-totalfreight.
  GET TIME STAMP FIELD ls_soheader-createdat.
  INSERT zsoheader FROM ls_soheader.

  IF sy-subrc NE 0.
    DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Error during order creation'
    ).

    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  er_entity = CORRESPONDING #( ls_soheader ).

ENDMETHOD.


METHOD soheaderset_delete_entity.

  DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  DATA(lv_salesorderid) = VALUE #( it_key_tab[ name = 'SalesOrderId' ]-value OPTIONAL ).

  DELETE FROM zsoitem WHERE salesorderid EQ lv_salesorderid.
  IF sy-subrc NE 0.
    ROLLBACK WORK.

    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Error during sales order item deletion'
    ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  DELETE FROM zsoheader WHERE salesorderid EQ lv_salesorderid.
  IF sy-subrc NE 0.
    ROLLBACK WORK.

    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Error during sales order deletion'
    ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDMETHOD.


METHOD soheaderset_get_entity.

  DATA(lv_salesorderid) = VALUE #( it_key_tab[ name = 'SalesOrderId' ]-value OPTIONAL ).

  SELECT SINGLE *
  FROM zsoheader
  WHERE salesorderid EQ @lv_salesorderid
  INTO @DATA(ls_soheader).

  IF sy-subrc NE 0.
    DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Invalid Sales Order'
    ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  er_entity = CORRESPONDING #( ls_soheader ).

ENDMETHOD.


METHOD soheaderset_get_entityset.

  DATA:
    lt_orderby TYPE TABLE OF string,
    lv_orderby TYPE string.

  LOOP AT it_order ASSIGNING FIELD-SYMBOL(<fs_order>).
    <fs_order>-order = to_upper( <fs_order>-order ).
    <fs_order>-order = COND #( WHEN <fs_order>-order EQ 'DESC' THEN 'DESCENDING'
                                                               ELSE 'ASCENDING' ).
    APPEND |{ <fs_order>-property } { <fs_order>-order }| TO lt_orderby.
  ENDLOOP.
  CONCATENATE LINES OF lt_orderby INTO lv_orderby.

  IF lv_orderby IS INITIAL.
    lv_orderby = |SalesOrderId ASCENDING|.
  ENDIF.

  SELECT *
  FROM zsoheader
  WHERE (iv_filter_string)
  ORDER BY (lv_orderby)
  INTO TABLE @DATA(lt_soheader)
  UP TO @is_paging-top ROWS
  OFFSET @is_paging-skip.

  IF lines( lt_soheader ) EQ 0.
    DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'No sales orders found'
    ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  et_entityset = CORRESPONDING #( lt_soheader ).

ENDMETHOD.


METHOD soheaderset_update_entity.

  DATA ls_soheader TYPE zsoheader.
  DATA has_error TYPE abap_bool.

  DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  io_data_provider->read_entry_data(
    IMPORTING
      es_data = ls_soheader
  ).

  IF ls_soheader-salesorderid EQ 0.
    has_error = abap_true.
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Invalid Sales Order number'
    ).
  ENDIF.

  IF ls_soheader-customerid EQ 0.
    has_error = abap_true.
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Invalid Customer number'
    ).
  ENDIF.

  IF has_error EQ abap_true.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  UPDATE zsoheader
  SET customerid = ls_soheader-customerid
      totalitems = ls_soheader-totalitems
      totalfreight = ls_soheader-totalfreight
      totalorder = ls_soheader-totalorder
      status = ls_soheader-status
  WHERE salesorderid EQ ls_soheader-salesorderid.

  IF sy-subrc NE 0.
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Error during update processing'
    ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

ENDMETHOD.
ENDCLASS.
