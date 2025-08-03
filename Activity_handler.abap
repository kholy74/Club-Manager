CLASS zcl_me_cm_activity_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS get_activity
      IMPORTING
        !iv_activity_id    TYPE z_me_cm_a_id
      RETURNING
        VALUE(rs_activity) TYPE zme_cm_activity
      RAISING
        zcx_cm_activity .
    METHODS create_activity
      IMPORTING
        !is_new_activity  TYPE zme_cm_activity
      RETURNING
        VALUE(rv_created) TYPE abap_bool
      RAISING
        zcx_cm_activity .
    METHODS update_activity
      IMPORTING
        !is_updated_activity TYPE zme_cm_activity
        !iv_activity_id      TYPE z_me_cm_a_id
      RETURNING
        VALUE(rv_updated)    TYPE abap_bool
      RAISING
        zcx_cm_activity .
    METHODS get_all_activities
      RETURNING
        VALUE(rt_activities) TYPE z_cm_activities .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS check_activity_id
      IMPORTING
        !iv_activity_id TYPE z_me_cm_a_id
      RAISING
        zcx_cm_activity .
    METHODS check_new_activity_id
      IMPORTING
        !iv_activity_id TYPE z_me_cm_a_id
      RAISING
        zcx_cm_activity .
    METHODS check_activity_data_fields
      IMPORTING
        !is_new_activity TYPE zme_cm_activity
      RAISING
        zcx_cm_activity .
    METHODS check_activity_name
      IMPORTING
        !iv_activity_name TYPE z_me_cm_a_name
      RAISING
        zcx_cm_activity .
     METHODS check_updated_activity_name
      IMPORTING
        !iv_activity_name TYPE z_me_cm_a_name
      RAISING
        zcx_cm_activity .
    METHODS check_description
      IMPORTING
        !iv_description TYPE z_me_cm_a_description
      RAISING
        zcx_cm_activity .
    METHODS check_place
      IMPORTING
        !iv_place TYPE z_me_cm_a_place
      RAISING
        zcx_cm_activity .
    METHODS check_max_capacity
      IMPORTING
        !iv_max_capacity TYPE z_me_cm_max_capacity
      RAISING
        zcx_cm_activity .
    METHODS check_updated_fields
      IMPORTING
        !is_updated_activity TYPE zme_cm_activity
        !iv_activity_id      TYPE z_me_cm_a_id
      RAISING
        zcx_cm_activity .
    METHODS check_updated_max_capacity
      IMPORTING
        !iv_max_capacity TYPE z_me_cm_max_capacity
        !iv_activity_id  TYPE z_me_cm_a_id
      RAISING
        zcx_cm_activity .
    METHODS change_member_activity_id
      IMPORTING
        !iv_old_activity_id TYPE z_me_cm_a_id
        !iv_new_activity_id TYPE z_me_cm_a_id
      RAISING
        zcx_cm_activity .

ENDCLASS.



CLASS ZCL_ME_CM_ACTIVITY_HANDLER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_ACTIVITY_HANDLER->CHANGE_MEMBER_ACTIVITY_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OLD_ACTIVITY_ID             TYPE        Z_ME_CM_A_ID
* | [--->] IV_NEW_ACTIVITY_ID             TYPE        Z_ME_CM_A_ID
* | [!CX!] ZCX_CM_ACTIVITY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD change_member_activity_id.


    " Updating the activity ID in the member activity database table
    " if the user enters a new valid one
    IF iv_new_activity_id IS NOT INITIAL.
      IF iv_new_activity_id <> iv_old_activity_id.

        UPDATE zme_cm_mem_activ
        SET activity_id = @iv_new_activity_id
        WHERE activity_id = @iv_old_activity_id.

      ENDIF.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_ACTIVITY_HANDLER->CHECK_ACTIVITY_DATA_FIELDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_NEW_ACTIVITY                TYPE        ZME_CM_ACTIVITY
* | [!CX!] ZCX_CM_ACTIVITY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_activity_data_fields.

    check_new_activity_id( is_new_activity-id ).
    check_activity_name( is_new_activity-name ).
    check_description( is_new_activity-description ).
    check_place( is_new_activity-place ).
    check_max_capacity( is_new_activity-max_capacity ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_ACTIVITY_HANDLER->CHECK_ACTIVITY_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTIVITY_ID                 TYPE        Z_ME_CM_A_ID
* | [!CX!] ZCX_CM_ACTIVITY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_activity_id.

    IF iv_activity_id IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_activity
        EXPORTING
          textid        = zcx_cm_activity=>missing_field_error
          missing_field = 'Activity ID'.
    ENDIF.
    SELECT SINGLE id FROM zme_cm_activity INTO @DATA(lv_id)
        WHERE id = @iv_activity_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_activity
        EXPORTING
          textid = zcx_cm_activity=>activity_id_not_found.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_ACTIVITY_HANDLER->CHECK_ACTIVITY_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTIVITY_NAME               TYPE        Z_ME_CM_A_NAME
* | [!CX!] ZCX_CM_ACTIVITY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_activity_name.

    IF iv_activity_name IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_activity
        EXPORTING
          textid        = zcx_cm_activity=>missing_field_error
          missing_field = 'Activity Name'.
    ENDIF.
    SELECT SINGLE id FROM zme_cm_activity INTO @DATA(lv_name)
        WHERE name = @iv_activity_name.
    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE zcx_cm_activity
        EXPORTING
          textid = zcx_cm_activity=>name_exists.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_ACTIVITY_HANDLER->CHECK_DESCRIPTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DESCRIPTION                 TYPE        Z_ME_CM_A_DESCRIPTION
* | [!CX!] ZCX_CM_ACTIVITY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_description.

    IF iv_description IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_activity
        EXPORTING
          textid        = zcx_cm_activity=>missing_field_error
          missing_field = 'Activity Descreption'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_ACTIVITY_HANDLER->CHECK_MAX_CAPACITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MAX_CAPACITY                TYPE        Z_ME_CM_MAX_CAPACITY
* | [!CX!] ZCX_CM_ACTIVITY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_max_capacity.

    IF iv_max_capacity IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_activity
        EXPORTING
          textid        = zcx_cm_activity=>missing_field_error
          missing_field = 'Maximum Capacity'.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_ACTIVITY_HANDLER->CHECK_NEW_ACTIVITY_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTIVITY_ID                 TYPE        Z_ME_CM_A_ID
* | [!CX!] ZCX_CM_ACTIVITY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_new_activity_id.

    IF iv_activity_id IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_activity
        EXPORTING
          textid        = zcx_cm_activity=>missing_field_error
          missing_field = 'Activity ID'.
    ENDIF.
    SELECT SINGLE id FROM zme_cm_activity INTO @DATA(lv_id)
        WHERE id = @iv_activity_id.
    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE zcx_cm_activity
        EXPORTING
          textid = zcx_cm_activity=>id_exists.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_ACTIVITY_HANDLER->CHECK_PLACE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PLACE                       TYPE        Z_ME_CM_A_PLACE
* | [!CX!] ZCX_CM_ACTIVITY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_place.

    IF iv_place IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_activity
        EXPORTING
          textid        = zcx_cm_activity=>missing_field_error
          missing_field = 'Activity Place'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_ACTIVITY_HANDLER->CHECK_UPDATED_ACTIVITY_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTIVITY_NAME               TYPE        Z_ME_CM_A_NAME
* | [!CX!] ZCX_CM_ACTIVITY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_updated_activity_name.

    SELECT COUNT( name ) FROM zme_cm_activity
        WHERE name = @iv_activity_name INTO @DATA(lv_name_counter).
    IF lv_name_counter > 1.
      RAISE EXCEPTION TYPE zcx_cm_activity
        EXPORTING
          textid = zcx_cm_activity=>name_exists.
    ENDIF.

    ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_ACTIVITY_HANDLER->CHECK_UPDATED_FIELDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_UPDATED_ACTIVITY            TYPE        ZME_CM_ACTIVITY
* | [--->] IV_ACTIVITY_ID                 TYPE        Z_ME_CM_A_ID
* | [!CX!] ZCX_CM_ACTIVITY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_updated_fields.

    " Checking the original activity ID
    check_activity_id( iv_activity_id ).

    " Checking the new Activity ID
    IF is_updated_activity-id IS NOT INITIAL.
      check_new_activity_id( is_updated_activity-id ).
    ENDIF.

    " Checking the new Name
    IF is_updated_activity-name IS NOT INITIAL.
      check_updated_activity_name( is_updated_activity-name ).
    ENDIF.

    " Checking the value of the maximum capacity
    check_updated_max_capacity( iv_max_capacity = is_updated_activity-max_capacity
                                iv_activity_id  = iv_activity_id ).


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_ACTIVITY_HANDLER->CHECK_UPDATED_MAX_CAPACITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MAX_CAPACITY                TYPE        Z_ME_CM_MAX_CAPACITY
* | [--->] IV_ACTIVITY_ID                 TYPE        Z_ME_CM_A_ID
* | [!CX!] ZCX_CM_ACTIVITY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_updated_max_capacity.
    IF iv_max_capacity IS NOT INITIAL.

      " Getting the value of the current maximum capacity of the activity
      SELECT SINGLE max_capacity FROM zme_cm_activity
        WHERE id = @iv_activity_id INTO @DATA(lv_current_capacity).

      " The new maximum capacity can not be less than the current maximum capacity
      IF iv_max_capacity < lv_current_capacity.
        RAISE EXCEPTION TYPE zcx_cm_activity
          EXPORTING
            textid           = zcx_cm_activity=>maximum_capacity_error
            current_capacity = lv_current_capacity - 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_ACTIVITY_HANDLER->CREATE_ACTIVITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_NEW_ACTIVITY                TYPE        ZME_CM_ACTIVITY
* | [<-()] RV_CREATED                     TYPE        ABAP_BOOL
* | [!CX!] ZCX_CM_ACTIVITY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_activity.
    rv_created = abap_false.
    DATA(ls_new_activity_data) = is_new_activity.
    " Checking all fields
    check_activity_data_fields( ls_new_activity_data ).

    " Assiging the maximum capacity as a current capacity
    ls_new_activity_data-capacity = ls_new_activity_data-max_capacity.

    " Activity status will be Active
    ls_new_activity_data-status = 'A'.

    " Inserting the new Activity into the database
    INSERT zme_cm_activity FROM ls_new_activity_data.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_activity
        EXPORTING
          textid = zcx_cm_activity=>creation_error.
    ENDIF.

    rv_created = abap_true.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_ACTIVITY_HANDLER->GET_ACTIVITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTIVITY_ID                 TYPE        Z_ME_CM_A_ID
* | [<-()] RS_ACTIVITY                    TYPE        ZME_CM_ACTIVITY
* | [!CX!] ZCX_CM_ACTIVITY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_activity.
    check_activity_id( iv_activity_id ).
    SELECT SINGLE * FROM zme_cm_activity WHERE id = @iv_activity_id
      INTO @rs_activity.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_ACTIVITY_HANDLER->GET_ALL_ACTIVITIES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_ACTIVITIES                  TYPE        Z_CM_ACTIVITIES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_all_activities.
    SELECT * FROM zme_cm_activity INTO TABLE @rt_activities ORDER BY id.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_ACTIVITY_HANDLER->UPDATE_ACTIVITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_UPDATED_ACTIVITY            TYPE        ZME_CM_ACTIVITY
* | [--->] IV_ACTIVITY_ID                 TYPE        Z_ME_CM_A_ID
* | [<-()] RV_UPDATED                     TYPE        ABAP_BOOL
* | [!CX!] ZCX_CM_ACTIVITY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_activity.
    rv_updated = abap_false.

    " Creating a local structure with the original data of the activity
    SELECT SINGLE * FROM zme_cm_activity WHERE id = @iv_activity_id
      INTO @DATA(ls_activity_data).

    " Checking the given new data
    check_updated_fields( iv_activity_id      = iv_activity_id
                          is_updated_activity = is_updated_activity ).

    " Adding the updated values to the local structure
    IF is_updated_activity-id IS NOT  INITIAL.
      ls_activity_data-id  = is_updated_activity-id.
    ENDIF.
    IF is_updated_activity-name IS NOT  INITIAL.
      ls_activity_data-name  = is_updated_activity-name.
    ENDIF.
    IF is_updated_activity-description IS NOT  INITIAL.
      ls_activity_data-description  = is_updated_activity-description.
    ENDIF.
    IF is_updated_activity-place IS NOT  INITIAL.
      ls_activity_data-place  = is_updated_activity-place.
    ENDIF.
    IF is_updated_activity-max_capacity IS NOT  INITIAL.
      ls_activity_data-max_capacity  = is_updated_activity-max_capacity.
    ENDIF.
    IF is_updated_activity-status IS NOT INITIAL.
      ls_activity_data-status  = is_updated_activity-status.
    ENDIF.

    " Changing the activity ID if the user gives a new valid one
    change_member_activity_id( iv_new_activity_id = is_updated_activity-id
                               iv_old_activity_id = iv_activity_id ).

    UPDATE zme_cm_activity SET id = @ls_activity_data-id,
                               name = @ls_activity_data-name,
                               description = @ls_activity_data-description,
                               place = @ls_activity_data-place,
                               status = @ls_activity_data-status,
                               max_capacity = @ls_activity_data-max_capacity
                               WHERE id = @iv_activity_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_activity
        EXPORTING
          textid = zcx_cm_activity=>update_error.
    ENDIF.
    rv_updated = abap_true.
  ENDMETHOD.
ENDCLASS.
