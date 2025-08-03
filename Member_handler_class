CLASS zcl_me_cm_member_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF zsrt_member_full_profil,
        member_data  TYPE zme_cm_member,
        member_tarif TYPE zme_cm_tarifs,
        activities   TYPE STANDARD TABLE OF zme_cm_activity WITH EMPTY KEY,
      END OF zsrt_member_full_profil .

    METHODS get_member_data
      IMPORTING
        !iv_member_id         TYPE z_me_cm_m_id
      RETURNING
        VALUE(rs_member_data) TYPE zme_cm_member
      RAISING
        zcx_cm_member .
    METHODS update_member
      IMPORTING
        !is_new_member_data TYPE zme_cm_member
        !iv_member_id       TYPE z_me_cm_m_id
      RETURNING
        VALUE(rv_updated)   TYPE abap_bool
      RAISING
        zcx_cm_member .
    METHODS set_member
      IMPORTING
        !is_member_data        TYPE zme_cm_member
        !iv_tarif_id           TYPE z_me_cm_t_id
        !iv_generate_family_id TYPE abap_bool
      RETURNING
        VALUE(rv_member_id)    TYPE z_me_cm_m_id
      RAISING
        zcx_cm_member .
    METHODS assign_member_to_activity
      IMPORTING
        !iv_member_id   TYPE z_me_cm_m_id
        !iv_activity_id TYPE z_me_cm_a_id
      RETURNING
        VALUE(rv_added) TYPE abap_bool
      RAISING
        zcx_cm_member .
    METHODS unassign_member_from_activity
      IMPORTING
        !iv_activity_id   TYPE z_me_cm_a_id
        !iv_member_id     TYPE z_me_cm_m_id
      RETURNING
        VALUE(rv_deleted) TYPE abap_bool
      RAISING
        zcx_cm_member .
    METHODS get_member_activities
      IMPORTING
        !iv_member_id        TYPE z_me_cm_m_id
      RETURNING
        VALUE(rt_activities) TYPE zme_cm_activity_type
      RAISING
        zcx_cm_member .
    METHODS get_member_tarif
      IMPORTING
        !iv_member_id   TYPE z_me_cm_m_id
      RETURNING
        VALUE(rs_tarif) TYPE zme_cm_tarifs
      RAISING
        zcx_cm_member .
    METHODS get_member_current_tarif
      IMPORTING
        !iv_member_id   TYPE z_me_cm_m_id
      RETURNING
        VALUE(rs_tarif) TYPE zme_cm_tarifs
      RAISING
        zcx_cm_member .
    METHODS get_member_full_profil
      IMPORTING
        !iv_member_id              TYPE z_me_cm_m_id
      RETURNING
        VALUE(rs_member_full_data) TYPE zsrt_member_full_profil
      RAISING
        zcx_cm_member .
    METHODS update_member_tarif
      IMPORTING
        !is_updated_tarif TYPE zme_cm_mem_tarif
        !iv_member_id     TYPE z_me_cm_m_id
      RETURNING
        VALUE(rv_updated) TYPE abap_bool
      RAISING
        zcx_cm_member .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS unassign_all_activities
      IMPORTING
        !iv_member_id TYPE z_me_cm_m_id
      RAISING
        zcx_cm_member .
    METHODS check_member_data_fields
      IMPORTING
        !iv_tarif_id    TYPE z_me_cm_t_id
        !is_member_data TYPE zme_cm_member
      RAISING
        zcx_cm_member .
    METHODS check_member_last_name
      IMPORTING
        !iv_name TYPE z_me_cm_m_name
      RAISING
        zcx_cm_member .
    METHODS check_member_birthday
      IMPORTING
        !iv_birthday TYPE z_me_cm_m_bd
      RAISING
        zcx_cm_member.
    METHODS check_member_family_id
      IMPORTING
        !iv_family_id TYPE z_me_cm_m_family_id
      RAISING
        zcx_cm_member .
    METHODS check_member_telephone
      IMPORTING
        !iv_telephone TYPE z_me_cm_m_telephone
      RAISING
        zcx_cm_member .
    METHODS check_member_email
      IMPORTING
        !iv_email TYPE z_me_cm_m_email
      RAISING
        zcx_cm_member .
    METHODS check_member_address
      IMPORTING
        !iv_address TYPE z_me_cm_m_address
      RAISING
        zcx_cm_member .
    METHODS check_updated_fields
      IMPORTING
        !iv_member_id                TYPE z_me_cm_m_id
        !is_new_member_data          TYPE zme_cm_member
      RETURNING
        VALUE(rs_all_updated_fields) TYPE zme_cm_member
      RAISING
        zcx_cm_member .
    METHODS generate_member_id
      RETURNING
        VALUE(rv_member_id) TYPE z_me_cm_m_id
      RAISING
        zcx_cm_member .
    METHODS check_duplicates
      IMPORTING
        !is_member_data TYPE zme_cm_member
      RAISING
        zcx_cm_member .
    METHODS generate_family_id
      IMPORTING
        !iv_member_id       TYPE z_me_cm_m_id
      RETURNING
        VALUE(rv_family_id) TYPE z_me_cm_m_family_id
      RAISING
        zcx_cm_member .
    METHODS check_member_first_name
      IMPORTING
        !iv_first_name TYPE z_me_cm_m_f_name
      RAISING
        zcx_cm_member .
    METHODS check_member_postal_code
      IMPORTING
        !iv_postal_code TYPE z_me_cm_m_postal_code
      RAISING
        zcx_cm_member .
    METHODS check_member_city
      IMPORTING
        !iv_city TYPE z_me_cm_m_city
      RAISING
        zcx_cm_member .
    METHODS check_tarif_id
      IMPORTING
        !iv_tarif_id TYPE z_me_cm_t_id
      RAISING
        zcx_cm_member .
    METHODS set_member_tarif
      IMPORTING
        !iv_member_id TYPE z_me_cm_m_id
        !iv_tarif_id  TYPE z_me_cm_t_id
      RAISING
        zcx_cm_member .
    METHODS insert_new_family_id
      IMPORTING
        !iv_family_id TYPE z_me_cm_m_family_id
        !iv_member_id TYPE z_me_cm_m_id
      RAISING
        zcx_cm_member .
    METHODS check_member_id
      IMPORTING
        !iv_member_id TYPE z_me_cm_m_id
      RAISING
        zcx_cm_member .
    METHODS update_status_tarif_date
      IMPORTING
        !iv_member_id          TYPE z_me_cm_m_id
        !iv_account_status     TYPE z_me_cm_m_status
        !iv_account_old_status TYPE z_me_cm_m_status
      RAISING
        zcx_cm_member .
    METHODS update_family_id
      IMPORTING
        !iv_member_id    TYPE z_me_cm_m_id
        !iv_new_tarif_id TYPE z_me_cm_t_id
      RAISING
        zcx_cm_member .
    METHODS check_ac_status_end_date
      IMPORTING
        !iv_account_status     TYPE z_me_cm_m_status
        !iv_new_tarif_end_date TYPE z_me_cm_t_valid_to
      RAISING
        zcx_cm_member .
    METHODS check_member_termination
      IMPORTING
        !iv_member_id      TYPE z_me_cm_m_id
        !iv_account_status TYPE z_me_cm_m_status
      RAISING
        zcx_cm_member .
    METHODS end_tarif_valid_to
      IMPORTING
        !iv_member_id TYPE z_me_cm_m_id
      RAISING
        zcx_cm_member .
    METHODS set_new_date_up_tarif
      EXPORTING
        !ev_end_date   TYPE z_me_cm_t_valid_to
        !ev_start_date TYPE z_me_cm_t_valid_from .
    METHODS check_tarif_start_update
      IMPORTING
        !iv_start_date TYPE z_me_cm_t_valid_to
        !iv_member_id  TYPE z_me_cm_m_id
      RAISING
        zcx_cm_member .
    METHODS delete_mem_tarif_mistake
      IMPORTING
        !is_member_tarif TYPE zme_cm_mem_tarif .
    METHODS extend_valid_to
      IMPORTING
        !is_tarif TYPE zme_cm_mem_tarif
      RAISING
        zcx_cm_member .
    METHODS check_family_id_tarif
      IMPORTING
        !iv_family_id TYPE z_me_cm_m_family_id
        !iv_tarif_id  TYPE z_me_cm_t_id
      RAISING
        zcx_cm_member .
    METHODS check_account_status
      IMPORTING
        !iv_member_id TYPE z_me_cm_m_id
      RAISING
        zcx_cm_member .
    METHODS set_activity_last_day
      IMPORTING
        !is_mem_activity TYPE zme_cm_mem_activ .
ENDCLASS.



CLASS ZCL_ME_CM_MEMBER_HANDLER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_MEMBER_ADDRESS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ADDRESS                     TYPE        Z_ME_CM_M_ADDRESS
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_member_address.

    " Checking if the Address field has a value
    IF iv_address IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>missing_field_error
          missing_field = 'Address'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_MEMBER_BIRTHDAY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BIRTHDAY                    TYPE        Z_ME_CM_M_BD
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_member_birthday.

    " Checking if the birthday field has a value
    IF iv_birthday IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>missing_field_error
          missing_field = 'Birthday'.
    ENDIF.

    " Checking if the birthday is not after today's date
    IF iv_birthday >= sy-datum.
      DATA lv_string_value TYPE string.
      lv_string_value = iv_birthday.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>invalid_field_value
          invalid_value = lv_string_value
          invalid_field = 'Birthday'.

    ENDIF.

    " Checking the the member's age is not above 99 years
    DATA lv_age TYPE cmp_noyrs.
    CALL FUNCTION 'HRCM_TIME_PERIOD_CALCULATE'
      EXPORTING
        begda         = iv_birthday
        endda         = sy-datum
      IMPORTING
        noyrs         = lv_age
      EXCEPTIONS
        invalid_dates = 1
        overflow      = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid = zcx_cm_member=>invalid_birthday.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_MEMBER_DATA_FIELDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TARIF_ID                    TYPE        Z_ME_CM_T_ID
* | [--->] IS_MEMBER_DATA                 TYPE        ZME_CM_MEMBER
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_member_data_fields.

    check_tarif_id( iv_tarif_id ).

    check_member_last_name( is_member_data-last_name ).

    check_member_first_name( is_member_data-first_name ).

    check_member_address( is_member_data-address ).

    check_member_postal_code( is_member_data-postal_code ).

    check_member_city( is_member_data-city ).

    check_member_birthday( is_member_data-birthday ).

    check_member_email( is_member_data-email ).

    check_member_family_id( is_member_data-family_id ).

    check_member_telephone( is_member_data-telephone ).

    check_family_id_tarif( iv_family_id = is_member_data-family_id
                           iv_tarif_id  = iv_tarif_id ).
    " Checking the duplicates
    check_duplicates( is_member_data ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_MEMBER_EMAIL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_EMAIL                       TYPE        Z_ME_CM_M_EMAIL
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_member_email.

    " Checking if the email field has a value
    IF iv_email IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>missing_field_error
          missing_field = 'Email'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_MEMBER_FAMILY_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FAMILY_ID                   TYPE        Z_ME_CM_M_FAMILY_ID
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_member_family_id.

    " Starting the check if the family ID has a value
    IF iv_family_id IS NOT INITIAL.

      " Checking validity the value of the family ID
      SELECT SINGLE * FROM zme_cm_family_id INTO @DATA(ls_family_id)
    WHERE family_id = @iv_family_id.
      IF sy-subrc <> 0.
        DATA lv_string_value TYPE string.
        lv_string_value = iv_family_id.
        RAISE EXCEPTION TYPE zcx_cm_member
          EXPORTING
            textid        = zcx_cm_member=>invalid_field_value
            invalid_value = lv_string_value
            invalid_field = 'Family ID'.

      ENDIF.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_MEMBER_TELEPHONE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TELEPHONE                   TYPE        Z_ME_CM_M_TELEPHONE
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_member_telephone.


    " Check if the telephone field is empty

    IF iv_telephone IS INITIAL.

      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>missing_field_error
          missing_field = 'Telephone'.

    ENDIF.

    " Check if the telephone contains only digits
    IF  iv_telephone CA 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!"§$%&/()=?*#''+-,.;:_'..
      DATA lv_string_value TYPE string.
      lv_string_value = iv_telephone.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>invalid_field_value
          invalid_value = lv_string_value
          invalid_field = 'Telephone'.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_MEMBER_HANDLER->GET_MEMBER_ACTIVITIES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [<-()] RT_ACTIVITIES                  TYPE        ZME_CM_ACTIVITY_TYPE
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_member_activities.

    " Import all activities assigned to the member
    SELECT a~*
      FROM zme_cm_activity AS a
      INNER JOIN zme_cm_mem_activ AS ma
        ON ma~activity_id = a~id
      WHERE ma~member_id = @iv_member_id
      AND ma~last_day IS INITIAL
      INTO TABLE @rt_activities.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_MEMBER_HANDLER->SET_MEMBER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MEMBER_DATA                 TYPE        ZME_CM_MEMBER
* | [--->] IV_TARIF_ID                    TYPE        Z_ME_CM_T_ID
* | [--->] IV_GENERATE_FAMILY_ID          TYPE        ABAP_BOOL
* | [<-()] RV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_member.

    " Adding the imported data structure to a local structure
    DATA(ls_member_data) = is_member_data.

    IF ls_member_data-family_id IS NOT INITIAL AND iv_generate_family_id = abap_true.

      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid = zcx_cm_member=>either_create_or_use_old.

    ENDIF.

    " Checking the values of each field
    check_member_data_fields( is_member_data = ls_member_data
                              iv_tarif_id    = iv_tarif_id ).
    ls_member_data-account_status = 'A'.
    " Creating member ID
    ls_member_data-id = generate_member_id( ).


    " Creation day will be exactly the day, when the account is being created
    ls_member_data-created_by = sy-uname.

    " Account Creator will be the SAP user
    ls_member_data-creation_day = sy-datum.

    " Creating a new Family ID if it's allowed
    IF iv_generate_family_id =  abap_true.

      ls_member_data-family_id = generate_family_id( ls_member_data-id ).

    ENDIF.

    " Inserting the structure to the database table
    INSERT zme_cm_member FROM ls_member_data.
    IF sy-subrc = 0.
      rv_member_id = ls_member_data-id.
    ENDIF.

    " Setting the member's tarif
    set_member_tarif( iv_member_id = ls_member_data-id
                      iv_tarif_id  = iv_tarif_id ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_MEMBER_HANDLER->UPDATE_MEMBER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_NEW_MEMBER_DATA             TYPE        ZME_CM_MEMBER
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [<-()] RV_UPDATED                     TYPE        ABAP_BOOL
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_member.

    DATA ls_updated_member_data TYPE zme_cm_member.
    rv_updated = abap_false.
    check_member_id( iv_member_id ).
    SELECT SINGLE account_status FROM zme_cm_member
      WHERE id = @iv_member_id INTO @DATA(lv_old_account_status).
    SELECT SINGLE tarif_id FROM zme_cm_mem_tarif
      WHERE member_id = @iv_member_id AND valid_from = ( SELECT MAX( valid_from ) FROM zme_cm_mem_tarif
                                                          WHERE member_id = @iv_member_id )
         INTO @DATA(lv_old_tarif_id).

    "  Updating the values after checking them.
    ls_updated_member_data = check_updated_fields( iv_member_id       = iv_member_id
                                                   is_new_member_data = is_new_member_data ).

    " assiging the member id
    ls_updated_member_data-id = iv_member_id.


    " Unassiging all activities from the user if the current status is terminated
    check_member_termination( iv_member_id      = iv_member_id
                              iv_account_status = ls_updated_member_data-account_status ).
    UPDATE zme_cm_member FROM ls_updated_member_data.

    " Chainging the tarif date depending of the change of the account status
    update_status_tarif_date( iv_member_id          = iv_member_id
                              iv_account_status     = is_new_member_data-account_status
                              iv_account_old_status = lv_old_account_status ).

    rv_updated = abap_true.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_MEMBER_HANDLER->ASSIGN_MEMBER_TO_ACTIVITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [--->] IV_ACTIVITY_ID                 TYPE        Z_ME_CM_A_ID
* | [<-()] RV_ADDED                       TYPE        ABAP_BOOL
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD assign_member_to_activity.

    rv_added = abap_false.
    SELECT SINGLE * FROM zme_cm_activity INTO @DATA(ls_activity)
      WHERE id = @iv_activity_id.
    IF sy-subrc <> 0.

      " Activity does not exist error
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid = zcx_cm_member=>activity_not_found.
    ENDIF.

    " The member must be an active member
    SELECT SINGLE * FROM zme_cm_member INTO @DATA(ls_member)
      WHERE id = @iv_member_id.
    IF ls_member-account_status <> 'A'.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid      = zcx_cm_member=>member_is_not_active
          member_name = ls_member-last_name.
    ENDIF.

    " The activity must be have free capacity
    IF ls_activity-capacity = 0 .
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid = zcx_cm_member=>activity_limit_reached.
    ENDIF.
    " The activity must be active
    IF ls_activity-status <> 'A'.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid = zcx_cm_member=>inactive_activity.
    ENDIF.

    " Deleting the member activity from the database if it was assigned and deleted today by mistake
    DELETE FROM zme_cm_mem_activ WHERE activity_id = @iv_activity_id
     AND member_id = @iv_member_id
      AND join_date = @sy-datum
      AND last_day = @sy-datum.
    " Getting the required field values of the activity
    SELECT SINGLE * FROM zme_cm_mem_activ INTO @DATA(ls_tmp_activity)
      WHERE member_id = @iv_member_id AND activity_id = @iv_activity_id.
    IF sy-subrc <> 0.

      DATA ls_member_activity TYPE zme_cm_mem_activ.
      ls_member_activity-activity_id = iv_activity_id.
      ls_member_activity-member_id = iv_member_id.
      ls_member_activity-join_date = sy-datum.
      INSERT zme_cm_mem_activ FROM ls_member_activity.

      " Chaning the status of the activity if the current capacity is 0
      ls_activity-capacity -= 1.
      IF ls_activity-capacity = 0.
        ls_activity-status = 'I'.
      ENDIF.

      rv_added = abap_true.
      UPDATE zme_cm_activity FROM ls_activity.
    ELSE.

      " Activity already assigned to the user
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid = zcx_cm_member=>actvity_assigned_to_member.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_DUPLICATES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MEMBER_DATA                 TYPE        ZME_CM_MEMBER
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_duplicates.

    SELECT * FROM zme_cm_member INTO TABLE @DATA(lt_members).

    " Checking if the name, birthday already exist
    DATA(lv_is_duplicated) =  COND #( WHEN line_exists( lt_members[ last_name = is_member_data-last_name ] ) AND
                                          line_exists( lt_members[ first_name = is_member_data-first_name ] ) AND
                                          line_exists( lt_members[ birthday = is_member_data-birthday ] )    THEN abap_true ELSE abap_false ).
    IF lv_is_duplicated = abap_true.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid = zcx_cm_member=>member_already_exists.
    ENDIF.

    " The user must always enter a new telephone number and Email
    SELECT SINGLE telephone FROM zme_cm_member INTO @DATA(lv_telephone) WHERE telephone = @is_member_data-telephone.
    IF sy-subrc = 0.
      DATA lv_string_value TYPE string.
      lv_string_value = is_member_data-telephone.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>value_exits
          invalid_field = 'Telephone'
          invalid_value = lv_string_value.
    ENDIF.

    SELECT SINGLE email FROM zme_cm_member INTO @DATA(lv_email) WHERE email = @is_member_data-email.
    IF sy-subrc = 0.
      DATA lv_string_value_email TYPE string.
      lv_string_value = is_member_data-email.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>value_exits
          invalid_field = 'Email'
          invalid_value = lv_string_value_email.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_MEMBER_CITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CITY                        TYPE        Z_ME_CM_M_CITY
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_member_city.

    " City must have a value
    IF iv_city IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>missing_field_error
          missing_field = 'City'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_MEMBER_FIRST_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FIRST_NAME                  TYPE        Z_ME_CM_M_F_NAME
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_member_first_name.

    " It must only have a value
    IF iv_first_name IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>missing_field_error
          missing_field = 'First Name'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_MEMBER_LAST_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        Z_ME_CM_M_NAME
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_member_last_name.

    " Checking if the last name field has a value
    IF iv_name IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>missing_field_error
          missing_field = 'Last Name'.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_MEMBER_POSTAL_CODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_POSTAL_CODE                 TYPE        Z_ME_CM_M_POSTAL_CODE
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_member_postal_code.

    " It must only have a value, as the user can't enter more than 5 digits
    IF iv_postal_code IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>missing_field_error
          missing_field = 'Postal Code'.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_TARIF_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TARIF_ID                    TYPE        Z_ME_CM_T_ID
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_tarif_id.

    " The tarif ID must have a value and it must be valid
    IF iv_tarif_id IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>missing_field_error
          missing_field = 'Tarif'.
    ENDIF.

    SELECT SINGLE id FROM zme_cm_tarifs WHERE id = @iv_tarif_id
      INTO @DATA(lv_tarif_id).
    IF sy-subrc <> 0.
      DATA lv_string_value TYPE string.
      lv_string_value = iv_tarif_id.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>invalid_field_value
          invalid_value = lv_string_value
          invalid_field = 'Tarif'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_UPDATED_FIELDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [--->] IS_NEW_MEMBER_DATA             TYPE        ZME_CM_MEMBER
* | [<-()] RS_ALL_UPDATED_FIELDS          TYPE        ZME_CM_MEMBER
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_updated_fields.

    check_member_id( iv_member_id ).

    SELECT SINGLE * FROM zme_cm_member INTO @DATA(ls_updated_member_data)
      WHERE id = @iv_member_id.

    " Checking the validity of the new value for account Status
    IF is_new_member_data-account_status IS NOT INITIAL.
      IF is_new_member_data-account_status <> 'A' AND is_new_member_data-account_status <> 'I'.
        DATA lv_string_value TYPE string.
        lv_string_value = is_new_member_data-account_status.
        RAISE EXCEPTION TYPE zcx_cm_member
          EXPORTING
            textid        = zcx_cm_member=>invalid_field_value
            invalid_value = lv_string_value
            invalid_field = 'Account Status'.
      ENDIF.

      " change the value if it's valid
      ls_updated_member_data-account_status = is_new_member_data-account_status.

    ENDIF.

    " change the value of address if the user want to change it
    IF is_new_member_data-address IS NOT INITIAL.
      ls_updated_member_data-address = is_new_member_data-address.
    ENDIF.

    " change the value of birthday if the user want to change it
    IF is_new_member_data-birthday IS NOT INITIAL.
      ls_updated_member_data-birthday = is_new_member_data-birthday.
    ENDIF.

    " change the value of email if the user want to change it
    IF is_new_member_data-email IS NOT INITIAL.
      check_member_email( is_new_member_data-email ).
      ls_updated_member_data-email = is_new_member_data-email.
      CONDENSE ls_updated_member_data-email NO-GAPS.
    ENDIF.

    " change the value of family_id if the user want to change it
    IF is_new_member_data-family_id IS NOT INITIAL.
      check_member_family_id( is_new_member_data-family_id ).
      ls_updated_member_data-family_id = is_new_member_data-family_id.
    ENDIF.

    " change the value of gender if the user want to change it
    IF is_new_member_data-gender IS NOT INITIAL.
      ls_updated_member_data-gender =  is_new_member_data-gender.
    ENDIF.

    " change the values of name if the user want to change them
    IF is_new_member_data-last_name IS NOT INITIAL.
      ls_updated_member_data-last_name = is_new_member_data-last_name.
    ENDIF.
    IF is_new_member_data-first_name IS NOT INITIAL.
      ls_updated_member_data-first_name = is_new_member_data-first_name.
    ENDIF.

    " change the value of telephone if the user want to change it
    IF is_new_member_data-telephone IS NOT INITIAL.
      check_member_telephone( is_new_member_data-telephone ).
      ls_updated_member_data-telephone = is_new_member_data-telephone.
    ENDIF.

    rs_all_updated_fields = ls_updated_member_data.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->GENERATE_MEMBER_ID
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD generate_member_id.

    " Creating a new member ID which will be the pervious ID + 1 using the Number Range Object
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '1'          " Nummernkreisnummer
        object      = 'Z_ME_CM_ME' " Nummernkreisobjekt
        quantity    = 1             " Anzahl der Nummern
      IMPORTING
        number      = rv_member_id
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid = zcx_cm_member=>member_id_limit_reached.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->SET_MEMBER_TARIF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [--->] IV_TARIF_ID                    TYPE        Z_ME_CM_T_ID
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_member_tarif.

    DATA ls_member_tarif TYPE zme_cm_mem_tarif.
    ls_member_tarif-member_id = iv_member_id.
    ls_member_tarif-tarif_id = iv_tarif_id.

    " Tarif start date is today
    ls_member_tarif-valid_from = sy-datum.

    " Tarif end date will be today in 50 years
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = ls_member_tarif-valid_from
        days      = 0
        months    = 0
        years     = 50
      IMPORTING
        calc_date = ls_member_tarif-valid_to.

    INSERT zme_cm_mem_tarif FROM ls_member_tarif.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_MEMBER_HANDLER->UNASSIGN_MEMBER_FROM_ACTIVITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTIVITY_ID                 TYPE        Z_ME_CM_A_ID
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [<-()] RV_DELETED                     TYPE        ABAP_BOOL
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD unassign_member_from_activity.

    rv_deleted = abap_false.
    " Checking if the activity ad member ID fields have values
    check_member_id( iv_member_id ).
    IF iv_activity_id IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>missing_field_error
          missing_field = 'Activity ID'.
    ENDIF.

    " The member should have an activity assigned to him
    SELECT * FROM zme_cm_mem_activ INTO TABLE @DATA(ls_all_activities)
      WHERE member_id  = @iv_member_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid = zcx_cm_member=>member_has_no_activities.
    ENDIF.

    " Deleting the activity from member's account
    " if he has the activity is assigned to him
    SELECT SINGLE * FROM  zme_cm_mem_activ
      WHERE activity_id = @iv_activity_id
      AND member_id = @iv_member_id
      AND ( last_day >= @sy-datum OR last_day IS INITIAL )
      INTO @DATA(ls_member_activity).
    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid = zcx_cm_member=>activity_not_assigned.
    ENDIF.
    set_activity_last_day( ls_member_activity ).
*    DELETE zme_cm_mem_activ FROM ls_member_activity.
    rv_deleted = abap_true.

    " Updating the capacity and status of the activity
    SELECT SINGLE * FROM zme_cm_activity INTO @DATA(ls_activity)
      WHERE id = @iv_activity_id.

    ls_activity-capacity += 1.

    " Activating the actctivity if it was inactive
    IF ls_activity-status <> 'A'.
      ls_activity-status = 'A'.
    ENDIF.


    UPDATE zme_cm_activity FROM ls_activity.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->GENERATE_FAMILY_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [<-()] RV_FAMILY_ID                   TYPE        Z_ME_CM_M_FAMILY_ID
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD generate_family_id.

    " Creating a new family ID which will be the pervious ID + 1 using the Number Range Object
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '2'          " Nummernkreisnummer
        object      = 'Z_ME_CM_ME' " Nummernkreisobjekt
        quantity    = 1             " Anzahl der Nummern
      IMPORTING
        number      = rv_family_id
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid = zcx_cm_member=>member_id_limit_reached.
    ENDIF.

    insert_new_family_id( iv_family_id = rv_family_id
                          iv_member_id = iv_member_id ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->INSERT_NEW_FAMILY_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FAMILY_ID                   TYPE        Z_ME_CM_M_FAMILY_ID
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD insert_new_family_id.

    DATA ls_new_family_id TYPE zme_cm_family_id.
    ls_new_family_id-family_id = iv_family_id.
    ls_new_family_id-member_id = iv_member_id.

    " inserting the new family ID to family IDs the table
    INSERT zme_cm_family_id FROM ls_new_family_id.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_MEMBER_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_member_id.

    " The member ID must have a value
    IF iv_member_id IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>missing_field_error
          missing_field = 'Member ID'.
    ENDIF.
    " The member ID must be valid
    SELECT SINGLE * FROM zme_cm_member INTO @DATA(ls_data)
      WHERE id = @iv_member_id.
    IF sy-subrc <> 0.
      DATA  lv_string_value TYPE string.
      lv_string_value = iv_member_id.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>invalid_field_value
          invalid_field = 'Member ID'
          invalid_value = lv_string_value.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_MEMBER_HANDLER->GET_MEMBER_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [<-()] RS_MEMBER_DATA                 TYPE        ZME_CM_MEMBER
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_member_data.
    check_member_id( iv_member_id ).
    SELECT SINGLE * FROM zme_cm_member INTO @rs_member_data
      WHERE id = @iv_member_id.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_MEMBER_HANDLER->GET_MEMBER_FULL_PROFIL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [<-()] RS_MEMBER_FULL_DATA            TYPE        ZSRT_MEMBER_FULL_PROFIL
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_member_full_profil.
    DATA ls_member_full_data TYPE zsrt_member_full_profil.
    check_member_id( iv_member_id ).
    ls_member_full_data-activities = get_member_activities( iv_member_id ).
    ls_member_full_data-member_data = get_member_data( iv_member_id ).

    " Getting the current tarif if the user has an active one, otherwise getting the last active one
    SELECT SINGLE * from zme_cm_mem_tarif
      where member_id = @iv_member_id and valid_from <= @sy-datum and valid_to >= @sy-datum
      INTO @DATA(ls_tarif).
      IF sy-subrc <> 0.
          ls_member_full_data-member_tarif = get_member_tarif( iv_member_id ).
        else.
          ls_member_full_data-member_tarif = get_member_current_tarif( iv_member_id ).
      ENDIF.



    rs_member_full_data = ls_member_full_data.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_MEMBER_HANDLER->GET_MEMBER_TARIF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [<-()] RS_TARIF                       TYPE        ZME_CM_TARIFS
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_member_tarif.
    check_member_id( iv_member_id ).

    " Getting the member tarif ID
    SELECT SINGLE tarif_id FROM zme_cm_mem_tarif
      WHERE member_id = @iv_member_id AND valid_from = ( SELECT MAX( valid_from ) FROM zme_cm_mem_tarif
                                                          WHERE member_id = @iv_member_id )
      INTO @DATA(lv_tarif_id).
    IF sy-subrc <> 0.
      DATA  lv_string_value TYPE string.
      lv_string_value = iv_member_id.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>invalid_field_value
          invalid_field = 'Member ID'
          invalid_value = lv_string_value.
    ENDIF.

    " Getting the member's tarif
    SELECT SINGLE * FROM zme_cm_tarifs WHERE id = @lv_tarif_id
      INTO @rs_tarif.
    IF sy-subrc <> 0.
      DATA  lv_string_value1 TYPE string.
      lv_string_value1 = lv_tarif_id.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>invalid_field_value
          invalid_field = 'Tarif ID'
          invalid_value = lv_string_value1.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->UPDATE_FAMILY_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [--->] IV_NEW_TARIF_ID                TYPE        Z_ME_CM_T_ID
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_family_id.

    check_member_id( iv_member_id ).
    " Getting the old tarif ID
    SELECT SINGLE tarif_id FROM zme_cm_mem_tarif WHERE member_id = @iv_member_id
      INTO @DATA(lv_old_tarif_id).
    IF sy-subrc <> 0.
      DATA  lv_string_value TYPE string.
      lv_string_value = iv_member_id.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>invalid_field_value
          invalid_field = 'Member ID'
          invalid_value = lv_string_value.
    ENDIF.

    " Getting the value of the family status of the old tarif
    SELECT SINGLE family FROM zme_cm_tarifs WHERE id = @lv_old_tarif_id
      INTO @DATA(lv_old_tarif_family_status).

    " Getting the value of the family status of the new tarif
    SELECT SINGLE family FROM zme_cm_tarifs WHERE id = @iv_new_tarif_id
    INTO @DATA(lv_new_tarif_family_status).

    " Stopping the method if they are the same
    IF lv_new_tarif_family_status = lv_old_tarif_family_status.
      RETURN.
    ENDIF.

    " Deleting the family ID if the new tarif is not a family tarif
    IF lv_old_tarif_family_status = 'X' AND lv_new_tarif_family_status = ' ' .

      SELECT SINGLE * FROM zme_cm_member WHERE id = @iv_member_id
      INTO @DATA(ls_member_data).
      ls_member_data-family_id = 0.
      UPDATE zme_cm_member FROM ls_member_data.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_MEMBER_HANDLER->UPDATE_MEMBER_TARIF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_UPDATED_TARIF               TYPE        ZME_CM_MEM_TARIF
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [<-()] RV_UPDATED                     TYPE        ABAP_BOOL
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_member_tarif.
    rv_updated = abap_false.
    check_tarif_id( is_updated_tarif-tarif_id ).
    check_account_status( iv_member_id ).
    " Getting the old  member-tarif data from zme_cm_mem_tarif
    SELECT SINGLE * FROM zme_cm_mem_tarif
      WHERE member_id = @iv_member_id AND valid_from = ( SELECT MAX( valid_from ) FROM zme_cm_mem_tarif
                                                         WHERE member_id = @iv_member_id )
      INTO @DATA(ls_updated_member_tarif).
    IF sy-subrc <> 0.
      DATA  lv_string_value TYPE string.
      lv_string_value = iv_member_id.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>invalid_field_value
          invalid_field = 'Member ID'
          invalid_value = lv_string_value.
    ENDIF.

    " Updating the start date of memeber's tarif if the user wants
    IF is_updated_tarif-valid_from IS NOT INITIAL.
      check_tarif_start_update( iv_member_id  = ls_updated_member_tarif-member_id
                                iv_start_date = is_updated_tarif-valid_from ).
      ls_updated_member_tarif-valid_from = is_updated_tarif-valid_from.


    ENDIF.

    " Updating the end date of memeber's taif if the user wants
    IF is_updated_tarif-valid_to IS NOT INITIAL.

      " the new end date of the tarif must no be before the start date
      IF is_updated_tarif-valid_to < ls_updated_member_tarif-valid_from.
        RAISE EXCEPTION TYPE zcx_cm_member
          EXPORTING
            textid = zcx_cm_member=>invalid_end_date.
      ENDIF.

      SELECT SINGLE * FROM zme_cm_member WHERE id = @iv_member_id
          INTO @DATA(ls_member_data).
      IF is_updated_tarif-valid_to <= sy-datum.

        ls_member_data-account_status = 'I'.
        unassign_all_activities( iv_member_id ).
        UPDATE zme_cm_member FROM ls_member_data.
      ENDIF.

      ls_updated_member_tarif-valid_to = is_updated_tarif-valid_to.


    ENDIF.

    " Updating the tarif ID also if the user wants and the new tarif id is not also the old one
    IF is_updated_tarif-tarif_id IS NOT INITIAL.

*      " Getting the old tarif
      DATA(lv_old_tarif) = ls_updated_member_tarif-tarif_id.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_cm_member
          EXPORTING
            textid = zcx_cm_member=>member_is_not_active.
      ENDIF.

      IF is_updated_tarif-tarif_id <> lv_old_tarif.


        " Updating the family ID if the member changes his tarif from Family or to Family
        update_family_id( iv_member_id    = iv_member_id
                          iv_new_tarif_id = is_updated_tarif-tarif_id ).

        " Updating the end date of the old tarif to today
        end_tarif_valid_to( iv_member_id ).

        " Settig the new tarif id
        ls_updated_member_tarif-tarif_id = is_updated_tarif-tarif_id.

        " New Tarif's start day should be the first day of the next month
        set_new_date_up_tarif( IMPORTING ev_start_date = ls_updated_member_tarif-valid_from
                                         ev_end_date   = ls_updated_member_tarif-valid_to ).

        " Deleting any tarifs for this user having the same member ID, valid to and from dates.
        " The deleted tarif might be done by mistake by the user
        delete_mem_tarif_mistake( ls_updated_member_tarif ).

        " Inserting the new member tarif to the database
        INSERT zme_cm_mem_tarif FROM ls_updated_member_tarif.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_cm_member
            EXPORTING
              textid = zcx_cm_member=>update_cancelled.
        ENDIF.
      ELSE.
        " Updating only the other fields if the user did not change the tarif id
        UPDATE zme_cm_mem_tarif FROM ls_updated_member_tarif.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_cm_member
            EXPORTING
              textid = zcx_cm_member=>update_cancelled.
        ENDIF.
      ENDIF.

    ENDIF.

    rv_updated = abap_true.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_AC_STATUS_END_DATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACCOUNT_STATUS              TYPE        Z_ME_CM_M_STATUS
* | [--->] IV_NEW_TARIF_END_DATE          TYPE        Z_ME_CM_T_VALID_TO
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_ac_status_end_date.

    " The user can't make the status terminated and set a new end date after today
    IF iv_new_tarif_end_date IS NOT INITIAL.
      IF iv_account_status = 'I' AND iv_new_tarif_end_date > sy-datum.
        RAISE EXCEPTION TYPE zcx_cm_member
          EXPORTING
            textid = zcx_cm_member=>invalid_end_date_terminatd.
      ENDIF.

    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_MEMBER_TERMINATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [--->] IV_ACCOUNT_STATUS              TYPE        Z_ME_CM_M_STATUS
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_member_termination.

    IF iv_account_status = 'I'.
      unassign_all_activities( iv_member_id ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_TARIF_START_UPDATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_START_DATE                  TYPE        Z_ME_CM_T_VALID_TO
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_tarif_start_update.
    SELECT * FROM zme_cm_mem_tarif
      WHERE member_id = @iv_member_id AND valid_to >= @iv_start_date INTO TABLE @DATA(lt_end_dates).
    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid = zcx_cm_member=>invalid_start_date.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->DELETE_MEM_TARIF_MISTAKE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MEMBER_TARIF                TYPE        ZME_CM_MEM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD delete_mem_tarif_mistake.
    DELETE FROM zme_cm_mem_tarif WHERE member_id = is_member_tarif-member_id
                                   AND valid_from = is_member_tarif-valid_from.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->END_TARIF_VALID_TO
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD end_tarif_valid_to.

    SELECT SINGLE * FROM zme_cm_mem_tarif
      WHERE member_id = @iv_member_id AND valid_to >= @sy-datum
        INTO @DATA(ls_member_tarif).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid = zcx_cm_member=>member_is_not_active.
    ENDIF.
    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = sy-datum
      IMPORTING
        last_day_of_month = ls_member_tarif-valid_to.
    UPDATE zme_cm_mem_tarif FROM ls_member_tarif.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->SET_NEW_DATE_UP_TARIF
* +-------------------------------------------------------------------------------------------------+
* | [<---] EV_END_DATE                    TYPE        Z_ME_CM_T_VALID_TO
* | [<---] EV_START_DATE                  TYPE        Z_ME_CM_T_VALID_FROM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_new_date_up_tarif.

    " New start date will be the first day of the next month
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        months    = 1
        years     = 0
        days      = 0
      IMPORTING
        calc_date = ev_start_date.
    ev_start_date+6(2) = '01'.

    " Tarif will be valid to 50 years from the start day
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = ev_start_date
        days      = 0
        months    = 0
        years     = 50
      IMPORTING
        calc_date = ev_end_date.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->UNASSIGN_ALL_ACTIVITIES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD unassign_all_activities.


    SELECT a~* FROM zme_cm_activity AS a
      INNER JOIN zme_cm_mem_activ AS ma
        ON ma~activity_id = a~id
      WHERE ma~member_id = @iv_member_id
      AND ma~last_day IS INITIAL
      INTO TABLE @DATA(lt_activiy_ids).
    LOOP AT lt_activiy_ids ASSIGNING FIELD-SYMBOL(<fs_id>).
      <fs_id>-capacity += 1.
    ENDLOOP.

    MODIFY zme_cm_activity FROM TABLE lt_activiy_ids.

    UPDATE zme_cm_mem_activ SET last_day = @sy-datum
      WHERE member_id = @iv_member_id.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->UPDATE_STATUS_TARIF_DATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [--->] IV_ACCOUNT_STATUS              TYPE        Z_ME_CM_M_STATUS
* | [--->] IV_ACCOUNT_OLD_STATUS          TYPE        Z_ME_CM_M_STATUS
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_status_tarif_date.

    IF iv_account_old_status = iv_account_status OR iv_account_status IS INITIAL.
      RETURN.
    ENDIF.
    " Get latest member tarif
    SELECT *
     FROM zme_cm_mem_tarif
     WHERE member_id = @iv_member_id
     ORDER BY valid_from DESCENDING
     INTO TABLE @DATA(lt_member_tarif).
    IF sy-subrc <> 0 OR lt_member_tarif IS INITIAL.
      RETURN.
    ENDIF.
    READ TABLE lt_member_tarif INTO DATA(ls_member_tarif) INDEX 1.
    IF iv_account_old_status = 'I' AND iv_account_status = 'A'.
      IF ls_member_tarif-valid_to = sy-datum.
        extend_valid_to( ls_member_tarif ).
      ELSE.
        set_new_date_up_tarif(
          IMPORTING
            ev_start_date = ls_member_tarif-valid_from
            ev_end_date   = ls_member_tarif-valid_to ).
        INSERT zme_cm_mem_tarif FROM ls_member_tarif.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_cm_member
            EXPORTING
              textid = zcx_cm_member=>update_cancelled.
        ENDIF.
      ENDIF.
    ELSEIF iv_account_old_status = 'A' AND iv_account_status = 'I'.
      unassign_all_activities( iv_member_id ).
      end_tarif_valid_to( iv_member_id ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_cm_member
          EXPORTING
            textid = zcx_cm_member=>update_cancelled.
      ENDIF.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_FAMILY_ID_TARIF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FAMILY_ID                   TYPE        Z_ME_CM_M_FAMILY_ID
* | [--->] IV_TARIF_ID                    TYPE        Z_ME_CM_T_ID
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_family_id_tarif.

    "Checking if the user gave the right tarif ID when he writes a family ID
    IF iv_family_id IS NOT INITIAL.

      " Getting the tarif of the from an active member with the same family ID
      SELECT SINGLE MAX( tarif_id ) FROM zme_cm_mem_tarif
           WHERE member_id = ( SELECT MAX( member_id ) FROM zme_cm_family_id
                                WHERE family_id = @iv_family_id )

           INTO @DATA(lv_right_tarif_id).

      IF iv_tarif_id <> lv_right_tarif_id.
        DATA  lv_string_value TYPE string.
        lv_string_value = iv_tarif_id.
        RAISE EXCEPTION TYPE zcx_cm_member
          EXPORTING
            textid        = zcx_cm_member=>invalid_field_value
            invalid_field = 'Tarif'
            invalid_value = lv_string_value.
      ENDIF.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->EXTEND_VALID_TO
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_TARIF                       TYPE        ZME_CM_MEM_TARIF
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD extend_valid_to.
    DATA(ls_member_tarif) = is_tarif.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = 0
        months    = 0
        years     = 50
      IMPORTING
        calc_date = ls_member_tarif-valid_to.
    UPDATE zme_cm_mem_tarif FROM ls_member_tarif.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid = zcx_cm_member=>update_cancelled.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->CHECK_ACCOUNT_STATUS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_account_status.

    SELECT SINGLE account_status FROM zme_cm_member
      WHERE id = @iv_member_id INTO @DATA(lv_status).
    IF lv_status <> 'A'.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid = zcx_cm_member=>member_is_not_active.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_MEMBER_HANDLER->GET_MEMBER_CURRENT_TARIF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [<-()] RS_TARIF                       TYPE        ZME_CM_TARIFS
* | [!CX!] ZCX_CM_MEMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_member_current_tarif.
    check_member_id( iv_member_id ).

    " Getting the member tarif ID
    SELECT SINGLE tarif_id FROM zme_cm_mem_tarif
      WHERE member_id = @iv_member_id AND valid_from <= @sy-datum AND valid_to >= @sy-datum
      INTO @DATA(lv_tarif_id).
    IF sy-subrc <> 0.
      DATA  lv_string_value TYPE string.
      lv_string_value = iv_member_id.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>invalid_field_value
          invalid_field = 'Member ID'
          invalid_value = lv_string_value.
    ENDIF.

    " Getting the member's tarif
    SELECT SINGLE * FROM zme_cm_tarifs WHERE id = @lv_tarif_id
      INTO @rs_tarif.
    IF sy-subrc <> 0.
      DATA  lv_string_value1 TYPE string.
      lv_string_value1 = lv_tarif_id.
      RAISE EXCEPTION TYPE zcx_cm_member
        EXPORTING
          textid        = zcx_cm_member=>invalid_field_value
          invalid_field = 'Tarif ID'
          invalid_value = lv_string_value1.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_MEMBER_HANDLER->SET_ACTIVITY_LAST_DAY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MEM_ACTIVITY                TYPE        ZME_CM_MEM_ACTIV
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_activity_last_day.
    DATA(ls_mem_activity) = is_mem_activity.
    ls_mem_activity-last_day = sy-datum.
    UPDATE zme_cm_mem_activ FROM  ls_mem_activity.
  ENDMETHOD.
ENDCLASS.
