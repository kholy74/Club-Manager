CLASS zcl_me_cm_tarif_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS calculate_tarif_price
      IMPORTING
        !iv_member_id         TYPE z_me_cm_m_id
        !iv_date              TYPE z_me_cm_t_valid_to
      RETURNING
        VALUE(rv_total_price) TYPE z_me_cm_t_basis_price
      RAISING
        zcx_cm_tarif .
    METHODS update_tarif
      IMPORTING
        !iv_tarif_id      TYPE z_me_cm_t_id
        !is_updated_tarif TYPE zme_cm_tarifs
      RETURNING
        VALUE(rv_updated) TYPE abap_bool
      RAISING
        zcx_cm_tarif .
    METHODS create_tarif
      IMPORTING
        !is_updated_tarif TYPE zme_cm_tarifs
      RETURNING
        VALUE(rv_added)   TYPE abap_bool
      RAISING
        zcx_cm_tarif .
    METHODS get_all_tarifs
      RETURNING
        VALUE(rt_tarifs) TYPE z_cm_tarifs .
    METHODS get_tarif
      IMPORTING
        !iv_tarif_id    TYPE z_me_cm_t_id
      RETURNING
        VALUE(rs_tarif) TYPE zme_cm_tarifs
      RAISING
        zcx_cm_tarif .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS calculate_family_tarif
      IMPORTING
        !iv_date              TYPE z_me_cm_t_valid_to
        !iv_tarif_id          TYPE z_me_cm_t_id
        !iv_member_id         TYPE z_me_cm_m_id
      RETURNING
        VALUE(rv_tarif_price) TYPE z_me_cm_t_basis_price
      RAISING
        zcx_cm_tarif .
    METHODS calculate_basis_price
      IMPORTING
        !iv_date              TYPE z_me_cm_t_valid_to
        !is_tarif             TYPE zme_cm_tarifs
        !iv_member_id         TYPE z_me_cm_m_id
      RETURNING
        VALUE(rv_tarif_price) TYPE z_me_cm_t_basis_price
      RAISING
        zcx_cm_tarif .
    METHODS calculate_extra_activities
      IMPORTING
        !iv_date              TYPE z_me_cm_t_valid_to
        !iv_member_id         TYPE z_me_cm_m_id
      RETURNING
        VALUE(rv_tarif_price) TYPE z_me_cm_t_basis_price .
    METHODS calculate_age_discount
      IMPORTING
        !iv_member_id         TYPE z_me_cm_m_id
        !iv_current_sum_price TYPE z_me_cm_t_basis_price
        !iv_discount          TYPE z_me_cm_t_discount_age
        iv_date               TYPE z_me_cm_t_valid_to
      RETURNING
        VALUE(rv_discount)    TYPE z_me_cm_t_basis_price
      RAISING
        zcx_cm_tarif .
    METHODS check_member_id
      IMPORTING
        !iv_member_id TYPE z_me_cm_m_id
      RAISING
        zcx_cm_tarif .
    METHODS check_tarif_name
      IMPORTING
        !iv_tarif_name TYPE z_me_cm_t_name
      RAISING
        zcx_cm_tarif .
    METHODS check_basis_price
      IMPORTING
        !iv_price TYPE z_me_cm_t_basis_price
      RAISING
        zcx_cm_tarif .
    METHODS check_family_based_values
      IMPORTING
        !iv_is_family_tarif TYPE z_me_cm_t_family
        !iv_family_discount TYPE z_me_cm_t_discount_age
      RAISING
        zcx_cm_tarif .
    METHODS check_age_discount
      IMPORTING
        !iv_age_discount TYPE z_me_cm_t_discount_age
      RAISING
        zcx_cm_tarif .
    METHODS create_tarif_id
      RETURNING
        VALUE(rv_tarif_id) TYPE z_me_cm_t_id
      RAISING
        zcx_cm_tarif .
    METHODS check_updated_fields
      IMPORTING
        !iv_is_family_tarif      TYPE z_me_cm_t_family
        !is_updated_tarif_fields TYPE zme_cm_tarifs
        !iv_tarif_id             TYPE z_me_cm_t_id
      RAISING
        zcx_cm_tarif .
    METHODS check_updated_name
      IMPORTING
        !iv_tarif_name TYPE z_me_cm_t_name
        !iv_tarif_id   TYPE z_me_cm_t_id
      RAISING
        zcx_cm_tarif .
ENDCLASS.



CLASS ZCL_ME_CM_TARIF_HANDLER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_TARIF_HANDLER->CALCULATE_AGE_DISCOUNT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [--->] IV_CURRENT_SUM_PRICE           TYPE        Z_ME_CM_T_BASIS_PRICE
* | [--->] IV_DISCOUNT                    TYPE        Z_ME_CM_T_DISCOUNT_AGE
* | [--->] IV_DATE                        TYPE        Z_ME_CM_T_VALID_TO
* | [<-()] RV_DISCOUNT                    TYPE        Z_ME_CM_T_BASIS_PRICE
* | [!CX!] ZCX_CM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD calculate_age_discount.

    SELECT SINGLE birthday FROM zme_cm_member WHERE id = @iv_member_id INTO @DATA(lv_birthday).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_tarif
        EXPORTING
          textid        = zcx_cm_tarif=>value_can_not_be_found
          missing_value = 'Birthday'.
    ENDIF.

    " Calculating the age of the member on the mentioned date iv_date
    DATA lv_age TYPE cmp_noyrs.
    CALL FUNCTION 'HRCM_TIME_PERIOD_CALCULATE'
      EXPORTING
        begda         = lv_birthday
        endda         = iv_date
      IMPORTING
        noyrs         = lv_age
      EXCEPTIONS
        invalid_dates = 1
        overflow      = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_tarif
        EXPORTING
          textid = zcx_cm_tarif=>age_calculating_error.
    ENDIF.

    " The members above the 70 and below the 15 get the discount
    IF lv_age < 15 OR lv_age > 70.
      rv_discount =  iv_current_sum_price * ( iv_discount / 100 ).

    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_TARIF_HANDLER->CALCULATE_EXTRA_ACTIVITIES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE                        TYPE        Z_ME_CM_T_VALID_TO
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [<-()] RV_TARIF_PRICE                 TYPE        Z_ME_CM_T_BASIS_PRICE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD calculate_extra_activities.

    " Counting the activities assigned to the memeber
    SELECT COUNT( member_id ) FROM zme_cm_mem_activ
      WHERE member_id = @iv_member_id AND join_date < @iv_date AND ( last_day IS INITIAL OR last_day > @iv_date )
        INTO @DATA(lv_number_of_activities).
    " Adding 5 Euros for each new activity
    rv_tarif_price = COND #( WHEN lv_number_of_activities > 2
                             THEN ( lv_number_of_activities - 2 ) * 5
                             ELSE 0 ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_TARIF_HANDLER->CALCULATE_FAMILY_TARIF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE                        TYPE        Z_ME_CM_T_VALID_TO
* | [--->] IV_TARIF_ID                    TYPE        Z_ME_CM_T_ID
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [<-()] RV_TARIF_PRICE                 TYPE        Z_ME_CM_T_BASIS_PRICE
* | [!CX!] ZCX_CM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD calculate_family_tarif.

    " Getting the family ID
    SELECT SINGLE family_id FROM zme_cm_member INTO @DATA(lv_family_id) WHERE id = @iv_member_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_tarif
        EXPORTING
          textid = zcx_cm_tarif=>no_family_id_assigned.
    ENDIF.

    " Getting the tarif information
    SELECT SINGLE * FROM zme_cm_tarifs INTO @DATA(ls_tarif_data)
      WHERE id = @iv_tarif_id.

    " Counting the family member's who were active during the search date
    SELECT COUNT( family_id ) FROM zme_cm_member
      WHERE family_id = @lv_family_id AND id = ( SELECT MAX( member_id ) FROM zme_cm_mem_tarif WHERE valid_to > @iv_date AND valid_from <= @iv_date  ) INTO @DATA(lv_family_members).

    " Each family member will get the family discount, which is the number of the family members, which should be more than 1, minus one
    " multiplied by the family discount value
    IF lv_family_members > 1.
      DATA lv_family_discount TYPE z_me_cm_t_basis_price.
      lv_family_discount = ( lv_family_members - 1 ) * ls_tarif_data-family_discount.
      rv_tarif_price = ls_tarif_data-basis_price - lv_family_discount.

    ELSE.
      rv_tarif_price = ls_tarif_data-basis_price.
    ENDIF.


    IF rv_tarif_price < 0.
      rv_tarif_price = 0.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_TARIF_HANDLER->CALCULATE_TARIF_PRICE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [--->] IV_DATE                        TYPE        Z_ME_CM_T_VALID_TO
* | [<-()] RV_TOTAL_PRICE                 TYPE        Z_ME_CM_T_BASIS_PRICE
* | [!CX!] ZCX_CM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD calculate_tarif_price.

    DATA lv_sum_price TYPE z_me_cm_t_basis_price.

    " Getting the tarif ID, which was assigned to the member the mentioned ID on the mentioned date
    SELECT SINGLE tarif_id FROM zme_cm_mem_tarif
      WHERE member_id = @iv_member_id AND valid_to >= @iv_date AND valid_from <= @iv_date
      INTO @DATA(lv_tarif_id).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_tarif
        EXPORTING
          textid = zcx_cm_tarif=>inactive_tarif.
    ENDIF.

    DATA(ls_tarif) = get_tarif( lv_tarif_id ).

    " Checking the validity of the member ID
    check_member_id( iv_member_id ).

    " Getting the basis price based on the tarif ID
    lv_sum_price += calculate_basis_price( is_tarif     = ls_tarif
                                           iv_member_id = iv_member_id
                                           iv_date      = iv_date ).

    " Getting the cost of the extra activities if the member has more than 2 activities
    lv_sum_price += calculate_extra_activities( iv_member_id = iv_member_id
                                                iv_date      = iv_date ).

    " A discount for members under 15 or over 70 years old
    DATA(lv_age_discount) = calculate_age_discount( iv_member_id         = iv_member_id
                                                    iv_current_sum_price = lv_sum_price
                                                    iv_discount          = ls_tarif-age_discount
                                                    iv_date              = iv_date ).
    lv_sum_price -= lv_age_discount.
    rv_total_price = lv_sum_price.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_TARIF_HANDLER->CREATE_TARIF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_UPDATED_TARIF               TYPE        ZME_CM_TARIFS
* | [<-()] RV_ADDED                       TYPE        ABAP_BOOL
* | [!CX!] ZCX_CM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_tarif.
    rv_added = abap_false.

    DATA(ls_new_tarif) = is_updated_tarif.
    check_tarif_name( ls_new_tarif-name ).
    check_basis_price( ls_new_tarif-basis_price ).
    check_family_based_values( iv_is_family_tarif = ls_new_tarif-family
                               iv_family_discount = ls_new_tarif-family_discount ).
    check_age_discount( iv_age_discount = ls_new_tarif-age_discount ).
    ls_new_tarif-id = create_tarif_id( ).

    rv_added = abap_true.
    INSERT zme_cm_tarifs FROM ls_new_tarif.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_TARIF_HANDLER->UPDATE_TARIF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TARIF_ID                    TYPE        Z_ME_CM_T_ID
* | [--->] IS_UPDATED_TARIF               TYPE        ZME_CM_TARIFS
* | [<-()] RV_UPDATED                     TYPE        ABAP_BOOL
* | [!CX!] ZCX_CM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_tarif.

    rv_updated = abap_false.
    " Checking the validity of the tarif ID
    SELECT SINGLE * FROM zme_cm_tarifs WHERE id = @iv_tarif_id
      INTO @DATA(ls_tarif_data).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_tarif
        EXPORTING
          textid = zcx_cm_tarif=>invalid_tarif_id.
    ENDIF.

    " Checking the validity of the updated fields if needed
    check_updated_fields( iv_is_family_tarif      = ls_tarif_data-family
                          is_updated_tarif_fields = is_updated_tarif
                          iv_tarif_id             = iv_tarif_id ).

    " Changing values the modified fields in the internal structure
    IF is_updated_tarif-name IS NOT INITIAL.
      ls_tarif_data-name = is_updated_tarif-name.
    ENDIF.

    IF is_updated_tarif-basis_price IS NOT INITIAL.
      ls_tarif_data-basis_price = is_updated_tarif-basis_price.
    ENDIF.

    IF is_updated_tarif-age_discount IS NOT INITIAL.
      ls_tarif_data-age_discount = is_updated_tarif-age_discount.
    ENDIF.

    " Changing the family discount only for family tarifs
    IF is_updated_tarif-family_discount IS NOT INITIAL.
      IF ls_tarif_data-family = abap_false.
        RAISE EXCEPTION TYPE zcx_cm_tarif
          EXPORTING
            textid = zcx_cm_tarif=>not_family_tarif.
      ELSE.
        ls_tarif_data-family_discount = is_updated_tarif-family_discount.
      ENDIF.
    ENDIF.
    " Adding the Tarif ID to the internal structure
    ls_tarif_data-id = iv_tarif_id.

    " Excuting the modification
    UPDATE zme_cm_tarifs FROM ls_tarif_data.
    rv_updated = abap_true.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_TARIF_HANDLER->CALCULATE_BASIS_PRICE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE                        TYPE        Z_ME_CM_T_VALID_TO
* | [--->] IS_TARIF                       TYPE        ZME_CM_TARIFS
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [<-()] RV_TARIF_PRICE                 TYPE        Z_ME_CM_T_BASIS_PRICE
* | [!CX!] ZCX_CM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD calculate_basis_price.

    IF is_tarif-family IS NOT INITIAL.
      rv_tarif_price = calculate_family_tarif( iv_member_id = iv_member_id
                                               iv_tarif_id  = is_tarif-id
                                               iv_date      = iv_date ).
    ELSE.
      rv_tarif_price = is_tarif-basis_price.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_TARIF_HANDLER->CHECK_AGE_DISCOUNT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_AGE_DISCOUNT                TYPE        Z_ME_CM_T_DISCOUNT_AGE
* | [!CX!] ZCX_CM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_age_discount.

    IF iv_age_discount IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_tarif
        EXPORTING
          textid        = zcx_cm_tarif=>missing_field
          missing_value = 'Age Discount'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_TARIF_HANDLER->CHECK_BASIS_PRICE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PRICE                       TYPE        Z_ME_CM_T_BASIS_PRICE
* | [!CX!] ZCX_CM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_basis_price.

    " The name must have a value
    IF iv_price IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_tarif
        EXPORTING
          textid        = zcx_cm_tarif=>missing_field
          missing_value = 'Basis Price'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_TARIF_HANDLER->CHECK_FAMILY_BASED_VALUES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_IS_FAMILY_TARIF             TYPE        Z_ME_CM_T_FAMILY
* | [--->] IV_FAMILY_DISCOUNT             TYPE        Z_ME_CM_T_DISCOUNT_AGE
* | [!CX!] ZCX_CM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_family_based_values.

    IF iv_is_family_tarif IS INITIAL AND iv_family_discount IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_tarif
        EXPORTING
          textid = zcx_cm_tarif=>not_family_tarif.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_TARIF_HANDLER->CHECK_MEMBER_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MEMBER_ID                   TYPE        Z_ME_CM_M_ID
* | [!CX!] ZCX_CM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_member_id.

    " Checking the validity of the member ID
    SELECT SINGLE id FROM zme_cm_member WHERE id = @iv_member_id
      INTO @DATA(lv_member_id).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_tarif
        EXPORTING
          textid = zcx_cm_tarif=>invalid_member_id.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_TARIF_HANDLER->CHECK_TARIF_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TARIF_NAME                  TYPE        Z_ME_CM_T_NAME
* | [!CX!] ZCX_CM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_tarif_name.

    " The name must have a value
    IF iv_tarif_name IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_tarif
        EXPORTING
          textid        = zcx_cm_tarif=>missing_field
          missing_value = 'Tarif Name'.
    ENDIF.

    " The user must enter a new tarif name
    SELECT SINGLE name FROM zme_cm_tarifs WHERE name = @iv_tarif_name
      INTO @DATA(lv_tarif_name).
    IF lv_tarif_name IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_cm_tarif
        EXPORTING
          textid     = zcx_cm_tarif=>tarif_name_already_exists
          tarif_name = iv_tarif_name.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_TARIF_HANDLER->CHECK_UPDATED_FIELDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_IS_FAMILY_TARIF             TYPE        Z_ME_CM_T_FAMILY
* | [--->] IS_UPDATED_TARIF_FIELDS        TYPE        ZME_CM_TARIFS
* | [--->] IV_TARIF_ID                    TYPE        Z_ME_CM_T_ID
* | [!CX!] ZCX_CM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_updated_fields.

    check_updated_name( iv_tarif_id   = iv_tarif_id
                        iv_tarif_name = is_updated_tarif_fields-name ).

    check_family_based_values( iv_is_family_tarif = iv_is_family_tarif
                               iv_family_discount = is_updated_tarif_fields-family_discount ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_TARIF_HANDLER->CHECK_UPDATED_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TARIF_NAME                  TYPE        Z_ME_CM_T_NAME
* | [--->] IV_TARIF_ID                    TYPE        Z_ME_CM_T_ID
* | [!CX!] ZCX_CM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_updated_name.


    IF iv_tarif_name IS NOT INITIAL.

      " Stopping the method if the user entered the sam value
      SELECT SINGLE name FROM zme_cm_tarifs WHERE id = @iv_tarif_id INTO @DATA(lv_current_name).
      IF lv_current_name = iv_tarif_name.
        RETURN.
      ENDIF.

      " The name must not match with any others
      SELECT SINGLE name FROM zme_cm_tarifs WHERE name = @iv_tarif_name INTO @DATA(lv_new_name).
      IF lv_new_name IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_cm_tarif
          EXPORTING
            textid = zcx_cm_tarif=>tarif_name_already_exists.
      ENDIF.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ME_CM_TARIF_HANDLER->CREATE_TARIF_ID
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_TARIF_ID                    TYPE        Z_ME_CM_T_ID
* | [!CX!] ZCX_CM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_tarif_id.

    " Creating a new member ID which will be the previous ID + 1 using the Number Range Object
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '1'          " Nummernkreisnummer
        object      = 'Z_ME_CM_TA' " Nummernkreisobjekt
        quantity    = 1             " Anzahl der Nummern
      IMPORTING
        number      = rv_tarif_id
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_tarif
        EXPORTING
          textid = zcx_cm_tarif=>tarif_id_limit_reached.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_TARIF_HANDLER->GET_ALL_TARIFS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_TARIFS                      TYPE        Z_CM_TARIFS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_all_tarifs.

    SELECT * FROM zme_cm_tarifs INTO TABLE @rt_tarifs ORDER BY id.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_TARIF_HANDLER->GET_TARIF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TARIF_ID                    TYPE        Z_ME_CM_T_ID
* | [<-()] RS_TARIF                       TYPE        ZME_CM_TARIFS
* | [!CX!] ZCX_CM_TARIF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_tarif.

    " Checking the validity of the tarif
    SELECT SINGLE * FROM zme_cm_tarifs INTO  @rs_tarif
      WHERE id  = @iv_tarif_id .
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cm_tarif
        EXPORTING
          textid = zcx_cm_tarif=>tarif_does_not_exist.
    ENDIF.



  ENDMETHOD.
ENDCLASS.
