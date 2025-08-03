CLASS zcl_me_cm_income_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF zsrt_member_full_profil,
        member_data  TYPE zme_cm_member,
        member_tarif TYPE zme_cm_tarifs,
        activities   TYPE z_cm_activities,
      END OF zsrt_member_full_profil .

    METHODS get_all_members
      RETURNING
        VALUE(rt_member_data) TYPE z_me_cm_all_members .
    METHODS get_monthly_income
      IMPORTING
        !iv_date                       TYPE z_me_cm_t_valid_from
      RETURNING
        VALUE(rv_total_monthly_income) TYPE z_me_cm_p_paid_amount .
    METHODS get_yearly_income
      RETURNING
        VALUE(rv_total_yearly_income) TYPE z_me_cm_p_paid_amount .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ME_CM_INCOME_HANDLER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_INCOME_HANDLER->GET_ALL_MEMBERS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_MEMBER_DATA                 TYPE        Z_ME_CM_ALL_MEMBERS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_all_members.
    SELECT  zme_cm_member~id FROM zme_cm_member INTO TABLE @DATA(lt_member_ids).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    DATA lt_member_full_data TYPE STANDARD TABLE OF zsrt_member_full_profil.
    DATA(lo_member_handler) = NEW zcl_me_cm_member_handler( ).

    SORT lt_member_ids BY id ASCENDING.

    LOOP AT lt_member_ids ASSIGNING FIELD-SYMBOL(<fs_member>).
      TRY.
          APPEND lo_member_handler->get_member_full_profil( <fs_member>-id ) TO lt_member_full_data.
        CATCH zcx_cm_member.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
    rt_member_data = lt_member_full_data.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_INCOME_HANDLER->GET_MONTHLY_INCOME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE                        TYPE        Z_ME_CM_T_VALID_FROM
* | [<-()] RV_TOTAL_MONTHLY_INCOME        TYPE        Z_ME_CM_P_PAID_AMOUNT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_monthly_income.

    DATA: lv_total_income TYPE z_me_cm_p_paid_amount,
          lv_member_price TYPE z_me_cm_p_paid_amount.

    " Creating sn internal list of all members
    SELECT * FROM zme_cm_member INTO TABLE @DATA(lt_members).
    LOOP AT lt_members INTO DATA(ls_member).
      TRY.
          " Claculating the end cost for the member on the mentioned date
          DATA(lo_tarif) = NEW zcl_me_cm_tarif_handler( ).
          lv_member_price = lo_tarif->calculate_tarif_price( iv_member_id = ls_member-id
                                                             iv_date      = iv_date ).
          lv_total_income += lv_member_price.
        CATCH zcx_cm_tarif.
          CONTINUE.
      ENDTRY.


    ENDLOOP.
    rv_total_monthly_income = lv_total_income.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ME_CM_INCOME_HANDLER->GET_YEARLY_INCOME
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_TOTAL_YEARLY_INCOME         TYPE        Z_ME_CM_P_PAID_AMOUNT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_yearly_income.
    DATA: lv_total_yearly_income TYPE z_me_cm_p_paid_amount,
          lv_current_year        TYPE numc4,
          lv_loop_month          TYPE numc2,
          lv_date                TYPE z_me_cm_t_valid_from,
          lv_monthly_income      TYPE z_me_cm_p_paid_amount.

    lv_current_year = sy-datum+0(4).

    " Getting the yearly income using the method get_monthly_income() by passing the first day of each month of this year
    " as a parameter
    DO 12 TIMES.
      lv_loop_month = sy-index.

      CONCATENATE lv_current_year lv_loop_month '01' INTO lv_date.

      TRY.
          lv_monthly_income = get_monthly_income( iv_date = lv_date ).
          lv_total_yearly_income += lv_monthly_income.
        CATCH zcx_cm_tarif.
          CONTINUE.
      ENDTRY.
    ENDDO.
    rv_total_yearly_income = lv_total_yearly_income.
  ENDMETHOD.
ENDCLASS.
