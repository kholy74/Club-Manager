*&---------------------------------------------------------------------*
*& Include          Z_ME_FORM_ROUTINE
*&---------------------------------------------------------------------*



CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column ,
      on_double_click_activity FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,
      on_double_click_tarif FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_double_click.

    READ TABLE gt_members INDEX e_row-index INTO DATA(ls_member).

    IF sy-subrc = 0.

      gv_member_id = ls_member-id.

      CALL SCREEN 240.
      PERFORM get_all_member_data.
    ENDIF.

  ENDMETHOD.

  METHOD on_double_click_activity.

    READ TABLE gt_all_activities1 INDEX e_row-index INTO DATA(ls_activity).

    IF sy-subrc = 0.

      gv_activity_id = ls_activity-id.
      PERFORM  get_selected_activity.
      CALL SCREEN 310.

    ENDIF.

  ENDMETHOD.
  METHOD on_double_click_tarif.

    READ TABLE gt_tarifs1 INDEX e_row-index INTO DATA(ls_tarif).

    IF sy-subrc = 0.

      gv_tarif_id = ls_tarif-id.
      PERFORM  get_selected_tarif.
      CALL SCREEN 410.

    ENDIF.

  ENDMETHOD.

ENDCLASS.


FORM display_members.

  " Connecting the local container with the layout container
  CREATE OBJECT gr_container
    EXPORTING
      container_name = 'MEMBERS_ALV'.
  CREATE OBJECT gr_alv
    EXPORTING
      i_parent = gr_container.

  " Getting all memmbers
  SELECT * FROM zme_cm_member INTO TABLE gt_members ORDER BY id.

  " Display the list in the container
  CALL METHOD gr_alv->set_table_for_first_display
    EXPORTING
      i_structure_name = 'ZME_CM_MEMBER'
    CHANGING
      it_outtab        = gt_members.

  " Handler for double clicks
  SET HANDLER lcl_event_handler=>on_double_click FOR gr_alv.

ENDFORM.

FORM display_all_activities1.
  DATA(go_avtivity) = NEW zcl_me_cm_activity_handler( ).
  CREATE OBJECT gr_container_all_activities1
    EXPORTING
      container_name = 'ALL_ACTIVITIES1_ALV'.
  CREATE OBJECT gr_alv_all_activities1
    EXPORTING
      i_parent = gr_container_all_activities1.
  TRY.
      gt_all_activities1 = go_avtivity->get_all_activities( ).
    CATCH zcx_cm_activity INTO DATA(lr_ex).
      MESSAGE lr_ex TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
  " Display the list in the container
  CALL METHOD gr_alv_all_activities1->set_table_for_first_display
    EXPORTING
      i_structure_name = 'ZME_CM_ACTIVITY'
    CHANGING
      it_outtab        = gt_all_activities1.

  SET HANDLER lcl_event_handler=>on_double_click_activity FOR gr_alv_all_activities1.
ENDFORM.

FORM display_all_tarifs1.
  CREATE OBJECT gr_container_all_tarifs1
    EXPORTING
      container_name = 'ALL_TARIFS1_ALV'.
  CREATE OBJECT gr_alv_all_tarifs1
    EXPORTING
      i_parent = gr_container_all_tarifs1.
  TRY.
      CREATE OBJECT go_tarif.
      gt_tarifs1 = go_tarif->get_all_tarifs( ).
    CATCH zcx_cm_tarif INTO DATA(lr_ex).
      MESSAGE lr_ex TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
  " Display the list in the container
  CALL METHOD gr_alv_all_tarifs1->set_table_for_first_display
    EXPORTING
      i_structure_name = 'ZME_CM_TARIFS'
    CHANGING
      it_outtab        = gt_tarifs1.

  SET HANDLER lcl_event_handler=>on_double_click_tarif FOR gr_alv_all_tarifs1.
ENDFORM.
