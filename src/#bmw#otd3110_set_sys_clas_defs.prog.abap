*&---------------------------------------------------------------------*
*& Include          /BMW/OTD3110_SET_SYS_CLAS_DEFS
*&---------------------------------------------------------------------*
* TITLE          : Set system defaults
* AUTHOR         : Madhuri Kalakonda (QXZ10LQ)
* DATE           : 25-06-2020
* DEVELOPMENT ID : OTD3110
* CHANGE REQUEST : CD 2000102391
* DESCRIPTION    : Class Definition
*
************************************************************************
* CHANGE HISTORY LOG
*-----------------------------------------------------------------------
* MOD. NO. | DATE       | NAME    | TR NUMBER  | CHANGE REFERENCE
*-----------------------------------------------------------------------

CLASS system_defaults_setter DEFINITION.

  PUBLIC SECTION.

    CONSTANTS:
      sap_all                    TYPE xuprofile VALUE 'SAP_ALL',
      sap_new                    TYPE xuprofile VALUE 'SAP_NEW',

      transport_id               TYPE wbosyscust-id VALUE 'WBO',

      client_role_demo           TYPE t000-cccategory VALUE 'D',
      client_role_test           TYPE t000-cccategory VALUE 'T',
      client_role_prod           TYPE t000-cccategory VALUE 'P',
      client_role_cust           TYPE t000-cccategory VALUE 'C',
      customizing_not_changeable TYPE t000-cccoractiv VALUE '2',
      no_changes_allowed         TYPE t000-ccnocliind VALUE '2',

      req                        TYPE c VALUE ' ',
      task_x                     TYPE c VALUE 'X',
      toc_t                      TYPE c VALUE 'T',
      req_task                   TYPE c VALUE 'B',
      req_toc                    TYPE c VALUE 'R',
      task_toc                   TYPE c VALUE 'Y',
      req_task_toc               TYPE c VALUE 'A',
      never                      TYPE c VALUE '0',
      on_prio1                   TYPE c VALUE '1',
      on_prio12                  TYPE c VALUE '2',
      on_prio123                 TYPE c VALUE '3',
      feature_not_available      TYPE c VALUE '-'.

    METHODS:
      constructor
        IMPORTING enable_errors TYPE abap_bool OPTIONAL,
      modify_selection_screen,
      set_screen_defaults_on_sel,
      validate_screen_input,
      main,
      display_log.

  PROTECTED SECTION.

    METHODS:
      check_user_authority.

    DATA:
      client_id                    TYPE tadir-pgmid VALUE 'HEAD',
      client_type                  TYPE tadir-object VALUE 'SYST',
      current_client_settings      TYPE ty_client,
      log_table                    TYPE /bmw/otd3110_output_tt,
      current_global_check_variant TYPE satc_ci_chk_variant,
      atc_setting_exists           TYPE abap_bool,
      current_tms_setting          TYPE char1.


  PRIVATE SECTION.

    DATA:
      inform_priority TYPE satc_ci_transport_setting,
      leave_on_errors TYPE abap_bool.

    METHODS:
      add_atc_db_lock,
      remove_db_locks,
      add_client_db_locks
        IMPORTING tablename TYPE rstable-tabname,
      restore_client_settings,
      restore_system_settings,
      restore_user_profile,
      restore_atc_settings,
      display_log_as_text,
      display_log_as_alv_grid,
      update_client
        RETURNING VALUE(client_updated) TYPE abap_bool,
      update_system
        IMPORTING requested_client_setting TYPE tadir-edtflag
        RETURNING VALUE(system_updated)    TYPE abap_bool,
      update_user
        IMPORTING user_name           TYPE ust04-bname
                  user_profiles       TYPE suid_tt_bapiprof
        RETURNING VALUE(user_updated) TYPE abap_bool,
      update_atc
        IMPORTING
                  current_atc_setting   TYPE char1
                  requested_tms_setting TYPE char1
                  requested_atc_setting TYPE char1
        RETURNING VALUE(atc_updated)    TYPE abap_bool,
      update_global_check_variant,
      update_atc_setting
        IMPORTING
          current_atc_setting   TYPE satc_ci_transport_setting
          requested_atc_setting TYPE satc_ci_transport_setting
          inform_priority       TYPE satc_ci_transport_setting,
      update_object_chks_on_release
        IMPORTING requested_tms_setting TYPE char1,
      get_current_atc_setting RETURNING VALUE(current_atc_setting) TYPE satc_ci_transport_setting,
      get_requested_atc_setting RETURNING VALUE(requested_atc_setting) TYPE satc_ci_transport_setting,
      get_descr_of_atc_setting
        IMPORTING atc_setting                 TYPE satc_ci_transport_setting
        RETURNING VALUE(descr_of_atc_setting) TYPE string,
      get_old_and_new_tms_setting
        RETURNING VALUE(requested_tms_setting) TYPE char1,
      get_descr_of_tms_setting
        IMPORTING tms_setting                 TYPE char1
        RETURNING VALUE(descr_of_tms_setting) TYPE string,
      raise_error_message.
    "IMPORTING leave_on_errors TYPE abap_bool OPTIONAL.

ENDCLASS.
