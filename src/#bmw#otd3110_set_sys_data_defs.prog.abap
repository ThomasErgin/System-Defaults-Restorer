*&---------------------------------------------------------------------*
*& Include          /BMW/OTD3110_SET_SYS_DATA_DEFS
*&---------------------------------------------------------------------*
* TITLE          : Set system defaults
* AUTHOR         : Madhuri Kalakonda (QXZ10LQ)
* DATE           : 25-06-2020
* DEVELOPMENT ID : OTD3110
* CHANGE REQUEST : CD 2000102391
* DESCRIPTION    : Declarations, types, constants and selection screen
************************************************************************
* CHANGE HISTORY LOG
*-----------------------------------------------------------------------
* MOD. NO. | DATE       | NAME    | TR NUMBER  | CHANGE REFERENCE
*-----------------------------------------------------------------------

TABLES: usr02.

TYPES:
  BEGIN OF ty_client,
    cccategory TYPE t000-cccategory,
    cccoractiv TYPE t000-cccoractiv,
    ccnocliind TYPE t000-ccnocliind,
    ccimaildis TYPE t000-ccimaildis,
  END OF ty_client,

  BEGIN OF ty_block_inform_on_errors,
    block_priority  TYPE satc_ci_transport_setting,
    inform_priority TYPE satc_ci_transport_setting,
  END OF ty_block_inform_on_errors,

  ty_software_components TYPE RANGE OF dlv_systc-dlvunit.

CONSTANTS:
  transport_changes_allowed TYPE t000-cccoractiv VALUE '1',
  customizing_not_allowed   TYPE t000-cccoractiv VALUE '2',
  changes_allowed           TYPE t000-ccnocliind VALUE ' ',
  ecatt_allowed             TYPE t000-ccimaildis VALUE 'X',
  ecatt_not_allowed         TYPE t000-ccimaildis VALUE ' ',
  no_crossclient_changes    TYPE t000-ccnocliind VALUE '3',
  not_modifiable_indicator  TYPE trchange VALUE 'N',
  modifiable                TYPE trchange VALUE 'X',
  restricted_modifiable     TYPE trchange VALUE 'R',
  system_modifiable         TYPE trchange VALUE 'F'.

SELECTION-SCREEN:BEGIN OF BLOCK client_settings WITH FRAME TITLE TEXT-cs1. "Client settings
  PARAMETERS:
    p_clnset TYPE abap_bool DEFAULT abap_true AS CHECKBOX USER-COMMAND toggle,
    p_role   TYPE t000-cccategory OBLIGATORY USER-COMMAND client_role AS LISTBOX VISIBLE LENGTH 20 MODIF ID cln,
    p_chg_tr TYPE t000-cccoractiv AS LISTBOX VISIBLE LENGTH 80 MODIF ID cln,
    p_objchg TYPE t000-ccnocliind AS LISTBOX VISIBLE LENGTH 60 MODIF ID cln,
    p_ecatt  TYPE t000-ccimaildis AS LISTBOX VISIBLE LENGTH 60 MODIF ID cln.
SELECTION-SCREEN:END OF BLOCK client_settings.

SELECTION-SCREEN:BEGIN OF BLOCK system_settings WITH FRAME TITLE TEXT-ss1. "System settings
  PARAMETERS:
    p_sysset TYPE abap_bool DEFAULT abap_false AS CHECKBOX USER-COMMAND toggle,
    p_glset  TYPE trchange  AS LISTBOX VISIBLE LENGTH 20 MODIF ID sys.
SELECTION-SCREEN:END OF BLOCK system_settings.

SELECTION-SCREEN:BEGIN OF BLOCK user_settings WITH FRAME TITLE TEXT-ups. "User profile settings
  PARAMETERS:
    p_usrset TYPE abap_bool DEFAULT abap_false AS CHECKBOX USER-COMMAND toggle.
  SELECT-OPTIONS:
    s_user   FOR usr02-bname MODIF ID usr.
SELECTION-SCREEN:END OF BLOCK user_settings.

SELECTION-SCREEN:BEGIN OF BLOCK atc_settings WITH FRAME TITLE TEXT-atc. "ATC settings
  PARAMETERS:
    p_atcset TYPE abap_bool DEFAULT abap_false AS CHECKBOX USER-COMMAND toggle.
  PARAMETERS:
    p_gcvar TYPE satc_ci_chk_variant MATCHCODE OBJECT satc_ci_chkvar_f4 MODIF ID atc.
  SELECTION-SCREEN:BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-bls.
    PARAMETERS:
      p_never  TYPE satc_ci_admin_setup_flags RADIOBUTTON GROUP bs MODIF ID a1,
      p_pri1   TYPE satc_ci_admin_setup_flags RADIOBUTTON GROUP bs MODIF ID a1,
      p_pri12  TYPE satc_ci_admin_setup_flags RADIOBUTTON GROUP bs DEFAULT 'X' MODIF ID a1,
      p_pri123 TYPE satc_ci_admin_setup_flags RADIOBUTTON GROUP bs MODIF ID a1.
  SELECTION-SCREEN:END OF BLOCK b5.
  SELECTION-SCREEN:BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-obc.
    PARAMETERS:
      p_req  TYPE c AS CHECKBOX DEFAULT 'X' MODIF ID a2,
      p_task TYPE c AS CHECKBOX DEFAULT 'X' MODIF ID a2,
      p_toc  TYPE c AS CHECKBOX MODIF ID a2.
  SELECTION-SCREEN:END OF BLOCK b6.
SELECTION-SCREEN:END OF BLOCK atc_settings.
PARAMETERS:
    p_test TYPE test_x AS CHECKBOX DEFAULT abap_true.
