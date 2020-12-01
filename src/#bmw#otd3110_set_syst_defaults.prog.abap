*&---------------------------------------------------------------------*
*& Report /BMW/OTD3110_SET_SYST_DEFAULTS :-)
*&---------------------------------------------------------------------*
* TITLE          : Set system defaults
* AUTHOR         : Madhuri Kalakonda (QXZ10LQ)
* DATE           : 25-06-2020
* DEVELOPMENT ID : OTD3110
* CHANGE REQUEST : CD 2000102391
* DESCRIPTION    : periodically restore system specific default settings
************************************************************************
* CHANGE HISTORY LOG
*-----------------------------------------------------------------------
* MOD. NO. | DATE       | NAME    | TR NUMBER  | CHANGE REFERENCE
* 00001    | 12.08.2020 | Madhuri Kalakonda | PRDK902578 | 2000105337
*-----------------------------------------------------------------------
REPORT /bmw/otd3110_set_syst_defaults MESSAGE-ID /bmw/otd3110_set_sys.

INCLUDE:
  /bmw/otd3110_set_sys_data_defs,  "Data declarations & selection screen
  /bmw/otd3110_set_sys_clas_defs,  "Class definition
  /bmw/otd3110_set_sys_clas_impl,  "Class implementation
  /bmw/otd3110_set_sys_tst_class.  "ABAP unit test class

INITIALIZATION.
  DATA(defaults_setter) = NEW system_defaults_setter( enable_errors = abap_true ) ##NEEDED.

AT SELECTION-SCREEN OUTPUT.
  defaults_setter->modify_selection_screen( ).

AT SELECTION-SCREEN.
  IF sy-ucomm = 'CLIENT_ROLE'.
    defaults_setter->set_screen_defaults_on_sel( ).
  ENDIF.
  defaults_setter->validate_screen_input( ).

START-OF-SELECTION.
  defaults_setter->main( ).
  defaults_setter->display_log( ).
