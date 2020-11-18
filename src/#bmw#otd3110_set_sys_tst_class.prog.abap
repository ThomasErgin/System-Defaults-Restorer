*&---------------------------------------------------------------------*
*& Include          /BMW/OTD3110_SET_SYS_TST_CLASS
*&---------------------------------------------------------------------*
CLASS set_syst_defaults_aut DEFINITION FOR TESTING FINAL
                            RISK LEVEL DANGEROUS
                            DURATION SHORT
                            INHERITING FROM system_defaults_setter.
  PUBLIC SECTION.
    CONSTANTS:
      test           TYPE pgmid VALUE 'TEST',
      usr1           TYPE pgmid VALUE 'USR1',
      test_user1     TYPE bname VALUE 'TEST_USER1',
      test_user2     TYPE bname VALUE 'TEST_USER2',
      global_variant TYPE satc_ci_chk_variant VALUE '/BMW/MINIMUM_CODE_REQUIREMENTS'.

  PRIVATE SECTION.
    CLASS-DATA:
      environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup.

    METHODS:
      setup,
      teardown,
      execute_main
        IMPORTING current_client_settings TYPE ty_client OPTIONAL
        RETURNING VALUE(return)           TYPE sy-subrc       ##NEEDED,

      system_setter_created FOR TESTING,
      scr_defaults_on_sel_demo FOR TESTING,
      scr_defaults_on_sel_cust FOR TESTING,
      scr_defaults_on_sel_prod FOR TESTING,
      scr_defaults_on_sel_test FOR TESTING,
      auth_on_clnset_validated FOR TESTING,
      auth_on_userset_validated FOR TESTING,
      auth_on_sysset_validated FOR TESTING,
      sel_scr_on_clnset_modified FOR TESTING,
      sel_scr_on_sysset_modified FOR TESTING,
      sel_scr_on_atcset_modified FOR TESTING,
      client_role_cust_in_testmode FOR TESTING,
      client_role_test_in_testmode FOR TESTING,
      client_sett_changed_updatemode FOR TESTING,
      system_sett_changed_testmode FOR TESTING,
      system_sett_changed_updatemode FOR TESTING,
      user_profile_changed FOR TESTING,
      atc_test_block_settings FOR TESTING,
      atc_test_object_release FOR TESTING,
      atc_test_global_variant FOR TESTING,
      atc_test_no_change FOR TESTING,
      atc_sett_changed_updatemode FOR TESTING.


ENDCLASS.

CLASS set_syst_defaults_aut IMPLEMENTATION.
  METHOD class_setup.
    environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'T000' )
                                                                                 ( 'TADIR' )
                                                                                 ( 'UST04' )
                                                                                 ( 'DLV_SYSTC' )
                                                                                 ( 'TRNSPACE' )
                                                                                 ( 'SCICHKV_HD' ) ) ).
  ENDMETHOD.

  METHOD setup.
    p_cltest = abap_true.
    p_sytest = abap_true.
    p_ustest = abap_true.

    "Set up the global system settings
    DATA(global_settings) = VALUE tt_tadir( ( pgmid = test object = test obj_name = space edtflag = space )
                                            ( pgmid = usr1 object = usr1 obj_name = usr1 edtflag = abap_true ) ).
    environment->insert_test_data( global_settings ).

    DATA: software_components TYPE STANDARD TABLE OF dlv_systc.
    "Set up software components
    software_components = VALUE #( ( dlvunit = test changeable = not_modifiable_indicator )
                                   ( dlvunit = usr1 changeable = not_modifiable_indicator ) ).
    environment->insert_test_data( software_components ).

    "Set up test users with SAP_ALL/SAP_NEW assigned
    DATA(super_users) = VALUE suid_tt_ust04( ( bname = test_user1 profile = sap_all )
                                             ( bname = test_user2 profile = sap_new ) ).
    environment->insert_test_data( super_users ).

  ENDMETHOD.

  METHOD teardown.
    " clean up test databases after module test
    environment->clear_doubles( ).

    CLEAR: p_cltest, p_sytest, p_ustest, p_clnset,
           p_role, p_chg_tr, p_objchg, p_sysset, p_glset, p_usrset.
  ENDMETHOD.

  METHOD system_setter_created.
    DATA(new_system_setter) = NEW system_defaults_setter( enable_errors = abap_true ) ##NEEDED.
    cl_abap_unit_assert=>assert_equals(
      act = current_client_settings-cccategory
      exp = p_role ).
  ENDMETHOD.

  METHOD scr_defaults_on_sel_demo.
    p_role = client_role_demo.
    sy-ucomm = 'CLIENT_ROLE'.
    set_screen_defaults_on_sel( ).
    cl_abap_unit_assert=>assert_equals(
      act = p_chg_tr
      exp = customizing_not_allowed ).
  ENDMETHOD.

  METHOD scr_defaults_on_sel_cust.
    p_role = client_role_cust.
    set_screen_defaults_on_sel( ).
    cl_abap_unit_assert=>assert_equals(
      act =  p_objchg
      exp = changes_allowed ).
  ENDMETHOD.

  METHOD scr_defaults_on_sel_test.
    p_role = client_role_test.
    set_screen_defaults_on_sel( ).
    cl_abap_unit_assert=>assert_equals(
      act =  p_ecatt
      exp = ecatt_allowed ).
  ENDMETHOD.

  METHOD scr_defaults_on_sel_prod.
    p_role = client_role_prod.
    set_screen_defaults_on_sel( ).
    cl_abap_unit_assert=>assert_equals(
      act =  p_ecatt
      exp = ecatt_not_allowed ).
  ENDMETHOD.

  METHOD sel_scr_on_atcset_modified.
    p_atcset = abap_true.
    modify_selection_screen( ).
    cl_abap_unit_assert=>assert_equals(
      act              = '/BMW/MINIMUM_CODE_REQUIREMENTS'
      exp              = p_gcvar ).
  ENDMETHOD.

  METHOD sel_scr_on_clnset_modified.
    p_clnset = abap_false.
    screen-group1 = 'CLN'.
    modify_selection_screen( ).
    LOOP AT SCREEN.
      CHECK screen-name CS 'P_ROLE'.
      cl_abap_unit_assert=>assert_equals(
        act              = screen-active
        exp              = '0' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD sel_scr_on_sysset_modified.
    p_sysset = abap_true.
    screen-group1 = 'SYS'.
    modify_selection_screen( ).
    LOOP AT SCREEN.
      CHECK screen-name CS 'P_GLSET'.
      cl_abap_unit_assert=>assert_equals(
        act              = screen-active
        exp              = '1' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD auth_on_clnset_validated.
    p_clnset = abap_true.   "change client settings
    p_cltest = abap_false.
    check_user_authority( ).
    cl_abap_unit_assert=>assert_equals(
      act   = '026'
      exp   = sy-msgno ).
  ENDMETHOD.

  METHOD auth_on_userset_validated.
    p_usrset = abap_true.   "change user settings
    p_ustest = abap_false.  "Test mode not active
    check_user_authority( ).
    cl_abap_unit_assert=>assert_equals(
      act   = '026'
      exp   = sy-msgno ).
  ENDMETHOD.

  METHOD auth_on_sysset_validated.
    p_sysset = abap_true.   "change system settings
    p_sytest = abap_false.  "Test mode not active
    check_user_authority( ).
    cl_abap_unit_assert=>assert_equals(
      act   = '026'
      exp   = sy-msgno ).
  ENDMETHOD.

  METHOD execute_main.
    main( ).
  ENDMETHOD.

  METHOD client_role_cust_in_testmode.
    "Test client settings when test mode is active
    p_clnset = abap_true.        "change client settings
    p_cltest = abap_true.        "Test mode is active
    p_role   = client_role_cust. "Client role: Customizing
    p_chg_tr = transport_changes_allowed. "Customizing in this client can be changed
    p_objchg = changes_allowed.   "changes to repository objects
    p_ecatt  = ecatt_allowed.     "ecatt allowed
    current_client_settings =  VALUE #( cccategory = client_role_cust
             cccoractiv = transport_changes_allowed
             ccnocliind = changes_allowed
             ccimaildis = ecatt_allowed ).
    execute_main( ).
    cl_abap_unit_assert=>assert_table_contains(
      line             = VALUE /bmw/otd3110_output( message = 'No client changes made'(ncm) )
      table            = log_table[] ).
  ENDMETHOD.

  METHOD client_role_test_in_testmode.
    "Test client settings when test mode is active
    p_clnset = abap_true.        "change client settings
    p_cltest = abap_true.        "Test mode is active
    p_role   = client_role_test. "Client role: Test
    p_chg_tr = customizing_not_changeable. "Customizing in this client cannot be changed
    p_objchg = no_changes_allowed.         "No changes to repository objects
    p_ecatt  = ecatt_allowed.     "ecatt allowed
    current_client_settings =  VALUE #( cccategory = client_role_cust
             cccoractiv = transport_changes_allowed
             ccnocliind = changes_allowed
             ccimaildis = ecatt_allowed ).
    execute_main( ).
    cl_abap_unit_assert=>assert_table_contains(
      line         = VALUE /bmw/otd3110_output(
        message    = 'Client settings changed from:'(csc)
        field_name = 'CCCATEGORY'(cat)
        current_value  = current_client_settings-cccategory
        requested_value  = p_role )
      table        = log_table[] ).

  ENDMETHOD.

  METHOD system_sett_changed_testmode.
    "given
    p_sysset = abap_true.     "change system settings
    p_sytest = abap_true.     "Test mode is active
    client_id = test.
    client_type = test.
    p_glset  = not_modifiable_indicator.         "Not modifiable
    "when
    execute_main( ).
    "then
    cl_abap_unit_assert=>assert_table_contains(
      line             = VALUE /bmw/otd3110_output(
        message    = 'Global settings changed from:'(gsc)
        field_name = 'EDTFLAG'(edt)
        current_value  = 'Modifiable'(mod)
        requested_value  = 'Not-Modifiable'(nmd)  )
      table        = log_table[] ).

    "given
    p_sysset = abap_true.     "change system settings
    p_sytest = abap_true.     "Test mode is active
    client_id = test.
    client_type = test.
    p_glset  = modifiable.         "modifiable
    "when
    execute_main( ).
    "then
    cl_abap_unit_assert=>assert_table_contains(
      line             = VALUE /bmw/otd3110_output( message = 'No system changes made'(nsm) )
      table            = log_table[] ).

  ENDMETHOD.

  METHOD user_profile_changed.
    "given
    p_usrset = abap_true.       "change user settings
    p_ustest = abap_true.       "Test mode is active
    s_user = VALUE #( sign = 'I' option = 'EQ' low = test_user1 ).
    "when
    execute_main( ).
    "then
    cl_abap_unit_assert=>assert_table_contains(
      line             = VALUE /bmw/otd3110_output(
        message = 'SAP_ALL/SAP_NEW removed from user'(sru)
        field_name = test_user1
        current_value = 'SAP_ALL/SAP_NEW'(prf)
        requested_value = space  )
      table        = log_table[] ).


    "given
    p_usrset = abap_true.       "change user settings
    p_ustest = abap_false.      "Test mode is not active
    s_user = VALUE #( sign = 'I' option = 'EQ' low = test_user2 ).
    "When, then
    cl_abap_unit_assert=>assert_initial( act = execute_main( ) ).

  ENDMETHOD.

  METHOD client_sett_changed_updatemode.
    "Test client settings when test mode is not active
    p_clnset = abap_true.        "change client settings
    p_cltest = abap_false.        "Test mode is not active
    p_role   = client_role_test. "Client role: Test
    p_chg_tr = customizing_not_changeable. "Customizing in this client cannot be changed
    p_objchg = no_changes_allowed.         "No changes to repository objects
    p_ecatt  = ecatt_allowed.
    current_client_settings =  VALUE #( cccategory = client_role_cust
             cccoractiv = transport_changes_allowed
             ccnocliind = changes_allowed
             ccimaildis = ecatt_allowed ).
    execute_main( ).
    cl_abap_unit_assert=>assert_table_contains(
      line             = VALUE /bmw/otd3110_output( message = 'Error updating client settings'(euc) )
      table            = log_table[] ).
  ENDMETHOD.

  METHOD system_sett_changed_updatemode.
    p_sysset = abap_true.       "change system settings
    p_sytest = abap_false.      "Test mode is not active
    client_id = test.
    client_type = test.
    p_glset  = not_modifiable_indicator.  "Not modifiable
    execute_main( ).
    cl_abap_unit_assert=>assert_table_contains(
      line             = VALUE /bmw/otd3110_output( message = 'Error updating namespaces'(eun) )
      table            = log_table[] ).
  ENDMETHOD.

  METHOD atc_test_no_change.
    p_atcset = abap_true.     "change ATC settings
    p_atctst = abap_true.     "Test mode is active
    current_global_check_variant = global_variant.
    p_gcvar  = global_variant.  "Global variant
    p_pri12  = abap_true.     "Priority 12
    p_pri123  = abap_false.
    p_req = abap_true.
    p_task = abap_true.
    p_toc  = abap_false.
    atc_setting_exists = abap_true.
    current_tms_setting = req_task.
    execute_main( ).
    cl_abap_unit_assert=>assert_table_contains(
      line             = VALUE /bmw/otd3110_output(
        message    = 'No client changes made'(ncm) )
      table        = log_table[] ).
  ENDMETHOD.

  METHOD atc_test_block_settings.
    p_atcset = abap_true.     "change ATC settings
    p_atctst = abap_true.     "Test mode is active
    p_pri12  = abap_false.    "Priority 12
    p_pri123 = abap_true.     "Any priority
    atc_setting_exists = abap_true.
    execute_main( ).
    cl_abap_unit_assert=>assert_table_contains(
      line             = VALUE /bmw/otd3110_output(
        message    = 'ATC settings changed from:'(bsc)
        current_value  = 'Priority 1,2'(p12)
        requested_value  = 'Priority 1,2,3'(p13)  )
      table        = log_table[] ).
  ENDMETHOD.

  METHOD atc_test_object_release.
    p_atcset = abap_true.     "change ATC settings
    p_atctst = abap_true.     "Test mode is active
    p_toc    = abap_true.     "TOC
    atc_setting_exists = abap_true.
    current_tms_setting = req_task.
    execute_main( ).
    cl_abap_unit_assert=>assert_table_contains(
      line             = VALUE /bmw/otd3110_output(
        message    = 'Object checks at request/task release changed from:'(ocr)
        current_value  = 'Request, Task'(rat)
        requested_value  = 'Request, Task, TOC'(rtt)  )
      table        = log_table[] ).
  ENDMETHOD.

  METHOD atc_test_global_variant.
    p_atcset = abap_true.     "change ATC settings
    p_atctst = abap_true.     "Test mode is active
    p_gcvar  = test.          "Global variant
    execute_main( ).
    cl_abap_unit_assert=>assert_table_contains(
      line             = VALUE /bmw/otd3110_output(
        message    = 'Gobal check variant changed from:'(gcv)
        current_value  = current_global_check_variant
        requested_value  = p_gcvar  )
      table        = log_table[] ).
  ENDMETHOD.

  METHOD atc_sett_changed_updatemode.
    p_atcset = abap_true.      "change ATC settings
    p_atctst = abap_false.     "Test mode is not active
    current_global_check_variant = '/BMW/COMPLETE_CHECK_SET'.
    p_gcvar  = global_variant. "Global variant '/BMW/MINIMUM_CODE_REQUIREMENTS'
    p_never  = abap_false.     "Never
    p_pri1   = abap_false.     "Priority 1
    p_pri12  = abap_true.      "Priority 12
    p_pri123 = abap_false.     "Priority 123
    p_req  = abap_true.        "Request
    p_task = abap_true.        "Task
    p_toc  = abap_false.       "TOC

    execute_main( ).
    cl_abap_unit_assert=>assert_table_contains(
      line             = VALUE /bmw/otd3110_output(
        message    = 'Gobal check variant changed from:'(gcv)
        current_value  = current_global_check_variant
        requested_value  = p_gcvar  )
      table        = log_table[] ).
  ENDMETHOD.

ENDCLASS.
