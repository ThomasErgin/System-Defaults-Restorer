*&---------------------------------------------------------------------*
*& Include          /BMW/OTD3110_SET_SYS_CLAS_IMPL
*&---------------------------------------------------------------------*

CLASS system_defaults_setter IMPLEMENTATION.

  METHOD constructor.
    leave_on_errors = enable_errors.

    SELECT SINGLE @abap_true
      FROM t000
      INTO @dev_client_already_exists
      WHERE mandt <> @sy-mandt AND cccategory = @client_role_cust.

    "Update selection screen with current client settings
    SELECT SINGLE cccategory cccoractiv ccnocliind ccimaildis
      FROM t000
      INTO current_client_settings
      WHERE mandt = sy-mandt.
    p_role = current_client_settings-cccategory.
    set_screen_defaults_on_sel( ).

    "Check if current netweaver version provides ATC transport blocking feature
    SELECT SINGLE @abap_true
      INTO @atc_setting_exists
      FROM dd03l
         WHERE tabname = 'SCA_LOCAL_CONFIG'.

    "Get current setting of the transport management system
    SELECT SINGLE *
      FROM wbosyscust
      INTO @DATA(tms_setting)
      WHERE id = @transport_id.

    "Check if current TMS version support the ATC integration feature
    ASSIGN COMPONENT 'WBO_OBJCHK_TASK' OF STRUCTURE tms_setting TO FIELD-SYMBOL(<wbo_objchk_task>).
    IF <wbo_objchk_task> IS NOT ASSIGNED.
      current_tms_setting = feature_not_available.
      RETURN.
    ENDIF.
    current_tms_setting = <wbo_objchk_task>.

  ENDMETHOD.

  METHOD set_screen_defaults_on_sel.
    CHECK p_role IS NOT INITIAL.

    CASE p_role.
      WHEN 'C'.         "Development System
        p_chg_tr  = transport_changes_allowed.  "Changes are recorded in transport request
        p_objchg  = changes_allowed.            "Changes to repository and cross-client customizing allowed
        p_ecatt   = ecatt_allowed.              "eCATT and CATT Allowed
        p_glset   = modifiable.                 "Modifiable

      WHEN 'P'.         "Production System
        p_chg_tr  = customizing_not_allowed.   "Customizing in this client cannot be changed
        p_objchg  = no_crossclient_changes.    "No changes to cross-client customizing objects
        p_ecatt   = ecatt_not_allowed.         "eCATT and CATT not Allowed
        p_glset   = not_modifiable_indicator.  "Not Modifiable

      WHEN OTHERS.      "Test, Demo, Reference, Training System
        p_chg_tr  = customizing_not_allowed.   "Customizing in this client cannot be changed
        p_objchg  = no_crossclient_changes.    "No changes to cross-client customizing objects
        p_ecatt   = ecatt_allowed.             "eCATT and CATT Allowed
        p_glset   = not_modifiable_indicator.  "Not Modifiable
    ENDCASE.

  ENDMETHOD.

  METHOD validate_screen_input.
    IF p_clnset = abap_false AND p_sysset = abap_false AND p_usrset = abap_false AND p_atcset = abap_false.
      MESSAGE e006(cj).     "Select at least one activity
    ENDIF.
  ENDMETHOD.

  METHOD modify_selection_screen.

    LOOP AT SCREEN.

      "if user deselect the settings, disable relevant selection fields
      IF p_clnset = abap_false AND screen-group1 = 'CLN'
        OR p_usrset = abap_false AND screen-group1 = 'USR'
        OR p_sysset = abap_false AND screen-group1 = 'SYS'
        OR p_atcset = abap_false AND ( screen-group1 = 'ATC' or screen-group1 = 'A1' or screen-group1 = 'A2' )
        "disable tms settings
        OR current_tms_setting = feature_not_available AND screen-group1 = 'A2'
        "disable ATC settings
        OR atc_setting_exists = abap_false and screen-group1 = 'A1'
        "disable both System (SE06) and ATC (SE01) settings
        OR dev_client_already_exists = abap_true
        AND ( screen-name = 'P_SYSSET' OR screen-group1 = 'SYS' OR screen-name = '%BSS1008_BLOCK_1000'
          OR screen-name = 'P_ATCSET' OR screen-group1 = 'ATC' OR screen-name = '%BATC018_BLOCK_1000' OR screen-group1 = 'A1' OR screen-group1 = 'A2' ).
        screen-active = '0'.
      ELSE.
        screen-active = '1'.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.

    "Read default ATC variant of this system
    IF p_atcset = abap_true.
      DATA atc_default_variant TYPE REF TO if_satc_ac_config_ci.
      atc_default_variant ?= cl_satc_ac_config_factory=>get_read_access( ).
      atc_default_variant->get_ci_check_variant( IMPORTING e_name = current_global_check_variant ).
      p_gcvar = current_global_check_variant.
    ENDIF.

  ENDMETHOD.

  METHOD main.
    check_user_authority( ).
    restore_client_settings( ).
    restore_system_settings( ).
    restore_user_profile( ).
    restore_atc_settings( ).
    remove_db_locks( ).
  ENDMETHOD.

  METHOD restore_client_settings.
    CHECK p_clnset = abap_true.

    "No change needs to be done
    IF current_client_settings-cccategory = p_role   AND
       current_client_settings-cccoractiv = p_chg_tr AND
       current_client_settings-ccnocliind = p_objchg AND
       current_client_settings-ccimaildis = p_ecatt.
      INSERT VALUE #( message = 'No client changes made'(ncm) ) INTO TABLE log_table.
      "User wants to update client settings
    ELSEIF p_cltest = abap_true OR update_client( ) = abap_true.
      INSERT VALUE #(
        message    = 'Client settings changed from:'(csc)
        field_name = 'CCCATEGORY'(cat)
        current_value  = current_client_settings-cccategory
        requested_value  = p_role ) INTO TABLE log_table.

      INSERT VALUE #(
        message    = 'Client settings changed from:'(csc)
        field_name = 'CCCORACTIV'(obt)
        current_value  = current_client_settings-cccoractiv
        requested_value  = p_chg_tr ) INTO TABLE log_table.

      INSERT VALUE #(
        message    = 'Client settings changed from:'(csc)
        field_name = 'CCNOCLIIND'(obn)
        current_value  = current_client_settings-ccnocliind
        requested_value  = p_objchg ) INTO TABLE log_table.

      INSERT VALUE #(
        message    = 'Client settings changed from:'(csc)
        field_name = 'CCIMAILDIS'(ect)
        current_value  = current_client_settings-ccimaildis
        requested_value  = p_ecatt ) INTO TABLE log_table.

    ENDIF.

  ENDMETHOD.

  METHOD restore_system_settings.

    CHECK p_sysset = abap_true AND dev_client_already_exists = abap_false.

    "get current system settings
    SELECT SINGLE edtflag
      FROM tadir
      INTO @DATA(is_current_client_editable)
     WHERE pgmid    = @client_id
       AND object   = @client_type
       AND obj_name = @space.
    ASSERT sy-subrc = 0. "If this fails, system is really wrong

    DATA:
      client_setting_after     TYPE string,
      requested_client_setting TYPE tadir-edtflag.

    IF p_glset = not_modifiable_indicator.
      client_setting_after = 'Not-Modifiable'(nmd).
      requested_client_setting = not_modifiable_indicator.
    ELSE.
      client_setting_after = 'Modifiable'(mod).
      requested_client_setting = abap_false.
    ENDIF.

    "No change needs to be done
    IF requested_client_setting = is_current_client_editable.
      INSERT VALUE #( message = 'No system changes made'(nsm) ) INTO TABLE log_table.
      "User wants to change the client setting
    ELSEIF p_sytest = abap_true OR  update_system( requested_client_setting ) = abap_true.

      DATA:
        client_setting_before TYPE string.

      "Convert flag settings into text format
      IF is_current_client_editable = not_modifiable_indicator.
        client_setting_before = 'Not-Modifiable'(nmd).
      ELSE.
        client_setting_before = 'Modifiable'(mod).
      ENDIF.

      INSERT VALUE #( message = 'Global settings changed from:'(gsc)
                                field_name = 'EDTFLAG'(edt)
                                current_value  = client_setting_before
                                requested_value  = client_setting_after  ) INTO TABLE log_table.
    ENDIF.

  ENDMETHOD.

  METHOD restore_user_profile.
    CHECK p_usrset = abap_true.

    "get all users with profile SAP_ALL or SAP_NEW assigned
    SELECT bname, profile
      FROM ust04
      INTO TABLE @DATA(super_users)
      WHERE bname IN @s_user
      AND ( profile = @sap_all OR profile = @sap_new )
      ORDER BY bname.
    IF sy-subrc <> 0.
      INSERT VALUE #( message = 'Users with SAP_ALL/SAP_NEW not found'(unf) ) INTO TABLE log_table.
      RETURN.
    ENDIF.

    DATA: user_profiles TYPE suid_tt_bapiprof.
    "Remove SAP_ALL or SAP_NEW profiles from users
    LOOP AT super_users REFERENCE INTO DATA(current_super_user).
      APPEND INITIAL LINE TO user_profiles REFERENCE INTO DATA(user_profiles_current_line).
      user_profiles_current_line->* = VALUE #( bapiprof = current_super_user->profile ).

      AT END OF bname.
        DATA(user_name) = current_super_user->bname.

        DATA(user_updated) = update_user( user_name = user_name
                                          user_profiles = user_profiles ).
        "Display report when test mode is active or when user profiles removed
        IF p_ustest = abap_true OR user_updated = abap_true.
          INSERT VALUE #(
            message = 'SAP_ALL/SAP_NEW removed from user'(sru)
            field_name = current_super_user->bname
            current_value = 'SAP_ALL/SAP_NEW'(prf)
            requested_value = space  ) INTO TABLE log_table.
        ENDIF.
        CLEAR user_profiles.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.

  METHOD restore_atc_settings.
    CHECK p_atcset = abap_true AND dev_client_already_exists = abap_false.

    DATA(current_atc_setting) = get_current_atc_setting( ).
    DATA(requested_atc_setting) = get_requested_atc_setting( ).
    DATA(requested_tms_setting) = get_old_and_new_tms_setting( ).

    "No change needs to be done
    IF current_global_check_variant = p_gcvar
      AND current_atc_setting = requested_atc_setting
      AND ( current_tms_setting = requested_tms_setting OR current_tms_setting = feature_not_available ).
      INSERT VALUE #( message = 'No client changes made'(ncm) ) INTO TABLE log_table.

      "Display report when test mode is active or when ATC settings updated
    ELSEIF p_atctst = abap_true OR update_atc(
      requested_tms_setting = requested_tms_setting
      requested_atc_setting = requested_atc_setting
      current_atc_setting = current_atc_setting ) = abap_true.
      IF current_global_check_variant <> p_gcvar.
        INSERT VALUE #(
          message    = 'Gobal check variant changed from:'(gcv)
          current_value  = current_global_check_variant
          requested_value  = p_gcvar ) INTO TABLE log_table.
      ENDIF.

      IF current_atc_setting <> requested_atc_setting.
        INSERT VALUE #(
          message    = 'ATC settings changed from:'(bsc)
          current_value  = get_descr_of_atc_setting( current_atc_setting )
          requested_value  = get_descr_of_atc_setting( requested_atc_setting ) ) INTO TABLE log_table.
      ENDIF.

      IF current_tms_setting <> requested_tms_setting AND current_tms_setting <> feature_not_available.
        INSERT VALUE #(
          message    = 'Object checks at request/task release changed from:'(ocr)
          current_value  = get_descr_of_tms_setting( current_tms_setting )
          requested_value  = get_descr_of_tms_setting( requested_tms_setting ) ) INTO TABLE log_table.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD check_user_authority.

    DATA no_authority_message TYPE string ##needed.

    "check authority to change client/global settings
    IF p_clnset = abap_true  AND p_cltest = abap_false
     OR p_sysset = abap_true AND p_sytest = abap_false
     OR p_atcset = abap_true AND p_atctst = abap_false.
      AUTHORITY-CHECK OBJECT 'S_CTS_ADMI'
       ID 'CTS_ADMFCT' FIELD 'TABL'.
      IF sy-subrc <> 0.
        MESSAGE e726(ta) INTO no_authority_message. "No authorization to makes changes in client table T000
        raise_error_message( ).
      ENDIF.
    ENDIF.

    "check authority to change client settings
    IF p_clnset = abap_true  AND p_cltest = abap_false.
      AUTHORITY-CHECK OBJECT 'S_ADMI_FCD'
        ID 'S_ADMI_FCD' FIELD 'T000'.
      IF sy-subrc <> 0.
        MESSAGE e726(ta) INTO no_authority_message. "No authorization to makes changes in client table T000
        raise_error_message( ).
      ENDIF.
    ENDIF.

    "check authority to change global settings
    IF p_sysset = abap_true AND p_sytest = abap_false.
      AUTHORITY-CHECK OBJECT 'S_CTS_ADMI'
       ID 'CTS_ADMFCT' FIELD 'SYSC'.
      IF sy-subrc <> 0.
        MESSAGE e068(0t) INTO no_authority_message. "No authorization for processing after installation
        raise_error_message( ).
      ENDIF.
    ENDIF.

    "check authority to change user profiles(SAP_ALL/SAP_NEW)
    IF p_usrset = abap_true AND p_ustest = abap_false.
      AUTHORITY-CHECK OBJECT 'S_USER_PRO'
       ID 'PROFILE' FIELD 'SAP_ALL'
       ID 'ACTVT'   FIELD '22'.
      IF sy-subrc <> 0.
        MESSAGE e500(s#) INTO no_authority_message. "You are not authorized to change user assignments
        raise_error_message( ).
      ENDIF.

      AUTHORITY-CHECK OBJECT 'S_USER_PRO'
       ID 'PROFILE' FIELD 'SAP_NEW'
       ID 'ACTVT'   FIELD '22'.
      IF sy-subrc <> 0.
        MESSAGE e500(s#) INTO no_authority_message. "You are not authorized to change user assignments
        raise_error_message( ).
      ENDIF.
    ENDIF.

    "Check Authority to change ATC Settings
    IF p_atcset = abap_true AND p_atctst = abap_false.
      AUTHORITY-CHECK OBJECT 'S_Q_ADM'
        ID 'ACTVT'   FIELD '02'
        ID 'ATC_OBJTYP' FIELD '04'
        ID 'ATC_VISIBL' FIELD ' '
        ID 'DEVCLASS' FIELD ' '
        ID 'APPL_COMP' FIELD ' '.
      IF sy-subrc <> 0.
        MESSAGE e000 INTO no_authority_message. "You are not authorized to change ATC Settings
        raise_error_message( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD display_log.
    "batch job is active
    IF sy-batch = abap_true.
      display_log_as_text( ).
    ELSE.
      display_log_as_alv_grid( ).
    ENDIF.
  ENDMETHOD.

  METHOD display_log_as_text.
    LOOP AT log_table REFERENCE INTO DATA(current_line).
      WRITE:/ '|',
              current_line->message,    '|',
              current_line->field_name, '|',
              current_line->current_value,  '|',
              current_line->requested_value,  '|'.
    ENDLOOP.
  ENDMETHOD.

  METHOD display_log_as_alv_grid.
    TRY.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = DATA(alv_grid)
          CHANGING t_table = log_table ).

        "Set zebra color for rows
        alv_grid->get_display_settings( )->set_striped_pattern( cl_salv_display_settings=>true ).

        "Set optimal column width
        alv_grid->get_columns( )->set_optimize( abap_true ).

        "Enable all generic ALV functions (e.g. sum, sort, export ...)
        alv_grid->get_functions( )->set_all( abap_true ).

        "Enable persistent ALV display variants
        alv_grid->get_layout( )->set_key( VALUE salv_s_layout_key( report = sy-repid ) ).
        alv_grid->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).

        alv_grid->display( ).
      CATCH cx_salv_msg cx_salv_not_found.
        MESSAGE e003. "Could not display ALV GRID
    ENDTRY.
  ENDMETHOD.

  METHOD update_client.
    "if test mode is not active, update client settings
    CHECK p_cltest = abap_false.

    "Lock client table T000
    add_client_db_locks( 'T000' ).

    "update client settings
    UPDATE t000 SET cccategory = p_role
                    cccoractiv = p_chg_tr
                    ccnocliind = p_objchg
                    ccimaildis = p_ecatt
                    changeuser = sy-uname
                    changedate = sy-datum
                    WHERE mandt = sy-mandt.
    IF sy-subrc <> 0. "Error updating client settings
      MESSAGE e005 INTO DATA(update_error_message) ##needed.
      raise_error_message( ).
    ENDIF.

    client_updated = abap_true.

  ENDMETHOD.

  METHOD update_system.
    "If test mode is not active, update global settings
    CHECK p_sytest = abap_false.

    DATA update_error_message TYPE string ##needed.

    add_client_db_locks( 'TRNSPACEL' ).
    add_client_db_locks( 'TRNSPACET' ).
    add_client_db_locks( 'TRNSPACETT' ).
    add_client_db_locks( 'TADIR' ).
    "Update global settings
    UPDATE tadir SET edtflag  = @requested_client_setting
               WHERE pgmid    = @client_id
                 AND object   = @client_type
                 AND obj_name = @space.
    IF sy-subrc <> 0.
      MESSAGE e004 INTO update_error_message. "Error updating global settings
      raise_error_message( ).
    ENDIF.

    "change software components and namespaces to not-modifiable
    IF requested_client_setting = not_modifiable_indicator.
      add_client_db_locks( 'DLV_SYSTC' ).
      "Remove change option for all Software Components
      UPDATE dlv_systc SET changeable = not_modifiable_indicator.
      IF sy-subrc <> 0.
        MESSAGE e007 INTO update_error_message. "Error updating software components
        raise_error_message(  ).
      ENDIF.

      add_client_db_locks( 'TRNSPACE' ).
      "Remove change option for all Namespaces
      UPDATE trnspace SET editflag = abap_false.
      IF sy-subrc <> 0.
        MESSAGE e006 INTO update_error_message. "Error updating namespaces
        raise_error_message( ).
      ENDIF.
    ENDIF.

    system_updated = abap_true.

  ENDMETHOD.

  METHOD update_user.
    CHECK p_ustest = abap_false.

    DATA:
      return TYPE TABLE OF bapiret2.

    "delete SAP_ALL/SAP_NEW profile from user
    CALL FUNCTION '/SDF/DELETE_USER_PROFILES'
      EXPORTING
        username = user_name
      TABLES
        profiles = user_profiles
        return   = return.

    "Check for message of type error, abort and exception
    LOOP AT return REFERENCE INTO DATA(return_line) WHERE type = 'E' OR type = 'X' OR type = 'A'.
      INSERT VALUE #(
        message = return_line->message
        field_name = user_name
        current_value  = 'SAP_ALL/SAP_NEW'(prf) )
        INTO TABLE log_table.
      RETURN.
    ENDLOOP.

    user_updated = abap_true.

  ENDMETHOD.

  METHOD update_atc.
    CHECK p_atctst = abap_false.

    add_atc_db_lock( ).

    update_global_check_variant( ).

    update_atc_setting(
      current_atc_setting = current_atc_setting
      requested_atc_setting = requested_atc_setting
      inform_priority = inform_priority ).

    update_object_chks_on_release( requested_tms_setting ).

    atc_updated = abap_true.

  ENDMETHOD.

  METHOD add_atc_db_lock.

    IF atc_setting_exists = abap_true.
      CALL FUNCTION 'ENQUEUE_ESCA_LOCAL_CONF'
        EXPORTING
          mode_sca_local_config = cl_ukm_lock=>sc_lock_mode_exclusive
        EXCEPTIONS
          OTHERS                = 1.
    ELSE.
      "for Netweaver version
      CALL FUNCTION 'ENQUEUE_ESATC_AC_CONFIG'
        EXPORTING
          mode_satc_ac_config = cl_ukm_lock=>sc_lock_mode_exclusive
        EXCEPTIONS
          OTHERS              = 1.
    ENDIF.

    IF sy-subrc <> 0.
      raise_error_message( ).
    ENDIF.

  ENDMETHOD.

  METHOD remove_db_locks.
    "unlock all database tables
    CALL FUNCTION 'DEQUEUE_ALL'.
  ENDMETHOD.

  METHOD add_client_db_locks.
    "Lock database table
    CALL FUNCTION 'ENQUEUE_E_TABLEE'
      EXPORTING
        mode_rstable   = cl_ukm_lock=>sc_lock_mode_exclusive
        tabname        = tablename
        _scope         = '2'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      raise_error_message( ).
    ENDIF.

  ENDMETHOD.

  METHOD raise_error_message.

    IF leave_on_errors = abap_true.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(error_message).
    ENDIF.
    INSERT VALUE #( message = error_message ) INTO TABLE log_table.

  ENDMETHOD.

  METHOD update_atc_setting.
    CHECK current_atc_setting <> requested_atc_setting
      AND atc_setting_exists = abap_true.

    DATA(method_name) = 'SET_BLOCK_INFORM_SETTING'.
    CALL METHOD cl_satc_ac_transport_svc=>(method_name)
      EXPORTING
        i_block_priority  = requested_atc_setting
        i_inform_priority = inform_priority
      EXCEPTIONS
        OTHERS            = 1.

    IF sy-subrc <> 0. ""Settings could not be changed (database error)
      MESSAGE e877(tk) INTO DATA(update_failed) ##needed.
      raise_error_message( ).
    ENDIF.

  ENDMETHOD.

  METHOD update_object_chks_on_release.
    CHECK current_tms_setting <> feature_not_available.

    add_client_db_locks( 'WBOSYSCUST' ).

    DATA wbo_objchk_task TYPE string.
    wbo_objchk_task = `WBO_OBJCHK_TASK = ` && `'` && requested_tms_setting && `'`.
    UPDATE wbosyscust SET (wbo_objchk_task) WHERE id = transport_id.

    IF sy-subrc <> 0. ""Settings could not be changed (database error)
      MESSAGE e877(tk) INTO DATA(update_failed) ##needed.
      raise_error_message( ).
    ENDIF.

  ENDMETHOD.

  METHOD update_global_check_variant.
    CHECK current_global_check_variant <> p_gcvar.
    DATA set_global_check_variant TYPE REF TO if_satc_ac_config_write_ci.
    set_global_check_variant ?= cl_satc_ac_config_factory=>get_write_access( ).
    set_global_check_variant->set_ci_check_variant( i_name = p_gcvar ).
  ENDMETHOD.

  METHOD get_old_and_new_tms_setting.
    CHECK current_tms_setting <> feature_not_available.

    "Get the new setting for ATC TMS integration
    IF p_task = abap_true AND p_req = abap_true AND p_toc = abap_true.
      requested_tms_setting = req_task_toc.
    ELSEIF p_task = abap_true AND p_req = abap_true.
      requested_tms_setting = req_task.
    ELSEIF p_task = abap_true AND p_toc = abap_true.
      requested_tms_setting = task_toc.
    ELSEIF p_req = abap_true AND p_toc = abap_true.
      requested_tms_setting = req_toc.
    ELSEIF p_task = abap_true.
      requested_tms_setting = task_x.
    ELSEIF p_req = abap_true.
      requested_tms_setting = req.
    ELSEIF p_toc = abap_true.
      requested_tms_setting = toc_t.
    ENDIF.

  ENDMETHOD.

  METHOD get_descr_of_tms_setting.
    CASE tms_setting.
      WHEN req.
        descr_of_tms_setting = 'Request'(req).
      WHEN task_x.
        descr_of_tms_setting = 'Task'(tsk).
      WHEN toc_t.
        descr_of_tms_setting = 'TOC'(toc).
      WHEN req_task.
        descr_of_tms_setting = 'Request, Task'(rat).
      WHEN req_toc.
        descr_of_tms_setting = 'Request, TOC'(rac).
      WHEN task_toc.
        descr_of_tms_setting = 'Task, TOC'(tat).
      WHEN req_task_toc.
        descr_of_tms_setting = 'Request, Task, TOC'(rtt).
    ENDCASE.
  ENDMETHOD.

  METHOD get_current_atc_setting.
    CHECK atc_setting_exists = abap_true.

    "Determine ATC settings of the system (will ATC checks prevent release of a transport request/task?)
    DATA(method_name) = 'GET_BLOCK_INFORM_SETTING'.
    DATA block_and_inform_setting TYPE ty_block_inform_on_errors.
    CALL METHOD cl_satc_ac_transport_svc=>(method_name)
      RECEIVING
        result = block_and_inform_setting
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    current_atc_setting = block_and_inform_setting-block_priority.
    inform_priority = block_and_inform_setting-inform_priority.
  ENDMETHOD.

  METHOD get_requested_atc_setting.
    "Requested block setting
    CASE abap_true.
      WHEN p_never.
        requested_atc_setting = never.
      WHEN p_pri1.
        requested_atc_setting = on_prio1.
      WHEN p_pri12.
        requested_atc_setting = on_prio12.
      WHEN p_pri123.
        requested_atc_setting = on_prio123.
    ENDCASE.
  ENDMETHOD.

  METHOD get_descr_of_atc_setting.
    CASE atc_setting.
      WHEN never.
        descr_of_atc_setting = 'Never'(nev).
      WHEN on_prio1.
        descr_of_atc_setting = 'Priority 1'(pr1).
      WHEN on_prio12.
        descr_of_atc_setting = 'Priority 1,2'(p12).
      WHEN on_prio123.
        descr_of_atc_setting = 'Priority 1,2,3'(p13).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
