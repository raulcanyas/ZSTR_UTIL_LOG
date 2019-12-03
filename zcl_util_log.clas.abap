class ZCL_UTIL_LOG definition
  public
  final
  create private .

public section.

  interfaces ZIF_UTIL_LOG .
  interfaces ZIF_UTIL_VLOG .

  constants:
    BEGIN OF cs_problem,
        very_serious TYPE balprobcl VALUE '1',
        serious      TYPE balprobcl VALUE '2',
        general      TYPE balprobcl VALUE '3',
      END OF cs_problem .
  constants:
    BEGIN OF cs_detlevel,
        user      TYPE ballevel VALUE '1',
        developer TYPE ballevel VALUE '2',
        admin     TYPE ballevel VALUE '3',
      END OF cs_detlevel .
  data MV_LOG_HANDLE type BALLOGHNDL .
  data MV_LOG_NUMBER type BALOGNR .

  class-methods CHECK_OBJ_SUBOBJ
    importing
      !IV_OBJECT type BALOBJ_D
      !IV_SUBOBJECT type BALSUBOBJ
    returning
      value(RV_OK) type FLAG .
  class-methods NEW_LOG
    importing
      !IV_OBJECT type BALOBJ_D
      !IV_SUBOBJECT type BALSUBOBJ
      !IV_EXTNUMBER type BALNREXT
    returning
      value(RI_LOG) type ref to ZIF_UTIL_LOG .
  class-methods NEW_VLOG
    returning
      value(RI_LOG) type ref to ZIF_UTIL_VLOG .
  class-methods BALM_2_BALS
    importing
      !IS_BALM type BALM
    returning
      value(RS_BALS) type BAL_S_MSG .
  class-methods BALS_2_BAPIRET2
    importing
      !IS_BALS type BAL_S_MSG
    returning
      value(RS_BAPIRET2) type BAPIRET2 .
  class-methods BAPIRET2_2_BALS
    importing
      value(IS_BAPIRET2) type BAPIRET2
    returning
      value(RS_BALS) type BAL_S_MSG .
  class-methods READ_LOG
    importing
      !IV_LOGNUMBER type BALOGNR
    returning
      value(RI_LOG) type ref to ZIF_UTIL_LOG .
  class-methods READ_LOG_BY_HANDLE
    importing
      !IV_LOGHANDLE type BALLOGHNDL
    returning
      value(RI_LOG) type ref to ZIF_UTIL_LOG .
  class-methods READ_LOG_BY_EXTNUMBER
    importing
      !IV_OBJECT type BALOBJ_D
      !IV_SUBOBJECT type BALSUBOBJ
      !IV_EXTNUMBER type BALNREXT
    returning
      value(RI_LOG) type ref to ZIF_UTIL_LOG .
protected section.
private section.

  types:
    BEGIN OF ty_log_alv,
      msg_icon TYPE icon_d,
      msg_line TYPE text255,
    END OF ty_log_alv .
  types:
    tty_log_alv TYPE TABLE OF ty_log_alv .

  data MO_DD type ref to CL_DD_DOCUMENT .
  data MO_ALV type ref to CL_GUI_ALV_GRID .
  data MT_ALV_DATA type TTY_LOG_ALV .
  data MV_DETLEVEL type BALLEVEL .
  data MS_HEADER type BALHDR .
  data MT_HEADER_PAR type BALHDRP_T .
  data MV_HEADER_PAR_NUM type BALPNR .
  data MT_LOG_MSG type ZTT_BAL_LOG .
  data MT_DB_MSG type ZTT_BAL_LOG .
  data MV_VIRTUAL_LOG type FLAG .
  data MV_SE61_DIALOG_TEXT type BALTEXT .

  methods SHOW_POPUP_LOG .
  methods SHOW_HEADER_LOG
    importing
      !IO_HEAD_CONTAINER type ref to CL_GUI_CONTAINER
      !IT_PARAM_TEXT type ZTT_BAL_PARAM_TEXT optional .
  methods SHOW_ALV_LOG
    importing
      !IO_MSG_CONTAINER type ref to CL_GUI_CONTAINER
      !IO_HEAD_CONTAINER type ref to CL_GUI_CONTAINER optional
      !IT_PARAM_TEXT type ZTT_BAL_PARAM_TEXT optional .
  methods CLEAR_ALV_LOG .
  methods ADD_MSG_DATA
    importing
      !IV_MSGTY type SYMSGTY
      !IV_MSGID type SYMSGID
      !IV_MSGNO type SYMSGNO
      !IV_MSGV1 type SYMSGV
      !IV_MSGV2 type SYMSGV
      !IV_MSGV3 type SYMSGV
      !IV_MSGV4 type SYMSGV
      !IV_DETLEVEL type BALLEVEL optional
      !IV_PROBCLASS type BALPROBCL optional .
  methods ADD_BAPIRET2
    importing
      !IS_BAPIRET2 type BAPIRET2 .
  methods GET_MESSAGES
    importing
      !IV_FILTER type CHAR1 default ''
    returning
      value(RT_MSG) type ZTT_BAL_LOG .
  methods GET_HEADER_PARAMETERS
    returning
      value(RT_HEADER_PAR) type BAL_T_PAR .
  methods HAS_ERROR
    returning
      value(RV_HAS) type FLAG .
  methods MODIFY_HEADER_PARAMETER
    importing
      !IV_PARNAME type BALPAR
      !IV_PARVALUE type BALPVAL .
  methods ADD_MSG
    importing
      !IS_MSG type BAL_S_MSG .
  methods GET_MESSAGES_BAPIRET2
    importing
      !IV_FILTER type CHAR1 default ''
    returning
      value(RT_BAPIRET2) type BAPIRET2_T .
  methods ADD_BAPIRET2_T
    importing
      !IT_BAPIRET2 type BAPIRET2_T .
  methods ADD_HEADER_PARAMETER
    importing
      !IV_PARNAME type BALPAR
      !IV_PARVALUE type BALPVAL .
  methods CHANGE_HEADER_PARAMETER
    importing
      !IV_PARNAME type BALPAR
      !IV_PARVALUE type BALPVAL .
  methods SET_USER_DETLEVEL .
  class-methods VALID_DETLEVEL
    importing
      !IV_DETLEVEL type BALLEVEL default ''
    returning
      value(RV_DETLEVEL) type BALLEVEL .
  class-methods VALID_PROBCLASS
    importing
      !IV_PROBCLASS type BALPROBCL default ''
    returning
      value(RV_PROBCLASS) type BALPROBCL .
ENDCLASS.



CLASS ZCL_UTIL_LOG IMPLEMENTATION.


METHOD add_bapiret2.

    DATA: ls_msg TYPE bal_s_msg.

    ls_msg = bapiret2_2_bals( is_bapiret2 ).
    IF ls_msg IS NOT INITIAL.

      " Determinar nivel de visualización
      ls_msg-detlevel = valid_detlevel( ).
      " Determinar clase de problema
      ls_msg-probclass = valid_probclass( ).

      add_msg( ls_msg ).

    ENDIF.

  ENDMETHOD.


METHOD add_bapiret2_t.

    DATA: ls_bapiret2 TYPE bapiret2,
          ls_msg      TYPE bal_s_msg.

    LOOP AT it_bapiret2 INTO ls_bapiret2.
      ls_msg = bapiret2_2_bals( ls_bapiret2 ).
      IF ls_msg IS NOT INITIAL.
        APPEND ls_msg TO mt_log_msg.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


METHOD add_header_parameter.

    DATA: ls_h_par TYPE balhdrp.

    " Aumentar contador de parámetros
    mv_header_par_num = mv_header_par_num + 1.

    " Crear nuevo parámetro
    ls_h_par-lognumber = mv_log_number.
    ls_h_par-parnumber = mv_header_par_num.
    ls_h_par-parname   = iv_parname.
    ls_h_par-parvalue  = iv_parvalue.

    INSERT ls_h_par INTO TABLE mt_header_par.

  ENDMETHOD.


METHOD add_msg.

    DATA: ls_msg TYPE bal_s_msg.

    ls_msg = is_msg.
    IF ls_msg IS NOT INITIAL.
      " Verificar nivel de visualización
      ls_msg-detlevel = valid_detlevel( ls_msg-detlevel ).
      " Verificar clase de problema
      ls_msg-probclass = valid_probclass( ls_msg-probclass ).

      APPEND ls_msg TO mt_log_msg.
    ENDIF.

  ENDMETHOD.


METHOD add_msg_data.

    DATA: ls_msg TYPE bal_s_msg.

    IF iv_msgid IS INITIAL OR
       iv_msgty IS INITIAL OR
       iv_msgno IS INITIAL.
      RETURN.
    ENDIF.

    ls_msg-msgty     = iv_msgty.
    ls_msg-msgid     = iv_msgid.
    ls_msg-msgno     = iv_msgno.
    ls_msg-msgv1     = iv_msgv1.
    ls_msg-msgv2     = iv_msgv2.
    ls_msg-msgv3     = iv_msgv3.
    ls_msg-msgv4     = iv_msgv4.

    " Determinar nivel de visualización
    ls_msg-detlevel = valid_detlevel( iv_detlevel ).
    " Determinar clase de problema
    ls_msg-probclass = valid_probclass( iv_probclass ).

    add_msg( ls_msg ).

  ENDMETHOD.


METHOD balm_2_bals.
    MOVE-CORRESPONDING is_balm TO rs_bals.
  ENDMETHOD.


METHOD bals_2_bapiret2.

    rs_bapiret2-type       = is_bals-msgty.
    rs_bapiret2-id         = is_bals-msgid.
    rs_bapiret2-number     = is_bals-msgno.
    rs_bapiret2-message_v1 = is_bals-msgv1.
    rs_bapiret2-message_v2 = is_bals-msgv2.
    rs_bapiret2-message_v3 = is_bals-msgv3.
    rs_bapiret2-message_v4 = is_bals-msgv4.

    MESSAGE ID     rs_bapiret2-id
            TYPE   rs_bapiret2-type
            NUMBER rs_bapiret2-number
            WITH   rs_bapiret2-message_v1
                   rs_bapiret2-message_v2
                   rs_bapiret2-message_v3
                   rs_bapiret2-message_v4
      INTO         rs_bapiret2-message.

  ENDMETHOD.


METHOD bapiret2_2_bals.

    rs_bals-msgty = is_bapiret2-type.
    rs_bals-msgid = is_bapiret2-id.
    rs_bals-msgno = is_bapiret2-number.
    rs_bals-msgv1 = is_bapiret2-message_v1.
    rs_bals-msgv2 = is_bapiret2-message_v2.
    rs_bals-msgv3 = is_bapiret2-message_v3.
    rs_bals-msgv4 = is_bapiret2-message_v4.

    " Determinar nivel de visualización
    rs_bals-detlevel  = cs_detlevel-user.
    " Determinar clase de problema
    rs_bals-probclass = cs_problem-general.

  ENDMETHOD.


METHOD change_header_parameter.

    FIELD-SYMBOLS: <fs_h_par> TYPE balhdrp.

    READ TABLE mt_header_par ASSIGNING <fs_h_par>
      WITH KEY parname = iv_parname.
    IF sy-subrc EQ 0.
      <fs_h_par>-parvalue = iv_parvalue.
    ENDIF.

  ENDMETHOD.


METHOD check_obj_subobj.

    CLEAR: rv_ok.
    CALL FUNCTION 'BAL_OBJECT_SUBOBJECT_CHECK'
      EXPORTING
        i_object            = iv_object
        i_subobject         = iv_subobject
      EXCEPTIONS
        object_not_found    = 1
        subobject_not_found = 2
        subobject_necessary = 3.

    IF sy-subrc EQ 0.
      rv_ok = abap_true.
    ENDIF.

  ENDMETHOD.


METHOD clear_alv_log.

    IF me->mo_alv IS NOT INITIAL.
      me->mo_alv->free( ).
    ENDIF.

    CLEAR: me->mo_alv, me->mt_alv_data, mo_dd.

  ENDMETHOD.


METHOD get_header_parameters.

    DATA: ls_par   TYPE bal_s_par,
          ls_h_par TYPE balhdrp.

    LOOP AT me->mt_header_par INTO ls_h_par.
      CLEAR: ls_par.
      ls_par-parname  = ls_h_par-parname.
      ls_par-parvalue = ls_h_par-parvalue.
      APPEND ls_par TO rt_header_par.
    ENDLOOP.

  ENDMETHOD.


METHOD get_messages.

    CLEAR: rt_msg[].

    IF iv_filter IS INITIAL.   " Todos los mensajes
      " Mensajes DB
      APPEND LINES OF mt_db_msg TO rt_msg.
      " Mensajes nuevos
      APPEND LINES OF mt_log_msg TO rt_msg.

    ELSEIF iv_filter EQ 'D'.   " Solo mensajes DB
      APPEND LINES OF mt_db_msg TO rt_msg.

    ELSEIF iv_filter EQ 'N'.   " Solo mensajes nuevos
      APPEND LINES OF mt_log_msg TO rt_msg.

    ENDIF.

  ENDMETHOD.


METHOD get_messages_bapiret2.

    DATA: ls_msg TYPE bal_s_msg.

    CLEAR: rt_bapiret2[].

    IF iv_filter IS INITIAL.   " Todos los mensajes
      " Mensajes DB
      LOOP AT mt_db_msg INTO ls_msg.
        APPEND bals_2_bapiret2( ls_msg ) TO rt_bapiret2.
      ENDLOOP.
      " Mensajes nuevos
      LOOP AT mt_log_msg INTO ls_msg.
        APPEND bals_2_bapiret2( ls_msg ) TO rt_bapiret2.
      ENDLOOP.

    ELSEIF iv_filter EQ 'D'.   " Solo mensajes DB
      LOOP AT mt_db_msg INTO ls_msg.
        APPEND bals_2_bapiret2( ls_msg ) TO rt_bapiret2.
      ENDLOOP.

    ELSEIF iv_filter EQ 'N'.   " Solo mensajes nuevos
      LOOP AT mt_log_msg INTO ls_msg.
        APPEND bals_2_bapiret2( ls_msg ) TO rt_bapiret2.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


METHOD has_error.

    " Error mensajes DB
    READ TABLE mt_db_msg TRANSPORTING NO FIELDS
      WITH KEY msgty = 'E'.
    IF sy-subrc EQ 0.
      rv_has = abap_true.
      RETURN.
    ENDIF.
    " Error Nuevos mensajes
    READ TABLE mt_log_msg TRANSPORTING NO FIELDS
      WITH KEY msgty = 'E'.
    IF sy-subrc EQ 0.
      rv_has = abap_true.
      RETURN.
    ENDIF.

    " Abort mensajes DB
    READ TABLE mt_db_msg TRANSPORTING NO FIELDS
      WITH KEY msgty = 'A'.
    IF sy-subrc EQ 0.
      rv_has = abap_true.
      RETURN.
    ENDIF.
    " Abort Nuevos mensajes
    READ TABLE mt_log_msg TRANSPORTING NO FIELDS
      WITH KEY msgty = 'A'.
    IF sy-subrc EQ 0.
      rv_has = abap_true.
      RETURN.
    ENDIF.

    " Dump mensajes DB
    READ TABLE mt_db_msg TRANSPORTING NO FIELDS
      WITH KEY msgty = 'X'.
    IF sy-subrc EQ 0.
      rv_has = abap_true.
      RETURN.
    ENDIF.
    " Dump Nuevos mensajes
    READ TABLE mt_log_msg TRANSPORTING NO FIELDS
      WITH KEY msgty = 'X'.
    IF sy-subrc EQ 0.
      rv_has = abap_true.
      RETURN.
    ENDIF.

  ENDMETHOD.


METHOD modify_header_parameter.

    IF iv_parname IS INITIAL.
      RETURN.
    ENDIF.

    " Comprobar si el parámetro existe
    READ TABLE mt_header_par TRANSPORTING NO FIELDS
      WITH KEY parname = iv_parname.
    IF sy-subrc EQ 0.
      " Modificar parámetro existente
      change_header_parameter( iv_parname  = iv_parname
                               iv_parvalue = iv_parvalue ).
    ELSE.
      " Crear nuevo parámetro
      add_header_parameter( iv_parname  = iv_parname
                            iv_parvalue = iv_parvalue ).
    ENDIF.

  ENDMETHOD.


METHOD new_log.

    DATA: lo_log        TYPE REF TO zcl_util_log,
          ls_log        TYPE bal_s_log,
          lv_log_handle TYPE balloghndl,
          lv_log_number TYPE balognr.

    " Comprobar si la combinación de Objeto/Sub-Objeto existe y es válida
    IF check_obj_subobj( iv_object    = iv_object
                         iv_subobject = iv_subobject ) IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_extnumber IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR: ls_log.
    ls_log-extnumber  = iv_extnumber.
    ls_log-object     = iv_object.
    ls_log-subobject  = iv_subobject.
    ls_log-aldate     = sy-datum.
    ls_log-altime     = sy-uzeit.
    ls_log-aluser     = sy-uname.
    ls_log-alprog     = sy-repid.

    " Crear nuevo log y capturar Log Handle
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = lv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc EQ 0.

      " Leer Log Number de la memoria
      CALL FUNCTION 'BAL_LOG_HDR_READ'
        EXPORTING
          i_log_handle  = lv_log_handle
        IMPORTING
          e_lognumber   = lv_log_number
        EXCEPTIONS
          log_not_found = 1
          OTHERS        = 2.
      IF sy-subrc EQ 0.

        CREATE OBJECT lo_log.
        MOVE-CORRESPONDING ls_log to lo_log->ms_header.

        " Fijar Log Handle y Number en el objeto
        lo_log->mv_log_handle = lv_log_handle.
        lo_log->mv_log_number = lv_log_number.

        " Determinar nivel de lectura de mensajes del usuario
        lo_log->set_user_detlevel( ).

        " Pasar a interface de Log nuevo de Aplicación SLG1
        ri_log ?= lo_log.

      ENDIF.
    ENDIF.

  ENDMETHOD.


METHOD new_vlog.

    DATA: lo_log TYPE REF TO zcl_util_log.

    " El log Virtual sirve para poder guardar mensajes temporalmente en
    " el objeto y tratarlos en programas, funciones o lugares donde se
    " quieran gestionar logs para un proceso.
    CREATE OBJECT lo_log.
    lo_log->mv_virtual_log = abap_true.

    " Determinar nivel de lectura de mensajes del usuario
    lo_log->set_user_detlevel( ).

    " Pasar a interface de Log nuevo de Aplicación SLG1
    ri_log ?= lo_log.

  ENDMETHOD.


METHOD read_log.

    DATA: lo_log               TYPE REF TO zcl_util_log,
          lt_lognumbers        TYPE szal_lognumbers,
          lt_messages          TYPE STANDARD TABLE OF balm,
          lt_header            TYPE STANDARD TABLE OF balhdr,
          lt_header_parameters TYPE STANDARD TABLE OF balhdrp,
          ls_lognumbers        LIKE LINE OF lt_lognumbers,
          ls_balm              TYPE balm,
          ls_s_log             TYPE bal_s_log,
          lv_number_of_logs    TYPE sydbcnt.

    " Leer por LogNumber
    ls_lognumbers-item = iv_lognumber.
    APPEND ls_lognumbers TO lt_lognumbers.

    CALL FUNCTION 'APPL_LOG_READ_DB_WITH_LOGNO'
      EXPORTING
        put_into_memory   = abap_true
      IMPORTING
        number_of_logs    = lv_number_of_logs
      TABLES
        lognumbers        = lt_lognumbers
        header_data       = lt_header
        header_parameters = lt_header_parameters
        messages          = lt_messages.

    IF sy-subrc EQ 0 AND lv_number_of_logs EQ 1.

      CREATE OBJECT lo_log.

      " Determinar nivel de lectura de mensajes del usuario
      lo_log->set_user_detlevel( ).

      " A partir de los datos de cabecera leídos:
      READ TABLE lt_header INTO lo_log->ms_header INDEX 1.

      " -> Fijar Log Handle y Number en el objeto
      lo_log->mv_log_handle = lo_log->ms_header-log_handle.
      lo_log->mv_log_number = lo_log->ms_header-lognumber.
      " -> Fijar tabla de Parámetros de cabecera y número de parámetros actuales
      lo_log->mt_header_par[]   = lt_header_parameters[].
      lo_log->mv_header_par_num = lines( lo_log->mt_header_par ).

      " Pasar Mensajes a tabla de mensajes DB del objeto
      LOOP AT lt_messages INTO ls_balm.
        APPEND balm_2_bals( ls_balm ) TO lo_log->mt_db_msg.
      ENDLOOP.

      " Leer datos de cabecera actuales
      CALL FUNCTION 'BAL_LOG_HDR_READ'
        EXPORTING
          i_log_handle  = lo_log->mv_log_handle
        IMPORTING
          e_s_log       = ls_s_log
        EXCEPTIONS
          log_not_found = 1
          OTHERS        = 2.

      " Fijar Texto de Diálogo para los parámetros de cabecera
      lo_log->mv_se61_dialog_text = ls_s_log-params-altext.

      " Retornar interfaz de Log de Aplicación DB (SLG1)
      ri_log ?= lo_log.

    ENDIF.

  ENDMETHOD.


METHOD read_log_by_extnumber.

    DATA: lv_lognumber TYPE balognr.

    SELECT SINGLE lognumber INTO lv_lognumber FROM balhdr
      WHERE object    EQ iv_object
        AND subobject EQ iv_subobject
        AND extnumber EQ iv_extnumber.
    IF sy-subrc EQ 0.
      ri_log = read_log( lv_lognumber ).
    ENDIF.

  ENDMETHOD.


METHOD read_log_by_handle.

    DATA: lv_lognumber TYPE balognr.

    SELECT SINGLE lognumber INTO lv_lognumber FROM balhdr
      WHERE log_handle EQ iv_loghandle.
    IF sy-subrc EQ 0.
      ri_log = read_log( lv_lognumber ).
    ENDIF.

  ENDMETHOD.


METHOD set_user_detlevel.

    DATA: lv_parva TYPE xuvalue.

    " Si el usuario tiene fijado en la SU01 el parámetro ZLOG_DETLEVEL
    " se comprueba si tiene un valor válido y se fija como valor
    " de lectura de mensajes.
    SELECT SINGLE parva INTO lv_parva FROM usr05
      WHERE bname EQ sy-uname
        AND parid EQ 'ZLOG_DETLEVEL'.
    IF sy-subrc EQ 0.
      IF lv_parva EQ cs_detlevel-user      OR
         lv_parva EQ cs_detlevel-developer OR
         lv_parva EQ cs_detlevel-admin.
        mv_detlevel = lv_parva.
      ELSE.
        mv_detlevel = cs_detlevel-user.
      ENDIF.
    ELSE.
      mv_detlevel = cs_detlevel-user.
    ENDIF.

  ENDMETHOD.


METHOD show_alv_log.

    DATA: lt_all_msg  TYPE ztt_bal_log,
          lt_fieldcat TYPE lvc_t_fcat,
          ls_fieldcat TYPE lvc_s_fcat,
          ls_bal_msg  TYPE bal_s_msg,
          ls_alv_data TYPE ty_log_alv,
          ls_stable   TYPE lvc_s_stbl.

    IF io_msg_container IS INITIAL.
      RETURN.
    ENDIF.

    IF me->mo_alv IS INITIAL.
      " Crear objeto ALV con el container de entrada
      CREATE OBJECT me->mo_alv
        EXPORTING
          i_parent = io_msg_container.

      " Catálogo de campos
      CLEAR: lt_fieldcat[], ls_fieldcat.
      ls_fieldcat-fieldname = 'MSG_ICON'.
      ls_fieldcat-icon      = abap_true.
      ls_fieldcat-outputlen = 8.
      ls_fieldcat-just      = 'C'.
      ls_fieldcat-coltext   = TEXT-h01.
      ls_fieldcat-scrtext_s = TEXT-h01.
      ls_fieldcat-scrtext_m = TEXT-h01.
      ls_fieldcat-scrtext_l = TEXT-h01.
      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR: ls_fieldcat.
      ls_fieldcat-fieldname = 'MSG_LINE'.
      ls_fieldcat-outputlen = 100.
      ls_fieldcat-coltext   = TEXT-h02.
      ls_fieldcat-scrtext_s = TEXT-h02.
      ls_fieldcat-scrtext_m = TEXT-h02.
      ls_fieldcat-scrtext_l = TEXT-h02.
      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR: lt_all_msg[], me->mt_alv_data[].
      APPEND LINES OF mt_db_msg  TO lt_all_msg.
      APPEND LINES OF mt_log_msg TO lt_all_msg.

      " Mover logs a tabla de datos de mensajes ALV
      LOOP AT lt_all_msg INTO ls_bal_msg.
        CLEAR: ls_alv_data.

        " Fijar icono en base al tipo
        CASE ls_bal_msg-msgty.
          WHEN 'I'.
            ls_alv_data-msg_icon = '@0S@'. " Informacion
          WHEN 'S'.
            ls_alv_data-msg_icon = '@5B@'. " Led Verde
          WHEN 'W'.
            ls_alv_data-msg_icon = '@5D@'. " Led Amarillo
          WHEN 'E'.
            ls_alv_data-msg_icon = '@5C@'. " Led Rojo
          WHEN 'X'.
            ls_alv_data-msg_icon = '@3U@'. " Stop
        ENDCASE.

        " Formatear mensaje
        MESSAGE ID     ls_bal_msg-msgid
                TYPE   ls_bal_msg-msgty
                NUMBER ls_bal_msg-msgno
                WITH   ls_bal_msg-msgv1
                       ls_bal_msg-msgv2
                       ls_bal_msg-msgv3
                       ls_bal_msg-msgv4
          INTO ls_alv_data-msg_line.

        APPEND ls_alv_data TO me->mt_alv_data.
      ENDLOOP.

      " Visualizar parámetros de cabecera en el contenedor de cabecera de entrada
      show_header_log( io_head_container = io_head_container
                       it_param_text     = it_param_text ).

      " Pasar datos y catálogo al objeto ALV
      CALL METHOD me->mo_alv->set_table_for_first_display
        CHANGING
          it_outtab       = me->mt_alv_data[]
          it_fieldcatalog = lt_fieldcat.

    ELSE.

      " Refrescar ALV y mantener posición barras desplazamiento
      ls_stable-row = 'X'.
      ls_stable-col = 'X'.

      me->mo_alv->refresh_table_display(
        EXPORTING
          is_stable      = ls_stable
        EXCEPTIONS
          finished       = 1
          OTHERS         = 2 ).

    ENDIF.

  ENDMETHOD.


METHOD show_header_log.

    DATA: lo_area_1     TYPE REF TO cl_dd_area,
          lo_area_2     TYPE REF TO cl_dd_area,
          ls_header_par TYPE balhdrp,
          ls_param_text TYPE zst_bal_param_text,
          lv_text       TYPE sdydo_text_element,
          lv_num_par    TYPE int4.

    IF mo_alv              IS INITIAL OR
       io_head_container   IS INITIAL OR
       me->mt_header_par[] is INITIAL.
      " Si no hay ALV o container o parámetros no tiene sentido la cabecera
      RETURN.
    ENDIF.

    " Crear documento dinámico
    CREATE OBJECT mo_dd
      EXPORTING
        style = 'ALV_GRID'.

    lv_num_par = lines( me->mt_header_par ).

    IF lv_num_par EQ 1 OR lv_num_par EQ 2.

      " Pasar parámetros de cabecera como líneas con nombre y valor
      LOOP AT me->mt_header_par INTO ls_header_par.

        IF sy-tabix NE 1.
          " Nueva línea
          mo_dd->new_line( ).
        ENDIF.

        " Nombre parámetro:
        READ TABLE it_param_text INTO ls_param_text
          WITH KEY parname = ls_header_par-parname.
        IF sy-subrc EQ 0.
          lv_text = ls_param_text-partext.
        ELSE.
          lv_text = ls_header_par-parname.
        ENDIF.
        CONCATENATE lv_text ':' INTO lv_text.
        mo_dd->add_text( text         = lv_text
                         sap_fontsize = cl_dd_document=>medium
                         sap_emphasis = cl_dd_document=>strong ).

        " Fijar espacio lateral para escribir el siguiente texto
        mo_dd->add_gap( width = 7 ).

        " Valor parámetro
        lv_text = ls_header_par-parvalue.
        mo_dd->add_text( text         = lv_text
                         sap_fontsize = cl_dd_document=>medium
                         sap_emphasis = cl_dd_document=>emphasis ).

      ENDLOOP.

    ELSEIF lv_num_par EQ 3 OR lv_num_par EQ 4.

      mo_dd->vertical_split(
        EXPORTING split_area  = mo_dd
                  split_width = '30%'
        IMPORTING right_area  = lo_area_1 ).

      " Pasar parámetros de cabecera como líneas con nombre y valor
      LOOP AT me->mt_header_par INTO ls_header_par.

        IF sy-tabix EQ 1 OR sy-tabix EQ 3.
          IF sy-tabix EQ 3.
            " Nueva línea
            mo_dd->new_line( ).
          ENDIF.

          " Nombre parámetro:
          READ TABLE it_param_text INTO ls_param_text
            WITH KEY parname = ls_header_par-parname.
          IF sy-subrc EQ 0.
            lv_text = ls_param_text-partext.
          ELSE.
            lv_text = ls_header_par-parname.
          ENDIF.
          CONCATENATE lv_text ':' INTO lv_text.
          mo_dd->add_text( text         = lv_text
                           sap_fontsize = cl_dd_document=>medium
                           sap_emphasis = cl_dd_document=>strong ).

          " Fijar espacio lateral para escribir el siguiente texto
          mo_dd->add_gap( width = 7 ).

          " Valor parámetro
          lv_text = ls_header_par-parvalue.
          mo_dd->add_text( text         = lv_text
                           sap_fontsize = cl_dd_document=>medium
                           sap_emphasis = cl_dd_document=>emphasis ).

        ELSEIF sy-tabix EQ 2 OR sy-tabix EQ 4.
          IF sy-tabix EQ 4.
            " Nueva línea
            lo_area_1->new_line( ).
          ENDIF.

          " Nombre parámetro:
          READ TABLE it_param_text INTO ls_param_text
            WITH KEY parname = ls_header_par-parname.
          IF sy-subrc EQ 0.
            lv_text = ls_param_text-partext.
          ELSE.
            lv_text = ls_header_par-parname.
          ENDIF.
          CONCATENATE lv_text ':' INTO lv_text.
          lo_area_1->add_text( text         = lv_text
                               sap_fontsize = cl_dd_document=>medium
                               sap_emphasis = cl_dd_document=>strong ).

          " Fijar espacio lateral para escribir el siguiente texto
          lo_area_1->add_gap( width = 7 ).

          " Valor parámetro
          lv_text = ls_header_par-parvalue.
          lo_area_1->add_text( text         = lv_text
                               sap_fontsize = cl_dd_document=>medium
                               sap_emphasis = cl_dd_document=>emphasis ).

        ENDIF.

      ENDLOOP.


    ELSEIF lv_num_par EQ 5 OR lv_num_par EQ 6.

      mo_dd->vertical_split(
        EXPORTING split_area  = mo_dd
                  split_width = '33%'
        IMPORTING right_area  = lo_area_1 ).

      mo_dd->vertical_split(
        EXPORTING split_area  = lo_area_1
                  split_width = '50%'
        IMPORTING right_area  = lo_area_2 ).

      " Pasar parámetros de cabecera como líneas con nombre y valor
      LOOP AT me->mt_header_par INTO ls_header_par.

        IF sy-tabix EQ 1 OR sy-tabix EQ 4.
          IF sy-tabix EQ 4.
            " Nueva línea
            mo_dd->new_line( ).
          ENDIF.

          " Nombre parámetro:
          READ TABLE it_param_text INTO ls_param_text
            WITH KEY parname = ls_header_par-parname.
          IF sy-subrc EQ 0.
            lv_text = ls_param_text-partext.
          ELSE.
            lv_text = ls_header_par-parname.
          ENDIF.
          CONCATENATE lv_text ':' INTO lv_text.
          mo_dd->add_text( text         = lv_text
                           sap_fontsize = cl_dd_document=>medium
                           sap_emphasis = cl_dd_document=>strong ).

          " Fijar espacio lateral para escribir el siguiente texto
          mo_dd->add_gap( width = 7 ).

          " Valor parámetro
          lv_text = ls_header_par-parvalue.
          mo_dd->add_text( text         = lv_text
                           sap_fontsize = cl_dd_document=>medium
                           sap_emphasis = cl_dd_document=>emphasis ).

        ELSEIF sy-tabix EQ 2 OR sy-tabix EQ 5.
          IF sy-tabix EQ 5.
            " Nueva línea
            lo_area_1->new_line( ).
          ENDIF.

          " Nombre parámetro:
          READ TABLE it_param_text INTO ls_param_text
            WITH KEY parname = ls_header_par-parname.
          IF sy-subrc EQ 0.
            lv_text = ls_param_text-partext.
          ELSE.
            lv_text = ls_header_par-parname.
          ENDIF.
          CONCATENATE lv_text ':' INTO lv_text.
          lo_area_1->add_text( text         = lv_text
                               sap_fontsize = cl_dd_document=>medium
                               sap_emphasis = cl_dd_document=>strong ).

          " Fijar espacio lateral para escribir el siguiente texto
          lo_area_1->add_gap( width = 7 ).

          " Valor parámetro
          lv_text = ls_header_par-parvalue.
          lo_area_1->add_text( text         = lv_text
                               sap_fontsize = cl_dd_document=>medium
                               sap_emphasis = cl_dd_document=>emphasis ).

        ELSEIF sy-tabix EQ 3 OR sy-tabix EQ 6.
          IF sy-tabix EQ 6.
            " Nueva línea
            lo_area_2->new_line( ).
          ENDIF.

          " Nombre parámetro:
          READ TABLE it_param_text INTO ls_param_text
            WITH KEY parname = ls_header_par-parname.
          IF sy-subrc EQ 0.
            lv_text = ls_param_text-partext.
          ELSE.
            lv_text = ls_header_par-parname.
          ENDIF.
          CONCATENATE lv_text ':' INTO lv_text.
          lo_area_2->add_text( text         = lv_text
                               sap_fontsize = cl_dd_document=>medium
                               sap_emphasis = cl_dd_document=>strong ).

          " Fijar espacio lateral para escribir el siguiente texto
          lo_area_2->add_gap( width = 7 ).

          " Valor parámetro
          lv_text = ls_header_par-parvalue.
          lo_area_2->add_text( text         = lv_text
                               sap_fontsize = cl_dd_document=>medium
                               sap_emphasis = cl_dd_document=>emphasis ).

        ENDIF.

      ENDLOOP.

    ENDIF.

    " Generar documento dinámico
    mo_dd->merge_document( ).

    " Visualizar datos documento dinámico
    mo_dd->display_document( parent = io_head_container ).

    " Llamada al evento que procesa la cabecera
    mo_alv->list_processing_events( i_event_name = 'TOP_OF_PAGE'
                                    i_dyndoc_id  = mo_dd ).

  ENDMETHOD.


METHOD show_popup_log.

    DATA: lt_all   TYPE ztt_bal_log,
          ls_log   TYPE bal_s_msg,
          lv_zeile TYPE int4.

    APPEND LINES OF mt_db_msg  TO lt_all.
    APPEND LINES OF mt_log_msg TO lt_all.

    IF lt_all[] IS INITIAL.
      RETURN.
    ELSE.

      CALL FUNCTION 'MESSAGES_INITIALIZE'.
      LOOP AT lt_all INTO ls_log.

        IF ls_log-msgid EQ 'X' OR
           ls_log-msgid EQ 'A'.
          ls_log-msgid = 'E'.
        ENDIF.

        CALL FUNCTION 'MESSAGE_STORE'
          EXPORTING
            arbgb                  = ls_log-msgid
            msgty                  = ls_log-msgty
            msgv1                  = ls_log-msgv1
            msgv2                  = ls_log-msgv2
            msgv3                  = ls_log-msgv3
            msgv4                  = ls_log-msgv4
            txtnr                  = ls_log-msgno
            zeile                  = lv_zeile
          EXCEPTIONS
            message_type_not_valid = 1
            not_active             = 2
            OTHERS                 = 3.
        " El contador es necesario para que los mensajes con mismo ID y Tipo
        " no se acumulen y se muestren en el lugar correcto tantos como existan.
        lv_zeile = lv_zeile + 1.

      ENDLOOP.

      CALL FUNCTION 'MESSAGES_STOP'
        EXCEPTIONS
          a_message = 1
          e_message = 2
          i_message = 3
          w_message = 4
          OTHERS    = 5.

      CALL FUNCTION 'MESSAGES_SHOW'
        EXPORTING
          i_use_grid         = abap_true
          send_if_one        = abap_true
        EXCEPTIONS
          inconsistent_range = 1
          no_messages        = 2
          OTHERS             = 3.

    ENDIF.

  ENDMETHOD.


METHOD valid_detlevel.

    IF iv_detlevel IS INITIAL.
      " Si está vacío se fija USER
      rv_detlevel = cs_detlevel-user.

    ELSEIF iv_detlevel EQ cs_detlevel-user      OR
           iv_detlevel EQ cs_detlevel-developer OR
           iv_detlevel EQ cs_detlevel-admin.
      " Valor de entrada válido
      rv_detlevel = iv_detlevel.

    ELSE.
      " Valor de entrada no es válido, se fija USER
      rv_detlevel = cs_detlevel-user.

    ENDIF.

  ENDMETHOD.


METHOD valid_probclass.

    IF iv_probclass IS INITIAL.
      " Si está vacío se fija GENERAL
      rv_probclass = cs_problem-general.

    ELSEIF iv_probclass EQ cs_problem-general      OR
           iv_probclass EQ cs_problem-serious      OR
           iv_probclass EQ cs_problem-very_serious.
      " Valor de entrada válido
      rv_probclass = iv_probclass.

    ELSE.
      " Valor de entrada no es válido, se fija GENERAL
      rv_probclass = cs_problem-general.

    ENDIF.

  ENDMETHOD.


METHOD zif_util_log~add_bapiret2.

    me->add_bapiret2( is_bapiret2 = is_bapiret2 ).

  ENDMETHOD.


METHOD zif_util_log~add_msg.

    me->add_msg( is_msg = is_msg ).

  ENDMETHOD.


METHOD zif_util_log~add_msg_data.

    me->add_msg_data(
        iv_msgty     = iv_msgty      " Tipo de mensaje
        iv_msgid     = iv_msgid      " Clase de mensajes
        iv_msgno     = iv_msgno      " Número de mensaje
        iv_msgv1     = iv_msgv1      " Variable de mensaje
        iv_msgv2     = iv_msgv2      " Variable de mensaje
        iv_msgv3     = iv_msgv3      " Variable de mensaje
        iv_msgv4     = iv_msgv4      " Variable de mensaje
        iv_detlevel  = iv_detlevel   " Log aplicación: Nivel de especificación
        iv_probclass = iv_probclass  " Log aplicación: Clase de problema de mensaje
    ).

  ENDMETHOD.


METHOD zif_util_log~clear_alv.

    clear_alv_log( ).

  ENDMETHOD.


METHOD zif_util_log~get_header_parameters.

    rt_header_par = me->get_header_parameters( ).

  ENDMETHOD.


METHOD zif_util_log~get_lognumber.

    rv_lognumber = me->mv_log_number.

  ENDMETHOD.


METHOD zif_util_log~get_messages.

    rt_msg = me->get_messages( iv_filter ).

  ENDMETHOD.


METHOD zif_util_log~get_messages_bapiret2.

    rt_bapiret2 = get_messages_bapiret2( iv_filter ).

  ENDMETHOD.


METHOD zif_util_log~has_error.

    rv_has = me->has_error( ).

  ENDMETHOD.


METHOD zif_util_log~modify_header_parameter.

    me->modify_header_parameter(
        iv_parname  = iv_parname    " Log aplicación: Parámetro
        iv_parvalue = iv_parvalue   " Log aplicación: Valor del parámetro
    ).

  ENDMETHOD.


METHOD zif_util_log~reload.

    DATA: lt_lognumbers        TYPE szal_lognumbers,
          lt_messages          TYPE STANDARD TABLE OF balm,
          lt_header            TYPE STANDARD TABLE OF balhdr,
          lt_header_parameters TYPE STANDARD TABLE OF balhdrp,
          ls_lognumbers        LIKE LINE OF lt_lognumbers,
          ls_balm              TYPE balm,
          ls_s_log             TYPE bal_s_log,
          lv_number_of_logs    TYPE sydbcnt.

    " Eliminar mensajes nuevos sin grabar
    CLEAR: me->mt_log_msg[], me->mt_db_msg[].

    " Leer por LogNumber
    ls_lognumbers-item = me->mv_log_number.
    APPEND ls_lognumbers TO lt_lognumbers.

    " Eliminar Log de memoria para asegurar que se capturan
    " los datos desde DB.
    CALL FUNCTION 'BAL_LOG_REFRESH'
      EXPORTING
        i_log_handle = me->mv_log_handle
      EXCEPTIONS
        OTHERS       = 0.

    " Leer Log desde DB vía LogNumber
    CALL FUNCTION 'APPL_LOG_READ_DB_WITH_LOGNO'
      EXPORTING
        put_into_memory   = abap_true
      IMPORTING
        number_of_logs    = lv_number_of_logs
      TABLES
        lognumbers        = lt_lognumbers
        header_data       = lt_header
        header_parameters = lt_header_parameters
        messages          = lt_messages.

    IF sy-subrc EQ 0 AND lv_number_of_logs EQ 1.

      " Determinar nivel de lectura de mensajes del usuario
      me->set_user_detlevel( ).

      " A partir de los datos de cabecera leídos:
      READ TABLE lt_header INTO me->ms_header INDEX 1.

      " -> Fijar Log Handle y Log Number en el objeto
      me->mv_log_handle = me->ms_header-log_handle.
      me->mv_log_number = me->ms_header-lognumber.

      " -> Fijar tabla de Parámetros de cabecera y número de parámetros actuales
      me->mt_header_par[]   = lt_header_parameters[].
      me->mv_header_par_num = lines( me->mt_header_par ).

      " Pasar Mensajes a tabla de mensajes DB del objeto
      LOOP AT lt_messages INTO ls_balm.
        APPEND balm_2_bals( ls_balm ) TO me->mt_db_msg.
      ENDLOOP.

      " Leer datos de cabecera actuales
      CALL FUNCTION 'BAL_LOG_HDR_READ'
        EXPORTING
          i_log_handle  = me->mv_log_handle
        IMPORTING
          e_s_log       = ls_s_log
        EXCEPTIONS
          log_not_found = 1
          OTHERS        = 2.

      " Fijar Texto de Diálogo para los parámetros de cabecera
      me->mv_se61_dialog_text = ls_s_log-params-altext.

    ENDIF.

  ENDMETHOD.


METHOD zif_util_log~save.

    DATA: lt_log_handle TYPE bal_t_logh,
          lt_param      TYPE TABLE OF spar,
          lt_balhdrp    TYPE balhdrp_t,
          ls_balhdrp    TYPE balhdrp,
          ls_s_log      TYPE bal_s_log,
          ls_par        TYPE bal_s_par,
          ls_param      TYPE spar,
          ls_h_par      TYPE balhdrp,
          ls_s_par      TYPE bal_s_par,
          ls_msg        TYPE bal_s_msg.

    FIELD-SYMBOLS: <fs_h_par> TYPE balhdrp.

    IF mv_log_handle IS INITIAL.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    " Añadir mensajes nuevos al Log
    LOOP AT mt_log_msg INTO ls_msg.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = mv_log_handle
          i_s_msg          = ls_msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.

    ENDLOOP.

    " Leer datos de cabecera actuales
    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle  = mv_log_handle
      IMPORTING
        e_s_log       = ls_s_log
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2.

    IF sy-subrc NE 0.
      " Error al leer cabecera Log
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    " Actualizar parámetros de caebecera solo si esta informado
    " el texto de diálogo y existen parámetros.
    IF me->mv_se61_dialog_text IS NOT INITIAL AND
       me->mt_header_par[]     IS NOT INITIAL.

      CLEAR: ls_s_log-params-t_par.
      LOOP AT me->mt_header_par INTO ls_h_par.
        ls_s_par-parname  = ls_h_par-parname.
        ls_s_par-parvalue = ls_h_par-parvalue.
        APPEND ls_s_par TO ls_s_log-params-t_par.
        ls_s_log-params-altext = me->mv_se61_dialog_text.
      ENDLOOP.
      " Actualizar parámetros de cabecera
      CALL FUNCTION 'BAL_LOG_HDR_CHANGE'
        EXPORTING
          i_log_handle = mv_log_handle
          i_s_log      = ls_s_log
        EXCEPTIONS
          OTHERS       = 1.
      IF sy-subrc <> 0.
        " Error al modificar cabecera log
        rv_ok = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    " Grabar log en DB
    INSERT mv_log_handle INTO TABLE lt_log_handle.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
        i_in_update_task = iv_update_task
        i_t_log_handle   = lt_log_handle
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

    IF sy-subrc EQ 0.

      " Leer Log Number de la memoria
      CALL FUNCTION 'BAL_LOG_HDR_READ'
        EXPORTING
          i_log_handle  = mv_log_handle
        IMPORTING
          e_lognumber   = mv_log_number
        EXCEPTIONS
          log_not_found = 1
          OTHERS        = 2.

      " Actualizar LogNumber en parámetros de cabecera
      lt_balhdrp[] = me->mt_header_par[].
      CLEAR: me->mt_header_par[].
      LOOP AT lt_balhdrp ASSIGNING <fs_h_par>.
        MOVE-CORRESPONDING <fs_h_par> TO ls_balhdrp.
        ls_balhdrp-lognumber = mv_log_number.
        INSERT ls_balhdrp INTO TABLE me->mt_header_par.
      ENDLOOP.
      " Actualizar contador de parámetros de cabecera del log
      me->mv_header_par_num = lines( me->mt_header_par ).

      " Pasar mensajes de tabla nuevos a tabla DB
      LOOP AT mt_log_msg INTO ls_msg.
        APPEND ls_msg TO mt_db_msg.
      ENDLOOP.

      " Vaciar tabla de mensajes nuevos
      CLEAR: mt_log_msg[].

      " Ejecutar Commit si está marcado en la entrada y no es en Update Task
      IF iv_commit EQ 'X' AND iv_update_task EQ space.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.

      " Terminar con retorno OK
      rv_ok = abap_true.
    ELSE.
      " Error al grabar Log en DB
      rv_ok = abap_false.

      " Ejecutar Rollback si está marcado en la entrada y no es en Update Task
      IF iv_commit EQ 'X' AND iv_update_task EQ space.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

    ENDIF.

  ENDMETHOD.


METHOD zif_util_log~set_header_text.

    me->mv_se61_dialog_text = iv_se61_dialog_text.

  ENDMETHOD.


METHOD zif_util_log~show_alv.

    show_alv_log( io_msg_container  = io_msg_container
                  io_head_container = io_head_container
                  it_param_text     = it_param_text ).

  ENDMETHOD.


METHOD zif_util_log~show_popup.

    show_popup_log( ).

  ENDMETHOD.


METHOD zif_util_vlog~add_bapiret2.

    me->add_bapiret2( is_bapiret2 = is_bapiret2 ).

  ENDMETHOD.


METHOD zif_util_vlog~add_msg.

    me->add_msg( is_msg = is_msg ).

  ENDMETHOD.


METHOD zif_util_vlog~add_msg_data.

    me->add_msg_data(
        iv_msgty     = iv_msgty      " Tipo de mensaje
        iv_msgid     = iv_msgid      " Clase de mensajes
        iv_msgno     = iv_msgno      " Número de mensaje
        iv_msgv1     = iv_msgv1      " Variable de mensaje
        iv_msgv2     = iv_msgv2      " Variable de mensaje
        iv_msgv3     = iv_msgv3      " Variable de mensaje
        iv_msgv4     = iv_msgv4      " Variable de mensaje
        iv_detlevel  = iv_detlevel   " Log aplicación: Nivel de especificación
        iv_probclass = iv_probclass  " Log aplicación: Clase de problema de mensaje
    ).

  ENDMETHOD.


METHOD zif_util_vlog~clear_alv.

    clear_alv_log( ).

  ENDMETHOD.


METHOD zif_util_vlog~convert_2_log.

    DATA: lt_msg TYPE ztt_bal_log,
          ls_msg TYPE bal_s_msg.

    " Crear nuevo log DB (SLG1)
    ri_log = new_log( iv_object    = iv_object
                      iv_subobject = iv_subobject
                      iv_extnumber = iv_extnumber ).

    " Pasar mensajes del Log Virtual al nuevo log DB (SLG1)
    lt_msg = me->get_messages( ).

    LOOP AT lt_msg INTO ls_msg.
      ri_log->add_msg( ls_msg ).
    ENDLOOP.

  ENDMETHOD.


METHOD zif_util_vlog~get_header_parameters.

    rt_header_par = me->get_header_parameters( ).

  ENDMETHOD.


METHOD zif_util_vlog~get_messages.

    rt_msg = me->get_messages( iv_filter ).

  ENDMETHOD.


METHOD zif_util_vlog~get_messages_bapiret2.

    rt_bapiret2 = get_messages_bapiret2( iv_filter ).

  ENDMETHOD.


METHOD zif_util_vlog~has_error.

    rv_has = me->has_error( ).

  ENDMETHOD.


METHOD zif_util_vlog~modify_header_parameter.

    me->modify_header_parameter(
        iv_parname  = iv_parname    " Log aplicación: Parámetro
        iv_parvalue = iv_parvalue   " Log aplicación: Valor del parámetro
    ).

  ENDMETHOD.


METHOD zif_util_vlog~show_alv.

    show_alv_log( io_msg_container  = io_msg_container
                  io_head_container = io_head_container
                  it_param_text     = it_param_text ).

  ENDMETHOD.


METHOD zif_util_vlog~show_popup.

    show_popup_log( ).

  ENDMETHOD.
ENDCLASS.
