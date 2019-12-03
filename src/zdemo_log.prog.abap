*&---------------------------------------------------------------------*
*& Report ZDEMO_LOG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_log.

**********************************************************************
*  -> Datos globales
**********************************************************************
DATA: gi_vlog TYPE REF TO zif_util_vlog,
      gi_log  TYPE REF TO zif_util_log.

DATA: go_split TYPE REF TO cl_gui_splitter_container,
      go_head  TYPE REF TO cl_gui_container,
      go_cont  TYPE REF TO cl_gui_container.

DATA: gt_param_text TYPE ztt_bal_param_text.

DATA: gv_save_ok TYPE flag.


**********************************************************************
*  -> Pantalla de Selección
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-t00.

PARAMETERS: p_obj  TYPE balobj_d  MODIF ID c00,
            p_sobj TYPE balsubobj MODIF ID c00,
            p_ext  TYPE balnrext.

PARAMETERS: p_baltxt TYPE baltext MODIF ID c01.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_create RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND rfsh.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_create.
PARAMETERS: p_change RADIOBUTTON GROUP g1.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_change.
PARAMETERS: p_show RADIOBUTTON GROUP g1.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_show.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_slg1 RADIOBUTTON GROUP g2 DEFAULT 'X' MODIF ID c20.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_slg1 MODIF ID c20.
PARAMETERS: p_vlog RADIOBUTTON GROUP g2 MODIF ID c20.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_vlog MODIF ID c20.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t03.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_alv RADIOBUTTON GROUP g3 DEFAULT 'X' MODIF ID c30.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_alv MODIF ID c30.
PARAMETERS: p_popup RADIOBUTTON GROUP g3 MODIF ID c30.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_popup MODIF ID c30.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN END OF BLOCK b0.


INITIALIZATION.
  PERFORM init.

AT SELECTION-SCREEN OUTPUT.
  PERFORM change_screen.


**********************************************************************
*  -> Lógica de programa
**********************************************************************
START-OF-SELECTION.

  CLEAR: gi_log, gi_log.

  IF p_create IS NOT INITIAL.

    IF p_vlog IS NOT INITIAL.
      " Nuevo log de aplicación virtual (No se graba en DB)
      gi_vlog = zcl_util_log=>new_vlog( ).

      " Añadir mensajes y parámetros de cabecera de ejemplo
      PERFORM add_virtual_messages.


    ELSEIF p_slg1 IS NOT INITIAL.
      " Nuevo log de aplicación SLG1 (Se graba en DB)
      gi_log = zcl_util_log=>new_log( iv_object    = p_obj
                                      iv_subobject = p_sobj
                                      iv_extnumber = p_ext ).

      " Añadir mensajes y parámetros de cabecera de ejemplo
      PERFORM add_db_messages.

      " Grabar log
      CLEAR: gv_save_ok.
      gv_save_ok = gi_log->save( ).

    ENDIF.

  ELSEIF p_change IS NOT INITIAL.

    " Leer log de aplicación SLG1
    gi_log = zcl_util_log=>read_log_by_extnumber( iv_object    = p_obj
                                                  iv_subobject = p_sobj
                                                  iv_extnumber = p_ext ).

    " Añadir mensajes adicionales y cambiar parámetros de cabecera
    PERFORM add_plus_messages.

    " Grabar log
    CLEAR: gv_save_ok.
    gv_save_ok = gi_log->save( ).

  ELSEIF p_show IS NOT INITIAL.

    gi_log = zcl_util_log=>read_log_by_extnumber( iv_object    = p_obj
                                                  iv_subobject = p_sobj
                                                  iv_extnumber = p_ext ).

  ENDIF.



**********************************************************************
*  -> Lógica de resultados
**********************************************************************
END-OF-SELECTION.

  IF p_create IS NOT INITIAL.

    IF p_vlog IS NOT INITIAL.
      " Visualizar Popup
      gi_vlog->show_popup( ).

    ELSEIF p_slg1 IS NOT INITIAL.

      WRITE:/ 'Crear Log en SLG1:'.
      ULINE.
      IF gv_save_ok IS NOT INITIAL.
        WRITE:/ '@5B@', 'Grabación correcta en DB.'.
        WRITE:/ '@0S@', 'Puede visualizar el log en la transacción SLG1,'.
        WRITE:/ '@0S@', 'O bien utilizar la opción de visualización de este programa.'.

      ELSE.
        WRITE:/ '@5C@', 'Error al grabar log en DB.'.
      ENDIF.

    ENDIF.

  ELSEIF p_change IS NOT INITIAL.

    WRITE:/ 'Modificar Log en SLG1:'.
    ULINE.
    IF gv_save_ok IS NOT INITIAL.
      WRITE:/ '@5B@', 'Grabación correcta en DB.'.
      WRITE:/ '@0S@', 'Puede visualizar el log en la transacción SLG1,'.
      WRITE:/ '@0S@', 'O bien utilizar la opción de visualización de este programa.'.

    ELSE.
      WRITE:/ '@5C@', 'Error al grabar log en DB.'.
    ENDIF.

  ELSEIF p_show IS NOT INITIAL.

    IF gi_log IS NOT INITIAL.

      IF p_alv IS NOT INITIAL.
        " Preparar textos para cabecera ALV
        PERFORM param_text.
        " Visualizar mensajes en ALV con Cabecera
        CALL SCREEN 9000.
      ELSEIF p_popup IS NOT INITIAL.
        " Visualizar mensajes en ventana POPUP
        gi_log->show_popup( ).
      ENDIF.

    ELSE.
      WRITE:/ '@5C@', 'Log DB no encontrado.'.
    ENDIF.

  ENDIF.


**********************************************************************
*  -> Rutinas de programa
**********************************************************************

*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM init .
  p_obj    = 'ZDEMO_LOG'.
  p_sobj   = 'DEMO'.
  p_ext    = 'LOG_IDENTIFIER'.
  p_baltxt = 'ZDEMO_LOG_HEADER_TEXT'.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  CHANGE_SCREEN
*&---------------------------------------------------------------------*
FORM change_screen .

  LOOP AT SCREEN.
    IF screen-group1 EQ 'C00'.
      screen-input = '0'.
      MODIFY SCREEN.

    ELSEIF screen-group1 EQ 'C01'.
      IF p_create IS NOT INITIAL.
        screen-invisible = '0'.
        screen-input     = '1'.
        MODIFY SCREEN.
      ELSEIF p_change IS NOT INITIAL.
        screen-invisible = '0'.
        screen-input     = '1'.
        MODIFY SCREEN.
      ELSEIF p_show IS NOT INITIAL.
        screen-invisible = '1'.
        screen-input     = '0'.
        MODIFY SCREEN.
      ENDIF.

    ELSEIF screen-group1 EQ 'C20'.
      IF p_create IS NOT INITIAL.
        screen-invisible = '0'.
        MODIFY SCREEN.
      ELSEIF p_change IS NOT INITIAL.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ELSEIF p_show IS NOT INITIAL.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.

    ELSEIF screen-group1 EQ 'C30'.
      IF p_create IS NOT INITIAL.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ELSEIF p_change IS NOT INITIAL.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ELSEIF p_show IS NOT INITIAL.
        screen-invisible = '0'.
        MODIFY SCREEN.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ADD_VIRTUAL_MESSAGES
*&---------------------------------------------------------------------*
FORM add_virtual_messages .

  DATA: ls_msg TYPE bal_s_msg.

  " Añadir mensajes de ejemplo
  CLEAR: ls_msg.
  ls_msg-msgid = '00'.
  ls_msg-msgty = 'S'.
  ls_msg-msgno = 398.
  ls_msg-msgv1 = 'Test mensaje informativo'.
  gi_vlog->add_msg( is_msg = ls_msg ).

  CLEAR: ls_msg.
  ls_msg-msgid = '00'.
  ls_msg-msgty = 'W'.
  ls_msg-msgno = 398.
  ls_msg-msgv1 = 'Test mensaje Advertencia'.
  gi_vlog->add_msg( is_msg = ls_msg ).

  CLEAR: ls_msg.
  ls_msg-msgid = '00'.
  ls_msg-msgty = 'E'.
  ls_msg-msgno = 398.
  ls_msg-msgv1 = 'Test mensaje Error'.
  gi_vlog->add_msg( is_msg = ls_msg ).

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ADD_DB_MESSAGES
*&---------------------------------------------------------------------*
FORM add_db_messages .

  DATA: ls_msg     TYPE bal_s_msg,
        lv_param   TYPE balpar,
        lv_value   TYPE balpval,
        lv_num_par TYPE numc1.

  " Añadir mensajes de ejemplo
  CLEAR: ls_msg.
  ls_msg-msgid = '00'.
  ls_msg-msgty = 'S'.
  ls_msg-msgno = 398.
  ls_msg-msgv1 = 'Test mensaje informativo'.
  gi_log->add_msg( is_msg = ls_msg ).

  CLEAR: ls_msg.
  ls_msg-msgid = '00'.
  ls_msg-msgty = 'W'.
  ls_msg-msgno = 398.
  ls_msg-msgv1 = 'Test mensaje Advertencia'.
  gi_log->add_msg( is_msg = ls_msg ).

  CLEAR: ls_msg.
  ls_msg-msgid = '00'.
  ls_msg-msgty = 'E'.
  ls_msg-msgno = 398.
  ls_msg-msgv1 = 'Test mensaje Error'.
  gi_log->add_msg( is_msg = ls_msg ).

  " Fijar Texto Dialogo (DT) de transacción SE61 donde están los parámetros añadidos
  gi_log->set_header_text( iv_se61_dialog_text = p_baltxt ).

  " Añadir 6 parámetros de cabecera con sus respectivos textos para la cabecera del ALV
  lv_num_par = 1.
  DO 6 TIMES.

    lv_param = 'PAR'.
    lv_value = 'Valor'.
    CONCATENATE lv_param lv_num_par INTO lv_param.
    CONCATENATE lv_value lv_num_par INTO lv_value SEPARATED BY space.

    " Añadir parámetros de cabecera de ejemplo
    gi_log->modify_header_parameter( iv_parname  = lv_param
                                     iv_parvalue = lv_value ).
    lv_num_par = lv_num_par + 1.
  ENDDO.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ADD_PLUS_MESSAGES
*&---------------------------------------------------------------------*
FORM add_plus_messages .

  DATA: ls_msg     TYPE bal_s_msg,
        lv_param   TYPE balpar,
        lv_value   TYPE balpval,
        lv_num_par TYPE numc1.

  " Añadir mensajes de ejemplo
  CLEAR: ls_msg.
  ls_msg-msgid = '00'.
  ls_msg-msgty = 'S'.
  ls_msg-msgno = 398.
  ls_msg-msgv1 = 'Test mensaje informativo adicional'.
  gi_log->add_msg( is_msg = ls_msg ).

  CLEAR: ls_msg.
  ls_msg-msgid = '00'.
  ls_msg-msgty = 'W'.
  ls_msg-msgno = 398.
  ls_msg-msgv1 = 'Test mensaje Advertencia adicional'.
  gi_log->add_msg( is_msg = ls_msg ).

  CLEAR: ls_msg.
  ls_msg-msgid = '00'.
  ls_msg-msgty = 'E'.
  ls_msg-msgno = 398.
  ls_msg-msgv1 = 'Test mensaje Error adicional'.
  gi_log->add_msg( is_msg = ls_msg ).

  " Fijar Texto Dialogo (DT) de transacción SE61 donde están los parámetros añadidos
  gi_log->set_header_text( iv_se61_dialog_text = p_baltxt ).

  " Añadir 6 parámetros de cabecera con sus respectivos textos para la cabecera del ALV
  lv_num_par = 1.
  DO 6 TIMES.

    lv_param = 'PAR'.
    lv_value = 'Valor'.
    CONCATENATE lv_param lv_num_par INTO lv_param.
    CONCATENATE lv_value lv_num_par INTO lv_value SEPARATED BY space.
    CONCATENATE lv_value 'cambiado' INTO lv_value SEPARATED BY space.

    " Añadir parámetros de cabecera de ejemplo
    gi_log->modify_header_parameter( iv_parname  = lv_param
                                     iv_parvalue = lv_value ).
    lv_num_par = lv_num_par + 1.
  ENDDO.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  PARAM_TEXT
*&---------------------------------------------------------------------*
FORM param_text .

  DATA: ls_param_text TYPE zst_bal_param_text,
        lv_num_par    TYPE numc1.

  CLEAR: gt_param_text[].

  " Añadir 6 parámetros de cabecera con sus respectivos textos para la cabecera del ALV
  lv_num_par = 1.
  DO 6 TIMES.

    ls_param_text-parname = 'PAR'.
    ls_param_text-partext = 'Parámetro'.
    CONCATENATE ls_param_text-parname lv_num_par INTO ls_param_text-parname.
    CONCATENATE ls_param_text-partext lv_num_par INTO ls_param_text-partext SEPARATED BY space.
    APPEND ls_param_text TO gt_param_text.

    lv_num_par = lv_num_par + 1.
  ENDDO.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ALV_LOG
*&---------------------------------------------------------------------*
FORM alv_log.

  DATA: lv_sash_position TYPE int4.

  IF go_split IS INITIAL.
    " Crear Splitter Container en pantalla completa
    CREATE OBJECT go_split
      EXPORTING
        metric            = '0001'
        parent            = cl_gui_container=>default_screen
        rows              = 2
        columns           = 1
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    " Recuperar container dentro del Splitter
    go_head = go_split->get_container( row = 1 column = 1 ).
    go_cont = go_split->get_container( row = 2 column = 1 ).

    " Calcular altura para el container de cabecera
    lv_sash_position = 10.
    go_split->set_row_height( id     = 1
                              height = lv_sash_position ).

    " Calcular altura para el container de mensajes
    lv_sash_position = 100 - lv_sash_position.
    go_split->set_row_height( id     = 2
                              height = lv_sash_position ).

    " Visualizar ALV de Mensajes de Log en el container creado
    gi_log->show_alv( io_msg_container  = go_cont
                      io_head_container = go_head
                      it_param_text     = gt_param_text ).

  ELSE.
    " Refrescar datos en el ALV de Mensajes de Log
    gi_log->show_alv( io_msg_container = go_cont ).
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'BASICO'.
  SET TITLEBAR 'TITULO'.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  ALV_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE alv_9000 OUTPUT.
  PERFORM alv_log.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN '&F03'.
      PERFORM clear_9000.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN '&F15' OR '&F12'.
      PERFORM clear_9000.
      SET SCREEN 0. LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  CLEAR_9000
*&---------------------------------------------------------------------*
FORM clear_9000.

  " Liberar datos ALV
  IF gi_log IS NOT INITIAL.
    gi_log->clear_alv( ).
  ENDIF.

  " Liberar containers pantalla
  IF go_head IS NOT INITIAL.
    go_head->free( ).
  ENDIF.

  IF go_cont IS NOT INITIAL.
    go_cont->free( ).
  ENDIF.

  IF go_split IS NOT INITIAL.
    go_split->free( ).
  ENDIF.

  CLEAR: go_head, go_cont, go_split.

ENDFORM.
