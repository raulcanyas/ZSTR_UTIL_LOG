interface ZIF_UTIL_VLOG
  public .


  methods MODIFY_HEADER_PARAMETER
    importing
      !IV_PARNAME type BALPAR
      !IV_PARVALUE type BALPVAL .
  methods ADD_MSG
    importing
      !IS_MSG type BAL_S_MSG .
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
  methods GET_MESSAGES_BAPIRET2
    importing
      !IV_FILTER type CHAR1 default ''
    returning
      value(RT_BAPIRET2) type BAPIRET2_T .
  methods HAS_ERROR
    returning
      value(RV_HAS) type FLAG .
  methods CONVERT_2_LOG
    importing
      !IV_OBJECT type BALOBJ_D
      !IV_SUBOBJECT type BALSUBOBJ
      !IV_EXTNUMBER type BALNREXT
    returning
      value(RI_LOG) type ref to ZIF_UTIL_LOG .
  methods SHOW_POPUP .
  methods SHOW_ALV
    importing
      !IO_MSG_CONTAINER type ref to CL_GUI_CONTAINER
      !IO_HEAD_CONTAINER type ref to CL_GUI_CONTAINER
      !IT_PARAM_TEXT type ZTT_BAL_PARAM_TEXT .
  methods CLEAR_ALV .
endinterface.
