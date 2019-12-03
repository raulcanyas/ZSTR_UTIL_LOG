interface ZIF_UTIL_LOG
  public .


  methods GET_LOGNUMBER
    returning
      value(RV_LOGNUMBER) type BALOGNR .
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
  methods SHOW_POPUP .
  methods SHOW_ALV
    importing
      !IO_MSG_CONTAINER type ref to CL_GUI_CONTAINER
      !IO_HEAD_CONTAINER type ref to CL_GUI_CONTAINER optional
      !IT_PARAM_TEXT type ZTT_BAL_PARAM_TEXT optional .
  methods CLEAR_ALV .
  methods SAVE
    importing
      !IV_UPDATE_TASK type BOOLEAN default SPACE
      !IV_COMMIT type FLAG default ''
    returning
      value(RV_OK) type FLAG .
  methods RELOAD .
  methods SET_HEADER_TEXT
    importing
      !IV_SE61_DIALOG_TEXT type BALTEXT .
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
      !IV_MSGV1 type SYMSGV optional
      !IV_MSGV2 type SYMSGV optional
      !IV_MSGV3 type SYMSGV optional
      !IV_MSGV4 type SYMSGV optional
      !IV_DETLEVEL type BALLEVEL optional
      !IV_PROBCLASS type BALPROBCL optional .
  methods ADD_BAPIRET2
    importing
      !IS_BAPIRET2 type BAPIRET2 .
endinterface.
