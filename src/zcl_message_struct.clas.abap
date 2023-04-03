class ZCL_MESSAGE_STRUCT definition
  public
  final
  create private .

public section.

*"* public components of class ZCL_MESSAGE_STRUCT
*"* do not include other source files here!!!
  interfaces ZIF_MESSAGE .

  aliases GET_MESSAGE
    for ZIF_MESSAGE~GET_MESSAGE .

  methods CONSTRUCTOR
    importing
      !IS_MSG type SYMSG .
  class-methods CREATE
    importing
      !MESSAGE_OBJECT type ANY
    returning
      value(RO_MESSAGE) type ref to ZIF_MESSAGE .
protected section.
*"* protected components of class ZCL_MESSAGE_STRUCT
*"* do not include other source files here!!!
private section.

  aliases MESSAGE
    for ZIF_MESSAGE~MESSAGE .

  types:
    BEGIN OF ty_hrpad_message_fieldlist,
      scrrprfd TYPE c LENGTH 132,
    END OF ty_hrpad_message_fieldlist .
  types:
    tt_hrpad_message_fieldlist TYPE STANDARD TABLE OF ty_hrpad_message_fieldlist
                                   WITH NON-UNIQUE KEY scrrprfd .
  types:
    BEGIN OF ty_hrpad_message,
      cause        TYPE c LENGTH 32,
      detail_level TYPE ballevel.
      INCLUDE TYPE symsg.
  TYPES:  fieldlist TYPE tt_hrpad_message_fieldlist,
          END OF ty_hrpad_message .

  class-methods GET_MESSAGE_FROM_STRUCT
    importing
      !MESSAGE_OBJECT type ANY
    returning
      value(RS_MSG) type SYMSG .
ENDCLASS.



CLASS ZCL_MESSAGE_STRUCT IMPLEMENTATION.


METHOD constructor.
* @TODO header comments

  MOVE-CORRESPONDING is_msg TO message.

ENDMETHOD.


METHOD create.
* @TODO header comments

  DATA: ls_msg TYPE symsg.

  CLEAR ro_message.

* Read the message variables from the supplied message structure.
  ls_msg = zcl_message_struct=>get_message_from_struct( message_object ).

  CREATE OBJECT ro_message TYPE zcl_message_struct
    EXPORTING
      is_msg = ls_msg.

ENDMETHOD.


METHOD get_message_from_struct.
* @TODO header comments

  DATA: lo_msg_type TYPE REF TO cl_abap_typedescr.

  FIELD-SYMBOLS: <bapiret1_msg>   TYPE bapiret1,
                 <bapiret2_msg>   TYPE bapiret2,
                 <bapireturn_msg> TYPE bapireturn,
                 <bdc_msg>        TYPE bdcmsgcoll,
                 <hrpad_msg>      TYPE ty_hrpad_message,
                 <symsg_msg>      TYPE symsg.

  CLEAR rs_msg.

* Determine the structure type being used.
  lo_msg_type = cl_abap_typedescr=>describe_by_data( message_object ).

  IF lo_msg_type->absolute_name = '\TYPE=SYMSG'.

*   Convert SYMSG to the system message structure.
    ASSIGN message_object TO <symsg_msg>.
    MOVE-CORRESPONDING <symsg_msg> TO rs_msg.

  ELSEIF lo_msg_type->absolute_name = '\TYPE=BAPIRET2'.

*   Convert BAPIRET2 to the system message structure.
    ASSIGN message_object TO <bapiret2_msg>.
    rs_msg-msgid = <bapiret2_msg>-id.
    rs_msg-msgno = <bapiret2_msg>-number.
    rs_msg-msgty = <bapiret2_msg>-type.
    rs_msg-msgv1 = <bapiret2_msg>-message_v1.
    rs_msg-msgv2 = <bapiret2_msg>-message_v2.
    rs_msg-msgv3 = <bapiret2_msg>-message_v3.
    rs_msg-msgv4 = <bapiret2_msg>-message_v4.

  ELSEIF lo_msg_type->absolute_name = '\TYPE=BAPIRET1'.

*   Convert BAPIRET1 to the system message structure.
    ASSIGN message_object TO <bapiret1_msg>.
    rs_msg-msgid = <bapiret1_msg>-id.
    rs_msg-msgno = <bapiret1_msg>-number.
    rs_msg-msgty = <bapiret1_msg>-type.
    rs_msg-msgv1 = <bapiret1_msg>-message_v1.
    rs_msg-msgv2 = <bapiret1_msg>-message_v2.
    rs_msg-msgv3 = <bapiret1_msg>-message_v3.
    rs_msg-msgv4 = <bapiret1_msg>-message_v4.

  ELSEIF lo_msg_type->absolute_name = '\TYPE=BAPIRETURN'.

*   Convert BAPIRETURN to the system message structure.
    ASSIGN message_object TO <bapireturn_msg>.
    rs_msg-msgid = <bapireturn_msg>-code(2).
    rs_msg-msgno = <bapireturn_msg>-code+2(3).
    rs_msg-msgty = <bapireturn_msg>-type.
    rs_msg-msgv1 = <bapireturn_msg>-message_v1.
    rs_msg-msgv2 = <bapireturn_msg>-message_v2.
    rs_msg-msgv3 = <bapireturn_msg>-message_v3.
    rs_msg-msgv4 = <bapireturn_msg>-message_v4.

  ELSEIF lo_msg_type->absolute_name = '\TYPE=BDCMSGCOLL'.

*   Convert BDCMSGCOLL to the system message structure.
    ASSIGN message_object TO <bdc_msg>.
    rs_msg-msgid = <bdc_msg>-msgid.
    rs_msg-msgno = <bdc_msg>-msgnr.
    rs_msg-msgty = <bdc_msg>-msgtyp.
    rs_msg-msgv1 = <bdc_msg>-msgv1.
    rs_msg-msgv2 = <bdc_msg>-msgv2.
    rs_msg-msgv3 = <bdc_msg>-msgv3.
    rs_msg-msgv4 = <bdc_msg>-msgv4.

  ELSEIF lo_msg_type->absolute_name = '\TYPE=HRPAD_MESSAGE'.

*   Convert HRPAD_MESSAGE to the system message structure.
    ASSIGN message_object TO <hrpad_msg>.
    MOVE-CORRESPONDING <hrpad_msg> TO rs_msg.

  ELSE.

*   @TODO Call BAdI enhancement to support any other structures.

  ENDIF.

  IF rs_msg IS INITIAL.

*   @TODO raise exception.

  ENDIF.

ENDMETHOD.


METHOD zif_message~get_message.
* @TODO header comments

  rs_msg = message.

ENDMETHOD.
ENDCLASS.
