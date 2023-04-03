class ZCL_MESSAGE_EXCEPTION definition
  public
  final
  create private .

public section.

*"* public components of class ZCL_MESSAGE_STRUCT
*"* do not include other source files here!!!
  interfaces ZIF_MESSAGE .

  aliases GET_MESSAGE
    for ZIF_MESSAGE~GET_MESSAGE .

  class-methods CREATE
    importing
      !MESSAGE_OBJECT type ANY
    returning
      value(RO_MESSAGE) type ref to ZIF_MESSAGE .
  methods CONSTRUCTOR
    importing
      !IS_MSG type SYMSG .
protected section.
*"* protected components of class ZCL_MESSAGE_STRUCT
*"* do not include other source files here!!!
private section.

  aliases MESSAGE
    for ZIF_MESSAGE~MESSAGE .
ENDCLASS.



CLASS ZCL_MESSAGE_EXCEPTION IMPLEMENTATION.


METHOD constructor.
* @TODO header comments

  MOVE-CORRESPONDING is_msg TO message.

ENDMETHOD.


METHOD create.
* @TODO header comments

  DATA: ls_msg TYPE symsg.

  CLEAR ro_message.

  TRY.
*     Map the class-based exception to the system variables.
      cl_message_helper=>set_msg_vars_for_if_msg(
        EXPORTING
          text   = message_object ).

      MOVE-CORRESPONDING syst TO ls_msg.

    CATCH cx_sy_message_illegal_text.    " @TODO Invalid MESSAGE Text Parameter
* @TODO exception handling
  ENDTRY.

* Create the message instance using the text string.
  CREATE OBJECT ro_message TYPE zcl_message_exception
    EXPORTING
      is_msg = ls_msg.

ENDMETHOD.


METHOD zif_message~get_message.
* @TODO header comments

  rs_msg = message.

ENDMETHOD.
ENDCLASS.
