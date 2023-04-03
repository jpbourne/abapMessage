class ZCL_MESSAGE_TEXT definition
  public
  final
  create public .

public section.

*"* public components of class ZCL_MESSAGE_TEXT
*"* do not include other source files here!!!
  interfaces ZIF_MESSAGE .

  aliases GET_MESSAGE
    for ZIF_MESSAGE~GET_MESSAGE .

  methods CONSTRUCTOR
    importing
      !IV_TEXT type STRING
      !IV_MSGTY type SYMSGTY .
  class-methods CREATE
    importing
      !TEXT type CLIKE
      !MESSAGE_TYPE type SYMSGTY default ZCL_MESSAGE=>GC_MSGTY_ERROR
    returning
      value(RO_MESSAGE) type ref to ZIF_MESSAGE .
protected section.
*"* protected components of class ZCL_MESSAGE_TEXT
*"* do not include other source files here!!!
private section.

  aliases MESSAGE
    for ZIF_MESSAGE~MESSAGE .

  constants GC_MSGID_DEFAULT type SYMSGID value 'SALV_BS_MSG' ##NO_TEXT.
  constants GC_MSGNO_DEFAULT type SYMSGNO value '000' ##NO_TEXT.

  methods SPLIT_TEXT_STRING
    importing
      !IV_TEXT type CLIKE
    changing
      !CS_SYMSG type SYMSG .
ENDCLASS.



CLASS ZCL_MESSAGE_TEXT IMPLEMENTATION.


METHOD constructor.
* @TODO header comments

  message-msgid = me->gc_msgid_default.
  message-msgty = iv_msgty.
  message-msgno = gc_msgno_default.

* Split the text string into message variables.
  me->split_text_string(
    EXPORTING
      iv_text  = iv_text
    CHANGING
      cs_symsg = message ).

ENDMETHOD.


METHOD create.
* @TODO header comments

  DATA: lv_text TYPE string.

  CLEAR ro_message.

  lv_text = text.

* Create the message instance using the text string.
  CREATE OBJECT ro_message TYPE zcl_message_text
    EXPORTING
      iv_text  = lv_text
      iv_msgty = message_type.

ENDMETHOD.


METHOD split_text_string.
"! Split the supplied text string into the four message variables MSGV1 to MSGV4 of the supplied message structure.
"!
"! @parameter iv_text  | Text string to be split.
"! @parameter cs_symsg | Message structure for the split result (fields MSGV1 to MSGV4).

  DATA: lt_stringtab TYPE stringtab,
        ls_stringtab LIKE LINE OF lt_stringtab,
        ls_msg TYPE symsg,
        lv_fieldname TYPE fieldname,
        lv_fieldnum TYPE n LENGTH 1,
        lv_msglen TYPE i,
        lv_msgtx TYPE string.

  FIELD-SYMBOLS: <field> TYPE any.

  CLEAR: cs_symsg-msgv1, cs_symsg-msgv2,
         cs_symsg-msgv3, cs_symsg-msgv4.

  DESCRIBE FIELD cs_symsg-msgv1 LENGTH lv_msglen IN CHARACTER MODE.

* Split the text based on the supplied line length.
  lv_msgtx = iv_text.
  CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
    EXPORTING
      text        = lv_msgtx
      line_length = lv_msglen
    TABLES
      text_tab    = lt_stringtab.
  IF lt_stringtab[] IS INITIAL.
    RETURN.
  ENDIF.

* Populate the message variables using the supplied data.
  DO 4 TIMES.
    READ TABLE lt_stringtab INTO ls_stringtab INDEX sy-index.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    lv_fieldnum = sy-index.
    lv_fieldname = 'CS_SYMSG' && '-' && 'MSGV' && lv_fieldnum.
    CONDENSE lv_fieldname NO-GAPS.
    ASSIGN (lv_fieldname) TO <field>.
    IF sy-subrc = 0.
*     Assign the current text line to the message variable
*     being processed after removing leading spaces.
      CONDENSE ls_stringtab.
      <field> = ls_stringtab.
    ENDIF.
  ENDDO.

ENDMETHOD.                    "split_text_string


METHOD ZIF_MESSAGE~GET_MESSAGE.
* @TODO header comments

  rs_msg = message.

ENDMETHOD.
ENDCLASS.
