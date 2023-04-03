CLASS zcl_message DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPE-POOLS abap .
    CLASS zcl_message DEFINITION LOAD .

    TYPES:
*"* public components of class ZCL_MESSAGE
*"* do not include other source files here!!!
      tt_long_text TYPE STANDARD TABLE OF bapitgb .

    CONSTANTS gc_msgty_abort TYPE symsgty VALUE 'A' ##NO_TEXT.
    CONSTANTS gc_msgty_error TYPE symsgty VALUE 'E' ##NO_TEXT.
    CONSTANTS gc_msgty_exit TYPE symsgty VALUE 'X' ##NO_TEXT.
    CONSTANTS gc_msgty_info TYPE symsgty VALUE 'I' ##NO_TEXT.
    CONSTANTS gc_msgty_success TYPE symsgty VALUE 'S' ##NO_TEXT.
    CONSTANTS gc_msgty_warning TYPE symsgty VALUE 'W' ##NO_TEXT.
    CONSTANTS gc_msg_long_text_ascii TYPE bapitga-textformat VALUE 'ASC' ##NO_TEXT.
    CONSTANTS gc_msg_long_text_html TYPE bapitga-textformat VALUE 'HTM' ##NO_TEXT.
    CONSTANTS gc_msg_long_text_none TYPE bapitga-textformat VALUE 'NON' ##NO_TEXT.
    DATA gs_symsg TYPE symsg READ-ONLY .
    CLASS-DATA dummy TYPE text255 .
    DATA gv_msg_text TYPE string READ-ONLY .
    DATA gv_free_text TYPE abap_bool READ-ONLY .

    CLASS-METHODS new
      IMPORTING
        !message_object   TYPE any OPTIONAL
      RETURNING
        VALUE(ro_message) TYPE REF TO zcl_message .
    CLASS-METHODS new_e
      IMPORTING
        !message_text     TYPE clike
      RETURNING
        VALUE(ro_message) TYPE REF TO zcl_message .
    CLASS-METHODS new_i
      IMPORTING
        !message_text     TYPE clike
      RETURNING
        VALUE(ro_message) TYPE REF TO zcl_message .
    CLASS-METHODS new_s
      IMPORTING
        !message_text     TYPE clike
      RETURNING
        VALUE(ro_message) TYPE REF TO zcl_message .
    CLASS-METHODS new_w
      IMPORTING
        !message_text     TYPE clike
      RETURNING
        VALUE(ro_message) TYPE REF TO zcl_message .
    METHODS get_aplog
      RETURNING
        VALUE(rs_bal_msg) TYPE bal_s_msg .
    METHODS get_bapiret2
      RETURNING
        VALUE(rs_bapiret2) TYPE bapiret2 .
    METHODS get_bapireturn
      RETURNING
        VALUE(rs_bapireturn) TYPE bapireturn .
    "! Read the long text of the message (if exists).
    "! @parameter iv_text_format | Format of long text, e.g. ASC = ASCII, HTM = HTML and RTF = Rich Text Format.
    "! @parameter et_long_text   | Long text of the message.
    METHODS get_long_text
      IMPORTING
        !iv_text_format     TYPE bapitga-textformat DEFAULT zcl_message=>gc_msg_long_text_ascii
      EXPORTING
        VALUE(et_long_text) TYPE tt_long_text .
    "! Fill the system variables (SYST) with the message details.
    METHODS get_syst_variables .
    METHODS get_t100key
      RETURNING
        VALUE(rs_t100key) TYPE scx_t100key .
    "! <p class="shorttext synchronized" lang="en">Is the message an error message?</p>
    "! @parameter RV_ERROR | X = message has an error type (Error, Abort or eXit).
    METHODS is_error
      RETURNING
        VALUE(rv_error) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Is the message an information message?</p>
    "! @parameter RV_INFO | X = Information message.
    METHODS is_info
      RETURNING
        VALUE(rv_info) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Is the message a success message?</p>
    "! @parameter RV_SUCCESS | X = Success message.
    METHODS is_success
      RETURNING
        VALUE(rv_success) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Is the message a warning message?</p>
    "! @parameter RV_WARNING | X = Warning message.
    METHODS is_warning
      RETURNING
        VALUE(rv_warning) TYPE abap_bool .

    "! <p class="shorttext synchronized" lang="en">Display a progress indicator using the message text</p>
    "! <p>When running in online mode the progress indicator will be displayed in SAP GUI together with the supplied percentage.
    "! When running in background mode the message is added to the job log. The percentage value is <em>not</em> used.
    "! The functionality has an in-built delay mechanism when the output immediately flag is set to FALSE.
    "! The online progress indicator is updated every ten seconds.
    "! The background progress indicator is updated every thirty minutes.</p>
    "! <p>To find the percentage value use the total number of records and number of processed records in
    "! the following formula: Percentage = ( Processed * 100 ) DIV Total.</p>
    "!
    "! @parameter iv_output_immed  | X = always show progress indicator (ignore the delay).
    "! @parameter iv_percentage    | Percentage completion for the online progress indicator.
    "! @parameter ev_progress_sent | X = Progress message displayed / added to job log.
    METHODS show_progress_indicator
      IMPORTING
        !iv_output_immed  TYPE abap_bool DEFAULT abap_false
        !iv_percentage    TYPE i DEFAULT 0
      EXPORTING
        !ev_progress_sent TYPE abap_bool .

  PROTECTED SECTION.
*"* protected components of class ZCL_MESSAGE
*"* do not include other source files here!!!
  PRIVATE SECTION.

*"* private components of class ZCL_MESSAGE
*"* do not include other source files here!!!
    DATA go_message TYPE REF TO zif_message .

    METHODS constructor
      IMPORTING
        !message_object TYPE any
        !message_type   TYPE symsgty OPTIONAL .
    METHODS get_message_object
      IMPORTING
        !message_object TYPE any
        !message_type   TYPE symsgty DEFAULT zcl_message=>gc_msgty_error .
ENDCLASS.



CLASS ZCL_MESSAGE IMPLEMENTATION.


  METHOD constructor.
* @TODO header comments

    DATA: lv_msgnr TYPE t100c-msgnr,
          lv_msgty TYPE symsgty.

* Identify the type of message object supplied.
    me->get_message_object( message_object = message_object
                            message_type   = message_type ).

    ASSERT go_message IS BOUND.

* Read the message variables.
    gs_symsg = go_message->get_message( ).

    IF gv_free_text <> abap_true.  "@TODO check for T100 message rather than free text flag?

*   Check if the message type should be changed based on customising.
*   This may be system-wide or user-specific configuration.
      lv_msgnr = gs_symsg-msgno.
      CALL FUNCTION 'READ_CUSTOMIZED_MESSAGE'
        EXPORTING
          i_arbgb = gs_symsg-msgid
          i_dtype = gs_symsg-msgty
          i_msgnr = lv_msgnr
        IMPORTING
          e_msgty = lv_msgty.

      IF lv_msgty <> '-'. "Ignore configuration for messages that are switched off.
        gs_symsg-msgty = lv_msgty.
      ENDIF.

    ENDIF.

* Retrieve the text for the message.
    me->get_syst_variables( ).

  ENDMETHOD.                    "constructor


  METHOD get_aplog.
* @TODO header comments

    CLEAR rs_bal_msg.

* Map the message variables to the Application Log structure.
    MOVE-CORRESPONDING gs_symsg TO rs_bal_msg.

  ENDMETHOD.


  METHOD get_bapiret2.
* @TODO header comments

    CLEAR rs_bapiret2.

* Map the message variables to the BAPIRET2 structure.
    rs_bapiret2-message    = gv_msg_text.
    rs_bapiret2-id         = gs_symsg-msgid.
    rs_bapiret2-type       = gs_symsg-msgty.
    rs_bapiret2-number     = gs_symsg-msgno.
    rs_bapiret2-message_v1 = gs_symsg-msgv1.
    rs_bapiret2-message_v2 = gs_symsg-msgv2.
    rs_bapiret2-message_v3 = gs_symsg-msgv3.
    rs_bapiret2-message_v4 = gs_symsg-msgv4.

  ENDMETHOD.                    "get_bapiret2


  METHOD get_bapireturn.
* @TODO header comments

    CLEAR rs_bapireturn.

* Map the message variables to the BAPIRETURN structure.
    rs_bapireturn-code(2)    = gs_symsg-msgid.
    rs_bapireturn-code+2(3)  = gs_symsg-msgno.
    rs_bapireturn-message    = gv_msg_text.
    rs_bapireturn-type       = gs_symsg-msgty.
    rs_bapireturn-message_v1 = gs_symsg-msgv1.
    rs_bapireturn-message_v2 = gs_symsg-msgv2.
    rs_bapireturn-message_v3 = gs_symsg-msgv3.
    rs_bapireturn-message_v4 = gs_symsg-msgv4.

  ENDMETHOD.                    "get_bapireturn


  METHOD get_long_text.

    DATA: lt_long_text TYPE tt_long_text.

    CLEAR et_long_text.

* Read the long text for the message.
    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = gs_symsg-msgid
        number     = gs_symsg-msgno
        language   = sy-langu
        textformat = iv_text_format
        message_v1 = gs_symsg-msgv1
        message_v2 = gs_symsg-msgv2
        message_v3 = gs_symsg-msgv3
        message_v4 = gs_symsg-msgv4
      TABLES
        text       = lt_long_text.

    IF lt_long_text[] IS NOT INITIAL.

*   Return the long text.
      et_long_text[] = lt_long_text[].

    ENDIF.

  ENDMETHOD.                    "get_long_text


  METHOD get_message_object.
* @TODO header comments

    DATA: lo_msg_type TYPE REF TO cl_abap_typedescr,
          ls_symsg    TYPE symsg.

    FIELD-SYMBOLS: <string> TYPE any.

    IF message_object IS INITIAL.

*   Message data is blank, use the message from the system variables.
      MOVE-CORRESPONDING syst TO ls_symsg.
      go_message = zcl_message_struct=>create( ls_symsg ).
      RETURN.

    ENDIF.

    "Determine the message object supplied.
    lo_msg_type = cl_abap_typedescr=>describe_by_data( message_object ).

    IF lo_msg_type->type_kind = cl_abap_typedescr=>typekind_struct1.

      go_message = zcl_message_struct=>create( message_object ).

    ELSEIF lo_msg_type->type_kind = cl_abap_typedescr=>typekind_char OR
           lo_msg_type->type_kind = cl_abap_typedescr=>typekind_clike OR
           lo_msg_type->type_kind = cl_abap_typedescr=>typekind_csequence OR
           lo_msg_type->type_kind = cl_abap_typedescr=>typekind_string.

*   Convert character string to the system message structure.
      ASSIGN message_object TO <string>.
      go_message = zcl_message_text=>create( text         = <string>
                                             message_type = message_type ).
      gv_free_text = abap_true.

    ELSEIF lo_msg_type->type_kind = cl_abap_typedescr=>typekind_oref.

      go_message = zcl_message_exception=>create( message_object ).
*   @TODO - How to log an exception? Refer to the logic in function module BAL_LOG_EXCEPTION_ADD.

    ELSE.

      "@TODO raise a generic message stating "non-supported object".

    ENDIF.

  ENDMETHOD.


  METHOD get_syst_variables.

    MESSAGE ID gs_symsg-msgid
            TYPE gs_symsg-msgty
            NUMBER gs_symsg-msgno
            WITH gs_symsg-msgv1 gs_symsg-msgv2 gs_symsg-msgv3 gs_symsg-msgv4
            INTO gv_msg_text.

  ENDMETHOD.                    "get_syst_variables


  METHOD get_t100key.
* @TODO header comments

    rs_t100key-msgid = me->gs_symsg-msgid.
    rs_t100key-msgno = me->gs_symsg-msgno.

  ENDMETHOD.                    "GET_T100KEY


  METHOD is_error.

    CLEAR rv_error.

* Set flag if the message type is Error, Abort or Exit.
    IF gs_symsg-msgty = gc_msgty_error OR
       gs_symsg-msgty = gc_msgty_abort OR
       gs_symsg-msgty = gc_msgty_exit.

      rv_error = abap_true.

    ENDIF.

  ENDMETHOD.                    "is_error


  METHOD is_info.

    CLEAR rv_info.

* Set flag if the message type is Information.
    IF gs_symsg-msgty = gc_msgty_info.

      rv_info = abap_true.

    ENDIF.

  ENDMETHOD.                    "IS_INFO


  METHOD is_success.

    CLEAR rv_success.

* Set flag if the message type is Success.
    IF gs_symsg-msgty = gc_msgty_success.

      rv_success = abap_true.

    ENDIF.

  ENDMETHOD.                    "IS_SUCCESS


  METHOD is_warning.

    CLEAR rv_warning.

* Set flag if the message type is Warning.
    IF gs_symsg-msgty = gc_msgty_warning.

      rv_warning = abap_true.

    ENDIF.

  ENDMETHOD.                    "IS_WARNING


  METHOD new.
* @TODO header comments
* @TODO sort out the ABAP Doc comments - why are syntax errors occurring in the SPLIT_TEXT_STRING method for ABAP Doc?

    CLEAR ro_message.

* Instantiate the message object.
    CREATE OBJECT ro_message
      EXPORTING
        message_object = message_object.

  ENDMETHOD.                    "get_instance


  METHOD new_e.
* @TODO header comments
* @TODO force this to be a string supplied? Using the CLIKE data type?

    CLEAR ro_message.

* Instantiate the message object.
    CREATE OBJECT ro_message
      EXPORTING
        message_object = message_text
        message_type   = zcl_message=>gc_msgty_error.

  ENDMETHOD.


  METHOD new_i.
* @TODO header comments

    CLEAR ro_message.

* Instantiate the message object.
    CREATE OBJECT ro_message
      EXPORTING
        message_object = message_text
        message_type   = zcl_message=>gc_msgty_info.

  ENDMETHOD.


  METHOD new_s.
* @TODO header comments

    CLEAR ro_message.

* Instantiate the message object.
    CREATE OBJECT ro_message
      EXPORTING
        message_object = message_text
        message_type   = zcl_message=>gc_msgty_success.

  ENDMETHOD.


  METHOD new_w.
* @TODO header comments

    CLEAR ro_message.

* Instantiate the message object.
    CREATE OBJECT ro_message
      EXPORTING
        message_object = message_text
        message_type   = zcl_message=>gc_msgty_warning.

  ENDMETHOD.


  METHOD show_progress_indicator.

    CLEAR ev_progress_sent.

"Use the message for the progress indicator.
    CALL METHOD cl_progress_indicator=>progress_indicate
      EXPORTING
        i_msgid              = gs_symsg-msgid
        i_msgno              = gs_symsg-msgno
        i_msgv1              = gs_symsg-msgv1
        i_msgv2              = gs_symsg-msgv2
        i_msgv3              = gs_symsg-msgv3
        i_msgv4              = gs_symsg-msgv4
        i_processed          = iv_percentage
        i_total              = '100'
        i_output_immediately = iv_output_immed
      IMPORTING
        e_progress_sent      = ev_progress_sent.

  ENDMETHOD.                    "show_progress_indicator
ENDCLASS.
