*----------------------------------------------------------------------*
*       CLASS lcl_Message_Helper_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_message_helper_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_Message_Helper_Test
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_MESSAGE_HELPER
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
* ================
    DATA: go_msg TYPE REF TO zcl_message.

    METHODS: setup.
    METHODS: teardown.
    METHODS:
      "!  Method to test the invocation of the Message class using the system variables.
      syst_variables_test FOR TESTING,

      "!  Method to test the invocation of the Message class using a text string.
      string_test FOR TESTING,

      "!  Method to test the long text retrieval for a message.
      long_text_test FOR TESTING,

      "!  Method to test the progress indicator for a message.
      progress_indicator_test FOR TESTING,

      "!  Method to test the invocation of the Message class using a BAPIRET2 structure.
      bapiret2_test FOR TESTING,

      "!  Method to test the invocation of the Message class using a class-based exception object.
      exception_test FOR TESTING,

      "!  Method to test the invocation of the Message class using a Class-based exception object with T100 messaging.
      exception_t100_test FOR TESTING.

ENDCLASS.       "lcl_Message_Helper_Test


*----------------------------------------------------------------------*
*       CLASS lcl_Message_Helper_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_message_helper_test IMPLEMENTATION.
* =============================================

  METHOD setup.
* =============

  ENDMETHOD.       "setup


  METHOD teardown.
* ================


  ENDMETHOD.       "teardown


  METHOD syst_variables_test.

    "Raise a message using system variables.
    MESSAGE e000(salv_exception) WITH 'Test Message'(u06) space space space
                                 INTO zcl_message=>dummy.
    go_msg = zcl_message=>new( ).

    cl_abap_unit_assert=>assert_equals(
      act   = go_msg->is_error( )
      exp   = abap_true
      msg   = 'Invalid message type generated'(u01) ).


    "@TODO remove the below as they are for testing purposes only.
    DATA: ls_bapiret2 TYPE bapiret2,
          ls_symsg    TYPE symsg,
          lv_text     TYPE text255.

    ls_bapiret2 = go_msg->get_bapiret2( ).

    ls_symsg = go_msg->gs_symsg.

    lv_text = go_msg->gv_msg_text.

  ENDMETHOD.                    "syst_variables_test

  METHOD string_test.

    DATA: lv_msg TYPE string.

    lv_msg = 'This is a test using a text string which has to be converted into a message.'(u05).
    go_msg = zcl_message=>new_w( lv_msg ).

    cl_abap_unit_assert=>assert_equals(
      act   = go_msg->is_warning( )
      exp   = abap_true
      msg   = 'Invalid message type generated'(u01) ).

  ENDMETHOD.                    "string_test

  METHOD progress_indicator_test.

    DATA: lv_progress_sent TYPE abap_bool.

    "Raise a message using system variables.
    MESSAGE e271(ed) WITH 'Progress indicator test' space space space
                     INTO zcl_message=>dummy.
    go_msg = zcl_message=>new( ).

    "Display the progress indicator.
    go_msg->show_progress_indicator(
      EXPORTING
        iv_percentage    = '0'
      IMPORTING
        ev_progress_sent = lv_progress_sent ).

    "Ensure progress indicator was raised.
    cl_abap_unit_assert=>assert_not_initial(
      EXPORTING
        act = lv_progress_sent
        msg = 'Progress indicator not sent'(u02) ).

  ENDMETHOD.                    "progress_indicator_test


  METHOD long_text_test.

    DATA: lt_long_text TYPE zcl_message=>tt_long_text,
          ls_long_text LIKE LINE OF lt_long_text.

    "Trigger the message and instantiate the message helper object.
    MESSAGE e242(salv_exception) WITH space space space
                                 INTO zcl_message=>dummy.
    go_msg = zcl_message=>new( ).

    "Get the message long text (ASCII format).
    go_msg->get_long_text(
      IMPORTING
        et_long_text = lt_long_text ).

    cl_abap_unit_assert=>assert_not_initial(
      EXPORTING
        act = lt_long_text[]
        msg = 'Long text table is blank'(u03) ).

    go_msg->get_long_text(
      EXPORTING
        iv_text_format = 'HTM'
      IMPORTING
        et_long_text   = lt_long_text ).

    cl_abap_unit_assert=>assert_not_initial(
      EXPORTING
        act = lt_long_text[]
        msg = 'HTML Long text table is blank'(u04) ).

    READ TABLE lt_long_text INTO ls_long_text INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_long_text
      exp   = '<HTML>'
      msg   = 'HTML tag not found in long text'(u07) ).

  ENDMETHOD.                    "long_text_test

  METHOD bapiret2_test.

    DATA: ls_bapiret2 TYPE bapiret2.

    "Trigger the message and instantiate the message helper object.
    ls_bapiret2-type   = 'E'.
    ls_bapiret2-number = '000'.
    ls_bapiret2-id     = 'SALV_EXCEPTION'.
    go_msg = zcl_message=>new( ls_bapiret2 ).

    cl_abap_unit_assert=>assert_equals(
      act   = go_msg->is_error( )
      exp   = abap_true
      msg   = 'Invalid message type generated'(u01) ).

  ENDMETHOD.                    "long_text_test

  METHOD exception_test.

    DATA: lx_error TYPE REF TO cx_salv_error.

    TRY.

        RAISE EXCEPTION TYPE cx_salv_error
          EXPORTING
            textid = cx_salv_error=>cx_root.

      CATCH cx_salv_error INTO lx_error.

        go_msg = zcl_message=>new( lx_error ).

        cl_abap_unit_assert=>assert_equals(
          act   = go_msg->is_error( )
          exp   = abap_true
          msg   = 'Invalid message type generated'(u01) ).

        cl_abap_unit_assert=>assert_not_initial(
          act   = go_msg->gv_msg_text
          msg   = 'Message text is empty'(u07) ).

    ENDTRY.

  ENDMETHOD.                    "exception_test

  METHOD exception_t100_test.

    "@TODO test a dynamic exception (T100_DYN_MSG).

    DATA: lx_error TYPE REF TO cx_salv_wd_nc_t100_error.

    TRY.

        RAISE EXCEPTION TYPE cx_salv_wd_nc_t100_error
          EXPORTING
            textid = cx_salv_wd_nc_t100_error=>cx_salv_wd_nc_t100_error.

      CATCH cx_salv_wd_nc_t100_error INTO lx_error.

        DATA: lv_msg TYPE string.
        lv_msg = lx_error->get_text( ).

        go_msg = zcl_message=>new( lx_error ).

        cl_abap_unit_assert=>assert_equals(
          act   = go_msg->is_error( )
          exp   = abap_true
          msg   = 'Invalid message type generated'(u01) ).

        cl_abap_unit_assert=>assert_not_initial(
          act   = go_msg->gv_msg_text
          msg   = 'Message text is empty'(u07) ).

    ENDTRY.

  ENDMETHOD.                    "exception_t100_test

ENDCLASS.       "lcl_Message_Helper_Test
