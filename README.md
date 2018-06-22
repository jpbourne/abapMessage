# abapMessage
**abapMessage** is a class that can be used to simplify individual message handling. 
It is common for an ABAP program to call a number of objects (e.g. function modules, BAPIs, classes, web services, proxy objects etc) that each have their own mechanism of message feedback (e.g. exception class instance, structured message, text string etc). 
Suitable for ABAP version 702 or higher.
abapMessage is instantiated using the message returned by the called object. Once instantiated the following functionality is available:
* _Convert the message:_ Depending on the required functionality the message can be returned in a desired format. E.g. for a custom BAPI function module return the message in BAPI Return format. For a batch job program return the message as formatted text for output in the spool.
* _Query the message metadata:_ Identify the severity of the message using the standard message types. E.g. after calling a BAPI function module it is possible to check if an error has occurred.
* _Progress indicator:_ Use the message as a batch job or SAP GUI progress indicator. 

## Installation

## Usage

## 

```abap
data: lo_message type ref to zcl_message.

lo_message = zcl_message=>new( ).
if lo_message->is_error = abap_true.
  "Error handling.
endif.
```
