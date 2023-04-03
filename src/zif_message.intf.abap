interface ZIF_MESSAGE
  public .


  data MESSAGE type SYMSG .

  methods GET_MESSAGE
    returning
      value(RS_MSG) type SYMSG .
endinterface.
