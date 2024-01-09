class ZCL_MM_PO_SEND_MAIL definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_MM_PO_SEND_MAIL
*"* do not include other source files here!!!

  interfaces BI_OBJECT .
  interfaces BI_PERSISTENT .
  interfaces IF_WORKFLOW .

  class-methods CLASS_CONSTRUCTOR .
  class-methods NOTIFY .
protected section.
*"* protected components of class ZCL_MM_PO_SEND_MAIL
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_MM_PO_SEND_MAIL
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_MM_PO_SEND_MAIL IMPLEMENTATION.


method BI_OBJECT~DEFAULT_ATTRIBUTE_VALUE.
endmethod.


method BI_OBJECT~EXECUTE_DEFAULT_METHOD.
endmethod.


method BI_OBJECT~RELEASE.
endmethod.


method BI_PERSISTENT~FIND_BY_LPOR.
endmethod.


method BI_PERSISTENT~LPOR.
endmethod.


method BI_PERSISTENT~REFRESH.
endmethod.


method CLASS_CONSTRUCTOR.
endmethod.


method NOTIFY.
endmethod.
ENDCLASS.
