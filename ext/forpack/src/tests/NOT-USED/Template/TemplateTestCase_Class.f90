Module TemplateTestCase_Class

  use pfunit_mod
  use Template_Library     ,only:  Template_Type

  implicit none

  private
  public  ::  TemplateTestCase_Type

  Type  ,extends(TestCase)  ::  TemplateTestCase_Type
    type(Template_Type)     ::  Template
  contains
    procedure :: setUp
    procedure :: DoSomething
  End Type

  contains

Subroutine setUp( This )
  class(TemplateTestCase_Type)                          ,intent(inout)  ::    This
End Subroutine

Subroutine DoSomething( This )
  class(TemplateTestCase_Type)                          ,intent(inout)  ::    This
End Subroutine

End Module