!    cd ~/Codes/ForPack/build/forpack-1.0-Serial-Debug-ifort-17.1/Template/bin; ./TemplateTestSingle.x

Module Template_TestCase

  use pfunit_mod
  use TemplateTestCase_Class  ,only:  TemplateTestCase_Type

  implicit none

  public :: TestTemplate_Type

@TestCase
  Type  ,extends(TemplateTestCase_Type)    ::  TestTemplate_Type
  contains
    procedure :: setUp
    procedure :: tearDown
  End Type

  contains

Subroutine setUp( This )
  class(TestTemplate_Type)                                 ,intent(inout)  ::    This
End Subroutine

Subroutine tearDown( This )
  class(TestTemplate_Type)                                 ,intent(inout)  ::    This
End Subroutine

@Test
Subroutine testTemplate_1( This )
  class(TestTemplate_Type)                                 ,intent(inout)  ::    This
End Subroutine

@Test
Subroutine testTemplate_2( This )
  class(TestTemplate_Type)                                 ,intent(inout)  ::    This
End Subroutine

End Module
