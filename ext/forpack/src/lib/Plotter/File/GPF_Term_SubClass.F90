SubModule(GPF_Term_Class) GPF_Term_SubClass

  implicit none

  contains

Module Procedure SetTerminalEnhanced
  use Utilities_Library         ,only:  PresentAndFalse
  use GPF_Parameters            ,only:  KEY_enhanced
  This%Enhanced   =   KEY_enhanced
  if ( PresentAndFalse(Enhanced) ) This%Enhanced = 'no'//KEY_enhanced
  This%Enhanced   =   This%Enhanced // " "
End Procedure

Module Procedure SetTerminalColor
  use Utilities_Library         ,only:  PresentAndFalse
  This%Color  =   'color'
  if ( PresentAndFalse(Color) ) This%Color = 'monochrome'
  This%Color  =   This%Color//' '
End Procedure

End SubModule