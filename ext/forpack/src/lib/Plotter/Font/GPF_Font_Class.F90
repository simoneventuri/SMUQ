Module GPF_Font_Class

  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private
  public  ::  GPF_Font_Type

  Type  ,extends(GPF_Command_Type)  ::  GPF_Font_Type
    private
    character(:)  ,allocatable  ::  Name    !< Font name
    character(:)  ,allocatable  ::  Size    !< Font size
  contains
    private
    procedure   ,public   ::  Initialize    =>  InitializeFont
    procedure   ,public   ::  Set_Command   =>  SetFontCommand
  End Type

  Interface

    Module Subroutine InitializeFont( This, Name, Size, Debug )
      class(GPF_Font_Type)                                  ,intent(out)    ::  This                            !< Font object to be constructed
      character(*)                                ,optional ,intent(in)     ::  Name                            !< Font name
      character(*)                                ,optional ,intent(in)     ::  Size                            !< Font size
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine SetFontCommand( This )
      class(GPF_Font_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Font object
    End Subroutine

  End Interface

End Module