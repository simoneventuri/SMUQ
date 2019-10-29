Module GPF_Margin_Class

  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private
  public  ::  GPF_Margin_Type

  Type  ,extends(GPF_Command_Type) ::  GPF_Margin_Type
    private
    character(1)                ::  Orientation                                     !< Orientation of current margin, either "t" (top), "b" (bottom), "r" (right) or "l" (left)
    character(:)  ,allocatable  ::  Value                                           !< Value of current Margin
  contains
    private
    procedure   ,public   ::  Initialize    =>  InitializeMargin
    procedure   ,public   ::  Set_Command   =>  SetMarginCommand
  End Type

  Interface

    Module Subroutine InitializeMargin( This, Orientation, Value, Debug )
      class(GPF_Margin_Type)                                ,intent(out)    ::  This                            !< Margin-Side object to be constructed
      character(1)                                          ,intent(in)     ::  Orientation                     !< Single letter refering the current margin which is being set (t, b, l or r)
      character(*)                                ,optional ,intent(in)     ::  Value                           !< Value of current Margin
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine SetMarginCommand( This )
      class(GPF_Margin_Type)                           ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Margin-Side object
    End Subroutine

  End Interface

End Module