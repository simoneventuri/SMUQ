Module GPF_Size_Class

  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private
  public  ::  GPF_Size_Type

  Type  ,extends(GPF_Command_Type) ::  GPF_Size_Type
    private
    character(:)  ,allocatable  ::  Ratio
    character(:)  ,allocatable  ::  Scales
  contains
    private
    procedure   ,public   ::  Initialize    =>  InitializeSize
    procedure   ,public   ::  Set_Command   =>  SetSizeCommand
  End Type

  Interface

    Module Subroutine InitializeSize( This, Ratio, X_Scale, Y_Scale, Scales, i_Isometric, Debug )
      class(GPF_Size_Type)                                  ,intent(out)    ::  This                            !< Passed-object dummy argument corresponding to the Size object
      character(*)                                ,optional ,intent(in)     ::  Ratio                           !< Ratio of the graph
      character(*)                                ,optional ,intent(in)     ::  X_Scale                         !< X scale
      character(*)                                ,optional ,intent(in)     ::  Y_Scale                         !< Y scale
      character(*)                                ,optional ,intent(in)     ::  Scales                          !< Scales
      logical                                     ,optional ,intent(in)     ::  i_Isometric                     !< Isometric ratio indicator (equal unit length for the x/y-axis)
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine SetSizeCommand( This )
      class(GPF_Size_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Size object
    End Subroutine

  End Interface

End Module