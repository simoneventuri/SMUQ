Module GPF_Origin_Class

  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private
  public  ::  GPF_Origin_Type

! ==============================================================================================================
!   THE ORIGIN COMMAND
! ==============================================================================================================
! The set origin command is used to specify the origin of a plotting surface (i.e., the graph and its margins)
! on the screen. The coordinates are given in the screen coordinate system (see coordinates (p. 22) for
! information about this system).
! Syntax:
! set origin <x-origin>,<y-origin>
! ==============================================================================================================

  Type  ,extends(GPF_Command_Type) ::  GPF_Origin_Type
    private
    character(:)  ,allocatable  ::  Coordinates                                     !< Coordinates of the graph origin
  contains
    private
    procedure   ,public   ::  Initialize    =>  InitializeOrigin
    procedure   ,public   ::  Set_Command   =>  SetOriginCommand
  End Type

  Interface

    Module Subroutine InitializeOrigin( This, X_Coord, Y_Coord, Coordinates, Debug )
      class(GPF_Origin_Type)                                ,intent(out)    ::  This
      character(*)                                ,optional ,intent(in)     ::  X_Coord                         !< X coordinate
      character(*)                                ,optional ,intent(in)     ::  Y_Coord                         !< Y coordinate
      character(*)                                ,optional ,intent(in)     ::  Coordinates                     !< Coordinates
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine SetOriginCommand( This )
      class(GPF_Origin_Type)                                ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Origin object
    End Subroutine

  End Interface

End Module