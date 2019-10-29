SubModule(GPF_Origin_Class) GPF_Origin_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

Module Procedure InitializeOrigin

  character(*)                                              ,parameter  ::  ProcName = "InitializeOrigin"
  character(*)                                              ,parameter  ::  Keyword='origin'
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling SetOriginCoordinates" )
  call SetOriginCoordinates( This, X_Coord, Y_Coord, Coordinates )

  if (Dbg) call Logger%Write( "Calling This%Set_Keyword" )
  call This%Set_Keyword( Keyword )

  if (Dbg) call Logger%Write( "Calling This%Set_Command" )
  call This%Set_Command()

  if (Dbg) call Logger%Write( "Command = ", trim(This%Command) )

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure SetOriginCommand
  This%Command          =       ''                                                                              ! Initialisation the command to an empty string: default value
  if ( len_trim(This%Coordinates)  /= 0 )    This%Command = This%Command // This%Coordinates                    ! Setting the graph origin coordinates in the command string if defined
  if ( len_trim(This%Command) /= 0 )    This%Command = 'set ' // This%Keyword // This%Command                   ! Setting the command string
  This%Presence         =       ( len_trim(This%Command) /= 0 )                                                 ! Setting the presence indicator
End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

! REMARK:
! The Coordinates optional input argument must be a string with "<x-origin>,<y-origin>" where <x-origin> and <y-origin> are
! real numbers. If the Coordinates optional input argument is present, then the associated component is set to its value.
! If it is absent, then the code looks for the presence of the X_Coord and Y_Coord optional input arguments.
! If none of X_Coord and Y_Coord are present, then no coordinates is defined for the graph and the associated component
! is set to an empty string. If X_Coord or Y_Coord is present, then the graph origin coordinates are set by setting the absent
! argument for X_Coord or Y_Coord to one.
!
! @TODO: Check that the optional input argument corresponds to valid coordinates strings.
! @TODO: Make the X_Coord and Y_Coord variable real real numbers instead of characters.

Subroutine SetOriginCoordinates( This, X_Coord, Y_Coord, Coordinates )
  type(GPF_Origin_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Origin object
  character(*)                                ,optional ,intent(in)     ::  X_Coord                         !< X Coord
  character(*)                                ,optional ,intent(in)     ::  Y_Coord                         !< Y Coord
  character(*)                                ,optional ,intent(in)     ::  Coordinates                     !< Coordinates
  character(:)  ,allocatable                                            ::  X_Coord_Loc                     ! Local value for the graph X-Coord
  character(:)  ,allocatable                                            ::  Y_Coord_Loc                     ! Local value for the graph Y-Coord
  This%Coordinates      =       ''                                                                              ! Initialisation to an empty string: default value
  if ( present(Coordinates) ) then                                                                              ! If present "Coordinates" optional input argument
    This%Coordinates    =       Coordinates                                                                     ! Then setting the graph origin coordinates
  else                                                                                                          ! If absent "Coordinates" optional input argument, then checking if either the "X_Coord" or "Y_Coord" optional input arguments are present
    if ( present(X_Coord) .or. present(Y_Coord) ) then                                                          ! If either the "X_Coord" or "Y_Coord" optional input arguments are present, then setting the graph Coords according to input values
      X_Coord_Loc       =       "1"                                                                             ! Initializing the local X-Coord to one
      Y_Coord_Loc       =       "1"                                                                             ! Initializing the local Y-Coord to one
      if ( present(X_Coord) ) X_Coord_Loc = X_Coord                                                             ! Setting the local X-Coord to the optional input value if present
      if ( present(Y_Coord) ) Y_Coord_Loc = Y_Coord                                                             ! Setting the local Y-Coord to the optional input value if present
      This%Coordinates  =       X_Coord_Loc // ',' // Y_Coord_Loc                                               ! Setting the graph origin coordinates according to input values
    end if                                                                                                      ! End if case on X/Y Coords optional argument presence
  end if                                                                                                        ! End if case on Coords optional argument presence
  if ( len_trim(This%Coordinates) /= 0 ) This%Coordinates = This%Coordinates // " "                             ! Adding an extra space at the end of the string
End Subroutine

End SubModule