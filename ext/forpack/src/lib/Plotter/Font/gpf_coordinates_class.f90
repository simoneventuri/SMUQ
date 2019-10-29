Module GPF_Coordinates_Class

  use GPF_Parameters            ,only:  DbgUnit, rkp
  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private

  public  ::  GPF_Coordinates_Type

! ==============================================================================================================
!   COMMAND DESCRIPTION: COORDINATES
! ==============================================================================================================
! The commands set arrow, set key, set label and set object allow you to draw something at an arbitrary
! position on the graph. This position is specified by the syntax:
!       {<system>} <x>, {<system>} <y> {,{<system>} <z>}
! Each <system> can either be first, second, graph, screen, or character.
! first places the x, y, or z coordinate in the system defined by the left and bottom axes; second places it
! in the system defined by the second axes (top and right); graph specifies the area within the axes — 0,0
! is bottom left and 1,1 is top right (for splot, 0,0,0 is bottom left of plotting area; use negative z to get to
! the base — see set xyplane (p. 163)); screen specifies the screen area (the entire area — not just the
! portion selected by set size), with 0,0 at bottom left and 1,1 at top right; and character gives the position
! in character widths and heights from the bottom left of the screen area (screen 0,0), character coordinates
! depend on the chosen font size.
! If the coordinate system for x is not specified, first is used. If the system for y is not specified, the one used
! for x is adopted.
! In some cases, the given coordinate is not an absolute position but a relative value (e.g., the second position
! in set arrow ... rto). In most cases, the given value serves as difference to the first position. If the given
! coordinate resides in a logarithmic axis the value is interpreted as factor. For example,
!       set logscale x
!       set arrow 100,5 rto 10,2
! plots an arrow from position 100,5 to position 1000,7 since the x axis is logarithmic while the y axis is linear.
! If one (or more) axis is timeseries, the appropriate coordinate should be given as a quoted time string
! according to the timefmt format string. See set xdata (p. 157) and set timefmt (p. 154). Gnuplot
! will also accept an integer expression, which will be interpreted as seconds from 1 January 2000.
! ==============================================================================================================

  Type  ,extends(GPF_Command_Type)                      ::  GPF_Coordinates_Type
    private
    character(:)        ,allocatable                    ::  Coordinates                                     !< Coordinates values which corresponds to either and empty string (default) or to "{<system>} <x>, {<system>} <y> {,{<system>} <z>}" where x, y and z are real numbers
    character(:)        ,allocatable                    ::  X_Value                                         !< X coorindate value
    character(:)        ,allocatable                    ::  Y_Value                                         !< Y coorindate value
    character(:)        ,allocatable                    ::  Z_Value                                         !< Z coorindate value
    character(:)        ,allocatable                    ::  X_System                                        !< X coorindate system
    character(:)        ,allocatable                    ::  Y_System                                        !< Y coorindate system
    character(:)        ,allocatable                    ::  Z_System                                        !< Z coorindate system
  contains
    private
    procedure   ,public   ::  Update          =>  Update_Coordinates                      !< Updates the object components
    procedure             ::  Initialize      =>  Initialize_Coordinates                  !< Initializes the object components
    procedure             ::  Set_Values      =>  Set_Coordinates_Values                  !< Sets the coordinates values
    procedure             ::  Set_Systems     =>  Set_Coordinates_Systems                 !< Sets the coordinates systems
    procedure   ,public   ::  Set_Command     =>  Set_Coordinates_Command                 !< Sets the coordinates command
!   @COMPILER_BUG ? : GFORTRAN.5.3.1 : Erreur: ‘set_command’ at (1) overrides a PUBLIC procedure and must not be PRIVATE
!     procedure   ,public   ::  Set_Command     =>  Set_Coordinates_Command                 !< Sets the coordinates command
  End Type

  Interface             GPF_Coordinates_Type
    module procedure    Construct_Coordinates
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         CONSTRUCTOR PROCEDURES                                             *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Coordinates( Debug, Keyword, X_Value, Y_Value, Z_Value, X_System, Y_System, Z_System ) result(This)
  implicit none
  type(GPF_Coordinates_Type)                                            ::  This                            !< Coord object to be constructed
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
  character(*)                                ,optional ,intent(in)     ::  Keyword                         !< Keyword corresponding to the coordinates prefix
  real(rkp)                                   ,optional ,intent(in)     ::  X_Value                         !< X-coordinate value
  real(rkp)                                   ,optional ,intent(in)     ::  Y_Value                         !< Y-coordinate value
  real(rkp)                                   ,optional ,intent(in)     ::  Z_Value                         !< Z-coordinate value
  character(*)                                ,optional ,intent(in)     ::  X_System                        !< X coorindate system
  character(*)                                ,optional ,intent(in)     ::  Y_System                        !< Y coorindate system
  character(*)                                ,optional ,intent(in)     ::  Z_System                        !< Z coorindate system

  call This%Set_Debug( Debug )                                                                                  ! Setting the debugging indicator
  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Coordinates]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Coordinates]: Calling This%Initialize')")
  call This%Initialize()                                                                                        ! Initializing the object components to default values

  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Coordinates]: Calling This%Set_Values')")
  call This%Set_Values( X_Value, Y_Value, Z_Value )                                                             ! Setting the coordinates values

  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Coordinates]: Calling This%Set_Systems')")
  call This%Set_Systems( X_System, Y_System, Z_System )                                                         ! Setting the coordinates systems

  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Coordinates]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the keyword

  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Coordinates]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the command

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PUBLIC PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Update_Coordinates( This, Debug, Keyword, X_Value, Y_Value, Z_Value, X_System, Y_System, Z_System )

  implicit none

  class(GPF_Coordinates_Type)                           ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Coordinates object
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
  character(*)                                ,optional ,intent(in)     ::  Keyword                         !< Keyword corresponding to the coordinates prefix
  real(rkp)                                   ,optional ,intent(in)     ::  X_Value                         !< X-coordinate value
  real(rkp)                                   ,optional ,intent(in)     ::  Y_Value                         !< Y-coordinate value
  real(rkp)                                   ,optional ,intent(in)     ::  Z_Value                         !< Z-coordinate value
  character(*)                                ,optional ,intent(in)     ::  X_System                        !< X coorindate system
  character(*)                                ,optional ,intent(in)     ::  Y_System                        !< Y coorindate system
  character(*)                                ,optional ,intent(in)     ::  Z_System                        !< Z coorindate system

  call This%Set_Debug( Debug )                                                                                  ! Setting the debugging indicator
  if (This%i_Debug) write(DbgUnit,"(16x,'[Update_Coordinates]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(16x,'[Update_Coordinates]: Calling This%Set_Values')")
  call This%Set_Values( X_Value, Y_Value, Z_Value )                                                             ! Setting the coordinates values

  if (This%i_Debug) write(DbgUnit,"(16x,'[Update_Coordinates]: Calling This%Set_Systems')")
  call This%Set_Systems( X_System, Y_System, Z_System )                                                         ! Setting the coordinates systems

  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Coordinates]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the keyword

  if (This%i_Debug) write(DbgUnit,"(16x,'[Update_Coordinates]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the command

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_Coordinates]: Exiting',/)")

End Subroutine



! **************************************************************************************************************
! **************************************************************************************************************
!                                       PRIVATE PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Initialize_Coordinates( This )
  implicit none
  class(GPF_Coordinates_Type)                           ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Coordinates object
  if (This%i_Debug) write(DbgUnit,"(18x,'[Initialize_Coordinates]: Entering')")
  This%X_Value          =       ""                                                                              ! Initializing current coordinate value  to an empty string => Default value is 0
  This%Y_Value          =       ""                                                                              ! Initializing current coordinate value  to an empty string => Default value is 0
  This%Z_Value          =       ""                                                                              ! Initializing current coordinate value  to an empty string => Default value is 0
  This%X_System         =       ""                                                                              ! Initializing current coordinate system to an empty string => Implicit default value is "first"
  This%Y_System         =       ""                                                                              ! Initializing current coordinate system to an empty string => Implicit default value is "first"
  This%Z_System         =       ""                                                                              ! Initializing current coordinate system to an empty string => Implicit default value is "first"
  This%Command          =       ""                                                                              ! Initializing the command to an empty string
  if (This%i_Debug) write(DbgUnit,"(18x,'[Initialize_Coordinates]: Exiting',/)")
End Subroutine

! REMARK:
! The default Gnuplot behaviour is that whenever a given coordinate value if not specified, then its value is zero.
! Therefore, the associated character string are initialized to an empty string.
Subroutine Set_Coordinates_Values( This, X_Value, Y_Value, Z_Value )
  use GPF_Tools         ,only:  Convert_To_String, Remove_Trailing_Zeros
  implicit none
  class(GPF_Coordinates_Type)                           ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Coordinates object
  real(rkp)                                   ,optional ,intent(in)     ::  X_Value                         !< X-coordinate value
  real(rkp)                                   ,optional ,intent(in)     ::  Y_Value                         !< Y-coordinate value
  real(rkp)                                   ,optional ,intent(in)     ::  Z_Value                         !< Z-coordinate value
  if (This%i_Debug) write(DbgUnit,"(18x,'[Set_Coordinates_Values]: Entering')")
  if ( present(X_Value) ) This%X_Value = Remove_Trailing_Zeros( Convert_To_String( X_Value ) )                  ! If present optional input argument, then setting current coordinate (converting the real number into a string and removing the trailing zeros)
  if ( present(Y_Value) ) This%Y_Value = Remove_Trailing_Zeros( Convert_To_String( Y_Value ) )                  ! If present optional input argument, then setting current coordinate (converting the real number into a string and removing the trailing zeros)
  if ( present(Z_Value) ) This%Z_Value = Remove_Trailing_Zeros( Convert_To_String( Z_Value ) )                  ! If present optional input argument, then setting current coordinate (converting the real number into a string and removing the trailing zeros)
  if (This%i_Debug) then
    write(DbgUnit,"(18x,'[Set_Coordinates_Values]: This%X_Value = ',a)") This%X_Value
    write(DbgUnit,"(18x,'[Set_Coordinates_Values]: This%Y_Value = ',a)") This%Y_Value
    write(DbgUnit,"(18x,'[Set_Coordinates_Values]: This%Z_Value = ',a)") This%Z_Value
    write(DbgUnit,"(18x,'[Set_Coordinates_Values]: Exiting',/)")
  end if
End Subroutine

! REMARK:
! The default Gnuplot behaviour is that whenever a given coordinate systemt, the "first" system is used.
! If the system for y is not specified, the one used for x is adopted.
Subroutine Set_Coordinates_Systems( This, X_System, Y_System, Z_System )
  use GPF_Parameters            ,only:  Coordinates_System_Valid
  use GPF_Tools                 ,only:  Check_Validity
  implicit none
  class(GPF_Coordinates_Type)                           ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Coordinates object
  character(*)                                ,optional ,intent(in)     ::  X_System                        !< X coorindate system
  character(*)                                ,optional ,intent(in)     ::  Y_System                        !< Y coorindate system
  character(*)                                ,optional ,intent(in)     ::  Z_System                        !< Z coorindate system
  if (This%i_Debug) write(DbgUnit,"(18x,'[Set_Coordinates_Systems]: Entering')")
  if ( present(X_System) )  This%X_System = Check_Validity( X_System, Coordinates_System_Valid ) // " "         ! If present optional input argument, then checking and setting the current coordinate system to the input value (validity check)
  if ( present(Y_System) )  This%Y_System = Check_Validity( Y_System, Coordinates_System_Valid ) // " "         ! If present optional input argument, then checking and setting the current coordinate system to the input value (validity check)
  if ( present(Z_System) )  This%Z_System = Check_Validity( Z_System, Coordinates_System_Valid ) // " "         ! If present optional input argument, then checking and setting the current coordinate system to the input value (validity check)
  if (This%i_Debug) then
    write(DbgUnit,"(18x,'[Set_Coordinates_Systems]: This%X_System = ',a)") This%X_System
    write(DbgUnit,"(18x,'[Set_Coordinates_Systems]: This%Y_System = ',a)") This%Y_System
    write(DbgUnit,"(18x,'[Set_Coordinates_Systems]: This%Z_System = ',a)") This%Z_System
    write(DbgUnit,"(18x,'[Set_Coordinates_Systems]: Exiting',/)")
  end if
End Subroutine

! @TODO: their might be a problem if X and Z are specified, but not Y. We won't have "X,,Z" but "X,Z" which is wrong
Subroutine Set_Coordinates_Command( This )
  implicit none
  class(GPF_Coordinates_Type)                           ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Coordinates object
  if (This%i_Debug) write(DbgUnit,"(18x,'[Set_Coordinates_Command]: Entering')")
  This%Command          =       ''                                                                              ! Initializing the command to an empty string
  if ( len_trim(This%X_Value) /= 0 ) This%Command =                        This%X_System // This%X_Value        ! Setting the command related to the X coordinate
  if ( len_trim(This%Y_Value) /= 0 ) This%Command = This%Command // "," // This%Y_System // This%Y_Value        ! Setting the command related to the Y coordinate
  if ( len_trim(This%Z_Value) /= 0 ) This%Command = This%Command // "," // This%Z_System // This%Z_Value        ! Setting the command related to the Z coordinate
  if ( len_trim(This%Command) /= 0 ) This%Command = This%Keyword // This%Command // " "                         ! Setting the command keyword
  This%Presence         =       ( len_trim(This%Command) /= 0 )                                                 ! Setting the presence indicator if required
  if (This%i_Debug) then
    write(DbgUnit,"(18x,'[Set_Coordinates_Command]: This%Command  = ',a )") This%Command
    write(DbgUnit,"(18x,'[Set_Coordinates_Command]: Exiting',/)")
  end if
End Subroutine

End Module