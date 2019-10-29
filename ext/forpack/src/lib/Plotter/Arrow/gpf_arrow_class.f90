Module GPF_Arrow_Class

  use GPF_Parameters            ,only:  DbgUnit, rkp
  use GPF_Command_Class         ,only:  GPF_Command_Type

  use GPF_Coordinates_Class     ,only:  GPF_Coordinates_Type

  implicit none

  private

  public  ::  GPF_Arrow_Type
  public  ::  Construct_Arrow


! ==============================================================================================================
!   COMMAND DESCRIPTION: ARROW
! ==============================================================================================================
! Arbitrary arrows can be placed on a plot using the set arrow command.
! Syntax:
!       set arrow {<tag>} {from <position>} {to|rto <position>}
!                 { {arrowstyle | as <arrow_style>}
!                   | { {nohead | head | backhead | heads}
!                     {size <length>,<angle>{,<backangle>}}
!                     {filled | empty | nofilled}
!                     {front | back}
!                     { {linestyle | ls <line_style>}
!                       | {linetype | lt <line_type>}
!                         {linewidth | lw <line_width} } } }
!       unset arrow {<tag>}
!       show arrow {<tag>}
! <tag> is an integer that identifies the arrow. If no tag is given, the lowest unused tag value is assigned
! automatically. The tag can be used to delete or change a specific arrow. To change any attribute of an
! existing arrow, use the set arrow command with the appropriate tag and specify the parts of the arrow to
! be changed.
! The <position>s are specified by either x,y or x,y,z, and may be preceded by first, second, graph, screen,
! or character to select the coordinate system. Unspecified coordinates default to 0. The end points can
! be specified in one of five coordinate systems — first or second axes, graph, screen, or character. See
! coordinates (p. 22) for details. A coordinate system specifier does not carry over from the "from" position
! to the "to" position. Arrows outside the screen boundaries are permitted but may cause device errors. If the
! end point is specified by "rto" instead of "to" it is drawn relatively to the start point. For linear axes, graph
! and screen coordinates, the distance between the start and the end point corresponds to the given relative
! coordinate. For logarithmic axes, the relative given coordinate corresponds to the factor of the coordinate
! between start and end point. Thus, a negative relative value or zero are not allowed for logarithmic axes.
! Specifying nohead produces an arrow drawn without a head — a line segment. This gives you yet another
! way to draw a line segment on the plot. By default, an arrow has a head at its end. Specifying backhead
! draws an arrow head at the start point of the arrow while heads draws arrow heads on both ends of the
! line. Not all terminal types support double-ended arrows.
! Head size can be controlled by size <length>,<angle> or size <length>,<angle>,<backangle>, where
! <length> defines length of each branch of the arrow head and <angle> the angle (in degrees) they make
! with the arrow. <Length> is in x-axis units; this can be changed by first, second, graph, screen, or
! character before the <length>; see coordinates (p. 22) for details. <Backangle> only takes effect when
! filled or empty is also used. Then, <backangle> is the angle (in degrees) the back branches make with the
! arrow (in the same direction as <angle>). The fig terminal has a restricted backangle function. It supports
! three different angles. There are two thresholds: Below 70 degrees, the arrow head gets an indented back
! angle. Above 110 degrees, the arrow head has an acute back angle. Between these thresholds, the back line
! is straight.
! Specifying filled produces filled arrow heads (if heads are used). Filling is supported on filled-polygon
! capable terminals, see help of pm3d (p. 134) for their list, otherwise the arrow heads are closed but not
! filled. The same result (closed but not filled arrow head) is reached by specifying empty. Further, filling and
! outline is obviously not supported on terminals drawing arrows by their own specific routines, like metafont,
! metapost, latex or tgif.
! The line style may be selected from a user-defined list of line styles (see set style line (p. 149)) or
! may be defined here by providing values for <line type> (an index from the default list of styles) and/or
! <line width> (which is a multiplier for the default width).
! Note, however, that if a user-defined line style has been selected, its properties (type and width) cannot be
! altered merely by issuing another set arrow command with the appropriate index and lt or lw.
! If front is given, the arrow is written on top of the graphed data. If back is given (the default), the arrow
! is written underneath the graphed data. Using front will prevent an arrow from being obscured by dense
! data.
! Examples:
! To set an arrow pointing from the origin to (1,2) with user-defined style 5, use:
!       set arrow to 1,2 ls 5
! To set an arrow from bottom left of plotting area to (-5,5,3), and tag the arrow number 3, use:
!       set arrow 3 from graph 0,0 to -5,5,3
! To change the preceding arrow to end at 1,1,1, without an arrow head and double its width, use:
!       set arrow 3 to 1,1,1 nohead lw 2
! To draw a vertical line from the bottom to the top of the graph at x=3, use:
!       set arrow from 3, graph 0 to 3, graph 1 nohead
! To draw a vertical arrow with T-shape ends, use:
!       set arrow 3 from 0,-5 to 0,5 heads size screen 0.1,90
! To draw an arrow relatively to the start point, where the relative distances are given in graph coordinates,
! use:
!       set arrow from 0,-5 rto graph 0.1,0.1
! To draw an arrow with relative end point in logarithmic x axis, use:
!       set logscale x
!       set arrow from 100,-5 rto 10,10
! This draws an arrow from 100,-5 to 1000,5. For the logarithmic x axis, the relative coordinate 10 means
! "factor 10" while for the linear y axis, the relative coordinate 10 means "difference 10".
! To delete arrow number 2, use:
!       unset arrow 2
! To delete all arrows, use:
!       unset arrow
! To show all arrows (in tag order), use:
!       show arrow
! ==============================================================================================================

!     character(:)        ,allocatable                    ::  Style                                           !< Arrow's style
!     character(:)        ,allocatable                    ::  Size_                                           !< Size of arrow's head (<length>,<angle>{,<backangle>})
!     character(:)        ,allocatable                    ::  Filled                                          !< Arrow's filled type (filled | empty | nofilled)
!     logical                                             ::  i_
!     character(:)        ,allocatable                    ::  LineStyle                                       !< Arrow's line style
!     character(:)        ,allocatable                    ::  LineType                                        !< Arrow's line type
!     character(:)        ,allocatable                    ::  LineWidth                                        !< Arrow's line width

  Type  ,extends(GPF_Command_Type)                      ::  GPF_Arrow_Type
    private
    integer                                             ::  Index                                           !< Arrow index
    type(GPF_Coordinates_Type)                          ::  From                                            !< Coordinates of the arrow starting point
    type(GPF_Coordinates_Type)                          ::  To                                              !< Coordinates of the arrow ending point
    character(:)        ,allocatable                    ::  Head                                            !< Type of arrow's head (either "", "nohead", "head", "backhead" or "heads")
    character(:)        ,allocatable                    ::  Front                                           !< Indicator whether the arrow is on top (true) or underneath the graph data (false, default)
    logical                                             ::  UnSetting                                       !< Unsetting indicator
  contains
    private
    procedure   ,public   ::  Update          =>  Update_Arrow                                    !< Updates the object components
    procedure             ::  Initialize      =>  Initialize_Arrow                                !< Initializes the object components
    procedure             ::  Set_Index       =>  Set_Arrow_Index                                 !< Sets the arrow's index
    procedure             ::  Set_From        =>  Set_Arrow_From                                  !< Sets the arrow's starting point coordinates
    procedure             ::  Set_To          =>  Set_Arrow_To                                    !< Sets the arrow's ending point coordinates
    procedure             ::  Set_Head        =>  Set_Arrow_Head                                  !< Sets the arrow's head
    procedure             ::  Set_Front       =>  Set_Arrow_Front                                 !< Sets the arrow's position
    procedure             ::  Set_Unsetting   =>  Set_Arrow_Unsetting                             !< Sets the arrow's unsetting indicator
    procedure   ,public   ::  Set_Command     =>  Set_Arrow_Command
  End Type

!   Interface             GPF_Arrow_Type
!     Module Procedure    Construct_Arrow
!   End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         CONSTRUCTOR PROCEDURES                                             *
! **************************************************************************************************************
! **************************************************************************************************************

! @TODO: These arguments are to be implemented
!   character(*)                                ,optional ,intent(in)     ::  Style                           !< Arrow's style
!   character(*)                                ,optional ,intent(in)     ::  Size_                           !< Size of arrow's head (<length>,<angle>{,<backangle>})
!   character(*)                                ,optional ,intent(in)     ::  Filled                          !< Arrow's filled type (filled | empty | nofilled)
!   character(*)                                ,optional ,intent(in)     ::  LineStyle                       !< Arrow's line style
!   character(*)                                ,optional ,intent(in)     ::  LineType                        !< Arrow's line type
!   character(*)                                ,optional ,intent(in)     ::  LineWidth                       !< Arrow's line width
! , To_, Style, Size_, Filled, i_Front, LineStyle, LineType, LineWidth

Function Construct_Arrow( Debug, Index,                                                 &
                From_X, From_Y, From_Z, From_X_System, From_Y_System, From_Z_System,    &
                To_X,   To_Y,   To_Z,   To_X_System,   To_Y_System,   To_Z_System,      &
                Head, i_Front, UnSetting                                                  ) result(This)

  implicit none

  type(GPF_Arrow_Type)                                                  ::  This                            !< Arrow object to be constructed
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
  integer                                     ,optional ,intent(in)     ::  Index                           !< Arrow index
! Arguments related to the From object
  real(rkp)                                   ,optional ,intent(in)     ::  From_X                          !< X-coordinate value of the arrow starting point
  real(rkp)                                   ,optional ,intent(in)     ::  From_Y                          !< Y-coordinate value of the arrow starting point
  real(rkp)                                   ,optional ,intent(in)     ::  From_Z                          !< Z-coordinate value of the arrow starting point
  character(*)                                ,optional ,intent(in)     ::  From_X_System                   !< X-coordinate system of the arrow starting point
  character(*)                                ,optional ,intent(in)     ::  From_Y_System                   !< Y-coordinate system of the arrow starting point
  character(*)                                ,optional ,intent(in)     ::  From_Z_System                   !< Z-coordinate bsystem of the arrow starting point
! Arguments related to the To object
  real(rkp)                                   ,optional ,intent(in)     ::  To_X                            !< X-coordinate value of the arrow ending point
  real(rkp)                                   ,optional ,intent(in)     ::  To_Y                            !< Y-coordinate value of the arrow ending point
  real(rkp)                                   ,optional ,intent(in)     ::  To_Z                            !< Z-coordinate value of the arrow ending point
  character(*)                                ,optional ,intent(in)     ::  To_X_System                     !< X-coordinate system of the arrow ending point
  character(*)                                ,optional ,intent(in)     ::  To_Y_System                     !< Y-coordinate system of the arrow ending point
  character(*)                                ,optional ,intent(in)     ::  To_Z_System                     !< Z-coordinate bsystem of the arrow ending point

  character(*)                                ,optional ,intent(in)     ::  Head                            !< Type of arrow's head (either "", "nohead", "head", "backhead" or "heads")
  logical                                     ,optional ,intent(in)     ::  i_Front                         !< Indicator whether the arrow is on top (true) or underneath the graph data (false, default)
  logical                                     ,optional ,intent(in)     ::  UnSetting                       !< Arrow unsetting indicator

  character(*)                                              ,parameter  ::  Keyword='arrow'

  call This%Set_Debug( Debug )                                                                                  ! Setting the debugging indicator
  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Arrow]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Arrow]: Calling This%Initialize')")
  call This%Initialize()                                                                                        ! Initializing the object components to default values

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Arrow]: Calling This%Set_Index')")
  call This%Set_Index( Index )                                                                                  ! Setting the arrow's starting point coordinates

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Arrow]: Calling This%Set_From')")
  call This%Set_From( From_X, From_Y, From_Z, From_X_System, From_Y_System, From_Z_System )                     ! Setting the arrow's starting point coordinates

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Arrow]: Calling This%Set_To')")
  call This%Set_To( To_X, To_Y, To_Z, To_X_System, To_Y_System, To_Z_System )                                   ! Setting the arrow's ending point coordinates

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Arrow]: Calling This%Set_Head')")
  call This%Set_Head( Head )                                                                                    ! Setting the arrow's head

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Arrow]: Calling This%Set_Front')")
  call This%Set_Front( i_Front )                                                                                ! Setting the arrow's front indicator

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Arrow]: Calling This%Set_Unsetting')")
  call This%Set_Unsetting( UnSetting )                                                                          ! Setting the arrow's unsetting indicator

! **********************************************
! ... More components to come ...
! **********************************************

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Arrow]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the command keyword

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Arrow]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the command string

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Arrow]: Exiting',/)")

End Function


! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PUBLIC PROCEDURES                                                  *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Update_Arrow( This, Debug,                                                   &
                From_X, From_Y, From_Z, From_X_System, From_Y_System, From_Z_System,    &
                To_X,   To_Y,   To_Z,   To_X_System,   To_Y_System,   To_Z_System,      &
                Head, i_Front, UnSetting                                                )

  implicit none

  class(GPF_Arrow_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Arrow object
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
! Arguments related to the From object
  real(rkp)                                   ,optional ,intent(in)     ::  From_X                          !< X-coordinate value of the arrow starting point
  real(rkp)                                   ,optional ,intent(in)     ::  From_Y                          !< Y-coordinate value of the arrow starting point
  real(rkp)                                   ,optional ,intent(in)     ::  From_Z                          !< Z-coordinate value of the arrow starting point
  character(*)                                ,optional ,intent(in)     ::  From_X_System                   !< X-coordinate system of the arrow starting point
  character(*)                                ,optional ,intent(in)     ::  From_Y_System                   !< Y-coordinate system of the arrow starting point
  character(*)                                ,optional ,intent(in)     ::  From_Z_System                   !< Z-coordinate bsystem of the arrow starting point
! Arguments related to the To object
  real(rkp)                                   ,optional ,intent(in)     ::  To_X                            !< X-coordinate value of the arrow ending point
  real(rkp)                                   ,optional ,intent(in)     ::  To_Y                            !< Y-coordinate value of the arrow ending point
  real(rkp)                                   ,optional ,intent(in)     ::  To_Z                            !< Z-coordinate value of the arrow ending point
  character(*)                                ,optional ,intent(in)     ::  To_X_System                     !< X-coordinate system of the arrow ending point
  character(*)                                ,optional ,intent(in)     ::  To_Y_System                     !< Y-coordinate system of the arrow ending point
  character(*)                                ,optional ,intent(in)     ::  To_Z_System                     !< Z-coordinate bsystem of the arrow ending point

  character(*)                                ,optional ,intent(in)     ::  Head                            !< Type of arrow's head (either "", "nohead", "head", "backhead" or "heads")
  logical                                     ,optional ,intent(in)     ::  i_Front                         !< Indicator whether the arrow is on top (true) or underneath the graph data (false, default)
  logical                                     ,optional ,intent(in)     ::  UnSetting                       !< Arrow unsetting indicator

  call This%Set_Debug( Debug )                                                                                  ! Setting the debugging indicator
  if (This%i_Debug) write(DbgUnit,"(10x,'[Update_Arrow]: Entering')")

!   if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Arrow]: Calling This%Set_Index')")
!   call This%Set_Index( Index )                                                                                  ! Setting the arrow's starting point coordinates

  if (This%i_Debug) write(DbgUnit,"(10x,'[Update_Arrow]: Calling This%Set_From')")
  call This%Set_From( From_X, From_Y, From_Z, From_X_System, From_Y_System, From_Z_System )                     ! Setting the arrow's starting point coordinates

  if (This%i_Debug) write(DbgUnit,"(10x,'[Update_Arrow]: Calling This%Set_To')")
  call This%Set_To( To_X, To_Y, To_Z, To_X_System, To_Y_System, To_Z_System )                                   ! Setting the arrow's ending point coordinates

  if (This%i_Debug) write(DbgUnit,"(10x,'[Update_Arrow]: Calling This%Set_Head')")
  call This%Set_Head( Head )                                                                                    ! Setting the arrow's head

  if (This%i_Debug) write(DbgUnit,"(10x,'[Update_Arrow]: Calling This%Set_Front')")
  call This%Set_Front( i_Front )                                                                                ! Setting the arrow's front indicator

  if (This%i_Debug) write(DbgUnit,"(10x,'[Update_Arrow]: Calling This%Set_Unsetting')")
  call This%Set_Unsetting( UnSetting )                                                                          ! Setting the arrow's unsetting indicator

  if (This%i_Debug) write(DbgUnit,"(10x,'[Update_Arrow]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the command string

  if (This%i_Debug) write(DbgUnit,"(10x,'[Update_Arrow]: Exiting',/)")

End Subroutine




! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Initialize_Arrow( This )
  implicit none
  class(GPF_Arrow_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Arrow object
  if (This%i_Debug) write(DbgUnit,"(12x,'[Initialize_Arrow]: Entering')")
  This%From             =       GPF_Coordinates_Type()                                                          ! Initializing the From component
  This%To               =       GPF_Coordinates_Type()                                                          ! Initializing the To component
  This%Head             =       ''                                                                              ! Initializing to an empty string: default value
  This%Front            =       ''                                                                              ! Initializing to an empty string: default value => arrow at the back
  This%UnSetting        =       .False.                                                                         ! Initializing the unsetting indicator to false so that the "unset arrow" command is not written by default
  This%Command          =       ''                                                                              ! Initializing the command to an empty string: default value => no Arrow
  if (This%i_Debug) write(DbgUnit,"(12x,'[Initialize_Arrow]: Exiting',/)")
End Subroutine

Subroutine Set_Arrow_Index( This, Index )
  implicit none
  class(GPF_Arrow_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Arrow object
  integer                                     ,optional ,intent(in)     ::  Index                           !< Arrow index
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Arrow_From]: Entering')")
  if ( present(Index) ) then
    if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Arrow_From]: Index = ',g0)") Index
    if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Arrow_From]: <WARNING> Feature not implemented')")
  end if
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Arrow_From]: Exiting',/)")
End Subroutine

Subroutine Set_Arrow_From( This, From_X, From_Y, From_Z, From_X_System, From_Y_System, From_Z_System )
  use GPF_Parameters            ,only:  Keyword => c_from_
  implicit none
  class(GPF_Arrow_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Arrow object
  real(rkp)                                   ,optional ,intent(in)     ::  From_X                          !< X-coordinate value of the arrow starting point
  real(rkp)                                   ,optional ,intent(in)     ::  From_Y                          !< Y-coordinate value of the arrow starting point
  real(rkp)                                   ,optional ,intent(in)     ::  From_Z                          !< Z-coordinate value of the arrow starting point
  character(*)                                ,optional ,intent(in)     ::  From_X_System                   !< X-coordinate system of the arrow starting point
  character(*)                                ,optional ,intent(in)     ::  From_Y_System                   !< Y-coordinate system of the arrow starting point
  character(*)                                ,optional ,intent(in)     ::  From_Z_System                   !< Z-coordinate bsystem of the arrow starting point
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Arrow_From]: Entering')")
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Arrow_From]: Calling GPF_Coordinates_Type')")
  call This%From%Update( This%i_Debug, Keyword, From_X, From_Y, From_Z, From_X_System, From_Y_System, From_Z_System ) ! Constructing the Coordinate object
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_Arrow_From]: This%From%Command = ',a)") This%From%Command
    write(DbgUnit,"(12x,'[Set_Arrow_From]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Arrow_To( This, To_X, To_Y, To_Z, To_X_System, To_Y_System, To_Z_System )
  use GPF_Parameters            ,only:  Keyword => c_to_
  implicit none
  class(GPF_Arrow_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Arrow object
  real(rkp)                                   ,optional ,intent(in)     ::  To_X                            !< X-coordinate value of the arrow starting point
  real(rkp)                                   ,optional ,intent(in)     ::  To_Y                            !< Y-coordinate value of the arrow starting point
  real(rkp)                                   ,optional ,intent(in)     ::  To_Z                            !< Z-coordinate value of the arrow starting point
  character(*)                                ,optional ,intent(in)     ::  To_X_System                     !< X-coordinate system of the arrow starting point
  character(*)                                ,optional ,intent(in)     ::  To_Y_System                     !< Y-coordinate system of the arrow starting point
  character(*)                                ,optional ,intent(in)     ::  To_Z_System                     !< Z-coordinate bsystem of the arrow starting point
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Arrow_To]: Entering')")
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Arrow_To]: Calling GPF_Coordinates_Type')")
  call This%To%Update( This%i_Debug, Keyword, To_X, To_Y, To_Z, To_X_System, To_Y_System, To_Z_System )         ! Constructing the Coordinate object
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_Arrow_To]: This%To%Command = ',a)") This%To%Command
    write(DbgUnit,"(12x,'[Set_Arrow_To]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Arrow_Head( This, Head )
  use GPF_Parameters            ,only:  Arrow_Head_Valid
  use GPF_Tools                 ,only:  Check_Validity
  implicit none
  class(GPF_Arrow_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Arrow object
  character(*)                                ,optional ,intent(in)     ::  Head                            !< Type of arrow's head (either "", "nohead", "head", "backhead" or "heads")
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Arrow_Head]: Entering')")
  if ( present(Head) ) This%Head = Check_Validity( Head, Arrow_Head_Valid ) // " "                              ! If present optional input argument, then checking and setting the arrow's head
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_Arrow_Head]: This%Head = ',a)") This%Head
    write(DbgUnit,"(12x,'[Set_Arrow_Head]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Arrow_Front( This, i_Front )
  use GPF_Parameters            ,only:  c_front_, c_back_
  implicit none
  class(GPF_Arrow_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Arrow object
  logical                                     ,optional ,intent(in)     ::  i_Front                         !< Indicator whether the arrow is on top (true) or underneath the graph data (false, default)
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Arrow_Front]: Entering')")
  if ( present(i_Front) ) then                                                                                  ! If present optional input argument
    if (i_Front) then;    This%Front = c_front_                                                                 ! If true, then setting the associated keyword
    else;               This%Front = c_back_                                                                    ! If false, then setting the associated keyword
    end if                                                                                                      ! End if case on front indicator
  end if                                                                                                        ! End if case optional input argument presence
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_Arrow_Front]: This%Front = ',a)") This%Front
    write(DbgUnit,"(12x,'[Set_Arrow_Front]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Arrow_Unsetting( This, UnSetting )
  implicit none
  class(GPF_Arrow_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Arrow object
  logical                                     ,optional ,intent(in)     ::  UnSetting                       !< Unsetting indicator
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Arrow_Unsetting]: Entering')")
  if ( present(UnSetting) ) This%UnSetting = UnSetting                                                          ! If present optional input argument, then setting the component value to the input value
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_Arrow_Unsetting]: This%UnSetting = ',l3)") This%UnSetting
    write(DbgUnit,"(12x,'[Set_Arrow_Unsetting]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Arrow_Command( This )
  implicit none
  class(GPF_Arrow_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Arrow object
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Arrow_Command]: Entering')")
  This%Command          =       ''                                                                              ! Initializing the command to an empty string: default value => no Arrow
  if ( len_trim(This%From%Command) /= 0 )       This%Command = This%Command // This%From%Command                ! Setting the Arrow
  if ( len_trim(This%To%Command)   /= 0 )       This%Command = This%Command // This%To%Command                  ! Setting the Arrow
  if ( len_trim(This%Head)         /= 0 )       This%Command = This%Command // This%Head                        ! Setting the Arrow
  if ( len_trim(This%Front)        /= 0 )       This%Command = This%Command // This%Front                       ! Setting the Arrow
  if ( len_trim(This%Command)      /= 0 )       This%Command = 'set ' // This%Keyword // This%Command           ! Setting the command keyword
  if ( This%UnSetting ) This%Command = 'unset ' // This%Keyword ! // This%Index
  This%Presence         =       ( len_trim(This%Command) /= 0 )                                                 ! Setting the presence indicator
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_Arrow_Command]: This%Command  = ',a )") This%Command
    write(DbgUnit,"(12x,'[Set_Arrow_Command]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(12x,'[Set_Arrow_Command]: Exiting',/)")
  end if
End Subroutine

End Module