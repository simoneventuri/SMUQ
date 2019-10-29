
! @TODO: Write the Check_Uniticty for the 1D constructor

Module GPF_LineStyle_Class

  use GPF_Parameters            ,only:  DbgUnit, i_Debug_Default
  use GPF_Command_Class         ,only:  GPF_Command_Type
  use GPF_Class                 ,only:  GPF

  use GPF_LineStyle_PointType_Class     ,only:  GPF_LineStyle_PointType_Type
  use GPF_LineStyle_PointSize_Class     ,only:  GPF_LineStyle_PointSize_Type
  use GPF_LineStyle_PointInterval_Class,only:   GPF_LineStyle_PointInterval_Type

  implicit none

  private

  public  ::  GPF_LineStyle_Type
  public  ::  LS_Global_Units
  public  ::  Construct_LineStyle

  type  ,extends(GPF_Command_Type)                      ::  GPF_LineStyle_Type
    private
    character(:)        ,allocatable                    ::  Object                                          !< LineStyle Object
    character(:)        ,allocatable                    ::  Unit                                            !< LineStyle Unit
    character(:)        ,allocatable                    ::  DashType                                        !< LineStyle dash-type
    character(:)        ,allocatable                    ::  Width                                           !< LineStyle Width
    character(:)        ,allocatable                    ::  Color                                           !< LineStyle Color
    type(GPF_LineStyle_PointType_Type)                  ::  PointType                                       !< LineStyle PointType object
    type(GPF_LineStyle_PointSize_Type)                  ::  PointSize                                       !< LineStyle PointSize object
    type(GPF_LineStyle_PointInterval_Type)              ::  PointInterval                                   !< LineStyle PointInterval object
    logical     ,public                                 ::  i_Multiple_Lines        =       .False.         !< Indicator that current LineStyle object is used for multiple lines (used for "line" object when the color line is given by a palette)
    integer     ,public                                 ::  iLine_Ini               =       0
    integer     ,public                                 ::  iLine_Fin               =       0
  contains
    private
    procedure   ,public   ::  Write           =>  Write_LineStyle                 !< Writing LineStyle Command
    procedure   ,public   ::  Get_Color                                               !< Gets the LineStyle color
    procedure   ,public   ::  Get_Unit        =>  Get_LineStyle_Unit              !< Gets the LineStyle unit
    procedure   ,public   ::  Is_PointType_Specified
    procedure   ,public   ::  Is_PointSize_Specified
    procedure   ,public   ::  Is_PointInterval_Specified
    procedure   ,private  ::  Construct       =>  Construct_LineStyle_0D
    procedure             ::  Set_Object      =>  Set_LineStyle_Object            !< Sets the LineStyle Object
    procedure   ,public   ::  Set_Command_Prefix
    procedure             ::  Set_Unit        =>  Set_LineStyle_Unit              !< Sets the LineStyle Unit
    procedure             ::  SetDashType     =>  SetLineStyleDashType            !< Sets the LineStyle dash-type
    procedure             ::  Set_LineWidth   =>  Set_LineStyle_LineWidth         !< Sets the LineStyle Width
    procedure             ::  Set_LineColor   =>  Set_LineStyle_LineColor         !< Sets the LineStyle Color
    procedure             ::  Set_PointType   =>  Set_LineStyle_PointType         !< Sets the LineStyle PointType
    procedure             ::  Set_PointSize   =>  Set_LineStyle_PointSize         !< Sets the LineStyle PointSize
    procedure             ::  Set_PointInterval =>    Set_LineStyle_PointInterval     !< Sets the LineStyle PointInterval
    procedure             ::  Get_Integer_Unit                                        !< Gets the LineStyle Unit as an interger variable
    procedure   ,public   ::  Set_Command     =>  Set_LineStyle_Command           !< Sets the LineStyle command
  End type

! ! Erreur: In generic interface 'gpf_linestyle_type' at (1) procedures must be all FUNCTIONs as the generic name is also the name of a derived type
!   Interface             GPF_LineStyle_Type
! !     Module Procedure    Construct_LineStyle_0D
!     Module Procedure    Construct_LineStyle_1D
!   End Interface


! Erreur: In generic interface 'gpf_linestyle_type' at (1) procedures must be all FUNCTIONs as the generic name is also the name of a derived type
  Interface             Construct_LineStyle
!     Module Procedure    Construct_LineStyle_0D
    Module Procedure    Construct_LineStyle_1D
  End Interface

  Interface             Free_Global_Units
    Module Procedure    Free_Global_Units_0D, Free_Global_Units_1D
  End Interface



  integer       ,dimension(:)   ,allocatable            ::  LS_Global_Units

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                                                                                            *
! *                                         PUBLIC PROCEDURES                                                  *
! *                                                                                                            *
! **************************************************************************************************************
! **************************************************************************************************************

! Function Construct_LineStyle_0D( Object, Unit, Type, Width, Color, PointType, PointSize, PointInterval, Debug ) result(LineStyle)
Subroutine Construct_LineStyle_0D( This, Object, Unit, DashType, Width, Color, PointType, PointSize, PointInterval, Debug )

  class(GPF_LineStyle_Type)                             ,intent(out)    ::  This                            !< LineStyle structure
  character(*)                                ,optional ,intent(in)     ::  Object                          !< LineStyle Object
  integer                                     ,optional ,intent(in)     ::  Unit                            !< LineStyle Unit
  character(*)                                ,optional ,intent(in)     ::  DashType                        !< LineStyle Type                         !< LineStyle Type
  character(*)                                ,optional ,intent(in)     ::  Width                           !< LineStyle Width
  character(*)                                ,optional ,intent(in)     ::  Color                           !< LineStyle color
  character(*)                                ,optional ,intent(in)     ::  PointType                       !< LineStyle point-type
  character(*)                                ,optional ,intent(in)     ::  PointSize                       !< LineStyle point-size
  character(*)                                ,optional ,intent(in)     ::  PointInterval                   !< LineStyle PointInterval
  logical                                     ,optional ,intent(in)     ::  Debug                           !< LineStyle Debug indicator

  call This%Set_Debug( Debug )                                                                                  ! Setting debugging indicator
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_0D]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_0D]: Calling This%Set_Object')")
  call This%Set_Object( Object )                                                                           ! Setting LineStyle parent object

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_0D]: Calling This%Set_Command_Prefix')")
  call This%Set_Command_Prefix( )                                                                          ! Setting LineStyle's command prefix

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_0D]: Calling This%Set_Unit')")
  call This%Set_Unit( Unit )                                                                               ! Setting LineStyle unit number

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_0D]: Calling This%SetDashType')")
  call This%SetDashType( DashType )                                                                           ! Setting LineStyle type

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_0D]: Calling This%Set_LineWidth')")
  call This%Set_LineWidth( Width )                                                                         ! Setting LineStyle width

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_0D]: Calling This%Set_LineColor')")
  call This%Set_LineColor( Color )                                                                         ! Setting LineStyle color

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_0D]: Calling This%Set_PointType')")
  call This%Set_PointType( PointType )                                                                     ! Setting LineStyle PointType

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_0D]: Calling This%Set_PointSize')")
  call This%Set_PointSize( PointSize )                                                                     ! Setting LineStyle PointSize

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_0D]: Calling This%Set_PointInterval')")
  call This%Set_PointInterval( PointInterval )                                                             ! Setting LineStyle PointInterval

!   if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_0D]: Calling This%Set_Command')")
!   call This%Set_Command()                                                                                  ! Setting LineStyle command

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_0D]: This%Command = ', a)") This%Command

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_0D]: Exiting')")

End Subroutine

Subroutine Construct_LineStyle_1D( LineStyle, NLine, DashType, Width, Color, PointType, PointSize, PointInterval, Debug )

  use GPF_Parameters            ,only:  Object_Loc => LineStyle_Object_Line


  type(GPF_LineStyle_Type) ,dimension(:) ,allocatable   ,intent(out)    ::  LineStyle                       !< LineStyle structure
  integer                                               ,intent(in)     ::  NLine                           !< Number of lines
  character(*)  ,dimension(:)   ,target       ,optional ,intent(in)     ::  DashType                        !< LineStyle Type
  character(*)  ,dimension(:)   ,target       ,optional ,intent(in)     ::  Width                           !< LineStyle Width
  character(*)  ,dimension(:)   ,target       ,optional ,intent(in)     ::  Color                           !< LineStyle Color
  character(*)  ,dimension(:)   ,target       ,optional ,intent(in)     ::  PointType                       !< LineStyle PointType
  character(*)  ,dimension(:)   ,target       ,optional ,intent(in)     ::  PointSize                       !< LineStyle PointSize
  character(*)  ,dimension(:)   ,target       ,optional ,intent(in)     ::  PointInterval                   !< LineStyle PointInterval
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

  integer                                                               ::  iLine                           ! Line index
  logical                                                               ::  i_Debug_Loc                     ! Local debugging indicator
  character(:)        ,pointer                                          ::  DashType_Loc                        ! Local LineStyle Type
  character(:)        ,pointer                                          ::  Width_Loc                       ! Local LineStyle Width
  character(:)        ,pointer                                          ::  Color_Loc                       ! Local LineStyle Color
  character(:)        ,pointer                                          ::  PointType_Loc                   ! Local LineStyle PointType
  character(:)        ,pointer                                          ::  PointSize_Loc                   ! Local LineStyle PointSize
  character(:)        ,pointer                                          ::  PointInterval_Loc               ! Local LineStyle PointInterval

!   character(:)        ,allocatable                                      ::  DashType_Loc                        ! Local LineStyle Type
!   character(:)        ,allocatable                                      ::  Width_Loc                       ! Local LineStyle Width
!   character(:)        ,allocatable                                      ::  Color_Loc                       ! Local LineStyle Color

  i_Debug_Loc = i_Debug_Default                                                                                 ! Setting debugging indicator to default value
  if ( present(Debug) ) i_Debug_Loc = Debug                                                                     ! If present optional input argument, then setting debugging indicator to input value

  if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_LineStyle_1D]: Entering')")

  if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_LineStyle_1D]: Allocating LineStyle')")
  allocate ( LineStyle(NLine) )                                                                                 ! Allocating LineStyle structure to the number of lines
  if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_LineStyle_1D]: size(LineStyle) = ',i3)") size(LineStyle)

  do iLine = 1,NLine                                                                                            ! Loop on all lines

    if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_LineStyle_1D]: Associating pointers to null')")
    DashType_Loc        =>  null()
    Width_Loc           =>  null()
    Color_Loc           =>  null()
    PointType_Loc       =>  null()
    PointSize_Loc       =>  null()
    PointInterval_Loc   =>  null()

    if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_LineStyle_1D]: Associating pointers to input variables')")
    if ( IsPresentAndHasElement(iLine,DashType)      ) DashType_Loc       =>  DashType(iLine)                                     ! Setting local LineStyle Type
    if ( IsPresentAndHasElement(iLine,Width)         ) Width_Loc          =>  Width(iLine)                                    ! Setting local LineStyle Width
    if ( IsPresentAndHasElement(iLine,Color)         ) Color_Loc          =>  Color(iLine)                                    ! Setting local LineStyle Color
    if ( IsPresentAndHasElement(iLine,PointType)     ) PointType_Loc      =>  PointType(iLine)                                ! Setting local LineStyle PointType
    if ( IsPresentAndHasElement(iLine,PointSize)     ) PointSize_Loc      =>  PointSize(iLine)                                ! Setting local LineStyle PointSize
    if ( IsPresentAndHasElement(iLine,PointInterval) ) PointInterval_Loc  =>  PointInterval(iLine)                            ! Setting local LineStyle PointInterval

    if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_LineStyle_1D]: Calling LineStyle%Construct for iLine = ',i3)") iLine
!     LineStyle(iLine) =  GPF_LineStyle_Type(                                     &                               ! Constructing LineStyle object for current line
!     call GPF_LineStyle_Type(    LineStyle(iLine),                               &                               ! Constructing LineStyle object for current line
    call LineStyle(iLine)%Construct(                            &                                               ! Constructing LineStyle object for current line
                Object          =       Object_Loc,             &
                Unit            =       iLine,                  &
                DashType        =       DashType_Loc,           &
                Width           =       Width_Loc,              &
                Color           =       Color_Loc,              &
                PointType       =       PointType_Loc,          &
                PointSize       =       PointSize_Loc,          &
                PointInterval   =       PointInterval_Loc,      &
                Debug           =       i_Debug_Loc             )

  end do                                                                                                        ! End loop on lines

  if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_LineStyle_1D]: Calling Compact_LineStyle')")
  call Compact_LineStyle( LineStyle, Debug )

!   call Check_Uniticty

  if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_LineStyle_1D]: Exiting',/)")

End Subroutine

Pure Function IsPresentAndHasElement(i,Var) result(Indicator)
  integer                                               ,intent(in)     ::  i
  character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  Var
  logical                                                               ::  Indicator
  Indicator   =   .False.
  if ( .Not. Present(Var) ) return
  if ( size(Var) < i ) return
  Indicator   =   .True.
End Function

! @TODO: Write a better and cleaner error message
! @TODO: Currently, only lines with consecutive "palette" color can be merged (So that all line with the palette acolor must be block together)
! REMARK:
! This procedure compacts the LineStyle object by using the iterative "for" keyword.
! A line in the LineStyle object is replace by an iteration of the for keyword if
! it has a palette color, ie. if the component i_Multiple_Lines is true.
Subroutine Compact_LineStyle( LineStyle, Debug )

  use GPF_Tools                 ,only:  Convert_To_Integer

  type(GPF_LineStyle_Type) ,dimension(:) ,allocatable   ,intent(inout)  ::  LineStyle                       !< LineStyle object to be compacted
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

  logical                                                               ::  i_Debug_Loc                     ! Local debugging indicator
  integer                                                               ::  iLine                           !
  integer                                                               ::  i                               !
  integer                                                               ::  NLines                          ! Number of element of the LineStyle object
  integer                                                               ::  NElements                       !
  logical                                                               ::  i_First_Line                    ! Indicator that the first line of a group of multiple-line with the same Linestyle is being proceed
  type(GPF_LineStyle_Type) ,dimension(:) ,allocatable                   ::  LineStyle_Tmp                   ! Temporary LineStyle object

  i_Debug_Loc = i_Debug_Default                                                                                 ! Setting debugging indicator to default value
  if ( present(Debug) ) i_Debug_Loc = Debug                                                                     ! If present optional input argument, then setting debugging indicator to input value
  if (i_Debug_Loc) write(DbgUnit,"(12x,'[Compact_LineStyle]: Entering')")

  if ( .not.allocated(LineStyle) ) then
    write(DbgUnit,"(12x,'[Compact_LineStyle]: ERROR: The LineStyle object must already be allocated on entry')")
    write(DbgUnit,"(12x,'[Compact_LineStyle]: ERROR: Stopping the code')")
    stop
  end if

  NLines    =       size(LineStyle)

  if (i_Debug_Loc) write(DbgUnit,"(12x,'[Compact_LineStyle]: NLines = ',i0)") NLines
  if (i_Debug_Loc) write(DbgUnit,"(12x,'[Compact_LineStyle]: count( LineStyle%i_Multiple_Lines ) = ',i0)") count( LineStyle%i_Multiple_Lines )

  if ( any(LineStyle%i_Multiple_Lines) ) then
    NElements    =       NLines - count( LineStyle%i_Multiple_Lines ) + 1
    if (i_Debug_Loc) write(DbgUnit,"(12x,'[Compact_LineStyle]: NElements = ',i0)") NElements
    if (i_Debug_Loc) write(DbgUnit,"(12x,'[Compact_LineStyle]: Freeing global units')")
    do i=1,size(LineStyle)
      call Free_Global_Units( Convert_To_Integer(LineStyle(i)%Unit) )                                           ! Freeing the global units
    end do
    if (i_Debug_Loc) write(DbgUnit,"(12x,'[Compact_LineStyle]: Allocating LineStyle_Tmp to LineStyle')")
#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     allocate( LineStyle_Tmp(size(LineStyle)) )
!     LineStyle_Tmp = LineStyle
    allocate( LineStyle_Tmp(size(LineStyle)), source = LineStyle )
#else
    allocate( LineStyle_Tmp, source = LineStyle )
#endif
    deallocate( LineStyle )
    allocate( LineStyle(NElements) )
    if (i_Debug_Loc) write(DbgUnit,"(12x,'[Compact_LineStyle]: size(LineStyle_Tmp) = ',i0)") size(LineStyle_Tmp)
    i_First_Line        =       .True.
    i                   =       0
    do iLine = 1,NLines
    associate( Old => LineStyle_Tmp(iLine) )
      if (i_Debug_Loc) write(DbgUnit,"(12x,'[Compact_LineStyle]: iLine = ',i0,3x,'Old%Color = ',a)") iLine, Old%Color
      if (.not.Old%i_Multiple_Lines ) then                                                                      ! If current element (ie current line) does not correspond to a Multi-Line LineStyle, then tyhe old element is copied into the new object
        i               =       i + 1                                                                           ! Incrementing the index of elements of the new (compacted) LineStyle object
        if (i_Debug_Loc) write(DbgUnit,"(12x,'[Compact_LineStyle]: NOT i_Multiple_Lines i = ',i0)") i
!         LineStyle(i)    =       GPF_LineStyle_Type(                                     &                       ! Constructing current element of the LineStyle object using the data from the old object
!         call GPF_LineStyle_Type(        LineStyle(i),                                   &                       ! Constructing current element of the LineStyle object using the data from the old object
        call LineStyle(i)%Construct(                            &                                               ! Constructing current element of the LineStyle object using the data from the old object
                Object          =       Old%Object,             &
                Unit            =       i,                      &
                DashType        =       Old%DashType,           &
                Width           =       Old%Width,              &
                Color           =       Old%Color,              &
                PointType       =       Old%PointType%Value,    &
                PointSize       =       Old%PointSize%Value,    &
                PointInterval   =       Old%PointInterval%Value )
        LineStyle(i)%iLine_Ini  =       iLine                                                                   ! Setting the index of the first line
        LineStyle(i)%iLine_Fin  =       iLine                                                                   ! Setting the index of the last line
      else if (i_First_Line) then                                                                               ! If current element corresponds to a Multi-Line LineStyle, and if the first line of the set of Palette lines is being processed
        i               =       i + 1                                                                           ! Incrementing the index of elements of the new (compacted) LineStyle object
        i_First_Line    =       .False.                                                                         ! Setting the first line indicator to false so that this block segment is executed only once
        if (i_Debug_Loc) write(DbgUnit,"(12x,'[Compact_LineStyle]: NOT i_Multiple_Lines i = ',i0)") i
!         LineStyle(i)    =       GPF_LineStyle_Type(                                     &
!         call GPF_LineStyle_Type(        LineStyle(i),                                   &
        call LineStyle(i)%Construct(                            &
                Object          =       Old%Object,             &
                Unit            =       i,                      &
                DashType        =       Old%DashType,               &
                Width           =       Old%Width,              &
                Color           =       'palette',              &
                PointType       =       Old%PointType%Value,    &
                PointSize       =       Old%PointSize%Value,    &
                PointInterval   =       Old%PointInterval%Value )
        LineStyle(i)%iLine_Ini  =       iLine                                                                   ! Setting the index of the first line
        LineStyle(i)%iLine_Fin  =       iLine + count(LineStyle_Tmp%i_Multiple_Lines) - 1                       ! Setting the index of the last line
      end if                                                                                                    ! End if
    end associate
    end do

  else
    do iLine = 1,NLines
      LineStyle(iLine)%iLine_Ini    =       iLine
      LineStyle(iLine)%iLine_Fin    =       iLine
    end do
  end if

  if (i_Debug_Loc) then
    do i = 1,size(LineStyle)
      write(DbgUnit,"(12x,'[Compact_LineStyle]: i = ',i3,3x,'i_Multiple_Lines = ',l3,3x,'iLine_Ini = ',i3,3x,'iLine_Fin = ',i3)") i, LineStyle(i)%i_Multiple_Lines, LineStyle(i)%iLine_Ini, LineStyle(i)%iLine_Fin
    end do
    do i = 1,size(LineStyle)
      write(DbgUnit,"(12x,'[Compact_LineStyle]: i = ',i3,3x,'LineStyle%Command = ',a)") i, LineStyle(i)%Command
    end do
  end if

  if (i_Debug_Loc) write(DbgUnit,"(12x,'[Compact_LineStyle]: Exiting',/)")

End Subroutine

Subroutine Write_LineStyle( This, Unit )
  use GPF_Parameters            ,only:  LineStyle_Object_Line
  class(GPF_LineStyle_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  integer                                               ,intent(in)     ::  Unit                            !< File unit number
  character(*)                                              ,parameter  ::  Comment='# Line Parameters:'    ! Comment line
  if ( (trim(This%Object)==LineStyle_Object_Line) .and. (This%Get_Integer_Unit()==1) )       &                  ! If the parent object is a line and if the first line is considered, then
  write(Unit,"(/,a)") Comment                                                                                   ! Wrtting comment line
  if ( len_trim(This%Command) /= 0 ) write(Unit,"(a)") trim(This%Command)                                       ! Writing the command line
End Subroutine

Function Get_Color( This ) result( Color )
  class(GPF_LineStyle_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  character(:)        ,allocatable                                      ::  Color                           !< LineStyle color
  Color         =       This%Color                                                                              ! Getting the LineStyle color
End Function

Function Get_LineStyle_Unit( This ) result(Unit)
  class(GPF_LineStyle_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  character(:)        ,allocatable                                      ::  Unit                            !< LineStyle unit
  Unit          =       This%Unit                                                                               ! Getting the LineStyle Unit
End Function

Function Is_PointType_Specified( This ) result(Indicator)
  class(GPF_LineStyle_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  logical                                                               ::  Indicator                       !< Indicator
  Indicator     =       This%PointType%Presence                                                                 ! Getting the indicator whether or not the LineStyle PointType is specified
End Function

Function Is_PointSize_Specified( This ) result(Indicator)
  class(GPF_LineStyle_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  logical                                                               ::  Indicator                       !< Indicator
  Indicator     =       This%PointSize%Presence                                                                 ! Getting the indicator whether or not the LineStyle PointSize is specified
End Function

Function Is_PointInterval_Specified( This ) result(Indicator)
  class(GPF_LineStyle_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  logical                                                               ::  Indicator                       !< Indicator
  Indicator     =       This%PointInterval%Presence                                                             ! Getting the indicator whether or not the LineStyle PointInterval is specified
End Function



! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Set_LineStyle_Object( This, Object )
  class(GPF_LineStyle_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument
  character(*)                                ,optional ,intent(in)     ::  Object                          !< LineStyle Object
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_LineStyle_Object]: Entering')")
  This%Object   =       Check_LineStyle_Object(Object)                                                          ! Checking validity of LineStyle object and setting its value into the structure
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_LineStyle_Object]: This%Object = ',a)") This%Object
    write(DbgUnit,"(16x,'[Set_LineStyle_Object]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_LineStyle_Unit( This, Unit )
  class(GPF_LineStyle_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument
  integer                                     ,optional ,intent(in)     ::  Unit                            ! LineStyle Unit
  integer                                                               ::  Unit_Loc                        ! Local LineStyle Unit
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_LineStyle_Unit]: Entering')")
  if ( present(Unit) ) then                                                                                     ! If present optional input argument
    Unit_Loc            =       Unit                                                                            ! Setting the LineStyle Unit number to the input argument value
    Unit_Loc            =       Get_Unit( This%Object, Unit )                                                   ! Getting a free LineStyle Unit number
    This%iLine_Ini      =       Unit_Loc                                                                   ! Setting the index of the first line
    This%iLine_Fin      =       Unit_Loc                                                                   ! Setting the index of the last line
  else                                                                                                          ! If absent optional input argument
    Unit_Loc            =       Get_Unit(This%Object)                                                                   ! Getting a free LineStyle Unit number
  end if                                                                                                        ! End if case on optional input argument presence
  call Set_Global_Units( Unit_Loc )                                                                             ! Setting global LineStyle units
  allocate( character(5) :: This%Unit )                                                                         ! Length allocation
  write(This%Unit,"(i4,1x)") Unit_Loc                                                                           ! Setting the LineStyle unit using internal I/O (integer to string conversion)
  if ( len_trim(This%Unit) /= 0 ) This%Command = This%Command // This%Unit                                      ! Updating the command
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_LineStyle_Unit]: This%Unit    = ',a)") This%Unit
    write(DbgUnit,"(16x,'[Set_LineStyle_Unit]: This%Command = ',a)") This%Command
    write(DbgUnit,"(16x,'[Set_LineStyle_Unit]: Exiting',/)")
  end if
End Subroutine

! @TODO: Check that the input LineStyle_LineType is correct
Subroutine SetLineStyleDashType( This, DashType )
  use GPF_Parameters            ,only:  Line_Type_Default
  class(GPF_LineStyle_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument
  character(*)                                ,optional ,intent(in)     ::  DashType                            !< LineStyle Type
  character(*)                                              ,parameter  ::  Char_fmt="('lt ',i4)"           ! Character string corresponding to the integer-to-string conversion format
  if (This%i_Debug) write(DbgUnit,"(16x,'[SetLineStyleDashType]: Entering')")
  This%DashType     =       ''                                                                                      ! Initializing to empty string
  if ( present(DashType) ) then                                                                                     ! If present optional input argument
    if ( len_trim(DashType) /= 0 ) This%DashType = trim(DashType)                                                           ! If non-empty input argument, then setting component to input value
  end if                                                                                                        ! End if case on optional input argument presence
  if ( len_trim(This%DashType) /= 0 ) This%Command = This%Command // 'dt ' // trim(DashType) // ' '                     ! Updating the command
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[SetLineStyleDashType]: This%DashType    = ',a)") This%DashType
    write(DbgUnit,"(16x,'[SetLineStyleDashType]: This%Command = ',a)") This%Command
    write(DbgUnit,"(16x,'[SetLineStyleDashType]: Exiting',/)")
  end if
End Subroutine

! @TODO: Check that the input LineWidth is correct
Subroutine Set_LineStyle_LineWidth( This, Width )
  class(GPF_LineStyle_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument
  character(*)                                ,optional ,intent(in)     ::  Width                           !< LineStyle Width
  character(:)  ,allocatable                                            ::  Width_Local
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_LineStyle_LineWidth]: Entering')")
  This%Width    =       ''                                                                                      ! Initializing to empty string
  if ( present(Width) ) then                                                                                    ! If present optional input argument
    Width_Local =       trim(Width)                                                                             ! Setting the local variable to the optional input value
  else                                                                                                          ! If absent optional input argument
    Width_Local =       trim( GPF%Get_Default_LineStyle_Width() )                                               ! Setting the local variable to the default value
  end if                                                                                                        ! End if case on optional input argument presence
  if ( len_trim(Width_Local) /= 0 ) This%Width = trim(Width_Local)                                              ! If non-empty character string, then setting the object component
  if ( len_trim(This%Width) /= 0 ) This%Command = This%Command // 'lw ' // trim(This%Width) // ' '              ! Updating the command
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_LineStyle_LineWidth]: Width_Local  = ',a)") Width_Local
    write(DbgUnit,"(16x,'[Set_LineStyle_LineWidth]: This%Width   = ',a)") This%Width
    write(DbgUnit,"(16x,'[Set_LineStyle_LineWidth]: This%Command = ',a)") This%Command
    write(DbgUnit,"(16x,'[Set_LineStyle_LineWidth]: Exiting',/)")
  end if
End Subroutine

! @TODO: Write a Check_Color_Validity procedure
Subroutine Set_LineStyle_LineColor( This, Color )
  use GPF_Tools                 ,only:  Check_Validity
  use GPF_Parameters            ,only:  LineStyle_Color_Default, LineStyle_Color_Valid, Key_PALETTE
  class(GPF_LineStyle_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument
  character(*)                                ,optional ,intent(in)     ::  Color                           !< LineStyle color
  character(:)        ,allocatable                                      ::  Color_loc                       ! Local line color
  character(:)        ,allocatable                                      ::  Prefix, Suffix
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_LineStyle_LineColor]: Entering')")
  This%Color            =       ''                                                                              ! Initializing the color to empty string
  This%i_Multiple_Lines  =       .False.                                                                         ! Initializing the palette color indicator to false
  if ( present(Color) ) then                                                                                    ! If present optional input argument
    if ( len_trim(Color) /= 0 ) then                                                                            ! If input argument is non-empty
!       Color_loc         =       Check_Validity( Color, LineStyle_Color_Valid, LineStyle_Color_Default )         ! Checking validity and setting the data into a termporay variable
      Color_loc         =       Color         ! Checking validity and setting the data into a termporay variable
      if ( trim(Color) == Key_PALETTE ) then                                                                    ! If the color corresponds to the 'palette' keyword
        Prefix                  =       'lc '
        Suffix                  =       ' '
        This%Color              =       trim(Color_loc)                                         ! Setting Color to input value
        This%i_Multiple_Lines   =       .True.                                                                  ! Setting the palette color indicator to true
      else                                                                                                      ! If real color
        Prefix                  =       'lc rgb "'                                           ! Setting Color to input value
        Suffix                  =       '" '                                           ! Setting Color to input value
        This%Color              =       trim(Color_loc)                                           ! Setting Color to input value
      end if                                                                                                    ! End if case on color value
    end if                                                                                                      ! End if case on length of input argument
  end if                                                                                                        ! End if case on presence of optional input argument
  if ( len_trim(This%Color) /= 0 ) This%Command = This%Command // Prefix // This%Color // Suffix                ! Updating the command
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_LineStyle_LineColor]: This%Color   = ',a)") This%Color
    write(DbgUnit,"(16x,'[Set_LineStyle_LineColor]: This%Command = ',a)") This%Command
    write(DbgUnit,"(16x,'[Set_LineStyle_LineColor]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_LineStyle_PointType( This, PointType )
  class(GPF_LineStyle_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument
  character(*)                                ,optional ,intent(in)     ::  PointType                       !< LineStyle PointType
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_LineStyle_PointType]: Entering')")
  This%PointType        =       GPF_LineStyle_PointType_Type( Value=PointType, Debug=This%i_Debug )             ! Constucting the Point-Type object, if any
  if ( len_trim(This%PointType%Command) /= 0 )  This%Command = This%Command // This%PointType%Command           ! Setting the PointType command if defined
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_LineStyle_PointType]: This%PointType%Command = ',a)") This%PointType%Command
    write(DbgUnit,"(16x,'[Set_LineStyle_PointType]: This%Command           = ',a)") This%Command
    write(DbgUnit,"(16x,'[Set_LineStyle_PointType]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_LineStyle_PointSize( This, PointSize )
  class(GPF_LineStyle_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument
  character(*)                                ,optional ,intent(in)     ::  PointSize                       !< LineStyle PointSize
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_LineStyle_PointSize]: Entering')")
  This%PointSize        =       GPF_LineStyle_PointSize_Type( Value=PointSize, Debug=This%i_Debug )             ! Constucting the PointSize object, if any
  if ( len_trim(This%PointSize%Command) /= 0 ) This%Command = This%Command // This%PointSize%Command            ! Setting the PointSize command if defined
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_LineStyle_PointSize]: This%PointSize%Command = ',a)") This%PointSize%Command
    write(DbgUnit,"(16x,'[Set_LineStyle_PointSize]: This%Command           = ',a)") This%Command
    write(DbgUnit,"(16x,'[Set_LineStyle_PointSize]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_LineStyle_PointInterval( This, PointInterval )
  class(GPF_LineStyle_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument
  character(*)                                ,optional ,intent(in)     ::  PointInterval                   !< LineStyle PointInterval
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_LineStyle_PointInterval]: Entering')")
  This%PointInterval        =       GPF_LineStyle_PointInterval_Type( Value=PointInterval, Debug=This%i_Debug ) ! Constucting the PointInterval object, if any
  if ( len_trim(This%PointInterval%Command) /= 0 ) This%Command = This%Command // This%PointInterval%Command    ! Setting the PointInterval command if defined
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_LineStyle_PointInterval]: This%PointInterval%Command = ',a)") This%PointInterval%Command
    write(DbgUnit,"(16x,'[Set_LineStyle_PointInterval]: This%Command           = ',a)") This%Command
    write(DbgUnit,"(16x,'[Set_LineStyle_PointInterval]: Exiting',/)")
  end if
End Subroutine


Subroutine Set_Command_Prefix( This )
  class(GPF_LineStyle_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument
  character(*)                                              ,parameter  ::  Prefix='set style line '        ! Prefix string
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Command_Prefix]: Entering')")
  This%Command  =       Prefix                                                                                  ! Setting command prefix
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_Command_Prefix]: This%Command = ',a)") This%Command
    write(DbgUnit,"(16x,'[Set_Command_Prefix]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_LineStyle_Command( This )
  class(GPF_LineStyle_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument
  character(*)                                              ,parameter  ::  Prefix='set style line '        ! Prefix string
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_LineStyle_Command]: Entering')")
!   This%Command  =       Prefix // This%Unit // This%Type // This%Width // This%Color                            ! Setting command
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_LineStyle_Command]: This%Command = ',a)") This%Command
    write(DbgUnit,"(16x,'[Set_LineStyle_Command]: Exiting',/)")
  end if
End Subroutine

Function Get_Integer_Unit( This ) result( Unit )
  class(GPF_LineStyle_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  integer                                                               ::  Unit                            !< LineStyle integer unit
  read(This%Unit,*) Unit                                                                                   ! Character to integer conversion
End Function

Pure Function Get_Unit( Object, Unit_Inp ) result( Unit_Out )
  use GPF_Parameters            ,only:  i_LineStyle_Object_Unknown,     LineStyle_Object_Unknown,       &
                                        i_LineStyle_Object_Line,        LineStyle_Object_Line,          &
                                        i_LineStyle_Object_Grid,        LineStyle_Object_Grid,          &
                                        i_LineStyle_Object_ColorBox,    LineStyle_Object_ColorBox
  character(*)                                          ,intent(in)     ::  Object                          !< Parent object of LineStyle
  integer                                     ,optional ,intent(in)     ::  Unit_Inp
  integer                                                               ::  Unit_Out                            !< LineStyle Unit
  if ( present(Unit_Inp) ) then
    Unit_Out    =       Unit_Inp
  else
    select case ( trim(Object) )                                                                                  ! Selecting object type
      case (LineStyle_Object_Line);       Unit_Out =       i_LineStyle_Object_Line                                    ! Case of an Line object
      case (LineStyle_Object_Grid);       Unit_Out =       i_LineStyle_Object_Grid                                    ! Case of an Grid object
      case (LineStyle_Object_ColorBox);   Unit_Out =       i_LineStyle_Object_ColorBox                                ! Case of an ColorBox object
      case (LineStyle_Object_Unknown);    Unit_Out =       i_LineStyle_Object_Unknown                                 ! Case of an Unknown object
    end select                                                                                                    ! End of object type selection
  end if
  if ( allocated(LS_Global_Units) ) then
    do                                                                                                            ! Infinit loop in order to select a unused LineStyle Unit number
      if ( .not.any(LS_Global_Units == Unit_Out) ) exit                                                               ! If current LineStyle unit is not used, then it is selected and the loop is exit
      Unit_Out        =       Unit_Out + 1                                                                                ! If current LineStyle unit is already used, then incrementation of the Unit number and cycle
    end do                                                                                                        ! End of loop for unit selection
  end if
End Function

Subroutine Free_Global_Units_0D( Unit )
  integer                                               ,intent(in)     ::  Unit                            !< LineStyle Unit
  integer       ,dimension(:)   ,allocatable                            ::  LS_Global_Units_Tmp             ! Temporary variable storing all LineStyle units
  logical       ,parameter      ::  i_Debug_Loc=.True.
  integer       ::  i,j
  if ( .not.allocated(LS_Global_Units) ) return                                                                 ! If the variable storing all LineStyle units has not yet been allocated, then exiting
!   if (i_Debug_Loc) write(DbgUnit,"(/,14x,'[Free_Global_Units_0D]: Entering')")
! LS_Global_Units_Tmp
  i = 0
  do
!   i = 1,size(LS_Global_Units)    ! loop on all global units cause problem because the size of LS_Global_Units changes
    i = i + 1
    if ( i > size(LS_Global_Units) ) exit

!     if (i_Debug_Loc) write(DbgUnit,"(14x,'[Free_Global_Units_0D]: Unit = ',i0,3x,'i = ',i0,3x,'LS_Global_Units(i) = ',i0)") Unit, i, LS_Global_Units(i)
    if ( Unit /= LS_Global_Units(i) ) cycle     ! If input unit is not a global unit, then goint to the next global unit
    allocate( LS_Global_Units_Tmp( size(LS_Global_Units)-1 ) )                                                  ! Allocating a temporary variable for LineStyle units

    if ( i /= 1) then
      do j = 1,i-1
        LS_Global_Units_Tmp(j)  =       LS_Global_Units(j)                                 ! Copying previous LineStyle units in temporary variable
      end do
    end if

    if (i /= size(LS_Global_Units)) then
      do j = i+1,size(LS_Global_Units)
        LS_Global_Units_Tmp(j-1)  =       LS_Global_Units(j)                                 ! Copying previous LineStyle units in temporary variable
      end do
    end if

!     if (i_Debug_Loc) write(DbgUnit,"(/,14x,'[Free_Global_Units_0D]: calling move_alloc')")
    call move_alloc( from=LS_Global_Units_Tmp, to=LS_Global_Units )                                             ! Transfering allocation from temporary to final variable, including the extra component

  end do
End Subroutine

Subroutine Free_Global_Units_1D( Unit )
  integer       ,dimension(:)                           ,intent(in)     ::  Unit                            !< LineStyle Unit
  integer       ::  i
  if ( .not.allocated(LS_Global_Units) ) return                                                                 ! If the variable storing all LineStyle units has not yet been allocated, then exiting
  do i = 1,size(Unit)
    call Free_Global_Units_0D( Unit(i) )
  end do
End Subroutine

Subroutine Set_Global_Units( Unit )
  integer                                               ,intent(in)     ::  Unit                            !< LineStyle Unit
  integer       ,dimension(:)   ,allocatable                            ::  LS_Global_Units_Tmp             ! Temporary variable storing all LineStyle units
  if ( .not.allocated(LS_Global_Units) ) then                                                                   ! If the variable storing all LineStyle units has not yet been allocated (first call)
    allocate( LS_Global_Units(1) )                                                                              ! Allocation to unity
  else                                                                                                          ! If the variable storing all LineStyle units has already been allocated
    allocate( LS_Global_Units_Tmp( size(LS_Global_Units)+1 ) )                                                  ! Allocating a temporary variable for LineStyle units with an extra component compared to previously one
    LS_Global_Units_Tmp(1:size(LS_Global_Units))        =       LS_Global_Units                                 ! Copying previous LineStyle units in temporary variable
    call move_alloc( from=LS_Global_Units_Tmp, to=LS_Global_Units )                                             ! Transfering allocation from temporary to final variable, including the extra component
  end if                                                                                                        ! End if case on allocation status of the variable storing all LineStyle units
  LS_Global_Units( size(LS_Global_Units) )      =       Unit                                                    ! Saving current LineStyle unit in the global LineStyle variable
End Subroutine

Function Check_LineStyle_Object( Object_Inp ) result( Object_Out )
  use GPF_Parameters            ,only:  LineStyle_Object_Unknown, LineStyle_Object_ALL
  character(*)                ,optional ,intent(in)     ::  Object_Inp                                      !< LineStyle Object to be checked for validity
  character(:)        ,allocatable                      ::  Object_Out                                      !< Checked LineStyle Object
  Object_Out    =       LineStyle_Object_Unknown                                                                ! Initializing of the LineStyle object to a unknown object
  if ( present(Object_Inp) ) then                                                                               ! If present input optional argument
    if ( any(LineStyle_Object_ALL==Object_Inp) ) Object_Out = trim(Object_Inp)                                  ! If valid input Object, then setting its value into the ouput object
  end if                                                                                                        ! End if case on optional argument presence
End Function

! Subroutine Check_Uniticty( LineStyle, Debug )
! !   use GPF_Tools                 ,only:  Check_Validity
! !   use GPF_Parameters            ,only:  LineStyle_Color_Default, LineStyle_Color_Valid
! !   implicit none
! !   class(GPF_LineStyle_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument
! !   character(*)                                ,optional ,intent(in)     ::  Color                        !< LineStyle color
! !   integer                                                               ::  iColor                          ! Line Color index
! !   integer                                                               ::  LineType                        ! Line Type indicator (For a single line) (Used only if Color argument is absent and if number of Line > maximum number of colore ColMax)
! !   integer                                                               ::  LinesType                       ! Indicator of Line Type (for all lines)
!
!   implicit none
!   type(GPF_LineStyle_Type) ,dimension(:)                ,intent(inout)  ::  LineStyle                       !< LineStyle structure
!   logical                                     ,optional ,intent(in)     ::  Debug                           !< LineStyle Debug indicator
!   integer                                                               ::  i, j                            ! Line index
!   character(:)  ,allocatable                                            ::  LS_Typ_i, LS_Typ_j              ! Type of line styles
!   character(:)  ,allocatable                                            ::  LS_Col_i, LS_Col_j              ! Color of line styles
!
!
!   do i = 1,size(LineStyle) - 1                                                                                  ! Loop on all lines except the last one
!     LS_Typ_i    =       LineStyle(i)%Type
!     LS_Col_i    =       LineStyle(i)%Color
!     if ( (len_trim(LS_Typ_i)==0) .and. (len_trim(LS_Col_i)==0) ) cycle                                          ! If current line has no defined type nor color, then going to the next line
!     do j = i+1,size(LineStyle)                                                                                  ! Loop on all lines above the i line
!       LS_Typ_j  =       LineStyle(j)%Type
!       LS_Col_j  =       LineStyle(j)%Color
!       if ( (LS_Typ_i==LS_Typ_j) .and. (LS_Col_i==LS_Col_j) ) then
! !     LineType    =       1                                                                                       ! Initialisation LType indicator
! !     iColor      =       0                                                                                       ! Initialisation of the Line Color index
! !     do iLine = 1,This%NLine                                                                                     ! Lopp on all Lines
! !       iColor    =       iColor + 1                                                                              ! Incrementation of the Line Color index
! !       if ( iColor > size(Line_Color_ALL) ) then                                                                 ! If the Line Color index is greater than the maximim number of color
! !         iColor          =       1                                                                               ! Reset the Line Color index to unity
! !         LineType        =       LineType + 1                                                                    ! Incrementation of the LType index
! !       end if                                                                                                    ! End of if case
! !       This%Color(iLine) =       ' ' // KEY_LineColor // ' "' // trim(Line_Color_ALL(iColor)) // '"'             ! Setting the Line Color index to the default value
! !       LinesType(iLine)  =       LineType                                                                        ! Setting the Line Type indicator for all lines
! !     end do                                                                                                      ! End do loop on Line number
! !     if ( LineType >= 2 ) call This%SetDashType( LinesType )                                                   ! If the maximum number of color have been exceeded, then changing the Lineype indicator
!       end if
!     end do                                                                                                        ! End loop on lines
!
!   end do                                                                                                        ! End loop on lines
!
! End Subroutine

End Module