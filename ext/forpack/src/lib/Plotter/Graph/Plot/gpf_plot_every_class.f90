Module GPF_Plot_Every_Class

  use GPF_Parameters            ,only:  DbgUnit, KEY_splot, rkp
  use GPF_Command_Class         ,only:  GPF_Command_Type
  use GPF_LineStyle_Class       ,only:  GPF_LineStyle_Type


  implicit none

  private

  public  ::  GPF_Plot_Every_Type

! REMARK
!   The every keyword allows a periodic sampling of a data set to be plotted.
!   In the discussion a "point" is a datum defined by a single record in the file; "block" here will mean the same thing as "datablock".
!   Syntax:
!      plot 'file' every {<point_incr>}
!                          {:{<block_incr>}
!                            {:{<start_point>}
!                              {:{<start_block>}
!                                {:{<end_point>}
!                                  {:<end_block>}}}}}
!   The data points to be plotted are selected according to a loop from <start point> to <end point> with increment <point incr>
!   and the blocks according to a loop from <start block> to <end block> with increment <block incr>.
!   The first datum in each block is numbered '0', as is the first block in the file.
!   Note that records containing unplottable information are counted.
!   Any of the numbers can be omitted; the increments default to unity, the start values to the first point or block,
!   and the end values to the last point or block. If every is not specified, all points in all lines are plotted.
!   Examples:
!     every :::3::3       # selects just the fourth block ('0' is first)
!     every :::::9        # selects the first 10 blocks
!     every 2:2           # selects every other point in every other block
!     every ::5::15       # selects points 5 through 15 in each block

  Type  ,extends(GPF_Command_Type)                      ::  GPF_Plot_Every_Type
    integer                                             ::  Point_Incr      =       -1
    integer                                             ::  Block_Incr      =       -1
    integer                                             ::  Start_Point     =       -1
    integer                                             ::  Start_Block     =       -1
    integer                                             ::  End_Point       =       -1
    integer                                             ::  End_Block       =       -1
    character(:)        ,allocatable                    ::  Value
  contains
    private                                                                                                     ! Setting private type-bound procedures
    procedure             ::  Set_Value               =>  Set_Every_Value                                 ! Sets the value of the Every option
    procedure             ::  Add_Entry               =>  Add_Every_Entry                                 ! Adds and entry to the value of the Every option
    procedure   ,public   ::  Set_Command             =>  Set_Every_Command                               ! Sets the command from object components
  End Type

  Interface             GPF_Plot_Every_Type
    Module Procedure    Construct_Plot_Every
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************

! @TODO: For the time being, only the Value input argument should be used since it is the only one to work.

Function Construct_Plot_Every( iElt, Value, Point_Incr, Block_Incr, Start_Point, Start_Block, End_Point, End_Block, Debug ) result(This)

  implicit none

  type(GPF_Plot_Every_Type)                                             ::  This                            !< Passed-object dummy argument corresponding to the Plot-Every object
  integer                                               ,intent(in)     ::  iElt                            !< Index of current element
  character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  Value                           !< Value of the Every option
  integer                                     ,optional ,intent(in)     ::  Point_Incr                      !< Point increment
  integer                                     ,optional ,intent(in)     ::  Block_Incr                      !< Block increment
  integer                                     ,optional ,intent(in)     ::  Start_Point                     !< Point starting index
  integer                                     ,optional ,intent(in)     ::  Start_Block                     !< Block starting index
  integer                                     ,optional ,intent(in)     ::  End_Point                       !< Point ending index
  integer                                     ,optional ,intent(in)     ::  End_Block                       !< Block ending index
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator

  character(*)                                              ,parameter  ::  Keyword='every'

  call This%Set_Debug( Debug )
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Every]: Entering')")

  if ( present(Point_Incr)  ) This%Point_Incr   =       Point_Incr                                              ! If the optional input argument is present, then setting the point increment
  if ( present(Block_Incr)  ) This%Block_Incr   =       Block_Incr                                              ! If the optional input argument is present, then setting the block increment
  if ( present(Start_Point) ) This%Start_Point  =       Start_Point                                             ! If the optional input argument is present, then setting the point starting index
  if ( present(Start_Block) ) This%Start_Block  =       Start_Block                                             ! If the optional input argument is present, then setting the block starting index
  if ( present(End_Point)   ) This%End_Point    =       End_Point                                               ! If the optional input argument is present, then setting the point ending index
  if ( present(End_Block)   ) This%End_Block    =       End_Block                                               ! If the optional input argument is present, then setting the block ending index
  if ( present(Value)       ) This%Value        =       Value(iElt)                                                   ! If the optional input argument is present, then setting the block ending index

!   if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Every]: Calling This%Set_Value')")
!   call This%Set_Value( )                                                                                  ! Setting entries

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Every]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the command keyword

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Every]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the command from object components

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Every]: Exiting')")

End Function


! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

! @TODO: Check somehow that the input Value variable contains a valid every specification
Subroutine Set_Every_Value( This )
  implicit none
  class(GPF_Plot_Every_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument
  character(*)                                              ,parameter  ::  Value_Default   =       ":::::" ! DEfault value for the Every option
  This%Value    =       ""                                                                                      ! Initializing the Value component to an empty string indicating that the every option is not used by default
  call This%Add_Entry( This%Point_Incr  )
  call This%Add_Entry( This%Block_Incr  )
  call This%Add_Entry( This%Start_Point )
  call This%Add_Entry( This%Start_Block )
  call This%Add_Entry( This%End_Point   )
  call This%Add_Entry( This%End_Block   )
  This%Value    =       This%Value(2:len_trim(This%Value))                                                      ! Removing the first character which corresponds to an extra ":"
  if ( This%Value == Value_Default ) This%Value = ""                                                            ! If the value has the default value, then setting the value variable to n empty string so that the every option is not plotted
End Subroutine

Subroutine Add_Every_Entry( This, Number )
  use GPF_Tools                 ,only:  Convert_To_String
  implicit none
  class(GPF_Plot_Every_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument
  integer                                               ,intent(in)     ::  Number                          !< Integer number to be added to the Every value
  character(*)                                              ,parameter  ::  Separator=":"                   ! Seprator character between each value
  character(:)  ,allocatable                                            ::  String                          ! Character string corresponding to the value to be added
  String        =       ""
  if ( Number >= 0 ) String = Convert_To_String(Number)
!   if ( len_trim(This%Value) == 0 ) then
!     This%Value  =       String
!   else
    This%Value  =       This%Value // Separator // String
!   end if
End Subroutine

Subroutine Set_Every_Command( This )
  implicit none
  class(GPF_Plot_Every_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Every object
  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Plot_Every]: Entering')")
  This%Command          =       ""                                                                              ! Initializing the command to an empty string
  if ( len_trim(This%Value) /= 0 ) This%Command = This%Keyword // This%Value // " "                             ! Setting the value associated to the every option if any
  This%Presence         =       ( len_trim(This%Command) /= 0 )                                                 ! Setting the presence indicator
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Construct_Plot_Every]: This%Command  = ',a )") This%Command
    write(DbgUnit,"(16x,'[Construct_Plot_Every]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(16x,'[Construct_Plot_Every]: Entering')")
  end if
End Subroutine

End Module