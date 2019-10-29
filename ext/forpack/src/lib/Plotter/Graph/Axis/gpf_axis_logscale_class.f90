Module GPF_Axis_LogScale_Class

  use GPF_Parameters            ,only:  DbgUnit, rkp
  use GPF_Command_Class         ,only:  GPF_Command_Type
  use GPF_Class                 ,only:  GPF

  implicit none

  private

  public  ::  GPF_Axis_LogScale_Type

! ==============================================================================================================
!   COMMAND DESCRIPTION: LOGSCALE
! ==============================================================================================================
! Syntax:
!       set logscale <axes> {<base>}
!       unset logscale <axes>
!       show logscale
! where <axes> may be any combinations of x, x2, y, y2, z, cb, and r in any order. <base> is the base
! of the log scaling (default is base 10). If no axes are specified, the command affects all axes except r. The
! command unset logscale turns off log scaling for all axes. Note that the ticmarks generated for logscaled
! axes are not uniformly spaced. See set xtics (p. 160).
! Examples:
!       To enable log scaling in both x and z axes:
!       set logscale xz
! To enable scaling log base 2 of the y axis:
!       set logscale y 2
! To enable z and color log axes for a pm3d plot:
!       set logscale zcb
! To disable z axis log scaling:
!       unset logscale z
! ==============================================================================================================

  Type  ,extends(GPF_Command_Type)                      ::  GPF_Axis_LogScale_Type
    logical                                             ::  LogScale                                        !< Log scale indicator (Default is false for linear scaling)
    character(:)        ,allocatable                    ::  LogBase                                         !< Base of the log scaling (Default is 10)
  contains
    private
    procedure   ,public   ::  Update          =>  Update_AxisScale                                !< Updates the object components
    procedure             ::  Initialize      =>  Initialize_AxisScale                            !< Initializes the object components
    procedure             ::  Set_LogScale    =>  Set_AxisScale_LogScale                          !< Sets the log scale indicator
    procedure             ::  Set_LogBase     =>  Set_AxisScale_LogBase                           !< Sets the log scale base
    procedure   ,public   ::  Set_Command     =>  Set_Axis_Command                                !< Sets the axis command
  End Type

  Interface             GPF_Axis_LogScale_Type
    Module Procedure    Construct_Axis_LogScale
  End Interface


  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Axis_LogScale( AxisName, LogScale, LogBase, Debug ) result(This)

  implicit none

  type(GPF_Axis_LogScale_Type)                                          ::  This                            !< Axis-LogScale object to be constructed
  character(*)                                          ,intent(in)     ::  AxisName                        !< Axis name
  logical                                     ,optional ,intent(in)     ::  LogScale                        !< Axis log scale indicator
  integer                                     ,optional ,intent(in)     ::  LogBase                         !< Axis log scale base
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator

  call This%Set_Debug( Debug )                                                                                  ! Setting debugging indicator
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_LogScale]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_LogScale]: Calling This%Initialize')")
  call This%Initialize()                                                                                        ! Initializing the object components to default values

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_LogScale]: Calling This%Set_Keyword')")
  call This%Set_Keyword( AxisName )                                                                             ! Setting the axis name in the Keyword component

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_LogScale]: Calling This%Set_LogScale')")
  call This%Set_LogScale( LogScale )                                                                            ! Setting the axis scale indicator

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_LogScale]: Calling This%Set_LogBase')")
  call This%Set_LogBase( LogBase )                                                                              ! Setting the log scale base

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_LogScale]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the axis command (actually, it is just a comment)

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_LogScale]: Exiting')")

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PUBLIC PROCEDURES                                                  *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Update_AxisScale( This, AxisName, LogScale, LogBase, Debug )

  implicit none

  class(GPF_Axis_LogScale_Type)                         ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-LogScale object
  character(*)                                ,optional ,intent(in)     ::  AxisName                        !< Axis name
  logical                                     ,optional ,intent(in)     ::  LogScale                        !< Axis log scale indicator
  integer                                     ,optional ,intent(in)     ::  LogBase                         !< Axis log scale base
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

  call This%Set_Debug( Debug )                                                                                  ! Setting the debugging indicator
  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_AxisScale]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_AxisScale]: Calling This%Set_Keyword')")
  call This%Set_Keyword( AxisName )                                                                             ! Setting the axis name in the Keyword component

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_AxisScale]: Calling This%Set_LogScale')")
  call This%Set_LogScale( LogScale )                                                                            ! Setting the axis scale indicator

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_AxisScale]: Calling This%Set_LogBase')")
  call This%Set_LogBase( LogBase )                                                                              ! Setting the log scale base

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_AxisScale]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the axis command (actually, it is just a comment)

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_AxisScale]: Exiting',/)")

End Subroutine

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Initialize_AxisScale( This )
  implicit none
  class(GPF_Axis_LogScale_Type)                         ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-LogScale object
  if (This%i_Debug) write(DbgUnit,"(20x,'[Initialize_AxisScale]: Entering')")
  This%LogBase          =       ''                                                                              ! Initializing to an empty string: default value => Base 10 if log scale is set on
  This%LogScale         =       .False.                                                                         ! Initializing the unsetting indicator to false so that the "unset log" command is not written by default
  if (This%i_Debug) write(DbgUnit,"(20x,'[Initialize_AxisScale]: Exiting',/)")
End Subroutine

Subroutine Set_AxisScale_LogScale( This, LogScale )
  implicit none
  class(GPF_Axis_LogScale_Type)                         ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-LogScale object
  logical                                     ,optional ,intent(in)     ::  LogScale                        !< Axis log scale indicator
  if (This%i_Debug) write(DbgUnit,"(20x,'[Set_AxisScale_LogScale]: Entering')")
  if ( present(LogScale) ) then                                                                                 ! If present input optional arguments
    This%LogScale       =       LogScale                                                                        ! If log scale is specified, setting the log scale indicator if the logical input optional argument is true
  else
    select case (This%Keyword)
      case ('x');  if (GPF%Default%i_X_LogScale  )   This%LogScale = GPF%Default%X_LogScale
!       case ('y');  if (GPF%Default%i_Y_LogScale  )   This%LogScale = GPF%Default%Y_LogScale
!       case ('z');  if (GPF%Default%i_Z_LogScale  )   This%LogScale = GPF%Default%Z_LogScale
!       case ('cb'); if (GPF%Default%i_CB_LogScale )   This%LogScale = GPF%Default%CB_LogScale
    end select
  end if                                                                                                        ! End if case on optional argument presence
  if (This%i_Debug) then
    write(DbgUnit,"(20x,'[Set_AxisScale_LogScale]: This%LogScale = ',l3)") This%LogScale
    write(DbgUnit,"(20x,'[Set_AxisScale_LogScale]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_AxisScale_LogBase( This, LogBase )
  use GPF_Tools         ,only:  Convert_To_String
  implicit none
  class(GPF_Axis_LogScale_Type)                         ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-LogScale object
  integer                                     ,optional ,intent(in)     ::  LogBase                         !< Axis log scale base
  if (This%i_Debug) write(DbgUnit,"(20x,'[Set_AxisScale_LogBase]: Entering')")
  if ( present(LogBase) ) This%LogBase = Convert_To_String( LogBase ) // ' '                                    ! If present optional input argument, then setting axis log scale base to input value
  if (This%i_Debug) then
    write(DbgUnit,"(20x,'[Set_AxisScale_LogBase]: This%LogBase = ',a)") This%LogBase
    write(DbgUnit,"(20x,'[Set_AxisScale_LogBase]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Axis_Command( This )
  implicit none
  class(GPF_Axis_LogScale_Type)                         ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the AxisScale object
  if (This%i_Debug) write(DbgUnit,"(20x,'[Set_Axis_Command]: Entering')")
  if (This%LogScale) then
    This%Command        = 'set log '    // This%Keyword
    if ( len_trim(This%LogBase) /= 0 ) This%Command = This%Command // This%LogBase
  else
    This%Command        = 'unset log '  // This%Keyword
  end if
  This%Presence         =       ( len_trim(This%Command) /= 0 )                                                 ! Setting the presence indicator
  if (This%i_Debug) then
    write(DbgUnit,"(20x,'[Set_Axis_Command]: This%Command  = ',a )") This%Command
    write(DbgUnit,"(20x,'[Set_Axis_Command]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(20x,'[Set_Axis_Command]: Exiting',/)")
  end if
End Subroutine
End Module