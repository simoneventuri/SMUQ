Module GPF_Axis_Format_Class

  use GPF_Parameters            ,only:  DbgUnit, rkp
  use GPF_Command_Class         ,only:  GPF_Command_Type
  use GPF_Class                 ,only:  GPF

  implicit none

  private

  public  ::  GPF_Axis_Format_Type

  Type  ,extends(GPF_Command_Type)                      ::  GPF_Axis_Format_Type
    character(:)        ,allocatable                    ::  String                                          !<
  contains
    private
    procedure   ,public   ::  Update          =>  Update_Format                                   !< Updates the object components
    procedure             ::  Initialize      =>  Initialize_Format                               !< Initializes the object components
    procedure             ::  Set_String      =>  Set_Format_String                               !< Sets the format text
    procedure   ,public   ::  Set_Command     =>  Set_AxisFormat_Command                          !< Sets the axis format command
  End Type

  Interface             GPF_Axis_Format_Type
    Module Procedure    Construct_Axis_Format
  End Interface


  character(*)  ,parameter      ::  LogScale_Format = "10^{%L}"

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Axis_Format( AxisName, String, LogScale, Debug ) result(This)

  implicit none

  type(GPF_Axis_Format_Type)                                            ::  This                            !< Axis-Format object to be constructed
  character(*)                                          ,intent(in)     ::  AxisName                        !< Axis name
  character(*)                                ,optional ,intent(in)     ::  String                          !< Axis format string
  logical                                     ,optional ,intent(in)     ::  LogScale                        !< Axis log scale indicator
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator

  call This%Set_Debug( Debug )                                                                                  ! Setting debugging indicator
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Format]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Format]: Calling This%Initialize')")
  call This%Initialize()                                                                                        ! Initializing the object components to default values

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Format]: Calling This%Set_Keyword')")
  call This%Set_Keyword( AxisName )                                                                             ! Setting the axis name in the Keyword component

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Format]: Calling This%Set_String')")
  call This%Set_String( String, LogScale )                                                                      ! Setting the axis format text

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Format]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the axis command (actually, it is just a comment)

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Format]: Exiting')")

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PUBLIC PROCEDURES                                                  *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Update_Format( This, AxisName, String, LogScale, Debug )

  implicit none

  class(GPF_Axis_Format_Type)                           ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Format object
  character(*)                                          ,intent(in)     ::  AxisName                        !< Axis name
  character(*)                                ,optional ,intent(in)     ::  String                          !< Axis format string
  logical                                     ,optional ,intent(in)     ::  LogScale                        !< Axis log scale indicator
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator

  call This%Set_Debug( Debug )                                                                                  ! Setting the debugging indicator
  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_Format]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_Format]: Calling This%Set_Keyword')")
  call This%Set_Keyword( AxisName )                                                                             ! Setting the axis name in the Keyword component

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_Format]: Calling This%Set_Format')")
  call This%Set_String( String, LogScale )                                                                      ! Setting the axis format text

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_Format]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the axis command (actually, it is just a comment)

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_Format]: Exiting',/)")

End Subroutine

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Initialize_Format( This )
  implicit none
  class(GPF_Axis_Format_Type)                           ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Format object
  if (This%i_Debug) write(DbgUnit,"(20x,'[Initialize_Format]: Entering')")
  This%String           =       ''                                                                              ! Initializing to an empty string: default value => "???"
  if (This%i_Debug) write(DbgUnit,"(20x,'[Initialize_Format]: Exiting',/)")
End Subroutine

Subroutine Set_Format_String( This, String, LogScale )
  implicit none
  class(GPF_Axis_Format_Type)                           ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Format object
  character(*)                                ,optional ,intent(in)     ::  String                          !< Axis format string
  logical                                     ,optional ,intent(in)     ::  LogScale                        !< Axis log scale indicator
  if (This%i_Debug) write(DbgUnit,"(20x,'[Set_Format_String]: Entering')")
  if ( present(String) ) then                                                                                   ! If the format string is present in the list of input optional arguments
    This%String         =       trim(String)                                                                    ! Setting the axis Format
    This%Presence       =       .True.                                                                          ! Setting the presence indicator so that the command must be written
  else                                                                                                          ! If it is absent
    if ( present(LogScale) ) then                                                                               ! If the log scale indicator is present in the list of input optional arguments ...
      if ( LogScale ) then                                                                                      ! ... and if has the true value, then
        This%String     =       trim(LogScale_Format)                                                           ! Setting the format adapted for log scales
        This%Presence   =       .True.                                                                          ! Setting the presence indicator so that the command must be written
      end if
    end if
  end if                                                                                                        ! End if case on optional argument presence
  if (This%i_Debug) then
    write(DbgUnit,"(20x,'[Set_Format_String]: This%String   = ',a)")  This%String
    write(DbgUnit,"(20x,'[Set_Format_String]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(20x,'[Set_Format_String]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_AxisFormat_Command( This )
  implicit none
  class(GPF_Axis_Format_Type)                         ,intent(inout)  ::  This                              !< Passed-object dummy argument corresponding to the Axis-Format object
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_AxisFormat_Command]: Entering')")
  This%Command          =       ''                                                                              ! Initializing the command to an empty string: default value => no format specification
  if ( This%Presence ) then                                                                                     ! If a forat is present
     This%Command = 'set format ' // This%Keyword // ' "'// trim(This%String) // '"'                            ! Setting the command string
  end if
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_AxisFormat_Command]: This%Command  = ',a )") This%Command
    write(DbgUnit,"(12x,'[Set_AxisFormat_Command]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(12x,'[Set_AxisFormat_Command]: Exiting',/)")
  end if
End Subroutine

End Module