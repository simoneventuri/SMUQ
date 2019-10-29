Module GPF_ColorBar_Class

  use GPF_Parameters            ,only:  DbgUnit, rkp
  use GPF_Command_Class         ,only:  GPF_Command_Type

  use GPF_Axis_Class            ,only:  GPF_Axis_Type
  USE GPF_ColorBox_Class        ,only:  GPF_ColorBox_Type

  implicit none

  private

  public  ::  GPF_ColorBar_Type

  Type  ,extends(GPF_Command_Type)                      ::  GPF_ColorBar_Type
    type(GPF_Axis_Type)                                 ::  Axis                                            !< ColorBar Axis structure
    type(GPF_ColorBox_Type)                             ::  ColorBox                                        !< ColorBar Box structure
  contains
    private
    procedure   ,public   ::  Write           =>  Write_ColorBar                                  !< Writes the ColorBar commands
    procedure             ::  Set_Axis        =>  Set_ColorBar_Axis                               !< Sets the ColorBar axis
    procedure             ::  Set_Box         =>  Set_ColorBar_Box                                !< Sets the ColorBar box
    procedure   ,public   ::  Set_Command     =>  Set_ColorBar_Command                            !< Sets the ColorBar commands
  End Type

  Interface             GPF_ColorBar_Type
    Module Procedure    Construct_ColorBar
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_ColorBar( Debug,                                                                     &
                Values, CB_Min, CB_Max, Reverse,                                                        &       ! Arguments related to the CB-Axis range
                LogScale, LogBase,                                                                      &       ! Arguments related to the CB-Axis scale
                Label, Label_Offset, Label_Font, Label_Font_Size, Label_Color, Label_Enhanced,          &       ! Arguments related to the CB-Axis label
                Format_,                                                                                &       ! Arguments related to the CB-Axis format
                Orientation, Origin, Size_, Position, Border, UnSetting )                               &       ! Arguments related to the CB-Box
                result(This)

  implicit none

  type(GPF_ColorBar_Type)                                               ::  This                            !< ColorBar object to be constructed
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
! Arguments related to the ColorBar-Axis
  real(rkp)     ,dimension(:)                 ,optional ,intent(in)     ::  Values                          !< ColorBar values
  real(rkp)                                   ,optional ,intent(in)     ::  CB_Min                          !< ColorBar minimum bound
  real(rkp)                                   ,optional ,intent(in)     ::  CB_Max                          !< ColorBar maximum bound
  logical                                     ,optional ,intent(in)     ::  Reverse                         !< ColorBar reversing indicator
  logical                                     ,optional ,intent(in)     ::  LogScale                        !< ColorBar log scale indicator
  integer                                     ,optional ,intent(in)     ::  LogBase                         !< ColorBar log scale base
  character(*)                                ,optional ,intent(in)     ::  Label                           !< ColorBar label text
  character(*)                                ,optional ,intent(in)     ::  Label_Offset                    !< ColorBar label offset
  character(*)                                ,optional ,intent(in)     ::  Label_Font                      !< ColorBar label font name
  character(*)                                ,optional ,intent(in)     ::  Label_Font_Size                 !< ColorBar label font size
  character(*)                                ,optional ,intent(in)     ::  Label_Color                     !< ColorBar label color
  logical                                     ,optional ,intent(in)     ::  Label_Enhanced                  !< ColorBar label enhanced indicator
  character(*)                                ,optional ,intent(in)     ::  Format_                         !< ColorBar format
! Arguments related to the ColorBar-Box
  character(*)                                ,optional ,intent(in)     ::  Orientation                     !< ColorBox orientation (either 'vertical' or 'horizontal')
  character(*)                                ,optional ,intent(in)     ::  Origin                          !< ColorBox origin coordinates ('x,y')
  character(*)                                ,optional ,intent(in)     ::  Size_                           !< ColorBox size ('h,w')
  character(*)                                ,optional ,intent(in)     ::  Position                        !< ColorBox position (either 'front' or 'back')
  character(*)                                ,optional ,intent(in)     ::  Border                          !< ColorBox borders type
  logical                                     ,optional ,intent(in)     ::  UnSetting                       !< ColorBox unsetting indicator

  call This%Set_Debug( Debug )                                                                                  ! Setting the debugging indicator
  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBar]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBar]: Calling This%Set_Axis')")
  call This%Set_Axis(                                                                                   &       ! Setting the ColorBar axis
                Values, CB_Min, CB_Max, Reverse,                                                        &       ! Arguments related to the CB-Axis range
                LogScale, LogBase,                                                                      &       ! Arguments related to the CB-Axis scale
                Label, Label_Offset, Label_Font, Label_Font_Size, Label_Color, Label_Enhanced,          &       ! Arguments related to the CB-Axis label
                Format_                                                                                 )       ! Arguments related to the CB-Axis format

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBar]: Calling This%Set_Box')")
  call This%Set_Box( Orientation, Origin, Size_, Position, Border, UnSetting )                                  ! Setting the ColorBar box

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBar]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the ColorBar command (actually, it is just a comment)

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBar]: Exiting',/)")

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PUBLIC PROCEDURES                                                  *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Write_ColorBar( This, Unit )
  implicit none
  class(GPF_ColorBar_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the ColorBar object
  integer                                               ,intent(in)     ::  Unit                            !< File unit number
  if ( .not.This%Presence ) return                                                                              ! Exiting the procedure if no ColorBar
  if ( len_trim(This%Command)  /= 0 )   write(Unit,"(/,a)") trim(This%Command)
  call This%Axis%Write(Unit)                                                                                    ! Writing Axis commands
  call This%ColorBox%Write(Unit)                                                                                ! Writing ColorBox commands
End Subroutine


! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Set_ColorBar_Axis( This,                                                                     &
                Values, CB_Min, CB_Max, Reverse,                                                        &       ! Arguments related to the CB-Axis range
                LogScale, LogBase,                                                                      &       ! Arguments related to the CB-Axis scale
                Label, Label_Offset, Label_Font, Label_Font_Size, Label_Color, Label_Enhanced,          &       ! Arguments related to the CB-Axis label
                Format_                                                                                 )       ! Arguments related to the CB-Axis format

  implicit none

  class(GPF_ColorBar_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the ColorBar object
  real(rkp)     ,dimension(:)                 ,optional ,intent(in)     ::  Values                          !< ColorBar values
  real(rkp)                                   ,optional ,intent(in)     ::  CB_Min                          !< ColorBar minimum bound
  real(rkp)                                   ,optional ,intent(in)     ::  CB_Max                          !< ColorBar maximum bound
  logical                                     ,optional ,intent(in)     ::  Reverse                         !< ColorBar reversing indicator
  logical                                     ,optional ,intent(in)     ::  LogScale                        !< ColorBar log scale indicator
  integer                                     ,optional ,intent(in)     ::  LogBase                         !< ColorBar log scale base
  character(*)                                ,optional ,intent(in)     ::  Label                           !< ColorBar label text
  character(*)                                ,optional ,intent(in)     ::  Label_Offset                    !< ColorBar label offset
  character(*)                                ,optional ,intent(in)     ::  Label_Font                      !< ColorBar label font name
  character(*)                                ,optional ,intent(in)     ::  Label_Font_Size                 !< ColorBar label font size
  character(*)                                ,optional ,intent(in)     ::  Label_Color                     !< ColorBar label color
  logical                                     ,optional ,intent(in)     ::  Label_Enhanced                  !< ColorBar label enhanced indicator
  character(*)                                ,optional ,intent(in)     ::  Format_                         !< ColorBar format

  character(*)                                              ,parameter  ::  AxisName='cb'                   ! Axis name
  real(rkp)                                                             ::  ValMin
  real(rkp)                                                             ::  ValMax

  if (This%i_Debug) write(DbgUnit,"(18x,'[Set_ColorBar_Axis]: Entering')")

! @COMPILER_BUG: ifort -v: ifort version 13.0.0
! The minval(Values) and maxval(Values) values cannot be passed directly to the GPF_Axis_Type constructor.
! Intermediate variable have to be used
! @TODO: Find a way to pass the minval(Values) and maxval(Values) to the GPF_Axis_Type procedure keeping the optional attribute
!       For the time being, the Xmin and Xmax argumet ares used
  if ( present(Values) ) then
    if (This%i_Debug) write(DbgUnit,"(18x,'[Set_ColorBar_Axis]: The Values (CB_Values) input argumetn is present')")
    ValMin      =       minval(Values)
    ValMax      =       maxval(Values)
!     This%Axis   =       GPF_Axis_Type( AxisName, ValMin, ValMax, Xmin, Xmax, LogScale, LogBase, Label, Fmt, This%i_Debug ) ! Setting axis structure

    if (This%i_Debug) write(DbgUnit,"(18x,'[Set_ColorBar_Axis]: ValMin = ',es15.8)") ValMin
    if (This%i_Debug) write(DbgUnit,"(18x,'[Set_ColorBar_Axis]: ValMax = ',es15.8)") ValMax

    This%Axis   =       GPF_Axis_Type( AxisName, This%i_Debug,                                          &       ! Constructing the CB-axis
                ValMin, ValMax, CB_Min, CB_Max, Reverse,                                                &       ! Arguments related to the CB-Axis range
                LogScale, LogBase,                                                                      &       ! Arguments related to the CB-Axis scale
                Label, Label_Offset, Label_Font, Label_Font_Size, Label_Color, Label_Enhanced,          &       ! Arguments related to the CB-Axis label
                Format_                                                                                 )       ! Arguments related to the CB-Axis format

  else
    if (This%i_Debug) write(DbgUnit,"(18x,'[Set_ColorBar_Axis]: The Values (CB_Values) input argumetn is absent')")
!     This%Axis   =       GPF_Axis_Type( AxisName, Xmin,   Xmax,   Xmin, Xmax, LogScale, LogBase, Label, Fmt, This%i_Debug ) ! Setting axis structure
    This%Axis   =       GPF_Axis_Type( AxisName, This%i_Debug,                                          &       ! Constructing the CB-axis
                CB_Min, CB_Max, CB_Min, CB_Max, Reverse,                                                &       ! Arguments related to the CB-Axis range
                LogScale, LogBase,                                                                      &       ! Arguments related to the CB-Axis scale
                Label, Label_Offset, Label_Font, Label_Font_Size, Label_Color, Label_Enhanced,          &       ! Arguments related to the CB-Axis label
                Format_                                                                                 )       ! Arguments related to the CB-Axis format

  end if

  if ( This%Axis%Get_Presence() ) This%Presence = .True.                                                        ! Setting the presence indicator if required
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_ColorBar_Axis]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(18x,'[Set_ColorBar_Axis]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_ColorBar_Box( This, Orientation, Origin, Size_, Position, Border, UnSetting )

  implicit none

  class(GPF_ColorBar_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the ColorBar object
  character(*)                                ,optional ,intent(in)     ::  Orientation                     !< ColorBox orientation (either 'vertical' or 'horizontal')
  character(*)                                ,optional ,intent(in)     ::  Origin                          !< ColorBox origin coordinates ('x,y')
  character(*)                                ,optional ,intent(in)     ::  Size_                           !< ColorBox size ('h,w')
  character(*)                                ,optional ,intent(in)     ::  Position                        !< ColorBox position (either 'front' or 'back')
  character(*)                                ,optional ,intent(in)     ::  Border                          !< ColorBox borders type
  logical                                     ,optional ,intent(in)     ::  UnSetting                       !< ColorBox unsetting indicator

  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_ColorBar_Box]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_ColorBar_Box]: Calling GPF_ColorBox_Type')")
  This%ColorBox = GPF_ColorBox_Type( Orientation, Origin, Size_, Position, Border, UnSetting, This%i_Debug )               ! Constructing the ColorBox structure

  if ( This%ColorBox%Get_Presence() ) This%Presence = .True.                                                    ! Setting the presence indicator if required
  if (This%i_Debug) then
    write(DbgUnit,"(18x,'[Set_ColorBar_Box]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(18x,'[Set_ColorBar_Box]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_ColorBar_Command( This )
  implicit none
  class(GPF_ColorBar_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the ColorBar object
  character(*)                                              ,parameter  ::  Comment='# ColorBar Parameters: ' ! Comment line
  if ( This%Presence ) This%Command = Comment                                                                   ! Setting the ColorBar command to the comment
End Subroutine

End Module