Module GPF_Axes_Class

  use GPF_Parameters            ,only:  DbgUnit, rkp
  use GPF_Command_Class         ,only:  GPF_Command_Type

  use GPF_Axis_Class            ,only:  GPF_Axis_Type

  implicit none

  private

  public  ::  GPF_Axes_Type

  Type  ,extends(GPF_Command_Type)                      ::  GPF_Axes_Type
    integer                                             ::  NAxes                                           !< Number of axis
    type(GPF_Axis_Type) ,dimension(:)   ,allocatable    ::  Axis                                            !< Axis object
  contains
    private
    procedure   ,public   ::  Write           =>  Write_Axes                                      !< Writes the axes commands
    procedure   ,public   ::  Set_Command     =>  Set_Axes_Command                                !< Sets the axis command
  End Type

  Interface             GPF_Axes_Type
    Module Procedure    Construct_Axes
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Axes( Data_, Axis_Type, Debug,                                                               &
!               Arguments related to the X-Axis object
                X_Min, X_Max, X_Reverse,                                                                        &       ! Arguments related to the X-Axis range
                X_LogScale, X_LogBase,                                                                          &       ! Arguments related to the X-Axis scale
                X_Label, X_Label_Offset, X_Label_Font, X_Label_Font_Size, X_Label_Color, X_Label_Enhanced,      &       ! Arguments related to the X-Axis label
                X_Format,                                                                                       &       ! Arguments related to the X-Axis format
!               Arguments related to the Y-Axis object
                Y_Min, Y_Max, Y_Reverse,                                                                        &       ! Arguments related to the Y-Axis range
                Y_LogScale, Y_LogBase,                                                                          &       ! Arguments related to the Y-Axis scale
                Y_Label, Y_Label_Offset, Y_Label_Font, Y_Label_Font_Size, Y_Label_Color, Y_Label_Enhanced,      &       ! Arguments related to the Y-Axis label
                Y_Format,                                                                                       &       ! Arguments related to the Y-Axis format
!               Arguments related to the Z-Axis object
                Z_Min, Z_Max, Z_Reverse,                                                                        &       ! Arguments related to the Z-Axis range
                Z_LogScale, Z_LogBase,                                                                          &       ! Arguments related to the Z-Axis scale
                Z_Label, Z_Label_Offset, Z_Label_Font, Z_Label_Font_Size, Z_Label_Color, Z_Label_Enhanced,      &       ! Arguments related to the Z-Axis label
                Z_Format )                                                                                      &       ! Arguments related to the Z-Axis format
                result(This)

  use GPF_Data_Base_Class       ,only:  GPF_Data_Base_Type

  implicit none

  type(GPF_Axes_Type)                                                   ::  This                            !< Axes object to be constructed
  class(GPF_Data_Base_Type)                             ,intent(in)     ::  Data_                           !< Data structure
  character(*)          ,dimension(:)                   ,intent(in)     ::  Axis_Type                       !< Axis types
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator
! Arguments related to the X-Axis object
  real(rkp)                                   ,optional ,intent(in)     ::  X_Min                           !< X-Axis minimum bound
  real(rkp)                                   ,optional ,intent(in)     ::  X_Max                           !< X-Axis maximum bound
  logical                                     ,optional ,intent(in)     ::  X_Reverse                       !< X-Axis reversing indicator
  logical                                     ,optional ,intent(in)     ::  X_LogScale                      !< X-Axis log scale indicator
  integer                                     ,optional ,intent(in)     ::  X_LogBase                       !< X-Axis log scale base
  character(*)                                ,optional ,intent(in)     ::  X_Label                         !< X-Axis label text
  character(*)                                ,optional ,intent(in)     ::  X_Label_Offset                  !< X-Axis label offset
  character(*)                                ,optional ,intent(in)     ::  X_Label_Font                    !< X-Axis label font name
  character(*)                                ,optional ,intent(in)     ::  X_Label_Font_Size               !< X-Axis label font size
  character(*)                                ,optional ,intent(in)     ::  X_Label_Color                   !< X-Axis label color
  logical                                     ,optional ,intent(in)     ::  X_Label_Enhanced                !< X-Axis label enhanced indicator
  character(*)                                ,optional ,intent(in)     ::  X_Format                        !< X-Axis format
! Arguments related to the Y-Axis object
  real(rkp)                                   ,optional ,intent(in)     ::  Y_Min                           !< Y-Axis minimum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Y_Max                           !< Y-Axis maximum bound
  logical                                     ,optional ,intent(in)     ::  Y_Reverse                       !< Y-Axis reversing indicator
  logical                                     ,optional ,intent(in)     ::  Y_LogScale                      !< Y-Axis log scale indicator
  integer                                     ,optional ,intent(in)     ::  Y_LogBase                       !< Y-Axis log scale base
  character(*)                                ,optional ,intent(in)     ::  Y_Label                         !< Y-Axis label text
  character(*)                                ,optional ,intent(in)     ::  Y_Label_Offset                  !< Y-Axis label offset
  character(*)                                ,optional ,intent(in)     ::  Y_Label_Font                    !< Y-Axis label font name
  character(*)                                ,optional ,intent(in)     ::  Y_Label_Font_Size               !< Y-Axis label font size
  character(*)                                ,optional ,intent(in)     ::  Y_Label_Color                   !< Y-Axis label color
  logical                                     ,optional ,intent(in)     ::  Y_Label_Enhanced                !< Y-Axis label enhanced indicator
  character(*)                                ,optional ,intent(in)     ::  Y_Format                        !< Y-Axis format
! Arguments related to the Z-Axis object
  real(rkp)                                   ,optional ,intent(in)     ::  Z_Min                           !< Z-Axis minimum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Z_Max                           !< Z-Axis maximum bound
  logical                                     ,optional ,intent(in)     ::  Z_Reverse                       !< Z-Axis reversing indicator
  logical                                     ,optional ,intent(in)     ::  Z_LogScale                      !< Z-Axis log scale indicator
  integer                                     ,optional ,intent(in)     ::  Z_LogBase                       !< Z-Axis log scale base
  character(*)                                ,optional ,intent(in)     ::  Z_Label                         !< Z-Axis label text
  character(*)                                ,optional ,intent(in)     ::  Z_Label_Offset                  !< Z-Axis label offset
  character(*)                                ,optional ,intent(in)     ::  Z_Label_Font                    !< Z-Axis label font name
  character(*)                                ,optional ,intent(in)     ::  Z_Label_Font_Size               !< Z-Axis label font size
  character(*)                                ,optional ,intent(in)     ::  Z_Label_Color                   !< Z-Axis label color
  logical                                     ,optional ,intent(in)     ::  Z_Label_Enhanced                !< Z-Axis label enhanced indicator
  character(*)                                ,optional ,intent(in)     ::  Z_Format                        !< Z-Axis format

  integer                                                               ::  iAxis                           ! Axis index


  call This%Set_Debug( Debug )                                                                                  ! Setting debugging indicator
  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Axes]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Axes]: Calling Data_%Get_NAxes')")
  This%NAxes    =       Data_%Get_NAxes()                                                                       ! Getting the number of axes

  allocate( This%Axis(This%NAxes) )                                                                             ! Allocating Axis component to the number of axis
  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Axes]: This%NAxes = ',i0)") This%NAxes

!     if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Axes]: Calling Construct_Axis: iAxis = ',i0,' => ',a)") iAxis, Axis_Type(iAxis)


  do iAxis = 1,This%NAxes                                                                                       ! Loop on all axis
    select case ( Axis_Type(iAxis) )
    case ('x')
      This%Axis(iAxis)  =       GPF_Axis_Type( Axis_Type(iAxis), Debug,                                         &       ! Constructing the X-axis
                Data_%Get_X_Min(), Data_%Get_X_Max(), X_Min, X_Max, X_Reverse,                                  &       ! Arguments related to the X-Axis range
                X_LogScale, X_LogBase,                                                                          &       ! Arguments related to the X-Axis scale
                X_Label, X_Label_Offset, X_Label_Font, X_Label_Font_Size, X_Label_Color, X_Label_Enhanced,      &       ! Arguments related to the X-Axis label
                X_Format                                                                                        )       ! Arguments related to the X-Axis format

    case ('y')
      This%Axis(iAxis)  =       GPF_Axis_Type( Axis_Type(iAxis), Debug,                                         &       ! Constructing the Y-axis
                Data_%Get_Y_Min(), Data_%Get_Y_Max(), Y_Min, Y_Max, Y_Reverse,                                  &       ! Arguments related to the Y-Axis range
                Y_LogScale, Y_LogBase,                                                                          &       ! Arguments related to the Y-Axis scale
                Y_Label, Y_Label_Offset, Y_Label_Font, Y_Label_Font_Size, Y_Label_Color, Y_Label_Enhanced,      &       ! Arguments related to the Y-Axis label
                Y_Format                                                                                        )       ! Arguments related to the Y-Axis format

    case ('z')
      This%Axis(iAxis)  =       GPF_Axis_Type( Axis_Type(iAxis), Debug,                                         &       ! Constructing the Z-axis
                Data_%Get_Z_Min(), Data_%Get_Z_Max(), Z_Min, Z_Max, Z_Reverse,                                  &       ! Arguments related to the Z-Axis range
                Z_LogScale, Z_LogBase,                                                                          &       ! Arguments related to the Z-Axis scale
                Z_Label, Z_Label_Offset, Z_Label_Font, Z_Label_Font_Size, Z_Label_Color, Z_Label_Enhanced,      &       ! Arguments related to the Z-Axis label
                Z_Format                                                                                        )       ! Arguments related to the Z-Axis format

    end select
  end do                                                                                                        ! End loop on axis

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Axes]: Exiting',/)")

End Function

Subroutine Write_Axes( This, Unit )
  implicit none
  class(GPF_Axes_Type)                                  ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Axis object
  integer                                               ,intent(in)     ::  Unit                            !< File unit number
  integer                                                               ::  iAxis                           ! Axis index
  if ( len_trim(This%Command) /= 0 ) write(Unit,"(/,a)") This%Command                                           ! Wrtting the comment line
  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Axes]: [DEBUG] This%NAxes = ',i0)")  This%NAxes
  do iAxis = 1,This%NAxes                                                                                       ! Loop on all axis
    if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Axes]: [DEBUG] Calling This%Axis(iAxis)%Write for iAxis = ',i0)")  iAxis
    call This%Axis(iAxis)%Write(Unit)                                                                           ! Writing the current Axis commands
  end do                                                                                                        ! End loop on axis
End Subroutine

Subroutine Set_Axes_Command( This )
  implicit none
  class(GPF_Axes_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis object
  character(*)                                              ,parameter  ::  Comment='# Axes Parameters: '   ! Comment line
  This%Command  =       Comment                                                                                 ! Setting the axis command to the comment
End Subroutine

End Module