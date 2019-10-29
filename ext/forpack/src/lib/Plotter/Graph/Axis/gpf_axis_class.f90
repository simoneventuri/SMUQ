
! @TODO: Check format validity in the Set_Format procedure (ex: set format x "10^{%L}" )

Module GPF_Axis_Class

  use GPF_Parameters            ,only:  DbgUnit, rkp
  use GPF_Command_Class         ,only:  GPF_Command_Type
  use GPF_Class                 ,only:  GPF

  use GPF_Axis_LogScale_Class   ,only:  GPF_Axis_LogScale_Type
  use GPF_Axis_Format_Class     ,only:  GPF_Axis_Format_Type
  use GPF_Axis_Range_Class      ,only:  GPF_Axis_Range_Type
  use GPF_Axis_Label_Class      ,only:  GPF_Axis_Label_Type

  implicit none

  private

  public  ::  GPF_Axis_Type

  Type  ,extends(GPF_Command_Type) ::  GPF_Axis_Type
    character(:)        ,allocatable                    ::  Name                                            !< Axis Name
    type(GPF_Axis_LogScale_Type)                        ::  LogScale                                        !< Axis LogScale object
    type(GPF_Axis_Format_Type)                          ::  Format                                          !< Axis format object
    type(GPF_Axis_Range_Type)                           ::  Range_                                          !< Axis Range object
    type(GPF_Axis_Label_Type)                           ::  Label                                           !< Axis label object
  contains
    private
    procedure   ,public   ::  Write           =>  Write_Axis                                      !< Writes the axis commands
    procedure             ::  Initialize      =>  Initialize_Axis                                 !< Initializes the object components
    procedure             ::  Set_Name        =>  Set_Axis_Name                                   !< Sets the axis name
    procedure             ::  Set_LogScale    =>  Set_Axis_LogScale                               !< Sets the axis scale
    procedure             ::  Set_Format      =>  Set_Axis_Format                                 !< Sets the axis format
    procedure             ::  Set_Range       =>  Set_Axis_Range                                  !< Sets the axis range
    procedure             ::  Set_Label       =>  Set_Axis_Label                                  !< Sets the axis label
    procedure   ,public   ::  Set_Command     =>  Set_Axis_Command                                !< Sets the axis command
  End Type

  Interface             GPF_Axis_Type
    Module Procedure    Construct_Axis
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Axis( Name, Debug,                                                           &
                Data_Min, Data_Max, Axis_Min, Axis_Max, Reverse,                                &               ! Arguments related to the Axis range
                LogScale, LogBase,                                                              &               ! Arguments related to the Axis scale
                Text, Offset, Font_Name, Font_Size, Color, Enhanced,                            &               ! Arguments related to the Axis label
                Format )                                                                        &               ! Arguments related to the Axis format
                result(This)

  implicit none

  type(GPF_Axis_Type)                                                   ::  This                            !< Axis object to be constructed
  character(*)                                          ,intent(in)     ::  Name                            !< Axis name
  logical                                     ,optional ,intent(in)     ::  Debug                           !< debugging indicator
! Arguments related to the Axis range
  real(rkp)                                   ,optional ,intent(in)     ::  Data_Min                        !< Data minimum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Data_Max                        !< Data maximum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Axis_Min                        !< Axis minimum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Axis_Max                        !< Axis maximum bound
  logical                                     ,optional ,intent(in)     ::  Reverse                         !< Axis reversing indicator
! Arguments related to the Axis scale
  logical                                     ,optional ,intent(in)     ::  LogScale                        !< Axis log scale indicator
  integer                                     ,optional ,intent(in)     ::  LogBase                         !< Axis log scale base
! Arguments related to the Axis label
  character(*)                                ,optional ,intent(in)     ::  Text                            !< Axis label text
  character(*)                                ,optional ,intent(in)     ::  Offset                          !< Axis label offset
  character(*)                                ,optional ,intent(in)     ::  Font_Name                       !< Axis label font name
  character(*)                                ,optional ,intent(in)     ::  Font_Size                       !< Axis label font size
  character(*)                                ,optional ,intent(in)     ::  Color                           !< Axis label color
  logical                                     ,optional ,intent(in)     ::  Enhanced                        !< Axis label enhanced indicator
! Arguments related to the Axis format
  character(*)                                ,optional ,intent(in)     ::  Format                          !< Axis format

  call This%Set_Debug( Debug )                                                                                  ! Setting debugging indicator
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis]: Calling This%Set_Name')")
  call This%Set_Name( Name )                                                                                    ! Setting the axis name

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis]: Calling This%Initialize')")
  call This%Initialize()                                                                                        ! Initializing the object components to default values

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis]: Calling This%Set_LogScale')")
  call This%Set_LogScale( LogScale, LogBase )                                                                   ! Setting the axis scale

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis]: Calling This%Set_Label')")
  call This%Set_Format( Format, This%LogScale%LogScale )                                                        ! Setting the axis format

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis]: Calling This%Set_Range')")
  call This%Set_Range( Data_Min, Data_Max, Axis_Min, Axis_Max, Reverse )                                        ! Setting the axis range

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis]: Calling This%Set_Label')")
  call This%Set_Label( Text, Offset, Font_Name, Font_Size, Color, Enhanced )                                    ! Setting the axis label

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the axis command (actually, it is just a comment)

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis]: Exiting')")

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PUBLIC PROCEDURES                                                  *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Write_Axis( This, Unit )
  implicit none
  class(GPF_Axis_Type)                                  ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Axis object
  integer                                               ,intent(in)     ::  Unit                            !< File unit number
  if ( .not.This%Presence ) return
  if ( len_trim(This%Command)          /= 0 )   write(Unit,"(/,a)")     This%Command
  if ( len_trim(This%LogScale%Command) /= 0 )   write(Unit,"(a)")       This%LogScale%Command
  if ( len_trim(This%Format%Command)   /= 0 )   write(Unit,"(a)")       This%Format%Command
  if ( len_trim(This%Range_%Command)   /= 0 )   write(Unit,"(a)")       This%Range_%Command
  if ( len_trim(This%Label%Command)    /= 0 )   write(Unit,"(a)")       This%Label%Command
End Subroutine

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

! REMARK:
! The Name component must  not be initialized
Subroutine Initialize_Axis( This )
  implicit none
  class(GPF_Axis_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis object
  if (This%i_Debug) write(DbgUnit,"(16x,'[Initialize_Axis]: Entering')")
  This%LogScale         =       GPF_Axis_LogScale_Type( This%Name )                                             ! Initializing the LogScale component: default value => Linear scaling
  This%Format           =       GPF_Axis_Format_Type( This%Name )                                               ! Initializing the Format component: default value => No format specified
  This%Range_           =       GPF_Axis_Range_Type( This%Name )                                                ! Initializing the Range_ component:   default value => Auto-scaling
  if (This%i_Debug) write(DbgUnit,"(16x,'[Initialize_Axis]: Exiting',/)")
End Subroutine

Subroutine Set_Axis_Name( This, Name )
  implicit none
  class(GPF_Axis_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis object
  character(*)                                          ,intent(in)     ::  Name                            !< Axis name
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Axis_Name]: Entering')")
  This%Name     =       Name                                                                                    ! Setting axis name
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_Axis_Name]: This%Name = ',a)") This%Name
    write(DbgUnit,"(16x,'[Set_Axis_Name]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Axis_LogScale( This, LogScale, LogBase )
  implicit none
  class(GPF_Axis_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis object
  logical                                     ,optional ,intent(in)     ::  LogScale                        !< Axis log scale indicator
  integer                                     ,optional ,intent(in)     ::  LogBase                         !< Axis log scale base
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Axis_LogScale]: Entering')")
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Axis_LogScale]: Calling This%LogScale%Update')")
  call This%LogScale%Update( This%Name, LogScale, LogBase, This%i_Debug )                                       ! Constructing the Axis-LogScale object
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_Axis_LogScale]: This%LogScale%Command = ',a )") This%LogScale%Command
    write(DbgUnit,"(16x,'[Set_Axis_LogScale]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Axis_Format( This, Format, LogScale )
  implicit none
  class(GPF_Axis_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis object
  character(*)                                ,optional ,intent(in)     ::  Format                          !< Axis format
  logical                                     ,optional ,intent(in)     ::  LogScale                        !< Axis log scale indicator
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Axis_Format]: Entering')")
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Axis_Format]: Calling This%Format%Update')")
  call This%Format%Update( This%Name, Format, LogScale, This%i_Debug )                                          ! Constructing the Axis-Format object
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_Axis_Label]: This%Format%Command = ',a )") This%Format%Command
    write(DbgUnit,"(16x,'[Set_Axis_Format]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Axis_Range( This, Data_Min, Data_Max, Axis_Min, Axis_Max, Reverse )
  implicit none
  class(GPF_Axis_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis object
  real(rkp)                                   ,optional ,intent(in)     ::  Data_Min                        !< Data minimum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Data_Max                        !< Data maximum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Axis_Min                        !< Axis minimum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Axis_Max                        !< Axis maximum bound
  logical                                     ,optional ,intent(in)     ::  Reverse                         !< Axis reversing indicator
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Axis_Range]: Entering')")
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Axis_Range]: Calling This%Range_%Update')")
  call This%Range_%Update( This%Name, Data_Min, Data_Max, Axis_Min, Axis_Max, Reverse, This%i_Debug )           ! Constructing the Axis-Range object
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_Axis_Range]: This%Range_%Command = ',a )") This%Range_%Command
    write(DbgUnit,"(16x,'[Set_Axis_Range]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Axis_Label( This, Text, Offset, Font_Name, Font_Size, Color, Enhanced )
  implicit none
  class(GPF_Axis_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis object
  character(*)                                ,optional ,intent(in)     ::  Text                            !< Axis label text
  character(*)                                ,optional ,intent(in)     ::  Offset                          !< Axis label offset
  character(*)                                ,optional ,intent(in)     ::  Font_Name                       !< Axis label font name
  character(*)                                ,optional ,intent(in)     ::  Font_Size                       !< Axis label font size
  character(*)                                ,optional ,intent(in)     ::  Color                           !< Axis label color
  logical                                     ,optional ,intent(in)     ::  Enhanced                        !< Axis label enhanced indicator
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Axis_Label]: Entering')")
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Axis_Label]: Calling GPF_Axis_Label_Type')")
  This%Label    =       GPF_Axis_Label_Type( This%Name, Text, Offset, Font_Name, Font_Size, Color, Enhanced, This%i_Debug ) ! Constructing the Axis-Label object
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_Axis_Label]: This%Label%Command = ',a )") This%Label%Command
    write(DbgUnit,"(16x,'[Set_Axis_Label]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Axis_Command( This )
  implicit none
  class(GPF_Axis_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis object
  character(*)                                              ,parameter  ::  Comment='# Axis Parameters: '   ! Comment line
  if ( len_trim(This%LogScale%Command) /= 0 )   This%Presence   =       .True.
  if ( len_trim(This%Format%Command)   /= 0 )   This%Presence   =       .True.
  if ( len_trim(This%Range_%Command)   /= 0 )   This%Presence   =       .True.
  if ( len_trim(This%Label%Command)    /= 0 )   This%Presence   =       .True.
  if ( This%Presence ) This%Command = Comment // This%Name                                                      ! Setting the axis command to the comment
End Subroutine

End Module