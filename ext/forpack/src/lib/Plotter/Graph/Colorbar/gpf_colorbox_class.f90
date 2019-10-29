
! @TODO: Define a Border class and a Border LineStyle

Module GPF_ColorBox_Class

  use GPF_Parameters            ,only:  DbgUnit
  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private

  public  ::  GPF_ColorBox_Type

  Type  ,extends(GPF_Command_Type)                      ::  GPF_ColorBox_Type
    private
    character(:)        ,allocatable                    ::  Orientation                                     !< ColorBox orientation (either 'vertical' or 'horizontal')
    character(:)        ,allocatable                    ::  Origin                                          !< ColorBox origin coordinates
    character(:)        ,allocatable                    ::  Size_                                           !< ColorBox size (height and width)
    character(:)        ,allocatable                    ::  User                                            !< ColorBox user option
    character(:)        ,allocatable                    ::  Position                                        !< ColorBox position (either 'front' or 'back')
    character(:)        ,allocatable                    ::  Border                                          !< ColorBox borders type
    logical                                             ::  UnSetting                                       !< ColorBox unsetting indicator
  contains
    private
    procedure             ::  Set_Orientation =>  Set_ColorBox_Orientation                        !< Sets the ColorBox orientation
    procedure             ::  Set_Origin      =>  Set_ColorBox_Origin                             !< Sets the ColorBox origin
    procedure             ::  Set_Size        =>  Set_ColorBox_Size                               !< Sets the ColorBox size
    procedure             ::  Set_User        =>  Set_ColorBox_User                               !< Sets the ColorBox user
    procedure             ::  Set_Position    =>  Set_ColorBox_Position                           !< Sets the ColorBox position
    procedure             ::  Set_Border      =>  Set_ColorBox_Border                             !< Sets the ColorBox borders
    procedure             ::  Set_Unsetting   =>  Set_ColorBox_Unsetting                          !< Sets the ColorBox unsetting indicator
    procedure   ,public   ::  Set_Command     =>  Set_ColorBox_Command                            !< Sets the ColorBox command
  End Type

  Interface             GPF_ColorBox_Type
    Module Procedure    Construct
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct( Orientation, Origin, Size_, Position, Border, UnSetting, Debug ) result(This)

  implicit none

  type(GPF_ColorBox_Type)                                               ::  This                            !< ColorBox object to be constructed
  character(*)                                ,optional ,intent(in)     ::  Orientation                     !< ColorBox orientation (either 'vertical' or 'horizontal')
  character(*)                                ,optional ,intent(in)     ::  Origin                          !< ColorBox origin coordinates ('x,y')
  character(*)                                ,optional ,intent(in)     ::  Size_                           !< ColorBox size ('h,w')
  character(*)                                ,optional ,intent(in)     ::  Position                        !< ColorBox position (either 'front' or 'back')
  character(*)                                ,optional ,intent(in)     ::  Border                          !< ColorBox borders type
  logical                                     ,optional ,intent(in)     ::  UnSetting                       !< ColorBox unsetting indicator
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

  character(*)                                              ,parameter  ::  Keyword='colorbox'

  call This%Set_Debug( Debug )                                                                                  ! Setting the debugging indicator
  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBox]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBox]: Calling This%Set_Orientation')")
  call This%Set_Orientation( Orientation )                                                                      ! Setting ColorBox Orientation

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBox]: Calling This%Set_Origin')")
  call This%Set_Origin( Origin )                                                                                ! Setting ColorBox Origin

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBox]: Calling This%Set_Size')")
  call This%Set_Size( Size_ )                                                                                   ! Setting ColorBox Size

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBox]: Calling This%Set_User')")
  call This%Set_User()                                                                                          ! Setting ColorBox User option

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBox]: Calling This%Set_Position')")
  call This%Set_Position( Position )                                                                            ! Setting ColorBox Position

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBox]: Calling This%Set_Border')")
  call This%Set_Border( Border )                                                                                ! Setting ColorBox Border

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBox]: Calling This%Set_Unsetting')")
  call This%Set_Unsetting( UnSetting )                                                                          ! Setting the ColorBox unsetting indicator

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBox]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the command keyword

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBox]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting ColorBox command

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBox]: Exiting',/)")

End Function


! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Set_ColorBox_Orientation( This, Orientation )
  use GPF_Parameters            ,only:  Orientation_Valid
  use GPF_Tools                 ,only:  Is_Valid
  implicit none
  class(GPF_ColorBox_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the ColorBox object
  character(*)                                ,optional ,intent(in)     ::  Orientation                     !< ColorBox Orientation
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_ColorBox_Orientation]: Entering')")
  This%Orientation      =       ''                                                                              ! Initialisation to empty string: default value
  if ( present(Orientation) ) then                                                                              ! If present optional input argument
    if ( Is_Valid(Orientation,Orientation_Valid) ) This%Orientation = trim(Orientation) // ' '                  ! If valid input argument, then setting position to input value
  end if                                                                                                        ! End if case on optional input argument presence
  if ( len_trim(This%Orientation) /= 0 ) This%Presence = .True.                                                 ! Setting the presence indicator if required
  if (This%i_Debug) then
    write(DbgUnit,"(18x,'[Set_ColorBox_Orientation]: This%Orientation = ',a )") This%Orientation
    write(DbgUnit,"(18x,'[Set_ColorBox_Orientation]: This%Presence    = ',l3)") This%Presence
    write(DbgUnit,"(18x,'[Set_ColorBox_Orientation]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_ColorBox_Origin( This, Coordinates )
  use GPF_Tools                 ,only:  Is_Valid_Coordinates
  implicit none
  class(GPF_ColorBox_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the ColorBox object
  character(*)                                ,optional ,intent(in)     ::  Coordinates                     !< Coordinates
  character(*)                                              ,parameter  ::  Prefix='origin '                ! Prefix string
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_ColorBox_Origin]: Entering')")
  This%Origin   =       ''                                                                                      ! Initialisation to empty string: default value
  if ( present(Coordinates) ) then                                                                              ! If present optional input argument
    if ( Is_Valid_Coordinates(trim(Coordinates)) ) This%Origin  =       Prefix // trim(Coordinates) // ' '      ! If valid input argument, then setting component to input value
  end if                                                                                                        ! End if case on optional input argument presence
  if ( len_trim(This%Origin) /= 0 )     This%Presence = .True.                                                  ! Setting the presence indicator if required
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_ColorBox_Origin]: This%Origin   = ',a )") This%Origin
    write(DbgUnit,"(12x,'[Set_ColorBox_Origin]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(12x,'[Set_ColorBox_Origin]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_ColorBox_Size( This, Coordinates )
  use GPF_Tools                 ,only:  Is_Valid_Coordinates
  implicit none
  class(GPF_ColorBox_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the ColorBox object
  character(*)                                ,optional ,intent(in)     ::  Coordinates                     !< Coordinates
  character(*)                                              ,parameter  ::  Prefix='size '                  ! Prefix string
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_ColorBox_Size]: Entering')")
  This%Size_    =       ''                                                                                      ! Initialisation to empty string: default value
  if ( present(Coordinates) ) then                                                                              ! If present optional input argument
    if ( Is_Valid_Coordinates(trim(Coordinates)) ) This%Size_   =       Prefix // trim(Coordinates) // ' '      ! If valid input argument, then setting component to input value
  end if                                                                                                        ! End if case on optional input argument presence
  if ( len_trim(This%Size_) /= 0 )      This%Presence = .True.                                                  ! Setting the presence indicator if required
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_ColorBox_Size]: This%Size_    = ',a )") This%Size_
    write(DbgUnit,"(12x,'[Set_ColorBox_Size]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(12x,'[Set_ColorBox_Size]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_ColorBox_User( This )
  implicit none
  class(GPF_ColorBox_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the ColorBox object
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_ColorBox_Size]: Entering')")
  This%User     =       ''                                                                                      ! Initialisation to empty string: default value
  if ( ( len_trim(This%Origin) /= 0 ) .or. ( len_trim(This%Size_) /= 0 ) ) This%User = 'user '                  ! If the origin of size component is non-empty, then setting the user option
  if ( len_trim(This%User) /= 0 )       This%Presence = .True.                                                  ! Setting the presence indicator if required
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_ColorBox_Size]: This%User     = ',a )") This%User
    write(DbgUnit,"(12x,'[Set_ColorBox_Size]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(12x,'[Set_ColorBox_Size]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_ColorBox_Position( This, Position )
  use GPF_Parameters            ,only:  Position_Valid
  use GPF_Tools                 ,only:  Is_Valid
  implicit none
  class(GPF_ColorBox_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the ColorBox object
  character(*)                                ,optional ,intent(in)     ::  Position                        !< ColorBox Position
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_ColorBox_Position]: Entering')")
  This%Position         =       ''                                                                              ! Initialisation to empty string: default value
  if ( present(Position) ) then                                                                                 ! If present optional input argument
    if ( Is_Valid(Position,Position_Valid) ) This%Position = trim(Position) // ' '                              ! If valid input argument, then setting position to input value
  end if                                                                                                        ! End if case on optional input argument presence
  if ( len_trim(This%Position) /= 0 ) This%Presence = .True.                                                    ! Setting the presence indicator if required
  if (This%i_Debug) then
    write(DbgUnit,"(18x,'[Set_ColorBox_Position]: This%Position = ',a )") This%Position
    write(DbgUnit,"(18x,'[Set_ColorBox_Position]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(18x,'[Set_ColorBox_Position]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_ColorBox_Border( This, Border )
  implicit none
  class(GPF_ColorBox_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the ColorBox object
  character(*)                                ,optional ,intent(in)     ::  Border                          !< ColorBox Border
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_ColorBox_Border]: Entering')")
  This%Border   =       ''                                                                                      ! Initialisation to empty string: default value
  if ( present(Border) ) then                                                                                   ! If present optional input argument
! @TODO: Define border LineStyle
  end if                                                                                                        ! End if case on optional input argument presence
  if ( len_trim(This%Border) /= 0 ) This%Presence = .True.                                                      ! Setting the presence indicator if required
  if (This%i_Debug) then
    write(DbgUnit,"(18x,'[Set_ColorBox_Border]: This%Border   = ',a )") This%Border
    write(DbgUnit,"(18x,'[Set_ColorBox_Border]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(18x,'[Set_ColorBox_Border]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_ColorBox_Unsetting( This, UnSetting )
  implicit none
  class(GPF_ColorBox_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the ColorBox object
  logical                                     ,optional ,intent(in)     ::  UnSetting                       !< Unsetting indicator
  if (This%i_Debug) write(DbgUnit,"(18x,'[Set_ColorBox_Unsetting]: Entering')")
  if ( present(UnSetting) ) This%UnSetting = UnSetting                                                          ! If present optional input argument, then setting the component value to the input value
  if (This%i_Debug) then
    write(DbgUnit,"(18x,'[Set_ColorBox_Unsetting]: This%UnSetting = ',l3)") This%UnSetting
    write(DbgUnit,"(18x,'[Set_ColorBox_Unsetting]: Exiting',/)")
  end if
End Subroutine


Subroutine Set_ColorBox_Command( This )
  implicit none
  class(GPF_ColorBox_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the ColorBox object
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_ColorBox_Command]: Entering')")
  This%Command  =       ''                                                                                      ! Initialisation to empty string: default value
  if (This%Presence) then
    This%Command        =       'set '                  // &
                                This%Keyword            // &
                                This%Orientation        // &
                                This%User               // &
                                This%Origin             // &
                                This%Size_              // &
                                This%Position
  end if
  if ( This%UnSetting ) This%Command = 'unset ' // This%Keyword
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_ColorBox_Command]: This%Command = ',a)") This%Command
    write(DbgUnit,"(12x,'[Set_ColorBox_Command]: Exiting',/)")
  end if
End Subroutine

End Module