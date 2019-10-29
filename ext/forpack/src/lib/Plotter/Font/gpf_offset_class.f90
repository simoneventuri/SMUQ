Module GPF_Offset_Class

  use GPF_Parameters            ,only:  DbgUnit
  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private

  public  ::  GPF_Offset_Type

  Type  ,extends(GPF_Command_Type)                      ::  GPF_Offset_Type
    private
    character(:)        ,allocatable                    ::  Coordinates                                     !< Offset values which corresponds to either and empty string (no offset), to "x,y" or to "x,y,z"
  contains
    private
    procedure             ::  Set_Coordinates =>  Set_Offset_Coordinates                  !< Sets the offset values
    procedure   ,public   ::  Set_Command     =>  Set_Offset_Command                      !< Sets the offset command
  End Type

  Interface             GPF_Offset_Type
    module procedure    Construct_Offset
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         CONSTRUCTOR PROCEDURES                                             *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Offset( Coordinates, Debug ) result(This)
  implicit none
  type(GPF_Offset_Type)                                                 ::  This                            !< Offset object to be constructed
  character(*)                                ,optional ,intent(in)     ::  Coordinates                     !< Offset values
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

  character(*)                                              ,parameter  ::  Keyword='offset'

  call This%Set_Debug( Debug )                                                                                  ! Setting the debugging indicator
  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Offset]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Offset]: Calling This%Set_Coordinates')")
  call This%Set_Coordinates( Coordinates )                                                                      ! Setting the offset values

  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Offset]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the keyword

  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Offset]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the command

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************
! @TODO: Write a Check_Coordinates_Validity procedure which checks if a given string corresponds to a coordinate specificaction.
! Such a character string must have the following format: {<system>} <x>, {<system>} <y> {,{<system>} <z>}
! where eatch <system> can either be first, second, graph, screen, or character.

Subroutine Set_Offset_Coordinates( This, Coordinates )
!   use GPF_Tools                 ,only:  Check_Coordinates_Validity
  implicit none
  class(GPF_Offset_Type)                                ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Offset object
  character(*)                                ,optional ,intent(in)     ::  Coordinates                     !< Offset values
  if (This%i_Debug) write(DbgUnit,"(18x,'[Set_Offset_Coordinates]: Entering')")
  This%Coordinates      =       ''                                                                              ! Initialisation to an empty string: default value
  if ( present(Coordinates) ) This%Coordinates = trim( Coordinates )                      ! If present optional input argument, then checking and setting the offset values to the input value
  if ( len_trim(This%Coordinates) /= 0 ) This%Coordinates = This%Coordinates // " "                             ! Adding an extra space at the end of the string
  This%Presence         =       ( len_trim(This%Coordinates) /= 0 )                                             ! Setting the presence indicator if required
  if (This%i_Debug) then
    write(DbgUnit,"(18x,'[Set_Offset_Coordinates]: This%Coordinates = ',a )") This%Coordinates
    write(DbgUnit,"(18x,'[Set_Offset_Coordinates]: This%Presence    = ',l3)") This%Presence
    write(DbgUnit,"(18x,'[Set_Offset_Coordinates]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Offset_Command( This )
  implicit none
  class(GPF_Offset_Type)                                ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Offset object
  if (This%i_Debug) write(DbgUnit,"(18x,'[Set_Offset_Command]: Entering')")
  This%Command          =       ''                                                                              ! Setting the command to empty value
  if ( This%Presence ) This%Command = This%Keyword // This%Coordinates                                          ! If present offset specification, setting the offset command
  if (This%i_Debug) then
    write(DbgUnit,"(18x,'[Set_Offset_Command]: This%Command  = ',a )") This%Command
    write(DbgUnit,"(18x,'[Set_Offset_Command]: Exiting',/)")
  end if
End Subroutine

End Module