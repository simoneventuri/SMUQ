Module GPF_Plot_Title_Class

  use GPF_Parameters            ,only:  DbgUnit
  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private

  public  ::  GPF_Plot_Title_Type

  Type  ,extends(GPF_Command_Type)                      ::  GPF_Plot_Title_Type
    character(:)        ,allocatable                    ::  Value                                           !< Character strong containing the title of current line
  contains
    private                                                                                                     ! Setting private type-bound procedures
    procedure   ,public   ::  GetValue               =>  Get_Title_Value                                 ! Gets the Title value
    procedure   ,public   ::  Set_Command             =>  Set_Title_Command                               ! Sets the command from object components
  End Type

  Interface             GPF_Plot_Title_Type
    Module Procedure    Construct_Plot_Title
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************


Function Construct_Plot_Title( iElt, iLine, Value, Debug ) result(This)

  implicit none

  type(GPF_Plot_Title_Type)                                             ::  This                            !< Passed-object dummy argument corresponding to the Plot-Title object
  integer                                               ,intent(in)     ::  iElt                            !< Index of current element
  integer                                               ,intent(in)     ::  iLine                           !<
  character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  Value                           !< Value of the Title option
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator

  character(*)                                              ,parameter  ::  Keyword='title'

  call This%Set_Debug( Debug )
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Title]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Title]: iElt = ',g0)") iElt

  This%Value    =       ""                                                                                      ! Initializing the title value to an empty string so that, by default, no line's title are written
  if ( present(Value) ) This%Value = trim(Value(iLine))                                                         ! If the optional input argument is present, then setting the line's title to the input value

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Title]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the command keyword

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Title]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the command from object components

  if (This%i_Debug) then
    write(DbgUnit,"(14x,'[Construct_Plot_Title]: This%Command   = ',a)")        This%Command
    write(DbgUnit,"(14x,'[Construct_Plot_Title]: Exiting')")
  end if

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PUBLIC PROCEDURES                                                  *
! **************************************************************************************************************
! **************************************************************************************************************

Function Get_Title_Value( This ) result(Value)
  implicit none
  class(GPF_Plot_Title_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Title object
  character(:)  ,allocatable                                            ::  Value                           !< Output character string corresponding to the title value
  Value         =       This%Value                                                                              ! Setting the title's value in the output string
End Function


! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Set_Title_Command( This )
  use GPF_Tools                 ,only:  Add_Apostroph
  implicit none
  class(GPF_Plot_Title_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Title object
  This%Command          =       " notitle "                                                                     ! Initializing the command to an empty string
  if ( len_trim(This%Value) /= 0 ) This%Command = This%Keyword // Add_Apostroph( This%Value ) // " "            ! Setting the value associated to the every option if any
End Subroutine

End Module