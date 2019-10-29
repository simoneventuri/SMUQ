Module GPF_Plot_With_Class

  use GPF_Parameters            ,only:  DbgUnit
  use GPF_Command_Class         ,only:  GPF_Command_Type
  use GPF_LineStyle_Class       ,only:  GPF_LineStyle_Type

  implicit none

  private

  public  ::  GPF_Plot_With_Type

  Type  ,extends(GPF_Command_Type)                      ::  GPF_Plot_With_Type
    character(:)                ,allocatable            ::  Style                                           !< Style of lines to be plotted
    character(:)                ,allocatable            ::  LineStyle                                       !< LineStyle of lins to be plotted
  contains
    private                                                                                                     ! Setting private type-bound procedures
    procedure             ::  Set_Style               =>  Set_With_Style                                  !< Sets the style of the line to be plotted
    procedure             ::  Set_LineStyle_Mapping   =>  Set_With_LineStyle_Mapping                      !< Sets the mapping with the associated LineStyle object
    procedure   ,public   ::  Set_Command             =>  Set_With_Command                                !< Sets the With command
  End Type

  Interface             GPF_Plot_With_Type
    Module Procedure    Construct_Plot_With
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Plot_With( iElt, LineStyle, Style, Debug ) result(This)

  implicit none

  type(GPF_Plot_With_Type)                                              ::  This                            !< Passed-object dummy argument corresponding to the Plot-With object
  integer                                               ,intent(in)     ::  iElt                            !< Index of current element
  type(GPF_LineStyle_Type)                              ,intent(in)     ::  LineStyle                       !< LineStyle object
  character(*)          ,dimension(:)         ,optional ,intent(in)     ::  Style                           !< Style of the line to be plotted
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator

  character(*)                                              ,parameter  ::  Keyword='with'

  call This%Set_Debug( Debug )
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_With]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_With]: Calling This%Set_Style')")
  call This%Set_Style( iElt, LineStyle, Style=Style )                                                           ! Setting the style of lines to be plotted

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_With]: Calling This%Set_LineStyle_Mapping')")
  call This%Set_LineStyle_Mapping( LineStyle )                                                                  ! Setting the LineStyle parameters of current line

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_With]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the command keyword

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_With]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the command

  if (This%i_Debug) then
    write(DbgUnit,"(14x,'[Construct_Plot_With]: This%Style      = ',a)") This%Style
    write(DbgUnit,"(14x,'[Construct_Plot_With]: This%LineStyle  = ',a)") This%LineStyle
    write(DbgUnit,"(14x,'[Construct_Plot_With]: This%Command    = ',a)") This%Command
    write(DbgUnit,"(14x,'[Construct_Plot_With]: Exiting',/)")
  end if

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

! If a Style is explicitely provided for lines, then their values are used.
! Otherwise, we use the LineStyle obbject to determine the linestyle.
!
Subroutine Set_With_Style( This, iElt, LineStyle, Style )
  use GPF_Tools                 ,only:  Check_Validity
  use GPF_Parameters            ,only:  Plot_LineType_Default, Plot_LineType_Valid, KEY_linespoints
  implicit none
  class(GPF_Plot_With_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Plot-With object
  integer                                               ,intent(in)     ::  iElt                            !< Index of current element
  type(GPF_LineStyle_Type)                              ,intent(in)     ::  LineStyle                       !< LineStyle object
  character(*)          ,dimension(:)         ,optional ,intent(in)     ::  Style                           !< style of lines to be plotted
!   logical                                                               ::  i_Default                       ! Default value indicator
!   i_Default     =       .True.                                                                                  ! Initializing the default indicator to false
!   if (i_Default)
  This%Style = Plot_LineType_Default                                                             ! If default value are to be set, then setting the linetype to the default value
  if ( present(Style) ) then                                                                                    ! If present optional input argument
!     i_Default   =       .False.                                                                                 ! Unsetting the default indicator
    This%Style  =       Check_Validity( Style(iElt), Plot_LineType_Valid, Plot_LineType_Default )               ! Checking validity of LineType and storing value in temporary variable
  else
    if (LineStyle%Is_PointType_Specified().or.LineStyle%Is_PointSize_Specified().or.LineStyle%Is_PointInterval_Specified())     &
    This%Style  =       KEY_linespoints
  end if                                                                                                        ! End if case on optional input argument presence
!   if (i_Default) This%Style = Plot_LineType_Default                                                             ! If default value are to be set, then setting the linetype to the default value
  This%Style = This%Style // " "                                                                                ! Adding an extra blank character at the end of the string
End Subroutine

Subroutine Set_With_LineStyle_Mapping( This, LineStyle )
  implicit none
  class(GPF_Plot_With_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Plot-With object
  type(GPF_LineStyle_Type)                              ,intent(in)     ::  LineStyle                       !< LineStyle object
  This%LineStyle        =       'ls ' // LineStyle%Get_Unit() // ' '
End Subroutine

Subroutine Set_With_Command( This )
  implicit none
  class(GPF_Plot_With_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Plot-With object
  This%Command  =       This%Keyword // This%Style // This%LineStyle                                            ! Setting the command associated to the with "option"
End Subroutine

End Module