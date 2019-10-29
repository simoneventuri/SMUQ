Module GPF_Plot_Class

  use GPF_Parameters            ,only:  DbgUnit, KEY_splot, rkp
  use GPF_Command_Class            ,only:  GPF_Command_Type
  use GPF_LineStyle_Class       ,only:  GPF_LineStyle_Type

  use GPF_Plot_Iteration_Class  ,only:  GPF_Plot_Iteration_Type
  use GPF_Plot_Every_Class      ,only:  GPF_Plot_Every_Type
  use GPF_Plot_Using_Class      ,only:  GPF_Plot_Using_Type
  use GPF_Plot_Title_Class      ,only:  GPF_Plot_Title_Type
  use GPF_Plot_With_Class       ,only:  GPF_Plot_With_Type

  implicit none

  private
  public  ::  GPF_Plot_Type

  Type  ,extends(GPF_Command_Type) ::  GPF_Plot_Type
    integer                                                     ::  NLines          =       1               !< Number of lines to be plotted
    integer                                                     ::  NAbsci          =       1               !< Number of abscisse
    integer                                                     ::  NElements                               !< Number of elements of the Plot object arrays' components
    integer                     ,dimension(:)   ,allocatable    ::  Line_To_Element                         !< Index mapping from lines to elements in the plot object
    character(:)                ,dimension(:)   ,allocatable    ::  Fct                                     !< Plot Function
    type(GPF_Plot_Iteration_Type) ,dimension(:) ,allocatable    ::  Iteration                               !< Plot Iteration option
    character(:)                ,dimension(:)   ,allocatable    ::  DataFile                                !< Plot DataFile
    type(GPF_Plot_Every_Type)   ,dimension(:)   ,allocatable    ::  Every                                   !< Plot "Every" option
    type(GPF_Plot_Using_Type)   ,dimension(:)   ,allocatable    ::  Using                                   !< Plot "Using" option
    type(GPF_Plot_Title_Type)   ,dimension(:)   ,allocatable    ::  Title                                   !< Plot "Title" option
    type(GPF_Plot_With_Type)    ,dimension(:)   ,allocatable    ::  With                                    !< Plot "With"  option
    character(:)                ,dimension(:)   ,allocatable    ::  Commands                                !< Plot command
! ****************************************************************************************************************
!     character(LenMax)                                           ::  FillStyle                               !< Plot FillStyle for lines of nature 'filledcurve'
! ****************************************************************************************************************
  contains
    private
    procedure   ,public   ::  Write           =>  Write_PLot                                      !< Writes the Plot commands
    procedure   ,public   ::  Insert_DataFile =>  Insert_Plot_DataFile                            !< Insertes the DataFile name in the plotting command (Used for MultiPlot only)
    procedure   ,public   ::  Get_Lines_Title                                                         !< Gets the lines title
    procedure             ::  Set_Dimension   =>  Set_Plot_Dimension                              !< Sets the dimension of array contains in the Plot object
    procedure             ::  Set_Function    =>  Set_Plot_Function                               !< Sets the Plot function
    procedure             ::  Set_Iteration   =>  Set_Plot_Iteration                              !< Sets the Plot iteration options, if any
    procedure             ::  Set_DataFile    =>  Set_Plot_DataFile                               !< Sets the Plot DataFile
    procedure             ::  Set_Every       =>  Set_Plot_Every                                  !< Sets the Plot "Every" option
    procedure             ::  Set_Using       =>  Set_Plot_Using                                  !< Sets the Plot "Using" option
    procedure             ::  Set_Title       =>  Set_Plot_Title                                  !< Sets the Plot "Title" option
    procedure             ::  Set_With        =>  Set_Plot_With                                   !< Sets the Plot "With"  option
    procedure             ::  Set_FillStyle   =>  Set_Plot_FillStyle                              !< Sets the Plot "FillStyle"  option
    procedure   ,public   ::  Set_Command     =>  Set_Plot_Command                                !< Sets the Plot Command
  End Type

  Interface             GPF_Plot_Type
    Module Procedure    Construct_Plot
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Plot( NLines, NAbsci, DataFile, Plot_Function, LineStyle,      &
                      CurveStyle, Plot_LineTitle, Plot_Every, Plot_FillStyle, CB_Values, Debug ) result(This)

  type(GPF_Plot_Type)                                                   ::  This                            !< Plot object to be constructed
  integer                                               ,intent(in)     ::  NLines                          !< Number of lines
  integer                                               ,intent(in)     ::  NAbsci                          !< Number of abscisse
  character(*)                                          ,intent(in)     ::  DataFile
  character(*)                                          ,intent(in)     ::  Plot_Function
  type(GPF_LineStyle_Type)      ,dimension(:)           ,intent(in)     ::  LineStyle                       !< LineStyle object
  character(*)          ,dimension(:)         ,optional ,intent(in)     ::  CurveStyle                      !< Curve's style (specified with the "with" option in gnuplot)
  character(*)          ,dimension(:)         ,optional ,intent(in)     ::  Plot_LineTitle                  !< Line Title
  character(*)          ,dimension(:)         ,optional ,intent(in)     ::  Plot_Every                      !< Line every
  character(*)                                ,optional ,intent(in)     ::  Plot_FillStyle                  !< Line FillStyle (for filledcurve line)
  real(rkp)             ,dimension(:)         ,optional ,intent(in)     ::  CB_Values                       !< ColorBox values (DIM=NLines)
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator

  call This%Set_Debug( Debug )
  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Plot]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Plot]: Calling This%Set_Dimension')")
  call This%Set_Dimension( LineStyle, NLines, NAbsci )                                                          ! Setting the dimension of arrays components of the Plot object

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Plot]: Calling This%Set_Function')")
  call This%Set_Function( Plot_Function )                                                                       ! Setting the plot function

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Plot]: Calling This%Set_Iteration')")
  call This%Set_Iteration( LineStyle )                                                                          ! Setting the plotting iteration options, if any

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Plot]: Calling This%Set_DataFile')")
  call This%Set_DataFile( DataFile )                                                                            ! Setting the datafile used for plotting

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Plot]: Calling This%Set_Every')")
  call This%Set_Every( Plot_Every )                                                                             ! Setting the plotting every options, if any

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Plot]: Calling This%Set_Using')")
  call This%Set_Using( LineStyle, CB_Values )                                                                   ! Setting the plotting using options, if any

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Plot]: Calling This%Set_Title')")
  call This%Set_Title( LineStyle, Plot_LineTitle )                                                              ! Setting Line Title

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Plot]: Calling This%Set_With')")
  call This%Set_With( LineStyle, CurveStyle )                                                                   ! Setting the "with" option

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Plot]: Calling This%Set_FillStyle')")
  call This%Set_FillStyle( Plot_FillStyle )                                                                     ! Setting the "Set_FillStyle" option

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Plot]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the plot command

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Plot]: Exiting',/)")

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PUBLIC PROCEDURES                                                  *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Write_PLot( This, Unit )
  class(GPF_Plot_Type)                                  ,intent(in)     ::  This                            !< Passed-object dummy argument
  integer                                               ,intent(in)     ::  Unit                            !< File unit number
  integer                                                               ::  iElt                            ! Elements index
  if ( len_trim(This%Command) /= 0 ) write(Unit,"(/,a)") This%Command
  do iElt = 1,This%NElements
    if ( len_trim(This%Commands(iElt)) /= 0 ) write(Unit,"(a)") trim( This%Commands(iElt) )     ! The "trim" function is required because there must not be a blanck character after the "\" character for continuation lines
  end do
End Subroutine

! REMARK:
! This procedure updates the name of the DataFile in both the associated object component This%DataFile and in
! the command string This%Command.
! This procedure is used only for MultiPlots. Whenever, MultiPlots are performed, this procedure is required
! because the Graph object (which is an array) is created before creating the File object.
! As a consequence, during the Graph object creation, the name of the DataFile is not yet known
! and the DataFile component thus corresponds to an empty string.
! Once all elements of the Graph object, the File object is then created. Since the name of the DataFile is
! specified during the File object creation, it is only known after the File object construction is performed.
! Then, the name of the DataFile is update in the Graph object by copying the value stored in the File object.
Subroutine Insert_Plot_DataFile( This, DataFile )
  class(GPF_Plot_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Plot object
  character(*)                                          ,intent(in)     ::  DataFile                        !< Name of the datafile to be inserted in the plotting command
  if (This%i_Debug) write(DbgUnit,"(12x,'[Insert_Plot_DataFile]: Entering')")
  if (This%i_Debug) write(DbgUnit,"(12x,'[Insert_Plot_DataFile]: Calling This%Set_DataFile')")
  call This%Set_DataFile( DataFile )                                                                            ! Setting the datafile used for plotting
  if (This%i_Debug) write(DbgUnit,"(12x,'[Insert_Plot_DataFile]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the plot command
  if (This%i_Debug) write(DbgUnit,"(12x,'[Insert_Plot_DataFile]: Exiting',/)")
End Subroutine

Subroutine Get_Lines_Title( This, Lines_Title )
  class(GPF_Plot_Type)                                  ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Plot object
  character(:)  ,dimension(:)   ,allocatable            ,intent(out)    ::  Lines_Title                     !< Lines' titles
  integer                                                               ::  iElt                            ! Index of elements
  integer                                                               ::  iLine                           ! Index of lines
  integer                                                               ::  Length                          ! Maximum length of character string
  logical                                                               ::  i_Debug_Loc=.False.             ! Local debugging indicator
  Length        =       0                                                                                       ! Initializing the command length
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements in order to get the maximum length of the command string
    Length      =       max( Length , len(This%Title(iElt)%GetValue()) )                                       ! Getting the maximum length of the command string
  end do                                                                                                        ! End loop on elements
  allocate( character(Length) :: Lines_Title(This%NLines) )                                                     ! Allocating the lines' title
  do iLine = 1,This%NLines
    iElt                =       This%Line_To_Element(iLine)
    Lines_Title(iLine)  =       This%Title(iElt)%GetValue()
  end do
  if (i_Debug_Loc) then
    do iLine = 1,This%NLines
      write(DbgUnit,"(12x,'[Get_Lines_Title]: iLine = ',i0,3x,'This%Line_To_Element(iLine) = ',i0,3x,'Lines_Title(iLine) = ',a)") iLine, This%Line_To_Element(iLine), Lines_Title(iLine)
    end do
  end if
End Subroutine



! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

! REMARK:
! This procedure set the number of elements of the arrays components contained in the Plot object.
! This number of elements, stored in the variable NElements, corresponds to the number of lines of the
! plotting instructions.
! Usually, each line to be plotted has a associated element in the Linestyle object and in the Plot object.
! so that NElements = NLines, where NLines is the number of lines to be plotted.
! This is not the case whenever a group of lines have their color defined by a palette.
! Indeed, in such case, all the "palette" lines are described by the same LineStyle element.
! Therefore, in order to reduce redundant information in the command file, the size of the LineStyle
! does not correspond to the number of lines (cf the GPF_LineStyle_Class module for more details).
! As a consequence, the size of the Plot object must be equal to the size of the LineStyle object.
! We always have NLines >= NElements
Subroutine Set_Plot_Dimension( This, LineStyle, NLines, NAbsci )

  class(GPF_Plot_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument
  type(GPF_LineStyle_Type)      ,dimension(:)           ,intent(in)     ::  LineStyle                       !< LineStyle object
  integer                                               ,intent(in)     ::  NLines                          !< Line number
  integer                                               ,intent(in)     ::  NAbsci                          !< Abscisse number

  integer                                                               ::  iElt                            ! Index of elements
  integer                                                               ::  iLine                           ! Index of lines
  integer                                                               ::  iLine_Ini                       ! Initial index of lines associated to a given element
  integer                                                               ::  iLine_Fin                       ! Final index of lines associated to a given element

  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_Dimension]: Entering')")

  This%NLines           =       NLines                                                                          ! Setting the number of lines to input value
  This%NAbsci           =       NAbsci                                                                          ! Setting the number of lines to the input value

  if ( any(LineStyle%i_Multiple_Lines) ) then                                                                   ! If the LineStyle object contains some elements which referes to multiple lines
    This%NElements      =       size(LineStyle)                                                                 ! Setting the number of elements to the size of the LineStyle object so that components of the Plot object has the same dimension than the LineStyle object
  else                                                                                                          ! Otherwise, the number of elements corresponds to the number of lines and so
    This%NElements      =       This%NLines                                                                     ! Setting the number of elements to the number of lines
  end if                                                                                                        ! End if case on presence of multiple lines in a single plot instruction

#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!   allocate( This%Line_To_Element(NLines) )
!   This%Line_To_Element = [(iLine,iLine=1,NLines)]
  allocate( This%Line_To_Element(NLines), source = [(iLine,iLine=1,NLines)] )
#else
  allocate( This%Line_To_Element, source = [(iLine,iLine=1,NLines)] )
#endif


  do iElt = 1,This%NElements
    iLine_Ini           =       LineStyle(iElt)%iLine_Ini
    iLine_Fin           =       LineStyle(iElt)%iLine_Fin
    This%Line_To_Element(iLine_Ini:iLine_Fin)   =       iElt
  end do

  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_Plot_Dimension]: This%NLines    = ',i0)") This%NLines
    write(DbgUnit,"(12x,'[Set_Plot_Dimension]: This%NAbsci    = ',i0)") This%NAbsci
    write(DbgUnit,"(12x,'[Set_Plot_Dimension]: This%NElements = ',i0)") This%NElements
    do iLine = 1,This%NLines
      write(DbgUnit,"(12x,'[Set_Plot_Dimension]: iLine = ',i0,3x,'This%Line_To_Element(iLine) = ',i0)") iLine, This%Line_To_Element(iLine)
    end do
    write(DbgUnit,"(12x,'[Set_Plot_Dimension]: Exiting',/)")
  end if

End Subroutine

! @TODO: Check if the input Plot_Function is valid
!       It should always be valid since the Plot_Function value is provided by the Data structre via the Get_Plot_Function function
!       The "optional" attribut could be added to the input argument, and a default value to the "plot" function could be set
Subroutine Set_Plot_Function( This, Plot_Function )

  class(GPF_Plot_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Plot object
  character(*)                                          ,intent(in)     ::  Plot_Function                   !< Type of plot

  integer                                                               ::  iElt                            ! Index of elements
  integer                                                               ::  Length                          ! Maximum length of character string

  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_Function]: Entering')")

  Length        =       len_trim(Plot_Function) + 1                                                             ! Setting the maximum length
  if ( allocated(This%Fct) ) deallocate( This%Fct )                                                             ! Deallocation of the structure component if required
  allocate( character(Length) :: This%Fct(This%NElements) )                                                     ! Allocating length and dimension

  This%Fct(:)   =       ""                                                                                      ! Initializing to a empty string
  This%Fct(1)   =       trim(Plot_Function)                                                                     ! Setting the plotting function for the first element to the input value

  if (This%i_Debug) then
    do iElt = 1,This%NElements
      write(DbgUnit,"(12x,'[Set_Plot_Function]: iElt = ',i3,3x,'This%Fct = ',a)") iElt, This%Fct(iElt)
    end do
    write(DbgUnit,"(12x,'[Set_Plot_Function]: Exiting',/)")
  end if

End Subroutine

Subroutine Set_Plot_Iteration( This, LineStyle )

  class(GPF_Plot_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Plot object
  type(GPF_LineStyle_Type)      ,dimension(:)           ,intent(in)     ::  LineStyle                       !< LineStyle object

  integer                                                               ::  iElt                            ! Index of elements
  integer                                                               ::  Length                          ! Maximum length of character string

  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_Iteration]: Entering')")

! ==============================================================================================================
!    CONSTRUCTING THE OBJECT
! ==============================================================================================================
  if ( allocated(This%Iteration) ) deallocate( This%Iteration )                                                 ! Deallocating the component if required
  allocate( This%Iteration( This%NElements ) )                                                                  ! Allocating the component to the number of elements
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements
    if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_Iteration]: iElt = ',i3,' Calling GPF_Plot_Iteration_Type')")iElt
    This%Iteration(iElt) = GPF_Plot_Iteration_Type( This%NAbsci, LineStyle(iElt), Debug=This%i_Debug)           ! Constructing current element
  end do                                                                                                        ! End loop on elements

! ==============================================================================================================
!    SETTING AN EQUAL LENGTH FOR THE COMMAND STRING ASSOCIATED TO EACH ELEMENT
! ==============================================================================================================
  Length        =       0                                                                                       ! Initializing the command length
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements in order to get the maximum length of the command string
    Length      =       max( Length , len(This%Iteration(iElt)%Command) )                                     ! Getting the maximum length of the command string
  end do                                                                                                        ! End loop on elements
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements in order to get the maximum length of the command string
    call This%Iteration(iElt)%Set_Command_Length(Length)                                                            ! Changing the length of the command (adding extra blanks so that each element has a command of identical length)
  end do                                                                                                        ! End loop on elements

  if (This%i_Debug) then
    do iElt = 1,This%NElements
      write(DbgUnit,"(12x,'[Set_Plot_Iteration]: iElt = ',i3,3x,'This%Iteration%Command = ',a)") iElt, This%Iteration(iElt)%Command
    end do
    write(DbgUnit,"(12x,'[Set_Plot_Iteration]: Exiting',/)")
  end if

End Subroutine


Subroutine Set_Plot_DataFile( This, DataFile )
  class(GPF_Plot_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Plot object
  character(*)                                          ,intent(in)     ::  DataFile                        !< Name of the datafile
  integer                                                               ::  iElt                            ! Element index
  integer                                                               ::  Length                          ! Maximum length of character string
  character(*)                                              ,parameter  ::  Prefix='"'                      !
  character(*)                                              ,parameter  ::  Suffix='" '                     !
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_DataFile]: Entering')")
  Length        =       len(Prefix) + len_trim(DataFile) + len(Suffix)                                          ! Setting the maximum length
  if ( allocated(This%DataFile) ) deallocate( This%DataFile )                                                   ! Deallocation of the structure component if required
  allocate( character(Length) :: This%DataFile(This%NElements) )                                                ! Allocating length and dimension
#ifdef GFORTRAN_WORKAROUND_ALLOCATABLE_CHARACTER
  do iElt = 1,This%NElements
    This%DataFile(iElt) =       Prefix // trim(DataFile) // Suffix                                                      ! Setting the datafile name
  end do
#else
  This%DataFile =       Prefix // trim(DataFile) // Suffix                                                      ! Setting the datafile name
#endif

  if (This%i_Debug) then
    do iElt = 1,This%NElements
      write(DbgUnit,"(12x,'[Set_Plot_DataFile]: iElt = ',i3,3x,'This%DataFile = ',a)") iElt, This%DataFile(iElt)
    end do
    write(DbgUnit,"(12x,'[Set_Plot_DataFile]: Exiting',/)")
  end if
End Subroutine

! @TODO: Not all the optional argument are used for the Every constructor
Subroutine Set_Plot_Every( This, Every )

  class(GPF_Plot_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Plot object
  character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  Every                           !< Every option

  integer                                                               ::  iElt                            ! Element index
  integer                                                               ::  Length                          ! Maximum length of character string

  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_Every]: Entering')")

! ==============================================================================================================
!    CONSTRUCTING THE OBJECT
! ==============================================================================================================
  if ( allocated(This%Every) ) deallocate( This%Every )                                                         ! Deallocating the component if required
  allocate( This%Every( This%NElements ) )                                                                      ! Allocating the component to the number of elements
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements
    if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_Every]: iElt = ',i3,' Calling GPF_Plot_Every_Type')") iElt
    This%Every(iElt) = GPF_Plot_Every_Type( iElt, Value=Every, Debug=This%i_Debug)                                    ! Constructing current element
  end do                                                                                                        ! End loop on elements

! ==============================================================================================================
!    SETTING AN EQUAL LENGTH FOR THE COMMAND STRING ASSOCIATED TO EACH ELEMENT
! ==============================================================================================================
  Length        =       0                                                                                       ! Initializing the command length
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements in order to get the maximum length of the command string
    Length      =       max( Length , len(This%Every(iElt)%Command) )                                           ! Getting the maximum length of the command string
  end do                                                                                                        ! End loop on elements
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements in order to get the maximum length of the command string
    call This%Every(iElt)%Set_Command_Length(Length)                                                            ! Changing the length of the command (adding extra blanks so that each element has a command of identical length)
  end do                                                                                                        ! End loop on elements

! ==============================================================================================================
!    PRINTING DEBUGGING INFORMATION
! ==============================================================================================================
  if (This%i_Debug) then
    do iElt = 1,This%NElements
      write(DbgUnit,"(12x,'[Set_Plot_Every]: iElt = ',i3,3x,'This%Every(iElt)%Command = ',a)") iElt, This%Every(iElt)%Command
    end do
    write(DbgUnit,"(12x,'[Set_Plot_Every]: Exiting',/)")
  end if

End Subroutine

! @TODO: In procedure "Set_Plot_Using", the optional input argument "CB_Values" is not being used.
Subroutine Set_Plot_Using( This, LineStyle, CB_Values )

  class(GPF_Plot_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Plot object
  type(GPF_LineStyle_Type)      ,dimension(:)           ,intent(in)     ::  LineStyle                       !< LineStyle object
  real(rkp)             ,dimension(:)         ,optional ,intent(in)     ::  CB_Values                       !< ColorBox values (DIM=NLines)

  integer                                                               ::  iElt                            ! Element index
  integer                                                               ::  Length                          ! Maximum length of character string

  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_Using]: Entering')")


  if ( present(CB_Values) ) then
    if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_Using]: The CB_Values optional input argument is present but is not being used')")
  end if


! ==============================================================================================================
!    CONSTRUCTING THE OBJECT
! ==============================================================================================================
  if ( allocated(This%Using) ) deallocate( This%Using )                                                         ! Deallocating the component if required
  allocate( This%Using( This%NElements ) )                                                                      ! Allocating the component to the number of elements
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements
    if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_Using]: iElt = ',i3,' Calling Construct_Plot_Using')")iElt
    This%Using(iElt) = GPF_Plot_Using_Type( iElt, This%NLines, This%NAbsci, LineStyle(iElt), Debug=This%i_Debug) ! Constructing current element
  end do                                                                                                        ! End loop on elements

! ==============================================================================================================
!    SETTING AN EQUAL LENGTH FOR THE COMMAND STRING ASSOCIATED TO EACH ELEMENT
! ==============================================================================================================
  Length        =       0                                                                                       ! Initializing the command length
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements in order to get the maximum length of the command string
    Length      =       max( Length , len(This%Using(iElt)%Command) )                                           ! Getting the maximum length of the command string
  end do                                                                                                        ! End loop on elements
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements in order to get the maximum length of the command string
    call This%Using(iElt)%Set_Command_Length(Length)                                                            ! Changing the length of the command (adding extra blanks so that each element has a command of identical length)
  end do                                                                                                        ! End loop on elements

! ==============================================================================================================
!    FORCING THE USING OPTION FOR SURFACE PLOT (TODO)
! ==============================================================================================================
! @TODO: forcing the using option for surface plot WARNING
  if ( This%Fct(1) == KEY_splot ) then
    do iElt = 1,This%NElements
      This%Using(iElt)%Command  =       'using 1:2:3 '
    end do
  end if

! ==============================================================================================================
!    PRINTING DEBUGGING INFORMATION
! ==============================================================================================================
  if (This%i_Debug) then
    do iElt = 1,This%NElements
      write(DbgUnit,"(12x,'[Set_Plot_Using]: iElt = ',i3,3x,'This%Using(iElt)%Command = ',a)") iElt, This%Using(iElt)%Command
    end do
    write(DbgUnit,"(12x,'[Set_Plot_Using]: Exiting',/)")
  end if

End Subroutine

! @TODO: Find a way to check that the dimension of the option input argument Title is correct.
! The dimension of Title should be size(Title)=NLines.
! The old implementation was:

!   i_Default     =       .True.                                                                                  ! Initialization of the default indicator
!   if ( allocated(This%Title) ) deallocate( This%Title )                                                         ! Deallocation of the structure component if required
!
!   if ( present(Title) ) then                                                                                    ! If present optional input argument
!     if ( Right_Dimension( Title, This%NElements ) ) then                                                        ! If the input argument has the right dimension
!       i_Default =       .False.                                                                                 ! Unsetting the default indicator
!       Length    =       len('title "') + maxval(len_trim(Title)) + len('" ' )                                   ! Setting the maximum length of title elements
!       allocate( character(Length) :: This%Title(This%NElements) )                                               ! Allocating length and dimension
!       do iElt = 1,This%NElements                                                                                ! Loop on all Lines
!         if ( len_trim(Title(iElt)) /= 0 ) then                                                                  ! If the considered Line Title is not an empty string
!           This%Title(iElt)     =       'title "' // trim(Title(iElt)) // '" '                                   ! Setting Line Title to the input value
!         else                                                                                                    ! If the considered Line Title is an empty string
!           This%Title(iElt)     =       'notitle '                                                               ! Unsetting Line Title for current line
!         end if                                                                                                  ! End if case on length of the current line title
!       end do                                                                                                    ! End do loop on Line number
!     end if                                                                                                      ! End if case on input argument dimension
!   end if                                                                                                        ! End if case on presence of optional input argument
!
!   if (i_Default) then                                                                                           ! If default value are to be set
!     allocate( character(len(LineTitle_Default)) :: This%Title(This%NElements) )                                 ! Allocating length and dimension
!     This%Title  =       LineTitle_Default                                                                       ! Setting default title
!   end if                                                                                                        ! End if case on default values
!
Subroutine Set_Plot_Title( This, LineStyle, Title )

  class(GPF_Plot_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Plot object
  type(GPF_LineStyle_Type)      ,dimension(:)           ,intent(in)     ::  LineStyle                       !< LineStyle object
  character(*)          ,dimension(:)         ,optional ,intent(in)     ::  Title                           !< Title of lines

  integer                                                               ::  iElt                            ! Element index
  integer                                                               ::  Length                          ! Maximum length of character string

  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_Title]: Entering')")

  if (This%i_Debug)  then
    if ( present(Title) ) then
      do iElt = 1,size(Title)
        write(DbgUnit,"(12x,'[Set_Plot_Title]: iElt = ',i0,3x,'Title(iElt) = ',a)") iElt, Title(iElt)
      end do
    end if
  end if

! ==============================================================================================================
!    CONSTRUCTING THE OBJECT
! ==============================================================================================================
  if ( allocated(This%Title) ) deallocate( This%Title )                                                         ! Deallocating the component if required
  allocate( This%Title( This%NElements ) )                                                                      ! Allocating the component to the number of elements
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements
    if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_Title]: iElt = ',i3,' Calling Construct_Plot_Title')")iElt
    This%Title(iElt) = GPF_Plot_Title_Type( iElt, LineStyle(iElt)%iLine_Ini, Title, Debug=This%i_Debug)                                    ! Constructing current element
  end do                                                                                                        ! End loop on elements

! ==============================================================================================================
!    SETTING AN EQUAL LENGTH FOR THE COMMAND STRING ASSOCIATED TO EACH ELEMENT
! ==============================================================================================================
  Length        =       0                                                                                       ! Initializing the command length
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements in order to get the maximum length of the command string
    Length      =       max( Length , len(This%Title(iElt)%Command) )                                           ! Getting the maximum length of the command string
  end do                                                                                                        ! End loop on elements
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements in order to get the maximum length of the command string
    call This%Title(iElt)%Set_Command_Length(Length)                                                            ! Changing the length of the command (adding extra blanks so that each element has a command of identical length)
  end do                                                                                                        ! End loop on elements

! ==============================================================================================================
!    PRINTING DEBUGGING INFORMATION
! ==============================================================================================================
  if (This%i_Debug) then
    do iElt = 1,This%NElements
      write(DbgUnit,"(12x,'[Set_Plot_Title]: iElt = ',i3,3x,'This%Title(iElt)%Command = ',a)") iElt, This%Title(iElt)%Command
    end do
    write(DbgUnit,"(12x,'[Set_Plot_Title]: Exiting',/)")
  end if

End Subroutine

Subroutine Set_Plot_With( This, LineStyle, CurveStyle )

  class(GPF_Plot_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Plot object
  type(GPF_LineStyle_Type)      ,dimension(:)           ,intent(in)     ::  LineStyle                       !< LineStyle object
  character(*)          ,dimension(:)         ,optional ,intent(in)     ::  CurveStyle                      !< Curve' style

  integer                                                               ::  iElt                            ! Elements index
  integer                                                               ::  Length                          ! Maximum length of character string

  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_With]: Entering')")

! ==============================================================================================================
!    CONSTRUCTING THE OBJECT
! ==============================================================================================================
  if ( allocated(This%With) ) deallocate( This%With )                                                           ! Deallocating the component if required
  allocate( This%With( This%NElements ) )                                                                       ! Allocating the component to the number of elements
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements
    if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_With]: iElt = ',i3,' Calling GPF_Plot_With_Type')") iElt
    This%With(iElt) = GPF_Plot_With_Type( iElt, LineStyle(iElt), Style=CurveStyle, Debug=This%i_Debug )         ! Constructing current element
  end do                                                                                                        ! End loop on elementrs

! ==============================================================================================================
!    SETTING AN EQUAL LENGTH FOR THE COMMAND STRING ASSOCIATED TO EACH ELEMENT
! ==============================================================================================================
  Length        =       0                                                                                       ! Initializing the command length
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements in order to get the maximum length of the command string
    Length      =       max( Length , len(This%With(iElt)%Command) )                                            ! Getting the maximum length of the command string
  end do                                                                                                        ! End loop on elements
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements in order to get the maximum length of the command string
    call This%With(iElt)%Set_Command_Length(Length)                                                             ! Changing the length of the command (adding extra blanks so that each element has a command of identical length)
  end do                                                                                                        ! End loop on elements

! ==============================================================================================================
!    FORCING THE WITH OPTION FOR SURFACE PLOT (TODO)
! ==============================================================================================================
! @TODO: forcing the with option for surface plot WARNING
  if ( This%Fct(1) == KEY_splot ) then
    do iElt = 1,This%NElements
      This%With(iElt)%Command   =       'with pm3d '
    end do
  end if

! ==============================================================================================================
!    PRINTING DEBUGGING INFORMATION
! ==============================================================================================================
  if (This%i_Debug) then
    do iElt = 1,This%NElements
      write(DbgUnit,"(12x,'[Set_Plot_With]: iElt = ',i3,3x,'This%With(iElt)%Command = ',a)") iElt, This%With(iElt)%Command
    end do
    write(DbgUnit,"(12x,'[Set_Plot_With]: Exiting',/)")
  end if

End Subroutine

! @TODO: The Set_Plot_FillStyle procedure is not implemented
Subroutine Set_Plot_FillStyle( This, Plot_FillStyle )
  class(GPF_Plot_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Plot object
  character(*)                                ,optional ,intent(in)     ::  Plot_FillStyle                  !< Line FillStyle (for filledcurve line)
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_FillStyle]: Entering')")
  if ( present(Plot_FillStyle) ) then
    if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Arrow_From]: Plot_FillStyle = ',g0)") Plot_FillStyle
    if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Arrow_From]: <WARNING> Feature not implemented')")
  end if
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_FillStyle]: Exiting',/)")
End Subroutine

Subroutine Set_Plot_Command( This )

  class(GPF_Plot_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Plot object

  character(*)                                              ,parameter  ::  Comment='# Plot Parameters'     ! Comment line
  integer                                                               ::  iElt                            ! Elements index
  integer                                                               ::  Length                          ! Maximum length of character string

  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Plot_Command]: Entering')")

  This%Command  =       Comment                                                                                 ! Setting the Command component to the comment line


  Length        =       0                                                                                       ! Initializing the command length
  do iElt = 1,This%NElements                                                                                    ! Loop on all elements in order to get the maximum length of the command string
    Length      =       max( Length ,   len( This%Fct(iElt)                     )       &
                                    +   len( This%Iteration(iElt)%Command       )       &
                                    +   len( This%DataFile(iElt)                )       &
                                    +   len( This%Every(iElt)%Command           )       &
                                    +   len( This%Using(iElt)%Command           )       &
                                    +   len( This%Title(iElt)%Command           )       &
                                    +   len( This%With(iElt)%Command            )       &
                                    +   len(',\')                               )
  end do                                                                                                        ! End loop on elements

  if ( allocated(This%Commands) ) deallocate( This%Commands )                                                     ! Deallocation of the structure component if required
  allocate( character(Length) :: This%Commands(This%NElements) )                                                 ! Allocating length and dimension

  do iElt = 1,This%NElements                                                                                    ! Loop on all elements
    This%Commands(iElt) =       This%Fct(iElt)                  &
                        //      This%Iteration(iElt)%Command    &
                        //      This%DataFile(iElt)             &
                        //      This%Every(iElt)%Command        &
                        //      This%Using(iElt)%Command        &
                        //      This%Title(iElt)%Command        &
                        //      This%With(iElt)%Command
    if ( iElt /= This%NElements ) This%Commands(iElt) = trim( This%Commands(iElt) ) // ' ,\'
  end do                                                                                                      ! End do loop on elements

  if (This%i_Debug) then
     write(DbgUnit,"(12x,'[Set_Plot_Command]: This%Command = ',a)") This%Command
    do iElt = 1,This%NElements
      write(DbgUnit,"(12x,'[Set_Plot_Command]: iElt = ',i3,3x,'This%Commands = ',a)") iElt, This%Commands(iElt)
    end do
    write(DbgUnit,"(12x,'[Set_Plot_Command]: Exiting',/)")
  end if

End Subroutine


End Module