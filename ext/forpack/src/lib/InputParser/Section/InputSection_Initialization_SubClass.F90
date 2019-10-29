SubModule(InputSection_Class) InputSection_Initialization_SubClass

  use Logger_Class            ,only:  Logger, LogLevel_HEAVYDEBUG
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.
  character(*)          ,parameter      ::  DefaultComment = "#"

  contains

Module Procedure ConstructSectionFromCharac

  use String_Library        ,only:  String_Type, LoadCharactersInStrings
  use InputParameter_Class  ,only:  InputParameter_Type

  character(*)                                              ,parameter  ::  ProcName = "ConstructSectionFromCharac"      ! Name of current procedure
  logical                                                               ::  Debug
  integer ,pointer                                                      ::  LogLevelPtr
  type(String_Type)         ,allocatable                                ::  Strings(:)                          ! Arrays of String objects
  integer                                                               ::  i
  type(InputSection_Type)    ,dimension(:)   ,allocatable               ::  Sections  ! Workaround for the pointer attribute
  type(InputParameter_Type)  ,dimension(:)   ,allocatable               ::  Parameters                      !< Arrays of Parameter objects to be stored in the "Parameters" componetns of the Section object being contructed

  Debug   =   .False.
  nullify(LogLevelPtr)
  if ( present(LogLevel) ) then
    LogLevelPtr => LogLevel
    call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_HEAVYDEBUG)
    Debug   =   Logger%On()
  end if

  if (Debug) call Logger%Write( "Loading the vector of characters into a vector of string objects" )
  if (Debug) call Logger%Write( "-> Calling LoadCharactersInStrings" )
  call LoadCharactersInStrings( Charac, Strings, LogLevel=LogLevelPtr )
  if (Debug) call Logger%Write( "-> Number of lines found: size(Strings) = ", size(Strings) )

  if (Debug) call Logger%Write( "Calling Construct_Sections" )
  call Construct_Sections( Strings, Sections, Parameters, Debug=Debug )
  Section    =   Sections(1)

  if ( present(LogLevel) ) call Logger%Exiting()
  nullify(LogLevelPtr)

End Procedure

Module Procedure ConstructSectionFromName
  Section%Name             =   Name
  allocate( Section%Sections(0) )
  if (allocated(Section%Parameters) ) deallocate( Section%Parameters )  ! @COMPILER_BUG ifort 17.0.4
  allocate( Section%Parameters(0) )
  Section%NSections        =   size(Section%Sections)
  Section%NParameters      =   size(Section%Parameters)
  Section%Defined          =   .True.
  Section%Empty            =   ( Section%NSections == 0 ) .and. ( Section%NParameters == 0 )
End Procedure

Module Procedure ConstructSectionFromParameter

  use String_Library                  ,only:  ParseFunction

  character(*)                                              ,parameter  ::  ProcName = "ConstructSectionFromParameter"
  logical                                                               ::  Dbg
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  String, FctName, ArgNames(:), ArgValues(:)
  character(:)  ,allocatable                                            ::  Name, Value

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  String     =   Param%GetRaw()

! Always set "DefToVal=True" otherwise when adding the parameter using "Section%AddParameter"
! the paramter will have no name but just a value.

  if (Dbg) call Logger%Write( "Calling ParseFunction: String = '"//String//"'" )
  call ParseFunction( String, FctName, ArgNames, ArgValues, DefArgNames, FctSep, ArgSep, ValSep, DefToVal=.False. )
!   call ParseFunction( String, FctName, ArgNames, ArgValues, DefToVal=.False. )
  if (Dbg) call Logger%Write( "-> FctName    = ", FctName )
  if (Dbg) call Logger%Write( "-> ArgNames   = ", ArgNames )
  if (Dbg) call Logger%Write( "-> ArgValues  = ", ArgValues )
  if (Dbg) call Logger%Write( "-> size(ArgNames)   = ", size(ArgNames)  )
  if (Dbg) call Logger%Write( "-> size(ArgValues)  = ", size(ArgValues) )

  if (Dbg) call Logger%Write( "Calling ConstructSection" )
  Section   =   ConstructSection(FctName)

  if (Dbg) call Logger%Write( "Adding parameters" )
  do i = 1,size(ArgNames)
    Name    =   trim( ArgNames(i)  )
    Value   =   trim( ArgValues(i) )
    if (Dbg) call Logger%Write( "-> i = ", i, "Calling Section%AddParameter: Name = ", Name, "Value = ", Value )
    call Section%AddParameter( Name, Value, Action="APPEND", Debug=Dbg )
  end do

  if (Dbg) call Logger%Exiting()

End Procedure




Module Procedure ExpandFunctionsToSections

  character(*)                                              ,parameter  ::  ProcName = "ExpandFunctionsToSections"
  logical                                                               ::  Dbg
  integer                                                               ::  i

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "-> This%GetNumberOfParameters() = ", This%GetNumberOfParameters() )
  do i = 1,This%GetNumberOfParameters()
    if (Dbg) call Logger%Write( "-> i = ", i, "This%Parameters(i)%IsFunction() = ", This%Parameters(i)%IsFunction() )
    if ( .Not. This%Parameters(i)%IsFunction() ) cycle

    call Logger%Write( "-> i = ", "Calling This%AddSection" )
    call This%AddSection( &
        ConstructSection( This%Parameters(i), Debug=Dbg ), &
        Action  =   "APPEND", &
        Debug   =   Dbg       )
  end do


  if (Dbg) call Logger%Exiting()

End Procedure



Module Procedure Construct_Sections

  use InputParameter_Class    ,only:  AddParameter
  use String_Library          ,only:  UpperCase, GetSubStringIndexes, GetFormat
  use Utilities_Library       ,only:  GetOptArgValue

  character(*)                                              ,parameter  ::  ProcName = "Construct_Sections"     ! Name of current procedure
  logical                                                               ::  Dbg
  integer                                                               ::  iSec                                ! Index of section
  integer                                                               ::  i                               ! Index of lines in the input file
  integer                                                               ::  iIni                                ! Index of initial line of a section
  integer                                                               ::  iFin                                ! Index of final line of a section
  integer       ,dimension(2)                                           ::  Indexes
  logical                                                               ::  InsideSection                      ! Indicator whether a section is being processed (we are "inside a section")
  logical                                                               ::  Start_Section                       ! Indicator whether a given line corresponds to the start of a section
  logical                                                               ::  Is_Set_iLine_Ini
  logical                                                               ::  Is_Set_iLine_Fin
  integer                                                               ::  iGroup, NGroups
  integer                                                               ::  Duplicate
  integer       ,allocatable    ,dimension(:)                           ::  iLine_Ini, iLine_Fin, List_iLines
  character(:)  ,allocatable                                            ::  Line, LineUC, SectionName, Fi

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

!   nullify(Sections)

!   call Line%Initialize( i_UpperCase=.True. )
  Duplicate             =   0
  iSec                  =   0                                                                                       ! Initializing the section index to zero (Required because of iterative computation)
  i                 =   0                                                                                       ! Initializing the line index to zero (Required because of iterative computation)
  iGroup                =   0
  InsideSection        =   .False.                                                                                 ! Initializing the section processing indicator to false (Required)
  Is_Set_iLine_Ini      =   .False.
  Is_Set_iLine_Fin      =   .False.
  SectionName           =   ""

  Fi    =   GetFormat( size(Lines) )

  if (Dbg) call Logger%Write( "Constructing array of Section/Parameter objects from array of String objects" )
  if (Dbg) call Logger%Write( "-> Looping on all String elements: size(Lines) = ", size(Lines) )
# define _Dbg !
! # define _Dbg
  do                                                                                                            ! Loop on all lines in the "Lines" object (Which correspond to the lines in the input file, omitting blanks and comments)
    i               =   i + 1                                                                       ! Incrementing line index
    if ( i > size(Lines) ) exit                                                                             ! Exiting the loop if all elements of the "Lines" object have been processed
    Line                 =   Lines(i)%GetValue()                                                        ! Extracting the current character string
    LineUC              =   UpperCase(Line)                                                                  ! ... and converting it to uppercase
    Start_Section       =   Section_Start( Line )                                                            ! Setting the indicator whether current line corresponds to the start of a section, that is, if the line is like: "START(<SectionName>))"

    _Dbg if (Dbg) call Logger%Write( "-> i = ", i, "Line = '"//Line//"'", Fi=Fi ) !, Newline=.True. )
    if ( len_trim(Line) == 0 ) cycle


!    CASE WHEN A SECTION IS BEING PROCESSED
! --------------------------------------------
! If a section is currently being processed, then we want to find the index of the line where the section ends.
! Then, using the index of the start and end of the section, and the section name, the section is created as
! and stored in the section component of the current section object.
! line corresponds to the start of a section:
    if (InsideSection) then                                                                                    ! If a section is being processed (If we are here, this means that a section name has been found and we now need to go to the end of the section)

      if ( .Not. Section_End(Line,SectionName) ) then
        if ( Section_Start(Line) ) then
          Indexes = GetSubStringIndexes( LineUC, [character(6)::"START(", ")"] )
          if ( Line(Indexes(1):Indexes(2)) == SectionName ) Duplicate = Duplicate + 1
        end if
        cycle                                                          ! If current line does not corresponds to the end of the section being processed, then we are still in the section being processed and so we need to cycle until the end of the section is found
      end if

      if ( Duplicate /= 0 ) then
        Duplicate = Duplicate - 1
        cycle
      end if

      _Dbg if (Dbg) call Logger%Write( "  -> Ending sub-section '" // SectionName // "'" )
      _Dbg if (Dbg) call Logger%Write( "  -> Adding Section to list of Sections" )
      iFin              =   i - 1                                                                       ! If the above line is passed, this means that current line corresponds to the end of the section being processed and so we need to save the index of the ending position of the section (the -1 is to remove the END(SECTION) line)
      iSec              =   iSec + 1                                                                        ! Incrementing section index
      InsideSection    =   .False.
      _Dbg if (Dbg) call Logger%Write( "  -> SectionName = ", SectionName )
      _Dbg if (Dbg) call Logger%Write( "  -> iSec = ", iSec, "iIni = ", iIni, "iFin = ", iFin )
      _Dbg if (Dbg) call Logger%Write( "  -> Calling AddElementToSectionsFromStrings" )
      call AddElementToSectionsFromStrings( Sections, SectionName, Lines(iIni:iFin), FileName=FileName, Debug=Debug )
      _Dbg if (Dbg) call Logger%Write( "  -> Done AddElementToSectionsFromStrings" )
      Is_Set_iLine_Ini  =   .False.
      cycle


!    CASE WHEN A SECTION IS NOT BEING PROCESSED
! -----------------------------------------------
! If a section is not currently being processed, then there are two case depending on whether or not the current
! line corresponds to the start of a section:
! - If current line corresponds to the start of a section then:
!   1) the section name have to be saved
!   2) the index where the section starts have to be saved (omiting the actual starting line)
!   3) the indicator for section processing have to be set to true
!   4) Cycle
! - If current line does not correspond to the start of a section then we are inside surrent section and so
!   the parameter should be added to the list of parameter of current section.
!   Note that each parameter are added one at the time suing the "AddParameter" procedure.
!   Thgis is rather inefficient since the list of parameter is re-allocated for each parameter.
!   A more efficient approach would be
!   1) to save the line index "i" corresponding to the start and end of the Lines object which correspond to parmaeter to be added.
!   2)
    else                                                                                                        ! If a section is not currently being processed
      if ( Section_Start(Line) ) then                                                                            ! If current line corresponds to the start of a section, then ...
        _Dbg if (Dbg) call Logger%Write( "  -> Starting sub-section" )
        Indexes       =   GetSubStringIndexes( LineUC, [character(6)::"START(", ")"] )
        SectionName   =   Line(Indexes(1):Indexes(2))
        iIni          =   i + 1
        InsideSection =   .True.
        if ( Is_Set_iLine_Ini ) Is_Set_iLine_Fin      =   .True. ! If "Is_Set_iLine_Ini" is true, then the line just before the one currently being processed corresponds to the index of the final parameter to be added
        _Dbg if (Dbg) call Logger%Write( "  -> SectionName = ", SectionName )
        cycle
      else

        _Dbg if (Dbg) call Logger%Write( "  -> Inside current section: Parameter definition" )
        if ( .Not. Is_Set_iLine_Ini ) then
          Is_Set_iLine_Ini      =   .True.
          _Dbg if (Dbg) call Logger%Write( "  -> Allocating iLine_Ini/iLine_Fin" )
          iGroup         =   iGroup + 1
          if ( .Not. allocated(iLine_Ini) ) allocate( iLine_Ini(0) )
          if ( allocated(List_iLines) ) deallocate(List_iLines)
          allocate( List_iLines, source = [iLine_Ini,i] )
          deallocate(iLine_Ini)
          allocate( iLine_Ini, source = List_iLines )
          if ( .Not. allocated(iLine_Fin) ) allocate( iLine_Fin(0) )
          if ( allocated(List_iLines) ) deallocate(List_iLines)
          allocate( List_iLines, source = [iLine_Fin,0] )
          deallocate(iLine_Fin)
          allocate( iLine_Fin, source = List_iLines )
        end if

        iLine_Fin(iGroup)       =   i
        _Dbg if (Dbg) call Logger%Write( "  -> iGroup = ", iGroup )
        _Dbg if (Dbg) call Logger%Write( "  -> iLine_Ini(:)  = ", iLine_Ini(:)  )
        _Dbg if (Dbg) call Logger%Write( "  -> iLine_Fin(:)  = ", iLine_Fin(:)  )

      end if
    end if

  end do
# undef _Dbg
  if (Dbg) call Logger%Write( "All elements processed" )
  if (Dbg) call Logger%Write( "-> allocated(Sections)   = ", allocated(Sections) )
  if (Dbg) call Logger%Write( "-> allocated(Parameters) = ", allocated(Parameters) )

  if ( .Not. allocated(Sections)    ) allocate( Sections(0)   )
  if ( .Not. allocated(Parameters)  ) allocate( Parameters(0) )

!   if ( iSec == 0 ) allocate(Sections(0))

  if (InsideSection) then
    call Logger%Write( "-> InsideSection = ", InsideSection )
    call Error_Section_Without_End( SectionName, FileName=FileName, CallProc=ProcName )
  end if

  NGroups    =   0
  if ( allocated(iLine_Ini) ) then
    NGroups  =   size(iLine_Ini)
    if ( .Not. allocated(iLine_Fin) ) allocate( iLine_Fin, source = [size(Lines)] )
  end if


! ==============================================================================================================
!   CONSTRUCTING THE PARAMETERS FROM THE LINES
! ==============================================================================================================
  if (Dbg) then
    call Logger%Write( "Constructing Parameters from lines" )
    call Logger%Write( "-> Number of groups of parameters to be processed: NGroups = ", NGroups )
  end if
  do iGroup = 1,NGroups   ! This loop is very long for large kinetic scheme
    iFin      =   iLine_Fin(iGroup)
    if (Dbg) call Logger%Write( "  -> iGroup = ", iGroup, "iLine_Ini(iGroup) = ", iLine_Ini(iGroup), "iLine_Fin(iGroup) = ", iLine_Fin(iGroup) )
    call AddParameter( Parameters, Lines(iLine_Ini(iGroup):iLine_Fin(iGroup)) )  ! Adding all consecutive Lines between iIni and iFin
  end do
  if (Dbg) call Logger%Write( "  -> size(Parameters) = ", size(Parameters) )
! ==============================================================================================================


  do iSec = 1,size(Sections)
  associate( Sec => Sections(iSec) )
    if (Dbg) call Logger%Write( "-> iSec = ", iSec )
    Sec%NSections     =   size(Sec%Sections)
    Sec%NParameters   =   size(Sec%Parameters)
    Sec%Empty         =   ( Sec%NSections == 0 ) .and. ( Sec%NParameters == 0 )
  end associate
  end do

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure InitializeSectionFromLines

  use Utilities_Library       ,only:  GetOptArgValue

  character(*)                                              ,parameter  ::  ProcName = "InitializeSectionFromLines" ! Name of current procedure
  logical                                                               ::  Dbg
  integer                                                               ::  i
  type(InputSection_Type)    ,dimension(:)   ,allocatable               ::  Sections  ! Workaround for the pointer attribute

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

# ifdef SUPPORTED_RECURSIVE_ALLOCATABLE_DERIVEDTYPE
    if (Dbg) call Logger%Write( "Calling Construct_Sections (allocatable)" )
    call Construct_Sections( Lines, This%Sections, This%Parameters, FileName=FileName, Debug=Debug )
    if (Dbg) call Logger%Write( "-> size(This%Sections)   = ", size(This%Sections) )
    if (Dbg) call Logger%Write( "-> size(This%Parameters) = ", size(This%Parameters) )
# else
    if (Dbg) call Logger%Write( "Calling Construct_Sections (pointer)" )
    call Construct_Sections( Lines, Sections, This%Parameters, FileName=FileName, Debug=Debug )
    if (Dbg) call Logger%Write( "-> size(Sections)        = ", size(Sections) )
    if (Dbg) call Logger%Write( "-> size(This%Parameters) = ", size(This%Parameters) )
    allocate( This%Sections(size(Sections)) )
    do i = 1,size(This%Sections)
      This%Sections(i)    =   Sections(i)
    end do
# endif

  This%Name             =   Name
  This%NSections        =   size(This%Sections)
  This%NParameters      =   size(This%Parameters)
  This%Defined          =   .True.
  This%Empty            =   ( This%NSections == 0 ) .and. ( This%NParameters == 0 )
  if (Dbg) then
    call Logger%Write( "This%Name        = ", This%Name )
    call Logger%Write( "This%NSections   = ", This%NSections   )
    call Logger%Write( "This%NParameters = ", This%NParameters )
    call Logger%Write( "This%Defined     = ", This%Defined )
    call Logger%Write( "This%Empty       = ", This%Empty )
  end if

  if (Dbg) call Logger%Exiting()

End Procedure


Module Procedure InitializeSectionFromCharacters
  use String_Module     ,only:  RemoveComment
  character(*)                                              ,parameter  ::  ProcName = "InitializeSectionFromCharacters" ! Name of current procedure
  logical                                                               ::  Dbg
  integer                                                               ::  i
  type(String_Type)     ,dimension(size(Charac))                        ::  Lines
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  Lines   =   Charac
  do i = 1,size(Lines)
    call Lines(i)%RemoveComment( DefaultComment )
  end do
  call This%Initialize( Name, Lines, FileName, Debug )
  if (Dbg) call Logger%Exiting()
End Procedure


Module Procedure InitializeSectionFromSectionsAndParameters

  use Utilities_Library       ,only:  GetOptArgValue

  character(*)                                              ,parameter  ::  ProcName = "InitializeSectionFromSectionsAndParameters" ! Name of current procedure
  logical                                                               ::  Dbg
  integer                                                               ::  i

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) then
    call Logger%Write( "Initializing a Section object from Name, Sections, Parameters" )
    call Logger%Write( "-> Name = ", Name )
    call Logger%Write( "-> size(Sections) = ", size(Sections) )
    do i = 1,size(Sections)
      call Logger%Write( "   -> i = ", i )
      call Logger%Write( "   -> Sections(i)%Name        = ", Sections(i)%Name )
      call Logger%Write( "   -> Sections(i)%NSections   = ", Sections(i)%NSections )
      call Logger%Write( "   -> Sections(i)%NParameters = ", Sections(i)%NParameters )
    end do
    call Logger%Write( "-> size(Parameters) = ", size(Parameters) )
    do i = 1,size(Parameters)
      call Logger%Write( "   -> i = ", i )
      call Logger%Write( "   -> Parameters(i)%Name      = ", Parameters(i)%Name )
      call Logger%Write( "   -> Parameters(i)%Value     = ", Parameters(i)%Value )
    end do
  end if

  if (Dbg) call Logger%Write( "Loading parameters" )
!   allocate( This%Parameters, source = Parameters )  ! @COMPILER_BUG
  allocate( This%Parameters( size(Parameters) ) )
  do i = 1,size(This%Parameters)
    This%Parameters(i)    =   Parameters(i)
  end do


  call Logger%Write( "Loading Sections" )
!   allocate( This%Sections,   source = Sections   )    ! @COMPILER_BUG !@TODO:WORKS?
  if ( RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(This%Sections) ) deallocate(This%Sections)
  allocate( This%Sections( size(Sections) ) )
  do i = 1,size(This%Sections)
    This%Sections(i)    =   Sections(i)
  end do

  This%Name             =   Name
  This%NSections        =   size(This%Sections)
  This%NParameters      =   size(This%Parameters)
  This%Defined          =   .True.
  This%Empty            =   ( This%NSections == 0 ) .and. ( This%NParameters == 0 )

  if (Dbg) then
    call Logger%Write( "Output section" )
    call Logger%Write( "-> This%Name = ", This%Name )
    call Logger%Write( "-> size(This%Sections) = ", size(This%Sections) )
    do i = 1,size(This%Sections)
      call Logger%Write( "   -> i = ", i )
      call Logger%Write( "   -> This%Sections(i)%Name        = ", This%Sections(i)%Name )
      call Logger%Write( "   -> This%Sections(i)%NSections   = ", This%Sections(i)%NSections )
      call Logger%Write( "   -> This%Sections(i)%NParameters = ", This%Sections(i)%NParameters )
    end do
    call Logger%Write( "-> size(This%Parameters) = ", size(This%Parameters) )
    do i = 1,size(This%Parameters)
      call Logger%Write( "   -> i = ", i )
      call Logger%Write( "   -> This%Parameters(i)%Name      = ", This%Parameters(i)%Name )
      call Logger%Write( "   -> This%Parameters(i)%Value    = ", This%Parameters(i)%Value )
    end do
    call Logger%Write( "This%Name        = ", This%Name )
    call Logger%Write( "This%NSections   = ", This%NSections   )
    call Logger%Write( "This%NParameters = ", This%NParameters )
    call Logger%Write( "This%Defined     = ", This%Defined )
    call Logger%Write( "This%Empty       = ", This%Empty  )
    call Logger%Exiting()
  end if

End Procedure


Module Procedure InitializeSectionFromParam

  use Utilities_Library       ,only:  GetOptArgValue
  use String_Library          ,only:  ParseFunction, Parse, RemoveQuotes

  character(*)                                              ,parameter  ::  ProcName='InitializeSectionFromParam'
  character(*)                                              ,parameter  ::  Separator = ","
  logical                                                               ::  Dbg
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  FctName, FctArgList, NamesValues(:,:)
  character(:)  ,allocatable                                            ::  Arguments(:), NameValue(:), Name, Value

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "-> Calling ParseFunction: Param%GetRaw() = '"//Param%GetRaw()//"'" )
  call ParseFunction( Param%GetRaw(), FctName, FctArgList )
!   call ParseFunction( Param%GetRaw(), FctName, NamesValues )
  if (Dbg) call Logger%Write( "-> FctName      = ", FctName )
  if (Dbg) call Logger%Write( "-> FctArgList   = ", FctArgList )
!   if (Dbg) call Logger%Write( "-> NamesValues   = ", NamesValues )

  if (Dbg) call Logger%Write( "-> Calling Parse: FctArgList = '"//FctArgList//"'" )
  call Parse( FctArgList, Separator, Arguments, IgnoreBetween=['"'] )
  if (Dbg) call Logger%Write( "-> size(Arguments) = ", size(Arguments) )
  if (Dbg) call Logger%Write( "-> i = ", "Arguments = ", Arguments )

  if (Dbg) call Logger%Write( "-> Calling This%Free()" )
  call This%Free()
  This%Name   =   FctName

!   if (Dbg) call Logger%Write( "-> Adding arguments as section parameters one-by-one" )
!   do i = 1,size(NamesValues,1)
!     call Section%AddParameter( NamesValues(i,1), NamesValues(i,2) )
!   end do
  do i = 1,size(Arguments)
    call Parse( Arguments(i), "=", NameValue, IgnoreBetween=['"'] )
    Name    =   trim( NameValue(1) )
    Value   =   RemoveQuotes( NameValue(2) )
    if (Dbg) call Logger%Write( "-> Calling This%AddParameter: i = ", i, "Name = ", Name, "Value = ", Value )
    call This%AddParameter( Name, Value )
  end do

!   if (Dbg) call This%Write(Logger)

  if (Dbg) call Logger%Exiting()

End Procedure


Module Procedure FreeSection
  call FinalizeSection(This)
End Procedure


Module Procedure FinalizeSection
  This%Mandatory    =   .False.
  This%Empty        =   .True.
  This%Defined      =   .False.
  This%NSections    =   0
  This%NParameters  =   0
  if ( allocated(  This%Name       ) ) deallocate( This%Name       )
  if ( allocated(  This%Parameters ) ) deallocate( This%Parameters )
  if ( RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED( This%Sections   ) ) deallocate( This%Sections )
End Procedure

Module Procedure SetSectionName
  This%Name     =   SectionName
End Procedure


! This procedure creates a sub-section structure inside the current section.
! The recursive structure os sub-sections to be created is specified by the "SubSections" input argument.
! This variable contains the name of the sub-section to be created inside the current section.
! If several level of sub-section neeed to be created, then the section names are separated by the ">"
! character.
! For example, the section object corresponding to the passed-object dummy argument has initially the
! following structure:
!
!     Start(Main_Section)
!     End(Main_Section)
!
! This corresponds to an empty section (ie. a section with no parameters nor sub-sections) called "Main_Section".
! Now, if we want to add a sub-section to produce the following:
!
!     Start(Main_Section)
!       Start(Sub_Section)
!       End(Sub_Section)
!     End(Main_Section)
!
! we just call:         CreateSubSections( This, "Sub_Section" )
! Let mak things a bit more complicates. To create the following structure
!
!     Start(Main_Section)
!       Start(Section_1)
!         Start(Section_1_A)
!           Start(Section_1_A_1)
!           End(Section_1_A_1)
!         End(Section_1_A)
!       End(Section_1)
!     End(Main_Section)
!
! the "CreateSubSections" has to be called using SubSections = "Section_1 > Section_1_A > Section_1_A_1"
Module Procedure CreateSubSections

  use Utilities_Library       ,only:  GetOptArgValue
  use InputSection_Tools      ,only:  ProcessListSectionNames

  character(*)                                              ,parameter  ::  ProcName = "CreateSubSections" ! Name of current procedure
  logical                                                               ::  Dbg
  character(:)  ,allocatable                                            ::  ListSectionNames
  character(:)  ,allocatable                                            ::  NewSectionNames
  type(InputSection_Type)                                               ::  NewSection
  type(InputSection_Type)     ,allocatable                              ::  SectionsList(:)
  integer                                                               ::  i
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  if (Dbg) call Logger%Write( "Adding sub-section '" //SubSections// "' to current section '" //This%Name//"'" )
  ListSectionNames  =   SubSections
  if (Dbg) call Logger%Write( "Calling ProcessListSectionNames" )
  call ProcessListSectionNames( NewSectionNames, ListSectionNames )
  if (Dbg) call Logger%Write( "-> NewSectionNames   = ", NewSectionNames )
  if (Dbg) call Logger%Write( "-> ListSectionNames  = ", ListSectionNames )
  NewSection  =   ConstructSection(NewSectionNames)
  if ( len_trim(ListSectionNames) /= 0 ) then
    if (Dbg) call Logger%Write( "Calling CreateSubSections")
    call NewSection%CreateSubSections( ListSectionNames, Debug )
  end if
!########################################################################
  if (Dbg) call Logger%Write( "Calling AddSection (AddSectionToSectionsFromSectionObject) ")
!   call AddSection( This%Sections, NewSection )                                  ! @COMPILER_BUG ifort 18.0.0: segmentation fault occurred
!   call AddSectionToSectionsFromSectionObject( This%Sections, NewSection )       ! @COMPILER_BUG ifort 18.0.0: segmentation fault occurred
!########################################################################
        if ( .Not. RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(This%Sections) ) allocate( This%Sections(0) )
        allocate( SectionsList, source = [This%Sections,NewSection] )
!         call move_alloc( SectionsList, This%Sections )
        deallocate( This%Sections )
        allocate( This%Sections(size(SectionsList)) )
        i = 1
        do i = 1, size(SectionsList)
          This%Sections(i) = SectionsList(i)
        end do
!########################################################################


  if (Dbg) call Logger%Write( "Done AddSection" )
  This%NSections        =   size(This%Sections)
  This%Defined          =   .True.
  This%Empty            =   ( This%NSections == 0 ) .and. ( This%NParameters == 0 )
  if (Dbg) call Logger%Write( "Section '"//NewSection%Name//"' has been created inside section '"//This%Name//"'" )
  if (Dbg) call Logger%Write( "This%NSections = ", This%NSections )

  if (Dbg) call Logger%Exiting()

End Procedure


Module Procedure GetSectionPointer

  use InputSection_Tools    ,only:  ProcessListSectionNames, ProcessCaseSensitiveString

  character(*)                                              ,parameter  ::  ProcName = "GetSectionPointer" ! Name of current procedure
  logical                                                               ::  Dbg
  integer                                                               ::  i                               ! Index of sub-section of current section
  character(:)  ,allocatable                                            ::  ListSectionNames                ! List of sub-section names (corresponds to the input argument "SubSectionName")
  character(:)  ,allocatable                                            ::  TargetSectionName               ! Name of the target section to be found as a sub-section of the passed-object dummy argument
  character(:)  ,allocatable                                            ::  CurrentSectionName              ! Name of the a given sub-section of the passed-object dummy argument

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Name of section to be pointed to" )
  if (Dbg) call Logger%Write( "-> Calling ProcessCaseSensitiveString" )
  ListSectionNames   =   ProcessCaseSensitiveString( SubSectionName, CaseSensitive )
  if (Dbg) call Logger%Write( "-> SubSectionName    = ", SubSectionName )
  if (Dbg) call Logger%Write( "-> ListSectionNames  = ", ListSectionNames )

  if (Dbg) call Logger%Write( "Calling ProcessListSectionNames" )
  call ProcessListSectionNames( TargetSectionName, ListSectionNames )
  if (Dbg) call Logger%Write( "-> TargetSectionName = ", TargetSectionName )
  if (Dbg) call Logger%Write( "-> ListSectionNames  = ", ListSectionNames )

  nullify( TargetSection )
  do i = 1,This%NSections
    CurrentSectionName  =   ProcessCaseSensitiveString( This%Sections(i)%Name, CaseSensitive )
    if (Dbg) call Logger%Write( "-> i = ", i, "CurrentSectionName = ", CurrentSectionName )
    if ( CurrentSectionName /= TargetSectionName ) cycle
    if ( len(ListSectionNames) == 0 ) then
      TargetSection   =>  This%Sections(i)
    else
      if (Dbg) call Logger%Write( "-> Processing subsection: ListSectionNames = ", ListSectionNames )
      call This%Sections(i)%GetSectionPointer( ListSectionNames, TargetSection, Debug=Debug )
    end if
    exit
  end do

  if (Dbg) call Logger%Exiting()

End Procedure







Module Procedure AssignSectionFromSection

  integer                                                               ::  i

  lhs%Mandatory   =   rhs%Mandatory
  lhs%Empty       =   rhs%Empty
  lhs%Defined     =   rhs%Defined
  lhs%NSections   =   rhs%NSections
  lhs%NParameters =   rhs%NParameters
  lhs%Name        =   rhs%Name

  if ( allocated(lhs%Parameters) ) deallocate( lhs%Parameters )
  if ( allocated(rhs%Parameters) ) then
    allocate( lhs%Parameters, source = rhs%Parameters )
  else
    allocate( lhs%Parameters(0) )
  end if

  if ( RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(lhs%Sections) ) deallocate( lhs%Sections )

  if ( RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(rhs%Sections) ) then
!     lhs%Sections    =   rhs%Sections                  !@COMPILER_BUG: Fails for ifort 19.0.3
!     allocate( lhs%Sections, source = rhs%Sections )   !@COMPILER_BUG: Fails for ifort 19.0.2
    allocate( lhs%Sections(rhs%NSections) )
    do i = 1, rhs%NSections
      lhs%Sections(i)   =   rhs%Sections(i)
    end do
  else
    allocate( lhs%Sections(0) )
  end if

End Procedure


Module Procedure ProcessEnvironmentVariables

  use Utilities_Library       ,only:  GetOptArgValue
  use String_Library          ,only:  GetSubString, Convert_Ratio, ReplaceCharacter
  use InputParameter_Class    ,only:  AddParameter

  character(*)                                              ,parameter  ::  ProcName = "ProcessEnvironmentVariables"
  character(*)                                              ,parameter  ::  InBetween(2) = ["${","} "]
  logical                                                               ::  Dbg
  integer                                                               ::  i

  if ( .Not. HasEnvVar(This) ) return

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Performing environment variables substitution for parameters in section '"//This%GetName()//"'" )
  do i = 1,This%GetNumberOfParameters()
    if (Dbg) call Logger%Write( "-> i = ", i, "Calling ProcessEnvironmentVariableInParameters" )
    call ProcessEnvironmentVariableInParameters( This%Parameters(i) )
  end do

  if (Dbg) call Logger%Write( "Performing environment variables substitution for subsections in section '"//This%GetName()//"'" )
  do i = 1,size(This%Sections)
    if (Dbg) call Logger%Write( "-> i = ", i, "Calling This%Sections(i)%ProcessEnvironmentVariables: This%Sections(i)%GetName() = ", This%Sections(i)%GetName() )
    call This%Sections(i)%ProcessEnvironmentVariables( Debug=Debug )
  end do

  if (Dbg) call Logger%Exiting()

  contains

Recursive Function HasEnvVar( This ) result(Indicator)
  type(InputSection_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument
  logical                                                               ::  Indicator
  integer                                                               ::  i
  Indicator   =   .False.
  do i = 1,This%GetNumberOfParameters()
    Indicator =   len_trim( GetSubString(This%Parameters(i)%GetRawValue(),InBetween) ) /= 0
    if (Indicator) return
  end do
  do i = 1,This%GetNumberOfSubSections()
    Indicator =   HasEnvVar( This%Sections(i) )
    if (Indicator) return
  end do
End Function

End Procedure


Recursive Subroutine ProcessEnvironmentVariableInParameters( Param )

  use String_Library              ,only:  GetSubString, ReplaceCharacter
  use EnvironmentVariable_Library ,only:  GetEnvVarValue

  type(InputParameter_Type)                             ,intent(inout)  ::  Param

  character(*)                                              ,parameter  ::  ProcName = "ProcessEnvironmentVariableInParameters"
  character(*)                                              ,parameter  ::  InBetween(2) = ["${","} "]
  integer                                                               ::  Status
  logical                                                               ::  Dbg, Found
  character(:)  ,allocatable                                            ::  OldValue, NewValue, VarName, VarValue

  Dbg   =   .False.!GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( "ProcessEnvironmentVariableInParameters" )

  OldValue    =   Param%GetRawValue()
  VarName     =   GetSubString( OldValue, InBetween=InBetween, Inner=.True. )
  Found       =   len_trim(VarName) /= 0

  if (Dbg) call Logger%Write( "Param%GetRaw() = '"//Param%GetRaw()//"'" )

  if ( .Not. Found ) then
    if (Dbg) call Logger%Write( "-> No environement variable detected => Exiting" )
    if (Dbg) call Logger%Exiting()
    return
  end if
  if (Dbg) call Logger%Write( "-> Environement variable detected" )

  if (Dbg) call Logger%Write( "Finding value associated to environement variable '"//VarName//"'" )
  if (Dbg) call Logger%Write( "-> Calling GetEnvVarValue: VarName = ", VarName )
  call GetEnvVarValue( VarName, VarValue, Status=Status )
  if ( Status /= 0 ) then
    if (Dbg) call Logger%Write( "-> Environement variable '"//VarName//"' not found => Exiting" )
    if (Dbg) call Logger%Exiting()
    return
  end if
  if (Dbg) call Logger%Write( "-> Environement variable '"//VarName//"' found with value '"//VarValue//"'" )

  VarName = trim(InBetween(1)) // VarName // trim(InBetween(2))
  if (Dbg) call Logger%Write( "-> Calling ReplaceCharacter('"//OldValue//"','"//VarName//"','"//VarValue//"')" )
  NewValue    =   ReplaceCharacter( OldValue, VarName, VarValue )
  if (Dbg) call Logger%Write( "-> NewValue = ", NewValue )

  if (Dbg) OldValue   =   Param%GetRaw()
  call Param%SetValue( NewValue, Debug=.False. )
  if (Dbg) NewValue   =   Param%GetRaw()
  if (Dbg) call Logger%Write( "****** Environement variable substitution: '"//OldValue//"' -> '"//NewValue//"' ******" )

  if (Dbg) call Logger%Write( "Calling ProcessEnvironmentVariableInParameters" )
  call ProcessEnvironmentVariableInParameters( Param )

  if (Dbg) call Logger%Exiting()

End Subroutine








Module Procedure ProcessMacroInSections

  use Utilities_Library       ,only:  GetOptArgValue
  use String_Library          ,only:  GetSubString, Convert_Ratio, ReplaceCharacter
  use InputParameter_Class    ,only:  AddParameter

  character(*)                                              ,parameter  ::  ProcName = "ProcessMacroInSections"
  character(*)                                              ,parameter  ::  InBetween(2) = ["$[","] "]
  logical                                                               ::  Dbg
  integer                                                               ::  i
  type(InputSection_Type)                                               ::  ParamList

  if ( .Not. HasMacro(This) ) return

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if ( present(Params) ) call ParamList%AddParameter( Params%Parameters )
  call ParamList%AddParameter( This%Parameters, AtStart=.True. )

  if (Dbg) call Logger%Write( "Performing macro substitution for parameters in section '"//This%GetName()//"'" )
  do i = 1,This%GetNumberOfParameters()
    if (Dbg) call Logger%Write( "-> i = ", i, "Calling ProcessMacroInParameters" )
    call ProcessMacroInParameters( This%Parameters(i), ParamList, Debug=Dbg )
  end do

  if (Dbg) call Logger%Write( "Performing macro substitution for subsections in section '"//This%GetName()//"'" )
  do i = 1,size(This%Sections)
    if (Dbg) call Logger%Write( "-> i = ", i, "Calling This%Sections(i)%ProcessMacroInSections: This%Sections(i)%GetName() = ", This%Sections(i)%GetName() )
    call This%Sections(i)%ProcessMacroInSections( Params=ParamList, Debug=Debug )
  end do

  if (Dbg) call Logger%Exiting()

  contains

Recursive Function HasMacro( This ) result(Indicator)
  type(InputSection_Type)                               ,intent(in)     ::  This
  logical                                                               ::  Indicator
  integer                                                               ::  i
  Indicator   =   .False.
  do i = 1,This%GetNumberOfParameters()
    Indicator =   len_trim( GetSubString(This%Parameters(i)%GetRawValue(),InBetween) ) /= 0
    if (Indicator) return
  end do
  do i = 1,This%GetNumberOfSubSections()
    Indicator =   HasMacro( This%Sections(i) )
    if (Indicator) return
  end do
End Function

End Procedure


Recursive Subroutine ProcessMacroInParameters( Param, ParamList, Debug )

  use String_Library          ,only:  GetSubString, ReplaceCharacter

  type(InputParameter_Type)                             ,intent(inout)  ::  Param
  type(InputSection_Type)                               ,intent(in)     ::  ParamList
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName = "ProcessMacroInSections"
  character(*)                                              ,parameter  ::  InBetween(2) = ["$[","] "]
  logical                                                               ::  Dbg, Found
  character(:)  ,allocatable                                            ::  OldValue, NewValue, VarName, VarValue

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( "ProcessMacroInParameters" )

  OldValue    =   Param%GetRawValue()
  VarName   =   GetSubString( OldValue, InBetween=InBetween, Inner=.True. )
  Found       =   len_trim(VarName) /= 0


  if ( .Not. Found ) then
    if (Dbg) call Logger%Write( "-> No macro detected for parameter '"//Param%GetRaw()//"' => Exiting" )
    if (Dbg) call Logger%Exiting()
    return
  end if

!   if (Dbg) call Logger%Write( "-> Macro detected: '"//Param%GetRaw()//"'" )

!   Searching for the value associated to the macro 'VarName' in the list of known macros
!   if (Dbg) call Logger%Write( "Finding value associated to macro '"//VarName//"'" )
!   if (Dbg) call Logger%Write( "-> Calling ParamList%GetValue: VarName = ", VarName )
  call ParamList%GetValue( VarValue, VarName, Found=Found )
  if ( .Not. Found ) then
    if (Dbg) call Logger%Write( "-> Macro '"//VarName//"' not found in parameter list => Exiting" )
    if (Dbg) call Logger%Exiting()
    return
  end if
!   if (Dbg) call Logger%Write( "-> Macro '"//VarName//"' found in parameter list with value '"//VarValue//"'" )

  VarName = trim(InBetween(1)) // VarName // trim(InBetween(2))
!   if (Dbg) call Logger%Write( "-> Calling ReplaceCharacter('"//OldValue//"','"//VarName//"','"//VarValue//"')" )
  NewValue    =   ReplaceCharacter( OldValue, VarName, VarValue )
!   if (Dbg) call Logger%Write( "-> NewValue = ", NewValue )

!   if (Dbg) OldValue   =   Param%GetRaw()
  call Param%SetValue( NewValue, Debug=.False. )
!   if (Dbg) NewValue   =   Param%GetRaw()
!   if (Dbg) call Logger%Write( "Macro substitution: '"//OldValue//"' -> '"//NewValue//"'" )

  if (Dbg) call Logger%Write( "-> Macro detected: '"//OldValue//"' -> '"//NewValue//"'" )


!   if (Dbg) call Logger%Write( "Calling ProcessMacroInParameters" )
  call ProcessMacroInParameters( Param, ParamList )

  if (Dbg) call Logger%Exiting()

End Subroutine






! **************************************************************************************************************
! **************************************************************************************************************
!                                           PRIVATE PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************


Pure Function Section_Start( Line ) result(Indicator)
  use String_Library        ,only:  UpperCase
  character(*)                                          ,intent(in)     ::  Line
  logical                                                               ::  Indicator
  character(*)                                              ,parameter  ::  Key = "START("
  integer                                                   ,parameter  ::  Length_Key = len_trim(Key)
  Indicator     =   .False.
  if ( len_trim(Line) <= Length_Key ) return
  if ( UpperCase(Line(1:Length_Key)) == Key ) Indicator = .True.
End Function

Pure Function Section_End( Line, SectionName ) result(Indicator)
  use String_Library        ,only:  UpperCase
  character(*)                                          ,intent(in)     ::  Line
  character(*)                                          ,intent(in)     ::  SectionName
  logical                                                               ::  Indicator
  character(:)  ,allocatable                                            ::  Target_Line
  character(:)  ,allocatable                                            ::  Current_Line
  Target_Line   =   UpperCase( "END(" // trim(SectionName) // ")" )
  Current_Line  =   UpperCase( trim(Line) )
  Indicator     =   Current_Line == Target_Line
End Function

Subroutine AddElementToSectionsFromStrings( Sections, Name, Lines, FileName, Debug )

  use Utilities_Library       ,only:  GetOptArgValue

  type(InputSection_Type)   ,allocatable                ,intent(inout)  ::  Sections(:)
  character(*)                                          ,intent(in)     ::  Name
  type(String_Type)                                     ,intent(in)     ::  Lines(:)
  character(*)                                ,optional ,intent(in)     ::  FileName                        !< Name of the file containing this section (Only used to print a nice error message if something goes wrong)
  logical                                     ,optional ,intent(in)     ::  Debug                         !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName = "AddElementToSectionsFromStrings" ! Name of current procedure
  logical                                                               ::  Dbg
  integer                                                               ::  i
  type(InputSection_Type)                                               ::  Section
  type(InputSection_Type)    ,dimension(:)   ,allocatable               ::  SectionsList

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if ( .Not. allocated(Sections) ) allocate( Sections(0) )

  if (Dbg) call Logger%Write( "Calling Section%Initialize: Name = ", Name, "size(Lines) = ", size(Lines) )
  call Section%Initialize( Name, Lines, FileName=FileName, Debug=Debug )
!   if (Dbg) then
!     call Logger%Write( "-> Local section: Calling Section%Write" )
!     call Section%Write()
!     call Section%Write( Logger%GetUnit() )
!   end if


! ----------------------------------------------------------------------------
  if (Dbg) call Logger%Write( "Allocating SectionsList" )
  allocate( SectionsList, source = [Sections,Section] )      ! @COMPILER_BUG: Works for 18.0.3 and 19.0.1... but not for gcc-8.2.0
!   allocate( SectionsList(size(Sections)+1) )
!   SectionsList(:)   =   [Sections,Section]

  if (Dbg) call Logger%Write( "Deallocate and then reallocate Sections using SectionsList as source" )
  deallocate( Sections )
  allocate(Sections(size(SectionsList)))
  i = 1
  do i =1, size(SectionsList)
    Sections(i) = SectionsList(i)
  end do

  if (Dbg) then
    do i = 1,size(Sections)
      if (Dbg) call Logger%Write( "-> Calling Sections(i)%Write(MaxParam=50): i = ", i, "Sections(i)%Name = ", Sections(i)%Name )
      call Sections(i)%Write( Logger, MaxParam=50  )
    end do
    if (Dbg) call Logger%Write( "Done: size(Sections) = ", size(Sections) )
  end if

!   allocate( SectionsList( size([Sections,Section]) ) )
!   do i = 1,size(Sections)
!     SectionsList(i)    =      Sections(i)
!   end do
!
!   if (Dbg) call Logger%Write( "size( SectionsList ) = ", size( SectionsList ) )
!
!   SectionsList(size(SectionsList))    =      Section
! ! ----------------------------------------------------------------------------
!   if ( allocated(Sections) ) deallocate( Sections )
! ! ----------------------------------------------------------------------------
! !   if (Dbg) call Logger%Write( "allocate Sections" )
! !   allocate( Sections, source = SectionsList )        ! @COMPILER_BUG
!   allocate( Sections( size(SectionsList) ) )
!   do i = 1,size(Sections)
!     Sections(i)    =      SectionsList(i)
!     if (Dbg) then
!       call Logger%Write( "-> Output section: Calling Sections(i)%Write" )
!       call Sections(i)%Write( Logger%GetUnit() )
!     end if
!   end do
! ! ----------------------------------------------------------------------------

  if (Dbg) call Logger%Exiting()

End Subroutine

Subroutine Error_Section_Without_End( SectionName, FileName, CallProc )

  use Error_Class    ,only:  Error

  character(*)                                          ,intent(in)     ::  SectionName                     !< Name of the section to be extracted from the input Section object (The passed-object dummy argument)
  character(*)                                ,optional ,intent(in)     ::  FileName                        !< Name of the file containing this section (Only used to print a nice error message if something goes wrong)
  character(*)                                ,optional ,intent(in)     ::  CallProc

  character(*)                                              ,parameter  ::  ProcName = "Error_Section_Without_End"
  character(:)  ,allocatable                                            ::  ProcPath

  ProcPath      =   Logger%GetPath()                                                             ! Getting the path of all calling procedures stored in the Logger object

  call Error%Set_Title( "Error during section construction" )

  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  An error occured when trying to construct a section object." )
  call Error%Add_Line( "  It seems that a section is missing the closing line." )

  call Error%Add_Line( "Solutions:" )
  call Error%Add_Line( "  A section is defined within a starting and ending line as in the following example:" )
  call Error%Add_Line( "  Start(<Name>)" )
  call Error%Add_Line( "    ..." )
  call Error%Add_Line( "  End(<Name>)" )
  call Error%Add_Line( "  where <Name> is the name of the section." )
  call Error%Add_Line( "  Check that all sections are correctly closed in the file being processed." )

  call Error%Add_Line( "Details:" )
  call Error%Add_Line( " Procedure raising the error: " // ProcName )
  if ( present(CallProc) )       call Error%Add_Line( " Calling procedure:           " // CallProc )
  if ( len_trim(ProcPath) /= 0 )          call Error%Add_Line( " Procedure path:              " // ProcPath )
  if ( present(FileName) )                call Error%Add_Line( " File name:                   " // FileName )
  call Error%Add_Line( " Section name:                " // SectionName            )

  call Error%Raise( Unit=Logger%GetUnit() )

End Subroutine


End SubModule
