SubModule(InputSection_Class) InputSection_Properties_SubClass

  use Logger_Class          ,only:  Logger, LogLevel_HEAVYDEBUG
  use Utilities_Library     ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

Module Procedure GetSectionName
  SectionName   =   This%Name
End Procedure

Module Procedure HasSection

  use Utilities_Library       ,only:  GetOptArgValue
  use InputSection_Tools      ,only:  ProcessListSectionNames, ProcessCaseSensitiveString

  character(*)                                              ,parameter  ::  ProcName = "HasSection" ! Name of current procedure
  logical                                                               ::  Dbg
  integer                                                               ::  i                               ! Index of sub-section of current section
  character(:)  ,allocatable                                            ::  ListSectionNames                ! List of sub-section names (corresponds to the input argument "SubSectionName")
  character(:)  ,allocatable                                            ::  TargetSectionName             ! Name of the target section to be found as a sub-section of the passed-object dummy argument
  character(:)  ,allocatable                                            ::  CurrentSectionName            ! Name of the a given sub-section of the passed-object dummy argument

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Name of section to be checked for existence" )
  ListSectionNames   =   ProcessCaseSensitiveString( SubSectionName, CaseSensitive )
  if (Dbg) call Logger%Write( "-> SubSectionName    = ", SubSectionName )
  if (Dbg) call Logger%Write( "-> ListSectionNames  = ", ListSectionNames )

  if (Dbg) call Logger%Write( "Calling ProcessListSectionNames" )
  call ProcessListSectionNames( TargetSectionName, ListSectionNames )
  if (Dbg) call Logger%Write( "-> TargetSectionName = ", TargetSectionName )
  if (Dbg) call Logger%Write( "-> ListSectionNames  = ", ListSectionNames )

  SectionFound    =   .False.                                                                 ! Initializing the indicator of section found to false
  do i = 1,This%NSections                                                                                    ! Loop on all sub-sections of current section
  associate( SubSec => This%Sections(i) )
    CurrentSectionName  =   ProcessCaseSensitiveString( SubSec%Name, CaseSensitive )
    if (Dbg) call Logger%Write( "iSec = ", i, "CurrentSectionName = ", CurrentSectionName )
    if ( CurrentSectionName /= TargetSectionName ) cycle                                                    ! If the current and target section name do not match, then going to the next sub-section
    if ( len(ListSectionNames) == 0 ) then                                                                      ! If there are no more sub-section names, then the section has been found and so
      SectionFound            =   .True.                                                                   ! ... setting the output indicator to true
    else                                                                                                        ! If there are some sub-section names left, then searching for the sub-section presence
      if (Dbg) call Logger%Write( "-> Processing subsection: ListSectionNames = ", ListSectionNames )
      SectionFound     =   SubSec%HasSection( ListSectionNames, Debug=Debug )
    end if
    exit                                                                                                        ! ... exiting the loop
  end associate
  end do                                                                                                        ! End loop on sub-sections of current section

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure FindTargetSection

  use InputSection_Tools    ,only:  Set_Procedure_Path

  character(*)                                              ,parameter  ::  ProcName = "FindTargetSection" ! Name of current procedure
  logical                                                               ::  Dbg
  logical                                                               ::  FoundSection_
  logical                                                               ::  IsRootSection_
  logical                                                               ::  Mandatory_
  character(:)  ,allocatable                                            ::  ProcPath

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  nullify( TargetSection )                                                                               ! Initializing the extraction section pointer

! ==============================================================================================================
!       CASE WHEN THE TARGET SECTION IS SEARCHED IN A SUBSECTION OF THE ROOT SECTION
! ==============================================================================================================
  if ( present(FromSubSection) ) then
    if (Dbg) call Logger%Write( "Searching for target section in sub-sections of the root section" )
    if (Dbg) call Logger%Write( "-> Calling This%HasSection: SubSection = ", FromSubSection )
    FoundSection_    =   This%HasSection( FromSubSection )
    if (Dbg) call Logger%Write( "-> FoundSection_ = ", FoundSection_ )
    if ( FoundSection_ ) then
      if (Dbg) call Logger%Write( "-> TargetSection '"// FromSubSection // "' has been found" )
      call This%GetSectionPointer( FromSubSection, TargetSection )
    else
      if (Dbg) call Logger%Write( "-> TargetSection '"// FromSubSection // "' has not been found" )
      Mandatory_  = .False.
      if ( present(Mandatory) ) Mandatory_ = Mandatory
      if ( Mandatory_ ) then
        ProcPath      =   Set_Procedure_Path( ProcName, CallProc )                                       ! Setting the procedure path
        if (Dbg) call Logger%Write( "-> Mandatory section not found called '"//FromSubSection//"' not found => Error" )
        call Error_Mandatory_Section_Not_Found( This, SectionName=FromSubSection, CallProc=ProcPath )
      end if
    end if
    IsRootSection_  =   .False.
! ==============================================================================================================


! ==============================================================================================================
!       CASE WHEN THE TARGET SECTION IS DIRECTLY THE CURRENT SECTION
! ==============================================================================================================
  else
    if (Dbg) call Logger%Write( "Target section is the root section" )
    TargetSection   =>  This
    FoundSection_   =   .True.
    IsRootSection_  =   .True.
  end if
! ==============================================================================================================

  if ( present(FoundSection) )   FoundSection   =   FoundSection_
  if ( present(IsRootSection) )  IsRootSection  =   IsRootSection_

  if (Dbg) call Logger%Exiting()

End Procedure



! This procedure returns the number of sub-sections contains in a given Section object.
! If the "Name" optional argument is present, then the number of sub-sections whose name corresponds the value
! stored in the "Name" variable is returned.
Module Procedure GetNumberOfSubSections
  use String_Library      ,only:  UpperCase
  integer                                                               ::  i                                   ! Index of sub-sections
  character(:)  ,allocatable                                            ::  SubSectionName                 ! Name of a given sub-section
  NSections             =   0
  if ( .Not. RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(This%Sections) ) return
  if ( size(This%Sections) == 0 ) return
  if ( present(Name) ) then
    do i = 1,This%NSections
      SubSectionName   =   This%Sections(i)%Name
      SubSectionName   =   UpperCase( SubSectionName )
      if ( SubSectionName /= Name ) cycle
      NSections         =   NSections + 1
    end do
  else
    NSections           =   size(This%Sections)
  end if
End Procedure

Module Procedure GetMaxLengthParameterName
  integer                                                               ::  i
  Length        =   0
!   write(*,*) "[to-be-removed] This%NParameters = ", This%NParameters
!   write(*,*) "[to-be-removed] allocated(This%Parameters) = ", allocated(This%Parameters)
!   write(*,*) "[to-be-removed] size(This%Parameters) = ", size(This%Parameters)
  do i = 1,This%NParameters
!     write(*,*) "[to-be-removed] allocated(This%Parameters(i)%Name) = ", allocated(This%Parameters(i)%Name)
!     write(*,*) "[to-be-removed] This%Parameters(i)%Name = ", This%Parameters(i)%Name
    Length      =   Max( Length , len_trim(This%Parameters(i)%Name) )
  end do
End Procedure

Module Procedure GetMaxLengthParameterValue
  integer                                                               ::  i
  Length        =   0
  do i = 1,This%NParameters
    Length      =   Max( Length , len_trim(This%Parameters(i)%Value) )
  end do
End Procedure
















! This procedure returns the index of the subsection whose name matches the input name.
! Returns 0 if the section is not found
Module Procedure GetSubSectionIndex

  use String_Library      ,only:  UpperCase

  character(*)                                              ,parameter  ::  ProcName = "GetSubSectionIndex" ! Name of current procedure
  logical                                                               ::  Dbg
  integer                                                               ::  iSec
  character(:)  ,allocatable                                            ::  CurrentSectionName
  character(:)  ,allocatable                                            ::  TargetSectionName
  logical                                                               ::  CaseSensitive_
  logical                                                               ::  SectionFound

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  CaseSensitive_    = .False.
  if ( present(CaseSensitive) ) CaseSensitive_ = CaseSensitive

  if (Dbg) call Logger%Write( "Input section name: '" // SectionName // "'" )
  TargetSectionName   =   SectionName                                                                    ! Setting the target section name to the input section name
  if ( CaseSensitive_ ) then                                                                                 ! If the section name is case sensitive, then nothing to do
    if (Dbg) call Logger%Write( "-> Section name is case-sensitive: Target section name '" // TargetSectionName // "'" )
  else                                                                                                          ! If case insensitive, then ...
    TargetSectionName   = UpperCase( TargetSectionName )                                                ! ... Converting the target section name to upper case
    if (Dbg) call Logger%Write( "-> Section name is not case-sensitive: Target section name '" // TargetSectionName // "'" )
  end if

  SectionFound   =    .False.                                                                 ! Initializing the indicator of section found to false
  SectionIndex   =    0                                                                       ! Initializing the target section index to zero (Required so that the calling procedure knows if the target section has not been found)
  do iSec = 1,This%NSections                                                                                    ! Loop on all sub-sections of current section
    CurrentSectionName  =   This%Sections(iSec)%Name                                                ! Getting the current section name
    if ( .Not. CaseSensitive_ ) CurrentSectionName = UpperCase(CurrentSectionName)                       ! If case insensitive, then converting the current section name to upper case
    if (Dbg) call Logger%Write( "iSec = ", iSec, "CurrentSectionName = ", CurrentSectionName )
    if ( CurrentSectionName /= TargetSectionName ) cycle                                                    ! If the current and target section name do not match, then going to the next sub-section
    SectionIndex        =   iSec                                                                    ! If we're here, then the target section has been found, and so setting its index in the output variable and ...
    SectionFound        =   .True.                                                                  ! Setting the section as found
    exit                                                                                                        ! ... exiting the loop
  end do                                                                                                        ! End loop on sub-sections of current section

  if ( present(Mandatory) ) then
  if ( Mandatory .and. .Not.SectionFound) then
    call Logger%Write( "Section '" // SectionName // "' has not been found and it is mandatory => Error" )
    error stop
!     @TODO: Write error message
  end if
  end if

  if (Dbg) call Logger%Exiting()

End Procedure































! **************************************************************************************************************
! **************************************************************************************************************
!                               PROCEDURES FOR EXTRACTING SUB-SECTIONS FROM A SECTION
! **************************************************************************************************************
! **************************************************************************************************************

! In the current implementation, section name are always case insensitive. Thus, the target section name is
! always uppercased, as well as the names of the sub-section contained in the current section object.
!
!
! @TODO: Write a better error message


! This procedure extracts a given sub-section from a Section object.
! The root section from which the extraction is done is denoted the "main section" and corresponds to the
! passed object dummy argument "This". The sub-section to be extracted from the main section is denoted the
! "target section" and it is stored in the function result "TargetSection". The section from which the
! target section is being directly extracted from is called the extraction section and corresponds to the
! vaiable "Extraction_Section". Note that the main section and the extraction section might be different.
! Indeed, the target section can be extracted from any sub-section of the main section, at all possible
! nesting level. In other words, the target section can either be extracted directly from the main section
! or from any of any of its sub-sections, whatever its nesting level.
! In order to achieve such a flexibility in the choice of the extraction section (extraction from the main
! section or from any of its sub-sections) a local Section object is defined with the pointer attribute
! (the variable "Extraction_Section") and this pointer points to the selected extraction section.
! The actual extraction section depends on the presence of the optional input argument "From _SubSection":
!  - It the optional argument is absent, then the section is to be extracted directly from the main section.
!    Thus, the pointer Section object associated to the extraction section points to the passed-object dummy
!    argument. No further processing is required in this case.
!  - If the optional argument "FromSubSection" is present, then the target section has to be extracted from
!    a sub-section of the current section. The variable "FromSubSection" contains the name of the sub-section
!    from which the target section has to be extracetd. Eventually, it can contains a recursive list of
!    sub-section names, the final one being the name of the extraction section. The sub-section names are
!    separated by the ">" character.
!    For example, if the input variables are FromSubSection="Name1>Name2>Name3" and SectionName="TarSec",
!    then the target section "TarSec" has to be extracted from the section called "Name3", which is a
!    sub-section of the section called "Name2", which is a sub-section of the section called "Name1",
!    which is a sub-section of the main section 'the passed-object dummy argument).
! If the target section cannot be found (either because this taget section does not exist in the extraction
! section or if the extraction section itself does not exist), then an empty Section object is returned.
! A set of optional argument can also be passed to the procedure for a very custom behavior. These arguments
! are the following:
!  - A "Mandatory" indicator can be passed to the procedure as an optional input argument. By default, the target
!    section is considered as not mandatory. If this argument is present and if the target section is not found,
!    then an error is raised. If this argument is absent, then no error is raised and an empty section object is
!    returned.

! This procedure is a workaround for calling GetSubsectionFromSection for gcc (7.2.0 does not work)
Module Procedure GetSubsectionFromSection_WORKAROUND
  TargetSection  =  GetSubsectionFromSection( This, SectionName, FromSubSection, Mandatory, Found, CallProc, Section_Index, Debug )
End Procedure

Module Procedure GetSubsectionFromSection

  use String_Library        ,only:  UpperCase
  use InputSection_Tools    ,only:  Set_Procedure_Path, ProcessCaseSensitiveString

  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName = "GetSubsectionFromSection" ! Name of current procedure
  character(:)  ,allocatable                                            ::  ProcPath
  integer                                                               ::  iSec                                ! Index of sub-sections
  character(:)  ,allocatable                                            ::  CurrentSectionName            ! Name of a given section while looping over all sub-sections (The name is uppercased if the section name is case insensitive)
  character(:)  ,allocatable                                            ::  TargetSectionName             ! Name of the taget section which corresponds to the input section name 'SectionName' (The name is uppercased if the section name is case insensitive)
  logical                                                               ::  FoundSection                   ! Indicator whether a section (either the extraction or target section) has been found
  logical                                                               ::  Mandatory_Section                   ! Indicator whether the target section is mandatory. If it is, and if the section is not found, then an error is raised
  type(InputSection_Type)    ,pointer                                   ::  ExtractionSection              ! Section from which the target section is directly extracted from

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  ProcPath      =   Set_Procedure_Path( ProcName, CallProc )                                       ! Setting the procedure path

  if ( present(Mandatory) ) then;  Mandatory_Section = Mandatory                                                ! Setting the local variable to the optional variable if present ...
  else;                            Mandatory_Section = .False.; end if                                          ! ... otherwise setting the local variable to the default variable

  TargetSectionName =   ProcessCaseSensitiveString( SectionName, CaseSensitive=.False. )                          ! Setting the target section name (The name is uppercased so that the comparison is case-insensitive)
  TargetSection     =   ConstructSection(SectionName)                                                      ! Setting the target section name inside the output section (This associate the Sub-section objects to 0)
!   TargetSection%Name          =   SectionName

  if (Dbg) call Logger%Write( "Name of target section to be extracted: SectionName = ", SectionName )

! ==============================================================================================================
!       SETTING THE EXTRACTION SECTION: SECTION FROM WHICH THE TARGET SECTION SHOULD BE EXTRACTED FROM
! ==============================================================================================================
! The following instructions define the Section object from which the target section should be extracted from.
! Note that one must check that the "This%Sections" is associated before proceding in order to handle the case
! when the section associated to the passed-object dummy argument has not been initialized properly.
! ==============================================================================================================
  if ( .Not. RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(This%Sections) ) then
    if (Dbg) call Logger%Write( "Current Section has no 'Subsection' (Variable 'This%Sections' is not associated)" )
    if (Dbg) call Logger%Write( "-> No Section object can be extracted" )
    FoundSection               =   .False.                                                                 ! Setting the indicator that the extraction section has not been found
  else                                                                                                          ! If the sub-section objects of the main section are associated, then we can try to find thge target section
    if (Dbg) call Logger%Write( "Finding Section object from where extraction is to be performed" )
    if (Dbg) call Logger%Write( "-> Calling FindTargetSection" )
    call FindTargetSection( This, ExtractionSection,  &
            FromSubSection  =   FromSubSection,       &
            FoundSection    =   FoundSection,         &
            Mandatory       =   Mandatory_Section)!,    &
!             Debug         =   Debug               )
    if (Dbg) call Logger%Write( "-> FoundSection = ", FoundSection )
  end if                                                                                                        ! End if case on associated sub-sections
! ==============================================================================================================


! ==============================================================================================================
!       EXTRACTING THE TARGET SECTION FROM THE EXTRACTION SECTION
! ==============================================================================================================
! This section extracts the target section from the extraction section if the latter section has been found.
! ==============================================================================================================
  if ( FoundSection ) then                                                                                     ! If the extraction section has been found, then try to extract the target section
    FoundSection               =   .False.                                                                 ! Initializing the indicator of found target section to false
    if (Dbg) call Logger%Write( "Loop on sub-sections of section '"//ExtractionSection%Name//"' to found section '"//TargetSectionName//"': NSections = ", ExtractionSection%NSections )
    do iSec = 1,ExtractionSection%NSections                                                                    ! Loop on all sub-sections of the extraction section to found the target section
      CurrentSectionName       =   ExtractionSection%Sections(iSec)%Name                                  ! Getting the name of curent section
      CurrentSectionName       =   UpperCase( CurrentSectionName )                                       ! CurrentSectionName     =   UpperCase( This%Sections(iSec)%Get_Name() )          ! @COMPILER_BUG: Not doing the upper case
      if (Dbg) call Logger%Write( "-> iSec = ", iSec, "CurrentSectionName = ", CurrentSectionName )
      if ( TargetSectionName /= CurrentSectionName ) cycle                                                      ! If the section names do not match, then current section is not the target section and so going to the next section
      if (Dbg) call Logger%Write( "-> Section '" // SectionName // "' has been found !" )
      FoundSection             =   .True.                                                                  ! Setting the indicator that the target section has been found
      TargetSection            =   ExtractionSection%Sections(iSec)   !@TODO:COMPILER_BUG:ifort-19.1.1: Not able to perform the code... fixed by adding a user-defiend assignment
      exit                                                                                                      ! Exiting the loop on all sub-sections of the extraction section since the job is done
    end do                                                                                                      ! End loop on sub-sections of the extraction section
    if (Dbg) call Logger%Write( "FoundSection = ", FoundSection )
  end if                                                                                                        ! End if case on found extraction section
  TargetSection%Defined        =   FoundSection                                                           ! Setting whether the target section is defined (Required ???)
! ==============================================================================================================


  if ( (.Not.FoundSection) .and. Mandatory_Section ) then
    call Error_Mandatory_Section_Not_Found( This, SectionName=SectionName, CallProc=ProcPath )
  end if

  if ( present(Found) ) Found = FoundSection

  if ( present(Section_Index) )  then
    Section_Index       =   0
    if (FoundSection) Section_Index = iSec
  end if

  nullify( ExtractionSection )

  if (Dbg) then
!     call Logger%Write( "DEBUG Before TargetSection%Write")
!     call TargetSection%Write(Logger%GetUnit())
!     call Logger%Write( "DEBUG After TargetSection%Write")

    call Logger%Write( "TargetSection%Mandatory   = ", TargetSection%Mandatory    )
    call Logger%Write( "TargetSection%Empty       = ", TargetSection%Empty        )
    call Logger%Write( "TargetSection%Defined     = ", TargetSection%Defined      )
    call Logger%Write( "TargetSection%NSections   = ", TargetSection%NSections    )
    call Logger%Write( "TargetSection%NParameters = ", TargetSection%NParameters  )
    call Logger%Write( "TargetSection%Name        = ", TargetSection%Name         )
    call Logger%Write( "RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(TargetSection%Sections)= ", RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(TargetSection%Sections))
    if (RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(TargetSection%Sections)) call Logger%Write( "size(TargetSection%Sections)= ", size(TargetSection%Sections))
    call Logger%Write( "allocated(TargetSection%Parameters)= ", allocated(TargetSection%Parameters))
    if (allocated(TargetSection%Parameters)) call Logger%Write( "size(TargetSection%Parameters)= ", size(TargetSection%Parameters))
  end if


  if (Dbg) call Logger%Write( "Exiting")

  if (Dbg) call Logger%Exiting()
End Procedure

! @TODO: Update this procedure as "GetSubsectionFromSection" (add the "FromSubSection" argument and comments)
! This procedure extracts several section from a given section.
! If the name of the sections to be extracted if not given, then all sections are extracted
Module Procedure GetSubsectionsFromSection

  use String_Library        ,only:  UpperCase
  use InputSection_Tools    ,only:  Set_Procedure_Path

  character(*)                                              ,parameter  ::  ProcName = "GetSubsectionsFromSection" ! Name of current procedure
  logical                                                               ::  Dbg
  character(:)  ,allocatable                                            ::  ProcPath
  logical                                                               ::  Found_
  integer                                                               ::  iSec, jSec                          ! Index of sub-sections
  integer                                                               ::  NSections                           ! Number of sub-sections
  character(:)  ,allocatable                                            ::  CurrentSectionName                ! Name of a given section while looping over all sub-sections (The name is uppercased if the section name is case insensitive)
  character(:)  ,allocatable                                            ::  TargetSectionName                 ! Name of the taget section which corresponds to the input section name 'SectionName' (The name is uppercased if the section name is case insensitive)
  logical                                                               ::  FoundSection                        ! Indicator whether a section (either the extraction or target section) has been found
  logical                                                               ::  Mandatory_Section                   ! Indicator whether the target section is mandatory. If it is, and if the section is not found, then an error is raised
  type(InputSection_Type)    ,pointer                                   ::  ExtractionSection                   ! Section from which the target section is directly extracted from

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  ProcPath      =   Set_Procedure_Path( ProcName, CallProc )                                       ! Setting the procedure path

  if ( present(Mandatory) ) then;  Mandatory_Section = Mandatory                                                ! Setting the local variable to the optional variable if present ...
  else;                            Mandatory_Section = .False.; end if                                          ! ... otherwise setting the local variable to the default variable

  if (Dbg) then
    call Logger%Write( "Input parameters" )
    call Logger%Write( "-> present(SectionName)     = ", present(SectionName) )
    call Logger%Write( "-> present(FromSubSection)  = ", present(FromSubSection) )
    call Logger%Write( "-> present(Mandatory)       = ", present(Mandatory) )
    if ( present(SectionName) )     call Logger%Write( "-> SectionName       = ", SectionName   )
    if ( present(FromSubSection) )  call Logger%Write( "-> FromSubSection   = ", FromSubSection )
    if ( present(Mandatory) )       call Logger%Write( "-> Mandatory        = ", Mandatory      )
  end if

! ==============================================================================================================
!   TREATING THE CASE WHEN THE SUB-SECTIONS ARE NOT ASSOCIATED
! ==============================================================================================================
! Note that one must check that the "This%Sections" is associated before proceding in order to handle the case
! when the section associated to the passed-object dummy argument has not been initialized properly.
! ==============================================================================================================
  if ( .Not. RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(This%Sections) ) then
    if (Dbg) call Logger%Write( "The variable 'This%Sections' is not associated => Impossible to extract any section" )
    if ( Mandatory_Section ) call Error_Mandatory_Section_Not_Found( This, SectionName=SectionName, CallProc=ProcPath )
    allocate( Sections(0) )
    if ( present(Found) ) Found = .False.
    if (Dbg) call Logger%Exiting()
    return
  end if
! ==============================================================================================================


! ==============================================================================================================
!   TREATING THE CASE WHEN NO SECTION NAME IS PROVIDED: EXTRACTING ALL SUB-SECTIONS
! ==============================================================================================================
! If the "SectionName" optional argument is absent, then all sub-sections need to be extratced, not only a
! specific section.
! First, one try to find the section 'ExtractionSection' from where the sections should be extracted.
! This sections is found in the procedure 'FindTargetSection'. The Mandatory input argument is also passed
! to this procedure so that if the section is not found and the extraction is mandatory, an error is raised.
! Then, then target sections are set only if the extraction section and its sub-sections are associated
! ==============================================================================================================
  if ( .Not. present(SectionName) ) then
    if (Dbg) call Logger%Write( "Treating the case when no section name is provided: extracting all sub-sections" )
    if (Dbg) call Logger%Write( "-> Finding the section from where the sections should be extracted" )
    if (Dbg) call Logger%Write( "-> Calling FindTargetSection" )
    call FindTargetSection( This, ExtractionSection,  &
            FromSubSection  =   FromSubSection,       &
            Mandatory       =   Mandatory_Section,    &
            Debug         =   Debug               )
    if ( associated(ExtractionSection) ) then
      if ( RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(ExtractionSection%Sections) ) then
        NSections           =   size(ExtractionSection%Sections)
        allocate( Sections(NSections) )    ! @COMPILER_BUG
        do iSec = 1,NSections
          Sections(iSec)    =   ExtractionSection%Sections(iSec)
        end do
      end if
    end if
! ==============================================================================================================


! ==============================================================================================================
!       TREATING THE CASE WHEN A SECTION NAME IS PROVIDED: EXTRACTING MATCHING SUB-SECTIONS
! ==============================================================================================================
! If the "SectionName" optional argument is present, then extracting all sub-section with that name.
! We first find the section from where the sections should be extracted. This section is ExtractionSection.
! Then, we get the number of sections to be extracted from the ExtractionSection. This number is zero is no
! ExtractionSection was found. If an ExtractionSection but it has no subsections, and the optional input
! argument Mandatory is true, then an error is raised.
! ==============================================================================================================
  else
    if (Dbg) call Logger%Write( "Name of the target section: SectionName = ", SectionName )
    if (Dbg) call Logger%Write( "-> Finding the section from where the sections should be extracted" )
    if (Dbg) call Logger%Write( "-> Calling FindTargetSection" )
    call FindTargetSection( This, ExtractionSection,  &
            FromSubSection  =   FromSubSection,       &
            Mandatory       =   Mandatory,            &
            FoundSection    =   FoundSection,         &
            Debug         =   Debug               )
    if (Dbg .and. FoundSection) call Logger%Write( "-> Section from which the target section has to be extracted: ", ExtractionSection%Name )
    if (Dbg) call Logger%Write( "-> Getting the number of sections to be extracted" )
    TargetSectionName   =   UpperCase( SectionName )                                                       ! Setting the target section name (The name is uppercased so that the comparison is case-insensitive)
    NSections             =   0                                                                               ! Initializing the number of sections to be extracted to zero (Required)
    if ( FoundSection ) then                                                                                   ! If the extraction section has been found, then try to extract the target section
      NSections           =   ExtractionSection%GetNumberOfSubSections( Name=TargetSectionName )           ! Counting the number of sub-section whose name corresponds the value stored in the "TargetSectionName" variable
      FoundSection        =   NSections /= 0                                                                  ! Setting the indicator whether sections have been found
      if (Dbg) call Logger%Write( "-> FoundSection = ", FoundSection, "NSections     = ", NSections )
    end if                                                                                                        ! End if case on found extraction section
    if ( (.Not.FoundSection) .and. Mandatory_Section ) &
    call Error_Mandatory_Section_Not_Found( This, SectionName=SectionName, CallProc=ProcPath )
    if (Dbg) call Logger%Write( "-> Allocating and setting the sections to be extracted" )
    if ( FoundSection ) then                                                                                   ! If some sub-sections have been found
      allocate( Sections(NSections) )
      jSec                          =   0
      do iSec = 1,ExtractionSection%NSections
        CurrentSectionName        =   ExtractionSection%Sections(iSec)%Name
        CurrentSectionName        =   UpperCase( CurrentSectionName )
        if ( TargetSectionName /= CurrentSectionName ) cycle
        jSec                        =   jSec + 1
        Sections(jSec)              =   ExtractionSection%Sections(iSec)
      end do
    end if                                                                                                      ! End if case on found sub-sections
  end if
! ==============================================================================================================


  if ( .Not. allocated(Sections) ) then
    if ( Mandatory_Section ) call Error_Mandatory_Section_Not_Found( This, SectionName=SectionName, CallProc=ProcPath )
    allocate( Sections(0) )
  end if

  if ( present(Found) ) Found = size(Sections) > 0

  nullify( ExtractionSection )

  if (Dbg) call Logger%Exiting()

End Procedure





! This procedure returns a function-like section.
Module Procedure GetFunctionSection

  use String_Library        ,only:  UpperCase, IsFunction
  use InputSection_Tools    ,only:  Set_Procedure_Path, ProcessCaseSensitiveString
  use Utilities_Library     ,only:  SetOptArg

  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName = "GetFunctionSection"
  logical                                                               ::  FoundSection                        ! Indicator whether a section (either the extraction or target section) has been found
  logical                                                               ::  FoundFunction
  logical                                                               ::  MandatorySection                   ! Indicator whether the target section is mandatory. If it is, and if the section is not found, then an error is raised
  logical                                                               ::  CaseSensitive_
  integer                                                               ::  i                                ! Index of sub-sections
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable                                            ::  TargetFunctionName
  type(InputSection_Type)    ,pointer                                   ::  ExtractionSection                   ! Section from which the target section is directly extracted from
  character(:)  ,allocatable                                            ::  String

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  ProcPath            =   Set_Procedure_Path( ProcName, CallProc )
  MandatorySection    =   GetOptArgValue( .False., Mandatory     )
  CaseSensitive_      =   GetOptArgValue( .False., CaseSensitive )  ! By default, make function names case insensitive
  TargetFunctionName  =   ProcessCaseSensitiveString( FunctionName, CaseSensitive=CaseSensitive_ )
  TargetSection       =   ConstructSection(FunctionName)

  if (Dbg) call Logger%Write( "Name of function to be extracted: FunctionName = ", FunctionName )

! ==============================================================================================================
!       SETTING THE EXTRACTION SECTION: SECTION FROM WHICH THE TARGET SECTION SHOULD BE EXTRACTED FROM
! ==============================================================================================================
! The following instructions define the Section object from which the target section should be extracted from.
! Note that one must check that the "This%Sections" is associated before proceding in order to handle the case
! when the section associated to the passed-object dummy argument has not been initialized properly.
! ==============================================================================================================
  if ( .Not. RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(This%Sections) ) then
    if (Dbg) call Logger%Write( "Current Section has no 'Subsection' (Variable 'This%Sections' is not associated)" )
    if (Dbg) call Logger%Write( "-> No Section object can be extracted" )
    FoundSection               =   .False.
  else
    if (Dbg) call Logger%Write( "Finding Section object from where extraction is to be performed" )
    if (Dbg) call Logger%Write( "-> Calling FindTargetSection" )
    call FindTargetSection( This, ExtractionSection , &
            FromSubSection  =   SectionName         , &
            FoundSection    =   FoundSection        , &
            Mandatory       =   MandatorySection      )
    if (Dbg) call Logger%Write( "-> FoundSection = ", FoundSection )
  end if
! ==============================================================================================================


! ==============================================================================================================
!       EXTRACTING THE TARGET SECTION FROM THE EXTRACTION SECTION
! ==============================================================================================================
! This section extracts the target section from the extraction section if the latter section has been found.
! ==============================================================================================================
  FoundFunction    =   .False.
  if ( FoundSection ) then
    if (Dbg) call Logger%Write( "Loop on all parameters of section '"//ExtractionSection%Name//"' to found function '"//TargetFunctionName//"': NParameters = ", ExtractionSection%NParameters )
    do i = 1,ExtractionSection%NParameters
      String     =   ExtractionSection%Parameters(i)%GetRaw()
      FoundFunction   =   IsFunction( String, Name=TargetFunctionName, CaseSensitive=CaseSensitive_ )
      if (Dbg) call Logger%Write( "-> i = ", i, "String = ", String, "FoundFunction = ", FoundFunction )
      if ( .Not. FoundFunction ) cycle
      if (Dbg) call Logger%Write( "-> Function '"//FunctionName//"' has been found !" )
      if (Dbg) call Logger%Write( "-> Calling ConstructSection" )
      TargetSection   =   ConstructSection( ExtractionSection%Parameters(i), Debug=Debug )
      exit
    end do
    if (Dbg) call Logger%Write( "-> FoundFunction = ", FoundFunction )
  end if
  nullify( ExtractionSection )
! ==============================================================================================================


!   if ( (.Not.FoundFunction) .and. MandatorySection ) then
!     call Error_Mandatory_Section_Not_Found( This, SectionName=FunctionName, CallProc=ProcPath )
!   end if


! # if 0

  if ( (.Not.FoundFunction) .and. MandatorySection ) then
    if ( present(Status) ) then
      call Status%Set(                            &
            ProcName  =   ProcName              , &
            Message   =   "The mandatory function ''"//FunctionName//"' has not been found"  )
      if (Dbg) call Logger%Exiting()
      return
    else
      call Error_Mandatory_Section_Not_Found( This, SectionName=FunctionName, CallProc=ProcPath )
    end if
  end if

! # endif
!     if ( present(Status) ) Status = Status_
!     if ( Status_ /= 0 ) then
!       if (Dbg) call Logger%Write( "-> Error converting string" )
!     end if


!   if ( UpdateStatus(Status,Proc=ProcName,ExitLogger=Dbg) ) return

  call SetOptArg( FoundFunction, Found )

  if (Dbg) then
    call Logger%Write( "DEBUG Before TargetSection%Write")
    call TargetSection%Write(Logger%GetUnit())
    call Logger%Write( "DEBUG After TargetSection%Write")
    call Logger%Write( "TargetSection%Mandatory   = ", TargetSection%Mandatory    )
    call Logger%Write( "TargetSection%Empty       = ", TargetSection%Empty        )
    call Logger%Write( "TargetSection%Defined     = ", TargetSection%Defined      )
    call Logger%Write( "TargetSection%NSections   = ", TargetSection%NSections    )
    call Logger%Write( "TargetSection%NParameters = ", TargetSection%NParameters  )
    call Logger%Write( "TargetSection%Name        = ", TargetSection%Name         )
    call Logger%Write( "allocated(TargetSection%Sections)= ", RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(TargetSection%Sections))
    if (RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(TargetSection%Sections)) call Logger%Write( "size(TargetSection%Sections)= ", size(TargetSection%Sections))
    call Logger%Write( "allocated(TargetSection%Parameters)= ", allocated(TargetSection%Parameters))
    if (allocated(TargetSection%Parameters)) call Logger%Write( "size(TargetSection%Parameters)= ", size(TargetSection%Parameters))
  end if

  if (Dbg) call Logger%Exiting()

End Procedure



! This procedure returns an array of function-like section.
Module Procedure GetFunctionSections

  use String_Library        ,only:  UpperCase, IsFunction
  use InputSection_Tools    ,only:  Set_Procedure_Path, ProcessCaseSensitiveString
  use Utilities_Library     ,only:  SetOptArg

  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName = "GetFunctionSections"
  logical                                                               ::  FoundSection                        ! Indicator whether a section (either the extraction or target section) has been found
  logical                                                               ::  FoundFunction
  logical                                                               ::  MandatorySection                   ! Indicator whether the target section is mandatory. If it is, and if the section is not found, then an error is raised
  logical                                                               ::  CaseSensitive_
  integer                                                               ::  i                                ! Index of sub-sections
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable                                            ::  TargetFunctionName
  character(:)  ,allocatable                                            ::  String
  type(InputSection_Type)    ,pointer                                   ::  ExtractionSection                   ! Section from which the target section is directly extracted from
  type(InputSection_Type)                                               ::  ContainerSection

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  ProcPath            =   Set_Procedure_Path( ProcName, CallProc )
  MandatorySection    =   GetOptArgValue( .False., Mandatory     )
  CaseSensitive_      =   GetOptArgValue( .False., CaseSensitive )  ! By default, make function names case insensitive
  TargetFunctionName  =   ProcessCaseSensitiveString( FunctionName, CaseSensitive=CaseSensitive_ )
!   allocate( TargetSection(0) )
!   TargetSection       =   ConstructSection(FunctionName)

  if (Dbg) call Logger%Write( "Name of function to be extracted: FunctionName = ", FunctionName )

! ==============================================================================================================
!       SETTING EXTRACTION SECTION: SECTION FROM WHICH TARGET SECTION SHOULD BE EXTRACTED FROM
! ==============================================================================================================
! The following instructions define the Section object from which the target section should be extracted from.
! Note that one must check that the "This%Sections" is associated before proceding in order to handle the case
! when the section associated to the passed-object dummy argument has not been initialized properly.
! ==============================================================================================================
  if ( .Not. RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(This%Sections) ) then
    if (Dbg) call Logger%Write( "Current Section has no 'Subsection' (Variable 'This%Sections' is not associated)" )
    if (Dbg) call Logger%Write( "-> No Section object can be extracted" )
    FoundSection               =   .False.
  else
    if (Dbg) call Logger%Write( "Finding Section object from where extraction is to be performed" )
    if (Dbg) call Logger%Write( "-> Calling FindTargetSection" )
    call FindTargetSection( This, ExtractionSection , &
            FromSubSection  =   SectionName         , &
            FoundSection    =   FoundSection        , &
            Mandatory       =   MandatorySection      )
    if (Dbg) call Logger%Write( "-> FoundSection = ", FoundSection )
  end if
! ==============================================================================================================


! ==============================================================================================================
!       EXTRACTING THE TARGET SECTION FROM THE EXTRACTION SECTION
! ==============================================================================================================
! This section extracts the target section from the extraction section if the latter section has been found.
! ==============================================================================================================
  FoundFunction    =   .False.
  if ( FoundSection ) then
    if (Dbg) call Logger%Write( "Loop on all parameters of section '"//ExtractionSection%Name//"' to found function '"//TargetFunctionName//"': NParameters = ", ExtractionSection%NParameters )
    do i = 1,ExtractionSection%NParameters
      String     =   ExtractionSection%Parameters(i)%GetRaw()
      FoundSection   =   IsFunction( String, Name=TargetFunctionName, CaseSensitive=CaseSensitive_ )
      if (Dbg) call Logger%Write( "-> i = ", i, "String = ", String, "FoundSection = ", FoundSection )
      if ( .Not. FoundSection ) cycle
      FoundFunction   =   .True.
      if (Dbg) call Logger%Write( "-> Function '"//FunctionName//"' has been found !" )
      if (Dbg) call Logger%Write( "-> Calling ContainerSection%AddSection(ConstructSection(...))" )
      call ContainerSection%AddSection( ConstructSection( ExtractionSection%Parameters(i), DefArgNames=DefArgNames, Debug=Debug ) )
    end do
    if (Dbg) call Logger%Write( "-> FoundFunction = ", FoundFunction )
  end if
  nullify( ExtractionSection )
! ==============================================================================================================
  if (ContainerSection%NSections /= 0 ) then
    allocate( TargetSections , source = ContainerSection%Sections )
  else
    allocate( TargetSections(0) )
  end if

  if ( (.Not.FoundFunction) .and. MandatorySection ) then
    if ( present(Status) ) then
      call Status%Set(                            &
            ProcName  =   ProcName              , &
            Message   =   "The mandatory function ''"//FunctionName//"' has not been found"  )
      if (Dbg) call Logger%Exiting()
      return
    else
      call Error_Mandatory_Section_Not_Found( This, SectionName=FunctionName, CallProc=ProcPath )
    end if
  end if

  call SetOptArg( FoundFunction, Found )

  if (Dbg) call Logger%Exiting()

End Procedure


! **************************************************************************************************************
! **************************************************************************************************************
!                                           properties: parameters
! **************************************************************************************************************
! **************************************************************************************************************
! PROCEDURES FOR EXTRACTING PARAMETERS FROM A SECTION or the properties of a parameter


! @TODO: Comment and add a Mandatory optional input argument (Error is subsection "FromSubSection" not found), add an Found optional input argument
Module Procedure GetNumberOfParameters

  use InputSection_Tools    ,only:  Set_Procedure_Path

  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName = "GetNumberOfParameters" ! Name of current procedure
  character(:)  ,allocatable                                            ::  ProcPath
  logical                                                               ::  FoundSection                   ! Indicator whether the target section has been found
  type(InputSection_Type)    ,pointer                                   ::  TargetSection                  ! Section from which the number of parameters has to be returned

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  ProcPath      =   Set_Procedure_Path( ProcName, CallProc )                                       ! Setting the procedure path

  if (Dbg) call Logger%Write( "Finding the section from where the parameters should be counted" )
  if (Dbg) call Logger%Write( "-> Calling FindTargetSection" )
  call FindTargetSection( This, TargetSection,  &
          FromSubSection  =   FromSubSection,       &
          FoundSection    =   FoundSection,         &
          CallProc= CallProc,     &
          Debug         =   Debug               )
  if (Dbg) call Logger%Write( "-> FoundSection = ", FoundSection )

  NParameters       =   0                                                                                 ! ... setting the number of parameters to zero
  if ( FoundSection ) NParameters = TargetSection%NParameters                                                        ! ... getting its number of parameters

  nullify( TargetSection )

  if (Dbg) call Logger%Exiting()

End Procedure

! The call to this procedure only make sens if the parameter has some ValidValues.
! If not, an error should appear.
Module Procedure GetParameterIndex

  use String_Library              ,only:  UpperCase, Inline, GetPosition

  character(*)                                              ,parameter  ::  ProcName = "GetParameterIndex" ! Name of current procedure
  logical                                                               ::  Dbg
  type(InputParameter_Type)                                             ::  Param                           !
  character(:)  ,allocatable                                            ::  Value

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

! ==============================================================================================================
!    EXTRACTING THE PARAMETER FROM THE SECTION
! ==============================================================================================================
! The parameter is first extracted from the target section using the Properties object. Note that the parameter
! is allowed not to be defined if it has a default value. In such case, the output optional indicators are set
! to "Found=F" and "Defined=T". From this point, the following error can be checked:
!  * If the parameter is not defined and if it has no default value, then an error should be raised
!  * If the parameter has no set of valid values, then error should be raised
! Raising an error is justified since no output index can be returned in both these cases. These errors are
! checked in the following sections.
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Extracting parameter " // Properties%GetName() )
  if (Dbg) call Logger%Write( "-> Calling This%GetParameter" )
  Param         =   This%GetParameter( Properties, SectionName=SectionName, Debug=Debug )            ! Extracting the parameter from the section
  if (Dbg) call Logger%Write( "-> Param%Defined = ", Param%Defined )
! ==============================================================================================================


! ==============================================================================================================
!    CHECKING THAT TARGET PARAMETER IS DEFINED OR HAS A DEFAULT VALUE
! ==============================================================================================================
! @TODO: Comment and error
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Checking that target parameter is defined or has a default value" )
  if (Dbg) call Logger%Write( "-> Param%Defined = ", Param%Defined )
  if (Dbg) call Logger%Write( "-> Param%Properties%HasDefaultValue = ", Param%Properties%HasDefaultValue )
  if (Dbg) call Logger%Write( "-> Properties%HasDefaultValue = ", Properties%HasDefaultValue )
  if ( (.Not.Param%Defined) .and. (.Not.Param%Properties%HasDefaultValue) ) &                                 ! If the parameter is not defined and if it has no default value, then ...
  call Error_Undefined_Param_And_No_DefaultValue( Param, ProcName )                                            ! ... error
  if (Dbg) call Logger%Write( "-> Check passed" )
! ==============================================================================================================


! ==============================================================================================================
!    CHECKING THAT THE TARGET PARAMETER HAS A SET OF VALID VALUES
! ==============================================================================================================
! This procedure returns the index associated to a parameter value in the list of valid values.
! Therefore, the call to this procedure only make sense if the target parameter has a list of valid values.
! As a result, the input Properties object need to have a set of valid values.
! Note that this check could have been done in the input Parameter-Properties object.
! However, the check is performed on the extracted Parameter object in order to have more info (its value)
! This section checks that this is indeed the case, and raises an error if not.
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Checking that the target parameter has a set of valid values" )
  if (Dbg) call Logger%Write( "-> Param%Defined = ", Param%Defined )
  if (Dbg) call Logger%Write( "-> Param%Properties%HasValidValues = ", Param%Properties%HasValidValues )
  if (Dbg) call Logger%Write( "-> Properties%HasValidValues = ", Properties%HasValidValues )
  if ( .Not. Param%Properties%HasValidValues ) call Error_Undefined_ValidValues( Param, ProcName )
  if (Dbg) call Logger%Write( "-> Check passed" )
! ==============================================================================================================


! ==============================================================================================================
!    EXTRACTING THE VALUE FROM THE PARAMETER
! ==============================================================================================================
! Here, the value is extracted from the parameter. This value is converted to uppercase if the parameter is not
! case-sensitive.  Note that if the parameter is not defined and if it has a default value, then the return
! value corresponds to the default value. Thus, if present, the output optional indicators are set to "Found=F"
! and "Defined=T".
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Extracting the value from parameter " // Properties%GetName() )
  if (Dbg) call Logger%Write( "-> Calling Param%GetValue" )
  call Param%GetValue(            &
        Value,                    &
        CallProc  =   ProcName  , &
        Status    =   Status      )
  if ( UpdateStatus(Status,Proc=ProcName,ExitLogger=Dbg) ) return

  if ( .Not. Param%Properties%CaseSensitive ) Value = UpperCase(Value)                                          ! Converting to uppercase if not case-sensitive
  if (Dbg) call Logger%Write( "-> Value = ", Value )
! ==============================================================================================================


! ==============================================================================================================
!    FINDING THE VALUE POSITION IN THE LIST OF VALID VALUES
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Finding the value's position in the list of valid values" )
  if (Dbg) call Logger%Write( "-> Calling GetPosition of string '" // Value // "' inside array '" // Inline(Param%Properties%ValidValues,Separator="  ") // "' with CaseSensitive = ", Param%Properties%CaseSensitive )
  iParam        =   GetPosition( Value, Param%Properties%ValidValues, CaseSensitive=Param%Properties%CaseSensitive )
  if (Dbg) call Logger%Write( "-> iParam = ", iParam )
! ==============================================================================================================


! ==============================================================================================================
!    CHECKING THAT THE INDEX CORRESPONDS TO A VALID VALUE
! ==============================================================================================================
! If the position of the parameter value in the list of accepted value is zero, this means that the parameter
! value does not corresponds to any exiting valid values.
! This case corresponds to an erroneous input an so an error is raised.
! ==============================================================================================================
  if ( iParam == 0 ) call Error_Value_Do_Not_Match_Any_ValidValues( Param, ProcName )                           ! If the returned index is zero, then the value has not been found in the list of valid values and so error
! ==============================================================================================================



! ==============================================================================================================
!    SETTING THE OUTPUT INDICATOR
! ==============================================================================================================
  if ( present(Found) )         Found   =   Param%Defined
  if ( present(Defined) )       Defined =   (iParam /= 0)
! ==============================================================================================================


  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure GetParameterFromIndex

  use InputSection_Tools    ,only:  Set_Procedure_Path

  character(*)                                              ,parameter  ::  ProcName = "GetParameterFromName" ! Name of current procedure
  logical                                                               ::  Dbg
  logical                                                               ::  Mandatory_
  logical                                                               ::  FoundParameter
  logical                                                               ::  Found_
  logical                                                               ::  IsRootSection
  character(:)  ,allocatable                                            ::  ProcPath
  type(InputSection_Type)                     ,pointer                  ::  TargetSection

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  Param%Name      =   ""
  FoundParameter  =   .False.

  if (Dbg) call Logger%Write( "Finding the section from where the parameters should be extracted" )
  if (Dbg) call Logger%Write( "-> Calling FindTargetSection" )
  call FindTargetSection( This, TargetSection,  &
          FromSubSection  =   FromSubSection,   &
          FoundSection    =   Found_,           &
          IsRootSection   =   IsRootSection,    &
          Debug         =   Debug           )

  if (Found_) then
    Found_    =     allocated( TargetSection%Parameters )
    if ( Found_ ) then
      Found_  =     ( ParameterIndex >= lbound(TargetSection%Parameters,1) )  &
              .and. ( ParameterIndex <= ubound(TargetSection%Parameters,1) )
    end if
    if (Found_) then
      Param = TargetSection%Parameters(ParameterIndex)
    end if
  end if

  Mandatory_  =   .False.
  if ( present(Mandatory) ) Mandatory_ = Mandatory                                 ! ... otherwise setting the local variable to the default variable

  if ( (.Not.Found_) .and. Mandatory_ ) then
    ProcPath      =   Set_Procedure_Path( ProcName )                                       ! Setting the procedure path
    call Error_Mandatory_Parameter_Not_Found( Param, CallProc=ProcPath )
  end if

  if ( present(Found) ) Found = Found_

  nullify( TargetSection )

  if (Dbg) call Logger%Exiting()

End Procedure


! This procedure extracts a Parameter from a given Section using a Parameter name.
! The parameter extraction occurs in two steps:
! 1) Construction of a ParameterProperties object from the input arguments,
! 2) Extraction of the Parameters object using the newly created ParameterProperties object.
Module Procedure GetParameterFromName
  character(*)                                              ,parameter  ::  ProcName = "GetParameterFromName" ! Name of current procedure
  logical                                                               ::  Dbg
  type(InputParamProperties_Type)                                       ::  Properties                          ! Parameter-Properties object associated to the parameter to be added
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  Properties  =   InputParamProperties_Type(              &
                      Name            =   ParameterName,  &
                      CaseSensitive   =   CaseSensitive,  &
                      Mandatory       =   Mandatory       )
  Param       =   This%GetParameter( Properties,          &
                      SectionName     =   SectionName,    &
                      Debug         =   Debug         )
  if (Dbg) call Logger%Exiting()
End Procedure


! This procedure extracts a Parameter from a given Section using a ParameterProperties object.
! The parameter extraction occurs in two steps:
! 1) Construction of a ParameterProperties object from the input arguments,
! 2) Extraction of the Parameters object using the newly created ParameterProperties object.
Module Procedure GetParameterFromNameProperties
  character(*)                                              ,parameter  ::  ProcName = "GetParameterFromNameProperties" ! Name of current procedure
  logical                                                               ::  Dbg
  type(InputParamProperties_Type)                                       ::  Properties_
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  Properties_ =   InputParamProperties_Type(              &
                      Name            =   Name,           &
                      Properties      =   Properties,     &
                      Mandatory       =   Mandatory       )
  Param       =   This%GetParameter( Properties_,         &
                      SectionName     =   SectionName,    &
                      Debug         =   Debug         )
  if (Dbg) call Logger%Exiting()
End Procedure





! This procedure searches for a parameter whose name corresponds to the name of the input Parameter-Properties object.
! The search is only done in the current section, and not in all its subsections, if any.
! If a parameter is found, then it is returned.
Module Procedure GetParameterFromProperties
  use String_Library              ,only:  UpperCase

  character(*)                                              ,parameter  ::  ProcName = "GetParameterFromProperties" ! Name of current procedure
  logical                                                               ::  Dbg
  logical                                                               ::  FoundParameter
  logical                                                               ::  Mandatory_                 ! Indicator whether the target parameter is mandatory. If it is, and if the section is not found, then an error is raised
  logical                                                               ::  FoundSection
  integer                                                               ::  i                                   ! Index of sections/parameters
  character(:)  ,allocatable                                            ::  TargetParamaterName           ! Name of the target parameter
  character(:)  ,allocatable                                            ::  CurrentParameterName          ! Name of a given parameter
  character(:)  ,allocatable                                            ::  SectionName_
  type(InputSection_Type)    ,pointer                                   ::  TargetSection

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  Mandatory_      = Properties%Mandatory;  if ( present(Mandatory)     ) Mandatory_      =   Mandatory
  SectionName_    = "";       if ( present(SectionName)   ) SectionName_    =   SectionName

  if (Dbg) then
    call Logger%Write( "Input variables:" )
    call Logger%Write( "-> Properties%GetName() = ", Properties%GetName() )
    call Logger%Write( "-> Mandatory_           = ", Mandatory_ )
    call Logger%Write( "-> SectionName_         = ", SectionName_ )
  end if

  call Param%SetName( Properties%GetName() )
  Param%Value      =   ""        ! Setting an empty string so that the param always has a value

  if ( len_trim(Properties%GetName()) == 0 ) &
  call Error_Nameless_Properties_Get( Properties, CallProc=ProcName )


  nullify( TargetSection )

! Case when the target parameter is to be searched in a subsection of current section
  if ( len_trim(SectionName_) /= 0 ) then
    if (Dbg) call Logger%Write( "Searching for target parameter '"//Properties%GetName()//"' in sub-section '"//SectionName_//"' of the root section" )
    if (Dbg) call Logger%Write( "-> Calling This%HasSection: SubSection = ", SectionName_ )
    FoundSection    =   This%HasSection( SectionName_ )
    if (Dbg) call Logger%Write( "-> FoundSection = ", FoundSection )
    if ( FoundSection ) then
      if (Dbg) call Logger%Write( "-> TargetSection '"// SectionName_ // "' has been found" )
      if (Dbg) call Logger%Write( "-> Calling This%GetSectionPointer" )
      call This%GetSectionPointer( SectionName_, TargetSection )
    end if
! Case when the target parameter is to be searched in the current section only
  else
    if (Dbg) call Logger%Write( "Searching for target parameter in the root section" )
    TargetSection       =>    This
  end if



  FoundParameter  =   .False.
  Param%Defined   =   .False.
  Param%Name      =   Properties%GetName()                                                                     ! ... just set the ouput parameter name (and at the end of this procedure, the ouput parametr properties are also set)
  Param%Value    =   ""        ! Setting an empty string so that the param always has a value

  if ( associated(TargetSection) ) then
    Param%ParentSection   =   TargetSection%Name
    TargetParamaterName   =   Param%Name                                                                   ! Getting the name of the target parameter from the input parameter name
    if ( .Not. Properties%CaseSensitive ) TargetParamaterName = UpperCase( TargetParamaterName )                          ! If the parameter name is case insensitive, then convert the target parameter name to upper case
    if (Dbg) call Logger%Write( "Target parameter name: ", Param%Name )
    do i = 1,TargetSection%NParameters                                                                                   ! Loop on all parameters of the current section
      CurrentParameterName    =   TargetSection%Parameters(i)%Name                                                 ! Getting the name of the current parameter
      if ( .Not. Properties%CaseSensitive ) CurrentParameterName = UpperCase( CurrentParameterName )               ! If the parameter name is case insensitive, then convert the current parameter name to upper case
      if (Dbg) call Logger%Write( "-> i = ", i, "Name = ", TargetSection%Parameters(i)%Name )
      if ( CurrentParameterName /=  TargetParamaterName ) cycle                                                 ! If the current and target parameters have different names, then going to the next parameter
      if (Dbg) call Logger%Write( "-> Parameter found" )
      Param           =   TargetSection%Parameters(i)                                                              ! Setting the ouput parameter to the current parameter
      FoundParameter  =   .True.
      exit                                                                                                      ! Exiting the loop on all parameters of current section since the target parameter has been found
    end do                                                                                                      ! End loop on parameters of the current section
  end if

  if (Dbg) call Logger%Write( "Param%Defined = ", Param%Defined )
  if (Dbg) call Logger%Write( "Copying input properties in output parameter => Calling Param%SetProperties" )
  call Param%SetProperties( Properties=Properties )                                                            ! Set the properties of the output parameter to the one of the input Parameter-Properties object

  if ( (.Not.FoundParameter) .and. Mandatory_ ) call Error_Mandatory_Parameter_Not_Found( Param, CallProc=ProcName )

  if (Dbg) call Logger%Exiting()

End Procedure






! This procedure extracts a set of parameters from a given section, or any of its sub-sections.
! The section from which the Parameter objects are extracted depends on the presence of the optional input
! argument 'FromSubSection'
! * if the argument is absent, then the Parameter objects are extracted from the current section 'This'.
! * if the argument is present, then the Parameter objects are extracted from a sub-section of current section
!  whose name matches the name given in 'FromSubSection'.
Module Procedure GetParametersFromSection

  character(*)                                              ,parameter  ::  ProcName = "GetParametersFromSection" ! Name of current procedure
  logical                                                               ::  Dbg
  integer                                                               ::  i
  logical                                                               ::  ParameterFound
  logical                                                               ::  SectionFound
  logical                                                               ::  Mandatory_
  type(InputSection_Type)    ,pointer                                   ::  TargetSection

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  Mandatory_  = .False.; if ( present(Mandatory) ) Mandatory_ = Mandatory

  if (Dbg) then
    call Logger%Write( "Input variables:" )
    call Logger%Write( "-> This%Name      = ", This%Name )
    if ( present(FromSubSection)) call Logger%Write( "-> FromSubSection = ", FromSubSection )
    if ( present(Mandatory) )     call Logger%Write( "-> Mandatory      = ", Mandatory )
  end if


  call FindTargetSection( This, TargetSection,  &
          FromSubSection  =   FromSubSection,   &
          FoundSection    =   SectionFound      )

!   if ( present(FromSubSection) ) then                                                                             ! If a section name is given in input,
!     if (Dbg) call Logger%Write( "Extracting parameters from subsection '"// FromSubSection // "' of section '" // This%Name // "'" )
!     if (Dbg) call Logger%Write( "-> Calling This%HasSection" )
!     SectionFound  =   This%HasSection( FromSubSection )
!     if (Dbg) call Logger%Write( "-> SectionFound = ", SectionFound )
!     if ( SectionFound ) then
!       if (Dbg) call Logger%Write( "-> TargetSection '"// FromSubSection // "' has been found" )
!       if (Dbg) call Logger%Write( "-> Calling This%GetSectionPointer" )
!       call This%GetSectionPointer( FromSubSection, TargetSection )
!     else
!       if (Dbg) call Logger%Write( "-> TargetSection '"// FromSubSection // "' has not been found" )
!     end if
!   else
!     TargetSection   =>  This
!     SectionFound    =   .True.
!   end if



  if (SectionFound) then
    if (Dbg) call Logger%Write( "Setting the output Parameter objects 'Parameters'" )
!     allocate( Parameters, source = TargetSection%Parameters )
    allocate( Parameters(TargetSection%NParameters) ); Parameters(:) = TargetSection%Parameters(:)
!     allocate( Parameters(TargetSection%NParameters) ); do i = 1,size(Parameters); Parameters(i) = TargetSection%Parameters(i); end do
    ParameterFound =   size(Parameters) >= 1                                                            ! If at lease one parameter has been found, then setting the found indicator to true
    if (Dbg) then
      call Logger%Write( "-> size(Parameters) = ", size(Parameters) )
      do i = 1,size(Parameters)
      associate( P => Parameters(i) )
        call Logger%Write( "-> i = ", i, "P%Name = ", P%Name, "P%Value = ", P%Value, "P%Defined = ", P%Defined, Fi="i5" )
      end associate
      end do
    end if
  else
    allocate( Parameters(0) )                                                                                 ! Allocating to zero the dimension of the output array of Parameter objects
    ParameterFound   =   .False.                                                                         ! Setting the found indicator to false since no parameter has been found
  end if

  if ( Mandatory_ ) then
    if ( .Not. SectionFound   ) call Error_Mandatory_Section_Not_Found(     This, SectionName=FromSubSection, CallProc=ProcName )
    if ( .Not. ParameterFound ) call Error_Mandatory_Parameters_Not_Found(  This, SectionName=FromSubSection, CallProc=ProcName )
  end if

  if ( present(Found) ) Found = ParameterFound

  nullify( TargetSection )

  if (Dbg) call Logger%Exiting()

End Procedure















! If the optional argument "SectionName" corresponding to the section name is present, then the parameter to be
! found is searched in the subsection of current section whose name matches the input section name.
Module Procedure SectionHasParameterFromProperties
  character(*)                                              ,parameter  ::  ProcName = "SectionHasParameterFromProperties" ! Name of current procedure
  logical                                                               ::  Dbg
  logical                                                               ::  CaseSensitive_                      ! Indicator whether the parameter name is case sensitive
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  CaseSensitive_ = Properties%CaseSensitive
  if ( present(CaseSensitive) ) CaseSensitive_ = CaseSensitive
  FoundParameter    =   This%HasParameter( Properties%GetName(), SectionName=SectionName, CaseSensitive=CaseSensitive_, Debug=Debug )
  if (Dbg) call Logger%Exiting()
End Procedure

Module Procedure SectionHasParameterFromName

  use String_Library      ,only:  UpperCase

  character(*)                                              ,parameter  ::  ProcName = "SectionHasParameterFromName" ! Name of current procedure
  logical                                                               ::  Dbg
  logical                                                               ::  CaseSensitive_                      ! Indicator whether the parameter name is case sensitive
  logical                                                               ::  FoundSection                        ! Indicator whether the parameter name is case sensitive
  integer                                                               ::  i                                   ! Index of sections/parameters
  character(:)  ,allocatable                                            ::  TargetParamaterName                 ! Name of the target parameter
  character(:)  ,allocatable                                            ::  CurrentParameterName                ! Name of a given parameter
  type(InputSection_Type)    ,pointer                                   ::  TargetSection

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  CaseSensitive_ = .False.
  if ( present(CaseSensitive) ) CaseSensitive_ = CaseSensitive

  TargetParamaterName   =   trim(ParameterName)
  if ( .Not. CaseSensitive_ ) TargetParamaterName = UpperCase( TargetParamaterName )


  call FindTargetSection( This, TargetSection,  &
          FromSubSection  =   SectionName,      &
          FoundSection    =   FoundSection      )
!   if ( present(SectionName) ) then
!     FoundSection  =   This%HasSection( SectionName )
!     if ( FoundSection ) call This%GetSectionPointer( SectionName, TargetSection )
!   else
!     TargetSection   =>  This
!     FoundSection    =   .True.
!   end if

  FoundParameter    =   .False.
  if ( FoundSection ) then                                                                                      ! If the target section has been found
    do i = 1,TargetSection%NParameters                                                                          ! Loop on all parameters of the target section
      CurrentParameterName  =   TargetSection%Parameters(i)%Name                                                ! Getting the name of the current parameter
      if ( .Not. CaseSensitive_ ) CurrentParameterName = UpperCase( CurrentParameterName )                      ! If the parameter name is case insensitive, then convert the current parameter name to upper case
      if (Dbg) call Logger%Write( "i = ", i, "Name = ", TargetSection%Parameters(i)%Name )
      if ( CurrentParameterName /= TargetParamaterName ) cycle                                                  ! If the current and target parameters have different names, then going to the next parameter
      if (Dbg) call Logger%Write( "Parameter found" )
      FoundParameter   =    .True.                                                                              ! Setting the output variable
      exit                                                                                                      ! Exiting the loop on all parameters of current section since the target parameter has been found
    end do                                                                                                      ! End loop on parameters of the current section
  end if                                                                                                        ! End if case on target section
  nullify( TargetSection )

  if (Dbg) call Logger%Exiting()

End Procedure
!




























! **************************************************************************************************************
! **************************************************************************************************************
!                                           PRIVATE PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Error_Mandatory_Section_Not_Found( Section, SectionName, CallProc )

  use Error_Class    ,only:  Error

  type(InputSection_Type)                               ,intent(in)     ::  Section
  character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the section to be extracted from the input Section object (The passed-object dummy argument)
  character(*)                                ,optional ,intent(in)     ::  CallProc

  character(*)                                              ,parameter  ::  ProcName = "Error_Mandatory_Section_Not_Found"
  character(:)  ,allocatable                                            ::  ProcPath

  ProcPath      =   Logger%GetPath()                                                             ! Getting the path of all calling procedures stored in the Logger object

  call Error%Set_Title( "Error while trying to extract a section" )

  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  One try to extract a mandatory sub-section from a section but this sub-section is not found." )

  call Error%Add_Line( "Solutions:" )
  call Error%Add_Line( "  Potential solutions for this problem are:" )
  call Error%Add_Line( "  * Check the name of the target section to be extracted," )
  call Error%Add_Line( "  * Check if the target section really need to be mandatory," )
  call Error%Add_Line( "  * Check if the target section  is extracted from the correct parent section." )

  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Procedure raising the error: " // ProcName )
  if ( present(CallProc) )       call Error%Add_Line( "  Calling procedure:           " // CallProc )
  if ( len_trim(ProcPath) /= 0 )          call Error%Add_Line( "  Procedure path:              " // ProcPath )
  if ( allocated(Section%Name) )          call Error%Add_Line( "  Parent section:              " // Section%Name            )
  if ( present(SectionName) )             call Error%Add_Line( "  Target section:              " // SectionName            )

  call Error%Raise( Unit=Logger%GetUnit() )

End Subroutine

Subroutine Error_Mandatory_Parameter_Not_Found( Param, CallProc )

  use Error_Class    ,only:  Error

  type(InputParameter_Type)                     ,intent(in)     ::  Param                           !< Parameter object
  character(*)                                ,optional ,intent(in)     ::  CallProc

  character(*)                                              ,parameter  ::  ProcName = "Error_Mandatory_Parameter_Not_Found"
  character(:)  ,allocatable                                            ::  ProcPath

  ProcPath      =   Logger%GetPath()                                                             ! Getting the path of all calling procedures stored in the Logger object

  call Error%Set_Title( "Error while trying to extract a section" )

  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  A mandatory parameter is not found." )

  call Error%Add_Line( "Solutions:" )
  call Error%Add_Line( "  Potential solutions for this problem are:" )
  call Error%Add_Line( "  * Check the name of the parameter to be extracted," )
  call Error%Add_Line( "  * Check the section where the parameter is being extracted," )
  call Error%Add_Line( "  * Check whether the parameter really needs to be mandatory." )

  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Procedure raising the error: " // ProcName )
  if ( present(CallProc) )       call Error%Add_Line( "  Calling procedure:           " // CallProc )
  if ( len_trim(ProcPath) /= 0 )          call Error%Add_Line( "  Procedure path:              " // ProcPath )
  if (allocated(Param%ParentSection) )   call Error%Add_Line( "  Parent section:              " // Param%ParentSection      )
                                          call Error%Add_Line( "  Parameter name:             " // Param%Name           )

!   if ( Param%Properties%HasValidValues )  call Text%Add_Line( " Parameter possible values:  " // Inline( Param%Properties%ValidValues, Separator="  ") )
  if ( Param%Properties%HasDefaultValue ) call Error%Add_Line( "  Parameter default value:    " // Param%Properties%DefaultValue )

  call Error%Raise( Unit=Logger%GetUnit() )

End Subroutine

Subroutine Error_Nameless_Properties_Get( Properties, CallProc )

  use Error_Class          ,only:  Error

  class(InputParamProperties_Type)                      ,intent(in)     ::  Properties
  character(*)                                ,optional ,intent(in)     ::  CallProc

  character(*)                                              ,parameter  ::  ProcName = "Error_Nameless_Properties_Get"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable    ,dimension(:)                           ::  Lines

  ProcPath      =   Logger%GetPath()                                                             ! Getting the path of all calling procedures stored in the Logger object
  call Properties%Output( Lines )                                                                               ! Extracting the information from the parameter properties object

  call Error%Set_Title( "Error while processing a parameter" )

  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  A Parameter-Properties object has an empty name while this parameter is mandatory." )
  call Error%Add_Line( "  The objective here is to add a Parameter object to a section object using a Parameter-Properties object." )
  call Error%Add_Line( "  In particular, the name of Parameter-Properties object is used to located the Parameter object within the Section object." )
  call Error%Add_Line( "  Thus, if the name of Parameter-Properties object is empty, there is no way to find the Parameter object." )

  call Error%Add_Line( "Details:" )
  call Error%Add_Line( " Procedure raising the error: " // ProcName )
  if ( present(CallProc) )       call Error%Add_Line( " Calling procedure:           " // CallProc )
  if ( len_trim(ProcPath) /= 0 )          call Error%Add_Line( " Procedure path:              " // ProcPath )
  call Error%Add_Line( "Parameter-Properties properties:" )
  call Error%Add_Line( "  * " // Lines )

  call Error%Raise( Unit=Logger%GetUnit() )

End Subroutine

Subroutine Error_Mandatory_Parameters_Not_Found( Section, SectionName, CallProc )

  use Error_Class    ,only:  Error

  type(InputSection_Type)                               ,intent(in)     ::  Section
  character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the section to be extracted from the input Section object (The passed-object dummy argument)
  character(*)                                ,optional ,intent(in)     ::  CallProc

  character(*)                                              ,parameter  ::  ProcName = "Error_Mandatory_Parameters_Not_Found"
  character(:)  ,allocatable                                            ::  ProcPath

  ProcPath      =   Logger%GetPath()                                                             ! Getting the path of all calling procedures stored in the Logger object

  call Error%Set_Title( "Error while trying to extract Parameter objects from a Section" )

  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  A list of mandatory Parameter objects are being extracted from a Section object" )
  call Error%Add_Line( "  but this Section object contains no parameters" )

  call Error%Add_Line( "Solutions:" )
  call Error%Add_Line( "  Potential solutions for this problem are:" )
  call Error%Add_Line( "  * Check the name of the Section object from which the Parameter object are being extracted." )
  call Error%Add_Line( "  * Check if the Parameters object which are being extracted really need to be mandatory." )
  call Error%Add_Line( "  * Check if the Parameters object which are being extracted are specified in the input file." )

  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Procedure raising the error: " // ProcName )
  if ( present(CallProc) )       call Error%Add_Line( "  Calling procedure:           " // CallProc )
  if ( len_trim(ProcPath) /= 0 )          call Error%Add_Line( "  Procedure path:              " // ProcPath )
  if ( allocated(Section%Name) )          call Error%Add_Line( "  Parent section:              " // Section%Name            )
  if ( present(SectionName) )             call Error%Add_Line( "  Target section:              " // SectionName            )

  call Error%Raise( Unit=Logger%GetUnit() )

End Subroutine

Subroutine Error_Undefined_Param_And_No_DefaultValue( Param, CallProc )

  use Error_Class    ,only:  Error

  type(InputParameter_Type)                     ,intent(in)     ::  Param                           !< Parameter object
  character(*)                                ,optional ,intent(in)     ::  CallProc

  character(*)                                              ,parameter  ::  ProcName = "Error_Undefined_Param_And_No_DefaultValue"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable    ,dimension(:)                           ::  Lines

  ProcPath      =   Logger%GetPath()                                                             ! Getting the path of all calling procedures stored in the Logger object
!@@@  call Param%Properties%Output( Lines )                                                                         ! Extracting the information from the parameter properties object

  call Error%Set_Title( "Error while processing a parameter" )

  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  A parameter is undefined and it has no default value." )

  call Error%Add_Line( "Solutions:" )
  call Error%Add_Line( "  Potential solutions for this problem are:" )
  call Error%Add_Line( "  * Defining the parameter in the input file," )
  call Error%Add_Line( "  * Adding a default value to the parameter" )

  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Procedure raising the error: " // ProcName )
  if ( present(CallProc) )       call Error%Add_Line( "  Calling procedure:           " // CallProc )
  if ( len_trim(ProcPath) /= 0 )          call Error%Add_Line( "  Procedure path:              " // ProcPath )
  if (allocated(Param%ParentSection) )   call Error%Add_Line( "  Parent section:              " // Param%ParentSection      )
  call Error%Add_Line( "  Parameter properties:" )
  call Error%Add_Line( "  * Name = " // Param%Name )
  call Error%Add_Line( "  * Value = " // Param%Value )
  call Error%Add_Line( "  * " // Lines )

  call Error%Raise( Unit=Logger%GetUnit() )

End Subroutine

Subroutine Error_Undefined_ValidValues( Param, CallProc )

  use Error_Class    ,only:  Error

  type(InputParameter_Type)                     ,intent(in)     ::  Param                           !< Parameter object
  character(*)                                ,optional ,intent(in)     ::  CallProc

  character(*)                                              ,parameter  ::  ProcName = "Error_Undefined_ValidValues"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable    ,dimension(:)                           ::  Lines

  ProcPath      =   Logger%GetPath()                                                             ! Getting the path of all calling procedures stored in the Logger object
!@@@  call Param%Properties%Output( Lines )                                                                         ! Extracting the information from the parameter properties object

  call Error%Set_Title( "Error while processing a parameter" )

  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  Undefined valid values." )

  call Error%Add_Line( "Solutions:" )
  call Error%Add_Line( "  Potential solutions for this problem are:" )
  call Error%Add_Line( "  * Add a set of valid value to the current parameter," )
  call Error%Add_Line( "  * Change the way the parameter is processes." )

  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Procedure raising the error: " // ProcName )
  if ( present(CallProc) )    call Error%Add_Line( "  Calling procedure:           " // CallProc )
  if ( len_trim(ProcPath) /= 0 )       call Error%Add_Line( "  Procedure path:              " // ProcPath )
  if (allocated(Param%ParentSection) ) call Error%Add_Line( "  Parent section:              " // Param%ParentSection      )
  call Error%Add_Line( "  Parameter properties:" )
  call Error%Add_Line( "  * Name  = " // Param%Name )
  call Error%Add_Line( "  * Value = " // Param%Value )
  call Error%Add_Line( "  * " // Lines )

  call Error%Raise( Unit=Logger%GetUnit() )

End Subroutine

Subroutine Error_Value_Do_Not_Match_Any_ValidValues( Param, CallProc )

  use Error_Class    ,only:  Error

  type(InputParameter_Type)                     ,intent(in)     ::  Param                           !< Parameter object
  character(*)                                ,optional ,intent(in)     ::  CallProc

  character(*)                                              ,parameter  ::  ProcName = "Error_Value_Do_Not_Match_Any_ValidValues"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable    ,dimension(:)                           ::  Lines

  ProcPath      =   Logger%GetPath()                                                             ! Getting the path of all calling procedures stored in the Logger object
!@@@  call Param%Properties%Output( Lines )                                                                         ! Extracting the information from the parameter properties object

  call Error%Set_Title( "Error while processing a parameter" )

  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  You are trying to get the index of the 'possible value' of a parameter which matches a given value." )
  call Error%Add_Line( "  However, the actual parameter value do not match value in the list of 'possible values'." )
  call Error%Add_Line( "  As a result, the index cannot be defined." )
  if ( Param%Value == "" ) call Error%Add_Line( "Actually, the current parameter has no value." )

  call Error%Add_Line( "Solutions:" )
  call Error%Add_Line( "  This error is probably due to the fact that the parameter has not been given a value," )
  call Error%Add_Line( "  or has been given a wrong value." )
  call Error%Add_Line( "  This error can also occures if a parameter is not being defined in a section and it has no default values." )
  call Error%Add_Line( "  To solve the problem, make sur that the parameter is defined in the correct section." )
  call Error%Add_Line( "  Also, if current procedure is always going to be call on the current parameter," )
  call Error%Add_Line( "  then it might be a good idea to defined the parameter has mandatory." )
  call Error%Add_Line( "  This will allow to catch sooner the error." )

  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Procedure raising the error: " // ProcName )
  if ( present(CallProc) )       call Error%Add_Line( "  Calling procedure:           " // CallProc )
  if ( len_trim(ProcPath) /= 0 )          call Error%Add_Line( "  Procedure path:              " // ProcPath )
  if (allocated(Param%ParentSection) )   call Error%Add_Line( "  Parent section:              " // Param%ParentSection      )
  call Error%Add_Line( "  Parameter properties:" )
  call Error%Add_Line( "  * Name = " // Param%Name )
  call Error%Add_Line( "  * Value = " // Param%Value )
  call Error%Add_Line( "  * " // Lines )

  call Error%Raise( Unit=Logger%GetUnit() )

End Subroutine

End SubModule
