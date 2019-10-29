SubModule(InputSection_Class) InputSection_AddSection_SubClass

  use Logger_Class          ,only:  Logger, LogLevel_HEAVYDEBUG

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

!   Interface             AddSection
!     Module Procedure    AddSectionToSectionsFromSectionObject
!     Module Procedure    AddSectionToSectionsFromSectionName
!     Module Procedure    AddSectionsToSections
!   End Interface

  contains


! **************************************************************************************************************
!                            PROCEDURES FOR ADDING SECTIONS TO A SECTION
! **************************************************************************************************************

Module Subroutine AddCommandLineArguments( This, To_SubSection, LogLevel )

  use CommandLineInterface_Class  ,only:  CommandLineInterface_Type
  use String_Library              ,only:  Parse

  class(InputSection_Type)   ,target                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
  character(*)                                ,optional ,intent(in)     ::  To_SubSection                   !< Name (Eventually nested names) of the sub-section where Section object has to be added
  integer                                     ,optional ,intent(in)     ::  LogLevel                        !< Indicator of the log level

  character(*)                                              ,parameter  ::  ProcName='AddCommandLineArguments' ! Name of current procedure
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  Argument, Name, Value
  character(:)  ,allocatable                                            ::  Strings(:)
  type(CommandLineInterface_Type)                                       ::  CommandLineInterface

  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_HEAVYDEBUG)

  call Logger%Write( "Initializing CommandLineInterface object" )
  call Logger%Write( "-> Calling CommandLineInterface%Initialize" )
  call CommandLineInterface%Initialize()

  call Logger%Write( "Adding command-line arguments as new parameters" )
  do i = 1,CommandLineInterface%NArg
  associate( Arg => CommandLineInterface%Arg(i) )
    call Logger%Write( "-> i = ", i, "Arg%GetName() = ", Arg%GetName(), "Arg%GetValue() = ", Arg%GetValue(),  Fi="i3" )
    call This%AddParameter( Arg%GetName(), Arg%GetValue(), SectionName=To_SubSection, Debug=Logger%On() ) !, Action
  end associate
  end do


  call Logger%Exiting()

End Subroutine



! The possible actions are:
! * Merge:              If the section does not exist, it is created. If it exists, then the input and already existing section are merged (the list of parameters of both section are merged)
! * Replace:            If the section does not exist, it is created. If it exists, then the already existing section is replaced by the input section
! * Append:             If the section does not exist, it is created. If it exists, then a new section is created with the same name than the already existing section
! * OnlyIfAbsent:       If the section does not exist, it is created. If it exists, then nothing is done

Module Procedure AddSectionsFromSections
  integer                                                               ::  i
  do i = 1,size(Sections)
    call This%AddSection(  Sections(i),                              &
                            To_SubSection     =   To_SubSection,      &
                            CallProc =   CallProc,  &
                            Action            =   Action,             &
                            Debug           =   Debug             )
  end do
End Procedure

! This procedure adds a sub-section to a given section from a already existing Section object.
! @TODO: Add the optional argument: OnlyIfDefined, HasBeenAdded as for AddParameterFromParameter
Module Procedure AddSectionFromSection

  use InputParameter_Class    ,only:  AddParameter
  use String_Library          ,only:  UpperCase
  use Utilities_Library       ,only:  GetOptArgValue

  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName = "AddSectionFromSection" ! Name of current procedure
  character(:)  ,allocatable                                            ::  Action_                   ! Local indicator of the action to be performed when adding the element
  logical                                                               ::  OnlyContent_
  logical                                                               ::  Found
  integer                                                               ::  NElements
  type(InputSection_Type)    ,pointer                                   ::  TargetSection
  type(InputSection_Type)    ,pointer                                   ::  ExistingSection
  type(InputSection_Type)    ,allocatable                               ::  ListSections(:)

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  OnlyContent_    =   .False.
  if ( present(OnlyContent) ) OnlyContent_  =  OnlyContent

  if (OnlyContent_) then
    if (Dbg) call Logger%Write( "Entering mode 'OnlyContent'" )
    if (Dbg) call Logger%Write( "-> Section%NSections = ", Section%NSections )
    if (Dbg) call Logger%Write( "-> Section%NParameters = ", Section%NParameters )
    if ( Section%NSections > 0 ) then
      if (Dbg) call Logger%Write( "-> Calling This%AddSection" )
      call This%AddSection(  Section%Sections(:),                      &
                              To_SubSection     =   To_SubSection,      &
                              CallProc =   CallProc,  &
                              Action            =   Action,             &
                              Debug           =   Debug             )
    end if
    if ( Section%NParameters > 0 ) then
      if (Dbg) call Logger%Write( "-> Calling This%AddParameter" )
      call This%AddParameter(  Section%Parameters(:),                  &
                              SectionName       =   To_SubSection,      &
                              Action            =   Action,             &
                              Debug           =   Debug             )
    end if
    if (Dbg) call Logger%Exiting()
    return
  end if

! ==============================================================================================================
!   SETTING THE ACTION TO BE PERFORMED WHEN ADDING THE ELEMENT
! ==============================================================================================================
!   @TODO: Check that the input arguemnt 'Action' has a valid value [APPEND,MERGE,REPLACE]
  Action_ =   "APPEND"
  if ( present(Action) ) then
    select case ( UpperCase(Action) )
    case ("APPEND");  Action_ = "APPEND"
    case ("MERGE");   Action_ = "MERGE"
    case ("REPLACE"); Action_ = "REPLACE"
    end select
  end if


! ==============================================================================================================
!       FINDING THE SECTION WHERE INPUT THE SECTION HAS TO BE ADDED
! ==============================================================================================================
! The following instructions define the Section object where the input Section object "Section" is to be added.
! Basically, the section to be added is either added directly to the current section (ie. to the passed-object
! dummy argument) or to one of its sub-sections. The actual Section object in which the input section "Section"
! is to be added is referred as the target section in the following.
! In order to handle both cases (addition in current section or in any of its sub-sections) a local Section
! object is defined with the pointer attribute (the variable "TargetSection") and this pointer points to the
! selected taget section. The actual target section depends on the presence of the optional input argument
! "To_SubSection":
!  - It the optional argument is absent, then the section is to be added directly inside the current section.
!    Thus, the pointer Section object associated to the target section points to the passed-object dummy
!    argument. No further processing is required in this case.
!  - If the optional argument "To_SubSection" is present, then the section has to be added to a subsection
!    of the current section. The variable "To_SubSection" contains the name of the sub-section where the input
!    Section object has to be added. Eventually, it can contains a recursive list of sub-section names, the
!    final one being the section where the addition has to be done. The sub-section names are separated by
!    the ">" character. For example, if the input variable "To_SubSection" has the value "Name1>Name2>Name3"
!    then, the input section has to be added to the section called "Name3", which is a sub-section of the
!    section called "Name2", which is a sub-section of the section called "Name1", which is a sub-section of
!    the Section object corresponding to the passed-object dummy argument.
!    So, the first task is to check is this sub-section
!    There is 2 situations:
!    - It the sub-section already exist in the
! Note that the variable "To_SubSection" may
! contains a nested list of sub-section names (as in "SubSec_1 > SubSec_2 > SubSec_3") where is section names is
! separated by the ">" character.
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Finding section where input section '"//Section%Name//"' has to be added" )
  if ( present(To_SubSection) ) then                                                                            ! If a section name is given in input,
    if (Dbg) call Logger%Write( "-> Adding section '"// Section%Name // "' to sub-section '"// To_SubSection // "' of section '" // This%Name // "'" )
    if (Dbg) call Logger%Write( "-> Calling This%HasSection: '"//To_SubSection//"'" )
    Found       =   This%HasSection( To_SubSection, Debug=Debug )
    if (Dbg) call Logger%Write( "-> Found = ", Found )
    if ( .Not. Found ) then
      if (Dbg) call Logger%Write( "-> Sub-section '"//To_SubSection//"' not found in section '"//This%Name//"' => Creating it: Calling This%CreateSubSections" )
      if (Dbg) call Logger%Write( "-> Calling This%CreateSubSections to create sub-section '"//To_SubSection//"'" )
      call This%CreateSubSections( To_SubSection, Debug=Debug )
    end if
    if (Dbg) call Logger%Write( "-> Getting the Section pointer to the target section: Calling This%GetSectionPointer" )
    call This%GetSectionPointer( To_SubSection, TargetSection, Debug=Debug )
  else
    if (Dbg) call Logger%Write( "-> Adding input section '"// Section%Name // "' to section '" // This%Name // "'" )
    TargetSection   =>  This
  end if                                                                                                        ! End if case on present of the optional section name
  if (Dbg) call Logger%Write( "-> Name of target section where input section need to be added: TargetSection%Name = ", TargetSection%Name )


! ==============================================================================================================
!       EXTRACTING FROM THE TARGET SECTION THE SECTION CORRESPONDING TO THE INPUT SECTION
! ==============================================================================================================

  if (Dbg) call Logger%Write( "Extracting from TargetSection '"//TargetSection%Name//"' pointer to input section '"//Section%Name//"'" )
  if (Dbg) call Logger%Write( "-> Calling TargetSection%GetSectionPointer: Section%Name = ", Section%Name )
  call TargetSection%GetSectionPointer( Section%Name, ExistingSection, Debug=Debug )
  Found   =   associated(ExistingSection)
  if (Dbg) call Logger%Write( "-> Found = ", Found )
! ************************************************************************************


! ==============================================================================================================
!       CASE WHEN THE SECTION TO BE ADDED DOES NOT ALREADY EXIST IN THE TARGET SECTION
! ==============================================================================================================
! If the section to be added does not already exist in the target section, then it is simply added as a
! subsection of the target section using the "AddElementToSectionsFromStrings" procedure.
! The number of section contained in the target section also need to be updated.
! ==============================================================================================================
  if ( .Not. Found) then
    if (Dbg) call Logger%Write( "-> No sub-section '"// Section%Name // "' in target section '" // TargetSection%Name // "'" )

!     if (Dbg) call Logger%Write( "-> Adding the input section '"//Section%Name//"' inside section '"// TargetSection%Name //"'" )
!     call AddSection( TargetSection%Sections, Section )

    if ( .Not. RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(TargetSection%Sections) ) allocate( TargetSection%Sections(0) )
    allocate( ListSections, source = [TargetSection%Sections,Section] )
    deallocate( TargetSection%Sections )
    allocate( TargetSection%Sections, source = ListSections )
    TargetSection%NSections    =   size(TargetSection%Sections)
    TargetSection%Empty        =   ( TargetSection%NSections == 0 ) .and. ( TargetSection%NParameters == 0 )



! ==============================================================================================================
!       CASE WHEN THE SECTION TO BE ADDED DOES NOT ALREADY EXIST IN THE TARGET SECTION
! ==============================================================================================================
! If the section to be added already exists in the target section, then the actual actions to be performed
! depends on the "Action" variable. The following action are allowed:
! - Append: If the input section is to be appended, then it is added as a subsection of the target section,
!           next to already existing section.
! - Replace: This will override the already existing section by the input section inside the target section.
! - Merge: This will merge all parameters and sub-sections of the already existing section and input section,
!          and override the already exixting section by the merged section.
!
! ==============================================================================================================
  else                                                                                                          ! If the section to be added already exists in the target section, then ...

    if (Dbg) call Logger%Write( "-> Sub-section '"// Section%Name // "' already exists in target section '" // TargetSection%Name // "'" )
!     if (Dbg) call Logger%Write( "-> Calling ExistingSection%Write" )
!     if (Dbg) call ExistingSection%Write( Logger%GetUnit() )
    if (Dbg) call Logger%Write( "-> Action to be performed is: ", Action_ )
    select case ( Action_ )
    case ("APPEND")
      if (Dbg) call Logger%Write( "-> Appending Section")
!########################################################################
! @COMPILER_BUG ifort 18.0.0: segmentation fault occurred
! WORKAROUND: For now, do not use the procedure 'AddSection'.
!########################################################################
!       if (Dbg) call Logger%Write( "-> Calling AddSection")
!       call AddSection( TargetSection%Sections, Section )
!########################################################################
      Block
        type(InputSection_Type)    ,dimension(:)   ,allocatable               ::  SectionsList
        integer                                                               ::  i
        if ( .Not. RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(TargetSection%Sections) ) allocate( TargetSection%Sections(0) )
        allocate( SectionsList, source = [TargetSection%Sections,Section] )
#     ifdef SUPPORTED_RECURSIVE_ALLOCATABLE_DERIVEDTYPE
        call move_alloc( SectionsList, TargetSection%Sections )
#     else
        do i = 1,size(TargetSection%Sections)
          call TargetSection%Sections(i)%Free()
        end do
        nullify( TargetSection%Sections )
        allocate( TargetSection%Sections, source = SectionsList )
#     endif
      end block
!########################################################################


      if (Dbg) call Logger%Write( "-> Ok appending Section")
      TargetSection%NSections    =   size(TargetSection%Sections)
      if (Dbg) call Logger%Write( "-> TargetSection%NSections = ", TargetSection%NSections )
    case ("REPLACE")
      if (Dbg) call Logger%Write( "-> Replacing Section")
      ExistingSection        =   Section
    case ("MERGE")
      if (Dbg) call Logger%Write( "-> Merging Sections")

      if (Dbg) call Logger%Write( "-> Adding parameters to ExistingSection from Section: Calling AddParameter" )
      NElements   =   size(ExistingSection%Parameters)
      call AddParameter( ExistingSection%Parameters, Section%Parameters )
      ExistingSection%NParameters   =   size(ExistingSection%Parameters)
      if (Dbg) call Logger%Write( "-> Done adding parameters: Number of parameters: ", NElements, "=>   ", ExistingSection%NParameters )

      if (Dbg) call Logger%Write( "-> Adding Sections from TargetSection to Section: Calling AddSection" )
      NElements   =   size(ExistingSection%Sections)
      if ( Section%NSections > 0 ) call ExistingSection%AddSection( Section%Sections, Action=Action_, Debug=Debug )
!       do i = 1,Section%NSections
!         if (Dbg) call Logger%Write( "-> j = ", j, "Adding Section Section%Sections(j)%Name = ", Section%Sections(j)%Name, "to section ExistingSection%Name = ", ExistingSection%Name )
!         call ExistingSection%AddSection( Section%Sections(j), Action=Action_, Debug=Debug )
!       end do
      ExistingSection%NSections     =   size(ExistingSection%Sections)
      if (Dbg) call Logger%Write( "-> Done adding sections: Number of sections: ", NElements, "=>   ", ExistingSection%NSections )

    end select


  end if

  nullify(ExistingSection)

  if (Dbg) call Logger%Exiting()
End Procedure

Module Procedure AddSectionFromSectionName
  use Utilities_Library       ,only:  GetOptArgValue
  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName = "AddSectionFromSectionName" ! Name of current procedure
  type(InputSection_Type)                                               ::  Section                             ! Section object to be added
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  Section%Name  = SectionName                                                                                  ! Setting the name of the Section object to be added
  call This%AddSection( Section, To_SubSection, CallProc, Action, Debug )                           ! Adding the section object
  if (Dbg) call Logger%Exiting()
End Procedure

Module Procedure AddSectionToSectionsFromSectionObject
  type(InputSection_Type)    ,dimension(:)   ,allocatable               ::  SectionsList
  integer                                                               ::  i
  if ( .Not. RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(Sections) ) allocate( Sections(0) )
  allocate( SectionsList, source = [Sections,NewSection] )
# ifdef SUPPORTED_RECURSIVE_ALLOCATABLE_DERIVEDTYPE
    call move_alloc( SectionsList, Sections )
# else
    do i = 1,size(Sections)
      call Sections(i)%Free()
    end do
    nullify( Sections )
    allocate( Sections, source = SectionsList )
# endif
End Procedure

Module Procedure AddSectionToSectionsFromSectionName
  type(InputSection_Type)                                               ::  New_Section
  New_Section%Name      =   NewSectionName
  call AddSectionToSectionsFromSectionObject( Sections, New_Section )
End Procedure


Module Procedure AddSectionsToSections
  type(InputSection_Type)    ,dimension(:)   ,allocatable                    ::  List_Sections
  if ( .Not. RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(Sections) ) allocate( Sections(0) )
  allocate( List_Sections, source = [Sections,NewSections] )
  deallocate( Sections )
  allocate( Sections, source = List_Sections )
End Procedure


End SubModule
