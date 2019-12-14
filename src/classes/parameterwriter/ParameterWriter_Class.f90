! -*-f90-*-
!!--------------------------------------------------------------------------------------------------------------------------------
!!
!! Stochastic Modeling & Uncertainty Quantification (SMUQ)
!!
!! Copyright (C) 2016 Venturi, Simone & Rostkowski, Przemyslaw (University of Illinois at Urbana-Champaign)
!!
!! This program is free software; you can redistribute it and/or modify it under the terms of the Version 2.1 GNU Lesser General
!! Public License as published by the Free Software Foundation.
!!
!! This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
!!
!! You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to the Free 
!! Software Foundation, Inc. 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
!!
!!--------------------------------------------------------------------------------------------------------------------------------

module ParameterWriter_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MFileInput_Class                                              ,only:    MFileInput_Type
use MFileInputContainer_Class                                     ,only:    MFileInputContainer_Type
use MFileInput_Factory_Class                                      ,only:    MFileInput_Factory
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use InputDet_Class                                                ,only:    InputDet_Type
use String_Library

implicit none

private

public                                                                ::    ParameterWriter_Type

type                                                                  ::    FileProcessor_Type
  type(String_Type), allocatable, dimension(:)                        ::    TemplateTranscript
  type(MFileInputContainer_Type), allocatable, dimension(:)           ::    MFileInputs
  integer                                                             ::    NbMFileInputs
  type(SMUQFile_Type)                                                 ::    ModelFile
contains
  procedure, public                                                   ::    Initialize              =>    Initialize_FP
  procedure, public                                                   ::    Reset                   =>    Reset_FP
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults_FP
  generic, public                                                     ::    Construct               =>    ConstructInput_FP
  procedure, private                                                  ::    ConstructInput_FP
  procedure, public                                                   ::    GetInput                =>    GetInput_FP
  procedure, public                                                   ::    WriteInput              =>    WriteInput_FP
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy                    =>    Copy_FP
  final                                                               ::    Finalizer               =>    Finalizer_FP
end type

type                                                                  ::    ParameterWriter_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbFiles
  type(FileProcessor_Type), allocatable, dimension(:)                 ::    FileProcessors
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    WriteInput
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(ParameterWriter_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'ParameterWriter'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(ParameterWriter_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%FileProcessors) ) deallocate(This%FileProcessors, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FileProcessors', ProcName=ProcName, stat=StatLoc )

    This%NbFiles = 0

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(ParameterWriter_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    use String_Library
    use StringRoutines_Module
    class(ParameterWriter_Type), intent(inout)                        ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(MFileInput_Type), allocatable                               ::    FileProcessor
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    integer                                                           ::    i
    

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix
    
    SectionName = 'files'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbFiles = InputSection%GetNumberofSubSections()

    allocate(This%FileProcessors(This%NbFiles), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%FileProcessors', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbFiles
      SubSectionName = SectionName // '>file' // ConvertToString(Value=i)
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call This%FileProcessors(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(ParameterWriter_Type), intent(in)                           ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i                        


    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )
    
    SectionName = 'files'
    call GetInput%AddSection( SectionName=SectionName )

    i = 1
    do i = 1, This%NbFiles
      SubSectionName = 'file' // ConvertToString(Value=i)
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/file' // ConvertToString(Value=i)
      call GetInput%AddSection( Section=This%FileProcessors(i)%GetInput(MainSectionName=SubSectionName, Prefix=PrefixLoc,         &
                                                                              Directory=DirectorySub), To_SubSection=SectionName )
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInput( This, Input )

    class(ParameterWriter_Type), intent(inout)                        ::    This
    type(InputDet_Type), intent(in)                                   ::    Input

    character(*), parameter                                           ::    ProcName='WriteInput'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    type(String_Type), allocatable, dimension(:)                      ::    Strings

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    i = 1
    do i = 1, This%NbFiles
      call This%FileProcessors(i)%WriteInput( Input=Input )
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(ParameterWriter_Type), intent(out)                          ::    LHS
    class(ParameterWriter_Type), intent(in)                           ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    select type (RHS)
  
      type is (ParameterWriter_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%NbFiles = RHS%NbFiles
          allocate(LHS%FileProcessors, source=RHS%FileProcessors, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%FileProcessors', ProcName=ProcName, stat=StatLoc )
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(ParameterWriter_Type),intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%FileProcessors) ) deallocate(This%FileProcessors, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FileProcessors', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_FP( This )

    class(FileProcessor_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Initialize_FP'

    if ( .not. This%Initialized ) then
      This%Name = 'FileProcessor'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_FP( This )

    class(FileProcessor_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Reset_FP'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%MFileInputs) ) deallocate(This%MFileInputs, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MFileInputs', ProcName=ProcName, stat=StatLoc )
    This%NbMFileInputs = 0

    if ( allocated(This%TemplateTranscript) ) deallocate(This%TemplateTranscript, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TemplateTranscript', ProcName=ProcName, stat=StatLoc )

    call This%ModelFile%Reset()

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_FP( This )

    class(FileProcessor_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults_FP'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_FP( This, Input, Prefix )

    use String_Library
    use StringRoutines_Module
    class(ParameterWriter_Type), intent(inout)                        ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput_FP'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(MFileInput_Type), allocatable                               ::    FileProcessor
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    integer                                                           ::    i
    class(MFileInput_Type), allocatable                               ::    MFileInput

    

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix
    
    SectionName = 'template'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ImportFile( Strings=This%TemplateTranscript, Input=InputSection, Prefix=PrefixLoc )
    nullify(InputSection)

    allocate(This%WorkStrings1, source=This%TemplateTranscript, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%WorkStrings1', ProcName=ProcName, stat=StatLoc )

    allocate(This%WorkStrings2, source=This%TemplateTranscript, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%WorkStrings2', ProcName=ProcName, stat=StatLoc )

    SectionName = 'file'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/file'
    call GetInput%AddSection( Section=This%ModelFile(i)%GetInput(MainSectionName=SectionName, Prefix=PrefixLoc,                   &
                                                                              Directory=DirectorySub), To_SubSection=SectionName )

    SectionName = 'formats'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbMFileInputs = InputSection%GetNumberofSubSections()
    nullify(InputSection)

    i = 1
    do i = 1, This%NbMFileInputs
      SubSectionName = SectionName // '>format' // ConvertToString(Value=i)
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call MFileInput_Factory%Construct( Object=MFileInput, Input=InputSection, Prefix=PrefixLoc )
      call This%MFileInputs(i)%Set( Object=MFileInput )
      deallocate(FileProcessor, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='FileProcessor', ProcName=ProcName, stat=StatLoc )
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_FP( This, MainSectionName, Prefix, Directory )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput_FP

    class(ParameterWriter_Type), intent(in)                           ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput_FP'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i
    class(MFileInput_Type), pointer                                   ::    MFileInputPtr=>null()


    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput_FP%SetName( SectionName = trim(adjustl(MainSectionName)) )
    
    SectionName = 'file'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/file'
    call GetInput_FP%AddSection( Section=This%ModelFile(i)%GetInput_FP(MainSectionName=SectionName, Prefix=PrefixLoc,             &
                                                                              Directory=DirectorySub) )

    SectionName = 'template'
    call GetInput_FP%AddSection( SectionName=SectionName )
    call GetInput_FP%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call ExportFile( Input=InputSection, Strings=This%TemplateTranscript )
    nullify(InputSection)

    SectionName = 'formats'
    call GetInput_FP%AddSection( SectionName=SectionName )

    i = 1
    do i = 1, This%NbMFileInputs
      SubSectionName = 'format' // ConvertToString(Value=i)
      MFileInputPtr => This%MFileInputs(i)%GetPointer()
      call GetInput_FP%AddSection( Section=MFileInput_Factory%GetObjectInput( Object=MFileInputPtr,MainSectionName=SubSectionName,&
                                                           Prefix=PrefixLoc, Directory=DirectorySub ), To_SubSection=SectionName )
      
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInput_FP( This, Input )

    class(ParameterWriter_Type), intent(inout)                        ::    This
    type(InputDet_Type), intent(in)                                   ::    Input

    character(*), parameter                                           ::    ProcName='WriteInput'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    class(MFileInput_Type), pointer                                   ::    MFileInputPtr=>null()
    type(String_Type), allocatable, dimension(:)                      ::    WorkStrings1
    type(String_Type), allocatable, dimension(:)                      ::    WorkStrings2

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(WorkStrings1, source=This%TemplateTranscript, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='WorkStrings1', ProcName=ProcName, stat=StatLoc )

    allocate(WorkStrings2, source=This%TemplateTranscript, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='WorkStrings2', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbFiles
      WorkStrings1 = WorkStrings2
      call This%MFileInputs(i)%WriteInput( Input=Input, Template=WorkStrings1, ProcessedTemplate=WorkStrings2 )
    end do

    call This%ModelFile(i)%Export( Strings=WorkStrings2 )

    deallocate(Workstrings2, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Workstrings2', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(ParameterWriter_Type), intent(out)                          ::    LHS
    class(ParameterWriter_Type), intent(in)                           ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    select type (RHS)
  
      type is (ParameterWriter_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%NbMFileInputs = RHS%NbMFileInputs
          allocate(LHS%TemplateTranscript, source=RHS%TemplateTranscript, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%TemplateTranscript', ProcName=ProcName, stat=StatLoc )
          LHS%ModelFile = RHS%ModelFile
          allocate(LHS%MFileInputs, source=RHS%MFileInputs, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%MFileInputContainer', ProcName=ProcName, stat=StatLoc )
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(ParameterWriter_Type),intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%MFileInputs) ) deallocate(This%MFileInputs, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%MFileInputs', ProcName=ProcName, stat=StatLoc )
    This%NbMFileInputs = 0

    if ( allocated(This%TemplateTranscript) ) deallocate(This%TemplateTranscript, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TemplateTranscript', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
