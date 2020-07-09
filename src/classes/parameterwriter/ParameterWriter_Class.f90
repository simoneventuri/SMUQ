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
use StringConversion_Module
use ArrayIORoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MFileInput_Class                                              ,only:    MFileInput_Type
use MFileInputContainer_Class                                     ,only:    MFileInputContainer_Type
use MFileInput_Factory_Class                                      ,only:    MFileInput_Factory
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use Input_Class                                                   ,only:    Input_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    ParameterWriter_Type

type                                                                  ::    FileProcessor_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  type(SMUQString_Type), allocatable, dimension(:)                    ::    TemplateTranscript
  type(MFileInputContainer_Type), allocatable, dimension(:)           ::    MFileInputs
  integer                                                             ::    NbMFileInputs
  type(SMUQFile_Type)                                                 ::    ModelFile
  character(:), allocatable                                           ::    TemplateComment
contains
  procedure, public                                                   ::    Initialize              =>    Initialize_FP
  procedure, public                                                   ::    Reset                   =>    Reset_FP
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults_FP
  generic, public                                                     ::    Construct               =>    ConstructInput1_FP,     &
                                                                                                          ConstructInput2_FP
  procedure, private                                                  ::    ConstructInput1_FP
  procedure, private                                                  ::    ConstructInput2_FP
  procedure, public                                                   ::    GetInput                =>    GetInput_FP
  procedure, public                                                   ::    WriteInput              =>    WriteInput_FP
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy                    =>    Copy_FP
  final                                                               ::    Finalizer_FP
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
  generic, public                                                     ::    Construct               =>    ConstructInput1,        &
                                                                                                          ConstructInput2
  procedure, private                                                  ::    ConstructInput1
  procedure, private                                                  ::    ConstructInput2
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    WriteInput
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(ParameterWriter_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'ParameterWriter'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(ParameterWriter_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    if (allocated(This%FileProcessors)) deallocate(This%FileProcessors, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%FileProcessors', ProcName=ProcName, stat=StatLoc)

    This%NbFiles = 0

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(ParameterWriter_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput1(This, Input, Prefix)

    use StringConversion_Module
    class(ParameterWriter_Type), intent(inout)                        ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput1'
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
    

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix
    
    SectionName = 'files'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    This%NbFiles = InputSection%GetNumberofSubSections()

    allocate(This%FileProcessors(This%NbFiles), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%FileProcessors', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbFiles
      SubSectionName = SectionName // '>file' // ConvertToString(Value=i)
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call This%FileProcessors(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput2(This, Input, ConstructPrefix, WritePrefix)

    use StringConversion_Module
    class(ParameterWriter_Type), intent(inout)                        ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    ConstructPrefix
    character(*), intent(in)                                          ::    WritePrefix

    character(*), parameter                                           ::    ProcName='ConstructInput2'
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

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()
    
    SectionName = 'files'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    This%NbFiles = InputSection%GetNumberofSubSections()

    allocate(This%FileProcessors(This%NbFiles), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%FileProcessors', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbFiles
      SubSectionName = SectionName // '>file' // ConvertToString(Value=i)
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call This%FileProcessors(i)%Construct(Input=InputSection, ConstructPrefix=ConstructPrefix, WritePrefix=WritePrefix)
      nullify(InputSection)
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    use StringConversion_Module

    type(InputSection_Type)                                           ::    GetInput

    class(ParameterWriter_Type), intent(in)                           ::    This
    character(*), intent(in)                                          ::    Name
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


    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput%SetName(SectionName = trim(adjustl(Name)))
    
    SectionName = 'files'
    call GetInput%AddSection(SectionName=SectionName)

    i = 1
    do i = 1, This%NbFiles
      SubSectionName = 'file' // ConvertToString(Value=i)
      if (ExternalFlag) DirectorySub = DirectoryLoc // '/file' // ConvertToString(Value=i)
      call GetInput%AddSection(Section=This%FileProcessors(i)%GetInput(Name=SubSectionName, Prefix=PrefixLoc,         &
                                                                              Directory=DirectorySub), To_SubSection=SectionName)
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInput(This, Input)

    class(ParameterWriter_Type), intent(inout)                        ::    This
    type(Input_Type), intent(in)                                      ::    Input

    character(*), parameter                                           ::    ProcName='WriteInput'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    i = 1
    do i = 1, This%NbFiles
      call This%FileProcessors(i)%WriteInput(Input=Input)
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

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

        if (RHS%Constructed) then
          LHS%NbFiles = RHS%NbFiles
          allocate(LHS%FileProcessors, source=RHS%FileProcessors, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%FileProcessors', ProcName=ProcName, stat=StatLoc)
        end if

      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(ParameterWriter_Type),intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if (allocated(This%FileProcessors)) deallocate(This%FileProcessors, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%FileProcessors', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_FP(This)

    class(FileProcessor_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Initialize_FP'

    if (.not. This%Initialized) then
      This%Name = 'FileProcessor'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_FP(This)

    class(FileProcessor_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Reset_FP'
    integer                                                           ::    StatLoc=0

    if (allocated(This%MFileInputs)) deallocate(This%MFileInputs, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%MFileInputs', ProcName=ProcName, stat=StatLoc)
    This%NbMFileInputs = 0

    if (allocated(This%TemplateTranscript)) deallocate(This%TemplateTranscript, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%TemplateTranscript', ProcName=ProcName, stat=StatLoc)

    call This%ModelFile%Reset()

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_FP(This)

    class(FileProcessor_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults_FP'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput1_FP(This, Input, Prefix)

    class(FileProcessor_Type), intent(inout)                          ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput1_FP'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    Found
    integer                                                           ::    i
    class(MFileInput_Type), allocatable                               ::    MFileInput

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix
    
    SectionName = 'template'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call ImportFile(Strings=This%TemplateTranscript, Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)

    SectionName = 'file'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call This%ModelFile%Construct(Input=InputSection, Prefix=PrefixLoc)

    SectionName = 'formats'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    This%NbMFileInputs = InputSection%GetNumberofSubSections()
    nullify(InputSection)

    allocate(This%MFileInputs(This%NbMFileInputs), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%MFileInputs', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbMFileInputs
      SubSectionName = SectionName // '>format' // ConvertToString(Value=i)
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call MFileInput_Factory%Construct(Object=MFileInput, Input=InputSection, Prefix=PrefixLoc)
      call This%MFileInputs(i)%Set(Object=MFileInput)
      deallocate(MFileInput, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='FileProcessor', ProcName=ProcName, stat=StatLoc)
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput2_FP(This, Input, ConstructPrefix, WritePrefix)

    class(FileProcessor_Type), intent(inout)                          ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    ConstructPrefix
    character(*), intent(in)                                          ::    WritePrefix

    character(*), parameter                                           ::    ProcName='ConstructInput2_FP'
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    Found
    integer                                                           ::    i
    class(MFileInput_Type), allocatable                               ::    MFileInput

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()
    
    SectionName = 'template'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call ImportFile(Strings=This%TemplateTranscript, Input=InputSection, Prefix=ConstructPrefix)
    nullify(InputSection)

    SectionName = 'file'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call This%ModelFile%Construct(Input=InputSection, Prefix=WritePrefix)

    SectionName = 'formats'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    This%NbMFileInputs = InputSection%GetNumberofSubSections()
    nullify(InputSection)

    allocate(This%MFileInputs(This%NbMFileInputs), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%MFileInputs', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbMFileInputs
      SubSectionName = SectionName // '>format' // ConvertToString(Value=i)
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call MFileInput_Factory%Construct(Object=MFileInput, Input=InputSection, Prefix=ConstructPrefix)
      call This%MFileInputs(i)%Set(Object=MFileInput)
      deallocate(MFileInput, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='FileProcessor', ProcName=ProcName, stat=StatLoc)
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_FP(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput_FP

    class(FileProcessor_Type), intent(in)                             ::    This
    character(*), intent(in)                                          ::    Name
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
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    File
    character(:), allocatable                                         ::    FileName

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput_FP%SetName(SectionName = trim(adjustl(Name)))
    if (ExternalFlag) call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

    SectionName = 'file'
    if (ExternalFlag) DirectorySub = DirectoryLoc // '/file'
    call GetInput_FP%AddSection(Section=This%ModelFile%GetInput(Name=SectionName, Prefix=PrefixLoc, Directory=DirectorySub))

    SectionName = 'template'
    call GetInput_FP%AddSection(SectionName=SectionName)
    call GetInput_FP%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    FileName = DirectoryLoc // '/' // SectionName // '.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Input=InputSection, Array=This%TemplateTranscript, File=File)
    nullify(InputSection)

    SectionName = 'formats'
    call GetInput_FP%AddSection(SectionName=SectionName)

    i = 1
    do i = 1, This%NbMFileInputs
      SubSectionName = 'format' // ConvertToString(Value=i)
      MFileInputPtr => This%MFileInputs(i)%GetPointer()
      call GetInput_FP%AddSection(Section=MFileInput_Factory%GetObjectInput(Object=MFileInputPtr,Name=SubSectionName,&
                                                           Prefix=PrefixLoc, Directory=DirectorySub), To_SubSection=SectionName)
      
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInput_FP(This, Input)

    class(FileProcessor_Type), intent(inout)                          ::    This
    type(Input_Type), intent(in)                                      ::    Input

    character(*), parameter                                           ::    ProcName='WriteInput'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii
    class(MFileInput_Type), pointer                                   ::    MFileInputPtr=>null()
    type(SMUQString_Type), allocatable, dimension(:)                  ::    WorkStrings1
    type(SMUQString_Type), allocatable, dimension(:)                  ::    WorkStrings2

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    allocate(WorkStrings1, source=This%TemplateTranscript, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='WorkStrings1', ProcName=ProcName, stat=StatLoc)

    allocate(WorkStrings2, source=This%TemplateTranscript, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='WorkStrings2', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbMFileInputs
      WorkStrings1 = WorkStrings2
      MFileInputPtr => This%MFileInputs(i)%GetPointer()
      call MFileInputPtr%WriteInput(Input=Input, Template=WorkStrings1, ProcessedTemplate=WorkStrings2, File=This%ModelFile)
      nullify(MFileInputPtr)
    end do

    call This%ModelFile%Export(Strings=WorkStrings2)

    deallocate(Workstrings2, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Workstrings2', ProcName=ProcName, stat=StatLoc)

    deallocate(Workstrings1, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Workstrings2', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_FP(LHS, RHS)

    class(FileProcessor_Type), intent(out)                            ::    LHS
    class(FileProcessor_Type), intent(in)                             ::    RHS

    character(*), parameter                                           ::    ProcName='Copy_FP'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    select type (RHS)
  
      type is (FileProcessor_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          LHS%NbMFileInputs = RHS%NbMFileInputs
          allocate(LHS%TemplateTranscript, source=RHS%TemplateTranscript, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%TemplateTranscript', ProcName=ProcName, stat=StatLoc)
          LHS%ModelFile = RHS%ModelFile
          allocate(LHS%MFileInputs, source=RHS%MFileInputs, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%MFileInputContainer', ProcName=ProcName, stat=StatLoc)
        end if

      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer_FP(This)

    type(FileProcessor_Type),intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Finalizer_FP'
    integer                                                           ::    StatLoc=0

    if (allocated(This%MFileInputs)) deallocate(This%MFileInputs, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%MFileInputs', ProcName=ProcName, stat=StatLoc)
    This%NbMFileInputs = 0

    if (allocated(This%TemplateTranscript)) deallocate(This%TemplateTranscript, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%TemplateTranscript', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
