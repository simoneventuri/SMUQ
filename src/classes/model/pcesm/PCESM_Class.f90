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

module PCESM_Class

use Input_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use String_Library
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Model_Class                                                   ,only:    Model_Type
use ModelInternal_Class                                           ,only:    ModelInternal_Type
use Output_Class                                                  ,only:    Output_Type
use PolyChaosModel_Class                                          ,only:    PolyChaosModel_Type
use Input_Class                                                   ,only:    Input_Type
use List1DAllocChar_Class                                         ,only:    List1DAllocChar_Type
use InputProcessor_Class                                          ,only:    InputProcessor_Type

implicit none

private

public                                                                ::    PCESM_Type

type, extends(ModelInternal_Type)                                     ::    PCESM_Type
  type(PolyChaosModel_Type), allocatable, dimension(:)                ::    PCEModels
  integer                                                             ::    NbModels=0
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run_0D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(PCESM_Type), intent(inout)                                  ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'pcesmmodel'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(PCESM_Type), intent(inout)                                  ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    if (allocated(This%PCEModels)) deallocate(This%PCEModels, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%PCEModels', ProcName=ProcName, stat=StatLoc)

    This%NbModels = 0

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(PCESM_Type), intent(inout)                                  ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%Label = 'pcesm'
    This%NbOutputs = 0
    This%Silent = .false.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    use String_Library

    class(PCESM_Type), intent(inout)                                  ::    This
    class(InputSection_Type), intent(in)                              ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    VarI0D
    integer                                                           ::    i, ii
    integer, allocatable, dimension(:)                                ::    OutputMap
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    real(rkp)                                                         ::    VarR0D
    logical                                                           ::    VarL0D
    character(:), allocatable, dimension(:)                           ::    LabelMap
    logical                                                           ::    Found

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'label'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
    This%Label = VarC0D

    ParameterName = 'silent'
    call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%Silent = VarL0D

    SectionName = 'models'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    This%NbModels = InputSection%GetNumberOfSubSections()
    allocate(This%PCEModels(This%NbModels), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Cell', ProcName=ProcName, stat=StatLoc)

    This%NbOutputs = This%NbModels

    Found = .false.

    i = 1
    do i = 1, This%NbModels
      SubSectionName = SectionName // '>model' // ConvertToString(Value=i)
      ParameterName = 'directory'
      call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found, SectionName=SubSectionName)
      if (Found) then
        call This%PCEModels(i)%Construct(Prefix=PrefixLoc // VarC0D)

        ParameterName = 'output_label'
        call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true.)
        call This%PCEModels(i)%ReplaceOutputLabel(NewLabel=VarC0D)

        SubSectionName = SectionName // '>model' // ConvertToString(Value=i) // '>input_label_map'
        if (Input%HasSection(SubSectionName=SubSectionName)) then
          ii = 1
          do ii = 1, Input%GetNumberofParameters(FromSubSection=SubSectionName)
            ParameterName = 'map' // ConvertToString(Value=ii)
            call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true.)
            call Parse(Input=VarC0D, Separator=' ', Output=LabelMap)
            if (size(LabelMap,1) /= 2) call Error%Raise(Line='Incorrect input label map format', ProcName=ProcName)
            call This%PCEModels(i)%ReplaceInputLabel(OldLabel=trim(adjustl(LabelMap(1))),  NewLabel=trim(adjustl(LabelMap(2))))
          end do
        end if
      else
        SubSectionName = SubSectionName // '>pce_input'
        call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
        call This%PCEModels(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
        nullify(InputSection)
      end if
    end do

    SectionName = 'input_preprocessor'
    if (Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%InputProcessor%Construct(Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput

    class(PCESM_Type), intent(in)                                     ::    This
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
    character(:), allocatable                                         ::    ParameterName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    integer                                                           ::    i
    character(:), allocatable                                         ::    VarC0D
    real(rkp)                                                         ::    VarR0D

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    DirectoryLoc = '<undefined>'
    PrefixLoc = ''
    DirectorySub = DirectoryLoc
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix

    if (DirectoryLoc /= '<undefined>') ExternalFlag = .true.

    if (ExternalFlag) call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput%AddParameter(Name='label', Value=This%Label)

    SectionName = 'models'
    call GetInput%AddSection(SectionName=SectionName)

    i = 1
    do i = 1, This%NbModels
      SubSectionName = 'model' // ConvertToString(Value=i)
      call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
      SubSectionName = SectionName // '>' // SubSectionName
      if (ExternalFlag) DirectorySub = DirectoryLoc // '/model' // ConvertToString(Value=i)
      call GetInput%AddSection(Section=This%PCEModels(i)%GetInput(Name='pce_input', Prefix=PrefixLoc,                 &
                                                                          Directory=DirectorySub), To_SubSection=SubSectionName)
    end do

    if (This%InputProcessor%IsConstructed()) call GetInput%AddSection(Section=This%InputProcessor%GetInput(                  &
                                                 Name='input_preprocessor', Prefix=PrefixLoc, Directory=DirectorySub))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_0D(This, Input, Output, Stat)

    class(PCESM_Type), intent(inout)                                  ::    This
    type(Input_Type), intent(in)                                      ::    Input
    type(Output_Type), dimension(:), intent(inout)                    ::    Output
    integer, optional, intent(out)                                    ::    Stat

    character(*), parameter                                           ::    ProcName='Run_0D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    if (size(Output,1) /= This%NbOutputs) call Error%Raise('Passed down an output array of incorrect length',                  &
                                                                                                               ProcName=ProcName)

    i = 1
    do i = 1, This%NbModels
      call This%PCEModels(i)%Run(Input=Input, Output=Output(i))
    end do

    if (present(Stat)) Stat = 0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(PCESM_Type), intent(out)                                    ::    LHS
    class(Model_Type), intent(in)                                     ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    select type (RHS)
  
      type is (PCESM_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          LHS%NbOutputs = RHS%NbOutputs
          LHS%Label = RHS%Label
          LHS%NbModels = RHS%NbModels
          allocate(LHS%PCEModels, source=RHS%PCEModels, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%PCEModels', ProcName=ProcName, stat=StatLoc)
          LHS%InputProcessor = RHS%InputProcessor
        end if

      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(PCESM_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0
  
    if (allocated(This%PCEModels)) deallocate(This%PCEModels, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%PCEModels', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
