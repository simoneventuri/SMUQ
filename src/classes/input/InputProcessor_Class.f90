! -*-f90-*-
!!----------------------------------------------------------------------------------------------------------------------------------
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
!!----------------------------------------------------------------------------------------------------------------------------------

module InputProcessor_Class

use StringRoutines_Module
use String_Library
use Parameters_Library
use Input_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type

implicit none

private

public                                                                ::    InputProcessor_Type

type                                                                  ::    InputTransform_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Constructed=.false.
  logical                                                             ::    Initialized=.false.
  type(String_Type), allocatable, dimension(:)                        ::    Transformation
  type(String_Type), allocatable, dimension(:)                        ::    Target
  integer                                                             ::    NbTargets
contains
  procedure, public                                                   ::    Initialize              =>    Initialize_IT
  procedure, public                                                   ::    Reset                   =>    Reset_IT
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults_IT
  generic, public                                                     ::    Construct               =>    ConstructInput_IT
  procedure, private                                                  ::    ConstructInput_IT    
  procedure, public                                                   ::    GetInput                =>    GetInput_IT
  procedure, public                                                   ::    TransformInput          =>    TransformInput_IT
  procedure, public                                                   ::    GetTargets              =>    GetTargets_IT
  generic, public                                                     ::    assignment(=)           =>    Copy_IT
  procedure, public                                                   ::    Copy_IT
  final                                                               ::    Finalizer_IT
end type 

type                                                                  ::    InputFixed_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Constructed=.false.
  logical                                                             ::    Initialized=.false.
  real(rkp), allocatable, dimension(:)                                ::    Value
  type(String_Type), allocatable, dimension(:)                        ::    Target
  integer                                                             ::    NbTargets
contains
  procedure, public                                                   ::    Initialize              =>    Initialize_IF
  procedure, public                                                   ::    Reset                   =>    Reset_IF
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults_IF
  generic, public                                                     ::    Construct               =>    ConstructInput_IF
  procedure, private                                                  ::    ConstructInput_IF    
  procedure, public                                                   ::    GetInput                =>    GetInput_IF
  procedure, public                                                   ::    AddFixedInput           =>    AddFixedInput_IF
  procedure, public                                                   ::    GetTargets              =>    GetTargets_IF
  generic, public                                                     ::    assignment(=)           =>    Copy_IF
  procedure, public                                                   ::    Copy_IT
  final                                                               ::    Finalizer_IT
end type 

type                                                                  ::    InputOperation_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Constructed=.false.
  logical                                                             ::    Initialized=.false.
  character(:), allocatable                                           ::    Operation 
  type(String_Type), allocatable, dimension(:)                        ::    Target
  integer                                                             ::    NbTargets
contains
  procedure, public                                                   ::    Initialize              =>    Initialize_IO
  procedure, public                                                   ::    Reset                   =>    Reset_IO
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults_IO
  generic, public                                                     ::    Construct               =>    ConstructInput_IO
  procedure, private                                                  ::    ConstructInput_IO
  procedure, public                                                   ::    GetInput                =>    GetInput_IO
  procedure, public                                                   ::    OperateInput            =>    OperateInput_IO
  procedure, private                                                  ::    OpNormalize             =>    OpNormalize_IO
  procedure, public                                                   ::    GetTargets              =>    GetTargets_IO
  generic, public                                                     ::    assignment(=)           =>    Copy_IO
  procedure, public                                                   ::    Copy_IO
  final                                                               ::    Finalizer_IO
end type

type                                                                  ::    InputProcessor_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Constructed=.false.
  logical                                                             ::    Initialized=.false.
  integer                                                             ::    NbFix
  integer                                                             ::    NbOperations
  integer                                                             ::    NbTransforms
  type(InputOperation_Type), allocatable, dimension(:)                ::    Operation
  type(InputFixed_Type), allocatable, dimension(:)                    ::    Fix
  type(InputTransform_Type), allocatable, dimension(:)                ::    Transform
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  generic, public                                                     ::    ProcessInput            =>    ProcessInput_0D,        &
                                                                                                          ProcessInput_1D
  procedure, private                                                  ::    ProcessInput_0D
  procedure, private                                                  ::    ProcessInput_1D
  procedure, public                                                   ::    IsConstructed
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize(This)

  class(InputProcessor_Type), intent(inout)                           ::    This

  character(*), parameter                                             ::    ProcName='Initialize'
  integer                                                             ::    StatLoc=0

  if (.not. This%Initialized) then
    This%Initialized = .true.
    This%Name = 'InputProcessor'
    call This%SetDefaults()
  end if

  end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(InputProcessor_Type), intent(inout)                           ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Initialized=.false.
  This%Constructed=.false.

  if (allocated(This%Operation)) deallocate(This%Operation, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Operation', ProcName=ProcName, stat=StatLoc)
  This%NbOperations = 0

  if (allocated(This%Fix)) deallocate(This%Fix, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Fix', ProcName=ProcName, stat=StatLoc)
  This%NbFixs = 0

  if (allocated(This%Transform)) deallocate(This%Transform, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Transform', ProcName=ProcName, stat=StatLoc)
  This%NbTransforms = 0

  call This%Initialize()

  end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults(This)

  class(InputProcessor_Type),intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults'

  end subroutine
!!-------------------------------------------------------------------------------------------------------------------------------- 

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(InputProcessor_Type), intent(inout)                           ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    PrefixLoc
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  logical                                                             ::    Found
  integer                                                             ::    i
  integer                                                             ::    ii
  integer                                                             ::    iii
  integer                                                             ::    iv
  character(:), allocatable                                           ::    VarC0D
  real(rkp)                                                           ::    VarR0D
  type(String_Type), allocatable, dimension(:)                        ::    Targets1
  type(String_Type), allocatable, dimension(:)                        ::    Targets2


  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()
  
  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  This%NbFixs = 0
  SectionName = 'fixed_values'
  if (Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    This%NbFixs = InputSection%GetNumberOfSubSections()
    nullify(InputSection)

    allocate(This%Fix(This%NbFixs), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Fix', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbFixs
      SubSectionName = SectionName // '>fixed_value' // ConvertToString(Value=i)
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%Fix(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
    end do

    i = 1
    do i = 1, This%NbFixs
      Targets1 = This%Fix(i)%GetTargets()
      ii = 1
      do ii = i + 1 , This%NbFixs
        Targets2 = This%Fix(ii)%GetTargets()
        do iii = 1, size(Targets1,1)
          VarC0D = Targets1%GetValue()
          do iv = 1, size(Targets2,1)
            if (VarC0D == Targets2%GetValue()) call Error%Raise('Duplicate fixed value labels', ProcName=ProcName)
          end do
        end do
      end do
    end do
  end if

  This%NbTransforms = 0
  SectionName = 'transformations'
  if (Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    This%NbTransforms = InputSection%GetNumberOfSubSections()

    allocate(This%Transform(This%NbTransforms), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Transform', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbTransforms
      SubSectionName = SectionName // '>transformation' // ConvertToString(Value=i)
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%Transform(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
    end do

    i = 1
    do i = 1, This%NbTransforms
      Targets1 = This%Transform(i)%GetTargets()
      ii = 1
      do ii = i + 1 , This%Transforms
        Targets2 = This%Transform(ii)%GetTargets()
        do iii = 1, size(Targets1,1)
          VarC0D = Targets1%GetValue()
          do iv = 1, size(Targets2,1)
            if (VarC0D == Targets2%GetValue()) call Error%Raise('Duplicate fixed value labels : ' // VarC0D, ProcName=ProcName)
          end do
        end do
      end do
    end do

    if (This%NbFixs > 0) then
      i = 1
      do i = 1, This%NbTransforms
        Targets1 = This%Transform(i)%GetTargets()
        ii = 1
        do ii = i + 1 , This%NbFixs
          Targets2 = This%Fix(ii)%GetTargets()
          do iii = 1, size(Targets1,1)
            VarC0D = Targets1%GetValue()
            do iv = 1, size(Targets2,1)
              if (VarC0D == Targets2%GetValue()) call Error%Raise('Transformation of a fixed parameter not allowed : '          &
                                                                    // VarC0D, ProcName=ProcName)
            end do
          end do
        end do
      end do
    end if
  end if

  This%NbOperations = 0
  SectionName = 'operations'
  if (Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    This%NbOperations = InputSection%GetNumberOfSubSections()

    allocate(This%Operation(This%NbOperations), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Transform', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbOperations
      SubSectionName = SectionName // '>transformation' // ConvertToString(Value=i)
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%Operation(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
    end do

    i = 1
    do i = 1, This%NbOperations
      Targets1 = This%Operation(i)%GetTargets()
      ii = 1
      do ii = i + 1 , This%Operations
        Targets2 = This%Operation(ii)%GetTargets()
        do iii = 1, size(Targets1,1)
          VarC0D = Targets1%GetValue()
          do iv = 1, size(Targets2,1)
            if (VarC0D == Targets2%GetValue()) call Error%Raise('Duplicate fixed value labels', ProcName=ProcName)
          end do
        end do
      end do
    end do
  end if

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(InputProcessor_Type), intent(in)                              ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  if (This%NbFixs > 0) then
    SectionName = 'fixed_values'
    call GetInput%AddSection(SectionName=SectionName)

    i = 1
    do i = 1, This%NbFixs
      SubSectionName ='fixed_value' // ConvertToString(i)
      call GetInput%AddSection(Section=This%Fix(i)%GetInput(Name=SubSectionName, Prefix=PrefixLoc, Directory=DirectoryLoc),       &
                               To_SubSection=SectionName)
    end do

  end if

  if (This%NbTransforms > 0) then
    SectionName = 'transformations'
    call GetInput%AddSection(SectionName=SectionName)

    i = 1
    do i = 1, This%NbTransforms
      SubSectionName ='transformation' // ConvertToString(i)
      call GetInput%AddSection(Section=This%Transform(i)%GetInput(Name=SubSectionName, Prefix=PrefixLoc, Directory=DirectoryLoc), &
                               To_SubSection=SectionName)
    end do

  end if

  if (This%NbOperations > 0) then
    SectionName = 'operations'
    call GetInput%AddSection(SectionName=SectionName)

    i = 1
    do i = 1, This%NbOperations
      SubSectionName ='operation' // ConvertToString(i)
      call GetInput%AddSection(Section=This%Operation(i)%GetInput(Name=SubSectionName, Prefix=PrefixLoc, Directory=DirectoryLoc), &
                               To_SubSection=SectionName)
    end do

  end if

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ProcessInput_0D(This, Input, ProcessedInput)

  class(InputProcessor_Type), intent(in)                              ::    This
  type(Input_Type), intent(in)                                        ::    Input
  type(Input_Type), intent(out)                                       ::    ProcessedInput

  character(*), parameter                                             ::    ProcName='ProcessInput_0D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  real(rkp)                                                           ::    VarR0D

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  ProcessedInput = Input

  i = 1
  do i = 1, ThisNbFixs
    call This%Fix(i)%AddFixedInput(Input=ProcessedInput)
  end do

  i = 1
  do i = 1, This%NbTransforms
    call This%Transform(i)%TransformInput(Input=ProcessedInput)
  end do

  i = 1
  do i = 1, This%NbOperations
    call This%Operation(i)%OperateInput(Input=ProcessedInput)
  end do

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ProcessInput_1D(This, Input, ProcessedInput)

  class(InputProcessor_Type), intent(in)                              ::    This
  type(Input_Type), dimension(:), intent(in)                          ::    Input
  type(Input_Type), allocatable, dimension(:), intent(out)            ::    ProcessedInput

  character(*), parameter                                             ::    ProcName='ProcessInput_1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbInputs

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  NbInputs = size(Input,1)

  allocate(ProcessedInput(NbInputs), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='ProcessedInput', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, NbInputs
    call This%ProcessInput(Input=Input(i), ProcessedInput=ProcessedInput(i))
  end do

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function IsConstructed(This)

  logical                                                             ::    IsConstructed

  class(InputProcessor_Type), intent(in)                              ::    This

  character(*), parameter                                             ::    ProcName='IsConstructed'

  IsConstructed = This%Constructed

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(InputProcessor_Type), intent(out)                             ::    LHS
  class(InputProcessor_Type), intent(in)                              ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  call LHS%Reset()
  LHS%Initialized = RHS%Initialized
  LHS%Constructed = RHS%Constructed

  if (RHS%Constructed) then
    LHS%NbFixs = RHS%NbFixs
    LHS%NbTransforms = RHS%NbTransforms
    LHS%NbOperations = RHS%NbOperations
    allocate(LHS%Fix, source=RHS%Fix, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Fix', ProcName=ProcName, stat=StatLoc)
    allocate(LHS%Transform, source=RHS%Transform, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Transform', ProcName=ProcName, stat=StatLoc)
    allocate(LHS%Operation, source=RHS%Operation, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Operation', ProcName=ProcName, stat=StatLoc)
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(InputProcessor_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  deallocate(This%Fix, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Fix', ProcName=ProcName, stat=StatLoc)

  deallocate(This%Transform, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Transform', ProcName=ProcName, stat=StatLoc)

  deallocate(This%Operation, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Operation', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
!! InputFixed_Type ---------------------------------------------------------------------------------------------------------------
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize_IF(This)

  class(InputFixed_Type), intent(inout)                               ::    This

  character(*), parameter                                             ::    ProcName='Initialize_IF'
  integer                                                             ::    StatLoc=0

  if (.not. This%Initialized) then
    This%Initialized = .true.
    This%Name = 'InputFixed'
    call This%SetDefaults()
  end if

  end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset_IF(This)

  class(InputFixed_Type), intent(inout)                               ::    This

  character(*), parameter                                             ::    ProcName='Reset_IF'
  integer                                                             ::    StatLoc=0

  This%Initialized=.false.
  This%Constructed=.false.

  if (allocated(This%Targets)) deallocate(This%Targets, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Targets', ProcName=ProcName, stat=StatLoc)
  This%NbTargets = 0

  call This%Initialize()

  end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults_IF(This)

  class(InputFixed_Type),intent(inout)                                ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults_IF'

  This%Value = Zero

  end subroutine
!!-------------------------------------------------------------------------------------------------------------------------------- 

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput_IF(This, Input, Prefix)

  class(InputFixed_Type), intent(inout)                               ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput_IF'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    PrefixLoc
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  logical                                                             ::    Found
  character(:), allocatable                                           ::    VarC0D
  real(rkp)                                                           ::    VarR0D


  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()
  
  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  This%NbTargets = 0
  call Input%GetValue(Value=VarC0D, ParameterName='label', Mandatory=.true.)
  This%Target = ConvertToStrings(Value=VarC0D, Separator=' ')
  This%NbTargets = size(This%Target,1)

  allocate(This%Value(This%NbTargets), stat=StatLoc)
  if ( StatLoc /= 0 ) call Error%Allocate(Name='This%Value', ProcName=ProcName, stat=StatLoc)
  call Input%GetValue(Value=VarR0D, ParameterName='value', Mandatory=.true.)
  This%Value = VarR0D

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput_IF(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput_IF

  class(InputFixed_Type), intent(in)                                  ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput_IF'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput_IF%SetName(SectionName=trim(adjustl(Name)))

  call GetInput_IF%AddParameter(Name='label', Value=ConvertToString(Values=This%Target))
  call GetInput_IF%AddParameter(Name='value', Value=ConvertToString(Value=This%Value(1)))

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine AddFixedInput(This, Input)

  type(InputFixed_Type), intent(in)                                   ::    This
  type(Input_Type), intent(inout)                                     ::    Input

  character(*), parameter                                             ::    ProcName='Copy_IF'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  call Input%Append(Values=This%Value, Labels=This%Target)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy_IF(LHS, RHS)

  class(InputFixed_Type), intent(out)                                 ::    LHS
  class(InputFixed_Type), intent(in)                                  ::    RHS

  character(*), parameter                                             ::    ProcName='Copy_IF'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  call LHS%Reset()
  LHS%Initialized = RHS%Initialized
  LHS%Constructed = RHS%Constructed

  if (RHS%Constructed) then
    LHS%NbTargets = RHS%NbTargets
    allocate(LHS%Target, source=RHS%Target, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Target', ProcName=ProcName, stat=StatLoc)
    allocate(LHS%Value, source=RHS%Value, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate(Name='LHS%Value', ProcName=ProcName, stat=StatLoc)
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer_IF(This)

  type(InputFixed_Type), intent(inout)                                ::    This

  character(*), parameter                                             ::    ProcName='Finalizer_IF'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Target)) deallocate(This%Target, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Target', ProcName=ProcName, stat=StatLoc)

  if ( allocated(This%Value) ) deallocate(This%Value, stat=StatLoc)
  if ( StatLoc /= 0 ) call Error%Deallocate(Name='This%Value', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
!! InputTransform_Type -----------------------------------------------------------------------------------------------------------
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize_IT(This)

  class(InputTransform_Type), intent(inout)                           ::    This

  character(*), parameter                                             ::    ProcName='Initialize_IT'
  integer                                                             ::    StatLoc=0

  if (.not. This%Initialized) then
    This%Initialized = .true.
    This%Name = 'InputTransform'
    call This%SetDefaults()
  end if

  end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset_IT(This)

  class(InputTransform_Type), intent(inout)                           ::    This

  character(*), parameter                                             ::    ProcName='Reset_IT'
  integer                                                             ::    StatLoc=0

  This%Initialized=.false.
  This%Constructed=.false.

  if (allocated(This%Targets)) deallocate(This%Targets, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Targets', ProcName=ProcName, stat=StatLoc)
  This%NbTargets = 0

  if ( allocated(This%Transformation) ) deallocate(This%Transformation, stat=StatLoc)
  if ( StatLoc /= 0 ) call Error%Deallocate(Name='This%Transformation', ProcName=ProcName, stat=StatLoc)

  call This%Initialize()

  end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults_IT(This)

  class(InputTransform_Type),intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults_IT'

  end subroutine
!!-------------------------------------------------------------------------------------------------------------------------------- 

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput_IT(This, Input, Prefix)

  class(InputTransform_Type), intent(inout)                           ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput_IT'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    PrefixLoc
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  logical                                                             ::    Found
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    i


  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()
  
  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  This%NbTargets = 0
  call Input%GetValue(Value=VarC0D, ParameterName='label', Mandatory=.true.)
  This%Target = ConvertToStrings(Value=VarC0D, Separator=' ')
  This%NbTargets = size(This%Target,1)

  allocate(This%Transformation(This%NbTargets), stat=StatLoc)
  if ( StatLoc /= 0 ) call Error%Allocate(Name='This%Transformation', ProcName=ProcName, stat=StatLoc)
  call Input%GetValue(Value=VarC0D, ParameterName='transformation', Mandatory=.true.)
  i = 1
  do i = 1, This%NbTargets
    This%Transformation(i) = VarC0D
  end do

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput_IT(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput_IT

  class(InputProcessor_Type), intent(in)                              ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput_IT'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput_IT%SetName(SectionName=trim(adjustl(Name)))

  call GetInput_IT%AddParameter(Name='label', Value=ConvertToString(Values=This%Target))
  call GetInput_IT%AddParameter(Name='transformation', Value=This%Transformation(1)%GetValue())

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine TransformInput(This, Input)

  type(InputTransform_Type), intent(in)                               ::    This
  type(Input_Type), intent(inout)                                     ::    Input

  character(*), parameter                                             ::    ProcName='TransformInput_IT'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  call Input%Transform(Transformations=This%Transformation, Labels=This%Target)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy_IT(LHS, RHS)

  class(InputTransform_Type), intent(out)                             ::    LHS
  class(InputTransform_Type), intent(in)                              ::    RHS

  character(*), parameter                                             ::    ProcName='Copy_IT'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  call LHS%Reset()
  LHS%Initialized = RHS%Initialized
  LHS%Constructed = RHS%Constructed

  if (RHS%Constructed) then
    LHS%NbTargets = RHS%NbTargets
    allocate(LHS%Target, source=RHS%Target, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Target', ProcName=ProcName, stat=StatLoc)
    allocate(LHS%Transformation, source=RHS%Transformation, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate(Name='LHS%Transformation', ProcName=ProcName, stat=StatLoc)
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer_IT(This)

  type(InputTransform_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='Finalizer_IT'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Target)) deallocate(This%Target, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Target', ProcName=ProcName, stat=StatLoc)

  if ( allocated(This%Transformations) ) deallocate(This%Transformations, stat=StatLoc)
  if ( StatLoc /= 0 ) call Error%Deallocate(Name='This%Transformations', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
!! InputOperation_Type -----------------------------------------------------------------------------------------------------------
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize_IO(This)

  class(InputOperation_Type), intent(inout)                           ::    This

  character(*), parameter                                             ::    ProcName='Initialize_IO'
  integer                                                             ::    StatLoc=0

  if (.not. This%Initialized) then
    This%Initialized = .true.
    This%Name = 'InputOperation'
    call This%SetDefaults()
  end if

  end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset_IO(This)

  class(InputOperation_Type), intent(inout)                           ::    This

  character(*), parameter                                             ::    ProcName='Reset_IO'
  integer                                                             ::    StatLoc=0

  This%Initialized=.false.
  This%Constructed=.false.

  if (allocated(This%Targets)) deallocate(This%Targets, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Targets', ProcName=ProcName, stat=StatLoc)
  This%NbTargets = 0

  if (allocated(This%Operation)) deallocate(This%Operation, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Operation', ProcName=ProcName, stat=StatLoc)

  call This%Initialize()

  end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults_IO(This)

  class(InputOperation_Type),intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults_IO'

  end subroutine
!!-------------------------------------------------------------------------------------------------------------------------------- 

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput_IO(This, Input, Prefix)

  class(InputOperation_Type), intent(inout)                           ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput_IO'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    PrefixLoc
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  logical                                                             ::    Found
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    i


  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()
  
  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  This%NbTargets = 0
  call Input%GetValue(Value=VarC0D, ParameterName='label', Mandatory=.true.)
  This%Target = ConvertToStrings(Value=VarC0D, Separator=' ')
  This%NbTargets = size(This%Target,1)

  call Input%GetValue(Value=VarC0D, ParameterName='operation', Mandatory=.true.)
  This%Operation = VarC0D

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput_IO(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput_IO

  class(InputProcessor_Type), intent(in)                              ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput_IO'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput_IO%SetName(SectionName=trim(adjustl(Name)))

  call GetInput_IO%AddParameter(Name='label', Value=ConvertToString(Values=This%Target))
  call GetInput_IO%AddParameter(Name='operation', Value=This%Operation)

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine OperateInput_IT(This, Input)

  type(InputTransform_Type), intent(in)                               ::    This
  type(Input_Type), intent(inout)                                     ::    Input

  character(*), parameter                                             ::    ProcName='OperateInput_IT'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  select case (This%Operation)
    case('normalize')
      call This%OpNormalize(Input=Input)
    case default
      call Error%Raise('Unrecognized operation', ProcName=ProcName)
  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine OpNormalize_IT(This, Input)

  type(InputTransform_Type), intent(in)                               ::    This
  type(Input_Type), intent(inout)                                     ::    Input

  character(*), parameter                                             ::    ProcName='OpNormalize_IT'
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:)                                ::    Values

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  call Input%GetValue(Labels=This%Target, Values=Values, Mandatory=.true.)

  Values = Values / sum(Values,1)

  call Input%Replace(Values=Values, Labels=This%Target)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy_IO(LHS, RHS)

  class(InputOperation_Type), intent(out)                             ::    LHS
  class(InputOperation_Type), intent(in)                              ::    RHS

  character(*), parameter                                             ::    ProcName='Copy_IO'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  call LHS%Reset()
  LHS%Initialized = RHS%Initialized
  LHS%Constructed = RHS%Constructed

  if (RHS%Constructed) then
    LHS%NbTargets = RHS%NbTargets
    LHS%Operation = RHS%Operation
    allocate(LHS%Target, source=RHS%Target, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Target', ProcName=ProcName, stat=StatLoc)
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer_IO(This)

  type(InputOperation_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='Finalizer_IO'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Target)) deallocate(This%Target, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Target', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module