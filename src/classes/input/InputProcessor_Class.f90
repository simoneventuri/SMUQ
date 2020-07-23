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

use StringConversion_Module
use Parameters_Library
use Input_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use SMUQString_Class                                              ,only:    SMUQString_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    InputProcessor_Type

type                                                                  ::    InputTransform_Type
  logical                                                             ::    Constructed=.false.
  type(SMUQString_Type), allocatable, dimension(:)                    ::    Transformation
  type(SMUQString_Type), allocatable, dimension(:)                    ::    Target
  integer                                                             ::    NbTargets
contains
  procedure, public                                                   ::    Reset                   =>    Reset_IT
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
  logical                                                             ::    Constructed=.false.
  real(rkp), allocatable, dimension(:)                                ::    Value
  type(SMUQString_Type), allocatable, dimension(:)                    ::    Target
  integer                                                             ::    NbTargets
contains
  procedure, public                                                   ::    Reset                   =>    Reset_IF
  generic, public                                                     ::    Construct               =>    ConstructInput_IF
  procedure, private                                                  ::    ConstructInput_IF    
  procedure, public                                                   ::    GetInput                =>    GetInput_IF
  procedure, public                                                   ::    AddFixedInput           =>    AddFixedInput_IF
  procedure, public                                                   ::    GetTargets              =>    GetTargets_IF
  generic, public                                                     ::    assignment(=)           =>    Copy_IF
  procedure, public                                                   ::    Copy_IF
  final                                                               ::    Finalizer_IF
end type

type                                                                  ::    InputProcessor_Type
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbFixs
  integer                                                             ::    NbTransforms
  type(InputFixed_Type), allocatable, dimension(:)                    ::    Fix
  type(InputTransform_Type), allocatable, dimension(:)                ::    Transform
contains
  procedure, public                                                   ::    Reset
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
subroutine Reset(This)

  class(InputProcessor_Type), intent(inout)                           ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed=.false.

  if (allocated(This%Fix)) deallocate(This%Fix, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Fix', ProcName=ProcName, stat=StatLoc)
  This%NbFixs = 0

  if (allocated(This%Transform)) deallocate(This%Transform, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Transform', ProcName=ProcName, stat=StatLoc)
  This%NbTransforms = 0

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
  type(SMUQString_Type), allocatable, dimension(:)                    ::    Targets1
  type(SMUQString_Type), allocatable, dimension(:)                    ::    Targets2
  type(InputVerifier_Type)                                            ::    InputVerifier 

  call This%Reset()
  
  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  This%NbFixs = 0
  SectionName = 'fixed_values'
  call InputVerifier%AddSection(Section=SectionName)
  if (Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    This%NbFixs = InputSection%GetNumberOfSubSections()
    nullify(InputSection)

    allocate(This%Fix(This%NbFixs), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Fix', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbFixs
      call InputVerifier%AddSection(Section='fixed_value' // ConvertToString(Value=i), ToSubSection=SectionName)
      SubSectionName = SectionName // '>fixed_value' // ConvertToString(Value=i)
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%Fix(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
    end do

    i = 1
    do i = 1, This%NbFixs
      call This%Fix(i)%GetTargets(Targets=Targets1)
      ii = 1
      do ii = i + 1 , This%NbFixs
        call This%Fix(ii)%GetTargets(Targets=Targets2)
        do iii = 1, size(Targets1,1)
          do iv = 1, size(Targets2,1)
            if (Targets1(iii) == Targets2(iv)) call Error%Raise('Duplicate fixed value labels', ProcName=ProcName)
          end do
        end do
      end do
    end do
  end if

  This%NbTransforms = 0
  SectionName = 'transformations'
  call InputVerifier%AddSection(Section=SectionName)
  if (Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    This%NbTransforms = InputSection%GetNumberOfSubSections()

    allocate(This%Transform(This%NbTransforms), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Transform', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbTransforms
      SubSectionName = SectionName // '>transformation' // ConvertToString(Value=i)
      call InputVerifier%AddSection(Section='transformation' // ConvertToString(Value=i), ToSubSection=SectionName)
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%Transform(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
    end do

    i = 1
    do i = 1, This%NbTransforms
      call This%Transform(i)%GetTargets(Targets=Targets1)
      ii = 1
      do ii = i + 1 , This%NbTransforms
        call This%Transform(ii)%GetTargets(Targets=Targets2)
        do iii = 1, size(Targets1,1)
          do iv = 1, size(Targets2,1)
            if (Targets1(iii) == Targets2(iv)) call Error%Raise('Duplicate fixed value labels', ProcName=ProcName)
          end do
        end do
      end do
    end do

    if (This%NbFixs > 0) then
      i = 1
      do i = 1, This%NbTransforms
        call This%Transform(i)%GetTargets(Targets=Targets1)
        ii = 1
        do ii = i + 1 , This%NbFixs
          call This%Fix(ii)%GetTargets(Targets=Targets2)
          do iii = 1, size(Targets1,1)
            do iv = 1, size(Targets2,1)
              if (Targets1(iii) == Targets2(iv)) call Error%Raise('Transformation of a fixed parameter not allowed : ', &
                                                                  ProcName=ProcName)
            end do
          end do
        end do
      end do
    end if
  end if

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

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
  do i = 1, This%NbFixs
    call This%Fix(i)%AddFixedInput(Input=ProcessedInput)
  end do

  i = 1
  do i = 1, This%NbTransforms
    call This%Transform(i)%TransformInput(Input=ProcessedInput)
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
  LHS%Constructed = RHS%Constructed

  if (RHS%Constructed) then
    LHS%NbFixs = RHS%NbFixs
    LHS%NbTransforms = RHS%NbTransforms
    allocate(LHS%Fix, source=RHS%Fix, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Fix', ProcName=ProcName, stat=StatLoc)
    allocate(LHS%Transform, source=RHS%Transform, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Transform', ProcName=ProcName, stat=StatLoc)
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(InputProcessor_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Fix)) deallocate(This%Fix, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Fix', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Transform)) deallocate(This%Transform, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Transform', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
!! InputFixed_Type ---------------------------------------------------------------------------------------------------------------
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset_IF(This)

  class(InputFixed_Type), intent(inout)                               ::    This

  character(*), parameter                                             ::    ProcName='Reset_IF'
  integer                                                             ::    StatLoc=0

  This%Constructed=.false.

  if (allocated(This%Target)) deallocate(This%Target, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Target', ProcName=ProcName, stat=StatLoc)
  This%NbTargets = 0

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
  type(InputVerifier_Type)                                            ::    InputVerifier 

  call This%Reset()
  
  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  This%NbTargets = 0
  call InputVerifier%AddParameter(Parameter='label')
  call Input%GetValue(Value=VarC0D, ParameterName='label', Mandatory=.true.)
  call ConvertToStrings(Value=VarC0D, Strings=This%Target, Separator=' ')
  This%NbTargets = size(This%Target,1)

  allocate(This%Value(This%NbTargets), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Value', ProcName=ProcName, stat=StatLoc)
  call InputVerifier%AddParameter(Parameter='value')
  call Input%GetValue(Value=VarR0D, ParameterName='value', Mandatory=.true.)
  This%Value = VarR0D

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

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
subroutine AddFixedInput_IF(This, Input)

  class(InputFixed_Type), intent(in)                                  ::    This
  type(Input_Type), intent(inout)                                     ::    Input

  character(*), parameter                                             ::    ProcName='AddFixedInput_IF'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  call Input%Append(Values=This%Value, Labels=This%Target)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine GetTargets_IF(This, Targets)

  class(InputFixed_Type), intent(in)                                  ::    This
  type(SMUQString_Type), allocatable, dimension(:), intent(inout)     ::    Targets

  character(*), parameter                                             ::    ProcName='GetTargets_IF'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  if (allocated(Targets)) then
    if (size(Targets,1) /= This%NbTargets) then
      deallocate(Targets, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Targets', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Targets)) then
    allocate(Targets(This%NbTargets), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Targets', ProcName=ProcName, stat=StatLoc)
  end if

  Targets = This%Target

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
  LHS%Constructed = RHS%Constructed

  if (RHS%Constructed) then
    LHS%NbTargets = RHS%NbTargets
    allocate(LHS%Target, source=RHS%Target, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Target', ProcName=ProcName, stat=StatLoc)
    allocate(LHS%Value, source=RHS%Value, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Value', ProcName=ProcName, stat=StatLoc)
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

  if (allocated(This%Value)) deallocate(This%Value, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Value', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
!! InputTransform_Type -----------------------------------------------------------------------------------------------------------
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset_IT(This)

  class(InputTransform_Type), intent(inout)                           ::    This

  character(*), parameter                                             ::    ProcName='Reset_IT'
  integer                                                             ::    StatLoc=0

  This%Constructed=.false.

  if (allocated(This%Target)) deallocate(This%Target, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Target', ProcName=ProcName, stat=StatLoc)
  This%NbTargets = 0

  if (allocated(This%Transformation)) deallocate(This%Transformation, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Transformation', ProcName=ProcName, stat=StatLoc)

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
  type(InputVerifier_Type)                                            ::    InputVerifier 

  call This%Reset()
  
  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  This%NbTargets = 0
  call InputVerifier%AddParameter(Parameter='label')
  call Input%GetValue(Value=VarC0D, ParameterName='label', Mandatory=.true.)
  call ConvertToStrings(Value=VarC0D, Strings=This%Target, Separator=' ')
  This%NbTargets = size(This%Target,1)

  allocate(This%Transformation(This%NbTargets), stat=StatLoc)
  call InputVerifier%AddParameter(Parameter='transformation')
  if (StatLoc /= 0) call Error%Allocate(Name='This%Transformation', ProcName=ProcName, stat=StatLoc)
  call Input%GetValue(Value=VarC0D, ParameterName='transformation', Mandatory=.true.)
  i = 1
  do i = 1, This%NbTargets
    This%Transformation(i) = VarC0D
  end do

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput_IT(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput_IT

  class(InputTransform_Type), intent(in)                              ::    This
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
  call GetInput_IT%AddParameter(Name='transformation', Value=This%Transformation(1)%Get())

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine TransformInput_IT(This, Input)

  class(InputTransform_Type), intent(in)                              ::    This
  type(Input_Type), intent(inout)                                     ::    Input

  character(*), parameter                                             ::    ProcName='TransformInput_IT'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  call Input%Transform(Transformations=This%Transformation, Labels=This%Target)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine GetTargets_IT(This, Targets)

  class(InputTransform_Type), intent(in)                              ::    This
  type(SMUQString_Type), allocatable, dimension(:), intent(inout)     ::    Targets

  character(*), parameter                                             ::    ProcName='GetTargets_IT'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  if (allocated(Targets)) then
    if (size(Targets,1) /= This%NbTargets) then
      deallocate(Targets, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Targets', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Targets)) then
    allocate(Targets(This%NbTargets), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Targets', ProcName=ProcName, stat=StatLoc)
  end if

  Targets = This%Target

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
  LHS%Constructed = RHS%Constructed

  if (RHS%Constructed) then
    LHS%NbTargets = RHS%NbTargets
    allocate(LHS%Target, source=RHS%Target, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Target', ProcName=ProcName, stat=StatLoc)
    allocate(LHS%Transformation, source=RHS%Transformation, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%Transformation', ProcName=ProcName, stat=StatLoc)
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

  if (allocated(This%Transformation)) deallocate(This%Transformation, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Transformation', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module