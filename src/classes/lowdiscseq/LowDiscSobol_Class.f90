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

module LowDiscSobol_Class

use Parameters_Library
use Input_Library
use Logger_Class                                                  ,only:  Logger
use Error_Class                                                   ,only:  Error
use LowDiscSequence_Class                                         ,only:  LowDiscSequence_Type
use Sobol_Library
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    LowDiscSobol_Type

type, extends(LowDiscSequence_Type)                                   ::    LowDiscSobol_Type
  integer                                                             ::    Skip=0
  integer                                                             ::    Leap=0
contains
  procedure, public                                                   ::    Reset
  generic, public                                                     ::    Construct               =>    ConstructCase1      
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    Get0D
  procedure, private                                                  ::    Get1D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(LowDiscSobol_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed=.false.

  This%Skip = 0
  This%Leap = 0

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  use StringConversion_Module

  class(LowDiscSobol_Type), intent(inout)                             ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ProcessInput'
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    ParameterName
  logical                                                             ::    Found
  integer                                                             ::    VarI0D
  character(:), allocatable                                           ::    VarC0D
  logical                                                             ::    VarL0D
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  ParameterName = 'skip'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) then
    This%Skip = VarI0D
    if (This%Skip < 0) call Error%Raise(Line="Step < 0 was specified, please supply a value at or above 0",                  &
                                                                                                              ProcName=ProcName)
  end if

  ParameterName = 'leap'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) then
    This%Leap = VarI0D
    if (This%Leap < 0) call Error%Raise(Line="Leap < 0 was specified, please supply a value at or above 0",                  &
                                                                                                              ProcName=ProcName)
  end if

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed=.true.

end subroutine 
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1 (This, Skip, Leap)

  class(LowDiscSobol_Type), intent(inout)                             ::    This
  integer, optional, intent(in)                                       ::    Skip
  integer, optional, intent(in)                                       ::    Leap

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0

  call This%Reset()

  if (present(Skip)) then
    This%Skip = Skip 
    if (This%Skip < 0) call Error%Raise(Line="Step < 0 was specified, please supply a value at or above 0",                  &
                                                                                                              ProcName=ProcName)
  end if

  if (present(Leap)) then
    This%Leap = Leap 
    if (This%Skip < 0) call Error%Raise(Line="Step < 0 was specified, please supply a value at or above 0",                  &
                                                                                                              ProcName=ProcName)
  end if

  This%Constructed = .true.

end subroutine 
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  use StringConversion_Module

  type(InputSection_Type)                                             ::    GetInput
  class(LowDiscSobol_Type), intent(in)                                ::    This
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
  character(100)                                                      ::    VarC0D

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  call GetInput%AddParameter(Name='skip', Value=ConvertToString(Value=This%Skip))
  call GetInput%AddParameter(Name='leap', Value=ConvertToString(Value=This%Leap))

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Get0D(This, Sequence, NbPoints, Offset)

  class(LowDiscSobol_Type), intent(in)                                ::    This
  real(rkp), contiguous, dimension(:), intent(inout)                  ::    Sequence
  integer, intent(in)                                                 ::    NbPoints  
  integer, optional, intent(in)                                       ::    Offset                                          

  character(*), parameter                                             ::    ProcName='Get0D'
  integer                                                             ::    StatLoc=0
  integer(8)                                                          ::    Step
  integer(8)                                                          ::    SkipLoc
  integer(8)                                                          ::    LeapLoc
  integer(8)                                                          ::    i
  integer(8)                                                          ::    OneLoc
  integer                                                             ::    OffsetLoc

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  if (NbPoints <= 0) call Error%Raise(Line='Requested 0 or less points from the sequence', ProcName=ProcName)

  if (size(Sequence,1) /= NbPoints) call Error%Raise('Incompatible array', ProcName=ProcName)

  OffsetLoc = 0
  if (present(Offset)) OffsetLoc = Offset

  OneLoc = int(1,8)

  LeapLoc = int(This%Leap,8) + 1
  SkipLoc = int(This%Skip,8) + int(OffSetLoc,8)*LeapLoc

  do i = 1, NbPoints
    Step = SkipLoc + LeapLoc*(i-1) + 1
    call i8_sobol(OneLoc, Step, Sequence(i:i))
  end do

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Get1D(This, Sequence, NbPoints, NbDim, Offset)

  class(LowDiscSobol_Type), intent(in)                                ::    This
  real(rkp), contiguous, dimension(:,:), intent(inout)                ::    Sequence
  integer, intent(in)                                                 ::    NbPoints
  integer, intent(in)                                                 ::    NbDim
  integer, optional, intent(in)                                       ::    Offset                                          

  character(*), parameter                                             ::    ProcName='Get1D'
  integer                                                             ::    StatLoc=0
  integer(8)                                                          ::    NbDimLoc
  integer(8)                                                          ::    Step
  integer(8)                                                          ::    SkipLoc
  integer(8)                                                          ::    LeapLoc
  integer(8)                                                          ::    i
  integer                                                             ::    OffsetLoc

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  if (NbPoints <= 0) call Error%Raise(Line='Requested 0 or less points from the sequence', ProcName=ProcName)

  if (NbDim <= 0) call Error%Raise(Line='Specified dimensionality of 0 or less frmo the sequence', ProcName=ProcName)

  if (size(Sequence,1) /= NbDim) call Error%Raise('Incompatible array', ProcName=ProcName)
  if (size(Sequence,2) /= NbPoints) call Error%Raise('Incompatible array', ProcName=ProcName)

  OffsetLoc = 0
  if (present(Offset)) OffsetLoc = Offset

  NbDimLoc = int(NbDim,8)

  LeapLoc = int(This%Leap,8) + 1
  SkipLoc = int(This%Skip,8) + int(OffSetLoc,8)*LeapLoc

  do i = 1, NbPoints
    Step = SkipLoc + LeapLoc*(i-1) + 1
    call i8_sobol(NbDimLoc, Step, Sequence(:,i))
  end do

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(LowDiscSobol_Type), intent(out)                               ::    LHS
  class(LowDiscSequence_Type), intent(in)                             ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (LowDiscSobol_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        LHS%Skip = RHS%Skip
        LHS%Leap = RHS%Leap
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(LowDiscSobol_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
