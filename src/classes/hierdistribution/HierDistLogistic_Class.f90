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

module HierDistLogistic_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StatisticsRoutines_Module
use HierDistProb_Class                                            ,only:    HierDistProb_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use DistLogistic_Class                                            ,only:    DistLogistic_Type
use DistProb_Class                                                ,only:    DistProb_Type
use IScalarValue_Class                                            ,only:    IScalarValue_Type
use IScalarFixed_Class                                            ,only:    IScalarFixed_Type
use IScalarValue_Factory_Class                                    ,only:    IScalarValue_Factory

implicit none

private

public                                                                ::    HierDistLogistic_Type

type, extends(HierDistProb_Type)                                      ::    HierDistLogistic_Type
  class(IScalarValue_Type), allocatable                               ::    A
  class(IScalarValue_Type), allocatable                               ::    B
  class(IScalarValue_Type), allocatable                               ::    Mu
  class(IScalarValue_Type), allocatable                               ::    S
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Generate
  procedure, private                                                  ::    GenerateDistribution
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize(This)

  class(HierDistLogistic_Type), intent(inout)                         ::    This

  character(*), parameter                                             ::    ProcName='Initialize'

  if (.not. This%Initialized) then
    This%Name = 'hierarchical_logistic'
    This%Initialized = .true.
    call This%SetDefaults()
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(HierDistLogistic_Type), intent(inout)                         ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Initialized = .false.
  This%Constructed = .false.

  if (allocated(This%A)) deallocate(This%A, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%A', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%B)) deallocate(This%B, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%B', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Mu)) deallocate(This%Mu, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Mu', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%S)) deallocate(This%S, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%S', ProcName=ProcName, stat=StatLoc)  

  call This%Initialize()

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults(This)

  class(HierDistLogistic_Type), intent(inout)                         ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults'

  This%TruncatedRight = .false.
  This%TruncatedLeft = .false.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(HierDistLogistic_Type), intent(inout)                         ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  integer                                                             ::    StatLoc=0
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    ParameterName
  logical                                                             ::    Found
  real(rkp)                                                           ::    VarR0D
  character(:), allocatable                                           ::    VarC0D
  logical                                                             ::    VarL0D
  character(:), allocatable                                           ::    PrefixLoc
  logical                                                             ::    MandatoryLoc
  type(IScalarFixed_Type)                                             ::    FixedScalar

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()
  
  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  SectionName = 'a'
  if(Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call IScalarValue_Factory%Construct(Object=This%A, Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)
    This%TruncatedRight = .true.
  else
    call FixedScalar%Construct(Value=-huge(One))
    allocate(This%A, source=FixedScalar, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%A', ProcName=ProcName, stat=StatLoc)
  end if

  SectionName = 'b'
  if(Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call IScalarValue_Factory%Construct(Object=This%B, Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)
    This%TruncatedRight = .true.
  else
    call FixedScalar%Construct(Value=huge(One))
    allocate(This%B, source=FixedScalar, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%B', ProcName=ProcName, stat=StatLoc)
  end if

  SectionName = 'mu'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%Mu, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SectionName = 's'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%S, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  use StringConversion_Module

  type(InputSection_Type)                                             ::    GetInput

  class(HierDistLogistic_Type), intent(in)                            ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  if (This%TruncatedLeft) then
    SectionName = 'a'
    if (ExternalFlag) DirectorySub = DirectoryLoc // '/' // SectionName
    call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%A, Name=SectionName, Prefix=PrefixLoc,       &
                                                                         Directory=DirectorySub))
  end if

  if (This%TruncatedRight) then
    SectionName = 'b'
    if (ExternalFlag) DirectorySub = DirectoryLoc // '/' // SectionName
    call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%B, Name=SectionName, Prefix=PrefixLoc,       &
                                                                         Directory=DirectorySub))
  end if

  SectionName = 'mu'
  if (ExternalFlag) DirectorySub = DirectoryLoc // '/' // SectionName
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%Mu, Name=SectionName, Prefix=PrefixLoc,        &
                                                                       Directory=DirectorySub))

  SectionName = 's'
  if (ExternalFlag) DirectorySub = DirectoryLoc // '/' // SectionName
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%S, Name=SectionName, Prefix=PrefixLoc,         &
                                                                       Directory=DirectorySub))

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Generate(This, Input, Distribution)

  class(HierDistLogistic_Type), intent(in)                            ::    This
  type(Input_Type), intent(in)                                        ::    Input
  class(DistProb_Type), allocatable, intent(out)                      ::    Distribution

  character(*), parameter                                             ::    ProcName='Generate'
  integer                                                             ::    StatLoc=0
  real(rkp)                                                           ::    VarR0D     
  real(rkp)                                                           ::    Mu
  real(rkp)                                                           ::    S  
  real(rkp)                                                           ::    A  
  real(rkp)                                                           ::    B  

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  A = Zero
  B = Zero

  Mu = This%Mu%GetValue(Input=Input)

  S = This%S%GetValue(Input=Input)

  A = This%A%GetValue(Input=Input)

  B = This%B%GetValue(Input=Input)

  call This%GenerateDistribution(Mu=Mu, S=S, A=A, B=B, Distribution=Distribution)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine GenerateDistribution(This, Mu, S, A, B, Distribution)

  class(HierDistLogistic_Type), intent(in)                            ::    This
  real(rkp), intent(in)                                               ::    Mu
  real(rkp), intent(in)                                               ::    S
  real(rkp), intent(in)                                               ::    A
  real(rkp), intent(in)                                               ::    B
  class(DistProb_Type), allocatable, intent(out)                      ::    Distribution

  character(*), parameter                                             ::    ProcName='GenerateDistribution'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  allocate(DistLogistic_Type :: Distribution)

  select type (Distribution)
    type is (DistLogistic_Type) 
      if (This%TruncatedLeft .and. This%TruncatedRight) then
        call Distribution%Construct(Mu=Mu, S=S, A=A, B=B)
      elseif (This%TruncatedLeft) then
        call Distribution%Construct(Mu=Mu, S=S, A=A)
      elseif (This%TruncatedRight) then
        call Distribution%Construct(Mu=Mu, S=S, B=B)
      else
        call Distribution%Construct(Mu=Mu, S=S)
      end if
    class default
      call Error%Raise("Something went wrong", ProcName=ProcName)
  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(HierDistLogistic_Type), intent(out)                           ::    LHS
  class(HierDistProb_Type), intent(in)                                ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (HierDistLogistic_Type)
      call LHS%Reset()
      LHS%Initialized = RHS%Initialized
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        allocate(LHS%A, source=RHS%A, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%A', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%B, source=RHS%B, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%B', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%Mu, source=RHS%Mu, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Mu', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%S, source=RHS%S, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%S', ProcName=ProcName, stat=StatLoc)
        LHS%TruncatedLeft = RHS%TruncatedLeft
        LHS%TruncatedRight = RHS%TruncatedRight
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(HierDistLogistic_Type), intent(inout)                          ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%A)) deallocate(This%A, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%A', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%B)) deallocate(This%B, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%B', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Mu)) deallocate(This%Mu, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Mu', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%S)) deallocate(This%S, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%S', ProcName=ProcName, stat=StatLoc)  

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
