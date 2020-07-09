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

module HierDistNorm_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use HierDistProb_Class                                            ,only:    HierDistProb_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use DistNorm_Class                                                ,only:    DistNorm_Type
use DistProb_Class                                                ,only:    DistProb_Type
use IScalarValue_Class                                            ,only:    IScalarValue_Type
use IScalarFixed_Class                                            ,only:    IScalarFixed_Type
use IScalarValue_Factory_Class                                    ,only:    IScalarValue_Factory

implicit none

private

public                                                                ::    HierDistNorm_Type

type, extends(HierDistProb_Type)                                      ::    HierDistNorm_Type
  class(IScalarValue_Type), allocatable                               ::    A
  class(IScalarValue_Type), allocatable                               ::    B
  class(IScalarValue_Type), allocatable                               ::    Mu
  class(IScalarValue_Type), allocatable                               ::    Sigma
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    Generate
  procedure, private                                                  ::    GenerateDistribution
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.
real(rkp), parameter                                                  ::    dlogof2pi=dlog(Two*pi)
contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize(This)

  class(HierDistNorm_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Initialize'

  if (.not. This%Initialized) then
    This%Name = 'hierarchical_normal'
    This%Initialized = .true.
    call This%SetDefaults()
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(HierDistNorm_Type), intent(inout)                             ::    This

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

  if (allocated(This%Sigma)) deallocate(This%Sigma, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Sigma', ProcName=ProcName, stat=StatLoc)

  call This%Initialize()

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults(This)

  class(HierDistNorm_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults'

  This%TruncatedRight = .false.
  This%TruncatedLeft = .false.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(HierDistNorm_Type), intent(inout)                             ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    ParameterName
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    SectionName
  logical                                                             ::    Found
  real(rkp)                                                           ::    VarR0D
  logical                                                             ::    VarL0D
  character(:), allocatable                                           ::    VarC0D
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

  SectionName = 'sigma'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%Sigma, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  use StringConversion_Module

  type(InputSection_Type)                                             ::    GetInput

  class(HierDistNorm_Type), intent(in)                                ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
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

  SectionName = 'sigma'
  if (ExternalFlag) DirectorySub = DirectoryLoc // '/' // SectionName
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%Sigma, Name=SectionName, Prefix=PrefixLoc,     &
                                                                       Directory=DirectorySub))

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Generate(This, Input, Distribution)

  class(HierDistNorm_Type), intent(in)                                ::    This
  type(Input_Type), intent(in)                                        ::    Input
  class(DistProb_Type), allocatable, intent(out)                      ::    Distribution

  character(*), parameter                                             ::    ProcName='Generate'
  integer                                                             ::    StatLoc=0
  real(rkp)                                                           ::    Mu
  real(rkp)                                                           ::    Sigma
  real(rkp)                                                           ::    A
  real(rkp)                                                           ::    B       

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  A = Zero
  B = Zero

  Mu = This%Mu%GetValue(Input=Input)

  Sigma = This%Sigma%GetValue(Input=Input)

  A = This%A%GetValue(Input=Input)

  B = This%B%GetValue(Input=Input)

  call This%GenerateDistribution(Mu=Mu, Sigma=Sigma, A=A, B=B, Distribution=Distribution)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine GenerateDistribution(This, Mu, Sigma, A, B, Distribution)

  class(HierDistNorm_Type), intent(in)                                ::    This
  real(rkp), intent(in)                                               ::    Mu
  real(rkp), intent(in)                                               ::    Sigma
  real(rkp), intent(in)                                               ::    A
  real(rkp), intent(in)                                               ::    B
  class(DistProb_Type), allocatable, intent(out)                      ::    Distribution

  character(*), parameter                                             ::    ProcName='GenerateDistribution'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  allocate(DistNorm_Type :: Distribution)

  select type (Distribution)
    type is (DistNorm_Type) 
      if (This%TruncatedLeft .and. This%TruncatedRight) then
        call Distribution%Construct(Mu=Mu, Sigma=Sigma, A=A, B=B)
      elseif (This%TruncatedLeft) then
        call Distribution%Construct(Mu=Mu, Sigma=Sigma, A=A)
      elseif (This%TruncatedRight) then
        call Distribution%Construct(Mu=Mu, Sigma=Sigma, B=B)
      else
        call Distribution%Construct(Mu=Mu, Sigma=Sigma)
      end if
    class default
      call Error%Raise("Something went wrong", ProcName=ProcName)
  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(HierDistNorm_Type), intent(out)                               ::    LHS
  class(HierDistProb_Type), intent(in)                                ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (HierDistNorm_Type)
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
        allocate(LHS%Sigma, source=RHS%Sigma, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Sigma', ProcName=ProcName, stat=StatLoc)
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

  type(HierDistNorm_Type), intent(inout)                              ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%A)) deallocate(This%A, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%A', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%B)) deallocate(This%B, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%B', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Mu)) deallocate(This%Mu, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Mu', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Sigma)) deallocate(This%Sigma, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Sigma', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
