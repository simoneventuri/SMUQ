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
! defined using shape and rate form
module HierDistBeta_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StatisticsRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use HierDistProb_Class                                            ,only:    HierDistProb_Type
use Input_Class                                                   ,only:    Input_Type
use DistBeta_Class                                                ,only:    DistBeta_Type
use DistProb_Class                                                ,only:    DistProb_Type
use IScalarValueClass                                             ,only:    IScalarValue_Type
use IScalarFixedClass                                             ,only:    IScalarFixed_Type
use IScalarValue_Factory_Class                                    ,only:    IScalarValue_Factory

implicit none

private

public                                                                ::    HierDistBeta_Type

type, extends(HierDistProb_Type)                                      ::    HierDistBeta_Type
  class(IScalarValue_Type), allocatable                               ::    A
  class(IScalarValue_Type), allocatable                               ::    B
  class(IScalarValue_Type), allocatable                               ::    Alpha
  class(IScalarValue_Type), allocatable                               ::    Beta
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

  class(HierDistBeta_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Initialize'

  if (.not. This%Initialized) then
    This%Name = 'hierarchical_gamma'
    This%Initialized = .true.
    call This%SetDefaults()
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(HierDistBeta_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Initialized = .false.
  This%Constructed = .false.

  if (allocated(This%A)) deallocate(This%A, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%A', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%B)) deallocate(This%B, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%B', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Alpha)) deallocate(This%Alpha, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Alpha', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Beta)) deallocate(This%Beta, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Beta', ProcName=ProcName, stat=StatLoc)

  call This%Initialize()

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults(This)

  class(HierDistBeta_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults'

  This%TruncatedRight = .true.
  This%TruncatedLeft = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(HierDistBeta_Type), intent(inout)                             ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ProcessInput'
  integer                                                             ::    StatLoc=0
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
  else
    call FixedScalar%Construct(Value=Zero)
    allocate(This%A, source=FixedScalar, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%A', ProcName=ProcName, stat=StatLoc)
  end if

  SectionName = 'b'
  if(Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call IScalarValue_Factory%Construct(Object=This%B, Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)
  else
    call FixedScalar%Construct(Value=One)
    allocate(This%B, source=FixedScalar, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%B', ProcName=ProcName, stat=StatLoc)
  end if

  SectionName = 'alpha'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%Alpha, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SectionName = 'beta'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%Beta, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  use StringRoutines_Module

  type(InputSection_Type)                                             ::    GetInput

  class(HierDistBeta_Type), intent(in)                                ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
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

  SectionName = 'a'
  if (ExternalFlag) DirectorySub = DirectoryLoc // '/' // SectionName
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%A, Name=SectionName, Prefix=PrefixLoc,         &
                                                                       Directory=DirectorySub))
  
  SectionName = 'b'
  if (ExternalFlag) DirectorySub = DirectoryLoc // '/' // SectionName
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%B, Name=SectionName, Prefix=PrefixLoc,         &
                                                                       Directory=DirectorySub))

  SectionName = 'alpha'
  if (ExternalFlag) DirectorySub = DirectoryLoc // '/' // SectionName
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%Alpha, Name=SectionName, Prefix=PrefixLoc,     &
                                                                       Directory=DirectorySub))

  SectionName = 'beta'
  if (ExternalFlag) DirectorySub = DirectoryLoc // '/' // SectionName
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%Beta, Name=SectionName, Prefix=PrefixLoc,      &
                                                                       Directory=DirectorySub))

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Generate(This, Input, Distribution)

  class(HierDistBeta_Type), intent(in)                                ::    This
  type(Input_Type), intent(in)                                        ::    Input
  class(DistProb_Type), allocatable, intent(out)                      ::    Distribution

  character(*), parameter                                             ::    ProcName='Generate'
  integer                                                             ::    StatLoc=0
  real(rkp)                                                           ::    Alpha
  real(rkp)                                                           ::    Beta
  real(rkp)                                                           ::    A
  real(rkp)                                                           ::    B

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  Alpha = This%Alpha%GetValue(Input=Input)

  Beta = This%Beta%GetValue(Input=Input)

  A = This%A%GetValue(Input=Input)
  
  B = This%B%GetValue(Input=Input)

  call This%GenerateDistribution(Alpha=Alpha, Beta=Beta, A=A, B=B, Distribution=Distribution)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine GenerateDistribution(This, Alpha, Beta, A, B, Distribution)

  class(HierDistBeta_Type), intent(in)                                ::    This
  real(rkp), intent(in)                                               ::    Alpha
  real(rkp), intent(in)                                               ::    Beta
  real(rkp), intent(in)                                               ::    A
  real(rkp), intent(in)                                               ::    B
  class(DistProb_Type), allocatable, intent(out)                      ::    Distribution

  character(*), parameter                                             ::    ProcName='GenerateDistribution'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  allocate(DistBeta_Type :: Distribution)

  select type (Distribution)
    type is (DistBeta_Type) 
      call Distribution%Construct(Alpha=Alpha, Beta=Beta, A=A, B=B)
    class default
      call Error%Raise("Something went wrong", ProcName=ProcName)
  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(HierDistBeta_Type), intent(out)                               ::    LHS
  class(HierDistProb_Type), intent(in)                                ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (HierDistBeta_Type)
      call LHS%Reset()
      LHS%Initialized = RHS%Initialized
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        allocate(LHS%A, source=RHS%A, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%A', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%B, source=RHS%B, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%B', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%Alpha, source=RHS%Alpha, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Alpha', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%Beta, source=RHS%Beta, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Beta', ProcName=ProcName, stat=StatLoc)
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

  type(HierDistBeta_Type), intent(inout)                              ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%A)) deallocate(This%A, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%A', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%B)) deallocate(This%B, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%B', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Alpha)) deallocate(This%Alpha, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Alpha', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Beta)) deallocate(This%Beta, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Beta', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
