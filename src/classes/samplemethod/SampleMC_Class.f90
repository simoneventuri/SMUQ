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

module SampleMC_Class

use Parameters_Library
use Input_Library
use CommandRoutines_Module
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SampleMethod_Class                                            ,only:    SampleMethod_Type
use RandPseudo_Class                                              ,only:    RandPseudo_Type

implicit none

private

public                                                                ::    SampleMC_Type

type, extends(SampleMethod_Type)                                      ::    SampleMC_Type
  type(RandPseudo_Type)                                               ::    RNG
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    Draw0D
  procedure, private                                                  ::    Draw1D
  procedure, private                                                  ::    Enrich0D
  procedure, private                                                  ::    Enrich1D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize(This)

  class(SampleMC_Type), intent(inout)                                 ::    This

  character(*), parameter                                             ::    ProcName='Initialize'
  integer(8)                                                          ::    SysTimeCount

  if (.not. This%Initialized) then
    This%Initialized = .true.
    This%Name = 'monte_carlo'
    call This%SetDefaults()
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(SampleMC_Type), intent(inout)                                 ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Initialized=.false.
  This%Constructed=.false.

  call This%RNG%Reset()

  call This%Initialize()

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults(This)

  class(SampleMC_Type), intent(inout)                                 ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults'

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput (This, Input, Prefix)

  class(SampleMC_Type), intent(inout)                                 ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  logical                                                             ::    Found
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    VarI0D
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  SectionName = 'rng'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.false., FoundSection=Found)
  if (Found) then
    call This%RNG%Construct(Input=InputSection, Prefix=PrefixLoc)
  else
    call This%RNG%Construct(DrawType=1)
  end if

  This%Constructed = .true.

end subroutine 
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1 (This, RNG)

  class(SampleMC_Type), intent(inout)                                 ::    This
  type(RandPseudo_Type), optional, intent(in)                         ::    RNG

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  if (present(RNG)) then
    This%RNG = RNG
  else
    call This%RNG%Construct(DrawType=1)
  end if

  This%Constructed = .true.

end subroutine 
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput
  class(SampleMC_Type), intent(in)                                    ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    FileName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  character(:), allocatable                                           ::    VarC0D

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  if (ExternalFlag) DirectorySub = DirectoryLoc // '/rng'
  call GetInput%AddSection(Section=This%RNG%GetInput(Name='rng', Prefix=PrefixLoc, Directory=DirectorySub))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Draw0D(This, Samples, NbSamples)

  class(SampleMC_Type), intent(inout)                                 ::    This
  real(rkp), , allocatable, dimension(:), intent(inout)               ::    Samples
  integer, intent(in)                                                 ::    NbSamples

  character(*), parameter                                             ::    ProcName='Draw0D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  call This%RNG%Draw(Samples=Samples, NbSamples=NbSamples)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Draw1D(This, Samples, NbSamples, NbDim)

  class(SampleMC_Type), intent(inout)                                 ::    This
  real(rkp), allocatable, dimension(:,:), intent(inout)               ::    Samples
  integer, intent(in)                                                 ::    NbSamples 
  integer, intent(in)                                                 ::    NbDim 

  character(*), parameter                                             ::    ProcName='Draw1D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  call This%RNG%Draw(Samples=Samples, NbSamples=NbSamples, NbDim=NbDim)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Enrich0D(This, Samples, NbEnrichmentSamples, EnrichmentSamples)

  class(SampleMC_Type), intent(inout)                                 ::    This
  real(rkp), dimension(:),intent(in)                                  ::    Samples
  real(rkp), dimension(:), allocatable, intent(inout)                 ::    EnrichmentSamples
  integer, intent(in)                                                 ::    NbEnrichmentSamples

  character(*), parameter                                             ::    ProcName='Enrich0D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  if (NbEnrichmentSamples < 1) call Error%Raise(Line='Inquired less than 1 enrichment sample', ProcName=ProcName)

  call This%RNG%Draw(Samples=EnrichmentSamples, NbSamples=NbEnrichmentSamples)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Enrich1D(This, Samples, NbEnrichmentSamples, EnrichmentSamples)

  class(SampleMC_Type), intent(inout)                                 ::    This
  real(rkp), dimension(:,:),intent(in)                                ::    Samples
  real(rkp), dimension(:,:), allocatable, intent(out)                 ::    EnrichmentSamples
  integer, intent(in)                                                 ::    NbEnrichmentSamples

  character(*), parameter                                             ::    ProcName='Enrich1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    NbDim

  NbDim = size(Samples,1)

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  if (NbEnrichmentSamples < 1) call Error%Raise(Line='Inquired less than 1 enrichment sample', ProcName=ProcName)

  call This%RNG%Draw(Samples=EnrichmentSamples, NbSamples=NbEnrichmentSamples, NbDim=NbDim)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(SampleMC_Type), intent(out)                                   ::    LHS
  class(SampleMethod_Type), intent(in)                                ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (SampleMC_Type)
      call LHS%Reset()
      LHS%Initialized = RHS%Initialized
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        LHS%RNG=RHS%RNG
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(SampleMC_Type), intent(inout)                                  ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
