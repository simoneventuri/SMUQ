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

module SampleQuasiMC_Class

use Parameters_Library
use Input_Library
use ArrayRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SampleMethod_Class                                            ,only:    SampleMethod_Type
use LowDiscSequence_Class                                         ,only:    LowDiscSequence_Type
use LowDiscSequence_Factory_Class                                 ,only:    LowDiscSequence_Factory
use LowDiscSobol_Class                                            ,only:    LowDiscSobol_Type

implicit none

private

public                                                                ::    SampleQuasiMC_Type

type, extends(SampleMethod_Type)                                      ::    SampleQuasiMC_Type
  class(LowDiscSequence_Type), allocatable                            ::    LowDiscSequence
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1      
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    Draw0D
  procedure, private                                                  ::    Draw1D
  procedure, private                                                  ::    Enrich0D
  procedure, private                                                  ::    Enrich1D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize(This)

  class(SampleQuasiMC_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='Initialize'

  if (.not. This%Initialized) then
    This%Initialized = .true.
    This%Name = 'quasi'
    call This%SetDefaults()
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(SampleQuasiMC_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Initialized=.false.
  This%Constructed=.false.

  if (allocated(This%LowDiscSequence)) deallocate(This%LowDiscSequence, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%LowDiscSequence', ProcName=ProcName, stat=StatLoc)

  call This%Initialize()

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults(This)

  class(SampleQuasiMC_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults'

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  use StringRoutines_Module

  class(SampleQuasiMC_Type), intent(inout)                            ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ProcessInput'
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    ParameterName
  logical                                                             ::    Found
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    VarI0D
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  SectionName = 'sequence'
  if (Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call LowDiscSequence_Factory%Construct(Object=This%LowDiscSequence, Input=InputSection)
    nullify(InputSection)
  else
    allocate(LowDiscSobol_Type   :: This%LowDiscSequence)
    select type (Object => This%LowDiscSequence)
      type is (LowDiscSobol_Type)
        call Object%Construct()
      class default
        call Error%Raise('Something went wrong', ProcName=ProcName)
    end select
  end if

  This%Constructed=.true.

end subroutine 
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1 (This, LowDiscSequence)

  class(SampleQuasiMC_Type), intent(inout)                            ::    This
  class(LowDiscSequence_Type), optional, intent(in)                   ::    LowDiscSequence

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  if (present(LowDiscSequence)) then
    allocate(This%LowDiscSequence, source=LowDiscSequence, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%LowDiscSequence', ProcName=ProcName, stat=StatLoc)
  else
    allocate(LowDiscSobol_Type   :: This%LowDiscSequence)
    select type (Object => This%LowDiscSequence)
      type is (LowDiscSobol_Type)
        call Object%Construct()
      class default
        call Error%Raise('Something went wrong', ProcName=ProcName)
    end select
  end if

  This%Constructed = .true.

end subroutine 
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput
  class(SampleQuasiMC_Type), intent(in)                               ::    This
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

  if (ExternalFlag) DirectorySub = DirectoryLoc // '/sequence'
  call GetInput%AddSection(Section=LowDiscSequence_Factory%GetObjectInput(Object=This%LowDiscSequence,                        &
                                                          Name='sequence', Prefix=PrefixLoc, Directory=DirectorySub))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Draw0D(This, Samples, NbSamples)

  class(SampleQuasiMC_Type), intent(inout)                            ::    This
  real(rkp), allocatable, dimension(:), intent(inout)                 ::    Samples
  integer, intent(in)                                                 ::    NbSamples

  character(*), parameter                                             ::    ProcName='Draw0D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  call EnsureArraySize(Array=Samples, Size1=NbSamples)

  call This%LowDiscSequence%GetSequence(Sequence=Samples, NbPoints=NbSamples)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Draw1D(This, Samples, NbSamples, NbDim)

  class(SampleQuasiMC_Type), intent(inout)                            ::    This
  real(rkp), allocatable, dimension(:,:), intent(inout)               ::    Samples
  integer, intent(in)                                                 ::    NbSamples 
  integer, intent(in)                                                 ::    NbDim 

  character(*), parameter                                             ::    ProcName='Draw1D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  call EnsureArraySize(Array=Samples, Size1=NbDim, Size2=NbSamples)

  call This%LowDiscSequence%GetSequence(Sequence=Samples, NbPoints=NbSamples, NbDim=NbDim)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Enrich0D(This, Samples, NbEnrichmentSamples, EnrichmentSamples)

  class(SampleQuasiMC_Type), intent(inout)                            ::    This
  real(rkp), dimension(:),intent(in)                                  ::    Samples
  real(rkp), dimension(:), allocatable, intent(inout)                 ::    EnrichmentSamples
  integer, intent(in)                                                 ::    NbEnrichmentSamples

  character(*), parameter                                             ::    ProcName='Enrich0D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  if (NbEnrichmentSamples < 1) call Error%Raise(Line='Inquired less than 1 enrichment sample', ProcName=ProcName)
  
  call EnsureArraySize(Array=EnrichmentSamples, Size1=NbEnrichmentSamples)

  call This%LowDiscSequence%GetSequence(Sequence=EnrichmentSamples, NbPoints=NbEnrichmentSamples, &
                                        Offset=size(Samples,1))

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Enrich1D(This, Samples, NbEnrichmentSamples, EnrichmentSamples)

  class(SampleQuasiMC_Type), intent(inout)                            ::    This
  real(rkp), dimension(:,:), intent(in)                               ::    Samples
  real(rkp), dimension(:,:), allocatable, intent(inout)               ::    EnrichmentSamples
  integer, intent(in)                                                 ::    NbEnrichmentSamples

  character(*), parameter                                             ::    ProcName='Enrich1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    NbDim

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  NbDim = size(Samples,1)

  if (NbEnrichmentSamples < 1) call Error%Raise(Line='Inquired less than 1 enrichment sample', ProcName=ProcName)

  call EnsureArraySize(Array=EnrichmentSamples, Size1=NbDim, Size2=NbEnrichmentSamples)

  call This%LowDiscSequence%GetSequence(Sequence=EnrichmentSamples, NbPoints=NbEnrichmentSamples, &
                                        NbDim=NbDim, Offset=size(Samples,2))

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(SampleQuasiMC_Type), intent(out)                              ::    LHS
  class(SampleMethod_Type), intent(in)                                ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0
  select type (RHS)

    type is (SampleQuasiMC_Type)
      call LHS%Reset()
      LHS%Initialized = RHS%Initialized
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        allocate(LHS%LowDiscSequence, source=RHS%LowDiscSequence, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%LowDiscSequence', ProcName=ProcName, stat=StatLoc)
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(SampleQuasiMC_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%LowDiscSequence)) deallocate(This%LowDiscSequence, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%LowDiscSequence', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
