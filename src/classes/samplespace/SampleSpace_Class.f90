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

module SampleSpace_Class

use Parameters_Library
use Input_Library
use CommandRoutines_Module
use ArrayIORoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use DistProb_Class                                                ,only:    DistProb_Type
use DistProbContainer_Class                                       ,only:    DistProbContainer_Type
use SampleMethod_Class                                            ,only:    SampleMethod_Type
use DistNorm_Class                                                ,only:    DistNorm_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    SampleSpace_Type

type, abstract                                                        ::    SampleSpace_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbDim=0
  logical                                                             ::    Correlated=.false.
  type(DistProbContainer_Type), allocatable, dimension(:)             ::    DistProb
  type(SMUQString_Type), allocatable, dimension(:)                    ::    ParamName
  type(SMUQString_Type), allocatable, dimension(:)                    ::    Label
  real(rkp), dimension(:,:), allocatable                              ::    CorrMat
contains
  procedure(Initialize_SampleSpace), deferred, public                 ::    Initialize
  procedure(Reset_SampleSpace), deferred, public                      ::    Reset
  procedure(SetDefaults_SampleSpace), deferred, public                ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(ConstructInput_SampleSpace), deferred, private            ::    ConstructInput
  procedure(GetInput_SampleSpace), deferred, public                   ::    GetInput
  procedure, public                                                   ::    GetNbDim
  procedure, public                                                   ::    IsCorrelated
  generic, public                                                     ::    GetDistribution         =>    GetDist0D_LabelString,  &
                                                                                                          GetDist0D_LabelChar,    &
                                                                                                          GetDist0D_Num,          &
  procedure, public                                                   ::    GetDistributions        =>    GetDist1D
  procedure, private                                                  ::    GetDist0D_LabelString
  procedure, private                                                  ::    GetDist0D_LabelChar
  procedure, private                                                  ::    GetDist0D_Num
  procedure, public                                                   ::    GetLabel                =>    GetLabel0D
  procedure, public                                                   ::    GetLabels               =>    GetLabel1D
  generic, public                                                     ::    GetName                 =>    GetName0D_LabelChar,    &
                                                                                                          GetName0D_LabelString,  &
                                                                                                          GetName0D_Num
  procedure, public                                                   ::    GetNames                =>    GetName1D
  procedure, private                                                  ::    GetName0D_LabelString
  procedure, private                                                  ::    GetName0D_LabelChar
  procedure, private                                                  ::    GetName0D_Num
  generic, public                                                     ::    GetDistributionPointer  =>    GetDistPtr_LabelChar,   &
                                                                                                          GetDistPtr_LabelString, &
                                                                                                          GetDistPtr_Num
  procedure, public                                                   ::    GetDistPtr_LabelChar
  procedure, public                                                   ::    GetDistPtr_LabelString
  procedure, public                                                   ::    GetDistPtr_Num
  procedure, public                                                   ::    GetCorrMat
  procedure, public                                                   ::    GetCorrMatPointer
  procedure, public                                                   ::    Draw
  procedure, public                                                   ::    Enrich
  procedure, nopass, public                                           ::    DrawMVarNormal
  procedure, nopass, public                                           ::    DrawMVarNormalLt
  procedure, public                                                   ::    WriteInfo
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure(Copy_SampleSpace), deferred, public                       ::    Copy
end type

logical, parameter                                                    ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_SampleSpace(This)
    import                                                            ::    SampleSpace_Type
    class(SampleSpace_Type), intent(inout)                            ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_SampleSpace(This)
    import                                                            ::    SampleSpace_Type
    class(SampleSpace_Type), intent(inout)                            ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_SampleSpace(This)
    import                                                            ::    SampleSpace_Type
    class(SampleSpace_Type),intent(inout)                             ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------  

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_SampleSpace(This, Input, Prefix)
    import                                                            ::    SampleSpace_Type
    import                                                            ::    InputSection_Type
    class(SampleSpace_Type), intent(inout)                            ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_SampleSpace(This, Name, Prefix, Directory)
      import                                                          ::    InputSection_Type
      import                                                          ::    SampleSpace_Type
      type(InputSection_Type)                                         ::    GetInput_SampleSpace
      class(SampleSpace_Type), intent(in)                             ::    This
      character(*), intent(in)                                        ::    Name
      character(*), optional, intent(in)                              ::    Prefix
      character(*), optional, intent(in)                              ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_SampleSpace(LHS, RHS)
    import                                                            ::    SampleSpace_Type
    class(SampleSpace_Type), intent(out)                              ::    LHS
    class(SampleSpace_Type), intent(in)                               ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

!!------------------------------------------------------------------------------------------------------------------------------
function GetName0D_Num(This, Num)

  character(:), allocatable                                           ::    GetName0D_Num
  class(SampleSpace_Type), intent(in)                                 ::    This
  integer, intent(in)                                                 ::    Num

  character(*), parameter                                             ::    ProcName='GetName0D_Num'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (Num > This%NbDim) call Error%Raise(Line='Num specifier above maximum number of distributions', ProcName=ProcName)
  if (Num < 1) call Error%Raise(Line='Num specifier below minimum of 1', ProcName=ProcName)

  GetName0D_Num = This%ParamName(Num)%Get()

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetName0D_LabelChar(This, Label)

  character(:), allocatable                                           ::    GetName0D_LabelChar
  class(SampleSpace_Type), intent(in)                                 ::    This
  character(*), intent(in)                                            ::    Label

  character(*), parameter                                             ::    ProcName='GetName0D_LabelChar'
  integer                                                             ::    i
  integer                                                             ::    ii

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  i = 1
  ii = 0
  do i = 1, This%NbDim
    if (This%Label(i) /= Label) cycle
    ii = i
    exit
  end do

  if (ii == 0) call Error%Raise('Did not find required parameter with label : ' // Label, ProcName=ProcName)

  GetName0D_LabelChar = This%ParamName(ii)%Get()      

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetName0D_LabelString(This, Label)

  character(:), allocatable                                           ::    GetName0D_LabelString
  class(SampleSpace_Type), intent(in)                                 ::    This
  type(SMUQString_Type), intent(in)                                   ::    Label

  character(*), parameter                                             ::    ProcName='GetName0D_LabelString'
  integer                                                             ::    i
  integer                                                             ::    ii

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  i = 1
  ii = 0
  do i = 1, This%NbDim
    if (This%Label(i) /= Label) cycle
    ii = i
    exit
  end do

  if (ii == 0) call Error%Raise('Did not find required parameter with label : ' // Label, ProcName=ProcName)

  GetName0D_LabelString = This%ParamName(ii)%Get()      

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetName1D(This, Names)

  class(SampleSpace_Type), intent(in)                                 ::    This
  type(SMUQString_Type), allocatable, dimension(:), intent(inout)     ::    Names

  character(*), parameter                                             ::    ProcName='GetName1D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (allocated(Names)) deallocate(Names, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Names', ProcName=ProcName, stat=StatLoc)

  allocate(Names, source=This%ParamName, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Names', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetLabel0D(This, Num)

  character(:), allocatable                                           ::    GetLabel0D
  class(SampleSpace_Type), intent(in)                                 ::    This
  integer, intent(in)                                                 ::    Num

  character(*), parameter                                             ::    ProcName='GetLabel0D'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetLabel0D = This%Label(Num)%Get()

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetLabel1D(This, Labels)

  class(SampleSpace_Type), intent(in)                                 ::    This
  type(SMUQString_Type), allocatable, dimension(:), intent(inout)     ::    Labels

  character(*), parameter                                             ::    ProcName='GetLabel1D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (allocated(Labels)) deallocate(Labels, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Labels', ProcName=ProcName, stat=StatLoc)

  allocate(Labels, source=This%Label, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Labels', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetDist0D_Num(This, Num)

  class(DistProb_Type), allocatable                                   ::    GetDist0D_Num

  class(SampleSpace_Type), intent(in)                                 ::    This
  integer, intent(in)                                                 ::    Num

  character(*), parameter                                             ::    ProcName='GetDist0D_Num'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (Num > This%NbDim) call Error%Raise(Line='Num specifier above maximum number of distributions', ProcName=ProcName)
  if (Num < 1) call Error%Raise(Line='Num specifier below minimum of 1', ProcName=ProcName)

  allocate(GetDist0D_Num, source=This%DistProb(Num)%GetPointer(), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='GetDist0D_Num', ProcName=ProcName, stat=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetDist0D_LabelChar(This, Label)

  class(DistProb_Type), allocatable                                   ::    GetDist0D_LabelChar

  class(SampleSpace_Type), intent(in)                                 ::    This
  character(*), intent(in)                                            ::    Label

  character(*), parameter                                             ::    ProcName='GetDist0D_LabelChar'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    ii

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  i = 1
  ii = 0
  do i = 1, This%NbDim
    if (This%Label(i) /= Label) cycle
    ii = i
    exit
  end do

  if (ii == 0) call Error%Raise('Did not find required parameter with label : ' // Label, ProcName=ProcName)

  allocate(GetDist0D_LabelChar, source=This%DistProb(ii)%GetPointer(), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='GetDist0D_Num', ProcName=ProcName, stat=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetDist0D_LabelString(This, Label)

  class(DistProb_Type), allocatable                                   ::    GetDist0D_LabelString

  class(SampleSpace_Type), intent(in)                                 ::    This
  type(SMUQString_Type), intent(in)                                   ::    Label

  character(*), parameter                                             ::    ProcName='GetDist0D_LabelString'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    ii

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  i = 1
  ii = 0
  do i = 1, This%NbDim
    if (This%Label(i) /= Label) cycle
    ii = i
    exit
  end do

  if (ii == 0) call Error%Raise('Did not find required parameter with label : ' // Label, ProcName=ProcName)

  allocate(GetDist0D_LabelString, source=This%DistProb(ii)%GetPointer(), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='GetDist0D_Num', ProcName=ProcName, stat=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetDist1D(This, Distributions)

  class(SampleSpace_Type), intent(in)                                 ::    This
  type(DistProbContainer_Type), allocatable, dimension(:), intent(inout)   ::    Distributions

  character(*), parameter                                             ::    ProcName='GetDist1D'
  integer                                                             ::    i
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (allocated(Distributions)) deallocate(Distributions, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Distributions', ProcName=ProcName, stat=StatLoc)

  allocate(Distributions, source=This%DistProb, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Distributions', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetDistPtr_LabelChar(This, Label)

  class(DistProb_Type), pointer                                       ::    GetDistPtr_LabelChar

  class(SampleSpace_Type), intent(in)                                 ::    This
  character(*), intent(in)                                            ::    Label

  character(*), parameter                                             ::    ProcName='GetDistPtr_LabelChar'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    ii

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  i = 1
  ii = 0
  do i = 1, This%NbDim
    if (This%Label(i) /= Label) cycle
    ii = i
    exit
  end do

  if (ii == 0) call Error%Raise('Did not find required parameter with label : ' // Label, ProcName=ProcName)

  GetDistPtr_LabelChar => This%DistProb(ii)%GetPointer()

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetDistPtr_LabelString(This, Label)

  class(DistProb_Type), pointer                                       ::    GetDistPtr_LabelString

  class(SampleSpace_Type), intent(in)                                 ::    This
  type(SMUQString_Type), intent(in)                                   ::    Label

  character(*), parameter                                             ::    ProcName='GetDistPtr_LabelString'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    ii

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  i = 1
  ii = 0
  do i = 1, This%NbDim
    if (This%Label(i) /= Label) cycle
    ii = i
    exit
  end do

  if (ii == 0) call Error%Raise('Did not find required parameter with label : ' // Label, ProcName=ProcName)

  GetDistPtr_LabelString => This%DistProb(ii)%GetPointer()

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetDistPtr_Num(This, Num)

  class(DistProb_Type), pointer                                       ::    GetDistPtr_Num

  class(SampleSpace_Type), intent(in)                                 ::    This
  integer, intent(in)                                                 ::    Num

  character(*), parameter                                             ::    ProcName='GetDistPtr_Num'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (Num > This%NbDim) call Error%Raise(Line='Num specifier above maximum number of distributions', ProcName=ProcName)
  if (Num < 1) call Error%Raise(Line='Num specifier below minimum of 1', ProcName=ProcName)

  GetDistPtr_Num => This%DistProb(Num)%GetPointer()

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetCorrMat(This. CorrMat)

  class(SampleSpace_Type), intent(in)                                 ::    This
  real(rkp), allocatable, dimension(:,:), intent(inout)               ::    CorrMat

  character(*), parameter                                             ::    ProcName='GetCorrMat'
  integer                                                             ::    StatLoc=0

  if (allocated(CorrMat)) deallocate(CorrMat, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='CorrMat', ProcName=ProcName, stat=StatLoc)

  allocate(GetCorrMat, source=This%CorrMat, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='GetCorrMat', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetCorrMatPointer(This)

  real(rkp), pointer, dimension(:,:)                                  ::    GetCorrMatPointer
  class(SampleSpace_Type), target, intent(in)                         ::    This

  character(*), parameter                                             ::    ProcName='GetCorrMatPointer'
  integer                                                             ::    StatLoc=0

  GetCorrMatPointer => This%CorrMat

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetNbDim(This)

  integer                                                             ::    GetNbDim
  class(SampleSpace_Type), intent(in)                                 ::    This

  character(*), parameter                                             ::    ProcName='GetNbDim'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetNbDim = This%NbDim

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function IsCorrelated(This)

  logical                                                             ::    IsCorrelated
  class(SampleSpace_Type), intent(in)                                 ::    This

  character(*), parameter                                             ::    ProcName='IsCorrelated'
  integer                                                             ::    StatLoc=0

  IsCorrelated = This%Correlated

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Draw(This, Sampler, NbSamples, Samples)

  class(SampleSpace_Type), intent(in)                                 ::    This
  class(SampleMethod_Type), intent(inout)                             ::    Sampler
  integer, intent(in)                                                 ::    NbSamples
  real(rkp), allocatable, dimension(:,:), intent(inout)               ::    Samples

  character(*), parameter                                             ::    ProcName='Draw'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    NbDim
  integer                                                             ::    i, ii
  class(DistProb_Type), pointer                                       ::    DistProb=>null()
  real(rkp), allocatable, dimension(:)                                ::    VarR1D
  real(rkp), dimension(:,:), allocatable                              ::    Lt

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  NbDim = This%NbDim

  call Sampler%Draw(NbDim=NbDim, NbSamples=NbSamples, Samples=Samples)

  if (.not. This%Correlated) then
    ii = 1
    do ii = 1, NbSamples
      i = 1
      do i = 1, NbDim
        DistProb => This%DistProb(i)%GetPointer()
        Samples(i,ii) = DistProb%InvCDF(P=Samples(i,ii))
        nullify(DistProb)
      end do
    end do
  else
    allocate(VarR1D(NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

    i = 1
    ii = 0
    do i = 1, NbDim
      DistProb => This%DistProb(i)%GetPointer()
      select type (DistProb)
        type is (DistNorm_Type)
          VarR1D(i) = DistProb%GetMu()
          ii = ii + 1
        class default
          exit
      end select
    end do
    if(associated(DistProb)) nullify(DistProb)
    if (ii < NbDim) call Error%Raise(Line='Sampling of correlated non normal multivariate spaces is not yet supported',      &
                                                                                                              ProcName=ProcName)

    allocate(Lt, source=This%CorrMat, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Lt', ProcName=ProcName, stat=StatLoc)

    call DPOTRF('U', NbDim, Lt, NbDim, StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line='Something went wrong in DPOTRF', ProcName=ProcName)

    i = 1    if (allocated(Samples)) deallocate(Samples, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Samples', ProcName=ProcName, stat=StatLoc)
    end do

    deallocate(Lt, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Lt', ProcName=ProcName, stat=StatLoc)

    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Enrich(This, Sampler, NbEnrichmentSamples, Samples, EnrichmentSamples)

  class(SampleSpace_Type), intent(in)                                 ::    This
  class(SampleMethod_Type), intent(inout)                             ::    Sampler
  real(rkp), dimension(:,:), intent(in)                               ::    Samples
  integer, intent(in)                                                 ::    NbEnrichmentSamples
  real(rkp), allocatable, dimension(:,:), intent(inout)               ::    EnrichmentSamples

  character(*), parameter                                             ::    ProcName='Enrich'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    NbDim
  integer                                                             ::    i, ii
  class(DistProb_Type), pointer                                       ::    DistProb=>null()
  real(rkp), allocatable, dimension(:)                                ::    VarR1D
  integer                                                             ::    NbSamples
  integer                                                             ::    NbEnrichSamplesLoc
  real(rkp), allocatable, dimension(:,:)                              ::    NormalizedSamples
  real(rkp), dimension(:,:), allocatable                              ::    Lt

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  NbDim = This%NbDim

  if (.not. This%Correlated) then
    allocate(NormalizedSamples, source=Samples, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='NormalizedSamples', ProcName=ProcName, stat=StatLoc)
    NbSamples = size(Samples,2)
    i = 1
    do i = 1, NbSamples
      ii = 1
      do ii = 1, NbDim
        DistProb => This%DistProb(ii)%GetPointer()
        NormalizedSamples(ii,i) = DistProb%CDF(X=Samples(ii,i))
        nullify(DistProb)
      end do
    end do
  else
    call Error%Raise(Line='Enrichment of samples from correlated spaces with methods that require normalized, ' //           &
                                                                    'independent values not yet supported', ProcName=ProcName)
  end if

  call Sampler%Enrich(Samples=NormalizedSamples, NbEnrichmentSamples=NbEnrichmentSamples, EnrichmentSamples=EnrichmentSamples)

  deallocate(NormalizedSamples, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='NormalizedSamples', ProcName=ProcName, stat=StatLoc)

  NbEnrichSamplesLoc = size(EnrichmentSamples,2)

  if (.not. This%Correlated) then
    i = 1
    do i = 1, NbEnrichSamplesLoc
      ii = 1
      do ii = 1, NbDim
        DistProb => This%DistProb(ii)%GetPointer()
        EnrichmentSamples(ii,i) = DistProb%InvCDF(P=EnrichmentSamples(ii,i))
        nullify(DistProb)
      end do
    end do
  else
    allocate(VarR1D(NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

    i = 1
    ii = 0
    do i = 1, NbDim
      DistProb => This%DistProb(i)%GetPointer()
      select type (DistProb)
        type is (DistNorm_Type)
          VarR1D(i) = DistProb%GetMu()
          ii = ii + 1
        class default
          exit
      end select
    end do
    if (associated(DistProb)) nullify(DistProb)
    if (ii < NbDim) call Error%Raise(Line='Sampling of correlated non normal multivariate spaces is not yet supported',  &
                                                                                                          ProcName=ProcName)

    allocate(Lt, source=This%CorrMat, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Lt', ProcName=ProcName, stat=StatLoc)

    call DPOTRF('U', NbDim, Lt, NbDim, StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line='Something went wrong in DPOTRF', ProcName=ProcName)

    do i = 1, NbEnrichSamplesLoc
      call This%DrawMVarNormalLt(Mu=VarR1D, Lt=Lt, Samples=EnrichmentSamples(:,i))
    end do

    deallocate(Lt, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Lt', ProcName=ProcName, stat=StatLoc)

    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
  end if

  if (allocated(NormalizedSamples)) deallocate(NormalizedSamples, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='NormalizedSamples', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
! Draw a vector of real(rkp) random numbers from a Multivariate Normal distribution N~[Mu,Cov] where Mu is a vector of means and 
! Cov is the covariance matrix
subroutine DrawMVarNormal(Mu, Cov, Samples)

  real(rkp), dimension(:), intent(in)                                 ::    Mu
  real(rkp), dimension(:,:), intent(in)                               ::    Cov
  real(rkp), dimension(:), intent(inout)                              ::    Samples

  character(*), parameter                                             ::    ProcName='DrawMVarNormal'
  integer                                                             ::    StatLoc=0
  real(rkp), dimension(:,:), allocatable                              ::    CovLoc
  real(rkp), dimension(:), allocatable                                ::    VarR1D
!    real(rkp)                                                           ::    Rand1, Rand2
  integer                                                             ::    NbDim
  integer                                                             ::    i, ii, imax
  type(DistNorm_Type)                                                 ::    DistNormal

  if (size(Cov,1) /= size(Cov,2)) call Error%Raise(Line='Covariance matrix is not a square matrix', ProcName=ProcName)

  NbDim = size(Mu,1)
  if (size(Cov,1) /= NbDim) call Error%Raise(Line='Leading dimension of Cov larger than number of means', ProcName=ProcName)

  if (NbDim /= size(Samples,1)) call Error%Raise('Incompatible samples vector', ProcName=ProcName)

  if (any(Samples < Zero)) call Error%Raise('Values in samples vector below 0', ProcName=ProcName)
  if (any(Samples > One)) call Error%Raise('Values in samples vector above 1', ProcName=ProcName)

  allocate(CovLoc, source=Cov, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='CovLoc', ProcName=ProcName, stat=StatLoc)

  call DPOTRF('U', NbDim, CovLoc, NbDim, StatLoc)

  if (StatLoc /= 0) call Error%Raise(Line='Something went wrong in DPOTRF', ProcName=ProcName)

  allocate(VarR1D(NbDim), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  call DistNormal%Construct(Mu=Zero, Sigma=One)

  i = 1
  do i = 1, NbDim
    VarR1D(i) = DistNormal%InvCDF(P=Samples(i))
  end do

!    ! Box-Muller for independent standard normal random numbers

!    allocate(RandVal(NbDim), stat=StatLoc)
!    if (StatLoc /= 0) call Error%Allocate(Name='RandVal', ProcName=ProcName, stat=StatLoc)

!    i = 1
!    ii = 0
!    do i = 1, NbDim
!      Rand1 = This%RNG%Draw()
!      Rand2 = This%RNG%Draw()
!      ii = ii + 1
!      RandVal(ii) = dsqrt(real(-Two*dlog(Rand1),8))*dcos(real(Two*pi*Rand2,8))
!      if (ii >= NbDim) exit
!      ii = ii + 1
!      RandVal(ii) = dsqrt(real(-Two*dlog(Rand1),8))*dsin(real(Two*pi*Rand2,8))
!      if (ii >= NbDim) exit
!    end do

  i = 1
  do i = 1, NbDim
    Samples(i) = dot_product(VarR1D(1:i),CovLoc(1:i,i)) + Mu(i)
  end do

  deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  deallocate(CovLoc, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='CovLoc', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
! Draw a vector of real(rkp) random numbers from a Multivariate Normal distribution N~[Mu,Cov] where Mu is a vector of means and 
! Lt is the transpose of the cholesky factor of the covariance matrix
subroutine DrawMVarNormalLt(Mu, Lt, Samples)

  real(rkp), dimension(:), intent(in)                                 ::    Mu
  real(rkp), dimension(:,:), intent(in)                               ::    Lt
  real(rkp), dimension(:), intent(inout)                              ::    Samples

  character(*), parameter                                             ::    ProcName='DrawMVarNormalLt'
  integer                                                             ::    StatLoc=0
  real(rkp), dimension(:), allocatable                                ::    VarR1D
!    real(rkp)                                                           ::    Rand1, Rand2
  integer                                                             ::    NbDim
  integer                                                             ::    i, ii, imax
  type(DistNorm_Type)                                                 ::    DistNormal

  if (size(Lt,1) /= size(Lt,2)) call Error%Raise(Line='Covariance matrix is not a square matrix', ProcName=ProcName)

  NbDim = size(Mu,1)
  if (size(Lt,1) /= NbDim) call Error%Raise(Line='Leading dimension of Cov larger than number of means', ProcName=ProcName)

  if (NbDim /= size(Samples,1)) call Error%Raise('Incompatible samples vector', ProcName=ProcName)

  if (any(Samples < Zero)) call Error%Raise('Values in samples vector below 0', ProcName=ProcName)
  if (any(Samples > One)) call Error%Raise('Values in samples vector above 1', ProcName=ProcName)

  allocate(VarR1D(NbDim), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  call DistNormal%Construct(Mu=Zero, Sigma=One)

  i = 1
  do i = 1, NbDim
    VarR1D(i) = DistNormal%InvCDF(P=Samples(i))
  end do

!    ! Box-Muller for independent standard normal random numbers

!    allocate(RandVal(NbDim), stat=StatLoc)
!    if (StatLoc /= 0) call Error%Allocate(Name='RandVal', ProcName=ProcName, stat=StatLoc)

!    i = 1
!    ii = 0
!    do i = 1, NbDim
!      Rand1 = This%RNG%Draw()
!      Rand2 = This%RNG%Draw()
!      ii = ii + 1
!      RandVal(ii) = dsqrt(real(-Two*dlog(Rand1),8))*dcos(real(Two*pi*Rand2,8))
!      if (ii >= NbDim) exit
!      ii = ii + 1
!      RandVal(ii) = dsqrt(real(-Two*dlog(Rand1),8))*dsin(real(Two*pi*Rand2,8))
!      if (ii >= NbDim) exit
!    end do

  i = 1
  do i = 1, NbDim
    Samples(i) = dot_product(VarR1D(1:i),Lt(1:i,i)) + Mu(i)
  end do

  deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine WriteInfo(This, Directory)

  class(SampleSpace_Type), intent(in)                                 ::    This
  character(*), intent(in)                                            ::    Directory

  character(*), parameter                                             ::    ProcName='WriteInfo'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    i
  type(SMUQFile_Type)                                                 ::    File
  class(DistProb_Type), pointer                                       ::    DistProb=>null()
  character(:), allocatable                                           ::    FileName

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (len_trim(Directory) /= 0) then

    call MakeDirectory(Path=Directory, Options='-p')

    PrefixLoc = Directory

    FileName = '/variables.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Array=This%Label, File=File)

    FileName = '/covariance.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Array=This%CorrMat, File=File)

    i = 1
    do i = 1, This%NbDim
      PrefixLoc = Directory // '/' // This%Label(i)
      call MakeDirectory(Path=PrefixLoc, Options='-p')
      
      FileName = '/name.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call File%Export(String=This%ParamName(i))

      FileName = '/label.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call File%Export(String=This%Label(i))

      FileName = '/distribution.dat'
      DistProb => This%DistProb(i)%GetPointer()
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call DistProb%WriteInfo(File=File)
      nullify(DistProb)
    end do

  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
