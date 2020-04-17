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

module SampleLHS_Class

use Parameters_Library
use Input_Library
use ArrayRoutines_Module
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SampleMethod_Class                                            ,only:    SampleMethod_Type
use RandPseudo_Class                                              ,only:    RandPseudo_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type

implicit none

private

public                                                                ::    SampleLHS_Type

type, extends(SampleMethod_Type)                                      ::    SampleLHS_Type
  type(RandPseudo_Type)                                               ::    RNG
  logical                                                             ::    MedianPoints=.false.
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    Draw_0D
  procedure, private                                                  ::    Draw_1D
  procedure, private                                                  ::    Enrich_0D
  procedure, private                                                  ::    Enrich_1D
  procedure, nopass, private                                          ::    CheckRepresentation
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(SampleLHS_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    integer(8)                                                        ::    SysTimeCount

    if (.not. This%Initialized) then
      This%Initialized = .true.
      This%Name = 'lhs'
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(SampleLHS_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    call This%RNG%Reset()

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(SampleLHS_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%MedianPoints = .false.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput (This, Input, Prefix)

    use StringRoutines_Module

    class(SampleLHS_Type), intent(inout)                              ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    logical                                                           ::    Found
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'median_points'
    call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found)
    if (Found) This%MedianPoints = VarL0D

    SectionName = 'rng'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.false., FoundSection=Found)
    if (Found) then
      call This%RNG%Construct(Input=InputSection, Prefix=PrefixLoc)
    else
      call This%RNG%Construct()
    end if

    This%Constructed = .true.

  end subroutine 
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1 (This, RNG, MedianPoints)

    class(SampleLHS_Type), intent(inout)                              ::    This
    type(RandPseudo_Type), optional, intent(in)                       ::    RNG
    logical, optional, intent(in)                                     ::    MedianPoints

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    if (present(RNG)) then
      This%RNG = RNG
    else
      call This%RNG%Construct()
    end if

    if (present(MedianPoints)) This%MedianPoints = MedianPoints

    This%Constructed = .true.

  end subroutine 
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    use CommandRoutines_Module
    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput
    class(SampleLHS_Type), intent(in)                                 ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    FileName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput%AddParameter(Name='median_points', Value=ConvertToString(Value=This%MedianPoints))

    if (ExternalFlag) DirectorySub = DirectoryLoc // '/rng'
    call GetInput%AddSection(Section=This%RNG%GetInput(Name='rng', Prefix=PrefixLoc, Directory=DirectorySub))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Draw_0D(This, NbSamples)

    real(rkp), allocatable, dimension(:)                              ::    Draw_0D

    class(SampleLHS_Type), intent(inout)                              ::    This
    integer, intent(in)                                               ::    NbSamples

    character(*), parameter                                           ::    ProcName='Draw_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    real(rkp)                                                         ::    dx

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    allocate(Draw_0D(NbSamples), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Draw_0D', ProcName=ProcName, stat=StatLoc)

    dx = One / real(NbSamples,rkp)

    i = 1
    do i = 1, NbSamples
      if (This%MedianPoints) then
        Draw_0D(i) = 0.5 *dx + real((i-1),rkp)*dx
      else
        if (i == NbSamples) then
          Draw_0D(i) = This%RNG%Draw(DrawType=1)*dx + real((i-1),rkp)*dx
        else
          Draw_0D(i) = This%RNG%Draw(DrawType=2)*dx + real((i-1),rkp)*dx
        end if
      end if
    end do

    call ScrambleArray(Array=Draw_0D, RNG=This%RNG)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Draw_1D(This, NbDim, NbSamples)

    real(rkp), allocatable, dimension(:,:)                            ::    Draw_1D

    class(SampleLHS_Type), intent(inout)                              ::    This
    integer, intent(in)                                               ::    NbDim
    integer, intent(in)                                               ::    NbSamples

    character(*), parameter                                           ::    ProcName='Draw_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i, ii
    real(rkp)                                                         ::    dx

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    if (NbDim <= 0) call Error%Raise(Line='Dimensionality of requested sample at or below 0', ProcName=ProcName)

    allocate(Draw_1D(NbDim, NbSamples), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Draw_1D', ProcName=ProcName, stat=StatLoc)

    dx = One / real(NbSamples,rkp)

    ii = 1
    do ii = 1, NbDim
      i = 1
      do i = 1, NbSamples
        if (This%MedianPoints) then
          Draw_1D(ii,i) = 0.5 *dx + real((i-1),rkp)*dx
        else
          if (i == NbSamples) then
            Draw_1D(ii,i) = This%RNG%Draw(DrawType=1)*dx + real((i-1),rkp)*dx
          else
            Draw_1D(ii,i) = This%RNG%Draw(DrawType=2)*dx + real((i-1),rkp)*dx
          end if
        end if
      end do
      call ScrambleArray(Array=Draw_1D(ii,:), RNG=This%RNG)
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Enrich_0D(This, Samples, NbEnrichmentSamples, EnrichmentSamples, ReqNormalized)

    class(SampleLHS_Type), intent(inout)                              ::    This
    real(rkp), dimension(:),intent(in)                                ::    Samples
    real(rkp), dimension(:), allocatable, intent(out)                 ::    EnrichmentSamples
    integer, intent(in)                                               ::    NbEnrichmentSamples
    logical, optional, intent(out)                                    ::    ReqNormalized

    character(*), parameter                                           ::    ProcName='Enrich_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i, ii
    logical, allocatable, dimension(:)                                ::    Representation
    integer                                                           ::    NbBins
    real(rkp)                                                         ::    dx
    integer                                                           ::    NbEnrichmentSamplesLoc

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    NbEnrichmentSamplesLoc = NbEnrichmentSamples

    if (NbEnrichmentSamplesLoc < 1) call Error%Raise(Line='Inquired less than 1 enrichment sample', ProcName=ProcName)

    if (present(ReqNormalized)) then
      ReqNormalized = .true.
    else 
      NbBins = NbEnrichmentSamplesLoc + size(Samples,1)
      dx = One / real(NbBins,rkp)

      Representation = This%CheckRepresentation(NbBins=NbBins, Array=Samples)

      NbEnrichmentSamplesLoc = count(Representation .eqv. .false.)

      allocate(EnrichmentSamples(NbEnrichmentSamplesLoc), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='EnrichmentSamples', ProcName=ProcName, stat=StatLoc)

      i = 1
      ii = 0
      do i = 1, size(Representation,1)
        if (Representation(i)) cycle
        ii = ii + 1

        if (This%MedianPoints) then
          EnrichmentSamples(ii) = 0.5 *dx + real((i-1),rkp)*dx
        else
          if (i == size(Representation,1)) then
            EnrichmentSamples(ii) = This%RNG%Draw(DrawType=1)*dx + real((i-1),rkp)*dx
          else
            EnrichmentSamples(ii) = This%RNG%Draw(DrawType=2)*dx + real((i-1),rkp)*dx
          end if
        end if
      end do

      deallocate(Representation, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Representation', ProcName=ProcName, stat=StatLoc)

      call ScrambleArray(Array=EnrichmentSamples, RNG=This%RNG)
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Enrich_1D(This, Samples, NbEnrichmentSamples, EnrichmentSamples, ReqNormalized)

    class(SampleLHS_Type), intent(inout)                              ::    This
    real(rkp), dimension(:,:),intent(in)                              ::    Samples
    real(rkp), dimension(:,:), allocatable, intent(out)               ::    EnrichmentSamples
    integer, intent(in)                                               ::    NbEnrichmentSamples
    logical, optional, intent(out)                                    ::    ReqNormalized

    character(*), parameter                                           ::    ProcName='Enrich_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i, ii, iii, iv
    logical, allocatable, dimension(:)                                ::    Representation
    integer                                                           ::    NbDim
    integer                                                           ::    NbBins
    integer                                                           ::    NbDegenerateBins
    type(LinkedList0D_Type)                                           ::    RepRecord
    integer, allocatable, dimension(:)                                ::    PermutationArray
    real(rkp)                                                         ::    dx
    integer                                                           ::    NbEnrichmentSamplesLoc

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    NbDim = size(Samples,1)
    if (NbDim <= 0) call Error%Raise(Line='Dimensionality of requested samples at or below 0', ProcName=ProcName)

    NbEnrichmentSamplesLoc = NbEnrichmentSamples

    if (NbEnrichmentSamplesLoc < 1) call Error%Raise(Line='Inquired less than 1 enrichment sample', ProcName=ProcName)

    if (present(ReqNormalized)) then
      ReqNormalized = .true.
    else 
      NbBins = NbEnrichmentSamplesLoc + size(Samples,2)
      dx = One / real(NbBins,rkp)
    
      i = 1
      do i = 1, size(Samples,1)
        Representation = This%CheckRepresentation(NbBins=NbBins, Array=Samples(i,:))
        ii = count(Representation .eqv. .false.)
        if (ii > NbEnrichmentSamplesLoc) NbEnrichmentSamplesLoc = ii
      end do

      allocate(EnrichmentSamples(NbDim, NbEnrichmentSamplesLoc), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='EnrichmentSamples', ProcName=ProcName, stat=StatLoc)

      do iii = 1, NbDim
        Representation = This%CheckRepresentation(NbBins=NbBins, Array=Samples(iii,:))
        iv = 0
        ii = 0
        do
          iv = iv + 1
          i = 1
          do i = 1, size(Representation,1)
            if (Representation(i)) cycle
            if (iv == 1) call RepRecord%Append(Value=i)
            ii = ii + 1
            if (This%MedianPoints) then
              EnrichmentSamples(iii,ii) = 0.5 *dx + real((i-1),rkp)*dx
            else
              if (i == size(Representation,1)) then
                EnrichmentSamples(iii,ii) = This%RNG%Draw(DrawType=1)*dx + real((i-1),rkp)*dx
              else
                EnrichmentSamples(iii,ii) = This%RNG%Draw(DrawType=2)*dx + real((i-1),rkp)*dx
              end if
            end if
          end do
          NbDegenerateBins = NbEnrichmentSamplesLoc - ii
          if (NbDegenerateBins < RepRecord%GetLength()) exit
        end do

        if (NbDegenerateBins > 0) then
          call RepRecord%Get(Values=PermutationArray)
          call ScrambleArray(Array=PermutationArray, RNG=This%RNG)
          i = 1
          do i = 1, NbDegenerateBins
            ii = ii + 1
            if (This%MedianPoints) then
              EnrichmentSamples(iii,ii) = 0.5 *dx + real((PermutationArray(i)-1),rkp)*dx
            else
              if (PermutationArray(i) == size(Representation,1)) then
                EnrichmentSamples(iii,ii) = This%RNG%Draw(DrawType=1)*dx + real((PermutationArray(i)-1),rkp)*dx
              else
                EnrichmentSamples(iii,ii) = This%RNG%Draw(DrawType=2)*dx + real((PermutationArray(i)-1),rkp)*dx
              end if
            end if
          end do
        end if
        call ScrambleArray(Array=EnrichmentSamples(iii,:), RNG=This%RNG)
      end do

      deallocate(Representation, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Representation', ProcName=ProcName, stat=StatLoc)
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CheckRepresentation(NbBins, Array)

    logical, allocatable, dimension(:)                                ::    CheckRepresentation
    integer, intent(in)                                               ::    NbBins
    real(rkp), dimension(:), intent(in)                               ::    Array

    character(*), parameter                                           ::    ProcName='CheckRepresentation'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    Length
    integer                                                           ::    i
    real(rkp)                                                         ::    BinMin
    real(rkp)                                                         ::    BinMax
    real(rkp)                                                         ::    dx

    Length = size(Array,1)

    allocate(CheckRepresentation(NbBins), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='CheckRepresentation', ProcName=ProcName, stat=StatLoc)

    CheckRepresentation = .false.
    dx = One / real(NbBins,rkp)

    i = 1
    do i = 1, NbBins
      BinMin = (i-1)*dx
      BinMax = i*dx
      if (i == NbBins) then
        if (any(Array >= BinMin .and. Array <= BinMax)) CheckRepresentation(i) = .true.
      else
        if (any(Array >= BinMin .and. Array < BinMax)) CheckRepresentation(i) = .true.
      end if
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(SampleLHS_Type), intent(out)                                ::    LHS
    class(SampleMethod_Type), intent(in)                              ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (SampleLHS_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          LHS%RNG=RHS%RNG
          LHS%MedianPoints = RHS%MedianPoints
        end if

      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(SampleLHS_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
