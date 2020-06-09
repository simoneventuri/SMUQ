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

module TrajectoryDesign_Class

use Parameters_Library
use Input_Library
use ArrayIORoutines_Module
use ArrayRoutines_Module
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use RandPseudo_Class                                              ,only:    RandPseudo_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use SampleMethod_Class                                            ,only:    SampleMethod_Type
use SampleMethod_Factory_Class                                    ,only:    SampleMethod_Factory
use SampleLHS_Class                                               ,only:    SampleLHS_Type
use RandPseudo_Class                                              ,only:    RandPseudo_Type

implicit none

private

public                                                                ::    TrajectoryDesign_Type

type                                                                  ::    TrajectoryDesign_Type
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    Name
  integer                                                             ::    NbGridLevels
  integer                                                             ::    PerturbationSize
  type(RandPseudo_Type)                                               ::    RNG
  class(SampleMethod_Type), allocatable                               ::    Sampler
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Draw
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(TrajectoryDesign_Type), intent(inout)                       ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    integer(8)                                                        ::    SysTimeCount

    if (.not. This%Initialized) then
      This%Initialized = .true.
      This%Name = 'trajectorydesign'
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(TrajectoryDesign_Type), intent(inout)                       ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    call This%RNG%Reset()

    if (allocated(This%Sampler)) deallocate(This%Sampler, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Sampler', ProcName=ProcName, stat=StatLoc)

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(TrajectoryDesign_Type), intent(inout)                       ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%NbGridLevels = 0
    This%PerturbationSize = 0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput (This, Input, Prefix)

    use StringRoutines_Module

    class(TrajectoryDesign_Type), intent(inout)                       ::    This
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

    ParameterName = 'nb_grid_levels'
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) then
      This%NbGridLevels = VarI0D
      if (This%NbGridLevels <= 1) call Error%Raise('Must specify at least 2 grid levels', ProcName=ProcName)
    end if

    ParameterName = 'perturbation_size'
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) then
      This%PerturbationSize = VarI0D
      if (This%PerturbationSize <= 0) call Error%Raise('Perturbation size must be above 0', ProcName=ProcName)
    end if

    SectionName = 'sampler'
    if (Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call SampleMethod_Factory%Construct(Object=This%Sampler, Input=InputSection, Prefix=PrefixLoc)
    else
      allocate(SampleLHS_Type :: This%Sampler)
      select type (Object => This%Sampler)
        type is (SampleLHS_Type)
          call Object%Construct()
        class default
          call Error%Raise(Line='Something went wrong', ProcName=ProcName)
      end select
    end if

    SectionName = 'rng'
    if (Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%RNG%Construct(Input=InputSection, Prefix=PrefixLoc)
    else
      call This%RNG%Construct()
    end if

    This%Constructed = .true.

  end subroutine 
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1 (This, NbGridLevels, PerturbationSize, Sampler, RNG)

    class(TrajectoryDesign_Type), intent(inout)                       ::    This
    integer, optional, intent(in)                                     ::    NbGridLevels
    integer, optional, intent(in)                                     ::    PerturbationSize
    class(SampleMethod_Type), optional, intent(in)                    ::    Sampler
    type(RandPseudo_Type), optional, intent(in)                       ::    RNG

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    if (present(NbGridLevels)) then
      This%NbGridLevels = NbGridLevels
      if (This%NbGridLevels <= 1) call Error%Raise('Must specify at least 2 grid levels', ProcName=ProcName)
    end if

    if (present(PerturbationSize)) then
      This%PerturbationSize = PerturbationSize
      if (This%PerturbationSize <= 0) call Error%Raise('Perturbation size must be above zero', ProcName=ProcName)
    end if

    if (present(RNG)) then
      This%RNG = RNG
    else
      call This%RNG%Construct()
    end if

    if (present(Sampler)) then
      allocate(This%Sampler, source=Sampler, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%Sampler', ProcName=ProcName, stat=StatLoc)
    else
      allocate(SampleLHS_Type :: This%Sampler)
      select type (Object => This%Sampler)
        type is (SampleLHS_Type)
          call Object%Construct()
        class default
          call Error%Raise(Line='Something went wrong', ProcName=ProcName)
      end select
    end if

    This%Constructed = .true.

  end subroutine 
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    use CommandRoutines_Module
    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput
    class(TrajectoryDesign_Type), intent(in)                          ::    This
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

    if (This%NbGridLevels > 0) call GetInput%AddParameter(Name='nb_grid_levels',                                               &
                                                                                  Value=ConvertToString(Value=This%NbGridLevels))
    if (This%PerturbationSize > 0) call GetInput%AddParameter(Name='perturbation_size',                                        &
                                                                              Value=ConvertToString(Value=This%PerturbationSize))

    if (ExternalFlag) DirectorySub = DirectoryLoc // '/rng'
    call GetInput%AddSection(Section=This%RNG%GetInput(Name='rng', Prefix=PrefixLoc, Directory=DirectorySub))

    if (ExternalFlag) DirectorySub = DirectoryLoc // '/sampler'
    call GetInput%AddSection(Section=SampleMethod_Factory%GetObjectInput(Object=This%Sampler, Name='sampler',        &
                                                                                      Prefix=PrefixLoc, Directory=DirectorySub))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Draw(This, NbDim, NbTrajectories, Trajectories, StepSize, Indices)

    class(TrajectoryDesign_Type), intent(inout)                       ::    This
    integer, intent(in)                                               ::    NbTrajectories
    integer, intent(in)                                               ::    NbDim
    real(rkp), dimension(:,:), intent(inout)                          ::    Trajectories
    real(rkp), dimension(:,:), optional, intent(inout)                ::    StepSize
    integer, dimension(:,:), optional, intent(inout)                  ::    Indices
    
    character(*), parameter                                           ::    ProcName='Draw'
    integer                                                           ::    StatLoc=0
    integer, allocatable, dimension(:)                                ::    Signs
    integer, allocatable, dimension(:)                                ::    PermutationIndices
    integer                                                           ::    NbGridLevelsLoc
    integer                                                           ::    NbEntries
    integer                                                           ::    NbDimP1
    real(rkp), allocatable, dimension(:,:)                            ::    XStar
    real(rkp), allocatable, dimension(:,:)                            ::    B
    real(rkp), allocatable, dimension(:,:)                            ::    P
    integer                                                           ::    VarI0D
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    integer, allocatable, dimension(:,:)                              ::    VarI2D
    real(rkp)                                                         ::    GridSize
    integer                                                           ::    PerturbationSizeLoc
    real(rkp)                                                         ::    PerturbationSize
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    integer                                                           ::    iv

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    NbGridLevelsLoc = NbDim
    if (This%NbGridLevels > 0) NbGridLevelsLoc = This%NbGridLevels

    PerturbationSizeLoc = nint(real(NbGridLevelsLoc,rkp)/Two)
    if (This%PerturbationSize > 0) PerturbationSizeLoc = This%PerturbationSize
    if (PerturbationSizeLoc <= 0) call Error%Raise('Perturbation size must be above zero', ProcName=ProcName)
    if (PerturbationSizeLoc >= NbGridLevelsLoc) call Error%Raise('Perturbation size must be lower than number of grid levels', &
                                                                                                               ProcName=ProcName)

    NbEntries = (NbDim+1)*NbTrajectories
    NbDimP1 = NbDim+1

    if (present(StepSize)) then
      if (size(StepSize,1) /= NbDim .or. size(StepSize,2) /= NbTrajectories) call Error%Raise('Passed step size array of ' //  &
                                                                                             'incorrect size', ProcName=ProcName)
    end if

    if (present(Indices)) then
      if (size(Indices,1) /= NbDim .or. size(Indices,2) /= NbTrajectories) call Error%Raise('Passed indices array of ' //      &
                                                                                             'incorrect size', ProcName=ProcName)
    end if

    if (size(Trajectories,2) /= NbEntries .or. size(Trajectories,1) /= NbDim) call Error%Raise('Passed trajectories ' //       &
                                                                                    'array of incorrect size', ProcName=ProcName)

    Trajectories = Zero

    GridSize = One / real(NbGridLevelsLoc-1,rkp)
    PerturbationSize = GridSize*real(PerturbationSizeLoc,rkp)

    allocate(Signs(NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Signs', ProcName=ProcName, stat=StatLoc)
    Signs = 1
      
    allocate(PermutationIndices(NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='PermutationIndices', ProcName=ProcName, stat=StatLoc)
    PermutationIndices = 0

    allocate(P(NbDim,NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='P', ProcName=ProcName, stat=StatLoc)
    P = Zero

    allocate(B(NbDim+1,NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='B', ProcName=ProcName, stat=StatLoc)
    call StrictTriangular(Array=B, UL='L')

    VarI0D = NbGridLevelsLoc-PerturbationSizeLoc

    VarR1D = LinSequence(SeqStart=0, SeqEnd=VarI0D)
    VarR1D = VarR1D / real(VarI0D,rkp)

    call This%Sampler%Draw(NbSamples=NbTrajectories, NbDim=NbDim, Samples=VarR2D)

    allocate(VarI2D(NbDim, NbTrajectories) , stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VarI2D', ProcName=ProcName, stat=StatLoc)
    VarI2D = 0

    i = 1
    do i = 1, NbTrajectories
      ii = 1
      do ii = 1, NbDim
        iv = 0
        iii = 1
        do iii = 1, VarI0D
          if (iii < VarI0D) then
            if (VarR2D(ii,i) >= VarR1D(iii) .and. VarR2D(ii,i) < VarR1D(iii+1)) iv = iii
          else
            if (VarR2D(ii,i) >= VarR1D(iii)) iv = iii
          end if
        end do
        if (iv == 0) call Error%Raise('Something went wrong', ProcName=ProcName)
        VarI2D(ii,i) = iv - 1
      end do
    end do 

    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

    XStar = real(VarI2D,rkp) * GridSize

    deallocate(VarI2D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarI2D', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, NbTrajectories
      PermutationIndices = LinSequence(SeqStart=1, SeqEnd=NbDim)
      call ScrambleArray(Array=PermutationIndices, RNG=This%RNG)
      if (present(Indices)) Indices(:,i) = PermutationIndices
      P = Zero

      Signs = 1
      ii = 1
      do ii = 1, NbDim
        VarR0D = This%RNG%Draw()
        if (VarR0D < 0.5) Signs(ii) = - 1
        P(PermutationIndices(ii),ii) = One
      end do
      if (present(StepSize)) StepSize(:,i) = PerturbationSize * real(Signs,rkp)

      VarR2D = B*Two - One

      ! performing loop to carry out multiplication of triangular matrix by a diagonal one and addition to the multiplication 
      ! of J by x' because it is faster
      VarR0D = PerturbationSize / Two
      ii = 1
      do ii = 1, NbDim
        VarR2D(:,ii) = (VarR2D(:,ii)*Signs(ii) + One)*VarR0D + XStar(ii,i)
      end do

      call DGEMM('N', 'T', NbDim, NbDimP1, NbDim, One, P, NbDim, VarR2D, NbDimP1, Zero,                                          &
                                                                                Trajectories(:,(i-1)*NbDimP1+1:i*NbDimP1), NbDim)

    end do

    deallocate(P, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='P', ProcName=ProcName, stat=StatLoc)

    deallocate(B, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='B', ProcName=ProcName, stat=StatLoc)

    deallocate(VarR2D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)

    deallocate(XStar, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='XStar', ProcName=ProcName, stat=StatLoc)

    deallocate(Signs, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Signs', ProcName=ProcName, stat=StatLoc)

    deallocate(PermutationIndices, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='PermutationIndices', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(TrajectoryDesign_Type), intent(out)                         ::    LHS
    class(TrajectoryDesign_Type), intent(in)                          ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    call LHS%Reset()
    LHS%Initialized = RHS%Initialized
    LHS%Constructed = RHS%Constructed

    if (RHS%Constructed) then
      LHS%RNG=RHS%RNG
      LHS%Sampler = RHS%Sampler
      LHS%PerturbationSize = RHS%PerturbationSize
      LHS%NbGridLevels = RHS%NbGridLevels
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(TrajectoryDesign_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
