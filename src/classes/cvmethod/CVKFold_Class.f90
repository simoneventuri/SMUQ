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

module CVKFold_Class

  use Input_Library
  use Parameters_Library
  use StringRoutines_Module
  use StatisticsRoutines_Module
  use ComputingRoutines_Module
  use Logger_Class                                                  ,only:    Logger
  use Error_Class                                                   ,only:    Error
  use CVMethod_Class                                                ,only:    CVMethod_Type, CVFitTarget
  
  implicit none
  
  private
  
  public                                                                ::    CVKFold_Type
  
  type, extends(CVMethod_Type)                                          ::    CVKFold_Type
    integer                                                             ::    NbFolds
  contains
    procedure, public                                                   ::    Initialize
    procedure, public                                                   ::    Reset
    procedure, public                                                   ::    SetDefaults
    generic, public                                                     ::    Construct               =>    ConstructCase1
    procedure, private                                                  ::    ConstructInput
    procedure, private                                                  ::    ConstructCase1
    procedure, public                                                   ::    GetInput
    procedure, public                                                   ::    Calculate
    procedure, public                                                   ::    Copy
  end type
  
  logical   ,parameter                                                  ::    DebugGlobal = .false.
  
  contains
  
  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)
  
    class(CVKFold_Type), intent(inout)                                  ::    This
  
    character(*), parameter                                             ::    ProcName='Initialize'
  
    if (.not. This%Initialized) then
      This%Name = 'CVKFold'
      This%Initialized = .true.
      call This%SetDefaults()
    end if
  
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------
  
  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)
  
    class(CVKFold_Type), intent(inout)                                  ::    This
  
    character(*), parameter                                             ::    ProcName='Reset'
  
    This%Initialized=.false.
    This%Constructed=.false.
  
    call This%SetDefaults()
  
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------
  
  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)
  
    class(CVKFold_Type), intent(inout)                                  ::    This
  
    character(*), parameter                                             ::    ProcName='SetDefaults'
  
    This%Normalized = .true.
    This%NbFolds = 10

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------
  
  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)
  
    class(CVKFold_Type), intent(inout)                                  ::    This
    type(InputSection_Type), intent(in)                                 ::    Input
    character(*), optional, intent(in)                                  ::    Prefix
  
    character(*), parameter                                             ::    ProcName='ConstructInput'
    type(InputSection_Type), pointer                                    ::    InputSection=>null()
    logical                                                             ::    VarL0D
    integer                                                             ::    VarI0D
    character(:), allocatable                                           ::    ParameterName
    character(:), allocatable                                           ::    PrefixLoc
    logical                                                             ::    Found
  
    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix
  
    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()
  
    ParameterName = 'normalized'
    call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if(Found) This%Normalized = VarL0D
  
    ParameterName = 'nb_folds'
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if(Found) This%NbFolds = VarI0D
    if (This%NbFolds < 1) call Error%Raise('Have to specify at least 1 fold', ProcName=ProcName)

    This%Constructed = .true.
  
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------
  
  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1(This, NbFolds, Normalized)
  
    class(CVKFold_Type), intent(inout)                                  ::    This
    integer, optional, intent(in)                                       ::    NbFolds
    logical, optional, intent(in)                                       ::    Normalized
  
    character(*), parameter                                             ::    ProcName='ConstructCase1'
    type(InputSection_Type), pointer                                    ::    InputSection=>null()
    logical                                                             ::    VarL0D
    character(:), allocatable                                           ::    ParameterName
    logical                                                             ::    Found
  
    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()
  
    if(present(Normalized)) This%Normalized = Normalized
  
    if(present(NbFolds)) This%NbFolds = NbFolds
    if (This%NbFolds < 1) call Error%Raise('Have to specify at least 1 fold', ProcName=ProcName)

    This%Constructed = .true.
  
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------
  
  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)
  
    use StringRoutines_Module
  
    type(InputSection_Type)                                             ::    GetInput
  
    class(CVKFold_Type), intent(in)                                     ::    This
    character(*), intent(in)                                            ::    Name
    character(*), optional, intent(in)                                  ::    Prefix
    character(*), optional, intent(in)                                  ::    Directory
  
    character(*), parameter                                             ::    ProcName='GetInput'
    character(:), allocatable                                           ::    PrefixLoc
    character(:), allocatable                                           ::    DirectoryLoc
    character(:), allocatable                                           ::    DirectorySub
    logical                                                             ::    ExternalFlag=.false.
  
    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)
  
    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc
  
    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.
  
    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    call GetInput%AddParameter(Name='normalized', Value=ConvertToString(Value=This%Normalized))
    call GetInput%AddParameter(Name='nb_folds', Value=ConvertToString(Value=This%NbFolds))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------
  
  !!------------------------------------------------------------------------------------------------------------------------------
  function Calculate(This, Fit, FitData)
  
    real(rkp)                                                           ::    Calculate
  
    class(CVKFold_Type), intent(in)                                     ::    This
    procedure(CVFitTarget), pointer                                     ::    Fit 
    real(rkp), dimension(:), intent(in)                                 ::    FitData
  
    character(*), parameter                                             ::    ProcName='Calculate'
    integer                                                             ::    StatLoc=0
    real(rkp)                                                           ::    MSESum
    integer, allocatable, dimension(:)                                  ::    ScrambledIndices
    real(rkp), allocatable, dimension(:)                                ::    TrainingSet
    integer, allocatable, dimension(:)                                  ::    TrainingSetIndices
    real(rkp), allocatable, dimension(:)                                ::    ValidationSet
    integer, allocatable, dimension(:)                                  ::    ValidationSetIndices
    real(rkp), allocatable, dimension(:)                                ::    Residual
    integer                                                             ::    NbData
    integer                                                             ::    FoldSize
    integer                                                             ::    MaxFoldSize
    integer                                                             ::    FoldSizeLoc
    integer                                                             ::    TrainingSizeLoc
    integer                                                             ::    FoldSizeRemainder
    integer                                                             ::    i
    integer                                                             ::    iStart
    integer                                                             ::    iEnd
    integer                                                             ::    iEndRelative
    integer                                                             ::    iOffset
    real(rkp)                                                           ::    FitDataVariance
    integer                                                             ::    NbFoldsLoc
  
    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)
  
    NbFoldsLoc = This%NbFolds

    NbData = size(FitData,1)
    if (NbData < NbFoldsLoc) NbFoldsLoc = NbData
    if (NbData < 2) call Error%Raise('Need to have at least 2 data points for cross validation', ProcName=ProcName)

    FoldSize = NbData / NbFoldsLoc
    if (NbFoldsLoc == 1) FoldSize = nint(real(NbData,rkp)/Two)

    FoldSizeRemainder = mod(NbData,NbFoldsLoc)

    MaxFoldSize = FoldSize
    if (FoldSizeRemainder > 0) MaxFoldSize = FoldSize + 1
      
    allocate(TrainingSet(NbData-FoldSize), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='TrainingSet', ProcName=ProcName, stat=StatLoc)
    TrainingSet = Zero 

    allocate(TrainingSetIndices(NbData-FoldSize), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='TrainingSetIndices', ProcName=ProcName, stat=StatLoc)
    TrainingSetIndices = 0

    allocate(ValidationSet(MaxFoldSize), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='ValidationSet', ProcName=ProcName, stat=StatLoc)
    ValidationSet = Zero

    allocate(ValidationSetIndices(MaxFoldSize), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='ValidationSetIndices', ProcName=ProcName, stat=StatLoc)
    ValidationSetIndices = 0

    allocate(Residual(MaxFoldSize), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Resiidual', ProcName=ProcName, stat=StatLoc)
    Residual = Zero

    allocate(ScrambledIndices(NbData), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='ScrambledIndices', ProcName=ProcName, stat=StatLoc)
    ScrambledIndices = LinSequence(SeqStart=1, SeqEnd=NbData, Scrambled=.true.)

    iOffset = 0
    if (FoldSizeRemainder > 0) iOffset = 1
    FoldSizeRemainder = FoldSizeRemainder - iOffset

    iStart = 1
    FoldSizeLoc = FoldSize + iOffset
    iEnd = FoldSizeLoc
    TrainingSizeLoc = NbData - FoldSizeLoc

    ValidationSetIndices(1:FoldSizeLoc) = ScrambledIndices(iStart:iEnd)
    ValidationSet(1:FoldSizeLoc) = FitData(ValidationSetIndices(1:FoldSizeLoc))
    TrainingSetIndices(1:TrainingSizeLoc) = ScrambledIndices(iEnd+1:NbData)
    TrainingSet(1:TrainingSizeLoc) = FitData(TrainingSetIndices(1:TrainingSizeLoc))

    MSESum = Zero

    i = 1
    do i = 1, NbFoldsLoc

      call Fit(TrainingSet=TrainingSet(1:TrainingSizeLoc), TrainingSetIndices=TrainingSetIndices(1:TrainingSizeLoc), &
               ValidationSet=ValidationSet(1:FoldSizeLoc), ValidationSetIndices=ValidationSetIndices(1:FoldSizeLoc), &
               Residual=Residual(1:FoldSizeLoc))

      MSESum = MSESum + dot_product(Residual(1:FoldSizeLoc), Residual(1:FoldSizeLoc)) / real(FoldSizeLoc,rkp)

      if (i == NbFoldsLoc) exit

      iOffset = 0
      if (FoldSizeRemainder > 0) iOffset = 1
      FoldSizeRemainder = FoldSizeRemainder - iOffset
  
      iStart = iEnd + 1
      FoldSizeLoc = FoldSize + iOffset
      iEnd = iEnd + FoldSizeLoc
      TrainingSizeLoc = NbData - FoldSizeLoc
  
      ValidationSetIndices(1:FoldSizeLoc) = ScrambledIndices(iStart:iEnd)
      ValidationSet(1:FoldSizeLoc) = FitData(ValidationSetIndices(1:FoldSizeLoc))
      TrainingSetIndices(1:iStart-1) = ScrambledIndices(1:iStart-1)
      if (iEnd /= NbData) TrainingSetIndices(iStart:TrainingSizeLoc) = ScrambledIndices(iEnd+1:NbData)
      TrainingSet(1:TrainingSizeLoc) = FitData(TrainingSetIndices(1:TrainingSizeLoc))

    end do

    Calculate = MSESum / real(NbFoldsLoc,rkp)

    if (This%Normalized) then
      FitDataVariance = ComputeVariance(Values=FitData)
      if (FitDataVariance > Zero) then
        Calculate = Calculate / FitDataVariance
      else
        Calculate = Zero
      end if
    end if

    deallocate(TrainingSet, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='TrainingSet', ProcName=ProcName, stat=StatLoc)

    deallocate(TrainingSetIndices, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='TrainingSetIndices', ProcName=ProcName, stat=StatLoc)

    deallocate(ValidationSet, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='ValidationSet', ProcName=ProcName, stat=StatLoc)

    deallocate(ValidationSetIndices, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='ValidationSetIndices', ProcName=ProcName, stat=StatLoc)

    deallocate(Residual, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Residual', ProcName=ProcName, stat=StatLoc)

    deallocate(ScrambledIndices, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='ScrambledIndices', ProcName=ProcName, stat=StatLoc)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------
  
  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)
  
    class(CVKFold_Type), intent(out)                                    ::    LHS
    class(CVMethod_Type), intent(in)                                    ::    RHS
  
    character(*), parameter                                             ::    ProcName='Copy'
    integer                                                             ::    StatLoc=0
  
    select type (RHS)
      type is (CVKFold_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if (RHS%Constructed) then
          LHS%NbFolds = RHS%NbFolds
          LHS%Normalized = RHS%Normalized
        end if
      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)
    end select
  
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------
  
  end module
  