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

module TransfSampleSpaceInt_Class

use Input_Library
use Parameters_Library
use StringRoutines_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use TransfSampleSpace_Class                                       ,only:    TransfSampleSpace_Type
use DistProb_Class                                                ,only:    DistProb_Type
use DistProb_Factory_Class                                        ,only:    DistProb_Factory
use DistProbContainer_Class                                       ,only:    DistProbContainer_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use ParamSpace_Class                                              ,only:    ParamSpace_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    TransfSampleSpaceInt_Type

type, extends(TransfSampleSpace_Type)                                 ::    TransfSampleSpaceInt_Type
  type(ParamSpace_Type)                                               ::    OrigSampleSpace
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1,         &
                                                                                                          ConstructCase2,         &
                                                                                                          ConstructCase3
  procedure, private                                                  ::    ConstructCase1
  procedure, private                                                  ::    ConstructCase2
  procedure, private                                                  ::    ConstructCase3
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Transform1D
  procedure, public                                                   ::    InvTransform1D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal=.false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(TransfSampleSpaceInt_Type), intent(inout)                   ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    if (.not. This%Initialized) then
      This%Initialized = .true.
      This%Name = 'parameter_space'
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(TransfSampleSpaceInt_Type), intent(inout)                   ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    if (allocated(This%DistProb)) deallocate(This%DistProb, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%DistProb', ProcName=ProcName, stat=StatLoc)
    This%NbDim = 0

    if (allocated(This%ParamName)) deallocate(This%ParamName, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamName', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%Label)) deallocate(This%Label, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Label', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%CorrMat)) deallocate(This%CorrMat, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%CorrMat', ProcName=ProcName, stat=StatLoc)
    This%Correlated=.false.

    This%Initialized=.false.
    This%Constructed=.false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(TransfSampleSpaceInt_Type),intent(inout)                    ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------     

  !!------------------------------------------------------------------------------------------------------------------------------         
  subroutine ConstructInput(This, Input, Prefix)

    class(TransfSampleSpaceInt_Type), intent(inout)                   ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    
    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(DistProb_Type), allocatable                                 ::    DistProb
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    ParameterName
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D_1
    real(rkp), allocatable, dimension(:)                              ::    VarR1D_2
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    integer                                                           ::    VarI0D
    character(:),  allocatable                                        ::    VarC0D
    logical                                                           ::    VarL0D
    integer                                                           ::    i, ii
    logical                                                           ::    Found
    character(:), allocatable                                         ::    SpaceType

    if (This%Constructed) call This%Reset
    if (.not. This%Initialized) call This%Initialize  

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    SectionName = 'parameters'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    This%NbDim = InputSection%GetNumberOfSubSections()

    allocate(This%ParamName(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%ParamName', ProcName=ProcName, stat=StatLoc)

    allocate(This%DistProb(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%DistProb', ProcName=ProcName, stat=StatLoc)

    allocate(This%Label(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Label', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbDim
      SubSectionName = SectionName // '>parameter' // ConvertToString(Value=i)

      ParameterName = 'name'
      call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true.)
      This%ParamName(i) = VarC0D

      ParameterName = 'label'
      call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true.)
      This%Label(i) = VarC0D

      SubSectionName = SubSectionName // '>distribution'
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call DistProb_Factory%Construct(Object=DistProb, Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
      call This%DistProb(i)%Set(Object=DistProb)
      deallocate(DistProb, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='DistProb', ProcName=ProcName, stat=StatLoc)
    end do 

    i = 1
    do i = 1, This%NbDim-1
      ii = 1
      do ii = i+1 ,This%NbDim
        if (This%Label(i) == This%Label(ii)) call Error%Raise(Line='Duplicate labels : ' // This%Label(i), ProcName=ProcName)
      end do
    end do

    SectionName = 'correlation_matrix'
    if (Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call ImportArray(Input=InputSection, Array=This%CorrMat, Prefix=PrefixLoc)
      nullify(InputSection)
      This%Correlated = .not. IsDiagonal(Array=This%CorrMat)
    else
      This%CorrMat = EyeR(N=This%NbDim)
      This%Correlated = .false.
    end if

    if (size(This%Corrmat,1) /= This%NbDim .or. size(This%CorrMat,2) /= This%NbDim) call Error%Raise( &
                                                         Line='Improper sizes for the input correlation matrix', ProcName=ProcName)

    SectionName = 'original_sample_space'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call This%OrigSampleSpace%Construct(Input=InputSection, Prefix=PrefixLoc)

    if (This%Correlated .or. This%OrigSampleSpace%IsCorrelated()) call Error%Raise('Integral sample space ' // &
                                                  'transformation is not yet implemented for correlated spaces', ProcName=ProcName)

    This%Constructed=.true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------         
  subroutine ConstructCase1(This, SampleSpace, OriginalSampleSpace)

    class(TransfSampleSpaceInt_Type), intent(inout)                   ::    This
    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    class(SampleSpace_Type), intent(in)                               ::    OriginalSampleSpace
    
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    if (This%Constructed) call This%Reset
    if (.not. This%Initialized) call This%Initialize  

    This%NbDim = SampleSpace%GetNbDim()

    allocate(This%Label(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Label', ProcName=ProcName, stat=StatLoc)
    This%Label = SampleSpace%GetLabel()

    allocate(This%ParamName(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Paramname', ProcName=ProcName, stat=StatLoc)
    This%ParamName = SampleSpace%GetName()

    allocate(This%DistProb(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%DistProb', ProcName=ProcName, stat=StatLoc)
    This%DistProb = SampleSpace%GetDistribution()

    allocate(This%CorrMat(This%NbDim,This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%CorrMat', ProcName=ProcName, stat=StatLoc)
    This%CorrMat = SampleSpace%GetCorrMat()

    This%Correlated = SampleSpace%IsCorrelated()

    call This%OrigSampleSpace%Construct(SampleSpace=OriginalSampleSpace)

    if (This%Correlated .or. This%OrigSampleSpace%IsCorrelated()) call Error%Raise('Integral sample space ' &
                                             // 'transformation is not yet implemented for correlated spaces', ProcName=ProcName)

    i = 1
    do i = 1, This%NbDim
      if (This%OrigSampleSpace%GetName(Num=i) /= This%ParamName(i)) call Error%Raise('Mismatch in names', ProcName=ProcName)
      if (This%OrigSampleSpace%GetLabel(Num=i) /= This%Label(i)) call Error%Raise('Mismatch in labels', ProcName=ProcName)
    end do

    This%Constructed=.true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------         
  subroutine ConstructCase2(This, Distributions, CorrMat, OriginalSampleSpace)

    class(TransfSampleSpaceInt_Type), intent(inout)                   ::    This
    type(DistProbContainer_Type), dimension(:), intent(in)            ::    Distributions
    class(SampleSpace_Type), intent(in)                               ::    OriginalSampleSpace
    real(rkp), dimension(:,:), optional, intent(in)                   ::    CorrMat
    
    character(*), parameter                                           ::    ProcName='ConstructCase2'
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset
    if (.not. This%Initialized) call This%Initialize  

    This%NbDim = OriginalSampleSpace%GetNbDim()

    if (This%NbDim /= size(Distributions,1)) call Error%Raise('Size of distributions and dimensionality of original sample ' //&
                                                                                         'space do not match', ProcName=ProcName)

    allocate(This%Label(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Label', ProcName=ProcName, stat=StatLoc)
    This%Label = OriginalSampleSpace%GetLabel()

    allocate(This%ParamName(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Paramname', ProcName=ProcName, stat=StatLoc)
    This%ParamName = OriginalSampleSpace%GetName()

    allocate(This%DistProb(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%DistProb', ProcName=ProcName, stat=StatLoc)
    This%DistProb = Distributions

    allocate(This%CorrMat(This%NbDim,This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%CorrMat', ProcName=ProcName, stat=StatLoc)

    if (present(CorrMat)) then
      This%CorrMat = CorrMat
      This%Correlated = IsDiagonal(Array=This%CorrMat)
    else
      call Eye(Array=This%CorrMat)
      This%Correlated = .false.
    end if

    call This%OrigSampleSpace%Construct(SampleSpace=OriginalSampleSpace)

    if (This%Correlated .or. This%OrigSampleSpace%IsCorrelated()) call Error%Raise('Integral sample space ' //                 &
                                                'transformation is not yet implemented for correlated spaces', ProcName=ProcName)

    This%Constructed=.true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------         
  subroutine ConstructCase3(This, Distributions, CorrMat, OriginalSampleSpace)

    class(TransfSampleSpaceInt_Type), intent(inout)                   ::    This
    class(DistProb_Type), dimension(:), intent(in)                    ::    Distributions
    class(SampleSpace_Type), intent(in)                               ::    OriginalSampleSpace
    real(rkp), dimension(:,:), optional, intent(in)                   ::    CorrMat
    
    character(*), parameter                                           ::    ProcName='ConstructCase2'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    if (This%Constructed) call This%Reset
    if (.not. This%Initialized) call This%Initialize  

    This%NbDim = OriginalSampleSpace%GetNbDim()

    if (This%NbDim /= size(Distributions,1)) call Error%Raise('Size of distributions and dimensionality of original sample ' //&
                                                                                         'space do not match', ProcName=ProcName)

    allocate(This%DistProb(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%DistProb', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbDim
      call This%DistProb(i)%Set(Object=Distributions(i))
    end do

    allocate(This%Label(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Label', ProcName=ProcName, stat=StatLoc)
    This%Label = OriginalSampleSpace%GetLabel()

    allocate(This%ParamName(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Paramname', ProcName=ProcName, stat=StatLoc)
    This%ParamName = OriginalSampleSpace%GetName()

    allocate(This%CorrMat(This%NbDim,This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%CorrMat', ProcName=ProcName, stat=StatLoc)

    if (present(CorrMat)) then
      This%CorrMat = CorrMat
      This%Correlated = IsDiagonal(Array=This%CorrMat)
    else
      call Eye(Array=This%CorrMat)
      This%Correlated = .false.
    end if

    call This%OrigSampleSpace%Construct(SampleSpace=OriginalSampleSpace)

    if (This%Correlated .or. This%OrigSampleSpace%IsCorrelated()) call Error%Raise('Integral sample space ' //                 &
                                                'transformation is not yet implemented for correlated spaces', ProcName=ProcName)

    This%Constructed=.true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput

    class(TransfSampleSpaceInt_Type), intent(in)                      ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(DistProb_Type), pointer                                     ::    DistProb=>null()
    character(:), allocatable                                         ::    FileName
    integer                                                           ::    i
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    type(SMUQFile_Type)                                               ::    File

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    if (ExternalFlag) call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    SectionName = 'parameters'
    call GetInput%AddSection(SectionName=SectionName)
    i = 1
    do i = 1, This%NbDim
      SubSectionName = 'parameter' // ConvertToString(Value=i)
      call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
      SubSectionName= SectionName // '>' // SubSectionName
      call GetInput%AddParameter(Name='name', Value=This%ParamName(i)%Get(), SectionName=SubSectionName)
      call GetInput%AddParameter(Name='label', Value=This%Label(i)%Get(), SectionName=SubSectionName)
      DistProb => This%DistProb(i)%GetPointer()
      if (ExternalFlag) DirectorySub = DirectoryLoc // '/distribution' // ConvertToString(i)
      call GetInput%AddSection(Section=DistProb_Factory%GetObjectInput(Object=DistProb, Name='distribution',         &
                                                        Prefix=PrefixLoc, Directory=DirectorySub), To_SubSection=SubSectionName)
      nullify(DistProb)
    end do

    SectionName='correlation_matrix'
    call GetInput%AddSection(SectionName=SectionName)
    call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    if (ExternalFlag) then
      FileName = DirectoryLoc // '/correlation_matrix.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call ExportArray(Input=InputSection, Array=This%CorrMat, File=File)
    else
      call ExportArray(Input=InputSection, Array=This%CorrMat)
    end if
    nullify(InputSection)

    if (ExternalFlag) DirectorySub = DirectoryLoc // '/original_sample_space'
    SectionName = 'original_sample_space'
    call GetInput%AddSection(Section=This%OrigSampleSpace%GetInput(Name=SectionName, Prefix=PrefixLoc,                &
                                                                                                        Directory=DirectorySub))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Transform1D(This, X)
    
    real(rkp), allocatable, dimension(:)                              ::    Transform1D

    class(TransfSampleSpaceInt_Type), intent(inout)                   ::    This
    real(rkp), dimension(:), intent(in)                               ::    X

    character(*), parameter                                           ::    ProcName='Transform1D'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    class(DistProb_Type), pointer                                     ::    Distribution=>null()
    class(DistProb_Type), pointer                                     ::    OrigDistribution=>null()

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    allocate(Transform1D, mold=X, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Transform1D', ProcName=ProcName, stat=StatLoc)

    do i = 1, This%NbDim
      Distribution => This%DistProb(i)%GetPointer()
      OrigDistribution => This%OrigSampleSpace%GetDistributionPointer(Label=This%Label(i))
      Transform1D(i) = Distribution%InvCDF(OrigDistribution%CDF(X(i)))
      nullify(Distribution)
      nullify(OrigDistribution)
    end do 

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvTransform1D(This, Z)
    
    real(rkp), allocatable, dimension(:)                              ::    InvTransform1D

    class(TransfSampleSpaceInt_Type), intent(inout)                   ::    This
    real(rkp), dimension(:), intent(in)                               ::    Z

    character(*), parameter                                           ::    ProcName='InvTransform1D'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    class(DistProb_Type), pointer                                     ::    Distribution=>null()
    class(DistProb_Type), pointer                                     ::    OrigDistribution=>null()

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    allocate(InvTransform1D, mold=Z, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='InvTransform1D', ProcName=ProcName, stat=StatLoc)

    do i = 1, This%NbDim
      Distribution => This%DistProb(i)%GetPointer()
      OrigDistribution => This%OrigSampleSpace%GetDistributionPointer(Label=This%Label(i))
      InvTransform1D(i) = OrigDistribution%InvCDF(Distribution%CDF(Z(i)))
    end do 

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(TransfSampleSpaceInt_Type), intent(out)                     ::    LHS
    class(SampleSpace_Type), intent(in)                               ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    select type (RHS)

      type is (TransfSampleSpaceInt_Type)
        call LHS%Reset()

        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        
        if (RHS%Constructed) then
          LHS%NbDim = RHS%NbDim
          allocate(LHS%DistProb, source=RHS%DistProb, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%DistProb', ProcName=ProcName, stat=StatLoc)
          allocate(LHS%ParamName, source=RHS%ParamName, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%Paramname', ProcName=ProcName, stat=StatLoc)
          LHS%Correlated = RHS%Correlated
          allocate(LHS%CorrMat, source=RHS%CorrMat, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='This%CorrMat', ProcName=ProcName, stat=StatLoc)
          allocate(LHS%Label, source=RHS%Label, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%Label', ProcName=ProcName, stat=StatLoc)
          LHS%OrigSampleSpace = RHS%OrigSampleSpace
        end if

      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(TransfSampleSpaceInt_Type),intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if (allocated(This%CorrMat)) deallocate(This%CorrMat, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%CorrMat', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%ParamName)) deallocate(This%ParamName, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamName', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%DistProb)) deallocate(This%DistProb, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%DistProb', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%Label)) deallocate(This%Label, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Label', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
