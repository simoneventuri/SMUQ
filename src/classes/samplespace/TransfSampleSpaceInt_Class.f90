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
use String_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use TransfSampleSpace_Class                                       ,only:    TransfSampleSpace_Type
use DistProb_Class                                                ,only:    DistProb_Type
use DistProb_Factory_Class                                        ,only:    DistProb_Factory
use DistProb_Vec_Class                                            ,only:    DistProb_Vec_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use ParamSpace_Class                                              ,only:    ParamSpace_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    TransfSampleSpaceInt_Type

type, extends(TransfSampleSpace_Type)                                 ::    TransfSampleSpaceInt_Type
  type(ParamSpace_Type), allocatable                                  ::    OrigSampleSpace
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
  subroutine Initialize( This, Debug )

    class(TransfSampleSpaceInt_Type), intent(inout)                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'parameter_space'
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(TransfSampleSpaceInt_Type), intent(inout)                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%DistProb) ) deallocate(This%DistProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%DistProb', ProcName=ProcName, stat=StatLoc )
    This%NbDim = 0

    if ( allocated(This%ParamName) ) deallocate(This%ParamName, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamName', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Label) ) deallocate(This%Label, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%CorrMat) ) deallocate(This%CorrMat, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CorrMat', ProcName=ProcName, stat=StatLoc )
    This%Correlated=.false.

    if ( allocated(This%OrigSampleSpace) ) deallocate(This%OrigSampleSpace, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%OrigSampleSpace', ProcName=ProcName, stat=StatLoc )

    This%Initialized=.false.
    This%Constructed=.false.

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(TransfSampleSpaceInt_Type),intent(inout)                    ::    This
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='SetDefaults'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------     

  !!------------------------------------------------------------------------------------------------------------------------------         
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(TransfSampleSpaceInt_Type), intent(inout)                   ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='ConstructInput'
    logical                                                           ::    DebugLoc
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
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset
    if ( .not. This%Initialized ) call This%Initialize  

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'parameters'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbDim = InputSection%GetNumberOfSubSections()

    allocate(This%ParamName(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamName', ProcName=ProcName, stat=StatLoc )

    allocate(This%DistProb(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%DistProb', ProcName=ProcName, stat=StatLoc )

    allocate(This%Label(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbDim
      SubSectionName = SectionName // '>parameter' // ConvertToString(Value=i)

      ParameterName = 'name'
      call This%ParamName(i)%Set_Value(Value='<undefined>')
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      This%ParamName(i) = VarC0D

      ParameterName = 'label'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      This%Label(i) = VarC0D

      SubSectionName = SubSectionName // '>distribution'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call DistProb_Factory%Construct( Object=DistProb, Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )
      call This%DistProb(i)%Set( Object=DistProb )
      deallocate(DistProb, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='DistProb', ProcName=ProcName, stat=StatLoc )
    end do 

    i = 1
    do i = 1, This%NbDim-1
      ii = 1
      do ii = i+1 ,This%NbDim
        if ( This%Label(i)%GetValue() == This%Label(ii)%GetValue() ) call Error%Raise( Line='Duplicate labels : ' //              &
                                                                                      This%Label(i)%GetValue(), ProcName=ProcName)
      end do
    end do

    SectionName = 'correlation_matrix'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=This%CorrMat, Prefix=PrefixLoc )
      nullify( InputSection )
      This%Correlated = .not. IsDiagonal( Array=This%CorrMat )
    else
      This%CorrMat = EyeR(N=This%NbDim)
      This%Correlated = .false.
    end if

    if ( size(This%Corrmat,1) /= This%NbDim .or. size(This%CorrMat,2) /= This%NbDim ) call Error%Raise(                           &
                                                       Line='Improper sizes for the input correlation matrix', ProcName=ProcName )

    SectionName = 'original_sample_space'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%OrigSampleSpace%Construct( Input=InputSection, Prefix=PrefixLoc )

    if ( This%Correlated .or. This%OrigSampleSpace%IsCorrelated() ) call Error%Raise( 'Integral sample space ' //                 &
                                                'transformation is not yet implemented for correlated spaces', ProcName=ProcName )

    This%Constructed=.true.

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------         
  subroutine ConstructCase1( This, SampleSpace, OriginalSampleSpace, Debug )

    class(TransfSampleSpaceInt_Type), intent(inout)                   ::    This
    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    class(SampleSpace_Type), intent(in)                               ::    OriginalSampleSpace
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset
    if ( .not. This%Initialized ) call This%Initialize  

    This%NbDim = SampleSpace%GetNbDim()

    allocate(This%Label(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )
    This%Label = SampleSpace%GetLabel()

    allocate(This%ParamName(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Paramname', ProcName=ProcName, stat=StatLoc )
    This%ParamName = SampleSpace%GetName()

    allocate(This%DistProb(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%DistProb', ProcName=ProcName, stat=StatLoc )
    This%DistProb = SampleSpace%GetDistribution()

    allocate(This%CorrMat(This%NbDim,This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%CorrMat', ProcName=ProcName, stat=StatLoc )
    This%CorrMat = SampleSpace%GetCorrMat()

    This%Correlated = SampleSpace%IsCorrelated()

    call This%OrigSampleSpace%Construct( SampleSpace=OriginalSampleSpace )

    if ( This%Correlated .or. This%OrigSampleSpace%IsCorrelated() ) call Error%Raise( 'Integral sample space ' //                 &
                                                'transformation is not yet implemented for correlated spaces', ProcName=ProcName )

    i = 1
    do i = 1, This%NbDim
      if ( This%OrigSampleSpace%GetName(Num=i) /= This%ParamName(i)%GetValue() ) call Error%Raise( 'Mismatch in names',           &
                                                                                                               ProcName=ProcName )
      if ( This%OrigSampleSpace%GetLabel(Num=i) /= This%Label(i)%GetValue() ) call Error%Raise( 'Mismatch in labels',             &
                                                                                                               ProcName=ProcName )
    end do

    This%Constructed=.true.

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------         
  subroutine ConstructCase2( This, Distributions, CorrMat, OriginalSampleSpace, Debug )

    class(TransfSampleSpaceInt_Type), intent(inout)                   ::    This
    type(DistProb_Vec_Type), dimension(:), intent(in)                 ::    Distributions
    class(SampleSpace_Type), intent(in)                               ::    OriginalSampleSpace
    real(rkp), dimension(:,:), optional, intent(in)                   ::    CorrMat
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='ConstructCase2'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset
    if ( .not. This%Initialized ) call This%Initialize  

    This%NbDim = OriginalSampleSpace%GetNbDim()

    if ( This%NbDim /= size(Distributions,1) ) call Error%Raise( 'Size of distributions and dimensionality of original sample ' //&
                                                                                         'space do not match', ProcName=ProcName )

    allocate(This%Label(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )
    This%Label = OriginalSampleSpace%GetLabel()

    allocate(This%ParamName(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Paramname', ProcName=ProcName, stat=StatLoc )
    This%ParamName = OriginalSampleSpace%GetName()

    allocate(This%DistProb(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%DistProb', ProcName=ProcName, stat=StatLoc )
    This%DistProb = Distributions

    allocate(This%CorrMat(This%NbDim,This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%CorrMat', ProcName=ProcName, stat=StatLoc )

    if ( present(CorrMat) ) then
      This%CorrMat = CorrMat
      This%Correlated = IsDiagonal(Array=This%CorrMat)
    else
      call Eye(Array=This%CorrMat)
      This%Correlated = .false.
    end if

    call This%OrigSampleSpace%Construct( SampleSpace=OriginalSampleSpace )

    if ( This%Correlated .or. This%OrigSampleSpace%IsCorrelated() ) call Error%Raise( 'Integral sample space ' //                 &
                                                'transformation is not yet implemented for correlated spaces', ProcName=ProcName )

    This%Constructed=.true.

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------         
  subroutine ConstructCase3( This, Distributions, CorrMat, OriginalSampleSpace, Debug )

    class(TransfSampleSpaceInt_Type), intent(inout)                   ::    This
    class(DistProb_Type), dimension(:), intent(in)                    ::    Distributions
    class(SampleSpace_Type), intent(in)                               ::    OriginalSampleSpace
    real(rkp), dimension(:,:), optional, intent(in)                   ::    CorrMat
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='ConstructCase2'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset
    if ( .not. This%Initialized ) call This%Initialize  

    This%NbDim = OriginalSampleSpace%GetNbDim()

    if ( This%NbDim /= size(Distributions,1) ) call Error%Raise( 'Size of distributions and dimensionality of original sample ' //&
                                                                                         'space do not match', ProcName=ProcName )

    allocate(This%DistProb(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%DistProb', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbDim
      call This%DistProb(i)%Set(Object=Distributions(i))
    end do

    allocate(This%Label(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )
    This%Label = OriginalSampleSpace%GetLabel()

    allocate(This%ParamName(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Paramname', ProcName=ProcName, stat=StatLoc )
    This%ParamName = OriginalSampleSpace%GetName()

    allocate(This%CorrMat(This%NbDim,This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%CorrMat', ProcName=ProcName, stat=StatLoc )

    if ( present(CorrMat) ) then
      This%CorrMat = CorrMat
      This%Correlated = IsDiagonal(Array=This%CorrMat)
    else
      call Eye(Array=This%CorrMat)
      This%Correlated = .false.
    end if

    call This%OrigSampleSpace%Construct( SampleSpace=OriginalSampleSpace )

    if ( This%Correlated .or. This%OrigSampleSpace%IsCorrelated() ) call Error%Raise( 'Integral sample space ' //                 &
                                                'transformation is not yet implemented for correlated spaces', ProcName=ProcName )

    This%Constructed=.true.

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(TransfSampleSpaceInt_Type), intent(in)                      ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
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

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    SectionName = 'parameters'
    call GetInput%AddSection(SectionName=SectionName)
    i = 1
    do i = 1, This%NbDim
      SubSectionName = 'parameter' // ConvertToString(Value=i)
      call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
      SubSectionName= SectionName // '>' // SubSectionName
      call GetInput%AddParameter( Name='name', Value=ConvertToString( Value=This%ParamName(i)%GetValue() ),                       &
                                                                                                      SectionName=SubSectionName )
      call GetInput%AddParameter( Name='label', Value=ConvertToString( Value=This%Label(i)%GetValue() ),                          &
                                                                                                      SectionName=SubSectionName )
      DistProb => This%DistProb(i)%GetPointer()
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/distribution' // ConvertToString(i)
      call GetInput%AddSection( Section=DistProb_Factory%GetObjectInput( Object=DistProb, MainSectionName='distribution',         &
                                                        Prefix=PrefixLoc, Directory=DirectorySub ), To_SubSection=SubSectionName )
      nullify(DistProb)
    end do

    SectionName='correlation_matrix'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    if ( ExternalFlag ) then
      FileName = DirectoryLoc // '/correlation_matrix.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Input=InputSection, Array=This%CorrMat, File=File )
    else
      call ExportArray( Input=InputSection, Array=This%CorrMat )
    end if
    nullify(InputSection)

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/original_sample_space'
    SectionName = 'original_sample_space'
    call GetInput%AddSection( Section=This%OrigSampleSpace%GetInput(MainSectionName=SectionName, Prefix=PrefixLoc,                &
                                                                                                        Directory=DirectorySub ) )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Transform1D( This, X, Debug )
    
    real(rkp), allocatable, dimension(:)                              ::    Transform1D

    class(TransfSampleSpaceInt_Type), intent(inout)                        ::    This
    real(rkp), dimension(:), intent(in)                               ::    X
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Transform1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate( Transform1D, source=X, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Transform1D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvTransform1D( This, Z, Debug )
    
    real(rkp), allocatable, dimension(:)                              ::    InvTransform1D

    class(TransfSampleSpaceInt_Type), intent(inout)                        ::    This
    real(rkp), dimension(:), intent(in)                               ::    Z
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='InvTransform1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate( InvTransform1D, source=Z, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='InvTransform1D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(TransfSampleSpaceInt_Type), intent(out)                     ::    LHS
    class(SampleSpace_Type), intent(in)                               ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type ( RHS )

      type is (TransfSampleSpaceInt_Type)
        call LHS%Reset()

        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        
        if ( RHS%Constructed ) then
          LHS%NbDim = RHS%NbDim
          allocate(LHS%DistProb, source=RHS%DistProb, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%DistProb', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%ParamName, source=RHS%ParamName, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Paramname', ProcName=ProcName, stat=StatLoc )
          LHS%Correlated = RHS%Correlated
          allocate(LHS%CorrMat, source=RHS%CorrMat, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='This%CorrMat', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%Label, source=RHS%Label, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Label', ProcName=ProcName, stat=StatLoc )
          LHS%OrigSampleSpace = RHS%OrigSampleSpace
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(TransfSampleSpaceInt_Type),intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%CorrMat) ) deallocate(This%CorrMat, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CorrMat', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamName) ) deallocate(This%ParamName, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamName', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%DistProb) ) deallocate(This%DistProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%DistProb', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Label) ) deallocate(This%Label, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
