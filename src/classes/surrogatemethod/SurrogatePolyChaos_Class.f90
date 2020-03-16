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

module SurrogatePolyChaos_Class

use Input_Library
use Parameters_Library
use ArrayRoutines_Module
use ArrayIORoutines_Module
use String_Library
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SurrogateMethod_Class                                         ,only:    SurrogateMethod_Type
use OrthoPoly_Class                                               ,only:    OrthoPoly_Type
use OrthoPolyContainer_Class                                      ,only:    OrthoPolyContainer_Type
use OrthoPoly_Factory_Class                                       ,only:    OrthoPoly_Factory
use OrthoMultiVar_Class                                           ,only:    OrthoMultiVar_Type
use OrthoNumerical_Class                                          ,only:    OrthoNumerical_Type
use OrthoLegendre_Class                                           ,only:    OrthoLegendre_Type
use OrthoLaguerre_Class                                           ,only:    OrthoLaguerre_Type
use OrthoHermite_Class                                            ,only:    OrthoHermite_Type
use IndexSetScheme_Class                                          ,only:    IndexSetScheme_Type
use PolyChaosMethod_Factory_Class                                 ,only:    PolyChaosMethod_Factory
use PolyChaosMethod_Class                                         ,only:    PolyChaosMethod_Type
use Response_Class                                                ,only:    Response_Type
use PolyChaosModel_Class                                          ,only:    PolyChaosModel_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use TransfSampleSpace_Class                                       ,only:    TransfSampleSpace_Type
use TransfSampleSpace_Factory_Class                               ,only:    TransfSampleSpace_Factory
use TransfSampleSpaceInt_Class                                    ,only:    TransfSampleSpaceInt_Type
use TransfSampleSpaceNone_Class                                   ,only:    TransfSampleSpaceNone_Type
use Model_Class                                                   ,only:    Model_Type
use ModelTransform_Class                                          ,only:    ModelTransform_Type
use ModelInterface_Class                                          ,only:    ModelInterface_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use LinkedList1D_Class                                            ,only:    LinkedList1D_Type
use LinkedList2D_Class                                            ,only:    LinkedList2D_Type
use DistProb_Class                                                ,only:    DistProb_Type
use DistProbContainer_Class                                       ,only:    DistProbContainer_Type
use DistUnif_Class                                                ,only:    DistUnif_Type
use DistLogUnif_Class                                             ,only:    DistLogUnif_Type
use DistLog10Unif_Class                                           ,only:    DistLog10Unif_Type
use DistNorm_Class                                                ,only:    DistNorm_Type
use DistLogNorm_Class                                             ,only:    DistLogNorm_Type
use DistLog10Norm_Class                                           ,only:    DistLog10Norm_Type
use DistGamma_Class                                               ,only:    DistGamma_Type
use List2D_Class                                                  ,only:    List2D_Type

implicit none

private

public                                                                ::    SurrogatePolyChaos_Type

type, extends(SurrogateMethod_Type)                                   ::    SurrogatePolyChaos_Type
  type(IndexSetScheme_Type)                                           ::    IndexSetScheme
  class(PolyChaosMethod_Type), allocatable                            ::    PolyChaosMethod
  logical                                                             ::    Silent=.false.
  character(:), allocatable                                           ::    BasisScheme
  type(String_Type), allocatable, dimension(:)                        ::    InputSamplesLabels
  logical                                                             ::    InputSamplesTransform
  real(rkp), allocatable, dimension(:,:)                              ::    InputSamples
  type(List2D_Type), allocatable, dimension(:)                        ::    OutputSamples
  type(String_Type), allocatable, dimension(:)                        ::    OutputSamplesLabels
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, public                                                   ::    ConstructInput
  procedure, nopass, private                                          ::    ConstructAskeyScheme
  procedure, nopass, private                                          ::    ConstructWeinerScheme
  procedure, nopass, private                                          ::    ConstructAskeyNumericalScheme
  procedure, nopass, private                                          ::    ConstructAskeyNumericalExtendedScheme
  procedure, nopass, private                                          ::    ConstructNumericalScheme
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run
  procedure, public                                                   ::    WriteOutput
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type                                                                

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(SurrogatePolyChaos_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    integer(8)                                                        ::    SysTimeCount

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'polychaos'

      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(SurrogatePolyChaos_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%PolyChaosMethod) ) deallocate(This%PolyChaosMethod, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%PolyChaosMethod', ProcName=ProcName, stat=StatLoc )

    call This%IndexSetScheme%Reset()

    if ( allocated(This%InputSamples) ) deallocate(This%InputSamples, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%InputSamples', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%OutputSamples) ) deallocate(This%OutputSamples, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%OutputSamples', ProcName=ProcName, stat=StatLoc )

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(SurrogatePolyChaos_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%BasisScheme = 'numerical'
    This%SectionChain = ''
    This%InputSamplesTransform = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput ( This, Input, SectionChain, Prefix )

    class(SurrogatePolyChaos_Type), intent(inout)                     ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    logical                                                           ::    VarL0D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    real(rkp), pointer, dimension(:,:)                                ::    VarR2DPointer=>null()
    integer                                                           ::    i
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbSamples
    integer                                                           ::    NbOutputs

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    This%SectionChain = SectionChain

    ParameterName= 'silent'
    call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Silent=VarL0D

    ParameterName = 'basis_scheme'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%BasisScheme = VarC0D

    SectionName = 'index_set_scheme'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call This%IndexSetScheme%Construct( Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )
    else
      call This%IndexSetScheme%Construct()
    end if

    SectionName = 'method'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call PolyChaosMethod_Factory%Construct( Object=This%PolyChaosMethod, Input=InputSection,                                      &
                                                                   SectionChain=This%SectionChain // '>method', Prefix=PrefixLoc )
    nullify( InputSection )

    SectionName = 'initial_samples'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then

      SubSectionName = SectionName // '>input'

      ParameterName = 'transform'
      call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.false., Found=Found )
      if ( Found ) This%InputSamplesTransform=VarL0D

      ParameterName = 'labels'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      allocate(This%InputSamplesLabels, source=ConvertToStrings(Value=VarC0D), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%InputSamplesLabels', ProcName=ProcName, stat=StatLoc )

      SubSectionName = SubSectionName // '>values'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=VarR2D, Prefix=PrefixLoc )
      nullify( InputSection )
      This%InputSamples = VarR2D
      deallocate(VarR2D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
      if ( size(This%InputSamples,1) /= size(This%InputSamplesLabels) ) call Error%Raise(                                         &
                                                          'Mismatch in the number of inputs and input labels', ProcName=ProcName )

      SubSectionName = SectionName // '>output'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      NbOutputs = InputSection%GetNumberOfSubSections()
      nullify( InputSection )

      allocate(This%OutputSamplesLabels(NbOutputs), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%OutputSamplesLabels', ProcName=ProcName, stat=StatLoc )

      allocate(This%OutputSamples(NbOutputs), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%OutputSamples', ProcName=ProcName, stat=StatLoc )

      i = 1
      do i = 1, NbOutputs
        SubSectionName = SectionName // '>output>response' // ConvertToString(Value=i)

        ParameterName = 'label'
        call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
        This%OutputSamplesLabels(i) = VarC0D

        SubSectionName = SubSectionName // '>values'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ImportArray( Input=InputSection, Array=VarR2D, Prefix=PrefixLoc, RowMajor=.true. )
        nullify( InputSection )
        call This%OutputSamples(i)%Set(Values=VarR2D)
        if ( size(VarR2D,1) /= size(This%InputSamples,2) ) call Error%Raise( 'Number of output samples does not match number' //  &
                                                                                          ' of input samples', ProcName=ProcName )
        deallocate(VarR2D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
      end do

    end if

    This%Constructed = .true.

  end subroutine 
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )
    
    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(SurrogatePolyChaos_Type), intent(inout)                     ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput%AddParameter( Name='silent', Value=ConvertToString( Value=This%Silent ) )

    call GetInput%AddParameter( Name='basis_scheme', Value=This%BasisScheme )

    SectionName = 'index_set_scheme'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/index_set_scheme'
    call GetInput%AddSection( This%IndexSetScheme%GetInput(MainSectionName=SectionName, Prefix=PrefixLoc,                         &
                                                                                                        Directory=DirectorySub ) )

    SectionName = 'method'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/method'
    call GetInput%AddSection( Section=PolyChaosMethod_Factory%GetObjectInput( Object=This%PolyChaosMethod,                        &
                                                         MainSectionName=SectionName, Prefix=PrefixLoc, Directory=DirectorySub ) )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run( This, SampleSPace, Responses, Model, SurrogateModel, OutputDirectory )

    class(SurrogatePolyChaos_Type), intent(inout)                     ::    This
    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    class(Model_Type), intent(inout)                                  ::    Model
    class(Model_Type), allocatable, dimension(:),optional,intent(out) ::    SurrogateModel
    character(*), optional, intent(in)                                ::    OutputDirectory

    character(*), parameter                                           ::    ProcName='Run'
    integer                                                           ::    StatLoc=0
    class(TransfSampleSpace_Type), allocatable                        ::    SpaceTransform
    type(ModelTransform_Type)                                         ::    ModelTransform
    type(OrthoMultiVar_Type)                                          ::    Basis
    type(LinkedList0D_Type), allocatable, dimension(:)                ::    CVErrors
    type(LinkedList1D_Type), allocatable, dimension(:)                ::    Coefficients
    type(LinkedList2D_Type), allocatable, dimension(:)                ::    Indices
    type(PolyChaosModel_Type), dimension(:), allocatable              ::    PolyChaosModelLoc
    character(:), allocatable                                         ::    OutputDirectoryLoc
    real(rkp), allocatable, dimension(:,:)                            ::    InputSamplesLoc
    type(List2D_Type), allocatable, dimension(:)                      ::    OutputSamplesLoc
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    real(rkp), allocatable, dimension(:)                              ::    X
    character(:), allocatable                                         ::    Line

    OutputDirectoryLoc = ''

    select case (This%BasisScheme)
      case('weiner')
        call This%ConstructWeinerScheme( SampleSpace, Basis, SpaceTransform )
      case('askey')
        call This%ConstructAskeyScheme( SampleSpace, Basis, SpaceTransform )
      case('askey_numerical')
        call This%ConstructAskeyNumericalScheme( SampleSpace, Basis, SpaceTransform )
      case('askey_numerical_extended')
        call This%ConstructAskeyNumericalExtendedScheme( SampleSpace, Basis, SpaceTransform )
      case('numerical')
        call This%ConstructNumericalScheme( SampleSpace, Basis, SpaceTransform )
      case default
        call Error%Raise( Line='Unrecognized orthogonal polynomial basis scheme: ' // This%BasisScheme, ProcName=ProcName )
    end select

    call ModelTransform%Construct( SpaceTransform=SpaceTransform, Model=Model )

    if ( present(OutputDirectory) ) OutputDirectoryLoc = OutputDirectory // '/solver'

    if ( allocated(This%InputSamples) ) then
      allocate(InputSamplesLoc(SpaceTransform%GetNbDim(),size(This%InputSamples,2)), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='InputSamplesLoc', ProcName=ProcName, stat=StatLoc )
      i = 1
      do i = 1, SpaceTransform%GetNbDim()
        ii = 1
        iii = 0
        do ii = 1, size(This%InputSamplesLabels,1)
          if ( SpaceTransform%GetLabel(Num=i) == This%InputSamplesLabels(ii)%GetValue() ) then
            iii = ii
            exit
          end if
        end do
        if ( iii == 0 ) call Error%Raise( 'Did not find a corresponding label in the samples :' // SpaceTransform%GetLabel(Num=i),&
                                                                                                               ProcName=ProcName )
        InputSamplesLoc(i,:) = This%InputSamples(iii,:)
      end do

      if ( This%InputSamplesTransform ) then
        i = 1
        do i = 1, size(InputSamplesLoc,2)
          X = InputSamplesLoc(:,i)
          InputSamplesLoc(:,i) = SpaceTransform%Transform( X=X )
        end do
        deallocate(X, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='X', ProcName=ProcName, stat=StatLoc )
      end if

      allocate(OutputSamplesLoc(size(This%OutputSamples,1)), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='OutputSamplesLoc', ProcName=ProcName, stat=StatLoc )

      i = 1
      do i = 1, size(Responses,1)
        ii = 1
        iii = 0
        do ii = 1, size(This%OutputSamples)
          if ( Responses(i)%GetLabel() == This%OutputSamplesLabels(ii)%GetValue() ) then
            iii = ii
            exit
          end if
        end do
        if ( iii == 0 ) call Error%Raise( 'Did not find matching label for initial output samples :' // Responses(i)%GetLabel(),  &
                                                                                                               ProcName=ProcName )
        OutputSamplesLoc(i) = This%OutputSamples(iii)
      end do
      if ( present(OutputDirectory) ) then
        call This%PolyChaosMethod%BuildModel( Basis=Basis, SampleSpace=SpaceTransform, Responses=Responses, Model=ModelTransform, &
             IndexSetScheme=This%IndexSetScheme, Coefficients=Coefficients, Indices=Indices, CVErrors=CVErrors,                   &
             OutputDirectory=OutputDirectoryLoc, InputSamples=InputSamplesLoc, OutputSamples=OutputSamplesLoc )
      else
        call This%PolyChaosMethod%BuildModel( Basis=Basis, SampleSpace=SpaceTransform, Responses=Responses, Model=ModelTransform, &
             IndexSetScheme=This%IndexSetScheme, Coefficients=Coefficients, Indices=Indices, CVErrors=CVErrors,                   &
             InputSamples=InputSamplesLoc, OutputSamples=OutputSamplesLoc )
      end if
      deallocate(InputSamplesLoc, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='InputSamplesLoc', ProcName=ProcName, stat=StatLoc )
      deallocate(OutputSamplesLoc, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='OutputSamplesLoc', ProcName=ProcName, stat=StatLoc )
    else
      if ( present(OutputDirectory) ) then
        call This%PolyChaosMethod%BuildModel( Basis=Basis, SampleSpace=SpaceTransform, Responses=Responses, Model=ModelTransform, &
             IndexSetScheme=This%IndexSetScheme, Coefficients=Coefficients, Indices=Indices, CVErrors=CVErrors,                   &
             OutputDirectory=OutputDirectoryLoc )
      else
        call This%PolyChaosMethod%BuildModel( Basis=Basis, SampleSpace=SpaceTransform, Responses=Responses, Model=ModelTransform, &
             IndexSetScheme=This%IndexSetScheme, Coefficients=Coefficients, Indices=Indices, CVErrors=CVErrors )
      end if
    end if

    allocate(PolyChaosModelLoc(size(Responses,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='PolyChaosModelLoc', ProcName=ProcName, stat=StatLoc )

    if ( .not. This%Silent ) then
      Line = 'Building polynomial chaos model input packages'
      write(*,'(A)') ''
      write(*,'(A)') Line
    end if

    i = 1
    do i = 1, size(Responses,1)
      call PolyChaosModelLoc(i)%Construct( Response=Responses(i), TransformedSpace=SpaceTransform, Basis=Basis,                   &
                                                          Coefficients=Coefficients(i), Indices=Indices(i), CVErrors=CVErrors(i) )
      if ( present(OutputDirectory) ) then
        if ( .not. This%Silent ) then
          Line = 'Writing contents of polynomial chaos model and other files for postprocessing for response : ' //               &
                                                                                                           Responses(i)%GetLabel()
          write(*,'(A)') ''
          write(*,'(A)') Line
        end if
        OutputDirectoryLoc = OutputDirectory // '/pce_models/' // Responses(i)%GetLabel()
        call This%WriteOutput( PolyChaosModel=PolyChaosModelLoc(i), Directory=OutputDirectoryLoc )
      end if
      call Coefficients(i)%Purge()
      call Indices(i)%Purge()
      call CVErrors(i)%Purge()
    end do

    deallocate(Coefficients, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Coefficients', ProcName=ProcName, stat=StatLoc )
    deallocate(Indices, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Indices', ProcName=ProcName, stat=StatLoc )
    deallocate(CVErrors, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='CVErrors', ProcName=ProcName, stat=StatLoc )

    if ( present(SurrogateModel) ) then
      allocate(SurrogateModel, source=PolyChaosModelLoc, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='SurrogateModel', ProcName=ProcName, stat=StatLoc )
    end if

    deallocate(PolyChaosModelLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='PolyChaosModelLoc', ProcName=ProcName, stat=StatLoc )

    deallocate(SpaceTransform, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='SpaceTransform', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteOutput( This, PolyChaosModel, Directory )

    use CommandRoutines_Module
    use StringRoutines_Module
    use ArrayRoutines_Module
    use SMUQFile_Class                                            ,only:    SMUQFile_Type

    class(SurrogatePolyChaos_Type), intent(inout)                     ::    This
    type(PolyChaosModel_Type), intent(inout)                          ::    PolyChaosModel
    character(*), intent(in)                                          ::    Directory

    character(*), parameter                                           ::    ProcName='WriteOutput'
    type(InputSection_Type)                                           ::    Input
    character(:), allocatable                                         ::    FileName
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    logical                                                           ::    SilentLoc
    character(:), allocatable                                         ::    Line
    integer                                                           ::    IOLoc
    integer                                                           ::    UnitLoc
    integer                                                           ::    NbManagers
    integer                                                           ::    NbCells
    type(SMUQFile_Type)                                               ::    File
    integer                                                           ::    i, ii

    if ( len_trim(Directory) /= 0 ) then

      call MakeDirectory( Path=Directory, Options='-p' )

      SilentLoc = This%Silent

      PrefixLoc = Directory // '/PCModelPackage'
      DirectoryLoc = '/PCModelInput'

      Input = PolyChaosModel%GetInput( MainSectionName='polychaos_model', Prefix=PrefixLoc, Directory=DirectoryLoc)
      
      FileName = '/PCModelInput.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      call Input%Write( FileUnit=UnitLoc )
      call File%Close()

    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructAskeyScheme( SampleSpace, Basis, SpaceTransform )

    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(OrthoMultiVar_Type), intent(out)                             ::    Basis
    class(TransfSampleSpace_Type), allocatable, intent(out)           ::    SpaceTransform

    character(*), parameter                                           ::    ProcName='ConstructAskeyScheme'
    integer                                                           ::    StatLoc=0
    class(DistProb_Type), allocatable                                 ::    DistProb
    type(DistProbContainer_Type), allocatable, dimension(:)           ::    DistProbVec
    class(DistProb_Type), pointer                                     ::    DistProbPointer
    class(OrthoPoly_Type), allocatable                                ::    OrthoPoly
    type(OrthoPolyContainer_Type), allocatable, dimension(:)          ::    OrthoPolyVec
    integer                                                           ::    NbDim=0
    integer                                                           ::    i

    NbDim = SampleSpace%GetNbDim()

    allocate( TransfSampleSpaceInt_Type :: SpaceTransform, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SpaceTransform', ProcName=ProcName, stat=StatLoc )
    
    allocate(OrthoPolyVec(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

    allocate(DistProbVec(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='DistProbVec', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbDim
      DistProbPointer => SampleSpace%GetDistributionPointer(Num=i)

      select type ( Object => DistProbPointer )

        class is ( DistUnif_Type )
          allocate(DistUnif_Type :: DistProb, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='DistProb', ProcName=ProcName, stat=StatLoc )
          allocate( OrthoLegendre_Type :: OrthoPoly, stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )

          select type( Object2 => DistProb  )
            type is (DistUnif_Type)
              call Object2%Construct( A=-One, B=One)
            class default
              call Error%Raise( Line='Something went wrong', ProcName=ProcName )
          end select

          select type ( Object2 => OrthoPoly )
            type is ( OrthoLegendre_Type )
              call Object2%Construct( Normalized=.true. )
            class default
              call Error%Raise( Line='Something went wrong', ProcName=ProcName )
          end select

        class is ( DistGamma_Type )
          allocate(DistGamma_Type :: DistProb, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='DistProb', ProcName=ProcName, stat=StatLoc )
          allocate( OrthoLaguerre_Type :: OrthoPoly, stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )

          select type( Object2 => DistProb  )
            type is (DistGamma_Type)
              call Object2%Construct( Alpha=Zero, Beta=One)
            class default
              call Error%Raise( Line='Something went wrong', ProcName=ProcName )
          end select

          select type ( Object2 => OrthoPoly )
            type is ( OrthoLaguerre_Type )
              call Object2%Construct( Normalized=.true. )
            class default
              call Error%Raise( Line='Something went wrong', ProcName=ProcName )
          end select

        class default
          allocate(DistNorm_Type :: DistProb, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='DistProb', ProcName=ProcName, stat=StatLoc )
          allocate( OrthoHermite_Type :: OrthoPoly, stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )

          select type( Object2 => DistProb  )
            type is (DistNorm_Type)
              call Object2%Construct( Mu=Zero, Sigma=One)
            class default
              call Error%Raise( Line='Something went wrong', ProcName=ProcName )
          end select

          select type ( Object2 => OrthoPoly )
            type is ( OrthoHermite_Type )
              call Object2%Construct( Normalized=.true. )
            class default
              call Error%Raise( Line='Something went wrong', ProcName=ProcName )
          end select

      end select

      call DistProbVec(i)%Set( Object=DistProb )
      call OrthoPolyVec(i)%Set( Object=OrthoPoly )

      nullify(DistProbPointer)
      deallocate(OrthoPoly, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )
      deallocate(DistProb, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='DistProb', ProcName=ProcName, stat=StatLoc )

    end do

    call Basis%Construct( Polynomials=OrthoPolyVec  )

    select type ( Object => SpaceTransform )
      type is (TransfSampleSpaceInt_Type)
        call Object%Construct( Distributions=DistProbVec, OriginalSampleSpace=SampleSpace )
      class default
        call Error%Raise( Line='Something went wrong', ProcName=ProcName )
    end select

    deallocate(DistProbVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='DistProbVec', ProcName=ProcName, stat=StatLoc )

    deallocate(OrthoPolyVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructWeinerScheme( SampleSpace, Basis, SpaceTransform )

    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(OrthoMultiVar_Type), intent(out)                             ::    Basis
    class(TransfSampleSpace_Type), allocatable, intent(out)           ::    SpaceTransform

    character(*), parameter                                           ::    ProcName='ConstructWeinerScheme'
    integer                                                           ::    StatLoc=0
    class(DistProb_Type), allocatable                                 ::    DistProbPointer
    class(OrthoPoly_Type), allocatable                                ::    OrthoPoly
    type(OrthoPolyContainer_Type), allocatable, dimension(:)               ::    OrthoPolyVec
    type(DistNorm_Type), allocatable, dimension(:)                    ::    DistNormal
    integer                                                           ::    NbDim=0
    integer                                                           ::    i

    NbDim = SampleSpace%GetNbDim()

    allocate( TransfSampleSpaceInt_Type :: SpaceTransform, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SpaceTransform', ProcName=ProcName, stat=StatLoc )

    allocate(OrthoPolyVec(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

    allocate( OrthoHermite_Type :: OrthoPoly, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )

    select type ( Object => OrthoPoly )
      type is ( OrthoHermite_Type )
        call Object%Construct( Normalized=.true. )
      class default
        call Error%Raise( Line='Something went wrong', ProcName=ProcName )
    end select

    allocate(DistNormal(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='DistNormal', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbDim
      call OrthoPolyVec(i)%Set( Object=OrthoPoly )
      call DistNormal(i)%Construct( Mu=Zero, Sigma=One )
    end do

    call Basis%Construct( Polynomials=OrthoPolyVec  )
  
    select type ( Object => SpaceTransform )
      type is (TransfSampleSpaceInt_Type)
        call Object%Construct( Distributions=DistNormal, OriginalSampleSpace=SampleSpace )
      class default
        call Error%Raise( Line='Something went wrong', ProcName=ProcName )
    end select

    deallocate(OrthoPolyVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

    deallocate(OrthoPoly, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )

    deallocate(DistNormal, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='DistNormal', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructAskeyNumericalScheme( SampleSpace, Basis, SpaceTransform )

    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(OrthoMultiVar_Type), intent(out)                             ::    Basis
    class(TransfSampleSpace_Type), allocatable, intent(out)           ::    SpaceTransform

    character(*), parameter                                           ::    ProcName='ConstructAskeyNumericalScheme'
    integer                                                           ::    StatLoc=0
    class(DistProb_Type), allocatable                                 ::    DistProb
    type(DistProbContainer_Type), allocatable, dimension(:)                ::    DistProbVec
    class(DistProb_Type), pointer                                     ::    DistProbPointer
    class(OrthoPoly_Type), allocatable                                ::    OrthoPoly
    type(OrthoPolyContainer_Type), allocatable, dimension(:)               ::    OrthoPolyVec
    integer                                                           ::    NbDim=0
    integer                                                           ::    i

    NbDim = SampleSpace%GetNbDim()

    allocate( TransfSampleSpaceInt_Type :: SpaceTransform, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SpaceTransform', ProcName=ProcName, stat=StatLoc )
    
    allocate(OrthoPolyVec(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

    allocate(DistProbVec(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='DistProbVec', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbDim
      DistProbPointer => SampleSpace%GetDistributionPointer(Num=i)

      select type ( Object => DistProbPointer )

        type is (DistNorm_Type)
          allocate(DistNorm_Type :: DistProb, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='DistProb', ProcName=ProcName, stat=StatLoc )
          allocate( OrthoHermite_Type :: OrthoPoly, stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )

          select type( Object2 => DistProb  )
            type is (DistNorm_Type)
              call Object2%Construct( Mu=Zero, Sigma=One)
            class default
              call Error%Raise( Line='Something went wrong', ProcName=ProcName )
          end select

          select type ( Object2 => OrthoPoly )
            type is ( OrthoHermite_Type )
              call Object2%Construct( Normalized=.true. )
            class default
              call Error%Raise( Line='Something went wrong', ProcName=ProcName )
          end select

        type is ( DistUnif_Type )
          allocate(DistUnif_Type :: DistProb, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='DistProb', ProcName=ProcName, stat=StatLoc )
          allocate( OrthoLegendre_Type :: OrthoPoly, stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )

          select type( Object2 => DistProb  )
            type is (DistUnif_Type)
              call Object2%Construct( A=-One, B=One)
            class default
              call Error%Raise( Line='Something went wrong', ProcName=ProcName )
          end select

          select type ( Object2 => OrthoPoly )
            type is ( OrthoLegendre_Type )
              call Object2%Construct( Normalized=.true. )
            class default
              call Error%Raise( Line='Something went wrong', ProcName=ProcName )
          end select

        type is ( DistGamma_Type )
          allocate(DistGamma_Type :: DistProb, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='DistProb', ProcName=ProcName, stat=StatLoc )
          allocate( OrthoLaguerre_Type :: OrthoPoly, stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )

          select type( Object2 => DistProb  )
            type is (DistGamma_Type)
              call Object2%Construct( Alpha=Zero, Beta=One)
            class default
              call Error%Raise( Line='Something went wrong', ProcName=ProcName )
          end select

          select type ( Object2 => OrthoPoly )
            type is ( OrthoLaguerre_Type )
              call Object2%Construct( Normalized=.true. )
            class default
              call Error%Raise( Line='Something went wrong', ProcName=ProcName )
          end select

        class default
          allocate(DistProb, source=DistProbPointer, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='DistProb', ProcName=ProcName, stat=StatLoc )
          allocate( OrthoNumerical_Type :: OrthoPoly, stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )

          select type ( Object2 => OrthoPoly )
            type is ( OrthoNumerical_Type )
              call Object2%Construct( Weights=DistProb, Normalized=.true. )
            class default
              call Error%Raise( Line='Something went wrong', ProcName=ProcName )
          end select

      end select

      call DistProbVec(i)%Set( Object=DistProb )
      call OrthoPolyVec(i)%Set( Object=OrthoPoly )

      nullify(DistProbPointer)
      deallocate(OrthoPoly, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )
      deallocate(DistProb, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='DistProb', ProcName=ProcName, stat=StatLoc )

    end do

    call Basis%Construct( Polynomials=OrthoPolyVec  )

    select type ( Object => SpaceTransform )
      type is (TransfSampleSpaceInt_Type)
        call Object%Construct( Distributions=DistProbVec, OriginalSampleSpace=SampleSpace )
      class default
        call Error%Raise( Line='Something went wrong when constructing askey scheme space transform', ProcName=ProcName )
    end select

    deallocate(DistProbVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='DistProbVec', ProcName=ProcName, stat=StatLoc )

    deallocate(OrthoPolyVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructAskeyNumericalExtendedScheme( SampleSpace, Basis, SpaceTransform )

    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(OrthoMultiVar_Type), intent(out)                             ::    Basis
    class(TransfSampleSpace_Type), allocatable, intent(out)           ::    SpaceTransform

    character(*), parameter                                           ::    ProcName='ConstructAskeyNumericalExtendedScheme'
    integer                                                           ::    StatLoc=0
    class(DistProb_Type), pointer                                     ::    DistProbPointer
    class(OrthoPoly_Type), allocatable                                ::    OrthoPoly
    type(OrthoPolyContainer_Type), allocatable, dimension(:)               ::    OrthoPolyVec
    integer                                                           ::    NbDim=0
    integer                                                           ::    i

    NbDim = SampleSpace%GetNbDim()

    allocate( TransfSampleSpaceNone_Type :: SpaceTransform, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SpaceTransform', ProcName=ProcName, stat=StatLoc )
    
    allocate(OrthoPolyVec(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbDim

      DistProbPointer => SampleSpace%GetDistributionPointer(Num=i)

      select type ( Object => DistProbPointer )

        type is (DistNorm_Type)
          if ( (Object%GetMu() == Zero .and. Object%GetSigma() == One) .and.                                                      &
                                               ( (.not. Object%IsTruncatedLeft()) .and. (.not. Object%IsTruncatedRight()) ) ) then
            allocate( OrthoHermite_Type :: OrthoPoly, stat=StatLoc )
            if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )
            select type ( Object2 => OrthoPoly )
              type is ( OrthoHermite_Type )
                call Object2%Construct( Normalized=.true. )
              class default
                call Error%Raise( Line='Something went wrong', ProcName=ProcName )
            end select
          else
            allocate( OrthoNumerical_Type :: OrthoPoly, stat=StatLoc )
            if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )
            select type ( Object2 => OrthoPoly )
              type is ( OrthoNumerical_Type )
                call Object2%Construct( Weights=DistProbPointer, Normalized=.true. )
              class default
                call Error%Raise( Line='Something went wrong', ProcName=ProcName )
            end select
          end if

        type is ( DistUnif_Type )

          if ( Object%GetA() == -One .and. Object%GetB() == One ) then
            allocate( OrthoLegendre_Type :: OrthoPoly, stat=StatLoc )
            if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )
            select type ( Object2 => OrthoPoly )
              type is ( OrthoLegendre_Type )
                call Object2%Construct( Normalized=.true. )
              class default
                call Error%Raise( Line='Something went wrong', ProcName=ProcName )
            end select
          else
            allocate( OrthoNumerical_Type :: OrthoPoly, stat=StatLoc )
            if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )
            select type ( Object2 => OrthoPoly )
              type is ( OrthoNumerical_Type )
                call Object2%Construct( Weights=DistProbPointer, Normalized=.true. )
              class default
                call Error%Raise( Line='Something went wrong', ProcName=ProcName )
            end select
          end if

        type is ( DistGamma_Type )
          if ( ( Object%GetA() == tiny(One) .and. ( .not. Object%IsTruncatedRight()) ) .and. Object%GetBeta()==One ) then
            allocate( OrthoLaguerre_Type :: OrthoPoly, stat=StatLoc )
            if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )
            select type ( Object2 => OrthoPoly )
              type is ( OrthoLaguerre_Type )
                call Object2%Construct( Normalized=.true., Alpha=Object%GetAlpha() )
              class default
                call Error%Raise( Line='Something went wrong', ProcName=ProcName )
            end select
          else
            allocate( OrthoNumerical_Type :: OrthoPoly, stat=StatLoc )
            if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )
            select type ( Object2 => OrthoPoly )
              type is ( OrthoNumerical_Type )
                call Object2%Construct( Weights=DistProbPointer, Normalized=.true. )
              class default
                call Error%Raise( Line='Something went wrong', ProcName=ProcName )
            end select
          end if

        class default
          allocate( OrthoNumerical_Type :: OrthoPoly, stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )

          select type ( Object2 => OrthoPoly )
            type is ( OrthoNumerical_Type )
              call Object2%Construct( Weights=DistProbPointer, Normalized=.true. )
            class default
              call Error%Raise( Line='Something went wrong', ProcName=ProcName )
          end select

      end select

      call OrthoPolyVec(i)%Set( Object=OrthoPoly )

      nullify(DistProbPointer)

      deallocate(OrthoPoly, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )

    end do

    call Basis%Construct( Polynomials=OrthoPolyVec  )

    select type ( Object => SpaceTransform )
      type is (TransfSampleSpaceNone_Type)
        call Object%Construct( OriginalSampleSpace=SampleSpace )
      class default
        call Error%Raise( Line='Something went wrong when constructing askey scheme space transform', ProcName=ProcName )
    end select

    deallocate(OrthoPolyVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructNumericalScheme( SampleSpace, Basis, SpaceTransform )

    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(OrthoMultiVar_Type), intent(out)                             ::    Basis
    class(TransfSampleSpace_Type), allocatable, intent(out)           ::    SpaceTransform

    character(*), parameter                                           ::    ProcName='ConstructNumericalScheme'
    integer                                                           ::    StatLoc=0
    class(DistProb_Type), allocatable                                 ::    DistProb
    class(DistProb_Type), pointer                                     ::    DistProbPointer
    class(OrthoPoly_Type), allocatable                                ::    OrthoPoly
    type(OrthoPolyContainer_Type), allocatable, dimension(:)               ::    OrthoPolyVec
    integer                                                           ::    NbDim=0
    integer                                                           ::    i

    NbDim = SampleSpace%GetNbDim()
    
    allocate( TransfSampleSpaceNone_Type :: SpaceTransform, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SpaceTransform', ProcName=ProcName, stat=StatLoc )

    allocate(OrthoPolyVec(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbDim
      DistProbPointer => SampleSpace%GetDistributionPointer(Num=i)

      allocate( OrthoNumerical_Type :: OrthoPoly, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )

      select type ( Object2 => OrthoPoly )
        type is ( OrthoNumerical_Type )
          call Object2%Construct( Weights=DistProbPointer, Normalized=.true. )
        class default
          call Error%Raise( Line='Something went wrong', ProcName=ProcName )
      end select

      call OrthoPolyVec(i)%Set( Object=OrthoPoly )

      deallocate(OrthoPoly, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )

    end do

    call Basis%Construct( Polynomials=OrthoPolyVec  )

    select type ( Object => SpaceTransform )
      type is (TransfSampleSpaceNone_Type)
        call Object%Construct( OriginalSampleSpace=SampleSpace )
      class default
        call Error%Raise( Line='Something went wrong when constructing askey scheme space transform', ProcName=ProcName )
    end select

    deallocate(OrthoPolyVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(SurrogatePolyChaos_Type), intent(out)                       ::    LHS
    class(SurrogateMethod_Type), intent(in)                           ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)

      type is (SurrogatePolyChaos_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%BasisScheme = RHS%BasisScheme
          LHS%Silent = RHS%Silent
          LHS%IndexSetScheme = RHS%IndexSetScheme
          allocate(LHS%PolyChaosMethod, source=RHS%PolyChaosMethod, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%PolyChaosMethod', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(SurrogatePolyChaos_Type), intent(inout)                      ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%PolyChaosMethod) ) deallocate(This%PolyChaosMethod, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%PolyChaosMethod', ProcName=ProcName, stat=StatLoc )

    call This%IndexSetScheme%Reset()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
