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
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SurrogateMethod_Class                                         ,only:    SurrogateMethod_Type
use OrthoPoly_Class                                               ,only:    OrthoPoly_Type
use OrthoPoly_Vec_Class                                           ,only:    OrthoPoly_Vec_Type
use OrthoPoly_Factory_Class                                       ,only:    OrthoPoly_Factory
use OrthoMultiVar_Class                                           ,only:    OrthoMultiVar_Type
use OrthoNumerical_Class                                          ,only:    OrthoNumerical_Type
use OrthoLegendre_Class                                           ,only:    OrthoLegendre_Type
use OrthoLaguerre_Class                                           ,only:    OrthoLaguerre_Type
use OrthoHermite_Class                                            ,only:    OrthoHermite_Type
use IndexSetScheme_Class                                          ,only:    IndexSetScheme_Type
use IndexSetScheme_Factory_Class                                  ,only:    IndexSetScheme_Factory
use IndexSetHyperbolic_Class                                      ,only:    IndexSetHyperbolic_Type
use PolyChaosMethod_Factory_Class                                 ,only:    PolyChaosMethod_Factory
use PolyChaosMethod_Class                                         ,only:    PolyChaosMethod_Type
use Response_Class                                                ,only:    Response_Type
use PolyChaosModel_Class                                          ,only:    PolyChaosModel_Type
use SpaceInput_Class                                              ,only:    SpaceInput_Type
use SpaceParam_Class                                              ,only:    SpaceParam_Type
use SpaceTransf_Class                                             ,only:    SpaceTransf_Type
use SpaceTransf_Factory_Class                                     ,only:    SpaceTransf_Factory
use SpaceTransfCustom_Class                                       ,only:    SpaceTransfCustom_Type
use SpaceTransfStdNormal_Class                                    ,only:    SpaceTransfStdNormal_Type
use Model_Class                                                   ,only:    Model_Type
use ModelTransform_Class                                          ,only:    ModelTransform_Type
use ModelInterface_Class                                          ,only:    ModelInterface_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use LinkedList1D_Class                                            ,only:    LinkedList1D_Type
use LinkedList2D_Class                                            ,only:    LinkedList2D_Type
use DistProb_Class                                                ,only:    DistProb_Type
use DistProb_Vec_Class                                            ,only:    DistProb_Vec_Type
use DistUnif_Class                                                ,only:    DistUnif_Type
use DistLogUnif_Class                                             ,only:    DistLogUnif_Type
use DistLog10Unif_Class                                           ,only:    DistLog10Unif_Type
use DistNorm_Class                                                ,only:    DistNorm_Type
use DistLogNorm_Class                                             ,only:    DistLogNorm_Type
use DistLog10Norm_Class                                           ,only:    DistLog10Norm_Type
use DistGamma_Class                                               ,only:    DistGamma_Type

implicit none

private

public                                                                ::    SurrogatePolyChaos_Type

type, extends(SurrogateMethod_Type)                                   ::    SurrogatePolyChaos_Type
  class(IndexSetScheme_Type), allocatable                             ::    IndexSetScheme
  class(PolyChaosMethod_Type), allocatable                            ::    PolyChaosMethod
  logical                                                             ::    Silent=.false.
  character(:), allocatable                                           ::    BasisScheme
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, public                                                   ::    ConstructInput
  procedure, nopass, private                                          ::    ConstructAskeyScheme
  procedure, nopass, private                                          ::    ConstructWeinerScheme
  procedure, nopass, private                                          ::    ConstructAskeyNumericalScheme
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
  subroutine Initialize( This, Debug )

    class(SurrogatePolyChaos_Type), intent(inout)                     ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'
    integer(8)                                                        ::    SysTimeCount

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'polychaos'

      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(SurrogatePolyChaos_Type), intent(inout)                     ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%PolyChaosMethod) ) deallocate(This%PolyChaosMethod, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%PolyChaosMethod', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%IndexSetScheme) ) deallocate(This%IndexSetScheme, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%IndexSetScheme', ProcName=ProcName, stat=StatLoc )

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This, Debug)

    class(SurrogatePolyChaos_Type), intent(inout)                     ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%BasisScheme = 'askey'
    This%SectionChain = ''

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput ( This, Input, SectionChain, Prefix, Debug )

    use StringRoutines_Module
    use ArrayRoutines_Module

    class(SurrogatePolyChaos_Type), intent(inout)                     ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    logical                                                           ::    VarL0D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    i
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

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

    SectionName = 'index_scheme'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call IndexSetScheme_Factory%Construct( Object=This%IndexSetScheme, Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )
    else
      allocate( IndexSetHyperbolic_Type :: This%IndexSetScheme, stat=StatLoc )
      select type ( Object => This%IndexSetScheme )
        type is ( IndexSetHyperbolic_Type )
          call Object%Construct( NormQ=real(0.4,rkp), Order=1, MinOrder=1, MaxOrder=100 )
        class default
          call Error%Raise( Line='Something went wrong when allocating default index generation scheme', ProcName=ProcName )
      end select
    end if

    SectionName = 'method'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call PolyChaosMethod_Factory%Construct( Object=This%PolyChaosMethod, Input=InputSection,                                      &
                                                                   SectionChain=This%SectionChain // '>method', Prefix=PrefixLoc )
    nullify( InputSection )

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine 
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )
    
    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(SurrogatePolyChaos_Type), intent(inout)                     ::    This
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
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i

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

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput%AddParameter( Name='silent', Value=ConvertToString( Value=This%Silent ) )

    call GetInput%AddParameter( Name='basis_scheme', Value=This%BasisScheme )

    SectionName = 'index_scheme'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/index_scheme'
    call GetInput%AddSection( Section=IndexSetScheme_Factory%GetObjectInput( Object=This%IndexSetScheme,                          &
                                                         MainSectionName=SectionName, Prefix=PrefixLoc, Directory=DirectorySub ) )

    SectionName = 'method'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/method'
    call GetInput%AddSection( Section=PolyChaosMethod_Factory%GetObjectInput( Object=This%PolyChaosMethod,                        &
                                                         MainSectionName=SectionName, Prefix=PrefixLoc, Directory=DirectorySub ) )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run( This, SpaceInput, Response, Model, SurrogateModel, OutputDirectory, Debug )

    class(SurrogatePolyChaos_Type), intent(inout)                     ::    This
    type(SpaceParam_Type), intent(in)                                 ::    SpaceInput
    type(Response_Type), dimension(:), intent(in)                     ::    Response
    class(Model_Type), intent(inout)                                  ::    Model
    class(Model_Type), allocatable, optional, intent(out)             ::    SurrogateModel
    character(*), optional, intent(in)                                ::    OutputDirectory
    logical, intent(in), optional                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Run'
    integer                                                           ::    StatLoc=0
    class(SpaceTransf_Type), allocatable                              ::    SpaceTransform
    type(ModelTransform_Type)                                         ::    ModelTransform
    type(OrthoMultiVar_Type)                                          ::    Basis
    type(LinkedList0D_Type), allocatable, dimension(:)                ::    CVErrors
    type(LinkedList1D_Type), allocatable, dimension(:)                ::    Coefficients
    type(LinkedList2D_Type), allocatable, dimension(:)                ::    Indices
    type(ModelInterface_Type)                                         ::    ModelInterface
    type(PolyChaosModel_Type)                                         ::    PolyChaosModelLoc
    character(:), allocatable                                         ::    OutputDirectoryLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    OutputDirectoryLoc = ''

    select case (This%BasisScheme)
      case('weiner')
        call This%ConstructWeinerScheme( SpaceInput, Basis, SpaceTransform )
      case('askey')
        call This%ConstructAskeyScheme( SpaceInput, Basis, SpaceTransform )
      case('askey_numerical')
        call Error%Raise( Line='Askey numerical scheme is not yet ready to be used (numerical orthopoly)', ProcName=ProcName )
        call This%ConstructAskeyNumericalScheme( SpaceInput, Basis, SpaceTransform )
      case('numerical')
        call Error%Raise( Line='Numerical scheme is not yet ready to be used (numerical orthopoly)', ProcName=ProcName )
        call This%ConstructNumericalScheme( SpaceInput, Basis, SpaceTransform )
      case default
        call Error%Raise( Line='Unrecognized orthogonal polynomial basis scheme: ' // This%BasisScheme, ProcName=ProcName )
    end select

    call ModelTransform%Construct( SpaceTransform=SpaceTransform, Model=Model )

    call ModelInterface%Construct( Model=ModelTransform, Response=Response )

    if ( present(OutputDirectory) ) OutputDirectoryLoc = OutputDirectory // '/solver'

    call This%PolyChaosMethod%BuildModel( Basis=Basis, SpaceInput=SpaceTransform, IndexSetScheme=This%IndexSetScheme,             &
         ModelInterface=ModelInterface, Coefficients=Coefficients, Indices=Indices, CVErrors=CVErrors,                            &
         OutputDirectory=OutputDirectoryLoc )

    call PolyChaosModelLoc%Construct( Response=Response, SpaceTransf=SpaceTransform, Basis=Basis, Coefficients=Coefficients,      &
                                                                                              Indices=Indices, CVErrors=CVErrors )
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

    if ( present(OutputDirectory) ) call This%WriteOutput( PolyChaosModel=PolyChaosModelLoc, Directory=OutputDirectory )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteOutput( This, PolyChaosModel, Directory, Debug )

    use CommandRoutines_Module
    use StringRoutines_Module
    use ArrayRoutines_Module
    use SMUQFile_Class                                            ,only:    SMUQFile_Type

    class(SurrogatePolyChaos_Type), intent(inout)                     ::    This
    type(PolyChaosModel_Type), intent(inout)                          ::    PolyChaosModel
    character(*), intent(in)                                          ::    Directory
    logical, intent(in), optional                                     ::    Debug

    logical                                                           ::    DebugLoc
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

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( len_trim(Directory) /= 0 ) then

      call MakeDirectory( Path=Directory, Options='-p' )

      SilentLoc = This%Silent

      if ( .not. SilentLoc ) then
        Line = 'Building polynomial chaos model input package'
        write(*,'(A)') ''
        write(*,'(A)') Line
      end if

      PrefixLoc = Directory // '/PCModelPackage'
      DirectoryLoc = '/PCModelInput'

      Input = PolyChaosModel%GetInput( MainSectionName='polychaos_model', Prefix=PrefixLoc, Directory=DirectoryLoc)
      
      FileName = '/PCModelInput.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      call Input%Write( FileUnit=UnitLoc )
      call File%Close()

      if ( .not. SilentLoc ) then
        Line = 'Writing contents of polynomial chaos model and other files for postprocessing'
        write(*,'(A)') ''
        write(*,'(A)') Line
      end if

    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructAskeyScheme( SpaceInput, Basis, SpaceTransform, Debug )

    type(SpaceParam_Type), intent(in)                                 ::    SpaceInput
    type(OrthoMultiVar_Type), intent(out)                             ::    Basis
    class(SpaceTransf_Type), allocatable, intent(out)                 ::    SpaceTransform
    logical, intent(in), optional                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructAskeyScheme'
    integer                                                           ::    StatLoc=0
    class(DistProb_Type), allocatable                                 ::    DistProb
    type(DistProb_Vec_Type), allocatable, dimension(:)                ::    DistProbVec
    class(DistProb_Type), pointer                                     ::    DistProbPointer
    class(OrthoPoly_Type), allocatable                                ::    OrthoPoly
    type(OrthoPoly_Vec_Type), allocatable, dimension(:)               ::    OrthoPolyVec
    integer                                                           ::    NbDim=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    NbDim = SpaceInput%GetNbDim()

    allocate( SpaceTransfCustom_Type :: SpaceTransform, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SpaceTransform', ProcName=ProcName, stat=StatLoc )
    
    allocate(OrthoPolyVec(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

    allocate(DistProbVec(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='DistProbVec', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbDim
      DistProbPointer => SpaceInput%GetDistributionPointer(Num=i)

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
              call Object2%Construct( Normalization=2 )
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
              call Object2%Construct( Normalization=2 )
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
              call Object2%Construct( Normalization=2 )
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

    call Basis%Construct( OrthoPolyVec=OrthoPolyVec  )

    select type ( Object => SpaceTransform )
      type is (SpaceTransfCustom_Type)
        call Object%Construct( SpaceInput=SpaceInput, Distributions=DistProbVec )
      class default
        call Error%Raise( Line='Something went wrong', ProcName=ProcName )
    end select

    deallocate(DistProbVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='DistProbVec', ProcName=ProcName, stat=StatLoc )

    deallocate(OrthoPolyVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructWeinerScheme( SpaceInput, Basis, SpaceTransform, Debug )

    type(SpaceParam_Type), intent(in)                                 ::    SpaceInput
    type(OrthoMultiVar_Type), intent(out)                             ::    Basis
    class(SpaceTransf_Type), allocatable, intent(out)                 ::    SpaceTransform
    logical, intent(in), optional                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructWeinerScheme'
    integer                                                           ::    StatLoc=0
    class(DistProb_Type), allocatable                                 ::    DistProb
    type(DistProb_Vec_Type), allocatable, dimension(:)                ::    DistProbVec
    class(DistProb_Type), allocatable                                 ::    DistProbPointer
    class(OrthoPoly_Type), allocatable                                ::    OrthoPoly
    type(OrthoPoly_Vec_Type), allocatable, dimension(:)               ::    OrthoPolyVec
    integer                                                           ::    NbDim=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    NbDim = SpaceInput%GetNbDim()

    allocate( SpaceTransfStdNormal_Type :: SpaceTransform, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SpaceTransform', ProcName=ProcName, stat=StatLoc )

    allocate(OrthoPolyVec(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

    allocate( OrthoHermite_Type :: OrthoPoly, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )

    select type ( Object => OrthoPoly )
      type is ( OrthoHermite_Type )
        call Object%Construct( Normalization=2 )
      class default
        call Error%Raise( Line='Something went wrong', ProcName=ProcName )
    end select

    i = 1
    do i = 1, NbDim
      call OrthoPolyVec(i)%Set( Object=OrthoPoly )
    end do

    call Basis%Construct( OrthoPolyVec=OrthoPolyVec  )
  
    select type ( Object => SpaceTransform )
      type is (SpaceTransfStdNormal_Type)
        call Object%Construct( SpaceInput=SpaceInput )
      class default
        call Error%Raise( Line='Something went wrong', ProcName=ProcName )
    end select

    deallocate(OrthoPolyVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

    deallocate(OrthoPoly, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructAskeyNumericalScheme( SpaceInput, Basis, SpaceTransform, Debug )

    type(SpaceParam_Type), intent(in)                                 ::    SpaceInput
    type(OrthoMultiVar_Type), intent(out)                             ::    Basis
    class(SpaceTransf_Type), allocatable, intent(out)                 ::    SpaceTransform
    logical, intent(in), optional                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructAskeyNumericalScheme'
    integer                                                           ::    StatLoc=0
    class(DistProb_Type), allocatable                                 ::    DistProb
    type(DistProb_Vec_Type), allocatable, dimension(:)                ::    DistProbVec
    class(DistProb_Type), pointer                                     ::    DistProbPointer
    class(OrthoPoly_Type), allocatable                                ::    OrthoPoly
    type(OrthoPoly_Vec_Type), allocatable, dimension(:)               ::    OrthoPolyVec
    integer                                                           ::    NbDim=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    NbDim = SpaceInput%GetNbDim()

    allocate( SpaceTransfCustom_Type :: SpaceTransform, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SpaceTransform', ProcName=ProcName, stat=StatLoc )
    
    allocate(OrthoPolyVec(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

    allocate(DistProbVec(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='DistProbVec', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbDim
      DistProbPointer => SpaceInput%GetDistributionPointer(Num=i)

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
              call Object2%Construct( Normalization=2 )
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
              call Object2%Construct( Normalization=2 )
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
              call Object2%Construct( Normalization=2 )
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

    call Basis%Construct( OrthoPolyVec=OrthoPolyVec  )

    select type ( Object => SpaceTransform )
      type is (SpaceTransfCustom_Type)
        call Object%Construct( SpaceInput=SpaceInput, Distributions=DistProbVec )
      class default
        call Error%Raise( Line='Something went wrong when constructing askey scheme space transform', ProcName=ProcName )
    end select

    deallocate(DistProbVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='DistProbVec', ProcName=ProcName, stat=StatLoc )

    deallocate(OrthoPolyVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructNumericalScheme( SpaceInput, Basis, SpaceTransform, Debug )

    type(SpaceParam_Type), intent(in)                                 ::    SpaceInput
    type(OrthoMultiVar_Type), intent(out)                             ::    Basis
    class(SpaceTransf_Type), allocatable, intent(out)                 ::    SpaceTransform
    logical, intent(in), optional                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructNumericalScheme'
    integer                                                           ::    StatLoc=0
    class(DistProb_Type), allocatable                                 ::    DistProb
    type(DistProb_Vec_Type), allocatable, dimension(:)                ::    DistProbVec
    class(DistProb_Type), pointer                                     ::    DistProbPointer
    class(OrthoPoly_Type), allocatable                                ::    OrthoPoly
    type(OrthoPoly_Vec_Type), allocatable, dimension(:)               ::    OrthoPolyVec
    integer                                                           ::    NbDim=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    NbDim = SpaceInput%GetNbDim()

    allocate( SpaceTransfCustom_Type :: SpaceTransform, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SpaceTransform', ProcName=ProcName, stat=StatLoc )
    
    allocate(OrthoPolyVec(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

    allocate(DistProbVec(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='DistProbVec', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbDim
      DistProbPointer => SpaceInput%GetDistributionPointer(Num=i)
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

      call DistProbVec(i)%Set( Object=DistProb )
      call OrthoPolyVec(i)%Set( Object=OrthoPoly )

      nullify(DistProbPointer)
      deallocate(OrthoPoly, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPoly', ProcName=ProcName, stat=StatLoc )
      deallocate(DistProb, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='DistProb', ProcName=ProcName, stat=StatLoc )
    end do

    call Basis%Construct( OrthoPolyVec=OrthoPolyVec  )

    select type ( Object => SpaceTransform )
      type is (SpaceTransfCustom_Type)
        call Object%Construct( SpaceInput=SpaceInput, Distributions=DistProbVec )
      class default
        call Error%Raise( Line='Something went wrong when constructing askey scheme space transform', ProcName=ProcName )
    end select

    deallocate(DistProbVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='DistProbVec', ProcName=ProcName, stat=StatLoc )

    deallocate(OrthoPolyVec, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='OrthoPolyVec', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(SurrogatePolyChaos_Type), intent(out)                       ::    LHS
    class(SurrogateMethod_Type), intent(in)                           ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)

      type is (SurrogatePolyChaos_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%BasisScheme = RHS%BasisScheme
          LHS%Silent = RHS%Silent
          allocate(LHS%PolyChaosMethod, source=RHS%PolyChaosMethod, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%PolyChaosMethod', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%IndexSetScheme, source=RHS%IndexSetScheme, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%IndexSetScheme', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(SurrogatePolyChaos_Type), intent(inout)                      ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%PolyChaosMethod) ) deallocate(This%PolyChaosMethod, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%PolyChaosMethod', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%IndexSetScheme) ) deallocate(This%IndexSetScheme, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%IndexSetScheme', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
