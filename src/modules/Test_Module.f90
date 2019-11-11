module Test_Module

use String_Library
use Input_Library
use Parameters_Library
use StringRoutines_Module
use StatisticsRoutines_Module
use ArrayIORoutines_Module
use ComputingRoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use DistProb_Vec_Class                                            ,only:    DistProb_Vec_Type
use DistNorm_Class                                                ,only:    DistNorm_Type
use DistLogNorm_Class                                             ,only:    DistLogNorm_Type
use DistLog10Norm_Class                                           ,only:    DistLog10Norm_Type
use DistUnif_Class                                                ,only:    DistUnif_Type
use DistLogUnif_Class                                             ,only:    DistLogUnif_Type
use DistLog10Unif_Class                                           ,only:    DistLog10Unif_Type
use DistLogistic_Class                                            ,only:    DistLogistic_Type
use DistGamma_Class                                               ,only:    DistGamma_Type
use HierDistNorm_Class                                            ,only:    HierDistNorm_Type
use HierDistLogNorm_Class                                         ,only:    HierDistLogNorm_Type
use HierDistLog10Norm_Class                                       ,only:    HierDistLog10Norm_Type
use HierDistUnif_Class                                            ,only:    HierDistUnif_Type
use HierDistLogUnif_Class                                         ,only:    HierDistLogUnif_Type
use HierDistLog10Unif_Class                                       ,only:    HierDistLog10Unif_Type
use HierDistLogistic_Class                                        ,only:    HierDistLogistic_Type
use HierDistGamma_Class                                           ,only:    HierDistGamma_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use Response_Class                                                ,only:    Response_Type
use ModelInterface_Class                                          ,only:    ModelInterface_Type
use SampleLHS_Class                                               ,only:    SampleLHS_Type
use InputDet_Class                                                ,only:    InputDet_Type
use List2DAllocInt_Class                                          ,only:    List2DAllocInt_Type
use List1DAllocReal_Class                                         ,only:    List1DAllocReal_Type
use ProgramDefs_Class                                             ,only:    ProgramDefs
use Model_Class                                                   ,only:    Model_Type
use ModelExtTemplate_Class                                        ,only:    ModelExtTemplate_Type
use ModelExt_Factory_Class                                        ,only:    ModelExt_Factory
use Output_Class                                                  ,only:    Output_Type
use OrthoNumerical_Class                                          ,only:    OrthoNumerical_Type
use OrthoLegendre_Class                                           ,only:    OrthoLegendre_Type
use OrthoHermite_Class                                            ,only:    OrthoHermite_Type
use OrthoLaguerre_Class                                           ,only:    OrthoLaguerre_Type
use DistProb_Class                                                ,only:    DistProb_Type

implicit none

private

public                                                                ::    Test

logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Test( Input, Prefix )

    class(InputSection_Type), intent(in)                              ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='Test'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    type(DistUnif_Type), target                                       ::    DistUnif
    type(DistLogUnif_Type), target                                    ::    DistLogUnif
    type(DistLog10Unif_Type), target                                  ::    DistLog10Unif
    type(DistNorm_Type), target                                       ::    DistNorm
    type(DistLogNorm_Type), target                                    ::    DistLogNorm
    type(DistLog10Norm_Type), target                                  ::    DistLog10Norm
    type(DistGamma_Type), target                                      ::    DistGamma
    type(DistLogistic_Type), target                                   ::    DistLogistic
    type(HierDistUnif_Type), target                                   ::    HierDistUnif
    type(HierDistLogUnif_Type), target                                ::    HierDistLogUnif
    type(HierDistLog10Unif_Type), target                              ::    HierDistLog10Unif
    type(HierDistNorm_Type), target                                   ::    HierDistNorm
    type(HierDistLogNorm_Type), target                                ::    HierDistLogNorm
    type(HierDistLog10Norm_Type), target                              ::    HierDistLog10Norm
    type(HierDistGamma_Type), target                                  ::    HierDistGamma
    type(HierDistLogistic_Type), target                               ::    HierDistLogistic
    class(DistProb_Type), pointer                                     ::    DistProbPtr
    integer                                                           ::    i
!    call DistUnif%Construct( A=-One, B=One )
!    call DistLogUnif%Construct( A=-One, B=One )
!    call DistLog10Unif%Construct( A=-One, B=One )
!    call DistNorm%Construct( Mu=Zero, Sigma=One )
!    call DistLogNorm%Construct( Mu=Zero, Sigma=One )
!    call DistLog10Norm%Construct( Mu=Zero, Sigma=One )
!    call DistGamma%Construct( Alpha=One, Beta=One )
!    call DistLogistic%Construct( Mu=One, S=One )

    call DistLogistic%Construct( Mu=Two, S=Two, B=Ten+two )

    DistProbPtr => DistLogistic

    write(*,*) DistProbPtr%PDF(X=Six)
    write(*,*) DistProbPtr%CDF(X=Six)
    write(*,*) DistProbPtr%InvCDF(P=0.2_rkp)

    i = 0
    do i = 0, 5
      write(*,*) DistProbPtr%GetMoment(Moment=i)!, DistProbPtr%ComputeMomentNumerical(Moment=i)
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------


!  !!------------------------------------------------------------------------------------------------------------------------------
!  subroutine Test( Input, Prefix )

!    class(InputSection_Type), intent(in)                              ::    Input
!    character(*), optional, intent(in)                                ::    Prefix

!    character(*), parameter                                           ::    ProcName='Test'
!    integer                                                           ::    StatLoc=0
!    character(:), allocatable                                         ::    PrefixLoc
!    character(:), allocatable                                         ::    OutputDirectory
!    class(ModelExtTemplate_Type), allocatable                         ::    Model
!    type(Response_Type), allocatable, dimension(:)                    ::    Response
!    type(InputSection_Type), pointer                                  ::    InputSection=>null()
!    type(SampleLHS_Type)                                              ::    Sampler
!    character(:), allocatable                                         ::    SectionName
!    character(:), allocatable                                         ::    SubSectionName
!    character(:), allocatable                                         ::    ParameterName
!    integer, allocatable, dimension(:)                                ::    Procs
!    integer                                                           ::    NbProcs
!    type(DistNorm_Type)                                               ::    DistNorm
!    integer                                                           ::    i   
!    integer                                                           ::    ii
!    integer                                                           ::    iii
!    integer                                                           ::    iv
!    type(SMUQFile_Type)                                               ::    ParamFile
!    type(SMUQFile_Type)                                               ::    PosteriorFile
!    type(SMUQFile_Type)                                               ::    OutputFile
!    integer                                                           ::    NbParams
!    integer                                                           ::    NbSamples
!    integer                                                           ::    NbProp
!    integer                                                           ::    NbResponses
!    real(rkp), allocatable, dimension(:,:)                            ::    ParamChain
!    real(rkp), allocatable, dimension(:,:)                            ::    ParamChainTemp
!    character(:), allocatable                                         ::    FileName
!    type(InputDet_Type)                                               ::    InputDet
!    type(String_Type), allocatable, dimension(:)                      ::    Labels
!    type(output_Type), allocatable, dimension(:)                      ::    Output
!    real(rkp), allocatable, dimension(:,:)                            ::    OutputTemp
!    integer, allocatable, dimension(:)                                ::    NbYBins
!    real(rkp), allocatable, dimension(:,:)                            ::    YBinsRange
!    real(rkp), allocatable, dimension(:)                              ::    EpsSamples
!    integer, allocatable, dimension(:)                                ::    BinCounts
!    type(List2DAllocInt_Type), allocatable, dimension(:)              ::    ResponseBinCounts
!    real(rkp), allocatable, dimension(:)                              ::    VarR1D
!    integer, allocatable, dimension(:,:)                              ::    VarI2D
!    type(List1DAllocReal_Type), allocatable, dimension(:)             ::    BinEdges
!    real(rkp)                                                         ::    dx
!    character(:), allocatable                                         ::    VarC0D
!    character(:), allocatable, dimension(:)                           ::    VarC1D
!    type(ModelInterface_Type)                                         ::    ModelInterface
!    real(rkp)                                                         ::    SigmaLoc
!    integer                                                           ::    MaxPosteriorIndex
!    real(rkp)                                                         ::    MaxPosterior
!    real(rkp), allocatable, dimension(:)                              ::    MaxPosteriorParam

!    PrefixLoc = ''
!    if ( present(Prefix) ) PrefixLoc = Prefix

!    NbSamples = 0
!    NbParams = 0
!    NbProp = 200

!    SectionName = 'analysis'
!    ParameterName = 'chains'
!    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
!    Procs = ConvertToIntegers(String=VarC0D)
!    NbProcs = size(Procs)

!    ParameterName = 'labels'
!    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
!    call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
!    NbParams = size(VarC1D)
!    allocate(Labels(NbParams), stat=StatLoc)
!    if ( StatLoc /= 0 ) call Error%Allocate( Name='Labels', ProcName=ProcName, stat=StatLoc )
!    i = 1
!    do i = 1, size(VarC1D)
!      Labels(i) = trim(adjustl(VarC1D(i)))
!    end do

!    SectionName = 'plugin'
!    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
!    call ModelExt_Factory%Construct( Object=Model, Input=InputSection, Prefix=PrefixLoc )
!    nullify ( InputSection )

!    SectionName = "responses"
!    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
!    NbResponses = InputSection%GetNumberofSubSections()
!    if ( NbResponses < 1 ) call Error%Raise( Line='Number of specified responses below minimum of 1', ProcName=ProcName )
!    allocate( Response(NbResponses), stat=StatLoc )
!    if ( StatLoc /= 0 ) call Error%Allocate( Name='Responses', ProcName=ProcName, stat=StatLoc )

!    allocate(NbYBins(NbResponses), stat=StatLoc)
!    if ( StatLoc /= 0 ) call Error%Allocate( Name='NbYBins', ProcName=ProcName, stat=StatLoc )
!    NbYBins = 501

!    allocate(YBinsRange(2,NbResponses), stat=StatLoc)
!    if ( StatLoc /= 0 ) call Error%Allocate( Name='YBinsRange', ProcName=ProcName, stat=StatLoc )
!    YBinsRange = Zero
!    YBinsRange(1,:) = 200
!    YBinsRange(2,:) = 1700

!    allocate(ResponseBinCounts(NbResponses), stat=StatLoc)
!    if ( StatLoc /= 0 ) call Error%Allocate( Name='ResponseBinCounts', ProcName=ProcName, stat=StatLoc )

!    allocate(BinEdges(NbResponses), stat=StatLoc)
!    if ( StatLoc /= 0 ) call Error%Allocate( Name='BinEdges', ProcName=ProcName, stat=StatLoc )

!    i = 1
!    do i = 1, NbResponses
!      SubSectionName = SectionName // ">response" // ConvertToString( Value=i )
!      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
!      call Response(i)%Construct( Input=InputSection, Prefix=PrefixLoc )
!      nullify ( InputSection )

!      allocate(VarI2D(NbYBins(i),size(Response(i)%GetAbscissaPointer())), stat=StatLoc)
!      if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
!      VarI2D = 0
!      call ResponseBinCounts(i)%Set(Values=VarI2D)
!      deallocate(VarI2D, stat=StatLoc)
!      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI2D', ProcName=ProcName, stat=StatLoc )

!      allocate(VarR1D(NbYBins(i)+1), stat=StatLoc)
!      if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
!      VarR1D = Zero
!      dx = (YBinsRange(2,i) - YBinsRange(1,i)) / (real(NbYBins(i),rkp)-One)
!      VarR1D(1) = YBinsRange(1,i)-dx/2.0
!      ii = 2
!      do ii = 2, NbYBins(i)+1
!        VarR1D(ii) = VarR1D(ii-1) + dx
!      end do
!      call BinEdges(i)%Set(Values=VarR1D)
!      deallocate(VarR1D, stat=StatLoc)
!      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

!    end do

!    call ModelInterface%Construct( Model=Model, Response=Response )

!    call Sampler%Construct( NbSamples=NbProp )

!    i = 1
!    do i = 1, NbProcs
!      FileName = '/samples/output/' // ConvertToString(Value=i) // '/' // 'parameter_chain.dat'
!      call ParamFile%Construct( File=FileName, Prefix=PrefixLoc )
!      call ParamFile%Open()
!      NbSamples = NbSamples + ParamFile%GetNbLines()
!      call ParamFile%Close()
!    end do

!    allocate(ParamChain(NbParams,NbSamples), stat=StatLoc)
!    if ( StatLoc /= 0 ) call Error%Allocate( Name='ParamChain', ProcName=ProcName, stat=StatLoc )
!    ParamChain = Zero

!    MaxPosterior = Zero
!    ii = 0

!    i = 1
!    do i = 1, NbProcs
!      FileName = '/samples/output/' // ConvertToString(Value=i) // '/' // 'parameter_chain.dat'
!      call ParamFile%Construct( File=FileName, Prefix=PrefixLoc )
!      call ImportArray( File=ParamFile, Array=ParamChainTemp, Mandatory=.true. )
!      if ( NbParams /= size(ParamChainTemp,1) ) call Error%Raise( Line='Mismatch in number of parameters', ProcName=ProcName )
!      ParamChain(:,ii+1:ii+size(ParamChainTemp,2)) = ParamChainTemp
!      ii = ii + size(ParamChainTemp,2)

!      FileName = '/samples/output/' // ConvertToString(Value=i) // '/' // 'posterior_chain.dat'
!      call PosteriorFile%Construct( File=FileName, Prefix=PrefixLoc )
!      call ImportArray( File=PosteriorFile, Array=VarR1D, Mandatory=.true. )
!      MaxPosteriorIndex = maxloc(VarR1D,1)
!      if ( VarR1D(MaxPosteriorIndex) > MaxPosterior ) then
!        MaxPosterior = VarR1D(MaxPosteriorIndex)
!        MaxPosteriorParam = ParamChainTemp(:,MaxPosteriorIndex)
!      end if
!    end do

!    i = 1
!    do i = 1, NbSamples
!      write(*,*) i
!      call InputDet%Construct( Input=ParamChain(:,i), Labels=Labels )
!      call ModelInterface%Run( Input=InputDet, Output=Output, Stat=StatLoc )
!      do ii = 1, NbResponses
!        call InputDet%GetValue( Value=SigmaLoc, Label='sigma_' // ConvertToString(ii) )
!        call DistNorm%Construct( Mu=Zero, Sigma=SigmaLoc )
!        iii = 1
!        do iii = 1, size(Output(ii)%Ordinate%Val,1)
!          EpsSamples = Sampler%Draw()
!          iv = 1
!          do iv = 1, size(EpsSamples,1)
!            EpsSamples(iv) = dexp(DistNorm%InvCDF(P=EpsSamples(iv))) * Output(ii)%Ordinate%Val(iii,1)
!          end do
!          call Bin( Values=EpsSamples, BinEdges=BinEdges(ii)%Values, BinCounts=BinCounts )
!          ResponseBinCounts(ii)%Values(:,iii) = ResponseBinCounts(ii)%Values(:,iii) + BinCounts
!        end do
!      end do
!    end do

!    call InputDet%Construct( Input=MaxPosteriorParam, Labels=Labels )
!    call ModelInterface%Run( Input=InputDet, Output=Output, Stat=StatLoc )

!    do i = 1, NbResponses
!      OutputDirectory = ProgramDefs%GetOutputDir() // '/response' // ConvertToString(i)
!      call MakeDirectory( Path=OutputDirectory, Options='-p' )

!      FileName = '/bin_counts.dat'
!      call OutputFile%Construct( File=FileName, Prefix=OutputDirectory )
!      call ExportArray( Array=ResponseBinCounts(i)%Values, File=OutputFile, RowMajor=.true. )

!      FileName = '/bin_edges.dat'
!      call OutputFile%Construct( File=FileName, Prefix=OutputDirectory )
!      call ExportArray( Array=BinEdges(i)%Values, File=OutputFile )

!      FileName = '/data.dat'
!      call OutputFile%Construct( File=FileName, Prefix=OutputDirectory )
!      call ExportArray( Array=Response(i)%GetData(), File=OutputFile, RowMajor=.true. )

!      FileName = '/abscissa.dat'
!      call OutputFile%Construct( File=FileName, Prefix=OutputDirectory )
!      call ExportArray( Array=Response(i)%GetAbscissa(), File=OutputFile )

!      FileName = '/max_posterior.dat'
!      call OutputFile%Construct( File=FileName, Prefix=OutputDirectory )
!      call ExportArray( Array=Output(i)%GetOrdinate(), File=OutputFile, RowMajor=.true. )

!      FileName = '/max_posterior_param.dat'
!      call OutputFile%Construct( File=FileName, Prefix=OutputDirectory )
!      call ExportArray( Array=MaxPosteriorParam, File=OutputFile )
!    end do

!  end subroutine
!  !!------------------------------------------------------------------------------------------------------------------------------

end module
