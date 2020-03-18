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
use DistProbContainer_Class                                       ,only:    DistProbContainer_Type
use DistNorm_Class                                                ,only:    DistNorm_Type
use DistBeta_Class                                                ,only:    DistBeta_Type
use DistLogNorm_Class                                             ,only:    DistLogNorm_Type
use DistLog10Norm_Class                                           ,only:    DistLog10Norm_Type
use DistUnif_Class                                                ,only:    DistUnif_Type
use DistLogUnif_Class                                             ,only:    DistLogUnif_Type
use DistLog10Unif_Class                                           ,only:    DistLog10Unif_Type
use DistLogistic_Class                                            ,only:    DistLogistic_Type
use DistGamma_Class                                               ,only:    DistGamma_Type
use DistKernel_Class                                              ,only:    DistKernel_Type
use DistProbContainer_Class                                       ,only:    DistProbContainer_Type
use HierDistProb_Class                                            ,only:    HierDistProb_Type
use HierDistNorm_Class                                            ,only:    HierDistNorm_Type
use HierDistBeta_Class                                            ,only:    HierDistBeta_Type
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
use Input_Class                                                   ,only:    Input_Type
use List2DAllocInt_Class                                          ,only:    List2DAllocInt_Type
use List1DAllocReal_Class                                         ,only:    List1DAllocReal_Type
use ProgramDefs_Class                                             ,only:    ProgramDefs
use Model_Class                                                   ,only:    Model_Type
use Model_Factory_Class                                           ,only:    Model_Factory
use Output_Class                                                  ,only:    Output_Type
use OrthoNumerical_Class                                          ,only:    OrthoNumerical_Type
use OrthoLegendre_Class                                           ,only:    OrthoLegendre_Type
use OrthoHermite_Class                                            ,only:    OrthoHermite_Type
use OrthoJacobi_Class                                             ,only:    OrthoJacobi_Type
use OrthoLaguerre_Class                                           ,only:    OrthoLaguerre_Type
use DistProb_Class                                                ,only:    DistProb_Type
use ParamSpace_Class                                              ,only:    ParamSpace_Type
use SampleLHS_Class                                               ,only:    SampleLHS_Type
use SampleMC_Class                                                ,only:    SampleMC_Type
use SampleQuasiMC_Class                                           ,only:    SampleQuasiMC_Type

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
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    PrefixLoc
    type(SampleLHS_Type)                                              ::    SamplerLHS
    type(SampleMC_Type)                                               ::    SamplerMC
    type(SampleQuasiMC_Type)                                          ::    SamplerQuasiMC
    real(rkp), allocatable, dimension(:,:)                            ::    SamplesLHS
    real(rkp), allocatable, dimension(:,:)                            ::    SamplesMC
    real(rkp), allocatable, dimension(:,:)                            ::    SamplesQuasiMC
    real(rkp), allocatable, dimension(:,:)                            ::    EnrichedSamplesLHS1
    real(rkp), allocatable, dimension(:,:)                            ::    EnrichedSamplesMC1
    real(rkp), allocatable, dimension(:,:)                            ::    EnrichedSamplesQuasiMC1
    real(rkp), allocatable, dimension(:,:)                            ::    EnrichedSamplesLHS2
    real(rkp), allocatable, dimension(:,:)                            ::    EnrichedSamplesMC2
    real(rkp), allocatable, dimension(:,:)                            ::    EnrichedSamplesQuasiMC2
    real(rkp), allocatable, dimension(:,:)                            ::    EnrichedSamplesLHS3
    real(rkp), allocatable, dimension(:,:)                            ::    EnrichedSamplesMC3
    real(rkp), allocatable, dimension(:,:)                            ::    EnrichedSamplesQuasiMC3
    integer                                                           ::    NbSamples
    integer                                                           ::    NbDim
    integer                                                           ::    NbEnrichmentSamples1
    integer                                                           ::    NbEnrichmentSamples2
    integer                                                           ::    NbEnrichmentSamples3
    character(:), allocatable                                         ::    FileName
    type(SMUQFile_Type)                                               ::    File
    character(:), allocatable                                         ::    OutputDir
    type(DistNorm_Type)                                               ::    DistNorm1
    type(DistNorm_Type)                                               ::    DistNorm2
    type(DistUnif_Type)                                               ::    DistUnif
    type(DistLogNorm_Type)                                            ::    DistLogNorm
    type(DistBeta_Type)                                               ::    DistBeta
    type(DistGamma_Type)                                              ::    DistGamma
    type(DistProbContainer_Type), dimension(6)                        ::    DistProbVec 
    type(ParamSpace_Type)                                             ::    SampleSpace
    type(String_Type), dimension(6)                                   ::    Labels


    OutputDir = ProgramDefs%GetOutputDir()

    NbSamples = 600
    NbEnrichmentSamples1 = 300
    NbEnrichmentSamples2 = 600
    NbEnrichmentSamples3 = 2000
!    NbDim = 2

    call SamplerLHS%Construct()
    call SamplerMC%Construct()
    call SamplerQuasiMC%Construct()

    call DistNorm1%Construct( Mu=Five, Sigma=Three )
    call DistNorm2%Construct( Mu=-Three, Sigma=Two, A=-Five, B=One )
    call DistUnif%Construct( A=-50.0_rkp, B=200.0_rkp )
    call DistLogNorm%Construct( Mu=Two, Sigma=One/Two )
    call DistBeta%Construct( Alpha=Two , Beta=Five )
    call DistGamma%Construct( Alpha=Two, Beta=Five )

    call DistProbVec(1)%Set( Object=DistNorm1 )
    call DistProbVec(2)%Set( Object=DistNorm2 )
    call DistProbVec(3)%Set( Object=DistUnif )
    call DistProbVec(4)%Set( Object=DistLogNorm )
    call DistProbVec(5)%Set( Object=DistBeta )
    call DistProbVec(6)%Set( Object=DistGamma )

    Labels(1) = '1'
    Labels(2) = '2'
    Labels(3) = '3'
    Labels(4) = '4'
    Labels(5) = '5'
    Labels(6) = '6' 

    call SampleSpace%Construct(Distributions=DistProbVec, Labels=Labels )

    SamplesLHS = SampleSpace%Draw( Sampler=SamplerLHS, NbSamples=NbSamples )
    SamplesMC = SampleSpace%Draw( Sampler=SamplerMC, NbSamples=NbSamples )
    SamplesQuasiMC = SampleSpace%Draw( Sampler=SamplerQuasiMC, NbSamples=NbSamples )

    call SampleSpace%Enrich( Sampler=SamplerLHS, Samples=SamplesLHS, NbEnrichmentSamples=NbEnrichmentSamples1,                    &
                                                                                           EnrichmentSamples=EnrichedSamplesLHS1 )
    call SampleSpace%Enrich( Sampler=SamplerMC, Samples=SamplesMC, NbEnrichmentSamples=NbEnrichmentSamples1,                      &
                                                                                            EnrichmentSamples=EnrichedSamplesMC1 )
    call SampleSpace%Enrich( Sampler=SamplerQuasiMC, Samples=SamplesQuasiMC, NbEnrichmentSamples=NbEnrichmentSamples1,            &
                                                                                       EnrichmentSamples=EnrichedSamplesQuasiMC1 )

    call SampleSpace%Enrich( Sampler=SamplerLHS, Samples=SamplesLHS, NbEnrichmentSamples=NbEnrichmentSamples2,                    &
                                                                                           EnrichmentSamples=EnrichedSamplesLHS2 )
    call SampleSpace%Enrich( Sampler=SamplerMC, Samples=SamplesMC, NbEnrichmentSamples=NbEnrichmentSamples2,                      &
                                                                                            EnrichmentSamples=EnrichedSamplesMC2 )
    call SampleSpace%Enrich( Sampler=SamplerQuasiMC, Samples=SamplesQuasiMC, NbEnrichmentSamples=NbEnrichmentSamples2,            &
                                                                                       EnrichmentSamples=EnrichedSamplesQuasiMC2 )

    call SampleSpace%Enrich( Sampler=SamplerLHS, Samples=SamplesLHS, NbEnrichmentSamples=NbEnrichmentSamples3,                    &
                                                                                           EnrichmentSamples=EnrichedSamplesLHS3 )
    call SampleSpace%Enrich( Sampler=SamplerMC, Samples=SamplesMC, NbEnrichmentSamples=NbEnrichmentSamples3,                      &
                                                                                            EnrichmentSamples=EnrichedSamplesMC3 )
    call SampleSpace%Enrich( Sampler=SamplerQuasiMC, Samples=SamplesQuasiMC, NbEnrichmentSamples=NbEnrichmentSamples3,            &
                                                                                       EnrichmentSamples=EnrichedSamplesQuasiMC3 )

!    SamplesLHS = SamplerLHS%Draw( NbDim=NbDim, NbSamples=NbSamples )
!    SamplesMC = SamplerMC%Draw( NbDim=NbDim, NbSamples=NbSamples )
!    SamplesQuasiMC = SamplerQuasiMC%Draw( NbDim=NbDim, NbSamples=NbSamples )
!    
!    call SamplerLHS%Enrich( Samples=SamplesLHS, NbEnrichmentSamples=NbEnrichmentSamples1, EnrichmentSamples=EnrichedSamplesLHS1 )
!    call SamplerMC%Enrich( Samples=SamplesMC, NbEnrichmentSamples=NbEnrichmentSamples1, EnrichmentSamples=EnrichedSamplesMC1 )
!    call SamplerQuasiMC%Enrich( Samples=SamplesQuasiMC, NbEnrichmentSamples=NbEnrichmentSamples1,                                 &
!                                                                                       EnrichmentSamples=EnrichedSamplesQuasiMC1 )

!    call SamplerLHS%Enrich( Samples=SamplesLHS, NbEnrichmentSamples=NbEnrichmentSamples2, EnrichmentSamples=EnrichedSamplesLHS2 )
!    call SamplerMC%Enrich( Samples=SamplesMC, NbEnrichmentSamples=NbEnrichmentSamples2, EnrichmentSamples=EnrichedSamplesMC2 )
!    call SamplerQuasiMC%Enrich( Samples=SamplesQuasiMC, NbEnrichmentSamples=NbEnrichmentSamples2,                                 &
!                                                                                       EnrichmentSamples=EnrichedSamplesQuasiMC2 )

!    call SamplerLHS%Enrich( Samples=SamplesLHS, NbEnrichmentSamples=NbEnrichmentSamples3, EnrichmentSamples=EnrichedSamplesLHS3 )
!    call SamplerMC%Enrich( Samples=SamplesMC, NbEnrichmentSamples=NbEnrichmentSamples3, EnrichmentSamples=EnrichedSamplesMC3 )
!    call SamplerQuasiMC%Enrich( Samples=SamplesQuasiMC, NbEnrichmentSamples=NbEnrichmentSamples3,                                 &
!                                                                                       EnrichmentSamples=EnrichedSamplesQuasiMC3 )

    FileName='/samplesLHS.dat'
    call File%Construct( File=FileName, Prefix=OutputDir )
    call ExportArray( Array=SamplesLHS, File=File )

    FileName='/samplesMC.dat'
    call File%Construct( File=FileName, Prefix=OutputDir )
    call ExportArray( Array=SamplesMC, File=File )

    FileName='/samplesQuasiMC.dat'
    call File%Construct( File=FileName, Prefix=OutputDir )
    call ExportArray( Array=SamplesQuasiMC, File=File )


    FileName='/enrichedsamplesLHS1.dat'
    call File%Construct( File=FileName, Prefix=OutputDir )
    call ExportArray( Array=EnrichedSamplesLHS1, File=File )

    FileName='/enrichedsamplesMC1.dat'
    call File%Construct( File=FileName, Prefix=OutputDir )
    call ExportArray( Array=EnrichedSamplesMC1, File=File )

    FileName='/enrichedsamplesQuasiMC1.dat'
    call File%Construct( File=FileName, Prefix=OutputDir )
    call ExportArray( Array=EnrichedSamplesQuasiMC1, File=File )


    FileName='/enrichedsamplesLHS2.dat'
    call File%Construct( File=FileName, Prefix=OutputDir )
    call ExportArray( Array=EnrichedSamplesLHS2, File=File )

    FileName='/enrichedsamplesMC2.dat'
    call File%Construct( File=FileName, Prefix=OutputDir )
    call ExportArray( Array=EnrichedSamplesMC2, File=File )

    FileName='/enrichedsamplesQuasiMC2.dat'
    call File%Construct( File=FileName, Prefix=OutputDir )
    call ExportArray( Array=EnrichedSamplesQuasiMC2, File=File )


    FileName='/enrichedsamplesLHS3.dat'
    call File%Construct( File=FileName, Prefix=OutputDir )
    call ExportArray( Array=EnrichedSamplesLHS3, File=File )

    FileName='/enrichedsamplesMC3.dat'
    call File%Construct( File=FileName, Prefix=OutputDir )
    call ExportArray( Array=EnrichedSamplesMC3, File=File )

    FileName='/enrichedsamplesQuasiMC3.dat'
    call File%Construct( File=FileName, Prefix=OutputDir )
    call ExportArray( Array=EnrichedSamplesQuasiMC3, File=File )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
