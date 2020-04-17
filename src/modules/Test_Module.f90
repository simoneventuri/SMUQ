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
use SMD_Class                                                     ,only:    SMD

implicit none

private

public                                                                ::    Test

logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Test(Input, Prefix)

    class(InputSection_Type), intent(in)                              ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='Test'
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    real(rkp), dimension(8,2)                                         ::    Response

    Response(1,1) = One
    Response(2,1) = Two
    Response(3,1) = Three
    Response(4,1) = Four
    Response(5,1) = Five
    Response(6,1) = Six
    Response(7,1) = Seven
    Response(8,1) = Eight

    call SMD(M=Two, C=Three, K=Two, X0=Four, Xdot0=-Two, Abscissa=Response(:,1), Response=Response(:,2))

    call WriteArray(Array=Response)

    call SMD(M=Two, C=Four, K=Two, X0=Four, Xdot0=-Two, Abscissa=Response(:,1), Response=Response(:,2))

    call WriteArray(Array=Response)

    call SMD(M=Two, C=Five, K=Two, X0=Four, Xdot0=-Two, Abscissa=Response(:,1), Response=Response(:,2))

    call WriteArray(Array=Response)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
