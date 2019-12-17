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
use DistLogNorm_Class                                             ,only:    DistLogNorm_Type
use DistLog10Norm_Class                                           ,only:    DistLog10Norm_Type
use DistUnif_Class                                                ,only:    DistUnif_Type
use DistLogUnif_Class                                             ,only:    DistLogUnif_Type
use DistLog10Unif_Class                                           ,only:    DistLog10Unif_Type
use DistLogistic_Class                                            ,only:    DistLogistic_Type
use DistGamma_Class                                               ,only:    DistGamma_Type
use DistKernel_Class                                              ,only:    DistKernel_Type
use HierDistProb_Class                                            ,only:    HierDistProb_Type
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
    type(DistUnif_Type)                                               ::    DistUnif
    type(OrthoNumerical_Type)                                         ::    OrthoNumericalE
    type(OrthoNumerical_Type)                                         ::    OrthoNumerical
    integer                                                           ::    i

    call DistUnif%Construct( A=real(2.25478E-11,rkp), B=real(6.76434E-11,rkp) )

    call OrthoNumerical%Construct( Weights=DistUnif, Normalized=.true., Order=30 )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
