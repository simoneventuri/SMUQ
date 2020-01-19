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
    integer                                                           ::    i
    class(Model_Type), allocatable                                    ::    Model1
    class(Model_Type), allocatable                                    ::    Model2
    real(rkp), dimension(9)                                           ::    param
    real(rkp), dimension(9)                                           ::    paramtransf
    type(String_Type), dimension(9)                                   ::    paramlabel
    type(String_Type), dimension(9)                                   ::    paramlabeltransf
    type(Input_Type)                                                  ::    InputTarget
    type(Input_Type)                                                  ::    InputPreProcess
    type(Output_Type), dimension(1)                                   ::    Output

    param(1) = 0.1
    param(2) = 0.2
    param(3) = 0.3
    param(4) = 0.4
    param(5) = 0.5
    param(6) = 0.6
    param(7) = 0.7
    param(8) = 0.8
    param(9) = 0.9

    paramtransf(1:3) = dexp(dsqrt(param(1:3)))**2
    paramtransf(4:6) = param(7:9)

    paramlabel(1) = 'p1'
    paramlabel(2) = 'p2'
    paramlabel(3) = 'p3'
    paramlabel(4) = 'p4'
    paramlabel(5) = 'p5'
    paramlabel(6) = 'p6'
    paramlabel(7) = 'p7'
    paramlabel(8) = 'p8'
    paramlabel(9) = 'p9'

    paramlabeltransf(1:3) = paramlabel(1:3)
    paramlabeltransf(4:6) = paramlabel(7:9)

    call InputTarget%Construct( Input=param, Labels=paramlabel )
    call InputPreProcess%Construct( Input=paramtransf, Labels=paramlabeltransf )

    SectionName = 'model1'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call Model_Factory%Construct( Object=Model1, Input=InputSection, Prefix=PrefixLoc )
    nullify ( InputSection )

    SectionName = 'model2'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call Model_Factory%Construct( Object=Model2, Input=InputSection, Prefix=PrefixLoc )
    nullify ( InputSection )

    call Model1%Run(Input=InputPreProcess,Output=Output)
    write(*,*) 'postprocess : ', Output(1)%GetValues()
    call Model2%Run(Input=InputTarget,Output=Output)
    write(*,*) 'original : ', Output(1)%GetValues()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
