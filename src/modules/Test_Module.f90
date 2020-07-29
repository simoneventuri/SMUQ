module Test_Module

use Input_Library
use Parameters_Library
use StringConversion_Module
use StatisticsRoutines_Module
use ArrayIORoutines_Module
use ArrayRoutines_Module
use ComputingRoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use LinSolverLAR_Class                                            ,only:    LinSolverLAR_Type
use LinSolverOMP_Class                                            ,only:    LinSolverOMP_Type
use LinSolverOLS_Class                                            ,only:    LinSolverOLS_Type
use CVKFold_Class                                                 ,only:    CVKFold_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use StopWatch_Class                                               ,only:    StopWatch, StopWatch_Type
use ParameterWriter_Class                                         ,only:    ParameterWriter_Type 
use Input_Class                                                   ,only:    Input_Type 
use SMUQString_Class                                              ,only:    SMUQString_Type 

implicit none

private

public                                                                ::    Test

logical, parameter                                                    ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Test(Input, Prefix)

  class(InputSection_Type), intent(in)                                ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='Test'
  integer                                                             ::    StatLoc=0
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  real(rkp), dimension(8,2)                                           ::    Response
  type(SMUQFile_Type)                                                 ::    File
  character(:), allocatable                                           ::    FileName
  real(rkp), allocatable, dimension(:)                                ::    VarR1D
  real(rkp), allocatable, dimension(:,:)                              ::    SampledParameters
  type(ParameterWriter_Type)                                          ::    ParameterWriter1
  type(ParameterWriter_Type)                                          ::    ParameterWriter2
  type(SMUQString_Type), allocatable, dimension(:)                    ::    ParameterLabels
  type(Input_Type)                                                    ::    ParameterInput
  integer                                                             ::    i 

  FileName = Prefix // 'sampled_parameters.dat'
  call File%Construct(File=FileName)
  call ImportArray(File=File, Array=SampledParameters)

  FileName = Prefix // 'parameter_labels.dat'
  call File%Construct(File=FileName)
  call ImportArray(File=File, Array=ParameterLabels)

  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection='parameter_writer', Mandatory=.true.)

  call ParameterWriter1%Construct(Input=InputSection, ConstructPrefix=Prefix, WritePrefix=Prefix // 'case1/')

  call ParameterWriter2%Construct(Input=InputSection, ConstructPrefix=Prefix, WritePrefix=Prefix // 'case2/' )

  call ParameterInput%Construct(Input=SampledParameters(:,50), Labels=ParameterLabels)

  call ParameterWriter1%WriteInput(Input=ParameterInput)

  call ParameterInput%Construct(Input=SampledParameters(:,51), Labels=ParameterLabels)

  call ParameterWriter2%WriteInput(Input=ParameterInput)
end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
