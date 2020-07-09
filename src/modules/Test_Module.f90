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


  FileName = '/home/rstkwsk2/workspace/runs/tests/variance_test/case/output.dat'
  call File%Construct(File=FileName)
  call ImportArray(File=File, Array=VarR1D)

  write(*,*) ComputeVariance(Values=VarR1D)
  write(*,*) IsArrayConstant(Array=VarR1D)
end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
