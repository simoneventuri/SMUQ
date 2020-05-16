module Test_Module

use Input_Library
use Parameters_Library
use StringRoutines_Module
use StatisticsRoutines_Module
use ArrayIORoutines_Module
use ComputingRoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use LinSolverLAR_Class                                            ,only:    LinSolverLAR_Type
use LinSolverOMP_Class                                            ,only:    LinSolverOMP_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type

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
    type(SMUQFile_Type)                                               ::    File
    character(:), allocatable                                         ::    FileName
    real(rkp), allocatable, dimension(:,:)                            ::    System
    real(rkp), allocatable, dimension(:)                              ::    Goal
    type(LinSolverOMP_Type)                                           ::    Solver
    real(rkp), allocatable, dimension(:)                              ::    Coefficients
    real(rkp)                                                         ::    CVerror
    integer                                                           ::    i

    FileName = '/home/rstkwsk2/workspace/runs/tests/test_newlar/case/system_longley.txt'
    call File%Construct(File=FileName)
    call ImportArray(File=File, Array=System, RowMajor=.true.)
    
    FileName = '/home/rstkwsk2/workspace/runs/tests/test_newlar/case/goal_longley.txt'
    call File%Construct(File=FileName)
    call ImportArray(File=File, Array=Goal, RowMajor=.true.)

    allocate(Coefficients(size(System,2)), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Coefficients', ProcName=ProcName, stat=StatLoc)
    Coefficients = Zero

    call Solver%Construct(MetaModelMethod='qr')

    call Solver%Solve(System=System, Goal=Goal, Coefficients=Coefficients, CVError=CVError)
write(*,*) '++++++++++++++++++++++++++'
write(*,*) Coefficients
write(*,*) CVError
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
