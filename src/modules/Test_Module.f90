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
use LinSolverOLS_Class                                            ,only:    LinSolverOLS_Type
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
  real(rkp), allocatable, dimension(:,:)                              ::    System
  real(rkp), allocatable, dimension(:)                                ::    Goal
  type(LinSolverLAR_Type)                                             ::    Solver
  real(rkp), allocatable, dimension(:)                                ::    Coefficients
  real(rkp)                                                           ::    CVerror
  integer                                                             ::    i
  type(StopWatch_Type)                                                ::    MainStopWatch

  FileName = '/home/rstkwsk2/workspace/runs/tests/lar_comparison/case/system.txt'
  call File%Construct(File=FileName)
  call ImportArray(File=File, Array=System, RowMajor=.false.)
  
  FileName = '/home/rstkwsk2/workspace/runs/tests/lar_comparison/case/goal.txt'
  call File%Construct(File=FileName)
  call ImportArray(File=File, Array=Goal, RowMajor=.true.)

  allocate(Coefficients(size(System,2)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Coefficients', ProcName=ProcName, stat=StatLoc)
  Coefficients = Zero

  call Solver%Construct(MetaModelMethod='qr', NbCVErrorInc=2000)

  write(*,*) size(system,1), size(system,2)

  i = 1
  do i = 1, 1
    call MainStopWatch%Start()
    call Solver%Solve(System=System, Goal=Goal, Coefficients=Coefficients, CVError=CVError)
    write(*,*) MainStopWatch%Lap()
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
