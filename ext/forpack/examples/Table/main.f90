! cd /home/blopez/Codes/ForPack/build/forpack-1.0-Serial-Debug-ifort-17.1/Table/bin; ./TableExample.x

Program TableExample

  use Logger_Class    ,only:  Logger
  use Table_Library   ,only:  Table_Type

  implicit none

  character(*)  ,parameter  ::    ProcName = 'TableExample'
  logical       ,parameter  ::    i_Debug_Loc = .True.

  if (i_Debug_Loc) call Logger%Entering( ProcName )

  if (i_Debug_Loc) call Logger%Write( "Calling SimpleTable" )
  call SimpleTable( i_Debug=i_Debug_Loc )
!
!
!   if (i_Debug_Loc) call Logger%Write( "Calling AdvancedExample" )
!   call AdvancedExample( i_Debug=i_Debug_Loc )

  if (i_Debug_Loc) call Logger%Exiting()

  contains

Subroutine SimpleTable( i_Debug )

  use String_Library    ,only:  Convert_To_String

  logical                                     ,optional ,intent(in)     ::    i_Debug                         !< Debugging indicator

  logical                                                               ::    i_Debug_Loc                     ! Local debugging indicator
  character(*)                                              ,parameter  ::    ProcName = 'SimpleTable'
  type(Table_Type)                                                      ::    Table
  integer                                                               ::    NRow, NCol
  integer                                                               ::    iRow, iCol, iCell
  character(:)  ,allocatable                                            ::    String
  class(*)      ,dimension(:,:) ,allocatable                            ::    MatrixPoly
  logical       ,dimension(:,:) ,allocatable                            ::    MatrixLogical
  integer       ,dimension(:,:) ,allocatable                            ::    MatrixInteger
  real(8)       ,dimension(:,:) ,allocatable                            ::    MatrixReal8
  character(:)  ,dimension(:,:) ,allocatable                            ::    MatrixCharacter

  i_Debug_Loc = .False.; if (present(i_Debug)) i_Debug_Loc = i_Debug
  if (i_Debug_Loc) call Logger%Entering( ProcName )

  NRow          =   8
  NCol          =   5

  if (i_Debug_Loc) call Logger%Write( "Testing Table construction with:" )
  if (i_Debug_Loc) call Logger%Write( "-> NRow = ", NRow )
  if (i_Debug_Loc) call Logger%Write( "-> NCol = ", NCol )

! ==============================================================================================================
  if (i_Debug_Loc) call Logger%Write( "Test-1: Manual construction", NewLine=.True. )
  if (i_Debug_Loc) call Logger%Write( "-> Calling Table%Initialize( NRow, NCol )" )
  call Table%Initialize( NRow, NCol )
  if (i_Debug_Loc) call Logger%Write( "-> Loop on all cells and calling Table%SetCellValue( iRow, iCol, String )" )
  iCell         =   0
  do iRow = 1,NRow
    do iCol = 1,NCol
      iCell     =   iCell + 1
      String    =   Convert_To_String( iCell )
      call Table%SetCellValue( iRow, iCol, String )
    end do
  end do
  if (i_Debug_Loc) call Logger%Write( "-> Calling Table%Output( Logger%Unit )" )
  call Table%Output( Logger%Unit )
! ==============================================================================================================


! ==============================================================================================================
  if (i_Debug_Loc) call Logger%Write( "Test-2: Construction using matrix of logicals", NewLine=.True. )
  if (i_Debug_Loc) call Logger%Write( "-> Setting the matrix of logical variables" )
  allocate( MatrixLogical(NRow,NCol) )
  MatrixLogical(:,:) = .False.
  iCell         =   0
  do iRow = 1,NRow
    do iCol = 1,NCol
      iCell     =   iCell + 1
      if ( mod(iCell,2) == 0 ) MatrixLogical(iRow,iCol) = .True.
    end do
  end do
  if (i_Debug_Loc) call Logger%Write( "-> Calling Table%Initialize( MatrixLogical )" )
  call Table%Initialize( MatrixLogical )
  if (i_Debug_Loc) call Logger%Write( "-> Calling Table%Output( Logger%Unit )" )
  call Table%Output( Logger%Unit )
! ==============================================================================================================


! ==============================================================================================================
  if (i_Debug_Loc) call Logger%Write( "Test-3: Construction using matrix of integers", NewLine=.True. )
  if (i_Debug_Loc) call Logger%Write( "-> Setting the matrix of integer variables" )
  allocate( MatrixInteger(NRow,NCol) )
  MatrixInteger(:,:) = 0
  iCell         =   0
  do iRow = 1,NRow
    do iCol = 1,NCol
      iCell     =   iCell + 1
      MatrixInteger(iRow,iCol) = iCell
    end do
  end do
  if (i_Debug_Loc) call Logger%Write( "-> Calling Table%Initialize( MatrixInteger )" )
  call Table%Initialize( MatrixInteger )
  if (i_Debug_Loc) call Logger%Write( "-> Calling Table%Output( Logger%Unit )" )
  call Table%Output( Logger%Unit )
! ==============================================================================================================


! ==============================================================================================================
  if (i_Debug_Loc) call Logger%Write( "Test-4: Construction using matrix of real-8", NewLine=.True. )
  if (i_Debug_Loc) call Logger%Write( "-> Setting the matrix of integer variables" )
  allocate( MatrixReal8(NRow,NCol) )
  MatrixReal8(:,:) = 0
  iCell         =   0
  do iRow = 1,NRow
    do iCol = 1,NCol
      iCell     =   iCell + 1
      MatrixReal8(iRow,iCol) = real(iCell,kind=8)
    end do
  end do
  if (i_Debug_Loc) call Logger%Write( "-> Calling Table%Initialize( MatrixReal8 )" )
  call Table%Initialize( MatrixReal8 )
  if (i_Debug_Loc) call Logger%Write( "-> Calling Table%Output( Logger%Unit )" )
  call Table%Output( Logger%Unit )
! ==============================================================================================================


! ==============================================================================================================
  if (i_Debug_Loc) call Logger%Write( "Test-5: Construction using a polymorphic matrix allocated to real(8)", NewLine=.True. )
  if (i_Debug_Loc) call Logger%Write( "-> Setting the matrix of real(8) variables" )
  allocate( real(8) :: MatrixPoly(NRow,NCol) )
  select type (MatrixPoly)
  type is (real(8))
    MatrixPoly(:,:) = 0
    iCell         =   0
    do iRow = 1,NRow
      do iCol = 1,NCol
        iCell     =   iCell + 1
        MatrixPoly(iRow,iCol) = real(iCell,kind=8)
      end do
    end do
  end select
  if (i_Debug_Loc) call Logger%Write( "-> Calling Table%Initialize( MatrixPoly )" )
  call Table%Initialize( MatrixPoly )
  if (i_Debug_Loc) call Logger%Write( "-> Calling Table%Output( Logger%Unit )" )
  call Table%Output( Logger%Unit )
! ==============================================================================================================


!
! ! ==============================================================================================================
!   if (i_Debug_Loc) call Logger%Write( "Manual construction", NewLine=.True. )
!   if (i_Debug_Loc) call Logger%Write( "-> Setting the matrix of integer variables" )
!   allocate( MatrixInteger(NRow,NCol) )
!   MatrixInteger(:,:) = 0
!   iCell         =   0
!   do iRow = 1,NRow
!     do iCol = 1,NCol
!       iCell     =   iCell + 1
!       MatrixInteger(iRow,iCol) = iCell
!     end do
!   end do
!   if (i_Debug_Loc) call Logger%Write( "-> Calling Table%Initialize( MatrixInteger )" )
!   call Table%Initialize( MatrixInteger )
!   if (i_Debug_Loc) call Logger%Write( "-> Calling Table%Output( Logger%Unit )" )
!   call Table%Output( Logger%Unit )
! ! ==============================================================================================================



!   do iCol = 1,Table%NCol
! !     call Table%Cells(:,iCol)%Set_Same_Length()
!     call Table%Set_Same_Cell_Row_Length( iCol )
!   end do
!   call Table%Cells(:,3)%Adjust_Right()
!   call Table%Adjust_To_Right_Col( 3 )

  if (i_Debug_Loc) call Logger%Exiting()

End Subroutine
!
! Subroutine AdvancedExample( i_Debug )
!
!   logical                                     ,optional ,intent(in)     ::    i_Debug                         !< Debugging indicator
!   logical                                                               ::    i_Debug_Loc                     ! Local debugging indicator
!   character(*)                                              ,parameter  ::    ProcName = 'AdvancedExample'
!   type(Timer_Type)                                                      ::    Timer
!
!   character(10) ,dimension(5)   ::  SubTimerNames = ["SubTimer-1","SubTimer-2","SubTimer-3","SubTimer-4","SubTimer-5"]
!   integer       ,dimension(5)   ::  SubTimerTimes = [10,          15,          15,          20,          40]
!
!
!   i_Debug_Loc = .False.; if (present(i_Debug)) i_Debug_Loc = i_Debug
!   if (i_Debug_Loc) call Logger%Entering( ProcName )
!
!   call Timer%Initialize(   Name = "Timer with 5 SubTimers" )
!   call Timer%AddSubTimer( Name = SubTimerNames(1) )
!   call Timer%AddSubTimer( Name = SubTimerNames(2) )
!   call Timer%AddSubTimer( Name = SubTimerNames(3) )
!   call Timer%AddSubTimer( Name = SubTimerNames(4) )
!   call Timer%AddSubTimer( Name = SubTimerNames(5) )
!   call Timer%Start()
!
!   call Timer%NextSubTimer()
!   call DoSomething( SubTimerTimes(1), TotalIterations )
!
!   call Timer%NextSubTimer()
!   call DoSomething( SubTimerTimes(2), TotalIterations )
!
!   call Timer%NextSubTimer()
!   call DoSomething( SubTimerTimes(3), TotalIterations )
!
!   call Timer%NextSubTimer()
!   call DoSomething( SubTimerTimes(4), TotalIterations )
!
!   call Timer%NextSubTimer()
!   call DoSomething( SubTimerTimes(5), TotalIterations )
!
!   call Timer%Stop()
!   call Timer%Output()
!
!   if (i_Debug_Loc) call Logger%Exiting()
!
! End Subroutine

End Program
