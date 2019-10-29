SubModule(Table_Class) Table_SubClass

  implicit none

  contains

Module Subroutine InitializeTable( This, NRow, NCol )
  class(Table_Type)                                     ,intent(out)    ::  This
  integer                                               ,intent(in)     ::  NRow
  integer                                               ,intent(in)     ::  NCol
  This%NRow = NRow
  This%NCol = NCol
  allocate( This%Cells(This%NRow,This%NCol) )
!   This%Cells(:,:)%Value = ""
End Subroutine

Module Procedure InitializeTablePolymorphic
  integer                                                               ::  iRow, iCol
  call This%Initialize( size(Matrix,1), size(Matrix,2) )
!   select type (Value)
!     type is (logical)
!       forall( iRow=1:This%NRow, iCol=1:This%NCol ) call This%SetCellValue( iRow, iCol, Matrix(iRow,iCol) )
! !       forall( iRow=1:This%NRow, iCol=1:This%NCol )
! !         StringMatrix          =   Convert_To_String( Matrix )
! !         This%Cells(:,:)%Value =   StringMatrix
! !       end forall
!     type is (integer)
!     type is (real(4))
!     type is (real(8))
!     type is (real(16))
!     type is (character(*))
!     class default
!   end select
  do iRow = 1,This%NRow
    do iCol = 1,This%NCol
      call This%SetCellValue( iRow, iCol, Matrix(iRow,iCol) )
    end do
  end do
End Procedure


! **************************************************************************************************************
!                               PROCEDURES TO WRITE THE TABLE
! **************************************************************************************************************
Module Procedure OutputTableToFile
  integer                                                               ::  iRow, iCol
!   write(Unit,*)
  do iRow = 1,This%NRow
    write(Unit,"(*(a,1x))") ( This%Cells(iRow,iCol)%Value, iCol = 1,This%NCol )
  end do
!   write(Unit,*)
End Procedure


! **************************************************************************************************************
!                               PROCEDURES TO SET CELLS VALUE
! **************************************************************************************************************
! Module Procedure SetCellValueCharacter
!   This%Cells(iRow,iCol)%Value   =       Value
! End Procedure

Module Procedure SetCellValuePolymorphic
  use String_Library    ,only:  Convert_To_String
  character(:)  ,allocatable                                            ::  String
  select type (Value)
    type is (logical);      String = Convert_To_String( Value )!, VarFmt=VarFmt, TypFormat=IntFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (integer);      String = Convert_To_String( Value )!, VarFmt=VarFmt, TypFormat=IntFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (real(4));      String = Convert_To_String( Value )!, VarFmt=VarFmt, TypFormat=ReaFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (real(8));      String = Convert_To_String( Value )!, VarFmt=VarFmt, TypFormat=ReaFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (real(16));     String = Convert_To_String( Value )!, VarFmt=VarFmt, TypFormat=ReaFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (character(*)); String = Convert_To_String( Value )!, VarFmt=VarFmt, TypFormat=ChaFmt, ComFmt=ComFmt, Status=Local_Status )
    class default;          String = "?"
  end select
  This%Cells(iRow,iCol)%Value   =       String
End Procedure


Module Procedure SetSameCellRowLength
  integer                                                               ::  i                               ! Index of columns
  integer                                                               ::  Length
  character(:)  ,allocatable                                            ::  String
  Length        =       0
  do i = 1,This%NRow
    Length      =       max( Length, len_trim(This%Cells(i,j)%Value) )
  end do
  allocate( character(Length) :: String )
  do i = 1,This%NRow
! ! ! ! !     call Set_Cell_Length( This, Length )
    String(:)   =       This%Cells(i,j)%Value
    if ( allocated(This%Cells(i,j)%Value) ) deallocate( This%Cells(i,j)%Value )
    allocate( character(Length) :: This%Cells(i,j)%Value )
    This%Cells(i,j)%Value(:)    =       String
  end do
End Procedure

Module Procedure AdjustToLeftColumn
  integer                                                               ::  i                               ! Index of columns
  do i = 1,This%NRow
    call This%Cells(i,j)%Adjust_Left()
  end do
End Procedure

Module Procedure AdjustToRightColumn
  integer                                                               ::  i                               ! Index of columns
  do i = 1,This%NRow
    call This%Cells(i,j)%Adjust_Right()
  end do
End Procedure

End SubModule

