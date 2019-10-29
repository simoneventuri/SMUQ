! The include file 'ConvertVar1dToChar0d_Inline.F90' is called from the file 'Logger_SubClass.F90'.
! Before calling the procedure, the following marco should be set:
! * _ProcedureName_   Name of the procedure
! * _VarType_         Type of the varibale variable to be added. Possible values:  'logical'
!                       'integer(INT8)', 'integer(INT16)', 'integer(INT32)', 'integer(INT64)'
!                       'real(REAL32)', 'real(REAL64)', 'real(REAL128)'
! * _DefFormat        Default format

Purity Subroutine _ProcedureName_( Variable, String, VarFmt, TypFormat, ComFmt, Status, NItemMax )

  use iso_fortran_env     ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128
  use Utilities_Library   ,only:  SkippedItemsRange

  _VarType_     ,dimension(:)                           ,intent(in)     ::  Variable
  character(:)  ,allocatable                            ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  integer                                     ,optional ,intent(in)     ::  NItemMax


  integer                                                   ,parameter  ::  NumberOfRangesMax  = 32
  integer                                                   ,parameter  ::  IterMax            = 10
  character(*)                                              ,parameter  ::  DefFormat = _DefFormat
  logical                                                               ::  SelectEltSubSet
  integer                                                               ::  ios
  integer                                                               ::  i, iIni, iFin, NElements, NTotal
  integer                                                               ::  NumberOfRanges
  integer                                                               ::  Factor
  integer                                                               ::  Iter, i1, i2, i3, i4
  integer                                                               ::  NEltInp, NEltMax
!   integer       ,allocatable                                            ::  VarIdx(:)
  character(10000)                                                      ::  LongString                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  character(:)  ,allocatable                                            ::  Separator

  Local_Format    =       "(" // GetVectorFormat( DefFormat, VarFmt, TypFormat, ComFmt ) // ")"

  NEltInp   =   size(Variable)

! In this case, there are no elements.
! However, the format may contains some usefule info so we should fake a write.
! the problem is than the variable type may not be character
  if ( NEltInp <= 0 ) then
    String = ""
    return
!     write( LongString, Local_Format, iostat=ios ) ""
!     if ( ios == 0 ) then
!       String = trim(LongString)
!       if ( present(Status) ) Status = ios
!       return
!     end if
  end if

! ==============================================================================================================
!   FIRST CONVERSION ATTEMPT
! ==============================================================================================================
! First, lets try to convert directly the string. The 'write' statment will fail if 'Variable' contains too many
! elements (of the order of 1000).
! ==============================================================================================================
  SelectEltSubSet   =   .False.
  if ( present(NItemMax) ) then
    NEltMax             =   2 * NItemMax + 1
    if ( NEltInp > NEltMax ) then
      SelectEltSubSet   =   .True.
      i1      =   1
      i2      =   NItemMax
      i3      =   NEltInp - NItemMax + 1
      i4      =   NEltInp
    end if
  end if
! ==============================================================================================================




  TryConverting: Block
    if (SelectEltSubSet) then

!     Converting the first set of columns
      write( LongString, Local_Format, iostat=ios ) Variable(:i2)
      if ( ios /= 0 ) exit TryConverting
      String    =   trim(LongString)

!     Writing the string for missing columns
      String    =   String // "   ..." // SkippedItemsRange(NEltInp,NItemMax) // "...   "

!     Converting the last set of columns
      write( LongString, Local_Format, iostat=ios ) Variable(i3:)
      if ( ios /= 0 ) exit TryConverting
      String    =   String // trim(LongString)

      if ( present(Status) ) Status = ios
      return

    else
      write( LongString, Local_Format, iostat=ios ) Variable
      if ( ios /= 0 ) exit TryConverting

      String  =   trim(LongString)
      if ( present(Status) ) Status = ios
      return
    end if
  End Block TryConverting
! ==============================================================================================================


! ==============================================================================================================
!   SECOND CONVERSION ATTEMPT
! ==============================================================================================================
! If the conversion has failed, then we try to convert groups of elements one at the time.
!   if ( ios /= 0 ) then    ! output statement overflows record, unit -5, file Internal Formatted Write
! ==============================================================================================================
  String            =   ""
  NumberOfRanges    =   1
  Factor            =   2
  Iter              =   0
  NTotal            =   size(Variable)
  MainLoop: do
    Iter            =   Iter + 1
    NumberOfRanges  =   NumberOfRanges * Factor
    if ( NumberOfRanges >= NumberOfRangesMax ) exit      !       if ( Iter > IterMax ) exit
    iFin        =   0
    Separator   =   ""
    NElements   =   floor( real(NTotal) / NumberOfRanges ) + mod( NTotal, NumberOfRanges )
    do i = 1,NumberOfRanges
      iIni      =   1    + iFin
      iFin      =   iIni + NElements - 1
      if ( i == NumberOfRanges ) iFin = min(iFin,NTotal)
      write( LongString, Local_Format, iostat=ios ) Variable(iIni:iFin)
      if ( ios /= 0 ) cycle MainLoop
      String    =   String // Separator // trim(LongString)
      Separator =   '   '
      if ( iFin == size(Variable) ) exit MainLoop
    end do
  end do  MainLoop
  if ( ios == 0 ) then
    if ( present(Status) ) Status = ios
    return
  end if

! ==============================================================================================================
!   THIRD CONVERSION ATTEMPT
! ==============================================================================================================
  Local_Format    =   "(*(g0,1x))"
  write( LongString, Local_Format, iostat=ios ) Variable
  String          =       trim(LongString)
  if ( present(Status) ) Status = ios
! ==============================================================================================================

End Subroutine

! Un-defining the marco for next call
# undef  _VarType_
# undef  _DefFormat
# undef  _ProcedureName_