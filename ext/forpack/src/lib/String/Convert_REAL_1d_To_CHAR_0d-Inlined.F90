! This procedure converts a rank-1 array real variable into a character string.
! Using character length allocation, the output character has a length which exactly
! matches the number of digits of the input integer.
! ======================================================================
! Module Procedure Convert_From_REAL<ikp>_1d_To_CHAR_0d
  use Utilities_Library    ,only:  GetOptArgValue
  character(*)                                              ,parameter  ::  DefaultFormat = "(*(g0,1x))"
  character(1000)                                                       ::  Str
  character(:)  ,allocatable                                            ::  Fmt_
  integer                                                               ::  ios
  Fmt_    =   "("//GetOptArgValue(DefaultFormat,Fmt)//")"
  write(Str,Fmt_,iostat=ios) Var
  if ( ios /= 0 ) write(Str,"("//DefaultFormat//")",iostat=ios) Var
  String  =   trim(Str)
  if ( .Not. present(Fmt) ) String = trim(adjustl(Str))
  if ( present(Len) ) String = SetLength( String, Len )
  if ( present(Pos) ) then
    if ( Pos == 'L') String(:) = adjustl(String)
    if ( Pos == 'R') String(:) = adjustr(String)
  end if
! End Procedure
! ======================================================================