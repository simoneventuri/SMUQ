! This procedure converts a real variable into a character string.
! The ouput string can be further customized using the following input
! optional arguments:
! * LeadingZeros: This argument forces the number of leading zeros. Default is no leading zeros
! * Fmt: This argument set the format for converting the variable into a string. Default is "i0"
! * Len: This argument set the length of the output string. Default is actual length
! * Pos: This argument set the position of the string
! Examples:
!       String = Convert( 50, LeadingZeros=4 )    =>      "0050"
!       String = Convert( 50 )                    =>      "50"
! @TODO: Check validity of input format
! ======================================================================
! Module Procedure Convert_From_REAL<rkp>_0d_To_CHAR_0d
  use Utilities_Library    ,only:  GetOptArgValue
  character(*)                                              ,parameter  ::  DefaultFormat = "g0"
  character(10000)                                                      ::  Str
  character(:)  ,allocatable                                            ::  Fmt_
  integer                                                               ::  ios
!   character(*)                                              ,parameter  ::  DefaultFormat = "(*(g0,1x))"
  Fmt_    =   "("//GetOptArgValue(DefaultFormat,Fmt)//")"
  write(Str,Fmt_,iostat=ios) Var
  if ( ios /= 0 ) write(Str,"("//DefaultFormat//")",iostat=ios ) Var
  String  =   trim(Str)
  if ( .not. present(Fmt) ) String = Remove_Trailing_Zeros(trim(adjustl(Str)))
  if ( present(Len) )       String = SetLength( String, Len )
  if ( present(Pos) ) then
    if ( Pos == 'L') String(:) = adjustl(String)
    if ( Pos == 'R') String(:) = adjustr(String)
  end if
! End Procedure
! ======================================================================