SubModule(StatusLevel_Class) StatusLevel_SubClass


  implicit none

  contains

Module Procedure NewStatusLevel
  use String_Library            ,only:  Convert_To_String
  if ( present(File) ) This%File = File
  if ( present(Line) ) This%Line = Convert_To_String(Line)
  if ( present(Proc) ) This%Proc = Proc
  if ( present(Desc) ) This%Desc = Desc
End Procedure

Module Procedure GetStatusLevelLine
  Line  =   ""
  if ( len_trim(This%File) /= 0 ) Line  =   Line//This%File
  if ( len_trim(This%Line) /= 0 ) Line  =   Line//"("//This%Line//")"
  if ( len_trim(This%Proc) /= 0 ) Line  =   Line//"["//This%Proc//"]"
  if ( len_trim(This%Desc) /= 0 ) Line  =   Line//" "//This%Desc
End Procedure

End SubModule