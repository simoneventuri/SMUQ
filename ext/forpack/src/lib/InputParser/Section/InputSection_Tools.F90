Module InputSection_Tools

  implicit none

  public

  contains

! This procedure create a path of calling procedures.
! This is done by adding a calling procedure name, if present, a the LHS of a current procedure name.
Pure Function Set_Procedure_Path( Current_Procedure, CallProc ) result(ProcPath)
  character(*)                                          ,intent(in)     ::  Current_Procedure                       !< Name of current procedure
  character(*)                                ,optional ,intent(in)     ::  CallProc                       !< Name of calling procedure to be added to LHS of the current procedure, if present
  character(:)  ,allocatable                                            ::  ProcPath                                !< Calling path
  ProcPath      =   Current_Procedure                                                                               ! Initializing the calling sequence path with the name of current procedure
  if ( present(CallProc) ) ProcPath = trim(CallProc) // " > "// Current_Procedure                     ! If present, adding the calling procedure to the LHS of the current procedure
End Function


!   @TODO: Refactor ProcessListSectionNames so that it can gives either the first or last item
!     call ProcessListSectionNames( &
!             First       = ... , &
!             Last        = ... , &
!             AllButFirst = ... , &
!             AllButLast  =       )

Pure Subroutine ProcessListSectionNames( FirstName, ListNames )
  use String_Library      ,only:  Split
  character(:)  ,allocatable                            ,intent(out)    ::  FirstName                     !< Name of the section to be added
  character(:)  ,allocatable                            ,intent(inout)  ::  ListNames                     !< Name of the section to be added
  character(*)                                              ,parameter  ::  Separator = ">"
  character(:)  ,allocatable                                            ::  LastNames
  call Split( ListNames, FirstName, Separator, LastNames )
  FirstName   =   trim( FirstName )
  ListNames   =   trim( LastNames )
End Subroutine

! This procedure process an input character string corresponding to a list of section names,
! followed by a parameter name. Here is an example:
!     String      = "sec1 > sec2 > sec3 > para"
! This procedure will return the list of section and the parameter name:
!     SectionList = "sec1 > sec2 > sec3"
!     ParamName   = "para"
! If the input string contains only the parameter name:   String = "para"
! then the output list of sections is an empty string:
!     SectionList = ""
!     ParamName   = "para"
Pure Subroutine ProcessListSectionsParam( String, SectionList, ParamName )
  character(*)                                          ,intent(in)     ::  String
  character(:)  ,allocatable                            ,intent(out)    ::  SectionList                   !< String containing the list of section
  character(:)  ,allocatable                            ,intent(out)    ::  ParamName                     !< Name of the section to be added
  character(*)                                              ,parameter  ::  Separator = ">"
  character(:)  ,allocatable                                            ::  LastNames
  integer                                                               ::  i
  i   =   index(String,Separator,Back=.True.)
  if ( i == 0 ) then
    SectionList =   ""
    ParamName   =   trim(String)
  else
    SectionList =   trim(String(:i-1))
    ParamName   =   trim(String(i+1:))
  end if
End Subroutine

Pure Function ProcessCaseSensitiveString( InputString, CaseSensitive ) result(OutputString)
  use String_Library      ,only:  UpperCase
  character(*)                                          ,intent(in)     ::  InputString
  logical                                     ,optional ,intent(in)     ::  CaseSensitive
  character(:)  ,allocatable                                            ::  OutputString
  logical                                                               ::  CaseSensitive_
  CaseSensitive_ = .False.; if ( present(CaseSensitive) ) CaseSensitive_ = CaseSensitive
  OutputString    =   InputString
  if ( .Not. CaseSensitive_ ) OutputString  =   UpperCase( OutputString )
End Function

End Module