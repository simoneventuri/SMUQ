Module SourceLocation_CLass

  implicit none

  private
  public  ::  GetSourceLoc

  integer                     ,parameter  ::  MAXLEN_FILE_NAME    = 500
  integer                     ,parameter  ::  UNKNOWN_LINE_NUMBER = -1
  character(MAXLEN_FILE_NAME) ,parameter  ::  UNKNOWN_FILE_NAME   = '<unknown file>'

  Type    ::  SourceLocation_Type
    private
    character(MAXLEN_FILE_NAME)   ::  FileName    =   UNKNOWN_FILE_NAME
    integer                       ::  LineNumber  =   UNKNOWN_LINE_NUMBER
  contains
    private
    procedure ,public   ::  ToString
  End Type


  contains

Function GetSourceLoc( FileName, LineNumber ) result(String)
  character(*)                                          ,intent(in)     ::  FileName
  integer                                               ,intent(in)     ::  LineNumber
  character(:)  ,allocatable                                            ::  String
  character(*)  ,parameter  ::  UNKNOWN_FILE_NAME   = '<unknown file>'
  character(1000)                                                       ::  LongString
  integer                                                               ::  Status
  if ( len_trim(FileName) == 0 ) then
    if ( LineNumber <= 0 ) then
      String  =   '<unknown location>'
    else
      write(LongString,'(a,":",i0)', iostat=Status) trim(UNKNOWN_FILE_NAME), LineNumber
      String  =   trim(LongString)
    end if
  else
    if ( LineNumber <= 0 ) then
      String  =   trim(FileName)
    else
      write(LongString,'(a,":",i0)', iostat=Status) trim(FileName), LineNumber
      String  =   trim(LongString)
    end if
  end if
  String  = '[' // trim(String) // ']'
End Function


Function ToString(This) result(String)

  class(SourceLocation_Type)                            ,intent(inout)  ::  This
  character(:)  ,allocatable                                            ::  String
  character(1000)                                                       ::  LongString
  integer                                                               ::  Status

  if ( This%FileName == UNKNOWN_FILE_NAME ) then
    if (This%LineNumber == UNKNOWN_LINE_NUMBER) then
      String  =   '<unknown location>'
    else
      write(LongString,'(a,":",i0)', iostat=Status) trim(UNKNOWN_FILE_NAME), This%LineNumber
      String  =   trim(LongString)
    end if
  else
    if (This%LineNumber == UNKNOWN_LINE_NUMBER) then
      String = trim(This%FileName)
    else
      write(LongString,'(a,":",i0)', iostat=Status) trim(This%FileName), This%LineNumber
      String  =   trim(LongString)
    end if
  end if
  String  = '[' // trim(String) // ']'
End Function

End Module
!
! module SourceLocation_mod
!    implicit none
!    private
!
!    public :: SourceLocation
!    public :: UNKNOWN_SOURCE_LOCATION
!    public :: UNKNOWN_FILE_NAME
!    public :: UNKNOWN_LINE_NUMBER
!
!    integer, parameter :: MAXLEN_FILE_NAME = 255
!    character(len=MAXLEN_FILE_NAME), parameter :: UNKNOWN_FILE_NAME= '<unknown file>'
!    integer, parameter :: UNKNOWN_LINE_NUMBER = -1
!
!    type :: SourceLocation
!       character(len=MAXLEN_FILE_NAME) :: fileName = UNKNOWN_FILE_NAME
!       integer :: LineNumber = UNKNOWN_LINE_NUMBER
!    contains
!       procedure :: toString
!    end type SourceLocation
!
!    type (SourceLocation), parameter :: UNKNOWN_SOURCE_LOCATION = &
!         & SourceLocation()
!
! contains
!
!    function toString(this) result(string)
!       class (SourceLocation), intent(inout) :: this
!       character(len=300) :: string
!       integer :: status
!
!       if (this%fileName == UNKNOWN_FILE_NAME) then
!          if (this%LineNumber == UNKNOWN_LINE_NUMBER) then
!             string = '<unknown location>'
!          else
!             write(string,'(a,":",i0)', iostat=status) trim(UNKNOWN_FILE_NAME), this%LineNumber
!          end if
!       else
!          if (this%LineNumber == UNKNOWN_LINE_NUMBER) then
!             string = trim(this%fileName)
!          else
!             write(string,'(a,":",i0)', iostat=status) trim(this%fileName), this%LineNumber
!          end if
!       end if
!
!       string = '[' // trim(string) // ']'
!
!    end function toString
!
! end module SourceLocation_mod
