Module Error_Class

  use String_Library      ,only:  Text_Type, String_Type

  implicit none

  private
  public  ::  Error

  Type                    ::  Error_Type
    logical               ::  Critical  =   .True.    ! Indicator of a critical error, if true, the code is stopped
    integer ,allocatable  ::  Units(:)
    type(String_Type)     ::  Title
    type(Text_Type)       ::  Message
  contains
    private
    Final                 ::  FinalizeError
    generic     ,public   ::  Raise           =>      RaiseErrorGeneric, RaiseErrorFromChar0D, RaiseErrorFromChar1D, RaiseErrorFromText
    generic     ,public   ::  NotImplemented  =>      ErrorNotImplemented_0d
    procedure   ,public   ::  Open            =>      ErrorOpen
    procedure   ,public   ::  Read            =>      ErrorRead
    procedure   ,public   ::  Write           =>      ErrorWrite
    procedure   ,public   ::  Rewind          =>      ErrorRewind
    procedure   ,public   ::  Close           =>      ErrorClose
    procedure   ,public   ::  Allocate        =>      ErrorAllocate
    procedure   ,public   ::  Deallocate      =>      ErrorDeallocate
    generic     ,public   ::  Add_Line        =>      Add_Error_Line_0d, Add_Error_Line_1d
    generic     ,public   ::  SetOutputUnit   =>      SetErrorOutputUnit_0d, SetErrorOutputUnit_1d
    procedure   ,public   ::  Set_Title
    procedure   ,public   ::  Set_Critical
    procedure             ::  RaiseErrorGeneric
    procedure             ::  RaiseErrorFromChar0D
    procedure             ::  RaiseErrorFromChar1D
    procedure             ::  RaiseErrorFromText
    procedure             ::  ErrorNotImplemented_0d
    procedure             ::  Add_Error_Line_0d
    procedure             ::  Add_Error_Line_1d
    procedure             ::  Get_Error_Length
    procedure             ::  SetErrorOutputUnit_0d
    procedure             ::  SetErrorOutputUnit_1d
    procedure             ::  Get_Units
  End Type

  type(Error_Type)     ::  Error

  Interface

    Pure Module Subroutine FinalizeError( This )
      type(Error_Type)                                      ,intent(inout)  ::  This
    End Subroutine

    Module Subroutine ErrorOpen( This, Line, Lines, ProcName, Unit, File, iostat )
      class(Error_Type)                                     ,intent(inout)  ::  This
      character(*)                                ,optional ,intent(in)     ::  Line
      character(*)                                ,optional ,intent(in)     ::  Lines(:)
      character(*)                                ,optional ,intent(in)     ::  ProcName
      integer                                     ,optional ,intent(in)     ::  Unit                                    !< File unit number
      character(*)                                ,optional ,intent(in)     ::  File                                    !< File name
      integer                                     ,optional ,intent(in)     ::  iostat
    End Subroutine

    Module Subroutine ErrorRead( This, Message, Messages, File, LineNumber, LineContent, ProcName, SourceLoc, Status, Unit )
      class(Error_Type)                                     ,intent(inout)  ::  This
      character(*)                                ,optional ,intent(in)     ::  Message
      character(*)                                ,optional ,intent(in)     ::  Messages(:)
      character(*)                                ,optional ,intent(in)     ::  File
      integer                                     ,optional ,intent(in)     ::  LineNumber
      character(*)                                ,optional ,intent(in)     ::  LineContent
      character(*)                                ,optional ,intent(in)     ::  ProcName
      character(*)                                ,optional ,intent(in)     ::  SourceLoc
      integer                                     ,optional ,intent(in)     ::  Status
      integer                                     ,optional ,intent(in)     ::  Unit
    End Subroutine

    Module Subroutine ErrorWrite( This, Line, Lines, ProcName, Unit, File, iostat )
      class(Error_Type)                                     ,intent(inout)  ::  This
      character(*)                                ,optional ,intent(in)     ::  Line
      character(*)                                ,optional ,intent(in)     ::  Lines(:)
      character(*)                                ,optional ,intent(in)     ::  ProcName
      integer                                     ,optional ,intent(in)     ::  Unit                                    !< File unit number
      character(*)                                ,optional ,intent(in)     ::  File                                    !< File name
      integer                                     ,optional ,intent(in)     ::  iostat
    End Subroutine

    Module Subroutine ErrorRewind( This, Line, Lines, ProcName, Unit, File, iostat )
      class(Error_Type)                                     ,intent(inout)  ::  This
      character(*)                                ,optional ,intent(in)     ::  Line
      character(*)                                ,optional ,intent(in)     ::  Lines(:)
      character(*)                                ,optional ,intent(in)     ::  ProcName
      integer                                     ,optional ,intent(in)     ::  Unit                                    !< File unit number
      character(*)                                ,optional ,intent(in)     ::  File                                    !< File name
      integer                                     ,optional ,intent(in)     ::  iostat
    End Subroutine

    Module Subroutine ErrorClose( This, Line, Lines, ProcName, Unit, File, iostat )
      class(Error_Type)                                     ,intent(inout)  ::  This
      character(*)                                ,optional ,intent(in)     ::  Line
      character(*)                                ,optional ,intent(in)     ::  Lines(:)
      character(*)                                ,optional ,intent(in)     ::  ProcName
      integer                                     ,optional ,intent(in)     ::  Unit                                    !< File unit number
      character(*)                                ,optional ,intent(in)     ::  File                                    !< File name
      integer                                     ,optional ,intent(in)     ::  iostat
    End Subroutine

    Module Subroutine ErrorAllocate( This, Line, Lines, ProcName, Name, Stat, ErrMsg )
      class(Error_Type)                                     ,intent(inout)  ::  This
      character(*)                                ,optional ,intent(in)     ::  Line
      character(*)                                ,optional ,intent(in)     ::  Lines(:)
      character(*)                                ,optional ,intent(in)     ::  ProcName
      character(*)                                ,optional ,intent(in)     ::  Name                                    !< Name of the variable
      integer                                     ,optional ,intent(in)     ::  Stat
      character(*)                                ,optional ,intent(in)     ::  ErrMsg
    End Subroutine

    Module Subroutine ErrorDeallocate( This, Line, Lines, ProcName, Name, Stat, ErrMsg )
      class(Error_Type)                                     ,intent(inout)  ::  This
      character(*)                                ,optional ,intent(in)     ::  Line
      character(*)                                ,optional ,intent(in)     ::  Lines(:)
      character(*)                                ,optional ,intent(in)     ::  ProcName
      character(*)                                ,optional ,intent(in)     ::  Name                                    !< Name of the variable
      integer                                     ,optional ,intent(in)     ::  Stat
      character(*)                                ,optional ,intent(in)     ::  ErrMsg
    End Subroutine

    Module Subroutine ErrorNotImplemented_0d( This, Line, Unit, Units, ProcName )
      class(Error_Type)                                     ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Line
      integer                                     ,optional ,intent(in)     ::  Unit
      integer                                     ,optional ,intent(in)     ::  Units(:)
      character(*)                                ,optional ,intent(in)     ::  ProcName
    End Subroutine

    Module Subroutine RaiseErrorGeneric( This, Unused, Title, Unused2, Line, Lines, Unit, Units, ProcName )
      class(Error_Type)                                     ,intent(inout)  ::  This
      logical                                     ,optional ,intent(in)     ::  Unused(:,:)
      character(*)                                ,optional ,intent(in)     ::  Title
      logical                                     ,optional ,intent(in)     ::  Unused2(:,:)
      character(*)                                ,optional ,intent(in)     ::  Line
      character(*)                                ,optional ,intent(in)     ::  Lines(:)
      integer                                     ,optional ,intent(in)     ::  Unit
      integer                                     ,optional ,intent(in)     ::  Units(:)
      character(*)                                ,optional ,intent(in)     ::  ProcName
    End Subroutine

    Module Subroutine RaiseErrorFromChar0D( This, ErrMsg, Unused, Title, Unit, Units, ProcName )
      class(Error_Type)                                     ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  ErrMsg
      logical                                     ,optional ,intent(in)     ::  Unused(:,:)
      character(*)                                ,optional ,intent(in)     ::  Title
      integer                                     ,optional ,intent(in)     ::  Unit
      integer                                     ,optional ,intent(in)     ::  Units(:)
      character(*)                                ,optional ,intent(in)     ::  ProcName
    End Subroutine

!     Module Subroutine RaiseErrorFromChar0DAlloc( This, ErrMsg, Unused, Title, Unit, Units, ProcName )
!       class(Error_Type)                                     ,intent(inout)  ::  This
!       character(:)  ,allocatable                            ,intent(in)     ::  ErrMsg
!       logical                                     ,optional ,intent(in)     ::  Unused(:,:)
!       character(*)                                ,optional ,intent(in)     ::  Title
!       integer                                     ,optional ,intent(in)     ::  Unit
!       integer       ,optional ,dimension(:)                 ,intent(in)     ::  Units
!       character(*)                                ,optional ,intent(in)     ::  ProcName
!     End Subroutine

    Module Subroutine RaiseErrorFromChar1D( This, ErrMsg, Unused, Title, Unit, Units, ProcName )
      class(Error_Type)                                     ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  ErrMsg(:)
      logical                                     ,optional ,intent(in)     ::  Unused(:,:)
      character(*)                                ,optional ,intent(in)     ::  Title
      integer                                     ,optional ,intent(in)     ::  Unit
      integer                                     ,optional ,intent(in)     ::  Units(:)
      character(*)                                ,optional ,intent(in)     ::  ProcName
    End Subroutine

    Module Subroutine RaiseErrorFromText( This, ErrMsg, Unused, Title, Unit, Units, ProcName )
      class(Error_Type)                                     ,intent(inout)  ::  This
      type(Text_Type)                                       ,intent(in)     ::  ErrMsg
      logical                                     ,optional ,intent(in)     ::  Unused(:,:)
      character(*)                                ,optional ,intent(in)     ::  Title
      integer                                     ,optional ,intent(in)     ::  Unit
      integer                                     ,optional ,intent(in)     ::  Units(:)
      character(*)                                ,optional ,intent(in)     ::  ProcName
    End Subroutine

    Pure Module Subroutine Set_Title( This, Title )
      class(Error_Type)                                     ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Title
    End Subroutine

    Pure Module Elemental Function Get_Error_Length( This ) result(Length)
      class(Error_Type)                                     ,intent(in)     ::  This
      integer                                                               ::  Length
    End Function

    Module Subroutine Add_Error_Line_0d( This, Line )
      class(Error_Type)                                     ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Line
    End Subroutine

    Module Subroutine Add_Error_Line_1d( This, Lines )
      class(Error_Type)                                     ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Lines(:)
    End Subroutine

    Pure Module Subroutine SetErrorOutputUnit_0d( This, Unit )
      class(Error_Type)                                     ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  Unit
    End Subroutine

    Pure Module Subroutine SetErrorOutputUnit_1d( This, Units )
      class(Error_Type)                                     ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  Units(:)
    End Subroutine

    Pure Module Subroutine Set_Critical( This, Critical )
      class(Error_Type)                                     ,intent(inout)  ::  This
      logical                                               ,intent(in)     ::  Critical
    End Subroutine

    Module Subroutine Get_Units( This, ErrUnit, Unit, Units )
      class(Error_Type)                                     ,intent(in)     ::  This
      integer ,allocatable                                  ,intent(out)    ::  ErrUnit(:)
      integer                                     ,optional ,intent(in)     ::  Unit
      integer                                     ,optional ,intent(in)     ::  Units(:)
    End Subroutine

  End Interface

# ifdef INTEL_COMPILER
    contains
#   include "Error_include.F90"
# endif

End Module
