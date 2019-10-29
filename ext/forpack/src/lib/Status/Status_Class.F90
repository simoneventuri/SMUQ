Module Status_Class

  use StatusLevel_Class         ,only:  StatusLevel_Type

  implicit none

  private
  public  ::  Status_Type
  public  ::  UpdateStatus
  public  ::  AddProcName

  Type    ::  Status_Type
    integer                                   ::  Code    =   0
    integer                                   ::  NItems  =   0
    type(StatusLevel_Type)  ,allocatable      ::  Items(:)
    character(:)  ,allocatable                ::  Procedure
    character(:)  ,allocatable  ,dimension(:) ::  Message
  contains
    private
    procedure   ,public   ::  Ok              =>      IsStatusOk
    procedure   ,public   ::  NotOk           =>      IsStatusNotOk
    procedure   ,public   ::  Set             =>      AddStatusProperties
    procedure   ,public   ::  AddProcedure    =>      AddProcedureToStatus
    procedure   ,public   ::  AddLine         =>      AddLineToStatus
    generic     ,public   ::  assignment(=)   =>      AssignStatusToINT
    generic     ,public   ::  operator(==)    =>      EquivStatusToINT
    generic     ,public   ::  operator(/=)    =>      NonEqStatusToINT
    generic     ,public   ::  SetMessage      =>      SetStatusMessage_0d, SetStatusMessage_1d
    procedure             ::  AssignStatusToINT
    procedure             ::  EquivStatusToINT, NonEqStatusToINT
    procedure             ::  SetStatusMessage_0d, SetStatusMessage_1d
!     procedure   ,public   ::  SetCode         =>      SetStatusCode
!     procedure   ,public   ::  SetProcedure    =>      SetStatusProcedure
!     procedure   ,public   ::  SetParameters   =>      SetStatusParameters

    procedure   ,public   ::  AddLevel  =>  AddStatusLevel
    procedure   ,public   ::  Write     =>  WriteStatus
    procedure   ,public   ::  Check     =>  CheckStatus


  End Type

  Interface

    Module Pure Function IsStatusOk( This ) result(Ok)
      class(Status_Type)                                    ,intent(in)     ::  This
      logical                                                               ::  Ok
    End Function

    Module Pure Function IsStatusNotOk( This ) result(NotOk)
      class(Status_Type)                                    ,intent(in)     ::  This
      logical                                                               ::  NotOk
    End Function


    Module Pure Subroutine AddStatusProperties( This, ProcName, Message, Messages )
      class(Status_Type)                                    ,intent(inout)  ::  This
      character(*)                                ,optional ,intent(in)     ::  ProcName
      character(*)                                ,optional ,intent(in)     ::  Message
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  Messages
    End Subroutine

    Module Pure Subroutine AddProcedureToStatus( This, ProcName )
      class(Status_Type)                                    ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  ProcName
    End Subroutine

    Module Pure Subroutine AddLineToStatus( This, Line )
      class(Status_Type)                                    ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Line
    End Subroutine

    Module Pure Subroutine SetStatusMessage_0d( This, Message )
      class(Status_Type)                                    ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Message
    End Subroutine

    Module Pure Subroutine SetStatusMessage_1d( This, Message )
      class(Status_Type)                                    ,intent(inout)  ::  This
      character(*)  ,dimension(:)                           ,intent(in)     ::  Message
    End Subroutine

    Recursive Pure Module Subroutine AssignStatusToINT( lhs, ivar )
      class(Status_Type)                                    ,intent(inout)  ::  lhs
      integer                                               ,intent(in)     ::  ivar
    End Subroutine

    Recursive Pure Module Function EquivStatusToINT( This, ivar ) result(Res)
      class(Status_Type)                                    ,intent(in)     ::  This
      integer                                               ,intent(in)     ::  ivar
      logical                                                               ::  Res
    End Function

    Recursive Pure Module Function NonEqStatusToINT( This, ivar ) result(Res)
      class(Status_Type)                                    ,intent(in)     ::  This
      integer                                               ,intent(in)     ::  ivar
      logical                                                               ::  Res
    End Function


!     Module Pure Subroutine SetStatusCode( This, Code )
!       class(Status_Type)                                    ,intent(inout)  ::  This
!       integer                                               ,intent(in)     ::  Code
!     End Subroutine
!
!     Module Pure Subroutine SetStatusProcedure( This, Procedure )
!       class(Status_Type)                                    ,intent(inout)  ::  This
!       character(*)                                          ,intent(in)     ::  Procedure
!     End Subroutine
!
!
!
!     Module Pure Subroutine SetStatusParameters( This, Code, Procedure, Msg, MsgVec )
!       class(Status_Type)                                    ,intent(inout)  ::  This
!       integer                                     ,optional ,intent(in)     ::  Code
!       character(*)                                ,optional ,intent(in)     ::  Procedure
!       character(*)                                ,optional ,intent(in)     ::  Msg
!       character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  MsgVec
!     End Subroutine

  End Interface


  Interface

    Module Pure Subroutine AddProcName( ProcName, Status )
      character(*)                                ,optional ,intent(in)     ::  ProcName
      type(Status_Type)                           ,optional ,intent(inout)  ::  Status
    End Subroutine

  End Interface




  Interface


    Module Subroutine AddStatusLevel( This, File, Line, Proc, Desc )
      class(Status_Type)                                    ,intent(inout)  ::  This
      character(*)                                ,optional ,intent(in)     ::  File
      integer                                     ,optional ,intent(in)     ::  Line
      character(*)                                ,optional ,intent(in)     ::  Proc
      character(*)                                ,optional ,intent(in)     ::  Desc
    End Subroutine

    Module Subroutine WriteStatus( This )
      class(Status_Type)                                    ,intent(in)     ::  This
    End Subroutine

    Module Subroutine CheckStatus( This, File, Line, Proc, Desc, Write, Fatal )
      class(Status_Type)                                    ,intent(inout)  ::  This
      character(*)                                ,optional ,intent(in)     ::  File
      integer                                     ,optional ,intent(in)     ::  Line
      character(*)                                ,optional ,intent(in)     ::  Proc
      character(*)                                ,optional ,intent(in)     ::  Desc
      logical                                     ,optional ,intent(in)     ::  Write
      logical                                     ,optional ,intent(in)     ::  Fatal
    End Subroutine

    Module Function UpdateStatus( Status, Desc, Err, iStat, File, Line, Proc, ExitLogger ) result(Indicator)
      type(Status_Type)                           ,optional ,intent(inout)  ::  Status
      character(*)                                ,optional ,intent(in)     ::  Desc
      logical                                     ,optional ,intent(in)     ::  Err
      integer                                     ,optional ,intent(in)     ::  iStat
      character(*)                                ,optional ,intent(in)     ::  File
      integer                                     ,optional ,intent(in)     ::  Line
      character(*)                                ,optional ,intent(in)     ::  Proc
      logical                                     ,optional ,intent(in)     ::  ExitLogger
      logical                                                               ::  Indicator
    End Function



  End Interface


End Module