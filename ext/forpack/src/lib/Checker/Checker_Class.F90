Module Checker_Class

  use Object_Class            ,only:  Object_Type
  use iso_fortran_env         ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

  implicit none

  private
  public                      ::  Checker_Type

  Type  ,extends(Object_Type) ::  Checker_Type
    private
!     logical                     ::  Initialized = .False.
!     logical                     ::  Finite      = .True.
    logical                     ::  Fatal       = .True.
    logical                     ::  IsNormal    = .True.
    character(:)  ,allocatable  ::  Msg
    character(:)  ,allocatable  ::  MsgPrefix
    character(:)  ,allocatable  ::  CallProc
  contains
    private
    procedure ,public   ::  Initialize    =>  InitializeChecker
    procedure ,public   ::  Set           =>  SetCheckerProperties
    generic   ,public   ::  CheckFinite   =>  CheckFinite_REAL64_0d, CheckFinite_REAL64_1d, CheckFinite_REAL64_2d, CheckFinite_REAL64_3d, CheckFinite_REAL64_4d
    procedure           ::  CheckFinite_REAL64_0d
    procedure           ::  CheckFinite_REAL64_1d
    procedure           ::  CheckFinite_REAL64_2d
    procedure           ::  CheckFinite_REAL64_3d
    procedure           ::  CheckFinite_REAL64_4d
!     generic     ,public       ::  Normal      =>  Normal_0d, Normal_1d, Normal_2d
!     procedure                 ::  Normal_0d, Normal_1d, Normal_2d
!     generic     ,public       ::  AssessVariable    =>  AssessVariable_3d     !, AssessVariable_0d, AssessVariable_1d, AssessVariable_2d
!     procedure                 ::  AssessVariable_3d!, AssessVariable_0d, AssessVariable_1d, AssessVariable_2d
  End type

  integer ,parameter :: rkp=8

  Interface

    Module Subroutine InitializeChecker( This, MsgPrefix, CallProc, Fatal )
      class(Checker_Type)                                   ,intent(out)    ::  This
      character(*)                                ,optional ,intent(in)     ::  MsgPrefix
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(in)     ::  Fatal
    End Subroutine

    Module Subroutine SetCheckerProperties( This, MsgPrefix, CallProc, Fatal )
      class(Checker_Type)                                   ,intent(inout)  ::  This
      character(*)                                ,optional ,intent(in)     ::  MsgPrefix
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(in)     ::  Fatal
    End Subroutine

    Module Subroutine CheckFinite_REAL64_0d( This, Var, Message, VarName, CallProc, Fatal, iName, iData )
      class(Checker_Type)                                   ,intent(inout)  ::  This
      real(REAL64)                                          ,intent(in)     ::  Var
      character(*)                                ,optional ,intent(in)     ::  Message
      character(*)                                ,optional ,intent(in)     ::  VarName
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(in)     ::  Fatal
      character(*)                                ,optional ,intent(in)     ::  iName
      integer                                     ,optional ,intent(in)     ::  iData(:)
    End Subroutine

    Module Subroutine CheckFinite_REAL64_1d( This, Var, Message, VarName, CallProc, Fatal, iName, iData )
      class(Checker_Type)                                   ,intent(inout)  ::  This
      real(REAL64)                                          ,intent(in)     ::  Var(:)
      character(*)                                ,optional ,intent(in)     ::  Message
      character(*)                                ,optional ,intent(in)     ::  VarName
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(in)     ::  Fatal
      character(*)                                ,optional ,intent(in)     ::  iName
      integer                                     ,optional ,intent(in)     ::  iData(:)
    End Subroutine

    Module Subroutine CheckFinite_REAL64_2d( This, Var, Message, VarName, CallProc, Fatal, iName, iData )
      class(Checker_Type)                                   ,intent(inout)  ::  This
      real(REAL64)                                          ,intent(in)     ::  Var(:,:)
      character(*)                                ,optional ,intent(in)     ::  Message
      character(*)                                ,optional ,intent(in)     ::  VarName
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(in)     ::  Fatal
      character(*)                                ,optional ,intent(in)     ::  iName
      integer                                     ,optional ,intent(in)     ::  iData(:)
    End Subroutine

    Module Subroutine CheckFinite_REAL64_3d( This, Var, Message, VarName, CallProc, Fatal, iName, iData )
      class(Checker_Type)                                   ,intent(inout)  ::  This
      real(REAL64)                                          ,intent(in)     ::  Var(:,:,:)
      character(*)                                ,optional ,intent(in)     ::  Message
      character(*)                                ,optional ,intent(in)     ::  VarName
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(in)     ::  Fatal
      character(*)                                ,optional ,intent(in)     ::  iName
      integer                                     ,optional ,intent(in)     ::  iData(:)
    End Subroutine

    Module Subroutine CheckFinite_REAL64_4d( This, Var, Message, VarName, CallProc, Fatal, iName, iData )
      class(Checker_Type)                                   ,intent(inout)  ::  This
      real(REAL64)                                          ,intent(in)     ::  Var(:,:,:,:)
      character(*)                                ,optional ,intent(in)     ::  Message
      character(*)                                ,optional ,intent(in)     ::  VarName
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(in)     ::  Fatal
      character(*)                                ,optional ,intent(in)     ::  iName
      integer                                     ,optional ,intent(in)     ::  iData(:)
    End Subroutine

!     Module Subroutine Normal_0d( This, Variable, ProcName, VarName, Fatal )
!       class(Checker_Type)                                   ,intent(in)   ::  This
!       class(*)                                              ,intent(in)   ::  Variable
!       character(*)                                ,optional ,intent(in)   ::  ProcName
!       character(*)                                ,optional ,intent(in)   ::  VarName
!       logical                                     ,optional ,intent(in)   ::  Fatal
!     End Subroutine
!
!     Module Subroutine Normal_1d( This, Variable, ProcName, VarName, Fatal )
!       class(Checker_Type)                                   ,intent(in)   ::  This
!       class(*)  ,dimension(:)                               ,intent(in)   ::  Variable
!       character(*)                                ,optional ,intent(in)   ::  ProcName
!       character(*)                                ,optional ,intent(in)   ::  VarName
!       logical                                     ,optional ,intent(in)   ::  Fatal
!     End Subroutine
!
!     Module Subroutine Normal_2d( This, Variable, ProcName, VarName, Fatal )
!       class(Checker_Type)                                   ,intent(in)   ::  This
!       class(*)  ,dimension(:,:)                             ,intent(in)   ::  Variable
!       character(*)                                ,optional ,intent(in)   ::  ProcName
!       character(*)                                ,optional ,intent(in)   ::  VarName
!       logical                                     ,optional ,intent(in)   ::  Fatal
!     End Subroutine

!     Module Subroutine AssessVariable_0d( This, Var, CheckIsFinite,  ProcName, Name, Message, Fatal )
!       class(Checker_Type)                                   ,intent(in)   ::  This
!       real(8)                                               ,intent(in)   ::  Var
!       logical                                     ,optional ,intent(in)   ::  CheckIsFinite
!       character(*)                                ,optional ,intent(in)   ::  ProcName
!       character(*)                                ,optional ,intent(in)   ::  Name
!       character(*)                                ,optional ,intent(in)   ::  Message
!       logical                                     ,optional ,intent(in)   ::  Fatal
!     End Subroutine

!     Module Subroutine AssessVariable_1d( This, Var, CheckIsFinite,  ProcName, Name, Message, Fatal )
!       class(Checker_Type)                                   ,intent(in)   ::  This
!       real(8)   ,dimension(:)                               ,intent(in)   ::  Var
!       logical                                     ,optional ,intent(in)   ::  CheckIsFinite
!       character(*)                                ,optional ,intent(in)   ::  ProcName
!       character(*)                                ,optional ,intent(in)   ::  Name
!       character(*)                                ,optional ,intent(in)   ::  Message
!       logical                                     ,optional ,intent(in)   ::  Fatal
!     End Subroutine

!     Module Subroutine AssessVariable_2d( This, Var, CheckIsFinite,  ProcName, Name, Message, Fatal )
!       class(Checker_Type)                                   ,intent(in)   ::  This
!       real(8)   ,dimension(:,:)                             ,intent(in)   ::  Var
!       logical                                     ,optional ,intent(in)   ::  CheckIsFinite
!       character(*)                                ,optional ,intent(in)   ::  ProcName
!       character(*)                                ,optional ,intent(in)   ::  Name
!       character(*)                                ,optional ,intent(in)   ::  Message
!       logical                                     ,optional ,intent(in)   ::  Fatal
!     End Subroutine

    Module Subroutine AssessVariable_3d( This, Var, CheckIsFinite, ProcName, Name, Message, Fatal )
      class(Checker_Type)                                   ,intent(in)   ::  This
      class(*)   ,dimension(:,:,:)                          ,intent(in)   ::  Var
!       real(8)   ,dimension(:,:,:)                           ,intent(in)   ::  Var
      logical                                     ,optional ,intent(in)   ::  CheckIsFinite
      character(*)                                ,optional ,intent(in)   ::  ProcName
      character(*)                                ,optional ,intent(in)   ::  Name
      character(*)                                ,optional ,intent(in)   ::  Message
      logical                                     ,optional ,intent(in)   ::  Fatal
    End Subroutine


  End Interface

End Module