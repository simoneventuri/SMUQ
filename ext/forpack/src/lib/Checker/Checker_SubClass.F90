SubModule(Checker_Class) Checker_SubClass

  use Logger_Class            ,only:  Logger
  use Error_Class             ,only:  Error
  use iso_fortran_env         ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128
  use Arithmetic_Library      ,only:  Get_IEEE_Class_Name
  use Utilities_Library       ,only:  PresentAndNotEmpty, GetVarRank
  use Checker_Tools           ,only:  IsNormal, IsFinite
  use String_Library          ,only:  Inline, Convert_To_String
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  contains

Module Procedure InitializeChecker
  This%IsNormal   =   .True.
  This%Msg        =   ""
  This%Fatal      =   GetOptArgValue( .True.  , Fatal     )
  This%CallProc   =   GetOptArgValue( ""      , CallProc  )
  This%MsgPrefix  =   GetOptArgValue( ""      , MsgPrefix )
End Procedure

Module Procedure SetCheckerProperties
  if ( present(MsgPrefix) ) This%MsgPrefix = MsgPrefix
  if ( present(CallProc ) ) This%CallProc  = CallProc
  if ( present(Fatal    ) ) This%Fatal     = Fatal
End Procedure

Module Procedure CheckFinite_REAL64_0d
  character(*)                                              ,parameter  ::  ProcName='CheckFinite_REAL64_0d'
  logical                                                               ::  Fatal_
  character(:)  ,allocatable                                            ::  Message_
  character(:)  ,allocatable                                            ::  VarName_
  character(:)  ,allocatable                                            ::  CallProc_
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable                                            ::  IEEEType

  This%IsNormal   =   IsFinite(Var)
  This%Msg        =   ""

  if ( This%IsNormal ) return

  Message_    =   GetOptArgValue( "", Message )
  VarName_    =   GetOptArgValue( "", VarName )
  CallProc_   =   GetOptArgValue( This%CallProc, CallProc  )
  Fatal_      =   GetOptArgValue( This%Fatal, Fatal )
  if ( len_trim(This%MsgPrefix) /= 0 ) Message_ = This%MsgPrefix//" "//Message_

  if ( len_trim(Message_)  /= 0 ) This%Msg = Message_//" "
  if ( len_trim(VarName_)  /= 0 ) This%Msg = This%Msg//"Var='"//VarName//"' "
  if ( len_trim(CallProc_) /= 0 ) This%Msg = This%Msg//"Proc='"//CallProc_//"' "

  IEEEType    =   Get_IEEE_Class_Name( Var )

  call Logger%Entering( ProcName )
  call Logger%Write( This%Msg )

!   if ( .Not. Fatal_ ) then
!     call Logger%Exiting
!     return
!   end if

  ProcPath      =   Logger%GetPath()
  call Error%Set_Critical(Fatal_)
  call Error%Set_Title( "Variable test failed" )
  if ( len_trim(Message_) /= 0 )  call Error%Add_Line( "-> IEEE type:       " // IEEEType )
  if ( len_trim(Message_) /= 0 )  call Error%Add_Line( "-> Message:         " // Message_ )
  if ( len_trim(VarName_) /= 0 )  call Error%Add_Line( "-> Variable:        " // VarName_ )
  if ( len_trim(CallProc_) /= 0 ) call Error%Add_Line( "-> Procedure:       " // CallProc_ )
  if ( len_trim(ProcPath) /= 0 )  call Error%Add_Line( "-> Procedure path:  " // ProcPath )
  if ( present(iName) .and. present(iData) ) call Error%Add_Line( "-> "//iName//":   "// Inline(iData) )
  call Error%Raise()

  call Logger%Exiting

End Procedure

Module Procedure CheckFinite_REAL64_1d
  character(*)                                              ,parameter  ::  ProcName='CheckFinite_REAL64_1d'
  logical                                                               ::  Fatal_
  character(:)  ,allocatable                                            ::  Message_
  character(:)  ,allocatable                                            ::  VarName_
  character(:)  ,allocatable                                            ::  CallProc_
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable                                            ::  IEEEType

  This%IsNormal   =   IsFinite(Var)
  This%Msg        =   ""

  if ( This%IsNormal ) return

  Message_    =   GetOptArgValue( "", Message )
  VarName_    =   GetOptArgValue( "", VarName )
  CallProc_   =   GetOptArgValue( This%CallProc, CallProc  )
  Fatal_      =   GetOptArgValue( This%Fatal, Fatal )
  if ( len_trim(This%MsgPrefix) /= 0 ) Message_ = This%MsgPrefix//" "//Message_

  if ( len_trim(Message_)  /= 0 ) This%Msg = Message_//" "
  if ( len_trim(VarName_)  /= 0 ) This%Msg = This%Msg//"Var='"//VarName//"' "
  if ( len_trim(CallProc_) /= 0 ) This%Msg = This%Msg//"Proc='"//CallProc_//"' "

  IEEEType    =   Get_IEEE_Class_Name( sum(Var) )

  call Logger%Entering( ProcName )
  call Logger%Write( This%Msg )

!   if ( .Not. Fatal_ ) then
!     call Logger%Exiting
!     return
!   end if

  ProcPath      =   Logger%GetPath()
  call Error%Set_Critical(Fatal_)
  call Error%Set_Title( "Variable test failed" )
  if ( len_trim(Message_) /= 0 )  call Error%Add_Line( "-> IEEE type:       " // IEEEType )
  if ( len_trim(Message_) /= 0 )  call Error%Add_Line( "-> Message:         " // Message_ )
  if ( len_trim(VarName_) /= 0 )  call Error%Add_Line( "-> Variable:        " // VarName_ )
  if ( len_trim(CallProc_) /= 0 ) call Error%Add_Line( "-> Procedure:       " // CallProc_ )
  if ( len_trim(ProcPath) /= 0 )  call Error%Add_Line( "-> Procedure path:  " // ProcPath )
  if ( present(iName) .and. present(iData) ) call Error%Add_Line( "-> "//iName//":   "// Inline(iData) )
  call Error%Raise()

  call Logger%Exiting

End Procedure

Module Procedure CheckFinite_REAL64_2d
  character(*)                                              ,parameter  ::  ProcName='CheckFinite_REAL64_2d'
  logical                                                               ::  Fatal_
  character(:)  ,allocatable                                            ::  Message_
  character(:)  ,allocatable                                            ::  VarName_
  character(:)  ,allocatable                                            ::  CallProc_
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable                                            ::  IEEEType

  This%IsNormal   =   IsFinite(Var)
  This%Msg        =   ""

  if ( This%IsNormal ) return

  Message_    =   GetOptArgValue( "", Message )
  VarName_    =   GetOptArgValue( "", VarName )
  CallProc_   =   GetOptArgValue( This%CallProc, CallProc  )
  Fatal_      =   GetOptArgValue( This%Fatal, Fatal )
  if ( len_trim(This%MsgPrefix) /= 0 ) Message_ = This%MsgPrefix//" "//Message_

  if ( len_trim(Message_)  /= 0 ) This%Msg = Message_//" "
  if ( len_trim(VarName_)  /= 0 ) This%Msg = This%Msg//"Var='"//VarName//"' "
  if ( len_trim(CallProc_) /= 0 ) This%Msg = This%Msg//"Proc='"//CallProc_//"' "

  IEEEType    =   Get_IEEE_Class_Name( sum(Var) )

  call Logger%Entering( ProcName )
  call Logger%Write( This%Msg )

!   if ( .Not. Fatal_ ) then
!     call Logger%Exiting
!     return
!   end if

  ProcPath      =   Logger%GetPath()
  call Error%Set_Critical(Fatal_)
  call Error%Set_Title( "Variable test failed" )
  if ( len_trim(Message_) /= 0 )  call Error%Add_Line( "-> IEEE type:       " // IEEEType )
  if ( len_trim(Message_) /= 0 )  call Error%Add_Line( "-> Message:         " // Message_ )
  if ( len_trim(VarName_) /= 0 )  call Error%Add_Line( "-> Variable:        " // VarName_ )
  if ( len_trim(CallProc_) /= 0 ) call Error%Add_Line( "-> Procedure:       " // CallProc_ )
  if ( len_trim(ProcPath) /= 0 )  call Error%Add_Line( "-> Procedure path:  " // ProcPath )
  if ( present(iName) .and. present(iData) ) call Error%Add_Line( "-> "//iName//":   "// Inline(iData) )
  call Error%Raise()

  call Logger%Exiting

End Procedure

Module Procedure CheckFinite_REAL64_3d

  character(*)                                              ,parameter  ::  ProcName='CheckFinite_REAL64_3d'
  logical                                                               ::  Fatal_
  character(:)  ,allocatable                                            ::  Message_
  character(:)  ,allocatable                                            ::  VarName_
  character(:)  ,allocatable                                            ::  CallProc_
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable                                            ::  IEEEType
  integer                                                               ::  i

  This%IsNormal   =   IsFinite(Var)
  This%Msg        =   ""

  if ( This%IsNormal ) return

  Message_    =   GetOptArgValue( "", Message )
  VarName_    =   GetOptArgValue( "", VarName )
  CallProc_   =   GetOptArgValue( This%CallProc, CallProc  )
  Fatal_      =   GetOptArgValue( This%Fatal, Fatal )
  if ( len_trim(This%MsgPrefix) /= 0 ) Message_ = This%MsgPrefix//" "//Message_

  if ( len_trim(Message_)  /= 0 ) This%Msg = Message_//" "
  if ( len_trim(VarName_)  /= 0 ) This%Msg = This%Msg//"Var='"//VarName//"' "
  if ( len_trim(CallProc_) /= 0 ) This%Msg = This%Msg//"Proc='"//CallProc_//"' "

  IEEEType    =   Get_IEEE_Class_Name( sum(Var) )

  call Logger%Entering( ProcName )
  call Logger%Write( This%Msg )

!   if ( .Not. Fatal_ ) then
!     call Logger%Exiting
!     return
!   end if

  ProcPath      =   Logger%GetPath()
  call Error%Set_Critical(Fatal_)
  call Error%Set_Title( "Variable test failed" )
  if ( len_trim(Message_) /= 0 )  call Error%Add_Line( "-> IEEE type:       " // IEEEType )
  if ( len_trim(Message_) /= 0 )  call Error%Add_Line( "-> Message:         " // Message_ )
  if ( len_trim(VarName_) /= 0 )  call Error%Add_Line( "-> Variable:        " // VarName_ )
  if ( len_trim(CallProc_) /= 0 ) call Error%Add_Line( "-> Procedure:       " // CallProc_ )
  if ( len_trim(ProcPath) /= 0 )  call Error%Add_Line( "-> Procedure path:  " // ProcPath )
  if ( present(iName) .and. present(iData) ) call Error%Add_Line( "-> "//iName//":   "// Inline(iData) )
  call Error%Add_Line( "-> Variable rank:   " // GetVarRank(Var) )


  do i = 1,size(Var,3)
    call Logger%Write( "Var(:,:,"//Convert_To_String(i)//") = " )
    call Logger%Write( "  ", Var(:,:,3), Fr="es15.8" )
  end do
!
!   do iY = 1,This%Field%Ny
!   do iX = 1,This%Field%Nx
!     call Logger%Write( "iX = ", iX, "iY = ", iY, "Field%RHS(:,iX,iY) = ", This%Field%RHS(:,iX,iY), F2="i4", F4="i4", F6="es15.8" )
!   end do
!   end do
!
!   do iY = 1,This%Field%Ny
!   do iX = 1,This%Field%Nx
!     call Logger%Write( "iX = ", iX, "iY = ", iY, "Field%Uc(:,iX,iY) = ", This%Field%Uc(:,iX,iY), F2="i4", F4="i4", F6="es15.8" )
!   end do
!   end do
!


  call Error%Raise()

  call Logger%Exiting

End Procedure

Module Procedure CheckFinite_REAL64_4d

  character(*)                                              ,parameter  ::  ProcName='CheckFinite_REAL64_4d'
  logical                                                               ::  Fatal_
  character(:)  ,allocatable                                            ::  Message_
  character(:)  ,allocatable                                            ::  VarName_
  character(:)  ,allocatable                                            ::  CallProc_
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable                                            ::  IEEEType

  This%IsNormal   =   IsFinite(Var)
  This%Msg        =   ""

  if ( This%IsNormal ) return

  Message_    =   GetOptArgValue( "", Message )
  VarName_    =   GetOptArgValue( "", VarName )
  CallProc_   =   GetOptArgValue( This%CallProc, CallProc  )
  Fatal_      =   GetOptArgValue( This%Fatal, Fatal )
  if ( len_trim(This%MsgPrefix) /= 0 ) Message_ = This%MsgPrefix//" "//Message_

  if ( len_trim(Message_)  /= 0 ) This%Msg = Message_//" "
  if ( len_trim(VarName_)  /= 0 ) This%Msg = This%Msg//"Var='"//VarName//"' "
  if ( len_trim(CallProc_) /= 0 ) This%Msg = This%Msg//"Proc='"//CallProc_//"' "

  IEEEType    =   Get_IEEE_Class_Name( sum(Var) )

  call Logger%Entering( ProcName )
  call Logger%Write( This%Msg )

!   if ( .Not. Fatal_ ) then
!     call Logger%Exiting
!     return
!   end if

  ProcPath      =   Logger%GetPath()
  call Error%Set_Critical(Fatal_)
  call Error%Set_Title( "Variable test failed" )
  if ( len_trim(Message_) /= 0 )  call Error%Add_Line( "-> IEEE type:       " // IEEEType )
  if ( len_trim(Message_) /= 0 )  call Error%Add_Line( "-> Message:         " // Message_ )
  if ( len_trim(VarName_) /= 0 )  call Error%Add_Line( "-> Variable:        " // VarName_ )
  if ( len_trim(CallProc_) /= 0 ) call Error%Add_Line( "-> Procedure:       " // CallProc_ )
  if ( len_trim(ProcPath) /= 0 )  call Error%Add_Line( "-> Procedure path:  " // ProcPath )
  if ( present(iName) .and. present(iData) ) call Error%Add_Line( "-> "//iName//":   "// Inline(iData) )
  call Error%Raise()

  call Logger%Exiting

End Procedure

# if 0
Module Procedure Normal_0d
  logical                                                             ::  Fatal_
  character(:)  ,allocatable                                          ::  ProcPath
  if ( IsNormal(Var) ) return
  Fatal_  =   This%GetOptArgValue( This%Fatal, Fatal )
  if ( .Not. Fatal_ ) return
  ProcPath    =   Logger%GetPath()
  call Error%Set_Title( "NaN found !!!" )
  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  A NaN value has been found in a scalar variable." )
  call Error%Add_Line( "Details:" )
  if ( present(ProcName)) call Error%Add_Line( " * Procedure raising the error: " // ProcName )
  if ( present(VarName))  call Error%Add_Line( " * Variable name:               " // VarName )
  if (len(ProcPath)/=0)   call Error%Add_Line( " * Full procedure path:         " // ProcPath )
  call Error%Raise( Unit=Logger%GetUnit() )
End Procedure

Module Procedure Normal_1d
  integer                                                             ::  i
  logical                                                             ::  Fatal_
  character(:)  ,allocatable                                          ::  ProcPath
  Fatal_  =   This%GetOptArgValue( This%Fatal, Fatal )
  do i = 1,size(Variable,1)
    if ( IsNormal(Variable(i)) ) cycle
    if ( .Not. Fatal_ ) cycle
    ProcPath    =   Logger%GetPath()
    call Error%Set_Title( "NaN found !!!" )
    call Error%Add_Line( "Description:" )
    call Error%Add_Line( "  A NaN value has been found in a rank-1 variable." )
    call Error%Add_Line( "Details:" )
    if ( present(ProcName)) call Error%Add_Line( " * Procedure raising the error: " // ProcName )
    if ( present(VarName))  call Error%Add_Line( " * Variable name:               " // VarName )
    if (len(ProcPath)/=0)   call Error%Add_Line( " * Full procedure path:         " // ProcPath )
                            call Error%Add_Line( " * Element: A(i) with i = " // Convert_To_String(i) )
    call Error%Raise( Unit=Logger%GetUnit() )
  end do
End Procedure

Module Procedure Normal_2d
  integer                                                             ::  i, j
  logical                                                             ::  Fatal_
  character(:)  ,allocatable                                          ::  ProcPath
  Fatal_  =   This%GetOptArgValue( This%Fatal, Fatal )
  do j = 1,size(Variable,2)
  do i = 1,size(Variable,1)
    if ( IsNormal(Variable(i,j)) ) cycle
    if ( .Not. Fatal_ ) cycle
    ProcPath    =   Logger%GetPath()
    call Error%Set_Title( "NaN found !!!" )
    call Error%Add_Line( "Description:" )
    call Error%Add_Line( "  A NaN value has been found in a rank-2 variable." )
    call Error%Add_Line( "Details:" )
    if ( present(ProcName)) call Error%Add_Line( " * Procedure raising the error: " // ProcName )
    if ( present(VarName))  call Error%Add_Line( " * Variable name:               " // VarName )
    if (len(ProcPath)/=0)   call Error%Add_Line( " * Full procedure path:         " // ProcPath )
                            call Error%Add_Line( " * Element: A(i,j) with i = " // Convert_To_String(i) // "  j = " // Convert_To_String(j) )
    call Error%Raise( Unit=Logger%GetUnit() )
  end do
  end do
End Procedure




Module Procedure AssessVariable_3d

  use Utilities_Module          ,only:  PresentAndTrue, PresentAndNotEmpty
  use Arithmetic_Library        ,only:  IsFinite, Get_IEEE_Class_Name
  use Utilities_Module          ,only:


  integer                                                             ::  i1, i2, i3
  real(8)                                                             ::  SumVar
  real(8)     ,dimension(size(Var,1))                                   ::  SumVar1
  real(8)     ,dimension(size(Var,2))                                   ::  SumVar2
  real(8)     ,dimension(size(Var,3))                                   ::  SumVar3
  character(*)                                              ,parameter  ::  ProcName_='CheckSystemRHS'
  character(:)  ,allocatable                                            ::  StrLoc

  if ( PresentAndTrue(CheckIsFinite) ) then

    IsFiniteCheck: Block

      SumVar    =   sum( Var )
      if ( IsFinite(SumVar) ) exit IsFiniteCheck

      do i1 = 1,size(Var,1)
        SumVar1(i1)  =   sum( Var(i1,:,:) )
      end do

      do i2 = 1,size(Var,2)
        SumVar2(i2)  =   sum( Var(:,i2,:) )
      end do

      do i3 = 1,size(Var,3)
        SumVar3(i3)  =   sum( Var(:,:,i3) )
      end do


!       SumVar1   =   sum(Var,1)
!       SumVar2   =   sum(Var,2)
!       SumVar3   =   sum(Var,3)
!       SumVar12  =   SumVar1 + SumVar2
!       SumVar13  =   SumVar1 + SumVar3
!       SumVar23  =   SumVar2 + SumVar3

!
    End Block IsFiniteCheck
  end if

  call Logger%Entering( ProcName_ )

  if ( PresentAndNotEmpty(Message) ) call Logger%Write( Message )
  if ( PresentAndNotEmpty(Name) )    call Logger%Write( "Variable Name: " // Message )

!   if ( len_trim(StrLoc) /= 0 ) call Logger%Write( StrLoc )
!   call Logger%Write( "IsFinite(SumVar) = ", IsFinite(SumVar) )
!   call Logger%Write( "Get_IEEE_Class_Name(SumVar) = ", Get_IEEE_Class_Name(SumVar) )
!
!   do iEqtTot = 1,This%Gas%NEqtTot
!     call Logger%Write( "iEqtTot = ", iEqtTot, "EquationsResidual(iEqtTot) = ", EquationsResidual(iEqtTot), F2="i3", F4="es15.8" )
!   end do
!
!   do iY = 1,This%Field%Ny
!   do iX = 1,This%Field%Nx
!     call Logger%Write( "iX = ", iX, "iY = ", iY, "Field%RHS(:,iX,iY) = ", This%Field%RHS(:,iX,iY), F2="i4", F4="i4", F6="es15.8" )
!   end do
!   end do
!
!   do iY = 1,This%Field%Ny
!   do iX = 1,This%Field%Nx
!     call Logger%Write( "iX = ", iX, "iY = ", iY, "Field%Uc(:,iX,iY) = ", This%Field%Uc(:,iX,iY), F2="i4", F4="i4", F6="es15.8" )
!   end do
!   end do
!
!   call Error%Raise( "Nan found in Field%RHS: The total resiual is a NaN", ProcName = ProcName )

End Procedure
# endif

End SubModule




! Subroutine CheckVariable_1d( Var, Message )
!   use Error_Class               ,only:  Error
!   use Arithmetic_Library        ,only:  IsFinite, Get_IEEE_Class_Name
!   use Utilities_Library         ,only:  PresentAndNotEmpty
!   class(Checker_Type)                                   ,intent(inout)  ::  This
!   real(rkp) ,dimension(:)                               ,intent(in)     ::  Var
!   character(*)                                ,optional ,intent(in)     ::  Message
!   character(*)                                              ,parameter  ::  ProcName='CheckVariable_1d'
!   integer                                                               ::  i, j, k
!   real(rkp)                                                             ::  SumVar1
!   real(rkp)                                                             ::  SumVar
! !   do i = 1,size(Var,1)
!     SumVar1  =   sum( Var(:) )
! !   end do
!   SumVar  =   ( SumVar1 )
!   if ( IsFinite(SumVar) ) return
!   call Logger%Entering( ProcName )
!   if ( PresentAndNotEmpty(Message) ) call Logger%Write( Message )
!   call Logger%Write( "IsFinite(SumVar) = ", IsFinite(SumVar), " => ", Get_IEEE_Class_Name(SumVar) )
! !   do i = 1,size(SumVar1,1)
!     call Logger%Write( "-> SumVar1 = ", SumVar1, Fi="i4", Fr="es15.8" )
! !   end do
! !   do k = 1,size(Var,3)
! !   do j = 1,size(Var,2)
!     call Logger%Write( "-> Var(:) = ", Var(:), Fi="i4", Fr="es15.8" )
! !   end do
! !   end do
!   call Error%Raise( "Variable test failed" )
! End Subroutine
!
! Subroutine CheckVariable_2d( Var, Message )
!   use Error_Class               ,only:  Error
!   use Arithmetic_Library        ,only:  IsFinite, Get_IEEE_Class_Name
!   use Utilities_Library         ,only:  PresentAndNotEmpty
!   class(Checker_Type)                                   ,intent(inout)  ::  This
!   real(rkp) ,dimension(:,:)                             ,intent(in)     ::  Var
!   character(*)                                ,optional ,intent(in)     ::  Message
!   character(*)                                              ,parameter  ::  ProcName='CheckVariable_2d'
!   integer                                                               ::  i, j, k
!   real(rkp)     ,dimension(size(Var,1))                                 ::  SumVar1
!   real(rkp)                                                             ::  SumVar
!   do i = 1,size(Var,1)
!     SumVar1(i)  =   sum( Var(i,:) )
!   end do
!   SumVar  =   sum( SumVar1 )
!   if ( IsFinite(SumVar) ) return
!   call Logger%Entering( ProcName )
!   if ( PresentAndNotEmpty(Message) ) call Logger%Write( Message )
!   call Logger%Write( "IsFinite(SumVar) = ", IsFinite(SumVar), " => ", Get_IEEE_Class_Name(SumVar) )
!   do i = 1,size(SumVar1,1)
!     call Logger%Write( "-> i = ", i, "SumVar1(i) = ", SumVar1(i), Fi="i4", Fr="es15.8" )
!   end do
! !   do k = 1,size(Var,3)
!   do j = 1,size(Var,2)
!     call Logger%Write( "-> j = ", j, "Var(:,j) = ", Var(:,j), Fi="i4", Fr="es15.8" )
!   end do
! !   end do
!   call Error%Raise( "Variable test failed" )
! End Subroutine
!
! Subroutine CheckVariable_3d( Var, Message )
!   use Error_Class               ,only:  Error
!   use Arithmetic_Library        ,only:  IsFinite, Get_IEEE_Class_Name
!   use Utilities_Library         ,only:  PresentAndNotEmpty
!   class(Checker_Type)                                   ,intent(inout)  ::  This
!   real(rkp) ,dimension(:,:,:)                           ,intent(in)     ::  Var
!   character(*)                                ,optional ,intent(in)     ::  Message
!   character(*)                                              ,parameter  ::  ProcName='CheckVariable_3d'
!   integer                                                               ::  i, j, k
!   real(rkp)     ,dimension(size(Var,1))                                 ::  SumVar1
!   real(rkp)                                                             ::  SumVar
!   do i = 1,size(Var,1)
!     SumVar1(i)  =   sum( Var(i,:,:) )
!   end do
!   SumVar  =   sum( SumVar1 )
!   if ( IsFinite(SumVar) ) return
!   call Logger%Entering( ProcName )
!   if ( PresentAndNotEmpty(Message) ) call Logger%Write( Message )
!   call Logger%Write( "IsFinite(SumVar) = ", IsFinite(SumVar), " => ", Get_IEEE_Class_Name(SumVar) )
!   do i = 1,size(SumVar1,1)
!     call Logger%Write( "-> i = ", i, "SumVar1(i) = ", SumVar1(i), Fi="i4", Fr="es15.8" )
!   end do
!   do k = 1,size(Var,3)
!   do j = 1,size(Var,2)
!     call Logger%Write( "-> k = ", k, "j = ", j, "Var(:,j,k) = ", Var(:,j,k), Fi="i4", Fr="es15.8" )
!   end do
!   end do
!   call Error%Raise( "Variable test failed" )
! End Subroutine

!