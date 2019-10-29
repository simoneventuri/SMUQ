SubModule(SharedLib_Class) SharedLib_SubClass

  use Logger_Class              ,only:  Logger, LogLevel_HEAVYDEBUG
  use Status_Library            ,only:  UpdateStatus

  implicit none

  contains

Module Procedure OpenSharedLib

  use SharedLib_Tools ,only:  dlopen, rtld_lazy

  character(*)                                              ,parameter  ::  ProcName='OpenSharedLib'
  logical                                                               ::  Error

  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_HEAVYDEBUG )

  This%FileName   =   FileName // c_null_char

  call Logger%Write( "Calling dlopen: FileName = ", FileName )
  This%handle     =   dlopen( This%FileName, rtld_lazy )

!   if ( .Not. c_associated(This%handle) )then
!     This%Status =   1
!     call This%Status%Set(                            &
!             ProcName  =   ProcName              , &
!             Message   =   "Unable to load shared library from file '"//This%FileName(1:len(This%FileName)-1)//"'" )
! !     call Logger%Exiting()
! !     return
!   end if
  Error   =   UpdateStatus( This%Status   , &
                        Err   =   .Not. c_associated(This%handle)        , &
                        Desc  =   "Unable to load shared library from file '"//This%FileName(1:len(This%FileName)-1)//"'" , &
                        Proc  =   ProcName  )

  call Logger%Exiting()

End Procedure

Module Procedure FindSharedLibSymb
  use SharedLib_Tools ,only:  dlsym

  character(*)                                              ,parameter  ::  ProcName='FindSharedLibSymb'
  logical                                                               ::  Error

  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_HEAVYDEBUG )
!   call Logger%Entering( ProcName )
  This%SymbName   =   SymbolName // c_null_char

  call Logger%Write( "Calling dlsym: SymbolName = ", SymbolName )
  This%proc_addr  =   dlsym( This%handle, This%SymbName )

!   if ( .Not. c_associated(This%proc_addr) )then
!     This%Status   =   1
!     Error         =   UpdateStatus( This%Status   , &
!                           Err   =   .True.        , &
!                           Desc  =   "Unable to load the procedure '"//SymbolName//"' from file '"//This%FileName(1:len(This%FileName)-1)//"'" , &
!                           Proc  =   ProcName  )
!   end if
  Error   =   UpdateStatus( This%Status   , &
                        Err   =   .Not. c_associated(This%proc_addr)        , &
                        Desc  =   "Unable to load the procedure '"//SymbolName//"' from file '"//This%FileName(1:len(This%FileName)-1)//"'" , &
                        Proc  =   ProcName  )

  call Logger%Exiting()

End Procedure

Module Procedure GetProcedure
  character(*)                                              ,parameter  ::  ProcName='GetProcedure'
  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_HEAVYDEBUG )

  if ( present(File) ) then
!     call Logger%Write( "Calling This%Open: File = ", File )
    call This%Open( File, LogLevel=LogLevel )
!     call Logger%Write( "-> This%Status%Ok() = ", This%Status%Ok()  )
  end if
  if ( present(Name) ) then
!     call Logger%Write( "Calling This%FindSymb: Name = ", Name )
    call This%FindSymb( Name, LogLevel=LogLevel )
!     call Logger%Write( "-> This%Status%Ok() = ", This%Status%Ok()  )
  end if


!   Error   =   UpdateStatus( This%Status   , &
!                         Err   =   .Not. c_associated(This%proc_addr)        , &
!                         Desc  =   "Unable to load the procedure '"//SymbolName//"' from file '"//This%FileName(1:len(This%FileName)-1)//"'" , &
!                         Proc  =   ProcName  )
!
  if ( .Not. This%Status%Ok() ) then
    call Logger%Exiting()
    return
  end if

  call Logger%Write( "Calling c_f_procpointer" )
  call c_f_procpointer( This%proc_addr, Proc )

  call Logger%Exiting()
End Procedure

End SubModule