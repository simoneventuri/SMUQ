! File: inline-GetValueFromProp_0d.F90
! Module Procedure GetValueFromProp_<VarType>_0d
!   character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_<VarType>_0d" ! Name of current procedure
  logical                                                               ::  Dbg
  character(:)  ,allocatable                                            ::  ProcPath                        ! Procedure path
  type(InputParameter_Type)                                             ::  Param                           ! Parameter object
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  ProcPath      =   Set_Procedure_Path( ProcName, CallProc )
  if (Dbg) call Logger%Write( "Extracting parameter '"//Properties%GetName()//"'" )
  if (Dbg) call Logger%Write( "-> Calling This%GetParameter" )
  Param   =   This%GetParameter( Properties, SectionName, Mandatory, Debug=Debug )
  if (Dbg) call Logger%Write( "-> Param%Defined = ", Param%Defined )
  if (Dbg) call Logger%Write( "Extracting parameter value" )
  if (Dbg) call Logger%Write( "-> Calling Param%GetValue" )
  call Param%GetValue(            &
        Value                   , &
        CallProc  =   ProcPath  , &
        Status    =   Status    , &
        Debug     =   Debug       )
  if (Dbg) call Logger%Write( "-> Value = ", Value )
  if ( present(Found) )   Found   = Param%Defined
  if ( present(Defined) ) Defined = Param%Defined .or. Param%Properties%HasDefaultValue
  if ( UpdateStatus(Status,Desc="Error extracting parameter value.",Proc=ProcName,ExitLogger=Dbg) ) return
  if (Dbg) call Logger%Exiting()
! End Procedure