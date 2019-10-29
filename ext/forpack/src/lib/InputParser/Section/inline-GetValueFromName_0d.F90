! File: inline-GetValueFromName_0d
! Module Procedure GetValueFromName_<VarType>_0d
!   character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_<VarType>_0d"
!************************************************************************************
  logical                                                               ::  Dbg
  type(InputParamProperties_Type)                                       ::  Parameter_Properties            ! Parameter-Properties object
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  call Parameter_Properties%Set(                        &
        Name            =   ParameterName, &
        DefaultValue    =   DefaultValue   )
  call This%GetValue( &
        Value,        &
        Parameter_Properties   , &
        SectionName , &
        CallProc    , &
        Found       , &
        Defined     , &
        Mandatory   , &
        Status      , &
        Debug         )
  if ( UpdateStatus(Status,Desc="Error extracting value of parameter '"//ParameterName//"'",Proc=ProcName,ExitLogger=Dbg) ) return
  if (Dbg) call Logger%Exiting()
!************************************************************************************
! End Procedure