! Module Procedure GetValueFromName_<VarType>_1d
!   character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_<VarType>_1d"
!************************************************************************************
  logical                                                               ::  Dbg
  type(InputParamProperties_Type)                                       ::  Parameter_Properties
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  call Parameter_Properties%Set(                &
        Name              =   ParameterName   , &
        DefaultValue      =   DefaultValue      )
  call This%GetValue(                           &
        Values, Parameter_Properties          , &
        SectionName       =   SectionName     , &
        CallProc          =   CallProc        , &
        Found             =   Found           , &
        Defined           =   Defined         , &
        Mandatory         =   Mandatory       , &
        Separator         =   Separator       , &
        IncreasingOrder   =   IncreasingOrder , &
        DecreasingOrder   =   DecreasingOrder , &
        Status            =   Status          , &
        Debug             =   Debug             )
  if (Dbg) call Logger%Exiting()
!************************************************************************************
! End Procedure