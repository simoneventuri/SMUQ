  logical                                                               ::  Dbg
  character(:)  ,allocatable                                            ::  ProcPath                        ! Procedure path
  type(InputParameter_Type)                                             ::  Param                           ! Parameter object
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  ProcPath      =       Set_Procedure_Path( ProcName, CallProc )
  if (Dbg) call Logger%Write( "Extracting the parameter " // Properties%GetName() )
  if (Dbg) call Logger%Write( "-> Calling This%GetParameter" )
  Param   =   This%GetParameter( Properties, SectionName, Mandatory, Debug=Debug )
  if (Dbg) call Logger%Write( "-> Param%Defined = ", Param%Defined )
  if (Dbg) call Logger%Write( "Extracting the value from parameter " // Properties%GetName() )
  if (Dbg) call Logger%Write( "-> Calling Param%GetValue" )
  call Param%GetValue(                          &
        Values                                , &
        CallProc          =   ProcPath        , &
        Separator         =   Separator       , &
        IncreasingOrder   =   IncreasingOrder , &
        DecreasingOrder   =   DecreasingOrder , &
        Status            =   Status          , &
        Debug             =   Debug             )
  if (Dbg) call Logger%Write( "-> Values = ", Values )
  if ( present(Found) )   Found   = Param%Defined
  if ( present(Defined) ) Defined = Param%Defined .or. Param%Properties%HasDefaultValue
  if ( UpdateStatus(Status,Proc=ProcName,ExitLogger=Dbg) ) return
  if (Dbg) call Logger%Exiting()