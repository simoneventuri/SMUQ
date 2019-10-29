! @TODO: Add an option to change the default loglevel
Module Logger_Parameters

  implicit none

  public

  integer       ,parameter  ::  LogLevel_NOLOGS       =   0
  integer       ,parameter  ::  LogLevel_ERROR        =   1
  integer       ,parameter  ::  LogLevel_WARNING      =   2
  integer       ,parameter  ::  LogLevel_INFO         =   3
  integer       ,parameter  ::  LogLevel_DEBUG        =   4
  integer       ,parameter  ::  LogLevel_HEAVYDEBUG   =   5
  integer       ,parameter  ::  LogLevel_DEFAULT      =   LogLevel_INFO
  integer       ,parameter  ::  IndentationStep       =   2

  character(*)  ,parameter  ::  Default_OpenStatus    =   "REPLACE"
  character(*)  ,parameter  ::  Default_OpenPosition  =   "REWIND"
  character(*)  ,parameter  ::  Valid_OpenStatus(2)   =   ["REPLACE","OLD    "]
  character(*)  ,parameter  ::  Valid_OpenPosition(2) =   ["REWIND","APPEND"]

  character(*)  ,parameter  ::  LogLevelNames(0:5)    =   [ Character(10) ::  &
                                    "NOLOGS"        &
                                  , "ERROR"         &
                                  , "WARNING"       &
                                  , "INFO"          &
                                  , "DEBUG"         &
                                  , "HEAVYDEBUG"    ]
End Module