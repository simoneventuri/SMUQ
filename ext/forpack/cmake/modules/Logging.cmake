#==========================================================================
# Utilities for writing cmake log messages.
#==========================================================================
# File Name:      Logging.cmake
# Functions:
#   LogMessage    Write a log message
#   setLogsOn     Activate logs
#   setLogsOff    Deactivate logs
#==========================================================================

# Global variables
set( ErrorLogLevel        "1" CACHE INTERNAL "Error log level")
set( WarningLogLevel      "2" CACHE INTERNAL "Warning log level")
set( InfoLogLevel         "3" CACHE INTERNAL "Info log level")
set( DebugLogLevel        "4" CACHE INTERNAL "Debug log level")
set( HeavyDebugLogLevel   "5" CACHE INTERNAL "HEavyDebug log level")

set( CurrentLogLevel      "${InfoLogLevel}"   CACHE INTERNAL "Current log level" )

function( LogMessage message )
  if (LOG_MESSAGE_STATUS)
    set ( LOG_MESSAGE_PREFIX "[${PROJECT_NAME}]" )
    message ( STATUS "${LOG_MESSAGE_PREFIX} ${message}" )
  endif()
endfunction( LogMessage )

function( setLogsOn )
  set( LOG_MESSAGE_STATUS "ON" CACHE INTERNAL "Status for log messages" )
endfunction( setLogsOn )

function( setLogsOff )
  set( LOG_MESSAGE_STATUS "OFF" CACHE INTERNAL "Status for log messages" )
endfunction( setLogsOff )

function( setLogLevel LogLevel )
  if ( ${LogLevel} MATCHES "^[0-9]+$" )
    if( (${LogLevel} EQUAL ${ErrorLogLevel}) OR (${LogLevel} GREATER ${ErrorLogLevel}) )
      set( CurrentLogLevel "${LogLevel}" CACHE INTERNAL "Current log level" )
    endif()
  else()
    if      ( ${LogLevel} STREQUAL "ERROR")
      set( LocalLogLevel "${ErrorLogLevel}" )
    elseif  ( ${LogLevel} STREQUAL "WARNING")
      set( LocalLogLevel "${WarningLogLevel}" )
    elseif  ( ${LogLevel} STREQUAL "INFO")
      set( LocalLogLevel "${InfoLogLevel}" )
    elseif  ( ${LogLevel} STREQUAL "DEBUG")
      set( LocalLogLevel "${DebugLogLevel}" )
    elseif  ( ${LogLevel} STREQUAL "HEAVYDEBUG")
      set( LocalLogLevel "${HeavyDebugLogLevel}" )
    endif()
    set( CurrentLogLevel "${LocalLogLevel}" CACHE INTERNAL "Current log level" )
  endif()
endfunction( setLogLevel )

# setLogLevel( "${InfoLogLevel}" )

function( LogError message )
  set( MessageLogLevel "${ErrorLogLevel}" )
  if( (${CurrentLogLevel} EQUAL ${MessageLogLevel}) OR (${CurrentLogLevel} GREATER ${MessageLogLevel}) )
    set ( LOG_MESSAGE_PREFIX "[${PROJECT_NAME}]" )
    message ( STATUS "${LOG_MESSAGE_PREFIX} ${message}" )
  endif()
endfunction( LogError )

function( LogWarning message )
  set( MessageLogLevel "${WarningLogLevel}" )
  if( (${CurrentLogLevel} EQUAL ${MessageLogLevel}) OR (${CurrentLogLevel} GREATER ${MessageLogLevel}) )
    set ( LOG_MESSAGE_PREFIX "[${PROJECT_NAME}]" )
    message ( STATUS "${LOG_MESSAGE_PREFIX} ${message}" )
  endif()
endfunction( LogWarning )

function( LogInfo message )
  set( MessageLogLevel "${InfoLogLevel}" )
  if( (${CurrentLogLevel} EQUAL ${MessageLogLevel}) OR (${CurrentLogLevel} GREATER ${MessageLogLevel}) )
    set ( LOG_MESSAGE_PREFIX "[${PROJECT_NAME}]" )
    message ( STATUS "${LOG_MESSAGE_PREFIX} ${message}" )
  endif()
endfunction( LogInfo )

function( LogDebug message )
  set( MessageLogLevel "${DebugLogLevel}" )
  if( (${CurrentLogLevel} EQUAL ${MessageLogLevel}) OR (${CurrentLogLevel} GREATER ${MessageLogLevel}) )
    set ( LOG_MESSAGE_PREFIX "[${PROJECT_NAME}]" )
    message ( STATUS "${LOG_MESSAGE_PREFIX} ${message}" )
  endif()
endfunction( LogDebug )

function( LogHeavyDebug message )
  set( MessageLogLevel "${HeavyDebugLogLevel}" )
  if( (${CurrentLogLevel} EQUAL ${MessageLogLevel}) OR (${CurrentLogLevel} GREATER ${MessageLogLevel}) )
    set ( LOG_MESSAGE_PREFIX "[${PROJECT_NAME}]" )
    message ( STATUS "${LOG_MESSAGE_PREFIX} ${message}" )
  endif()
endfunction( LogHeavyDebug )
