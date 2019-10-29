
find_package(Git)
if ( EXISTS "${PROJECT_SOURCE_DIR}/.git" AND ${GIT_FOUND} )

  # Getting the current git working branch
  LogMessage( "Getting the current git working branch" )
  set( GIT_BRANCH "UNKNOWN" CACHE STRING "Current git working branch" FORCE )
  execute_process(
    COMMAND           ${GIT_EXECUTABLE} rev-parse --abbrev-ref HEAD
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    OUTPUT_VARIABLE   GIT_BRANCH
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  LogMessage( "-> GIT_BRANCH:             ${GIT_BRANCH}" )

  # Getting SHA-1 of the latest git commit: git log -1 --format=%h
  LogMessage( "Getting SHA-1 of the latest git commit" )
  set( GIT_SHA1 "UNKNOWN" CACHE STRING "SHA-1 of the latest git commit" FORCE )
  execute_process(
    COMMAND           ${GIT_EXECUTABLE} rev-parse --short HEAD
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    OUTPUT_VARIABLE   GIT_SHA1
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  LogMessage( "-> GIT_SHA1:               ${GIT_SHA1}" )

  # Getting the latest git version tag
  LogMessage( "Getting the latest git version tag" )
  set( GIT_TAG "UNKNOWN" CACHE STRING "Latest git version tag" FORCE )
  execute_process(
    COMMAND           ${GIT_EXECUTABLE} describe
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    OUTPUT_VARIABLE   GIT_TAG
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  LogMessage( "-> GIT_TAG:                ${GIT_TAG}" )

  if ( NOT GIT_TAG )
#     message(FATAL_ERROR "-> Latest git version tag cannot found !!! '${GIT_TAG}'")
    set( GIT_VERSION_MAJOR     "UNKNOWN" )
    set( GIT_VERSION_MINOR     "UNKNOWN" )
    set( GIT_VERSION_PATCH     "UNKNOWN" )
    set( GIT_VERSION           "UNKNOWN" )
    set( GIT_TAG               "UNKNOWN" )
    set( PROJECT_FULL_VERSION  ${PROJECT_VERSION} )
  else()
    string(REGEX REPLACE "^v([0-9]+)\\..*" "\\1"                GIT_VERSION_MAJOR "${GIT_TAG}")
    string(REGEX REPLACE "^v[0-9]+\\.([0-9]+).*" "\\1"          GIT_VERSION_MINOR "${GIT_TAG}")
    string(REGEX REPLACE "^v[0-9]+\\.[0-9]+\\.([0-9]+).*" "\\1" GIT_VERSION_PATCH "${GIT_TAG}")
    set( GIT_VERSION "${GIT_VERSION_MAJOR}.${GIT_VERSION_MINOR}.${GIT_VERSION_PATCH}")
    set( PROJECT_VERSION_MAJOR  ${GIT_VERSION_MAJOR} )
    set( PROJECT_VERSION_MINOR  ${GIT_VERSION_MINOR} )
    set( PROJECT_VERSION_PATCH  ${GIT_VERSION_PATCH} )
    set( PROJECT_VERSION        ${GIT_VERSION} )
    set( PROJECT_FULL_VERSION   ${GIT_TAG} )
  endif()

else()
  LogMessage( "Using the default version" )
endif()

