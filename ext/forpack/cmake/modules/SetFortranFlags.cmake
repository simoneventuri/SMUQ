
####################################
# Fortran compiler version numbers #
####################################
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
string( REPLACE "." ";" VERSION_LIST ${CMAKE_Fortran_COMPILER_VERSION} )
list(GET VERSION_LIST 0 Fortran_COMPILER_VERSION_MAJOR)
list(GET VERSION_LIST 1 Fortran_COMPILER_VERSION_MINOR)
list(GET VERSION_LIST 2 Fortran_COMPILER_VERSION_PATCH)
# set( CMAKE_Fortran_COMPILER_VERSION "${Fortran_COMPILER_VERSION_MAJOR}.${Fortran_COMPILER_VERSION_MINOR}.${Fortran_COMPILER_VERSION_PATCH}" )
set( Fortran_COMPILER_VERSION "${Fortran_COMPILER_VERSION_MAJOR}.${Fortran_COMPILER_VERSION_MINOR}.${Fortran_COMPILER_VERSION_PATCH}" )
set( Fortran_COMPILER_CONFIG ${CMAKE_Fortran_COMPILER_ID}-${CMAKE_Fortran_COMPILER_VERSION} )
string( TOLOWER ${Fortran_COMPILER_CONFIG} Fortran_COMPILER_CONFIG )


###############################
# Print Fortran compiler info #
###############################
LogMessage( "Fortran compiler info" )
LogMessage( "-> CMAKE_Fortran_COMPILER              ${CMAKE_Fortran_COMPILER}" )
LogMessage( "-> CMAKE_Fortran_COMPILER_ID           ${CMAKE_Fortran_COMPILER_ID}" )
LogMessage( "-> CMAKE_Fortran_COMPILER_VERSION      ${CMAKE_Fortran_COMPILER_VERSION}" )
LogMessage( "-> Fortran_COMPILER_NAME               ${Fortran_COMPILER_NAME}" )
LogMessage( "-> Fortran_COMPILER_VERSION            ${Fortran_COMPILER_VERSION}" )
LogMessage( "-> Fortran_COMPILER_VERSION_MAJOR      ${Fortran_COMPILER_VERSION_MAJOR}" )
LogMessage( "-> Fortran_COMPILER_VERSION_MINOR      ${Fortran_COMPILER_VERSION_MINOR}" )
LogMessage( "-> Fortran_COMPILER_VERSION_PATCH      ${Fortran_COMPILER_VERSION_PATCH}" )
LogMessage( "-> Fortran_COMPILER_CONFIG             ${Fortran_COMPILER_CONFIG}" )


######################################################
# Determine and set the Fortran compiler flags we want
######################################################
function( AddFortranFlags FortranFlags )
  set( Fortran_FLAGS "${Fortran_FLAGS} ${FortranFlags}" CACHE STRING "Set Fortran flag: ${FortranFlags}" FORCE)
endfunction()
function( AppendFortranFlags FortranFlags )
  set( Fortran_FLAGS "${Fortran_FLAGS}${FortranFlags}" CACHE STRING "Set Fortran flag: ${FortranFlags}" FORCE)
endfunction()
set( Fortran_FLAGS "" CACHE STRING "Set the ${FortranFlags} flags" FORCE)

LogMessage( "Setting Fortram compiler flags" )
LogMessage( "-> Operating System:   ${CMAKE_SYSTEM_NAME}" )
LogMessage( "-> Build type:         ${CMAKE_BUILD_TYPE}" )
LogMessage( "-> Fortran Compiler:   ${CMAKE_Fortran_COMPILER_ID}" )


set (CMAKE_POSITION_INDEPENDENT_CODE TRUE )



if ( ${CMAKE_SYSTEM_NAME} MATCHES "Windows" )
    message ( FATAL_ERROR "Windows builds are currently not supported" )
endif ()

if ( CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" )

  add_definitions( -DGCC_COMPILER )
  add_definitions( -DWORKAROUND_GFORTRAN_SOURCE_ALLOCATION )        # https://gcc.gnu.org/bugzilla/show_bug.cgi?id=44672
  add_definitions( -DWORKAROUND_GCC_ALLOCATABLE_OUTPUT_CHARACTER_ARRAY_IN_FUNCTION )
  add_definitions( -DWORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER )
  add_definitions( -DWORKAROUND_GFORTRAN_DIFFERENT_CHARACTER_LENGTHS_ARRAY_CONSTRUCTOR )
  add_definitions( -DWORKAROUND_GFORTRAN_RECURSIVE_ASSOCIATION )    # https://gcc.gnu.org/bugzilla/show_bug.cgi?id=64678
  add_definitions( -DWORKAROUND_GFORTRAN_INQUIRE_DIRECTORY )
  add_definitions( -DWORKAROUND_GFORTRAN_SELECT_TYPE )
  add_definitions( -DWORKAROUND_GFORTRAN_INTERNAL_WRITE )

#   if ( Fortran_COMPILER_VERSION VERSION_GREATER "18.0.3" )
#     add_definitions( -DSUPPORTED_RECURSIVE_ALLOCATABLE_DERIVEDTYPE )
#     add_definitions( -DRECURSIVE_ALLOCATABLE_DERIVEDTYPE_ATTRIBUTE=,allocatable )
#     add_definitions( -DRECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED=allocated )
#     add_definitions( -DRECURSIVE_ALLOCATABLE_DERIVEDTYPE_DEALLOCATE=deallocate )
#   else()
    add_definitions( -DRECURSIVE_ALLOCATABLE_DERIVEDTYPE_ATTRIBUTE=,pointer )
    add_definitions( -DRECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED=associated )
    add_definitions( -DRECURSIVE_ALLOCATABLE_DERIVEDTYPE_DEALLOCATE=nullify )
#   endif()

#   AddFortranFlags( "-traditional" )
  AddFortranFlags( "-cpp" )
  AddFortranFlags( "-x f95-cpp-input" )
  AddFortranFlags( "-fno-unsafe-math-optimizations" )
  AddFortranFlags( "-fdefault-double-8" )
  AddFortranFlags( "-fdefault-real-8" )
  AddFortranFlags( "-ffree-line-length-none" )
  AddFortranFlags( "-frealloc-lhs" )                # An allocatable left-hand side of an intrinsic assignment is automatically (re)allocated if it is either unallocated or has a different shape. The option is enabled by default except when -std=f95 is given.
  AddFortranFlags( "-fPIC" )

#   AddFortranFlags( "-Wrealloc-lhs")                 # Warn when the compiler might insert code to for allocation or reallocation of an allocatable array variable of intrinsic type in intrinsic assignments.
#   AddFortranFlags( "-Wrealloc-lhs-all")             # Warn when the compiler inserts code to for allocation or reallocation of an allocatable variable; this includes scalars and derived types.
#   AddFortranFlags( "-Wall" )                      # Enables commonly used warning options pertaining to usage that we recommend avoiding and that we believe are easy to avoid. (See below for the full list of options)
#       AddFortranFlags( "-Waliasing" )
#       AddFortranFlags( "-Wampersand" )
#       AddFortranFlags( "-Wconversion" )
#       AddFortranFlags( "-Wsurprising" )
#       AddFortranFlags( "-Wc-binding-type" )
#       AddFortranFlags( "-Wintrinsics-std" )
#       AddFortranFlags( "-Wtabs" )
#       AddFortranFlags( "-Wintrinsic-shadow" )
#       AddFortranFlags( "-Wline-truncation" )
#       AddFortranFlags( "-Wtarget-lifetime" )
#       AddFortranFlags( "-Winteger-division" )
#       AddFortranFlags( "-Wreal-q-constant" )
#       AddFortranFlags( "-Wunused" )
#       AddFortranFlags( "-Wundefined-do-loop" )

  if ( CMAKE_BUILD_TYPE STREQUAL "Debug" )
    AddFortranFlags( "-O0" )
    AddFortranFlags( "-g" )
    AddFortranFlags( "-fbounds-check" )
    AddFortranFlags( "-fbacktrace" )
    AddFortranFlags( "-fdump-core" )
    AddFortranFlags( "-ggdb" )
    AddFortranFlags( "-pg" )
#     AddFortranFlags( "-Wno-line-truncation" )
#   Removing some warnings
#     AddFortranFlags( "-Wno-surprising" )
#     AddFortranFlags( "-Wno-maybe-uninitialized" )
#     AddFortranFlags( "-Wno-uninitialized" )
  endif ()
  if ( CMAKE_BUILD_TYPE STREQUAL "Release" )
    AddFortranFlags( "-O2" )
  endif ()


elseif ( CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" )

  add_definitions( -DINTEL_COMPILER )

#   if ( Fortran_COMPILER_VERSION VERSION_GREATER "19.0.0" )
    LogMessage( "Enabling full support for recursive allocatable derived-type" )
    add_definitions( -DSUPPORTED_RECURSIVE_ALLOCATABLE_DERIVEDTYPE )
    add_definitions( -DRECURSIVE_ALLOCATABLE_DERIVEDTYPE_ATTRIBUTE=,allocatable )
    add_definitions( -DRECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED=allocated )
    add_definitions( -DRECURSIVE_ALLOCATABLE_DERIVEDTYPE_DEALLOCATE=deallocate )
#   else()
#     LogMessage( "Setting workaround for recursive allocatable derived-type" )
#     add_definitions( -DRECURSIVE_ALLOCATABLE_DERIVEDTYPE_ATTRIBUTE=,pointer )
#     add_definitions( -DRECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED=associated )
#     add_definitions( -DRECURSIVE_ALLOCATABLE_DERIVEDTYPE_DEALLOCATE=nullify )
#   endif()



  AddFortranFlags( "-Wl,--export-dynamic" ) # ONly for executable
#   set ( DIAGNOSTIC_FLAGS "-diag-disable 5268,8208,7712,10182,8031,5462" )
  AddFortranFlags( "-real-size 64" )  # Alternate Options: -r8, -autodouble
#   AddFortranFlags( "-save-temps")                   # Tells the compiler to save intermediate files created during compilation.
  AddFortranFlags( "-fpp" )
  AddFortranFlags( "-Qoption,fpp,-f_com=no")          # Determines whether Fortran-style end-of-line comments are recognized or ignored by fpp.
  AddFortranFlags( "-fPIC" )
#   AddFortranFlags( "-prof-gen=srcpos" )   # Option to generate a ".spi" file to be used by the intel code coverage tool
  AddFortranFlags( "-diag-disable " )
  AppendFortranFlags( "5268"    )   # warning #5268: Extension to standard: The text exceeds right hand column allowed on the line.
  AppendFortranFlags( ",8208"   )   # warning #8208: If type specification is omitted, each ac-value expression in the array constructor of type CHARACTER must have the same length type parameters.
  AppendFortranFlags( ",7712"   )   # remark #7712: This variable has not been used.
  AppendFortranFlags( ",10182"  )   # warning #10182: disabling optimization; runtime debug checks enabled
  AppendFortranFlags( ",8031"   )
  AppendFortranFlags( ",5462"   )   # warning #5462: Global name too long, shortened from: <...> to: <...>
  AppendFortranFlags( ",7925"   )   # warning #7925: An interface-block in a subprogram that contains an interface-body for a procedure defined by that subprogram is non-standard.
#       AddFortranFlags( ",8037" )
#       AddFortranFlags( ",7953" )
#       AddFortranFlags( ",7416" )
#       AddFortranFlags( ",6477" )
#       AddFortranFlags( ",10397" )
#       AddFortranFlags( ",10382" )
#       AddFortranFlags( ",10346" )

  AddFortranFlags( "-assume realloc_lhs" )    # Default: should not be required
  AddFortranFlags( "-fp-model precise" )
  AddFortranFlags( "-stand f08" )             # Tells the compiler to issue compile-time messages for nonstandard language elements.

  if ( CMAKE_BUILD_TYPE STREQUAL "Debug" )
    AddFortranFlags( "-check all" )
#     bounds    Determines whether checking occurs for array subscript and character substring expressions.
#     shape     Determines whether array conformance checking is performed.
    AddFortranFlags( "-check noarg_temp_created" )
    AddFortranFlags( "-check nopointers" )
    AddFortranFlags( "-check noshap" )
#     AddFortranFlags( "-check uninit" )
    AddFortranFlags( "-p" )                   # Compiles and links for function profiling with gprof.
    AddFortranFlags( "-g" )                   # Tells the compiler to generate a level of debugging information in the object file.
    AddFortranFlags( "-warn all" )
    AddFortranFlags( "-traceback" )           # Tells the compiler to generate extra information in the object file to provide source file traceback information when a severe error occurs at run time.
    AddFortranFlags( "-O0" )
    AddFortranFlags( "-ftrapuv" )             # Initializes stack local variables to an unusual value to aid error detection.
#     AddFortranFlags( "-fpe0" )                # Allows some control over floating-point exception handling for the main program at run-time.
  elseif ( CMAKE_BUILD_TYPE STREQUAL "Release" )
    AddFortranFlags( "-O3" )
    AddFortranFlags( "-no-prec-div" )
    AddFortranFlags( "-xHost" )
    AddFortranFlags( "-ip" )
#     AddFortranFlags( "-ipo" )                 # Removing for now the interprocedural optimization since it leads to a too big increase of the compilation time
  endif ()



elseif ( CMAKE_Fortran_COMPILER_ID STREQUAL "PGI" )

  add_definitions( -DPGI_COMPILER )

#   AddFortranFlags( "-fpp" )
#   AddFortranFlags( "-fPIC" )
# r8      #–r8: Interpret REAL variables as DOUBLE PRECISION.

  AddFortranFlags( "-Mextend -Ktrap=fp -Mpreprocess -Mnoupcase -Mlarge_arrays" )
  if ( CMAKE_BUILD_TYPE STREQUAL "Debug" )
    AddFortranFlags( "-O0" )
    AddFortranFlags( "-Mbounds" )
#     –C      # Generates code to check array bounds.
#     -c      # Instrument the generated executable to perform array bounds checking at runtime.
#     -E      # Stops after the preprocessing phase and displays the preprocessed file on the standard output.
#     -g      # Includes debugging information in the object module.
#     AddFortranFlags( "-check all" )
#     AddFortranFlags( "-check noarg_temp_created" )
#     AddFortranFlags( "-check nopointers" )
#     AddFortranFlags( "-check noshap" )
    AddFortranFlags( "-pg" )    # Instrument the generated executable to produce a gprof-style gmon.out sample- based profiling trace file; –qp is equivalent to –pg.
    AddFortranFlags( "-g" )
#     - -pedantic   #Prints warnings from included <system header files>
#     AddFortranFlags( "-warn all" )
    AddFortranFlags( "-traceback" )
#     AddFortranFlags( "-stand f08" )
  elseif ( CMAKE_BUILD_TYPE STREQUAL "Release" )
    AddFortranFlags( "-O4" )
#     -fastsse      # Generally optimal set of flags for targets that include SSE/SSE2 capability.
  endif ()

else ()
   message ( FATAL_ERROR "Unknown compiler: ${CMAKE_Fortran_COMPILER_ID}" )
endif()

set ( CMAKE_Fortran_FLAGS "${Fortran_FLAGS}" )
set ( CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "" )

LogMessage( "Fortran Flags: CMAKE_Fortran_FLAGS: ${CMAKE_Fortran_FLAGS}" )




######################
# Adding definitions #
######################

LogMessage( "Adding definitions" )

if ( ${PFUNIT_FOUND} )
  add_definitions( -DUSE_PFUNIT )
endif ()

string( TIMESTAMP BUILD_DATE "%Y-%m-%d" )
string( TOUPPER   ${PROJECT_NAME} VAR_PREFIX )
# add_definitions( -D${VAR_PREFIX}_BUILD_DATE=${BUILD_DATE} )
add_definitions( -D${VAR_PREFIX}_BUILD_CONFIG=${PROJECT_BUILD_CONFIGURATION} )
add_definitions( -D${VAR_PREFIX}_VERSION=${PROJECT_VERSION} )
add_definitions( -D${VAR_PREFIX}_VERSION_MAJOR=${PROJECT_VERSION_MAJOR} )
add_definitions( -D${VAR_PREFIX}_VERSION_MINOR=${PROJECT_VERSION_MINOR} )
add_definitions( -D${VAR_PREFIX}_VERSION_PATCH=${PROJECT_VERSION_PATCH} )


# LogMessage( "Adding extra definitions" )
# set( EXTRA_DEFINITION_LIST "" )
# # *******************
# set( NewDefinition "_SrcLoc_='__FILE__ //\":\"//STRINGIFY(__LINE__)'" )
# add_definitions( "-D${NewDefinition}" )
# LogMessage( "NewDefinition        ${NewDefinition}" )
# list( APPEND EXTRA_DEFINITION_LIST "${NewDefinition}" )
# # *******************
# set( NewDefinition "'STRINGIFY(s)=str(s)'" )
# add_definitions( "-D${NewDefinition}" )
# LogMessage( "NewDefinition        ${NewDefinition}" )
# list( APPEND EXTRA_DEFINITION_LIST "${NewDefinition}" )
# # *******************
# set( NewDefinition "'str(s)=\#s'" )
# add_definitions( "-D${NewDefinition}" )
# LogMessage( "NewDefinition        ${NewDefinition}" )
# list( APPEND EXTRA_DEFINITION_LIST "${NewDefinition}" )
# # *******************
# LogMessage( "EXTRA_DEFINITION_LIST        ${EXTRA_DEFINITION_LIST}" )
# foreach( d ${EXTRA_DEFINITION_LIST} )
#   LogMessage( "-> ${d}" )
# endforeach()


LogMessage( "Printing definitions" )
set( LIST_DEFINITIONS "" )
get_directory_property( DEFINITION_LIST DIRECTORY ${CMAKE_SOURCE_DIR} COMPILE_DEFINITIONS )
# # # # # LogMessage( "Before DEFINITION_LIST         ${DEFINITION_LIST}" )
# # # # # list( APPEND DEFINITION_LIST "${SrcLocDefinition}" )
# # # # # LogMessage( "After DEFINITION_LIST         ${DEFINITION_LIST}" )
foreach( d ${DEFINITION_LIST} )
  LogMessage( "-> ${d}" )
  set( LIST_DEFINITIONS "${LIST_DEFINITIONS}-D${d} " )
endforeach()
LogMessage( "-> LIST_DEFINITIONS                    ${LIST_DEFINITIONS}" )





# message( FATAL_ERROR "stop*****")




if ( PARALLEL_ENVIRONMENT STREQUAL "Coarray" )
  add_definitions( -DCOARRAY )
endif ()



# For gcc the option '-Wall' contains:
# -Waliasing
# -Wampersand
# -Wconversion
# -Wsurprising
# -Wc-binding-type
# -Wintrinsics-std
# -Wtabs
# -Wintrinsic-shadow
# -Wline-truncation
# -Wtarget-lifetime
# -Winteger-division
# -Wreal-q-constant
# -Wunused
# -Wundefined-do-loop.
