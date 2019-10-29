#!/bin/bash
set -e

# TODO
# * Add the ability to specify include directories
# * Add the ability to specify library directories
# * Add by default the same Fortran flags that the ones used to compiler the code

Name="@PROJECT_NAME_LC@"
ScriptName="${BASH_SOURCE[0]}"
ScriptName=${ScriptName##*/}
Verbosity=0
RemoveTmpFile=1

#################################
# Stylistic functions
#################################
Bold()          { echo -e "\e[1m$1\e[0m"; }
Italic()        { echo -e "\e[3m$1\e[0m"; }
Underline()     { echo -e "\e[4m$1\e[0m"; }
Strikethrough() { echo -e "\e[9m$1\e[0m"; }
Red()           { echo -e "\e[31m$1\e[0m"; }
Green()         { echo -e "\e[32m$1\e[0m"; }


#################################
# Log Message
#################################
PrintLog () {
  [[ $Verbosity -ne 1 ]] && return 0
  local Width=40
  local LogPrefix="[${ScriptName}]: "
  local Format="%s%-${Width}s %s\n"
  case $# in
    0) local LogMsg=$(printf "") ;;
    1) local LogMsg=$(printf "${LogPrefix}$1") ;;
    2) local LogMsg=$(printf "${Format}" "${LogPrefix}" "$1" "$2")
  ;;
  esac
echo "${LogMsg}"
}


#################################
# Error Message
#################################
PrintError() {
    ErrMsg=$(Underline $(Bold $(Red Error)))
    printf "\n  $ErrMsg in $(Italic $ScriptName) script"
    echo -e "\n    $1\n"  >&2
    exit 1
}


#################################
# Help Message
#################################
HelpMessage() {
    cat <<EOM
    $(Bold NAME)
        $ScriptName — Script for building $(Italic $Name) applications.

    $(Bold SYNOPSIS)
        $ScriptName <list-source-files> [-h] [-v] [-m modulefiles-list]
                                        [-f fortran-compiler-flags]
                                        [-x executable-name] [-l libraries-list] [-d defintions-list]

    $(Bold DESCRIPTION)
        This script can be used to automatically build applications based on the ${Name} code.
        Using a set of ${Name} environment variables defined in a ${Name} modulefile, this script
        will generate the correct instructions to compile a Fortran application based on ${Name}.
        This script takes as mandatory arguments the list of Fortran source files to be compiled.
        This list needs to be specified as the first argument of this script and the files need
        to be separated by a space. The optional arguments are then specified next, if any. These
        optional arguments all start by one or two '-' character depending on whether the short or
        long version of the argument name is considered.

        In particular, this script will:
         - Compile an application using the same Fortran compiler what was used to compile ${Name}.
         - Pass to the compiler the directories containing the include files of ${Name} and its dependencies (Compile step).
         - Pass to the compiler the directories containing the libraries of ${Name} and its dependencies (Link step).
         - Pass to the compiler the the list of libraries of ${Name} and its dependencies (Link step).
        For example, the include/library directories will always contain the directories associated
        to the $(Italic ${Name}) library since this library is always a dependency of ${Name}. If ${Name} has been
        compiled with hdf5 support, then there directories will also include the directories related
        to hdf5.

        Several arguments can be passed to this script for further customization. All options have a
        long and short version. When specifying optional arguments, the argument's name and value can
        be separated either by a space ($(Italic "--name value")) or an equal symbol ($(Italic "--name=value")).
        The order in which optional arguments are specified is irrelevant.

    $(Bold "MANDATORY ARGUMENTS")
          $(Bold "<list-source-files>")
                The list of Fortran source files to be compiled is the only mandatory argument to be passed
                to this script. This list needs to be specified as the first argument of this script and the
                files need to be separated by a space. The list of Fortran source files should be order in the
                same order than the files should be compiled. Thus, the last file in the list should corresponds
                to the Fortran file containing the main program.

    $(Bold "OPTIONAL ARGUMENTS")
          $(Bold "-h, --help")
                This option prints a help message describing all of the available command-line options.

          $(Bold "-v, --verbose")
                This option increases the amount of information you are given during the build process.

          $(Bold "-m, --modulefile=")$(Underline "modulefiles-list")
                This option enables to specify a list of modulefile to be loaded. Each modulefile needs to to be
                separated by a comma. This option can be used to set the  Fortran compiler to be used
                for compiler the application to the same Fortran compiler that was used to compile ${Name}.
                If a ${Name} modulefile is already loaded when this script is called, then there is no need
                the add it to this modulefile list. This option is useful to additional load modulefile
                upon which the ${Name} application depends
                Example: $(Italic --modulefile="modulefile1,modulefile2") or $(Italic -m="modulefile1,modulefile2")

          $(Bold "-f, --compiler-flags=")$(Underline "fortran-compiler-flags")
                This option enables to specify a set of Fortran compiler flags to be used when compiling the
                user application. If no Fortran compiler flags are specified, then the code is compiled only
                with the compiler flags and definitions which were used to compile ${Name}.
                any flags, ie. there is no default Fortran flags.
                Example: $(Italic --compiler-flags="-O3") or $(Italic -f="-O3")

          $(Bold "-l, --linker-flags=")$(Underline "linker-flags")
                This option enables to specify a set of additional linker flags. This option is useful when the
                user application needs to be linked to additional libraries. In such situation, this option can
                be used to specified the path where to find the external libraies ('-L<lib-path>') and the name
                of the library ('-l<lib-name>'). This of list additional linker flags are then added to  a  set
                of linker flags automatically generated by the ${Name} code. These ${Name} linker flags  correspond
                to  (1) the list of libraries required by ${Name} ('-l<lib-name>'), and (2) to the list of  their
                paths ('-L<lib-path>'). These ${Name} libraries (including dependencies) and the directories where
                to find these librarties are stored in the environment variables $(Italic @PROJECT_NAME_UC@_LIBRARIES_FFLAGS) and
                $(Italic @PROJECT_NAME_UC@_LIBRARY_DIR_FFLAGS) respectively. These variables are defined in the ${Name} modulefiles.
                Example: $(Italic --linker-flags=\""-lhdf5 -L/opt/hdf5/lib\"")

          $(Bold "-x, --executable-name=")$(Underline "executable-name")
                This option specifies the name of the exectuable to be generated. By default, if this option
                is not specified, then the executable name is constructed from the name of the first Fortran
                source file by replacing the Fortran extension by "$(Italic .x)".
                Example: $(Italic --executable-name="MyApp") or $(Italic -x="MyApp")

          $(Bold "-k, --keep-files")
                This option indicates to the script not to remove the temporary files created during the compilation step.
                These temporary files corresponds to the object files ($(Italic *.o), module files ($(Italic *.mod) and submodule files ($(Italic *.smod).

    $(Bold EXAMPLES)
          $ScriptName Main.F90
          $ScriptName Main.F90 PostProcess_Module.F90 Plotting_Module.F90 -x=AppName
EOM
exit 0
}


#################################
# Process Arguments
#################################
ProcessArguments() {

  ModuleFiles=""
  SourceFiles=""
  ExecutableName=""

  while :; do
    case $1 in
#   ****
#   Help
#   ****
      -h|-\?|--help)
          HelpMessage
          ;;
#   *********
#   Verbosity
#   *********
      -v|-\?|--verbose)
          Verbosity=1
          ;;
#   ***************
#   Modulefile list
#   ***************
      -m|--modulefile)
          FirstCharacter=${2:0:1}
          if [ "$FirstCharacter" == "-" ]; then PrintError "Option '$1' requires a non-empty value."; fi
          if [ "$2" ]; then
            ModuleFiles=$2
            shift
          else
            PrintError "Option '$1' requires a non-empty value."
          fi
          ;;
      --modulefile=?*|-m=?*)
          ModuleFiles=${1#*=}
          ;;
      --modulefile=|-m=)
          PrintError "Option '$1' requires a non-empty value."
          ;;
#   **********************
#   Fortran compiler flags
#   **********************
      -f|--compiler-flags)
          FirstCharacter=${2:0:1}
          if [ "$FirstCharacter" == "-" ]; then PrintError "Option '$1' requires a non-empty value."; fi
          if [ "$2" ]; then
            CompilerFlags=$2
            shift
          else
            PrintError "Option '$1' requires a non-empty value."
          fi
          ;;
      --compiler-flags=?*|-f=?*)
          CompilerFlags=${1#*=}
          ;;
      --compiler-flags=|-f=)
          PrintError "Option '$1' requires a non-empty value."
          ;;
#   ****************
#   Linker flags
#   ****************
      -l|--linker-flags)
          FirstCharacter=${2:0:1}
          if [ "$FirstCharacter" == "-" ]; then PrintError "Option '$1' requires a non-empty value."; fi
          if [ "$2" ]; then
            LinkerFlags=$2
            shift
          else
            PrintError "Option '$1' requires a non-empty value."
          fi
          ;;
      --linker-flags=?*|-l=?*)
          LinkerFlags=${1#*=}
          ;;
      --linker-flags=|-l=)
          PrintError "Option '$1' requires a non-empty value."
          ;;
#   ****************
#   Executable name
#   ****************
      -x|--executable-name)
          FirstCharacter=${2:0:1}
          if [ "$FirstCharacter" == "-" ]; then PrintError "Option '$1' requires a non-empty value."; fi
          if [ "$2" ]; then
            ExecutableName=$2
            shift
          else
            PrintError "Option '$1' requires a non-empty value."
          fi
          ;;
      --executable-name=?*|-x=?*)
          ExecutableName=${1#*=}
          ;;
      --executable-name=|-x=)
          PrintError "Option '$1' requires a non-empty value."
          ;;
#   ****************
#   Keep tmp files
#   ****************
      -k|-\?|--keep-files)
          RemoveTmpFile=0
          ;;
#     End of all options.
      --)
#           PrintLog "treating option $1"
          shift
          break
          ;;
#       -?*) PrintError "Unknown option $1."
#           ;;
      *)
#           PrintLog "case *): $1"
          [[ ! -n "$1" ]] && break
          SourceFiles="${SourceFiles}$1 "
#           break # Default case: No more options, so break out of the loop.
    esac

#     PrintLog "After case:  $1"
    shift
  done

  if [ ! -n "${SourceFiles}" ]; then
    PrintError "List of Fortran source files not specified.
    The list of Fortran source files must be specified as the first argument.
    See '$(Italic $ScriptName) --$(Italic help)' for more details."
  fi

  [[ -n "${SourceFiles}" ]]       && PrintLog "SourceFiles"       "${SourceFiles}"
  [[ -n "${ModuleFiles}" ]]       && PrintLog "ModuleFiles"       "${ModuleFiles}"
  [[ -n "${CompilerFlags}" ]]     && PrintLog "CompilerFlags"     "${CompilerFlags}"
  [[ -n "${LinkerFlags}" ]]       && PrintLog "LinkerFlags"       "${LinkerFlags}"
  [[ -n "${ExecutableName}" ]]    && PrintLog "ExecutableName"    "${ExecutableName}"
  [[ ${RemoveTmpFile} -eq 0 ]]    && PrintLog "Removing tmp files"

  return 0
}


###############################################
# Loading the required modulefiles if any
###############################################
LoadModuleFiles() {
  if   [ -n "${ModuleFiles}" ]; then
    PrintLog "Loading module files: ${ModuleFiles}"
    for ModuleFile in $(echo ${ModuleFiles} | sed "s/,/ /g")
    do
      Command="module load ${ModuleFile} &>/dev/null"
      eval "$Command"
    done
#     Command="module list" && eval "$Command"
  fi
}


PrintCodeInfo() {
  PrintLog "-> @PROJECT_NAME_UC@_NAME"                  "${@PROJECT_NAME_UC@_NAME}"
  PrintLog "-> @PROJECT_NAME_UC@_VERSION"               "${@PROJECT_NAME_UC@_VERSION}"
  PrintLog "-> @PROJECT_NAME_UC@_GIT_BRANCH"            "${@PROJECT_NAME_UC@_GIT_BRANCH}"
  PrintLog "-> @PROJECT_NAME_UC@_GIT_SHA1"              "${@PROJECT_NAME_UC@_GIT_SHA1}"
  PrintLog "-> @PROJECT_NAME_UC@_SOURCE_DIR"            "${@PROJECT_NAME_UC@_SOURCE_DIR}"
  PrintLog "-> @PROJECT_NAME_UC@_INSTALL_DIR"           "${@PROJECT_NAME_UC@_INSTALL_DIR}"
  PrintLog "-> @PROJECT_NAME_UC@_MODULES_DIR"           "${@PROJECT_NAME_UC@_MODULES_DIR}"
  PrintLog "-> @PROJECT_NAME_UC@_BUILD_DATE"            "${@PROJECT_NAME_UC@_BUILD_DATE}"
  PrintLog "-> @PROJECT_NAME_UC@_LIBRARIES_FFLAGS"      "${@PROJECT_NAME_UC@_LIBRARIES_FFLAGS}"
  PrintLog "-> @PROJECT_NAME_UC@_DEFINITION_FFLAGS"     "${@PROJECT_NAME_UC@_DEFINITION_FFLAGS}"
  PrintLog "-> @PROJECT_NAME_UC@_INCLUDE_DIR_FFLAGS"    "${@PROJECT_NAME_UC@_INCLUDE_DIR_FFLAGS}"
  PrintLog "-> @PROJECT_NAME_UC@_LIBRARY_DIR_FFLAGS"    "${@PROJECT_NAME_UC@_LIBRARY_DIR_FFLAGS}"
}


############################################
# Checking existence of source files
############################################
function CheckSourceFiles() {
  PrintLog "SourceFiles"                    "${SourceFiles}"
  MissingFiles=""
  for File in ${SourceFiles}
  do
    if [ -f ${File} ]; then
      PrintLog "-> File '${File}' exists"
      continue
    else
      PrintLog "-> File '${File}' does not exist"
      MissingFiles="${MissingFiles} ${File}"
    fi
  done
  [[ -n ${MissingFiles} ]] && PrintError "Missing Fortran source file: ${MissingFiles}"
  return 0
}

############################################
# Compilation step
############################################
function CompileStep() {

  SOURCE_FILES="${SourceFiles}"
  ListFiles=(${SOURCE_FILES})
  NSourceFiles=${#ListFiles[@]}
  PrintLog "-> SOURCE_FILES"        "${SOURCE_FILES}"
  PrintLog "-> NSourceFiles"        "${NSourceFiles}"
  EXECUTABLE_NAME="${ExecutableName}"
  if   [ ! -n "${EXECUTABLE_NAME}" ]; then
    PrintLog "Executable name not specified"
    PrintLog "-> Setting executable name from main program name"
    LastFile=${ListFiles[NSourceFiles-1]}
    PrintLog "-> LastFile"            "${LastFile}"
    EXECUTABLE_NAME="${LastFile%.*}.x"
  else
    PrintLog "Executable name specified"
  fi

#   LinkerFlags=$(echo ${LinkerFlags} | sed "s/,/ /g")
#   CompilerFlags=$(echo ${CompilerFlags} | sed "s/,/ /g")

# Final variables used for compilation/linking
  INCLUDE_DIR_FLAGS="${@PROJECT_NAME_UC@_INCLUDE_DIR_FFLAGS}"
  LIBRARY_DIR_FLAGS="${@PROJECT_NAME_UC@_LIBRARY_DIR_FFLAGS}"
  LIST_LIBRARY_FLAGS="${@PROJECT_NAME_UC@_LIBRARIES_FFLAGS}"
  DEFINITION_FLAGS="${@PROJECT_NAME_UC@_DEFINITION_FFLAGS}"
  FORTRAN_COMPILER="${@PROJECT_NAME_UC@_FORTRAN_COMPILER_NAME}"
  COMPILER_FLAGS="${DEFINITION_FLAGS} ${INCLUDE_DIR_FLAGS} ${CompilerFlags}"
  LINKER_FLAGS="${LIBRARY_DIR_FLAGS} ${LIST_LIBRARY_FLAGS} ${LinkerFlags}"

  PrintLog "-> EXECUTABLE_NAME"         "${EXECUTABLE_NAME}"
  PrintLog "-> FORTRAN_COMPILER"        "${FORTRAN_COMPILER}"
  PrintLog "-> COMPILER_FLAGS"          "${COMPILER_FLAGS}"
  PrintLog "-> LINKER_FLAGS"            "${LINKER_FLAGS}"
  PrintLog "-> DEFINITION_FLAGS"        "${DEFINITION_FLAGS}"
  PrintLog "-> LIST_LIBRARY_FLAGS"      "${LIST_LIBRARY_FLAGS}"
  PrintLog "-> INCLUDE_DIR_FLAGS"       "${INCLUDE_DIR_FLAGS}"
  PrintLog "-> LIBRARY_DIR_FLAGS"       "${LIBRARY_DIR_FLAGS}"

  PrintLog "Compiling Fortran source files"
  ObjectFiles=""
  for ((i=1; i <= ${NSourceFiles}; i++)); do
    File=${ListFiles[$i-1]}
    PrintLog "-> Compiling file $i/${NSourceFiles} => ${File}"
    Command="${FORTRAN_COMPILER} ${COMPILER_FLAGS} ${File} -c"
    PrintLog "-> Command: ${Command}"
    eval ${Command}
    ObjectFiles="${ObjectFiles} ${File%.*}.o"
  done

  PrintLog "Compiling and linking executable"
  Command="
      ${FORTRAN_COMPILER} ${ObjectFiles} -o ${EXECUTABLE_NAME} \
      ${COMPILER_FLAGS} ${LINKER_FLAGS}"
  PrintLog "-> Command: ${Command}"
  eval ${Command}

}


############################################
# Cleaning-up
############################################
function Cleanup() {
#   [[ ${RemoveTmpFile} -eq 1 ]] && rm -f *.o *.mod *.smod
  if [ ${RemoveTmpFile} -eq 1 ]; then
    rm -f *.o *.mod *.smod
  fi
}


########################################################################################
########################################################################################
#                           Start of the build script
########################################################################################
########################################################################################

PrintLog "Calling $(Underline "ProcessArguments")"
ProcessArguments "$@"

PrintLog
PrintLog "Calling $(Underline "LoadModuleFiles")"
LoadModuleFiles

PrintLog
PrintLog "Calling $(Underline "PrintCodeInfo")"
PrintCodeInfo

PrintLog
PrintLog "Calling $(Underline "CheckSourceFiles")"
CheckSourceFiles

PrintLog
PrintLog "Calling $(Underline "CompileStep")"
CompileStep

Cleanup

PrintLog "End-of-script => Exiting"