#!/bin/bash -i
set -e

#################################
# Default parameters
#################################
DefaultParallelEnvironment="Serial"   # Default parallel environment: Serial Coarray
DefaultBuildType="Debug"              # Default build type: Release Debug
DefaultFortranCompiler="ifort"        # Default fortran compiler: ifort gfortran
DefaultRequiredModuleFiles=""         # Default list of the required modulefiles to be loaded and passed to cmake
DefaultVerbosity=1                    # Default verbosity indicator: 1:Print from script, >=2:Print from CMake
BuildTests=0                          # Indicator that tests need to be build
BranchInSuffix=0                      # Indicator whether the git branch name should be part of the configuration
ConfigSuffix=""                       # Configuration suffix is specified. Default: empty
DryRun=0                              # Indicator of a 'fake run' (Do nothing)
NProcMake=""                          # Indicator of the number of processor used to build the code (empty means 'all')
DefaultModulefile=0                   # Indicator that the generated modulefile is the default modulefile

Name="forpack"
TopDirectory=$(pwd)
ScriptName="${BASH_SOURCE[0]}"                                # using "$(basename $0)" will not give the correct script name if current script is sourced by another script
ScriptName=${ScriptName##*/}
SourceDirectory="${TopDirectory}/${Name}"                     # Default source directory
BuildDirectory="${TopDirectory}/build"                        # Default build directory
InstallationDirectory="${TopDirectory}/install"               # Default installation directory
ModuleFileInstallationDirectory="${HOME}/Modules/${Name}"     # Default modulefile installation directory
PfunitDirectory=""
MAKE_VERBOSITY="" # "VERBOSE=1"

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
  [[ $Verbosity -lt 1 ]] && return 0
  local Width=40
  local LogPrefix="[${ScriptName}]: "
  local Format="%s%-${Width}s %s\n"
  case $# in
    0) local LogMsg=$(printf "") ;;
#     1) local LogMsg=$(printf "${LogPrefix}$1") ;;
    1) local LogMsg=$(printf "%s%s" "${LogPrefix}" "$1") ;;
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
# Timming function
#################################
GetTime() {
    local  TimeString
    SecPerMin=60
    MinPerHour=60
    SecPerHour=3600
    if [ $RunTime -lt "$SecPerMin" ]; then
      Seconds=${RunTime}
      TimeString="${Seconds}s"
    elif [ $RunTime -lt "$SecPerHour" ]; then
      Minutes=$((RunTime / SecPerMin))
      Seconds=$((RunTime - Minutes * SecPerMin))
      [[ ${Minutes} -lt 10 ]] && Minutes="0${Minutes}"
      [[ ${Seconds} -lt 10 ]] && Seconds="0${Seconds}"
      TimeString="${Minutes}m${Seconds}s"
    else
      Hours=$((RunTime / SecPerHour))
      Seconds=$((RunTime - Hours * SecPerHour))
      if [ ${Seconds} -lt "$SecPerMin" ]; then
        Minutes=00
        [[ ${Seconds} -lt 10 ]] && Seconds="0${Seconds}"
        TimeString="${Hours}h${Minutes}m${Seconds}s"
      else
        Minutes=$((Seconds / SecPerMin))
        Seconds=$((Seconds - Minutes * SecPerMin))
        [[ ${Minutes} -lt 10 ]] && Minutes="0${Minutes}"
        [[ ${Seconds} -lt 10 ]] && Seconds="0${Seconds}"
        TimeString="${Hours}h${Minutes}m${Seconds}s"
      fi
    fi
    echo "${TimeString}"
}


# s_p_h=3600
# h = 5; m = 0; s = 2
# t = h * 3600 + m * 60 + s
#
# h=floor(t/s_p_h)
# m=t-h*s_p_h
# s=m-



#################################
# Help Message
#################################
HelpMessage() {
    cat <<EOM
  $(Bold NAME)
      $ScriptName — Installation script for $(Italic $Name).

  $(Bold SYNOPSIS)
      $ScriptName [-h] [-v] [-n] [-f fortran-compiler-name]
                  [-m modulefiles-list] [-u build-type] [-s source-dir]
                  [-b build-dir] [-i installation-dir] [-o modulefile-dir]
                  [-t]  [-p pfunit-dir]

  $(Bold DESCRIPTION)
      $ScriptName is an installation script for the $Name code. All options
      have a long and short version. For options which need a parameter, the
      name of the option and the parameter value can be separated either by a
      space ($(Italic "--name value")) or an equal symbol ($(Italic "--name=value")).
      The order in which options are specified is irrelevant.

  $(Bold OPTIONS)
        $(Bold "-h, --help")
              Prints a help message describing all of the command-line options.

        $(Bold "-v, --verbose")
              This option increases the amount of information you are given
              during the build process.

        $(Bold "-n, --dry-run")
              This option makes a 'fake run' that doesn't make any changes and
              produces the same output as a real run. It is useful to see what
              the build script is going to do before one actually runs it.

        $(Bold "-f, --fortran-compiler=")$(Underline "fortran-compiler-name")
              This option specifies the name of the Fortran compiler to be  use
              to compile the code. Note that if the Fortran compiler command is
              loaded from a modulefile, this option need to be used in
              conjunction with the '-m' option which tells the script which
              modulefile need to be loaded. By default, the Intel Fortran
              compiler is used, $(Italic ifort).
              Example: $(Italic --fortran-compiler="gfortran")

        $(Bold "-m, --modulefile=")$(Underline "modulefiles-list")
              This option specifies the list of modulefile to be loaded.
              The specified modulefiles will be loaded before the build
              process starts. Thus, this option can be used to select a
              specific Fortran compiler. The name of these modulefiles
              will also be passed to cmake which will add them the list of
              modulefiles needed when running the application of building
              other applications. In particular, the generated modulefile
              will contain an variable called 'SPARK_MODULEFILE' which
              contains this list of modulefiles. Each modulefile needs to
              be separated by a comma ',' with no space on either side of
              the comma.
              Example: $(Italic --modulefile="modulefile1,modulefile2,modulefile3")

        $(Bold "-u, --build-type=")$(Underline "build-type")
              This option specifies the build type. Possible values are $(Italic Debug)
              and $(Italic Release). This build type will impact the compilation
              options, the name of the build/installation directory and the
              name of the generated modulefile. By default, the $(Italic Debug) build
              type is used.
              Example: $(Italic --build-type=Release)

        $(Bold "-e, --parallel-environment=")$(Underline "parallel-environment")
              This option specifies the parallel environment. Possible values
              are $(Italic Serial) and $(Italic Coarray). This parallel environment
              will impact the compilation options, the name of the
              build/installation directory and the name of the generated
              modulefile. By default, the $(Italic Serial) parallel
              environment is used.
              Example: $(Italic --parallel-environment=Coarray)

        $(Bold "-s, --source-dir=")$(Underline "path")
              This option specifies the source directory. By default, the
              source directory is assumed to be located in the current
              directory and its name to match the current code name.

        $(Bold "-b, --build-dir=")$(Underline "path")
              This option specifies the build directory. By default, the build
              directory is created in the current directory and its name is
              automatically generated based as the code parameter and build
              configuration using:
                  $(Italic "build-dir-name=Name-Version-BuildType-FortranCompilerName-FortranCompilerVersion")

        $(Bold "-i, --installation-dir=")$(Underline "path")
              This option specifies the installation directory. By default,
              the installation directory is created in the current directory
              and its name $(Italic install). In the $(Italic cmake) script, an additional
              the sub-directory is created whose name is automatically
              generated based as the code parameter and build configuration
              using:
                  $(Italic "installation-dir-name=Name-Version-BuildType-FortranCompilerName-FortranCompilerVersion")

        $(Bold "-o, --modulefile-dir=")$(Underline "path")
              This option specifies the installation directory for the
              generated modulefile. By default, this directory is the same
              than the installation directory.

        $(Bold "-t, --build-tests")
              This option specifies that the tests should be build along with
              the code. Since testing relies on the $(Italic pFunit) tool, its
              installation directory need to be known to build the tests.
              This can be done in several ways:
               1 - Set the $(Italic PFUNIT) environment variable before launching
                   this installation script
               2 - Specify the $(Italic pFunit) modulefile using the $(Italic --modulefile) option
                   (This modulefile should define the $(Italic PFUNIT) variable)
               3 - Specify the $(Italic pFunit) installation directory using the
                   $(Italic --pfunit-dir) option.

        $(Bold "-x, --suffix=")$(Underline "path")
              This option specifies a suffix to be applied to the configuration
              string. This string is used to set the installation directory and
              the modulefile name.

        $(Bold "-g, --git-branch-config")
              This option specifies that the name of git branch should be added
              as a prefix to the configuration string. This string is used to set
              the installation directory and the modulefile name. This option is
              then pass to cmake which will also include the git branch name into
              the configuration string.

        $(Bold "-j=")$(Underline "<number-or-processors>")
              This option specifies the number of processor to be used to
              compile the code. This option is passed to the make command.
              If no value is provided, or if this option is not used, then
              all available processors will be used. This is equivalent to
              $(Italic make -j).

        $(Bold "-p, --pfunit-dir=")$(Underline "path")
              This option specifies the installation directory for $(Italic pFunit).
              This directory will be used to set the $(Italic PFUNIT)
              environment variable.

        $(Bold "--default-modulefile")
              This option specifies that the generated modulefile should be the default
              modulefile

  $(Bold EXAMPLES)
          ./$ScriptName -m="intel/18.0.0,pfunit/3.1.1-release-intel-18.0.0" -f="ifort" -t -o="/home/geek/Modules/Codes/${Name}" -u="Debug"
          ./$ScriptName -m="gcc/7.2.0,pfunit/3.1.1-release-gnu-7.2.0" -f="gfortran" -t -o="/home/geek/Modules/Codes/${Name}" -u="Release"
          ./$ScriptName -m=intel/18.0.0
          ./$ScriptName -m=gcc/7.2.0 -f=gfortran

EOM
exit 0
}


#################################
# Process Optional Arguments
#################################
ProcessOptionalArguments() {
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
      -v|-\?|--verbosity)
          FirstChar=${2:0:1}
          if [ "$2" ]; then
            Verbosity=$2
            shift
          else
            Verbosity=1
          fi
          ;;
      -v=?*|--verbosity=?*)
          Verbosity=${1#*=}
          ;;
      -v=|--verbosity=)
          Verbosity=0
          ;;
      -vv)
          Verbosity=2
          ;;
#   *********
#   Fake run
#   *********
      -n|-\?|--dry-run)
          DryRun=1
          ;;
#   ****************
#   Fortran Compiler
#   ****************
      -f|--fortran-compiler)
          FirstChar=${2:0:1}
          if [ "$FirstChar" == "-" ]; then PrintError "Option '$1' requires a non-empty value."; fi
          if [ "$2" ]; then
            FortranCompiler=$2
            shift
          else
            PrintError "Option '$1' requires a non-empty value."
          fi
          ;;
      --fortran-compiler=?*|-f=?*)
          FortranCompiler=${1#*=}
          ;;
      --fortran-compiler=|-f=)
          PrintError "Option '$1' requires a non-empty value."
          ;;
#   ***************
#   Modulefile list
#   ***************
      -m|--modulefile)
          FirstChar=${2:0:1}
          if [ "$FirstChar" == "-" ]; then PrintError "Option '$1' requires a non-empty value."; fi
          if [ "$2" ]; then
            RequiredModuleFiles=$2
            shift
          else
            PrintError "Option '$1' requires a non-empty value."
          fi
          ;;
      --modulefile=?*|-m=?*)
          RequiredModuleFiles=${1#*=}
          ;;
      --modulefile=|-m=)
          PrintError "Option '$1' requires a non-empty value."
          ;;
#   **********
#   Build type
#   **********
      -u|--build-type)
          FirstChar=${2:0:1}
          if [ "$FirstChar" == "-" ]; then PrintError "Option '$1' requires a non-empty value."; fi
          if [ "$2" ]; then
            BuildType=$2
            shift
          else
            PrintError "Option '$1' requires a non-empty value."
          fi
          ;;
      -u=?*|--build-type=?*)
          BuildType=${1#*=}
          ;;
      -u=|--build-type=)
          PrintError "Option '$1' requires a non-empty value."
          ;;
#   *********************
#   Parallel Environement
#   *********************
      -e|--parallel-environment)
          FirstChar=${2:0:1}
          if [ "$FirstChar" == "-" ]; then PrintError "Option '$1' requires a non-empty value."; fi
          if [ "$2" ]; then
            ParallelEnvironment=$2
            shift
          else
            PrintError "Option '$1' requires a non-empty value."
          fi
          ;;
      -e=?*|--parallel-environment=?*)
          ParallelEnvironment=${1#*=}
          ;;
      -e=|--parallel-environment=)
          PrintError "Option '$1' requires a non-empty value."
          ;;
#   ****************
#   Source directory
#   ****************
      -s|--source-dir)
          FirstChar=${2:0:1}
          if [ "$FirstChar" == "-" ]; then PrintError "Option '$1' requires a non-empty value."; fi
          if [ "$2" ]; then
            SourceDirectory=$2
            shift
          else
            PrintError "Option '$1' requires a non-empty value."
          fi
          ;;
      -s=?*|--source-dir=?*)
          SourceDirectory=${1#*=}
          ;;
      -s=|--source-dir=)
          PrintError "Option '$1' requires a non-empty value."
          ;;
#   ***************
#   Build directory
#   ***************
      -b|--build-dir)
          FirstChar=${2:0:1}
          if [ "$FirstChar" == "-" ]; then PrintError "Option '$1' requires a non-empty value."; fi
          if [ "$2" ]; then
            BuildDirectory=$2
            shift
          else
            PrintError "Option '$1' requires a non-empty value."
          fi
          ;;
      -b=?*|--build-dir=?*)
          BuildDirectory=${1#*=}
          ;;
      -b=|--build-dir=)
          PrintError "Option '$1' requires a non-empty value."
          ;;
#   **********************
#   Installation directory
#   **********************
      -i|--installation-dir)
          FirstChar=${2:0:1}
          if [ "$FirstChar" == "-" ]; then PrintError "Option '$1' requires a non-empty value."; fi
          if [ "$2" ]; then
            InstallationDirectory=$2
            shift
          else
            PrintError "Option '$1' requires a non-empty value."
          fi
          ;;
      -i=?*|--installation-dir=?*)
          InstallationDirectory=${1#*=}
          ;;
      -i=|--installation-dir=)
          PrintError "Option '$1' requires a non-empty value."
          ;;
      --no-install)
          NoInstallation=1
          ;;
#   **********************
#   Modulefile directory
#   **********************
      -o|--modulefile-dir)
          FirstChar=${2:0:1}
          if [ "$FirstChar" == "-" ]; then PrintError "Option '$1' requires a non-empty value."; fi
          if [ "$2" ]; then
            ModuleFileInstallationDirectory=$2
            shift
          else
            PrintError "Option '$1' requires a non-empty value."
          fi
          ;;
      -o=?*|--modulefile-dir=?*)
          ModuleFileInstallationDirectory=${1#*=}
          ;;
      -o=|--modulefile-dir=)
          PrintError "Option '$1' requires a non-empty value."
          ;;
#   **********************
#   Build tests
#   **********************
      -t|-\?|--build-tests)
          BuildTests=1
          ;;
#   ********************
#   Configuration suffix
#   ********************
      -x|--suffix)
          FirstChar=${2:0:1}
          if [ "$FirstChar" == "-" ]; then PrintError "Option '$1' requires a non-empty value."; fi
          if [ "$2" ]; then
            ConfigSuffix=$2
            shift
          else
            PrintError "Option '$1' requires a non-empty value."
          fi
          ;;
      -x=?*|--suffix=?*)
          ConfigSuffix=${1#*=}
          ;;
      -x=|--suffix=)
          PrintError "Option '$1' requires a non-empty value."
          ;;
#   ********************
#   Git branch in config
#   ********************
      -g|--git-branch-config)
          BranchInSuffix=1
          ;;
#   **********************************************************
#   Number of processor to be used during build (make -j <n>)
#   **********************************************************
      -j)
          FirstChar=${2:0:1}
          if [ "$2" ]; then
            NProcMake=$2
            shift
          fi
          ;;
      -j=?*)
          NProcMake=${1#*=}
          ;;
#   **********************
#   Pfunit directory
#   **********************
      -p|--pfunit-dir)
          FirstChar=${2:0:1}
          if [ "$FirstChar" == "-" ]; then PrintError "Option '$1' requires a non-empty value."; fi
          if [ "$2" ]; then
            PfunitDirectory=$2
            shift
          else
            PrintError "Option '$1' requires a non-empty value."
          fi
          ;;
      -p=?*|--pfunit-dir=?*)
          PfunitDirectory=${1#*=}
          ;;
      -p=|--pfunit-dir=)
          PrintError "Option '$1' requires a non-empty value."
          ;;
#   **********************
#   Default modulefile
#   **********************
      --default-modulefile)
          DefaultModulefile=1
          ;;
## End of all options.
      --)
          shift
          break
          ;;
#       -?*) PrintError "Unknown option $1."
#           ;;
      *)  break # Default case: No more options, so break out of the loop.
    esac
    shift
  done



  if [ ! -n "$Verbosity" ]; then
    if [ -n "${DefaultVerbosity}" ]; then
      PrintLog "Setting default vebosity:   ${DefaultVerbosity}"
      Verbosity=${DefaultVerbosity}
    fi
  fi

  PrintLog "Setting default build parameters"
  if [ ! -n "${BuildType}" ]; then
    if [ -n "${DefaultBuildType}" ]; then
      PrintLog "-> Setting default build type: ${DefaultBuildType}"
      BuildType=${DefaultBuildType}
    else
      PrintError "Build type has not been specified. See '$(Italic $ScriptName) --$(Italic help)' for more details."
    fi
  fi


  PrintLog "Setting default parallel environment"
  if [ ! -n "${ParallelEnvironment}" ]; then
    if [ -n "${DefaultParallelEnvironment}" ]; then
      PrintLog "-> Setting default parallel environment type: ${DefaultParallelEnvironment}"
      ParallelEnvironment=${DefaultParallelEnvironment}
    else
      PrintError "Parallel environment has not been specified. See '$(Italic $ScriptName) --$(Italic help)' for more details."
    fi
  fi
  IsParallel=0
  if [ "${ParallelEnvironment}" != "Serial" ]; then IsParallel=1; fi


  if [ ! -n "${FortranCompiler}" ]; then
    if [ -n "${DefaultFortranCompiler}" ]; then
      PrintLog "-> Setting default Fortran compiler: ${DefaultFortranCompiler}"
      FortranCompiler=${DefaultFortranCompiler}
    else
      PrintError "Fortran compiler has not been specified. See '$(Italic $ScriptName) --$(Italic help)' for more details."
    fi
  fi

  if [ ! -n "${RequiredModuleFiles}" ]; then
    if [ -n "${DefaultRequiredModuleFiles}" ]; then
      PrintLog "-> Setting default required modulefiles list: ${DefaultRequiredModuleFiles}"
      RequiredModuleFiles=${DefaultRequiredModuleFiles}
    fi
  fi

  if [ ! -n "${ModuleFileInstallationDirectory}" ]; then
    if [ ! -n "${NoInstallation}" ]; then
      PrintLog "-> Setting default modulefile installation directory: ${InstallationDirectory}"
      ModuleFileInstallationDirectory=${InstallationDirectory}
    fi
  fi

  if [ -n "${NProcMake}" ]; then
    PrintLog "Number of processor to be used during build: ${NProcMake}"
    if [ "${NProcMake}" -ne "${NProcMake}" ]; then
      PrintError "Wrong option for the number of processor to be used during build (option '-j')
    The specified value is '${NProcMake}' which is not an integer.
    See '$(Italic $ScriptName) --$(Italic help)' for more details."
    fi
  fi

  PrintLog
  PrintLog "Summary of build parameters"
  PrintLog "-> TopDirectory"        "${TopDirectory}"
  PrintLog "-> Verbosity"           "$Verbosity"
  PrintLog "-> BuildType"           "${BuildType}"
  PrintLog "-> ParallelEnvironment" "${ParallelEnvironment}"
  PrintLog "-> IsParallel"     "${IsParallel}"
  PrintLog "-> FortranCompiler"     "${FortranCompiler}"
  [ -n "${RequiredModuleFiles}" ] && PrintLog "-> RequiredModuleFiles" "${RequiredModuleFiles}"
  [ -n "${ModuleFileInstallationDirectory}" ]  && PrintLog "-> ModuleFileInstallationDirectory" "${ModuleFileInstallationDirectory}"
}


#################################
# Process Optional Arguments
#################################
LoadModuleFiles() {
  if [ -n "${RequiredModuleFiles}" ]; then
    PrintLog
    PrintLog "Loading module files: ${RequiredModuleFiles}"
    for ModuleFile in $(echo ${RequiredModuleFiles} | sed "s/,/ /g")
    do
      Command="module load ${ModuleFile} "
      PrintLog "-> Command: ${Command}"
      eval "$Command"
      Command="${Command} 2>&1"
      ModuleOutputMsg=$(module load ${ModuleFile} 2>&1)
#       if [ -n "$ModuleOutputMsg" ]; then PrintError "ERROR: Problem loading modulefile '${ModuleFile}': $ModuleOutputMsg"; fi
    done
#   else
#     PrintLog "No modulefile to be loaded"
  fi
}


#################################
# Get Fortran compiler info
#################################
# Set the variables related to the Fortran compiler.
# On input, the nale of the Fortran compiler need to be set in the variable FORTRAN_COMPILER
# On output, the following variable are set:
# - FORTRAN_COMPILER_VERSION      Version of the Fortran compiler
# - FORTRAN_COMPILER_VENDOR       Vendor of the Fortran compiler
# - FORTRAN_COMPILER_FULLPATH     Full path to the fortran compiler
# - FORTRAN_COMPILER_CONFIG       Configuration of the Fortran compiler
PrintFortranCompilerInfo() {
  PrintLog
  PrintLog "Fortran compiler info"
  PrintLog "-> FORTRAN_COMPILER"          "${FORTRAN_COMPILER}"
  PrintLog "-> FORTRAN_COMPILER_VERSION"  "${FORTRAN_COMPILER_VERSION}"
  PrintLog "-> FORTRAN_COMPILER_VENDOR"   "${FORTRAN_COMPILER_VENDOR}"
  PrintLog "-> FORTRAN_COMPILER_FULLPATH" "${FORTRAN_COMPILER_FULLPATH}"
  PrintLog "-> FORTRAN_COMPILER_CONFIG"   "${FORTRAN_COMPILER_CONFIG}"
}



SetFortranCompilerInfo() {
  if (( $# != 1 )); then PrintError "[Error in $FUNCNAME]: Illegal number of parameters... First argument should be the Fortran compiler (ie. gfortran, ifort, ...)"; fi
  FORTRAN_COMPILER=$1
  if [ ! -n "${FORTRAN_COMPILER}" ]; then
    PrintError "ERROR: The variable FORTRAN_COMPILER is not defiend"
  fi
  if [ "${FORTRAN_COMPILER}" == "ifort" ]; then
    VERSION_POSITION=3; VENDOR_POSITION=8
  elif [ "${FORTRAN_COMPILER}" == "gfortran" ]; then
    VERSION_POSITION=4; VENDOR_POSITION=1
  elif [ "${FORTRAN_COMPILER}" == "pgfortran" ]; then
    VERSION_POSITION=2; VENDOR_POSITION=10
  fi
  export FORTRAN_COMPILER_VERSION=$(echo $(${FORTRAN_COMPILER} --version) | awk "{ print \$${VERSION_POSITION} }" | sed -r 's/-/./g'  )
  FORTRAN_COMPILER_VERSION=$(echo ${FORTRAN_COMPILER_VERSION} | cut -f1,2,3 -d'.')  # Remove the TWEAK number if the version string is as "MAJOR.MINOR.PATCH.TWEAK"
  export FORTRAN_COMPILER_VENDOR=$( echo $(${FORTRAN_COMPILER} --version) | awk "{ print \$${VENDOR_POSITION} }")
  export FORTRAN_COMPILER_FULLPATH=$(which ${FORTRAN_COMPILER})
  export FORTRAN_COMPILER_CONFIG="${FORTRAN_COMPILER_VENDOR,,}-${FORTRAN_COMPILER_VERSION,,}"
  PrintFortranCompilerInfo
}


#################################
# Get Git repository info
#################################
# Set the code version from the git repository
# It also set the name of th code including the code version
PrintGitRepositoryInfo() {
  PrintLog
  PrintLog "Git repository info"
  PrintLog "-> GitRepositoryPath"     "${GitRepositoryPath}"
  PrintLog "-> Name"                  "${Name}"
  PrintLog "-> Version"               "${Version}"
  PrintLog "-> NameVersion"           "${NameVersion}"
  PrintLog "-> Full git branch"       "${FullGitBranchName}"
  PrintLog "-> Git branch"            "${GitBranchName}"
}

SetCmakeVersion() {
  if (( $# != 1 )); then PrintError "[Error in $FUNCNAME]: Illegal number of parameters... First argument should be the path to the folder containg the camke file"; fi
  CmakeFile="$1/CMakeLists.txt"
  PrintLog "-> Searching for cmake file '${CmakeFile}'"
  if [ -f "${CmakeFile}" ]; then
    PrintLog "-> File '${CmakeFile}' does exist"
    CmakeVersion=$(\
      grep -i "Project" ${CmakeFile} | \
      grep -i "${Name}"   | grep -i "VERSION" | \
      grep -o -P '(?<=VERSION ).*(?=\))' )
    PrintLog "-> cmake version"  "${CmakeVersion}"
    Version="${CmakeVersion}"
    Version="$(echo -e "${Version}" | tr -d '[:space:]')"
  fi
  export Version
  PrintLog "-> Version"   "${Version}"
}

# This functions will search for the code version.
# The version is fist taken from the git repository as the most recent tag.
# If there is not git directory, then version is taken from
# the main 'CMakeLists.txt' file as the project version.
# If this version umber is not found, then the version
# is set to "0.0.0".
SetGitRepositoryInfo() {
  if (( $# != 1 )); then PrintError "[Error in $FUNCNAME]: Illegal number of parameters... First argument should be the path to the git repository"; fi
  GitRepositoryPath=$1
  local Name=$(basename ${GitRepositoryPath})
  GitRepository="${GitRepositoryPath}/.git"

  PrintLog "-> Searching for git repository '${GitRepository}'"
  if [ -d "${GitRepositoryPath}/.git" ]; then
    FullGitBranchName="$( git -C ${GitRepositoryPath} branch | sed -n -e 's/^\* \(.*\)/\1/p' | sed -r 's/\//_/g' )"
    GitBranchName="$( echo ${FullGitBranchName} | sed -r 's/\//_/g' )"
    PrintLog "-> Git repository found... setting version as most recent tag"
    Version="$(git -C ${GitRepositoryPath} tag)"
    Version="$(echo -e "${Version}" | tr -d '[:space:]')"
    NameVersion="${Name}"
    if   [ -n "${Version}" ]; then
      Version="$(git -C ${GitRepositoryPath} describe --abbrev=0)"      # Only execute this if tags are actually present
      if [ "${Version:0:1}" == "v" ]; then Version="${Version:1}"; fi
      NameVersion="${NameVersion}-${Version}"
    fi
  fi

  if   [ ! -n "${Version}" ]; then
    PrintLog "-> Version string empty"
    PrintLog "-> Searching for cmake version"
    SetCmakeVersion $1
    PrintLog "-> Version   ${Version}"
  fi

  if   [ ! -n "${Version}" ]; then
    Version="0.0.0"
  fi

  NameVersion="${Name}-${Version}"
  export Version
  export NameVersion="${NameVersion,,}"
  PrintGitRepositoryInfo
}

#################################
# Setting build parameters
#################################
SetBuildParametersInfo() {

  # Checking preconditions
  if [ ! -n "${BuildDirectory}" ]; then           PrintError "[Error in $FUNCNAME]: The variable 'BuildDirectory' need to be defiend"; fi
  if [ ! -n "${BuildType}" ]; then                PrintError "[Error in $FUNCNAME]: The variable 'BuildType' need to be defiend"; fi
  if [ ! -n "${FORTRAN_COMPILER_CONFIG}" ]; then  PrintError "[Error in $FUNCNAME]: The variable 'FORTRAN_COMPILER_CONFIG' need to be defiend"; fi

  # Setting build configuration
  Config="${BuildType,,}-${FORTRAN_COMPILER_CONFIG}"

  # If requested, and if there is a branch name, add it to the configuration
  if [ ${BranchInSuffix} -eq "1" ] && [ -n "${GitBranchName}" ]; then
    Config="${GitBranchName,,}-${Config}"
  fi

# If parallel build, add it to the configuration
  if [ ${IsParallel} == 1 ]; then Config="${ParallelEnvironment,,}-${Config}"; fi

# If requested, add a user-defined suffix to the configuration
  [[ -n "${ConfigSuffix}" ]] && Config="${Config}-${ConfigSuffix}"

  export NameVersionConfig="${NameVersion}-${Config}"
  VersionConfig="${Version}-${Config}"

  # Setting build directory
  if [ -n "${BuildDirectory}" ]; then
    BuildDirectory="${BuildDirectory}/${NameVersionConfig}"
  else
    BuildDirectory="${TopDirectory}/build/${NameVersionConfig}"
  fi
  export BuildDirectory

  PrintLog
  PrintLog "Build configuration"
  PrintLog "-> NameVersionConfig"     "${NameVersionConfig}"
  PrintLog "-> VersionConfig"         "${VersionConfig}"
  PrintLog "-> Config"                "${Config}"
  PrintLog "-> BuildDirectory"        "${BuildDirectory}"
  PrintLog "Directories"
  PrintLog "-> TopDirectory"                    "${TopDirectory}"
  PrintLog "-> SourceDirectory"                 "${SourceDirectory}"
  PrintLog "-> BuildDirectory"                  "${BuildDirectory}"
  PrintLog "-> InstallationDirectory"           "${InstallationDirectory}"
  PrintLog "-> ModuleFileInstallationDirectory" "${ModuleFileInstallationDirectory}"
  PrintLog "-> PfunitDirectory"                 "${PfunitDirectory}"

  # Create the build directory and move inside
  if [ ! -n "${BuildDirectory}" ]; then PrintError "ERROR: No build directory"; fi
  Command="mkdir -p ${BuildDirectory}"
  PrintLog "-> Command: ${Command}"
  [[ ${DryRun} -ne 1 ]] && eval ${Command}

  Command="cd ${BuildDirectory}"
  PrintLog "-> Command: ${Command}"
  [[ ${DryRun} -ne 1 ]] && eval ${Command}
  return 0
}


##############################
# Add a cmake definition
##############################
function AddCmakeDefinition() {
  CmakeDefinition="$1"
  CmakeDefinitionsList="${CmakeDefinitionsList} ${CmakeDefinition}"
  PrintLog "-> Adding Cmake definition:       ${CmakeDefinition}"
}


##############################
# Configuration step
##############################
function ConfigureStep() {
PrintLog
  PrintLog "Configuration the code"
  AddCmakeDefinition "-DCMAKE_BUILD_TYPE=${BuildType}"
  AddCmakeDefinition "-DCMAKE_Fortran_COMPILER=${FORTRAN_COMPILER}"
  [[ ${IsParallel} -eq 1 ]]         && AddCmakeDefinition "-DPARALLEL_ENVIRONMENT=${ParallelEnvironment}"
  [[ ! -n "${NoInstallation}" ]]    && AddCmakeDefinition "-DCMAKE_INSTALL_PREFIX=${InstallationDirectory}"
  [[ ! -n "${NoInstallation}" ]]    && AddCmakeDefinition "-DMODULEFILE_INSTALL_DIR=${ModuleFileInstallationDirectory}"
  [[ ${DefaultModulefile} -eq 1 ]]  && AddCmakeDefinition "-DDEFAULT_MODULEFILE=ON"
  [[ ${BuildTests} -eq 1 ]]         && AddCmakeDefinition "-DBUILD_TESTS=ON"
  [[ -n "${PfunitDirectory}" ]]     && AddCmakeDefinition "-DPFUNIT_DIR=${PfunitDirectory}"
  [[ -n "${ConfigSuffix}" ]]        && AddCmakeDefinition "-DCONFIG_SUFFIX=${ConfigSuffix}"
  [[ -n "${RequiredModuleFiles}" ]] && AddCmakeDefinition "-DREQUIRED_MODULEFILES=${RequiredModuleFiles}"
  [[ ${BranchInSuffix} -eq 1 ]]     && AddCmakeDefinition "-DBRANCH_IN_CONFIG=ON"

  Command="cd ${BuildDirectory}"
  PrintLog "-> Command: ${Command}"
  [[ ${DryRun} -ne 1 ]] && eval ${Command}

  Command="cmake ${SourceDirectory} ${CmakeDefinitionsList}"
  PrintLog "-> Command: ${Command}"

  if [ ${DryRun} -ne 1 ]; then
    StartTime=$(date +%s)
    eval ${Command}
    EndTime=$(date +%s)
    RunTime=$((EndTime-StartTime))
    BuildTime=$(GetTime)
    PrintLog "-> BuildTime: $BuildTime"
  fi
  return 0
}


############################################
# Compilation step
############################################
function CompileStep() {
  PrintLog
  PrintLog "Compilation step"

  Command="cd ${BuildDirectory}"
  PrintLog "-> Command: ${Command}"
  [[ ${DryRun} -ne 1 ]] && eval ${Command}

  if [ ${Verbosity} -ge "2" ]; then
    MAKE_VERBOSITY="VERBOSE=1"
  else
    MAKE_VERBOSITY=""
  fi

  if [ -n "${NProcMake}" ]; then
    Command="make -j ${NProcMake} all ${MAKE_VERBOSITY}"
    PrintLog "-> Command: ${Command}"
  else
    Command="make -j all ${MAKE_VERBOSITY}"
    PrintLog "-> Command: ${Command}"
  fi

  if [ ${DryRun} -ne 1 ]; then
    StartTime=$(date +%s)
    eval ${Command}
    EndTime=$(date +%s)
    RunTime=$((EndTime-StartTime))
    CompileTime=$(GetTime)
    PrintLog "-> CompileTime: $CompileTime"
  fi
  return 0
}


###########################################
# Installation step
###########################################
function InstallStep() {
  PrintLog
  PrintLog "Installation step"
  if [ -n "${NoInstallation}" ]; then
    PrintLog "-> No installation required"
    return 0
  fi
  Command="cd ${BuildDirectory}"
  PrintLog "-> Command: ${Command}"
  [[ ${DryRun} -ne 1 ]] && eval ${Command}
  Command="make install ${MAKE_VERBOSITY}"
  PrintLog "-> Command: ${Command}"
  if [ ${DryRun} -ne 1 ]; then
    StartTime=$(date +%s)
    eval ${Command}
    EndTime=$(date +%s)
    RunTime=$((EndTime-StartTime))
    InstallTime=$(GetTime)
    PrintLog "-> InstallTime: $InstallTime"
  fi
}


###########################################
# Unit-test step
###########################################
function UnitTestStep() {
  if [ ${BuildTests} -eq "0" ]; then return 0; fi
  PrintLog
  PrintLog "Executing tests"
  Command="cd ${BuildDirectory}"
  PrintLog "-> Command: ${Command}"
  [[ ${DryRun} -ne 1 ]] && eval ${Command}
  Command="ctest"
  PrintLog "-> Command: ${Command}"
  if [ ${DryRun} -ne 1 ]; then
    StartTime=$(date +%s)
    eval ${Command}
    EndTime=$(date +%s)
    RunTime=$((EndTime-StartTime))
    UnitTestTime=$(GetTime)
    PrintLog "-> UnitTestTime: $UnitTestTime"
  fi
}


###########################################
# Setting default modulefile
###########################################
function SetDefaultModuleFile() {
  PrintLog
  PrintLog "Setting default module file"
  if [ ${DefaultModulefile} -eq "0" ]; then
    PrintLog "-> No default modulefile version file to be written"
    return 0
  fi
  ModuleVersionFile="${ModuleFileInstallationDirectory}/.version"
  PrintLog "-> Setting default module file to ${VersionConfig}"
#   PrintLog "-> ModuleVersionFile: ${ModuleVersionFile}"
  Command='echo -e "#%Module\\nset ModulesVersion \"${VersionConfig}\"" > ${ModuleVersionFile}""'
  PrintLog "-> Command: '${Command}'"
  [[ ${DryRun} -ne 1 ]] && eval ${Command}
}


########################################################################################
########################################################################################
#                           Start of the build script
########################################################################################
########################################################################################

ProcessOptionalArguments "$@"

LoadModuleFiles

SetFortranCompilerInfo ${FortranCompiler}

SetGitRepositoryInfo ${SourceDirectory}

SetBuildParametersInfo

ConfigureStep

CompileStep

InstallStep

UnitTestStep

# SetDefaultModuleFile

cd ${TopDirectory}

PrintLog "Timing"
PrintLog "-> Build     " "$BuildTime"
PrintLog "-> Compile   " "$CompileTime"
PrintLog "-> Install   " "$InstallTime"
PrintLog "-> UnitTest  " "$UnitTestTime"
PrintLog "End-of-script => Exiting"
