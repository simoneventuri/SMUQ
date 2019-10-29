# ifndef FORPACK_INCLUDE_SEEN
# define FORPACK_INCLUDE_SEEN


/*
 *******************************************************************************
 *                  MACRO FOR LOGGING IN PURE PROCEDURES
 *******************************************************************************
 * All the computationally intensive procedures use the 'pure' attribute.
 * This pure attribute offers the compiler additional hits for code optimization
 * such as inlining and vectorization. Thus, it allows the compile to generate
 * highly optimized code. However, one condition for a procedure to be declared
 * as 'pure' is that it should not contain any 'side-effets'. Since external
 * write statements are a side-effect, they are not allowed in 'pure' procedures.
 * As a result, the Logger object cannot be used either in 'pure' procedures.
 * The following preprocessor marcos are intended to provide a way to maintain
 * the pure attribute for computationally intensive procedures when bulding the
 * code in 'Release' mode, but to remove this attribute when bulding the code
 * in 'Debug' mode, thus allowing to print from these procedures. This way, a
 * highly optimized code is still produced for 'Release' builds while logging
 * capabilities are allowed for 'Debug ' builds. This ability to swith on/off
 * the purity attribute is a really usefull feature for code developments and
 * for debugging steps.
 * A set of proprocessor variables are defined to enable:
 * - either logging/debugging capabilities,
 * - or  optimized and side-effect-free code
 * depending on the actual build type used to compile the code.
 * The approach is to statically (ie. at compile-time) specify the purity
 * attribute of a set of predefined Fortran procedures and to conditionally
 * include side-effect instructions such a 'write' statements depending on the
 * build type used to compile the code.
 * These variable are briefly described here:
 *
 * ----------
 * |_Purity_|   Purity attribute for Fortran procedure declation
 * ----------
 *   This variabe is used to add different 'purity' attribute to Fortran
 *   procedure (functions and subroutines) depending on the code build type.
 *   It should be used in the declaration line of any Fortran procedure in
 *   which we need to write logs for debugging, but which also need to be
 *   optimized in production runs. Such procedures need to be 'impure'/'pure'
 *   when a 'Debug'/'Release' build type is used.
 *   The '_Purity_' proprocessor variable is replaced at compile-time by
 *   either an empty string or by the 'Pure' keyword for 'Debug'/'Release'
 *   build type respectively. It should be used instead of the Fortran
 *   keyword 'Pure' to declare a pure Fortran procedure with logging
 *   capabilities in 'Debug' build.
 *   Example:
 *     Replace:               Pure   Subroutine DoSomthing( Inp, Out )
 *     by:                  _Purity_ Subroutine DoSomthing( Inp, Out )
 *
 * --------------
 * |_IfImpure_|   Single-line conditional statement
 * --------------
 *   The '_Purity_' variable only allows to statically select the purity
 *   attribute for some Fortran procedures depending on the build type.
 *   However, when these procedures are pure (for a 'Release' build type), any
 *   call to a external 'write' will result in a compile-time error. This is
 *   the correct behavior since, by definition of pure procedures, no side
 *   effect are allowed and writing to external file units is a type of side
 *   effect. Thus, another preprocessor variable is defined to conditionally
 *   include/exclude at compile-time  a single line of instructions.
 *   This '_IfImpure_' proprocessor variable is replaced at compile-time by
 *   either an empty string or by the Fortran-90 comment character '!' for
 *   'Debug'/'Release' build type respectively. It should be used in front of
 *   each 'write' statment to external file unit in procedures which are
 *   defined with the '_Purity_' attribute. Note that the '_IfImpure_'
 *   variable can also be used on other type of procedure procedures.
 *   Example:
 *     Replace:             call Logger%Write( "Var = ", Var )
 *     by:                  _IfImpure_ call Logger%Write( "Var = ", Var )
 *
 * ------------
 * |_ImpureProcedure_|     Multi-line conditional block
 * ------------
 *   The '_IfImpure_' variable only works for single line statement.
 *   If one wish to conditionally include or exclude several lines of
 *   instructions, one will need to append each line with the '_IfImpure_'
 *   preporcessor variable.
 *   To avoid this inelegant situation, the '_ImpureProcedure_' variable is defined
 *   and set to '1'/'0' for 'Debug'/'Release' build type respectively.
 *   It should be used inside a 'if' preprocessor statment to include or exclude
 *   lines of code in 'Debug'/'Release' build type respectively.
 *   Example:
 *     Replace:
 *         do i = 1,M
 *           call Logger%Write(  "i", i, "Var(i) = ", Var(i) )
 *         end do
 *     By:
 *         # if _ImpureProcedure_
 *            do i = 1,M
 *              call Logger%Write(  "i", i, "Var(i) = ", Var(i) )
 *            end do
 *         # endif
 *******************************************************************************
*/

# ifdef RELEASE
#   define _Purity_               Pure
#   define _IfImpure_             !*** <ExcludedLine> _IfImpure_ ***!
#   define _ImpureProcedure_      0
#   define _IfImpureAndTrue_(a)   !*** <ExcludedLine> _IfImpureAndTrue_(a) ***!
#   define _EndIfImpureAndTrue_   !*** <ExcludedLine> _EndIfImpureAndTrue_ ***!
# else
#   define _Purity_
#   define _IfImpure_
#   define _ImpureProcedure_      1
#   define _IfImpureAndTrue_(a)   if (a)
#   define _EndIfImpureAndTrue_   end if
# endif


/*
 *******************************************************************************
 *            MACRO FOR STRING CONCATENATION
 *******************************************************************************
*/

# ifdef __GFORTRAN__
#   define PASTE(a) a
#   define CONCAT(a,b)        PASTE(a)b
#   define CONCAT3(a,b,c)     CONCAT(a,b)c
#   define CONCAT4(a,b,c,d)   CONCAT3(a,b,c)d
# else
#   define PASTE(a,b)         a ## b
#   define PASTE3(a,b,c)      a ## b ## c
#   define PASTE4(a,b,c,d)    a ## b ## c ## d
#   define CONCAT(a,b)        PASTE(a,b)
#   define CONCAT3(a,b,c)     PASTE3(a,b,c)
#   define CONCAT4(a,b,c,d)   PASTE4(a,b,c,d)
# endif
# define _EVAL(a)       a


/*
 *******************************************************************************
 *            MACRO FOR STRINGIFICATION OF A PREPROCESSOR VARIABLE
 *******************************************************************************
 * The STRINGIFY marco will convert a preprocessor variable into a Fortran string
 *******************************************************************************
*/
# define STRINGIFY(s) str(s)
# define str(s) #s


/*
 *******************************************************************************
 *                    MACRO FOR HANDLING VERSION NUMBERS
 *******************************************************************************
 * The '_IS_VERSION_' function macro is true if the 3 current version numbers
 * (major/minor/patch) corresponds to the ones given in input to the macro.
 * The 3 input arguments of the macro are the 3  integer numbers associated to
 * the major/minor/patch version number to be tested.
 * Each of these arguments can also be equal to the 'x' character in order to
 * indicate that all possible number should be considered.
 * Thus, the macro call "_IS_VERSION_(x,x,x)" will always be true.
 * This macro cannot be used as a Fortran variable.
 * Example:
 *   Assuming the code has been compile with the definitions:
 *      -D FORPACK_VERSION_MAJOR=1 -D FORPACK_VERSION_MINOR=2 -D FORPACK_VERSION_PATCH=3
 *   we will then have:
 *   - if _IS_VERSION_(1,1,1)  ->  False
 *   - if _IS_VERSION_(1,2,2)  ->  False
 *   - if _IS_VERSION_(1,2,3)  ->  True
 *   - if _IS_VERSION_(1,x,x)  ->  True
 *   - if _IS_VERSION_(1,2,x)  ->  True
 *   - if _IS_VERSION_(x,x,x)  ->  True
 *******************************************************************************
*/
# define _IS_VERSION_(Vmaj,Vmin,Vpat) \
      ( (Vmaj == x) .OR. (Vmaj == FORPACK_VERSION_MAJOR) ) .AND. \
      ( (Vmin == x) .OR. (Vmin == FORPACK_VERSION_MINOR) ) .AND. \
      ( (Vpat == x) .OR. (Vpat == FORPACK_VERSION_PATCH) )


/*
 *******************************************************************************
 *                MACRO FOR CONVERTING VERSION NUMBERS IN INTEGERS
 *******************************************************************************
 *
 * ------------------
 * |_NDIGITSVERSION_|
 * ------------------
 *   This preprocessor variable stores the number of digit used to encode
 *   each sub-version number into an integer. Currenlty, only 3 digit numbers
 *   are used. This number can be extended if a given sub-version number has
 *   more than 3 digits. This preprocessor variable is only used in the
 *   '_VERSIONIZE_' preprocessor variable. Note that it can be used as a
 *   regular Fortran integer variable since the returned value is an integer.
 *
 * --------------
 * |_VERSIONIZE_|
 * --------------
 *   The '_VERSIONIZE_' preprocessor macro returns a single integer number
 *   which corresponds to the version number constructed using the 3 input
 *   arguments for the major/minor/patch sub-version numbers. Each
 *   sub-version number is stored in an integer containing '_NDIGITSVERSION_'
 *   digits. The variable returned by the '_VERSIONIZE_' macro has the form
 *   '_VERSIONIZE_ = xxxyyyzzz' where 'xxx', 'yyy' and 'zzz' are 3 integers
 *   storing the major/minor/patch version number associated to the 3 input
 *   arguments Vmaj/Vmin/Vpat. The returned value of the proprocessor macro
 *   can be used as a regular Fortran integer variable since the returned
 *   value is an integer.
 *   Example:
 *     _VERSIONIZE_(1,2,3)    =   001002003
 *     _VERSIONIZE_(8,10,45)  =   008010045
 *     _VERSIONIZE_(2,0,0)    =   002000000
 *
 * ------------------
 * |_VERSION_NUMBER_|
 * ------------------
 *   The _VERSION_NUMBER_ preprocessor macro returns a single integer number
 *   corresponding to the actual version numbers associated to major, minor
 *   and patch sub-versions. The variable returned by the '_VERSION_NUMBER_'
 *   has the form '_VERSION_NUMBER_ = xxxyyyzzz" where xxx/yyy/zzz are
 *   integers storing the major/minor/patch version number. This macro can
 *   be used as a regular Fortran integer variable since the returned value
 *   is an integer.
 *   Example
 *     If the code has been compile with
 *        -D FORPACK_VERSION_MAJOR=1 -D FORPACK_VERSION_MINOR=2 -D FORPACK_VERSION_PATCH=3
 *     we will then have
 *        _VERSION_NUMBER_  =  1002003
 *******************************************************************************
 * Example-1 * To include instructions only for version 1.2.3, use either
 *************
 *   # if _IS_VERSION_(1,2,3)
 *     ... Code to be conditionally included ...
 *   # endif
 *
 *   # if _VERSION_NUMBER_ == _VERSIONIZE_(1,2,3)
 *     ... Code to be conditionally included ...
 *   # endif
 *   # if FORPACK_VERSION_MAJOR == 1 && FORPACK_VERSION_MINOR == 2 && FORPACK_VERSION_PATCH == 3
 *     ... Code to be conditionally included ...
 *   # endif
 *******************************************************************************
 * Example-2 * To include instructions only for all versions greater or equal
 ************* than version 1.2.3, use either
 *   # if _VERSION_NUMBER_ >= _VERSIONIZE_(1,2,3)
 *     ... Code to be conditionally included ...
 *   # endif
 *
 *   # if FORPACK_VERSION_MAJOR >= 1 && FORPACK_VERSION_MINOR >= 2 && FORPACK_VERSION_PATCH >= 3
 *     ... Code to be conditionally included ...
 *   # endif
 *******************************************************************************
 * Example-3 * To include instructions only for all versions with a major version
 ************* of 1 and a minor version of 2, use either
 *   # if _IS_VERSION_(1,2,*)
 *     ... Code to be conditionally included ...
 *   # endif
 *
 *   # if FORPACK_VERSION_MAJOR == 1 && FORPACK_VERSION_MINOR == 2
 *     ... Code to be conditionally included ...
 *   # endif
 *******************************************************************************
*/
# define _NDIGITSVERSION_ 3
# define _VERSIONIZE_(Vmaj,Vmin,Vpat) \
      ( Vmaj * 10**(_NDIGITSVERSION_*2) + \
        Vmin * 10**(_NDIGITSVERSION_*1) + \
        Vpat * 10**(_NDIGITSVERSION_*0) )
# define _VERSION_NUMBER_ _VERSIONIZE_(_EVAL(FORPACK_VERSION_MAJOR),_EVAL(FORPACK_VERSION_MINOR),_EVAL(FORPACK_VERSION_PATCH))



/*
 *******************************************************************************
 *                    MACRO RELATED TO FORTRAN COARRAY
 *******************************************************************************
 * The current version of the intel fortran compiler (v16) has a bug which
 * prevent using coarrays in nested derived-type object. Thus, for the time
 * being, the Mesh%Blocks object is not declared as a coarray but as a normal
 * scalar variable. All the data which need to be accesed from one image to
 * another are then defined separatly in the "Coarray_Variables_Module" in
 * the Base directory.
 * Note that this form does NOT with gcc-6.3.0:
 *    # if ( defined(COARRAY) .and. ! defined(COARRAY_DERIVEDTYPE_WORKAROUND) )
 *******************************************************************************
*/
# if ( defined(COARRAY) && ! defined(COARRAY_DERIVEDTYPE_WORKAROUND) )
#   define _ALLOCATABLE_CODIMENSION_   ,allocatable,codimension[:]
# else
#   define _ALLOCATABLE_CODIMENSION_
# endif

# ifdef COARRAY
#   define _CAF_ASSUMED_CODIM_BRACKET_        [*]
#   define _CAF_DEFERRED_CODIM_               ,codimension[:]
#   define _CAF_ASSUMED_CODIM_                ,codimension[*]
#   define _CAF_FALSE_ALLOC_DEFERRED_CODIM_   ,allocatable,codimension[:]
#   define _CAF_TRUE_ALLOC_DEFERRED_CODIM_    ,allocatable,codimension[:]
# else
#   define _CAF_ASSUMED_CODIM_BRACKET_
#   define _CAF_DEFERRED_CODIM_
#   define _CAF_ASSUMED_CODIM_
#   define _CAF_FALSE_ALLOC_DEFERRED_CODIM_
#   define _CAF_TRUE_ALLOC_DEFERRED_CODIM_    ,allocatable
# endif


/*
 *******************************************************************************
 *                    MACRO RELATED TO GIT VERSION
 *******************************************************************************
 * The '_IS_GIT_BRANCH_' function macro is used to test if the current git
 * branch (ie. the one used to compile the code) matches a given git branch
 * name. The name of the current git branch is stored in the preprocessor
 * variable "FORPACK_GIT_BRANCH" which is set at compile-time.
 * Thus, the '_IS_GIT_BRANCH_(GitBranch)' function macro is true if the current
 * git branch in the variable "FORPACK_GIT_BRANCH" matches the git branch provided
 * in the input argument "GitBranch".
 * This marco can be used to execute different instructions depending on the
 * git branch. For example:
 *      if      ( _IS_GIT_BRANCH_(master)  ) then
 *      else if ( _IS_GIT_BRANCH_(develop) ) then
 *      else if ( _IS_GIT_BRANCH_(feature) ) then
 *      else if ( _IS_GIT_BRANCH_(bugfix)  ) then
 *      else
 *      end if
 * will execute different code depending on whether the current git branch is
 * "master", "develop", "feature", "bugfix" or any other branch.
 *******************************************************************************
*/
# define _IS_GIT_BRANCH_(GitBranch) ( STRINGIFY(FORPACK_GIT_BRANCH) == GitBranch )



/*
 *******************************************************************************
 *                    MACRO RELATED TO NAN CHECKING
 *******************************************************************************
 * The preprocessor variable 'NAN_CHECKING' is used to enter blocks of code in
 * which NaN checking is enabled using a "ifdef ... else ... endif" conditional
 * block. The preprocessor variable '_CheckVar_' depended on whether NaN checking
 * is enabled:
 * - If disabled: The variable '_CheckVar_' corresponds to a Fortran comment line.
 *                As a result, all lines starting by '_CheckVar_' will be ignored
 *                at compile-time
 * - If enabled:  The variable '_CheckVar_' corresponds to the string "call Checker%CheckFinite".
 *                As a result, all lines starting by '_CheckVar_' will be be calling
 *                the checker object.
 * By default, the variable 'NAN_CHECKING' is undefined and so NaN checking is
 * disabled by default.
 * When the code is compiled in 'Release' build, NaN checking is always disabled.
 * When the code is compiled in 'Debug' build, NaN checking can be enabled by
 * defining the variable 'NAN_CHECKING' using '#define NAN_CHECKING' before
 * including the current header file "forpack-include.inc".
 * For example, in order to activate NaN checking in a particular file, add the
 * following at the top of the file:
 *          # define NAN_CHECKING
 *          # include "forpack-include.inc"
# ifdef RELEASE
#   undef  NAN_CHECKING
# endif
 *******************************************************************************
*/

# ifdef NAN_CHECKING
#   define _CheckVar_  call Checker%CheckFinite
# else
#   define _CheckVar_  !call Checker%CheckFinite
# endif




# define _SrcLoc_    __FILE__ // ":" // STRINGIFY(__LINE__)


/*
 *******************************************************************************
 *                  MACRO FOR COARRY VARIABLES
 *******************************************************************************
 * The following preprocessor marcos are intended to provide a uniform way to
 * support both coarray and non-coarray builds by making the code portable
 * when compiled with/without coarray support.
 * When the code is compiled with coarray support, an extra attribute need to
 * be added on the declaration line of coarray variables. This extra attribute
 * is realted to the co-dimension of the coarray variable. However, when the
 * code is compiled without coarray support, these variables are then regular
 * variable (ie. not coarray variables) and so the coarray attributes need to
 * be removed from the declaration line of these variables.
 * The values of these preprocessor marcos is set depending on the preprocessor
 * macro COARRAY. This macro is set using a Fortran definition when compiling
 * the source files. It is true (1) of the code is compiled with coarray support
 * and false (0) otherwise.
 * The following variable are introduced
 *
 * -----------------
 * |_ASSUMED_CODIM_|   Assumed rank co-dimension
 * -----------------
 *   This variabe is used on the declaration line of a coarray variables and
 *   corresponds to the assumed rank co-dimension of the variable.
 *
 *******************************************************************************
*/
# ifdef COARRAY
#   define _ASSUMED_CODIM_         ,codimension[*]
# else
#   define _ASSUMED_CODIM_
# endif



# define _ASSIGN_ALLOCATABLE_CHARACTER_(LHS,RHS) \
    Block; \
      character(:) ,allocatable   :: TmpStr_(:); \
      TmpStr_    =   RHS; \
      call move_alloc( TmpStr_, LHS ); \
    End Block;


# endif /* FORPACK_INCLUDE_SEEN */


