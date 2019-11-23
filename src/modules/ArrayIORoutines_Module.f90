module ArrayIORoutines_Module

use String_Library
use Input_Library
use Parameters_Library
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    ImportArray
public                                                                ::    ExportArray
public                                                                ::    ImportFile
public                                                                ::    ExportFile

logical, parameter                                                    ::    DebugGlobal = .false.

interface ImportArray
  module procedure                                                    ::    ImportArrayInput_R1D
  module procedure                                                    ::    ImportArrayInput_I1D
  module procedure                                                    ::    ImportArrayInput_I81D
  module procedure                                                    ::    ImportArrayInput_C1D
  module procedure                                                    ::    ImportArrayInput_L1D
  module procedure                                                    ::    ImportArrayInput_CX1D
  module procedure                                                    ::    ImportArrayInput_String1D
  module procedure                                                    ::    ImportArrayInput_R2D
  module procedure                                                    ::    ImportArrayInput_I2D
  module procedure                                                    ::    ImportArrayInput_I82D
  module procedure                                                    ::    ImportArrayInput_C2D
  module procedure                                                    ::    ImportArrayInput_L2D
  module procedure                                                    ::    ImportArrayInput_CX2D
  module procedure                                                    ::    ImportArrayInput_String2D
  module procedure                                                    ::    ImportArray_R1D
  module procedure                                                    ::    ImportArray_I1D
  module procedure                                                    ::    ImportArray_I81D
  module procedure                                                    ::    ImportArray_C1D
  module procedure                                                    ::    ImportArray_L1D
  module procedure                                                    ::    ImportArray_CX1D
  module procedure                                                    ::    ImportArray_String1D
  module procedure                                                    ::    ImportArray_R2D
  module procedure                                                    ::    ImportArray_I2D
  module procedure                                                    ::    ImportArray_I82D
  module procedure                                                    ::    ImportArray_C2D
  module procedure                                                    ::    ImportArray_L2D
  module procedure                                                    ::    ImportArray_CX2D
  module procedure                                                    ::    ImportArray_String2D
end interface

interface ExportArray
  module procedure                                                    ::    ExportArrayInput_R1D
  module procedure                                                    ::    ExportArrayInput_I1D
  module procedure                                                    ::    ExportArrayInput_I81D
  module procedure                                                    ::    ExportArrayInput_C1D
  module procedure                                                    ::    ExportArrayInput_L1D
  module procedure                                                    ::    ExportArrayInput_CX1D
  module procedure                                                    ::    ExportArrayInput_String1D
  module procedure                                                    ::    ExportArrayInput_R2D
  module procedure                                                    ::    ExportArrayInput_I2D
  module procedure                                                    ::    ExportArrayInput_I82D
  module procedure                                                    ::    ExportArrayInput_C2D
  module procedure                                                    ::    ExportArrayInput_L2D
  module procedure                                                    ::    ExportArrayInput_CX2D
  module procedure                                                    ::    ExportArrayInput_String2D
  module procedure                                                    ::    ExportArray_R1D
  module procedure                                                    ::    ExportArray_I1D
  module procedure                                                    ::    ExportArray_I81D
  module procedure                                                    ::    ExportArray_C1D
  module procedure                                                    ::    ExportArray_L1D
  module procedure                                                    ::    ExportArray_CX1D
  module procedure                                                    ::    ExportArray_String1D
  module procedure                                                    ::    ExportArray_R2D
  module procedure                                                    ::    ExportArray_I2D
  module procedure                                                    ::    ExportArray_I82D
  module procedure                                                    ::    ExportArray_C2D
  module procedure                                                    ::    ExportArray_L2D
  module procedure                                                    ::    ExportArray_CX2D
  module procedure                                                    ::    ExportArray_String2D
end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArrayInput_R1D( Input, Array, Prefix, RowMajor, Debug )

    class(InputSection_Type), intent(in)                              ::    Input
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Array
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ImportArrayInput_R1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    ArrayFile
    integer                                                           ::    NbLinesSkip=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    Source = VarC0D

    SectionName = 'source'
    NbLinesSkip = 0

    select case (Source)
      case('external')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        ParameterName = 'nb_lines_skip'
        call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) NbLinesSkip = VarI0D

        SubSectionName = SectionName // '>file'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ArrayFile%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
        call ImportArray( File=ArrayFile, Array=Array, NbLinesSkip=NbLinesSkip, RowMajor=RowMajorLoc )
      case('internal')
        ParameterName = 'values'
        call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.true. )
        allocate(Array, source=ConvertToReals( String=VarC0D, Separator=' ' ), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArrayInput_I1D( Input, Array, Prefix, RowMajor, Debug )

    class(InputSection_Type), intent(in)                              ::    Input
    integer, allocatable, dimension(:), intent(out)                   ::    Array
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ImportArrayInput_I1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    ArrayFile
    integer                                                           ::    NbLinesSkip=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    Source = VarC0D

    SectionName = 'source'
    NbLinesSkip = 0

    select case (Source)
      case('external')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        ParameterName = 'nb_lines_skip'
        call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) NbLinesSkip = VarI0D

        SubSectionName = SectionName // '>file'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ArrayFile%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
        call ImportArray( File=ArrayFile, Array=Array, NbLinesSkip=NbLinesSkip, RowMajor=RowMajorLoc )
      case('internal')
        ParameterName = 'values'
        call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.true. )
        allocate(Array, source=ConvertToIntegers( String=VarC0D, Separator=' ' ), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArrayInput_I81D( Input, Array, Prefix, RowMajor, Debug )

    class(InputSection_Type), intent(in)                              ::    Input
    integer(8), allocatable, dimension(:), intent(out)                ::    Array
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ImportArrayInput_I81D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    ArrayFile
    integer                                                           ::    NbLinesSkip=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    Source = VarC0D

    SectionName = 'source'
    NbLinesSkip = 0

    select case (Source)
      case('external')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        ParameterName = 'nb_lines_skip'
        call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) NbLinesSkip = VarI0D

        SubSectionName = SectionName // '>file'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ArrayFile%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
        call ImportArray( File=ArrayFile, Array=Array, NbLinesSkip=NbLinesSkip, RowMajor=RowMajorLoc )
      case('internal')
        ParameterName = 'values'
        call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.true. )
        allocate(Array, source=ConvertToInteger8s( String=VarC0D, Separator=' ' ), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArrayInput_C1D( Input, Array, Prefix, RowMajor, Debug )

    class(InputSection_Type), intent(in)                              ::    Input
    character(:), allocatable, dimension(:), intent(out)              ::    Array
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ImportArrayInput_C1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    ArrayFile
    integer                                                           ::    NbLinesSkip=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    Source = VarC0D

    SectionName = 'source'
    NbLinesSkip = 0

    select case (Source)
      case('external')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        ParameterName = 'nb_lines_skip'
        call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) NbLinesSkip = VarI0D

        SubSectionName = SectionName // '>file'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ArrayFile%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
        call ImportArray( File=ArrayFile, Array=Array, NbLinesSkip=NbLinesSkip, RowMajor=RowMajorLoc )
      case('internal')
        ParameterName = 'values'
        call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.true. )
        call Parse( Input=VarC0D, Separator=' ', Output=Array )
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArrayInput_L1D( Input, Array, Prefix, RowMajor, Debug )

    class(InputSection_Type), intent(in)                              ::    Input
    logical, allocatable, dimension(:), intent(out)                   ::    Array
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ImportArrayInput_L1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    ArrayFile
    integer                                                           ::    NbLinesSkip=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    Source = VarC0D

    SectionName = 'source'
    NbLinesSkip = 0

    select case (Source)
      case('external')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        ParameterName = 'nb_lines_skip'
        call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) NbLinesSkip = VarI0D

        SubSectionName = SectionName // '>file'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ArrayFile%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
        call ImportArray( File=ArrayFile, Array=Array, NbLinesSkip=NbLinesSkip, RowMajor=RowMajorLoc )
      case('internal')
        ParameterName = 'values'
        call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.true. )
        allocate(Array, source=ConvertToLogicals( String=VarC0D, Separator=' ' ), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArrayInput_CX1D( Input, Array, Prefix, RowMajor, Debug )

    class(InputSection_Type), intent(in)                              ::    Input
    complex, allocatable, dimension(:), intent(out)                   ::    Array
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ImportArrayInput_CX1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    ArrayFile
    integer                                                           ::    NbLinesSkip=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    Source = VarC0D

    SectionName = 'source'
    NbLinesSkip = 0

    select case (Source)
      case('external')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        ParameterName = 'nb_lines_skip'
        call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) NbLinesSkip = VarI0D

        SubSectionName = SectionName // '>file'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ArrayFile%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
        call ImportArray( File=ArrayFile, Array=Array, NbLinesSkip=NbLinesSkip, RowMajor=RowMajorLoc )
      case('internal')
        ParameterName = 'values'
        call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.true. )
        allocate(Array, source=ConvertToComplexs( String=VarC0D, Separator=' ' ), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArrayInput_String1D( Input, Array, Prefix, RowMajor, Debug )

    class(InputSection_Type), intent(in)                              ::    Input
    type(String_Type), allocatable, dimension(:), intent(out)         ::    Array
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ImportArrayInput_String1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    ArrayFile
    integer                                                           ::    NbLinesSkip=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    integer                                                           ::    i
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    Source = VarC0D

    SectionName = 'source'
    NbLinesSkip = 0

    select case (Source)
      case('external')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        ParameterName = 'nb_lines_skip'
        call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) NbLinesSkip = VarI0D

        SubSectionName = SectionName // '>file'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ArrayFile%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
        call ImportArray( File=ArrayFile, Array=Array, NbLinesSkip=NbLinesSkip, RowMajor=RowMajorLoc )
      case('internal')
        ParameterName = 'values'
        call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.true. )
        call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
        allocate(Array(size(VarC1D,1)), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
        i = 1
        do i = 1, size(VarC1D)
          call Array(i)%Set_Value( Value=trim(adjustl(VarC1D(i))) )
        end do
        deallocate(VarC1D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarC1D', ProcName=ProcName, stat=StatLoc )
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArrayInput_R2D( Input, Array, Prefix, RowMajor, Debug )

    class(InputSection_Type), intent(in)                              ::    Input
    real(rkp), allocatable, dimension(:,:), intent(out)               ::    Array
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ImportArrayInput_R2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    ArrayFile
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbEntries=0
    integer                                                           ::    NbLinesSkip=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    integer                                                           ::    VarI0D
    integer                                                           ::    i, ii
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc
    character(:), allocatable                                         ::    ParamPrefix

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    Source = VarC0D

    SectionName = 'source'
    NbLinesSkip = 0

    select case (Source)
      case('external')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        ParameterName = 'nb_lines_skip'
        call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) NbLinesSkip = VarI0D

        SubSectionName = SectionName // '>file'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ArrayFile%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
        call ImportArray( File=ArrayFile, Array=Array, NbLinesSkip=NbLinesSkip, RowMajor=RowMajorLoc )
      case('internal')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        SubSectionName = SectionName // '>array'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        NbLines = InputSection%GetNumberofParameters()
        if ( NbLines <= 0 ) call Error%Raise( Line='Specified 0 or less columns to be read in', ProcName=ProcName )

        ParamPrefix = 'column'
        if ( RowMajorLoc ) ParamPrefix = 'row'

        i = 1
        do i = 1, NbLines
          ParameterName = ParamPrefix // ConvertToString( Value=i )
          call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
          call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
          if ( i == 1 ) then
            NbEntries = size(VarC1D,1)
            if ( RowMajorLoc ) then
              allocate(Array(NbLines,NbEntries), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
            else
              allocate(Array(NbEntries,NbLines), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
            end if
          end if
          if ( size(VarC1D,1) /= NbEntries ) call Error%Raise(Line='Specified line not equal to the length of the first line',&
                                                             ProcName=ProcName )

          if ( RowMajorLoc ) then
            Array(i,:) = ConvertToReals( Strings=VarC1D )
          else
            Array(:,i) = ConvertToReals( Strings=VarC1D )
          end if

        end do

      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArrayInput_I2D( Input, Array, Prefix, RowMajor, Debug )

    class(InputSection_Type), intent(in)                              ::    Input
    integer, allocatable, dimension(:,:), intent(out)                 ::    Array
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ImportArrayInput_I2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    ArrayFile
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbEntries=0
    integer                                                           ::    NbLinesSkip=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    integer                                                           ::    VarI0D
    integer                                                           ::    i, ii
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc
    character(:), allocatable                                         ::    ParamPrefix

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    Source = VarC0D

    SectionName = 'source'
    NbLinesSkip = 0

    select case (Source)
      case('external')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        ParameterName = 'nb_lines_skip'
        call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) NbLinesSkip = VarI0D

        SubSectionName = SectionName // '>file'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ArrayFile%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
        call ImportArray( File=ArrayFile, Array=Array, NbLinesSkip=NbLinesSkip, RowMajor=RowMajorLoc )
      case('internal')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        SubSectionName = SectionName // '>array'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        NbLines = InputSection%GetNumberofParameters()
        if ( NbLines <= 0 ) call Error%Raise( Line='Specified 0 or less columns to be read in', ProcName=ProcName )

        ParamPrefix = 'column'
        if ( RowMajorLoc ) ParamPrefix = 'row'

        i = 1
        do i = 1, NbLines
          ParameterName = ParamPrefix // ConvertToString( Value=i )
          call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
          call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
          if ( i == 1 ) then
            NbEntries = size(VarC1D,1)
            if ( RowMajorLoc ) then
              allocate(Array(NbLines,NbEntries), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
            else
              allocate(Array(NbEntries,NbLines), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
            end if
          end if
          if ( size(VarC1D,1) /= NbEntries ) call Error%Raise(Line='Specified line not equal to the length of the first line',&
                                                             ProcName=ProcName )
          if ( RowMajorLoc ) then
            Array(i,:) = ConvertToIntegers( Strings=VarC1D )
          else
            Array(:,i) = ConvertToIntegers( Strings=VarC1D )
          end if
        end do

      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArrayInput_I82D( Input, Array, Prefix, RowMajor, Debug )

    class(InputSection_Type), intent(in)                              ::    Input
    integer(8), allocatable, dimension(:,:), intent(out)              ::    Array
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ImportArrayInput_I82D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    ArrayFile
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbEntries=0
    integer                                                           ::    NbLinesSkip=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    integer                                                           ::    VarI0D
    integer                                                           ::    i, ii
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc
    character(:), allocatable                                         ::    ParamPrefix

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    Source = VarC0D

    SectionName = 'source'
    NbLinesSkip = 0

    select case (Source)
      case('external')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        ParameterName = 'nb_lines_skip'
        call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) NbLinesSkip = VarI0D

        SubSectionName = SectionName // '>file'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ArrayFile%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
        call ImportArray( File=ArrayFile, Array=Array, NbLinesSkip=NbLinesSkip, RowMajor=RowMajorLoc )
      case('internal')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        SubSectionName = SectionName // '>array'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        NbLines = InputSection%GetNumberofParameters()
        if ( NbLines <= 0 ) call Error%Raise( Line='Specified 0 or less columns to be read in', ProcName=ProcName )

        ParamPrefix = 'column'
        if ( RowMajorLoc ) ParamPrefix = 'row'

        i = 1
        do i = 1, NbLines
          ParameterName = ParamPrefix // ConvertToString( Value=i )
          call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
          call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
          if ( i == 1 ) then
            NbEntries = size(VarC1D,1)
            if ( RowMajorLoc ) then
              allocate(Array(NbLines,NbEntries), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
            else
              allocate(Array(NbEntries,NbLines), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
            end if
          end if
          if ( size(VarC1D,1) /= NbEntries ) call Error%Raise(Line='Specified line not equal to the length of the first line',&
                                                             ProcName=ProcName )
          if ( RowMajorLoc ) then
            Array(i,:) = ConvertToInteger8s( Strings=VarC1D )
          else
            Array(:,i) = ConvertToInteger8s( Strings=VarC1D )
          end if
        end do

      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArrayInput_C2D( Input, Array, Prefix, RowMajor, Debug )

    class(InputSection_Type), intent(in)                              ::    Input
    character(:), allocatable, dimension(:,:), intent(out)            ::    Array
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ImportArrayInput_C2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    ArrayFile
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbEntries=0
    integer                                                           ::    NbLinesSkip=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    integer                                                           ::    VarI0D
    integer                                                           ::    i, ii
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc
    character(:), allocatable                                         ::    ParamPrefix

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    Source = VarC0D

    SectionName = 'source'
    NbLinesSkip = 0

    select case (Source)
      case('external')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        ParameterName = 'nb_lines_skip'
        call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) NbLinesSkip = VarI0D

        SubSectionName = SectionName // '>file'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ArrayFile%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
        call ImportArray( File=ArrayFile, Array=Array, NbLinesSkip=NbLinesSkip, RowMajor=RowMajorLoc )
      case('internal')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        SubSectionName = SectionName // '>array'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        NbLines = InputSection%GetNumberofParameters()
        if ( NbLines <= 0 ) call Error%Raise( Line='Specified 0 or less columns to be read in', ProcName=ProcName )

        ParamPrefix = 'column'
        if ( RowMajorLoc ) ParamPrefix = 'row'

        i = 1
        do i = 1, NbLines
          ParameterName = ParamPrefix // ConvertToString( Value=i )
          call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
          call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
          if ( i == 1 ) then
            NbEntries = size(VarC1D,1)
            if ( RowMajorLoc ) then
              allocate(character(200) :: Array(NbLines,NbEntries), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
            else
              allocate(character(200) :: Array(NbEntries,NbLines), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
            end if
          end if
          if ( size(VarC1D,1) /= NbEntries ) call Error%Raise(Line='Specified line not equal to the length of the first line',&
                                                             ProcName=ProcName )
          if ( RowMajorLoc ) then
            Array(i,:) = VarC1D
          else
            Array(:,i) = VarC1D
          end if
        end do

      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArrayInput_L2D( Input, Array, Prefix, RowMajor, Debug )

    class(InputSection_Type), intent(in)                              ::    Input
    logical, allocatable, dimension(:,:), intent(out)                 ::    Array
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ImportArrayInput_L2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    ArrayFile
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbEntries=0
    integer                                                           ::    NbLinesSkip=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    integer                                                           ::    VarI0D
    integer                                                           ::    i, ii
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc
    character(:), allocatable                                         ::    ParamPrefix

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    Source = VarC0D

    SectionName = 'source'
    NbLinesSkip = 0

    select case (Source)
      case('external')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        ParameterName = 'nb_lines_skip'
        call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) NbLinesSkip = VarI0D

        SubSectionName = SectionName // '>file'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ArrayFile%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
        call ImportArray( File=ArrayFile, Array=Array, NbLinesSkip=NbLinesSkip, RowMajor=RowMajorLoc )
      case('internal')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        SubSectionName = SectionName // '>array'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        NbLines = InputSection%GetNumberofParameters()
        if ( NbLines <= 0 ) call Error%Raise( Line='Specified 0 or less columns to be read in', ProcName=ProcName )

        ParamPrefix = 'column'
        if ( RowMajorLoc ) ParamPrefix = 'row'

        i = 1
        do i = 1, NbLines
          ParameterName = ParamPrefix // ConvertToString( Value=i )
          call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
          call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
          if ( i == 1 ) then
            NbEntries = size(VarC1D,1)
            if ( RowMajorLoc ) then
              allocate(Array(NbLines,NbEntries), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
            else
              allocate(Array(NbEntries,NbLines), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
            end if
          end if
          if ( size(VarC1D,1) /= NbEntries ) call Error%Raise(Line='Specified line not equal to the length of the first line',&
                                                             ProcName=ProcName )
          if ( RowMajorLoc ) then
            Array(i,:) = ConvertToLogicals( Strings=VarC1D )
          else
            Array(:,i) = ConvertToLogicals( Strings=VarC1D )
          end if
        end do

      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArrayInput_CX2D( Input, Array, Prefix, RowMajor, Debug )

    class(InputSection_Type), intent(in)                              ::    Input
    complex, allocatable, dimension(:,:), intent(out)                 ::    Array
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ImportArrayInput_CX2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    ArrayFile
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbEntries=0
    integer                                                           ::    NbLinesSkip=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    integer                                                           ::    VarI0D
    integer                                                           ::    i, ii
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc
    character(:), allocatable                                         ::    ParamPrefix

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    Source = VarC0D

    SectionName = 'source'
    NbLinesSkip = 0

    select case (Source)
      case('external')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        ParameterName = 'nb_lines_skip'
        call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) NbLinesSkip = VarI0D

        SubSectionName = SectionName // '>file'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ArrayFile%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
        call ImportArray( File=ArrayFile, Array=Array, NbLinesSkip=NbLinesSkip, RowMajor=RowMajorLoc )
      case('internal')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        SubSectionName = SectionName // '>array'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        NbLines = InputSection%GetNumberofParameters()
        if ( NbLines <= 0 ) call Error%Raise( Line='Specified 0 or less columns to be read in', ProcName=ProcName )

        ParamPrefix = 'column'
        if ( RowMajorLoc ) ParamPrefix = 'row'

        i = 1
        do i = 1, NbLines
          ParameterName = ParamPrefix // ConvertToString( Value=i )
          call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
          call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
          if ( i == 1 ) then
            NbEntries = size(VarC1D,1)
            if ( RowMajorLoc ) then
              allocate(Array(NbLines,NbEntries), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
            else
              allocate(Array(NbEntries,NbLines), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
            end if
          end if
          if ( size(VarC1D,1) /= NbEntries ) call Error%Raise(Line='Specified line not equal to the length of the first line',&
                                                             ProcName=ProcName )
          if ( RowMajorLoc ) then
            Array(i,:) = ConvertToComplexs( Strings=VarC1D )
          else
            Array(:,i) = ConvertToComplexs( Strings=VarC1D )
          end if
        end do
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArrayInput_String2D( Input, Array, Prefix, RowMajor, Debug )

    class(InputSection_Type), intent(in)                              ::    Input
    type(String_Type), allocatable, dimension(:,:), intent(out)       ::    Array
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ImportArrayInput_String2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    ArrayFile
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbEntries=0
    integer                                                           ::    NbLinesSkip=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    integer                                                           ::    VarI0D
    integer                                                           ::    i, ii
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc
    character(:), allocatable                                         ::    ParamPrefix

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    Source = VarC0D

    SectionName = 'source'
    NbLinesSkip = 0

    select case (Source)
      case('external')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        ParameterName = 'nb_lines_skip'
        call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) NbLinesSkip = VarI0D

        SubSectionName = SectionName // '>file'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ArrayFile%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
        call ImportArray( File=ArrayFile, Array=Array, NbLinesSkip=NbLinesSkip, RowMajor=RowMajorLoc )
      case('internal')
        ParameterName = 'row_major'
        call Input%GetValue( Value=VarL0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) RowMajorLoc = VarL0D

        SubSectionName = SectionName // '>array'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        NbLines = InputSection%GetNumberofParameters()
        if ( NbLines <= 0 ) call Error%Raise( Line='Specified 0 or less columns to be read in', ProcName=ProcName )

        ParamPrefix = 'column'
        if ( RowMajorLoc ) ParamPrefix = 'row'

        i = 1
        do i = 1, NbLines
          ParameterName = ParamPrefix // ConvertToString( Value=i )
          call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
          call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
          if ( i == 1 ) then
            NbEntries = size(VarC1D,1)
            if ( RowMajorLoc ) then
              allocate(Array(NbLines,NbEntries), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
            else
              allocate(Array(NbEntries,NbLines), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
            end if
          end if
          if ( size(VarC1D,1) /= NbEntries ) call Error%Raise(Line='Specified line not equal to the length of the first line',&
                                                             ProcName=ProcName )
          if ( RowMajorLoc ) then
            ii = 1
            do ii = 1, NbEntries
              Array(i,ii) = trim(adjustl(VarC1D(ii)))
            end do
          else
            ii = 1
            do ii = 1, NbEntries
              Array(ii,i) = trim(adjustl(VarC1D(ii)))
            end do
          end if

        end do

      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArray_R1D( File, Array, NbLinesSkip, Mandatory, Found, RowMajor, Debug )

    type(SMUQFile_Type), intent(inout)                                ::    File
    real(rkp), dimension(:), allocatable, intent(out)                 ::    Array
    integer, optional, intent(in)                                     ::    NbLinesSkip
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ImportArray_R1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    logical                                                           ::    MandatoryLoc=.true.
    logical                                                           ::    FoundLoc=.false.
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbLinesSkipLoc=0
    integer                                                           ::    Size1=0
    integer                                                           ::    i, ii
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    Comment
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc
    character(:), allocatable, dimension(:)                           ::    VarC1D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    if (present(Mandatory)) MandatoryLoc = Mandatory

    call File%Open( Unit=UnitLoc, Action='read', Status='old', Position='rewind', Mandatory=MandatoryLoc, Found=FoundLoc )

    if ( FoundLoc ) then
      NbLines = File%GetNbLines()
      Comment = File%GetComment()
      if ( present(NbLinesSkip) ) NbLinesSkipLoc = NbLinesSkip

      ii = 0
      i = 1
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
      end do

      if ( RowMajorLoc ) then
        Size1 = ii
        if ( Size1 <= 0 ) call Error%Raise( Line='File was found to contain no usable lines', ProcName=ProcName )
        call File%Rewind()
        allocate(Array(Size1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
        i = 1
        ii = 0
        do i = 1, NbLines
          call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
          if ( i <= NbLinesSkipLoc ) cycle
          if ( VarC0D(1:len(Comment)) == Comment ) cycle
          ii = ii + 1
          Array(ii) = ConvertToReal( String=VarC0D )
        end do
      else
        if ( ii /= 1 ) call Error%Raise( Line='Only one line can specify the array to be read in column wise', ProcName=ProcName )
        call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
        allocate(Array(size(VarC1D,1)), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
        Array = ConvertToReals(Strings=VarC1D)
        deallocate(VarC1D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarC1D', ProcName=ProcName, stat=StatLoc )
      end if

    end if

    call File%Close()

    if ( present(Found) ) Found = FoundLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArray_I1D( File, Array, NbLinesSkip, Mandatory, Found, RowMajor, Debug )

    type(SMUQFile_Type), intent(inout)                                ::    File
    integer, dimension(:), allocatable, intent(out)                   ::    Array
    integer, optional, intent(in)                                     ::    NbLinesSkip
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ImportArray_I1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    logical                                                           ::    MandatoryLoc=.true.
    logical                                                           ::    FoundLoc=.false.
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbLinesSkipLoc=0
    integer                                                           ::    Size1=0
    integer                                                           ::    i, ii
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    Comment
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc
    character(:), allocatable, dimension(:)                           ::    VarC1D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    if (present(Mandatory)) MandatoryLoc = Mandatory

    call File%Open( Unit=UnitLoc, Action='read', Status='old', Position='rewind', Mandatory=MandatoryLoc, Found=FoundLoc )

    if ( FoundLoc ) then
      NbLines = File%GetNbLines()
      Comment = File%GetComment()
      if ( present(NbLinesSkip) ) NbLinesSkipLoc = NbLinesSkip

      ii = 0
      i = 1
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
      end do

      if ( RowMajorLoc ) then
        Size1 = ii
        if ( Size1 <= 0 ) call Error%Raise( Line='File was found to contain no usable lines', ProcName=ProcName )
        call File%Rewind()
        allocate(Array(Size1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
        i = 1
        ii = 0
        do i = 1, NbLines
          call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
          if ( i <= NbLinesSkipLoc ) cycle
          if ( VarC0D(1:len(Comment)) == Comment ) cycle
          ii = ii + 1
          Array(ii) = ConvertToInteger( String=VarC0D )
        end do
      else
        if ( ii /= 1 ) call Error%Raise( Line='Only one line can specify the array to be read in column wise', ProcName=ProcName )
        call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
        allocate(Array(size(VarC1D,1)), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
        Array = ConvertToIntegers(Strings=VarC1D)
        deallocate(VarC1D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarC1D', ProcName=ProcName, stat=StatLoc )
      end if

    end if

    call File%Close()

    if ( present(Found) ) Found = FoundLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArray_I81D( File, Array, NbLinesSkip, Mandatory, Found, RowMajor, Debug )

    type(SMUQFile_Type), intent(inout)                                ::    File
    integer(8), dimension(:), allocatable, intent(out)                ::    Array
    integer, optional, intent(in)                                     ::    NbLinesSkip
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ImportArray_I81D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    logical                                                           ::    MandatoryLoc=.true.
    logical                                                           ::    FoundLoc=.false.
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbLinesSkipLoc=0
    integer                                                           ::    Size1=0
    integer                                                           ::    i, ii
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    Comment
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc
    character(:), allocatable, dimension(:)                           ::    VarC1D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    if (present(Mandatory)) MandatoryLoc = Mandatory

    call File%Open( Unit=UnitLoc, Action='read', Status='old', Position='rewind', Mandatory=MandatoryLoc, Found=FoundLoc )

    if ( FoundLoc ) then
      NbLines = File%GetNbLines()
      Comment = File%GetComment()
      if ( present(NbLinesSkip) ) NbLinesSkipLoc = NbLinesSkip

      ii = 0
      i = 1
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
      end do

      if ( RowMajorLoc ) then
        Size1 = ii
        if ( Size1 <= 0 ) call Error%Raise( Line='File was found to contain no usable lines', ProcName=ProcName )
        call File%Rewind()
        allocate(Array(Size1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
        i = 1
        ii = 0
        do i = 1, NbLines
          call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
          if ( i <= NbLinesSkipLoc ) cycle
          if ( VarC0D(1:len(Comment)) == Comment ) cycle
          ii = ii + 1
          Array(ii) = ConvertToInteger( String=VarC0D )
        end do
      else
        if ( ii /= 1 ) call Error%Raise( Line='Only one line can specify the array to be read in column wise', ProcName=ProcName )
        call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
        allocate(Array(size(VarC1D,1)), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
        Array = ConvertToInteger8s(Strings=VarC1D)
        deallocate(VarC1D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarC1D', ProcName=ProcName, stat=StatLoc )
      end if

    end if

    call File%Close()

    if ( present(Found) ) Found = FoundLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArray_C1D( File, Array, NbLinesSkip, Mandatory, Found, RowMajor, Debug )

    type(SMUQFile_Type), intent(inout)                                ::    File
    character(:), dimension(:), allocatable, intent(out)              ::    Array
    integer, optional, intent(in)                                     ::    NbLinesSkip
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ImportArray_C1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    logical                                                           ::    MandatoryLoc=.true.
    logical                                                           ::    FoundLoc=.false.
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbLinesSkipLoc=0
    integer                                                           ::    Size1=0
    integer                                                           ::    i, ii
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    Comment
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc
    character(:), allocatable, dimension(:)                           ::    VarC1D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    if (present(Mandatory)) MandatoryLoc = Mandatory

    call File%Open( Unit=UnitLoc, Action='read', Status='old', Position='rewind', Mandatory=MandatoryLoc, Found=FoundLoc )

    if ( FoundLoc ) then
      NbLines = File%GetNbLines()
      Comment = File%GetComment()
      if ( present(NbLinesSkip) ) NbLinesSkipLoc = NbLinesSkip

      ii = 0
      i = 1
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
      end do

      if ( RowMajorLoc ) then
        Size1 = ii
        if ( Size1 <= 0 ) call Error%Raise( Line='File was found to contain no usable lines', ProcName=ProcName )
        call File%Rewind()
        allocate(character(200) :: Array(Size1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
        i = 1
        ii = 0
        do i = 1, NbLines
          call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
          if ( i <= NbLinesSkipLoc ) cycle
          if ( VarC0D(1:len(Comment)) == Comment ) cycle
          ii = ii + 1
          Array(ii) = VarC0D
        end do
      else
        if ( ii /= 1 ) call Error%Raise( Line='Only one line can specify the array to be read in column wise', ProcName=ProcName )
        call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
        allocate(character(200) :: Array(size(VarC1D,1)), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
        Array = VarC1D
        deallocate(VarC1D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarC1D', ProcName=ProcName, stat=StatLoc )
      end if

    end if

    call File%Close()

    if ( present(Found) ) Found = FoundLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArray_L1D( File, Array, NbLinesSkip, Mandatory, Found, RowMajor, Debug )

    type(SMUQFile_Type), intent(inout)                                ::    File
    logical, dimension(:), allocatable, intent(out)                   ::    Array
    integer, optional, intent(in)                                     ::    NbLinesSkip
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ImportArray_L1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    logical                                                           ::    MandatoryLoc=.true.
    logical                                                           ::    FoundLoc=.false.
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbLinesSkipLoc=0
    integer                                                           ::    Size1=0
    integer                                                           ::    i, ii
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    Comment
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc
    character(:), allocatable, dimension(:)                           ::    VarC1D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    if (present(Mandatory)) MandatoryLoc = Mandatory

    call File%Open( Unit=UnitLoc, Action='read', Status='old', Position='rewind', Mandatory=MandatoryLoc, Found=FoundLoc )

    if ( FoundLoc ) then
      NbLines = File%GetNbLines()
      Comment = File%GetComment()
      if ( present(NbLinesSkip) ) NbLinesSkipLoc = NbLinesSkip

      ii = 0
      i = 1
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
      end do

      if ( RowMajorLoc ) then
        Size1 = ii
        if ( Size1 <= 0 ) call Error%Raise( Line='File was found to contain no usable lines', ProcName=ProcName )
        call File%Rewind()
        allocate(Array(Size1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
        i = 1
        ii = 0
        do i = 1, NbLines
          call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
          if ( i <= NbLinesSkipLoc ) cycle
          if ( VarC0D(1:len(Comment)) == Comment ) cycle
          ii = ii + 1
          Array(ii) = ConvertToLogical( String=VarC0D )
        end do
      else
        if ( ii /= 1 ) call Error%Raise( Line='Only one line can specify the array to be read in column wise', ProcName=ProcName )
        call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
        allocate(Array(size(VarC1D,1)), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
        Array = ConvertToLogicals(Strings=VarC1D)
        deallocate(VarC1D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarC1D', ProcName=ProcName, stat=StatLoc )
      end if

    end if

    call File%Close()

    if ( present(Found) ) Found = FoundLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArray_CX1D( File, Array, NbLinesSkip, Mandatory, Found, RowMajor, Debug )

    type(SMUQFile_Type), intent(inout)                                ::    File
    complex, dimension(:), allocatable, intent(out)                   ::    Array
    integer, optional, intent(in)                                     ::    NbLinesSkip
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ImportArray_CX1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    logical                                                           ::    MandatoryLoc=.true.
    logical                                                           ::    FoundLoc=.false.
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbLinesSkipLoc=0
    integer                                                           ::    Size1=0
    integer                                                           ::    i, ii
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    Comment
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc
    character(:), allocatable, dimension(:)                           ::    VarC1D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    if (present(Mandatory)) MandatoryLoc = Mandatory

    call File%Open( Unit=UnitLoc, Action='read', Status='old', Position='rewind', Mandatory=MandatoryLoc, Found=FoundLoc )

    if ( FoundLoc ) then
      NbLines = File%GetNbLines()
      Comment = File%GetComment()
      if ( present(NbLinesSkip) ) NbLinesSkipLoc = NbLinesSkip

      ii = 0
      i = 1
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
      end do

      if ( RowMajorLoc ) then
        Size1 = ii
        if ( Size1 <= 0 ) call Error%Raise( Line='File was found to contain no usable lines', ProcName=ProcName )
        call File%Rewind()
        allocate(Array(Size1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
        i = 1
        ii = 0
        do i = 1, NbLines
          call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
          if ( i <= NbLinesSkipLoc ) cycle
          if ( VarC0D(1:len(Comment)) == Comment ) cycle
          ii = ii + 1
          Array(ii) = ConvertToComplex( String=VarC0D )
        end do
      else
        if ( ii /= 1 ) call Error%Raise( Line='Only one line can specify the array to be read in column wise', ProcName=ProcName )
        call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
        allocate(Array(size(VarC1D,1)), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
        Array = ConvertToComplexs(Strings=VarC1D)
        deallocate(VarC1D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarC1D', ProcName=ProcName, stat=StatLoc )
      end if

    end if

    call File%Close()

    if ( present(Found) ) Found = FoundLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArray_String1D( File, Array, NbLinesSkip, Mandatory, Found, RowMajor, Debug )

    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:), allocatable, intent(out)         ::    Array
    integer, optional, intent(in)                                     ::    NbLinesSkip
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ImportArray_String1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    logical                                                           ::    MandatoryLoc=.true.
    logical                                                           ::    FoundLoc=.false.
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbLinesSkipLoc=0
    integer                                                           ::    Size1=0
    integer                                                           ::    i, ii
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    Comment
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc
    character(:), allocatable, dimension(:)                           ::    VarC1D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    if (present(Mandatory)) MandatoryLoc = Mandatory

    call File%Open( Unit=UnitLoc, Action='read', Status='old', Position='rewind', Mandatory=MandatoryLoc, Found=FoundLoc )

    if ( FoundLoc ) then
      NbLines = File%GetNbLines()
      Comment = File%GetComment()
      if ( present(NbLinesSkip) ) NbLinesSkipLoc = NbLinesSkip

      ii = 0
      i = 1
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
      end do

      if ( RowMajorLoc ) then
        Size1 = ii
        if ( Size1 <= 0 ) call Error%Raise( Line='File was found to contain no usable lines', ProcName=ProcName )
        call File%Rewind()
        allocate(Array(Size1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
        i = 1
        ii = 0
        do i = 1, NbLines
          call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
          if ( i <= NbLinesSkipLoc ) cycle
          if ( VarC0D(1:len(Comment)) == Comment ) cycle
          ii = ii + 1
          Array(ii) = VarC0D
        end do
      else
        if ( ii /= 1 ) call Error%Raise( Line='Only one line can specify the array to be read in column wise', ProcName=ProcName )
        call Parse( Input=VarC0D, Separator=' ', Output=VarC1D )
        allocate(Array(size(VarC1D,1)), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
        Array = VarC1D
        deallocate(VarC1D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarC1D', ProcName=ProcName, stat=StatLoc )
      end if

    end if

    call File%Close()

    if ( present(Found) ) Found = FoundLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArray_R2D( File, Array, NbLinesSkip, Mandatory, Found, RowMajor, Debug )

    type(SMUQFile_Type), intent(inout)                                ::    File
    real(rkp), dimension(:,:), allocatable, intent(out)               ::    Array
    integer, optional, intent(in)                                     ::    NbLinesSkip
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ImportArray_R2D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    logical                                                           ::    MandatoryLoc=.true.
    logical                                                           ::    FoundLoc=.false.
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbLinesSkipLoc=0
    integer                                                           ::    NbEntries=0
    integer                                                           ::    Size1=0
    integer                                                           ::    Size2=0
    integer                                                           ::    i, ii, iii
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    character(:), allocatable                                         ::    Comment
    character(:), allocatable                                         ::    Separator
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    if (present(Mandatory)) MandatoryLoc = Mandatory

    call File%Open( Unit=UnitLoc, Action='read', Status='old', Position='rewind', Mandatory=MandatoryLoc, Found=FoundLoc )

    if ( FoundLoc ) then
      NbLines = File%GetNbLines()
      Comment = File%GetComment()
      Separator = File%GetSeparator()
      if ( present(NbLinesSkip) ) NbLinesSkipLoc = NbLinesSkip

      ii = 0
      i = 1
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
        if ( ii == 1 ) then
          call Parse( Input=VarC0D, Separator=Separator, Output=VarC1D )
          Size1 = size(VarC1D,1)
        end if
      end do
      Size2 = ii
      if ( Size1 <= 0 ) call Error%Raise( Line='File was found to contain no usable lines', ProcName=ProcName )

      call File%Rewind()
      
      if ( RowMajorLoc ) then
        allocate(Array(Size2,Size1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      else
        allocate(Array(Size1,Size2), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      end if

      i = 1
      ii = 0
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
        call Parse( Input=VarC0D, Separator=Separator, Output=VarC1D )
        if ( size(VarC1D) /= Size1 ) call Error%Raise( Line='Number of entries mismatch in the line', ProcName=ProcName )
        if ( RowMajorLoc ) then
          Array(ii,:) = ConvertToReals( Strings=VarC1D )
        else
          Array(:,ii) = ConvertToReals( Strings=VarC1D )
        end if
      end do

    end if

    call File%Close()

    if ( present(Found) ) Found = FoundLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArray_I2D( File, Array, NbLinesSkip, Mandatory, Found, RowMajor, Debug )

    type(SMUQFile_Type), intent(inout)                                ::    File
    integer, dimension(:,:), allocatable, intent(out)                 ::    Array
    integer, optional, intent(in)                                     ::    NbLinesSkip
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ImportArray_I2D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    logical                                                           ::    MandatoryLoc=.true.
    logical                                                           ::    FoundLoc=.false.
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbLinesSkipLoc=0
    integer                                                           ::    NbEntries=0
    integer                                                           ::    Size1=0
    integer                                                           ::    Size2=0
    integer                                                           ::    i, ii, iii
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    character(:), allocatable                                         ::    Comment
    character(:), allocatable                                         ::    Separator
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    if (present(Mandatory)) MandatoryLoc = Mandatory

    call File%Open( Unit=UnitLoc, Action='read', Status='old', Position='rewind', Mandatory=MandatoryLoc, Found=FoundLoc )

    if ( FoundLoc ) then
      NbLines = File%GetNbLines()
      Comment = File%GetComment()
      Separator = File%GetSeparator()
      if ( present(NbLinesSkip) ) NbLinesSkipLoc = NbLinesSkip

      ii = 0
      i = 1
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
        if ( ii == 1 ) then
          call Parse( Input=VarC0D, Separator=Separator, Output=VarC1D )
          Size1 = size(VarC1D,1)
        end if
      end do
      Size2 = ii
      if ( Size1 <= 0 ) call Error%Raise( Line='File was found to contain no usable lines', ProcName=ProcName )

      call File%Rewind()

      if ( RowMajorLoc ) then
        allocate(Array(Size2,Size1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      else
        allocate(Array(Size1,Size2), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      end if

      i = 1
      ii = 0
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
        call Parse( Input=VarC0D, Separator=Separator, Output=VarC1D )
        if ( size(VarC1D) /= Size1 ) call Error%Raise( Line='Number of entries mismatch in the line', ProcName=ProcName )
        if ( RowMajorLoc ) then
          Array(ii,:) = ConvertToIntegers( Strings=VarC1D )
        else
          Array(:,ii) = ConvertToIntegers( Strings=VarC1D )
        end if
      end do

    end if

    call File%Close()

    if ( present(Found) ) Found = FoundLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArray_I82D( File, Array, NbLinesSkip, Mandatory, Found, RowMajor, Debug )

    type(SMUQFile_Type), intent(inout)                                ::    File
    integer(8), dimension(:,:), allocatable, intent(out)              ::    Array
    integer, optional, intent(in)                                     ::    NbLinesSkip
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ImportArray_I82D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    logical                                                           ::    MandatoryLoc=.true.
    logical                                                           ::    FoundLoc=.false.
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbLinesSkipLoc=0
    integer                                                           ::    NbEntries=0
    integer                                                           ::    Size1=0
    integer                                                           ::    Size2=0
    integer                                                           ::    i, ii, iii
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    character(:), allocatable                                         ::    Comment
    character(:), allocatable                                         ::    Separator
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    if (present(Mandatory)) MandatoryLoc = Mandatory

    call File%Open( Unit=UnitLoc, Action='read', Status='old', Position='rewind', Mandatory=MandatoryLoc, Found=FoundLoc )

    if ( FoundLoc ) then
      NbLines = File%GetNbLines()
      Comment = File%GetComment()
      Separator = File%GetSeparator()
      if ( present(NbLinesSkip) ) NbLinesSkipLoc = NbLinesSkip

      ii = 0
      i = 1
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
        if ( ii == 1 ) then
          call Parse( Input=VarC0D, Separator=Separator, Output=VarC1D )
          Size1 = size(VarC1D,1)
        end if
      end do
      Size2 = ii
      if ( Size1 <= 0 ) call Error%Raise( Line='File was found to contain no usable lines', ProcName=ProcName )

      call File%Rewind()

      if ( RowMajorLoc ) then
        allocate(Array(Size2,Size1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      else
        allocate(Array(Size1,Size2), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      end if

      i = 1
      ii = 0
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
        call Parse( Input=VarC0D, Separator=Separator, Output=VarC1D )
        if ( size(VarC1D) /= Size1 ) call Error%Raise( Line='Number of entries mismatch in the line', ProcName=ProcName )
        if ( RowMajorLoc ) then
          Array(ii,:) = ConvertToInteger8s( Strings=VarC1D )
        else
          Array(:,ii) = ConvertToInteger8s( Strings=VarC1D )
        end if
      end do

    end if

    call File%Close()

    if ( present(Found) ) Found = FoundLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArray_C2D( File, Array, NbLinesSkip, Mandatory, Found, RowMajor, Debug )

    type(SMUQFile_Type), intent(inout)                                ::    File
    character(:), dimension(:,:), allocatable, intent(out)            ::    Array
    integer, optional, intent(in)                                     ::    NbLinesSkip
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ImportArray_C2D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    logical                                                           ::    MandatoryLoc=.true.
    logical                                                           ::    FoundLoc=.false.
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbLinesSkipLoc=0
    integer                                                           ::    NbEntries=0
    integer                                                           ::    Size1=0
    integer                                                           ::    Size2=0
    integer                                                           ::    i, ii, iii
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    character(:), allocatable                                         ::    Comment
    character(:), allocatable                                         ::    Separator
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    if (present(Mandatory)) MandatoryLoc = Mandatory

    call File%Open( Unit=UnitLoc, Action='read', Status='old', Position='rewind', Mandatory=MandatoryLoc, Found=FoundLoc )

    if ( FoundLoc ) then
      NbLines = File%GetNbLines()
      Comment = File%GetComment()
      Separator = File%GetSeparator()
      if ( present(NbLinesSkip) ) NbLinesSkipLoc = NbLinesSkip

      ii = 0
      i = 1
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
        if ( ii == 1 ) then
          call Parse( Input=VarC0D, Separator=Separator, Output=VarC1D )
          Size1 = size(VarC1D,1)
        end if
      end do
      Size2 = ii
      if ( Size1 <= 0 ) call Error%Raise( Line='File was found to contain no usable lines', ProcName=ProcName )

      call File%Rewind()

      if ( RowMajorLoc ) then
        allocate(character(200) :: Array(Size2,Size1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      else
        allocate(character(200) :: Array(Size1,Size2), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      end if

      i = 1
      ii = 0
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
        call Parse( Input=VarC0D, Separator=Separator, Output=VarC1D )
        if ( RowMajorLoc ) then
          Array(ii,:) = VarC1D
        else
          Array(:,ii) = VarC1D
        end if
      end do

    end if

    call File%Close()

    if ( present(Found) ) Found = FoundLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArray_L2D( File, Array, NbLinesSkip, Mandatory, Found, RowMajor, Debug )

    type(SMUQFile_Type), intent(inout)                                ::    File
    logical, dimension(:,:), allocatable, intent(out)                 ::    Array
    integer, optional, intent(in)                                     ::    NbLinesSkip
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ImportArray_L2D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    logical                                                           ::    MandatoryLoc=.true.
    logical                                                           ::    FoundLoc=.false.
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbLinesSkipLoc=0
    integer                                                           ::    NbEntries=0
    integer                                                           ::    Size1=0
    integer                                                           ::    Size2=0
    integer                                                           ::    i, ii, iii
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    character(:), allocatable                                         ::    Comment
    character(:), allocatable                                         ::    Separator
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    if (present(Mandatory)) MandatoryLoc = Mandatory

    call File%Open( Unit=UnitLoc, Action='read', Status='old', Position='rewind', Mandatory=MandatoryLoc, Found=FoundLoc )

    if ( FoundLoc ) then
      NbLines = File%GetNbLines()
      Comment = File%GetComment()
      Separator = File%GetSeparator()
      if ( present(NbLinesSkip) ) NbLinesSkipLoc = NbLinesSkip

      ii = 0
      i = 1
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
        if ( ii == 1 ) then
          call Parse( Input=VarC0D, Separator=Separator, Output=VarC1D )
          Size1 = size(VarC1D,1)
        end if
      end do
      Size2 = ii
      if ( Size1 <= 0 ) call Error%Raise( Line='File was found to contain no usable lines', ProcName=ProcName )

      call File%Rewind()

      if ( RowMajorLoc ) then
        allocate(Array(Size2,Size1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      else
        allocate(Array(Size1,Size2), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      end if

      i = 1
      ii = 0
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
        call Parse( Input=VarC0D, Separator=Separator, Output=VarC1D )
        if ( size(VarC1D) /= Size1 ) call Error%Raise( Line='Number of entries mismatch in the line', ProcName=ProcName )
        if ( RowMajorLoc ) then
          Array(ii,:) = ConvertToLogicals( Strings=VarC1D )
        else
          Array(:,ii) = ConvertToLogicals( Strings=VarC1D )
        end if
      end do

    end if

    call File%Close()

    if ( present(Found) ) Found = FoundLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArray_CX2D( File, Array, NbLinesSkip, Mandatory, Found, RowMajor, Debug )

    type(SMUQFile_Type), intent(inout)                                ::    File
    complex, dimension(:,:), allocatable, intent(out)                 ::    Array
    integer, optional, intent(in)                                     ::    NbLinesSkip
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ImportArray_CX2D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    logical                                                           ::    MandatoryLoc=.true.
    logical                                                           ::    FoundLoc=.false.
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbLinesSkipLoc=0
    integer                                                           ::    NbEntries=0
    integer                                                           ::    Size1=0
    integer                                                           ::    Size2=0
    integer                                                           ::    i, ii, iii
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    character(:), allocatable                                         ::    Comment
    character(:), allocatable                                         ::    Separator
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    if (present(Mandatory)) MandatoryLoc = Mandatory

    call File%Open( Unit=UnitLoc, Action='read', Status='old', Position='rewind', Mandatory=MandatoryLoc, Found=FoundLoc )

    if ( FoundLoc ) then
      NbLines = File%GetNbLines()
      Comment = File%GetComment()
      Separator = File%GetSeparator()
      if ( present(NbLinesSkip) ) NbLinesSkipLoc = NbLinesSkip

      ii = 0
      i = 1
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
        if ( ii == 1 ) then
          call Parse( Input=VarC0D, Separator=Separator, Output=VarC1D )
          Size1 = size(VarC1D,1)
        end if
      end do
      Size2 = ii
      if ( Size1 <= 0 ) call Error%Raise( Line='File was found to contain no usable lines', ProcName=ProcName )

      call File%Rewind()

      if ( RowMajorLoc ) then
        allocate(Array(Size2,Size1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      else
        allocate(Array(Size1,Size2), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      end if

      i = 1
      ii = 0
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
        call Parse( Input=VarC0D, Separator=Separator, Output=VarC1D )
        if ( size(VarC1D) /= Size1 ) call Error%Raise( Line='Number of entries mismatch in the line', ProcName=ProcName )
        if ( RowMajorLoc ) then
          Array(ii,:) = ConvertToComplexs( Strings=VarC1D )
        else
          Array(:,ii) = ConvertToComplexs( Strings=VarC1D )
        end if
      end do

    end if

    call File%Close()

    if ( present(Found) ) Found = FoundLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportArray_String2D( File, Array, NbLinesSkip, Mandatory, Found, RowMajor, Debug )

    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:,:), allocatable, intent(out)       ::    Array
    integer, optional, intent(in)                                     ::    NbLinesSkip
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ImportArray_String2D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    UnitLoc
    logical                                                           ::    MandatoryLoc=.true.
    logical                                                           ::    FoundLoc=.false.
    integer                                                           ::    NbLines=0
    integer                                                           ::    NbLinesSkipLoc=0
    integer                                                           ::    NbEntries=0
    integer                                                           ::    Size1=0
    integer                                                           ::    Size2=0
    integer                                                           ::    i, ii, iii
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable, dimension(:)                           ::    VarC1D
    character(:), allocatable                                         ::    Comment
    character(:), allocatable                                         ::    Separator
    logical                                                           ::    VarL0D
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    if (present(Mandatory)) MandatoryLoc = Mandatory

    call File%Open( Unit=UnitLoc, Action='read', Status='old', Position='rewind', Mandatory=MandatoryLoc, Found=FoundLoc )

    if ( FoundLoc ) then
      NbLines = File%GetNbLines()
      Comment = File%GetComment()
      Separator = File%GetSeparator()
      if ( present(NbLinesSkip) ) NbLinesSkipLoc = NbLinesSkip

      ii = 0
      i = 1
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
        if ( ii == 1 ) then
          call Parse( Input=VarC0D, Separator=Separator, Output=VarC1D )
          Size1 = size(VarC1D,1)
        end if
      end do
      Size2 = ii
      if ( Size1 <= 0 ) call Error%Raise( Line='File was found to contain no usable lines', ProcName=ProcName )

      call File%Rewind()

      if ( RowMajorLoc ) then
        allocate(Array(Size2,Size1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      else
        allocate(Array(Size1,Size2), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Array', ProcName=ProcName, stat=StatLoc )
      end if

      i = 1
      ii = 0
      do i = 1, NbLines
        call File%ReadRecord( Unit=UnitLoc, Record=VarC0D )
        if ( i <= NbLinesSkipLoc ) cycle
        if ( VarC0D(1:len(Comment)) == Comment ) cycle
        ii = ii + 1
        call Parse( Input=VarC0D, Separator=Separator, Output=VarC1D )
        if ( size(VarC1D) /= Size1 ) call Error%Raise( Line='Number of entries mismatch in the line', ProcName=ProcName )
        if ( RowMajorLoc ) then
          iii = 1
          do iii = 1, Size1
            Array(ii,iii) = trim(adjustl(VarC1D(iii)))
          end do
        else
          iii = 1
          do iii = 1, Size1
            Array(iii,ii) = trim(adjustl(VarC1D(iii)))
          end do
        end if
      end do

    end if

    call File%Close()

    if ( present(Found) ) Found = FoundLoc

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArrayInput_R1D( Input, Array, File, Format, RowMajor, Debug )

    class(InputSection_Type), intent(inout)                           ::    Input
    real(rkp), dimension(:), intent(in)                               ::    Array
    type(SMUQFile_Type), optional, intent(inout)                      ::    File
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArrayInput_R1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    FormatLoc
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    Source = 'internal'
    if ( present(File) ) Source = 'external'
    call Input%AddParameter( Name='source', Value=Source )

    SectionName = 'source'
    call Input%AddSection( SectionName=SectionName )

    select case (Source)
      case('external')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        call Input%AddSection( Section=File%GetInput(MainSectionName='file'), To_SubSection=SectionName )
        call ExportArray( Array=Array, File=File, Format=FormatLoc, RowMajor=RowMajorLoc )
      case('internal')
        call Input%AddParameter( Name='values', Value=ConvertToString(Values=Array,Format=FormatLoc, Separator=' '),              &
                                                                                                         SectionName=SectionName )
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArrayInput_I1D( Input, Array, File, Format, RowMajor, Debug )

    class(InputSection_Type), intent(inout)                           ::    Input
    integer, dimension(:), intent(in)                                 ::    Array
    type(SMUQFile_Type), optional, intent(inout)                      ::    File
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArrayInput_I1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    FormatLoc
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    Source = 'internal'
    if ( present(File) ) Source = 'external'
    call Input%AddParameter( Name='source', Value=Source )

    SectionName = 'source'
    call Input%AddSection( SectionName=SectionName )

    select case (Source)
      case('external')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        call Input%AddSection( Section=File%GetInput(MainSectionName='file'), To_SubSection=SectionName )
        call ExportArray( Array=Array, File=File, Format=FormatLoc, RowMajor=RowMajorLoc )
      case('internal')
        call Input%AddParameter( Name='values', Value=ConvertToString(Values=Array,Format=FormatLoc, Separator=' '),              &
                                                                                                         SectionName=SectionName )
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArrayInput_I81D( Input, Array, File, Format, RowMajor, Debug )

    class(InputSection_Type), intent(inout)                           ::    Input
    integer(8), dimension(:), intent(in)                              ::    Array
    type(SMUQFile_Type), optional, intent(inout)                      ::    File
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArrayInput_I81D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    FormatLoc
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    Source = 'internal'
    if ( present(File) ) Source = 'external'
    call Input%AddParameter( Name='source', Value=Source )

    SectionName = 'source'
    call Input%AddSection( SectionName=SectionName )

    select case (Source)
      case('external')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        call Input%AddSection( Section=File%GetInput(MainSectionName='file'), To_SubSection=SectionName )
        call ExportArray( Array=Array, File=File, Format=FormatLoc, RowMajor=RowMajorLoc )
      case('internal')
        call Input%AddParameter( Name='values', Value=ConvertToString(Values=Array,Format=FormatLoc, Separator=' '),              &
                                                                                                         SectionName=SectionName )
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArrayInput_C1D( Input, Array, File, Format, RowMajor, Debug )

    class(InputSection_Type), intent(inout)                           ::    Input
    character(*), dimension(:), intent(in)                            ::    Array
    type(SMUQFile_Type), optional, intent(inout)                      ::    File
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArrayInput_C1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    FormatLoc
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    Source = 'internal'
    if ( present(File) ) Source = 'external'
    call Input%AddParameter( Name='source', Value=Source )

    SectionName = 'source'
    call Input%AddSection( SectionName=SectionName )

    select case (Source)
      case('external')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        call Input%AddSection( Section=File%GetInput(MainSectionName='file'), To_SubSection=SectionName )
        call ExportArray( Array=Array, File=File, Format=FormatLoc, RowMajor=RowMajorLoc )
      case('internal')
        call Input%AddParameter( Name='values', Value=ConvertToString(Values=Array,Format=FormatLoc, Separator=' '),              &
                                                                                                         SectionName=SectionName )
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArrayInput_L1D( Input, Array, File, Format, RowMajor, Debug )

    class(InputSection_Type), intent(inout)                           ::    Input
    logical, dimension(:), intent(in)                                 ::    Array
    type(SMUQFile_Type), optional, intent(inout)                      ::    File
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArrayInput_L1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    FormatLoc
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    Source = 'internal'
    if ( present(File) ) Source = 'external'
    call Input%AddParameter( Name='source', Value=Source )

    SectionName = 'source'
    call Input%AddSection( SectionName=SectionName )

    select case (Source)
      case('external')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        call Input%AddSection( Section=File%GetInput(MainSectionName='file'), To_SubSection=SectionName )
        call ExportArray( Array=Array, File=File, Format=FormatLoc, RowMajor=RowMajorLoc )
      case('internal')
        call Input%AddParameter( Name='values', Value=ConvertToString(Values=Array,Format=FormatLoc, Separator=' '),              &
                                                                                                         SectionName=SectionName )
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArrayInput_CX1D( Input, Array, File, Format, RowMajor, Debug )

    class(InputSection_Type), intent(inout)                           ::    Input
    complex, dimension(:), intent(in)                                 ::    Array
    type(SMUQFile_Type), optional, intent(inout)                      ::    File
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArrayInput_CX1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    FormatLoc
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    Source = 'internal'
    if ( present(File) ) Source = 'external'
    call Input%AddParameter( Name='source', Value=Source )

    SectionName = 'source'
    call Input%AddSection( SectionName=SectionName )

    select case (Source)
      case('external')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        call Input%AddSection( Section=File%GetInput(MainSectionName='file'), To_SubSection=SectionName )
        call ExportArray( Array=Array, File=File, Format=FormatLoc, RowMajor=RowMajorLoc )
      case('internal')
        call Input%AddParameter( Name='values', Value=ConvertToString(Values=Array,Format=FormatLoc, Separator=' '),              &
                                                                                                         SectionName=SectionName )
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArrayInput_String1D( Input, Array, File, Format, RowMajor, Debug )

    class(InputSection_Type), intent(inout)                           ::    Input
    type(String_Type), dimension(:), intent(in)                       ::    Array
    type(SMUQFile_Type), optional, intent(inout)                      ::    File
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArrayInput_String1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    FormatLoc
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    Source = 'internal'
    if ( present(File) ) Source = 'external'
    call Input%AddParameter( Name='source', Value=Source )

    SectionName = 'source'
    call Input%AddSection( SectionName=SectionName )

    select case (Source)
      case('external')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        call Input%AddSection( Section=File%GetInput(MainSectionName='file'), To_SubSection=SectionName )
        call ExportArray( Array=Array, File=File, Format=FormatLoc, RowMajor=RowMajorLoc )
      case('internal')
        call Input%AddParameter( Name='values', Value=ConvertToString(Values=Array,Format=FormatLoc, Separator=' '),              &
                                                                                                         SectionName=SectionName )
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArrayInput_R2D( Input, Array, File, Format, RowMajor, Debug )

    class(InputSection_Type), intent(inout)                           ::    Input
    real(rkp), dimension(:,:), intent(in)                             ::    Array
    type(SMUQFile_Type), optional, intent(inout)                      ::    File
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArrayInput_R2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    NbLines=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    Source = 'internal'
    if ( present(File) ) Source = 'external'
    call Input%AddParameter( Name='source', Value=Source )

    SectionName = 'source'
    call Input%AddSection( SectionName=SectionName )

    select case (Source)
      case('external')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        call Input%AddSection( Section=File%GetInput(MainSectionName='file'), To_SubSection=SectionName )
        call ExportArray( Array=Array, File=File, Format=FormatLoc, RowMajor=RowMajorLoc )
      case('internal')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        NbLines = size(Array,2)
        SubSectionName = 'array'
        call Input%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        SubSectionName = SectionName // '>' // SubSectionName
        if ( RowMajorLoc ) then
          i = 1
          do i = 1, NbLines
            ParameterName = 'row' // ConvertToString(Value=i)
            call Input%AddParameter( Name=Parametername, Value=ConvertToString(Values=Array(i,:)), SectionName=SubSectionName )
          end do
        else
          i = 1
          do i = 1, NbLines
            ParameterName = 'column' // ConvertToString(Value=i)
            call Input%AddParameter( Name=Parametername, Value=ConvertToString(Values=Array(:,i)), SectionName=SubSectionName )
          end do
        end if
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArrayInput_I2D( Input, Array, File, Format, RowMajor, Debug )

    class(InputSection_Type), intent(inout)                           ::    Input
    integer, dimension(:,:), intent(in)                               ::    Array
    type(SMUQFile_Type), optional, intent(inout)                      ::    File
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArrayInput_I2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    NbLines=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    Source = 'internal'
    if ( present(File) ) Source = 'external'
    call Input%AddParameter( Name='source', Value=Source )

    SectionName = 'source'
    call Input%AddSection( SectionName=SectionName )

    select case (Source)
      case('external')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        call Input%AddSection( Section=File%GetInput(MainSectionName='file'), To_SubSection=SectionName )
        call ExportArray( Array=Array, File=File, Format=FormatLoc, RowMajor=RowMajorLoc )
      case('internal')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        NbLines = size(Array,2)
        SubSectionName = 'array'
        call Input%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        SubSectionName = SectionName // '>' // SubSectionName
        if ( RowMajorLoc ) then
          i = 1
          do i = 1, NbLines
            ParameterName = 'row' // ConvertToString(Value=i)
            call Input%AddParameter( Name=Parametername, Value=ConvertToString(Values=Array(i,:)), SectionName=SubSectionName )
          end do
        else
          i = 1
          do i = 1, NbLines
            ParameterName = 'column' // ConvertToString(Value=i)
            call Input%AddParameter( Name=Parametername, Value=ConvertToString(Values=Array(:,i)), SectionName=SubSectionName )
          end do
        end if
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArrayInput_I82D( Input, Array, File, Format, RowMajor, Debug )

    class(InputSection_Type), intent(inout)                           ::    Input
    integer(8), dimension(:,:), intent(in)                            ::    Array
    type(SMUQFile_Type), optional, intent(inout)                      ::    File
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArrayInput_I82D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    NbLines=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    Source = 'internal'
    if ( present(File) ) Source = 'external'
    call Input%AddParameter( Name='source', Value=Source )

    SectionName = 'source'
    call Input%AddSection( SectionName=SectionName )

    select case (Source)
      case('external')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        call Input%AddSection( Section=File%GetInput(MainSectionName='file'), To_SubSection=SectionName )
        call ExportArray( Array=Array, File=File, Format=FormatLoc, RowMajor=RowMajorLoc )
      case('internal')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        NbLines = size(Array,2)
        SubSectionName = 'array'
        call Input%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        SubSectionName = SectionName // '>' // SubSectionName
        if ( RowMajorLoc ) then
          i = 1
          do i = 1, NbLines
            ParameterName = 'row' // ConvertToString(Value=i)
            call Input%AddParameter( Name=Parametername, Value=ConvertToString(Values=Array(i,:)), SectionName=SubSectionName )
          end do
        else
          i = 1
          do i = 1, NbLines
            ParameterName = 'column' // ConvertToString(Value=i)
            call Input%AddParameter( Name=Parametername, Value=ConvertToString(Values=Array(:,i)), SectionName=SubSectionName )
          end do
        end if
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArrayInput_C2D( Input, Array, File, Format, RowMajor, Debug )

    class(InputSection_Type), intent(inout)                           ::    Input
    character(*), dimension(:,:), intent(in)                          ::    Array
    type(SMUQFile_Type), optional, intent(inout)                      ::    File
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArrayInput_C2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    NbLines=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    Source = 'internal'
    if ( present(File) ) Source = 'external'
    call Input%AddParameter( Name='source', Value=Source )

    SectionName = 'source'
    call Input%AddSection( SectionName=SectionName )

    select case (Source)
      case('external')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        call Input%AddSection( Section=File%GetInput(MainSectionName='file'), To_SubSection=SectionName )
        call ExportArray( Array=Array, File=File, Format=FormatLoc, RowMajor=RowMajorLoc )
      case('internal')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        NbLines = size(Array,2)
        SubSectionName = 'array'
        call Input%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        SubSectionName = SectionName // '>' // SubSectionName
        if ( RowMajorLoc ) then
          i = 1
          do i = 1, NbLines
            ParameterName = 'row' // ConvertToString(Value=i)
            call Input%AddParameter( Name=Parametername, Value=ConvertToString(Values=Array(i,:)), SectionName=SubSectionName )
          end do
        else
          i = 1
          do i = 1, NbLines
            ParameterName = 'column' // ConvertToString(Value=i)
            call Input%AddParameter( Name=Parametername, Value=ConvertToString(Values=Array(:,i)), SectionName=SubSectionName )
          end do
        end if
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArrayInput_L2D( Input, Array, File, Format, RowMajor, Debug )

    class(InputSection_Type), intent(inout)                           ::    Input
    logical, dimension(:,:), intent(in)                               ::    Array
    type(SMUQFile_Type), optional, intent(inout)                      ::    File
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArrayInput_L2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    NbLines=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    Source = 'internal'
    if ( present(File) ) Source = 'external'
    call Input%AddParameter( Name='source', Value=Source )

    SectionName = 'source'
    call Input%AddSection( SectionName=SectionName )

    select case (Source)
      case('external')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        call Input%AddSection( Section=File%GetInput(MainSectionName='file'), To_SubSection=SectionName )
        call ExportArray( Array=Array, File=File, Format=FormatLoc, RowMajor=RowMajorLoc )
      case('internal')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        NbLines = size(Array,2)
        SubSectionName = 'array'
        call Input%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        SubSectionName = SectionName // '>' // SubSectionName
        if ( RowMajorLoc ) then
          i = 1
          do i = 1, NbLines
            ParameterName = 'row' // ConvertToString(Value=i)
            call Input%AddParameter( Name=Parametername, Value=ConvertToString(Values=Array(i,:)), SectionName=SubSectionName )
          end do
        else
          i = 1
          do i = 1, NbLines
            ParameterName = 'column' // ConvertToString(Value=i)
            call Input%AddParameter( Name=Parametername, Value=ConvertToString(Values=Array(:,i)), SectionName=SubSectionName )
          end do
        end if
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArrayInput_CX2D( Input, Array, File, Format, RowMajor, Debug )

    class(InputSection_Type), intent(inout)                           ::    Input
    complex, dimension(:,:), intent(in)                               ::    Array
    type(SMUQFile_Type), optional, intent(inout)                      ::    File
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArrayInput_CX2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    NbLines=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    Source = 'internal'
    if ( present(File) ) Source = 'external'
    call Input%AddParameter( Name='source', Value=Source )

    SectionName = 'source'
    call Input%AddSection( SectionName=SectionName )

    select case (Source)
      case('external')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        call Input%AddSection( Section=File%GetInput(MainSectionName='file'), To_SubSection=SectionName )
        call ExportArray( Array=Array, File=File, Format=FormatLoc, RowMajor=RowMajorLoc )
      case('internal')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        NbLines = size(Array,2)
        SubSectionName = 'array'
        call Input%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        SubSectionName = SectionName // '>' // SubSectionName
        if ( RowMajorLoc ) then
          i = 1
          do i = 1, NbLines
            ParameterName = 'row' // ConvertToString(Value=i)
            call Input%AddParameter( Name=Parametername, Value=ConvertToString(Values=Array(i,:)), SectionName=SubSectionName )
          end do
        else
          i = 1
          do i = 1, NbLines
            ParameterName = 'column' // ConvertToString(Value=i)
            call Input%AddParameter( Name=Parametername, Value=ConvertToString(Values=Array(:,i)), SectionName=SubSectionName )
          end do
        end if
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArrayInput_String2D( Input, Array, File, Format, RowMajor, Debug )

    class(InputSection_Type), intent(inout)                           ::    Input
    type(String_Type), dimension(:,:), intent(in)                     ::    Array
    type(SMUQFile_Type), optional, intent(inout)                      ::    File
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArrayInput_String2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    NbLines=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    Source = 'internal'
    if ( present(File) ) Source = 'external'
    call Input%AddParameter( Name='source', Value=Source )

    SectionName = 'source'
    call Input%AddSection( SectionName=SectionName )

    select case (Source)
      case('external')
        call Input%AddSection( Section=File%GetInput(MainSectionName='file'), To_SubSection=SectionName )
        call ExportArray( Array=Array, File=File, Format=FormatLoc, RowMajor=RowMajorLoc )
      case('internal')
        call Input%AddParameter( Name='row_major', Value=ConvertToString(Value=RowMajorLoc), SectionName=SectionName )
        NbLines = size(Array,2)
        SubSectionName = 'array'
        call Input%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        SubSectionName = SectionName // '>' // SubSectionName
        if ( RowMajorLoc ) then
          i = 1
          do i = 1, NbLines
            ParameterName = 'row' // ConvertToString(Value=i)
            call Input%AddParameter( Name=Parametername, Value=ConvertToString(Values=Array(i,:)), SectionName=SubSectionName )
          end do
        else
          i = 1
          do i = 1, NbLines
            ParameterName = 'column' // ConvertToString(Value=i)
            call Input%AddParameter( Name=Parametername, Value=ConvertToString(Values=Array(:,i)), SectionName=SubSectionName )
          end do
        end if
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArray_R1D( Array, File, Header, Format, RowMajor, Append, Debug )

    real(rkp), dimension(:), intent(in)                               ::    Array
    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:), optional, intent(in)             ::    Header
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Append
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArray_R1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    i
    character(:), allocatable                                         ::    SeparatorLoc
    logical                                                           ::    RowMajorLoc
    logical                                                           ::    AppendLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    AppendLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor
    if ( present(Append) ) AppendLoc = Append

    if ( File%Exists() ) then
      if ( AppendLoc ) then
        call File%Open( Unit=UnitLoc, Action='write', Status='old', Position='append' )
      else
        call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      end if
    else
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
    end if

    if ( present(Header) ) then
      i = 1
      do i = 1, size(Header,1)
        write(UnitLoc, '(A)',iostat=StatLoc ) Header(i)%GetValue()
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( RowMajorLoc ) then
      i = 1
      do i = 1, size(Array,1)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Value=Array(i), Format=FormatLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    else
      SeparatorLoc = File%GetSeparator()
      write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array, Format=FormatLoc, Separator=SeparatorLoc )
    end if

    call File%Close()

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArray_I1D( Array, File, Header, Format, RowMajor, Append, Debug )

    integer, dimension(:), intent(in)                                 ::    Array
    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:), optional, intent(in)             ::    Header
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Append
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArray_I1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc
    logical                                                           ::    AppendLoc
    character(:), allocatable                                         ::    SeparatorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    AppendLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor
    if ( present(Append) ) AppendLoc = Append

    if ( File%Exists() ) then
      if ( AppendLoc ) then
        call File%Open( Unit=UnitLoc, Action='write', Status='old', Position='append' )
      else
        call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      end if
    else
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
    end if

    if ( present(Header) ) then
      i = 1
      do i = 1, size(Header,1)
        write(UnitLoc, '(A)',iostat=StatLoc ) Header(i)%GetValue()
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( RowMajorLoc ) then
      i = 1
      do i = 1, size(Array,1)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Value=Array(i), Format=FormatLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    else
      SeparatorLoc = File%GetSeparator()
      write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array, Format=FormatLoc, Separator=SeparatorLoc )
    end if

    call File%Close()

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArray_I81D( Array, File, Header, Format, RowMajor, Append, Debug )

    integer(8), dimension(:), intent(in)                              ::    Array
    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:), optional, intent(in)             ::    Header
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Append
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArray_I81D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc
    logical                                                           ::    AppendLoc
    character(:), allocatable                                         ::    SeparatorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    AppendLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor
    if ( present(Append) ) AppendLoc = Append

    if ( File%Exists() ) then
      if ( AppendLoc ) then
        call File%Open( Unit=UnitLoc, Action='write', Status='old', Position='append' )
      else
        call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      end if
    else
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
    end if

    if ( present(Header) ) then
      i = 1
      do i = 1, size(Header,1)
        write(UnitLoc, '(A)',iostat=StatLoc ) Header(i)%GetValue()
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( RowMajorLoc ) then
      i = 1
      do i = 1, size(Array,1)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Value=Array(i), Format=FormatLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    else
      SeparatorLoc = File%GetSeparator()
      write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array, Format=FormatLoc, Separator=SeparatorLoc )
    end if

    call File%Close()

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArray_C1D( Array, File, Header, Format, RowMajor, Append, Debug )

    character(*), dimension(:), intent(in)                            ::    Array
    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:), optional, intent(in)             ::    Header
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Append
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArray_C1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc
    logical                                                           ::    AppendLoc
    character(:), allocatable                                         ::    SeparatorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    AppendLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor
    if ( present(Append) ) AppendLoc = Append

    if ( File%Exists() ) then
      if ( AppendLoc ) then
        call File%Open( Unit=UnitLoc, Action='write', Status='old', Position='append' )
      else
        call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      end if
    else
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
    end if

    if ( present(Header) ) then
      i = 1
      do i = 1, size(Header,1)
        write(UnitLoc, '(A)',iostat=StatLoc ) Header(i)%GetValue()
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( RowMajorLoc ) then
      i = 1
      do i = 1, size(Array,1)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Value=Array(i), Format=FormatLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    else
      SeparatorLoc = File%GetSeparator()
      write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array, Format=FormatLoc, Separator=SeparatorLoc )
    end if

    call File%Close()

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArray_L1D( Array, File, Header, Format, RowMajor, Append, Debug )

    logical, dimension(:), intent(in)                                 ::    Array
    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:), optional, intent(in)             ::    Header
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Append
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArray_L1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc
    logical                                                           ::    AppendLoc
    character(:), allocatable                                         ::    SeparatorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    AppendLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor
    if ( present(Append) ) AppendLoc = Append

    if ( File%Exists() ) then
      if ( AppendLoc ) then
        call File%Open( Unit=UnitLoc, Action='write', Status='old', Position='append' )
      else
        call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      end if
    else
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
    end if

    if ( present(Header) ) then
      i = 1
      do i = 1, size(Header,1)
        write(UnitLoc, '(A)',iostat=StatLoc ) Header(i)%GetValue()
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( RowMajorLoc ) then
      i = 1
      do i = 1, size(Array,1)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Value=Array(i), Format=FormatLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    else
      SeparatorLoc = File%GetSeparator()
      write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array, Format=FormatLoc, Separator=SeparatorLoc )
    end if

    call File%Close()

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArray_CX1D( Array, File, Header, Format, RowMajor, Append, Debug )

    complex, dimension(:), intent(in)                                 ::    Array
    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:), optional, intent(in)             ::    Header
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Append
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArray_CX1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc
    logical                                                           ::    AppendLoc
    character(:), allocatable                                         ::    SeparatorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    AppendLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor
    if ( present(Append) ) AppendLoc = Append

    if ( File%Exists() ) then
      if ( AppendLoc ) then
        call File%Open( Unit=UnitLoc, Action='write', Status='old', Position='append' )
      else
        call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      end if
    else
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
    end if

    if ( present(Header) ) then
      i = 1
      do i = 1, size(Header,1)
        write(UnitLoc, '(A)',iostat=StatLoc ) Header(i)%GetValue()
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( RowMajorLoc ) then
      i = 1
      do i = 1, size(Array,1)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Value=Array(i), Format=FormatLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    else
      SeparatorLoc = File%GetSeparator()
      write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array, Format=FormatLoc, Separator=SeparatorLoc )
    end if

    call File%Close()

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArray_String1D( Array, File, Header, Format, RowMajor, Append, Debug )

    type(String_Type), dimension(:), intent(in)                       ::    Array
    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:), optional, intent(in)             ::    Header
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Append
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArray_String1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc
    logical                                                           ::    AppendLoc
    character(:), allocatable                                         ::    SeparatorLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .true.
    AppendLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor
    if ( present(Append) ) AppendLoc = Append

    if ( File%Exists() ) then
      if ( AppendLoc ) then
        call File%Open( Unit=UnitLoc, Action='write', Status='old', Position='append' )
      else
        call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      end if
    else
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
    end if

    if ( present(Header) ) then
      i = 1
      do i = 1, size(Header,1)
        write(UnitLoc, '(A)',iostat=StatLoc ) Header(i)%GetValue()
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( RowMajorLoc ) then
      i = 1
      do i = 1, size(Array,1)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Value=Array(i), Format=FormatLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    else
      SeparatorLoc = File%GetSeparator()
      write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array, Format=FormatLoc, Separator=SeparatorLoc )
    end if

    call File%Close()

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArray_R2D( Array, File, Header, Format, RowMajor, Append, Debug )

    real(rkp), dimension(:,:), intent(in)                             ::    Array
    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:), optional, intent(in)             ::    Header
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Append
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArray_R2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc
    logical                                                           ::    AppendLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    AppendLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor
    if ( present(Append) ) AppendLoc = Append

    if ( File%Exists() ) then
      if ( AppendLoc ) then
        call File%Open( Unit=UnitLoc, Action='write', Status='old', Position='append' )
      else
        call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      end if
    else
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
    end if

    SeparatorLoc = File%GetSeparator()

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( present(Header) ) then
      i = 1
      do i = 1, size(Header,1)
        write(UnitLoc, '(A)',iostat=StatLoc ) Header(i)%GetValue()
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    if ( RowMajorLoc ) then
      i = 1
      do i = 1, size(Array,1)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array(i,:), Format=FormatLoc, Separator=SeparatorLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    else
      i = 1
      do i = 1, size(Array,2)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array(:,i), Format=FormatLoc, Separator=SeparatorLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    call File%Close()

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArray_I2D( Array, File, Header, Format, RowMajor, Append, Debug )

    integer, dimension(:,:), intent(in)                               ::    Array
    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:), optional, intent(in)             ::    Header
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Append
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArray_I2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc
    logical                                                           ::    AppendLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    AppendLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor
    if ( present(Append) ) AppendLoc = Append

    if ( File%Exists() ) then
      if ( AppendLoc ) then
        call File%Open( Unit=UnitLoc, Action='write', Status='old', Position='append' )
      else
        call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      end if
    else
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
    end if

    SeparatorLoc = File%GetSeparator()

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( present(Header) ) then
      i = 1
      do i = 1, size(Header,1)
        write(UnitLoc, '(A)',iostat=StatLoc ) Header(i)%GetValue()
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    if ( RowMajorLoc ) then
      i = 1
      do i = 1, size(Array,1)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array(i,:), Format=FormatLoc, Separator=SeparatorLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    else
      i = 1
      do i = 1, size(Array,2)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array(:,i), Format=FormatLoc, Separator=SeparatorLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if
    call File%Close()

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArray_I82D( Array, File, Header, Format, RowMajor, Append, Debug )

    integer(8), dimension(:,:), intent(in)                            ::    Array
    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:), optional, intent(in)             ::    Header
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Append
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArray_I82D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc
    logical                                                           ::    AppendLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    AppendLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor
    if ( present(Append) ) AppendLoc = Append

    if ( File%Exists() ) then
      if ( AppendLoc ) then
        call File%Open( Unit=UnitLoc, Action='write', Status='old', Position='append' )
      else
        call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      end if
    else
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
    end if

    SeparatorLoc = File%GetSeparator()

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( present(Header) ) then
      i = 1
      do i = 1, size(Header,1)
        write(UnitLoc, '(A)',iostat=StatLoc ) Header(i)%GetValue()
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    if ( RowMajorLoc ) then
      i = 1
      do i = 1, size(Array,1)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array(i,:), Format=FormatLoc, Separator=SeparatorLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    else
      i = 1
      do i = 1, size(Array,2)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array(:,i), Format=FormatLoc, Separator=SeparatorLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    call File%Close()

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArray_C2D( Array, File, Header, Format, RowMajor, Append, Debug )

    character(*), dimension(:,:), intent(in)                          ::    Array
    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:), optional, intent(in)             ::    Header
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Append
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArray_C2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc
    logical                                                           ::    AppendLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    AppendLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor
    if ( present(Append) ) AppendLoc = Append

    if ( File%Exists() ) then
      if ( AppendLoc ) then
        call File%Open( Unit=UnitLoc, Action='write', Status='old', Position='append' )
      else
        call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      end if
    else
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
    end if

    SeparatorLoc = File%GetSeparator()

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( present(Header) ) then
      i = 1
      do i = 1, size(Header,1)
        write(UnitLoc, '(A)',iostat=StatLoc ) Header(i)%GetValue()
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    if ( RowMajorLoc ) then
      i = 1
      do i = 1, size(Array,1)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array(i,:), Format=FormatLoc, Separator=SeparatorLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    else
      i = 1
      do i = 1, size(Array,2)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array(:,i), Format=FormatLoc, Separator=SeparatorLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    call File%Close()

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArray_L2D( Array, File, Header, Format, RowMajor, Append, Debug )

    logical, dimension(:,:), intent(in)                               ::    Array
    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:), optional, intent(in)             ::    Header
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Append
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArray_L2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc
    logical                                                           ::    AppendLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    AppendLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor
    if ( present(Append) ) AppendLoc = Append

    if ( File%Exists() ) then
      if ( AppendLoc ) then
        call File%Open( Unit=UnitLoc, Action='write', Status='old', Position='append' )
      else
        call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      end if
    else
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
    end if

    SeparatorLoc = File%GetSeparator()

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( present(Header) ) then
      i = 1
      do i = 1, size(Header,1)
        write(UnitLoc, '(A)',iostat=StatLoc ) Header(i)%GetValue()
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    if ( RowMajorLoc ) then
      i = 1
      do i = 1, size(Array,1)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array(i,:), Format=FormatLoc, Separator=SeparatorLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    else
      i = 1
      do i = 1, size(Array,2)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array(:,i), Format=FormatLoc, Separator=SeparatorLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    call File%Close()

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArray_CX2D( Array, File, Header, Format, RowMajor, Append, Debug )

    complex, dimension(:,:), intent(in)                               ::    Array
    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:), optional, intent(in)             ::    Header
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Append
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArray_CX2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc
    logical                                                           ::    AppendLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    AppendLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor
    if ( present(Append) ) AppendLoc = Append

    if ( File%Exists() ) then
      if ( AppendLoc ) then
        call File%Open( Unit=UnitLoc, Action='write', Status='old', Position='append' )
      else
        call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      end if
    else
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
    end if

    SeparatorLoc = File%GetSeparator()

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( present(Header) ) then
      i = 1
      do i = 1, size(Header,1)
        write(UnitLoc, '(A)',iostat=StatLoc ) Header(i)%GetValue()
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    if ( RowMajorLoc ) then
      i = 1
      do i = 1, size(Array,1)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array(i,:), Format=FormatLoc, Separator=SeparatorLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    else
      i = 1
      do i = 1, size(Array,2)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array(:,i), Format=FormatLoc, Separator=SeparatorLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    call File%Close()

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportArray_String2D( Array, File, Header, Format, RowMajor, Append, Debug )

    type(String_Type), dimension(:,:), intent(in)                     ::    Array
    type(SMUQFile_Type), intent(inout)                                ::    File
    type(String_Type), dimension(:), optional, intent(in)             ::    Header
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    RowMajor
    logical, optional, intent(in)                                     ::    Append
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportArray_String2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    UnitLoc=0
    integer                                                           ::    i
    logical                                                           ::    RowMajorLoc
    logical                                                           ::    AppendLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    RowMajorLoc = .false.
    AppendLoc = .false.
    if ( present(RowMajor) ) RowMajorLoc = RowMajor
    if ( present(Append) ) AppendLoc = Append

    if ( File%Exists() ) then
      if ( AppendLoc ) then
        call File%Open( Unit=UnitLoc, Action='write', Status='old', Position='append' )
      else
        call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
      end if
    else
      call File%Open( Unit=UnitLoc, Action='write', Status='replace', Position='rewind' )
    end if

    SeparatorLoc = File%GetSeparator()

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( present(Header) ) then
      i = 1
      do i = 1, size(Header,1)
        write(UnitLoc, '(A)',iostat=StatLoc ) Header(i)%GetValue()
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    if ( RowMajorLoc ) then
      i = 1
      do i = 1, size(Array,1)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array(i,:), Format=FormatLoc, Separator=SeparatorLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    else
      i = 1
      do i = 1, size(Array,2)
        write(UnitLoc, '(A)', iostat=StatLoc) ConvertToString( Values=Array(:,i), Format=FormatLoc, Separator=SeparatorLoc )
        if ( StatLoc /= 0 ) call Error%Write( ProcName=ProcName, File=File%GetFullFile(), Unit=UnitLoc, iostat=StatLoc )
      end do
    end if

    call File%Close()

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ImportFile( Input, Strings, Prefix, Comment, Separator, Debug )

    class(InputSection_Type), intent(in)                              ::    Input
    type(String_Type), allocatable, dimension(:), intent(out)         ::    Strings
    character(*), optional, intent(out)                               ::    Comment
    character(*), optional, intent(out)                               ::    Separator
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ImportFile'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    File
    integer                                                           ::    NbLinesSkip=0
    integer                                                           ::    NbLines=0
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    type(String_Type), allocatable, dimension(:)                      ::    VarString1D
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    Source = VarC0D

    SectionName = 'source'
    
    select case (Source)
      case('external')
        ParameterName = 'nb_lines_skip'
        call Input%GetValue( Value=VarI0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( Found ) NbLinesSkip = VarI0D

        SubSectionName = SectionName // '>file'
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call File%Construct( Input=InputSection, Prefix=PrefixLoc )
        nullify(InputSection)
        if ( NbLinesSkip == 0 ) then
          call File%Import( Strings=Strings, Mandatory=.true. )
        else
          call File%Import( Strings=VarString1D, Mandatory=.true. )
          if ( NbLinesSkip >= size(VarString1D,1) ) call Error%Raise( Line='Specified too many lines to skip', ProcName=ProcName )
          allocate(Strings, source=VarString1D(NbLinesSkip+1:size(VarString1D,1)), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='Strings', ProcName=ProcName, stat=StatLoc )
          deallocate(VarString1D, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarString1D', ProcName=ProcName, stat=StatLoc )
        end if
        if ( present(Comment) ) Comment = File%GetComment()
        if ( present(Separator) ) Separator = File%GetSeparator()
      case('internal')

        ParameterName = 'comment'
        call Input%GetValue(Value=VarC0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( present( Comment ) ) Comment = '#'
        if ( Found .and. present(Comment) ) Comment = VarC0D

        ParameterName = 'separator'
        call Input%GetValue(Value=VarC0D, ParameterName=Parametername, Mandatory=.false., SectionName=SectionName, Found=Found)
        if ( present( Separator ) ) Separator = ' '
        if ( Found .and. present(Separator) ) Separator = VarC0D

        ParameterName = 'nb_lines'
        call Input%GetValue(Value=VarI0D, ParameterName=Parametername, Mandatory=.true., SectionName=SectionName)
        NbLines = VarI0D

        allocate(Strings(NbLines), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Strings', ProcName=ProcName, stat=StatLoc )

        SubSectionName = SectionName // '>lines'
        i = 1
        do i = 1, Nblines
          ParameterName = 'line' // ConvertToString(Value=i)
          call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
          call Strings(i)%Set_Value( Value=VarC0D )
        end do
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ExportFile( Input, Strings, File, Format, Debug )

    class(InputSection_Type), intent(inout)                           ::    Input
    type(String_Type), dimension(:), intent(in)                       ::    Strings
    type(SMUQFile_Type), optional, intent(inout)                      ::    File
    character(*), optional, intent(in)                                ::    Format
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='ExportFile'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    Source
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    Source = 'internal'
    if ( present(File) ) Source = 'external'
    call Input%AddParameter( Name='source', Value=Source )

    SectionName = 'source'
    call Input%AddSection( SectionName=SectionName )

    select case (Source)
      case('external')
        call Input%AddSection( Section=File%GetInput(MainSectionName='file'), To_SubSection=SectionName )
        call ExportArray( Array=Strings, File=File, Format=FormatLoc )
      case('internal')
        call Input%AddParameter( Name='nb_lines', Value=ConvertToString(Value=size(Strings,1)),&
                                                                                                         SectionName=SectionName )
        SubSectionName = 'lines'        
        call Input%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        i = 1
        do i = 1, size(Strings,1)
          call Input%AddParameter( Name='line' // ConvertToString(Value=i), Value=Strings(i)%GetValue() )
        end do
      case default
        call Error%Raise( Line='Unrecognized source format', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
