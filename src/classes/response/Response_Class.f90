! -*-f90-*-
!!--------------------------------------------------------------------------------------------------------------------------------
!!
!! Stochastic Modeling & Uncertainty Quantification (SMUQ)
!!
!! Copyright (C) 2016 Venturi, Simone & Rostkowski, Przemyslaw (University of Illinois at Urbana-Champaign)
!!
!! This program is free software; you can redistribute it and/or modify it under the terms of the Version 2.1 GNU Lesser General
!! Public License as published by the Free Software Foundation.
!!
!! This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
!!
!! You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to the Free 
!! Software Foundation, Inc. 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
!!
!!--------------------------------------------------------------------------------------------------------------------------------

module Response_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use ArrayIORoutines_Module
use ArrayRoutines_Module
use StringRoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use CovarianceConstructor_Class                                   ,only:    CovarianceConstructor_Type
use CovarianceConstructor_Factory_Class                           ,only:    CovarianceConstructor_Factory
use CovariancePredefined_Class                                    ,only:    CovariancePredefined_Type
use Input_Class                                                   ,only:    Input_Type
use InputDet_Class                                                ,only:    InputDet_Type

implicit none

private

public                                                                ::    Response_Type

type                                                                  ::    Response_Type
  character(:), allocatable                                           ::    Name
  character(:), allocatable                                           ::    ResponseName
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    Label
  real(rkp), dimension(:,:), pointer                                  ::    ResponseData=>null()
  real(rkp), dimension(:), pointer                                    ::    Abscissa=>null()
  character(:), allocatable                                           ::    AbscissaName
  integer                                                             ::    NbDataSets
  logical                                                             ::    DataDefined
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    GetAbscissa
  procedure, public                                                   ::    GetAbscissaPointer
  procedure, public                                                   ::    GetData
  procedure, public                                                   ::    GetDataPointer
  procedure, public                                                   ::    GetNbDataSets
  procedure, public                                                   ::    GetResponseName
  procedure, public                                                   ::    GetAbscissaName
  procedure, public                                                   ::    GetLabel
  procedure, public                                                   ::    GetName
  procedure, public                                                   ::    IsDataDefined
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(Response_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'response'
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(Response_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    if ( associated(This%Abscissa) ) deallocate(This%Abscissa, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Abscissa', ProcName=ProcName, stat=StatLoc )

    if ( associated(This%ResponseData) ) deallocate(This%ResponseData, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ResponseData', ProcName=ProcName, stat=StatLoc )

    This%NbDataSets = 0

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(Response_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Label = '<undefined>'
    This%ResponseName = '<undefined>'
    This%AbscissaName = '<undefined>'
    This%NbDataSets = 0

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(Response_Type), intent(inout)                               ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    character(:), allocatable                                         ::    VarC0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    integer, allocatable, dimension(:)                                ::    VarI1D
    integer                                                           ::    NbDataSets
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    integer                                                           ::    i
    integer                                                           ::    AbscissaColumn
    character(:), allocatable                                         ::    AbscissaSource
    integer                                                           ::    VarI0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()
    
    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix
    
    ParameterName = 'name'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.false., Found=Found )
    if ( Found ) This%ResponseName = VarC0D

    ParameterName = 'label'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    This%Label = VarC0D

    SectionName = 'abscissa'

    ParameterName = 'name'
    call Input%GetValue( ParameterName=ParameterName, Value=VarC0D, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( Found ) This%AbscissaName = VarC0D

    ParameterName = 'source' 
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
    AbscissaSource = VarC0D
    SubSectionName = SectionName // '>source'
    select case ( AbscissaSource )
      case ( 'values' )
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ImportArray( Input=InputSection, Array=VarR1D, Prefix=PrefixLoc )
      case ( 'computed' )
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        VarR1D = LinSpaceVec( Input=InputSection )
      case default
        call Error%Raise( Line='Abscissa source not recognized', ProcName=ProcName )
    end select

    allocate(This%Abscissa, source=VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Abscissa', ProcName=ProcName, stat=StatLoc )
    deallocate(VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    SectionName = 'data'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.false., FoundSection=Found )
    if ( Found ) then
      SubSectionName = SectionName // '>source'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=VarR2D, Prefix=PrefixLoc, RowMajor=.true. )

      AbscissaColumn = 1
      ParameterName = 'abscissa_column'
      call Input%GetValue( ParameterName=ParameterName, Value=VarI0D, SectionName=SectionName, Mandatory=.false., Found=Found )
      if ( Found ) AbscissaColumn = VarI0D

      
      ParameterName = 'ordinate_column'
      call Input%GetValue( ParameterName=ParameterName, Value=VarC0D, SectionName=SectionName, Mandatory=.false., Found=Found )
      if ( Found ) then
        VarI1D = ConvertToIntegers( String=VarC0D )
      else
        VarI1D = LinSequence( SeqStart=2, SeqEnd=size(VarR2D,2)-1 )
      end if
      This%NbDataSets = size(VarI1D,1)

      allocate(This%ResponseData(size(This%Abscissa,1),This%NbDataSets), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ResponseData', ProcName=ProcName, stat=StatLoc )

      i = 1
      do i = 1, size(VarI1D,1)
        This%ResponseData(:,i) = Interpolate( Abscissa=VarR2D(:,AbscissaColumn), Ordinate=VarR2D(:,VarI1D(i)),Nodes=This%Abscissa)
      end do

      deallocate(VarI1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI1D', ProcName=ProcName, stat=StatLoc )

      deallocate(VarR2D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )

      nullify(InputSection)
    end if

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(Response_Type), intent(in)                                  ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    FileName
    type(SMUQFile_Type)                                               ::    File
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    if ( len_trim(This%Label) /= 0 ) call GetInput%AddParameter( Name='label', Value=This%Label )
    if ( len_trim(This%ResponseName) /= 0 ) call GetInput%AddParameter( Name='name', Value=This%ResponseName )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/abscissa'
    SectionName = 'abscissa'
    call GetInput%AddSection( SectionName=SectionName )
    if ( len_trim(This%AbscissaName) /= 0 ) call GetInput%AddParameter( Name='name', Value=This%Name, SectionName=SectionName )

    call GetInput%AddParameter( Name='source', Value='values', SectionName=SectionName )

    SubSectionName = 'source'
    call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
    call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,             &
                                                                                                                Mandatory=.true. )
    if ( ExternalFlag ) then
      FileName = DirectoryLoc // '/abscissa.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Input=InputSection, Array=This%Abscissa, File=File )
    else
      call ExportArray( Input=InputSection, Array=This%Abscissa )
    end if
    nullify(InputSection)

    if ( This%NbDataSets > 1 ) then
      SectionName = 'data'
      call GetInput%AddSection( SectionName=SectionName )

      call GetInput%AddParameter( Name='abscissa_column', Value='1', SectionName=SectionName )
      call GetInput%AddParameter( Name='ordinate_column', Value=ConvertToString(                                                  &
                                                      LinSequence(SeqStart=2,SeqEnd=This%NbDataSets)+1), SectionName=SectionName )

      SubSectionName = 'source'
      call GetInput%AddSection( SectionName=SubSectionName, To_Subsection=SectionName )
      call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,           &
                                                                                                                Mandatory=.true. )
      if ( ExternalFlag ) then
        FileName = DirectoryLoc // '/data.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Input=InputSection, Array=This%ResponseData, File=File, RowMajor=.true. )
      else
        call ExportArray( Input=InputSection, Array=This%ResponseData, RowMajor=.true. )
      end if
      nullify(InputSection)
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetAbscissa( This, Debug )

    real(rkp), allocatable, dimension(:)                              ::    GetAbscissa

    class(Response_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetAbscissa'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    allocate(GetAbscissa, source=This%Abscissa, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetAbscissa', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetAbscissaPointer( This, Debug )

    real(rkp), dimension(:), pointer                                  ::    GetAbscissaPointer

    class(Response_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetAbscissaPointer'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetAbscissaPointer => This%Abscissa

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetData( This, Debug )

    real(rkp), allocatable, dimension(:,:)                            ::    GetData

    class(Response_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetData'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( This%NbDataSets < 1 ) call Error%Raise( Line='No data available', ProcName=ProcName )

    allocate(GetData, source=This%ResponseData, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetData', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDataPointer( This, Debug )

    real(rkp), dimension(:,:), pointer                                ::    GetDataPointer

    class(Response_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetDataPointer'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( This%NbDataSets < 1 ) call Error%Raise( Line='No data available', ProcName=ProcName )

    GetDataPointer => This%ResponseData

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetAbscissaName( This, Debug )

    character(:), allocatable                                         ::    GetAbscissaName

    class(Response_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetAbscissaName'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetAbscissaName = This%Abscissaname

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetResponseName( This, Debug )

    character(:), allocatable                                         ::    GetResponseName

    class(Response_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetResponseName'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetResponseName = This%ResponseName

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel( This, Debug )

    character(:), allocatable                                         ::    GetLabel

    class(Response_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetLabel'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetLabel = This%Label

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName

    class(Response_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetName'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetName = This%Name

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbDataSets( This, Debug )

    integer                                                           ::    GetNbDataSets

    class(Response_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNbDataSets'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetNbDataSets = This%NbDataSets

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsDataDefined( This, Debug )

    logical                                                           ::    IsDataDefined

    class(Response_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='IsDataDefined'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    IsDataDefined = .false.
    if ( This%NbDataSets > 0 ) IsDataDefined = .true.

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(Response_Type), intent(out)                                 ::    LHS
    class(Response_Type), intent(in)                                  ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (Response_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%ResponseName = RHS%ResponseName
          LHS%Name = RHS%Name
          LHS%AbscissaName = RHS%AbscissaName
          LHS%Label = RHS%Label
          LHS%NbDataSets = RHS%NbDataSets
          allocate(LHS%Abscissa, source=RHS%Abscissa, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Abscissa', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%ResponseData, source=RHS%ResponseData, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%ResponseData', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(Response_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( associated(This%Abscissa) ) deallocate(This%Abscissa, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Abscissa', ProcName=ProcName, stat=StatLoc )

    if ( associated(This%ResponseData) ) deallocate(This%ResponseData, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ResponseData', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
