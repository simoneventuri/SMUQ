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
use String_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use CovarianceConstructor_Class                                   ,only:    CovarianceConstructor_Type
use CovarianceConstructor_Factory_Class                           ,only:    CovarianceConstructor_Factory
use CovariancePredefined_Class                                    ,only:    CovariancePredefined_Type
use Input_Class                                                   ,only:    Input_Type

implicit none

private

public                                                                ::    Response_Type

type                                                                  ::    Response_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    Label
  real(rkp), dimension(:,:), allocatable                              ::    Coordinates
  type(String_Type), allocatable, dimension(:)                        ::    CoordinatesLabels
  integer                                                             ::    NbIndCoordinates=0
  integer                                                             ::    NbNodes=0
  real(rkp), dimension(:,:), allocatable                              ::    ResponseData
  integer                                                             ::    NbDataSets=0
  logical                                                             ::    DataDefined=.false.
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  generic, public                                                     ::    GetCoordinates          =>    GetCoordsLabel_R1D,     &
                                                                                                          GetCoordsLabels_R2D,    &
                                                                                                          GetCoords_R2D
  generic, public                                                     ::    GetCoordinatesPointer   =>    GetCoordsLabelPtr_R1D,  &
                                                                                                          GetCoordsPtr_R2D
  procedure, public                                                   ::    GetCoordsLabel_R1D
  procedure, public                                                   ::    GetCoordsLabels_R2D
  procedure, public                                                   ::    GetCoords_R2D
  procedure, public                                                   ::    GetCoordsLabelPtr_R1D
  procedure, public                                                   ::    GetCoordsPtr_R2D
  procedure, public                                                   ::    GetCoordinateLabels
  procedure, public                                                   ::    GetNbIndCoordinates
  procedure, public                                                   ::    GetData
  procedure, public                                                   ::    GetDataPointer
  procedure, public                                                   ::    GetNbDataSets
  procedure, public                                                   ::    GetLabel
  procedure, public                                                   ::    GetNbNodes
  procedure, public                                                   ::    GetName
  procedure, public                                                   ::    IsDataDefined
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(Response_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'response'
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(Response_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%Coordinates) ) deallocate(This%Coordinates, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Coordinates', ProcName=ProcName, stat=StatLoc )
    This%NbIndCoordinates = 0

    if ( allocated(This%CoordinatesLabels) ) deallocate(This%CoordinatesLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CoordinatesLabels', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ResponseData) ) deallocate(This%ResponseData, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ResponseData', ProcName=ProcName, stat=StatLoc )
    This%NbDataSets = 0
    This%DataDefined = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(Response_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'
    integer                                                           ::    StatLoc=0

    This%Label = '<undefined>'
    This%NbNodes = 0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    class(Response_Type), intent(inout)                               ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    integer, allocatable, dimension(:)                                ::    VarI1D
    integer                                                           ::    NbDataSets
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    integer                                                           ::    i
    integer                                                           ::    AbscissaColumn
    character(:), allocatable                                         ::    AbscissaSource

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()
    
    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix
    
    ParameterName = 'name'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.false., Found=Found )
    if ( Found ) This%Name = VarC0D

    ParameterName = 'label'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.true. )
    This%Label = VarC0D

    SectionName = 'coordinates'
    ParameterName = 'labels'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.true. )

    allocate(This%Coordinateslabels, source=ConvertToStrings(Value=VarC0D), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Coordinateslabels', ProcName=ProcName, stat=StatLoc )

    SubSectionName = SectionName // '>values'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
    call ImportArray( Input=InputSection, Array=VarR2D, RowMajor=.true.,Prefix=PrefixLoc )
    nullify(InputSection)
    allocate(This%Coordinates, source=VarR2D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Coordinates', ProcName=ProcName, stat=StatLoc )
    deallocate(VarR2D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )

    This%NbNodes = size(This%Coordinates,1)
    This%NbIndCoordinates = size(This%Coordinates,2)

    if ( size(This%CoordinatesLabels,1) /= This%NbIndCoordinates ) call Error%Raise( 'Mismatch in the number of coordinate ' //   &
                                                               'labels and number of independent coordinates', ProcName=ProcName )

    SectionName = 'data'
    if ( Input%HasSection(SubSectionName=SectionName) ) then
      SubSectionName = SectionName // '>values'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=VarR2D, Prefix=PrefixLoc, RowMajor=.true. )
      
      if ( size(VarR2D,1) /= This%NbNodes ) call Error%Raise( Line='Number of lines in data file does not match specified ' //    &
                                                                           'number of nodes for the response', ProcName=ProcName )

      ParameterName = 'column'
      call Input%GetValue( ParameterName=ParameterName, Value=VarC0D, SectionName=SectionName, Mandatory=.false., Found=Found )
      if ( Found ) then
        VarI1D = ConvertToIntegers( String=VarC0D )
      else
        VarI1D = LinSequence( SeqStart=1, SeqEnd=size(VarR2D,2) )
      end if
      This%NbDataSets = size(VarI1D,1)

      allocate(This%ResponseData(This%NbNodes,This%NbDataSets), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ResponseData', ProcName=ProcName, stat=StatLoc )

      i = 1
      do i = 1, size(VarI1D,1)
        This%ResponseData(:,i) = VarR2D(:,VarI1D(i))
      end do

      deallocate(VarI1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI1D', ProcName=ProcName, stat=StatLoc )

      deallocate(VarR2D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )

      This%DataDefined = .true.

      nullify(InputSection)
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput

    class(Response_Type), intent(in)                                  ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

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
    if ( len_trim(This%Name) /= 0 ) call GetInput%AddParameter( Name='name', Value=This%Name )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/coordinates'
    SectionName = 'coordinates'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%AddParameter( Name='labels', Value=ConvertToString(This%CoordinatesLabels), SectionName=SectionName )
    SubSectionName = 'values'
    call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
    call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,           &
                                                                                                              Mandatory=.true. )
    if ( ExternalFlag ) then
      FileName = DirectoryLoc // '/coordinates.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Input=InputSection, Array=This%Coordinates, RowMajor=.true., File=File )
    else
      call ExportArray( Input=InputSection, Array=This%Coordinates, RowMajor=.true. )
    end if
    nullify(InputSection)

    if ( This%DataDefined ) then
      SectionName = 'data'
      call GetInput%AddSection( SectionName=SectionName )

      call GetInput%AddParameter( Name='column', Value=ConvertToString(LinSequence(SeqStart=1,SeqEnd=This%NbDataSets)),           &
                                                                                                         SectionName=SectionName )

      SubSectionName = 'values'
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

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCoordsLabel_R1D( This, Label )

    real(rkp), allocatable, dimension(:)                              ::    GetCoordsLabel_R1D

    class(Response_Type), intent(in)                                  ::    This
    character(*), intent(in)                                          ::    Label

    character(*), parameter                                           ::    ProcName='GetCoordsLabel_R1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    i = 1
    ii = 0
    do i = 1, This%NbIndCoordinates
      if ( Label /= This%CoordinatesLabels(i)%GetValue() ) cycle
      ii = i
      exit
    end do

    if ( ii == 0 ) call Error%Raise( Line='Did not finding a coordinate with requested label', ProcName=ProcName )

    allocate(GetCoordsLabel_R1D, source=This%Coordinates(:,ii), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetCoordsLabel_R1D', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCoordsLabels_R2D( This, Labels )

    real(rkp), allocatable, dimension(:,:)                            ::    GetCoordsLabels_R2D

    class(Response_Type), intent(in)                                  ::    This
    type(String_Type), dimension(:), intent(in)                       ::    Labels

    character(*), parameter                                           ::    ProcName='GetCoordsLabels_R2D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbLabels
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    NbLabels = size(Labels)

    allocate(GetCoordsLabels_R2D(This%NbNodes,NbLabels), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetCoordsLabel_R2D', ProcName=ProcName, stat=StatLoc )

    iii = 1
    do iii = 1, NbLabels
      i = 1
      ii = 0
      do i = 1, This%NbIndCoordinates
        if ( Labels(iii)%GetValue() /= This%CoordinatesLabels(i)%GetValue() ) cycle
        ii = i
        exit
      end do
      if ( ii == 0 ) call Error%Raise( Line='Did not finding a coordinate with requested label', ProcName=ProcName )
      GetCoordsLabels_R2D(:,iii) = This%Coordinates(:,iii)
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCoords_R2D( This )

    real(rkp), allocatable, dimension(:,:)                            ::    GetCoords_R2D

    class(Response_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetCoords_R2D'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    allocate(GetCoords_R2D, source=This%Coordinates, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetCoords_R2D', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCoordsLabelPtr_R1D( This, Label )

    real(rkp), pointer, dimension(:)                                  ::    GetCoordsLabelPtr_R1D

    class(Response_Type), target, intent(in)                          ::    This
    character(*), intent(in)                                          ::    Label

    character(*), parameter                                           ::    ProcName='GetCoordsLabel_R1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    i = 1
    ii = 0
    do i = 1, This%NbIndCoordinates
      if ( Label /= This%CoordinatesLabels(i)%GetValue() ) cycle
      ii = i
      exit
    end do

    if ( ii == 0 ) call Error%Raise( Line='Did not finding a coordinate with requested label', ProcName=ProcName )

    GetCoordsLabelPtr_R1D => This%Coordinates(:,ii)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCoordsPtr_R2D( This )

    real(rkp), pointer, dimension(:,:)                                ::    GetCoordsPtr_R2D

    class(Response_Type), target, intent(in)                          ::    This

    character(*), parameter                                           ::    ProcName='GetCoordsPtr_R2D'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetCoordsPtr_R2D => This%Coordinates

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetData( This )

    real(rkp), allocatable, dimension(:,:)                            ::    GetData

    class(Response_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetData'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( This%NbDataSets < 1 ) call Error%Raise( Line='No data available', ProcName=ProcName )

    allocate(GetData, source=This%ResponseData, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetData', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDataPointer( This )

    real(rkp), dimension(:,:), pointer                                ::    GetDataPointer

    class(Response_Type), target, intent(in)                          ::    This

    character(*), parameter                                           ::    ProcName='GetDataPointer'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( This%NbDataSets < 1 ) call Error%Raise( Line='No data available', ProcName=ProcName )

    GetDataPointer => This%ResponseData

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCoordinateLabels( This )

    type(String_Type), allocatable, dimension(:)                      ::    GetCoordinateLabels

    class(Response_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetCoordinateLabels'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    allocate(GetCoordinateLabels, source=This%CoordinatesLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetCoordinatesLabels', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel( This )

    character(:), allocatable                                         ::    GetLabel

    class(Response_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetLabel'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetLabel = This%Label

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This )

    character(:), allocatable                                         ::    GetName

    class(Response_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetName'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbDataSets( This )

    integer                                                           ::    GetNbDataSets

    class(Response_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetNbDataSets'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetNbDataSets = This%NbDataSets

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbIndCoordinates( This )

    integer                                                           ::    GetNbIndCoordinates

    class(Response_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetNbIndCoordinates'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetNbIndCoordinates = This%NbIndCoordinates

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbNodes( This )

    integer                                                           ::    GetNbNodes

    class(Response_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetNbNodes'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetNbNOdes = This%NbNodes

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsDataDefined( This )

    logical                                                           ::    IsDataDefined

    class(Response_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='IsDataDefined'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    IsDataDefined = .false.
    if ( This%NbDataSets > 0 ) IsDataDefined = .true.

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(Response_Type), intent(out)                                 ::    LHS
    class(Response_Type), intent(in)                                  ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (Response_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Name = RHS%Name
          LHS%Label = RHS%Label
          LHS%NbDataSets = RHS%NbDataSets
          LHS%DataDefined = RHS%DataDefined
          LHS%NbNodes = RHS%NbNodes
          LHS%NbIndCoordinates = RHS%NbIndCoordinates
          allocate(LHS%ResponseData, source=RHS%ResponseData, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%ResponseData', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%CoordinatesLabels, source=RHS%CoordinatesLabels, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%CoordinatesLabels', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%Coordinates, source=RHS%Coordinates, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Coordinates', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(Response_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%Coordinates) ) deallocate(This%Coordinates, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Coordinates', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%CoordinatesLabels) ) deallocate(This%CoordinatesLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CoordinatesLabels', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ResponseData) ) deallocate(This%ResponseData, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ResponseData', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
