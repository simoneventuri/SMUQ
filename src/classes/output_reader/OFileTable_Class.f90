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

module OFileTable_Class

use Input_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use StringRoutines_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use OFileFormated_Class                                           ,only:    OFileFormated_Type
use Output_Class                                                  ,only:    Output_Type
use String_Library

implicit none

private

public                                                                ::    OFileTable_Type

type, extends(OFileFormated_Type)                                     ::    OFileTable_Type
  type(SMUQFile_Type)                                                 ::    OutputFile
  integer                                                             ::    OutputColumn
  integer                                                             ::    AbscissaColumn
  real(rkp), allocatable, dimension                                   ::    InterpolationNodes
  logical                                                             ::    Interpolated
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    GetOutput
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(OFileTable_Type), intent(inout)                             ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'ofiletable'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(OFileTable_Type), intent(inout)                             ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    call This%OutputFile%Reset()

    if ( allocated(This%InterpolationNodes) ) deallocate(This%InterpolationNodes, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%InterpolationNodes', ProcName=ProcName, stat=StatLoc )

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(OFileTable_Type), intent(inout)                             ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%AbscissaColumn = 0
    This%OutputColumn = 0
    This%Interpolated = .false.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    use String_Library

    class(OFileTable_Type), intent(inout)                             ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'abscissa_column'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.true. )
    This%AbscissaColumn = VarI0D

    ParameterName = 'output_column'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.true. )
    This%OutputColumn = VarI0D

    SectionName = 'file'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%OutputFile%Construct( Input=InputSection, Prefix=PrefixLoc )
    nullify(InputSection)

    SectionName = 'interpolation_nodes'

    if ( Input%HasSection( SubSectionName = SectionName ) then
      This%Interpolated = .true.
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
      allocate(This%InterpolationNodes, source=VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%InterpolationNodes', ProcName=ProcName, stat=StatLoc )
      deallocate(VarR1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
    end if

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(OFileTable_Type), intent(in)                                ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i
    character(:), allocatable                                         ::    FileName
    type(SMUQFile_Type)                                               ::    File

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/file'
    call GetInput%AddSection( Section=This%OutputFile%GetInput( MainSectionName='file', Prefix=PrefixLoc, Directory=DirectorySub))

    call GetInput%AddParameter( Name='abscissa_column', Value=Convert_To_String(This%AbscissaColumn) )
    call GetInput%AddParameter( Name='output_column', Value=Convert_To_String(This%OutputColumn) )

    if ( This%Interpolated ) then
      SectionName = 'interpolation_nodes'
      call GetInput%AddParameter( Name='source', Value='values', SectionName=SectionName )

      call GetInput%AddSection( SectionName=SectionName )
      SubSectionName = 'source'
      call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
      call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,           &
                                                                                                                Mandatory=.true. )
      if ( ExternalFlag ) then
        FileName = DirectoryLoc // '/interpolation_nodes.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Input=InputSection, Array=This%InterpolationNodes, File=File )
      else
        call ExportArray( Input=InputSection, Array=This%InterpolationNodes )
      end if
      nullify(InputSection)
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ReadOutput( This, Values, Debug )

    class(OFileTable_Type), intent(in)                                ::    This
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Values
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ReadOutput'
    integer                                                           ::    StatLoc=0
    type(String_Type), allocatable, dimension(:,:)                    ::    Strings
    real(rkp), allocatable, dimension(:)                              ::    TableOutput
    real(rkp), allocatable, dimension(:)                              ::    Abscissa
    real(rkp), allocatable, dimension(:)                              ::    InterpolatedOutput
    integer                                                           ::    NbLines
    integer                                                           ::    i
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    call ImportArray( Array=Strings, File=This%OutputFile )

    NbLines = size(Strings,2)

    allocate(TableOutput(NbLines), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='TableOutput', ProcName=ProcName, stat=StatLoc )
    allocate(Abscissa(NbLines), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Abscissa', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbLines
      TableOutput(i) = ConvertToRealrkp( String=Strings(This%OutputColumn,i)%GetValue() )
      Abscissa(i) = ConvertToRealrkp( String=Strings(This%AbscissaColumn,i)%GetValue() )
    end do

    if ( This%Interpolated ) then
      Values = Interpolate( Abscissa=Abscissa, Ordinate=TableOutput, Nodes=This%InterpolationNodes )
    else
      Values = TableOutput
    end if

    deallocate(Strings, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Strings', ProcName=ProcName, stat=StatLoc )

    deallocate(Abscissa, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Abscissa', ProcName=ProcName, stat=StatLoc )

    deallocate(TableOutput, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='TableOutput', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(OFileTable_Type), intent(out)                               ::    LHS
    class(OFileFormated_Type), intent(in)                             ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (OFileTable_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%OutputFile = RHS%OutputFile
          LHS%AbscissaColumn = RHS%AbscissaColumn
          LHS%OutputColumn = RHS%OutputColumn
          LHS%Interpolated = RHS%Interpolated
          if ( LHS%Interpolated ) then
            allocate(LHS%InterpolationNodes, source=RHS%InterpolationNodes, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%InterpolationNodes', ProcName=ProcName, stat=StatLoc )
          end if
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(OFileTable_Type),intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%OutputColumn) ) deallocate(This%OutputColumn, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%OutputColumn', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%OutputLabel) ) deallocate(This%OutputLabel, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%OutputLabel', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
