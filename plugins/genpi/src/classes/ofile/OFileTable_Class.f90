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
  integer                                                             ::    NbOutputs=0
  integer                                                             ::    AbscissaColumn=0
  integer, allocatable, dimension(:)                                  ::    OutputColumn
  type(String_Type), allocatable, dimension(:)                        ::    OutputLabel
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    GetNbOutputs
  procedure, public                                                   ::    GetOutput
  procedure, public                                                   ::    GetOutputLabels
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

    if ( allocated(This%OutputColumn) ) deallocate(This%OutputColumn, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%OutputColumn', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%OutputLabel) ) deallocate(This%OutputLabel, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%OutputLabel', ProcName=ProcName, stat=StatLoc )

    This%NbOutputs = 0

    call This%OutputFile%Reset()

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
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'file'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%OutputFile%Construct( Input=InputSection, Prefix=PrefixLoc )
    nullify(InputSection)

    ParameterName = 'abscissa_column'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.true. )
    This%AbscissaColumn = VarI0D


    SectionName = 'outputs'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbOutputs = InputSection%GetNumberofSubSections()

    allocate(This%OutputColumn(This%NbOutputs), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%OutputColumn', ProcName=ProcName, stat=StatLoc )

    allocate(This%OutputLabel(This%NbOutputs), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%OutputLabel', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbOutputs
      SubSectionName = SectionName // '>output' // Convert_To_String(i)
      ParameterName = 'column'
      call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      This%OutputColumn(i) = VarI0D
      ParameterName = 'label'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      This%OutputLabel(i) = VarC0D
    end do
    
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

    call GetInput%AddParameter( Name='nb_outputs', Value=Convert_To_String(This%NbOutputs) )

    SectionName = 'outputs'

    i = 1
    do i = 1, This%NbOutputs
      SubSectionName = 'output' // Convert_To_String(i)
      call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
      SubSectionName = SectionName // '>' // SubSectionName
      call GetInput%AddParameter( Name='column', Value=ConvertToString(Value=This%OutputColumn(i)), SectionName=SubSectionName )
      call GetInput%AddParameter( Name='label', Value=This%OutputLabel(i)%GetValue(), SectionName=SubSectionName )
    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbOutputs( This, Debug )

    integer                                                           ::    GetNbOutputs

    class(OFileTable_Type), intent(in)                                ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNbOutputs'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbOutputs = This%NbOutputs

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetOutput( This, Output, Debug )

    class(OFileTable_Type), intent(inout)                             ::    This
    type(Output_Type), dimension(:), intent(inout)                    ::    Output
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetOutput'
    integer                                                           ::    StatLoc=0
    type(String_Type), allocatable, dimension(:,:)                    ::    Strings
    real(rkp), allocatable, dimension(:)                              ::    Abscissa
    real(rkp), allocatable, dimension(:)                              ::    TableOutput
    integer                                                           ::    NbLines
    integer                                                           ::    i, ii
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    call ImportArray( Array=Strings, File=This%OutputFile )

    NbLines = size(Strings,2)

    allocate(Abscissa(NbLines), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Abscissa', ProcName=ProcName, stat=StatLoc )

    allocate(TableOutput(NbLines), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='TableOutput', ProcName=ProcName, stat=StatLoc )

    if ( size(Output,1) /= This%NbOutputs ) call Error%Raise( Line='Passed down an incorrect size Output', ProcName=ProcName )

    i = 1
    do i = 1, NbLines
      Abscissa(i) = ConvertToRealrkp( String=Strings(This%AbscissaColumn,i)%GetValue() )
    end do

    ii = 1
    do ii = 1, This%NbOutputs
      i = 1
      do i = 1, NbLines
        TableOutput(i) = ConvertToRealrkp( String=Strings(This%OutputColumn(ii),i)%GetValue() )
      end do
      call Output(ii)%Construct( Abscissa=Abscissa, Ordinate=TableOutput, Label=This%OutputLabel(ii)%GetValue() )
    end do

    deallocate(Strings, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Strings', ProcName=ProcName, stat=StatLoc )

    deallocate(TableOutput, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='TableOutput', ProcName=ProcName, stat=StatLoc )

    deallocate(Abscissa, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Abscissa', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------


  !!------------------------------------------------------------------------------------------------------------------------------
  function GetOutputLabels( This, Debug )

    type(String_Type), allocatable, dimension(:)                      ::    GetOutputLabels

    class(OFileTable_Type), intent(in)                                ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetOutputLabels'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetOutputLabels, source=This%OutputLabel, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetOutputLabels', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
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
          LHS%NbOutputs = RHS%NbOutputs
          LHS%AbscissaColumn = RHS%AbscissaColumn
          LHS%OutputFile = RHS%OutputFile
          allocate(LHS%OutputColumn, source=RHS%OutputColumn, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%OutputColumn', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%OutputLabel, source=RHS%OutputLabel, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%OutputLabel', ProcName=ProcName, stat=StatLoc )
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
