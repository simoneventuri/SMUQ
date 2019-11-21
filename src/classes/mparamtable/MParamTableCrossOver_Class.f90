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

module MParamTableCrossOver_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use String_Library
use ArrayRoutines_Module
use ArrayIORoutines_Module
use StringRoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MParamTable_Class                                             ,only:    MParamTable_Type
use MParamTablePoly_Class                                         ,only:    MParamTablePoly_Type
use InputDet_Class                                                ,only:    InputDet_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type



implicit none

private

public                                                                ::    MParamTableCrossOver_Type

type, extends(MParamTable_Type)                                       ::    MParamTableCrossOver_Type
  type(MParamTablePoly_Type)                                          ::    PolyParam
  real(rkp), allocatable, dimension(:,:)                              ::    OriginalTable
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    GetValue
  procedure, public                                                   ::    GetCharValue
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(MParamTableCrossOver_Type), intent(inout)                   ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'mparamtablecrossover'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(MParamTableCrossOver_Type), intent(inout)                   ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%OriginalTable) ) deallocate(This%OriginalTable, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%OriginalTable', ProcName=ProcName, stat=StatLoc )

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(MParamTableCrossOver_Type), intent(inout)                   ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(MParamTableCrossOver_Type), intent(inout)                   ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    integer                                                           ::    i
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    integer                                                           ::    ParamColumn
    integer                                                           ::    AbscissaColumn
    type(String_Type), allocatable, dimension(:,:)                    ::    VarR2D
    

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'original_values'

    SubSectionName = SectionName // '>values'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
    call ImportArray( Input=InputSection, Array=VarR2D, Prefix=PrefixLoc )
    nullify(InputSection)

    allocate(This%OriginalTable(size(VarR2D,2),2), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%OriginalTable', ProcName=ProcName, stat=StatLoc )

    ParamColumn = 2
    ParameterName = 'parameter_column'
    call Input%GetValue( Value=VarI0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( Found ) ParamColumn = VarI0D

    AbscissaColumn = 1
    ParameterName = 'abscissa_column'
    call Input%GetValue( Value=VarI0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( Found ) AbscissaColumn = VarI0D

    if ( ParamColumn > size(VarR2D,1) ) call Error%Raise( Line='Specified parameter column is greater than number of columns',    &
                                                                                                               ProcName=ProcName )

    if ( AbscissaColumn > size(VarR2D,1) ) call Error%Raise( Line='Specified abscissa column is greater than number of columns',  &
                                                                                                               ProcName=ProcName ) 

    if ( AbscissaColumn == ParamColumn ) call Error%Raise( Line='Abscissa and parameter columns set to be the same',              &
                                                                                                               ProcName=ProcName )  

    i = 1
    do i = 1, size(VarR2D,2)
      This%OriginalTable(i,1) = ConvertToRealrkp( String=VarR2D(AbscissaColumn,i)%GetValue() )
      This%OriginalTable(i,2) = ConvertToRealrkp( String=VarR2D(ParamColumn,i)%GetValue() )
    end do

    deallocate(VarR2D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )

    SectionName = 'polynomial'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%PolyParam%Construct( Input=InputSection )
    nullify(InputSection)

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(MParamTableCrossOver_Type), intent(in)                      ::    This
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
    character(:), allocatable                                         ::    FileName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
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

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    SectionName = 'original_values'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%AddParameter( Name='abscissa_column', Value='1', SectionName=SectionName )
    call GetInput%AddParameter( Name='parameter_column', Value='2', SectionName=SectionName )

    SubSectionName = 'values'
    call GetInput%AddSection( SectionName=SubSectionName )
    SubSectionName = SectionName // '>values'
    call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
    if ( ExternalFlag ) then
      FileName = DirectoryLoc // '/values.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Input=InputSection, Array=transpose(This%OriginalTable), File=File )
    else
      call ExportArray( Input=InputSection, Array=transpose(This%OriginalTable) )
    end if

    SectionName = 'polynomial'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/polynomial'
    call GetInput%AddSection( Section=This%PolyParam%GetInput(MainSectionName=SectionName, Prefix=PrefixLoc,                      &
                                                                                                         Directory=DirectorySub) )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetValue( This, Input, Abscissa, Debug )

    real(rkp), allocatable, dimension(:)                              ::    GetValue

    class(MParamTableCrossOver_Type), intent(in)                      ::    This
    type(InputDet_Type), intent(in)                                   ::    Input
    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetValue'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    real(rkp), allocatable, dimension(:)                              ::    PolyVal
    real(rkp), allocatable, dimension(:)                              ::    TableVal
    logical                                                           ::    TripFlag=.false.

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetValue(size(Abscissa,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetValue', ProcName=ProcName, stat=StatLoc )

    GetValue = Zero
    
    allocate(TableVal, source=Interpolate(Abscissa=This%OriginalTable(:,1), Ordinate=This%OriginalTable(:,2), Nodes=Abscissa),    &
                                                                                                                     stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    allocate(Polyval, source=This%PolyParam%GetValue(Input=Input, Abscissa=Abscissa), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Polyval', ProcName=ProcName, stat=StatLoc )

    TripFlag = .false.
    GetValue = Zero

    i = 1
    do i = 1, size(Abscissa,1)
      if ( TripFlag ) then
        GetValue(i) = TableVal(i)
      else
        if ( TableVal(i) > PolyVal(i) ) then
          GetValue(i) = TableVal(i)
          TripFlag = .true.
        else
          GetValue(i) = PolyVal(i)
        end if
      end if
    end do

    deallocate(TableVal, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='TableVal', ProcName=ProcName, stat=StatLoc )

    deallocate(PolyVal, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='PolyVal', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCharValue( This, Input, Abscissa, Format, Debug )

    use String_Library
    use StringRoutines_Module
    use ComputingRoutines_Module
    use Input_Class                                               ,only:    Input_Type

    type(String_Type), allocatable, dimension(:)                      ::    GetCharValue

    class(MParamTableCrossOver_Type), intent(in)                      ::    This
    type(InputDet_Type), intent(in)                                   ::    Input
    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetValue'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    real(rkp), allocatable, dimension(:)                              ::    PolyVal
    real(rkp), allocatable, dimension(:)                              ::    TableVal
    logical                                                           ::    TripFlag=.false.
    character(:), allocatable                                         ::    FormatLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    allocate(GetCharValue(size(Abscissa,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetValue', ProcName=ProcName, stat=StatLoc )
    
    allocate(TableVal, source=Interpolate(Abscissa=This%OriginalTable(:,1), Ordinate=This%originalTable(:,2), Nodes=Abscissa),    &
                                                                                                                     stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    allocate(Polyval, source=This%PolyParam%GetValue(Input=Input, Abscissa=Abscissa), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Polyval', ProcName=ProcName, stat=StatLoc )

    TripFlag = .false.

    i = 1
    do i = 1, size(Abscissa,1)
      if ( TripFlag ) then
        call GetCharValue(i)%Set_Value( ConvertToString(Value=TableVal(i), Format=FormatLoc) )
      else
        if ( TableVal(i) > PolyVal(i) ) then
          call GetCharValue(i)%Set_Value( ConvertToString(Value=TableVal(i), Format=FormatLoc) )
          TripFlag = .true.
        else
          call GetCharValue(i)%Set_Value( ConvertToString(Value=PolyVal(i), Format=FormatLoc) )
        end if
      end if
    end do

    deallocate(TableVal, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='TableVal', ProcName=ProcName, stat=StatLoc )

    deallocate(PolyVal, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='PolyVal', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(MParamTableCrossOver_Type), intent(out)                     ::    LHS
    class(MParamTable_Type), intent(in)                               ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (MParamTableCrossOver_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if ( RHS%Constructed ) then
          LHS%OriginalTable = RHS%OriginalTable
          LHS%PolyParam = RHS%PolyParam
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(MParamTableCrossOver_Type), intent(inout)                    ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%OriginalTable) ) deallocate(This%OriginalTable, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%OriginalTable', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module