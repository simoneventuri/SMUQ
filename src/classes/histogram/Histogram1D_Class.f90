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

module Histogram1D_Class

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

implicit none

private

public                                                                ::    Histogram1D_Type
public                                                                ::    BinValues

type                                                                  ::    Histogram1D_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbBins
  real(rkp), pointer, dimension(:)                                    ::    BinEdges=>null()
  integer, pointer, dimension(:)                                      ::    BinCounts=>null()
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  generic, public                                                     ::    Bin                     =>    Bin_R0D,                &
                                                                                                          Bin_R1D
  procedure, public                                                   ::    Bin_R0D
  procedure, public                                                   ::    Bin_R1D
  procedure, public                                                   ::    GetBinCounts
  procedure, public                                                   ::    GetBinCountsPointer
  procedure, public                                                   ::    GetBinEdges
  procedure, public                                                   ::    GetBinEdgesPointer
  procedure, public                                                   ::    GetNbBins
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

interface BinValues
  module procedure                                                    ::    BinValues_R0D
  module procedure                                                    ::    BinValues_R1D
end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(Histogram1D_Type), intent(inout)                            ::    This
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

    class(Histogram1D_Type), intent(inout)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    if ( associated(This%BinEdges) ) deallocate(This%BinEdges, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%BinEdges', ProcName=ProcName, stat=StatLoc )

    if ( associated(This%BinCounts) ) deallocate(This%BinCounts, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%BinCounts', ProcName=ProcName, stat=StatLoc )

    This%NbBins = 0

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(Histogram1D_Type), intent(inout)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(Histogram1D_Type), intent(inout)                            ::    This
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
    integer, allocatable, dimension(:)                                ::    VarI1D
    type(InputSection_Type), pointer                                  ::    InputSection=>null()

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()
    
    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix
    
    ParameterName = 'name'
    call Input%GetValue( Value=VarC0D, ParameterName=Parametername, Mandatory=.false., Found=Found )
    if ( Found ) This%Name = VarC0D

    SectionName = 'bin_edges'
    ParameterName = 'source'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
    SubSectionName = SectionName // '>source'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
    select case (VarC0D)
      case('computed')
        This%BinEdges = LinSpaceVec( Input=InputSection )
      case('imported')
        call ImportArray( Input=InputSection, Array=VarR1D, Prefix=PrefixLoc )
        allocate(This%BinEdges, source=VarR1D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='This%BinEdges', ProcName=ProcName, stat=StatLoc )
        deallocate(VarR1D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
      case default
        call Error%Raise( Line='Source not recognized', ProcName=ProcName )
    end select

    This%NbBins = size(This%BinEdges,1)-1

    SectionName = 'bin_counts'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=VarI1D, Prefix=PrefixLoc )
      allocate(This%BinCounts, source=VarI1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%BinCounts', ProcName=ProcName, stat=StatLoc )
      deallocate(VarI1D, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI1D', ProcName=ProcName, stat=StatLoc )
      if ( size(This%BinCounts,1) /= This%NbBins ) call Error%Raise( 'Mismatch between size of bincounts and number of bins',     &
                                                                                                               ProcName=ProcName )
    else
      allocate(This%BinCounts(This%NbBins), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%BinCounts', ProcName=ProcName, stat=StatLoc )
      This%BinCounts = 0
    end if

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(Histogram1D_Type), intent(in)                                  ::    This
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

    if ( len_trim(This%Name) /= 0 ) call GetInput%AddParameter( Name='name', Value=This%Name )

    SectionName = 'bin_edges'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%AddParameter( Name='source', Value='imported', SectionName=SectionName )
    SubSectionName = 'source'
    call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
    call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,           &
                                                                                                              Mandatory=.true. )
    if ( ExternalFlag ) then
      FileName = DirectoryLoc // '/bin_edges.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Input=InputSection, Array=This%BinEdges, File=File )
    else
      call ExportArray( Input=InputSection, Array=This%BinEdges )
    end if
    nullify(InputSection)

    SectionName = 'bin_counts'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    if ( ExternalFlag ) then
      FileName = DirectoryLoc // '/bin_counts.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Input=InputSection, Array=This%BinCounts, File=File )
    else
      call ExportArray( Input=InputSection, Array=This%BinCounts )
    end if
    nullify(InputSection)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Bin_R0D( This, Value, Debug )

    class(Histogram1D_Type), intent(inout)                            ::    This
    real(rkp), intent(in)                                             ::    Value
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='BinR0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    This%BinCounts = 0

    call BinValues( Value=Value, BinEdges=This%BinEdges, BinCounts=This%BinCounts )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Bin_R1D( This, Values, Debug )

    class(Histogram1D_Type), intent(inout)                            ::    This
    real(rkp), dimension(:), intent(in)                               ::    Values
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='BinR1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    This%BinCounts = 0

    call BinValues( Values=Values, BinEdges=This%BinEdges, BinCounts=This%BinCounts )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine BinValues_R0D( Value, BinEdges, BinCounts, Debug )

    real(rkp), intent(in)                                             ::    Value
    real(rkp), dimension(:), intent(in)                               ::    BinEdges
    integer, dimension(:), intent(inout)                              ::    BinCounts
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='BinValue'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    integer                                                           ::    NbBins
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( size(BinCounts) /= size(BinEdges)-1 ) call Error%Raise( 'Mismatch between bin counts and number of bins',                &
                                                                                                               ProcName=ProcName )

    NbBins = size(BinCounts)

    if ( Value < BinEdges(1) ) then
      BinCounts(1) = BinCounts(1) + 1
    elseif (Value > BinEdges(NbBins+1)) then
      BinCounts(NbBins) = BinCounts(NbBins) + 1
    else
      i = 1
      do i = 1, NbBins
        if ( Value < BinEdges(i+1) .and. Value >= BinEdges(i) ) then
          BinCounts(i) = BinCounts(i) + 1
          exit
        end if
      end do
    end if

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine BinValues_R1D( Values, BinEdges, BinCounts, Debug )

    real(rkp), dimension(:), intent(in)                               ::    Values
    real(rkp), dimension(:), intent(in)                               ::    BinEdges
    integer, dimension(:), intent(inout)                              ::    BinCounts
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='BinValues'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbBins
    integer                                                           ::    NbValues
    integer                                                           ::    i
    integer                                                           ::    ii

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( size(BinCounts) /= size(BinEdges)-1 ) call Error%Raise( 'Mismatch between bin counts and number of bins',                &
                                                                                                               ProcName=ProcName )

    NbBins = size(BinCounts)
    NbValues = size(Values)

    ii = 1
    do ii = 1, NbValues
      if ( Values(ii) < BinEdges(1) ) then
        BinCounts(1) = BinCounts(1) + 1
      elseif ( Values(ii) > BinEdges(NbBins+1)) then
        BinCounts(NbBins) = BinCounts(NbBins) + 1
      else
        i = 1
        do i = 1, NbBins
          if ( Values(ii) < BinEdges(i+1) .and. Values(ii) >= BinEdges(i) ) then
            BinCounts(i) = BinCounts(i) + 1
            exit
          end if
        end do
      end if
    end do

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetBinEdges( This, Debug )

    real(rkp), allocatable, dimension(:)                              ::    GetBinEdges

    class(Histogram1D_Type), intent(in)                               ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetBinEdges'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    allocate(GetBinEdges, source=This%BinEdges, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetBinEdges', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetBinEdgesPointer( This, Debug )

    real(rkp), pointer, dimension(:)                                  ::    GetBinEdgesPointer

    class(Histogram1D_Type), intent(in)                               ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetBinEdgesPointer'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetBinEdgesPointer => This%BinEdges

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetBinCounts( This, Debug )

    integer, allocatable, dimension(:)                                ::    GetBinCounts

    class(Histogram1D_Type), intent(in)                               ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetBinCounts'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    allocate(GetBinCounts, source=This%BinCounts, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetBinCounts', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetBinCountsPointer( This, Debug )

    integer, pointer, dimension(:)                                    ::    GetBinCountsPointer

    class(Histogram1D_Type), intent(in)                               ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetBinCountsPointer'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetBinCountsPointer => This%BinCounts

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName

    class(Histogram1D_Type), intent(in)                               ::    This
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
  function GetNbBins( This, Debug )

    integer                                                           ::    GetNbBins

    class(Histogram1D_Type), intent(in)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNbBins'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    GetNbBins = This%NbBins

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(Histogram1D_Type), intent(out)                              ::    LHS
    class(Histogram1D_Type), intent(in)                               ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (Histogram1D_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Name = RHS%Name
          LHS%NbBins = RHS%NbBins
          allocate(LHS%BinEdges, source=RHS%BinEdges, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%BinEdges', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%BinCounts, source=RHS%BinCounts, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%BinCounts', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(Histogram1D_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( associated(This%BinEdges) ) deallocate(This%BinEdges, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%BinEdges', ProcName=ProcName, stat=StatLoc )

    if ( associated(This%BinCounts) ) deallocate(This%BinCounts, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%BinCounts', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
