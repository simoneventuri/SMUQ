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

module OutputReader_Class

use Input_Library
use Parameters_Library
use StringRoutines_Module
use String_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Output_Class                                                  ,only:    Output_Type
use OFileFormated_Class                                           ,only:    OFileFormated_Type
use OFileFormated_Vec_Class                                       ,only:    OFileFormated_Vec_Type
use OFileFormated_Factory_Class                                   ,only:    OFileFormated_Factory
use LinkedList2D_Class                                            ,only:    LinkedList2D_Type 
implicit none

private

public                                                                ::    OutputReader_Type

type                                                                  ::    Cell_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    Label
  type(OFileFormated_Vec_Type), allocatable, dimension(:)             ::    OFile
  integer                                                             ::    NbOFiles
contains
  procedure, public                                                   ::    Initialize              =>    Initialize_Cell
  procedure, public                                                   ::    Reset                   =>    Reset_Cell
  procedure, public                                                   ::    SetDefaults             =>    SetDefaults_Cell
  generic, public                                                     ::    Construct               =>    ConstructInput_Cell
  procedure, private                                                  ::    ConstructInput_Cell
  procedure, public                                                   ::    GetInput                =>    GetInput_Cell
  procedure, public                                                   ::    ReadOutput              =>    ReadOutput_Cell
  generic, public                                                     ::    assignment(=)           =>    Copy_Cell
  procedure, public                                                   ::    Copy_Cell
  final                                                               ::    Finalizer_Cell
end type

type                                                                  ::    OutputReader_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  type(Cell_Type), allocatable, dimension(:)                          ::    Cells
  integer                                                             ::    NbCells
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    ReadOutput
  procedure, public                                                   ::    GetNbOutputs
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(OutputReader_Type), intent(inout)                           ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'output_reader'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(OutputReader_Type), intent(inout)                           ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%Cells) ) deallocate(This%Cells, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )

    This%NbCells = 0

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(OutputReader_Type), intent(inout)                           ::    This
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

    class(OutputReader_Type), intent(inout)                           ::    This
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
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'outputs'
    This%NbCells = InputSection%GetNumberofSubSections( Name=SectionName )

    allocate(This%Cells(This%NbCells), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbCells
      SubSectionName = SectionName // '>output' // ConvertToString(i)
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call This%Cells(i)%Construct( Input=InputSection, Prefix=PrefixLoc )
      nullify(InputSection)
    end do

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(OutputReader_Type), intent(in)                              ::    This
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
    class(OFileFormated_Type), pointer                                ::    OFile=>null()
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

    SectionName = 'outputs'
    call GetInput%AddSection( SectionName=SectionName )
    
    i = 1
    do i = 1, This%NbCells
      SubSectionName = 'output' // ConvertToString(Value=i)
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/output' // ConvertToString(Value=i)
      call GetInput%AddSection( Section=This%Cells(i)%GetInput( MainSectionName=SubSectionName, Prefix=PrefixLoc,                 &
                                                                             Directory=DirectorySub ), To_SubSection=SectionName )
    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ReadOutput( This, Output, Debug )

    class(OutputReader_Type), intent(inout)                           ::    This
    type(Output_Type), dimension(:), intent(inout)                    ::    Output
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ReadOutput'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( size(Output,1) /= This%NbCells ) call Error%Raise( Line='Passed down incorrect size Output array', ProcName=ProcName )

    i = 1
    do i = 1, This%NbCells
      call This%Cells(i)%ReadOutput( Output=Output(i) )
    end do

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbOutputs( This, Debug )

    integer                                                           ::    GetNbOutputs

    class(OutputReader_Type), intent(in)                              ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNbOutputs'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbOutputs = This%NbCells

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(OutputReader_Type), intent(out)                             ::    LHS
    class(OutputReader_Type), intent(in)                              ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (OutputReader_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%NbCells = RHS%NbCells
          allocate(LHS%Cells, source=RHS%Cells, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Cells', ProcName=ProcName, stat=StatLoc )
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(OutputReader_Type),intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%Cells) ) deallocate(This%Cells, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Cells', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_Cell( This, Debug )

    class(Cell_Type), intent(inout)                                   ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize_Cell'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'output_cell'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_Cell( This, Debug )

    class(Cell_Type), intent(inout)                                   ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset_Cell'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%OFile) ) deallocate(This%OFile, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%OFile', ProcName=ProcName, stat=StatLoc )

    This%NbOFiles = 0

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_Cell( This, Debug )

    class(Cell_Type), intent(inout)                                   ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults_Cell'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Label = '<undefined>'

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_Cell( This, Input, Prefix, Debug )

    use StringRoutines_Module
    use String_Library

    class(Cell_Type), intent(inout)                                   ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput_Cell'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(OFileFormated_Type), allocatable                            ::    OFile
    class(OFileFormated_Type), pointer                                ::    OFilePointer=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    integer                                                           ::    i


    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'label'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    This%Label = VarC0D

    SectionName = 'files'
    This%NbOFiles = InputSection%GetNumberofSubSections( Name=SectionName )

    allocate(This%OFile(This%NbOFiles), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%OFile', ProcName=ProcName, stat=StatLoc )
    
    i = 1
    do i = 1, This%NbOFiles
      SubSectionName = SectionName // '>file' // ConvertToString(i)
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call OFileFOrmated_Factory%Construct( Object=OFile, Input=InputSection, Prefix=PrefixLoc )
      call This%OFile(i)%Set( Object=OFile ) 
      deallocate(OFile, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='OFile', ProcName=ProcName, stat=StatLoc )
      nullify(InputSection)
    end do

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_Cell( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput_Cell

    class(Cell_Type), intent(in)                                      ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput_Cell'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    class(OFileFormated_Type), pointer                                ::    OFile=>null()
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

    call GetInput_Cell%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput_Cell%AddParameter( Name='label', Value=This%Label )

    SectionName = 'files'
    call GetInput_Cell%AddSection( SectionName=SectionName )
    
    i = 1
    do i = 1, This%NbOFiles
      SubSectionName = 'file' // ConvertToString(Value=i)
      OFile => This%OFile(i)%GetPointer()
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/file' // ConvertToString(Value=i)
      call GetInput_Cell%AddSection( Section=OFileFormated_Factory%GetObjectInput( MainSectionName=SubSectionName, Object=OFile,  &
                                                           Prefix=PrefixLoc, Directory=DirectorySub ), To_SubSection=SectionName )
      nullify(OFile)
    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ReadOutput_Cell( This, Output, Debug )

    class(Cell_Type), intent(inout)                                   ::    This
    type(Output_Type), intent(inout)                                  ::    Output
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ReadOutput_Cell'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii
    class(OFileFormated_Type), pointer                                ::    OFile=>null()
    type(LinkedList2D_Type)                                           ::    Outputs
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    real(rkp), dimension(:,:), pointer                                ::    VarR2DPointer=>null()
    integer                                                           ::    OutputSize
    integer                                                           ::    OutputNbDegen
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    OutputSize = 0
    i = 1
    do i = 1, This%NbOFiles
      OFile => This%OFile(i)%GetPointer()
      call OFile%ReadOutput( Values=VarR2D )
      if ( i == 1 ) OutputNbDegen = size(VarR2D,2)
      if ( OutputNbDegen /= size(VarR2D,2) ) call Error%Raise( 'Output has different number of realizations between files : ' //  &
                                                                                                   This%Label, ProcName=ProcName )
      OutputSize = OutputSize + size(VarR2D,1)
      call Outputs%Append( Values=VarR2D )
      nullify(OFile)
    end do

    allocate(VarR2D(OutputSize,OutputNbDegen), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    ii = 0
    i = 1
    do i = 1, This%NbOFiles
      call Outputs%GetPointer( Node=i, Values=VarR2DPointer )
      VarR2D(ii+1:ii+size(VarR2DPointer,1),:) = VarR2DPointer
      ii = ii + size(VarR2DPointer,1)
      nullify(VarR2DPointer)
    end do

    call Output%Construct( Values=VarR2D, Label=This%Label )

    call Outputs%Purge()

    deallocate(VarR2D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_Cell( LHS, RHS )

    class(Cell_Type), intent(out)                                     ::    LHS
    class(Cell_Type), intent(in)                                      ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (Cell_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%NbOFiles = RHS%NbOFiles
          allocate(LHS%OFile, source=RHS%OFile, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%OFile', ProcName=ProcName, stat=StatLoc )
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer_Cell( This )

    type(Cell_Type),intent(inout)                                     ::    This

    character(*), parameter                                           ::    ProcName='Finalizer_Cell'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%OFile) ) deallocate(This%OFile, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%OFile', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
