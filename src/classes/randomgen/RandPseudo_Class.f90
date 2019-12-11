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

module RandPseudo_Class

use Parameters_Library
use Input_Library
use ArrayIORoutines_Module
use StringRoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MT64_Class                                                    ,only:    MT64_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    RandPseudo_Type

type                                                                  ::    RandPseudo_Type
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    Name
  integer(8)                                                          ::    Seed=1
  integer(8)                                                          ::    SeedDefault=1
  type(MT64_Type)                                                     ::    PRNG
  integer                                                             ::    DrawType=2
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Draw
  procedure, public                                                   ::    DrawVec
  procedure, public                                                   ::    DrawMat
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize( This )

    class(RandPseudo_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    integer(8)                                                        ::    SysTimeCount

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'pseudo'

      call system_clock( SysTimeCount )
      This%SeedDefault = SysTimeCount

      call This%SetDefaults()
    end if

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset( This )

    class(RandPseudo_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    call This%Initialize()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults(This)

    class(RandPseudo_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%Seed = This%SeedDefault
    call This%PRNG%init_genrand64(This%Seed)
    This%DrawType=2

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput ( This, Input, Prefix )

    class(RandPseudo_Type), intent(inout)                             ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    logical                                                           ::    Found
    character(:), allocatable                                         ::    VarC0D
    logical                                                           ::    VarL0D
    integer                                                           ::    VarI0D
    integer(8), allocatable, dimension(:)                             ::    VarI1D_8
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'seed'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then 
      This%Seed = ConvertToInteger8(String=VarC0D)
      call This%PRNG%init_genrand64(This%Seed)
    end if

    SectionName = 'internal_generator'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      ParameterName = 'mti'
      call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName )
      This%PRNG%mti = VarI0D

      SubSectionName = SectionName // '>mt'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=VarI1D_8, Prefix=PrefixLoc )
      nullify( InputSection )
      This%PRNG%mt = VarI1D_8
      deallocate(VarI1D_8, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI1D_8', ProcName=ProcName, stat=StatLoc )
    end if

    This%Constructed = .true.

  end subroutine 
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructCase1 ( This, Seed )

    class(RandPseudo_Type), intent(inout)                             ::    This
    integer(8), optional, intent(in)                                  ::    Seed

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    if ( present(seed) ) then
      This%Seed = Seed
      call This%PRNG%init_genrand64(This%Seed)
    end if

    This%Constructed = .true.

  end subroutine 
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput
    class(RandPseudo_Type), intent(in)                                ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    FileName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    type(SMUQFile_Type)                                               ::    File
    character(:), allocatable                                         ::    VarC0D

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput%AddParameter( Name='seed', Value=ConvertToString( Value=This%Seed ) )

    SectionName = 'internal_generator'
    call GetInput%AddSection( SectionName=SectionName )

    call GetInput%AddParameter( Name='mti', Value=ConvertToString(Value=This%PRNG%mti), SectionName=SectionName )

    SubSectionName = 'mt'
    call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
    SubSectionName = SectionName // '>' // SubSectionName
    call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
    if ( ExternalFlag ) then
      FileName = DirectoryLoc // '/mt.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Input=InputSection, Array=This%PRNG%mt, File=File )
    else
      call ExportArray( Input=InputSection, Array=This%PRNG%mt )
    end if
    nullify(InputSection)

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Draw( This, DrawType )

    real(rkp)                                                         ::    Draw

    class(RandPseudo_Type), intent(inout)                             ::    This
    integer, optional, intent(in)                                     ::    DrawType

    character(*), parameter                                           ::    ProcName='Draw'
    integer                                                           ::    DrawTypeLoc

    if ( .not. This%Initialized ) call This%Initialize()

    if ( present(DrawType) ) then 
      DrawTypeLoc = DrawType
    else
      DrawTypeLoc = This%DrawType
    end if

    select case ( DrawTypeLoc )
      case (1)
        Draw = real(This%PRNG%genrand64_real1(),rkp)
      case (2)
        Draw = real(This%PRNG%genrand64_real2(),rkp)
      case (3)
        Draw = real(This%PRNG%genrand64_real3(),rkp)
      case default
        call Error%Raise( Line='Something went wrong when selecting draw type', ProcName=ProcName )
    end select

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function DrawVec( This, Size1, DrawType )

    real(rkp), allocatable, dimension(:)                              ::    DrawVec

    class(RandPseudo_Type), intent(inout)                             ::    This
    integer, intent(in)                                               ::    Size1
    integer, optional, intent(in)                                     ::    DrawType

    character(*), parameter                                           ::    ProcName='DrawVec'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    DrawTypeLoc

    if ( Size1 <= 0 ) call Error%Raise( Line='Size 1 of requested sample at or below 0', ProcName=ProcName )

    allocate( DrawVec(Size1), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='DrawSample_1D', ProcName=ProcName, stat=StatLoc )

    if ( present(DrawType) ) then 
      DrawTypeLoc = DrawType
    else
      DrawTypeLoc = This%DrawType
    end if

    i = 1
    do i = 1, Size1
      DrawVec(i) = This%Draw( DrawType=DrawTypeLoc )
    end do

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function DrawMat( This, Size1, Size2, DrawType )

    real(rkp), allocatable, dimension(:,:)                            ::    DrawMat

    class(RandPseudo_Type), intent(inout)                             ::    This
    integer, intent(in)                                               ::    Size1
    integer, intent(in)                                               ::    Size2
    integer, optional, intent(in)                                     ::    DrawType

    character(*), parameter                                           ::    ProcName='DrawMat'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    DrawTypeLoc

    if ( Size1 <= 0 ) call Error%Raise( Line='Size 1 of requested sample at or below 0', ProcName=ProcName )

    if ( Size2 <= 0 ) call Error%Raise( Line='Size 2 of requested samples at or below 0', ProcName=ProcName )

    allocate(DrawMat(Size1, Size2), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='DrawMat', ProcName=ProcName, stat=StatLoc )

    if ( present(DrawType) ) then 
      DrawTypeLoc = DrawType
    else
      DrawTypeLoc = This%DrawType
    end if

    i = 1
    do i = 1, Size2
      DrawMat(:,i) = This%DrawVec( Size1=Size1, DrawType=DrawTypeLoc )
    end do

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  impure elemental subroutine Copy( LHS, RHS )

    class(RandPseudo_Type), intent(out)                               ::    LHS
    class(Randpseudo_Type), intent(in)                                ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    call LHS%Reset()
    LHS%Initialized = RHS%Initialized
    LHS%Constructed = RHS%Constructed

    if ( RHS%Constructed ) then
      LHS%Seed = RHS%Seed
      LHS%SeedDefault = RHS%SeedDefault
      LHS%PRNG = RHS%PRNG
      LHS%DrawType = RHS%DrawType
    end if

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  impure elemental subroutine Finalizer( This )

    type(RandPseudo_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
