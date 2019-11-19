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

module MParamTablePoly_Class

use Input_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MParamTable_Class                                             ,only:    MParamTable_Type
use PolyCoeff_Class                                               ,only:    PolyCoeff_Type
use PolyCoeff_Vec_Class                                           ,only:    PolyCoeff_Vec_Type
use PolyCoeff_Factory_Class                                       ,only:    PolyCoeff_Factory
use PolyCoeffScalar_Class                                         ,only:    PolyCoeffScalar_Type
use InputDet_Class                                                ,only:    InputDet_Type
use String_Library
use StringRoutines_Module


implicit none

private

public                                                                ::    MParamTablePoly_Type

type, extends(MParamTable_Type)                                       ::    MParamTablePoly_Type
  type(PolyCoeff_Vec_Type), dimension(:), allocatable                 ::    PolyCoeff
  integer                                                             ::    Order
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
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

    class(MParamTablePoly_Type), intent(inout)                        ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'mparamtablepoly'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(MParamTablePoly_Type), intent(inout)                        ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%PolyCoeff) ) deallocate(This%PolyCoeff, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%PolyCoeff', ProcName=ProcName, stat=StatLoc )

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(MParamTablePoly_Type), intent(inout)                        ::    This
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

    class(MParamTablePoly_Type), intent(inout)                        ::    This
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
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    integer                                                           ::    i, ii
    logical                                                           ::    Found
    class(PolyCoeff_Type), allocatable                                ::    PolyCoeff
    type(PolyCoeffScalar_Type)                                        ::    PolyCoeffScalar
    type(InputSection_Type), pointer                                  ::    InputSection=>null()

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'order'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.true. )
    This%Order = VarI0D
    if ( This%Order < 0 ) call Error%Raise( Line='Specified a polynomial of order less than 0', ProcName=ProcName )

    allocate(This%PolyCoeff(This%Order+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='PolyCoeff', ProcName=ProcName, stat=StatLoc )

    call PolyCoeffScalar%Construct( Value=Zero )

    SectionName = 'coefficients'    

    i = 1
    ii = 0
    do i = 1, This%Order + 1
      SubSectionName = SectionName // '>coefficient' // Convert_To_String(i)
      if ( Input%HasSection( SubSectionName=SubSectionName ) ) then
        ii = ii + 1
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
        call PolyCoeff_Factory%Construct( Object=PolyCoeff, Input=InputSection, Prefix=PrefixLoc )
        call This%PolyCoeff(i)%Set( Object=PolyCoeff )
        deallocate(PolyCoeff, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='PolyCoeff', ProcName=ProcName, stat=StatLoc )
        nullify(InputSection)
      else
        call This%PolyCoeff(i)%Set( Object=PolyCoeffScalar )
      end if
    end do

    if ( ii <= 0 ) call Error%Raise( Line='Specified polynomial but no coefficients were given', ProcName=ProcName )

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, PolyCoeff, Debug )

    class(MParamTablePoly_Type), intent(inout)                        ::    This
    class(PolyCoeff_Vec_Type), dimension(:), intent(in)               ::    PolyCoeff
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    allocate(This%PolyCoeff, source=PolyCoeff, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%PolyCoeff', ProcName=ProcName, stat=StatLoc )

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(MParamTablePoly_Type), intent(in)                           ::    This
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
    integer                                                           ::    NbCoeffs
    class(PolyCoeff_Type), pointer                                    ::    PolyCoeffPointer=>null()

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
    call GetInput%AddParameter( Name='order', Value=ConvertToString( Value=This%Order ) )

    SectionName = 'coefficients'
    i = 1
    do i = 1, size(This%PolyCoeff,1)
      PolyCoeffPointer => This%PolyCoeff(i)%GetPointer()
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/coefficient' // ConvertToString(Value=i)
      SubSectionName = 'coefficient' // ConvertToString(Value=i)
      call GetInput%AddSection( Section=PolyCoeff_Factory%GetObjectInput( MainSectionName=SubSectionName, Object=PolyCoeffPointer,&
                                                           Prefix=PrefixLoc, Directory=DIrectoryLoc ), To_SubSection=SectionName )
      nullify(PolyCoeffPointer)
    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetValue( This, Input, Abscissa, Debug )

    real(rkp), allocatable, dimension(:)                              ::    GetValue

    class(MParamTablePoly_Type), intent(in)                           ::    This
    type(InputDet_Type), intent(in)                                   ::    Input
    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetValue'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i, ii
    class(PolyCoeff_Type), pointer                                    ::    PolyCoeffPointer=>null()

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetValue(size(Abscissa,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetValue', ProcName=ProcName, stat=StatLoc )

    GetValue = Zero

    ii = 1
    do ii = 1, size(Abscissa,1)
      i = 1
      do i = 1, This%Order+1
        PolyCoeffPointer => This%PolyCoeff(i)%GetPointer()
        GetValue(ii) = GetValue(ii) + PolyCoeffPointer%GetValue(Input=Input)*Abscissa(ii)**(i-1)
        nullify(PolyCoeffPointer)
      end do
    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCharValue( This, Input, Abscissa, Format, Debug )

    type(String_Type), allocatable, dimension(:)                      ::    GetCharValue

    class(MParamTablePoly_Type), intent(in)                           ::    This
    type(InputDet_Type), intent(in)                                   ::    Input
    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetCharValue'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i, ii
    class(PolyCoeff_Type), pointer                                    ::    PolyCoeffPointer=>null()
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    FormatLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    allocate(GetCharValue(size(Abscissa,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetValue', ProcName=ProcName, stat=StatLoc )

    ii = 1
    do ii = 1, size(Abscissa,1)
      VarR0D = Zero
      i = 1
      do i = 1, This%Order+1
        PolyCoeffPointer => This%PolyCoeff(i)%GetPointer()
        VarR0D = VarR0D + PolyCoeffPointer%GetValue(Input=Input)*Abscissa(ii)**(i-1)
        nullify(PolyCoeffPointer)
      end do
      call GetCharValue(ii)%Set_Value( Value=ConvertToString(Value=VarR0D, Format=FormatLoc) )
    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(MParamTablePoly_Type), intent(out)                          ::    LHS
    class(MParamTable_Type), intent(in)                               ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (MParamTablePoly_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if ( RHS%Constructed ) then
          LHS%Order = RHS%Order
          allocate(LHS%PolyCoeff, source=RHS%PolyCoeff, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%PolyCoeff', ProcName=ProcName, stat=StatLoc )
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(MParamTablePoly_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%PolyCoeff) ) deallocate(This%PolyCoeff, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%PolyCoeff', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
