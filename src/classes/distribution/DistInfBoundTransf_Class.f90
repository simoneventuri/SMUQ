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

module DistInfBoundTransf_Class

use Prob_Library
use Input_Library
use Parameters_Library
use CommandRoutines_Module
use StringRoutines_Module
use DistProb_Class                                                ,only:    DistProb_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use BaseDistProb_Factory_Class                                    ,only:    BaseDistProb_Factory
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    DistInfBoundTransf_Type

type, extends(DistProb_Type)                                          ::    DistInfBoundTransf_Type
  class(DistProb_Type), allocatable                                   ::    DistProb
  logical                                                             ::    DistTLeft
  logical                                                             ::    DistTRight
  real(rkp)                                                           ::    DistA
  real(rkp)                                                           ::    DistB
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    PDF
  procedure, public                                                   ::    CDF
  procedure, public                                                   ::    InvCDF
  generic, public                                                     ::    Transform               =>    Transform_0D,           &
                                                                                                          Transform_1D
  procedure, private                                                  ::    Transform_0D
  procedure, private                                                  ::    Transform_1D
  generic, public                                                     ::    InvTransform            =>    InvTransform_0D,        &
                                                                                                          InvTransform_1D
  procedure, private                                                  ::    InvTransform_0D
  procedure, private                                                  ::    InvTransform_1D
  generic, public                                                     ::    fInvTransform           =>    fInvTransform_0D
  procedure, private                                                  ::    fInvTransform_0D
  procedure, public                                                   ::    WriteInfo
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(DistInfBoundTransf_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'infinite_bound_transform'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(DistInfBoundTransf_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    if (allocated(This%DistProb)) deallocate(This%DistProb, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%DistProb', ProcName=ProcName, stat=StatLoc)

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(DistInfBoundTransf_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%DistTLeft = .false.
    This%DistTRight = .false.
    This%DistA = Zero
    This%DistB = Zero
    This%TruncatedRight = .false.
    This%TruncatedLeft = .false.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(DistInfBoundTransf_Type), intent(inout)                     ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    SectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()
    
    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    SectionName = 'distribution'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call BaseDistProb_Factory%Construct(Object=This%DistProb, Input=InputSection, Prefix=PrefixLoc)

    This%DistTLeft = This%DistProb%IsTruncatedLeft()
    This%DistTRIght = This%DistProb%IsTruncatedRight()

    if (This%DistTLeft) This%DistA = This%DistProb%GetA()
    if (This%DistTRight) This%DistB = This%DistProb%GetB()

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1(This, Distribution)

    class(DistInfBoundTransf_Type), intent(inout)                     ::    This
    class(DistProb_Type), intent(in)                                  ::    Distribution

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    allocate(This%DistProb, source=Distribution, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%DistProb', ProcName=ProcName, stat=StatLoc)

    This%DistTLeft = This%DistProb%IsTruncatedLeft()
    This%DistTRIght = This%DistProb%IsTruncatedRight()

    if (This%DistTLeft) This%DistA = This%DistProb%GetA()
    if (This%DistTRight) This%DistB = This%DistProb%GetB()

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    if (ExternalFlag) call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

    call GetInput%SetName(SectionName = trim(adjustl(Name)))
    
    if (ExternalFlag) DirectorySub = DirectoryLoc // '/distribution'

    SectionName = 'distribution'
    call GetInput%AddSection(Section=BaseDistProb_Factory%GetObjectInput(Object=This%DistProb, Name=SectionName,     &
                                                                                      Prefix=PrefixLoc, Directory=DirectorySub))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF(This, X)

    real(rkp)                                                         ::    PDF

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), intent(in)                                             ::    X

    character(*), parameter                                           ::    ProcName='PDF'
    real(rkp)                                                         ::    XLoc

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    XLoc = X
    call This%Transform(Value=XLoc)

    PDF = This%DistProb%PDF(X=XLoc)

    call This%fInvTransform(Value=PDF, X=XLoc)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF(This, X)

    real(rkp)                                                         ::    CDF

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), intent(in)                                             ::    X

    character(*), parameter                                           ::    ProcName='CDF'
    real(rkp)                                                         ::    XLoc

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    XLoc = X
    call This%Transform(Value=XLoc)

    CDF = This%DistProb%CDF(X=XLoc)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF(This, P)

    real(rkp)                                                         ::    InvCDF

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), intent(in)                                             ::    P

    character(*), parameter                                           ::    ProcName='InvCDF'
    real(rkp)                                                         ::    XLoc

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    InvCDF = This%DistProb%InvCDF(P=P)
    call This%InvTransform(Value=InvCDF)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform_0D(This, Value)

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), intent(inout)                                          ::    Value

    character(*), parameter                                           ::    ProcName='Transform_0D'
    integer                                                           ::    StatLoc=0

    if (This%DistTLeft .and. This%DistTRight) then
      Value = (This%DistB*dexp(Value)+This%DistA) / (One+dexp(Value))
    elseif (This%DistTLeft) then
      Value = dexp(Value) + This%DistA
    elseif (This%DistTRight) then
      Value = This%DistB - One / dexp(Value)
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform_1D(This, Values)

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), dimension(:), intent(inout)                            ::    Values

    character(*), parameter                                           ::    ProcName='Transform_1D'
    integer                                                           ::    StatLoc=0

    if (This%DistTLeft .and. This%DistTRight) then
      Values = (This%DistB*dexp(Values)+This%DistA) / (One+dexp(Values))
    elseif (This%DistTLeft) then
      Values = dexp(Values) + This%DistA
    elseif (This%DistTRight) then
      Values = This%DistB - One / dexp(Values)
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine InvTransform_0D(This, Value)

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), intent(inout)                                          ::    Value

    character(*), parameter                                           ::    ProcName='InvTransform_0D'
    integer                                                           ::    StatLoc=0

    if (This%DistTLeft .and. This%DistTRight) then
      Value = dlog((Value-This%DistA)/(This%DistB-Value))
    elseif (This%DistTLeft) then
      Value = dlog((Value-This%DistA))
    elseif (This%DistTRight) then
      Value = dlog(One/(This%DistB-Value))
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine InvTransform_1D(This, Values)

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), dimension(:), intent(inout)                            ::    Values

    character(*), parameter                                           ::    ProcName='InvTransform_1D'
    integer                                                           ::    StatLoc=0

    if (This%DistTLeft .and. This%DistTRight) then
      Values = dlog((Values-This%DistA)/(This%DistB-Values))
    elseif (This%DistTLeft) then
      Values = dlog((Values-This%DistA))
    elseif (This%DistTRight) then
      Values = dlog(One/(This%DistB-Values))
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine fInvTransform_0D(This, Value, X)

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), intent(inout)                                          ::    Value
    real(rkp), intent(in)                                             ::    X

    character(*), parameter                                           ::    ProcName='fInvTransform_0D'
    integer                                                           ::    StatLoc=0

    if (This%DistTLeft .and. This%DistTRight) then
      Value = Value * dabs(((X-This%DistA)*(This%DistB-X))/(This%DistB-This%DistA))
    elseif (This%DistTLeft) then
      Value = Value * dabs(X-This%DistA)
    elseif (This%DistTRight) then
      Value = Value * dabs(-(This%DistB-X))
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInfo(This, File)

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    type(SMUQFile_Type), intent(inout)                                ::    File

    character(*), parameter                                           ::    ProcName='WriteInfo'
    integer                                                           ::    i
    type(SMUQString_Type), dimension(3)                               ::    Strings

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)
    
    Strings(1) = 'infboundtransf'
    Strings(2) = '-Inf'
    Strings(3) = 'Inf'

    call File%Append(Strings=Strings)

    call This%DistProb%WriteInfo(File=File)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(DistInfBoundTransf_Type), intent(out)                       ::    LHS
    class(DistProb_Type), intent(in)                                  ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (DistInfBoundTransf_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          allocate(LHS%DistProb, source=RHS%DistProb, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%DistProb', ProcName=ProcName, stat=StatLoc)
          LHS%DistTLeft = RHS%DistTLeft
          LHS%DistTRight = RHS%DistTRight
          LHS%DistA = RHS%DistA
          LHS%DistB = RHS%DistB
        end if

      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(DistInfBoundTransf_Type), intent(inout)                      ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if (allocated(This%DistProb)) deallocate(This%DistProb, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%DistProb', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
