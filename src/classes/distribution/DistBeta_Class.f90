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
! defined using shape and rate form
module DistBeta_Class

use CDF_Library
use Prob_Library
use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StatisticsRoutines_Module
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use DistProb_Class                                                ,only:    DistProb_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    DistBeta_Type

type, extends(DistProb_Type)                                          ::    DistBeta_Type
  real(rkp)                                                           ::    Alpha=One
  real(rkp)                                                           ::    Beta=One
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    PDF_R0D
  procedure, nopass, public                                           ::    ComputePDF
  procedure, public                                                   ::    CDF_R0D
  procedure, nopass, public                                           ::    ComputeCDF
  procedure, public                                                   ::    InvCDF_R0D
  procedure, nopass, public                                           ::    ComputeInvCDF
  procedure, public                                                   ::    GetAlpha
  procedure, public                                                   ::    GetBeta
  procedure, public                                                   ::    GetMoment
  procedure, public                                                   ::    WriteInfo
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(DistBeta_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'gamma'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(DistBeta_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(DistBeta_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%A = Zero
    This%B = One
    This%Alpha = One
    This%Beta = One
    This%TruncatedRight = .true.
    This%TruncatedLeft = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(DistBeta_Type), intent(inout)                               ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ProcessInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    VarC0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    PrefixLoc

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()
    
    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'alpha'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, Mandatory=.true.)
    This%Alpha = VarR0D

    ParameterName = 'beta'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, Mandatory=.true.)
    This%Beta = VarR0D

    ParameterName = 'a'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%A = VarR0D

    ParameterName = 'b'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%B = VarR0D

    if (This%Alpha <= Zero) call Error%Raise("Alpha parameter at or below zero")
    if (This%Beta <= Zero) call Error%Raise("Beta parameter at or below zero")
    if (This%B < This%A) call Error%Raise(Line='Upper limit < lower limit', ProcName=ProcName)

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1(This, Alpha, Beta, A, B)
    
    class(DistBeta_Type), intent(inout)                               ::    This
    real(rkp), intent(in)                                             ::    Alpha
    real(rkp), intent(in)                                             ::    Beta
    real(rkp), optional, intent(in)                                   ::    A
    real(rkp), optional, intent(in)                                   ::    B 

    character(*), parameter                                           ::    ProcName='ConstructCase1'

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    This%Alpha = Alpha

    This%Beta = Beta

    if (present(A)) This%A = A

    if (present(B)) This%B = B

    if (Alpha <= Zero) call Error%Raise("Alpha parameter at or below zero")
    if (Beta <= Zero) call Error%Raise("Beta parameter at or below zero")
    if (This%B < This%A) call Error%Raise(Line='Upper limit < lower limit', ProcName=ProcName)

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(DistBeta_Type), intent(in)                                  ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput%SetName(SectionName = trim(adjustl(Name)))
    call GetInput%AddParameter(Name='alpha', Value=ConvertToString(Value=This%Alpha))
    call GetInput%AddParameter(Name='beta', Value=ConvertToString(Value=This%Beta))
    call GetInput%AddParameter(Name='a', Value=ConvertToString(Value=This%A))
    call GetInput%AddParameter(Name='b', Value=ConvertToString(Value=This%B))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_R0D(This, X)

    real(rkp)                                                         ::    PDF_R0D

    class(DistBeta_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    X

    character(*), parameter                                           ::    ProcName='PDF_R0D'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    PDF_R0D = This%ComputePDF(X=X, Alpha=This%Alpha, Beta=This%Beta, A=This%A, B=This%B)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Assumes that A will not be passed unless it is above 0.0 and that all parameters are valid
  function ComputePDF(X, Alpha, Beta, A, B)

    real(rkp)                                                         ::    ComputePDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Alpha
    real(rkp), intent(in)                                             ::    Beta
    real(rkp), optional, intent(in)                                   ::    A
    real(rkp), optional, intent(in)                                   ::    B

    character(*), parameter                                           ::    ProcName='ComputePDF'
    real(rkp)                                                         ::    ALoc
    real(rkp)                                                         ::    BLoc
    real(8)                                                           ::    AlphaLoc
    real(8)                                                           ::    BetaLoc
    real(8)                                                           ::    XLoc
    real(8)                                                           ::    VarR0D
    logical                                                           ::    TripFlag

    TripFlag = .false.

    ALoc = Zero
    if (present(A)) ALoc = A

    BLoc = One
    if (present(B)) BLoc = B

    if (X < ALoc) then
      ComputePDF = Zero
      TripFlag=.true.
    end if

    if (X > BLoc) then
      ComputePDF = Zero
      TripFlag=.true.
    end if

    if (.not. TripFlag) then
      XLoc = (X-ALoc)/(BLoc-ALoc)
      AlphaLoc = real(Alpha,8)
      BetaLoc = real(Beta,8)
      call beta_pdf(XLoc, AlphaLoc, BetaLoc, VarR0D)
      ComputePDF = real(VarR0D,rkp)/(BLoc-ALoc)
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF_R0D(This, X)

    real(rkp)                                                         ::    CDF_R0D

    class(DistBeta_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    X

    character(*), parameter                                           ::    ProcName='CDF_R0D'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    CDF_R0D = This%ComputeCDF(X=X, Alpha=This%Alpha, Beta=This%Beta, A=This%A, B=This%B)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeCDF(X, Alpha, Beta, A, B)

    real(rkp)                                                         ::    ComputeCDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    Alpha
    real(rkp), intent(in)                                             ::    Beta
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B

    character(*), parameter                                           ::    ProcName='ComputeCDF'
    real(rkp)                                                         ::    ALoc
    real(rkp)                                                         ::    BLoc
    real(8)                                                           ::    AlphaLoc
    real(8)                                                           ::    BetaLoc
    real(8)                                                           ::    XLoc
    real(8)                                                           ::    VarR0D
    logical                                                           ::    TripFlag

    TripFlag = .false.

    ALoc = Zero
    if (present(A)) ALoc = A

    BLoc = One
    if (present(B)) BLoc = B

    if (X < ALoc) then
      ComputeCDF = Zero
      TripFlag=.true.
    end if

    if (X > BLoc) then
      ComputeCDF = One
      TripFlag=.true.
    end if

    if (.not. TripFlag) then
      XLoc = (X-ALoc)/(BLoc-ALoc)
      AlphaLoc = real(Alpha,8)
      BetaLoc = real(Beta,8)
      call beta_cdf(XLoc, AlphaLoc, BetaLoc, VarR0D)
      ComputeCDF = real(VarR0D,rkp)
    end if 

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF_R0D(This, P)

    real(rkp)                                                         ::    InvCDF_R0D

    class(DistBeta_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    P

    character(*), parameter                                           ::    ProcName='InvCDF_R0D'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    InvCDF_R0D = This%ComputeInvCDF(P=P, Alpha=This%Alpha, Beta=This%Beta, A=This%A, B=This%B)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Assumes that A will not be passed unless it is above 0.0 and that all parameters are valid
  function ComputeInvCDF(P, Alpha, Beta, A, B)

    real(rkp)                                                         ::    ComputeInvCDF

    real(rkp), intent(in)                                             ::    p
    real(rkp), intent(in)                                             ::    Alpha
    real(rkp), intent(in)                                             ::    Beta
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B

    character(*), parameter                                           ::    ProcName='ComputeInvCDF'
    real(rkp)                                                         ::    ALoc
    real(rkp)                                                         ::    BLoc
    real(8)                                                           ::    AlphaLoc
    real(8)                                                           ::    BetaLoc
    real(8)                                                           ::    XLoc
    real(8)                                                           ::    PLoc
    integer                                                           ::    StatLoc=0
    logical                                                           ::    TripFlag

    if (P < Zero) call Error%Raise(Line='P value below the minimum of 0 in the inverse CDF calculation', ProcName=ProcName)
    if (P > One) call Error%Raise(Line='P value above the maximum of 1 in the inverse CDF calculation', ProcName=ProcName)

    TripFlag = .false.

    ALoc = Zero
    if (present(A)) ALoc = A

    BLoc = One
    if (present(B)) BLoc = B

    if (P == Zero) then
      ComputeInvCDF = ALoc
      TripFlag=.true.
    end if

    if (P == One) then
      ComputeInvCDF = BLoc
      TripFlag=.true.
    end if

    if (.not. TripFlag) then
      PLoc = real(P,8)
      AlphaLoc = real(Alpha,8)
      BetaLoc = real(Beta,8)
      call beta_cdf_inv(P, AlphaLoc, BetaLoc, XLoc)
      ComputeInvCDF = real(XLoc,rkp)*(BLoc-ALoc) + ALoc
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetAlpha(This)

    real(rkp)                                                         ::    GetAlpha

    class(DistBeta_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetAlpha'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetAlpha = This%Alpha

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetBeta(This)

    real(rkp)                                                         ::    GetBeta

    class(DistBeta_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetBeta'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetBeta = This%Beta

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMoment(This, Moment)

    real(rkp)                                                         ::    GetMoment

    class(DistBeta_Type), intent(in)                                  ::    This
    integer, intent(in)                                               ::    Moment

    character(*), parameter                                           ::    ProcName='GetMoment'
    integer                                                           ::    i

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (Moment < 0) call Error%Raise("Requested a distribution moment below 0", ProcName=ProcName)

    if (Moment > 2) then
      GetMoment = This%ComputeMomentNumerical(Moment=Moment)
    elseif (Moment == 1) then
      GetMoment = (This%Alpha*This%B+This%Beta*This%A) / (This%Alpha+This%Beta)
    elseif(Moment == 2) then
      GetMoment = (This%Alpha*This%Beta*(This%B-This%A)**2) /((This%Alpha+This%Beta)**2*(This%Alpha+This%Beta+One))
      GetMoment = GetMoment + ((This%Alpha*This%B+This%Beta*This%A) / (This%Alpha+This%Beta))**2
    else
      GetMoment = One
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInfo(This, File)

    class(DistBeta_Type), intent(in)                                  ::    This
    type(SMUQFile_Type), intent(inout)                                ::    File

    character(*), parameter                                           ::    ProcName='WriteInfo'
    integer                                                           ::    i
    type(SMUQString_Type), dimension(5)                               ::    Strings

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    Strings(1) = 'beta'
    Strings(2) = ConvertToString(Value=This%Alpha)
    Strings(3) = ConvertToString(Value=This%Beta)
    Strings(4) = ConvertToString(Value=This%A)
    Strings(5) = ConvertToString(Value=This%B)

    call File%Append(Strings=Strings)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(DistBeta_Type), intent(out)                                 ::    LHS
    class(DistProb_Type), intent(in)                                  ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (DistBeta_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          LHS%A = RHS%A
          LHS%B = RHS%B
          LHS%Alpha = RHS%Alpha
          LHS%Beta = RHS%Beta
          LHS%TruncatedLeft = RHS%TruncatedLeft
          LHS%TruncatedRight = RHS%TruncatedRight
        end if
      
      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(DistBeta_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
