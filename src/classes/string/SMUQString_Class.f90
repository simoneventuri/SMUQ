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

module SMUQString_Class

use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    SMUQString_Type

type                                                                  ::    SMUQString_Type
  character(:), allocatable                                           ::    Value
contains
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    Length
  procedure, public                                                   ::    Get
  procedure, public                                                   ::    Strip
  procedure, public                                                   ::    LStrip
  procedure, public                                                   ::    RStrip
  procedure, public                                                   ::    Upper
  procedure, public                                                   ::    Lower
  procedure, public                                                   ::    Split
  generic, public                                                     ::    Set                   =>    SetString,                &
                                                                                                        SetChar
  procedure, private                                                  ::    SetString
  procedure, private                                                  ::    SetChar
  generic, public                                                     ::    assignment(=)         =>    CopyString,               &
                                                                                                        CopyChar
  procedure, private                                                  ::    CopyString
  procedure, private                                                  ::    CopyChar
  generic, public                                                     ::    operator(//)          =>    ConcatStringString,       &
                                                                                                        ConcatStringChar,         &
                                                                                                        ConcatCharString
  procedure, private                                                  ::    ConcatStringString
  procedure, private                                                  ::    ConcatStringChar
  procedure, private                                                  ::    ConcatCharString
  generic, public                                                     ::    operator(==)          =>    CompareStringString,      &
                                                                                                        CompareStringChar,        &
                                                                                                        CompareCharString
  procedure, private                                                  ::    CompareStringString
  procedure, private                                                  ::    CompareStringChar
  procedure, private                                                  ::    CompareCharString
  final                                                               ::    Finalizer
end type
  
logical, parameter                                                    ::    DebugGlobal = .false.
character(26), parameter                                              ::    UpperList = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
character(26), parameter                                              ::    LowerList = 'abcdefghijklmnopqrstuvwxyz'

contains
  
!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(SMUQString_Type), intent(inout)                               ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%ValueSet = .false.
  
  if (allocated(This%Value)) deallocate(This%Value, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Value', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function Length(This)

  integer                                                             ::    Length

  class(SMUQString_Type), intent(in)                                  ::    This

  character(*), parameter                                             ::    ProcName='Length'

  Length = 0
  if (allocated(This%Value)) Length = len(This%Value)

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function Get(This)

  character(:), allocatable                                           ::    Get

  class(SMUQString_Type), intent(in)                                  ::    This

  character(*), parameter                                             ::    ProcName='Get'

  Get = ''
  if (This%Length() > 0) Get = This%Value

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function Strip(This)

  character(:), allocatable                                           ::    Strip

  class(SMUQString_Type), intent(in)                                  ::    This

  character(*), parameter                                             ::    ProcName='Strip'

  Strip = ''
  if (This%Length() > 0) Strip = trim(adjustl(This%Value))

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function LStrip(This)

  character(:), allocatable                                           ::    LStrip

  class(SMUQString_Type), intent(in)                                  ::    This

  character(*), parameter                                             ::    ProcName='LStrip'
  integer                                                             ::    NbLeadBlanks

  LStrip = ''
  if (This%Length() > 0) then
    NbLeadBlanks = len(This%RStrip()) - len(This%Strip())
    if (NbLeadBlanks < This%Length()) LStrip = This%Value(NbLeadBlanks+1:)
  end if

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function RStrip(This)

  character(:), allocatable                                           ::    RStrip

  class(SMUQString_Type), intent(in)                                  ::    This

  character(*), parameter                                             ::    ProcName='RStrip'

  RStrip = ''
  if (This%Length() > 0) RStrip = trim(This%Value)

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function Lower(This)

  character(:), allocatable                                           ::    Lower

  class(SMUQString_Type), intent(in)                                  ::    This

  character(*), parameter                                             ::    ProcName='Lower'
  integer                                                             ::    i
  integer                                                             ::    ii

  Lower = ''
  if (This%Length() > 0) then
    Lower = This%Value
    i = 1
    do i = 1, This%Length()
      ii = index(UpperList,Lower(i:i))
      if (ii /= 0) Lower(i:i) = LowerList(ii:ii) 
    end do
  end if

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function Upper(This)

  character(:), allocatable                                           ::    Upper

  class(SMUQString_Type), intent(in)                                  ::    This

  character(*), parameter                                             ::    ProcName='Upper'
  integer                                                             ::    i
  integer                                                             ::    ii

  Upper = ''
  if (This%Length() > 0) then
    Upper = This%Value
    i = 1
    do i = 1, This%Length()
      ii = index(LowerList,Upper(i:i))
      if (ii /= 0) Upper(i:i) = UpperList(ii:ii) 
    end do
  end if

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function Split(This, Separator, Escape)

  use String_Library                                                ,only:    Parse

  type(SMUQString), allocatable, dimension(:)                         ::    Split

  class(SMUQString_Type), intent(in)                                  ::    This
  character(*), optional, intent(in)                                  ::    Separator
  character(*), optional, intent(in)                                  ::    Escape

  character(*), parameter                                             ::    ProcName='Split'
  character(:), allocatable                                           ::    SeparatorLoc
  character(:), allocatable, dimension(:)                             ::    SplitChar
  integer                                                             ::    NbStrings
  integer                                                             ::    if

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  if (len(This%Strip()) == 0 ) then
    allocate(Split(1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Split', ProcName=ProcName, stat=StatLoc)
    Split(1) = ''
    return
  end if

  if (present(Escape)) then
    call Parse(Input=This%Value, Separator=SeparatorLoc, Output=SplitChar, EscRHS=Escape)
  else
    call Parse(Input=This%Value, Separator=SeparatorLoc, Output=SplitChar)
  end if

  NbStrings = size(SplitChar)

  allocate(Split(NbStrings), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Split', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, NbStrings
    Split(i) = trim(adjustl(SplitChar(i)))
  end do

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine SetString(This, String)

  class(SMUQString_Type), intent(inout)                               ::    This
  class(SMUQString_Type), intent(in)                                  ::    String

  character(*), parameter                                             ::    ProcName='SetString'

  This%Value = String%Get()

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine SetChar(This, String)

  class(SMUQString_Type), intent(inout)                               ::    This
  character(*), intent(in)                                            ::    String

  character(*), parameter                                             ::    ProcName='SetChar'

  This%Value = String

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function ConcatStringString(String1, String2)

  character(:), allocatable                                           ::    ConcatStringString

  class(SMUQString_Type), intent(in)                                  ::    String1
  class(SMUQString_Type), intent(in)                                  ::    String2

  character(*), parameter                                             ::    ProcName='ConcatStringString'

  ConcatStringString = ''

  if(allocated(String1%Value)) ConcatStringString = String1%Value
  if(allocated(String2%Value)) ConcatStringString = ConcatStringString // String2%Value

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function ConcatStringChar(String1, String2)

  character(:), allocatable                                           ::    ConcatStringChar

  class(SMUQString_Type), intent(in)                                  ::    String1
  character(*), intent(in)                                            ::    String2

  character(*), parameter                                             ::    ProcName='ConcatStringChar'

  if(allocated(String1%Value)) then
    ConcatStringString = String1%Value // String2
  else
    ConcatStringString = String2
  end if

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function ConcatCharString(String1, String2)

  character(:), allocatable                                           ::    ConcatCharString

  character(*), intent(in)                                            ::    String1
  class(SMUQString_Type), intent(in)                                  ::    String2

  character(*), parameter                                             ::    ProcName='ConcatCharString'

  if(allocated(String2%Value)) then
    ConcatCharString = String1 // String2%Value
  else
    ConcatCharString = String1
  end if

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function CompareStringString(String1, String2)

  logical                                                             ::    CompareStringString

  class(SMUQString_Type), intent(in)                                  ::    String1
  class(SMUQString_Type), intent(in)                                  ::    String2

  character(*), parameter                                             ::    ProcName='CompareStringString'

  if( allocated(String1%Value) .and. allocated(String2%Value)) then
    CompareStringString = String1%Value == String2%Value
  else
    CompareStringString = .false.
  end if

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------------------------------------
function CompareStringChar(String1, String2)

  logical                                                             ::    CompareStringChar

  class(SMUQString_Type), intent(in)                                  ::    String1
  character(*), intent(in)                                            ::    String2

  character(*), parameter                                             ::    ProcName='CompareStringChar'

  if( allocated(String1%Value) ) then
    CompareStringChar = String1%Value == String2
  else
    CompareStringChar = .false.
  end if

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------------------------------------
function CompareCharString(String1, String2)

  logical                                                             ::    CompareCharString

  character(*), intent(in)                                            ::    String1
  class(SMUQString_Type), intent(in)                                  ::    String2

  character(*), parameter                                             ::    ProcName='CompareCharString'

  if( allocated(String2%Value) ) then
    CompareCharString = String2%Value == String1
  else
    CompareCharString = .false.
  end if

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine CopyString(LHS, RHS)

  class(SMUQString_Type), intent(out)                                 ::    LHS
  class(SMUQString_Type), intent(in)                                  ::    RHS

  character(*), parameter                                             ::    ProcName='CopyString'
  integer                                                             ::    StatLoc=0

  LHS%Value = ''
  if (allocated(RHS%Value)) LHS%Value = RHS%Value

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine CopyChar(LHS, RHS)

  class(SMUQString_Type), intent(out)                                 ::    LHS
  character(*), intent(in)                                            ::    RHS

  character(*), parameter                                             ::    ProcName='CopyChar'
  integer                                                             ::    StatLoc=0

  LHS%Value = RHS

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(SMUQString_Type), intent(inout)                                ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Value)) deallocate(This%Value, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Value', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module