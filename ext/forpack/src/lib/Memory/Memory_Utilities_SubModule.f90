SubModule(Memory_Utilities_Module) Memory_Utilities_SubModule

! 1 bite = 8 octets = 8 bytes

  implicit none

  real(8)       ,parameter      ::  BitsPerOctet  = 8
  real(8)       ,parameter      ::  BitsPerKio    = 1024
  real(8)       ,parameter      ::  BitsPerMio    = 1024**2
  real(8)       ,parameter      ::  BitsPerGio    = 1024**3

  contains

Module Procedure ConvertBits
  if ( present(Oct)) Oct = Bits / BitsPerOctet
  if ( present(Kio)) Kio = Bits / BitsPerKio
  if ( present(Mio)) Mio = Bits / BitsPerMio
  if ( present(Gio)) Gio = Bits / BitsPerGio
End Procedure

Module Procedure BitsToOctets
  Octets  =   Bits / BitsPerOctet
End Procedure

Module Procedure BitsToKio
  Kio  =   Bits / BitsPerKio
End Procedure

Module Procedure BitsToMio
  Mio  =   Bits / BitsPerMio
End Procedure

Module Procedure BitsToGio
  Gio  =   Bits / BitsPerGio
End Procedure

Module Procedure ConvertBitsToXOctet

  use String_Library    ,only:  Convert_To_String

  real(8)                                                                       ::  RealValue_
  character(:)  ,allocatable                                                    ::  Units_
  character(:)  ,allocatable                                                    ::  Fmt_

  if      ( Bits < BitsPerKio ) then
    RealValue_  =   BitsToOctets(Bits)
    Units_      =   "octets"
  else if ( Bits < BitsPerMio ) then
    RealValue_  =   BitsToKio(Bits)
    Units_      =   "Kio"
  else if ( Bits < BitsPerGio ) then
    RealValue_  =   BitsToMio(Bits)
    Units_      =   "Mio"
  else
    RealValue_  =   BitsToGio(Bits)
    Units       =   "Gio"
  end if

  if ( present(Units) )     Units     = Units_
  if ( present(RealValue) ) RealValue = RealValue_

  if ( present(CharValue) ) then
    Fmt_        =   "(f8.3)"
    if ( present(Fmt) ) Fmt_ = Fmt
    CharValue   =       Convert_To_String( RealValue_, Fmt=Fmt_ )
  end if

End Procedure

End SubModule