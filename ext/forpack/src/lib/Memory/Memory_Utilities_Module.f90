Module Memory_Utilities_Module

  implicit none

  private
  public  ::  ConvertBits
  public  ::  BitsToOctets
  public  ::  BitsToKio
  public  ::  BitsToMio
  public  ::  BitsToGio
  public  ::  ConvertBitsToXOctet

  Interface
    Pure Module Subroutine ConvertBits( Bits, Oct, Kio, Mio, Gio )
      integer                                                       ,intent(in)     ::  Bits                    !< Bits value
      real(8)                                             ,optional ,intent(out)    ::  Oct
      real(8)                                             ,optional ,intent(out)    ::  Kio
      real(8)                                             ,optional ,intent(out)    ::  Mio
      real(8)                                             ,optional ,intent(out)    ::  Gio
    End Subroutine
    Pure Elemental Module Function BitsToOctets( Bits ) result(Octets)
      integer                                                       ,intent(in)     ::  Bits                    !< Bits value
      real(8)                                                                       ::  Octets
    End Function
    Pure Elemental Module Function BitsToKio( Bits ) result(Kio)
      integer                                                       ,intent(in)     ::  Bits                    !< Bits value
      real(8)                                                                       ::  Kio
    End Function
    Pure Elemental Module Function BitsToMio( Bits ) result(Mio)
      integer                                                       ,intent(in)     ::  Bits                    !< Bits value
      real(8)                                                                       ::  Mio
    End Function
    Pure Elemental Module Function BitsToGio( Bits ) result(Gio)
      integer                                                       ,intent(in)     ::  Bits                    !< Bits value
      real(8)                                                                       ::  Gio
    End Function
    Pure Module Subroutine ConvertBitsToXOctet( Bits, RealValue, CharValue, Units, Fmt )
      integer                                                       ,intent(in)     ::  Bits                    !< Bits value
      real(8)                                             ,optional ,intent(out)    ::  RealValue               ! String containing the bits expressed in octet
      character(:)  ,allocatable                          ,optional ,intent(out)    ::  CharValue               ! String containing the bits expressed in octet
      character(:)  ,allocatable                          ,optional ,intent(out)    ::  Units                   ! Memory value expressed either in octet, Kio, Mio or Gio depending on the input number of bits
      character(*)                                        ,optional ,intent(in)     ::  Fmt
    End Subroutine
  End Interface

End Module