Module Object_Class

  use Utilities_Library  ,only:  GetOptArgValue_LOG, &
                                GetOptArgValue_CHAR, &
                                GetOptArgValue_INT8, GetOptArgValue_INT16, GetOptArgValue_INT32, GetOptArgValue_INT64, &
                                GetOptArgValue_REAL32, GetOptArgValue_REAL64, GetOptArgValue_REAL128

  implicit none

  private
  public  ::  Object_Type

  Type  ,abstract         ::  Object_Type
  contains
    private
    generic     ,public   ::  GetOptArgValue => GetOptArgValue_LOG, &
                                                           GetOptArgValue_CHAR, &
                                                           GetOptArgValue_INT8, GetOptArgValue_INT16, GetOptArgValue_INT32, GetOptArgValue_INT64, &
                                                           GetOptArgValue_REAL32, GetOptArgValue_REAL64, GetOptArgValue_REAL128
    procedure   ,nopass   ::  GetOptArgValue_LOG
    procedure   ,nopass   ::  GetOptArgValue_INT8
    procedure   ,nopass   ::  GetOptArgValue_INT16
    procedure   ,nopass   ::  GetOptArgValue_INT32
    procedure   ,nopass   ::  GetOptArgValue_INT64
    procedure   ,nopass   ::  GetOptArgValue_REAL32
    procedure   ,nopass   ::  GetOptArgValue_REAL64
    procedure   ,nopass   ::  GetOptArgValue_REAL128
    procedure   ,nopass   ::  GetOptArgValue_CHAR
  End Type

End Module
