Module PipeInterfaces_Module

  use, intrinsic ::  iso_c_binding

  implicit none

  private
  public  ::  popen
  public  ::  fgets
  public  ::  pclose
  public  ::  getline
  public  ::  getdelim
  public  ::  free

  ! error codes
  public ::  CLOSE_FAILED

  integer(C_INT)  ,parameter  ::  CLOSE_FAILED = -1

  Interface

    Function popen( Command, Mode ) result(File) bind( C, name='popen' )
      use, intrinsic ::  iso_c_binding
      character(C_CHAR) ,dimension(*)                       ,intent(in)     ::  Command
      character(C_CHAR) ,dimension(*)                       ,intent(in)     ::  Mode
      type(C_PTR)                                                           ::  File
    End Function

    Function fgets(str, size, stream) bind(C, name='fgets')
      use, intrinsic ::  iso_c_binding
      character(C_CHAR) ,dimension(*)                       ,intent(inout)  ::  str
      integer(C_INT)  ,value                                ,intent(in)     ::  size
      type(C_PTR)     ,value                                                ::  stream
      type(C_PTR)                                                           ::  fgets
    End Function

    Function pclose(stream) bind(C, name='pclose')
      use, intrinsic ::  iso_c_binding
      type(C_PTR)     ,value                                                ::  stream
      integer(C_INT)                                                        ::  pclose
    End Function

    Function getline( linep, linecapp, stream ) bind(C, name='getline')
      use, intrinsic ::  iso_c_binding
      type(C_PTR)                                                           ::  linep
      integer(C_SIZE_T)                                                     ::  linecapp
      type(C_PTR) ,value                                                    ::  stream
      integer(C_SIZE_T)                                                     ::  getline
    End Function

    Function getdelim(linep, linecapp, delimeter, stream) bind(C, name='getdelim')
      use, intrinsic ::  iso_c_binding
      integer(C_SIZE_T)                                                     ::  getdelim
      type(C_PTR)                                                           ::  linep
      integer(C_SIZE_T)                                                     ::  linecapp
      integer(C_INT)  ,value                                                ::  delimeter
      type(C_PTR)     ,value                                                ::  stream
    End Function

    Subroutine free(ptr) bind(C, name='free')
      use, intrinsic ::  iso_c_binding
      type(C_PTR)     ,value                                                ::  ptr
    End Subroutine

  End Interface

End Module