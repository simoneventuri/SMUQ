Module Checker_Tools

  use ,intrinsic :: IEEE_Arithmetic ,only:  ieee_Is_Finite, ieee_Is_Normal
  use iso_fortran_env ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

  implicit none

  private
  public  ::  IsNormal
  public  ::  IsFinite

  Interface           IsNormal
    Module Procedure  IsNormal_0d
    Module Procedure  IsNormal_1d
    Module Procedure  IsNormal_2d
    Module Procedure  IsNormal_3d
    Module Procedure  IsNormal_4d
    Module Procedure  IsNormal_5d
    Module Procedure  IsNormal_6d
    Module Procedure  IsNormal_7d
  End Interface

  Interface           IsFinite
    Module Procedure  IsFinite_0d
    Module Procedure  IsFinite_1d
    Module Procedure  IsFinite_2d
    Module Procedure  IsFinite_3d
    Module Procedure  IsFinite_4d
    Module Procedure  IsFinite_5d
    Module Procedure  IsFinite_6d
    Module Procedure  IsFinite_7d
  End Interface

  contains

Pure Function IsNormal_0d( Variable ) result(Finite)
  class(*)                                              ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = ieee_Is_Normal(real(Variable,kind=REAL64))
    type is (integer(INT16)); Finite = ieee_Is_Normal(real(Variable,kind=REAL64))
    type is (integer(INT32)); Finite = ieee_Is_Normal(real(Variable,kind=REAL64))
    type is (integer(INT64)); Finite = ieee_Is_Normal(real(Variable,kind=REAL64))
    type is (real(REAL32));   Finite = ieee_Is_Normal(Variable)
    type is (real(REAL64));   Finite = ieee_Is_Normal(Variable)
    type is (real(REAL128));  Finite = ieee_Is_Normal(Variable)
    class default;            Finite = .False.
  end select
End Function

Pure Function IsNormal_1d( Variable ) result(Finite)
  class(*)       ,dimension(:)                          ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Normal(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Normal(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Normal(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Function IsNormal_2d( Variable ) result(Finite)
  class(*)       ,dimension(:,:)                        ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Normal(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Normal(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Normal(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Function IsNormal_3d( Variable ) result(Finite)
  class(*)       ,dimension(:,:,:)                      ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Normal(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Normal(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Normal(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Function IsNormal_4d( Variable ) result(Finite)
  class(*)       ,dimension(:,:,:,:)                    ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Normal(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Normal(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Normal(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Function IsNormal_5d( Variable ) result(Finite)
  class(*)       ,dimension(:,:,:,:,:)                  ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Normal(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Normal(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Normal(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Function IsNormal_6d( Variable ) result(Finite)
  class(*)       ,dimension(:,:,:,:,:,:)                ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Normal(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Normal(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Normal(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Function IsNormal_7d( Variable ) result(Finite)
  class(*)       ,dimension(:,:,:,:,:,:,:)              ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Normal(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Normal(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Normal(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Normal(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Function IsFinite_0d( Variable ) result(Finite)
  class(*)                                              ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = ieee_Is_Finite(real(Variable,kind=REAL64))
    type is (integer(INT16)); Finite = ieee_Is_Finite(real(Variable,kind=REAL64))
    type is (integer(INT32)); Finite = ieee_Is_Finite(real(Variable,kind=REAL64))
    type is (integer(INT64)); Finite = ieee_Is_Finite(real(Variable,kind=REAL64))
    type is (real(REAL32));   Finite = ieee_Is_Finite(Variable)
    type is (real(REAL64));   Finite = ieee_Is_Finite(Variable)
    type is (real(REAL128));  Finite = ieee_Is_Finite(Variable)
    class default;            Finite = .False.
  end select
End Function

Pure Function IsFinite_1d( Variable ) result(Finite)
  class(*)       ,dimension(:)                          ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Finite(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Function IsFinite_2d( Variable ) result(Finite)
  class(*)       ,dimension(:,:)                        ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Finite(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Function IsFinite_3d( Variable ) result(Finite)
  class(*)       ,dimension(:,:,:)                      ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Finite(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Function IsFinite_4d( Variable ) result(Finite)
  class(*)       ,dimension(:,:,:,:)                    ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Finite(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Function IsFinite_5d( Variable ) result(Finite)
  class(*)       ,dimension(:,:,:,:,:)                  ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Finite(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Function IsFinite_6d( Variable ) result(Finite)
  class(*)       ,dimension(:,:,:,:,:,:)                ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Finite(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Function IsFinite_7d( Variable ) result(Finite)
  class(*)       ,dimension(:,:,:,:,:,:,:)              ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Finite(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

End Module