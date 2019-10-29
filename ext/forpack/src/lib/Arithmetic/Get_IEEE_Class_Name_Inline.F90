  type(IEEE_CLASS_TYPE) ::  class_type
  Name  =   ""
  if( .Not. IEEE_SUPPORT_DATATYPE( Variable ) ) return
  class_type  =   IEEE_CLASS( Variable )
  if ( IEEE_SUPPORT_NAN( Variable ) ) then
    if ( class_type == IEEE_SIGNALING_NAN )     Name  =  "IEEE_SIGNALING_NAN"
    if ( class_type == IEEE_QUIET_NAN )         Name  =  "IEEE_QUIET_NAN"
  end if
  if ( IEEE_SUPPORT_INF( Variable ) ) then
    if ( class_type == IEEE_NEGATIVE_INF )      Name  =  "IEEE_NEGATIVE_INF"
    if ( class_type == IEEE_POSITIVE_INF )      Name  =  "IEEE_POSITIVE_INF"
  end if
  if ( IEEE_SUPPORT_DENORMAL( Variable ) ) then
    if ( class_type == IEEE_NEGATIVE_DENORMAL ) Name  =  "IEEE_NEGATIVE_DENORMAL"
    if ( class_type == IEEE_POSITIVE_DENORMAL ) Name  =  "IEEE_POSITIVE_DENORMAL"
  end if
  if ( class_type == IEEE_NEGATIVE_NORMAL )     Name  =  "IEEE_NEGATIVE_NORMAL"
  if ( class_type == IEEE_POSITIVE_NORMAL )     Name  =  "IEEE_POSITIVE_NORMAL"
  if ( class_type == IEEE_NEGATIVE_ZERO )       Name  =  "IEEE_NEGATIVE_ZERO"
  if ( class_type == IEEE_POSITIVE_ZERO )       Name  =  "IEEE_POSITIVE_ZERO"
  if ( Name == "" )                             Name  =  "IEEE_OTHER_VALUE"