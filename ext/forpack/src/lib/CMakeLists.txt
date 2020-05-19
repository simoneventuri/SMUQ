#########################
# Adding the components #
#########################
#add_subdirectory( CoarrayCollective )         # -
add_subdirectory( Algebra     )               # Dependencies: -
add_subdirectory( PhysParam   )               # Dependencies: -
add_subdirectory( DateAndTime )               # Dependencies: -
add_subdirectory( Template    )               # Dependencies: -
add_subdirectory( Utilities   )               # Dependencies: -
add_subdirectory( String      )               # Dependencies: Utilities
add_subdirectory( Object      )               # Dependencies: Utilities
add_subdirectory( Arithmetic  )               # Dependencies: String
add_subdirectory( Table       )               # Dependencies: String
add_subdirectory( File        )               # Dependencies: Utilities, String
add_subdirectory( Error       )               # Dependencies: String
add_subdirectory( EnvironmentVariable)        # Dependencies: Error
add_subdirectory( CommandLineInterface)       # Dependencies: Utilities, String, Error
add_subdirectory( Logger      )               # Dependencies: Utilities, String, File
#add_subdirectory( Debugger    )               # Dependencies: String, Logger
add_subdirectory( Status      )               # Dependencies: Utilities, String, Logger
add_subdirectory( Comparator  )               # Dependencies: PhysParam, Logger, String
add_subdirectory( Timer       )               # Dependencies: Logger, String
add_subdirectory( Memory      )               # Dependencies: String, Error, Logger, Table
#add_subdirectory( Checker     )               # Dependencies: String, Error, Logger, Object
#add_subdirectory( Fitter      )               # Dependencies: Utilities String, Error, Logger
add_subdirectory( Command     )               # Dependencies: Logger, Utilities, Error, String, File
#add_subdirectory( Plotter     )               # Dependencies: Logger, Command
add_subdirectory( Directory   )               # Dependencies: Utilities, String, Object, File, Command
#add_subdirectory( Todo/url    )               # Dependencies: Utilities, String, Logger
add_subdirectory( InputParser )               # Dependencies: Utilities, Logger, String, Error, CommandLineInterface
#add_subdirectory( Application )               # Dependencies: Utilities, Logger, String, File, Error, Input, CommandLineInterface
#add_subdirectory( Tecplot     )               # Dependencies: ...
# add_subdirectory( Todo/SourceLocation )


#add_subdirectory( Registration  )             # Dependencies: Object, String, Logger
#add_subdirectory( SharedLib )               # Dependencies: ...
