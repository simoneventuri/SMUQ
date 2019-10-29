add_sources (
  gpf_plot_iteration_class.f90
  gpf_plot_every_class.f90
  gpf_plot_using_class.f90
  gpf_plot_title_class.f90
  gpf_plot_with_class.f90
  gpf_plot_class.f90
)

# include $(SPARKHOME)/Make.inc
#
# OBJ =   gpf_plot_iteration_class.o      \
#         gpf_plot_every_class.o          \
#         gpf_plot_using_class.o          \
#         gpf_plot_title_class.o          \
#         gpf_plot_with_class.o           \
#         gpf_plot_class.o
#
# all:
#         @$(MAKE) .compile_current_directory
#
# clean:
#         @$(MAKE) .clean_current_directory
#
# cleanall:
#         @$(MAKE) .cleanall_current_directory
#
