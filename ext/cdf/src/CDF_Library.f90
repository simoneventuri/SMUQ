module CDF_Library

implicit none

private                                                               ::    Beta
public                                                                ::    F_Beta

interface F_Beta
  module procedure                                                    ::    Beta
end interface

contains

# include "split_files/beta.f90"
# include "split_files/beta_grat.f90"
# include "split_files/beta_inc.f90"
# include "split_files/beta_inc_values.f90"
# include "split_files/binomial_cdf_values.f90"
# include "split_files/cdfbet.f90"
# include "split_files/cdfbin.f90"
# include "split_files/cdfchi.f90"
# include "split_files/cdfchn.f90"
# include "split_files/cdff.f90"
# include "split_files/cdffnc.f90"
# include "split_files/cdfgam.f90"
# include "split_files/cdfnbn.f90"
# include "split_files/cdfnor.f90"
# include "split_files/cdfpoi.f90"
# include "split_files/cdft.f90"
# include "split_files/chi_noncentral_cdf_values.f90"
# include "split_files/chi_square_cdf_values.f90"
# include "split_files/cumbet.f90"
# include "split_files/cumbin.f90"
# include "split_files/cumchi.f90"
# include "split_files/cumchn.f90"
# include "split_files/cumf.f90"
# include "split_files/cumfnc.f90"
# include "split_files/cumgam.f90"
# include "split_files/cumnbn.f90"
# include "split_files/cumpoi.f90"
# include "split_files/cumt.f90"
# include "split_files/dbetrm.f90"
# include "split_files/dexpm1.f90"
# include "split_files/dinvr.f90"
# include "split_files/dlanor.f90"
# include "split_files/dzror.f90"
# include "split_files/erf_values.f90"
# include "split_files/f_cdf_values.f90"
# include "split_files/f_noncentral_cdf_values.f90"
# include "split_files/gamma.f90"
# include "split_files/gamma_inc.f90"
# include "split_files/gamma_inc_inv.f90"
# include "split_files/gamma_inc_values.f90"
# include "split_files/gamma_rat1.f90"
# include "split_files/gamma_values.f90"
# include "split_files/negative_binomial_cdf_values.f90"
# include "split_files/normal_01_cdf_values.f90"
# include "split_files/normal_cdf_values.f90"
# include "split_files/poisson_cdf_values.f90"
# include "split_files/psi_values.f90"
# include "split_files/student_cdf_values.f90"
# include "split_files/timestamp.f90"

end module

# include "split_files/eval_pol.f90"
# include "split_files/gam1.f90"
# include "split_files/rlog.f90"
# include "split_files/ipmpar.f90"
# include "split_files/gamma_ln1.f90"
# include "split_files/error_f.f90"
# include "split_files/error_fc.f90"
# include "split_files/alnrel.f90"
# include "split_files/rexp.f90"
# include "split_files/gamma_log.f90"
# include "split_files/rcomp.f90"
# include "split_files/exparg.f90"
# include "split_files/dinvnr.f90"
# include "split_files/dstrem.f90"
# include "split_files/dt1.f90"
# include "split_files/beta_rcomp1.f90"
# include "split_files/rlog1.f90"
# include "split_files/bcorr.f90"
# include "split_files/beta_log.f90"
# include "split_files/algdiv.f90"
# include "split_files/apser.f90"
# include "split_files/fpser.f90"
# include "split_files/beta_pser.f90"
# include "split_files/beta_frac.f90"
# include "split_files/beta_up.f90"
# include "split_files/beta_asym.f90"
# include "split_files/beta_rcomp.f90"
# include "split_files/psi.f90"
# include "split_files/gsumln.f90"
# include "split_files/stvaln.f90"
# include "split_files/cumnor.f90"
# include "split_files/esum.f90"
# include "split_files/r8_swap.f90"
