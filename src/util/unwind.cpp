// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "unwind.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
struct RUnWind::Data{ SEXP expr; SEXP envir; };
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP RUnWind::fun(void *data)
{
  Data *data_ = static_cast<Data*>(data);
  return Rf_eval(data_->expr, data_->envir);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
void RUnWind::clean(void* data, Rboolean jump)
{
  if(jump)
  {
    pSEXP cont = std::move(*static_cast<pSEXP*>(data));
    throw exception(cont);
  }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP RUnWind::eval(SEXP expr, SEXP envir)
{
  Data data{expr, envir};
  pSEXP cont = R_MakeUnwindCont();
  return R_UnwindProtect(fun, &data, clean, &cont, cont);
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
RUnWind::exception::exception(pSEXP& cont)
  : runtime_error("")
  , cont(std::move(cont))
{ }
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
RUnWind::exception::~exception()
{
  if(cont != R_NilValue) R_ContinueUnwind(cont);
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
const char* RUnWind::exception::what() const noexcept
{
  SEXP msg = Rf_eval(Rf_lang1(Rf_install("geterrmessage")), R_BaseEnv);
  return (Rf_length(msg) == 0) ? "" : Rf_translateChar(STRING_ELT(msg, 0));
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
