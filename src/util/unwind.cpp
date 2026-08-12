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
    PSEXP cont = std::move(*static_cast<PSEXP*>(data));
    throw exception(cont);
  }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP RUnWind::eval(SEXP expr, SEXP envir)
{
  Data data{expr, envir};
  PSEXP cont = R_MakeUnwindCont();
  return R_UnwindProtect(fun, &data, clean, &cont, cont);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP RUnWind::call(void* data, SEXP (*fun)(void* data))
{
  PSEXP cont = R_MakeUnwindCont();
  return R_UnwindProtect(fun, data, clean, &cont, cont);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
void RUnWind::stop(const char* msg)
{
  throw exception(msg);
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
RUnWind::exception::exception(PSEXP& cont, const char* msg)
  : runtime_error(msg)
  , cont(std::move(cont))
{ }
RUnWind::exception::exception(const char* msg)
  : runtime_error(msg)
{ }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
RUnWind::exception::~exception()
{
  if(!cont.empty())
  {
    SEXP cont = this->cont;
    this->cont.release();
    if(cont != R_NilValue) R_ContinueUnwind(cont);
  }
  if(strlen(runtime_error::what())) Rf_error(runtime_error::what());
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
const char* RUnWind::exception::what() const noexcept
{
  SEXP msg = Rf_eval(Rf_lang1(Rf_install("geterrmessage")), R_BaseEnv);
  return (Rf_length(msg) == 0) ? "" : Rf_translateChar(STRING_ELT(msg, 0));
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
