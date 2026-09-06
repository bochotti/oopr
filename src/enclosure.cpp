// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "enclosure.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP interface(SEXP env, SEXP nme, SEXP nms, SEXP cls, bool chk) try
{
  if(!REnv<>::is(env)) stop("`env` must be an environment");
  const REnv<SEXP> from(env);

  if(!(Rf_isNull(nms) || RChr<>::is(nms)))
  {
    stop("`nms` must be a character vector");
  }
  const RChr<PSEXP> names(Rf_isNull(nms) ? from.names() : RChr<PSEXP>(nms));

  const R_xlen_t len = names.size();
  REnv<PSEXP> out(from.parent(), true, len);

  if(!(Rf_isNull(cls) || RChr<>::is(cls)))
  {
    stop("`cls` must be a character vector");
  }
  out.attr(R_ClassSymbol) = Rf_isNull(cls) ? *from.attr(R_ClassSymbol) : cls;

  for(const RSym name : names)
  {
    const REnv<SEXP>::Bind fr(from[name]);
    REnv<PSEXP>::Bind      to(out[name]);
    if(fr.active())
    {
      to.fun = fr.fun;
    }
    else
    {
      const RObj<SEXP, ALLSXP> mem(fr.get0());
      if(mem.type() == CLOSXP)
      {
        to = mem;
      }
      else
      {
        symlinkR(*from, nme, *out, *name, chk);
      }
    }
    if(fr.locked()) to.lock(true);
  }
  if(from.locked()) out.lock();
  return *out;
}
catchR
