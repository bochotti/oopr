// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#ifndef OOPR_MODELS_OOPR_H
#define OOPR_MODELS_OOPR_H
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "common.h"
#include "./models/meta.h"
#include <string>
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
class OoprC;
class Oopr;
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
class Oopr : public REnv<SEXP>
{
public:
  static bool is(const RObj<SEXP, ALLSXP> intf, const RChr<SEXP> name = { });

  Oopr(const RObj<SEXP, ALLSXP> intf, const bool check = true);
  REnv<SEXP> encl;
  REnv<SEXP> thiz;

};

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Check if a SEXP object is an ooprC
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class OoprC : public RObj<SEXP, CLOSXP>
{
public:
  static bool is(const RObj<SEXP, ALLSXP> gen, const RChr<SEXP> name = { });

  OoprC(const RObj<SEXP, ALLSXP> gen, const bool check = true);
  const RChr<SEXP>         name;
  const RChr<SEXP>         inhr;
  const OoprMeta           meta;
  const REnv<SEXP>         encl;
  const REnv<SEXP>         thiz;
  const Oopr               oopr;
};

bool is_ooprC(SEXP obj, const std::string& name = "");
bool is_ooprC(SEXP obj, SEXP name);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Check if a SEXP object is an oopr
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
bool is_oopr(SEXP obj, const std::string& name = "");
bool is_oopr(SEXP obj, SEXP name);



// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#endif /* OOPR_MODELS_OOPR_H */
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
