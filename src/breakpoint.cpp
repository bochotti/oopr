// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "breakpoint.h"
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
class InstanceFinder
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  bool skip = false;
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  InstanceFinder(SEXP name, SEXP pkg)
  {
    name_ = CHAR(STRING_ELT(name, 0));
    pkg_  = getEnvName(pkg);
    skip  = pkg_.empty();
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void walk(SEXP x)
  {
    if(Rf_isEnvironment(x))
    {
      if(is_oopr(x))
      {
        // check that the topenv is the same, by name... because
        // devtools::load_all() creates a different namespace
        if(   Rf_inherits(x, name_.c_str())
           && getEnvName(ENCLOS(ENCLOS(x))) == pkg_
        )
        {
          instances_.push_back(ENCLOS(x));
        }
        // swap from interface to this
        searched_.push_back(x);
        x = Rf_findVarInFrame(ENCLOS(x), sym.get("this"));
      }
      searchEnv(x);
    }
    else if(Rf_isNewList(x))
    {
      searchList(x);
    }
    else if(is_ooprC(x))
    {
      searchOoprC(x);
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  SEXP toList()
  {
    pSEXP out = Rf_allocVector(VECSXP, (R_xlen_t)instances_.size());
    for(std::size_t i = 0; i < instances_.size(); ++i)
    {
      SET_VECTOR_ELT(out, (R_xlen_t)i, instances_[i]);
    }
    return(out);
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  std::string name_;
  std::string pkg_;
  Symbols sym{"meta", "encl", "this", ".this", "environmentName"};
  std::vector<SEXP> searched_;
  std::vector<SEXP> instances_;

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  std::string getEnvName(SEXP x)
  {
    pSEXP call = Rf_lang2(sym.get("environmentName"), x);
    return CHAR(STRING_ELT(Rf_eval(call, R_BaseEnv), 0));
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  bool alreadySearched(SEXP x)
  {
    for(std::size_t i = 0; i < searched_.size(); ++i)
    {
      if(x == searched_[i]) return true;
    }
    return false;
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void searchEnv(SEXP x)
  {
    if(alreadySearched(x)) return;
    searched_.push_back(x);
    SEXP names = R_lsInternal3(x, TRUE, FALSE);
    const R_xlen_t len = Rf_xlength(names);
    for(R_xlen_t i = 0; i < len; ++i)
    {
      SEXP name = Rf_installChar(STRING_ELT(names, i));
      if(!R_BindingIsActive(name, x)) walk(Rf_findVarInFrame(x, name));
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void searchList(SEXP x)
  {
    const R_xlen_t len = Rf_xlength(x);
    for(R_xlen_t i = 0; i < len; ++i) walk(VECTOR_ELT(x, i));
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void searchOoprC(SEXP x)
  {
    SEXP encl = Rf_getAttrib(x, sym.get("encl"));
    SEXP thiz = Rf_findVarInFrame(encl, sym.get("this"));

    if(alreadySearched(thiz)) return;
    searched_.push_back(encl);
    searched_.push_back(Rf_findVarInFrame(encl, sym.get(".this")));
    searched_.push_back(thiz);

    // search inside static members
    OoprMeta meta(Rf_getAttrib(x, sym.get("meta")));
    for(R_xlen_t i = 0; i < meta.size(); ++i)
    {
      if(meta.isStatic(i)) walk(Rf_findVarInFrame(thiz, meta.name(i)));
    }
  }
};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP find_instances(SEXP ooprC)
{
  if(!is_ooprC(ooprC))                              return R_NilValue;
  SEXP name = getAttrib(ooprC, Rf_install("name"));
  SEXP encl = getAttrib(ooprC, Rf_install("encl"));
  SEXP pkg  = ENCLOS(encl);
  InstanceFinder obj(name, pkg);
  if(obj.skip)                                      return R_NilValue;
  obj.walk(pkg);
  if(pkg != R_GlobalEnv) obj.walk(R_GlobalEnv);
  return obj.toList();
}
