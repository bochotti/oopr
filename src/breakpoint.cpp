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
  InstanceFinder(SEXP name, SEXP pkg, SEXP fun)
  {
    name_  = CHAR(STRING_ELT(name, 0));
    clazz_ = Rf_install(name_.c_str());
    pkg_   = getEnvName(pkg);
    if(Rf_isString(fun))
    {
      fun_ = CHAR(STRING_ELT(fun, 0));
    }
    skip   = pkg_.empty();
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void walk(SEXP x)
  {
    if(is_oopr(x))
    {
      searchOopr(x);
    }
    else if(Rf_isEnvironment(x))
    {
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
    else if(Rf_isList(x))
    {
      searchCAR(x);
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
  SEXP        clazz_;
  std::string pkg_;
  std::string fun_;
  Symbols sym{"meta", "encl", "this", ".this", "format.default"};
  std::vector<SEXP> searched_;
  std::vector<SEXP> instances_;

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  std::string getEnvName(SEXP x)
  {
    pSEXP call = Rf_lang2(sym.get("format.default"), x);
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

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void searchCAR(SEXP x)
  {
    for(; x != R_NilValue; x = CDR(x)) walk(CAR(x));
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void searchOopr(SEXP x)
  {
    if(alreadySearched(x)) return;
    searched_.push_back(x);

    SEXP clazz = Rf_installChar(STRING_ELT(Rf_getAttrib(x, R_ClassSymbol), 0));
    SEXP encl  = ENCLOS(x);

    // check if the instance came from the ooprC
    if(clazz_ == clazz && getEnvName(ENCLOS(encl)) == pkg_)
    {
      instances_.push_back(encl);
    }
    // check if the instance inherits from the ooprC. must check:
    //   1. That the enclosure holds an instance of the ooprC
    //   2. The instances own ooprC to see if member exists and not overridden
    else if(!fun_.empty() && is_oopr(Rf_findVarInFrame(encl, clazz_)))
    {
      SEXP ooprC = Rf_findVarInFrame(ENCLOS(encl), clazz);
      if(is_ooprC(ooprC, clazz))
      {
        OoprMeta meta(Rf_getAttrib(ooprC, Rf_install("meta")));
        const int i = meta.which(fun_);
        if(i >= 0 && meta.inherit(i) == clazz_)
        {
          instances_.push_back(encl);
        }
      }
    }
    searchEnv(encl);
  }
};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP find_instances(SEXP ooprC, SEXP frames, SEXP fun)
{
  if(!is_ooprC(ooprC))                              return R_NilValue;
  SEXP name = getAttrib(ooprC, Rf_install("name"));
  SEXP encl = getAttrib(ooprC, Rf_install("encl"));
  SEXP pkg  = ENCLOS(encl);
  InstanceFinder obj(name, pkg, fun);
  if(obj.skip)                                      return R_NilValue;
  obj.walk(pkg);
  if(pkg != R_GlobalEnv)   obj.walk(R_GlobalEnv);
  if(frames != R_NilValue) obj.walk(frames);
  return obj.toList();
}
