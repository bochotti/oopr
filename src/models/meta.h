// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#ifndef OOPR_MODELS_META_H
#define OOPR_MODELS_META_H
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "common.h"
#include <vector>
#include <string>
#include "./util/psexp.h"
#include "./util/symbols.h"
#include "./models/robj.h"
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Data model for the meta object
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class OoprMeta
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  static bool is(const RObj<SEXP, ALLSXP> x);
  OoprMeta(const RObj<SEXP, ALLSXP> x, const bool check = true);

  R_xlen_t size()                                const;
  RSym name(const int i)                         const;
  RSym inherit(const int i)                      const;
  bool isAccess(const int i, const char* access) const;
  bool isMethod(const int i)                     const;
  bool isProperty(const int i)                   const;
  bool isStatic(const int i)                     const;
  bool isClass(const int i)                      const;
  bool isInherit(const int i)                    const;
  bool isVirtual(const int i)                    const;
  int  which(const std::string& name)            const;
  RChr<PSEXP> subName(
    const char* access
   ,const bool  inverse = false
  )                                              const;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  const REnv<SEXP> meta_;
  const RChr<SEXP> names_;
  const RChr<SEXP> access_;
  const RLgl<SEXP> method_;
  const RChr<SEXP> property_;
  const RLgl<SEXP> static_;
  const RLgl<SEXP> class_;
  const RChr<SEXP> inherit_;
  const RLgl<SEXP> virtual_;

};
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#endif /* OOPR_MODELS_META_H */
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
