// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#ifndef OOPR_UTIL_UNWIND_H
#define OOPR_UTIL_UNWIND_H
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "common.h"
#include "psexp.h"
#include <stdexcept>
#include <vector>
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Evaluate an R expression, allowing for destruction of cpp objects.
 * Wrap RUnWind::eval in a try block, and catch(const RUnWind::exception& e).
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class RUnWind
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Evaluate an R expression.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  static SEXP eval(const SEXP expr, const SEXP envir);

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Call a C function. Would be a good idea to pass custom struct to data.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  static SEXP call(void* data, SEXP (*fun)(void* data));

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Throw a C++ exception.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  static void stop(const char* msg = "");

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Exception to catch.
   * .what() pulls the last error message when called.
   * Destructor will:
   *   -  Call R_ContinueUnwind unless .cont = R_NilValue.
   *   -  If msg != "", then Rf_error (long jump) will be called.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  class exception : public std::runtime_error
  {
  public:
    exception(PSEXP& cont, const char* msg = "");
    exception(const char* msg);
    ~exception();
    exception(exception&& x) = default;
    PSEXP cont;
    const char* what() const noexcept;
  };

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  struct Data;
  static SEXP fun(void *data);
  static void clean(void* data, Rboolean jump);
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // RUnwind
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Use this instead of Rf_error, as Rf_error causes long-jump.
 * As-long as all the R API endpoints can catch it, ofcourse.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
template<typename... Args>
inline void stop(const char* fmt, Args&&... args)
{
  int sz = std::snprintf(nullptr, 0, fmt, std::forward<Args>(args)...);
  std::vector<char> msg(sz + 1, '\0');
  std::sprintf(msg.data(), fmt, std::forward<Args>(args)...);
  RUnWind::stop(const_cast<const char*>(msg.data()));
}
inline void stop(const char* msg) { RUnWind::stop(msg); }
#define catchR catch(const RUnWind::exception& e) { return R_NilValue; }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#endif /* OOPR_UTIL_UNWIND_H */
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
