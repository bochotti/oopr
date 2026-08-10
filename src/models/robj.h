// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#ifndef OOPR_MODELS_ROBJECTS_H
#define OOPR_MODELS_ROBJECTS_H
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Various models for R Objects.
 * Due to varying underlying types, templates are used here.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
#include "common.h"
#include "util/psexp.h"
#include <iterator>
#include <cstddef>
#include <vector>
class RObjR{};
class RSym;
template<typename P> class RStr;
template<typename P> class RChr;
constexpr SEXPTYPE ALLSXP = static_cast<SEXPTYPE>(-1);
#define ENABLE_IF(T, E, ...)                                   \
  template <                                                   \
    typename T                                                 \
   ,typename std::enable_if<__VA_ARGS__::value, int>::type E   \
  >
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Generalised class for all R Objects, carries a SEXP object with helpers.
 *
 * Template parameters:
 *   - <P>: PSEXP for protection (created in cpp) or just SEXP (from R).
 *   - <S>: Assert the incoming sexp type. ALLSXP for any type.
 *
 * Default construction starts with R_NilValue regardless of <S>, this is
 * to prevent protecting SEXPs that would be over-written soon after. This
 * does mean that some methods may not work until a SEXP is assigned.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
template<typename P = PSEXP, SEXPTYPE S = ALLSXP>
class RObj : public RObjR
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
static_assert(
  std::is_same<P, PSEXP>::value || std::is_same<P, SEXP>::value
 ,"RObj: template<P> must be either SEXP or PSEXP"
);
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  RObj()  noexcept = default;
  ~RObj() noexcept = default;

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Moveable
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  RObj(RObj&& x)            noexcept = default;
  RObj& operator=(RObj&& x) noexcept = default;

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Can be copied if <P> is SEXP. A PSEXP cannot, as it will own the
   * protection of the underlying SEXP.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  RObj(const RObj& x) : RObj(x.sexp())
  {
    static_assert(
      !std::is_same<P, PSEXP>::value
     ,"RObj: Copy constructor is disabled for <P> = PSEXP"
    );
  }

  ENABLE_IF(U = P, = 0, std::is_same<U, SEXP>)
  RObj& operator=(const RObj& x) { sexp_ = x.sexp(); }

  ENABLE_IF(U = P, = 0, std::is_same<U, PSEXP>)
  RObj& operator=(const RObj& x) = delete;

  ENABLE_IF(U,     = 0, std::is_same<U, SEXP>)
  RObj(const RObj<U, S>& x) : RObj(x.sexp()) { }

  ENABLE_IF(U,     = 0, std::is_same<U, SEXP>)
  RObj& operator=(const RObj<U, S>& x) { sexp_ = x.sexp(); }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Can assign new SEXP objects.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  RObj(const SEXP x)             { set(x); }
  RObj& operator= (const SEXP x) { set(x); return *this; }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Information.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  SEXPTYPE type()            const { return TYPEOF(sexp_); }
  R_xlen_t size()            const { return Rf_xlength(sexp_); }
  bool inhr(const char* cls) const { return Rf_inherits(sexp_, cls); }
  void     print()           const { Rf_PrintValue(sexp_); }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Get the underlying SEXP, or duplicate it.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  SEXP     sexp()          const noexcept { return sexp_; }
  operator SEXP()          const noexcept { return sexp_; }
  operator RObj<SEXP, S>() const noexcept { return RObj<SEXP, S>(sexp_); }
  SEXP     copy()          const          { return Rf_duplicate(sexp_); }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Check if another SEXP object matches the type
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  static bool is(const SEXP x)                 { return TYPEOF(x) == S; }
  bool operator==(const SEXP x) const noexcept { return sexp_ == x; }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Get and set attributes
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  class Attr
  {
  public:
    friend class ::RObj<P, S>;
    using RObj = ::RObj<P, S>;
    Attr(Attr&& x)                 noexcept = default;
    Attr& operator=(Attr&& x)      noexcept = default;
    Attr(const Attr& x)            noexcept = default;

    operator SEXP()                 const { return Rf_getAttrib(x, i); }
    operator ::RObj<SEXP, ALLSXP>() const { return Rf_getAttrib(x, i); }
    Attr& operator=(const SEXP v)       { Rf_setAttrib(x, i, v); return *this; }
    Attr& operator=(const Attr& v)      { Rf_setAttrib(x, i, v); return *this; }
  private:
    Attr(const RObj& x, const SEXP i) : x(x), i(i) { }
    const RObj& x;
    const SEXP  i;
  };
  Attr attr(const RSym& nm);
  const RObj<SEXP, ALLSXP> attr(const RSym& nm) const;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  P sexp_{R_NilValue};
  void set(const SEXP x)
  {
    if constexpr(S != ALLSXP)
    {
      if(TYPEOF(x) != S)
      {
        Rf_error(
          "RObj: SEXPTYPE of incoming object must be `%s`, not `%s`"
          ,Rf_type2char(S), Rf_type2char(TYPEOF(x))
        );
      }
    }
    sexp_ = x;
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // RObj
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Single symbol. Does not need protecting.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class RSym : public RObj<SEXP, SYMSXP>
{
public:
  using RObj::RObj;
  RSym(const char* x) : RSym(Rf_install(x)) { }
  const char* c_str() const { return R_CHAR(PRINTNAME(sexp())); }
  bool operator==(const SEXP x)  const noexcept { return x == sexp(); }
  bool operator==(const char* x) const          { return RSym(x) == sexp(); }
  Attr attr(const RSym& name)             = delete;
  const Attr attr(const RSym& name) const = delete;
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // RSym
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * RObj::Attr reliant on RSym
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
template<typename P, SEXPTYPE S>
typename RObj<P, S>::Attr RObj<P, S>::attr(const RSym& nm)
{
  return {*this, nm};
}

template<typename P, SEXPTYPE S>
const RObj<SEXP, ALLSXP> RObj<P, S>::attr(const RSym& nm) const
{
  return static_cast<RObj<SEXP, ALLSXP>>(const_cast<RObj&>(*this).attr(nm));
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Templated class for vector objects. Holds additional helpers.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
template <typename D, typename T, typename I, typename P, SEXPTYPE S>
class RVec : public RObj<P, S>
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  using RObj<P, S>::RObj;
  RVec(const R_xlen_t size = 0) : RObj<P, S>(Rf_allocVector(S, size)) { }

  RVec(const R_xlen_t size, const T& val) : RVec(size)
  {
    for(R_xlen_t i = 0; i < size; ++i) { D::setv(this->sexp(), i, val); }
  }

  RVec(const T* x, const R_xlen_t size) : RVec(size)
  {
    for(R_xlen_t i = 0; i < size; ++i) { D::setv(this->sexp(), i, x[i]); }
  }

  RVec(std::initializer_list<T> x) : RVec(x.begin(), x.size()) { }

  ENABLE_IF(U = T, = 0, !std::is_const<U>)
  RVec(const std::vector<T>& x) : RVec(x.data(), x.size()) { }

  ENABLE_IF(U = T, = 0, !std::is_same<U, RObj<SEXP, ALLSXP>>)
  RVec(std::initializer_list<std::pair<const char*, T>> x);

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Get names
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool named() const { return this->attr("names") != R_NilValue; }
  RChr<SEXP> names() const;

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Access elements of a vector
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  class Elem
  {
  public:
    friend class ::RVec<D, T, I, P, S>;
    using RVec = ::RVec<D, T, I, P, S>;

    Elem(Elem&& x)                 noexcept = default;
    Elem& operator=(Elem&& x)      noexcept = default;
    Elem(const Elem& x)            noexcept = default;
    // Elem& operator=(const Elem& x) noexcept = default;

    ENABLE_IF(U = T, = 0, std::is_base_of<RObjR, U>)
    operator SEXP()            const { return this->operator T(); }
    operator T()               const { return D::getv(x.sexp(), i); }
    Elem& operator=(const T v)       { D::setv(x.sexp(), i, v); return *this; }
    Elem& operator=(const Elem& v)   { D::setv(x.sexp(), i, v); return *this; }

  private:
    Elem(const RVec& x, const R_xlen_t i ) : x(x), i(i)
    {
      if(!(0 <= i && i < x.size())) Rf_error(
        "index %ld is out of bounds [%d, %ld)"
       ,i, 0, x.size()
      );
    }
    const RVec& x;
    const R_xlen_t i{0};
  };
  Elem    operator[](const R_xlen_t i) { return {*this, i}; }
  const T operator[](const R_xlen_t i) const
  {
    return static_cast<T>(const_cast<RVec&>(*this)[i]);
  }

  Elem    operator[](const RStr<PSEXP>& nm);
  const T operator[](const RStr<PSEXP>& nm) const;

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Iterator for range-based loops
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  class Iter
  {
  public:
    using difference_type = std::ptrdiff_t;
    using value_type      = T;

    Iter(const RVec<D, T, I, P, S>& x, const R_xlen_t i) : x(x), i(i) { }

    Elem  operator*()  const { return {x, i}; }
    Iter& operator++() noexcept { ++i; return *this; }

    bool operator==(const Iter& x) const { return i == x.i; }
    bool operator!=(const Iter& x) const { return i != x.i; }

  private:
    const RVec<D, T, I, P, S>& x;
    R_xlen_t                   i;
  };
  Iter begin() { return {*this, 0}; }
  Iter end()   { return {*this, this->size()}; }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Const pointer to underlying data and const iterator
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  const I* data()     const { return D::ptr(this->sexp()); }
  operator const I*() const { return data(); }

  class CIter
  {
  public:
    using difference_type = std::ptrdiff_t;
    using value_type      = I;

    CIter(const I* ptr) noexcept : p(ptr) { }

    const T operator* () const noexcept { return *p; }
    T*      operator->()       noexcept { return  p; }
    CIter&  operator++()       noexcept { p++; return *this; }

    bool    operator== (const CIter& x) const noexcept { return p == x.p;}
    bool    operator!= (const CIter& x) const noexcept { return p != x.p;}

  private:
    const I* p;
  };
  const CIter begin() const { return data(); }
  const CIter end()   const { return data() + this->size(); }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // RVec
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Normal C types.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
#define RVECTOR(N, T, S, F)                                                    \
  template<typename P = PSEXP>                                                 \
  class N final : public RVec<N<P>, T, T, P, S>                                \
  {                                                                            \
  public:                                                                      \
    friend class ::RVec<N<P>, T, T, P, S>;                                     \
    using RVec = ::RVec<N<P>, T, T, P, S>;                                     \
    using RVec::RVec;                                                          \
                                                                               \
  private:                                                                     \
    static inline T getv(const SEXP x, const R_xlen_t i)                       \
    {                                                                          \
      return F##_ELT(x, i);                                                    \
    }                                                                          \
    static inline void setv(const SEXP x, const R_xlen_t i, T v)               \
    {                                                                          \
      SET_##F##_ELT(x, i, v);                                                  \
    }                                                                          \
    static inline const T* ptr(const SEXP x)                                   \
    {                                                                          \
      return F##_RO(x);                                                        \
    }                                                                          \
  }

RVECTOR(RDbl, double, REALSXP, REAL);
RVECTOR(RInt, int,    INTSXP,  INTEGER);
RVECTOR(RLgl, int,    LGLSXP,  LOGICAL);

#undef RVECTOR


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * A CHARSXP wrapper.
 * Only exposes const methods, as CHARSXPs should be immutable.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
template<typename P = SEXP>
class RStr final : public RVec<RStr<P>, const char, const char, P, CHARSXP>
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  friend class ::RVec<RStr<P>, const char, const char, P, CHARSXP>;
  using RVec = ::RVec<RStr<P>, const char, const char, P, CHARSXP>;
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  RStr(const SEXP x)  : RVec(x) { }
  RStr(const char* x) : RVec(Rf_mkChar(x)) { };

  ENABLE_IF(U = P, = 0, std::is_same<U, SEXP>)
  RStr(const RStr& x) : RStr(x.sexp()) { }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  const RSym sym() const { return Rf_installChar(this->sexp()); }
  operator RSym()  const { return sym(); }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  bool operator==(const char* x) const { return !std::strcmp(x, this->data()); }
  bool operator==(std::string x) const { return x == this->data(); }

  typename RVec::RObj::Attr attr(const RSym& nm)             = delete;
  const typename RVec::RObj::Attr attr(const RSym& nm) const = delete;

  bool named() const                                         = delete;
  RChr<SEXP> names() const                                   = delete;

  using RVec::begin;
  using RVec::end;
  typename RVec::Iter begin()                                = delete;
  typename RVec::Iter end()                                  = delete;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  static inline const char getv(const SEXP x, const R_xlen_t i)
  {
    return R_CHAR(x)[i];
  }
  static inline void setv(const SEXP x, const R_xlen_t i, const char v)
  {
    Rf_error("Cannot set an element of a CHARSXP");
  }
  static inline const char* ptr(const SEXP x)
  {
    return R_CHAR(x);
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // RStr
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Character vector. Additional constructor methods to use C strings.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
template<typename P = PSEXP>
class RChr final : public RVec<RChr<P>, const RStr<SEXP>, SEXP, P, STRSXP>
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  friend class ::RVec<RChr<P>, const RStr<SEXP>, SEXP, P, STRSXP>;
  using RVec = ::RVec<RChr<P>, const RStr<SEXP>, SEXP, P, STRSXP>;
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  using RVec::RVec;
  RChr(const std::vector<const char*>& x) : RChr(x.size())
  {
    for(std::size_t i = 0; i < x.size(); ++i)
    {
      setv(this->sexp(), i, Rf_mkChar(x[i]));
    }
  }

  RChr(const std::vector<std::string>& x) : RChr(x.size())
  {
    for(std::size_t i = 0; i < x.size(); ++i)
    {
      setv(this->sexp(), i, Rf_mkChar(x[i].c_str()));
    }
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  static inline const RStr<SEXP> getv(const SEXP x, const R_xlen_t i)
  {
    return STRING_ELT(x, i);
  }
  static inline void setv(const SEXP x, const R_xlen_t i, const SEXP v)
  {
    SET_STRING_ELT(x, i, v);
  }
  static inline const SEXP* ptr(const SEXP x)
  {
    return STRING_PTR_RO(x);
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // RChr
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * RVec & RVec::Elem reliant on RChr
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
template <typename D, typename T, typename I, typename P, SEXPTYPE S>
ENABLE_IF(U,, !std::is_same<U, RObj<SEXP, ALLSXP>>)
RVec<D, T, I, P, S>::RVec(std::initializer_list<std::pair<const char*, T>> x)
  : RVec(x.size())
{
  RChr<PSEXP> keys(x.size());
  R_xlen_t i{0};
  for(const std::pair<const char*, T>& v: x)
  {
    keys[i] = v.first;
    D::setv(this->sexp(), i, v.second);
    ++i;
  }
  this->attr("names") = keys;
}

template <typename D, typename T, typename I, typename P, SEXPTYPE S>
RChr<SEXP> RVec<D, T, I, P, S>::names() const
{
  SEXP x = RObj<P, S>::attr("names");
  return x == R_NilValue ? RChr<SEXP>() : RChr<SEXP>(x);
}

template <typename D, typename T, typename I, typename P, SEXPTYPE S>
typename RVec<D, T, I, P, S>::Elem RVec<D, T, I, P, S>::operator[](
  const RStr<PSEXP>& nm
)
{
  if(!named()) Rf_error("vector is not named");
  R_xlen_t i{0};
  for(const RStr<SEXP>& name : names())
  {
    if(name == nm) return {*this, i};
    ++i;
  }
  Rf_error("index `%s` is out of bounds", nm.data());
}

template <typename D, typename T, typename I, typename P, SEXPTYPE S>
const T RVec<D, T, I, P, S>::operator[](const RStr<PSEXP>& nm) const
{
  return static_cast<T>(const_cast<RVec&>(*this)[nm]);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * List. Additional constructor methods to use all SEXP objects.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
template<typename P = PSEXP>
class RList final : public RVec<RList<P>, RObj<SEXP, ALLSXP>, SEXP, P, VECSXP>
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  friend class ::RVec<RList<P>, RObj<SEXP, ALLSXP>, SEXP, P, VECSXP>;
  using RVec = ::RVec<RList<P>, RObj<SEXP, ALLSXP>, SEXP, P, VECSXP>;
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  using RVec::RVec;
  RList(std::initializer_list<const SEXP> x) : RList(x.size())
  {
    const SEXP s = this->sexp();
    const SEXP* p = x.begin();
    for(std::size_t i = 0; i < x.size(); ++i) { setv(s, i, *(p + i)); }
  }

  RList(const std::vector<SEXP>& x) : RList(x.size())
  {
    const SEXP s = this->sexp();
    for(std::size_t i = 0; i < x.size(); ++i) { setv(s, i, x[i]); }
  }

  RList(std::initializer_list<std::pair<const char*, const SEXP>> x)
    : RList(x.size())
  {
    const SEXP s = this->sexp();
    RChr<PSEXP> names(x.size());
    R_xlen_t i{0};
    for(const std::pair<const char*, const SEXP>& v : x)
    {
      names[i] = v.first;
      setv(s, i, v.second);
      ++i;
    }
    this->attr("names") = names;
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  static inline const SEXP getv(const SEXP x, const R_xlen_t i)
  {
    return VECTOR_ELT(x, i);
  }
  static inline void setv(const SEXP x, const R_xlen_t i, const SEXP v)
  {
    SET_VECTOR_ELT(x, i, v);
  }
  static inline const SEXP* ptr(const SEXP x)
  {
    return VECTOR_PTR_RO(x);
  }
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // RList
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * An R Environment
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
template<typename P = PSEXP>
class REnv final : public RObj<P, ENVSXP>
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  using RObj<P, ENVSXP>::RObj;

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Create a new environment
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  template<typename T>
  explicit REnv(
    const REnv<T>& parent
   ,const bool     hash = true
   ,const int      size = 29
  )
    : RObj<P, ENVSXP>(R_NewEnv(parent, hash, size))
  { }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Get info
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  RChr<SEXP> names(bool all = true, bool sort = false) const
  {
    return R_lsInternal3(this->sexp(), all ? TRUE : FALSE, sort ? TRUE : FALSE);
  }

  bool locked() const
  {
    return R_EnvironmentIsLocked(this->sexp());
  }

  void lock(bool binds = false)
  {
    R_LockEnvironment(this->sexp(), binds ? TRUE : FALSE);
  }

  REnv<SEXP> parent() const { return R_ParentEnv(this->sexp()); }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Binding information.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  class Bind
  {
  public:
    friend class ::REnv<P>;
    using REnv = ::REnv<P>;
    Bind(Bind&& x)                 noexcept = default;
    Bind& operator=(Bind&& x)      noexcept = default;
    Bind(const Bind& x)            noexcept = default;

    bool exists() const       { return R_existsVarInFrame(x, nm); }
    void assign(const SEXP v) { Rf_defineVar(nm, v, x); }
    void remove()             { R_removeVarFromFrame(nm, x); }

    RObj<SEXP, ALLSXP> get() const { return R_getVar(nm, x, FALSE); };
    RObj<SEXP, ALLSXP> get0(const SEXP ifnotfound = R_NilValue) const
    {
      return R_getVarEx(nm, x, FALSE, ifnotfound);
    }

    operator RObj<SEXP, ALLSXP>()    { return get(); }
    operator SEXP() const            { return get(); }
    Bind& operator=(const SEXP   v)  { assign(v); return *this; }
    Bind& operator=(const Bind& v)   { assign(v); return *this; }

    bool locked() const { return R_BindingIsLocked(nm, x); }
    void lock(bool on) { on ? R_LockBinding(nm, x) : R_unLockBinding(nm, x); }

    bool active() const { return R_BindingIsActive(nm, x); }
    RObj<SEXP, CLOSXP> fun() const { return R_ActiveBindingFunction(nm, x); }
    void fun(const RObj<SEXP, CLOSXP>& v) { R_MakeActiveBinding(nm, v, x); }

  private:
    Bind(const REnv& x, const RSym& nm) : x(x), nm(nm.sexp()) { }
    const REnv& x;
    const RSym  nm;
  };

  Bind       operator[](const RSym& nm)       { return {*this, nm }; }
  const Bind operator[](const RSym& nm) const { return {*this, nm }; }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // REnv
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#undef ENABLE_IF
#endif /* OOPR_MODELS_ROBJECTS_H */
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
