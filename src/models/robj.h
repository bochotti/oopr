// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#ifndef OOPR_MODELS_ROBJECTS_H
#define OOPR_MODELS_ROBJECTS_H
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Various models for R Objects.
 * Due to varying underlying types, templates are used here.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
#include "common.h"
#include "./util/unwind.h"
#include "./util/psexp.h"
#include <iterator>
#include <cstddef>
#include <vector>
#include <type_traits>
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
constexpr SEXPTYPE ALLSXP = static_cast<SEXPTYPE>(-1);
namespace ROBJ
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
class RObjR{};
class RSym;
template<typename P> class RStr;
template<typename P> class RChr;
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
 * Note: default construction starts with R_NilValue regardless of <S>, this is
 * to prevent protecting SEXPs that would be over-written soon after. This
 * does mean that some methods may not work until a SEXP is assigned.
 *
 * Traits are used to allow conversion for RObj types.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
template<typename P, SEXPTYPE S>
struct RObjT : RObjR
{
  using           p = P;
  enum SEXPTYPE { s = S };
};

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * In the default case, incoming type T must convertible to a SEXP.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
template<typename T, typename P, SEXPTYPE S, typename = void>
struct Traits
{
  using           p = SEXP;
  enum SEXPTYPE { s = ALLSXP };

  static constexpr bool IsSValid   = true;
  static constexpr bool IsPValid   = true;
  static constexpr bool IsSEXP     =
       std::is_constructible<SEXP, const T&>::value
    || std::is_convertible  <const T&, SEXP>::value;

  static constexpr bool CanCopy    = IsSEXP;
  static constexpr bool CanMove    = false;
};

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * If incoming type T is an RObj, then:
 * For P:
 *   1. SEXP  -> SEXP
 *   2. SEXP  -> PSEXP
 *   3. PSEXP -> SEXP
 *   4. PSEXP -> PSEXP (move only)
 * For S:
 *   1. S      -> S
 *   2. ALLSXP -> S
 *   3. S      -> ALLSXP
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
template <typename T>
constexpr bool IsRObj()
{
  return std::is_base_of<RObjR, typename std::decay<T>::type>::value;
}

template<typename T, typename P, unsigned int S>
struct Traits<T, P, S, typename std::enable_if<IsRObj<T>()>::type>
{
  using DT = typename std::decay<T>::type;
  using CT = typename std::remove_reference<T>::type;
  using p           = typename DT::p;
  enum SEXPTYPE { s = DT::s };

  static constexpr bool IsSValid = S == ALLSXP || s == ALLSXP || S == s;
  static constexpr bool IsPValid = !(
       std::is_same<typename std::decay<p>::type, PSEXP>::value
    && std::is_same<typename std::decay<P>::type, PSEXP>::value
  );
  static constexpr bool IsSEXP   = false;

  static constexpr bool CanCopy  = IsSValid && IsPValid;
  static constexpr bool CanMove  = !std::is_const<CT>::value && IsSValid;
};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
template<typename P = PSEXP, SEXPTYPE S = ALLSXP>
class RObj : public RObjT<P, S>
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
static_assert(
  std::is_same<P, PSEXP>::value || std::is_same<P, SEXP>::value
 ,"RObj: template<P> must be either SEXP or PSEXP"
);
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  template <typename U, typename R = int>
  using EnableCopy = typename std::enable_if<Traits<U, P, S>::CanCopy, R>::type;

  template <typename U, typename R = int>
  using EnableMove = typename std::enable_if<Traits<U, P, S>::CanMove, R>::type;

public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  RObj()  noexcept                   = default;
  ~RObj() noexcept                   = default;
  RObj(const RObj&)                  = default;
  RObj& operator=(const RObj&)       = default;
  RObj(RObj&& x)            noexcept = default;
  RObj& operator=(RObj&& x) noexcept = default;

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Copyable if allowed by the traits
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  template<typename U>
  RObj(const U& x, EnableCopy<U, int*> = nullptr)
  {
    set<U>(static_cast<SEXP>(x));
  }

  template<typename U, EnableCopy<U, int> = 0>
  RObj& operator=(const U& x)
  {
    set<U>(static_cast<SEXP>(x));
    return static_cast<RObj&>(*this);
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Moveable if allowed by the traits
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  template<typename U>
  RObj(U&& x, EnableMove<U, int*> = nullptr) : RObj(std::move(x.sexp())) { }

  template<typename U, EnableMove<U, int> = 0>
  RObj& operator=(U&& x)
  {
    if(static_cast<const void*>(this) != static_cast<const void*>(&x))
    {
      set<U>(std::move(x.sexp()));
    }
    return static_cast<RObj&>(*this);
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Information.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  SEXPTYPE   type()                const { return TYPEOF(sexp_); }
  R_xlen_t   size()                const { return Rf_xlength(sexp_); }
  bool       inhr(const char* cls) const { return Rf_inherits(sexp_, cls); }
  void       print()               const { Rf_PrintValue(sexp_); }
  const RChr<SEXP> cls()           const;

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Get the underlying SEXP using * operator, or duplicate it.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  SEXP     operator*()     const noexcept { return sexp_; }
  SEXP     sexp()          const noexcept { return **this; }
  explicit operator SEXP() const noexcept { return **this; }
  operator RObj<SEXP, S>() const noexcept { return **this; }
  SEXP     copy()          const          { return Rf_duplicate(sexp_); }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Check if another SEXP object matches the type
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  static bool is(const SEXP x)                 { return TYPEOF(x) == S; }
  bool operator==(const SEXP x) const noexcept { return sexp_ == x; }
  bool operator!=(const SEXP x) const noexcept { return sexp_ != x; }
  template<typename T, SEXPTYPE K>
  bool operator==(const RObj<T, K>& x) const noexcept { return x == sexp_; }
  template<typename T, SEXPTYPE K>
  bool operator!=(const RObj<T, K>& x) const noexcept { return x != sexp_; }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Get and set attributes
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  class Attr
  {
  public:
    friend class ROBJ::RObj<P, S>;
    using RObj = ROBJ::RObj<P, S>;
    Attr(Attr&& x)             noexcept = default;
    Attr& operator=(Attr&& x)  noexcept = default;
    Attr(const Attr& x)        noexcept = default;

    SEXP     operator *()               const { return Rf_getAttrib(x, i); }
    SEXP     sexp()                     const { return **this; }
    explicit operator SEXP()            const { return **this; }
    operator ROBJ::RObj<SEXP, ALLSXP>() const { return **this; }
    operator bool()                     const { return **this != R_NilValue; }
    Attr& operator=(const SEXP v)   { Rf_setAttrib(x, i, v);  return *this; }
    Attr& operator=(const ROBJ::RObj<SEXP, ALLSXP> v) { return *this = *v; }
    Attr& operator=(const Attr& v)                    { return *this = *v; }
  private:
    Attr(const SEXP x, const SEXP i) : x(x), i(i) { }
    const SEXP x;
    const SEXP i;
  };
  Attr attr(const RSym& nm);
  const RObj<SEXP, ALLSXP> attr(const RSym& nm) const;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  P sexp_{R_NilValue};
  template<typename U>
  typename std::enable_if<(S == ALLSXP || Traits<U, P, S>::s == S), void>::type
  set(const SEXP x)
  {
    sexp_ = x;
  }
  template<typename U>
  typename std::enable_if<(S != ALLSXP && Traits<U, P, S>::s != S), void>::type
  set(const SEXP x)
  {
    if(TYPEOF(x) != S) stop(
      "RObj: SEXPTYPE of incoming object must be `%s`, not `%s`"
     ,Rf_type2char(S), Rf_type2char(TYPEOF(x))
    );
    sexp_ = x;
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // RObj
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //

template <typename P1, SEXPTYPE S1, typename P2, SEXPTYPE S2, bool C, bool M>
struct TestRObj
{
  using T1 = RObj<P1, S1>;
  using T2 = RObj<P2, S2>;
  static constexpr bool cc = std::is_constructible<T1,  const T2&>::value == C;
  static constexpr bool ca = std::is_assignable   <T1&, const T2&>::value == C;
  static constexpr bool mc = std::is_constructible<T1,  T2&&>::value == M;
  static constexpr bool ma = std::is_assignable   <T1&, T2&&>::value == M;
  static constexpr bool v  = cc && ca && mc && ma;
  static_assert(cc, "failed copy construction");
  static_assert(ca, "failed copy assignment");
  static_assert(mc, "failed move construction");
  static_assert(ma, "failed move assignment");
};

// Different P, same S.
static_assert(TestRObj<SEXP,  0, SEXP,  0, 1, 1>::v, "SEXP ->SEXP,  same S");
static_assert(TestRObj<SEXP,  0, PSEXP, 0, 1, 1>::v, "SEXP ->PSEXP, same S");
static_assert(TestRObj<PSEXP, 0, SEXP,  0, 1, 1>::v, "PSEXP->SEXP,  same S");
static_assert(TestRObj<PSEXP, 0, PSEXP, 0, 0, 1>::v, "PSEXP->PSEXP, same S");

// Different P, different S
static_assert(TestRObj<SEXP,  0, SEXP,  1, 0, 0>::v, "SEXP ->SEXP,  diff S");
static_assert(TestRObj<SEXP,  0, PSEXP, 1, 0, 0>::v, "SEXP ->PSEXP, diff S");
static_assert(TestRObj<PSEXP, 0, SEXP,  1, 0, 0>::v, "PSEXP->SEXP,  diff S");
static_assert(TestRObj<PSEXP, 0, PSEXP, 1, 0, 0>::v, "PSEXP->PSEXP, diff S");

// ALLSXP exception
static_assert(TestRObj<SEXP,  -1u, SEXP,    0, 1, 1>::v, "SEXP ->SEXP,  ALL");
static_assert(TestRObj<SEXP,  -1u, PSEXP,   0, 1, 1>::v, "SEXP ->PSEXP, ALL");
static_assert(TestRObj<PSEXP, -1u, SEXP,    0, 1, 1>::v, "PSEXP->SEXP,  ALL");
static_assert(TestRObj<PSEXP, -1u, PSEXP,   0, 0, 1>::v, "PSEXP->PSEXP, ALL");
static_assert(TestRObj<SEXP,    0, SEXP,  -1u, 1, 1>::v, "SEXP ->SEXP,  ALL");
static_assert(TestRObj<SEXP,    0, PSEXP, -1u, 1, 1>::v, "SEXP ->PSEXP, ALL");
static_assert(TestRObj<PSEXP,   0, SEXP,  -1u, 1, 1>::v, "PSEXP->SEXP,  ALL");
static_assert(TestRObj<PSEXP,   0, PSEXP, -1u, 0, 1>::v, "PSEXP->PSEXP, ALL");

static_assert(
  sizeof(RObj<SEXP, ALLSXP>) == sizeof(SEXP)
 ,"RObj<SEXP> must be the same size as a SEXP"
);
static_assert(
  sizeof(RObj<PSEXP, ALLSXP>) == sizeof(SEXP)
 ,"RObj<PSEXP> must be the same size as a SEXP"
);

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Single symbol. Does not need protecting.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class RSym : public RObj<SEXP, SYMSXP>
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  using RObj::RObj;
  RSym(const char* x) : RSym(Rf_install(x)) { }
  template<typename T>
  RSym(const RStr<T> x);

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  const char* c_str() const { return R_CHAR(PRINTNAME(sexp())); }
  RStr<SEXP>  chr()   const;
  operator RStr<SEXP>() const;

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  using RObj::operator==;
  bool operator==(const RSym& x) const noexcept { return x.sexp() == sexp(); }
  bool operator==(const char* x) const    { return !std::strcmp(x, c_str()); }
  template<typename T>
  bool operator==(const RStr<T>& x) const;

  using RObj::operator!=;
  bool operator!=(const RSym& x) const noexcept { return x.sexp() != sexp(); }
  bool operator!=(const char* x) const    { return  std::strcmp(x, c_str()); }
  template<typename T>
  bool operator!=(const RStr<T>& x) const;

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
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
  return Attr(**this, *nm);
}

template<typename P, SEXPTYPE S>
const RObj<SEXP, ALLSXP> RObj<P, S>::attr(const RSym& nm) const
{
  return Rf_getAttrib(**this, *nm);
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
    const SEXP s = **this;
    for(R_xlen_t i = 0; i < size; ++i) { D::setv(s, i, val); }
  }

  RVec(const T* x, const R_xlen_t size) : RVec(size)
  {
    const SEXP s = **this;
    for(R_xlen_t i = 0; i < size; ++i) { D::setv(s, i, x[i]); }
  }

  RVec(std::initializer_list<T> x) : RVec(x.begin(), x.size()) { }

  ENABLE_IF(U = T, = 0, !std::is_const<U>)
  RVec(const std::vector<T>& x) : RVec(x.data(), x.size()) { }

  ENABLE_IF(U = T, = 0, !std::is_same<U, RObj<SEXP, ALLSXP>>)
  RVec(std::initializer_list<std::pair<const char*, T>> x);

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Get names
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool named()       const { return this->attr(R_NamesSymbol) != R_NilValue; }
  RChr<SEXP> names() const;

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Access elements of a vector
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  class Elem
  {
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  public:
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
    friend class ROBJ::RVec<D, T, I, P, S>;
    using RVec = ROBJ::RVec<D, T, I, P, S>;

    Elem(Elem&& x)                 noexcept = default;
    Elem& operator=(Elem&& x)      noexcept = default;
    Elem(const Elem& x)            noexcept = delete;

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
    ENABLE_IF(U = T, = 0, std::is_base_of<RObjR, U>)
    SEXP operator*() const { return static_cast<SEXP>(D::getv(x, i)); }
    ENABLE_IF(U = T, = 0, std::is_base_of<RObjR, U>)
    explicit operator SEXP()   const { return **this; }
    operator T()               const { return D::getv(x, i); }
    Elem& operator=(const T v)
    {
      D::setv(x, i, static_cast<SEXP>(v));
      return *this;
    }
    Elem& operator=(const Elem& v)   { return *this = static_cast<T>(v); }

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
    bool operator==(const T x) { return x == static_cast<T>(*this); }
    bool operator!=(const T x) { return x != static_cast<T>(*this); }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  private:
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
    Elem(const RVec& x, const R_xlen_t i) : x(x), i(i)
    {
      if(!(0 <= i && i < x.size())) stop(
        "index %ld is out of bounds [0, %ld)", i, x.size()
      );
    }
    const SEXP     x;
    const R_xlen_t i;

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  }; // Elem
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //

  Elem    operator[](const R_xlen_t i)       { return {*this, i}; }
  const T operator[](const R_xlen_t i) const { return D::getv(**this, i); }

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
    CIter&  operator++()       noexcept { p++;  return *this; }
    CIter&  operator+=(int o)  noexcept { p+=o; return *this; }
    CIter   operator+(int o) const noexcept
    {
      CIter tmp = *this;
      tmp += o;
      return tmp;
    }
    friend CIter operator+(int o, const CIter& x) { return x + o; }
    CIter&  operator-=(int o)  noexcept { p-=o; return *this; }
    CIter   operator-(int o) const noexcept
    {
      CIter tmp = *this;
      tmp -= o;
      return tmp;
    }
    friend CIter operator-(int o, const CIter& x) { return x - o; }


    bool    operator== (const CIter& x) const noexcept { return p == x.p;}
    bool    operator!= (const CIter& x) const noexcept { return p != x.p;}

  private:
    const I* p;
  };
  const CIter begin() const { return data(); }
  const CIter end()   const { return data() + this->size(); }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  R_xlen_t idx(const RStr<PSEXP>& nm) const;
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
    friend class ROBJ::RVec<N<P>, T, T, P, S>;                                 \
    using RVec = ROBJ::RVec<N<P>, T, T, P, S>;                                 \
    using RVec::RVec;                                                          \
                                                                               \
    using RVec::operator==;                                                    \
    template<typename Z>                                                       \
    bool operator==(const N<Z>& x) const noexcept { return *x == **this; }     \
    using RVec::operator!=;                                                    \
    template<typename Z>                                                       \
    bool operator!=(const N<Z>& x) const noexcept { return *x != **this; }     \
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
  friend class ROBJ::RVec<RStr<P>, const char, const char, P, CHARSXP>;
  using RVec = ROBJ::RVec<RStr<P>, const char, const char, P, CHARSXP>;
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  using RVec::RVec;
  RStr(const char* x) : RVec(Rf_mkChar(x)) { }
  RStr(const RSym x)  : RVec(x.chr())      { }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  const RSym sym()  const { return Rf_installChar(this->sexp()); }
  operator RSym()   const { return sym(); }
  const char* str() const { return R_CHAR(this->sexp()); }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  using RVec::operator==;
  template<typename T>
  bool operator==(const RStr<T>& x) const noexcept { return *x == **this; }
  bool operator==(const char* x) const { return !std::strcmp(x, this->data()); }
  bool operator==(std::string x) const { return x         == this->data(); }
  bool operator==(const RSym& x) const { return x.c_str() == *this; }

  using RVec::operator!=;
  template<typename T>
  bool operator!=(const RStr<T>& x) const noexcept { return *x != **this; }
  bool operator!=(const char* x) const { return std::strcmp(x, this->data()); }
  bool operator!=(std::string x) const { return x         != this->data(); }
  bool operator!=(const RSym& x) const { return x.c_str() != *this; }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  typename RVec::RObj::Attr attr(const RSym& nm)             = delete;
  const typename RVec::RObj::Attr attr(const RSym& nm) const = delete;

  bool named() const                                         = delete;
  RChr<SEXP> names() const                                   = delete;

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
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
    stop("Cannot set an element of a CHARSXP");
  }
  static inline const char* ptr(const SEXP x)
  {
    return R_CHAR(x);
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // RStr
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //

template<typename T>
RSym::RSym(const RStr<T> x) : RSym(x.sym()) { }
inline RStr<SEXP> RSym::chr()           const { return PRINTNAME(sexp()); }
inline RSym::operator RStr<SEXP>()      const { return chr(); }
template<typename T>
bool RSym::operator==(const RStr<T>& x) const { return x == chr(); }
template<typename T>
bool RSym::operator!=(const RStr<T>& x) const { return x != chr(); }

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
  friend class ROBJ::RVec<RChr<P>, const RStr<SEXP>, SEXP, P, STRSXP>;
  using RVec = ROBJ::RVec<RChr<P>, const RStr<SEXP>, SEXP, P, STRSXP>;
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  using RVec::RVec;
  RChr(const std::vector<const char*>& x) : RChr(x.size())
  {
    const SEXP s = **this;
    for(std::size_t i = 0; i < x.size(); ++i)
    {
      setv(s, i, Rf_mkChar(x[i]));
    }
  }
  RChr(const char* x) : RChr(1) { setv(**this, 0, Rf_mkChar(x)); }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  RChr(const std::vector<std::string>& x) : RChr(x.size())
  {
    const SEXP s = **this;
    for(std::size_t i = 0; i < x.size(); ++i)
    {
      setv(s, i, Rf_mkChar(x[i].c_str()));
    }
  }
  RChr(const std::string& x) : RChr(x.c_str()) { }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  using RVec::operator==;
  template<typename T>
  bool operator==(const RChr<T>& x) { return *x == **this; }
  using RVec::operator!=;
  template<typename T>
  bool operator!=(const RChr<T>& x) { return *x != **this; }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  static inline const RStr<SEXP> getv(const SEXP x, const R_xlen_t i)
  {
    return STRING_ELT(x, i);
  }
  static inline void setv(const SEXP x, const R_xlen_t i, const RStr<SEXP> v)
  {
    SET_STRING_ELT(x, i, *v);
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
template<typename P, SEXPTYPE S>
const RChr<SEXP> RObj<P, S>::cls() const
{
  SEXP cls = *attr(R_ClassSymbol);
  return (cls == R_NilValue) ? RChr<SEXP>() : RChr<SEXP>(cls);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
template<typename D, typename T, typename I, typename P, SEXPTYPE S>
ENABLE_IF(U,, !std::is_same<U, RObj<SEXP, ALLSXP>>)
RVec<D, T, I, P, S>::RVec(std::initializer_list<std::pair<const char*, T>> x)
  : RVec(x.size())
{
  const SEXP s = **this;
  RChr<PSEXP> keys(x.size());
  R_xlen_t i{0};
  for(const std::pair<const char*, T>& v: x)
  {
    keys[i] = v.first;
    D::setv(s, i, v.second);
    ++i;
  }
  this->attr("names") = keys;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
template<typename D, typename T, typename I, typename P, SEXPTYPE S>
RChr<SEXP> RVec<D, T, I, P, S>::names() const
{
  SEXP x = *RObj<P, S>::attr("names");
  return x == R_NilValue ? RChr<SEXP>() : RChr<SEXP>(x);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
template<typename D, typename T, typename I, typename P, SEXPTYPE S>
R_xlen_t RVec<D, T, I, P, S>::idx(const RStr<PSEXP>& nm) const
{
    if(!named()) stop("vector is not named");
    R_xlen_t i{0};
    for(const RStr<SEXP>& name : names()) { if(name == nm) return i; ++i; }
    stop("index `%s` is out of bounds", nm.data());
    return -1;
  }
template<typename D, typename T, typename I, typename P, SEXPTYPE S>
typename RVec<D, T, I, P, S>::Elem RVec<D, T, I, P, S>::operator[](
  const RStr<PSEXP>& nm
)
{
  return { *this, idx(nm) };
}
template<typename D, typename T, typename I, typename P, SEXPTYPE S>
const T RVec<D, T, I, P, S>::operator[](const RStr<PSEXP>& nm) const
{
  return D::getv(**this, idx(nm));
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
  friend class ROBJ::RVec<RList<P>, RObj<SEXP, ALLSXP>, SEXP, P, VECSXP>;
  using RVec = ROBJ::RVec<RList<P>, RObj<SEXP, ALLSXP>, SEXP, P, VECSXP>;
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  using RVec::RVec;
  RList(std::initializer_list<const SEXP> x) : RList(x.size())
  {
    const SEXP s = **this;
    const SEXP* p = x.begin();
    for(std::size_t i = 0; i < x.size(); ++i) { setv(s, i, *(p + i)); }
  }

  RList(const std::vector<SEXP>& x) : RList(x.size())
  {
    const SEXP s = **this;
    for(std::size_t i = 0; i < x.size(); ++i) { setv(s, i, x[i]); }
  }

  RList(std::initializer_list<std::pair<const char*, const SEXP>> x)
    : RList(x.size())
  {
    const SEXP s = **this;
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

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  using RVec::operator==;
  template<typename T>
  bool operator==(const RList<T>& x) { return *x == **this; }
  using RVec::operator!=;
  template<typename T>
  bool operator!=(const RList<T>& x) { return *x != **this; }

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
class REnv : public RObj<P, ENVSXP>
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
    : RObj<P, ENVSXP>(R_NewEnv(*parent, hash, size))
  { }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Get info
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  RChr<SEXP> names(bool all = true, bool sort = false) const
  {
    return R_lsInternal3(**this, all ? TRUE : FALSE, sort ? TRUE : FALSE);
  }
  REnv<SEXP> parent() const { return R_ParentEnv(**this); }
  REnv<SEXP> topenv() const { return Rf_topenv(R_EmptyEnv, **this); }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Locking
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool locked() const
  {
    return R_EnvironmentIsLocked(**this);
  }
  void lock(bool binds = false)
  {
    R_LockEnvironment(**this, binds ? TRUE : FALSE);
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  using RObj<P, ENVSXP>::operator==;
  template<typename T>
  bool operator==(const REnv<T>& x) { return *x == **this; }
  using RObj<P, ENVSXP>::operator!=;
  template<typename T>
  bool operator!=(const REnv<T>& x) { return *x != **this; }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Binding information.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  class Bind
  {
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  public:
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
    friend class ROBJ::REnv<P>;
    using REnv = ROBJ::REnv<P>;
    Bind(Bind&& x) noexcept : Bind(std::move(x.x), std::move(x.nm)) { }
    Bind& operator=(Bind&&) noexcept = default;
    // Bind(const Bind&)                = delete;
    Bind(const Bind& x) : Bind(x.x, x.nm) { }
    // Bind& operator=(const Bind&)     = delete;

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
    bool exists() const { return R_existsVarInFrame(x, nm); }
    void remove()       { chklck(); R_removeVarFromFrame(nm, x); }
    bool locked() const { return R_BindingIsLocked(nm, x); }
    void lock(bool on)
    {
      on ? R_LockBinding(nm, x) : R_unLockBinding(nm, x);
    }

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
    RObj<SEXP, ALLSXP> get() const
    {
      if(!exists()) stop("REnv::RBind: `%s` not found", nm_str());
      return R_getVar(nm, x, FALSE);
    };
    RObj<SEXP, ALLSXP> get0(const SEXP ifnotfound = R_NilValue) const
    {
      return R_getVarEx(nm, x, FALSE, ifnotfound);
    }
    SEXP operator*()               const        { return *get(); }
    SEXP sexp()                    const        { return **this; }
    explicit operator SEXP()       const        { return **this; }
    operator RObj<SEXP, ALLSXP>()  const        { return  get(); }
    operator bool()                const        { return exists(); }

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
    void assign(const SEXP v)           { chklck(); Rf_defineVar(nm, v, x); }
    Bind& operator=(const SEXP  v)      { assign(v); return *this; }
    template<typename T, SEXPTYPE S>
    void assign(const RObj<T, S> v)     { assign(*v); }
    template<typename T, SEXPTYPE S>
    Bind& operator=(const RObj<T, S> v) { assign(*v); return *this; }

    template<bool B, typename R = int>
    using EnableIf = typename std::enable_if<B, R>::type;
    template <
      typename U,
      typename R = decltype(std::declval<const U&>().get()),
      EnableIf<IsRObj<R>() && Traits<R, P, 0>::s == ALLSXP> = 0
    >
    Bind& operator=(const U& v)
    {
      assign(v.get());
      return *this;
    }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  private:
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
    Bind(const REnv& x, const RSym& nm) : x(*x), nm(*nm), fun(*this) { }
    SEXP x;
    SEXP nm;
    void chklck( ) const
    {
      if(!exists() && R_EnvironmentIsLocked(x))
      {
        stop("REnv::RBind: Environment is locked");
      }
      if(exists()  && locked())
      {
        stop("REnv::RBind: `%s` is locked", nm_str());
      }
    }
    const char* nm_str( ) const { return CHAR(PRINTNAME(nm)); }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  public:
    /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
     * Active bindings
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
    bool active() const
    {
      if(!exists()) stop("REnv::RBind: `%s` not found", nm_str());
      return R_BindingIsActive(nm, x);
    }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
    class Fun
    {
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
    public:
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
      friend class REnv::Bind;
      Fun(const Fun&)             = delete;
      Fun(Fun&&)                  = delete;
      Fun& operator=(const Fun&)  = delete;
      Fun& operator=(const Fun&&) = delete;

      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
      RObj<SEXP, CLOSXP> get() const
      {
        if(!x.active()) stop("REnv::RBind: `%s` not active", x.nm_str());
        return R_ActiveBindingFunction(x.nm, x.x);
      }
      SEXP operator*()              const { return *get(); }
      SEXP sexp()                   const { return **this; }
      explicit operator SEXP()      const { return **this; }
      operator RObj<SEXP, CLOSXP>() const { return get(); }
      operator bool()               const { return x.active(); }

      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
      template<typename T>
      void set(const RObj<T, CLOSXP> v)
      {
        x.chklck();
        if(x.exists() && !x.active())
        {
          stop("REnv::RBind: `%s` already exists but not active", x.nm_str());
        }
        R_MakeActiveBinding(x.nm, *v, x.x);
      }
      Fun& operator=(const RObj<SEXP, CLOSXP> v) { set(v) ; return *this; }
      template<bool B, typename R = int>
      using EnableIf = typename std::enable_if<B, R>::type;
      template <
        typename U,
        typename R = decltype(std::declval<const U&>().get()),
        EnableIf<IsRObj<R>() && Traits<R, P, 0>::s == CLOSXP> = 0
      >
      Fun& operator=(const U& v) { set(v.get()); return *this; }

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
    private:
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
      Fun(const Bind& x) : x(x) { }
      const Bind& x;
    } fun;

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  }; // Bind
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //

  Bind operator[](const RSym& nm)             { return { *this, nm }; }
  const Bind operator[](const RSym& nm) const { return { *this, nm }; }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
}; // REnv
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#undef ENABLE_IF
} /* ROBJ */

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
template<typename P = PSEXP, SEXPTYPE S = ALLSXP>
using RObj = ROBJ::RObj<P,S>;
using RSym = ROBJ::RSym;
template<typename P = SEXP>
using RStr = ROBJ::RStr<P>;
#define ROBJEXPORT(NAME) template<typename P = PSEXP> using NAME = ROBJ::NAME<P>
ROBJEXPORT(RDbl);
ROBJEXPORT(RInt);
ROBJEXPORT(RLgl);
ROBJEXPORT(RChr);
ROBJEXPORT(RList);
ROBJEXPORT(REnv);
#undef ROBJEXPORT
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#endif /* OOPR_MODELS_ROBJECTS_H */
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
