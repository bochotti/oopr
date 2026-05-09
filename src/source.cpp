// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#include "source.h"
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Represents opening and closing statements, e.g. ( ... )
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class Complement
{
public:
  Complement(std::pair<const char, const char> p)
    : open(p.first), close(p.second)
  { }
  char open;
  char close;
  int  count = 0;
};

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * A collection of complements.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class Complements
{
public:
  Complements(std::initializer_list<std::pair<const char, const char>> args)
  {
    for(const std::pair<const char, const char>& arg : args)
    {
      std::shared_ptr<Complement> comp(new Complement(arg));
      opens.emplace(arg.first, comp);
      closes.emplace(arg.second, comp);
    }
  }
  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Complement tokens share the same complement, but can be referred
   * to by their opening or closing token.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  std::map<const char, std::shared_ptr<Complement>> opens;
  std::map<const char, std::shared_ptr<Complement>> closes;

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Check if a token is and open or close token.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool open(const char& key)  { return opens.count(key); }
  bool close(const char& key) { return closes.count(key); }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Add to the counter of a complement.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void add(const char& key)
  {
    if(open(key))
    {
      ++opens[key]->count;
    }
    else if(close(key))
    {
      ++closes[key]->count;
    }
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Subtract from the counter of a complement.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  void rm(const char& key)
  {
    if(open(key))
    {
      --opens[key]->count;
    }
    else if(close(key))
    {
      --closes[key]->count;
    }
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Check if any complement has a count.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool inside(const char& key)
  {
    if(open(key))  return opens[key]->count;
    if(close(key)) return closes[key]->count;
    return false;
  }
  bool inside()
  {
    for(const auto& [key, val] : opens) if(val->count) return true;
    return false;
  }
};

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 *
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class VectorString
{
public:
  VectorString(std::initializer_list<std::string> args)
  {
    const std::size_t len = args.size();
    x = Rf_allocVector(STRSXP, (R_xlen_t)len);
    R_xlen_t i = 0;
    for(const std::string& arg : args)
    {
      SET_STRING_ELT(x, i, Rf_mkChar(arg.c_str()));
      ++i;
    }
  }
  operator SEXP()          { return x; }
private:
  SEXP x;
};

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
 * Find the evaluation string of a $ completion.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class EvaluationContext
{
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
public:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  EvaluationContext(SEXP text, SEXP row, SEXP col)
  {
    const R_xlen_t len = Rf_xlength(text);
    for(R_xlen_t i = 0; i < len; ++i)
    {
      text_.append(CHAR(STRING_ELT(text, i)));
      text_.append("\n");
    }
    makePos(INTEGER_ELT(row, 0), INTEGER_ELT(col, 0));
  }

  Complements comps{{'(',')'}, {'[',']'}};

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * The character at the current position.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  char val() { return text_[pos_]; }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Move the position backwards.
   * Returns false if at start of text.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool atStart() { return pos_ == 0; }
  bool moveBwd()
  {
    if(atStart()) return false;
    --pos_;
    return true;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Move the position forwards.
   * Returns false if at end of text.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool atEnd() { return pos_ == text_.size() - 1L; }
  bool moveFwd()
  {
    if(atEnd()) return false;
    ++pos_;
    return true;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Check if the current character is whitespace.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool atWS()
  {
    atStart() || atEnd();
    char val = this->val();
    return val == ' ' || val == '\t' || val == '\n' || val == '\r';
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Move up a row, intended to avoid comments #.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool moveUp()
  {
    if(atStart() || this->val() != '\n') return false;
    int pos = this->pos_ - 1;
    while(moveBwd())
    {
      char val = this->val();
      if(val == '#')
      {
        pos = this->pos_ - 1;
      }
      else if(val == '\n') break;
    }
    this->pos_ = pos;
    return true;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Move to the previous quote, may not be allowed on the previous line.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  char peekBwd(const unsigned int& i = 1)
  {
    if(i > pos_) return '\0'; else return text_[pos_ - i];
  }
  bool moveBwdToQuote(const char& key, const bool& lb = false)
  {
    if(this->val() != key) return false;
    while(moveBwd())
    {
      char val = this->val();
      if(val == key && peekBwd() != '\\')
      {
        return true;
      }
      else if(lb && val == '\n') break;
    }
    return false;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Move backward to the opening part of (, [, etc.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool moveBwdToOpeningComplement(const char& key)
  {
    if(!comps.close(key)) return false;
    comps.add(key);
    while(moveBwd())
    {
      const char val = this->val();
      if(comps.close(val))
      {
        comps.add(val);
      }
      else if(comps.open(val))
      {
        comps.rm(val);
        if(!comps.inside(val))
        {
          return true;
        }
      }
    }
    return false;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Move the position to the closest $ on the LHS.
   * Returns false if not found / reaches a terminating character first.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool moveToLastDollar()
  {
    while(moveBwd()) switch(val())
    {
    case '$': return true;
    case '(':
    case ',':
    case '=': isInFun = true;
    case ')':
    case '{':
    case '}':
    case '-':
    case ';': return false;
    default:  continue;
    }
    return false;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Move to the position to the closest ( on the LHS.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool moveToLastParenthesis()
  {
    while(moveBwd())
    {
      char val = this->val();
      if(comps.close(val))
      {
        if(!moveBwdToOpeningComplement(val)) return false;
      }
      else if(comps.open(val))
      {
        return true;
      }
      else switch(val)
      {
      case '\'':
      case '"':
      {
        if(moveBwdToQuote(val, false)) continue;
        return false;
      }
      case '`':
      {
        if(moveBwdToQuote('`', true)) continue;
        return false;
      }
      default: continue;
      }
    }
    return false;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Move the position to the start of the call.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool moveToStart()
  {
    while(moveBwd())
    {
      char val = this->val();
      if(comps.close(val))
      {
        if(!moveBwdToOpeningComplement(val)) return false;
      }
      else if(comps.open(val))
      {
        moveFwd();
        isInFun = true;
        break;
      }
      else switch(val)
      {
      case '\n':
      {
        // line break is OK, if the end of previous line is a $
        moveUp();
        while(atWS() && moveBwd()) { }
        val = this->val();
        if(val == '$' || val == '@') continue;
        moveFwd();
        goto stop;
      }
      case ' ':
      {
        // make sure I dont go over for(i `in` this$a)
        if(peekBwd(1) == 'n' && peekBwd(2) == 'i' && peekBwd(3) == ' ')
        {
          moveFwd();
          isInFun = true;
          goto stop;
        }
        continue;
      }
      case '\'':
      case '"':
      {
        if(moveBwdToQuote(val, false)) continue;
        return false;
      }
      case '`':
      {
        if(moveBwdToQuote('`', true)) continue;
        return false;
      }
      case '=':
      case ',':
      {
        isInFun = true;
      }
      case '-':
      case ';':
      {
        moveFwd();
        goto stop;
      }
      default:  continue;
      }
    }
stop:
    while(atWS() && moveFwd()) { }
    return true;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Save a match
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  struct Match
  {
    int stt = -1;
    int end = -1;
    std::string ctx = "";
  };
  std::vector<Match> matches;

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Whether the search stopped because it hit a function
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool isInFun = false;

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Search backwards for evaluation context.
   * If in a function, then go outside and grab next match.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  bool collect()
  {
    Match m;
    bool move = false;
    if(isInFun)
    {
      m.end = pos_;
      move  = true;
      if(moveToLastParenthesis())
      {
        m.end = pos_;
      }
      isInFun = false;
    }
    else if(moveToLastDollar())
    {
      m.end = pos_;
      move  = true;
    }
    else if (isInFun)
    {
      moveFwd();
    }
    if(move && moveToStart())
    {
      m.stt = pos_;
    }
    if(m.stt >= 0 && m.end >= 0)
    {
      m.ctx = text_.substr(m.stt, m.end - m.stt);
    }
    matches.push_back(std::move(m));
    if(isInFun) collect();
    return true;
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
   * Convert matches to R list.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  SEXP toList()
  {
    const R_xlen_t len = matches.size();
    pSEXP out = Rf_allocVector(VECSXP, len);

    for(R_xlen_t i = 0; i < len; ++i)
    {
      Match m = matches[i];
      pSEXP v;
      if(m.stt >= 0 && m.end >= 0)
      {
        v = Rf_allocVector(VECSXP, 3);
        SET_VECTOR_ELT(v, 0, Rf_ScalarInteger(m.stt + 1));
        SET_VECTOR_ELT(v, 1, Rf_ScalarInteger(m.end));
        SET_VECTOR_ELT(v, 2, Rf_mkString(m.ctx.c_str()));
        Rf_setAttrib(v, R_NamesSymbol, VectorString{"stt", "end", "ctx"});
      }
      else
      {
        v = R_NilValue;
      }
      SET_VECTOR_ELT(out, i, v);
    }

    return out;
  }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
private:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  std::string text_;
  unsigned int row_ = 0;
  unsigned int col_ = 0;
  unsigned int pos_ = 0;

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
  void makePos(int row, int col)
  {
    int line = 1;
    for(std::size_t i = 0; i < text_.size(); ++i)
    {
      if(line == row) break;
      if(text_[i] == '\n') ++line;
      ++pos_;
    }
    pos_ = pos_ + col - 1;
    pos_ = pos_ >= text_.size() ? text_.size() - 1 : pos_;
    row_ = row;
    col_ = col;
  }
};
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
SEXP eval_context(SEXP text, SEXP row, SEXP col)
{
  if(!Rf_isString(text)) Rf_error("`text` must be a string");
  if(!Rf_isInteger(row)) Rf_error("`row` must be an integer");
  if(!Rf_isInteger(col)) Rf_error("`col` must be an integer");
  EvaluationContext obj(text, row, col);
  if(!obj.collect()) return R_NilValue;
  return obj.toList();
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
