type! ParserError(String)

enum Token {
  Def;
  Extern;
  // If;
  // Then;
  // Else;
  Identifier(String);
  Number(Int);
  Symbol(Char);
} derive(Show)

enum Ast {
  Number(Int)
  Variable(String)
  Binary(Char, Ast, Ast)
  Call(String, Array[Ast])
  Prototype(String, Array[String])
  Function(Ast, Ast) // prototype, body
}

fn lexing(code: String) -> Array[Token]!Error {
  // kaleidoscope
  let toks : Array[Token] = Array::new()

  loop code.view() {
    s => match s {
      [.."def", ..ss] => { toks.push(Token::Def); continue ss; }
      [.."extern", ..ss] => { toks.push(Token::Extern); continue ss; }
      ['0'..='9', ..] => { 
        let mut idx = 0
        while s.char_at(idx) is '0'..='9' {
          idx += 1
        }
        let tok = @strconv.parse_int!(s[0:idx].to_string()) |> Token::Number
        toks.push(tok)
        continue s[idx:]
      }
      ['a'..='z' | 'A' ..='Z', ..] => { 
        let mut idx = 0
        while s.char_at(idx) is ('a'..='z' | 'A'..='Z') {
          idx += 1
        }
        let tok = s[0:idx].to_string() |> Token::Identifier
        toks.push(tok)
        continue s[idx:]
      }
      ['(' | ')' | '<' | '>' | '+' | '-' | '*' | '/' | ',', ..ss] => { 
        toks.push(Token::Symbol(s.char_at(0))); continue ss
      }
      [' ' | '\n' | '\t' | '\r', ..ss] => { continue ss }
      _ => { break }
    }
  }
  toks.iter().each(fn(t) {println(t)})
  toks
}

fn parse_prototype(toks: ArrayView[Token]) -> Ast!ParserError {
  match toks {
    [Token::Def, Token::Identifier(name), Token::Symbol('('), ..rest_toks] => {
      let args = Array::new()
      loop rest_toks {
        [Token::Identifier(arg), ..rest_toks] => { args.push(arg); continue rest_toks }
        [Token::Symbol(','), ..rest_toks] => { continue rest_toks }
        [Token::Symbol(')'), ..] => { break }
        _ => { raise ParserError("Expected identifier or ')' in prototype") }
      }
      Ast::Prototype(name, args)
    }
    _ => { raise ParserError("Expected 'def' in prototype") }
  }
}

fn parse_expr(toks: ArrayView[Token]) -> Ast!ParserError {
  fn precedence(op: Char) -> Int {
    match op {
      '+' | '-' => 10
      '*' | '/' => 20
      _ => -1
    }
  }
  let stack = Array::new()
  let ops = Array::new()
  loop toks {
    [Token::Identifier(name) , ..rest_toks] => {
      stack.push(Ast::Variable(name))
      continue rest_toks
    }
    [Token::Number(num), ..rest_toks] => {
      stack.push(Ast::Number(num))
      continue rest_toks
    }
    [Token::Symbol(op), ..rest_toks] if op is ('+' | '-' | '*' | '/') => {
      if not(ops.is_empty()) && precedence(ops.last().unwrap()) >= precedence(op) {
        if stack.length() < 2 {
          raise ParserError("Expected two operands before binary operator")
        }
        let rhs = stack.pop().unwrap()
        let lhs = stack.pop().unwrap()
        let mop = ops.pop().unwrap()
        let bin = Ast::Binary(mop, lhs, rhs)
        stack.push(bin)
      }
      ops.push(op)
      continue rest_toks
    }
    _ => { break }
  }
  stack.pop().unwrap()
}

fn main {
  // let code = "def fib(x) if x < 3 then 1 else fib(x-1) + fib(x-2)"
  let code = "def plus(x) x + 1 "
  let _ = lexing?(code)
}
