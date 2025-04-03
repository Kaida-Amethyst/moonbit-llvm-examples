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
} derive(Show)

fn lexing(code: String) -> Array[Token]!Error {
  let code = code + "#"
  // kaleidoscope
  let toks : Array[Token] = Array::new()

  loop code.view() {
    ['#'] => break
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
      _ => break
    }
  }
  toks
}

fn parse_prototype(toks: ArrayView[Token]) -> (Ast, ArrayView[Token])!ParserError {
  match toks {
    [Token::Def, Token::Identifier(name), Token::Symbol('('), ..rest_toks] => {
      let args = Array::new()
      let rest_toks = loop rest_toks {
        [Token::Identifier(arg), ..rest_toks] => { args.push(arg); continue rest_toks }
        [Token::Symbol(','), ..rest_toks] => { continue rest_toks }
        [Token::Symbol(')'), ..] => { break rest_toks }
        _ => { raise ParserError("Expected identifier or ')' in prototype") }
      }
      (Ast::Prototype(name, args), rest_toks)
    }
    _ => { raise ParserError("Expected 'def' in prototype") }
  }
}

fn parse_expr(toks: ArrayView[Token]) -> (Ast, ArrayView[Token])!ParserError {
  fn precedence(op: Char) -> Int!ParserError {
    match op {
      '+' | '-' => 10
      '*' | '/' => 20
      _ => raise ParserError("Unknown operator")
    }
  }
  let ast_stack = Array::new()
  let op_stack = Array::new()
  let (ast, rest_toks) = loop toks {
    [Token::Identifier(name) , ..rest_toks] => {
      ast_stack.push(Ast::Variable(name))
      continue rest_toks
    }
    [Token::Number(num), ..rest_toks] => {
      ast_stack.push(Ast::Number(num))
      continue rest_toks
    }
    [Token::Symbol(op), ..rest_toks] if op is ('+' | '-' | '*' | '/') => {
      match (ast_stack, op_stack) {
        ([.., _], []) => { op_stack.push(op);}
        ([.., lhs, rhs], [.., top]) => {
          if precedence!(top) >= precedence!(op) {
            let bin = Ast::Binary(top, lhs, rhs)
            let _ = (ast_stack.pop(), ast_stack.pop(), op_stack.pop())
            ast_stack.push(bin)
          }
          op_stack.push(op)
        }
        _ => { raise ParserError("Expected two operands before binary operator") }
      }
      continue rest_toks
    }
    _ => {
      match (ast_stack, op_stack) {
        ([ast], []) => (ast, toks)
        ([lhs, rhs], [op]) => {
          println("here!")
          let bin = Ast::Binary(op, lhs, rhs)
          let _ = (ast_stack.pop(), ast_stack.pop(), op_stack.pop())
          ast_stack.push(bin)
          (ast_stack[0], toks)
        }
        _ => { raise ParserError("Expected expression") }
      }
    }
  }
  (ast, rest_toks)
}

fn parse_function(toks: ArrayView[Token]) -> (Ast, ArrayView[Token])!ParserError {
  match toks {
    [Token::Def, ..] => {
      let (proto, rest_toks) = parse_prototype!(toks)
      let (body, rest_toks) = parse_expr!(rest_toks)
      (Ast::Function(proto, body), rest_toks)
    }
    _ => { raise ParserError("Expected 'def' in function") }
  }
}

fn main {
  // let code = "def fib(x) if x < 3 then 1 else fib(x-1) + fib(x-2)"
  let code = "def plus(x) x + 123"
  let toks = lexing?(code).unwrap()
  let (ast, _) = parse_function?(toks[:]).unwrap()
  println(ast)

  // let code = "x + 123"
  // let toks = lexing?(code).unwrap()
  // toks.iter().each(fn(t) {println(t)})
  // let (ast, _) = parse_expr?(toks[:]).unwrap()
  // println(ast)
}
