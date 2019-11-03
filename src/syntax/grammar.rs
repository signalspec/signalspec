use super::{ ast, BinOp, FileSpan, Spanned };
use std::char;

peg::parser!(pub grammar signalspec() for str {
  use peg::ParseLiteral;

  rule spanned<T>(inner: rule<T>) -> Spanned<T>
    = start:position!() node:inner() end:position!()
    { Spanned { node, span: FileSpan::new(start, end) } }

  pub rule module() -> ast::Module
      = _ entries:spanned(<module_entry()>)**__ _
      { ast::Module { entries: entries } }

  rule letstmt() -> ast::LetDef
      = KW("let") _ name:IDENTIFIER() _ "=" _ value:expr()
      { ast::LetDef(name, value) }

  rule module_entry() -> ast::ModuleEntry
      = l:letstmt() { ast::ModuleEntry::Let(l) }
      / KW("use") _ name:IDENTIFIER() { ast::ModuleEntry::Use(name) }
      / KW("with") _ bottom:protocol_ref() _
        KW("def") _ name:IDENTIFIER() _ param:expr_tup() _ "=" _ processes:process_chain()
        { ast::ModuleEntry::WithDef(ast::Def { bottom, name, param, processes }) }
      / KW("protocol") _ name:IDENTIFIER() _ param:expr_tup() _ entries:protocol_block()
        { ast::ModuleEntry::Protocol(ast::Protocol { name, param, entries }) }
      / t:test_block() { ast::ModuleEntry::Test(t) }

  pub rule primitive_header() -> ast::PrimitiveHeader
    = KW("with") _ down_protocol:protocol_ref() _ KW("def") _ name:IDENTIFIER() _ param:expr_tup() _ up_protocol:(":" _ p:protocol_ref() {p})?
      { ast::PrimitiveHeader { bottom:down_protocol, name:name, param:param, top:up_protocol } }

  rule protocol_ref() -> ast::ProtocolRef
    = name:IDENTIFIER() _ param:expr_tup() { ast::ProtocolRef { name: name, param: param } }

  rule block() -> ast::Block
      = "{" _ lets:spanned(<letstmt()>)**_ _ actions:spanned(<action()>)**(_ (";" _)?) _ "}"
      { ast::Block{ lets:lets, actions:actions } }

      rule action() -> ast::Action
          = KW("repeat") _ count:expr()? _ block:block()
              { ast::Action::Repeat(count, block) }
          / KW("on") _ name:IDENTIFIER() _ expr:expr_tup() _ body:block()?
              { ast::Action::On(name, expr, body) }
          / KW("for") _ items:(l:IDENTIFIER() _ "=" _ r:expr() { (l,r) })**__ _ body:block()
              { ast::Action::For(items, body) }
          / KW("alt") _ expr:expr() _ "{" _ arms:alt_arm()**__ _ "}"
              { ast::Action::Alt(expr, arms) }
          / p:process_chain()
              { ast::Action::Process(p) }

          rule alt_arm() -> ast::AltArm
            = discriminant:expr() _ "=>" _ block:block()
            { ast::AltArm { discriminant, block } }

  rule protocol_block() -> Vec<ast::ProtocolEntry> = "{" _ e:protocol_entry() ** (_ "," _) _ ","? _ "}" { e }

    rule protocol_entry() -> ast::ProtocolEntry
      = n:IDENTIFIER() _ e:expr_tup() { ast::ProtocolEntry::Message(n, e) }

  // Expressions
  rule expr() -> ast::SpannedExpr
      = expr_tup()
      / valexpr()

  rule expr_tup() -> ast::SpannedExpr
      = es:spanned(<"(" e:COMMASEP(<expr()>) ")" {e}>)
      { if es.len() == 1 { es.node.into_iter().next().unwrap() } else { Spanned { node: ast::Expr::Tup(es.node), span: es.span } } }

  pub rule valexpr() -> ast::SpannedExpr = precedence! {
    start:position!() node:@ end:position!() { Spanned { node, span: FileSpan::new(start, end) } }
    --
    l:@ _ "=>" _ r:(@)  { ast::Expr::Func { args: Box::new(l), body: Box::new(r) } }
    --
    l:@ _ "!" _ r:(@)   { ast::Expr::Flip(Some(Box::new(l)), Some(Box::new(r))) } // TODO: nonassociative
    "<:" _ x:@          { ast::Expr::Flip(Some(Box::new(x)), None) }
    ":>" _ x:@          { ast::Expr::Flip(None, Some(Box::new(x))) }
    --
    l:@ _ "|" _ r:(@)   { ast::Expr::Union(vec![l, r]) }
    --
    l:@ _ ".." _ r:(@)  { ast::Expr::Range(Box::new(l), Box::new(r)) } // TODO: nonassociative
    --
    l:(@) _ "+" _ r:@   { ast::Expr::Bin(Box::new(l), BinOp::Add, Box::new(r)) }
    l:(@) _ "-" _ r:@   { ast::Expr::Bin(Box::new(l), BinOp::Sub, Box::new(r)) }
    --
    l:(@) _ "*" _ r:@   { ast::Expr::Bin(Box::new(l), BinOp::Mul, Box::new(r)) }
    l:(@) _ "/" _ r:@   { ast::Expr::Bin(Box::new(l), BinOp::Div, Box::new(r)) }
    --
    e:@ "[" vs:COMMASEP(<k:valexpr() _ "=" _ v:valexpr() { (k,v) }>) "]" { ast::Expr::Choose(Box::new(e), vs) }
    e:@ arg:expr_tup() { ast::Expr::Call(Box::new(e), Box::new(arg)) }
    --
    v:literal() { ast::Expr::Value(v) }
    s:STRING() { ast::Expr::String(s) }
    "_" { ast::Expr::Ignore }
    "[" vs:COMMASEP(<concat_elem()>) "]" { ast::Expr::Concat(vs) }
    i:IDENTIFIER() { ast::Expr::Var(i) }
    "(" e:valexpr() ")" { e.node }
  }

  rule concat_elem() -> (Option<usize>, ast::SpannedExpr) = w:(w:INTEGER() ":" {w as usize})? e:valexpr() { (w, e) }

  pub rule literal() -> ast::Value
    = "#" i:IDENTIFIER() { ast::Value::Symbol(i) }
    / v:FLOAT() u:unit() { ast::Value::Number(v) }
    / n:INTEGER() { ast::Value::Integer(n) }
    / bitsliteral()

    rule bitsliteral() -> ast::Value
        = "'h" v:(hexchar()+) {
            ast::Value::Vector(v.iter().flat_map(|&i| {
              (0..4).map(move |b|
                ast::Value::Integer(((i & 1<<(3-b)) != 0) as i64))
            }).collect())
        }
        / "'" "b"? v:(binbit()+)
            { ast::Value::Vector(v.iter().map(|&b| ast::Value::Integer(b as i64)).collect()) }

        rule hexchar() -> u8
            = c:$(['0'..='9' | 'a'..='f' | 'A'..='F'])
            { u8::from_str_radix(c, 16).unwrap() }

        rule binbit() -> bool
            = "0" {false} / "1" {true}

    rule unit() -> String
        = u:$(['a'..='z' | 'A'..='Z']+) { u.to_string() }
        / "" { "".to_string() }

  pub rule process_chain() -> Vec<ast::Process>
    = process() ++ (_ "|" _)

    rule process() -> ast::Process
      = proto:protocol_ref() _ block:block() { ast::Process::Seq(proto, block) }
      / name:IDENTIFIER() _ param:expr_tup() { ast::Process::Call(name, param) }
      / block:block() { ast::Process::InferSeq(block) }
      / dir:literal_direction() _ "(" t:protocol_ref() ")" _ b:block()
          { ast::Process::Literal(dir, t, b) }

      rule literal_direction() -> ast::ProcessLiteralDirection
        = "@up"   { ast::ProcessLiteralDirection::Up }
        / "@dn"   { ast::ProcessLiteralDirection::Down }
        / "@both" { ast::ProcessLiteralDirection::Both }
        / "@roundtrip" { ast::ProcessLiteralDirection::RoundTrip }

  rule test_block() -> ast::Test
    = KW("test") _ fails:(KW("fail") __)? processes:process_chain() _ ";"
    { ast::Test { should_fail: fails.is_some(), processes: processes } }

  // Lexer

  rule TERM() = "\r"? "\n"
  rule COMMENT() = "//" (!TERM() [_])*
  rule TERMINATOR() = COMMENT()? TERM()
  rule WHITESPACE()
      = ['\u{09}' | '\u{0B}' | '\u{0C}' | '\u{20}' | '\u{A0}' ]
      / "\\" TERM()
  rule _()  = quiet!{ (WHITESPACE() / TERMINATOR())* }
  rule __() = quiet!{ (WHITESPACE() / TERMINATOR())+ }

  rule COMMASEP<T>(x: rule<T>) -> Vec<T> = v:(x() ** (_ "," _)) (_ ",")? {v}

  rule KW(id: &str) = ##parse_string_literal(id) !['0'..='9' | 'a'..='z' | 'A'..='Z' | '_']

  rule IDENTIFIER() -> String
    = quiet!{ i:$(['a'..='z' | 'A'..='Z'| '_']['0'..='9' | 'a'..='z' | 'A'..='Z' | '_']*) { i.to_string() } }
    / expected!("identifier")
  
  rule INTEGER() -> i64
    = quiet!{ i:$("-"?['0'..='9']+) { i.parse().unwrap() } }
    / expected!("integer")
  
  rule FLOAT() -> f64
    = quiet!{ i:$("-"?['0'..='9']+ "." !"." ['0'..='9']*) { i.parse().unwrap() } }
    / expected!("float")

  rule STRING() -> String
    = quiet!{ "\"" s:doubleQuotedCharacter()* "\"" { s.into_iter().collect() } }
    / expected!("string")

    rule doubleQuotedCharacter() -> char
      = !("\"" / "\\" / TERM()) c:$([_]) { c.chars().next().unwrap() }
      / "\\u{" value:$(['0'..='9' | 'a'..='f' | 'A'..='F']+) "}" { char::from_u32(u32::from_str_radix(value, 16).unwrap()).unwrap() }
      / expected!("valid escape sequence")
});