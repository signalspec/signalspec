use super::{ ast, BinOp, FilePos, FileSpan, Spanned };
use std::char;

const FAKE_SPAN: FileSpan = FileSpan { start: FilePos(0), end: FilePos(0) };

peg::parser!(pub grammar signalspec() for str {
  use peg::ParseLiteral;

  rule spanned<T>(inner: rule<T>) -> Spanned<T>
    = start:position!() node:inner() end:position!()
    { Spanned { node, span: FileSpan::new(start, end) } }

  rule pos() -> FilePos = p:position!() { p.into() }

  pub rule module() -> ast::Module
      = start:position!() _ entries:module_entry()**__ _ end:position!()
      { ast::Module { span: FileSpan::new(start, end), entries } }

  rule letstmt() -> ast::LetDef
      = start:pos() tok_let:KW("let") _ name:IDENTIFIER() _ "=" _ expr:expr() end:pos()
      { ast::LetDef { span: FileSpan{ start, end }, tok_let, name, expr } }

  rule attribute() -> ast::Attribute
    = start:pos() tok_at:TOK("@") name:IDENTIFIER() _ args:expr_tup() __ end:pos()
    { ast::Attribute { span: FileSpan{ start, end }, tok_at, name, args } }

  rule module_entry() -> ast::ModuleEntry
    = l:letstmt() { ast::ModuleEntry::Let(l) }
    / d:def() { ast::ModuleEntry::WithDef(d) }
    / p:protocol() { ast::ModuleEntry::Protocol(p) }

    rule def() -> ast::Def
      = start:pos()
        attributes:attribute()*
        tok_with:KW("with") _ bottom:protocol_ref() _
        tok_def:KW("def") _ name:IDENTIFIER() _  params:def_params() _ "=" _ processes:process_chain()
        end:pos()
        { ast::Def { span:FileSpan{start, end}, attributes, bottom, name, params, processes } }

    rule protocol() -> ast::Protocol
      = start:pos()
        attributes:attribute()*
        tok_protocol:KW("protocol") _ name:IDENTIFIER() _ param:expr_tup() _ dir:expr() _ entries:protocol_block()
        end:pos()
        { ast::Protocol { span:FileSpan{start, end}, attributes, name, param: param.into(), dir, entries }}
      
      rule protocol_block() -> Vec<ast::ProtocolEntry> = "{" _ e:protocol_entry() ** (_ "," _) _ ","? _ "}" { e }

      rule protocol_entry() -> ast::ProtocolEntry
        = start:pos() name:IDENTIFIER() _ params:def_params() end:pos() 
        { ast::ProtocolEntry::Message( ast::ProtocolMessageDef { span: FileSpan{start, end}, name, params }) }

  pub rule primitive_header() -> ast::PrimitiveHeader
    = start:pos() KW("with") _ bottom:protocol_ref() _ KW("def") _ name:IDENTIFIER() _ params:def_params() _ top:(":" _ p:protocol_ref() {p})? end:pos()
      { ast::PrimitiveHeader { span: FileSpan{start, end}, bottom, name, params, top } }

  rule protocol_ref() -> ast::ProtocolRef
    = start:pos() name:IDENTIFIER() _ param:expr_tup() end:pos() { ast::ProtocolRef { span: FileSpan{ start, end }, name, param: param.into() } }
    
  rule def_params() -> Vec<ast::DefParam>
    = "(" _ params:COMMASEP(<def_param()>) _ ")" { params }

    rule def_param() -> ast::DefParam
      = start:pos() "const" _ expr:expr() end:pos() { ast::DefParam::Const(ast::ParamConst { span: FileSpan{start, end}, expr }) }
      / start:pos() "var" _ "(" _ direction:expr() _ ")" _ expr:expr() end:pos()
        { ast::DefParam::Var(ast::ParamVar{ span: FileSpan{start, end}, direction, expr}) }

  rule block() -> ast::Block
      = start:pos() "{" _ lets:letstmt()**_ _ actions:action()**(_ (";" _)?) _ "}" end:pos()
      { ast::Block{ span: FileSpan{start, end}, lets, actions } }

      rule action() -> ast::Action
        = start:pos() KW("repeat") _ dir_count:(dir:expr() _ count:expr() { (dir, count) })? _ block:block() end:pos()
          { ast::Action::Repeat(ast::ActionRepeat{ span: FileSpan{start, end}, dir_count, block }) }
        / start:pos() KW("on") _ name:IDENTIFIER() _ args:expr_tup() _ block:block()? end:pos()
          { ast::Action::On(ast::ActionOn{ span: FileSpan{start, end}, name, args, block }) }
        / start:pos() KW("for") _ vars:(l:IDENTIFIER() _ "=" _ r:expr() { (l,r) })**__ _ block:block() end:pos()
          { ast::Action::For(ast::ActionFor{ span: FileSpan{start, end}, vars, block }) }
        / start:pos() KW("alt") _ dir:expr() _ expr:expr() _ "{" _ arms:alt_arm()**__ _ "}" end:pos()
          { ast::Action::Alt(ast::ActionAlt{ span: FileSpan{start, end}, dir, expr, arms }) }
        / start:pos() processes:process_chain() end:pos()
          { ast::Action::Process(ast::ProcessChain { span: FileSpan{start, end}, processes }) }

          rule alt_arm() -> ast::AltArm
            = start:pos() discriminant:expr() _ "=>" _ block:block() end:pos()
            { ast::AltArm { span: FileSpan{start, end}, discriminant, block } }

  // Expressions
  rule expr_tup() -> ast::ExprTup
      =  start:pos() "(" items:COMMASEP(<expr()>) ")" end:pos() { ast::ExprTup { span: FileSpan{start, end}, items }}

  pub rule expr() -> ast::Expr = precedence! {
    start:position!() e:@ end:position!() { let e:ast::Expr = e; e.with_span(FileSpan::from(start..end)) }
    --
    l:@ _ "=>" _ r:(@)  { ast::ExprFunc { span: FAKE_SPAN, args: Box::new(l), body: Box::new(r) }.into() }
    --
    l:@ _ "!" _ r:(@)   { ast::ExprFlip { span: FAKE_SPAN, dn: Some(Box::new(l)), up: Some(Box::new(r)) }.into() } // TODO: nonassociative
    "<:" _ x:@          { ast::ExprFlip { span: FAKE_SPAN, dn: Some(Box::new(x)), up: None }.into() }
    ":>" _ x:@          { ast::ExprFlip { span: FAKE_SPAN, dn: None, up: Some(Box::new(x))}.into() }
    --
    l:@ _ "|" _ r:(@)   { ast::ExprUnion { span: FAKE_SPAN, items: vec![l, r] }.into() }
    --
    l:@ _ ".." _ r:(@)  { ast::ExprRange { span: FAKE_SPAN, lo: Box::new(l), hi: Box::new(r) }.into() } // TODO: nonassociative
    --
    l:(@) _ "+" _ r:@   { ast::ExprBin { span: FAKE_SPAN, l: Box::new(l), op: BinOp::Add, r:Box::new(r) }.into() }
    l:(@) _ "-" _ r:@   { ast::ExprBin { span: FAKE_SPAN, l: Box::new(l), op: BinOp::Sub, r:Box::new(r) }.into() }
    --
    l:(@) _ "*" _ r:@   { ast::ExprBin { span: FAKE_SPAN, l: Box::new(l), op: BinOp::Mul, r: Box::new(r) }.into() }
    l:(@) _ "/" _ r:@   { ast::ExprBin { span: FAKE_SPAN, l: Box::new(l), op: BinOp::Div, r: Box::new(r) }.into() }
    --
    e:@ "[" choices:COMMASEP(<k:expr() _ "=" _ v:expr() { (k,v) }>) "]" { ast::ExprChoose { span: FAKE_SPAN, e: Box::new(e), choices }.into() }
    e:@ arg:expr_tup() { ast::ExprCall { span: FAKE_SPAN, func: Box::new(e), arg }.into() }
    --
    start:pos() value:literal() end:pos() { ast::ExprLiteral { span: FileSpan { start, end }, value }.into() }
    start:pos() value:STRING() end:pos() { ast::ExprString { span: FileSpan { start, end }, value }.into() }
    start:pos() "_" end:pos() { ast::ExprIgnore { span: FileSpan { start, end } }.into() }
    start:pos() "[" elems:COMMASEP(<concat_elem()>) "]" end:pos() { ast::ExprConcat { span: FileSpan { start, end }, elems }.into() }
    i:IDENTIFIER() { i.into() }
    e:expr_tup() { e.into() }
  }

  rule concat_elem() -> (Option<u32>, ast::Expr) = w:(w:INTEGER() ":" {w as u32})? e:expr() { (w, e) }

  pub rule literal() -> ast::Value
    = "#" i:IDENTIFIER() { ast::Value::Symbol(i.name) }
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
      = start:pos() top:protocol_ref() _ block:block() end:pos() { ast::Process::Seq(ast::ProcessSeq { span: FileSpan { start, end }, top, block }) }
      / start:pos() name:IDENTIFIER() _ args:expr_tup() end:pos() { ast::Process::Call(ast::ProcessCall { span: FileSpan { start, end }, name, args }) }
      / block:block() { ast::Process::InferSeq(block) }

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

  rule TOK(s: &str) -> FileSpan = start:position!() ##parse_string_literal(s) end:position!() {
    FileSpan::new(start, end)
  }

  rule KW(id: &str) -> FileSpan = start:position!() ##parse_string_literal(id) !['0'..='9' | 'a'..='z' | 'A'..='Z' | '_'] end:position!() {
    FileSpan::new(start, end)
  }

  rule IDENTIFIER() -> ast::Identifier
    = quiet!{ start:position!() i:$(['a'..='z' | 'A'..='Z'| '_']['0'..='9' | 'a'..='z' | 'A'..='Z' | '_']*) end:position!() { 
      ast::Identifier { span: (start..end).into(), name: i.to_string() } 
    }}
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