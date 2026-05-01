use super::{ ast, BinOp, FilePos, FileSpan, Spanned, Number };
use std::char;

peg::parser!(pub grammar signalspec() for str {
  use peg::ParseLiteral;

  inject span(_input, lpos, rpos) -> FileSpan {
    FileSpan { start: FilePos(lpos as u32), end: FilePos(rpos as u32) }
  }

  rule spanned<T>(inner: rule<T>) -> Spanned<T>
    = node:inner() { Spanned { node, span } }

  rule tok_node(s: &str) -> ast::Token
    = span: TOK(s) { ast::Token { span } }

  rule tok_node_or_err(s: &'static str) -> Result<ast::Token, ast::Error>
    = n:tok_node(s) { Ok(n) }
    / { Err(ast::Error { span, expected: s }) }

  rule recover(expected: &'static str, skip_until: rule<()>) -> ast::Error
    = (!("//" / skip_until()) [_])*
    { ast::Error { span, expected } }

  rule skip_recover(expected: &'static str, skip_until: rule<()>) -> ast::Error
    = [^'}' | ')' | ']' | ';'] (!("//" / skip_until()) [_])*
    { ast::Error { span, expected } }

  rule module_recovery_set() -> ()
    = RAW_KW("let")
    / RAW_KW("with")
    / "@"

  rule action_recovery_set() -> ()
    = RAW_KW("repeat")
    / RAW_KW("on")
    / RAW_KW("for")
    / RAW_KW("alt")
    / RAW_KW("any")
    / "}"
    / ";"
    / module_recovery_set()

  pub rule module() -> ast::Module
      = _ entries:module_entry()**(_ &[_]) _
      { ast::Module { span, entries } }

  rule letstmt() -> ast::LetDef
      = tok_let:KW("let") _ name:IDENTIFIER() _ "=" _ expr:expr()
      { ast::LetDef { span, tok_let, name, expr } }

  rule attribute() -> ast::Attribute
    = tok_at:TOK("@") name:IDENTIFIER() _ args:expr_tup() __
    { ast::Attribute { span, tok_at, name, args } }

  rule module_entry() -> ast::ModuleEntry
    = l:letstmt() { ast::ModuleEntry::Let(l) }
    / d:def() { ast::ModuleEntry::WithDef(d) }
    / p:protocol() { ast::ModuleEntry::Protocol(p) }
    / e:skip_recover("module item", <module_recovery_set()>) {e.into() }

    rule def() -> ast::Def
      = attributes:attribute()*
        tok_with:KW("with") _ bottom:protocol_ref() _
        tok_def:KW("def") _ name:IDENTIFIER() _  params:expr_tup() _ "=" _ process:process()

        { ast::Def { span, attributes, bottom, name, params, process } }

    rule protocol() -> ast::Protocol
      = attributes:attribute()*
        tok_protocol:KW("protocol") _ name:IDENTIFIER() _ param:expr_tup() _ dir:expr() _
        open:tok_node("{") _ entries:protocol_entry() ** (_ "," _) _ ","? _ close:tok_node_or_err("}")

        { ast::Protocol { span, attributes, name, param: param.into(), dir, open, entries, close }}

      rule protocol_entry() -> ast::ProtocolEntry
        = name:IDENTIFIER() _ "(" _ params:COMMASEP(<message_param()>) _ ")" child:(_ ":" _ p:protocol_ref() {p})?
          {ast::ProtocolEntry::Message( ast::ProtocolMessageDef { span, name, params, child }) }
        / name:IDENTIFIER() _ ":" _ child_protocol:protocol_ref()
          { ast::ProtocolEntry::Child(ast::ProtocolChild { span, name, child_protocol }) }

      rule message_param() -> ast::MessageParam
        = name:(name: IDENTIFIER() _ "=" _ {name})? "var" _ "(" _ direction:expr() _ ")" _ expr:expr()
          { ast::MessageParam { span, name, direction, expr} }

  rule protocol_ref() -> ast::ProtocolRef
    = name:IDENTIFIER() _ param:expr_tup() { ast::ProtocolRef { span, name, param: param.into() } }

  rule block() -> ast::Block
      = open:tok_node("{") _
        lets:letstmt()**_ _
        actions:action()**(_ (";" _)?) _
        close:tok_node_or_err("}")
      { ast::Block{ span, open, lets, actions, close } }

      rule action() -> ast::Action
        = KW("repeat") _ dir_count:(!['{'] dir:expr() _ count:expr() { (dir, count) })? _ block:block()
          { ast::Action::Repeat(ast::ActionRepeat{ span, dir_count, block }) }
        / KW("on") _ name:IDENTIFIER() _ args:expr_tup()? _ block:block()?
          { ast::Action::On(ast::ActionOn{ span, name, args, block }) }
        / KW("for") _ vars:COMMASEP_ONE(<(l:IDENTIFIER() _ "=" _ r:expr() { (l,r) })>) _ block:block()
          { ast::Action::For(ast::ActionFor{ span, vars, block }) }
        / KW("any") _ open:tok_node("{") _ arms:action()**__ _ close:tok_node_or_err("}")
          { ast::Action::Any(ast::ActionAny { span, arms }) }
        / KW("alt") _ dir:expr() _ expr:expr() _ open:tok_node("{") _ arms:alt_arm()**__ _ close:tok_node_or_err("}")
          { ast::Action::Alt(ast::ActionAlt { span, dir, expr, arms }) }
        / process:process()
          { ast::Action::Process(process) }
        / e:skip_recover("action", <action_recovery_set()>) { e.into() }

          rule alt_arm() -> ast::AltArm
            = discriminant:expr() _ "=>" _ block:block()
            { ast::AltArm { span, discriminant, block } }

  // Expressions
  rule expr_tup() -> ast::ExprTup
      = open:tok_node("(") fields:COMMASEP(<(![')'] e:tuple_field() {e})>) close:tok_node_or_err(")")
        { ast::ExprTup { span, open, fields, close }}

    rule tuple_field() -> ast::TupleField
      = name:(name: IDENTIFIER() _ "=" _ {name})? expr:expr()
        { ast::TupleField { span, name, expr } }

  pub rule expr() -> ast::Expr = precedence! {
    l:@ _ "=>" _ r:(@)  { ast::ExprFunc { span, args: Box::new(l), body: Box::new(r) }.into() }
    RAW_KW("type") _ expr:@        { ast::ExprType { span, expr: Box::new(expr) }.into() }
    --
    l:@ _ "!" _ r:(@)   { ast::ExprFlip { span, dn: Some(Box::new(l)), up: Some(Box::new(r)) }.into() } // TODO: nonassociative
    "~" _ x:@           { ast::ExprFlip { span, dn: Some(Box::new(x)), up: None }.into() }
    --
    l:(@) _ ":" _ r:@ { ast::ExprTyped { span, expr:Box::new(l), ty: Box::new(r) }.into() }
    --
    l:@ _ "|" _ r:(@)   { ast::ExprUnion { span, items: vec![l, r] }.into() }
    --
    l:@ _ ".." r:(_ &[^')'] r:expr() {r})? step:(__ "by" _ s:expr() {s})?  { ast::ExprRange { span, lo: Box::new(l), hi: r.map(Box::new), step: step.map(Box::new) }.into() }
    --
    l:(@) _ "+" _ r:@   { ast::ExprBin { span, l: Box::new(l), op: BinOp::Add, r:Box::new(r) }.into() }
    l:(@) _ "-" _ r:@   { ast::ExprBin { span, l: Box::new(l), op: BinOp::Sub, r:Box::new(r) }.into() }
    --
    l:(@) _ "*" _ r:@   { ast::ExprBin { span, l: Box::new(l), op: BinOp::Mul, r: Box::new(r) }.into() }
    l:(@) _ "/" _ r:@   { ast::ExprBin { span, l: Box::new(l), op: BinOp::Div, r: Box::new(r) }.into() }
    --
    e:@ "[" choices:COMMASEP(<k:expr() _ "=" _ v:expr() { (k,v) }>) "]" { ast::ExprChoose { span, e: Box::new(e), choices }.into() }
    e:@ arg:expr_tup() { ast::ExprCall { span, func: Box::new(e), arg }.into() }
    --
    value:literal() { ast::ExprLiteral { span, value }.into() }
    value:STRING() { ast::ExprString { span, value }.into() }
    "_" { ast::ExprIgnore { span }.into() }
    "[" _ elem:expr() _ ";" _ count:expr() _ "]" { ast::ExprArrayRep { span, elem: Box::new(elem), count: Box::new(count) }.into() }
    "[" _ elems:COMMASEP(<concat_elem()>) _ "]" { ast::ExprConcat { span, elems }.into() }
    i:IDENTIFIER() { i.into() }
    e:expr_tup() { e.into() }
  } / e:recover("expression", <action_recovery_set() / [',' | ')' | ']' | '}'] {}>) { e.into() }

  rule concat_elem() -> (Option<u32>, ast::Expr) = w:(w:INTEGER() ":" {w})? e:expr() { (w, e) }

  pub rule literal() -> ast::Literal
    = "#" i:IDENTIFIER() { ast::Literal::Symbol(i.name) }
    / v:NUMBER() u:unit() { ast::Literal::Number(v) }
    / "'h" v:(hexchar()+) { ast::Literal::Hex(v) }
    / "'" "b"? v:(binbit()+) { ast::Literal::Bin(v) }

    rule hexchar() -> u8
        = c:$(['0'..='9' | 'a'..='f' | 'A'..='F'])
        {? u8::from_str_radix(c, 16).map_err(|_| "hex char") }

    rule binbit() -> bool
        = "0" {false} / "1" {true}

    rule unit() -> String
        = u:$(['a'..='z' | 'A'..='Z']+) { u.to_string() }
        / "" { "".to_string() }

  #[cache_left_rec]
  pub rule process() -> ast::Process
      = lower:process() _ "|" _ upper:process() {
          ast::Process::Stack(ast::ProcessStack{ span, lower: Box::new(lower), upper: Box::new(upper) })
      }
      / RAW_KW("seq") _ block:block() { ast::Process::Seq(block) }
      / "new" _ top:protocol_ref() _ block:block() {
          ast::Process::New(ast::ProcessNew { span, top, block })
      }
      / "primitive" _ name:IDENTIFIER() _ args:expr_tup() {
          ast::Process::Primitive(ast::ProcessPrimitive { span, name, args })
      }
      / name:IDENTIFIER() _ args:expr_tup() {
          ast::Process::Call(ast::ProcessCall { span, name, args })
      }
      / name:IDENTIFIER() {
          ast::Process::Child(ast::ProcessChild { span, name })
      }

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
  rule COMMASEP_ONE<T>(x: rule<T>) -> Vec<T> = v:(x() ++ (_ "," _)) (_ ",")? {v}

  rule TOK(s: &str) -> FileSpan = start:position!() #{|input, pos| input.parse_string_literal(pos, s)} end:position!() {
    FileSpan::new(start, end)
  }

  rule RAW_KW(id: &str) -> () = #{|input, pos| input.parse_string_literal(pos, id)} !['0'..='9' | 'a'..='z' | 'A'..='Z' | '_']

  rule KW(id: &str) -> FileSpan = start:position!() RAW_KW(id) end:position!() {
    FileSpan::new(start, end)
  }

  rule IDENTIFIER() -> ast::Identifier
    = quiet!{ start:position!() i:$(['a'..='z' | 'A'..='Z'| '_']['0'..='9' | 'a'..='z' | 'A'..='Z' | '_']*) end:position!() {
      ast::Identifier { span: (start..end).into(), name: i.to_string() }
    }}
    / expected!("identifier")

  rule INTEGER<T: std::str::FromStr>() -> T
    = quiet!{ i:$("-"?['0'..='9']+) {? i.parse().map_err(|e| "valid integer") } }
    / expected!("integer")

  rule NUMBER() -> Number
    = quiet!{ int:$("-"?['0'..='9']+) frac:("." !"." f:$(['0'..='9']*) {f})? {?
      const EXPECTED: &'static str = "number that fits in a 64-bit fraction";
      let int: i64 = int.parse().map_err(|_| EXPECTED)?;
      let pow: i64 = frac.map_or(Some(1), |s| 10i64.checked_pow(s.len() as u32)).ok_or(EXPECTED)?;
      let frac: i64 = frac.map_or(Ok(0), |s| s.parse()).map_err(|_| EXPECTED)?;
      let num = int.checked_mul(pow).and_then(|n| n.checked_add(frac)).ok_or(EXPECTED)?;
      Ok(Number::new(num, pow))
    } }
    / expected!("number")

  rule STRING() -> String
    = quiet!{ "\"" s:doubleQuotedCharacter()* "\"" { s.into_iter().collect() } }
    / expected!("string")

    rule doubleQuotedCharacter() -> char
      = !("\"" / "\\" / TERM()) c:$([_]) { c.chars().next().unwrap() }
      / "\\u{" value:$(['0'..='9' | 'a'..='f' | 'A'..='F']+) "}" {? u32::from_str_radix(value, 16).ok().and_then(char::from_u32).ok_or("unicode codepoint in hex") }
      / expected!("valid escape sequence")
});
