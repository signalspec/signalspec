use super::{ ast, BinOp, FilePos, FileSpan, Spanned, Number };
use std::char;

const FAKE_SPAN: FileSpan = FileSpan { start: FilePos(0), end: FilePos(0) };

peg::parser!(pub grammar signalspec() for str {
  use peg::ParseLiteral;

  rule spanned<T>(inner: rule<T>) -> Spanned<T>
    = start:position!() node:inner() end:position!()
    { Spanned { node, span: FileSpan::new(start, end) } }

  rule tok_node(s: &str) -> ast::Token
    = span: TOK(s) { ast::Token { span } }

  rule tok_node_or_err(s: &'static str) -> Result<ast::Token, ast::Error>
    = n:tok_node(s) { Ok(n) }
    / p:pos() { Err(ast::Error { span: FileSpan{ start: p, end: p }, expected: s }) }

  rule recover(expected: &'static str, skip_until: rule<()>) -> ast::Error
    = start:pos() (!("//" / skip_until()) [_])* end:pos()
    { ast::Error { span: FileSpan{ start, end }, expected } }

  rule skip_recover(expected: &'static str, skip_until: rule<()>) -> ast::Error
    = start:pos() [^'}' | ')' | ']' | ';'] (!("//" / skip_until()) [_])* end:pos()
    { ast::Error { span: FileSpan{ start, end }, expected } }

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

  rule pos() -> FilePos = p:position!() { p.into() }

  pub rule module() -> ast::Module
      = start:position!() _ entries:module_entry()**(_ &[_]) _ end:position!()
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
    / e:skip_recover("module item", <module_recovery_set()>) {e.into() }

    rule def() -> ast::Def
      = start:pos()
        attributes:attribute()*
        tok_with:KW("with") _ bottom:protocol_ref() _
        tok_def:KW("def") _ name:IDENTIFIER() _  params:expr_tup() _ "=" _ process:process()
        end:pos()
        { ast::Def { span:FileSpan{start, end}, attributes, bottom, name, params, process } }

    rule protocol() -> ast::Protocol
      = start:pos()
        attributes:attribute()*
        tok_protocol:KW("protocol") _ name:IDENTIFIER() _ param:expr_tup() _ dir:expr() _
        open:tok_node("{") _ entries:protocol_entry() ** (_ "," _) _ ","? _ close:tok_node_or_err("}")
        end:pos()
        { ast::Protocol { span:FileSpan{start, end}, attributes, name, param: param.into(), dir, open, entries, close }}
      
      rule protocol_entry() -> ast::ProtocolEntry
        = start:pos() name:IDENTIFIER() _ "(" _ params:COMMASEP(<message_param()>) _ ")" child:(_ ":" _ p:protocol_ref() {p})? end:pos()
          {ast::ProtocolEntry::Message( ast::ProtocolMessageDef { span: FileSpan{start, end}, name, params, child }) }
        / start:pos() name:IDENTIFIER() _ ":" _ child_protocol:protocol_ref() end:pos()
          { ast::ProtocolEntry::Child(ast::ProtocolChild { span: FileSpan { start, end }, name, child_protocol }) }

      rule message_param() -> ast::MessageParam
        = start:pos() name:(name: IDENTIFIER() _ "=" _ {name})? "var" _ "(" _ direction:expr() _ ")" _ expr:expr() end:pos()
          { ast::MessageParam { span: FileSpan{start, end}, name, direction, expr} }

  rule protocol_ref() -> ast::ProtocolRef
    = start:pos() name:IDENTIFIER() _ param:expr_tup() end:pos() { ast::ProtocolRef { span: FileSpan{ start, end }, name, param: param.into() } }
    
  rule block() -> ast::Block
      = start:pos() open:tok_node("{") _
        lets:letstmt()**_ _
        actions:action()**(_ (";" _)?) _ 
        close:tok_node_or_err("}") end:pos()
      { ast::Block{ span: FileSpan{start, end}, open, lets, actions, close } }

      rule action() -> ast::Action
        = start:pos() KW("repeat") _ dir_count:(!['{'] dir:expr() _ count:expr() { (dir, count) })? _ block:block() end:pos()
          { ast::Action::Repeat(ast::ActionRepeat{ span: FileSpan{start, end}, dir_count, block }) }
        / start:pos() KW("on") _ name:IDENTIFIER() _ args:expr_tup()? _ block:block()? end:pos()
          { ast::Action::On(ast::ActionOn{ span: FileSpan{start, end}, name, args, block }) }
        / start:pos() KW("for") _ vars:COMMASEP_ONE(<(l:IDENTIFIER() _ "=" _ r:expr() { (l,r) })>) _ block:block() end:pos()
          { ast::Action::For(ast::ActionFor{ span: FileSpan{start, end}, vars, block }) }
        / start:pos() KW("any") _ open:tok_node("{") _ arms:action()**__ _ close:tok_node_or_err("}") end:pos()
          { ast::Action::Any(ast::ActionAny { span: FileSpan{start, end}, arms }) }
        / start:pos() KW("alt") _ dir:expr() _ expr:expr() _ open:tok_node("{") _ arms:alt_arm()**__ _ close:tok_node_or_err("}") end:pos()
          { ast::Action::Alt(ast::ActionAlt{ span: FileSpan{start, end}, dir, expr, arms }) }
        / start:pos() process:process() end:pos()
          { ast::Action::Process(process) }
        / e:skip_recover("action", <action_recovery_set()>) { e.into() }

          rule alt_arm() -> ast::AltArm
            = start:pos() discriminant:expr() _ "=>" _ block:block() end:pos()
            { ast::AltArm { span: FileSpan{start, end}, discriminant, block } }

  // Expressions
  rule expr_tup() -> ast::ExprTup
      = start:pos() open:tok_node("(") fields:COMMASEP(<(![')'] e:tuple_field() {e})>) close:tok_node_or_err(")") end:pos()
        { ast::ExprTup { span: FileSpan{start, end}, open, fields, close }}

    rule tuple_field() -> ast::TupleField
      = start:pos() name:(name: IDENTIFIER() _ "=" _ {name})? expr:expr() end:pos()
        { ast::TupleField { span: FileSpan{start, end}, name, expr } }

  pub rule expr() -> ast::Expr = precedence! {
    start:position!() e:@ end:position!() { let e:ast::Expr = e; e.with_span(FileSpan::from(start..end)) }
    --
    l:@ _ "=>" _ r:(@)  { ast::ExprFunc { span: FAKE_SPAN, args: Box::new(l), body: Box::new(r) }.into() }
    --
    l:@ _ "!" _ r:(@)   { ast::ExprFlip { span: FAKE_SPAN, dn: Some(Box::new(l)), up: Some(Box::new(r)) }.into() } // TODO: nonassociative
    "<:" _ x:@          { ast::ExprFlip { span: FAKE_SPAN, dn: Some(Box::new(x)), up: None }.into() }
    ":>" _ x:@          { ast::ExprFlip { span: FAKE_SPAN, dn: None, up: Some(Box::new(x))}.into() }
    --
    l:(@) _ ":" _ r:@ { ast::ExprTyped { span: FAKE_SPAN, expr:Box::new(l), ty: Box::new(r) }.into() } 
    --
    l:@ _ "|" _ r:(@)   { ast::ExprUnion { span: FAKE_SPAN, items: vec![l, r] }.into() }
    --
    l:@ _ ".." _ r:expr() step:(__ "by" _ s:expr() {s})?  { ast::ExprRange { span: FAKE_SPAN, lo: Box::new(l), hi: Box::new(r), step: step.map(Box::new) }.into() }
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
    start:pos() "[" _ elems:COMMASEP(<concat_elem()>) _ "]" end:pos() { ast::ExprConcat { span: FileSpan { start, end }, elems }.into() }
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
      = start:pos() lower:process() _ "|" _ upper:process() end:pos() {
          ast::Process::Stack(ast::ProcessStack{ span: FileSpan { start, end }, lower: Box::new(lower), upper: Box::new(upper) })
      }
      / RAW_KW("seq") _ block:block() { ast::Process::Seq(block) }
      / start:pos() "new" _ top:protocol_ref() _ block:block() end:pos() {
          ast::Process::New(ast::ProcessNew { span: FileSpan { start, end }, top, block })
      }
      / start:pos() "primitive" _ name:IDENTIFIER() _ args:expr_tup() end:pos() {
          ast::Process::Primitive(ast::ProcessPrimitive { span: FileSpan { start, end }, name, args })
      }
      / start:pos() name:IDENTIFIER() _ args:expr_tup() end:pos() {
          ast::Process::Call(ast::ProcessCall { span: FileSpan { start, end }, name, args })
      }
      / start:pos() name:IDENTIFIER() end:pos() {
          ast::Process::Child(ast::ProcessChild { span: FileSpan { start, end }, name })
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