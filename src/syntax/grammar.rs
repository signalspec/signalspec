use super::{ ast, BinOp, FilePos, FileSpan, Number, lexer::{Token, Tokens} };
use std::char;
use Token::*;

peg::parser!(pub grammar signalspec<'a>() for Tokens<'a> {
  inject span(input, lpos, rpos) -> FileSpan {
    let start = input.positions.get(lpos).map(|s| s.start).unwrap_or(FilePos(0));
    let end = rpos.checked_sub(1).and_then(|rpos| input.positions.get(rpos)).map(|s| s.end).unwrap_or(FilePos(0));
    FileSpan { start, end }
  }

  rule tok_node(s: Token) -> ast::Token
    = span: TOK(s) { ast::Token { span } }

  rule tok_node_or_err(s: Token) -> Result<ast::Token, ast::Error>
    = n:tok_node(s) { Ok(n) }
    / #{|input, pos| peg::RuleResult::Matched(pos,
        Err(ast::Error { span: input.get_span(pos), expected: s.describe() } )) }

  rule skip()
    = [OpenBrace] (![CloseBrace] skip())* [CloseBrace]
    / [OpenBracket] (![CloseBracket] skip())* [CloseBracket]
    / [OpenParen] (![CloseParen] skip())* [CloseParen]
    / [_]

  rule recover(expected: &'static str, skip_until: rule<()>) -> ast::Error
    = (!skip_until() skip())*
    { ast::Error { span, expected } }

  rule skip_recover(expected: &'static str, skip_until: rule<()>) -> ast::Error
    = skip() (!skip_until() skip())*
      { ast::Error { span, expected } }

  rule module_recovery_set() -> () = [KwLet | KwWith | At] {}

  rule action_recovery_set() -> ()
    = [KwRepeat | KwOn | KwFor | KwAlt | KwAny | CloseBrace | Semicolon | NewlineSep] {}
    / module_recovery_set()

  pub rule module() -> ast::Module
    = entries:module_entry()* { ast::Module { span, entries } }

  rule letstmt() -> ast::LetDef
    = tok_let:TOK(KwLet) name:identifier() [Equals] expr:expr() (sep() / EOF())
    { ast::LetDef { span, tok_let, name, expr } }

  rule attribute() -> ast::Attribute
    = tok_at:TOK(At) name:identifier() args:expr_tup()
    { ast::Attribute { span, tok_at, name, args } }

  rule parameter_list() -> ast::ParameterList
    = open:tok_node(OpenParen) list:comma_sep(<(![CloseParen] e:parameter() {e})>) close:tok_node_or_err(CloseParen)
        { ast::ParameterList { span, list } }

    rule parameter() -> ast::Parameter
      = name:identifier() ty:([Colon] ty:expr() { ty })? default:([Equals] expr:expr() { expr })?
        { ast::Parameter { span, name, ty, default } }

  rule module_entry() -> ast::ModuleEntry
    = l:letstmt() { ast::ModuleEntry::Let(l) }
    / d:def() { ast::ModuleEntry::WithDef(d) }
    / p:protocol() { ast::ModuleEntry::Protocol(p) }
    / e:skip_recover("module item", <module_recovery_set()>) { e.into() }

    rule def() -> ast::Def
      = attributes:(a: attribute() [NewlineSep] {a})*
        tok_with:TOK(KwWith) bottom:protocol_ref() [NewlineSep]?
        tok_def:TOK(KwDef) name:identifier() params:parameter_list() [NewlineSep]? [Equals] process:process() [NewlineSep]?
        { ast::Def { span, attributes, bottom, name, params, process } }

    rule protocol() -> ast::Protocol
      = attributes:(a: attribute() [NewlineSep] {a})*
        tok_protocol:TOK(KwProtocol) name:identifier() params:parameter_list() dir:expr()
        open:tok_node(OpenBrace) entries:protocol_entry() ** [Comma] [Comma]? [NewlineSep]? close:tok_node_or_err(CloseBrace) [NewlineSep]?
        { ast::Protocol { span, attributes, name, params, dir, open, entries, close }}

      rule protocol_entry() -> ast::ProtocolEntry
        = name:identifier() [OpenParen] params:comma_sep(<message_param()>) [CloseParen] child:([Colon] p:protocol_ref() {p})?
          { ast::ProtocolEntry::Message( ast::ProtocolMessageDef { span, name, params, child }) }
        / name:identifier() [Colon] child_protocol:protocol_ref()
          { ast::ProtocolEntry::Child(ast::ProtocolChild { span, name, child_protocol }) }

      rule message_param() -> ast::MessageParam
        = name:identifier() [Colon] ty:expr() TOK(KwIs) direction:expr()
          { ast::MessageParam { span, name, direction, ty } }

  rule protocol_ref() -> ast::ProtocolRef
    = name:identifier() !SP() args:expr_tup() { ast::ProtocolRef { span, name, args } }

  rule block() -> ast::Block
      = open:tok_node(OpenBrace)
        lets:letstmt()*
        actions:semi_sep(<action()>)
        close:tok_node_or_err(CloseBrace)
      { ast::Block{ span, open, lets, actions, close } }

      rule action() -> ast::Action
        = [KwRepeat] dir_count:(![OpenBrace] SP() dir:expr() count:expr() { (dir, count) })? block:block()
          { ast::Action::Repeat(ast::ActionRepeat{ span, dir_count, block }) }
        / [KwOn] name:identifier() args:(!SP() a:expr_tup() {a})? block:block()?
          { ast::Action::On(ast::ActionOn { span, name, args, block }) }
        / [KwFor] vars:comma_sep_one(<(l:identifier() [Equals] r:expr() { (l,r) })>) block:block()
          { ast::Action::For(ast::ActionFor{ span, vars, block }) }
        / [KwAny] open:tok_node(OpenBrace) arms:semi_sep(<action()>) close:tok_node_or_err(CloseBrace)
          { ast::Action::Any(ast::ActionAny { span, arms }) }
        / [KwAlt] SP() dir:expr() expr:expr() [OpenBrace] arms:semi_sep(<alt_arm()>) close:tok_node_or_err(CloseBrace)
          { ast::Action::Alt(ast::ActionAlt{ span, dir, expr, arms }) }
        / process:process()
          { ast::Action::Process(process) }
        / e:skip_recover("action", <action_recovery_set()>) { e.into() }

          rule alt_arm() -> ast::AltArm
            = discriminant:expr() [Arrow] block:block()
            { ast::AltArm { span, discriminant, block } }

  // Expressions
  rule expr_tup() -> ast::ExprTup
  = open:tok_node(OpenParen) fields:comma_sep(<(![CloseParen] e:tuple_field() {e})>) close:tok_node_or_err(CloseParen)
        { ast::ExprTup { span, open, fields, close }}

    rule tuple_field() -> ast::TupleField
    = name:(name: identifier() [Equals] {name})? expr:expr()
        { ast::TupleField { span, name, expr } }

  pub rule expr() -> ast::Expr = precedence! {
    l:@ [Arrow] r:(@)  { ast::ExprFunc { span, args: Box::new(l), body: Box::new(r) }.into() }
    [KwType] expr:@      { ast::ExprType { span, expr: Box::new(expr) }.into() }
    --
    l:@ [Bang] r:(@)   { ast::ExprFlip { span, dn: Some(Box::new(l)), up: Some(Box::new(r)) }.into() } // TODO: nonassociative
    [Tilde] x:@           { ast::ExprFlip { span, dn: Some(Box::new(x)), up: None }.into() }
    --
    l:(@) [Colon] r:@  { ast::ExprTyped { span, expr:Box::new(l), ty: Box::new(r) }.into() }
    --
    l:@ [Pipe] r:(@)   { ast::ExprUnion { span, items: vec![l, r] }.into() }
    --
    l:@ [DotDot] r:(&[^CloseParen | Comma | NewlineSep] r:expr() {r})? step:([KwBy] s:expr() {s})? { ast::ExprRange { span, lo: Box::new(l), hi: r.map(Box::new), step: step.map(Box::new) }.into() }
    --
    l:(@) [Plus] r:@   { ast::ExprBin { span, l: Box::new(l), op: BinOp::Add, r:Box::new(r) }.into() }
    l:(@) [Minus] r:@  { ast::ExprBin { span, l: Box::new(l), op: BinOp::Sub, r:Box::new(r) }.into() }
    --
    l:(@) [Star] r:@   { ast::ExprBin { span, l: Box::new(l), op: BinOp::Mul, r: Box::new(r) }.into() }
    l:(@) [Slash] r:@  { ast::ExprBin { span, l: Box::new(l), op: BinOp::Div, r: Box::new(r) }.into() }
    --
    e:@ [OpenBracket] choices:comma_sep(<k:expr() [Equals] v:expr() { (k,v) }>) [CloseBracket] { ast::ExprChoose { span, e: Box::new(e), choices }.into() }
    e:@ !SP() arg:expr_tup() { ast::ExprCall { span, func: Box::new(e), arg }.into() }
    --
    value:literal() { ast::ExprLiteral { span, value }.into() }
    value:STRING() { ast::ExprString { span, value }.into() }
    [Underscore] { ast::ExprIgnore { span }.into() }
    [OpenBracket] elem:expr() [Semicolon] count:expr() [CloseBracket] { ast::ExprArrayRep { span, elem: Box::new(elem), count: Box::new(count) }.into() }
    [OpenBracket] elems:comma_sep(<concat_elem()>) [CloseBracket] { ast::ExprConcat { span, elems }.into() }
    i:identifier() { i.into() }
    e:expr_tup() { e.into() }
  } / e:recover("expression", <action_recovery_set() / [Comma | CloseParen | CloseBracket | CloseBrace] {}>) { e.into() }

  rule concat_elem() -> (Option<u32>, ast::Expr) = w:(w:INTEGER() [Colon] {w})? e:expr() { (w, e) }

  pub rule literal() -> ast::Literal
    = i:$[SymbolLiteral] { ast::Literal::Symbol(i[1..].to_string()) }
    / v:NUMBER() u:unit() { ast::Literal::Number(v) }
    / i:$[HexBitsLiteral] { ast::Literal::Hex(hex_chars(&i[2..]).collect()) }
    / i:$[BinaryBitsLiteral] { ast::Literal::Bin(bin_chars(&i[2..]).collect()) }
    / i:$[BareBitsLiteral] { ast::Literal::Bin(bin_chars(&i[1..]).collect()) }

  rule unit() -> String = s:$[Identifier] { s.to_string() } / { String::new() }

  #[cache_left_rec]
  pub rule process() -> ast::Process
      = lower:process() [NewlineSep]? [Pipe] upper:process() {
          ast::Process::Stack(ast::ProcessStack{ span, lower: Box::new(lower), upper: Box::new(upper) })
      }
      / [KwSeq] block:block() { ast::Process::Seq(block) }
      / [KwNew] top:protocol_ref() block:block() {
          ast::Process::New(ast::ProcessNew { span, top, block })
      }
      / [KwPrimitive] name:identifier() args:expr_tup() {
          ast::Process::Primitive(ast::ProcessPrimitive { span, name, args })
      }
      / name:identifier() !SP() args:expr_tup() {
          ast::Process::Call(ast::ProcessCall { span, name, args })
      }
      / name:identifier() {
          ast::Process::Child(ast::ProcessChild { span, name })
      }

  rule identifier() -> ast::Identifier
    = name:$([Identifier])
      { ast::Identifier { span, name: name.to_string() } }

  rule sep() = [Semicolon | NewlineSep]

  rule semi_sep<T>(element: rule<T>) -> Vec<T>
      = es:((![CloseBrace] e:element() {e}) ** sep()) sep()? {es}

  rule comma_sep<T>(x: rule<T>) -> Vec<T> = v:(x() ** [Comma]) [Comma]? {v}
  rule comma_sep_one<T>(x: rule<T>) -> Vec<T> = v:(x() ++ [Comma]) [Comma]? {v}

  // Requires whitespace between the previous token and the next token
  rule SP() = #{|input, pos| {
      match (pos.checked_sub(1).and_then(|p| input.positions.get(p)).copied(), input.positions.get(pos).copied()) {
          (Some(prev), Some(next)) if prev.end < next.start => peg::RuleResult::Matched(pos, ()),
          _ => peg::RuleResult::Failed
      }
  }}

  rule EOF() = ![_]

  // Matches a token and returns its span
  rule TOK(id: Token) -> FileSpan = #{|input, pos| match input.tokens.get(pos) {
      Some(&token) if token == id => {
          peg::RuleResult::Matched(pos + 1, input.positions[pos])
      },
      _ => peg::RuleResult::Failed,
  }}

  rule STRING() -> String = s:$[StringLiteral] {? token_parts::string(s).map_err(|_| "string literal") }
  rule INTEGER<T: std::str::FromStr>() -> T = i:$([Minus]? [IntLiteral]) {? i.parse().map_err(|e| "valid integer") }
  rule NUMBER() -> Number = n:$([Minus]? [NumberLiteral]) {? token_parts::number(n).map_err(|_| "number literal") }
});

fn bin_chars(s: &str) -> impl Iterator<Item = bool> + '_ {
    s.chars().filter_map(|c| match c { '0' => Some(false), '1' => Some(true), _ => None})
}

fn hex_chars(s: &str) -> impl Iterator<Item = u8> + '_ {
    s.chars().filter_map(|c| match c {
        '0'..='9' => Some(c as u8 - b'0'),
        'a'..='f' => Some(c as u8 - b'a' + 10),
        'A'..='F' => Some(c as u8 - b'A' + 10),
        _ => None
    })
}

peg::parser!(pub grammar token_parts() for str {
    pub rule number() -> Number
        = quiet!{ int:$("-"?['0'..='9']+) frac:("." f:$(['0'..='9']+) {f})? {?
        const EXPECTED: &'static str = "number that fits in a 64-bit fraction";
        let int: i64 = int.parse().map_err(|_| EXPECTED)?;
        let pow: i64 = frac.map_or(Some(1), |s| 10i64.checked_pow(s.len() as u32)).ok_or(EXPECTED)?;
        let frac: i64 = frac.map_or(Ok(0), |s| s.parse()).map_err(|_| EXPECTED)?;
        let num = int.checked_mul(pow).and_then(|n| n.checked_add(frac)).ok_or(EXPECTED)?;
        Ok(Number::new(num, pow))
        } }
        / expected!("number")

    pub rule string() -> String
        = quiet!{ "\"" s:doubleQuotedCharacter()* "\"" { s.into_iter().collect() } }
        / expected!("string")

    rule doubleQuotedCharacter() -> char
        = !("\"" / "\\") [c] { c }
        / "\\u{" value:$(['0'..='9' | 'a'..='f' | 'A'..='F']+) "}" {? u32::from_str_radix(value, 16).ok().and_then(char::from_u32).ok_or("unicode codepoint in hex") }
        / expected!("valid escape sequence")
});
