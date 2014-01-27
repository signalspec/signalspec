use bitv;
use bitv::Bitv;

#[deriving(Eq)]
pub enum Type {
	SymbolType, // TODO: include variants?
	BitsType(uint),
	NumberType,
	EntityType,
	InvalidType,
	TopType
}

pub enum BinOp {
	BiAdd,
	BiMul,
	BiSub,
	BiDiv,

	BiAnd,
	BiOr,
	BiXor,
}

pub enum Expr {
	ValueExpr(Value),
	IgnoreExpr,
	
	FlipExpr(~Expr, ~Expr),
	RangeExpr(~Expr, ~Expr),
	ChooseExpr(~Expr, ~[(Expr, Expr)]),
	ConcatExpr(~[Expr]),

	BinExpr(~Expr, BinOp, ~Expr),

	VarExpr(~str),
	DotExpr(~Expr, ~str),
}

#[deriving(Clone, Eq)]
pub enum Value {
	NumberValue(f64),
	SymbolValue(~str),
	BitsValue(Bitv),
}

fn common_type(a: Type, b: Type) -> Type{
	match (a, b) {
		(TopType, x) | (x, TopType) => x,
		(a, b) if a == b => a,
		_ => InvalidType
	}
}

#[inline]
fn common_type_all<T:Iterator<Type>>(l: &mut T) -> Type{
	l.fold(TopType, common_type)
}

impl Expr {
	pub fn get_type(&self) -> Type {
		match (*self) {
			IgnoreExpr => TopType,
			ValueExpr(ref val) => val.get_type(),
			FlipExpr(ref l, ref r) => common_type(l.get_type(), r.get_type()),
			RangeExpr(..) => NumberType,
			ChooseExpr(ref e, ref c) => {
				let ls = common_type_all(&mut c.iter().map(|&(ref a,_)| a.get_type()));
				let rs = common_type_all(&mut c.iter().map(|&(_,ref b)| b.get_type()));
				match common_type(e.get_type(), ls) {
					InvalidType => InvalidType,
					_ => rs
				}
			}
			ConcatExpr(ref v) =>  {
				let mut len: uint = 0;
				for e in v.iter() {
					match e.get_type() {
						BitsType(n) => {
							len += n;
						}
						_ => {
							return InvalidType;
						}
					}
				}
				BitsType(len)
			}
			BinExpr(~ref a, _, ~ref b) => common_type(a.get_type(), b.get_type()),
			VarExpr(..) | DotExpr(..) => InvalidType, // TODO: need context lookup

		}
	}

	pub fn const_down(&self) -> Option<Value> {
		fn binop(l: &Expr, r: &Expr, f: |a: f64, b: f64| -> f64) -> Option<Value>{
			match (l.const_down(), r.const_down()) {
				(Some(NumberValue(lv)), Some(NumberValue(rv))) =>
					Some(NumberValue(f(lv, rv))),
				_ => None,
			}
		}

		match (*self) {
			IgnoreExpr => None,
			ValueExpr(ref v) => Some((*v).clone()),
			FlipExpr(ref l, _) => l.const_down(),
			RangeExpr(..) => None,
			ChooseExpr(ref e, ref c) => {
				match e.const_down() {
					Some(ev) => {
						for &(ref l, ref r) in c.iter() {
							if l.const_up(&ev) {
								return r.const_down();
							}
						}
						None
					}
					None => None
				}
			}
			ConcatExpr(ref subexprs) => {
				let values: ~[Bitv] = subexprs.iter()
					.filter_map(|e| e.const_down())
					.filter_map(|v| match v { BitsValue(r) => Some(r), _ => None })
					.collect();

				if values.len() == subexprs.len() {
					Some(BitsValue(bitv::concat(values)))
				} else {
					None
				}
			}
			BinExpr(~ref l, BiAdd, ~ref r) => binop(l, r, |a, b| a+b),
			BinExpr(~ref l, BiMul, ~ref r) => binop(l, r, |a, b| a*b),
			BinExpr(..) => None,

			VarExpr(..) | DotExpr(..) => None,
		}
	}

	pub fn const_up(&self, value: &Value) -> bool {
		match (*self) {
			IgnoreExpr => true,
			ValueExpr(ref v) => v.matches(value),
			FlipExpr(_, ref r) => r.const_up(value),
			RangeExpr (ref min, ref max) => {
				match (value, min.const_down(), max.const_down()) {
					(&NumberValue(v), Some(NumberValue(minv)), Some(NumberValue(maxv))) => {
						v >= minv && v < maxv
					}
					_ => false
				}
			}
			ChooseExpr(ref e, ref c) => {
				for &(ref l, ref r) in c.iter() {
					if r.const_up(value) {
						return l.const_down().map_or(false, |lv| e.const_up(&lv))
					}
				}
				false
			}
			ConcatExpr(ref subexprs) => {
				let mut pos = 0u;

				let val = match *value {
					BitsValue(ref v) => v,
					_ => return false
				};

				for subexpr in subexprs.iter() {
					match subexpr.get_type() {
						BitsType(width) => {
							let slice = BitsValue(val.slice(pos, pos+width));
							pos += width;
							if subexpr.const_up(&slice) == false {
								return false;
							}
						}
						_ => return false
					}
				}
				true
			}
			BinExpr(..) => self.const_down().map_or(false, |x| x.matches(value)),
			VarExpr(..) | DotExpr(..) => false,
		}
	}
}

impl Value {
	pub fn get_type(&self) -> Type {
		match (*self) {
			NumberValue(..) => NumberType,
			SymbolValue(..) => SymbolType,
			BitsValue(ref n) => BitsType(n.len()),
		}
	}

	pub fn matches(&self, other: &Value) -> bool {
		*self == *other
	}
}

impl ToStr for Value {
	fn to_str(&self) -> ~str {
		match (*self) {
			NumberValue(n) => n.to_str(),
			SymbolValue(ref s) => "$" + *s,
			BitsValue(ref n) => "'b" + n.to_str(),
		}
	}
}