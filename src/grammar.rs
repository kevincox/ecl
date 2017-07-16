type ParseResult = Result<::Almost,ParseError>;

extern crate std;

use dict;
use func;
use rc::Rc;

#[derive(Clone,Copy,Debug,PartialEq)]
pub struct Loc {
	line: usize,
	col: usize,
}

#[derive(Clone,Debug,PartialEq)]
pub enum Token {
	Add,
	Assign,
	Call,
	DictClose,
	DictOpen,
	Dot,
	Eq,
	Func,
	Ident(String),
	ListClose,
	ListOpen,
	Num(f64),
	ParenOpen,
	ParenClose,
	StrOpen,
	StrChunk(String),
	StrClose,
	Unexpected(char),
	Unfinished,
}

#[derive(Debug,PartialEq)]
pub enum ErrorType {
	Unexpected(Loc, Token, &'static [Token]),
	Unfinished,
	Unused(Loc, Token),
}

#[derive(Debug,PartialEq)]
pub struct ParseError {
	typ: ErrorType,
	msg: &'static str,
}

macro_rules! while_next {
	{$i:ident, $($($p:pat)|+ => $e:expr),+ $(,)* } => {
		loop {
			match $i.next() {
				$($(Some($p))|+ => $e),+
				Some(c) => { $i.unget(c); break },
				None => break,
			}
		}
	}
}

macro_rules! if_next {
	{$i:ident, $($($p:pat)|+ => $e:expr),+ $(,)* } => {
		match $i.next() {
			$($(Some($p))|+ => { Some($e) }),+
			Some(c) => { $i.unget(c); None },
			None => None,
		}
	}
}

struct Lexer<Input: Iterator<Item=char>> {
	input: Input,
	current: Option<char>,
	state: Vec<usize>,
	loc: Loc,
}

impl<Input: Iterator<Item=char>> Lexer<Input> {
	fn new(input: Input) -> Self {
		let mut statestack = Vec::with_capacity(2);
		statestack.push(1);
		
		Lexer {
			input: input,
			current: None,
			state: statestack,
			loc: Loc{line: 1, col: 0},
		}
	}
	
	fn ident_start_char(c: char) -> bool{
		c.is_alphabetic() || c == '_'
	}
	
	fn ident_char(c: char) -> bool{
		c.is_alphanumeric() || c == '-' || c == '_'
	}
	
	fn ident(&mut self, first: char) -> Token {
		if !Self::ident_start_char(first) { return Token::Unexpected(first) }
		
		let mut s = String::new();
		s.push(first);
		
		while let Some(c) = self.next() {
			if !Self::ident_char(c) {
				self.unget(c);
				break
			}
			s.push(c);
		}
		
		Token::Ident(s)
	}
	
	fn next_is_ident(&mut self) -> bool {
		if let Some(c) = self.peek() {
			return Self::ident_char(c)
		}
		false
	}
	
	fn raw_next(&mut self) -> Option<char> {
		let next = self.input.next();
		if next == Some('\n') {
			self.loc.line += 1;
			self.loc.col = 1;
		} else {
			self.loc.col += 1;
		}
		next
	}
	
	fn next(&mut self) -> Option<char> {
		std::mem::replace(&mut self.current, None).or_else(|| self.raw_next())
	}
	
	fn peek(&mut self) -> Option<char> {
		if self.current.is_none() {
			self.current = self.raw_next();
		}
		self.current
	}
	
	fn consume(&mut self, c: char) -> bool {
		if self.peek() == Some(c) { self.next(); true } else { false }
	}
	
	fn unget(&mut self, c: char) {
		debug_assert!(self.current.is_none());
		self.current = Some(c);
	}
	
	fn next_token(&mut self) -> Option<Token> {
		self.next().and_then(|c| {
			match self.state.last() {
				Some(&0) => self.lex_str(c),
				_ => self.lex_code(c),
			}
		})
	}
	
	// Parses the bit of a number after the 0.
	fn num(&mut self) -> Token {
		if_next!(self,
			'b' => self.num_base(2, '0'),
			'o' => self.num_base(8, '0'),
			'd' => self.num_base(10, '0'),
			'x' => self.num_base(16, '0'),
			c@'0'...'9' => self.num_base(10, c),
		).unwrap_or(Token::Num(0.0))
	}
	
	fn num_int(&mut self, base: u32, first: char) -> u64 {
		let mut n = first.to_digit(base).unwrap() as u64;
		
		while_next! { self,
			'_' => {},
			c@'0'...'9' |
			c@'a'...'f' |
			c@'A'...'F' => {
				if let Some(d) = c.to_digit(base) {
					n *= base as u64;
					n += d as u64;
				} else {
					self.unget(c);
					break;
				}
			},
		}
		
		n
	}
	
	fn num_base(&mut self, base: u32, first: char) -> Token {
		let mut n = self.num_int(base, first) as f64;
		
		let mut numerator = 0 as u64;
		let mut denominator = 1 as u64;
		if self.consume('.') {
			while_next!{self,
				'_' => {},
				c@'0'...'9' |
				c@'a'...'f' |
				c@'A'...'F' => {
					if let Some(d) = c.to_digit(base) {
						numerator *= base as u64;
						numerator += d as u64;
						denominator *= base as u64;
					} else {
						self.unget(c);
						break
					}
				},
			}
		}
		n += numerator as f64 / denominator as f64;
		
		let bindec = |this: &mut Self, bin, dec| {
			if this.consume('i') { bin } else { dec }
		};
		
		n *= if_next!{self,
			'E' => bindec(self, 1152921504606846976.0, 1e18),
			'P' => bindec(self, 1125899906842624.0, 1e15),
			'T' => bindec(self, 1099511627776.0, 1e12),
			'G' => bindec(self, 1073741824.0, 1e9),
			'M' => bindec(self, 1048576.0, 1e6),
			'K' => bindec(self, 1024.0, 1e3), // Accept this as kilo is the only >1 lower case.
			'k' => bindec(self, 1024.0, 1e3),
			'm' => bindec(self, 0.0009765625, 1e-3),
			'u' => bindec(self, 9.5367431640625e-07, 1e-6),
			'µ' => bindec(self, 9.5367431640625e-07, 1e-9),
			'n' => bindec(self, 9.313225746154785e-10, 1e-12),
			'p' => bindec(self, 9.094947017729282e-13, 1e-15),
			'f' => bindec(self, 8.881784197001252e-16, 1e-18),
			'a' => bindec(self, 8.673617379884035e-19, 1e-21),
			'e' => {
				if base != 10 {
					return Token::Unexpected('e')
				}
				
				let sign = match self.next() {
					Some('+') => 1,
					Some('-') => -1,
					Some(c) => { self.unget(c); 1 },
					None => 1,
				};
				let exp = self.num_int(10, '0') as i32;
				(base as f64).powi(sign * exp)
			},
		}.unwrap_or(1.0);
		
		if self.next_is_ident() { return Token::Unexpected(self.next().unwrap()) }
		
		Token::Num(n)
	}
	
	fn lex_code(&mut self, c: char) -> Option<Token> {
		Some(match c {
			'+' => Token::Add,
			':' => Token::Call,
			'0' => self.num(),
			n@'1'...'9' => self.num_base(10, n),
			'=' => match self.next() {
				Some('=') => Token::Eq,
				Some(c) => { self.unget(c); Token::Assign },
				None => Token::Assign,
			},
			'-' => match self.next() {
				Some('>') => Token::Func,
				_ => Token::Unexpected('-'),
			},
			'.' => Token::Dot,
			'{' => {
				self.state.last_mut().map(|depth| *depth += 1).unwrap();
				Token::DictOpen
			},
			'}' => {
				let endofiterpolation = self.state.last_mut().map(|depth| {
					*depth -= 1;
					*depth == 0
				}).unwrap();
				if endofiterpolation { return self.next_token() }
				Token::DictClose
			},
			'[' => Token::ListOpen,
			']' => Token::ListClose,
			'(' => Token::ParenOpen,
			')' => Token::ParenClose,
			'"' => {
				self.state.push(0);
				Token::StrOpen
			},
			'#' => {
				while let Some(c) = self.next() {
					if c == '\n' { return self.next_token() }
				}
				return None
			},
			c if c.is_whitespace() => return self.next_token(),
			c => self.ident(c),
		})
	}
	
	fn lex_str(&mut self, c: char) -> Option<Token> {
		match c {
			'"' => {
				self.state.pop();
				return Some(Token::StrClose)
			},
			'$' => {
				match self.next() {
					Some('{') => {
						debug_assert!(self.state.last() == Some(&0));
						if let Some(c) = self.next() {
							self.state.last_mut().map(|s| *s = 1);
							return self.lex_code(c)
						}
						return Some(Token::Unfinished)
					},
					Some(c) => return Some(self.ident(c)),
					None => return Some(Token::Unfinished),
				}
			},
			c => self.unget(c),
		}
		
		let mut s = String::new();
		
		loop {
			match self.next() {
				Some('"') => { self.unget('"'); break },
				Some('$') => { self.unget('$'); break },
				Some('\\') => match self.next() {
					Some('n') => s.push('\n'),
					Some('t') => s.push('\t'),
					Some('0') => s.push('\0'),
					Some(c@'a'...'z') |
					Some(c@'A'...'Z') |
					Some(c@'0'...'9') =>
						panic!("Unknown escape sequence \"\\{}\"", c),
					Some(c) => s.push(c),
					None => return None,
				},
				Some(c) => s.push(c),
				None => return Some(Token::Unfinished),
			}
		}
		
		Some(Token::StrChunk(s))
	}
}

impl<Input: Iterator<Item=char>> Iterator for Lexer<Input> {
	type Item = (Token,Loc);
	
	fn next(&mut self) -> Option<Self::Item> {
		self.next_token().map(|t| (t, self.loc))
	}
}

macro_rules! expect_next {
	{$i:ident : $c:expr, $($($p:pat)|+ => $e:expr),+ $(,)* } => {
		match $i.next() {
			$($(Some(($p,_)))|+ => { $e }),+
			Some((t,l)) => {
				return Err(ParseError{
					typ: ErrorType::Unexpected(l, t, &[]),
					msg: $c,
				})
			},
			None => {
				return Err(ParseError{
					typ: ErrorType::Unfinished,
					msg: $c,
				})
			},
		}
	}
}

struct Parser<Input: Iterator<Item=(Token,Loc)>> {
	input: Input,
	current: Option<(Token,Loc)>,
}

impl<Input: Iterator<Item=(Token,Loc)>> Parser<Input> {
	fn next(&mut self) -> Option<(Token,Loc)> {
		std::mem::replace(&mut self.current, None).or_else(|| self.input.next())
	}
	
	fn peek(&mut self) -> Option<&(Token,Loc)> {
		if self.current.is_none() {
			self.current = self.input.next();
		}
		self.current.as_ref()
	}
	
	fn unget(&mut self, t: (Token,Loc)) {
		debug_assert!(self.current.is_none());
		self.current = Some(t);
	}
	
	
	fn consume(&mut self, tok: Token) -> bool {
		if let Some(t) = self.next() {
			if t.0 == tok {
				return true
			}
			self.unget(t)
		}
		
		false
	}
	
	fn document(&mut self) -> ParseResult {
		let e = match self.next() {
			Some((Token::Ident(s), _)) => match self.next() {
				Some((Token::Ident(s2), loc)) => {
					if s == "local" {
						self.document_local(s2)
					} else {
						Err(ParseError{
							typ: ErrorType::Unexpected(loc, Token::Ident(s2), &[]),
							msg: "Two identifiers in a row",
						})
					}
				},
				Some(tl@(Token::Dot, _)) |
				Some(tl@(Token::Assign, _)) => {
					self.unget(tl);
					let path = vec![::Almost::StrStatic(s)];
					let first = self.dict_item_end(path)?;
					self.document_items(first)
				},
				Some(tl) => {
					self.unget(tl);
					self.suffixes(::Almost::Ref(s))
				},
				None => Ok(::Almost::Ref(s)),
			},
			Some(tl) => {
				self.unget(tl);
				self.expr()
			}
			None => {
				Err(ParseError{
					typ: ErrorType::Unfinished,
					msg: "Empty document",
				})
			}
		}?;
		
		match self.next() {
			Some((t,l)) => Err(ParseError{
				typ: ErrorType::Unused(l, t),
				msg: "At end of document",
			}),
			None => Ok(e),
		}
	}
	
	fn document_local(&mut self, ident: String) -> ParseResult {
		expect_next!{self: "parsing initial local", Token::Assign => {}};
		let first = dict::AlmostDictElement::Priv(ident, self.expr()?);
		self.document_items(first)
	}
	
	fn document_items(&mut self, first: dict::AlmostDictElement) -> ParseResult {
		let mut items = vec![first];
		while self.peek().is_some() {
			items.push(self.dict_item()?);
		}
		Ok(::Almost::Dict(items))
	}
	
	fn dict_items(&mut self) -> ParseResult {
		let mut items = Vec::new();
		while !self.consume(Token::DictClose) {
			items.push(self.dict_item()?);
		}
		Ok(::Almost::Dict(items))
	}
	
	fn dict_item(&mut self) -> Result<dict::AlmostDictElement,ParseError> {
		let mut path = vec![];
		
		expect_next!{self: "parsing dict element",
			Token::Ident(s) => match s.as_str() {
				"local" => match self.next() {
					Some((Token::Ident(s),_)) => {
						expect_next!{self: "parsing local var", Token::Assign => {}};
						return Ok(dict::AlmostDictElement::Priv(s, self.expr()?))
					}
					Some(t) => self.unget(t),
					None => {},
				},
				_ => path.push(::Almost::StrStatic(s)),
			},
			Token::StrOpen => path.push(self.string()?),
		};
		
		self.dict_item_end(path)
	}
	
	fn dict_item_end(&mut self, mut path: Vec<::Almost>)
		-> Result<dict::AlmostDictElement,ParseError>
	{
		while self.consume(Token::Dot) {
			expect_next!{self: "parsing dict",
				Token::Ident(s) => path.push(::Almost::StrStatic(s)),
				Token::StrOpen => path.push(self.string()?),
			}
		}
		expect_next!{self: "parsing dict eq", Token::Assign => {}};
		Ok(dict::AlmostDictElement::Dyn(Rc::new((path, self.expr()?))))
	}
	
	fn list_items(&mut self) -> ParseResult {
		let mut items = Vec::new();
		while !self.consume(Token::ListClose) {
			items.push(self.expr()?);
		}
		Ok(::Almost::list(items))
	}
	
	fn expr(&mut self) -> ParseResult {
		let r = self.atom()?;
		self.suffixes(r)
	}
	
	fn suffixes(&mut self, mut r: ::Almost) -> ParseResult {
		loop {
			match self.next() {
				Some((Token::Add, _)) => r = ::Almost::Add(Box::new(r), Box::new(self.expr()?)),
				Some((Token::Call, _)) => r = ::Almost::Call(Box::new(r), Box::new(self.expr()?)),
				Some((Token::Dot, _)) => expect_next!{self: "parsing index",
					Token::Ident(s) =>
						r = ::Almost::Index(Box::new(r), Box::new(::Almost::StrStatic(s))),
					Token::StrOpen =>
						r = ::Almost::Index(Box::new(r), Box::new(self.string()?)),
				},
				Some((Token::Eq, _)) => r = ::Almost::Eq(Box::new(r), Box::new(self.expr()?)),
				Some(other) => { self.unget(other); break },
				None => break,
			}
		}
		
		Ok(r)
	}
	
	fn func(&mut self) -> ParseResult {
		let args = self.args()?;
		Ok(::Almost::function(args, self.expr()?))
	}
	
	fn args(&mut self) -> Result<func::Arg,ParseError> {
		expect_next!{self: "parsing function argument",
			Token::Ident(name) => Ok(func::Arg::One(name)),
			Token::DictOpen => {
				let mut args = Vec::new();
				
				while !self.consume(Token::DictClose) {
					let k = expect_next!{self: "destructure dict key",
						Token::Ident(k) => k,
					};
					
					if self.consume(Token::Assign) {
						args.push((k.clone(), false, self.expr()?));
					} else {
						args.push((k.clone(), true, ::Almost::Nil));
					}
				}
				
				Ok(func::Arg::Dict(args))
			},
		}
	}
	
	fn atom(&mut self) -> ParseResult {
		expect_next!{self: "parsing atom",
			Token::DictOpen => self.dict_items(),
			Token::Func => self.func(),
			Token::Ident(s) => Ok(::Almost::Ref(s)),
			Token::ListOpen => self.list_items(),
			Token::Num(n) => Ok(::Almost::Num(n)),
			Token::ParenOpen => {
				let e = self.expr()?;
				expect_next!{self: "closing bracket", Token::ParenClose => {}};
				Ok(e)
			},
			Token::StrOpen => self.string(),
		}
	}
	
	fn string(&mut self) -> ParseResult {
		let mut pieces = vec![];
		loop {
			match self.next() {
				Some((Token::StrChunk(s), _)) => pieces.push(::StringPart::Lit(s)),
				Some((Token::StrClose, _)) => break,
				Some(t) => {
					self.unget(t);
					pieces.push(::StringPart::Exp(self.expr()?))
				},
				None => {
					return Err(ParseError{
						typ: ErrorType::Unfinished,
						msg: "parsing string",
					})
				},
			}
		}
		Ok(::Almost::Str(pieces))
	}
}

pub fn parse<Input: Iterator<Item=char>>(input: Input) -> ParseResult {
	let lexer = Lexer::new(input);
	// let lexer = lexer.inspect(|t| println!("Token: {:?}", t));
	Parser{input: lexer, current: None}.document()
}


#[cfg(test)]
mod tests {
	use super::*;
	
	#[test]
	fn num_decimal() {
		assert_eq!(::parse("10"), Ok(::Val::new(10.0)));
		assert_eq!(::parse("0d10"), Ok(::Val::new(10.0)));
		assert_eq!(::parse("10.4"), Ok(::Val::new(10.4)));
		assert_eq!(::parse("10.4e6"), Ok(::Val::new(10400000.0)));
		assert_eq!(::parse("10.4e+6"), Ok(::Val::new(10400000.0)));
		assert_eq!(::parse("10.4e-6"), Ok(::Val::new(0.0000104)));
		assert_eq!(::parse("104_000__000_e-6_"), Ok(::Val::new(104.0)));
		assert_eq!(::parse("1M"), Ok(::Val::new(1_000_000.0)));
		assert_eq!(::parse("1ki"), Ok(::Val::new(1024.0)));
		assert_eq!(::parse("4u"), Ok(::Val::new(0.000_004)));
	}
	
	#[test]
	fn num_binary() {
		assert_eq!(::parse("0b10"), Ok(::Val::new(2.0)));
		assert_eq!(::parse("0b10.1"), Ok(::Val::new(2.5)));
		assert_eq!(::parse("0b10.1e6"), Err(ParseError{
			typ: ErrorType::Unexpected(
				Loc{line: 1, col: 7}, Token::Unexpected('e'), &[]),
			msg: "parsing atom",
		}));
		assert_eq!(::parse("0b1M"), Ok(::Val::new(1_000_000.0)));
		assert_eq!(::parse("0b1u"), Ok(::Val::new(0.000_001)));
	}
	
	#[test]
	fn num_hex() {
		assert_eq!(::parse("0x10"), Ok(::Val::new(16.0)));
		assert_eq!(::parse("0x8.8"), Ok(::Val::new(8.5)));
		assert_eq!(::parse("0x00.1"), Ok(::Val::new(0.0625)));
	}
}
