use std;

type ParseResult = Result<crate::Almost,ParseError>;

#[derive(Clone,Copy,PartialEq)]
pub struct Loc {
	pub line: usize,
	pub col: usize,
}

impl std::fmt::Debug for Loc {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}.{}", self.line, self.col)
	}
}

#[derive(Clone,Debug,PartialEq)]
pub enum StrType {
	String,
	Relative,
	Parent,
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
	Great,
	GreatEq,
	Ident(String),
	Less,
	LessEq,
	ListClose,
	ListOpen,
	Ne,
	Neg,
	Not,
	Num(f64),
	ParenClose,
	ParenOpen,
	StrChunk(String),
	StrClose,
	StrOpen(StrType),
	StructIdent(usize, String),
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

impl std::fmt::Display for ParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{} {:?}", self.msg, self.typ)
	}
}

impl std::error::Error for ParseError { }

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

#[derive(Debug,PartialEq)]
enum LexerMode {
	Code(usize),
	String,
	Path,
}

struct Lexer<Input: Iterator<Item=char>> {
	input: Input,
	current: Option<char>,
	state: Vec<LexerMode>,
	start_loc: Loc,
	loc: Loc,
}

impl<Input: Iterator<Item=char>> Lexer<Input> {
	fn new(input: Input) -> Self {
		let mut statestack = Vec::with_capacity(2);
		statestack.push(LexerMode::Code(2));

		Lexer {
			input: input,
			current: None,
			state: statestack,
			start_loc: Loc{line: 1, col: 0},
			loc: Loc{line: 1, col: 0},
		}
	}

	fn ident_start_char(c: char) -> bool{
		c.is_alphabetic() || c == '_'
	}

	fn ident_char(c: char) -> bool{
		c.is_alphanumeric() || c == '-' || c == '_'
	}

	fn ident_str(&mut self, first: char) -> Result<String,Token> {
		if !Self::ident_start_char(first) { return Err(self.unexpected(first)) }

		let mut s = String::new();
		s.push(first);

		while let Some(c) = self.next() {
			if !Self::ident_char(c) {
				self.unget(c);
				break
			}
			s.push(c);
		}

		Ok(s)
	}

	fn ident(&mut self, first: char) -> Token {
		self.ident_str(first).map(Token::Ident).unwrap_or_else(|t| t)
	}

	fn next_is_ident(&mut self) -> bool {
		if let Some(c) = self.peek() {
			return Self::ident_char(c)
		}
		false
	}

	fn raw_next(&mut self) -> Option<char> {
		let next = self.input.next();
		next
	}

	fn unexpected(&mut self, c: char) -> Token {
		self.start_loc = self.loc; // Report the bad char, not the start of the token.
		Token::Unexpected(c)
	}

	fn next(&mut self) -> Option<char> {
		let next = std::mem::replace(&mut self.current, None).or_else(|| self.raw_next());
		if next == Some('\n') {
			self.loc.line += 1;
			self.loc.col = 0;
		} else {
			self.loc.col += 1;
		}
		// println!("At {:?} got {:?}", self.loc, next);
		next
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
		if c == '\n' { self.loc.line -= 1 } else { self.loc.col -= 1 }
		self.current = Some(c);
	}

	fn next_token(&mut self) -> Option<Token> {
		self.next().and_then(|c| {
			self.start_loc = self.loc; // Capture at start.
			match self.state.last().unwrap() {
				&LexerMode::Code(_) => self.lex_code(c),
				&LexerMode::Path => Some(self.lex_path(c)),
				&LexerMode::String => Some(self.lex_str(c)),
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
			c@'0'..='9' => self.num_base(10, c),
			'.' => self.num_base(10, '0'),
		).unwrap_or(Token::Num(0.0))
	}

	fn num_int(&mut self, base: u32, first: char) -> u64 {
		let mut n = first.to_digit(base).unwrap() as u64;

		while_next! { self,
			'_' => {},
			c@'0'..='9' |
			c@'a'..='f' |
			c@'A'..='F' => {
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
				c@'0'..='9' |
				c@'a'..='f' |
				c@'A'..='F' => {
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
					return self.unexpected('e')
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

		if self.next_is_ident() {
			let c = self.next().unwrap();
			return self.unexpected(c)
		}

		Token::Num(n)
	}

	fn relative_reference(&mut self) -> Token {
		let mut up = 1;
		loop {
			match self.next() {
				Some('.') => up += 1,
				Some(c) =>
					return self.ident_str(c)
						.map(|i| Token::StructIdent(up, i))
						.unwrap_or_else(|t| t),
				None => return Token::Unfinished,
			}
		}
	}

	fn lex_code(&mut self, c: char) -> Option<Token> {
		Some(match c {
			'!' => match self.next() {
				Some('=') => Token::Ne,
				Some(c) => { self.unget(c); Token::Not },
				None => Token::Not,
			}
			'+' => Token::Add,
			':' => Token::Call,
			'0' => self.num(),
			n@'1'..='9' => self.num_base(10, n),
			'=' => match self.next() {
				Some('=') => Token::Eq,
				Some(c) => { self.unget(c); Token::Assign },
				None => Token::Assign,
			},
			'<' => match self.next() {
				Some('=') => Token::LessEq,
				Some(c) => { self.unget(c); Token::Less },
				None => Token::Less,
			}
			'>' => match self.next() {
				Some('=') => Token::GreatEq,
				Some(c) => { self.unget(c); Token::Great },
				None => Token::Great,
			}
			'-' => match self.next() {
				Some('>') => Token::Func,
				Some(c) => { self.unget(c); Token::Neg },
				None => Token::Neg,
			},
			'.' => match self.next() {
				Some('/') => {
					self.state.push(LexerMode::Path);
					Token::StrOpen(StrType::Relative)
				},
				Some('.') => match self.next() {
					Some('/') => {
						self.state.push(LexerMode::Path);
						Token::StrOpen(StrType::Parent)
					}
					Some(c) => {
						self.unget(c);
						self.relative_reference()
					}
					None => Token::Unfinished,
				},
				Some(c) => { self.unget(c); Token::Dot },
				None => Token::Assign,
			},
			'{' => {
				match self.state.last_mut() {
					Some(&mut LexerMode::Code(ref mut depth)) => *depth += 1,
					other => unreachable!("Unexpected mode {:?}", other)
				};
				Token::DictOpen
			},
			'}' => {
				let depth = match self.state.last_mut() {
					Some(&mut LexerMode::Code(ref mut depth)) => {
						*depth -= 1;
						*depth
					},
					other => unreachable!("Unexpected mode {:?}", other),
				};
				if depth == 0 {
					self.state.pop();
					return self.next_token()
				} else {
					Token::DictClose
				}
			},
			'[' => Token::ListOpen,
			']' => Token::ListClose,
			'(' => Token::ParenOpen,
			')' => Token::ParenClose,
			'"' => {
				self.state.push(LexerMode::String);
				Token::StrOpen(StrType::String)
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

	fn lex_interpolation(&mut self) -> Token {
		match self.next() {
			Some('{') => {
				if let Some(c) = self.next() {
					self.state.push(LexerMode::Code(1));
					return self.lex_code(c).unwrap_or(Token::Unfinished);
				}
				return Token::Unfinished
			},
			Some(c) => return self.ident(c),
			None => return Token::Unfinished,
		}
	}

	fn lex_str(&mut self, c: char) -> Token {
		match c {
			'"' => {
				let old = self.state.pop();
				debug_assert_eq!(old, Some(LexerMode::String));
				return Token::StrClose
			},
			'$' => return self.lex_interpolation(),
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
					Some(c@'a'..='z') |
					Some(c@'A'..='Z') |
					Some(c@'0'..='9') =>
						// Unknown escape sequence.
						return Token::Unexpected(c),
					Some(c) => s.push(c),
					None => return Token::Unfinished,
				},
				Some(c) => s.push(c),
				None => return Token::Unfinished,
			}
		}

		Token::StrChunk(s)
	}

	fn lex_path(&mut self, c: char) -> Token {
		match c {
			'$' => return self.lex_interpolation(),
			c => self.unget(c),
		}

		let mut s = String::new();

		loop {
			match self.next() {
				Some('$') => { self.unget('$'); break },
				Some(c@'/') | Some(c@'.') => s.push(c),
				Some(c) if Self::ident_char(c) => s.push(c),
				Some(c) => { self.unget(c); break },
				None => {
					self.unget(' '); // Hack to close path.
					break
				},
			}
		}

		if s.is_empty() {
			let old = self.state.pop();
			debug_assert_eq!(old, Some(LexerMode::Path));
			Token::StrClose
		} else {
			Token::StrChunk(s)
		}

	}
}

impl<Input: Iterator<Item=char>> Iterator for Lexer<Input> {
	type Item = (Token,Loc);

	fn next(&mut self) -> Option<Self::Item> {
		self.next_token().map(|t| (t, self.start_loc))
	}
}

macro_rules! expect_next {
	{$i:ident : $c:expr, $($($p:pat)|+ => $e:expr),+ $(,)* } => {
		match $i.next() {
			$($(Some($p))|+ => { $e }),+
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

struct Parser<'a, Input: Iterator<Item=(Token,Loc)>> {
	input: Input,
	current: Option<(Token,Loc)>,
	// file: &'a str,
	directory: &'a str,
}

impl<'a, Input: Iterator<Item = (Token, Loc)>> Parser<'a, Input> {
	fn new(path: &'a str, input: Input) -> Self {
		let last_slash = path.rfind('/').map(|i| i + 1).unwrap_or(0);

		Parser{
			// file: path,
			directory: &path[0..last_slash],
			input: input,
			current: None,
		}
	}

	fn next(&mut self) -> Option<(Token,Loc)> {
		std::mem::replace(&mut self.current, None).or_else(|| self.input.next())
	}

	fn peek(&mut self) -> Option<&Token> {
		if self.current.is_none() {
			self.current = self.input.next();
		}
		self.current.as_ref().map(|p| &p.0)
	}

	fn unget(&mut self, t: (Token,Loc)) {
		debug_assert!(self.current.is_none());
		self.current = Some(t);
	}


	fn consume(&mut self, tok: Token) -> Option<Loc> {
		if let Some(t) = self.next() {
			if t.0 == tok {
				return Some(t.1)
			}
			self.unget(t)
		}

		None
	}

	fn document(&mut self) -> ParseResult {
		let is_expr = match self.peek() {
			Some(&Token::DictOpen) |
			Some(&Token::Func) |
			Some(&Token::ListOpen) |
			Some(&Token::ParenOpen) => true,
			_ => false,
		};

		let e = if is_expr { self.expr()? } else { self.document_items()? };

		match self.next() {
			Some((t,l)) => Err(ParseError{
				typ: ErrorType::Unused(l, t),
				msg: "At end of document",
			}),
			None => Ok(e),
		}
	}

	fn document_items(&mut self) -> ParseResult {
		let mut items = Vec::new();
		while self.peek().is_some() {
			items.push(self.dict_item()?);
		}
		items.sort_unstable_by(crate::dict::AlmostDictElement::sort_cmp);
		Ok(crate::Almost::Dict(items))
	}

	fn dict_items(&mut self) -> ParseResult {
		let mut items = Vec::new();
		while !self.consume(Token::DictClose).is_some() {
			items.push(self.dict_item()?);
		}
		items.sort_unstable_by(crate::dict::AlmostDictElement::sort_cmp);

		Ok(crate::Almost::Dict(items))
	}

	fn dict_item(&mut self) -> Result<crate::dict::AlmostDictElement, ParseError> {
		let ade = expect_next! {self: "parsing dict element",
			(Token::Ident(s), loc) => {
				match self.peek() {
					Some(Token::Assign) | Some(Token::Dot) => {
						let val = self.dict_val()?;
						crate::dict::AlmostDictElement::public(loc, s, val)
					}
					_ => {
						match s.as_str() {
							"assert" => {
								crate::dict::AlmostDictElement::assert(loc, self.expr()?)
							}
							"local" => {
								let name = expect_next!{self: "parsing local name",
									(Token::Ident(name), _) => name,
								};
								expect_next!{self: "parsing local var", (Token::Assign, _) => {}};

								crate::dict::AlmostDictElement::local(loc, name, self.expr()?)
							}
							_ => {
								expect_next!{self: "parsing dict key",
									(Token::Assign, _) => unreachable!("invalid type"),
								};
							}
						}
					}
				}
			},
			(Token::StrOpen(StrType::String), loc) => {
				let s = expect_next!{self: "parsing quoted dict key",
					(Token::StrChunk(s), _) => {
						expect_next!{self: "parsing quoted dict key", (Token::StrClose, _) => {}};
						s
					},
					(Token::StrClose, _) => "".to_owned(),
				};

				let val = self.dict_val()?;
				crate::dict::AlmostDictElement::public(loc, s, val)
			},
		};

		Ok(ade)
	}

	fn dict_val(&mut self) -> ParseResult {
		expect_next!{self: "parsing dict key",
			(Token::Dot, _) => {
				let k = expect_next!{self: "parsing dict key",
					(Token::Ident(k), _) => k,
					(Token::StrOpen(StrType::String), _) => {
						let s = expect_next!{self: "parsing quoted dict key", (Token::StrChunk(s), _) => s};
						expect_next!{self: "parsing quoted dict key", (Token::StrClose, _) => {}};
						s
					},
				};
				let v = self.dict_val()?;
				Ok(crate::Almost::ADict(k, Box::new(v)))
			},
			(Token::Assign, _) => self.expr(),
		}
	}

	fn list_items(&mut self) -> ParseResult {
		let mut items = Vec::new();
		while !self.consume(Token::ListClose).is_some() {
			items.push(self.expr()?);
		}
		Ok(crate::Almost::List(items))
	}

	fn func(&mut self) -> ParseResult {
		let args = self.args()?;
		Ok(crate::Almost::Func(Box::new(crate::func::FuncData{arg: args, body: self.expr()?})))
	}

	fn args(&mut self) -> Result<crate::func::Arg,ParseError> {
		expect_next!{self: "parsing function argument",
			(Token::Ident(name), _) => Ok(crate::func::Arg::One(name)),
			(Token::DictOpen, _) => {
				let mut args = Vec::new();

				while !self.consume(Token::DictClose).is_some() {
					let k = expect_next!{self: "destructure dict key",
						(Token::Ident(k), _) => k,
					};

					if let Some(_) = self.consume(Token::Assign) {
						args.push((k.clone(), false, self.expr()?));
					} else {
						args.push((k.clone(), true, crate::Almost::Nil));
					}
				}

				Ok(crate::func::Arg::Dict(args))
			},
			(Token::ListOpen, _) => {
				let mut args = Vec::new();

				while !self.consume(Token::ListClose).is_some() {
					let k = expect_next!{self: "destructure list key",
						(Token::Ident(k), _) => k,
					};

					if let Some(_) = self.consume(Token::Assign) {
						args.push((k.clone(), false, self.expr()?));
					} else {
						args.push((k.clone(), true, crate::Almost::Nil));
					}
				}

				Ok(crate::func::Arg::List(args))
			},
		}
	}

	fn expr(&mut self) -> ParseResult {
		self.expr_eq()
	}

	fn expr_eq(&mut self) -> ParseResult {
		let mut r = self.expr_ops()?;

		loop {
			match self.next() {
				Some((Token::Eq, loc)) =>
					r = crate::Almost::Eq(loc, Box::new(r), Box::new(self.expr_ops()?)),
				Some((Token::Great, loc)) =>
					r = crate::Almost::Great(loc, Box::new(r), Box::new(self.expr_ops()?)),
				Some((Token::GreatEq, loc)) =>
					r = crate::Almost::GreatEq(loc, Box::new(r), Box::new(self.expr_ops()?)),
				Some((Token::Less, loc)) =>
					r = crate::Almost::Less(loc, Box::new(r), Box::new(self.expr_ops()?)),
				Some((Token::LessEq, loc)) =>
					r = crate::Almost::LessEq(loc, Box::new(r), Box::new(self.expr_ops()?)),
				Some(other) => { self.unget(other); break },
				None => break,
			}
		}

		Ok(r)
	}

	fn expr_ops(&mut self) -> ParseResult {
		let mut r = self.expr_unary()?;

		loop {
			match self.next() {
				Some((Token::Add, l)) => {
					r = crate::Almost::Add(l, Box::new(r), Box::new(self.expr_unary()?))
				},
				Some((Token::Ne, l)) => {
					r = crate::Almost::Ne(l, Box::new(r), Box::new(self.expr_unary()?))
				},
				Some((Token::Neg, l)) => {
					r = crate::Almost::Sub(l, Box::new(r), Box::new(self.expr_unary()?))
				},
				Some(other) => { self.unget(other); break },
				None => break,
			}
		}

		Ok(r)
	}

	fn expr_unary(&mut self) -> ParseResult {
		match self.consume(Token::Neg) {
			Some(loc) => Ok(crate::Almost::Neg(loc, Box::new(self.expr_call()?))),
			None => self.expr_call(),
		}
	}

	fn expr_call(&mut self) -> ParseResult {
		let mut r = self.expr_index()?;

		while let Some(loc) = self.consume(Token::Call) {
			r = crate::Almost::Call(loc, Box::new(r), Box::new(self.expr_index()?));
		}

		Ok(r)
	}


	fn expr_index(&mut self) -> ParseResult {
		let mut r = self.atom()?;

		while let Some(loc) = self.consume(Token::Dot) {
			expect_next!{self: "parsing index",
				(Token::Ident(s), _) =>
					r = crate::Almost::Index(loc, Box::new(r), Box::new(crate::Almost::StrStatic(s))),
				(Token::StrOpen(t), _) =>
					r = crate::Almost::Index(loc, Box::new(r), Box::new(self.string(t)?)),
			}
		}

		Ok(r)
	}

	fn atom(&mut self) -> ParseResult {
		expect_next!{self: "parsing atom",
			(Token::DictOpen, _) => self.dict_items(),
			(Token::Func, _) => self.func(),
			(Token::Ident(s), loc) => Ok(crate::Almost::Ref(loc, s)),
			(Token::StructIdent(d, s), loc) => Ok(crate::Almost::StructRef(loc, d, s)),
			(Token::ListOpen, _) => self.list_items(),
			(Token::Num(n), _) => Ok(crate::Almost::Num(n)),
			(Token::ParenOpen, _) => {
				let e = self.expr()?;
				expect_next!{self: "closing bracket", (Token::ParenClose, _) => {}};
				Ok(e)
			},
			(Token::StrOpen(t), _) => self.string(t),
		}
	}

	fn string(&mut self, typ: StrType) -> ParseResult {
		let mut pieces = vec![];

		match typ {
			StrType::Relative => {
				pieces.push(crate::StringPart::Lit(self.directory.to_owned()));
			}
			StrType::Parent => {
				pieces.push(crate::StringPart::Lit(self.directory.to_owned() + "../"));
			}
			StrType::String => {},
		}

		// println!("Pieces: {:?}", pieces);

		loop {
			match self.next() {
				Some((Token::StrChunk(s), _)) => pieces.push(crate::StringPart::Lit(s)),
				Some((Token::StrClose, _)) => break,
				Some(t) => {
					self.unget(t);
					pieces.push(crate::StringPart::Exp(self.expr()?))
				},
				None => {
					return Err(ParseError{
						typ: ErrorType::Unfinished,
						msg: "parsing string",
					})
				},
			}
		}

		if let &[crate::StringPart::Lit(_)] = pieces.as_slice() {
			if let crate::StringPart::Lit(s) = pieces.pop().unwrap() {
				Ok(crate::Almost::StrStatic(s))
			} else {
				unreachable!();
			}
		} else {
			Ok(crate::Almost::Str(pieces))
		}
	}
}

pub fn parse<Input: Iterator<Item=char>>(file: &str, input: Input) -> ParseResult {
	let lexer = Lexer::new(input);
	// let lexer = lexer.inspect(|t| println!("Token: {:?}", t));
	Parser::new(file, lexer).document()
}

pub fn parse_expr<Input: Iterator<Item=char>>(file: &str, input: Input) -> ParseResult {
	let lexer = Lexer::new(input);
	// let lexer = lexer.inspect(|t| println!("Token: {:?}", t));
	Parser::new(file, lexer).expr()
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn num_decimal() {
		assert_eq!(crate::eval("<str>", "(10)").get_num(), Some(10.0));
		assert_eq!(crate::eval("<str>", "(0d10)").get_num(), Some(10.0));
		assert_eq!(crate::eval("<str>", "(10.4)").get_num(), Some(10.4));
		assert_eq!(crate::eval("<str>", "(10.4e6)").get_num(), Some(10400000.0));
		assert_eq!(crate::eval("<str>", "(10.4e+6)").get_num(), Some(10400000.0));
		assert_eq!(crate::eval("<str>", "(10.4e-6)").get_num(), Some(0.0000104));
		assert_eq!(crate::eval("<str>", "(104_000__000_e-6_)").get_num(), Some(104.0));
		assert_eq!(crate::eval("<str>", "(1M)").get_num(), Some(1_000_000.0));
		assert_eq!(crate::eval("<str>", "(1ki)").get_num(), Some(1024.0));
		assert_eq!(crate::eval("<str>", "(4u)").get_num(), Some(0.000_004));
		assert_eq!(crate::eval("<str>", "(2 - 1)").get_num(), Some(1.0));
	}

	#[test]
	fn num_binary() {
		assert_eq!(crate::eval("<str>", "(0b10)").get_num(), Some(2.0));
		assert_eq!(crate::eval("<str>", "(0b10.1)").get_num(), Some(2.5));
		assert_eq!(parse("<str>", "(0b10.1e6)".chars()), Err(ParseError{
			typ: ErrorType::Unexpected(
				Loc{line: 1, col: 8}, Token::Unexpected('e'), &[]),
			msg: "parsing atom",
		}));
		assert_eq!(crate::eval("<str>", "(0b1M)").get_num(), Some(1_000_000.0));
		assert_eq!(crate::eval("<str>", "(0b1u)").get_num(), Some(0.000_001));
	}

	#[test]
	fn num_hex() {
		assert_eq!(crate::eval("<str>", "(0x10)").get_num(), Some(16.0));
		assert_eq!(crate::eval("<str>", "(0x8.8)").get_num(), Some(8.5));
		assert_eq!(crate::eval("<str>", "(0x00.1)").get_num(), Some(0.0625));
	}
}
