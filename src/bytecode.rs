use byteorder::{self, ByteOrder, ReadBytesExt};
use std;
use std::io::{BufRead};
use std::rc::Rc;

const MAGIC: &[u8] = b"ECL\0v001";
const START_OFFSET: usize = 8;
const DEBUG_OFFSET: usize = 16;

macro_rules! codes {
	( $type:ident $( $item:ident, )* ) => {
		codes!{$type: u8 $( $item, )*}
	};
	( $type:ident : $repr:ident $( $item:ident, )* ) => {
		#[derive(Clone,Copy,Debug)]
		enum $type { $( $item ),* }

		impl $type {
			#[inline(always)]
			fn from(i: $repr) -> Result<Self,crate::Val> {
				$( if i == $type::$item as $repr {
					Ok($type::$item)
				} else )* {
					panic!("Unknown {} 0x{:02x}", stringify!($type), i)
				}
			}

			#[inline]
			fn to(self) -> $repr {
				self as $repr
			}
		}
	};
}

codes!{Op
	Global,
	Add,
	Call,
	Ge,
	Gt,
	Eq,
	Lt,
	Le,
	ADict,
	Dict,
	Func,
	Index,
	Interpolate,
	List,
	Ne,
	Neg,
	Num,
	Ref,
	RefRel,
	Str,
	Sub,
}

codes!{DictItem
	Assert,
	Pub,
	Local,
}

codes!{ArgType
	One,
	Dict,
	List,
}

codes!{ArgReq
	Required,
	Optional,
}

type EclByteOrder = byteorder::LittleEndian;

struct Scope {
	vars: Vec<(String, usize)>,
	parent: Option<Rc<Scope>>,
}

impl Scope {
	fn find(&self, name: &str) -> Option<(usize,usize)> {
		self.vars.iter()
			.find(|i| i.0 == name)
			.map(|i| (0, i.1))
			.or_else(||
				self.parent.as_ref()
					.and_then(|p| p.find(name))
					.map(|(d, id)| (d+1, id)))
	}

	fn find_at(&self, name: &str, ups: usize) -> Option<usize> {
		if ups > 0 {
			return self.parent.as_ref().and_then(|p| p.find_at(name, ups-1))
		}

		self.vars.iter()
			.find(|i| i.0 == name)
			.map(|i| i.1)
	}
}

enum Compilable {
	Almost(crate::Almost),
	Func(Box<crate::func::FuncData>),
}

struct ToCompile {
	ref_off: usize,
	item: Compilable,
	scope: Rc<Scope>,
}

impl ToCompile {
	fn compile(self, ctx: &mut CompileContext) -> Result<usize, crate::Val> {
		ctx.scope = self.scope;
		match self.item {
			Compilable::Almost(ast) => ctx.compile(ast),
			Compilable::Func(data) => {
				let data = *data;
				let crate::func::FuncData{arg, body} = data;
				let off = match arg {
					crate::func::Arg::One(arg) => {
						let argoff = ctx.out.write_u8(ArgType::One.to());

						let (k, id) = ctx.new_var(arg.clone(), false);
						ctx.out.write_varint(id);

						ctx.scope = Rc::new(Scope{
							vars: vec![(k, id)],
							parent: Some(ctx.scope.clone()),
						});

						argoff
					}
					crate::func::Arg::Dict(args) => {
						let mut vars = Vec::with_capacity(args.len());

						let args = args.into_iter()
							.map(|(key, required, val)| {
								let (k, id) = ctx.new_var(key.clone(), true);
								vars.push((k, id));
								(key, required, val)
							})
							.collect::<Vec<_>>();

						ctx.scope = Rc::new(Scope{
							vars,
							parent: Some(ctx.scope.clone()),
						});

						let args = args.into_iter().map(|(key, required, val)| {
								if required {
									(key, Ok(0))
								} else {
									(key, ctx.compile(val))
								}
							})
							.collect::<Vec<_>>();

						let argoff = ctx.out.write_u8(ArgType::Dict.to());
						ctx.out.write_usize(args.len());
						for (key, off) in args {
							let off = off?;

							ctx.out.write_str(&key);
							if off == 0 {
								ctx.out.write_u8(ArgReq::Required.to());
							} else {
								ctx.out.write_u8(ArgReq::Optional.to());
								ctx.out.write_usize(off);
							}
						}

						argoff
					}
					crate::func::Arg::List(args) => {
						let mut vars = Vec::with_capacity(args.len());

						let args = args.into_iter()
							.map(|(key, required, val)| {
								let (k, id) = ctx.new_var(key, false);
								vars.push((k, id));
								(id, required, val)
							})
							.collect::<Vec<_>>();

						ctx.scope = Rc::new(Scope{
							vars,
							parent: Some(ctx.scope.clone()),
						});

						let args = args.into_iter().map(|(id, required, val)| {
								if required {
									(id, Ok(0))
								} else {
									(id, ctx.compile(val))
								}
							})
							.collect::<Vec<_>>();

						let argoff = ctx.out.write_u8(ArgType::List.to());
						ctx.out.write_varint(args.len());
						for (id, off) in args {
							let off = off?;

							ctx.out.write_varint(id);
							if off == 0 {
								ctx.out.write_u8(ArgReq::Required.to());
							} else {
								ctx.out.write_u8(ArgReq::Optional.to());
								ctx.out.write_usize(off);
							}
						}

						argoff
					}
				};

				ctx.compile(body)?;
				Ok(off)
			}
		}
	}
}

struct Buffer {
	data: Vec<u8>,
}

impl Buffer {
	fn new() -> Self {
		Buffer {
			data: Vec::new(),
		}
	}

	fn len(&self) -> usize {
		self.data.len()
	}

	fn write<T: IntoIterator<Item=u8>>(&mut self, bytes: T) -> usize {
		let start_offset = self.data.len();
		self.data.extend(bytes);
		start_offset
	}

	fn write_u8(&mut self, byte: u8) -> usize {
		self.write(Some(byte))
	}

	fn write_u16(&mut self, n: u16) -> usize {
		let mut buf = [0; 2];
		EclByteOrder::write_u16(&mut buf, n);
		self.write(buf.into_iter().cloned())
	}

	fn write_u32(&mut self, n: u32) -> usize {
		let mut buf = [0; 4];
		EclByteOrder::write_u32(&mut buf, n);
		self.write(buf.into_iter().cloned())
	}

	fn write_u64(&mut self, n: u64) -> usize {
		let mut buf = [0; 8];
		EclByteOrder::write_u64(&mut buf, n);
		self.write(buf.into_iter().cloned())
	}

	fn write_usize(&mut self, n: usize) -> usize {
		self.write_u64(std::convert::TryFrom::try_from(n).unwrap())
	}

	fn write_varint(&mut self, n: usize) -> usize {
		let n: u64 = std::convert::TryFrom::try_from(n).unwrap();
		if n < 1 << 7 {
			self.write_u8((n as u8) << 1 | 0b1)
		} else if n < 1 << 14 {
			self.write_u16((n as u16) << 2 | 0b10)
		} else if n < 1 << 29 {
			self.write_u32((n as u32) << 3 | 0b100)
		} else {
			let r = self.write_u8(0);
			self.write_u64(n);
			r
		}
	}

	fn write_f64(&mut self, f: f64) -> usize {
		let mut buf = [0; 8];
		EclByteOrder::write_f64(&mut buf, f);
		self.write(buf.into_iter().cloned())
	}

	fn write_str(&mut self, s: &str) -> usize {
		let off = self.write_varint(s.len());
		self.write(s.as_bytes().iter().cloned());
		off
	}

	fn reserve_reference(&mut self) -> usize {
		return self.write_u64(0)
	}

	fn write_reference(&mut self, ref_off: usize, code_off: usize) {
		let code_off = std::convert::TryFrom::try_from(code_off).unwrap();
		EclByteOrder::write_u64(&mut self.data[ref_off..][..8], code_off);
	}

	fn to_bytes(self) -> Vec<u8> {
		self.data
	}
}

struct CompileContext {
	out: Buffer,
	last_local: usize,
	compile_queue: Vec<ToCompile>,
	scope: Rc<Scope>,

	debug: Buffer,
	debug_last: usize,
}

impl CompileContext {
	fn new_var(&mut self, name: String, public: bool) -> (String, usize) {
		let id = if public {
			0
		} else {
			self.last_local += 1;
			self.last_local
		};

		(name, id)
	}

	fn compile_outofline(&mut self, scope: Rc<Scope>, node: crate::Almost) -> usize {
		let ref_off = self.out.reserve_reference();
		self.compile_queue.push(ToCompile{
			ref_off,
			item: Compilable::Almost(node),
			scope,
		});
		return ref_off
	}

	fn compile(&mut self, ast: crate::Almost) -> Result<usize,crate::Val> {
		self.compile_expr(ast)
	}

	fn compile_expr(&mut self, ast: crate::Almost) -> Result<usize, crate::Val> {
		match ast {
			crate::Almost::Add(loc, left, right) => {
				self.compile_binary_op(loc, Op::Add, *left, *right)
			}
			crate::Almost::Call(loc, left, right) => {
				self.compile_binary_op(loc, Op::Call, *left, *right)
			}
			crate::Almost::GreatEq(loc, left, right) => {
				self.compile_binary_op(loc, Op::Ge, *left, *right)
			}
			crate::Almost::Great(loc, left, right) => {
				self.compile_binary_op(loc, Op::Gt, *left, *right)
			}
			crate::Almost::Eq(loc, left, right) => {
				self.compile_binary_op(loc, Op::Eq, *left, *right)
			}
			crate::Almost::Less(loc, left, right) => {
				self.compile_binary_op(loc, Op::Lt, *left, *right)
			}
			crate::Almost::LessEq(loc, left, right) => {
				self.compile_binary_op(loc, Op::Le, *left, *right)
			}
			crate::Almost::ADict(key, element) => {
				let off = self.write_op(Op::ADict);
				let scope = self.scope.clone();
				self.compile_outofline(scope, *element);
				self.out.write_str(&key);
				Ok(off)
			}
			crate::Almost::Dict(elements) => {
				let mut vars = Vec::with_capacity(elements.len());

				for e in &elements {
					if e.is_element() {
						vars.push(self.new_var(e.key.clone(), e.is_public()));
					}
				}

				let scope = Rc::new(Scope{
					vars,
					parent: Some(self.scope.clone()),
				});

				let off = self.write_op(Op::Dict);
				self.out.write_varint(elements.len());
				for e in elements {
					self.write_debug(e.loc);
					match e.visibility {
						crate::dict::Visibility::Assert => {
							self.out.write_u8(DictItem::Assert.to());
						}
						crate::dict::Visibility::Local => {
							self.out.write_u8(DictItem::Local.to());
							self.out.write_varint(scope.find_at(&e.key, 0).unwrap());
						},
						crate::dict::Visibility::Pub => {
							self.out.write_u8(DictItem::Pub.to());
							self.out.write_str(&e.key);
						},
					}
					self.compile_outofline(scope.clone(), e.val);
				}

				Ok(off)
			}
			crate::Almost::Func(data) => {
				let off = self.write_op(Op::Func);
				let refoff = self.out.reserve_reference();
				self.compile_queue.push(ToCompile{
					ref_off: refoff,
					item: Compilable::Func(data),
					scope: self.scope.clone(),
				});
				Ok(off)
			}
			crate::Almost::Index(loc, left, right) => {
				self.compile_binary_op(loc, Op::Index, *left, *right)
			}
			crate::Almost::List(elements) => {
				let off = self.write_op(Op::List);
				self.out.write_varint(elements.len());
				for element in elements {
					let scope = self.scope.clone();
					self.compile_outofline(scope, element);
				}
				Ok(off)
			},
			crate::Almost::Ne(loc, left, right) => {
				self.compile_binary_op(loc, Op::Ne, *left, *right)
			}
			crate::Almost::Neg(_, v) => {
				let off = self.write_op(Op::Neg);
				self.compile_expr(*v)?;
				Ok(off)
			}
			crate::Almost::Nil => self.compile_global(0),
			crate::Almost::Num(n) => {
				let off = self.write_op(Op::Num);
				self.out.write_f64(n);
				Ok(off)
			}
			crate::Almost::Ref(loc, name) => {
				self.write_debug(loc);
				if let Some((depth, id)) = self.scope.find(&name) {
					let off = self.write_op(Op::Ref);
					self.out.write_str(&name);
					self.out.write_varint(depth);
					self.out.write_varint(id);
					Ok(off)
				} else if let Some(id) = crate::builtins::builtin_id(&name) {
					let off = self.write_op(Op::Global);
					self.out.write_varint(id);
					Ok(off)
				} else {
					Err(crate::err::Err::new(format!("{:?} Invalid reference {:?}", loc, name)))
				}
			}
			crate::Almost::StructRef(loc, depth, key) => {
				self.write_debug(loc);
				if let Some(id) = self.scope.find_at(&key, depth) {
					let off = self.write_op(Op::Ref);
					self.out.write_str(&key);
					self.out.write_varint(depth);
					self.out.write_varint(id);
					Ok(off)
				} else {
					let off = self.write_op(Op::RefRel);
					self.out.write_str(&key);
					self.out.write_varint(depth);
					Ok(off)
				}
			}
			crate::Almost::Str(parts) => {
				let off = self.write_op(Op::Interpolate);
				self.out.write_varint(parts.len());
				for part in parts {
					match part {
						crate::StringPart::Exp(s) => {
							self.compile_expr(s)?;
						},
						crate::StringPart::Lit(s) => {
							self.write_op(Op::Str);
							self.out.write_str(&s);
						},
					}
				}
				Ok(off)
			}
			crate::Almost::StrStatic(s) => {
				let off = self.write_op(Op::Str);
				self.out.write_str(&s);
				Ok(off)
			}
			crate::Almost::Sub(loc, left, right) => {
				self.compile_binary_op(loc, Op::Sub, *left, *right)
			}
		}
	}

	fn compile_binary_op(&mut self,
		loc: crate::grammar::Loc,
		op: Op,
		left: crate::Almost,
		right: crate::Almost,
	) -> Result<usize,crate::Val> {
		self.write_debug(loc);
		let off = self.write_op(op);
		self.compile_expr(left)?;
		self.compile_expr(right)?;
		Ok(off)
	}

	fn compile_global(&mut self, global: usize) -> Result<usize,crate::Val> {
		let off = self.write_op(Op::Global);
		self.out.write_varint(global);
		Ok(off)
	}

	fn write_op(&mut self, op: Op) -> usize {
		self.out.write_u8(op.to())
	}

	fn write_debug(&mut self, start: crate::grammar::Loc) {
		let new_bytes = self.out.len() - self.debug_last;
		self.debug_last = self.out.len();

		self.debug.write_varint(new_bytes);

		self.debug.write_varint(start.line);
		self.debug.write_varint(start.col);

		// self.write_varint(end.line - start.line);
		// self.write_varint(end.col);
	}
}

pub fn compile_to_vec(ast: crate::Almost) -> Result<Vec<u8>,crate::Val> {
	let mut ctx = CompileContext{
		out: Buffer::new(),
		last_local: 0,
		compile_queue: Vec::new(),
		scope: Rc::new(Scope{
			vars: Vec::new(),
			parent: None,
		}),

		debug: Buffer::new(),
		debug_last: 0,
	};

	ctx.out.write(MAGIC.iter().cloned());

	ctx.compile_outofline(ctx.scope.clone(), ast);

	let debug_ref = ctx.out.reserve_reference();

	while let Some(compilable) = ctx.compile_queue.pop() {
		let ref_off = compilable.ref_off;
		let code_off = compilable.compile(&mut ctx)?;
		ctx.out.write_reference(ref_off, code_off);
	}

	let debug_off = ctx.out.write(ctx.debug.to_bytes());
	ctx.out.write_varint(ctx.out.len()); // Sentinel after last record.
	ctx.out.write_reference(debug_ref, debug_off);

	Ok(ctx.out.to_bytes())
}

#[derive(Clone,Eq,Ord,PartialEq,PartialOrd)]
pub struct Module {
	pub file: std::path::PathBuf,
	pub code: Vec<u8>,
}

impl Module {
	fn unique_id(&self) -> usize {
		self.code.as_ptr() as usize
	}

	fn start_pc(&self) -> u64 {
		self.read_u64(START_OFFSET)
	}

	fn start_debug(&self) -> u64 {
		self.read_u64(DEBUG_OFFSET)
	}

	fn read_u64(&self, i: usize) -> u64 {
		EclByteOrder::read_u64(&self.code[i..(i+8)])
	}

	pub fn loc(&self, off: usize) -> crate::grammar::Loc {
		let debug_off = self.start_debug() as usize;
		let mut cursor = std::io::Cursor::new(&self.code[debug_off..]);

		let mut rec_off = 0;
		let loc = loop {
			rec_off += cursor.read_varint();

			if rec_off > off {
				break crate::grammar::Loc { line: 0, col: 0, };
			}

			let loc = crate::grammar::Loc {
				line: cursor.read_varint(),
				col: cursor.read_varint(),
			};
			if rec_off == off {
				break loc;
			}
		};

		loc
	}
}

trait CursorExt<'a> {
	fn pos(&self) -> usize;
	fn read_usize(&mut self) -> usize;
	fn read_varint(&mut self) -> usize;
	fn read_ref(&mut self) -> usize;
	fn read_str(&mut self) -> &'a str;
}

impl<'a> CursorExt<'a> for std::io::Cursor<&'a [u8]> {
	fn pos(&self) -> usize {
		self.position() as usize
	}

	#[inline]
	fn read_usize(&mut self) -> usize {
		let n = self.read_u64::<EclByteOrder>().unwrap();
		std::convert::TryFrom::try_from(n).unwrap()
	}

	fn read_varint(&mut self) -> usize {
		let off = self.position() as usize;

		let (bytes, val) = {
			let buf = &self.get_ref()[off..];
			match buf[0].trailing_zeros() {
				0 => (1, (buf[0] >> 1) as u64),
				1 => (2, (EclByteOrder::read_u16(&buf) >> 2) as u64),
				2 => (4, (EclByteOrder::read_u32(&buf) >> 3) as u64),
				8 => (9, EclByteOrder::read_u64(&buf[1..])),
				b => unreachable!("Bits: {}", b),
			}
		};

		self.set_position(off as u64 + bytes);
		std::convert::TryFrom::try_from(val).unwrap()
	}

	fn read_str(&mut self) -> &'a str {
		let len = self.read_varint();
		let off = std::convert::TryInto::try_into(self.position()).unwrap();
		self.consume(len);
		let bytes = &self.get_ref()[off..][..len];
		std::str::from_utf8(bytes).unwrap()
	}

	fn read_ref(&mut self) -> usize {
		std::convert::TryInto::try_into(self.read_u64::<EclByteOrder>().unwrap()).unwrap()
	}
}

impl std::fmt::Debug for Module {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "Module({:?})", self.file)
	}
}

struct EvalContext<'a> {
	module: &'a Rc<Module>,
	cursor: std::io::Cursor<&'a [u8]>,
	parent: Rc<crate::Parent>,
}

impl<'a> EvalContext<'a> {
	fn eval(mut self) -> crate::Val {
		self.continue_eval()
	}

	fn continue_eval(&mut self) -> crate::Val {
		let off = self.cursor.pos();
		let op = Op::from(self.cursor.read_u8().unwrap())?;
		// eprintln!("Executing OP {:?} @ {}", op, off);
		match op {
			Op::Global => {
				let id = self.cursor.read_varint();
				crate::builtins::get_id(id)
			}
			Op::Add => self.eval_binop(off, "addition", crate::Val::add),
			Op::Call => self.eval_binop(off, "call", crate::Val::call),
			Op::Ge => {
				self.eval_cmp(off, ">=", |o| o != std::cmp::Ordering::Less)
			}
			Op::Gt => {
				self.eval_cmp(off, ">", |o| o == std::cmp::Ordering::Greater)
			}
			Op::Eq => self.eval_binop(off, "==", crate::Val::eq),
			Op::Lt => {
				self.eval_cmp(off, "<", |o| o == std::cmp::Ordering::Less)
			}
			Op::Le => {
				self.eval_cmp(off, "<=", |o| o != std::cmp::Ordering::Greater)
			}
			Op::ADict => {
				let childoff = self.cursor.read_u64::<EclByteOrder>().unwrap();
				let key = self.cursor.read_str();
				crate::dict::Dict::new_adict(
					self.parent.clone(),
					key.to_owned(),
					Value::new(self.module.clone(), childoff))
			}
			Op::Dict => {
				let entries_len = self.cursor.read_varint();
				let mut entries = Vec::with_capacity(entries_len);
				for _ in 0..entries_len {
					let pos = self.cursor.pos();
					let source = match DictItem::from(self.cursor.read_u8().unwrap())? {
						DictItem::Assert => {
							let offset = self.cursor.read_u64::<EclByteOrder>().unwrap();
							crate::dict::Source::Assert {
								debug: pos,
								almost: Value::new(self.module.clone(), offset),
							}
						}
						DictItem::Pub => {
							let key = crate::dict::Key::Pub(self.cursor.read_str().to_owned());

							let offset = self.cursor.read_u64::<EclByteOrder>().unwrap();
							crate::dict::Source::Entry {
								key,
								almost: Value::new(self.module.clone(), offset),
							}
						}
						DictItem::Local => {
							let mut id = self.cursor.read_varint();
							id += self.module.unique_id();
							let key = crate::dict::Key::Local(id);

							let offset = self.cursor.read_u64::<EclByteOrder>().unwrap();
							crate::dict::Source::Entry {
								key,
								almost: Value::new(self.module.clone(), offset),
							}
						},
					};
					entries.push(source);
				}

				crate::dict::Dict::new(self.parent.clone(), entries)
			}
			Op::Func => {
				let bodyoff = self.cursor.read_ref();
				crate::func::Func::new(
					self.parent.clone(),
					Func::new(self.module.clone(), bodyoff))
			}
			Op::Index => self.eval_binop(off, "index", crate::Val::index),
			Op::Interpolate => {
				let mut buf = String::new();

				let chunks = self.cursor.read_varint();
				for _ in 0..chunks {
					let b = self.continue_eval();
					let b = b.to_string();
					let b = b.get_str().unwrap();
					buf += b;
				}

				crate::Val::new_atomic(buf)
			}
			Op::List => {
				let len = self.cursor.read_varint();
				let mut items = Vec::with_capacity(len);
				for _ in 0..len {
					let offset = self.cursor.read_u64::<EclByteOrder>().unwrap();
					items.push(Value::new(self.module.clone(), offset));
				}
				crate::list::List::new(self.parent.clone(), items)
			}
			Op::Ne => self.eval_binop(off, "!=", crate::Val::ne),
			Op::Neg => {
				self.continue_eval().neg()
			}
			Op::Num => {
				let num = self.cursor.read_f64::<EclByteOrder>().unwrap();
				// eprintln!("Num: {}", num);
				crate::num::get(num)
			}
			Op::Ref => {
				let strkey = self.cursor.read_str();
				let depth = self.cursor.read_varint();
				let mut id = self.cursor.read_varint();
				let key = if id == 0 {
					crate::dict::Key::Pub(strkey.to_owned())
				} else {
					id += self.module.unique_id();
					crate::dict::Key::Local(id)
				};
				// eprintln!("Ref: {:?}@{} {:?}", key, depth, strkey);
				self.parent.structural_lookup(depth, &key)
					.annotate_at_with(|| (
						self.module.loc(off),
						format!("Referenced by {:?}", strkey),
					))
			}
			Op::RefRel => {
				let key = self.cursor.read_str();
				let depth = self.cursor.read_varint();
				let key = crate::dict::Key::Pub(key.to_owned());
				self.parent.structural_lookup(depth, &key)
					.annotate_at_with(|| (
						self.module.loc(off),
						format!("referenced by {:.>2$}{}", "", key, depth+1),
					))
			}
			Op::Str => {
				let s = crate::str::CodeString {
					module: self.module.clone(),
					len: self.cursor.read_varint(),
					offset: self.cursor.pos(),
				};
				self.cursor.consume(s.len);
				// eprintln!("  {:?}", s);
				crate::Val::new_atomic(s)
			}
			Op::Sub => self.eval_binop(off, "subtraction", crate::Val::subtract),
		}
	}

	fn eval_binop(&mut self,
		off: usize,
		desc: &str,
		f: impl FnOnce(&crate::Val, crate::Val) -> crate::Val,
	) -> crate::Val {
		let left = self.continue_eval()
			.annotate_at_with(|| (self.module.loc(off), format!("On left side of {}", desc)))?;
		let right = self.continue_eval()
			.annotate_at_with(|| (self.module.loc(off), format!("On right side of {}", desc)))?;
		f(&left, right)
	}

	fn assert_binop(&mut self,
		assert_loc: crate::grammar::Loc,
		off: u64,
		desc: &str,
		f: impl FnOnce(&crate::Val, crate::Val) -> crate::Val,
	) -> Result<(), crate::Val> {
		let loc = self.module.loc(off as usize);
		let left = self.continue_eval()
			.annotate_at_with(|| (loc, format!("On left side of {}", desc)))?;
		let right = self.continue_eval()
			.annotate_at_with(|| (loc, format!("On right side of {}", desc)))?;
		let val = f(&left, right.clone())
			.to_bool()?;
		if !val.get_bool().unwrap() {
			Err(crate::err::Err::new_at(assert_loc,
				format!("Assertion failed left {} right:\nleft:  {:?}\nright: {:?}", desc, left, right)))
		} else {
			Ok(())
		}
	}

	fn eval_cmp(&mut self,
		off: usize,
		desc: &str,
		f: impl FnOnce(std::cmp::Ordering) -> bool,
	) -> crate::Val {
		let left = self.continue_eval()
			.annotate_at_with(|| (self.module.loc(off), format!("On left side of {}", desc)))?;
		let right = self.continue_eval()
			.annotate_at_with(|| (self.module.loc(off), format!("On right side of {}", desc)))?;

		let ord = left.cmp(right)
			.map_err(|e| e.annotate_at_with(|| (self.module.loc(off), format!("At {}", desc))))?;

		crate::bool::get(f(ord))
	}

	fn assert_cmp(&mut self,
		assert_loc: crate::grammar::Loc,
		off: u64,
		desc: &str,
		f: impl FnOnce(std::cmp::Ordering) -> bool,
	) -> Result<(), crate::Val> {
		let loc = self.module.loc(off as usize);
		let left = self.continue_eval()
			.annotate_at_with(|| (loc, format!("On left side of {}", desc)))?;
		let right = self.continue_eval()
			.annotate_at_with(|| (loc, format!("On right side of {}", desc)))?;

		let ord = left.cmp(right.clone())
			.map_err(|e| e.annotate_at_with(|| (loc, format!("At {}", desc))))?;

		if !f(ord) {
			Err(crate::err::Err::new_at(assert_loc,
				format!("Assertion failed left {} right:\nleft:  {:?}\nright: {:?}", desc, left, right)))
		} else {
			Ok(())
		}
	}
}

pub fn eval(code: Vec<u8>) -> crate::Val {
	let module = Rc::new(Module{
		file: "myfile".into(),
		code: code,
	});

	let pc = module.start_pc();
	let parent = Rc::new(crate::Parent {
		parent: crate::nil::get().value,
		grandparent: None,
	});
	Value::new(module, pc).eval(parent)
}

#[derive(Clone,Debug,Eq,PartialEq)]
pub struct Value {
	pub module: Rc<Module>,
	offset: u64,
}

impl Value {
	fn new(module: Rc<Module>, offset: u64) -> Self {
		Value{module, offset}
	}

	pub fn eval(&self, parent: Rc<crate::Parent>) -> crate::Val {
		let mut cursor = std::io::Cursor::new(&self.module.code[..]);
		cursor.set_position(self.offset);
		EvalContext { module: &self.module, cursor, parent }.eval()
	}

	pub fn eval_assert(&self, parent: Rc<crate::Parent>, debug: usize) -> Result<(), crate::Val> {
		let loc = self.module.loc(debug);

		let mut cursor = std::io::Cursor::new(&self.module.code[..]);
		cursor.set_position(self.offset);
		let op = Op::from(cursor.read_u8().unwrap())?;
		let mut context = EvalContext { module: &self.module, cursor, parent };

		match op {
			Op::Ge => {
				context.assert_cmp(loc, self.offset, ">=", |o| o != std::cmp::Ordering::Less)
			}
			Op::Gt => {
				context.assert_cmp(loc, self.offset, ">", |o| o == std::cmp::Ordering::Greater)
			}
			Op::Eq => context.assert_binop(loc, self.offset, "==", crate::Val::eq),
			Op::Lt => {
				context.assert_cmp(loc, self.offset, "<", |o| o == std::cmp::Ordering::Less)
			}
			Op::Le => {
				context.assert_cmp(loc, self.offset, "<=", |o| o != std::cmp::Ordering::Greater)
			}
			Op::Ne => context.assert_binop(loc, self.offset, "!=", crate::Val::ne),
			_ => {
				context.cursor.set_position(self.offset as u64);
				let val = context.eval();
				if val.is_nil() {
					return Err(crate::err::Err::new_at(loc, "Assertion nil".into()))
				}

				let val = val
					.to_bool()
					.annotate_at(loc, "Evaluating assertion condition")?;

				if !val.get_bool().unwrap() {
					Err(crate::err::Err::new_at(loc, "Assertion failed".into()))
				} else {
					Ok(())
				}
			}
		}
	}

	pub fn loc(&self) -> crate::grammar::Loc {
		self.module.loc(self.offset as usize)
	}
}

#[derive(Clone,Debug)]
pub struct Func {
	module: std::rc::Rc<Module>,
	offset: usize,
}

impl Func {
	fn new(module: Rc<Module>, offset: usize) -> Self {
		Func{module, offset}
	}

	pub fn call(&self, parent: Rc<crate::Parent>, arg: crate::Val) -> crate::Val {
		// eprintln!("Executing {:?}", self);

		let mut cursor = std::io::Cursor::new(&self.module.code[..]);
		cursor.set_position(self.offset as u64);

		let args = crate::dict::Dict::new(parent.clone(), Vec::new());
		let dict = args.clone();
		let pool = dict.pool.clone();
		let dict = dict.downcast_ref::<crate::dict::Dict>().unwrap();

		let parent = Rc::new(crate::Parent {
			parent: args.value.clone(),
			grandparent: Some(parent),
		});

		match ArgType::from(cursor.read_u8().unwrap())? {
			ArgType::One => {
				let mut id = cursor.read_varint();
				id += self.module.unique_id();
				pool.merge(arg.pool.clone());
				dict._set_val(crate::dict::Key::Local(id), crate::thunk::shim(arg));
			}
			ArgType::Dict => {
				use crate::Value;

				let sourcedict = match arg.downcast_ref::<crate::dict::Dict>() {
					Ok(d) => d,
					Err(_) => return crate::err::Err::new(format!(
						"Function must be called with dict, called with {:?}",
						arg)),
				};
				let mut unused_args = sourcedict.len().get_num().unwrap() as usize;

				let len = cursor.read_usize();
				for _ in 0..len {
					let key = cursor.read_str();
					let passed = sourcedict.index(&crate::dict::Key::Pub(key.to_owned()))
						.map(|v| v.annotate("Looking up argument value"));
					let val = match ArgReq::from(cursor.read_u8().unwrap())? {
						ArgReq::Required => match passed {
							Some(val) => {
								unused_args -= 1;
								pool.merge(val.pool.clone());
								crate::thunk::shim(val)
							}
							None => return crate::err::Err::new(format!(
								"Required argument {:?} not set in {:?}",
								key, sourcedict)),
						}
						ArgReq::Optional => {
							let off = cursor.read_u64::<EclByteOrder>().unwrap();
							match passed {
								Some(v) => {
									unused_args -= 1;
									crate::thunk::shim(v)
								}
								None => {
									let value = self::Value::new(self.module.clone(), off);
									crate::thunk::bytecode(parent.clone(), value)
								}
							}
						}
					};
					dict._set_val(crate::dict::Key::Pub(key.to_owned()), val)
				}

				if unused_args != 0 {
					return crate::err::Err::new(format!(
						"Function called with {} unused arguments.", unused_args));
				}
			}
			ArgType::List => {
				use crate::Value;

				let list = match arg.downcast_ref::<crate::list::List>() {
					Ok(l) => l,
					Err(_) => return crate::err::Err::new(format!(
						"Function must be called with list, called with {:?}",
						arg)),
				};
				let arg_len = list.len().get_num().unwrap() as usize;

				let len = cursor.read_varint();
				if arg_len > len {
					return crate::err::Err::new(format!(
						"Function called with too many arguments, expected {} got {}",
						len, arg_len))
				}

				for i in 0..len {
					let mut id = cursor.read_varint();
					id += self.module.unique_id();

					let passed = list.get(i);
					let val = match ArgReq::from(cursor.read_u8().unwrap())? {
						ArgReq::Required => match passed {
							Some(val) => {
								pool.merge(val.pool.clone());
								crate::thunk::shim(val)
							}
							None => return crate::err::Err::new(format!(
								"Required argument {} not provided.", i)),
						}
						ArgReq::Optional => {
							let off = cursor.read_u64::<EclByteOrder>().unwrap();
							passed
								.map(crate::thunk::shim)
								.unwrap_or_else(|| {
									let value = self::Value::new(self.module.clone(), off);
									crate::thunk::bytecode(parent.clone(), value)
								})
						}
					};
					dict._set_val(crate::dict::Key::Local(id), val)
				}
			}
		};

		let r = EvalContext { module: &self.module, cursor, parent }.eval();
		pool.merge(r.pool.clone());
		r
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn compile_global() {
		assert_eq!(
			compile_to_vec(crate::Almost::Nil).unwrap(),
			b"ECL\0v001\x18\0\0\0\0\0\0\0\x1A\0\0\0\0\0\0\0\x00\x01\x35".to_vec());
	}
}
