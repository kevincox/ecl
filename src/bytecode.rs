use byteorder::{self, ByteOrder, ReadBytesExt};
use std;
use std::io::{BufRead};
use std::rc::Rc;

const MAGIC: &[u8] = b"ECL\0v001";
const START_OFFSET: usize = 8;

macro_rules! codes {
	( $type:ident $( $item:ident, )* ) => {
		codes!{$type: u8 $( $item, )*}
	};
	( $type:ident : $repr:ident $( $item:ident, )* ) => {
		#[derive(Clone,Copy,Debug)]
		enum $type { $( $item ),* }

		impl $type {
			#[inline(always)]
			fn from(i: $repr) -> Result<Self,::Val> {
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
	Ret,
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
	Neg,
	Num,
	Ref,
	RefRel,
	Str,
	Sub,
}

codes!{DictItem
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
	Almost(::Almost),
	Func(Box<::func::FuncData>),
}

struct ToCompile {
	ref_off: usize,
	item: Compilable,
	scope: Rc<Scope>,
}

impl ToCompile {
	fn compile(self, ctx: &mut CompileContext) -> Result<usize, ::Val> {
		ctx.scope = self.scope;
		match self.item {
			Compilable::Almost(ast) => ctx.compile(ast),
			Compilable::Func(data) => {
				let data = *data;
				let ::func::FuncData{arg, body} = data;
				let off = match arg {
					::func::Arg::One(arg) => {
						let argoff = ctx.write_u8(ArgType::One.to());

						let (k, id) = ctx.new_var(arg.clone(), false);
						ctx.write_varint(id);

						ctx.scope = Rc::new(Scope{
							vars: vec![(k, id)],
							parent: Some(ctx.scope.clone()),
						});

						argoff
					}
					::func::Arg::Dict(args) => {
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

						let argoff = ctx.write_u8(ArgType::Dict.to());
						ctx.write_usize(args.len());
						for (key, off) in args {
							let off = off?;

							ctx.write_str(&key);
							if off == 0 {
								ctx.write_u8(ArgReq::Required.to());
							} else {
								ctx.write_u8(ArgReq::Optional.to());
								ctx.write_usize(off);
							}
						}

						argoff
					}
					::func::Arg::List(args) => {
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

						let argoff = ctx.write_u8(ArgType::List.to());
						ctx.write_varint(args.len());
						for (id, off) in args {
							let off = off?;

							ctx.write_varint(id);
							if off == 0 {
								ctx.write_u8(ArgReq::Required.to());
							} else {
								ctx.write_u8(ArgReq::Optional.to());
								ctx.write_usize(off);
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

struct CompileContext {
	bytes: Vec<u8>,
	last_local: usize,
	compile_queue: Vec<ToCompile>,
	scope: Rc<Scope>,
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

	fn write<T: IntoIterator<Item=u8>>(&mut self, bytes: T) -> usize {
		let start_offset = self.bytes.len();
		self.bytes.extend(bytes);
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

	fn write_op(&mut self, op: Op) -> usize {
		self.write_u8(op.to())
	}

	fn write_str(&mut self, s: &str) -> usize {
		let off = self.write_varint(s.len());
		self.write(s.bytes());
		off
	}

	fn reserve_reference(&mut self) -> usize {
		return self.write_u64(0)
	}

	fn compile_outofline(&mut self, scope: Rc<Scope>, node: ::Almost) -> usize {
		let ref_off = self.reserve_reference();
		self.compile_queue.push(ToCompile{
			ref_off,
			item: Compilable::Almost(node),
			scope,
		});
		return ref_off
	}

	fn compile(&mut self, ast: ::Almost) -> Result<usize,::Val> {
		let off = self.compile_expr(ast)?;
		self.write_op(Op::Ret);
		Ok(off)
	}

	fn compile_expr(&mut self, ast: ::Almost) -> Result<usize,::Val> {
		match ast {
			::Almost::Add(_, left, right) => {
				self.compile_binary_op(Op::Add, *left, *right)
			}
			::Almost::Call(_, left, right) => {
				self.compile_binary_op(Op::Call, *left, *right)
			}
			::Almost::GreatEq(left, right) => {
				self.compile_binary_op(Op::Ge, *left, *right)
			}
			::Almost::Great(left, right) => {
				self.compile_binary_op(Op::Gt, *left, *right)
			}
			::Almost::Eq(left, right) => {
				self.compile_binary_op(Op::Eq, *left, *right)
			}
			::Almost::Less(left, right) => {
				self.compile_binary_op(Op::Lt, *left, *right)
			}
			::Almost::LessEq(left, right) => {
				self.compile_binary_op(Op::Le, *left, *right)
			}
			::Almost::ADict(key, element) => {
				let off = self.write_op(Op::ADict);
				let scope = self.scope.clone();
				self.compile_outofline(scope, *element);
				self.write_str(&key);
				Ok(off)
			}
			::Almost::Dict(elements) => {
				let mut vars = Vec::with_capacity(elements.len());

				let elements = elements.into_iter()
					.map(|e| {
						let (k, id) = self.new_var(e.key.clone(), e.is_public());
						vars.push((k, id));
						(e.key, id, e.val)
					})
					.collect::<Vec<_>>();

				let scope = Rc::new(Scope{
					vars: vars,
					parent: Some(self.scope.clone()),
				});

				let off = self.write_op(Op::Dict);
				self.write_varint(elements.len());
				for (key, id, val) in elements {
					match id {
						0 => {
							self.write_u8(DictItem::Pub.to());
							self.write_str(&key);
						}
						id => {
							self.write_u8(DictItem::Local.to());
							self.write_varint(id);
						}
					}
					self.compile_outofline(scope.clone(), val);
				}

				Ok(off)
			}
			::Almost::Func(data) => {
				let off = self.write_op(Op::Func);
				let refoff = self.reserve_reference();
				self.compile_queue.push(ToCompile{
					ref_off: refoff,
					item: Compilable::Func(data),
					scope: self.scope.clone(),
				});
				Ok(off)
			}
			::Almost::Index(_, left, right) => {
				self.compile_binary_op(Op::Index, *left, *right)
			}
			::Almost::List(elements) => {
				let off = self.write_op(Op::List);
				self.write_varint(elements.len());
				for element in elements {
					let scope = self.scope.clone();
					self.compile_outofline(scope, element);
				}
				Ok(off)
			},
			::Almost::Neg(_, v) => {
				let off = self.compile_expr(*v)?;
				self.write_op(Op::Neg);
				Ok(off)
			}
			::Almost::Nil => self.compile_global(0),
			::Almost::Num(n) => {
				let off = self.write_op(Op::Num);
				self.write_f64(n);
				Ok(off)
			}
			::Almost::Ref(loc, name) => {
				if let Some((depth, id)) = self.scope.find(&name) {
					let off = self.write_op(Op::Ref);
					self.write_str(&name); // TODO: remove this.
					self.write_varint(depth);
					self.write_varint(id);
					Ok(off)
				} else if let Some(id) = ::builtins::builtin_id(&name) {
					let off = self.write_op(Op::Global);
					self.write_varint(id);
					Ok(off)
				} else {
					Err(::err::Err::new(format!("{:?} Invalid reference {:?}", loc, name)))
				}
			}
			::Almost::StructRef(_, depth, key) => {
				if let Some(id) = self.scope.find_at(&key, depth) {
					let off = self.write_op(Op::Ref);
					self.write_str(&key); // TODO: remove this.
					self.write_varint(depth);
					self.write_varint(id);
					Ok(off)
				} else {
					let off = self.write_op(Op::RefRel);
					self.write_str(&key); // TODO: remove this.
					self.write_varint(depth);
					Ok(off)
				}
			}
			::Almost::Str(parts) => {
				let off = self.write_op(Op::Str);
				self.write_str("");
				for part in parts {
					match part {
						::StringPart::Exp(s) => {
							self.compile_expr(s)?;
						},
						::StringPart::Lit(s) => {
							self.write_op(Op::Str);
							self.write_str(&s);
						},
					}
					self.write_op(Op::Interpolate);
				}
				Ok(off)
			}
			::Almost::StrStatic(s) => {
				let off = self.write_op(Op::Str);
				self.write_str(&s);
				Ok(off)
			}
			::Almost::Sub(_, left, right) => {
				self.compile_binary_op(Op::Sub, *left, *right)
			}
		}
	}

	fn compile_binary_op(&mut self, op: Op, left: ::Almost, right: ::Almost)
		-> Result<usize,::Val>
	{
		let off = self.compile_expr(left)?;
		self.compile_expr(right)?;
		self.write_op(op);
		Ok(off)
	}

	fn compile_global(&mut self, global: usize) -> Result<usize,::Val> {
		let off = self.write_op(Op::Global);
		self.write_varint(global);
		Ok(off)
	}
}

pub fn compile_to_vec(ast: ::Almost) -> Result<Vec<u8>,::Val> {
	let mut ctx = CompileContext{
		bytes: MAGIC.to_owned(),
		last_local: 0,
		compile_queue: Vec::new(),
		scope: Rc::new(Scope{
			vars: Vec::new(),
			parent: None,
		}),
	};

	let scope = ctx.scope.clone();
	ctx.compile_outofline(scope, ast);

	while let Some(compilable) = ctx.compile_queue.pop() {
		let ref_off = compilable.ref_off;
		let code_off = compilable.compile(&mut ctx)?;
		let code_off = std::convert::TryFrom::try_from(code_off).unwrap();
		EclByteOrder::write_u64(&mut ctx.bytes[ref_off..][..8], code_off);
	}

	Ok(ctx.bytes)
}

#[derive(PartialEq)]
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

	fn read_u64(&self, i: usize) -> u64 {
		EclByteOrder::read_u64(&self.code[i..(i+8)])
	}
}

trait CursorExt<'a> {
	fn read_usize(&mut self) -> usize;
	fn read_varint(&mut self) -> usize;
	fn read_ref(&mut self) -> usize;
	fn read_str(&mut self) -> &'a str;
}

impl<'a> CursorExt<'a> for std::io::Cursor<&'a [u8]> {
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

struct EvalContext {
	module: Rc<Module>,
	pc: u64,
	parent: Rc<::Parent>,
}

impl EvalContext {
	pub fn eval(&mut self) -> ::Val {
		// eprintln!("Executing @ {}", self.pc);
		let mut cursor = std::io::Cursor::new(&self.module.code[..]);
		cursor.set_position(self.pc as u64);

		let mut stack = Vec::new();
		let r = loop {
			let op = Op::from(cursor.read_u8().unwrap())?;
			// eprintln!("Executing OP {:?} @ {}", op, cursor.position() as usize - 1);
			match op {
				Op::Ret => {
					assert_eq!(stack.len(), 1);
					break stack.pop().unwrap()
				},
				Op::Global => {
					let id = cursor.read_varint();
					stack.push(::builtins::get_id(id));
				}
				Op::Add => {
					let right = stack.pop().expect("One item in stack for add")
						.annotate("On right side of add");
					let left = stack.pop().expect("No items in stack for add")
						.annotate("On left side of add");
					stack.push(left.add(right));
				}
				Op::Call => {
					let right = stack.pop().expect("One item in stack for call");
					let left = stack.pop().expect("No items in stack for call");
					stack.push(left.call(right));
				}
				Op::Ge => {
					let r = stack.pop().expect("One item in stack for >=");
					let l = stack.pop().expect("No items in stack for >=");
					stack.push(::bool::get(l.cmp(r)? != std::cmp::Ordering::Less))
				}
				Op::Gt => {
					let r = stack.pop().expect("One item in stack for >");
					let l = stack.pop().expect("No items in stack for >");
					stack.push(::bool::get(l.cmp(r)? == std::cmp::Ordering::Greater))
				}
				Op::Eq => {
					let right = stack.pop().expect("One item in stack for ==");
					let left = stack.pop().expect("No items in stack for ==");
					stack.push(::bool::get(left == right));
				}
				Op::Lt => {
					let r = stack.pop().expect("One item in stack for <");
					let l = stack.pop().expect("No items in stack for <");
					stack.push(::bool::get(l.cmp(r)? == std::cmp::Ordering::Less))
				}
				Op::Le => {
					let r = stack.pop().expect("One item in stack for <=");
					let l = stack.pop().expect("No items in stack for <=");
					stack.push(::bool::get(l.cmp(r)? != std::cmp::Ordering::Greater))
				}
				Op::ADict => {
					let childoff = cursor.read_u64::<EclByteOrder>().unwrap();
					let key = cursor.read_str();
					stack.push(::dict::Dict::new_adict(
						self.parent.clone(),
						key.to_owned(),
						Value::new(self.module.clone(), childoff)));
				}
				Op::Dict => {
					let len = cursor.read_varint();
					let mut items = Vec::with_capacity(len);
					for _ in 0..len {
						let key = match DictItem::from(cursor.read_u8().unwrap())? {
							DictItem::Pub => ::dict::Key::Pub(cursor.read_str().to_owned()),
							DictItem::Local => {
								let mut id = cursor.read_varint();
								id += self.module.unique_id();
								::dict::Key::Local(id)
							},
						};
						let offset = cursor.read_u64::<EclByteOrder>().unwrap();
						items.push((key, Value::new(self.module.clone(), offset)));
					}
					stack.push(::dict::Dict::new(self.parent.clone(), items));
				}
				Op::Func => {
					let bodyoff = cursor.read_ref();
					stack.push(::func::Func::new(
						self.parent.clone(),
						Func::new(self.module.clone(), bodyoff)));
				}
				Op::Index => {
					let key = stack.pop().expect("No items in stack for index");
					let val = stack.pop().expect("One item in stack for index");
					stack.push(val.index(key));
				}
				Op::Interpolate => {
					let b = stack.pop().expect("No items in stack for interpolate");
					let b = b.to_string();
					let b = b.get_str().unwrap();

					let a = stack.pop().expect("One item in stack for interpolate");
					let a = a.get_str().expect("Interpolating to something not a string.");

					let r = a.to_owned() + b;
					stack.push(::Val::new_atomic(r))
				}
				Op::List => {
					let len = cursor.read_varint();
					let mut items = Vec::with_capacity(len);
					for _ in 0..len {
						let offset = cursor.read_u64::<EclByteOrder>().unwrap();
						items.push(Value::new(self.module.clone(), offset));
					}
					stack.push(::list::List::new(self.parent.clone(), items));
				}
				Op::Neg => {
					let v = stack.pop().expect("No items in stack for neg");
					stack.push(v.neg());
				}
				Op::Num => {
					let num = cursor.read_f64::<EclByteOrder>().unwrap();
					// eprintln!("Num: {}", num);
					stack.push(::Val::new_num(num));
				}
				Op::Ref => {
					let strkey = cursor.read_str();
					let depth = cursor.read_varint();
					let mut id = cursor.read_varint();
					let key = if id == 0 {
						::dict::Key::Pub(strkey.to_owned())
					} else {
						id += self.module.unique_id();
						::dict::Key::Local(id)
					};
					// eprintln!("Ref: {:?}@{} {:?}", key, depth, strkey);
					let v = self.parent.structural_lookup(depth, &key)
						.annotate_with(|| format!("Referenced by {:?}", strkey));
					stack.push(v);
				}
				Op::RefRel => {
					let key = cursor.read_str();
					let depth = cursor.read_varint();
					let key = ::dict::Key::Pub(key.to_owned());
					stack.push(self.parent.structural_lookup(depth, &key));
				}
				Op::Str => {
					let s = ::str::CodeString {
						module: self.module.clone(),
						len: cursor.read_varint(),
						offset: std::convert::TryInto::try_into(cursor.position()).unwrap(),
					};
					cursor.consume(s.len);
					stack.push(::Val::new_atomic(s));
				}
				Op::Sub => {
					let right = stack.pop().expect("One item in stack for add");
					let left = stack.pop().expect("No items in stack for add");
					stack.push(left.subtract(right));
				}
			}
		};

		r
	}
}

pub fn eval(code: Vec<u8>) -> ::Val {
	let module = Rc::new(Module{
		file: "myfile".into(),
		code: code,
	});

	let pc = module.start_pc();
	let parent = Rc::new(::dict::ParentSplitter{
		parent: ::nil::get().value,
		grandparent: None,
	});
	EvalContext{module, pc, parent}.eval()
}

#[derive(Clone,Debug)]
pub struct Value {
	module: Rc<Module>,
	offset: u64,
}

impl Value {
	fn new(module: Rc<Module>, offset: u64) -> Self {
		Value{module, offset}
	}

	pub fn eval(&self, parent: Rc<::Parent>) -> ::Val {
		EvalContext{
			module: self.module.clone(),
			pc: self.offset,
			parent: parent,
		}.eval()
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

	pub fn call(&self, parent: Rc<::Parent>, arg: ::Val) -> ::Val {
		// eprintln!("Executing {:?}", self);

		let mut cursor = std::io::Cursor::new(&self.module.code[..]);
		cursor.set_position(self.offset as u64);

		let args = ::dict::Dict::new(parent.clone(), Vec::new());
		let dict = args.clone();
		let pool = dict.pool.clone();
		let dict = dict.downcast_ref::<::dict::Dict>().unwrap();

		let parent = Rc::new(::dict::ParentSplitter{
			parent: args.value.clone(),
			grandparent: Some(parent),
		});

		match ArgType::from(cursor.read_u8().unwrap())? {
			ArgType::One => {
				let mut id = cursor.read_varint();
				id += self.module.unique_id();
				pool.merge(arg.pool.clone());
				dict._set_val(::dict::Key::Local(id), ::thunk::shim(arg));
			}
			ArgType::Dict => {
				use Value;

				let sourcedict = match arg.downcast_ref::<::dict::Dict>() {
					Some(d) => d,
					None => return ::err::Err::new(format!(
						"Function must be called with dict, called with {:?}",
						arg)),
				};
				let mut unused_args = sourcedict.len();

				let len = cursor.read_usize();
				for _ in 0..len {
					let key = cursor.read_str();
					let passed = sourcedict.index(&::dict::Key::Pub(key.to_owned()))
						.map(|v| v.annotate("Looking up argument value"));
					let val = match ArgReq::from(cursor.read_u8().unwrap())? {
						ArgReq::Required => match passed {
							Some(val) => {
								unused_args -= 1;
								pool.merge(val.pool.clone());
								::thunk::shim(val)
							}
							None => return ::err::Err::new(format!(
								"Required argument {:?} not set in {:?}",
								key, sourcedict)),
						}
						ArgReq::Optional => {
							let off = cursor.read_u64::<EclByteOrder>().unwrap();
							match passed {
								Some(v) => {
									unused_args -= 1;
									::thunk::shim(v)
								}
								None => {
									let value = self::Value::new(self.module.clone(), off);
									::thunk::bytecode(parent.clone(), value)
								}
							}
						}
					};
					dict._set_val(::dict::Key::Pub(key.to_owned()), val)
				}

				if unused_args != 0 {
					return ::err::Err::new(format!(
						"Function called with {} unused arguments.", unused_args));
				}
			}
			ArgType::List => {
				use Value;

				let list = match arg.downcast_ref::<::list::List>() {
					Some(l) => l,
					None => return ::err::Err::new(format!(
						"Function must be called with list, called with {:?}",
						arg)),
				};

				let len = cursor.read_varint();
				if list.len() > len {
					return ::err::Err::new(format!(
						"Function called with too many arguments, expected {} got {}",
						len, list.len()))
				}

				for i in 0..len {
					let mut id = cursor.read_varint();
					id += self.module.unique_id();

					let passed = list.get(i);
					let val = match ArgReq::from(cursor.read_u8().unwrap())? {
						ArgReq::Required => match passed {
							Some(val) => {
								pool.merge(val.pool.clone());
								::thunk::shim(val)
							}
							None => return ::err::Err::new(format!(
								"Required argument {} not provided.", i)),
						}
						ArgReq::Optional => {
							let off = cursor.read_u64::<EclByteOrder>().unwrap();
							passed
								.map(::thunk::shim)
								.unwrap_or_else(|| {
									let value = self::Value::new(self.module.clone(), off);
									::thunk::bytecode(parent.clone(), value)
								})
						}
					};
					dict._set_val(::dict::Key::Local(id), val)
				}
			}
		};

		let r = EvalContext{
			module: self.module.clone(),
			pc: cursor.position(),
			parent: parent,
		}.eval();
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
			compile_to_vec(::Almost::Nil),
			Ok(b"ECL\0v001\x10\0\0\0\0\0\0\0\x01\x01\0".as_ref().into()));
	}
}
