use byteorder::{self, ByteOrder, ReadBytesExt};
use std;
use std::fmt::Write;
use std::io::{Read, Seek};
use std::rc::Rc;

const MAGIC: &[u8] = b"ECL\0v001";
const START_OFFSET: usize = 8;
const START_LEN: usize = 8;
const START_END: usize = START_OFFSET + START_LEN;

macro_rules! codes {
	( $type:ident $( $item:ident, )* ) => {
		codes!{$type: u8 $( $item, )*}
	};
	( $type:ident : $repr:ident $first:ident , $( $item:ident, )* ) => {
		#[derive(Clone,Copy,Debug)]
		enum $type { $first = 0, $( $item ),* }

		impl $type {
			fn from(i: $repr) -> Result<Self,::Val> {
				if i == $type::$first as $repr { Ok($type::$first) }
				$( else if i == $type::$item as $repr { Ok($type::$item) } )*
				else { Err(::err::Err::new(format!("Unknown {} {:02x}", stringify!($type), i))) }
			}
			
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
	JumpLazy,
	JumpFunc,
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

struct CompileContext {
	bytes: Vec<u8>,
	scope: Vec<(String,usize,usize)>,
	depth: usize,
	last_local: usize
}

impl CompileContext {
	fn scope_open(&mut self) {
		self.depth += 1;
	}

	fn scope_add(&mut self, name: String, public: bool) -> usize {
		let id = if public {
			0
		} else {
			self.last_local += 1;
			self.last_local
		};

		self.scope.push((name, self.depth, id));

		id
	}

	fn scope_find(&self, name: &str) -> Option<(usize,usize)> {
		self.scope.iter().rev()
			.find(|i| i.0 == name)
			.map(|i| (self.depth - i.1, i.2))
	}

	fn scope_find_at(&self, name: &str, ups: usize) -> Option<usize> {
		let depth = self.depth - ups;
		self.scope.iter().rev()
			.skip_while(|i| i.1 > depth)
			.take_while(|i| i.1 == depth)
			.find(|i| i.0 == name)
			.map(|i| i.2)
	}

	fn scope_close(&mut self) {
		self.depth -= 1;
		let todrop = self.scope.iter().rev()
			.take_while(|i| i.1 > self.depth)
			.count();
		let newend = self.scope.len() - todrop;
		self.scope.truncate(newend);
	}

	fn write<T: IntoIterator<Item=u8>>(&mut self, bytes: T) -> usize {
		let start_offset = self.bytes.len();
		self.bytes.extend(bytes);
		start_offset
	}

	fn write_u8(&mut self, byte: u8) -> usize {
		self.write(Some(byte))
	}

	fn write_u64(&mut self, n: u64) -> usize {
		let mut buf = [0; 8];
		EclByteOrder::write_u64(&mut buf, n);
		self.write(buf.into_iter().cloned())
	}

	fn write_usize(&mut self, n: usize) -> usize {
		self.write_u64(std::convert::TryFrom::try_from(n).unwrap())
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
		let off = self.write_usize(s.len());
		self.write(s.bytes());
		off
	}

	fn start_jump(&mut self, op: Op) -> usize {
		self.write_op(op);
		return self.write_u64(0)
	}

	fn set_jump(&mut self, jump: usize) {
		let target = std::convert::TryFrom::try_from(self.bytes.len()).unwrap();
		EclByteOrder::write_u64(&mut self.bytes[jump..(jump+8)], target);
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
				let jump = self.start_jump(Op::JumpLazy);
				let childoff = self.compile(Rc::try_unwrap(element).unwrap())?;
				self.set_jump(jump);
				let off = self.write_op(Op::ADict);
				self.write_usize(childoff);
				self.write_str(&key);
				Ok(off)
			}
			::Almost::Dict(elements) => {
				let jump = self.start_jump(Op::JumpLazy);

				self.scope_open();

				let elements = elements.into_iter()
					.map(|e| {
						let id = self.scope_add(e.key.clone(), e.is_public());
						(e.key, id, e.val)
					})
					.collect::<Vec<_>>();

				let elements = elements.into_iter()
					.map(|(key, id, val)| (key, id, self.compile(val)))
					.collect::<Vec<_>>();

				self.scope_close();

				self.set_jump(jump);
				let off = self.write_op(Op::Dict);
				self.write_usize(elements.len());
				for (key, id, offset) in elements {
					match id {
						0 => {
							self.write_u8(DictItem::Pub.to());
							self.write_str(&key);
						}
						id => {
							self.write_u8(DictItem::Local.to());
							self.write_usize(id);
						}
					}
					self.write_usize(offset?);
				}
				Ok(off)
			}
			::Almost::Func(data) => {
				let jump = self.start_jump(Op::JumpFunc);

				self.scope_open();
				let ::func::FuncData{arg, body} = Rc::try_unwrap(data).unwrap();
				let argoff = match arg {
					::func::Arg::One(arg) => {
						let argoff = self.write_u8(ArgType::One.to());
						let id = self.scope_add(arg.clone(), false);
						self.write_usize(id);
						argoff
					}
					::func::Arg::Dict(args) => {
						let args = args.into_iter()
							.map(|(key, required, val)| {
								self.scope_add(key.clone(), true);
								(key, required, val)
							})
							.collect::<Vec<_>>();

						let args = args.into_iter().map(|(key, required, val)| {
								if required {
									(key, Ok(0))
								} else {
									(key, self.compile(val))
								}
							})
							.collect::<Vec<_>>();

						let argoff = self.write_u8(ArgType::Dict.to());
						self.write_usize(args.len());
						for (key, off) in args {
							let off = off?;

							self.write_str(&key);
							if off == 0 {
								self.write_u8(ArgReq::Required.to());
							} else {
								self.write_u8(ArgReq::Optional.to());
								self.write_usize(off);
							}
						}

						argoff
					}
					::func::Arg::List(args) => {
						let args = args.into_iter()
							.map(|(key, required, val)| {
								(self.scope_add(key, false), required, val)
							})
							.collect::<Vec<_>>();

						let args = args.into_iter().map(|(id, required, val)| {
								if required {
									(id, Ok(0))
								} else {
									(id, self.compile(val))
								}
							})
							.collect::<Vec<_>>();

						let argoff = self.write_u8(ArgType::List.to());
						self.write_usize(args.len());
						for (id, off) in args {
							let off = off?;

							self.write_usize(id);
							if off == 0 {
								self.write_u8(ArgReq::Required.to());
							} else {
								self.write_u8(ArgReq::Optional.to());
								self.write_usize(off);
							}
						}

						argoff
					}
				};
				self.compile(body)?;
				self.scope_close();

				self.set_jump(jump);
				let off = self.write_op(Op::Func);
				self.write_usize(argoff);
				Ok(off)
			}
			::Almost::Index(_, left, right) => {
				self.compile_binary_op(Op::Index, *left, *right)
			}
			::Almost::List(elements) => {
				let jump = self.start_jump(Op::JumpLazy);

				let offsets = elements.into_iter().map(|element| 
					self.compile(Rc::try_unwrap(element).unwrap()))
					.collect::<Vec<_>>();

				self.set_jump(jump);
				let off = self.write_op(Op::List);
				self.write_usize(offsets.len());
				for offset in offsets {
					self.write_usize(offset?);
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
				if let Some((depth, id)) = self.scope_find(&name) {
					let off = self.write_op(Op::Ref);
					self.write_str(&name); // TODO: remove this.
					self.write_usize(depth);
					self.write_usize(id);
					Ok(off)
				} else if let Some(id) = ::builtins::builtin_id(&name) {
					let off = self.write_op(Op::Global);
					self.write_usize(id);
					Ok(off)
				} else {
					Err(::err::Err::new(format!("{:?} Invalid reference {:?}", loc, name)))
				}
			}
			::Almost::StructRef(_, depth, key) => {
				if let Some(id) = self.scope_find_at(&key, depth) {
					let off = self.write_op(Op::Ref);
					self.write_str(&key); // TODO: remove this.
					self.write_usize(depth);
					self.write_usize(id);
					Ok(off)
				} else {
					let off = self.write_op(Op::RefRel);
					self.write_str(&key); // TODO: remove this.
					self.write_usize(depth);
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
		self.write_usize(global);
		Ok(off)
	}
}

pub fn compile_to_vec(ast: ::Almost) -> Result<Vec<u8>,::Val> {
	let mut ctx = CompileContext{
		bytes: MAGIC.into_iter()
			.chain(&[0; 8]) // Offset buffer.
			.cloned().collect(),
		scope: Vec::with_capacity(20),
		depth: 0,
		last_local: 0,
	};
	let start = ctx.compile(ast)?;
	let start = std::convert::TryFrom::try_from(start).unwrap();
	EclByteOrder::write_u64(&mut ctx.bytes[START_OFFSET..START_OFFSET+8], start);
	Ok(ctx.bytes)
}

#[derive(PartialEq)]
pub struct Module {
	file: std::path::PathBuf,
	code: Vec<u8>,
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

trait CursorExt {
	fn read_usize(&mut self) -> usize;
	fn read_str(&mut self) -> String;
}

impl<'a> CursorExt for std::io::Cursor<&'a [u8]> {
	fn read_usize(&mut self) -> usize {
		let n = self.read_u64::<EclByteOrder>().unwrap();
		std::convert::TryFrom::try_from(n).unwrap()
	}
	
	fn read_str(&mut self) -> String {
		let len = self.read_usize();
		let mut buf = vec![0; len];
		self.read_exact(&mut buf).unwrap();
		String::from_utf8(buf).unwrap()
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
	pstruct: ::Val,
}

impl EvalContext {
	pub fn eval(&mut self) -> ::Val {
		// eprintln!("Executing @ {}", pc);
		let mut cursor = std::io::Cursor::new(&self.module.code[..]);
		cursor.seek(std::io::SeekFrom::Start(self.pc)).unwrap();

		let mut stack = Vec::new();
		let r = loop {
			let op = Op::from(cursor.read_u8().unwrap())?;
			// eprintln!("Executing OP {:?} @ {}", op, pc + cursor.position() as usize - 1);
			match op {
				Op::Ret => {
					assert_eq!(stack.len(), 1);
					break stack.pop().unwrap()
				},
				Op::Global => {
					let id = cursor.read_usize();
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
						self.pstruct.clone(),
						key,
						Value::new(self.module.clone(), childoff)));
				}
				Op::Dict => {
					let len = cursor.read_usize();
					let mut items = Vec::with_capacity(len);
					for _ in 0..len {
						let key = match DictItem::from(cursor.read_u8().unwrap())? {
							DictItem::Pub => ::dict::Key::Pub(cursor.read_str()),
							DictItem::Local => {
								let mut id = cursor.read_usize();
								id += self.module.unique_id();
								::dict::Key::Local(id)
							},
						};
						let offset = cursor.read_u64::<EclByteOrder>().unwrap();
						items.push((key, Value::new(self.module.clone(), offset)));
					}
					stack.push(::dict::Dict::new(self.pstruct.clone(), items));
				}
				Op::Func => {
					let bodyoff = cursor.read_usize();
					stack.push(::func::Func::new(
						self.pstruct.clone(),
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
					stack.push(::Val::new(r))
				}
				Op::JumpFunc | Op::JumpLazy => {
					let target = cursor.read_u64::<EclByteOrder>().unwrap();
					cursor.seek(std::io::SeekFrom::Start(target)).unwrap();
				}
				Op::List => {
					let len = cursor.read_usize();
					let mut items = Vec::with_capacity(len);
					for _ in 0..len {
						let offset = cursor.read_u64::<EclByteOrder>().unwrap();
						items.push(Value::new(self.module.clone(), offset));
					}
					stack.push(::list::List::new(self.pstruct.clone(), items));
				}
				Op::Neg => {
					let v = stack.pop().expect("No items in stack for neg");
					stack.push(v.neg());
				}
				Op::Num => {
					stack.push(::Val::new(cursor.read_f64::<EclByteOrder>().unwrap()));
				}
				Op::Ref => {
					let strkey = cursor.read_str();
					let depth = cursor.read_usize();
					let mut id = cursor.read_usize();
					let key = if id == 0 {
						::dict::Key::Pub(strkey.clone())
					} else {
						id += self.module.unique_id();
						::dict::Key::Local(id)
					};
					let v = self.pstruct.structural_lookup(depth, &key)
						.annotate_with(|| format!("Referenced by {:?}", strkey));
					stack.push(v);
				}
				Op::RefRel => {
					let key = cursor.read_str();
					let depth = cursor.read_usize();
					let key = ::dict::Key::Pub(key);
					stack.push(self.pstruct.structural_lookup(depth, &key));
				}
				Op::Str => {
					let s = cursor.read_str();
					stack.push(::Val::new(s));
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
	EvalContext{module, pc, pstruct: ::nil::get()}.eval()
}

#[derive(Clone,Copy)]
pub struct DisassembleOptions {
	pub relative_offset_ops: bool,
	pub relative_offset_references: bool,
}

impl DisassembleOptions {
	pub fn new() -> Self {
		DisassembleOptions{
			relative_offset_ops: false,
			relative_offset_references: false,
		}
	}

	pub fn diffable() -> Self {
		DisassembleOptions{
			relative_offset_ops: true,
			relative_offset_references: true,
		}
	}

	pub fn disassemble(&self, code: &[u8]) -> Result<String,::Val> {
		DisassembeContext::new(*self, code).disassemble()
	}
}

struct DisassembeContext<'a> {
	options: DisassembleOptions,
	cursor: std::io::Cursor<&'a [u8]>,
	previous_pc: u64,
	out: String,
}

impl<'a> DisassembeContext<'a> {
	fn new(options: DisassembleOptions, code: &'a[u8]) -> Self {
		let mut cursor = std::io::Cursor::new(code);
		cursor.seek(std::io::SeekFrom::Start(START_END as u64)).unwrap();

		DisassembeContext{
			options,
			cursor,
			previous_pc: 0,
			out: String::with_capacity(code.len() * 8),
		}
	}

	fn write_prefix(&mut self, op: Op) -> Result<(), ::Val> {
		let pc = self.cursor.position()-1;
		if self.options.relative_offset_ops {
			write!(self.out, "{:+8} {:?}", pc - self.previous_pc, op)?;
		} else {
			write!(self.out, "{:08} {:?}", pc, op)?;
		}
		self.previous_pc = pc;
		Ok(())
	}

	fn write_continuation(&mut self) -> std::fmt::Result {
		write!(self.out, "     ...")
	}

	fn write_ref(&mut self, off: u64) -> std::fmt::Result {
		if self.options.relative_offset_references {
			write!(self.out, "{:+}", off as i128 - self.previous_pc as i128)
		} else {
			write!(self.out, "{:08}", off)
		}
	}

	fn disassemble(mut self) -> Result<String,::Val> {
		while let Ok(op) = self.cursor.read_u8() {
			let op = Op::from(op)?;

			self.write_prefix(op)?;

			match op {
				Op::Ret => writeln!(self.out)?,
				Op::Global => {
					let id = self.cursor.read_usize();
					writeln!(self.out, " {} {:?}", id, ::builtins::get_id(id))?;
				}
				Op::Add => writeln!(self.out)?,
				Op::Call => writeln!(self.out)?,
				Op::Ge => writeln!(self.out)?,
				Op::Gt => writeln!(self.out)?,
				Op::Eq => writeln!(self.out)?,
				Op::Lt => writeln!(self.out)?,
				Op::Le => writeln!(self.out)?,
				Op::ADict => {
					let childoff = self.cursor.read_u64::<EclByteOrder>()?;
					let key = self.cursor.read_str();
					write!(self.out, " {:?}@", key)?;
					self.write_ref(childoff)?;
					writeln!(self.out)?;
				}
				Op::Dict => {
					let len = self.cursor.read_u64::<EclByteOrder>()?;
					writeln!(self.out, " {} items", len)?;
					for _ in 0..len {
						self.write_continuation()?;
						match DictItem::from(self.cursor.read_u8().unwrap())? {
							DictItem::Pub => {
								let key = self.cursor.read_str();
								write!(self.out, " PUB   {:?}", key).unwrap()
							}
							DictItem::Local => {
								let mut id = self.cursor.read_usize();
								write!(self.out, " LOCAL {:04x}", id).unwrap();
							},
						}
						let off = self.cursor.read_u64::<EclByteOrder>()?;
						write!(self.out, " @ ")?;
						self.write_ref(off)?;
						writeln!(self.out)?;
					}
				}
				Op::Func => {
					let off = self.cursor.read_usize();
					writeln!(self.out, " @ {:08}", off)?;
				}
				Op::Index => writeln!(self.out)?,
				Op::Interpolate => writeln!(self.out)?,
				Op::JumpFunc => {
					let target = self.cursor.read_u64::<EclByteOrder>().unwrap();
					writeln!(self.out, " @{:08}", target)?;
					// TODO: Disassemble rather than seek over.
					self.cursor.seek(std::io::SeekFrom::Start(target)).unwrap();
				}
				Op::JumpLazy => {
					let target = self.cursor.read_u64::<EclByteOrder>().unwrap();
					writeln!(self.out, " @{:08}", target)?;
				}
				Op::List => {
					let len = self.cursor.read_usize();
					writeln!(self.out, " {} items", len)?;
					for i in 0..len {
						let off = self.cursor.read_usize();
						self.write_continuation()?;
						writeln!(self.out, " {:2} @ {}", i, off)?;
					}
				}
				Op::Neg => writeln!(self.out)?,
				Op::Num => {
					let num = self.cursor.read_f64::<EclByteOrder>().unwrap();
					writeln!(self.out, " {}", num)?;
				}
				Op::Ref => {
					let key = self.cursor.read_str();
					let depth = self.cursor.read_usize();
					let mut id = self.cursor.read_usize();
					writeln!(self.out, " depth:{} {:?} id:{}", depth, key, id)?;
				}
				Op::RefRel => {
					let key = self.cursor.read_str();
					let depth = self.cursor.read_usize();
					writeln!(self.out, " depth:{} {:?}", depth, key)?;
				}
				Op::Str => {
					let s = self.cursor.read_str();
					writeln!(self.out, " {:?}", s)?;
				}
				Op::Sub => writeln!(self.out)?,
			}
		}

		return Ok(self.out)
	}
}

#[derive(Clone,Debug,Trace)]
pub struct Value {
	#[unsafe_ignore_trace]
	module: Rc<Module>,
	offset: u64,
}

impl Value {
	fn new(module: Rc<Module>, offset: u64) -> Self {
		Value{module, offset}
	}

	pub fn eval(&self, parent: ::Val) -> ::Val {
		EvalContext{
			module: self.module.clone(),
			pc: self.offset,
			pstruct: parent,
		}.eval()
	}
}

#[derive(Clone,Debug,Trace)]
pub struct Func {
	#[unsafe_ignore_trace]
	module: std::rc::Rc<Module>,
	offset: usize,
}

impl Func {
	fn new(module: Rc<Module>, offset: usize) -> Self {
		Func{module, offset}
	}

	pub fn call(&self, parent: ::Val, arg: ::Val) -> ::Val {
		let mut cursor = std::io::Cursor::new(&self.module.code[..]);
		cursor.seek(std::io::SeekFrom::Start(self.offset as u64)).unwrap();

		let args = ::dict::Dict::new(parent.clone(), Vec::new());
		let dict = args.clone();
		let dict = dict.downcast_ref::<::dict::Dict>().unwrap();

		let parent = ::Val::new(::dict::ParentSplitter{
			parent: args.clone(),
			grandparent: parent,
		});

		match ArgType::from(cursor.read_u8().unwrap())? {
			ArgType::One => {
				let mut id = cursor.read_usize();
				id += self.module.unique_id();
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
					let passed = sourcedict.index(&::dict::Key::Pub(key.clone()))
						.map(|v| v.annotate("Looking up argument value"));
					let val = match ArgReq::from(cursor.read_u8().unwrap())? {
						ArgReq::Required => match passed {
							Some(val) => {
								unused_args -= 1;
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
					dict._set_val(::dict::Key::Pub(key), val)
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

				let len = cursor.read_usize();
				if list.len() > len {
					return ::err::Err::new(format!(
						"Function called with too many arguments, expected {} got {}",
						len, list.len()))
				}

				for i in 0..len {
					let mut id = cursor.read_usize();
					id += self.module.unique_id();

					let passed = list.get(i);
					let val = match ArgReq::from(cursor.read_u8().unwrap())? {
						ArgReq::Required => match passed {
							Some(val) => ::thunk::shim(val),
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

		EvalContext{
			module: self.module.clone(),
			pc: cursor.position(),
			pstruct: parent,
		}.eval()
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn compile_global() {
		assert_eq!(
			compile_to_vec(::Almost::Nil),
			Ok(b"ECL\0v001\x10\0\0\0\0\0\0\0\x01\0\0\0\0\0\0\0\0\x00".as_ref().into()));
	}
}
