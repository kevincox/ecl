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
		#[derive(Debug)]
		enum $type { $first = 0, $( $item ),* }
		
		impl $type {
			fn from(i: $repr) -> Result<Self,::Val> {
				if i == $type::$first as $repr { Ok($type::$first) }
				$( else if i == $type::$item as $repr { Ok($type::$item) } )*
				else { Err(::err::Err::new(format!("Unknown {} {:02x}", stringify!($type), i))) }
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
		byteorder::LittleEndian::write_u64(&mut buf, n);
		self.write(buf.into_iter().cloned())
	}
	
	fn write_usize(&mut self, n: usize) -> usize {
		self.write_u64(n as u64)
	}
	
	fn write_f64(&mut self, f: f64) -> usize {
		let mut buf = [0; 8];
		byteorder::LittleEndian::write_f64(&mut buf, f);
		self.write(buf.into_iter().cloned())
	}
	
	fn write_op(&mut self, op: Op) -> usize {
		self.write(Some(op as u8))
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
		let target = self.bytes.len() as u64;
		byteorder::LittleEndian::write_u64(&mut self.bytes[jump..(jump+8)], target);
	}
}

fn compile_expr(ctx: &mut CompileContext, ast: ::Almost) -> Result<usize,::Val> {
	match ast {
		::Almost::Add(_, left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(Op::Add);
			Ok(off)
		}
		::Almost::Call(_, left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(Op::Call);
			Ok(off)
		}
		::Almost::GreatEq(left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(Op::Ge);
			Ok(off)
		}
		::Almost::Great(left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(Op::Gt);
			Ok(off)
		}
		::Almost::Eq(left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(Op::Eq);
			Ok(off)
		}
		::Almost::Less(left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(Op::Lt);
			Ok(off)
		}
		::Almost::LessEq(left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(Op::Le);
			Ok(off)
		}
		::Almost::ADict(key, element) => {
			let jump = ctx.start_jump(Op::JumpLazy);
			let childoff = compile(ctx, Rc::try_unwrap(element).unwrap())?;
			ctx.set_jump(jump);
			let off = ctx.write_op(Op::ADict);
			ctx.write_usize(childoff);
			ctx.write_str(&key);
			Ok(off)
		}
		::Almost::Dict(elements) => {
			let jump = ctx.start_jump(Op::JumpLazy);
			
			ctx.scope_open();
			
			let elements = elements.into_iter()
				.map(|e| {
					let id = ctx.scope_add(e.key.clone(), e.is_public());
					(e.key, id, e.val)
				})
				.collect::<Vec<_>>();
			
			let elements = elements.into_iter()
				.map(|(key, id, val)| (key, id, compile(ctx, val)))
				.collect::<Vec<_>>();
			
			ctx.scope_close();
			
			ctx.set_jump(jump);
			let off = ctx.write_op(Op::Dict);
			ctx.write_u64(elements.len() as u64);
			for (key, id, offset) in elements {
				match id {
					0 => {
						ctx.write_u8(DictItem::Pub as u8);
						ctx.write_str(&key);
					}
					id => {
						ctx.write_u8(DictItem::Local as u8);
						ctx.write_usize(id);
					}
				}
				ctx.write_usize(offset?);
			}
			Ok(off)
		}
		::Almost::Func(data) => {
			let jump = ctx.start_jump(Op::JumpFunc);
			
			ctx.scope_open();
			let ::func::FuncData{arg, body} = Rc::try_unwrap(data).unwrap();
			let argoff = match arg {
				::func::Arg::One(arg) => {
					let argoff = ctx.write_u8(ArgType::One as u8);
					let id = ctx.scope_add(arg.clone(), false);
					ctx.write_usize(id);
					argoff
				}
				::func::Arg::Dict(args) => {
					let args = args.into_iter()
						.map(|(key, required, val)| {
							ctx.scope_add(key.clone(), true);
							(key, required, val)
						})
						.collect::<Vec<_>>();
					
					let args = args.into_iter().map(|(key, required, val)| {
							if required {
								(key, Ok(0))
							} else {
								(key, compile(ctx, val))
							}
						})
						.collect::<Vec<_>>();
					
					let argoff = ctx.write_u8(ArgType::Dict as u8);
					ctx.write_usize(args.len());
					for (key, off) in args {
						let off = off?;
						
						ctx.write_str(&key);
						if off == 0 {
							ctx.write_u8(ArgReq::Required as u8);
						} else {
							ctx.write_u8(ArgReq::Optional as u8);
							ctx.write_usize(off);
						}
					}
					
					argoff
				}
				::func::Arg::List(args) => {
					let args = args.into_iter()
						.map(|(key, required, val)| {
							(ctx.scope_add(key, false), required, val)
						})
						.collect::<Vec<_>>();
					
					let args = args.into_iter().map(|(id, required, val)| {
							if required {
								(id, Ok(0))
							} else {
								(id, compile(ctx, val))
							}
						})
						.collect::<Vec<_>>();
					
					let argoff = ctx.write_u8(ArgType::List as u8);
					ctx.write_usize(args.len());
					for (id, off) in args {
						let off = off?;
						
						ctx.write_usize(id);
						if off == 0 {
							ctx.write_u8(ArgReq::Required as u8);
						} else {
							ctx.write_u8(ArgReq::Optional as u8);
							ctx.write_usize(off);
						}
					}
					
					argoff
				}
			};
			compile(ctx, body)?;
			ctx.scope_close();
			
			ctx.set_jump(jump);
			let off = ctx.write_op(Op::Func);
			ctx.write_usize(argoff);
			Ok(off)
		}
		::Almost::Index(_, left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(Op::Index);
			Ok(off)
		}
		::Almost::List(elements) => {
			let jump = ctx.start_jump(Op::JumpLazy);
			
			let offsets = elements.into_iter().map(|element| 
				compile(ctx, Rc::try_unwrap(element).unwrap()))
				.collect::<Vec<_>>();
			
			ctx.set_jump(jump);
			let off = ctx.write_op(Op::List);
			ctx.write_usize(offsets.len());
			for offset in offsets {
				ctx.write_usize(offset?);
			}
			Ok(off)
		},
		::Almost::Neg(_, v) => {
			let off = compile_expr(ctx, *v)?;
			ctx.write_op(Op::Neg);
			Ok(off)
		}
		::Almost::Nil => compile_global(ctx, 0),
		::Almost::Num(n) => {
			let off = ctx.write_op(Op::Num);
			ctx.write_f64(n);
			Ok(off)
		}
		::Almost::Ref(loc, name) => {
			if let Some((depth, id)) = ctx.scope_find(&name) {
				let off = ctx.write_op(Op::Ref);
				ctx.write_str(&name); // TODO: remove this.
				ctx.write_usize(depth);
				ctx.write_usize(id);
				Ok(off)
			} else if let Some(id) = ::builtins::builtin_id(&name) { // TODO get global.
				let off = ctx.write_op(Op::Global);
				ctx.write_usize(id);
				Ok(off)
			} else {
				Err(::err::Err::new(format!("{:?} Invalid reference {:?}", loc, name)))
			}
		}
		::Almost::StructRef(_, depth, key) => {
			if let Some(id) = ctx.scope_find_at(&key, depth) {
				let off = ctx.write_op(Op::Ref);
				ctx.write_str(&key); // TODO: remove this.
				ctx.write_usize(depth);
				ctx.write_usize(id);
				Ok(off)
			} else {
				let off = ctx.write_op(Op::RefRel);
				ctx.write_str(&key); // TODO: remove this.
				ctx.write_usize(depth);
				Ok(off)
			}
		}
		::Almost::Str(parts) => {
			let off = ctx.write_op(Op::Str);
			ctx.write_str("");
			for part in parts {
				match part {
					::StringPart::Exp(s) => {
						compile_expr(ctx, s)?;
					},
					::StringPart::Lit(s) => {
						ctx.write_op(Op::Str);
						ctx.write_str(&s);
					},
				}
				ctx.write_op(Op::Interpolate);
			}
			Ok(off)
		}
		::Almost::StrStatic(s) => {
			let off = ctx.write_op(Op::Str);
			ctx.write_str(&s);
			Ok(off)
		}
		::Almost::Sub(_, left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(Op::Sub);
			Ok(off)
		}
		other => unimplemented!("compile({:?})", other),
	}
}

fn compile_global(ctx: &mut CompileContext, global: usize) -> Result<usize,::Val> {
	let off = ctx.write_op(Op::Global);
	ctx.write_usize(global);
	Ok(off)
}

fn compile(ctx: &mut CompileContext, ast: ::Almost) -> Result<usize,::Val> {
	let off = compile_expr(ctx, ast)?;
	ctx.write_op(Op::Ret);
	Ok(off)
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
	let start = compile(&mut ctx, ast)? as u64;
	ctx.bytes[START_OFFSET+0] = (start >> 8*0) as u8;
	ctx.bytes[START_OFFSET+1] = (start >> 8*1) as u8;
	ctx.bytes[START_OFFSET+2] = (start >> 8*2) as u8;
	ctx.bytes[START_OFFSET+3] = (start >> 8*3) as u8;
	ctx.bytes[START_OFFSET+4] = (start >> 8*4) as u8;
	ctx.bytes[START_OFFSET+5] = (start >> 8*5) as u8;
	ctx.bytes[START_OFFSET+6] = (start >> 8*6) as u8;
	ctx.bytes[START_OFFSET+7] = (start >> 8*7) as u8;
	
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
	
	fn start_pc(&self) -> usize {
		self.read_usize(START_OFFSET)
	}
	
	fn read_u64(&self, i: usize) -> u64 {
		EclByteOrder::read_u64(&self.code[i..(i+8)])
	}
	
	fn read_usize(&self, i: usize) -> usize {
		let n = self.read_u64(i);
		assert!(n <= usize::max_value() as u64);
		n as usize
	}
}

trait CursorExt {
	fn read_str(&mut self) -> String;
}

impl<'a> CursorExt for std::io::Cursor<&'a [u8]> {
	fn read_str(&mut self) -> String {
		let len = self.read_u64::<EclByteOrder>().unwrap() as usize;
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

pub fn eval(code: Vec<u8>) -> ::Val {
	let module = Rc::new(Module{
		file: "myfile".into(),
		code: code,
	});
	
	let pc = module.start_pc();
	eval_at(module, pc, ::nil::get())
}

pub fn eval_at(module: Rc<Module>, pc: usize, pstruct: ::Val) -> ::Val {
	// eprintln!("Executing @ {}", pc);
	let mut cursor = std::io::Cursor::new(&module.code[..]);
	cursor.seek(std::io::SeekFrom::Start(pc as u64)).unwrap();
	
	let mut stack = Vec::new();
	loop {
		let op = Op::from(cursor.read_u8().unwrap())?;
		// eprintln!("Executing OP {:?} @ {}", op, pc + cursor.position() as usize - 1);
		match op {
			Op::Ret => {
				assert_eq!(stack.len(), 1);
				return stack.pop().unwrap()
			},
			Op::Global => {
				let id = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
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
				let childoff = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let key = cursor.read_str();
				
				stack.push(::dict::Dict::new_adict(
					pstruct.clone(),
					key,
					Value::new(module.clone(), childoff)));
			}
			Op::Dict => {
				let len = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let mut items = Vec::with_capacity(len);
				for _ in 0..len {
					let key = match DictItem::from(cursor.read_u8().unwrap())? {
						DictItem::Pub => ::dict::Key::Pub(cursor.read_str()),
						DictItem::Local => {
							let mut id = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
							id += module.unique_id();
							::dict::Key::Local(id)
						},
					};
					let offset = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
					items.push((key, Value::new(module.clone(), offset)));
				}
				stack.push(::dict::Dict::new(pstruct.clone(), items));
			}
			Op::Func => {
				let bodyoff = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				stack.push(::func::Func::new(
					pstruct.clone(),
					Func::new(module.clone(), bodyoff)));
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
				let len = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let mut items = Vec::with_capacity(len);
				for _ in 0..len {
					let offset = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
					items.push(Value::new(module.clone(), offset));
				}
				stack.push(::list::List::new(pstruct.clone(), items));
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
				let depth = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let mut id = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let key = if id == 0 {
					::dict::Key::Pub(strkey.clone())
				} else {
					id += module.unique_id();
					::dict::Key::Local(id)
				};
				let v = pstruct.structural_lookup(depth, &key)
					.annotate_with(|| format!("Referenced by {:?}", strkey));
				stack.push(v);
			}
			Op::RefRel => {
				let key = cursor.read_str();
				let depth = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let key = ::dict::Key::Pub(key);
				stack.push(pstruct.structural_lookup(depth, &key));
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
	}
}

pub fn decompile(code: &[u8]) -> Result<String,::Val> {
	let mut cursor = std::io::Cursor::new(code);
	cursor.seek(std::io::SeekFrom::Start(START_END as u64)).unwrap();
	let mut out = String::with_capacity(code.len() * 8);
	
	while let Ok(op) = cursor.read_u8() {
		match Op::from(op)? {
			Op::Ret => {
				writeln!(out, "{:08} RET", cursor.position()).unwrap();
			}
			Op::Global => {
				let id = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				writeln!(out, "{:08} GLOBAL {} {:?}",
					cursor.position(),
					id, ::builtins::get_id(id)).unwrap();
			}
			Op::Add => {
				writeln!(out, "{:08} ADD", cursor.position()).unwrap();
			}
			Op::Call => {
				writeln!(out, "{:08} CALL", cursor.position()).unwrap();
			}
			Op::Ge => {
				writeln!(out, "{:08} GE", cursor.position()).unwrap();
			}
			Op::Gt => {
				writeln!(out, "{:08} GT", cursor.position()).unwrap();
			}
			Op::Eq => {
				writeln!(out, "{:08} EQ", cursor.position()).unwrap();
			}
			Op::Lt => {
				writeln!(out, "{:08} LT", cursor.position()).unwrap();
			}
			Op::Le => {
				writeln!(out, "{:08} LE", cursor.position()).unwrap();
			}
			Op::ADict => {
				let childoff = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let key = cursor.read_str();
				writeln!(out, "{:08} ADICT {:?}@{:08}", cursor.position(), key, childoff).unwrap();
			}
			Op::Dict => {
				let len = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				writeln!(out, "{:08} DICT {} items", cursor.position(), len).unwrap();
				for _ in 0..len {
					write!(out, "     ... ").unwrap();
					match DictItem::from(cursor.read_u8().unwrap())? {
						DictItem::Pub => {
							let key = cursor.read_str();
							write!(out, "PUB   {:?}", key).unwrap()
						}
						DictItem::Local => {
							let mut id = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
							write!(out, "LOCAL {:04x}", id).unwrap();
						},
					}
					let off = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
					writeln!(out, " @ {:08}", off).unwrap();
				}
			}
			Op::Func => {
				let off = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				writeln!(out, "{:08} FUNC @ {:08}", cursor.position(), off).unwrap();
			}
			Op::Index => {
				writeln!(out, "{:08} INDEX", cursor.position()).unwrap();
			}
			Op::Interpolate => {
				writeln!(out, "{:08} OP_INTERPOLATE", cursor.position()).unwrap();
			}
			Op::JumpFunc => {
				let target = cursor.read_u64::<EclByteOrder>().unwrap();
				writeln!(out, "{:08} OP_JUMP_FUNC @{:08}", cursor.position(), target).unwrap();
				// TODO: Recompile rather than seek over.
				cursor.seek(std::io::SeekFrom::Start(target)).unwrap();
			}
			Op::JumpLazy => {
				let target = cursor.read_u64::<EclByteOrder>().unwrap();
				writeln!(out, "{:08} JUMP_LAZY @{:08}", cursor.position(), target).unwrap();
			}
			Op::List => {
				let len = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				writeln!(out, "{:08} LIST {} items", cursor.position(), len).unwrap();
				for i in 0..len {
					let off = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
					writeln!(out, "     ... {:2} @ {}", i, off).unwrap();
				}
			}
			Op::Neg => {
				writeln!(out, "{:08} NEG", cursor.position()).unwrap();
			}
			Op::Num => {
				let num = cursor.read_f64::<EclByteOrder>().unwrap();
				writeln!(out, "{:08} NUM {}", cursor.position(), num).unwrap();
			}
			Op::Ref => {
				let key = cursor.read_str();
				let depth = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let mut id = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				writeln!(out, "{:08} REF depth:{} {:?} id:{}",
					cursor.position(), depth, key, id).unwrap();
			}
			Op::RefRel => {
				let key = cursor.read_str();
				let depth = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				writeln!(out, "{:08} REF_REL depth:{} {:?}", cursor.position(), depth, key).unwrap();
			}
			Op::Str => {
				let s = cursor.read_str();
				writeln!(out, "{:08} STR {:?}", cursor.position(), s).unwrap();
			}
			Op::Sub => {
				writeln!(out, "{:08} SUB", cursor.position()).unwrap();
			}
		}
	}
	
	return Ok(out)
}

#[derive(Clone,Debug,Trace)]
pub struct Value {
	#[unsafe_ignore_trace]
	module: Rc<Module>,
	offset: usize,
}

impl Value {
	fn new(module: Rc<Module>, offset: usize) -> Self {
		Value{module, offset}
	}
	
	pub fn eval(&self, parent: ::Val) -> ::Val {
		eval_at(self.module.clone(), self.offset, parent.clone())
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
				let mut id = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				id += self.module.unique_id();
				dict._set_val(::dict::Key::Local(id), ::thunk::Thunk::shim(arg));
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
				
				let len = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				for _ in 0..len {
					let key = cursor.read_str();
					let passed = sourcedict.index(&::dict::Key::Pub(key.clone()))
						.map(|v| v.annotate("Looking up argument value"));
					let val = match ArgReq::from(cursor.read_u8().unwrap())? {
						ArgReq::Required => match passed {
							Some(val) => {
								unused_args -= 1;
								::thunk::Thunk::shim(val)
							}
							None => return ::err::Err::new(format!(
								"Required argument {:?} not set in {:?}",
								key, sourcedict)),
						}
						ArgReq::Optional => {
							let off = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
							match passed {
								Some(v) => {
									unused_args -= 1;
									::thunk::Thunk::shim(v)
								}
								None => {
									let value = self::Value::new(self.module.clone(), off);
									::thunk::Thunk::bytecode(parent.clone(), value)
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
				
				let len = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				if list.len() > len {
					return ::err::Err::new(format!(
						"Function called with too many arguments, expected {} got {}",
						len, list.len()))
				}
				
				for i in 0..len {
					let mut id = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
					id += self.module.unique_id();
					
					let passed = list.get(i);
					let val = match ArgReq::from(cursor.read_u8().unwrap())? {
						ArgReq::Required => match passed {
							Some(val) => ::thunk::Thunk::shim(val),
							None => return ::err::Err::new(format!(
								"Required argument {} not provided.", i)),
						}
						ArgReq::Optional => {
							let off = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
							passed
								.map(::thunk::Thunk::shim)
								.unwrap_or_else(|| {
									let value = self::Value::new(self.module.clone(), off);
									::thunk::Thunk::bytecode(parent.clone(), value)
								})
						}
					};
					dict._set_val(::dict::Key::Local(id), val)
				}
			}
		};
		
		eval_at(self.module.clone(), cursor.position() as usize, parent)
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
