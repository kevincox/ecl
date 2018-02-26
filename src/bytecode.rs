use byteorder::{self, ByteOrder, ReadBytesExt};
use std;
use std::fmt::Write;
use std::io::{Read, Seek};
use std::rc::Rc;

const MAGIC: &[u8] = b"ECL\0v001";
const START_OFFSET: usize = 8;
const START_LEN: usize = 8;
const START_END: usize = START_OFFSET + START_LEN;

iota! {
	const OP_RET: u8 = iota;
		| OP_GLOBAL
		| OP_ADD
		| OP_CALL
		| OP_GE
		| OP_GT
		| OP_EQ
		| OP_LT
		| OP_LE
		| OP_ADICT
		| OP_DICT
		| OP_FUNC
		| OP_INDEX
		| OP_INTERPOLATE
		| OP_JUMP_LAZY
		| OP_JUMP_FUNC
		| OP_LIST
		| OP_NEG
		| OP_NUM
		| OP_REF
		| OP_REF_REL
		| OP_STR
}

iota! {
	const DI_PUB: u8 = iota;
		| DI_LOCAL
}

iota! {
	const ARG_REQ: u8 = iota;
		| ARG_OPT
		| ARG_ONE
		| ARG_DICT
		| ARG_LIST
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
	
	fn scope_add(&mut self, name: String, local: bool) -> usize {
		let id = if local {
			self.last_local += 1;
			self.last_local
		} else {
			0
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
	
	fn write_op(&mut self, op: u8) -> usize {
		// eprintln!("OP 0x{:02x} written", op);
		self.write(Some(op))
	}
	
	fn write_str(&mut self, s: &str) -> usize {
		let off = self.write_usize(s.len());
		self.write(s.bytes());
		off
	}
	
	fn start_jump(&mut self, typ: u8) -> usize {
		self.write_op(typ);
		return self.write_u64(0)
	}
	
	fn set_jump(&mut self, jump: usize) {
		let target = self.bytes.len() as u64;
		byteorder::LittleEndian::write_u64(&mut self.bytes[jump..(jump+8)], target);
	}
}

fn compile_expr(ctx: &mut CompileContext, ast: ::Almost) -> Result<usize,String> {
	// eprintln!("Compiling at {}: {:?}", ctx.bytes.len(), ast);
	match ast {
		::Almost::Add(_, left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(OP_ADD);
			Ok(off)
		}
		::Almost::Call(_, left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(OP_CALL);
			Ok(off)
		}
		::Almost::GreatEq(left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(OP_GE);
			Ok(off)
		}
		::Almost::Great(left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(OP_GT);
			Ok(off)
		}
		::Almost::Eq(left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(OP_EQ);
			Ok(off)
		}
		::Almost::Less(left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(OP_LT);
			Ok(off)
		}
		::Almost::LessEq(left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(OP_LE);
			Ok(off)
		}
		::Almost::ADict(key, element) => {
			let jump = ctx.start_jump(OP_JUMP_LAZY);
			let childoff = compile(ctx, Rc::try_unwrap(element).unwrap())?;
			ctx.set_jump(jump);
			let off = ctx.write_op(OP_ADICT);
			ctx.write_usize(childoff);
			ctx.write_str(&key);
			Ok(off)
		}
		::Almost::Dict(elements) => {
			let jump = ctx.start_jump(OP_JUMP_LAZY);
			
			ctx.scope_open();
			
			let ids = elements.iter().map(|element|
				ctx.scope_add(element.key.key.clone(), element.key.namespace != 0))
				.collect::<Vec<_>>();
			
			let mut offsets = Vec::with_capacity(elements.len());
			for (element, id) in elements.into_iter().zip(ids) {
				offsets.push((
					element.key,
					id,
					compile(ctx, Rc::try_unwrap(element.val).unwrap())?));
			}
			
			ctx.scope_close();
			
			ctx.set_jump(jump);
			let off = ctx.write_op(OP_DICT);
			ctx.write_u64(offsets.len() as u64);
			for (key, id, offset) in offsets {
				ctx.write_str(&key.key);
				if key.namespace == 0 {
					ctx.write_u8(DI_PUB);
				} else {
					ctx.write_u8(DI_LOCAL);
					ctx.write_usize(id);
				}
				ctx.write_usize(offset);
			}
			Ok(off)
		}
		::Almost::Func(data) => {
			let jump = ctx.start_jump(OP_JUMP_FUNC);
			let argoff;
			
			ctx.scope_open();
			let ::func::FuncData{arg, body} = Rc::try_unwrap(data).unwrap();
			match arg {
				::func::Arg::One(arg) => {
					argoff = ctx.write_u8(ARG_ONE);
					ctx.write_str(&arg);
					ctx.scope_add(arg.clone(), false);
				}
				::func::Arg::Dict(mut args) => {
					for arg in &args {
						ctx.scope_add(arg.0.clone(), false);
					}
					let args = args.into_iter().map(|(k, req, v)| {
							if req {
								(k, req, 0)
							} else {
								(k, req, compile(ctx, v).unwrap())
							}
						}).collect::<Vec<_>>();
					
					argoff = ctx.write_u8(ARG_DICT);
					ctx.write_usize(args.len());
					for (k, req, voff) in args {
						ctx.write_str(&k);
						if req {
							ctx.write_u8(ARG_REQ);
						} else {
							ctx.write_u8(ARG_OPT);
							ctx.write_usize(voff);
						}
					}
				}
				::func::Arg::List(args) => {
					for arg in &args {
						ctx.scope_add(arg.0.clone(), false);
					}
					let args = args.into_iter().map(|(k, req, v)| {
							if req {
								(k, req, 0)
							} else {
								(k, req, compile(ctx, v).unwrap())
							}
						}).collect::<Vec<_>>();
					
					argoff = ctx.write_u8(ARG_LIST);
					ctx.write_usize(args.len());
					for (k, req, voff) in args {
						ctx.write_str(&k);
						if req {
							ctx.write_u8(ARG_REQ);
						} else {
							ctx.write_u8(ARG_OPT);
							ctx.write_usize(voff);
						}
					}
				}
			}
			compile(ctx, body)?;
			ctx.scope_close();
			
			ctx.set_jump(jump);
			let off = ctx.write_op(OP_FUNC);
			ctx.write_usize(argoff);
			Ok(off)
		}
		::Almost::Index(_, left, right) => {
			let off = compile_expr(ctx, *left)?;
			compile_expr(ctx, *right)?;
			ctx.write_op(OP_INDEX);
			Ok(off)
		}
		::Almost::List(elements) => {
			let jump = ctx.start_jump(OP_JUMP_LAZY);
			
			let offsets = elements.into_iter().map(|element| 
				compile(ctx, Rc::try_unwrap(element).unwrap()))
				.collect::<Vec<_>>();
			
			ctx.set_jump(jump);
			let off = ctx.write_op(OP_LIST);
			ctx.write_usize(offsets.len());
			for offset in offsets {
				ctx.write_usize(offset?);
			}
			Ok(off)
		},
		::Almost::Neg(_, v) => {
			let off = compile_expr(ctx, *v)?;
			ctx.write_op(OP_NEG);
			Ok(off)
		}
		::Almost::Nil => compile_global(ctx, 0),
		::Almost::Num(n) => {
			let off = ctx.write_op(OP_NUM);
			ctx.write_f64(n);
			Ok(off)
		}
		::Almost::Ref(loc, name) => {
			if let Some((depth, id)) = ctx.scope_find(&name) {
				let off = ctx.write_op(OP_REF);
				ctx.write_str(&name); // TODO: remove this.
				ctx.write_usize(depth);
				ctx.write_usize(id);
				Ok(off)
			} else if let Some(id) = ::builtins::builtin_id(&name) { // TODO get global.
				let off = ctx.write_op(OP_GLOBAL);
				ctx.write_usize(id);
				Ok(off)
			} else {
				Err(format!("{:?} Invalid reference {:?}", loc, name))
			}
		}
		::Almost::StructRef(_, depth, key) => {
			if let Some(id) = ctx.scope_find_at(&key.key, depth) {
				let off = ctx.write_op(OP_REF);
				ctx.write_str(&key.key); // TODO: remove this.
				ctx.write_usize(depth);
				ctx.write_usize(id);
				Ok(off)
			} else {
				let off = ctx.write_op(OP_REF_REL);
				ctx.write_str(&key.key); // TODO: remove this.
				ctx.write_usize(depth);
				Ok(off)
			}
		}
		::Almost::Str(parts) => {
			let off = ctx.write_op(OP_STR);
			ctx.write_str("");
			for part in parts {
				match part {
					::StringPart::Exp(s) => {
						compile_expr(ctx, s)?;
					},
					::StringPart::Lit(s) => {
						ctx.write_op(OP_STR);
						ctx.write_str(&s);
					},
				}
				ctx.write_op(OP_INTERPOLATE);
			}
			Ok(off)
		}
		::Almost::StrStatic(s) => {
			let off = ctx.write_op(OP_STR);
			ctx.write_str(&s);
			Ok(off)
		}
		other => unimplemented!("compile({:?})", other),
	}
}

fn compile_global(ctx: &mut CompileContext, global: usize) -> Result<usize,String> {
	let off = ctx.write_op(OP_GLOBAL);
	ctx.write_usize(global);
	Ok(off)
}

fn compile(ctx: &mut CompileContext, ast: ::Almost) -> Result<usize,String> {
	let off = compile_expr(ctx, ast)?;
	ctx.write_op(OP_RET);
	Ok(off)
}

pub fn compile_to_vec(ast: ::Almost) -> Vec<u8> {
	let mut ctx = CompileContext{
		bytes: MAGIC.into_iter()
			.chain(&[0; 8]) // Offset buffer.
			.cloned().collect(),
		scope: Vec::with_capacity(20),
		depth: 0,
		last_local: 0,
	};
	let start = compile(&mut ctx, ast).unwrap() as u64;
	ctx.bytes[START_OFFSET+0] = (start >> 8*0) as u8;
	ctx.bytes[START_OFFSET+1] = (start >> 8*1) as u8;
	ctx.bytes[START_OFFSET+2] = (start >> 8*2) as u8;
	ctx.bytes[START_OFFSET+3] = (start >> 8*3) as u8;
	ctx.bytes[START_OFFSET+4] = (start >> 8*4) as u8;
	ctx.bytes[START_OFFSET+5] = (start >> 8*5) as u8;
	ctx.bytes[START_OFFSET+6] = (start >> 8*6) as u8;
	ctx.bytes[START_OFFSET+7] = (start >> 8*7) as u8;
	
	ctx.bytes
}

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
	eprintln!("Executing @ {}", pc);
	let mut cursor = std::io::Cursor::new(&module.code[..]);
	cursor.seek(std::io::SeekFrom::Start(pc as u64)).unwrap();
	
	let mut stack = Vec::new();
	loop {
		let op = cursor.read_u8().unwrap();
		eprintln!("Executing OP 0x{:02x} @ {}", op, pc + cursor.position() as usize - 1);
		match op {
			OP_RET => {
				assert_eq!(stack.len(), 1);
				return stack.pop().unwrap()
			},
			OP_GLOBAL => {
				let id = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				stack.push(::builtins::get_id(id));
			}
			OP_ADD => {
				let right = stack.pop().expect("One item in stack for add");
				let left = stack.pop().expect("No items in stack for add");
				stack.push(left.add(right));
			}
			OP_CALL => {
				let right = stack.pop().expect("One item in stack for call");
				let left = stack.pop().expect("No items in stack for call");
				stack.push(left.call(right));
			}
			OP_GE => {
				let r = stack.pop().expect("One item in stack for >=");
				let l = stack.pop().expect("No items in stack for >=");
				stack.push(::bool::get(l.cmp(r)? != std::cmp::Ordering::Less))
			}
			OP_GT => {
				let r = stack.pop().expect("One item in stack for >");
				let l = stack.pop().expect("No items in stack for >");
				stack.push(::bool::get(l.cmp(r)? == std::cmp::Ordering::Greater))
			}
			OP_EQ => {
				let right = stack.pop().expect("One item in stack for ==");
				let left = stack.pop().expect("No items in stack for ==");
				stack.push(::bool::get(left == right));
			}
			OP_LT => {
				let r = stack.pop().expect("One item in stack for <");
				let l = stack.pop().expect("No items in stack for <");
				stack.push(::bool::get(l.cmp(r)? == std::cmp::Ordering::Less))
			}
			OP_LE => {
				let r = stack.pop().expect("One item in stack for <=");
				let l = stack.pop().expect("No items in stack for <=");
				stack.push(::bool::get(l.cmp(r)? != std::cmp::Ordering::Greater))
			}
			OP_ADICT => {
				let childoff = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let key = cursor.read_str();
				
				stack.push(::dict::Dict::new_adict(
					::err::Err::new("ADict plex".into()),
					pstruct.clone(),
					key,
					Rc::new(::Almost::Bytecode(module.clone(), childoff))));
			}
			OP_DICT => {
				let len = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let mut items = Vec::with_capacity(len);
				for _ in 0..len {
					let mut key = ::dict::Key::new(cursor.read_str());
					match cursor.read_u8().unwrap() {
						DI_PUB => {},
						DI_LOCAL => {
							key.namespace = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
							key.namespace += module.unique_id();
						},
						other => panic!("Unknown dict item type 0x{:02x}", other),
					}
					let offset = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
					items.push(::dict::AlmostDictElement{
						key: key,
						val: Rc::new(::Almost::Bytecode(module.clone(), offset)),
					});
				}
				stack.push(::dict::Dict::new(
					::err::Err::new("Butecode plex".into()),
					pstruct.clone(),
					&items));
			}
			OP_FUNC => {
				let bodyoff = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let mut bodycursor = std::io::Cursor::new(&module.code[..]);
				bodycursor.seek(std::io::SeekFrom::Start(bodyoff as u64)).unwrap();
				let arg = match bodycursor.read_u8().unwrap() {
					ARG_ONE => {
						::func::Arg::One(bodycursor.read_str())
					}
					ARG_DICT => {
						let len = bodycursor.read_u64::<EclByteOrder>().unwrap() as usize;
						let mut args = Vec::with_capacity(len);
						for _ in 0..len {
							let k = bodycursor.read_str();
							match bodycursor.read_u8().unwrap() {
								ARG_REQ => args.push((k, true, ::Almost::Nil)),
								ARG_OPT => {
									let o = bodycursor.read_u64::<EclByteOrder>().unwrap() as usize;
									args.push((k, false, ::Almost::Bytecode(module.clone(), o)));
								}
								other => panic!("Expected ARG_REQ or ARG_OPT got {}", other),
							}
						}
						::func::Arg::Dict(args)
					}
					ARG_LIST => {
						let len = bodycursor.read_u64::<EclByteOrder>().unwrap() as usize;
						let mut args = Vec::with_capacity(len);
						for _ in 0..len {
							let k = bodycursor.read_str();
							match bodycursor.read_u8().unwrap() {
								ARG_REQ => args.push((k, true, ::Almost::Nil)),
								ARG_OPT => {
									let o = bodycursor.read_u64::<EclByteOrder>().unwrap() as usize;
									args.push((k, false, ::Almost::Bytecode(module.clone(), o)));
								}
								other => panic!("Expected ARG_REQ or ARG_OPT got {}", other),
							}
						}
						::func::Arg::List(args)
					}
					other => panic!("Unexpected arg type: {}", other),
				};
				let data = Rc::new(::func::FuncData{
					arg: arg,
					body: ::Almost::Bytecode(module.clone(), bodycursor.position() as usize),
				});
				stack.push(::func::Func::new(
					::err::Err::new("func plex".into()),
					pstruct.clone(),
					data));
			}
			OP_INDEX => {
				let key = stack.pop().expect("No items in stack for index");
				let val = stack.pop().expect("One item in stack for index");
				stack.push(val.index(key));
			}
			OP_INTERPOLATE => {
				let b = stack.pop().expect("No items in stack for interpolate");
				let b = b.to_string();
				let b = b.get_str().unwrap();
				
				let a = stack.pop().expect("One item in stack for interpolate");
				let a = a.get_str().expect("Interpolating to something not a string.");
				
				let r = a.to_owned() + b;
				stack.push(::Val::new(r))
			}
			OP_JUMP_FUNC | OP_JUMP_LAZY => {
				let target = cursor.read_u64::<EclByteOrder>().unwrap();
				cursor.seek(std::io::SeekFrom::Start(target)).unwrap();
			}
			OP_LIST => {
				let len = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let mut items = Vec::with_capacity(len);
				for _ in 0..len {
					let offset = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
					items.push(Rc::new(::Almost::Bytecode(module.clone(), offset)));
				}
				stack.push(::list::List::new(
					::err::Err::new("Butecode plex".into()),
					pstruct.clone(),
					&items));
			}
			OP_NEG => {
				let v = stack.pop().expect("No items in stack for neg");
				stack.push(v.neg());
			}
			OP_NUM => {
				stack.push(::Val::new(cursor.read_f64::<EclByteOrder>().unwrap()));
			}
			OP_REF => {
				let key = cursor.read_str();
				let depth = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let mut id = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				if id != 0 {
					id += module.unique_id();
				}
				let key = ::dict::Key::local(id, key);
				stack.push(pstruct.structural_lookup(depth, &key, false)
					.expect("Ref lookup failed"));
			}
			OP_REF_REL => {
				let key = cursor.read_str();
				let depth = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let key = ::dict::Key::new(key);
				stack.push(pstruct.structural_lookup(depth, &key, false)
					.expect("Ref lookup failed"));
			}
			OP_STR => {
				let s = cursor.read_str();
				stack.push(::Val::new(s));
			}
			unknown => panic!("Unknown opcode 0x{:02x}", unknown),
		}
	}
}

pub fn decompile(code: &[u8]) -> Result<String,String> {
	let mut cursor = std::io::Cursor::new(code);
	cursor.seek(std::io::SeekFrom::Start(START_END as u64)).unwrap();
	let mut out = String::with_capacity(code.len() * 8);
	
	while let Ok(op) = cursor.read_u8() {
		match op {
			OP_RET => {
				writeln!(out, "{:08} RET", cursor.position()).unwrap();
			}
			OP_GLOBAL => {
				let id = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				writeln!(out, "{:08} GLOBAL {} {:?}",
					cursor.position(),
					id, ::builtins::get_id(id)).unwrap();
			}
			OP_ADD => {
				writeln!(out, "{:08} ADD", cursor.position()).unwrap();
			}
			OP_CALL => {
				writeln!(out, "{:08} CALL", cursor.position()).unwrap();
			}
			OP_GE => {
				writeln!(out, "{:08} GE", cursor.position()).unwrap();
			}
			OP_GT => {
				writeln!(out, "{:08} GT", cursor.position()).unwrap();
			}
			OP_EQ => {
				writeln!(out, "{:08} EQ", cursor.position()).unwrap();
			}
			OP_LT => {
				writeln!(out, "{:08} LT", cursor.position()).unwrap();
			}
			OP_LE => {
				writeln!(out, "{:08} LE", cursor.position()).unwrap();
			}
			OP_ADICT => {
				let childoff = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let key = cursor.read_str();
				writeln!(out, "{:08} ADICT {:?}@{:08}", cursor.position(), key, childoff).unwrap();
			}
			OP_DICT => {
				let len = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				writeln!(out, "{:08} DICT {} items", cursor.position(), len).unwrap();
				for _ in 0..len {
					write!(out, "     ... ").unwrap();
					let mut key = cursor.read_str();
					match cursor.read_u8().unwrap() {
						DI_PUB => write!(out, "PUB   {:?}", key).unwrap(),
						DI_LOCAL => {
							let mut namespace = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
							write!(out, "LOCAL {:04x} {:?}", namespace, key).unwrap();
						},
						other => {
							writeln!(out, "ERR unknown dict item type 0x{:02x}", other).unwrap();
							return Err(out)
						}
					}
					let off = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
					writeln!(out, " @ {:08}", off).unwrap();
				}
			}
			OP_FUNC => {
				let off = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				writeln!(out, "{:08} FUNC @ {:08}", cursor.position(), off).unwrap();
			}
			OP_INDEX => {
				writeln!(out, "{:08} INDEX", cursor.position()).unwrap();
			}
			OP_INTERPOLATE => {
				writeln!(out, "{:08} INDEX", cursor.position()).unwrap();
			}
			OP_JUMP_FUNC => {
				let target = cursor.read_u64::<EclByteOrder>().unwrap();
				writeln!(out, "{:08} OP_JUMP_FUNC @{:08}", cursor.position(), target).unwrap();
				// TODO: Recompile rather than seek over.
				cursor.seek(std::io::SeekFrom::Start(target)).unwrap();
			}
			OP_JUMP_LAZY => {
				let target = cursor.read_u64::<EclByteOrder>().unwrap();
				writeln!(out, "{:08} JUMP_LAZY @{:08}", cursor.position(), target).unwrap();
			}
			OP_LIST => {
				let len = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				writeln!(out, "{:08} LIST {} items", cursor.position(), len).unwrap();
				for i in 0..len {
					let off = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
					writeln!(out, "     ... {:2} @ {}", i, off).unwrap();
				}
			}
			OP_NEG => {
				writeln!(out, "{:08} NEG", cursor.position()).unwrap();
			}
			OP_NUM => {
				let num = cursor.read_f64::<EclByteOrder>().unwrap();
				writeln!(out, "{:08} NUM {}", cursor.position(), num).unwrap();
			}
			OP_REF => {
				let key = cursor.read_str();
				let depth = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				let mut id = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				writeln!(out, "{:08} REF depth:{} {:?} id:{}",
					cursor.position(), depth, key, id).unwrap();
			}
			OP_REF_REL => {
				let key = cursor.read_str();
				let depth = cursor.read_u64::<EclByteOrder>().unwrap() as usize;
				writeln!(out, "{:08} REF_REL depth:{} {:?}", cursor.position(), depth, key).unwrap();
			}
			OP_STR => {
				let s = cursor.read_str();
				writeln!(out, "{:08} STR {:?}", cursor.position(), s).unwrap();
			}
			other => {
				writeln!(out, "ERR unknown opcode 0x{:02x}", other).unwrap();
				return Err(out)
			}
		}
	}
	
	return Ok(out)
}

#[cfg(test)]
mod tests {
	use super::*;
	
	#[test]
	fn compile_global() {
		assert_eq!(
			compile_to_vec(::Almost::Nil),
			b"ECL\0v001\x10\0\0\0\0\0\0\0\x01\0\0\0\0\0\0\0\0\x00")
	}
}
