extern crate serde;

use std::error;
use std::fmt;
use std::result;

pub type Result<T> = result::Result<T,Error>;

#[derive(Debug)]
pub enum Error {
	Custom(String),
	Io(fmt::Error),
	// InvalidKey(String),
	NonStringKey(String),
	NotImplemented(&'static str),
}

impl fmt::Display for Error {
	// This trait requires `fmt` with this exact signature.
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{:?}", self)
	}
}

impl error::Error for Error {
	fn description(&self) -> &str {
		"Error serializing to lines"
	}
}

impl serde::ser::Error for Error {
	fn custom<T: Into<String>>(msg: T) -> Self {
		Error::Custom(msg.into())
	}
}

pub struct Serializer<W: fmt::Write> {
	out: W,
	path: String,
	path_seperators: Vec<usize>,
}

impl<W: fmt::Write> Serializer<W> {
	pub fn new(out: W) -> Self {
		Serializer { out: out, path: String::new(), path_seperators: Vec::new() }
	}
	
	fn write(&mut self, typ: &str, data: &str) -> Result<()> {
		writeln!(self.out, "{}\t{}\t{}", self.path, typ, data).map_err(|e| Error::Io(e))
	}
	
	fn push(&mut self, segment: &str) {
		let prev_len = self.path.len();
		if !self.path_seperators.is_empty() {
			self.path.push('.');
		}
		self.path += &super::format_key(segment);
		self.path_seperators.push(prev_len);
	}
	
	fn pop(&mut self) {
		self.path.truncate(self.path_seperators.pop().unwrap())
	}
}

impl<W: fmt::Write> serde::Serializer for Serializer<W> {
	type Error = Error;
	
	type SeqState = usize;
	type TupleState = ();
	type TupleStructState = ();
	type TupleVariantState = ();
	type MapState = ();
	type StructState = ();
	type StructVariantState = ();
	
	fn serialize_bool(&mut self, value: bool) -> Result<()> {
		self.write("BOOL", if value { "true" } else { "false" })
	}
	
	fn serialize_isize(&mut self, _value: isize) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_i8(&mut self, _value: i8) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_i16(&mut self, _value: i16) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_i32(&mut self, _value: i32) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_i64(&mut self, _value: i64) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_usize(&mut self, _value: usize) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_u8(&mut self, _value: u8) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_u16(&mut self, _value: u16) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_u32(&mut self, _value: u32) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_u64(&mut self, _value: u64) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_f32(&mut self, _value: f32) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_f64(&mut self, value: f64) -> Result<()> {
		self.write("NUM", &value.to_string())
	}
	
	fn serialize_char(&mut self, _value: char) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_str(&mut self, value: &str) -> Result<()> {
		self.write("STR", &super::escape_string(value))
	}
	
	fn serialize_bytes(&mut self, _value: &[u8]) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_unit(&mut self) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_unit_struct(&mut self, _name: &'static str) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_unit_variant(
		&mut self,
		_name: &'static str,
		_variant_index: usize,
		_variant: &'static str
	) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_newtype_struct<T: serde::Serialize>(
		&mut self, _name: &'static str, _value: T) -> Result<()>
	{
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_newtype_variant<T>(
		&mut self,
		_name: &'static str,
		_variant_index: usize,
		_variant: &'static str,
		_value: T
	) -> Result<()>
		where T: serde::Serialize,
	{
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_none(&mut self) -> Result<()> {
		self.write("NIL", "nil")
	}
	
	fn serialize_some<T>(&mut self, _value: T) -> Result<()>
		where T: serde::Serialize,
	{
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_seq(&mut self, _len: Option<usize>) -> Result<Self::SeqState> {
		try!(self.write("LIST", "["));
		Ok(0)
	}
	
	fn serialize_seq_fixed_size(&mut self, _size: usize) -> Result<Self::SeqState> {
		self.serialize_seq(None)
	}
	
	fn serialize_seq_elt<T: serde::Serialize>(
		&mut self,
		state: &mut Self::SeqState,
		value: T
	) -> Result<()>
		where T: serde::Serialize,
	{
		*state += 1;
		self.push(&state.to_string());
		try!(value.serialize(self));
		self.pop();
		Ok(())
	}
	
	fn serialize_seq_end(&mut self, _state: Self::SeqState) -> Result<()> {
		self.write("LIST", "]")
	}
	
	fn serialize_tuple(&mut self, _len: usize) -> Result<Self::TupleState> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_tuple_elt<T: serde::Serialize>(
		&mut self,
		_state: &mut Self::TupleState,
		_value: T
	) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_tuple_end(&mut self, _state: Self::TupleState) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_tuple_struct(
		&mut self,
		_name: &'static str,
		_len: usize
	) -> Result<Self::TupleStructState> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_tuple_struct_elt<T: serde::Serialize>(
		&mut self,
		_state: &mut Self::TupleStructState,
		_value: T
	) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_tuple_struct_end(&mut self, _state: Self::TupleStructState) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_tuple_variant(
		&mut self,
		_name: &'static str,
		_variant_index: usize,
		_variant: &'static str,
		_len: usize
	) -> Result<Self::TupleVariantState> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_tuple_variant_elt<T: serde::Serialize>(
		&mut self,
		_state: &mut Self::TupleVariantState,
		_value: T
	) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_tuple_variant_end(&mut self, _state: Self::TupleVariantState) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_map(&mut self, _len: Option<usize>) -> Result<Self::MapState> {
		self.write("DICT", "{")
	}
	
	fn serialize_map_key<T: serde::Serialize>(
		&mut self,
		_state: &mut Self::MapState,
		key: T,
	) -> Result<()> {
		key.serialize(&mut KeySerializer { parent: self })
	}
	
	fn serialize_map_value<T: serde::Serialize>(
		&mut self,
		_: &mut Self::MapState,
		value: T,
	) -> Result<()> {
		try!(value.serialize(self));
		self.pop();
		Ok(())
	}
	
	fn serialize_map_end(&mut self, _state: Self::MapState) -> Result<()> {
		self.write("DICT", "}")
	}
	
	fn serialize_struct(
		&mut self,
		_name: &'static str,
		_len: usize
	) -> Result<Self::StructState> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_struct_elt<V: serde::Serialize>(
		&mut self,
		_state: &mut Self::StructState,
		_key: &'static str,
		_value: V
	) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_struct_end(&mut self, _state: Self::StructState) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_struct_variant(
		&mut self,
		_name: &'static str,
		_variant_index: usize,
		_variant: &'static str,
		_len: usize
	) -> Result<Self::StructVariantState> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_struct_variant_elt<V: serde::Serialize>(
		&mut self,
		_state: &mut Self::StructVariantState,
		_key: &'static str,
		_value: V
	) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
	
	fn serialize_struct_variant_end(&mut self, _state: Self::StructVariantState) -> Result<()> {
		Err(Error::NotImplemented(concat!(file!(), ":", line!())))
	}
}

struct KeySerializer<'a, W: fmt::Write + 'a> { parent: &'a mut Serializer<W> }

impl<'a, W: fmt::Write> serde::Serializer for KeySerializer<'a, W> {
	type Error = Error;
	
	type SeqState = ();
	type TupleState = ();
	type TupleStructState = ();
	type TupleVariantState = ();
	type MapState = ();
	type StructState = ();
	type StructVariantState = ();
	
	fn serialize_bool(&mut self, _value: bool) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) } 
	fn serialize_isize(&mut self, _value: isize) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) } 
	fn serialize_i8(&mut self, _value: i8) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) } 
	fn serialize_i16(&mut self, _value: i16) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) } 
	fn serialize_i32(&mut self, _value: i32) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) } 
	fn serialize_i64(&mut self, _value: i64) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) } 
	fn serialize_usize(&mut self, _value: usize) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) } 
	fn serialize_u8(&mut self, _value: u8) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) } 
	fn serialize_u16(&mut self, _value: u16) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) } 
	fn serialize_u32(&mut self, _value: u32) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) } 
	fn serialize_u64(&mut self, _value: u64) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) } 
	fn serialize_f32(&mut self, _value: f32) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) } 
	fn serialize_f64(&mut self, _value: f64) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) } 
	fn serialize_char(&mut self, _value: char) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) } 
	
	fn serialize_str(&mut self, value: &str) -> Result<()> {
		self.parent.push(value);
		Ok(())
	}
	
	fn serialize_bytes(&mut self, _value: &[u8]) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) } 
	fn serialize_unit(&mut self) -> Result<()> { Err(Error::NonStringKey(self.parent.path.clone())) }
	
	fn serialize_unit_struct(&mut self, _name: &'static str) -> Result<()> {
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_unit_variant(
		&mut self,
		_name: &'static str,
		_variant_index: usize,
		_variant: &'static str
	) -> Result<()> {
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_newtype_struct<T: serde::Serialize>(
		&mut self, _name: &'static str, _value: T) -> Result<()>
	{
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_newtype_variant<T>(
		&mut self,
		_name: &'static str,
		_variant_index: usize,
		_variant: &'static str,
		_value: T
	) -> Result<()>
		where T: serde::Serialize,
	{
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_none(&mut self) -> Result<()> {
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_some<T>(&mut self, _value: T) -> Result<()>
		where T: serde::Serialize,
	{
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_seq(&mut self, _len: Option<usize>) -> Result<Self::SeqState> {
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_seq_elt<T: serde::Serialize>(
		&mut self,
		_state: &mut Self::SeqState,
		_value: T
	) -> Result<()>
		where T: serde::Serialize,
	{
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_seq_end(&mut self, _state: Self::SeqState) -> Result<()> {
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_seq_fixed_size(&mut self, _size: usize) -> Result<Self::SeqState> {
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_tuple(&mut self, _len: usize) -> Result<Self::TupleState> {
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_tuple_elt<T: serde::Serialize>(
		&mut self,
		_state: &mut Self::TupleState,
		_value: T
	) -> Result<()> {
		unreachable!()
	}
	
	fn serialize_tuple_end(&mut self, _state: Self::TupleState) -> Result<()> {
		unreachable!()
	}
	
	fn serialize_tuple_struct(
		&mut self,
		_name: &'static str,
		_len: usize
	) -> Result<Self::TupleStructState> {
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_tuple_struct_elt<T: serde::Serialize>(
		&mut self,
		_state: &mut Self::TupleStructState,
		_value: T
	) -> Result<()> {
		unreachable!()
	}
	
	fn serialize_tuple_struct_end(&mut self, _state: Self::TupleStructState) -> Result<()> {
		unreachable!()
	}
	
	fn serialize_tuple_variant(
		&mut self,
		_name: &'static str,
		_variant_index: usize,
		_variant: &'static str,
		_len: usize
	) -> Result<Self::TupleVariantState> {
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_tuple_variant_elt<T: serde::Serialize>(
		&mut self,
		_state: &mut Self::TupleVariantState,
		_value: T
	) -> Result<()> {
		unreachable!()
	}
	
	fn serialize_tuple_variant_end(&mut self, _state: Self::TupleVariantState) -> Result<()> {
		unreachable!()
	}
	
	fn serialize_map(&mut self, _len: Option<usize>) -> Result<Self::MapState> {
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_map_key<T: serde::Serialize>(
		&mut self,
		_state: &mut Self::MapState,
		_key: T,
	) -> Result<()> {
		unreachable!()
	}
	
	fn serialize_map_value<T: serde::Serialize>(
		&mut self,
		_: &mut Self::MapState,
		_value: T,
	) -> Result<()> {
		unreachable!()
	}
	
	fn serialize_map_end(&mut self, _state: Self::MapState) -> Result<()> {
		unreachable!()
	}
	
	fn serialize_struct(
		&mut self,
		_name: &'static str,
		_len: usize
	) -> Result<Self::StructState> {
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_struct_elt<V: serde::Serialize>(
		&mut self,
		_state: &mut Self::StructState,
		_key: &'static str,
		_value: V
	) -> Result<()> {
		unreachable!()
	}
	
	fn serialize_struct_end(&mut self, _state: Self::StructState) -> Result<()> {
		unreachable!()
	}
	
	fn serialize_struct_variant(
		&mut self,
		_name: &'static str,
		_variant_index: usize,
		_variant: &'static str,
		_len: usize
	) -> Result<Self::StructVariantState> {
		Err(Error::NonStringKey(self.parent.path.clone()))
	}
	
	fn serialize_struct_variant_elt<V: serde::Serialize>(
		&mut self,
		_state: &mut Self::StructVariantState,
		_key: &'static str,
		_value: V
	) -> Result<()> {
		unreachable!()
	}
	
	fn serialize_struct_variant_end(&mut self, _state: Self::StructVariantState) -> Result<()> {
		unreachable!()
	}
}
