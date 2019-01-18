use std::fmt;
use std::io;

use serde::{Serialize, Serializer};
#[cfg(feature = "json")]
use serde_json::Serializer as JsonSerializer;

use crate::{Argument, FormatType};

#[cfg(feature = "json")]
type CompactJsonSerializer<W> = JsonSerializer<W, serde_json::ser::CompactFormatter>;
#[cfg(feature = "json")]
type PrettyJsonSerializer<W> = JsonSerializer<W, serde_json::ser::PrettyFormatter<'static>>;

pub type FormatFn<T> = fn(&T, fmt: &mut fmt::Formatter) -> fmt::Result;

struct FmtProxy<'a> {
    data: &'a (),
    func: FormatFn<()>,
}

impl<'a> FmtProxy<'a> {
    pub fn new<T>(data: &'a T, func: FormatFn<T>) -> Self {
        unsafe {
            FmtProxy {
                data: &*(data as *const T as *const ()),
                func: std::mem::transmute(func),
            }
        }
    }
}

impl fmt::Display for FmtProxy<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        (self.func)(self.data, fmt)
    }
}

#[derive(Debug)]
pub enum FormatError {
    Type(FormatType),
    Serde(String),
    Io(io::Error),
}

impl fmt::Display for FormatError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FormatError::Type(format) => write!(f, "cannot format as {}", format),
            FormatError::Serde(error) => write!(f, "{}", error),
            FormatError::Io(error) => write!(f, "{}", error),
        }
    }
}

impl std::error::Error for FormatError {}

impl serde::ser::Error for FormatError {
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        FormatError::Serde(msg.to_string())
    }
}

#[cfg(feature = "json")]
impl From<serde_json::Error> for FormatError {
    fn from(error: serde_json::Error) -> Self {
        FormatError::Serde(error.to_string())
    }
}

enum FormatterTarget<W> {
    Write(W),
    #[cfg(feature = "json")]
    Compact(CompactJsonSerializer<W>),
    #[cfg(feature = "json")]
    Pretty(PrettyJsonSerializer<W>),
}

impl<W> FormatterTarget<W>
where
    W: io::Write,
{
    pub fn new(write: W) -> Self {
        FormatterTarget::Write(write)
    }

    #[cfg(feature = "json")]
    pub fn compact(write: W) -> Self {
        FormatterTarget::Compact(JsonSerializer::new(write))
    }

    #[cfg(feature = "json")]
    pub fn pretty(write: W) -> Self {
        FormatterTarget::Pretty(JsonSerializer::pretty(write))
    }

    pub fn into_inner(self) -> W {
        match self {
            FormatterTarget::Write(write) => write,
            #[cfg(feature = "json")]
            FormatterTarget::Compact(write) => write.into_inner(),
            #[cfg(feature = "json")]
            FormatterTarget::Pretty(write) => write.into_inner(),
        }
    }

    pub fn as_write(&mut self) -> &mut W {
        self.convert(FormatterTarget::new);
        #[cfg_attr(not(feature = "json"), allow(unreachable_patterns))]
        match self {
            FormatterTarget::Write(inner) => inner,
            _ => unreachable!(),
        }
    }

    #[cfg(feature = "json")]
    pub fn as_compact(&mut self) -> &mut CompactJsonSerializer<W> {
        self.convert(FormatterTarget::compact);
        #[cfg_attr(not(feature = "json"), allow(unreachable_patterns))]
        match self {
            FormatterTarget::Compact(inner) => inner,
            _ => unreachable!(),
        }
    }

    #[cfg(feature = "json")]
    pub fn as_pretty(&mut self) -> &mut PrettyJsonSerializer<W> {
        self.convert(FormatterTarget::pretty);
        match self {
            FormatterTarget::Pretty(inner) => inner,
            _ => unreachable!(),
        }
    }

    fn convert<F>(&mut self, f: F)
    where
        F: FnOnce(W) -> Self,
    {
        *self = f(std::mem::replace(self, unsafe { std::mem::uninitialized() }).into_inner());
    }
}

pub struct Formatter<W> {
    target: FormatterTarget<W>,
    ty: FormatType,
    alternate: bool,
}

impl<W> Formatter<W>
where
    W: io::Write,
{
    pub fn new(write: W) -> Self {
        Formatter {
            target: FormatterTarget::new(write),
            ty: FormatType::Display,
            alternate: false,
        }
    }

    pub fn with_type(mut self, ty: FormatType) -> Self {
        self.ty = ty;
        self
    }

    pub fn with_alternate(mut self, alternate: bool) -> Self {
        self.alternate = alternate;
        self
    }

    pub fn format(&mut self, value: Argument<'_>) -> Result<(), FormatError> {
        // TODO: Serde calls erased_serialize here, which always passes the error through
        // Error::custom. In this process we lose the original error.
        value.serialize(self)
    }

    #[cfg(feature = "json")]
    fn serialize<D: Serialize>(&mut self, value: &D) -> Result<(), FormatError> {
        if self.alternate {
            value.serialize(self.target.as_pretty()).map_err(Into::into)
        } else {
            value
                .serialize(self.target.as_compact())
                .map_err(Into::into)
        }
    }

    #[cfg(not(feature = "json"))]
    fn serialize<D: Serialize>(&mut self, _value: &D) -> Result<(), FormatError> {
        Err(FormatError::Type(FormatType::Object))
    }

    fn fmt_internal<T>(&mut self, value: &T, fmt: FormatFn<T>) -> Result<(), FormatError> {
        let proxy = FmtProxy::new(value, fmt);

        if self.alternate {
            write!(self.target.as_write(), "{:#}", proxy).map_err(FormatError::Io)
        } else {
            write!(self.target.as_write(), "{}", proxy).map_err(FormatError::Io)
        }
    }

    // TODO: Implement this
    #[allow(unused)]
    fn debug<D: fmt::Debug>(&mut self, value: &D) -> Result<(), FormatError> {
        self.fmt_internal(value, fmt::Debug::fmt)
    }

    fn display<D: fmt::Display>(&mut self, value: &D) -> Result<(), FormatError> {
        self.fmt_internal(value, fmt::Display::fmt)
    }

    fn octal<D: fmt::Octal>(&mut self, value: &D) -> Result<(), FormatError> {
        self.fmt_internal(value, fmt::Octal::fmt)
    }

    fn lower_hex<D: fmt::LowerHex>(&mut self, value: &D) -> Result<(), FormatError> {
        self.fmt_internal(value, fmt::LowerHex::fmt)
    }

    fn upper_hex<D: fmt::UpperHex>(&mut self, value: &D) -> Result<(), FormatError> {
        self.fmt_internal(value, fmt::UpperHex::fmt)
    }

    fn pointer<D: fmt::Pointer>(&mut self, value: &D) -> Result<(), FormatError> {
        self.fmt_internal(value, fmt::Pointer::fmt)
    }

    fn binary<D: fmt::Binary>(&mut self, value: &D) -> Result<(), FormatError> {
        self.fmt_internal(value, fmt::Binary::fmt)
    }

    fn lower_exp<D: fmt::LowerExp>(&mut self, value: &D) -> Result<(), FormatError> {
        self.fmt_internal(value, fmt::LowerExp::fmt)
    }

    fn upper_exp<D: fmt::UpperExp>(&mut self, value: &D) -> Result<(), FormatError> {
        self.fmt_internal(value, fmt::UpperExp::fmt)
    }
}

#[cfg(feature = "json")]
pub enum SerializeSeq<'a, W: io::Write> {
    Compact(<&'a mut CompactJsonSerializer<W> as Serializer>::SerializeSeq),
    Pretty(<&'a mut PrettyJsonSerializer<W> as Serializer>::SerializeSeq),
}

#[cfg(feature = "json")]
impl<'a, W: io::Write> serde::ser::SerializeSeq for SerializeSeq<'a, W> {
    type Ok = ();
    type Error = FormatError;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: Serialize,
    {
        match self {
            SerializeSeq::Compact(compound) => compound.serialize_element(value),
            SerializeSeq::Pretty(compound) => compound.serialize_element(value),
        }
        .map_err(Into::into)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        match self {
            SerializeSeq::Compact(compound) => compound.end(),
            SerializeSeq::Pretty(compound) => compound.end(),
        }
        .map_err(Into::into)
    }
}

#[cfg(feature = "json")]
pub enum SerializeTuple<'a, W: io::Write> {
    Compact(<&'a mut CompactJsonSerializer<W> as Serializer>::SerializeTuple),
    Pretty(<&'a mut PrettyJsonSerializer<W> as Serializer>::SerializeTuple),
}

#[cfg(feature = "json")]
impl<'a, W: io::Write> serde::ser::SerializeTuple for SerializeTuple<'a, W> {
    type Ok = ();
    type Error = FormatError;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: Serialize,
    {
        match self {
            SerializeTuple::Compact(compound) => compound.serialize_element(value),
            SerializeTuple::Pretty(compound) => compound.serialize_element(value),
        }
        .map_err(Into::into)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        match self {
            SerializeTuple::Compact(compound) => compound.end(),
            SerializeTuple::Pretty(compound) => compound.end(),
        }
        .map_err(Into::into)
    }
}

#[cfg(feature = "json")]
pub enum SerializeTupleStruct<'a, W: io::Write> {
    Compact(<&'a mut CompactJsonSerializer<W> as Serializer>::SerializeTupleStruct),
    Pretty(<&'a mut PrettyJsonSerializer<W> as Serializer>::SerializeTupleStruct),
}

#[cfg(feature = "json")]
impl<'a, W: io::Write> serde::ser::SerializeTupleStruct for SerializeTupleStruct<'a, W> {
    type Ok = ();
    type Error = FormatError;

    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: Serialize,
    {
        match self {
            SerializeTupleStruct::Compact(compound) => compound.serialize_field(value),
            SerializeTupleStruct::Pretty(compound) => compound.serialize_field(value),
        }
        .map_err(Into::into)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        match self {
            SerializeTupleStruct::Compact(compound) => compound.end(),
            SerializeTupleStruct::Pretty(compound) => compound.end(),
        }
        .map_err(Into::into)
    }
}

#[cfg(feature = "json")]
pub enum SerializeTupleVariant<'a, W: io::Write> {
    Compact(<&'a mut CompactJsonSerializer<W> as Serializer>::SerializeTupleVariant),
    Pretty(<&'a mut PrettyJsonSerializer<W> as Serializer>::SerializeTupleVariant),
}

#[cfg(feature = "json")]
impl<'a, W: io::Write> serde::ser::SerializeTupleVariant for SerializeTupleVariant<'a, W> {
    type Ok = ();
    type Error = FormatError;

    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: Serialize,
    {
        match self {
            SerializeTupleVariant::Compact(compound) => compound.serialize_field(value),
            SerializeTupleVariant::Pretty(compound) => compound.serialize_field(value),
        }
        .map_err(Into::into)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        match self {
            SerializeTupleVariant::Compact(compound) => compound.end(),
            SerializeTupleVariant::Pretty(compound) => compound.end(),
        }
        .map_err(Into::into)
    }
}

#[cfg(feature = "json")]
pub enum SerializeMap<'a, W: io::Write> {
    Compact(<&'a mut CompactJsonSerializer<W> as Serializer>::SerializeMap),
    Pretty(<&'a mut PrettyJsonSerializer<W> as Serializer>::SerializeMap),
}

#[cfg(feature = "json")]
impl<'a, W: io::Write> serde::ser::SerializeMap for SerializeMap<'a, W> {
    type Ok = ();
    type Error = FormatError;

    fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: Serialize,
    {
        match self {
            SerializeMap::Compact(compound) => compound.serialize_key(key),
            SerializeMap::Pretty(compound) => compound.serialize_key(key),
        }
        .map_err(Into::into)
    }

    fn serialize_value<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: Serialize,
    {
        match self {
            SerializeMap::Compact(compound) => compound.serialize_value(value),
            SerializeMap::Pretty(compound) => compound.serialize_value(value),
        }
        .map_err(Into::into)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        match self {
            SerializeMap::Compact(compound) => compound.end(),
            SerializeMap::Pretty(compound) => compound.end(),
        }
        .map_err(Into::into)
    }

    fn serialize_entry<K: ?Sized, V: ?Sized>(
        &mut self,
        key: &K,
        value: &V,
    ) -> Result<(), Self::Error>
    where
        K: Serialize,
        V: Serialize,
    {
        match self {
            SerializeMap::Compact(compound) => compound.serialize_entry(key, value),
            SerializeMap::Pretty(compound) => compound.serialize_entry(key, value),
        }
        .map_err(Into::into)
    }
}

#[cfg(feature = "json")]
pub enum SerializeStruct<'a, W: io::Write> {
    Compact(<&'a mut CompactJsonSerializer<W> as Serializer>::SerializeStruct),
    Pretty(<&'a mut PrettyJsonSerializer<W> as Serializer>::SerializeStruct),
}

#[cfg(feature = "json")]
impl<'a, W: io::Write> serde::ser::SerializeStruct for SerializeStruct<'a, W> {
    type Ok = ();
    type Error = FormatError;

    fn serialize_field<T: ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: Serialize,
    {
        match self {
            SerializeStruct::Compact(compound) => compound.serialize_field(key, value),
            SerializeStruct::Pretty(compound) => compound.serialize_field(key, value),
        }
        .map_err(Into::into)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        match self {
            SerializeStruct::Compact(compound) => compound.end(),
            SerializeStruct::Pretty(compound) => compound.end(),
        }
        .map_err(Into::into)
    }

    fn skip_field(&mut self, key: &'static str) -> Result<(), Self::Error> {
        match self {
            SerializeStruct::Compact(compound) => compound.skip_field(key),
            SerializeStruct::Pretty(compound) => compound.skip_field(key),
        }
        .map_err(Into::into)
    }
}

#[cfg(feature = "json")]
pub enum SerializeStructVariant<'a, W: io::Write> {
    Compact(<&'a mut CompactJsonSerializer<W> as Serializer>::SerializeStructVariant),
    Pretty(<&'a mut PrettyJsonSerializer<W> as Serializer>::SerializeStructVariant),
}

#[cfg(feature = "json")]
impl<'a, W: io::Write> serde::ser::SerializeStructVariant for SerializeStructVariant<'a, W> {
    type Ok = ();
    type Error = FormatError;

    fn serialize_field<T: ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: Serialize,
    {
        match self {
            SerializeStructVariant::Compact(compound) => compound.serialize_field(key, value),
            SerializeStructVariant::Pretty(compound) => compound.serialize_field(key, value),
        }
        .map_err(Into::into)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        match self {
            SerializeStructVariant::Compact(compound) => compound.end(),
            SerializeStructVariant::Pretty(compound) => compound.end(),
        }
        .map_err(Into::into)
    }

    fn skip_field(&mut self, key: &'static str) -> Result<(), Self::Error> {
        match self {
            SerializeStructVariant::Compact(compound) => compound.skip_field(key),
            SerializeStructVariant::Pretty(compound) => compound.skip_field(key),
        }
        .map_err(Into::into)
    }
}

impl<'a, W> Serializer for &'a mut Formatter<W>
where
    W: io::Write,
{
    type Ok = ();
    type Error = FormatError;

    #[cfg(feature = "json")]
    type SerializeSeq = SerializeSeq<'a, W>;
    #[cfg(feature = "json")]
    type SerializeTuple = SerializeTuple<'a, W>;
    #[cfg(feature = "json")]
    type SerializeTupleStruct = SerializeTupleStruct<'a, W>;
    #[cfg(feature = "json")]
    type SerializeTupleVariant = SerializeTupleVariant<'a, W>;
    #[cfg(feature = "json")]
    type SerializeMap = SerializeMap<'a, W>;
    #[cfg(feature = "json")]
    type SerializeStruct = SerializeStruct<'a, W>;
    #[cfg(feature = "json")]
    type SerializeStructVariant = SerializeStructVariant<'a, W>;

    #[cfg(not(feature = "json"))]
    type SerializeSeq = serde::ser::Impossible<Self::Ok, Self::Error>;
    #[cfg(not(feature = "json"))]
    type SerializeTuple = serde::ser::Impossible<Self::Ok, Self::Error>;
    #[cfg(not(feature = "json"))]
    type SerializeTupleStruct = serde::ser::Impossible<Self::Ok, Self::Error>;
    #[cfg(not(feature = "json"))]
    type SerializeTupleVariant = serde::ser::Impossible<Self::Ok, Self::Error>;
    #[cfg(not(feature = "json"))]
    type SerializeMap = serde::ser::Impossible<Self::Ok, Self::Error>;
    #[cfg(not(feature = "json"))]
    type SerializeStruct = serde::ser::Impossible<Self::Ok, Self::Error>;
    #[cfg(not(feature = "json"))]
    type SerializeStructVariant = serde::ser::Impossible<Self::Ok, Self::Error>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        match self.ty {
            FormatType::Display => self.display(&v),
            FormatType::Object => self.serialize(&v),
            other => Err(FormatError::Type(other)),
        }
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        match self.ty {
            FormatType::Display => self.display(&v),
            FormatType::Object => self.serialize(&v),
            FormatType::Octal => self.octal(&v),
            FormatType::LowerHex => self.lower_hex(&v),
            FormatType::UpperHex => self.upper_hex(&v),
            FormatType::Binary => self.binary(&v),
            other => Err(FormatError::Type(other)),
        }
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        match self.ty {
            FormatType::Display => self.display(&v),
            FormatType::Object => self.serialize(&v),
            FormatType::Octal => self.octal(&v),
            FormatType::LowerHex => self.lower_hex(&v),
            FormatType::UpperHex => self.upper_hex(&v),
            FormatType::Binary => self.binary(&v),
            other => Err(FormatError::Type(other)),
        }
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        match self.ty {
            FormatType::Display => self.display(&v),
            FormatType::Object => self.serialize(&v),
            FormatType::Octal => self.octal(&v),
            FormatType::LowerHex => self.lower_hex(&v),
            FormatType::UpperHex => self.upper_hex(&v),
            FormatType::Binary => self.binary(&v),
            other => Err(FormatError::Type(other)),
        }
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        match self.ty {
            FormatType::Display => self.display(&v),
            FormatType::Object => self.serialize(&v),
            FormatType::Octal => self.octal(&v),
            FormatType::LowerHex => self.lower_hex(&v),
            FormatType::UpperHex => self.upper_hex(&v),
            FormatType::Binary => self.binary(&v),
            other => Err(FormatError::Type(other)),
        }
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        match self.ty {
            FormatType::Display => self.display(&v),
            FormatType::Object => self.serialize(&v),
            FormatType::Octal => self.octal(&v),
            FormatType::LowerHex => self.lower_hex(&v),
            FormatType::UpperHex => self.upper_hex(&v),
            FormatType::Binary => self.binary(&v),
            other => Err(FormatError::Type(other)),
        }
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        match self.ty {
            FormatType::Display => self.display(&v),
            FormatType::Object => self.serialize(&v),
            FormatType::Octal => self.octal(&v),
            FormatType::LowerHex => self.lower_hex(&v),
            FormatType::UpperHex => self.upper_hex(&v),
            FormatType::Binary => self.binary(&v),
            other => Err(FormatError::Type(other)),
        }
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        match self.ty {
            FormatType::Display => self.display(&v),
            FormatType::Object => self.serialize(&v),
            FormatType::Octal => self.octal(&v),
            FormatType::LowerHex => self.lower_hex(&v),
            FormatType::UpperHex => self.upper_hex(&v),
            FormatType::Binary => self.binary(&v),
            other => Err(FormatError::Type(other)),
        }
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        match self.ty {
            FormatType::Display => self.display(&v),
            FormatType::Object => self.serialize(&v),
            FormatType::Octal => self.octal(&v),
            FormatType::LowerHex => self.lower_hex(&v),
            FormatType::UpperHex => self.upper_hex(&v),
            FormatType::Binary => self.binary(&v),
            other => Err(FormatError::Type(other)),
        }
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        match self.ty {
            FormatType::Display => self.display(&v),
            FormatType::Object => self.serialize(&v),
            FormatType::LowerExp => self.lower_exp(&v),
            FormatType::UpperExp => self.upper_exp(&v),
            other => Err(FormatError::Type(other)),
        }
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        match self.ty {
            FormatType::Display => self.display(&v),
            FormatType::Object => self.serialize(&v),
            FormatType::LowerExp => self.lower_exp(&v),
            FormatType::UpperExp => self.upper_exp(&v),
            other => Err(FormatError::Type(other)),
        }
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        match self.ty {
            FormatType::Display => self.display(&v),
            FormatType::Object => self.serialize(&v),
            other => Err(FormatError::Type(other)),
        }
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        match self.ty {
            FormatType::Display => self.display(&v),
            FormatType::Object => self.serialize(&v),
            FormatType::Pointer => self.pointer(&v),
            other => Err(FormatError::Type(other)),
        }
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        match self.ty {
            FormatType::Object => self.serialize(&v),
            FormatType::Pointer => self.pointer(&v),
            other => Err(FormatError::Type(other)),
        }
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit()
    }

    fn serialize_some<T: ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        match self.ty {
            FormatType::Display => self.display(&"null"),
            FormatType::Object => self.serialize(&()),
            other => Err(FormatError::Type(other)),
        }
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T: ?Sized>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        Err(FormatError::Type(self.ty))
    }

    #[cfg(feature = "json")]
    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        if self.ty != FormatType::Object && self.ty != FormatType::Display {
            return Err(FormatError::Type(self.ty));
        }

        if self.alternate {
            self.target
                .as_pretty()
                .serialize_seq(len)
                .map(SerializeSeq::Pretty)
                .map_err(Into::into)
        } else {
            self.target
                .as_compact()
                .serialize_seq(len)
                .map(SerializeSeq::Compact)
                .map_err(Into::into)
        }
    }

    #[cfg(feature = "json")]
    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        if self.ty != FormatType::Object && self.ty != FormatType::Display {
            return Err(FormatError::Type(self.ty));
        }

        if self.alternate {
            self.target
                .as_pretty()
                .serialize_tuple(len)
                .map(SerializeTuple::Pretty)
                .map_err(Into::into)
        } else {
            self.target
                .as_compact()
                .serialize_tuple(len)
                .map(SerializeTuple::Compact)
                .map_err(Into::into)
        }
    }

    #[cfg(feature = "json")]
    fn serialize_tuple_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        if self.ty != FormatType::Object && self.ty != FormatType::Display {
            return Err(FormatError::Type(self.ty));
        }

        if self.alternate {
            self.target
                .as_pretty()
                .serialize_tuple_struct(name, len)
                .map(SerializeTupleStruct::Pretty)
                .map_err(Into::into)
        } else {
            self.target
                .as_compact()
                .serialize_tuple_struct(name, len)
                .map(SerializeTupleStruct::Compact)
                .map_err(Into::into)
        }
    }

    #[cfg(feature = "json")]
    fn serialize_tuple_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        if self.ty != FormatType::Object && self.ty != FormatType::Display {
            return Err(FormatError::Type(self.ty));
        }

        if self.alternate {
            self.target
                .as_pretty()
                .serialize_tuple_variant(name, variant_index, variant, len)
                .map(SerializeTupleVariant::Pretty)
                .map_err(Into::into)
        } else {
            self.target
                .as_compact()
                .serialize_tuple_variant(name, variant_index, variant, len)
                .map(SerializeTupleVariant::Compact)
                .map_err(Into::into)
        }
    }

    #[cfg(feature = "json")]
    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        if self.ty != FormatType::Object && self.ty != FormatType::Display {
            return Err(FormatError::Type(self.ty));
        }

        if self.alternate {
            self.target
                .as_pretty()
                .serialize_map(len)
                .map(SerializeMap::Pretty)
                .map_err(Into::into)
        } else {
            self.target
                .as_compact()
                .serialize_map(len)
                .map(SerializeMap::Compact)
                .map_err(Into::into)
        }
    }

    #[cfg(feature = "json")]
    fn serialize_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        if self.ty != FormatType::Object && self.ty != FormatType::Display {
            return Err(FormatError::Type(self.ty));
        }

        if self.alternate {
            self.target
                .as_pretty()
                .serialize_struct(name, len)
                .map(SerializeStruct::Pretty)
                .map_err(Into::into)
        } else {
            self.target
                .as_compact()
                .serialize_struct(name, len)
                .map(SerializeStruct::Compact)
                .map_err(Into::into)
        }
    }

    #[cfg(feature = "json")]
    fn serialize_struct_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        if self.ty != FormatType::Object && self.ty != FormatType::Display {
            return Err(FormatError::Type(self.ty));
        }

        if self.alternate {
            self.target
                .as_pretty()
                .serialize_struct_variant(name, variant_index, variant, len)
                .map(SerializeStructVariant::Pretty)
                .map_err(Into::into)
        } else {
            self.target
                .as_compact()
                .serialize_struct_variant(name, variant_index, variant, len)
                .map(SerializeStructVariant::Compact)
                .map_err(Into::into)
        }
    }

    #[cfg(not(feature = "json"))]
    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Err(FormatError::Type(self.ty))
    }

    #[cfg(not(feature = "json"))]
    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(FormatError::Type(self.ty))
    }

    #[cfg(not(feature = "json"))]
    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(FormatError::Type(self.ty))
    }

    #[cfg(not(feature = "json"))]
    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(FormatError::Type(self.ty))
    }

    #[cfg(not(feature = "json"))]
    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Err(FormatError::Type(self.ty))
    }

    #[cfg(not(feature = "json"))]
    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Err(FormatError::Type(self.ty))
    }

    #[cfg(not(feature = "json"))]
    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(FormatError::Type(self.ty))
    }
}
