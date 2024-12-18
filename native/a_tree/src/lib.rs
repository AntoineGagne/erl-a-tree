use a_tree::{ATree, ATreeError, AttributeDefinition, EventError};
use rustler::{
    resource_impl, Atom, Error, NifResult, NifUnitEnum, NifUntaggedEnum, Resource, ResourceArc,
};
use std::sync::RwLock;

struct ATreeResource(RwLock<ATree<u64>>);

type ATreeArc = ResourceArc<ATreeResource>;

#[resource_impl]
impl Resource for ATreeResource {}

#[derive(NifUnitEnum)]
enum AttributeType {
    Boolean,
    Integer,
    String,
    IntegerList,
    StringList,
}

#[rustler::nif(name = "new")]
fn new(attributes: Vec<(AttributeType, &str)>) -> NifResult<(Atom, ATreeArc)> {
    let attributes: Vec<AttributeDefinition> = attributes
        .iter()
        .map(|(attribute_type, name)| match attribute_type {
            AttributeType::Boolean => AttributeDefinition::boolean(name),
            AttributeType::Integer => AttributeDefinition::integer(name),
            AttributeType::String => AttributeDefinition::string(name),
            AttributeType::StringList => AttributeDefinition::string_list(name),
            AttributeType::IntegerList => AttributeDefinition::integer_list(name),
        })
        .collect();
    ATree::<u64>::new(&attributes)
        .map(|x| (atoms::ok(), ResourceArc::new(ATreeResource(RwLock::new(x)))))
        .map_err(|error| from_error(error))
}

#[rustler::nif(name = "insert")]
fn insert(atree: ATreeArc, id: u64, expression: &str) -> NifResult<Atom> {
    let mut locked = atree
        .0
        .write()
        .map_err(|_| Error::Term(Box::new(atoms::lock_fail())))?;
    locked.insert(&id, expression).map_err(from_error)?;
    Ok(atoms::ok())
}

#[rustler::nif(name = "search")]
fn search(atree: ATreeArc, attributes: Vec<(&str, EventType)>) -> NifResult<(Atom, Vec<u64>)> {
    let locked = atree
        .0
        .read()
        .map_err(|_| Error::Term(Box::new(atoms::lock_fail())))?;
    let mut builder = locked.make_event();
    for (name, kind) in &attributes {
        match kind {
            EventType::String(value) => {
                builder.with_string(name, value).map_err(from_event_error)?;
            }
            EventType::Boolean(value) => {
                builder
                    .with_boolean(name, *value)
                    .map_err(from_event_error)?;
            }
            EventType::Integer(value) => {
                builder
                    .with_integer(name, *value)
                    .map_err(from_event_error)?;
            }
            EventType::IntegerList(value) => {
                builder
                    .with_integer_list(name, value)
                    .map_err(from_event_error)?;
            }
            EventType::StringList(value) => {
                builder
                    .with_string_list(name, value)
                    .map_err(from_event_error)?;
            }
        }
    }
    let event = builder.build().map_err(from_event_error)?;
    let report = locked.search(event).map_err(from_error)?;
    let matches = report.matches().iter().cloned().cloned().collect();
    Ok((atoms::ok(), matches))
}

#[rustler::nif(name = "delete")]
fn delete(atree: ATreeArc, id: u64) -> NifResult<Atom> {
    let mut locked = atree
        .0
        .write()
        .map_err(|_| Error::Term(Box::new(atoms::lock_fail())))?;
    locked.delete(&id);
    Ok(atoms::ok())
}

#[derive(NifUntaggedEnum)]
enum EventType<'a> {
    Boolean(bool),
    String(&'a str),
    Integer(i64),
    IntegerList(Vec<i64>),
    StringList(Vec<&'a str>),
}

fn from_error(error: ATreeError) -> Error {
    match error {
        ATreeError::ParseError(error) => {
            Error::Term(Box::new((atoms::parse_error(), error.to_string())))
        }
        ATreeError::Event(error) => from_event_error(error),
    }
}

fn from_event_error(error: EventError) -> Error {
    let to_return = match error {
        error @ EventError::WrongType { .. } => (atoms::wrong_type(), error.to_string()),
        error @ EventError::MissingAttributes { .. } => {
            (atoms::missing_attributes(), error.to_string())
        }
        error @ EventError::AlreadyPresent { .. } => {
            (atoms::duplicate_attribute(), error.to_string())
        }
        error @ EventError::NonExistingAttribute(_) => {
            (atoms::non_existing_attribute(), error.to_string())
        }
        error @ EventError::MismatchingTypes { .. } => {
            (atoms::mismatching_types(), error.to_string())
        }
    };
    Error::Term(Box::new(to_return))
}

mod atoms {
    rustler::atoms! {
        ok,
        error,
        eof,
        unknown,
        lock_fail,

        // Parse Errors
        parse_error,

        // Event Errors
        wrong_type,
        missing_attributes,
        duplicate_attribute,
        non_existing_attribute,
        mismatching_types
    }
}

rustler::init!("a_tree");
