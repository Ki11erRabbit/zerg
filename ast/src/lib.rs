use std::borrow::Cow;
use std::path::PathBuf;
use crate::function_resolver::{FunctionResolver, ResolverError};

pub mod parse_tree;
pub mod desugared_tree;
mod desugarer;
mod function_resolver;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

#[derive(Debug, Clone)]
pub struct PathSegment<'input> {
    pub segment: Cow<'input, str>,
    pub span: Span,
}

impl<'input> PathSegment<'input> {
    pub fn new(segment: Cow<'input, str>, span: Span) -> PathSegment<'input> {
        PathSegment { segment, span }
    }
}

#[derive(Debug, Clone)]
pub struct Path<'input> {
    pub segments: Vec<PathSegment<'input>>,
    pub span: Span,
}

impl<'input> Path<'input> {
    pub fn new(segments: Vec<PathSegment<'input>>, span: Span) -> Path<'input> {
        Path { segments, span }
    }

    pub fn to_vec_strings(&self) -> Vec<String> {
        self.segments.iter().map(|s| s.segment.to_string()).collect()
    }
}

#[derive(Debug, Clone)]
pub struct OwnedPathSegment {
    pub segment: String,
    pub span: Span,
}

impl OwnedPathSegment {
    pub fn new(segment: &str, span: Span) -> OwnedPathSegment {
        OwnedPathSegment { segment: segment.to_string(), span }
    }
}

#[derive(Debug, Clone)]
pub struct OwnedPath {
    pub segments: Vec<OwnedPathSegment>,
    pub span: Span,
}

impl OwnedPath {
    pub fn new(segments: Vec<OwnedPathSegment>, span: Span) -> OwnedPath {
        OwnedPath { segments, span }
    }
}

impl From<Vec<String>> for OwnedPath {
    fn from(segments: Vec<String>) -> OwnedPath {
        let segments = segments.into_iter().map(|seg| {
            OwnedPathSegment::new(seg.as_str(), Span::new(0, 0))
        }).collect();
        OwnedPath::new(segments, Span::new(0, 0))
    }
}


pub fn desugar_and_typecheck<'input>(files: Vec<(PathBuf, parse_tree::File<'input>)>) -> Result<Vec<desugared_tree::File<'input>>, ResolverError> {
    let files = files.into_iter()
        .map(|(path, file)| (path, desugarer::desugar(file)))
        .collect();

    let mut resolver = FunctionResolver::new();
    resolver.resolve(files)
}