pub mod parse_tree;
pub mod desugared_tree;

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

#[derive(Debug, Clone)]
pub struct PathSegment<'input> {
    pub segment: &'input str,
    pub span: Span,
}

impl<'input> PathSegment<'input> {
    pub fn new(segment: &'input str, span: Span) -> PathSegment {
        PathSegment { segment, span }
    }
}

#[derive(Debug, Clone)]
pub struct Path<'input> {
    pub segments: Vec<PathSegment<'input>>,
    pub span: Span,
}

impl<'input> Path<'input> {
    pub fn new(segments: Vec<PathSegment<'input>>, span: Span) -> Path {
        Path { segments, span }
    }
}