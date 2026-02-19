pub mod parse_tree;

pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }
}

pub struct PathSegment<'input> {
    pub segment: &'input str,
    pub span: Span,
}

impl<'input> PathSegment<'input> {
    pub fn new(segment: &'input str, span: Span) -> PathSegment {
        PathSegment { segment, span }
    }
}

pub struct Path<'input> {
    segments: Vec<PathSegment<'input>>,
    pub span: Span,
}

impl<'input> Path<'input> {
    pub fn new(segments: Vec<PathSegment<'input>>, span: Span) -> Path {
        Path { segments, span }
    }
}