use lalrpop_util::{lalrpop_mod, ParseError};
use crate::lexer;
use crate::lexer::{LexerError, Token};

lalrpop_mod!(pub grammar);

pub fn parse<'input>(text: &'input str) -> Result<ast::parse_tree::File<'input>, ParseError<usize, Token<'input>, LexerError>> {
    let lexer = lexer::Lexer::new(text);
    let parser = grammar::FileParser::new();
    parser.parse(text, lexer)
}