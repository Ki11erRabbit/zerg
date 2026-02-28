use std::borrow::Cow;
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug)]
pub enum LexerError {
    UnexpectedEOF,
    InvalidCharacter(char),
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LexerError::UnexpectedEOF => write!(f, "unexpected end of input"),
            LexerError::InvalidCharacter(c) => write!(f, "invalid character '{}'", c),
        }
    }
}

impl std::error::Error for LexerError {}


#[derive(Clone, Debug)]
pub enum Token<'input> {
    // Keywords
    Fun,
    Let,
    If,
    Else,
    Elif,
    While,
    For,
    Return,
    Import,
    Public,
    Inline,
    Lazy,
    Comptime,
    // Symbols
    Arrow,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Period,
    Comma,
    Semicolon,
    Colon,
    Scope,
    At,
    Assignment,
    // Operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Remainder,
    Or,
    And,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    Not,
    // Constants
    Number(Cow<'input, str>),
    String(Cow<'input, str>),
    // Identifier
    Identifier(Cow<'input, str>),
}


pub struct Lexer<'input> {
    input: &'input str,
    char_indices: Peekable<CharIndices<'input>>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            char_indices: input.char_indices().peekable(),
        }
    }

    fn next_char(&mut self) -> Option<(usize, char)> {
        self.char_indices.next()
    }

    fn peek(&mut self) -> Option<&(usize, char)> {
        self.char_indices.peek()
    }

    fn next_token(&mut self) -> Option<Result<(usize, Token<'input>, usize), LexerError>> {
        while let Some((start, ch)) = self.next_char() {
            match ch {
                '(' => return Some(Ok((start, Token::OpenParen,  start + ')'.len_utf8()))),
                ')' => return Some(Ok((start, Token::CloseParen,  start + ')'.len_utf8()))),
                '{' => return Some(Ok((start, Token::OpenBrace,  start + '{'.len_utf8()))),
                '}' => return Some(Ok((start, Token::CloseBrace,  start + '}'.len_utf8()))),
                '[' => return Some(Ok((start, Token::OpenBracket,  start + '['.len_utf8()))),
                ']' => return Some(Ok((start, Token::CloseBracket,  start + '['.len_utf8()))),
                '.' => return Some(Ok((start, Token::Period,  start + '.'.len_utf8()))),
                ',' => return Some(Ok((start, Token::Comma,  start + ','.len_utf8()))),
                ';' => return Some(Ok((start, Token::Semicolon,  start + ';'.len_utf8()))),
                ':' => {
                    if let Some((next, ch)) = self.peek() {
                        if *ch == ':' {
                            let next = *next;
                            self.next_char();
                            return Some(Ok((start, Token::Scope,  next + ':'.len_utf8())));
                        }
                    }
                    return Some(Ok((start, Token::Colon,  start + ':'.len_utf8())));
                }
                '@' => return Some(Ok((start, Token::At,  start + '@'.len_utf8()))),
                '=' => {
                    if let Some((next, ch)) = self.peek() {
                        if *ch == '=' {
                            let next = *next;
                            self.next_char();
                            return Some(Ok((start, Token::Equals,  next + '='.len_utf8())));
                        }
                    }
                    return Some(Ok((start, Token::Assignment,  start + '='.len_utf8())))
                },
                '+' => return Some(Ok((start, Token::Plus,  start + '+'.len_utf8()))),
                '-' => {
                    if let Some((next, ch)) = self.peek() {
                        if *ch == '>' {
                            let next = *next;
                            self.next_char();
                            return Some(Ok((start, Token::Arrow,  next + '>'.len_utf8())));
                        }
                    }
                    return Some(Ok((start, Token::Minus,  start + '-'.len_utf8())))
                },
                '*' => return Some(Ok((start, Token::Multiply,  start + '*'.len_utf8()))),
                '/' => return Some(Ok((start, Token::Divide,  start + '/'.len_utf8()))),
                '%' => return Some(Ok((start, Token::Remainder,  start + '%'.len_utf8()))),
                '|' => {
                    if let Some((next, ch)) = self.peek() {
                        if *ch == '|' {
                            let next = *next;
                            self.next_char();
                            return Some(Ok((start, Token::Or,  next + '|'.len_utf8())));
                        }
                    }
                    return Some(Err(LexerError::UnexpectedEOF));
                }
                '&' => {
                    if let Some((next, ch)) = self.peek() {
                        if *ch == '&' {
                            let next = *next;
                            self.next_char();
                            return Some(Ok((start, Token::And,  next + '&'.len_utf8())));
                        }
                    }
                    return Some(Err(LexerError::UnexpectedEOF));
                }
                '!' => {
                    if let Some((next, ch)) = self.peek() {
                        if *ch == '=' {
                            let next = *next;
                            self.next_char();
                            return Some(Ok((start, Token::NotEquals,  next + '='.len_utf8())));
                        }
                    }
                    return Some(Ok((start, Token::Not,  start + '!'.len_utf8())));
                }
                '<' => {
                    if let Some((next, ch)) = self.peek() {
                        if *ch == '=' {
                            let next = *next;
                            self.next_char();
                            return Some(Ok((start, Token::LessThanEquals,  next + '='.len_utf8())));
                        }
                    }
                    return Some(Ok((start, Token::LessThan,  start + '<'.len_utf8())));
                }
                '>' => {
                    if let Some((next, ch)) = self.peek() {
                        if *ch == '=' {
                            let next = *next;
                            self.next_char();
                            return Some(Ok((start, Token::GreaterThanEquals,  next + '='.len_utf8())));
                        }
                    }
                    return Some(Ok((start, Token::GreaterThan,  start + '>'.len_utf8())));
                }
                '"' => {
                    let start = start + '"'.len_utf8();
                    let mut end = start + '"'.len_utf8();
                    let mut found_escape = false;
                    let mut just_found_escape;
                    while let Some((next, ch)) = self.peek() {
                        just_found_escape = false;
                        if *ch == '\\' {
                            found_escape = true;
                            just_found_escape = true;
                        }
                        if *ch == '"' && !found_escape {
                            end = *next;
                            self.next_char();
                            break;
                        } else if found_escape && !just_found_escape {
                            found_escape = false;
                        }
                        self.next_char();
                    }
                    let string = &self.input[start..end];
                    return Some(Ok((start, Token::String(Cow::Borrowed(string)),  end)));
                }
                '\'' => {
                    let start = start + '\''.len_utf8();
                    let mut end = start + '\''.len_utf8();
                    let mut found_escape = false;
                    let mut just_found_escape;
                    while let Some((next, ch)) = self.peek() {
                        just_found_escape = false;
                        if *ch == '\\' {
                            found_escape = true;
                            just_found_escape = true;
                        }
                        if *ch == '\'' && !found_escape {
                            end = *next;
                            self.next_char();
                            break;
                        } else if found_escape && !just_found_escape {
                            found_escape = false;
                        }
                        self.next_char();
                    }
                    let string = &self.input[start..end];
                    return Some(Ok((start, Token::String(Cow::Borrowed(string)),  end)));
                }
                '0'..='9' => {
                    let mut end = start;
                    while let Some((next, chr)) = self.peek() {
                        let next = *next;
                        let chr = *chr;
                        if chr.is_digit(10) || chr == '.' {
                            end = next;
                            self.next_token();
                        } else {
                            break;
                        }
                    }
                    let string = &self.input[start..end];
                    let token = Token::Number(Cow::Borrowed(string));
                    return Some(Ok((start, token,  end)));
                }
                c if c.is_whitespace() => {
                    continue;
                }
                 c if c.is_ascii_alphabetic() || c == '_' => {
                    let mut end = start;
                     while let Some((next, chr)) = self.peek() {
                         end = *next;
                         let chr = *chr;
                         if chr.is_ascii_alphanumeric() || chr == '_' || chr.is_ascii_digit() {
                             self.next_token();
                         } else {
                             break;
                         }
                     }
                     let string = &self.input[start..end];
                     return match string {
                         "fun" => Some(Ok((start, Token::Fun,  end))),
                         "let" => Some(Ok((start, Token::Let,  end))),
                         "if" => Some(Ok((start, Token::If,  end))),
                         "else" => Some(Ok((start, Token::Else,  end))),
                         "elif" => Some(Ok((start, Token::Elif,  end))),
                         "while" => Some(Ok((start, Token::While,  end))),
                         "for" => Some(Ok((start, Token::For,  end))),
                         "return" => Some(Ok((start, Token::Return,  end))),
                         "import" => Some(Ok((start, Token::Import,  end))),
                         "pub" => Some(Ok((start, Token::Public,  end))),
                         "inline" => Some(Ok((start, Token::Inline,  end))),
                         "lazy" => Some(Ok((start, Token::Lazy,  end))),
                         "comptime" => Some(Ok((start, Token::Comptime,  end))),
                         x => Some(Ok((start, Token::Identifier(Cow::Borrowed(x)),  end))),
                     };
                }
                c => return Some(Err(LexerError::InvalidCharacter(c)))
            }
        }
        None
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(usize, Token<'input>, usize), LexerError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}