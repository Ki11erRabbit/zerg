use std::iter::Peekable;
use std::str::CharIndices;

pub struct SpannedToken<'input> {
    start: usize,
    end: usize,
    pub token: Token<'input>
}

impl<'input> SpannedToken<'input> {
    pub fn new(start: usize, end: usize, token: Token<'input>) -> SpannedToken<'input> {
        Self {
            start,
            end,
            token
        }
    }

    pub fn token(&self) -> &Token<'input> {
        &self.token
    }
}

impl SpannedToken<'_> {
    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }
}

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
    Comptime,
    // Symbols
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
    Number(&'input str),
    String(&'input str),
    // Identifier
    Identifier(&'input str),
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

    pub fn completed(&mut self) -> bool {
        self.char_indices.peek().is_none()
    }

    fn next_char(&mut self) -> Option<(usize, char)> {
        self.char_indices.next()
    }

    fn peek(&mut self) -> Option<&(usize, char)> {
        self.char_indices.peek()
    }

    fn next_token(&mut self) -> Option<SpannedToken<'input>> {
        while let Some((start, ch)) = self.next_char() {
            match ch {
                '(' => return Some(SpannedToken::new(start, start + ')'.len_utf8(), Token::OpenParen)),
                ')' => return Some(SpannedToken::new(start, start + ')'.len_utf8(), Token::CloseParen)),
                '{' => return Some(SpannedToken::new(start, start + '{'.len_utf8(), Token::OpenBrace)),
                '}' => return Some(SpannedToken::new(start, start + '}'.len_utf8(), Token::CloseBrace)),
                '[' => return Some(SpannedToken::new(start, start + '['.len_utf8(), Token::OpenBracket)),
                ']' => return Some(SpannedToken::new(start, start + '['.len_utf8(), Token::CloseBracket)),
                '.' => return Some(SpannedToken::new(start, start + '.'.len_utf8(), Token::Period)),
                ',' => return Some(SpannedToken::new(start, start + ','.len_utf8(), Token::Comma)),
                ';' => return Some(SpannedToken::new(start, start + ';'.len_utf8(), Token::Semicolon)),
                ':' => {
                    if let Some((next, ch)) = self.peek() {
                        if *ch == ':' {
                            let next = *next;
                            self.next_char();
                            return Some(SpannedToken::new(start, next + ':'.len_utf8(), Token::Scope));
                        }
                    }
                    return Some(SpannedToken::new(start, start + ':'.len_utf8(), Token::Colon));
                }
                '@' => return Some(SpannedToken::new(start, start + '@'.len_utf8(), Token::At)),
                '=' => {
                    if let Some((next, ch)) = self.peek() {
                        if *ch == '=' {
                            let next = *next;
                            self.next_char();
                            return Some(SpannedToken::new(start, next + '='.len_utf8(), Token::Equals));
                        }
                    }
                    return Some(SpannedToken::new(start, start + '='.len_utf8(), Token::Assignment))
                },
                '+' => return Some(SpannedToken::new(start, start + '+'.len_utf8(), Token::Plus)),
                '-' => return Some(SpannedToken::new(start, start + '-'.len_utf8(), Token::Minus)),
                '*' => return Some(SpannedToken::new(start, start + '*'.len_utf8(), Token::Multiply)),
                '/' => return Some(SpannedToken::new(start, start + '/'.len_utf8(), Token::Divide)),
                '%' => return Some(SpannedToken::new(start, start + '%'.len_utf8(), Token::Remainder)),
                '|' => {
                    if let Some((next, ch)) = self.peek() {
                        if *ch == '|' {
                            let next = *next;
                            self.next_char();
                            return Some(SpannedToken::new(start, next + '|'.len_utf8(), Token::Or));
                        }
                    }
                    return None
                }
                '&' => {
                    if let Some((next, ch)) = self.peek() {
                        if *ch == '&' {
                            let next = *next;
                            self.next_char();
                            return Some(SpannedToken::new(start, next + '&'.len_utf8(), Token::And));
                        }
                    }
                    return None
                }
                '!' => {
                    if let Some((next, ch)) = self.peek() {
                        if *ch == '=' {
                            let next = *next;
                            self.next_char();
                            return Some(SpannedToken::new(start, next + '='.len_utf8(), Token::NotEquals));
                        }
                    }
                    return Some(SpannedToken::new(start, start + '!'.len_utf8(), Token::Not));
                }
                '<' => {
                    if let Some((next, ch)) = self.peek() {
                        if *ch == '=' {
                            let next = *next;
                            self.next_char();
                            return Some(SpannedToken::new(start, next + '='.len_utf8(), Token::LessThanEquals));
                        }
                    }
                    return Some(SpannedToken::new(start, start + '<'.len_utf8(), Token::LessThan));
                }
                '>' => {
                    if let Some((next, ch)) = self.peek() {
                        if *ch == '=' {
                            let next = *next;
                            self.next_char();
                            return Some(SpannedToken::new(start, next + '='.len_utf8(), Token::GreaterThanEquals));
                        }
                    }
                    return Some(SpannedToken::new(start, start + '>'.len_utf8(), Token::GreaterThan));
                }
                '"' => {
                    let start = start + '"'.len_utf8();
                    let mut end = start + '"'.len_utf8();
                    let mut found_escape = false;
                    let mut just_found_escape = false;
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
                    return Some(SpannedToken::new(start, end, Token::String(string)));
                }
                '\'' => {
                    let start = start + '\''.len_utf8();
                    let mut end = start + '\''.len_utf8();
                    let mut found_escape = false;
                    let mut just_found_escape = false;
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
                    return Some(SpannedToken::new(start, end, Token::String(string)));
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
                    let token = Token::Number(string);
                    return Some(SpannedToken::new(start, end, token));
                }
                c if c.is_whitespace() => {
                    continue;
                }
                 c if c.is_ascii_alphabetic() || c == '_' => {
                    let mut end = start;
                     while let Some((next, chr)) = self.peek() {
                         let next = *next;
                         let chr = *chr;
                         if chr.is_ascii_alphanumeric() || chr == '_' {
                             end = next;
                             self.next_token();
                         } else {
                             break;
                         }
                     }
                     let string = &self.input[start..end];
                     return match string {
                         "fun" => Some(SpannedToken::new(start, end, Token::Fun)),
                         "let" => Some(SpannedToken::new(start, end, Token::Let)),
                         "if" => Some(SpannedToken::new(start, end, Token::If)),
                         "else" => Some(SpannedToken::new(start, end, Token::Else)),
                         "elif" => Some(SpannedToken::new(start, end, Token::Elif)),
                         "while" => Some(SpannedToken::new(start, end, Token::While)),
                         "for" => Some(SpannedToken::new(start, end, Token::For)),
                         "return" => Some(SpannedToken::new(start, end, Token::Return)),
                         "import" => Some(SpannedToken::new(start, end, Token::Import)),
                         "pub" => Some(SpannedToken::new(start, end, Token::Public)),
                         "inline" => Some(SpannedToken::new(start, end, Token::Inline)),
                         "comptime" => Some(SpannedToken::new(start, end, Token::Comptime)),
                         x => Some(SpannedToken::new(start, end, Token::Identifier(x))),
                     };
                }
                _ => return None
            }
        }
        None
    }
}