use std::collections::HashMap;

use self::token::Pos;
use self::token::Token;
use self::token::TokenType;
use crate::error_at;

pub mod token;

pub struct Lexer {
    src: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    pos: usize,
    err: bool,
    keywords: HashMap<String, TokenType>,
}

impl Lexer {
    pub fn new(src: String) -> Self {
        let mut keywords = HashMap::new();
        keywords.insert("enum".to_string(), TokenType::Enum);
        keywords.insert("struct".to_string(), TokenType::Struct);
        keywords.insert("fn".to_string(), TokenType::Fn);
        keywords.insert("impl".to_string(), TokenType::Impl);
        keywords.insert("trait".to_string(), TokenType::Trait);

        keywords.insert("pub".to_string(), TokenType::Pub);

        keywords.insert("nil".to_string(), TokenType::Nil);
        keywords.insert("str".to_string(), TokenType::Str);
        keywords.insert("num".to_string(), TokenType::Num);
        keywords.insert("int".to_string(), TokenType::Int);
        keywords.insert("float".to_string(), TokenType::Float);
        keywords.insert("any".to_string(), TokenType::Any);
        keywords.insert("vec".to_string(), TokenType::Vec);
        keywords.insert("bool".to_string(), TokenType::Bool);
        keywords.insert("map".to_string(), TokenType::Map);
        keywords.insert("false".to_string(), TokenType::False);
        keywords.insert("true".to_string(), TokenType::True);

        keywords.insert("continue".to_string(), TokenType::Continue);
        keywords.insert("break".to_string(), TokenType::Break);
        keywords.insert("return".to_string(), TokenType::Return);

        keywords.insert("Self".to_string(), TokenType::SelfStatic);
        keywords.insert("self".to_string(), TokenType::Self_);

        keywords.insert("let".to_string(), TokenType::Let);
        keywords.insert("mut".to_string(), TokenType::Mut);
        keywords.insert("const".to_string(), TokenType::Const);

        keywords.insert("while".to_string(), TokenType::While);
        keywords.insert("for".to_string(), TokenType::For);
        keywords.insert("loop".to_string(), TokenType::Loop);

        keywords.insert("match".to_string(), TokenType::Match);
        keywords.insert("if".to_string(), TokenType::If);
        keywords.insert("else".to_string(), TokenType::Else);

        Self {
            src,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
            pos: 1,
            keywords,
            err: false,
        }
    }

    pub fn tokenize(&mut self) -> (&Vec<Token>, bool) {
        while !self.is_at_end() {
            self.start = self.current;
            self.token();
        }

        self.tokens.push(Token::new(
            TokenType::Eof,
            "".to_string(),
            "".to_string(),
            self.pos(),
        ));

        (&self.tokens, self.err)
    }

    fn token(&mut self) {
        match self.advance() {
            '{' => self.add_token(TokenType::LeftCurlyBrace),
            '}' => self.add_token(TokenType::RightCurlyBrace),
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '[' => self.add_token(TokenType::LeftBracket),
            ']' => self.add_token(TokenType::RightBracket),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            ';' => self.add_token(TokenType::Semicolon),
            ':' => {
                let t_type = if self.match_char(':') {
                    TokenType::ColonColon
                } else {
                    TokenType::Colon
                };
                self.add_token(t_type);
            }
            '-' => {
                let t_type = if self.match_char('=') {
                    TokenType::MinusEqual
                } else if self.match_char('>') {
                    TokenType::Arrow
                } else {
                    TokenType::Minus
                };
                self.add_token(t_type);
            }
            '+' => {
                let t_type = if self.match_char('=') {
                    TokenType::PlusEqual
                } else {
                    TokenType::Plus
                };
                self.add_token(t_type);
            }
            '*' => {
                let t_type = if self.match_char('=') {
                    TokenType::AsteriskEqual
                } else {
                    TokenType::Asterisk
                };
                self.add_token(t_type);
            }
            '%' => {
                let t_type = if self.match_char('=') {
                    TokenType::ModulusEqual
                } else {
                    TokenType::Modulus
                };
                self.add_token(t_type);
            }
            '&' => {
                let t_type = if self.match_char('&') {
                    TokenType::LogicAnd
                } else {
                    TokenType::BitwiseAnd
                };
                self.add_token(t_type);
            }
            '|' => {
                let t_type = if self.match_char('|') {
                    TokenType::LogicOr
                } else {
                    TokenType::BitwiseOr
                };
                self.add_token(t_type);
            }
            '!' => {
                let t_type = if self.match_char('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.add_token(t_type);
            }
            '=' => {
                let t_type = if self.match_char('=') {
                    TokenType::EqualEqual
                } else if self.match_char('>') {
                    TokenType::FatArrow
                } else {
                    TokenType::Equal
                };
                self.add_token(t_type);
            }
            '<' => {
                let t_type = if self.match_char('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(t_type);
            }
            '>' => {
                let t_type = if self.match_char('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(t_type);
            }
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.match_char('*') {
                    let mut count = 1;
                    loop {
                        if self.peek() == '/' && self.peek_next() == '*' {
                            count += 1;
                        }

                        if self.peek() == '*' && self.peek_next() == '/' {
                            count -= 1;
                        }

                        if count == 0 {
                            break;
                        }

                        if count != 0 && self.is_at_end() {
                            self.err = true;
                            error_at(self.pos(), "Unclosed comment");
                            return;
                        }

                        self.advance();
                    }
                    self.advance();
                    self.advance();
                } else if self.match_char('=') {
                    self.add_token(TokenType::SlashEqual);
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            ' ' | '\r' | '\t' => {
                self.pos += 1;
            }
            '\n' => {
                self.line += 1;
                self.pos = 0;
            }
            '"' => self.string(),
            c => {
                if self.is_digit(c) {
                    self.number();
                } else if self.is_alphabetic(c) {
                    self.identifier();
                } else {
                    self.err = true;
                    error_at(self.pos(), "Unknown character");
                }
            }
        };
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.src.len()
    }

    fn advance(&mut self) -> char {
        self.current += 1;

        self.src.chars().nth(self.current - 1).unwrap()
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_with_literal(token_type, "".to_string());
    }

    fn add_token_with_literal(&mut self, token_type: TokenType, literal: String) {
        let text = self.src_substr(self.start, self.current);
        let len = text.len();
        self.tokens
            .push(Token::new(token_type, text, literal, self.pos()));

        self.pos += len;
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.src.chars().nth(self.current).unwrap() != expected {
            return false;
        }

        self.current += 1;

        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.src.chars().nth(self.current).unwrap()
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
                self.pos = 0;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.err = true;
            error_at(self.pos(), "Unterminated string");
            return;
        }

        self.advance();

        let val: String = self.src_substr(self.start + 1, self.current - 1);
        self.add_token_with_literal(TokenType::String, val);
    }

    fn src_substr(&self, start: usize, end: usize) -> String {
        self.src.chars().skip(start).take(end - start).collect()
    }

    fn is_digit(&self, c: char) -> bool {
        c.is_digit(10)
    }

    fn is_alphabetic(&self, c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_alphanumeric(&self, c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    fn number(&mut self) {
        let mut float = false;

        while self.is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            float = true;
            // consume '.'
            self.advance();

            while self.is_digit(self.peek()) {
                self.advance();
            }
        }

        self.add_token_with_literal(
            if float {
                TokenType::NumberFloat
            } else {
                TokenType::NumberInt
            },
            self.src_substr(self.start, self.current),
        );
    }

    fn identifier(&mut self) {
        while self.is_alphanumeric(self.peek()) {
            self.advance();
        }

        let text = self.src_substr(self.start, self.current);
        let token_type = *self.keywords.get(&text).unwrap_or(&TokenType::Identifier);

        self.add_token(token_type);
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.src.len() {
            return '\0';
        }

        self.src.chars().nth(self.current + 1).unwrap()
    }

    fn pos(&self) -> Pos {
        (self.line, self.pos)
    }
}
