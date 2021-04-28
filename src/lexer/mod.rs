use std::collections::HashMap;

use self::error::LexerError;
use self::token::token_type::TokenType;
use self::token::{Pos, Token};

mod error;
pub mod token;

pub struct Lexer {
    src: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    pos: usize,
    keywords: HashMap<&'static str, TokenType>,
    errors: Vec<LexerError>,
}

impl Lexer {
    pub fn new(src: String) -> Self {
        use TokenType::*;

        let mut keywords = HashMap::new();
        for token_t in &[
            Enum, Struct, Fn, Impl, Trait, Mod, Const, Pub, As, Nil, Str, Num, Int, Float, Any,
            Vec, Bool, Map, False, True, Continue, Break, Return, SelfStatic, Self_, Let, Mut,
            Loop, While, For, In, Match, If, Else,
        ] {
            keywords.insert(token_t.value().unwrap(), *token_t);
        }

        Self {
            src,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
            pos: 1,
            keywords,
            errors: vec![],
        }
    }

    pub fn tokenize(&mut self) -> (&[Token], &[LexerError]) {
        while !self.is_at_end() {
            self.start = self.current;
            self.token();
        }

        self.tokens.push(Token::new(
            TokenType::Eof,
            String::from(""),
            String::from(""),
            self.pos(),
        ));

        (&self.tokens, &self.errors)
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
                } else if self.match_char('=') {
                    TokenType::BitwiseAndEqual
                } else {
                    TokenType::BitwiseAnd
                };
                self.add_token(t_type);
            }
            '|' => {
                let t_type = if self.match_char('|') {
                    TokenType::LogicOr
                } else if self.match_char('=') {
                    TokenType::BitwiseOrEqual
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
            '^' => {
                let t_type = if self.match_char('=') {
                    TokenType::BitwiseXorEqual
                } else {
                    TokenType::BitwiseXor
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
                            self.errors.push(LexerError::UnclosedComment(self.pos()));
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
                    self.pos += 1;
                    self.errors
                        .push(LexerError::UnknownCharacter(self.pos(), c));
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
        self.add_token_with_literal(token_type, "");
    }

    fn add_token_with_literal(&mut self, token_type: TokenType, literal: &str) {
        let text = self.src_substr(self.start, self.current);
        let len = text.len();
        self.tokens.push(Token::new(
            token_type,
            text,
            literal.to_string(),
            self.pos(),
        ));

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
            self.errors.push(LexerError::UnterminatedString(self.pos()));
            return;
        }

        self.advance();

        let val = self.src_substr(self.start + 1, self.current - 1);
        self.add_token_with_literal(TokenType::String, &val);
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
            &self.src_substr(self.start, self.current),
        );
    }

    fn identifier(&mut self) {
        while self.is_alphanumeric(self.peek()) {
            self.advance();
        }

        let text = self.src_substr(self.start, self.current);
        let token_type = *self
            .keywords
            .get(&text as &str)
            .unwrap_or(&TokenType::Identifier);

        self.add_token(token_type);
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.src.len() {
            return '\0';
        }

        self.src.chars().nth(self.current + 1).unwrap()
    }

    fn pos(&self) -> Pos {
        Pos(self.line, self.pos)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize() {
        let mut lexer = Lexer::new(str::to_string("let x = 100;"));

        let (tokens, errs) = lexer.tokenize();
        assert!(errs.is_empty());
        assert_eq!(
            tokens,
            &vec![
                Token::new(
                    TokenType::Let,
                    String::from("let"),
                    String::from(""),
                    Pos(1, 1)
                ),
                Token::new(
                    TokenType::Identifier,
                    String::from("x"),
                    String::from(""),
                    Pos(1, 5)
                ),
                Token::new(
                    TokenType::Equal,
                    String::from("="),
                    String::from(""),
                    Pos(1, 7)
                ),
                Token::new(
                    TokenType::NumberInt,
                    String::from("100"),
                    String::from("100"),
                    Pos(1, 9)
                ),
                Token::new(
                    TokenType::Semicolon,
                    String::from(";"),
                    String::from(""),
                    Pos(1, 12)
                ),
                Token::new(
                    TokenType::Eof,
                    String::from(""),
                    String::from(""),
                    Pos(1, 13)
                ),
            ]
        );
    }

    #[test]
    fn test_err_tokenize() {
        let mut lexer = Lexer::new(str::to_string("const X = \"string"));
        let (tokens, errs) = lexer.tokenize();
        assert!(!errs.is_empty());
        assert_eq!(
            tokens,
            &vec![
                Token::new(
                    TokenType::Const,
                    String::from("const"),
                    String::from(""),
                    Pos(1, 1)
                ),
                Token::new(
                    TokenType::Identifier,
                    String::from("X"),
                    String::from(""),
                    Pos(1, 7)
                ),
                Token::new(
                    TokenType::Equal,
                    String::from("="),
                    String::from(""),
                    Pos(1, 9)
                ),
                Token::new(
                    TokenType::Eof,
                    String::from(""),
                    String::from(""),
                    Pos(1, 11)
                ),
            ]
        );

        let mut lexer = Lexer::new(str::to_string("/* comment"));
        let (tokens, errs) = lexer.tokenize();
        assert!(!errs.is_empty());
        assert_eq!(
            tokens,
            &vec![Token::new(
                TokenType::Eof,
                String::from(""),
                String::from(""),
                Pos(1, 1)
            ),]
        );
    }
}
