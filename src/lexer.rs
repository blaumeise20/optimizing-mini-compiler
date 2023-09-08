use std::{str::Chars, iter::Peekable};

/// A lexer for the Mini language.
pub struct Lexer<'a> {
    text: &'a str,
    chars: Peekable<Chars<'a>>,
    position: usize,
    pub errors: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new lexer for the given text.
    pub fn new(text: &'a str) -> Self {
        Lexer {
            text,
            chars: text.chars().peekable(),
            position: 0,
            errors: 0,
        }
    }

    /// Completely lexes the given text to the end and returns a list of tokens.
    pub fn lex(&mut self) -> Vec<Token<'a>> {
        let mut tokens = vec![];
        loop {
            let token = self.next_token();
            let exit = token.kind == TokenKind::EOF; // rust moment
            if token.kind != TokenKind::Whitespace {
                tokens.push(token);
            }
            if exit {
                break;
            }
        }
        tokens
    }

    fn next_char(&mut self) -> char {
        let c = self.chars.next();
        if c.is_some() {
            self.position += 1;
        }
        c.unwrap_or('\0')
    }

    fn consume(&mut self, c: char) -> bool {
        if let Some(&next) = self.chars.peek() {
            if next == c {
                self.next_char();
                return true;
            }
        }
        false
    }

    /// Lexes one single token from the input.
    pub fn next_token(&mut self) -> Token<'a> {
        let start = self.position;
        let c = self.next_char();

        let kind;

        match c {
            '\0' => { kind = TokenKind::EOF; }
            '+' => { kind = TokenKind::Plus; }
            '-' => { kind = TokenKind::Minus; }
            '*' => { kind = TokenKind::Star; }
            '/' => {
                if self.consume('/') {
                    while let Some(&next) = self.chars.peek() {
                        if next == '\n' {
                            break;
                        }
                        self.next_char();
                    }
                    return self.next_token();
                }
                else {
                    kind = TokenKind::Slash;
                }
            }
            '%' => { kind = TokenKind::Percent; }
            '=' => { kind = TokenKind::Equals; }
            '#' => { kind = TokenKind::Hash; }
            '<' => {
                if self.consume('=') { kind = TokenKind::LessThanEquals; }
                else { kind = TokenKind::LessThan; }
            }
            '>' => {
                if self.consume('=') { kind = TokenKind::GreaterThanEquals; }
                else { kind = TokenKind::GreaterThan; }
            }
            '(' => { kind = TokenKind::LParen; }
            ')' => { kind = TokenKind::RParen; }
            '[' => { kind = TokenKind::LBracket; }
            ']' => { kind = TokenKind::RBracket; }
            '.' => { kind = TokenKind::Dot; }
            ',' => { kind = TokenKind::Comma; }
            ':' => {
                if self.consume('=') { kind = TokenKind::ColonEquals; }
                else { kind = TokenKind::Colon; }
            }
            ';' => { kind = TokenKind::Semicolon; }
            ' ' | '\t' | '\r' | '\n' => { kind = TokenKind::Whitespace; }
            '0'..='9' => {
                while let Some(&next) = self.chars.peek() {
                    if !next.is_ascii_digit() {
                        break;
                    }
                    self.next_char();
                }
                kind = TokenKind::Number;
            }
            'a'..='z' | 'A'..='Z' => {
                while let Some(&next) = self.chars.peek() {
                    if !next.is_ascii_alphanumeric() {
                        break;
                    }
                    self.next_char();
                }
                kind = match &self.text[start..self.position] {
                    "PROGRAM" => TokenKind::K_Program,
                    "BEGIN" => TokenKind::K_Begin,
                    "END" => TokenKind::K_End,
                    "VAR" => TokenKind::K_Var,
                    "ARRAY" => TokenKind::K_Array,
                    "OF" => TokenKind::K_Of,
                    "IF" => TokenKind::K_If,
                    "THEN" => TokenKind::K_Then,
                    "ELSIF" => TokenKind::K_Elsif,
                    "ELSE" => TokenKind::K_Else,
                    "WHILE" => TokenKind::K_While,
                    "DO" => TokenKind::K_Do,
                    "READ" => TokenKind::K_Read,
                    "WRITE" => TokenKind::K_Write,
                    _ => TokenKind::Identifier,
                };
            }
            _ => { kind = TokenKind::Error; self.errors += 1; }
        }

        Token { kind, text: &self.text[start..self.position] }
    }
}

/// One lexical unit like a number, an operator or a keyword.
#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub text: &'a str,
}

/// The kind of a token.
#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenKind {
    EOF,
    Whitespace,
    Error,

    // Complex tokens
    Number,
    Identifier,

    // Simple tokens
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equals,
    Hash,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Dot,
    Comma,
    Colon,
    Semicolon,
    ColonEquals,

    // Keywords
    K_Program,
    K_Begin,
    K_End,
    K_Var,
    K_Array,
    K_Of,
    K_If,
    K_Then,
    K_Elsif,
    K_Else,
    K_While,
    K_Do,
    K_Read,
    K_Write,
}
