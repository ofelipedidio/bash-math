use std::{io::stdin, fmt::{Display, Debug}};

use error_handling::ContextMessage;

#[derive(Debug, Clone)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

impl BinOp {
    fn precedence(&self) -> usize {
        match self {
            BinOp::Add => 10,
            BinOp::Sub => 10,
            BinOp::Mul => 20,
            BinOp::Div => 20,
            BinOp::Pow => 30,
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Pow => write!(f, "^"),
        }
    }
}

#[derive(Debug)]
enum Expr {
    Number(f64),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Parenthesized(Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Number(num) => write!(f, "{}", num),
            Expr::BinOp(op, e1, e2) => write!(f, "({}{}{})", e1, op, e2),
            Expr::Parenthesized(e) => write!(f, "{}", e),
        }
    }
}

type Result<T> = std::result::Result<T, error_handling::Error<Error>>;

enum Error {
    // Tokenizer errors
    UnexpectedCharacter(usize, char),

    // Parser errors
    UnexpectedToken(usize, Token),
    UnexpectedEOF(usize),
}

impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnexpectedCharacter(input_index, character) => write!(f, "Unexpected charcter ({:?}) at input index {}", character, input_index),
            Error::UnexpectedToken(token_index, token) => write!(f, "Unexpected token ({:?}) at token index {}", token, token_index),
            Error::UnexpectedEOF(token_index) => write!(f, "Unexpected end of file at token index {}", token_index),
        }
    }
}

#[derive(Debug, Clone)]
enum Token {
    Number(f64),
    Left,
    Right,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Caret,
}

struct Tokenizer<'a> {
    input: &'a [char],
    index: usize,
    output: Vec<Token>,
}

impl <'a> Tokenizer<'a> {
    fn push_and_inc(&mut self, value: Token) {
        self.output.push(value);
        self.index += 1;
    }

    fn push(&mut self, value: Token) {
        self.output.push(value);
    }

    fn tokenize(mut self) -> Result<Vec<Token>> {
        while self.index < self.input.len() {
            // Is this unwrap safe? I think so
            match self.input.get(self.index).unwrap() {
                ' ' | '\t' | '\r' | '\n' => {
                    self.index += 1;
                    continue;
                },

                '(' => self.push_and_inc(Token::Left),
                ')' => self.push_and_inc(Token::Right),
                '+' => self.push_and_inc(Token::Plus),
                '-' => self.push_and_inc(Token::Minus),
                '*' => self.push_and_inc(Token::Asterisk),
                '/' => self.push_and_inc(Token::Slash),
                '^' => self.push_and_inc(Token::Caret),
                '0'..='9' => {
                    let mut num: f64 = 0.0;

                    loop {
                        match self.input.get(self.index) {
                            Some(c @ '0'..='9') => {
                                num *= 10.0;
                                num += ((c.clone() as i32) - ('0' as i32)) as f64;
                                self.index += 1;
                            }
                            _ => break,
                        }
                    }

                    if let Some('.') = self.input.get(self.index) {
                        self.index += 1;

                        let mut frac: f64 = 0.0;
                        let mut scale: f64 = 1.0;

                        loop {
                            match self.input.get(self.index) {
                                Some(c @ '0'..='9') => {
                                    scale /= 10.0;
                                    frac += scale * (((c.clone() as i32) - ('0' as i32)) as f64);
                                    self.index += 1;
                                }
                                _ => break,
                            }
                        }

                        num += frac;
                    }

                    self.push(Token::Number(num));
                }
                c => return Err(Error::UnexpectedCharacter(self.index, c.clone()).into()).context("tokenizing input"),
            }
        }

        Ok(self.output)
    }
}

fn tokenize(input: &[char]) -> Result<Vec<Token>> {
    Tokenizer{ input, index: 0, output: Vec::new() }.tokenize()
}

struct Parser<'a> {
    input: &'a [Token],
    index: usize,
}

impl <'a> Parser<'a> {
    fn parse_base_expression(&mut self) -> Result<Expr> {
        let initial_index = self.index;

        match self.input.get(self.index) {
            Some(Token::Number(num)) => {
                self.index += 1;
                Ok(Expr::Number(num.clone()))
            },
            Some(Token::Left) => {
                self.index += 1;
                let inner_expression = self.parse_expression().context("parsing parenthesized")?;
                match self.input.get(self.index) {
                    Some(Token::Right) => {
                        self.index += 1;
                        Ok(Expr::Parenthesized(Box::new(inner_expression)))
                    },
                    Some(token) => Err(Error::UnexpectedToken(self.index.clone(), token.clone()).into()).context("parsing closing parenthesis"),
                    None => Err(Error::UnexpectedEOF(self.index.clone()).into()).context("parsing closing parenthesis"),
                }
            },
            Some(token) => Err(Error::UnexpectedToken(self.index.clone(), token.clone()).into()).context("parsing base expression"),
            None => Err(Error::UnexpectedEOF(self.index.clone()).into()).context("parsing base expression"),
        }.or_else(|error| {
            self.index = initial_index;
            Err(error)
        })
    }

    fn parse_expression(&mut self) -> Result<Expr> {
        let mut expression = self.parse_base_expression().context("parsing expression")?;

        loop {
            match self.input.get(self.index) {
                Some(Token::Plus) => {
                    self.index += 1;
                    let lhs = expression;
                    let rhs = self.parse_base_expression().context("parsing following addition expression (lhs: {})")?;

                    match lhs {
                        Expr::BinOp(op, e1, e2) if op.precedence() < BinOp::Add.precedence() => {
                            expression = Expr::BinOp( op, e1, Box::new( Expr::BinOp( BinOp::Add, e2, Box::new(rhs))));
                        }
                        lhs => expression = Expr::BinOp(BinOp::Add, Box::new(lhs), Box::new(rhs)),
                    }
                }
                Some(Token::Minus) => {
                    self.index += 1;
                    let lhs = expression;
                    let rhs = self.parse_base_expression().context("parsing following subtraction expression")?;

                    match lhs {
                        Expr::BinOp(op, e1, e2) if op.precedence() < BinOp::Sub.precedence() => {
                            expression = Expr::BinOp( op, e1, Box::new( Expr::BinOp( BinOp::Sub, e2, Box::new(rhs))));
                        }
                        lhs => expression = Expr::BinOp(BinOp::Sub, Box::new(lhs), Box::new(rhs)),
                    }
                }
                Some(Token::Asterisk) => {
                    self.index += 1;
                    let lhs = expression;
                    let rhs = self.parse_base_expression().context("parsing following multiplication expression")?;

                    match lhs {
                        Expr::BinOp(op, e1, e2) if op.precedence() < BinOp::Mul.precedence() => {
                            expression = Expr::BinOp( op, e1, Box::new( Expr::BinOp( BinOp::Mul, e2, Box::new(rhs))));
                        }
                        lhs => expression = Expr::BinOp(BinOp::Mul, Box::new(lhs), Box::new(rhs)),
                    }
                }
                Some(Token::Slash) => {
                    self.index += 1;
                    let lhs = expression;
                    let rhs = self.parse_base_expression().context("parsing following division expression")?;

                    match lhs {
                        Expr::BinOp(op, e1, e2) if op.precedence() < BinOp::Div.precedence() => {
                            expression = Expr::BinOp( op, e1, Box::new( Expr::BinOp( BinOp::Div, e2, Box::new(rhs))));
                        }
                        lhs => expression = Expr::BinOp(BinOp::Div, Box::new(lhs), Box::new(rhs)),
                    }
                }
                Some(Token::Caret) => {
                    self.index += 1;
                    let lhs = expression;
                    let rhs = self.parse_base_expression().context("parsing following exponentiation expression")?;

                    match lhs {
                        Expr::BinOp(op, e1, e2) if op.precedence() < BinOp::Pow.precedence() => {
                            expression = Expr::BinOp( op, e1, Box::new( Expr::BinOp( BinOp::Pow, e2, Box::new(rhs))));
                        }
                        lhs => expression = Expr::BinOp(BinOp::Pow, Box::new(lhs), Box::new(rhs)),
                    }
                }
                Some(Token::Right) | None => break,
                Some(token) => return Err(Error::UnexpectedToken(self.index.clone(), token.clone()).into()).context("parsing following expression"),
            }
        }

        Ok(expression)
    }
}

fn parse_expression(input: &[Token]) -> Result<Expr> {
    Parser{ input, index: 0 }.parse_expression().context("parsing root expression")
}

fn eval_expression(expr: &Expr) -> Result<f64> {
    match expr {
        Expr::Number(n) => Ok(n.clone()),
        Expr::Parenthesized(e) => eval_expression(&*e),
        Expr::BinOp(BinOp::Add, e1, e2) => Ok(eval_expression(&*e1)? + eval_expression(&*e2)?),
        Expr::BinOp(BinOp::Sub, e1, e2) => Ok(eval_expression(&*e1)? - eval_expression(&*e2)?),
        Expr::BinOp(BinOp::Mul, e1, e2) => Ok(eval_expression(&*e1)? * eval_expression(&*e2)?),
        Expr::BinOp(BinOp::Div, e1, e2) => {
            let lhs = eval_expression(&*e1)?;
            let rhs = eval_expression(&*e2)?;
            Ok(lhs / rhs)
        },
        Expr::BinOp(BinOp::Pow, e1, e2) => Ok(eval_expression(&*e1)?.powf(eval_expression(&*e2)?)),
    }
}

fn main() {
    let command_args: Vec<_> = std::env::args().collect();

    let expr;

    if command_args.len() > 1 {
        let input = command_args[1..].join(" ");
        let tokens = tokenize(&input.chars().collect::<Vec<_>>()).unwrap();
        expr = parse_expression(&tokens).unwrap();
    } else {
        let mut buf = String::new();
        while let Ok(read_size) = stdin().read_line(&mut buf) {
            if read_size == 0 {
                break;
            }
        }
        let input = buf;
        let tokens = tokenize(&input.chars().collect::<Vec<_>>()).unwrap();
        expr = parse_expression(&tokens).unwrap();
    }

    let value = eval_expression(&expr).unwrap();
    println!("{}", value);
}

