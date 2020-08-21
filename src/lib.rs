use nom::{branch::*, bytes::complete::*, character::complete::*, combinator::*, IResult};

pub type Result<'a, T> = IResult<&'a str, T>;

#[derive(Debug, PartialEq)]
pub enum Token {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Int(u16),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Sum,
    Mul,
}

pub fn parse(source: &str) -> Result<Expr> {
    expr(source, Precedence::Lowest)
}

fn expr(source: &str, left_precedence: Precedence) -> Result<Expr> {
    let (mut source, mut lhs) = token_int(source)?;

    while let Ok((s, op)) = token_op(source) {
        let right_precedence = infix_binding_power(&op);
        if right_precedence < left_precedence {
            break;
        }

        let (s, rhs) = expr(s, right_precedence)?;
        source = s;
        lhs = match op {
            Token::Add => Expr::Add(Box::new(lhs), Box::new(rhs)),
            Token::Sub => Expr::Sub(Box::new(lhs), Box::new(rhs)),
            Token::Mul => Expr::Mul(Box::new(lhs), Box::new(rhs)),
            Token::Div => Expr::Div(Box::new(lhs), Box::new(rhs)),
            Token::Mod => Expr::Mod(Box::new(lhs), Box::new(rhs)),
        };
    }

    Ok((source, lhs))
}

fn token_int(source: &str) -> Result<Expr> {
    map(is_a("0123456789"), |s: &str| Expr::Int(s.parse().unwrap()))(source)
}

fn token_op(source: &str) -> Result<Token> {
    alt((
        map(char('+'), |_| Token::Add),
        map(char('-'), |_| Token::Sub),
        map(char('*'), |_| Token::Mul),
        map(char('/'), |_| Token::Div),
        map(char('%'), |_| Token::Mod),
    ))(source)
}

fn infix_binding_power(token: &Token) -> Precedence {
    match *token {
        Token::Add | Token::Sub => Precedence::Sum,
        Token::Mul | Token::Div | Token::Mod => Precedence::Mul,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn execute(source: &str) -> Expr {
        parse(source).unwrap().1
    }

    #[test]
    fn one_plus_one() {
        let source = "1+1";
        assert_eq!(
            execute(source),
            Expr::Add(Box::new(Expr::Int(1)), Box::new(Expr::Int(1)))
        );
    }

    #[test]
    fn one_multiply_one() {
        let source = "1*1";
        assert_eq!(
            execute(source),
            Expr::Mul(Box::new(Expr::Int(1)), Box::new(Expr::Int(1)))
        );
    }

    #[test]
    fn one_multiply_one_plus_one() {
        let source = "1*1+1";
        assert_eq!(
            execute(source),
            Expr::Add(
                Box::new(Expr::Mul(Box::new(Expr::Int(1)), Box::new(Expr::Int(1)))),
                Box::new(Expr::Int(1)),
            )
        );
    }

    #[test]
    fn one_plus_one_multiply_one() {
        let source = "1+1*1";
        assert_eq!(
            execute(source),
            Expr::Add(
                Box::new(Expr::Int(1)),
                Box::new(Expr::Mul(Box::new(Expr::Int(1)), Box::new(Expr::Int(1)))),
            )
        );
    }
}
