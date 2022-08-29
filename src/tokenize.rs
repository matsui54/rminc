#[derive(Debug)]
pub enum TokenKind<'a> {
    Reserved(&'a str),
    Num(i64),
}

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    at: usize,
}

pub struct Tokenizer<'a> {
    code: &'a str,
    cur: usize,
}

impl Tokenizer<'_> {
    pub fn new(code: &str) -> Tokenizer {
        Tokenizer { code, cur: 0 }
    }

    fn skip_space(&mut self) {
        match self.code[self.cur..].find(|c: char| !c.is_whitespace()) {
            Some(pos) => {
                self.cur += pos;
            }
            None => {
                self.cur = self.code.len() - 1;
            }
        }
    }

    fn get_number(&mut self) -> Option<i64> {
        let rem = &self.code[self.cur..];
        match rem.find(|c: char| !c.is_digit(10)) {
            Some(pos) => {
                if pos == 0 {
                    None
                } else {
                    self.cur += pos;
                    Some(rem[..pos].parse::<i64>().unwrap())
                }
            }
            None => {
                self.cur = self.code.len() - 1;
                Some(rem.parse::<i64>().unwrap())
            }
        }
    }

    fn get_punct_size(&self) -> usize {
        let rem = &self.code[self.cur..];
        if rem.starts_with("==")
            || rem.starts_with("!=")
            || rem.starts_with("<=")
            || rem.starts_with(">=")
        {
            2
        } else if rem.starts_with(|c: char| c.is_ascii_punctuation()) {
            1
        } else {
            0
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        while self.cur < self.code.len() - 1 {
            self.skip_space();

            if let Some(num) = self.get_number() {
                tokens.push(Token {
                    kind: TokenKind::Num(num),
                    at: self.cur,
                });
                continue;
            }
            let ps = self.get_punct_size();
            if ps != 0 {
                tokens.push(Token {
                    kind: TokenKind::Reserved(&self.code[self.cur..(self.cur + ps)]),
                    at: self.cur,
                });
                self.cur += ps;
            }
        }
        tokens
    }
}