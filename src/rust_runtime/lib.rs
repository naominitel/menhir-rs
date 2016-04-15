// The Rust runtime for Menhir.
// All the generated parsers run the function below that implements a classic
// LR(1) automaton with their generated parse table.

#[macro_use] extern crate log;
use std::ops::Index;

// An action, i.e. an entry in the action table. Error, accept, reduce with a
// semantic function that takes the stack, modifie it and returns the next state
// or shift to state, discarding the current token.
pub enum Action<YYType> {
    Err,
    Acc,
    Reduce(fn(usize, &mut Vec<(usize, YYType)>) -> usize),
    Shift(usize)
}

pub type Stack<YYType> = Vec<(usize, YYType)>;

// To avoid unnecessary out-of-bounds checks (we know that the parser will never
// attempt out-of-bounds access on the parse table), we pass the tables to the
// parse loop as pointers to fixed-sized arrays rather than slices. The problem
// is that there is no unique type for fixed-size arrays, so we are
// parameterized here by the concrete type of the table with a trait bound that
// allows us to index it by a state and a token. (To simplify, we use a tuple as
// index. This avoids an additionnal type parameter.). In practice the type will
// always be [[Action ; N] ; M] wrapped in a type that implements indexing by a
// tuple over it.
// We don't need the goto table, only the semantic actions will actually use it.
pub fn parse<TokenType, LexerType, ParseTableType, YYType>(
        mut lexer: &mut LexerType, parse_table: &ParseTableType,
        next_token: fn(&mut LexerType) -> (YYType, usize),
        initial: usize
    ) -> Result<Stack<YYType>, ()>
    where LexerType:      Iterator<Item = TokenType>,
          ParseTableType: Index<(usize, usize), Output = Action<YYType>> {
    // the current state
    let mut state = initial;

    // the current token and its semantic data
    let (mut yylval, mut tok) = next_token(&mut lexer);
    let mut stack = Vec::new();

    // the parsing loop
    loop {
        debug!("current state: {}, token: {}", state, tok);
        match parse_table[(state, tok)] {
            Action::Shift(shift) => {
                stack.push((state, yylval));
                state = shift;
                let (v, t) = next_token(&mut lexer);
                tok = t;
                yylval = v;
            }

            Action::Reduce(reduce) => state = reduce(state, &mut stack),
            Action::Acc => break,
            Action::Err => return Err(())
        }
    }

    // Extracting the final semantic value requires to know the label of the
    // YYType variant that corresponds to the start symbol. We don't have this
    // information here so we just return the stack and let the generated code
    // do something with it...
    Ok(stack)
}
