// The Rust runtime for Menhir.
// All the generated parsers run the function below that implements a classic
// LR(1) automaton with their generated parse table.

#[macro_use] extern crate log;
use std::ops::Index;

// The type of semantic actions.
// Some(ptr) = a flat (not closure) code pointer to the handler
// None = this is a start reduction. it's actually never executed but indicates
// an Accept action instead. Since NULL is not a valid function pointer in correct
// Rust code, this should be optimized to be just the size of a function pointer.
pub type SemAct<YYType> = Option<fn(usize, &mut Vec<(usize, YYType)>) -> usize>;

// An action, i.e. an entry in the action table. Error, accept, reduce with a
// semantic function that takes the stack, modifie it and returns the next state
// or shift to state, discarding the current token.
pub enum Action<YYType> {
    Err,
    Reduce(SemAct<YYType>),
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
pub fn parse<TokenType, LexerType, ParseTableType, DefRedType, YYType>(
        mut lexer: &mut LexerType,
        parse_table: &ParseTableType,
        default_reduction: &DefRedType,
        next_token: fn(&mut LexerType) -> (YYType, usize),
        initial: usize
    ) -> Result<Stack<YYType>, ()>
    where LexerType:      Iterator<Item = TokenType>,
          ParseTableType: Index<(usize, usize), Output = Action<YYType>>,
          DefRedType:     Index<usize, Output = Option<SemAct<YYType>>> {

    // the current state
    let mut state = initial;
    let mut stack = Vec::new();

    // the current token and its semantic data
    let (mut yylval, mut tok) = next_token(&mut lexer);

    // the parsing loop
    'a: loop {
        debug!("current state: {}, token: {}", state, tok);
        match parse_table[(state, tok)] {
            Action::Shift(shift) => {
                stack.push((state, yylval));
                state = shift;

                while let Some(red) = default_reduction[state] {
                    match red {
                        Some(code) => state = code(state, &mut stack),
                        None => break 'a
                    }
                }

                // discard
                let (nval, ntok) = next_token(&mut lexer);
                tok = ntok;
                yylval = nval;
            }

            Action::Reduce(Some(reduce)) => {
                state = reduce(state, &mut stack);
                while let Some(red) = default_reduction[state] {
                    match red {
                        Some(code) => state = code(state, &mut stack),
                        None => break 'a
                    }
                }
            }

            Action::Reduce(None) => break,
            Action::Err => return Err(())
        }
    }

    // Extracting the final semantic value requires to know the label of the
    // YYType variant that corresponds to the start symbol. We don't have this
    // information here so we just return the stack and let the generated code
    // do something with it...
    Ok(stack)
}
