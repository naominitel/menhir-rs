// The Rust runtime for Menhir.
// All the generated parsers run the function below that implements a classic
// LR(1) automaton with their generated parse table.

pub type Stack<YYType, State> = Vec<(State, YYType)>;

// The type of semantic actions.
// Some(ptr) = a flat (not closure) code pointer to the handler
// None = this is a start reduction. it's actually never executed but indicates
// an Accept action instead. Since NULL is not a valid function pointer in correct
// Rust code, this should be optimized to be just the size of a function pointer.
pub type SemAct<YYType, State> = Option<fn(State, &mut Stack<YYType, State>) -> State>;

// An action, i.e. an entry in the action table. Error, accept, reduce with a
// semantic function that takes the stack, modifie it and returns the next state
// or shift to state, discarding the current token.
pub enum Action<YYType, State> {
    Err,
    Reduce(SemAct<YYType, State>),
    Shift(State)
}

impl<T, U: Copy> Copy for Action<T, U> {}

impl<T, U: Clone> Clone for Action<T, U> {
    fn clone(&self) -> Self {
        match *self {
            Action::Err => Action::Err,
            Action::Reduce(act) => Action::Reduce(act),
            Action::Shift(ref shift) => Action::Shift(shift.clone())
        }
    }
}

pub trait LRParser {
    type Terminal: Copy;
    type State: Copy;
    type YYType;

    fn default_reduction(state: Self::State)
                         -> Option<SemAct<Self::YYType, Self::State>>;
    fn action(state: Self::State, token: Self::Terminal)
              -> Action<Self::YYType, Self::State>;
}

fn next_token<Lexer, Parser>(lexer: &mut Lexer)
                             -> Result<(Parser::YYType, Parser::Terminal), ()>
    where Lexer: Iterator,
          Parser: LRParser,
          Lexer::Item: Into<(Parser::YYType, Parser::Terminal)> {
    match lexer.next() {
        Some(tok) => Ok(tok.into()),
        None => Err(())
    }
}

pub fn parse<Lexer, Parser>(mut lexer: &mut Lexer, initial: Parser::State)
                            -> Result<Stack<Parser::YYType, Parser::State>, ()>
    where Lexer: Iterator,
          Parser: LRParser,
          Lexer::Item: Into<(Parser::YYType, Parser::Terminal)> {

    // the current state
    let mut state = initial;
    let mut stack = Vec::new();

    // the current token and its semantic data
    let (mut yylval, mut tok) = try!(next_token::<_, Parser>(&mut lexer));

    // the parsing loop
    'a: loop {
        match Parser::action(state, tok) {
            Action::Shift(shift) => {
                stack.push((state, yylval));
                state = shift;

                while let Some(red) = Parser::default_reduction(state) {
                    match red {
                        Some(code) => state = code(state, &mut stack),
                        None => break 'a
                    }
                }

                // discard
                let (nval, ntok) = try!(next_token::<_, Parser>(&mut lexer));
                tok = ntok;
                yylval = nval;
            }

            Action::Reduce(Some(reduce)) => {
                state = reduce(state, &mut stack);
                while let Some(red) = Parser::default_reduction(state) {
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
