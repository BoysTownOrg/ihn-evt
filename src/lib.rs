type Microseconds = i64;

#[derive(Debug, PartialEq, Clone)]
pub struct Trigger {
    pub time_microseconds: Microseconds,
    pub code: i32,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Trial<S> {
    pub stimulus: S,
    pub stimulus_onset_microseconds: Microseconds,
    pub response: Option<Response>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Response {
    pub choice: Choice,
    pub time_microseconds: i64,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Button {
    One,
    Two,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Choice {
    Clearly(Button),
    Ambiguous,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReactionTime {
    pub microseconds: Microseconds,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Evaluation {
    Correct(ReactionTime),
    Incorrect,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Behavior<S> {
    pub stimulus: S,
    pub evaluation: Evaluation,
}

#[derive(Debug)]
pub struct ContextualError {
    pub context: Vec<String>,
    pub detailed: Option<String>,
    what: &'static str,
}

impl From<&'static str> for ContextualError {
    fn from(what: &'static str) -> Self {
        ContextualError {
            context: vec![],
            detailed: None,
            what,
        }
    }
}

impl std::error::Error for ContextualError {}

impl std::fmt::Display for ContextualError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in self.context.iter().rev() {
            write!(f, "{c}: ")?;
        }
        if let Some(detail) = &self.detailed {
            write!(f, "{}: {}", self.what, detail)
        } else {
            write!(f, "{}", self.what)
        }
    }
}

pub fn parse_triggers(input: &str) -> Result<Vec<Trigger>, ContextualError> {
    input
        .lines()
        .enumerate()
        .filter(|(_, line)| line.contains("FIFF Trigger"))
        .map(|(i, line)| {
            parse_trigger_line(line).map_err(|mut e| {
                e.context.push(format!("on line {i}"));
                e
            })
        })
        .collect()
}

fn parse_trigger_line(line: &str) -> Result<Trigger, ContextualError> {
    let mut tokens = line.trim().split_whitespace();
    let time_microseconds = tokens
        .next()
        .ok_or("missing time (Tmu)")?
        .parse::<i64>()
        .map_err(|_| "unable to parse time (Tmu) as integer")?;
    tokens.next();
    let code = tokens
        .next()
        .ok_or("missing trigger number (TriNo)")?
        .parse::<i32>()
        .map_err(|_| "unable to parse trigger number (TriNo) as integer")?;
    Ok(Trigger {
        time_microseconds,
        code,
    })
}

#[cfg(test)]
mod tests {
    use super::parse_triggers;
    use super::Trigger;

    #[test]
    fn parses_triggers() {
        let triggers = parse_triggers(
            "Tmu         	Code	TriNo	Comnt	Ver-C
2142000        	1	4096	FIFF Trigger: 4096                      
2202000        	1	60	FIFF Trigger: 60                        
2209000        	1	4156	FIFF Trigger: 4156                      
Someone put something unexpected on this line
",
        )
        .unwrap();
        assert_eq!(
            vec![
                Trigger {
                    time_microseconds: 2142000,
                    code: 4096
                },
                Trigger {
                    time_microseconds: 2202000,
                    code: 60
                },
                Trigger {
                    time_microseconds: 2209000,
                    code: 4156
                }
            ],
            triggers
        );
    }

    #[test]
    fn parse_triggers_detects_bad_trigger_number() {
        let triggers = parse_triggers(
            "Tmu         	Code	TriNo	Comnt	Ver-C
2142000        	1	4096	FIFF Trigger: 4096                      
2202000        	1	6?	FIFF Trigger: 60                        
2209000        	1	4156	FIFF Trigger: 4156                      
",
        );
        assert_eq!(
            "on line 2: unable to parse trigger number (TriNo) as integer",
            format!("{}", triggers.err().unwrap())
        );
    }
}
