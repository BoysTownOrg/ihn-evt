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
pub enum Behavior {
    Correct(ReactionTime),
    Incorrect,
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

pub fn find_trials<S, Q, T>(triggers: &[Trigger], is_stimulus: Q, to_stimulus: T) -> Vec<Trial<S>>
where
    Q: Fn(&Trigger) -> bool,
    T: Fn(&Trigger) -> S,
{
    let triggers = remove_duplicate_triggers(triggers);
    find_stimulus_indices(&triggers, is_stimulus)
        .windows(2)
        .map(|stimulus_bounds| {
            triggers_to_trial(
                &triggers[stimulus_bounds[0]..stimulus_bounds[1]],
                &to_stimulus,
            )
        })
        .collect()
}

fn remove_duplicate_triggers(triggers: &[Trigger]) -> Vec<Trigger> {
    triggers
        .iter()
        .take(1)
        .chain(
            triggers
                .windows(2)
                .enumerate()
                .filter(|(_, window)| {
                    let first = &window[0];
                    let second = &window[1];
                    first.code != second.code
                        || second.time_microseconds - first.time_microseconds > 1024
                })
                .map(|(index, _)| &triggers[index + 1]),
        )
        .cloned()
        .collect()
}

fn find_stimulus_indices<T>(triggers: &[Trigger], is_stimulus: T) -> Vec<usize>
where
    T: Fn(&Trigger) -> bool,
{
    triggers
        .into_iter()
        .enumerate()
        .filter(|(_, event)| is_stimulus(event))
        .map(|(index, _)| index)
        .chain([triggers.len()].into_iter())
        .collect()
}

fn triggers_to_trial<T, S>(triggers: &[Trigger], to_stimulus: T) -> Trial<S>
where
    T: Fn(&Trigger) -> S,
{
    let stimulus_trigger = &triggers[0];
    let stimulus = to_stimulus(stimulus_trigger);
    let stimulus_onset_microseconds = stimulus_trigger.time_microseconds;
    let response = if let Some(response_index) = response_index(triggers) {
        Some(Response {
            choice: if has_bit_set(triggers[response_index].code, BUTTON1BIT)
                && has_bit_set(triggers[response_index].code, BUTTON2BIT)
            {
                Choice::Ambiguous
            } else if has_bit_set(triggers[response_index].code, BUTTON1BIT) {
                Choice::Clearly(Button::One)
            } else if has_bit_set(triggers[response_index].code, BUTTON2BIT) {
                Choice::Clearly(Button::Two)
            } else {
                panic!("Can't get here!")
            },
            time_microseconds: triggers[response_index].time_microseconds,
        })
    } else {
        None
    };

    Trial {
        stimulus,
        stimulus_onset_microseconds,
        response,
    }
}

const BUTTON1BIT: usize = 8;
const BUTTON2BIT: usize = 9;

fn response_index(triggers: &[Trigger]) -> Option<usize> {
    triggers.into_iter().position(|trigger| {
        let button1_mask = 1 << BUTTON1BIT;
        let button2_mask = 1 << BUTTON2BIT;
        trigger.code & (button1_mask | button2_mask) != 0
    })
}

fn has_bit_set(x: i32, n: usize) -> bool {
    x & (1 << n) == (1 << n)
}

pub fn trials_to_behavior<S, Q, T: Iterator<Item = Trial<S>>>(
    trials: T,
    button_is_correct: Q,
) -> impl Iterator<Item = Behavior>
where
    Q: Fn(&Button, &S) -> bool,
{
    trials.map(move |trial| {
        if let Some(response) = &trial.response {
            if let Choice::Clearly(button) = &response.choice {
                if button_is_correct(button, &trial.stimulus) {
                    return Behavior::Correct(ReactionTime {
                        microseconds: response.time_microseconds
                            - trial.stimulus_onset_microseconds,
                    });
                }
            }
        }
        Behavior::Incorrect
    })
}

#[cfg(test)]
mod tests {
    use super::find_trials;
    use super::parse_triggers;
    use super::trials_to_behavior;
    use super::Behavior;
    use super::Button;
    use super::Choice;
    use super::ReactionTime;
    use super::Response;
    use super::Trial;
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

    #[test]
    fn finds_two_trials_with_almost_late_response() {
        assert_eq!(
            vec![
                Trial {
                    stimulus: "hello".to_string(),
                    stimulus_onset_microseconds: 373920000,
                    response: Some(Response {
                        time_microseconds: 376340992,
                        choice: Choice::Clearly(Button::Two)
                    })
                },
                Trial {
                    stimulus: "hello".to_string(),
                    stimulus_onset_microseconds: 377353984,
                    response: Some(Response {
                        time_microseconds: 378139008,
                        choice: Choice::Clearly(Button::One)
                    })
                }
            ],
            find_trials(
                &[
                    Trigger {
                        time_microseconds: 372508992,
                        code: 60
                    },
                    Trigger {
                        time_microseconds: 372521984,
                        code: 4096
                    },
                    Trigger {
                        time_microseconds: 373920000,
                        code: 42
                    },
                    Trigger {
                        time_microseconds: 375921984,
                        code: 60
                    },
                    Trigger {
                        time_microseconds: 375932000,
                        code: 4156
                    },
                    Trigger {
                        time_microseconds: 376340992,
                        code: 512
                    },
                    Trigger {
                        time_microseconds: 377353984,
                        code: 42
                    },
                    Trigger {
                        time_microseconds: 378139008,
                        code: 256
                    },
                ],
                |trigger| trigger.code == 42,
                |_| "hello".to_string()
            )
        )
    }

    #[test]
    fn behavior() {
        assert_eq!(
            vec![
                Behavior::Correct(ReactionTime {
                    microseconds: 376340992 - 373920000
                }),
                Behavior::Incorrect,
                Behavior::Incorrect,
                Behavior::Correct(ReactionTime {
                    microseconds: 35 - 12
                }),
            ],
            trials_to_behavior(
                [
                    Trial {
                        stimulus: 1,
                        stimulus_onset_microseconds: 373920000,
                        response: Some(Response {
                            time_microseconds: 376340992,
                            choice: Choice::Clearly(Button::One)
                        })
                    },
                    Trial {
                        stimulus: 1,
                        stimulus_onset_microseconds: 377353984,
                        response: Some(Response {
                            time_microseconds: 378139008,
                            choice: Choice::Clearly(Button::Two)
                        })
                    },
                    Trial {
                        stimulus: 2,
                        stimulus_onset_microseconds: 88,
                        response: None
                    },
                    Trial {
                        stimulus: 2,
                        stimulus_onset_microseconds: 12,
                        response: Some(Response {
                            time_microseconds: 35,
                            choice: Choice::Clearly(Button::Two)
                        })
                    }
                ]
                .into_iter(),
                |button, &stimulus| match button {
                    Button::One => stimulus == 1,
                    Button::Two => stimulus == 2,
                }
            )
            .collect::<Vec<_>>()
        )
    }
}
