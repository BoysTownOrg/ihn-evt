use anyhow::{anyhow, Context};

type Microseconds = i64;

#[derive(Debug, PartialEq, Clone)]
pub struct Trigger {
    pub time_microseconds: Microseconds,
    pub code: i32,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Trial<S> {
    pub stimulus: S,
    pub stimulus_trigger: Trigger,
    pub response: Option<Response>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Response {
    pub choice: Choice,
    pub trigger: Trigger,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Button {
    One,
    Two,
    Three,
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
pub struct Behavior {
    pub mean_reaction_time_milliseconds: Option<i64>,
    pub accuracy_percentage: f32,
}

pub fn parse_triggers(input: &str) -> anyhow::Result<Vec<Trigger>> {
    input
        .lines()
        .enumerate()
        .filter(|(_, line)| line.contains("FIFF Trigger"))
        .map(|(i, line)| parse_trigger_line(line).with_context(|| format!("on line {i}")))
        .collect()
}

fn parse_trigger_line(line: &str) -> anyhow::Result<Trigger> {
    let mut tokens = line.trim().split_whitespace();
    let time_microseconds = tokens
        .next()
        .ok_or(anyhow!("missing time (Tmu)"))?
        .parse::<i64>()
        .map_err(|_| anyhow!("unable to parse time (Tmu) as integer"))?;
    tokens.next();
    let code = tokens
        .next()
        .ok_or(anyhow!("missing trigger number (TriNo)"))?
        .parse::<i32>()
        .map_err(|_| anyhow!("unable to parse trigger number (TriNo) as integer"))?;
    Ok(Trigger {
        time_microseconds,
        code,
    })
}

pub fn find_trials<S, T>(
    triggers: &[Trigger],
    to_stimulus: T,
    button_choices: &[Button],
) -> Vec<Trial<S>>
where
    T: Fn(&Trigger) -> Option<S>,
{
    let triggers = remove_duplicate_triggers(triggers);
    let indexed_stimuli = find_stimulus_indices(&triggers, to_stimulus);
    let bounds: Vec<_> = indexed_stimuli
        .iter()
        .map(|(i, _)| *i)
        .chain([triggers.len()].into_iter())
        .collect();
    bounds
        .windows(2)
        .zip(indexed_stimuli.into_iter().map(|(_, s)| s))
        .map(|(window, s)| triggers_to_trial(&triggers[window[0]..window[1]], s, button_choices))
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

fn find_stimulus_indices<S, T>(triggers: &[Trigger], to_stimulus: T) -> Vec<(usize, S)>
where
    T: Fn(&Trigger) -> Option<S>,
{
    triggers
        .into_iter()
        .enumerate()
        .filter_map(|(index, event)| to_stimulus(event).map(|s| (index, s)))
        .collect()
}

fn triggers_to_trial<S>(triggers: &[Trigger], stimulus: S, button_choices: &[Button]) -> Trial<S> {
    let stimulus_trigger = &triggers[0];
    let response = triggers.iter().find_map(|trigger| {
        let mut chosen_buttons = button_choices
            .iter()
            .filter(|button| has_bit_set(trigger.code, button_bit(button)));
        if let Some(button) = chosen_buttons.next() {
            Some(match chosen_buttons.next() {
                Some(_) => Choice::Ambiguous,
                None => Choice::Clearly(button.clone()),
            })
        } else {
            None
        }
        .map(|choice| Response {
            choice,
            trigger: trigger.clone(),
        })
    });

    Trial {
        stimulus,
        stimulus_trigger: stimulus_trigger.clone(),
        response,
    }
}

fn button_bit(button: &Button) -> usize {
    match button {
        Button::One => 8,
        Button::Two => 9,
        Button::Three => 10,
    }
}

fn has_bit_set(x: i32, n: usize) -> bool {
    x & (1 << n) != 0
}

pub fn evaluate_trial<S, Q>(trial: &Trial<S>, button_is_correct: Q) -> Evaluation
where
    Q: Fn(&Button, &S) -> bool,
{
    if let Some(response) = &trial.response {
        if let Choice::Clearly(button) = &response.choice {
            if button_is_correct(button, &trial.stimulus) {
                return Evaluation::Correct(ReactionTime {
                    microseconds: response.trigger.time_microseconds
                        - trial.stimulus_trigger.time_microseconds,
                });
            }
        }
    }
    Evaluation::Incorrect
}

pub fn get_behavior<T: Iterator<Item = Evaluation> + Clone>(evaluations: T) -> Behavior {
    Behavior {
        mean_reaction_time_milliseconds: {
            let reaction_times_microseconds = evaluations
                .clone()
                .map(|e| match e {
                    Evaluation::Correct(reaction_time) => Some(reaction_time.microseconds),
                    Evaluation::Incorrect => None,
                })
                .flatten();
            let count = reaction_times_microseconds.clone().count();
            if count > 0 {
                Some(rounded_divide(
                    rounded_divide(reaction_times_microseconds.sum(), count.try_into().unwrap()),
                    1000,
                ))
            } else {
                None
            }
        },
        accuracy_percentage: {
            let count = evaluations.clone().count();
            if count > 0 {
                100. * evaluations
                    .filter(|e| match e {
                        Evaluation::Correct(_) => true,
                        Evaluation::Incorrect => false,
                    })
                    .count() as f32
                    / count as f32
            } else {
                std::f32::NAN
            }
        },
    }
}

fn rounded_divide(dividend: i64, divisor: i64) -> i64 {
    (dividend + divisor / 2) / divisor
}

#[cfg(test)]
mod tests {
    use super::evaluate_trial;
    use super::find_trials;
    use super::get_behavior;
    use super::parse_triggers;
    use super::Behavior;
    use super::Button;
    use super::Choice;
    use super::Evaluation;
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
            format!("{:#}", triggers.err().unwrap())
        );
    }

    #[test]
    fn finds_two_trials_with_almost_late_response() {
        assert_eq!(
            vec![
                Trial {
                    stimulus: "hello",
                    stimulus_trigger: Trigger {
                        time_microseconds: 373920000,
                        code: 42
                    },
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 376340992,
                            code: 512
                        },
                        choice: Choice::Clearly(Button::Two)
                    })
                },
                Trial {
                    stimulus: "hello",
                    stimulus_trigger: Trigger {
                        time_microseconds: 377353984,
                        code: 42
                    },
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 378139008,
                            code: 256
                        },
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
                |trigger| match trigger.code {
                    42 => Some("hello"),
                    _ => None,
                },
                &[Button::One, Button::Two]
            )
        )
    }

    #[test]
    fn evaluates_trials() {
        assert_eq!(
            vec![
                Evaluation::Correct(ReactionTime {
                    microseconds: 376340992 - 373920000
                }),
                Evaluation::Incorrect,
                Evaluation::Incorrect,
                Evaluation::Correct(ReactionTime {
                    microseconds: 35 - 12
                }),
            ],
            [
                Trial {
                    stimulus: 1,
                    stimulus_trigger: Trigger {
                        time_microseconds: 373920000,
                        code: 1
                    },
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 376340992,
                            code: 2
                        },
                        choice: Choice::Clearly(Button::One)
                    })
                },
                Trial {
                    stimulus: 1,
                    stimulus_trigger: Trigger {
                        time_microseconds: 377353984,
                        code: 3
                    },
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 378139008,
                            code: 4
                        },
                        choice: Choice::Clearly(Button::Two)
                    })
                },
                Trial {
                    stimulus: 2,
                    stimulus_trigger: Trigger {
                        time_microseconds: 88,
                        code: 5
                    },
                    response: None
                },
                Trial {
                    stimulus: 2,
                    stimulus_trigger: Trigger {
                        time_microseconds: 12,
                        code: 6
                    },
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 35,
                            code: 7
                        },
                        choice: Choice::Clearly(Button::Two)
                    })
                }
            ]
            .into_iter()
            .map(
                |trial| evaluate_trial(&trial, |button, &stimulus| match button {
                    Button::One => stimulus == 1,
                    Button::Two => stimulus == 2,
                    Button::Three => stimulus == 3,
                })
            )
            .collect::<Vec<_>>()
        )
    }

    #[test]
    fn gets_behavior() {
        assert_eq!(
            Behavior {
                mean_reaction_time_milliseconds: Some(3),
                accuracy_percentage: 60.
            },
            get_behavior(
                [
                    Evaluation::Correct(ReactionTime { microseconds: 1000 }),
                    Evaluation::Incorrect,
                    Evaluation::Incorrect,
                    Evaluation::Correct(ReactionTime { microseconds: 2000 }),
                    Evaluation::Correct(ReactionTime { microseconds: 6000 }),
                ]
                .into_iter()
            )
        )
    }
}
