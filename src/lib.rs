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
    pub propixx_trigger: Option<Trigger>,
    pub response: Option<Response>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Response {
    pub choice: Choice,
    pub trigger: Trigger,
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
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

#[derive(Debug, PartialEq)]
pub struct ReactionTimeStats {
    pub mean_ms: f64,
    pub std_ms: f64,
}

#[derive(Debug, PartialEq)]
pub struct Accuracy {
    pub percent: f32,
    pub count: usize,
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
    let mut tokens = line.split_whitespace();
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

pub fn write_triggers<W>(file: &mut W, triggers: &[Trigger]) -> anyhow::Result<()>
where
    W: std::io::Write,
{
    writeln!(file, "Tmu\tCode\tTriNo").context("Unable to write to file")?;
    for trigger in triggers {
        writeln!(file, "{}\t1\t{}", trigger.time_microseconds, trigger.code)
            .context("Unable to write to file")?;
    }
    Ok(())
}

pub fn find_trials<S, T>(
    triggers: &[Trigger],
    to_stimulus: T,
    button_choices: &std::collections::BTreeSet<Button>,
) -> Vec<Trial<S>>
where
    T: Fn(&Trigger) -> Option<S>,
{
    let indexed_stimuli = find_stimulus_indices(triggers, to_stimulus, button_choices);
    let bounds: Vec<_> = indexed_stimuli
        .iter()
        .map(|(i, _)| *i)
        .chain([triggers.len()])
        .collect();
    bounds
        .windows(2)
        .zip(indexed_stimuli.into_iter().map(|(_, stimulus)| stimulus))
        .map(|(window, stimulus)| {
            triggers_to_trial(&triggers[window[0]..window[1]], stimulus, button_choices)
        })
        .collect()
}

const PROPIXXBIT: usize = 12;

fn find_stimulus_indices<S, T>(
    triggers: &[Trigger],
    to_stimulus: T,
    button_choices: &std::collections::BTreeSet<Button>,
) -> Vec<(usize, S)>
where
    T: Fn(&Trigger) -> Option<S>,
{
    let mut last_stimulus_time_microseconds = None;
    triggers
        .iter()
        .enumerate()
        .filter_map(|(index, trigger)| {
            if let Some(stimulus) = to_stimulus(&Trigger {
                time_microseconds: trigger.time_microseconds,
                code: trigger.code
                    & !(1 << PROPIXXBIT)
                    & !button_choices
                        .iter()
                        .fold(0, |a, b| a | (1 << button_bit(b))),
            }) {
                if last_stimulus_time_microseconds
                    .is_some_and(|u| trigger.time_microseconds - u <= 16000)
                {
                    None
                } else {
                    last_stimulus_time_microseconds = Some(trigger.time_microseconds);
                    Some((index, stimulus))
                }
            } else {
                None
            }
        })
        .collect()
}

pub fn triggers_to_trial<S>(
    triggers: &[Trigger],
    stimulus: S,
    button_choices: &std::collections::BTreeSet<Button>,
) -> Trial<S> {
    let stimulus_trigger = &triggers[0];
    let propixx_trigger = triggers
        .iter()
        .find(|t| {
            has_bit_set(t.code, PROPIXXBIT)
                && t.time_microseconds - stimulus_trigger.time_microseconds <= 34_000
        })
        .cloned();
    let response = find_response(triggers, button_choices);

    Trial {
        stimulus,
        stimulus_trigger: stimulus_trigger.clone(),
        propixx_trigger,
        response,
    }
}

pub fn find_response(
    triggers: &[Trigger],
    button_choices: &std::collections::BTreeSet<Button>,
) -> Option<Response> {
    triggers.windows(2).find_map(|window| {
        let previous_trigger = &window[0];
        let trigger = &window[1];
        let mut chosen_buttons = button_choices
            .iter()
            .filter(|button| has_bit_set(trigger.code, button_bit(button)));
        if let Some(button) = chosen_buttons.next() {
            match chosen_buttons.next() {
                Some(_) => Some(Choice::Ambiguous),
                None => {
                    if has_bit_set(previous_trigger.code, button_bit(button))
                        && trigger.code != (1 << button_bit(button))
                    {
                        None
                    } else {
                        Some(Choice::Clearly(button.clone()))
                    }
                }
            }
        } else {
            None
        }
        .map(|choice| Response {
            choice,
            trigger: trigger.clone(),
        })
    })
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

pub fn accuracy<T: Iterator<Item = Evaluation> + Clone>(evaluations: T) -> Accuracy {
    let count = evaluations
        .clone()
        .filter(|e| match e {
            Evaluation::Correct(_) => true,
            Evaluation::Incorrect => false,
        })
        .count();
    let total_count = evaluations.count();
    let percent = 100. * count as f32 / total_count as f32;

    Accuracy { percent, count }
}

// f64 can represent all integers less than or equal to 2^53.
// This includes all integer microsecond values less than 285 years.
// You're not going to lose precision by casting a microsecond value to a f64 here.
pub fn reaction_time_stats<T: Iterator<Item = Evaluation> + Clone>(
    evaluations: T,
) -> ReactionTimeStats {
    let reaction_times_ms = evaluations.clone().flat_map(|e| match e {
        Evaluation::Correct(rt) => Some(rt.microseconds as f64 / 1000.),
        Evaluation::Incorrect => None,
    });
    let rt_sum_ms = reaction_times_ms.clone().sum::<f64>();
    let rt_count = reaction_times_ms.clone().count();
    let mean_ms = rt_sum_ms / rt_count as f64;
    //https://rust-lang-nursery.github.io/rust-cookbook/science/mathematics/statistics.html
    // MATLAB uses a normalization factor of N - 1: https://www.mathworks.com/help/matlab/ref/std.html
    let std_ms = if rt_count > 0 {
        let variance = reaction_times_ms
            .map(|rt_ms| {
                let diff = mean_ms - rt_ms;
                diff * diff
            })
            .sum::<f64>()
            / (rt_count - 1) as f64;
        variance.sqrt()
    } else {
        f64::NAN
    };

    ReactionTimeStats { mean_ms, std_ms }
}

pub fn reaction_time_is_outlier(stats: &ReactionTimeStats, rt: &ReactionTime) -> bool {
    (rt.microseconds as f64 / 1000. - stats.mean_ms).abs() >= 3. * stats.std_ms
}

#[cfg(test)]
mod tests {
    use super::accuracy;
    use super::evaluate_trial;
    use super::find_trials;
    use super::parse_triggers;
    use super::reaction_time_stats;
    use super::Accuracy;
    use super::Button;
    use super::Choice;
    use super::Evaluation;
    use super::ReactionTime;
    use super::ReactionTimeStats;
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
                    propixx_trigger: None,
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
                    propixx_trigger: None,
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
                &std::collections::BTreeSet::from([Button::One, Button::Two])
            )
        )
    }

    #[test]
    fn finds_trials_with_propixx() {
        assert_eq!(
            vec![
                Trial {
                    stimulus: "hello",
                    stimulus_trigger: Trigger {
                        time_microseconds: 9697000,
                        code: 40
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 9705000,
                        code: 4136
                    }),
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 10676000,
                            code: 512
                        },
                        choice: Choice::Clearly(Button::Two)
                    })
                },
                Trial {
                    stimulus: "hello",
                    stimulus_trigger: Trigger {
                        time_microseconds: 14299000,
                        code: 40
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 14307000,
                        code: 4136
                    }),
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 15053000,
                            code: 256
                        },
                        choice: Choice::Clearly(Button::One)
                    })
                }
            ],
            find_trials(
                &[
                    Trigger {
                        time_microseconds: 6547000,
                        code: 4156
                    },
                    Trigger {
                        time_microseconds: 7700000,
                        code: 32
                    },
                    Trigger {
                        time_microseconds: 7720000,
                        code: 4096
                    },
                    Trigger {
                        time_microseconds: 9697000,
                        code: 40
                    },
                    Trigger {
                        time_microseconds: 9705000,
                        code: 4136
                    },
                    Trigger {
                        time_microseconds: 10676000,
                        code: 512
                    },
                    Trigger {
                        time_microseconds: 10699000,
                        code: 4156
                    },
                    Trigger {
                        time_microseconds: 12302000,
                        code: 31
                    },
                    Trigger {
                        time_microseconds: 12323000,
                        code: 4096
                    },
                    Trigger {
                        time_microseconds: 14299000,
                        code: 40
                    },
                    Trigger {
                        time_microseconds: 14307000,
                        code: 4136
                    },
                    Trigger {
                        time_microseconds: 15053000,
                        code: 256
                    },
                ],
                |trigger| match trigger.code {
                    40 => Some("hello"),
                    _ => None,
                },
                &std::collections::BTreeSet::from([Button::One, Button::Two, Button::Three])
            )
        )
    }

    #[test]
    fn finds_trials_with_combined_propixx() {
        assert_eq!(
            vec![
                Trial {
                    stimulus: "hello",
                    stimulus_trigger: Trigger {
                        time_microseconds: 9705000,
                        code: 4136
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 9705000,
                        code: 4136
                    }),
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 10676000,
                            code: 512
                        },
                        choice: Choice::Clearly(Button::Two)
                    })
                },
                Trial {
                    stimulus: "hello",
                    stimulus_trigger: Trigger {
                        time_microseconds: 14307000,
                        code: 4136
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 14307000,
                        code: 4136
                    }),
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 15053000,
                            code: 256
                        },
                        choice: Choice::Clearly(Button::One)
                    })
                }
            ],
            find_trials(
                &[
                    Trigger {
                        time_microseconds: 6547000,
                        code: 4156
                    },
                    Trigger {
                        time_microseconds: 7700000,
                        code: 32
                    },
                    Trigger {
                        time_microseconds: 7720000,
                        code: 4096
                    },
                    Trigger {
                        time_microseconds: 9705000,
                        code: 4136
                    },
                    Trigger {
                        time_microseconds: 10676000,
                        code: 512
                    },
                    Trigger {
                        time_microseconds: 10699000,
                        code: 4156
                    },
                    Trigger {
                        time_microseconds: 12302000,
                        code: 31
                    },
                    Trigger {
                        time_microseconds: 12323000,
                        code: 4096
                    },
                    Trigger {
                        time_microseconds: 14307000,
                        code: 4136
                    },
                    Trigger {
                        time_microseconds: 15053000,
                        code: 256
                    },
                ],
                |trigger| match trigger.code {
                    40 => Some("hello"),
                    _ => None,
                },
                &std::collections::BTreeSet::from([Button::One, Button::Two, Button::Three])
            )
        )
    }

    #[test]
    fn finds_trials_with_duplicate_triggers() {
        assert_eq!(
            vec![
                Trial {
                    stimulus: "hello",
                    stimulus_trigger: Trigger {
                        time_microseconds: 9697000,
                        code: 40
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 9705000,
                        code: 4136
                    }),
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 10676000,
                            code: 512
                        },
                        choice: Choice::Clearly(Button::Two)
                    })
                },
                Trial {
                    stimulus: "hello",
                    stimulus_trigger: Trigger {
                        time_microseconds: 14299000,
                        code: 40
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 14307000,
                        code: 4136
                    }),
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 15053000,
                            code: 256
                        },
                        choice: Choice::Clearly(Button::One)
                    })
                }
            ],
            find_trials(
                &[
                    Trigger {
                        time_microseconds: 6547000,
                        code: 4156
                    },
                    Trigger {
                        time_microseconds: 7700000,
                        code: 32
                    },
                    Trigger {
                        time_microseconds: 7720000,
                        code: 4096
                    },
                    Trigger {
                        time_microseconds: 9697000,
                        code: 40
                    },
                    Trigger {
                        time_microseconds: 9698000,
                        code: 40
                    },
                    Trigger {
                        time_microseconds: 9705000,
                        code: 4136
                    },
                    Trigger {
                        time_microseconds: 10676000,
                        code: 512
                    },
                    Trigger {
                        time_microseconds: 10699000,
                        code: 4156
                    },
                    Trigger {
                        time_microseconds: 12302000,
                        code: 31
                    },
                    Trigger {
                        time_microseconds: 12323000,
                        code: 4096
                    },
                    Trigger {
                        time_microseconds: 14299000,
                        code: 40
                    },
                    Trigger {
                        time_microseconds: 14300000,
                        code: 40
                    },
                    Trigger {
                        time_microseconds: 14307000,
                        code: 4136
                    },
                    Trigger {
                        time_microseconds: 15053000,
                        code: 256
                    },
                ],
                |trigger| match trigger.code {
                    40 => Some("hello"),
                    _ => None,
                },
                &std::collections::BTreeSet::from([Button::One, Button::Two, Button::Three])
            )
        )
    }

    #[test]
    fn finds_trials_with_nonadjacent_duplicate_trigger() {
        assert_eq!(
            vec![
                Trial {
                    stimulus: "a",
                    stimulus_trigger: Trigger {
                        time_microseconds: 39907000,
                        code: 42
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 39917000,
                        code: 4138
                    }),
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 42158000,
                            code: 256
                        },
                        choice: Choice::Clearly(Button::One)
                    })
                },
                Trial {
                    stimulus: "b",
                    stimulus_trigger: Trigger {
                        time_microseconds: 44202000,
                        code: 44
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 44219000,
                        code: 4096
                    }),
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 45812000,
                            code: 512
                        },
                        choice: Choice::Clearly(Button::Two)
                    })
                }
            ],
            find_trials(
                &[
                    Trigger {
                        time_microseconds: 38153000,
                        code: 4146
                    },
                    Trigger {
                        time_microseconds: 39907000,
                        code: 42
                    },
                    Trigger {
                        time_microseconds: 39913000,
                        code: 46
                    },
                    Trigger {
                        time_microseconds: 39917000,
                        code: 4138
                    },
                    Trigger {
                        time_microseconds: 42158000,
                        code: 256
                    },
                    Trigger {
                        time_microseconds: 42610000,
                        code: 4146
                    },
                    Trigger {
                        time_microseconds: 44202000,
                        code: 44
                    },
                    Trigger {
                        time_microseconds: 44219000,
                        code: 4096
                    },
                    Trigger {
                        time_microseconds: 45812000,
                        code: 512
                    },
                ],
                |trigger| match trigger.code {
                    42 => Some("a"),
                    44 => Some("b"),
                    _ => None,
                },
                &std::collections::BTreeSet::from([Button::One, Button::Two])
            )
        )
    }

    #[test]
    fn wild_5th_bit() {
        assert_eq!(
            vec![Trial {
                stimulus: "hey",
                stimulus_trigger: Trigger {
                    code: 23,
                    time_microseconds: 291763008
                },
                propixx_trigger: Some(Trigger {
                    code: 4103,
                    time_microseconds: 291775008
                }),
                response: Some(Response {
                    choice: Choice::Clearly(Button::Two),
                    trigger: Trigger {
                        code: 512,
                        time_microseconds: 292801984
                    }
                })
            },],
            find_trials(
                &[
                    Trigger {
                        code: 4156,
                        time_microseconds: 289998016
                    },
                    Trigger {
                        code: 23,
                        time_microseconds: 291763008
                    },
                    Trigger {
                        code: 4103,
                        time_microseconds: 291775008
                    },
                    Trigger {
                        code: 512,
                        time_microseconds: 292801984
                    },
                    Trigger {
                        code: 4156,
                        time_microseconds: 293764992
                    },
                ],
                |t| {
                    let masked = t.code & !(1 << 4);
                    match masked {
                        4..=9 => Some("hey"),
                        _ => None,
                    }
                },
                &std::collections::BTreeSet::from([Button::One, Button::Two])
            )
        )
    }

    #[test]
    fn finds_trial_with_lateish_propixx() {
        assert_eq!(
            vec![
                Trial {
                    stimulus: "a",
                    stimulus_trigger: Trigger {
                        code: 34,
                        time_microseconds: 12450000
                    },
                    propixx_trigger: Some(Trigger {
                        code: 4096,
                        time_microseconds: 12476000
                    }),
                    response: Some(Response {
                        trigger: Trigger {
                            code: 512,
                            time_microseconds: 14023000
                        },
                        choice: Choice::Clearly(Button::Two)
                    })
                },
                Trial {
                    stimulus: "b",
                    stimulus_trigger: Trigger {
                        code: 41,
                        time_microseconds: 16058000
                    },
                    propixx_trigger: Some(Trigger {
                        code: 4137,
                        time_microseconds: 16070000
                    }),
                    response: Some(Response {
                        trigger: Trigger {
                            code: 256,
                            time_microseconds: 17501000
                        },
                        choice: Choice::Clearly(Button::One)
                    })
                }
            ],
            find_trials(
                &[
                    Trigger {
                        code: 4116,
                        time_microseconds: 10946000
                    },
                    Trigger {
                        code: 34,
                        time_microseconds: 12450000
                    },
                    Trigger {
                        code: 4096,
                        time_microseconds: 12476000
                    },
                    Trigger {
                        code: 512,
                        time_microseconds: 14023000
                    },
                    Trigger {
                        code: 4116,
                        time_microseconds: 14453000
                    },
                    Trigger {
                        code: 41,
                        time_microseconds: 16058000
                    },
                    Trigger {
                        code: 4137,
                        time_microseconds: 16070000
                    },
                    Trigger {
                        code: 256,
                        time_microseconds: 17501000
                    },
                ],
                |trigger| match trigger.code {
                    34 => Some("a"),
                    41 => Some("b"),
                    _ => None,
                },
                &std::collections::BTreeSet::from([Button::One, Button::Two])
            )
        )
    }

    #[test]
    fn finds_trials_masked_by_button() {
        assert_eq!(
            vec![
                Trial {
                    stimulus: "a",
                    stimulus_trigger: Trigger {
                        time_microseconds: 14119000,
                        code: 11
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 14126000,
                        code: 4107
                    }),
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 14171000,
                            code: 4352
                        },
                        choice: Choice::Clearly(Button::One)
                    })
                },
                Trial {
                    stimulus: "a",
                    stimulus_trigger: Trigger {
                        time_microseconds: 18319000,
                        code: 267
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 18330000,
                        code: 4363
                    }),
                    response: None
                }
            ],
            find_trials(
                &[
                    Trigger {
                        time_microseconds: 14119000,
                        code: 11
                    },
                    Trigger {
                        time_microseconds: 14126000,
                        code: 4107
                    },
                    Trigger {
                        time_microseconds: 14171000,
                        code: 4352
                    },
                    Trigger {
                        time_microseconds: 18284000,
                        code: 256
                    },
                    Trigger {
                        time_microseconds: 18319000,
                        code: 267
                    },
                    Trigger {
                        time_microseconds: 18330000,
                        code: 4363
                    },
                ],
                |trigger| match trigger.code {
                    11 => Some("a"),
                    _ => None,
                },
                &std::collections::BTreeSet::from([Button::One])
            )
        )
    }

    #[test]
    fn finds_trials_masked_by_button_2() {
        assert_eq!(
            vec![
                Trial {
                    stimulus: "a",
                    stimulus_trigger: Trigger {
                        time_microseconds: 64516000,
                        code: 11
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 64527000,
                        code: 4363
                    }),
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 64521000,
                            code: 267
                        },
                        choice: Choice::Clearly(Button::One)
                    })
                },
                Trial {
                    stimulus: "a",
                    stimulus_trigger: Trigger {
                        time_microseconds: 68716000,
                        code: 267
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 68730000,
                        code: 4352
                    }),
                    response: Some(Response {
                        trigger: Trigger {
                            time_microseconds: 72906000,
                            code: 256
                        },
                        choice: Choice::Clearly(Button::One)
                    })
                }
            ],
            find_trials(
                &[
                    Trigger {
                        time_microseconds: 64516000,
                        code: 11
                    },
                    Trigger {
                        time_microseconds: 64521000,
                        code: 267
                    },
                    Trigger {
                        time_microseconds: 64527000,
                        code: 4363
                    },
                    Trigger {
                        time_microseconds: 68634000,
                        code: 256
                    },
                    Trigger {
                        time_microseconds: 68716000,
                        code: 267
                    },
                    Trigger {
                        time_microseconds: 68730000,
                        code: 4352
                    },
                    Trigger {
                        time_microseconds: 72906000,
                        code: 256
                    },
                ],
                |trigger| match trigger.code {
                    11 => Some("a"),
                    _ => None,
                },
                &std::collections::BTreeSet::from([Button::One])
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
                    propixx_trigger: None,
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
                    propixx_trigger: None,
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
                    propixx_trigger: None,
                    response: None
                },
                Trial {
                    stimulus: 2,
                    stimulus_trigger: Trigger {
                        time_microseconds: 12,
                        code: 6
                    },
                    propixx_trigger: None,
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
    fn calculates_accuracy() {
        assert_eq!(
            Accuracy {
                percent: 60.,
                count: 3
            },
            accuracy(
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

    #[test]
    fn calculates_rt_stats() {
        assert_eq!(
            ReactionTimeStats {
                mean_ms: 3.,
                std_ms: 2.6457513110645905,
            },
            reaction_time_stats(
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

    #[test]
    fn calculates_accuracy_2() {
        assert_eq!(
            Accuracy {
                percent: 57.1428571428571,
                count: 4
            },
            accuracy(
                [
                    Evaluation::Correct(ReactionTime { microseconds: 1234 }),
                    Evaluation::Incorrect,
                    Evaluation::Incorrect,
                    Evaluation::Correct(ReactionTime { microseconds: 5678 }),
                    Evaluation::Correct(ReactionTime { microseconds: 9876 }),
                    Evaluation::Incorrect,
                    Evaluation::Correct(ReactionTime { microseconds: 5432 }),
                ]
                .into_iter()
            )
        )
    }

    #[test]
    fn calculates_rt_stats_2() {
        assert_eq!(
            ReactionTimeStats {
                mean_ms: 5.555,
                std_ms: 3.5295108254072074,
            },
            reaction_time_stats(
                [
                    Evaluation::Correct(ReactionTime { microseconds: 1234 }),
                    Evaluation::Incorrect,
                    Evaluation::Incorrect,
                    Evaluation::Correct(ReactionTime { microseconds: 5678 }),
                    Evaluation::Correct(ReactionTime { microseconds: 9876 }),
                    Evaluation::Incorrect,
                    Evaluation::Correct(ReactionTime { microseconds: 5432 }),
                ]
                .into_iter()
            )
        )
    }

    #[test]
    fn calculates_accuracy_all_incorrect() {
        assert_eq!(
            Accuracy {
                percent: 0.,
                count: 0
            },
            accuracy(
                [
                    Evaluation::Incorrect,
                    Evaluation::Incorrect,
                    Evaluation::Incorrect,
                ]
                .into_iter()
            )
        )
    }

    #[test]
    fn calculates_rt_stats_all_incorrect() {
        let stats = reaction_time_stats(
            [
                Evaluation::Incorrect,
                Evaluation::Incorrect,
                Evaluation::Incorrect,
            ]
            .into_iter(),
        );
        assert!(stats.std_ms.is_nan());
        assert!(stats.mean_ms.is_nan());
    }

    #[test]
    fn calculates_accuracy_none() {
        let acc = accuracy([].into_iter());
        assert!(acc.percent.is_nan());
        assert_eq!(0, acc.count);
    }

    #[test]
    fn calculates_rt_stats_none() {
        let stats = reaction_time_stats([].into_iter());
        assert!(stats.std_ms.is_nan());
        assert!(stats.mean_ms.is_nan());
    }
}
