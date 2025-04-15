use anyhow::{anyhow, Context};

type Microseconds = i64;

#[derive(Debug, PartialEq, Copy, Clone)]
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
    pub evaluation: Evaluation,
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ReactionTime {
    pub microseconds: Microseconds,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReactionTimes {
    stimulus: ReactionTime,
    propixx: Option<ReactionTime>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Evaluation {
    Correct(ReactionTimes),
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

#[derive(Debug, PartialEq)]
pub struct Behavior {
    accuracy: Accuracy,
    rt_stats: ReactionTimeStats,
}

fn parse_triggers(input: &str) -> anyhow::Result<Vec<Trigger>> {
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

pub fn find_trials<S, T, Q>(
    input: &str,
    to_stimulus: T,
    button_choices: &[Button],
    button_is_correct: Q,
) -> anyhow::Result<Vec<Trial<S>>>
where
    T: Fn(&Trigger) -> Option<S>,
    Q: Fn(&Button, &S) -> bool,
{
    let triggers = parse_triggers(input)?;
    let indexed_stimuli = find_stimulus_indices(&triggers, to_stimulus, button_choices);
    let bounds: Vec<_> = indexed_stimuli
        .iter()
        .map(|(i, _)| *i)
        .chain([triggers.len()])
        .collect();
    Ok(bounds
        .windows(2)
        .zip(indexed_stimuli.into_iter().map(|(_, stimulus)| stimulus))
        .map(|(window, stimulus)| {
            triggers_to_trial(
                &triggers[window[0]..window[1]],
                stimulus,
                button_choices,
                &button_is_correct,
            )
        })
        .collect())
}

const PROPIXXBIT: usize = 12;

fn find_stimulus_indices<S, T>(
    triggers: &[Trigger],
    to_stimulus: T,
    button_choices: &[Button],
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

fn triggers_to_trial<S, Q>(
    triggers: &[Trigger],
    stimulus: S,
    button_choices: &[Button],
    button_is_correct: &Q,
) -> Trial<S>
where
    Q: Fn(&Button, &S) -> bool,
{
    let stimulus_trigger = triggers[0];
    let propixx_trigger = triggers
        .iter()
        .find(|t| {
            has_bit_set(t.code, PROPIXXBIT)
                && t.time_microseconds - stimulus_trigger.time_microseconds <= 34_000
        })
        .cloned();
    let response = find_response(triggers, button_choices);
    let mut evaluation = Evaluation::Incorrect;
    if let Some(response) = &response {
        if let Choice::Clearly(button) = &response.choice {
            if button_is_correct(button, &stimulus) {
                evaluation = Evaluation::Correct(ReactionTimes {
                    stimulus: ReactionTime {
                        microseconds: response.trigger.time_microseconds
                            - stimulus_trigger.time_microseconds,
                    },
                    propixx: propixx_trigger.map(|t| ReactionTime {
                        microseconds: response.trigger.time_microseconds - t.time_microseconds,
                    }),
                });
            }
        }
    }

    Trial {
        stimulus,
        stimulus_trigger,
        propixx_trigger,
        response,
        evaluation,
    }
}

fn find_response(triggers: &[Trigger], button_choices: &[Button]) -> Option<Response> {
    triggers.windows(2).find_map(|window| {
        let previous_trigger = &window[0];
        let trigger = window[1];
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
        .map(|choice| Response { choice, trigger })
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

// f64 can represent all integers less than or equal to 2^53.
// This includes all integer microsecond values less than 285 years.
// You're not going to lose precision by casting a microsecond value to a f64 here.
pub fn behavior_matching<S, P, T>(
    trials: &[Trial<S>],
    predicate: P,
    to_reaction_time: T,
) -> anyhow::Result<Behavior>
where
    P: Fn(&S) -> bool,
    T: Fn(&ReactionTimes) -> Option<ReactionTime>,
{
    let filtered = trials.iter().filter(|t| predicate(&t.stimulus));
    let reaction_times_ms = filtered
        .clone()
        .flat_map(|t| match &t.evaluation {
            Evaluation::Correct(rts) => Some(
                to_reaction_time(rts)
                    .map(|rt| rt.microseconds as f64 / 1000.)
                    .ok_or(anyhow::anyhow!(
                        "missing reaction time for stimulus at {} us",
                        t.stimulus_trigger.time_microseconds
                    )),
            ),
            Evaluation::Incorrect => None,
        })
        .collect::<anyhow::Result<Vec<_>>>()?;
    let correct_count = reaction_times_ms.len();
    let correct_percent = 100. * correct_count as f32 / filtered.count() as f32;
    let rt_mean_ms = reaction_times_ms.iter().sum::<f64>() / correct_count as f64;
    //https://rust-lang-nursery.github.io/rust-cookbook/science/mathematics/statistics.html
    // MATLAB uses a normalization factor of N - 1: https://www.mathworks.com/help/matlab/ref/std.html
    let rt_std_ms = if correct_count > 0 {
        let variance = reaction_times_ms
            .iter()
            .map(|rt_ms| {
                let diff = rt_mean_ms - rt_ms;
                diff * diff
            })
            .sum::<f64>()
            / (correct_count - 1) as f64;
        variance.sqrt()
    } else {
        f64::NAN
    };

    Ok(Behavior {
        accuracy: Accuracy {
            percent: correct_percent,
            count: correct_count,
        },
        rt_stats: ReactionTimeStats {
            mean_ms: rt_mean_ms,
            std_ms: rt_std_ms,
        },
    })
}

pub fn reaction_time_is_outlier(stats: &ReactionTimeStats, rt: &ReactionTime) -> bool {
    (rt.microseconds as f64 / 1000. - stats.mean_ms).abs() >= 3. * stats.std_ms
}

#[cfg(test)]
mod tests {
    use super::behavior_matching;
    use super::find_trials;
    use super::parse_triggers;
    use super::Accuracy;
    use super::Button;
    use super::Choice;
    use super::Evaluation;
    use super::ReactionTime;
    use super::ReactionTimeStats;
    use super::ReactionTimes;
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
                    }),
                    evaluation: Evaluation::Incorrect
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
                    }),
                    evaluation: Evaluation::Incorrect
                }
            ],
            find_trials(
                "Tmu         	Code	TriNo	Comnt	Ver-C
372508992	1	60	FIFF Trigger: 60
372521984	1	4096	FIFF Trigger: 4096
373920000	1	42	FIFF Trigger: 42
375921984	1	60	FIFF Trigger: 60
375932000	1	4156	FIFF Trigger: 4156
376340992	1	512	FIFF Trigger: 512
377353984	1	42	FIFF Trigger: 42
378139008	1	256	FIFF Trigger: 256
",
                |trigger| match trigger.code {
                    42 => Some("hello"),
                    _ => None,
                },
                &[Button::One, Button::Two],
                |_, _| false
            )
            .unwrap()
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
                    }),
                    evaluation: Evaluation::Incorrect
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
                    }),
                    evaluation: Evaluation::Incorrect
                }
            ],
            find_trials(
                "Tmu         	Code	TriNo	Comnt	Ver-C
6547000	1	4156	FIFF Trigger: 4156
7700000	1	32	FIFF Trigger: 32
7720000	1	4096	FIFF Trigger: 4096
9697000	1	40	FIFF Trigger: 40
9705000	1	4136	FIFF Trigger: 4136
10676000	1	512	FIFF Trigger: 512
10699000	1	4156	FIFF Trigger: 4156
12302000	1	31	FIFF Trigger: 31
12323000	1	4096	FIFF Trigger: 4096
14299000	1	40	FIFF Trigger: 40
14307000	1	4136	FIFF Trigger: 4136
15053000	1	256	FIFF Trigger: 256
",
                |trigger| match trigger.code {
                    40 => Some("hello"),
                    _ => None,
                },
                &[Button::One, Button::Two, Button::Three],
                |_, _| false
            )
            .unwrap()
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
                    }),
                    evaluation: Evaluation::Incorrect
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
                    }),
                    evaluation: Evaluation::Incorrect
                }
            ],
            find_trials(
                "Tmu         	Code	TriNo	Comnt	Ver-C
6547000	1	4156	FIFF Trigger: 4156
7700000	1	32	FIFF Trigger: 32
7720000	1	4096	FIFF Trigger: 4096
9705000	1	4136	FIFF Trigger: 4136
10676000	1	512	FIFF Trigger: 512
10699000	1	4156	FIFF Trigger: 4156
12302000	1	31	FIFF Trigger: 31
12323000	1	4096	FIFF Trigger: 4096
14307000	1	4136	FIFF Trigger: 4136
15053000	1	256	FIFF Trigger: 256
",
                |trigger| match trigger.code {
                    40 => Some("hello"),
                    _ => None,
                },
                &[Button::One, Button::Two, Button::Three],
                |_, _| false
            )
            .unwrap()
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
                    }),
                    evaluation: Evaluation::Incorrect
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
                    }),
                    evaluation: Evaluation::Incorrect
                }
            ],
            find_trials(
                "Tmu         	Code	TriNo	Comnt	Ver-C
6547000	1	4156	FIFF Trigger: 4156
7700000	1	32	FIFF Trigger: 32
7720000	1	4096	FIFF Trigger: 4096
9697000	1	40	FIFF Trigger: 40
9698000	1	40	FIFF Trigger: 40
9705000	1	4136	FIFF Trigger: 4136
10676000	1	512	FIFF Trigger: 512
10699000	1	4156	FIFF Trigger: 4156
12302000	1	31	FIFF Trigger: 31
12323000	1	4096	FIFF Trigger: 4096
14299000	1	40	FIFF Trigger: 40
14300000	1	40	FIFF Trigger: 40
14307000	1	4136	FIFF Trigger: 4136
15053000	1	256	FIFF Trigger: 256
",
                |trigger| match trigger.code {
                    40 => Some("hello"),
                    _ => None,
                },
                &[Button::One, Button::Two, Button::Three],
                |_, _| false
            )
            .unwrap()
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
                    }),
                    evaluation: Evaluation::Incorrect
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
                    }),
                    evaluation: Evaluation::Incorrect
                }
            ],
            find_trials(
                "Tmu         	Code	TriNo	Comnt	Ver-C
38153000	1	4146	FIFF Trigger: 4146
39907000	1	42	FIFF Trigger: 42
39913000	1	46	FIFF Trigger: 46
39917000	1	4138	FIFF Trigger: 4138
42158000	1	256	FIFF Trigger: 256
42610000	1	4146	FIFF Trigger: 4146
44202000	1	44	FIFF Trigger: 44
44219000	1	4096	FIFF Trigger: 4096
45812000	1	512	FIFF Trigger: 512
",
                |trigger| match trigger.code {
                    42 => Some("a"),
                    44 => Some("b"),
                    _ => None,
                },
                &[Button::One, Button::Two],
                |_, _| false
            )
            .unwrap()
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
                }),
                evaluation: Evaluation::Incorrect
            },],
            find_trials(
                "Tmu         	Code	TriNo	Comnt	Ver-C
289998016	1	4156	FIFF Trigger: 4156
291763008	1	23	FIFF Trigger: 23
291775008	1	4103	FIFF Trigger: 4103
292801984	1	512	FIFF Trigger: 512
293764992	1	4156	FIFF Trigger: 4156
",
                |t| {
                    let masked = t.code & !(1 << 4);
                    match masked {
                        4..=9 => Some("hey"),
                        _ => None,
                    }
                },
                &[Button::One, Button::Two],
                |_, _| false
            )
            .unwrap()
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
                    }),
                    evaluation: Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime {
                            microseconds: 14023000 - 12450000
                        },
                        propixx: Some(ReactionTime {
                            microseconds: 14023000 - 12476000
                        })
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
                    }),
                    evaluation: Evaluation::Incorrect
                }
            ],
            find_trials(
                "Tmu         	Code	TriNo	Comnt	Ver-C
10946000	1	4116	FIFF Trigger: 4116
12450000	1	34	FIFF Trigger: 34
12476000	1	4096	FIFF Trigger: 4096
14023000	1	512	FIFF Trigger: 512
14453000	1	4116	FIFF Trigger: 4116
16058000	1	41	FIFF Trigger: 41
16070000	1	4137	FIFF Trigger: 4137
17501000	1	256	FIFF Trigger: 256
",
                |trigger| match trigger.code {
                    34 => Some("a"),
                    41 => Some("b"),
                    _ => None,
                },
                &[Button::One, Button::Two],
                |button, _| *button == Button::Two
            )
            .unwrap()
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
                    }),
                    evaluation: Evaluation::Incorrect
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
                    response: None,
                    evaluation: Evaluation::Incorrect
                }
            ],
            find_trials(
                "Tmu         	Code	TriNo	Comnt	Ver-C
14119000	1	11	FIFF Trigger: 11
14126000	1	4107	FIFF Trigger: 4107
14171000	1	4352	FIFF Trigger: 4352
18284000	1	256	FIFF Trigger: 256
18319000	1	267	FIFF Trigger: 267
18330000	1	4363	FIFF Trigger: 4363
",
                |trigger| match trigger.code {
                    11 => Some("a"),
                    _ => None,
                },
                &[Button::One],
                |_, _| false
            )
            .unwrap()
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
                    }),
                    evaluation: Evaluation::Incorrect
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
                    }),
                    evaluation: Evaluation::Incorrect
                }
            ],
            find_trials(
                "Tmu         	Code	TriNo	Comnt	Ver-C
64516000	1	11	FIFF Trigger: 11
64521000	1	267	FIFF Trigger: 267
64527000	1	4363	FIFF Trigger: 4363
68634000	1	256	FIFF Trigger: 256
68716000	1	267	FIFF Trigger: 267
68730000	1	4352	FIFF Trigger: 4352
72906000	1	256	FIFF Trigger: 256
",
                |trigger| match trigger.code {
                    11 => Some("a"),
                    _ => None,
                },
                &[Button::One],
                |_, _| false
            )
            .unwrap()
        )
    }

    #[test]
    fn propixx_rts() {
        let trials = find_trials(
            "Tmu         	Code	TriNo	Comnt	Ver-C
4590000        	1	20	FIFF Trigger: 20                        
6243000        	1	23	FIFF Trigger: 23                        
6258000        	1	4096	FIFF Trigger: 4096                      
7745000        	1	25	FIFF Trigger: 25                        
10248000       	1	33	FIFF Trigger: 33                        
10260000       	1	4096	FIFF Trigger: 4096                      
10925000       	1	256	FIFF Trigger: 256                       
11554000       	1	20	FIFF Trigger: 20                        
12857000       	1	24	FIFF Trigger: 24                        
12878000       	1	4096	FIFF Trigger: 4096                      
14360000       	1	25	FIFF Trigger: 25                        
16863000       	1	34	FIFF Trigger: 34                        
16880000       	1	4096	FIFF Trigger: 4096                      
17647000       	1	512	FIFF Trigger: 512                       
18168000       	1	20	FIFF Trigger: 20                        
19520000       	1	23	FIFF Trigger: 23                        
19531000       	1	4096	FIFF Trigger: 4096                      
21023000       	1	25	FIFF Trigger: 25                        
23526000       	1	33	FIFF Trigger: 33                        
23550000       	1	4096	FIFF Trigger: 4096                      
24351000       	1	256	FIFF Trigger: 256                       
",
            |trigger| match trigger.code {
                33 => Some(Button::One),
                34 => Some(Button::Two),
                _ => None,
            },
            &[Button::One, Button::Two],
            |b, s| b == s,
        )
        .unwrap();
        assert_eq!(
            vec![
                Trial {
                    stimulus: Button::One,
                    stimulus_trigger: Trigger {
                        time_microseconds: 10248000,
                        code: 33
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 10260000,
                        code: 4096
                    }),
                    response: Some(Response {
                        choice: Choice::Clearly(Button::One),
                        trigger: Trigger {
                            time_microseconds: 10925000,
                            code: 256
                        }
                    }),
                    evaluation: Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime {
                            microseconds: 10925000 - 10248000
                        },
                        propixx: Some(ReactionTime {
                            microseconds: 10925000 - 10260000
                        })
                    })
                },
                Trial {
                    stimulus: Button::Two,
                    stimulus_trigger: Trigger {
                        time_microseconds: 16863000,
                        code: 34
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 16880000,
                        code: 4096
                    }),
                    response: Some(Response {
                        choice: Choice::Clearly(Button::Two),
                        trigger: Trigger {
                            time_microseconds: 17647000,
                            code: 512
                        }
                    }),
                    evaluation: Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime {
                            microseconds: 17647000 - 16863000
                        },
                        propixx: Some(ReactionTime {
                            microseconds: 17647000 - 16880000
                        })
                    })
                },
                Trial {
                    stimulus: Button::One,
                    stimulus_trigger: Trigger {
                        time_microseconds: 23526000,
                        code: 33
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 23550000,
                        code: 4096
                    }),
                    response: Some(Response {
                        choice: Choice::Clearly(Button::One),
                        trigger: Trigger {
                            time_microseconds: 24351000,
                            code: 256
                        }
                    }),
                    evaluation: Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime {
                            microseconds: 24351000 - 23526000
                        },
                        propixx: Some(ReactionTime {
                            microseconds: 24351000 - 23550000
                        })
                    })
                }
            ],
            trials
        );

        let behavior = behavior_matching(&trials, |_| true, |rts| rts.propixx).unwrap();
        assert_eq!(100., behavior.accuracy.percent);
        assert_eq!(3, behavior.accuracy.count);
        assert_eq!(
            (24351 - 23550 + 17647 - 16880 + 10925 - 10260) as f64 / 3.,
            behavior.rt_stats.mean_ms
        );

        let button_one_behavior =
            behavior_matching(&trials, |s| *s == Button::One, |rts| rts.propixx).unwrap();
        assert_eq!(100., button_one_behavior.accuracy.percent);
        assert_eq!(2, button_one_behavior.accuracy.count);
        assert_eq!(
            (24351 - 23550 + 10925 - 10260) as f64 / 2.,
            button_one_behavior.rt_stats.mean_ms
        );
    }

    #[test]
    fn propixx_rts_2() {
        let trials = find_trials(
            "Tmu         	Code	TriNo	Comnt	Ver-C
218442000      	1	20	FIFF Trigger: 20                        
219694000      	1	24	FIFF Trigger: 24                        
219715008      	1	4096	FIFF Trigger: 4096                      
221196000      	1	25	FIFF Trigger: 25                        
223699008      	1	34	FIFF Trigger: 34                        
223718000      	1	4096	FIFF Trigger: 4096                      
225002000      	1	20	FIFF Trigger: 20                        
225264000      	1	512	FIFF Trigger: 512                       
226404000      	1	24	FIFF Trigger: 24                        
226420000      	1	4096	FIFF Trigger: 4096                      
227906000      	1	25	FIFF Trigger: 25                        
230408000      	1	34	FIFF Trigger: 34                        
230415008      	1	4096	FIFF Trigger: 4096                      
231711008      	1	20	FIFF Trigger: 20                        
231908992      	1	512	FIFF Trigger: 512                       
232912992      	1	23	FIFF Trigger: 23                        
232916992      	1	4096	FIFF Trigger: 4096                      
234415008      	1	25	FIFF Trigger: 25                        
236918000      	1	33	FIFF Trigger: 33                        
236938000      	1	4096	FIFF Trigger: 4096                      
",
            |trigger| match trigger.code {
                33 => Some(Button::One),
                34 => Some(Button::Two),
                _ => None,
            },
            &[Button::One, Button::Two],
            |b, s| b == s,
        )
        .unwrap();
        assert_eq!(
            vec![
                Trial {
                    stimulus: Button::Two,
                    stimulus_trigger: Trigger {
                        time_microseconds: 223699008,
                        code: 34
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 223718000,
                        code: 4096
                    }),
                    response: Some(Response {
                        choice: Choice::Clearly(Button::Two),
                        trigger: Trigger {
                            time_microseconds: 225264000,
                            code: 512
                        }
                    }),
                    evaluation: Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime {
                            microseconds: 225264000 - 223699008
                        },
                        propixx: Some(ReactionTime {
                            microseconds: 225264000 - 223718000
                        })
                    })
                },
                Trial {
                    stimulus: Button::Two,
                    stimulus_trigger: Trigger {
                        time_microseconds: 230408000,
                        code: 34
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 230415008,
                        code: 4096
                    }),
                    response: Some(Response {
                        choice: Choice::Clearly(Button::Two),
                        trigger: Trigger {
                            time_microseconds: 231908992,
                            code: 512
                        }
                    }),
                    evaluation: Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime {
                            microseconds: 231908992 - 230408000
                        },
                        propixx: Some(ReactionTime {
                            microseconds: 231908992 - 230415008
                        })
                    })
                },
                Trial {
                    stimulus: Button::One,
                    stimulus_trigger: Trigger {
                        time_microseconds: 236918000,
                        code: 33
                    },
                    propixx_trigger: Some(Trigger {
                        time_microseconds: 236938000,
                        code: 4096
                    }),
                    response: None,
                    evaluation: Evaluation::Incorrect
                }
            ],
            trials
        );

        let behavior = behavior_matching(&trials, |_| true, |rts| rts.propixx).unwrap();
        assert_eq!(2. * 100. / 3., behavior.accuracy.percent);
        assert_eq!(2, behavior.accuracy.count);
        assert_eq!(
            (231908992 - 230415008 + 225264000 - 223718000) as f64 / 1000. / 2.,
            behavior.rt_stats.mean_ms
        );

        let button_one_behavior =
            behavior_matching(&trials, |s| *s == Button::One, |rts| rts.propixx).unwrap();
        assert_eq!(0., button_one_behavior.accuracy.percent);
        assert_eq!(0, button_one_behavior.accuracy.count);
        assert!(button_one_behavior.rt_stats.mean_ms.is_nan());
    }

    fn trialify_evaluations(evals: impl IntoIterator<Item = Evaluation>) -> Vec<Trial<()>> {
        evals
            .into_iter()
            .map(|evaluation| Trial {
                stimulus: (),
                stimulus_trigger: Trigger {
                    time_microseconds: 0,
                    code: 0,
                },
                propixx_trigger: None,
                response: None,
                evaluation,
            })
            .collect()
    }

    #[test]
    fn calculates_accuracy() {
        assert_eq!(
            Accuracy {
                percent: 60.,
                count: 3
            },
            behavior_matching(
                &trialify_evaluations([
                    Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime { microseconds: 1000 },
                        propixx: None
                    }),
                    Evaluation::Incorrect,
                    Evaluation::Incorrect,
                    Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime { microseconds: 2000 },
                        propixx: None
                    }),
                    Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime { microseconds: 6000 },
                        propixx: None
                    }),
                ]),
                |_| true,
                |rts| Some(rts.stimulus)
            )
            .unwrap()
            .accuracy
        )
    }

    #[test]
    fn calculates_rt_stats() {
        assert_eq!(
            ReactionTimeStats {
                mean_ms: 3.,
                std_ms: 2.6457513110645905,
            },
            behavior_matching(
                &trialify_evaluations([
                    Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime { microseconds: 1000 },
                        propixx: None
                    }),
                    Evaluation::Incorrect,
                    Evaluation::Incorrect,
                    Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime { microseconds: 2000 },
                        propixx: None
                    }),
                    Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime { microseconds: 6000 },
                        propixx: None
                    }),
                ]),
                |_| true,
                |rt| Some(rt.stimulus)
            )
            .unwrap()
            .rt_stats
        )
    }

    #[test]
    fn calculates_accuracy_2() {
        assert_eq!(
            Accuracy {
                percent: 57.1428571428571,
                count: 4
            },
            behavior_matching(
                &trialify_evaluations([
                    Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime { microseconds: 1234 },
                        propixx: None
                    }),
                    Evaluation::Incorrect,
                    Evaluation::Incorrect,
                    Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime { microseconds: 5678 },
                        propixx: None
                    }),
                    Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime { microseconds: 9876 },
                        propixx: None
                    }),
                    Evaluation::Incorrect,
                    Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime { microseconds: 5432 },
                        propixx: None
                    }),
                ]),
                |_| true,
                |rts| Some(rts.stimulus)
            )
            .unwrap()
            .accuracy
        )
    }

    #[test]
    fn calculates_rt_stats_2() {
        assert_eq!(
            ReactionTimeStats {
                mean_ms: 5.555,
                std_ms: 3.5295108254072074,
            },
            behavior_matching(
                &trialify_evaluations([
                    Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime { microseconds: 1234 },
                        propixx: None
                    }),
                    Evaluation::Incorrect,
                    Evaluation::Incorrect,
                    Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime { microseconds: 5678 },
                        propixx: None
                    }),
                    Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime { microseconds: 9876 },
                        propixx: None
                    }),
                    Evaluation::Incorrect,
                    Evaluation::Correct(ReactionTimes {
                        stimulus: ReactionTime { microseconds: 5432 },
                        propixx: None
                    }),
                ]),
                |_| true,
                |rt| Some(rt.stimulus)
            )
            .unwrap()
            .rt_stats
        )
    }

    #[test]
    fn calculates_accuracy_all_incorrect() {
        assert_eq!(
            Accuracy {
                percent: 0.,
                count: 0
            },
            behavior_matching(
                &trialify_evaluations([
                    Evaluation::Incorrect,
                    Evaluation::Incorrect,
                    Evaluation::Incorrect,
                ]),
                |_| true,
                |rts| Some(rts.stimulus)
            )
            .unwrap()
            .accuracy
        )
    }

    #[test]
    fn calculates_rt_stats_all_incorrect() {
        let stats = behavior_matching(
            &trialify_evaluations([
                Evaluation::Incorrect,
                Evaluation::Incorrect,
                Evaluation::Incorrect,
            ]),
            |_| true,
            |rt| Some(rt.stimulus),
        )
        .unwrap()
        .rt_stats;
        assert!(stats.std_ms.is_nan());
        assert!(stats.mean_ms.is_nan());
    }

    #[test]
    fn calculates_accuracy_none() {
        let acc = behavior_matching(&[], |_: &()| true, |rts| Some(rts.stimulus))
            .unwrap()
            .accuracy;
        assert!(acc.percent.is_nan());
        assert_eq!(0, acc.count);
    }

    #[test]
    fn calculates_rt_stats_none() {
        let stats = behavior_matching(&[], |_: &()| true, |rt| Some(rt.stimulus))
            .unwrap()
            .rt_stats;
        assert!(stats.std_ms.is_nan());
        assert!(stats.mean_ms.is_nan());
    }

    #[test]
    fn missing_propixx_rt() {
        let behavior = behavior_matching(
            &trialify_evaluations([
                Evaluation::Correct(ReactionTimes {
                    stimulus: ReactionTime { microseconds: 1000 },
                    propixx: Some(ReactionTime { microseconds: 1000 }),
                }),
                Evaluation::Incorrect,
                Evaluation::Incorrect,
                Evaluation::Correct(ReactionTimes {
                    stimulus: ReactionTime { microseconds: 2000 },
                    propixx: None,
                }),
                Evaluation::Correct(ReactionTimes {
                    stimulus: ReactionTime { microseconds: 6000 },
                    propixx: Some(ReactionTime { microseconds: 6000 }),
                }),
            ]),
            |_| true,
            |rt| rt.propixx,
        );
        assert!(behavior.is_err());
    }
}
