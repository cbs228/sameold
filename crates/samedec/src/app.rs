//! State machine logic for child processes
//!
//! The state machine has two states:
//!
//! 1. `Waiting`: Looking for a SAME message
//!
//! 2. `Alerting`: A SAME message is being received and
//!     written to a child process
//!
//! ```txt
//!   start
//!   ||                                        ||=== ZCZC … ==||
//!   \/                                        ||             ||
//! +-------------+                    +--------------+        ||
//! |   Waiting   | == ZCZC | NNNN ==> |   Alerting   | <======||
//! +-------------+                    +--------------+
//!   ||      /\                            ||
//!   ||      ||=== NNNN, EOF, no child ====||
//!   ||
//!   \/
//!   EOF
//! ```
//!
//! For `Message::StartOfMessage`, the Alerting` state will run a child
//! process until the next `Message` or EOF is received. The `Alerting`
//! state will also return to `Waiting` if there is no child process
//! to start.
//!
//! The `Waiting` state will exit when the input iterator is exhausted.

use std::process::Child;

use byteorder::{NativeEndian, WriteBytesExt};
use chrono::{DateTime, Utc};
use clap::ArgMatches;
use log::{debug, error, warn};
use sameold::{Message, MessageHeader, SameReceiver};

use crate::spawner;

/// Run the application
///
/// Runs the `samedec` state machine with the given command-line
/// `args`, a fully-initialized `receiver`, and an `input` iterator
/// which returns each `i16` sample from some input source until
/// it is exhausted.
///
/// In demo mode (see `args`), we print a demo message, wait, and
/// then exit.
pub fn run<I>(args: &ArgMatches, receiver: &mut SameReceiver, mut input: I)
where
    I: Iterator<Item = i16>,
{
    let child_args: Vec<&str> = match args.values_of("CHILD") {
        Some(child_args) => child_args.collect(),
        None => Vec::default(),
    };

    let cfg = Config {
        child_args,
        input_rate_str: args.value_of("rate").unwrap(),
        quiet: args.occurrences_of("quiet") >= 1,
        demo: args.occurrences_of("demo") >= 1,
    };

    if cfg.demo {
        // demo mode: issue a DMO message, run the child for eight seconds,
        // and exit
        let duration_message = (receiver.input_rate() * 8) as usize;
        let dmo = make_demo_message(&Utc::now());

        warn!("demonstration (--demo) mode: the following messages are NOT LIVE!");

        let mut alerting = State::<Alerting>::from(dmo);
        alerting.until_message_end(&cfg, receiver, &mut input.take(duration_message));

        for _i in 0..3 {
            alerting = State::<Alerting>::from(Message::EndOfMessage);
            alerting.until_message_end(&cfg, receiver, &mut std::iter::once(0i16));
        }
    } else {
        // live mode: look for messages in a loop
        let mut waiting = State::<Waiting>::new();
        while let Some(alerting) = waiting.until_message_start(receiver, &mut input) {
            waiting = alerting.until_message_end(&cfg, receiver, &mut input);
        }
    }
}

/// Configuration
#[derive(Clone, Debug)]
struct Config<'args> {
    child_args: Vec<&'args str>,
    input_rate_str: &'args str,
    quiet: bool,
    demo: bool,
}

#[derive(Debug)]
struct State<S> {
    state: S,
}

#[derive(Debug)]
struct Waiting {}

#[derive(Debug)]
struct Alerting {
    next: Option<Message>,
}

impl<S> State<S> {
    /// Create initial state
    pub fn new() -> State<Waiting> {
        State { state: Waiting {} }
    }
}

impl State<Waiting> {
    /// Look for start of message
    ///
    /// Consumes values from `input` until a message is
    /// found or the iterator is exhausted.
    pub fn until_message_start<I>(
        self,
        receiver: &mut SameReceiver,
        input: &mut I,
    ) -> Option<State<Alerting>>
    where
        I: Iterator<Item = i16>,
    {
        // read from iterator until we get any message
        for msg in receiver.iter_messages(input.map(|sa| sa as f32)) {
            // any kind of message → Alerting
            return Some(msg.into());
        }

        None
    }
}

impl State<Alerting> {
    /// Look for end of message
    ///
    /// Consumes values from `input` until an "end of message" is
    /// found or the iterator is exhausted.
    pub fn until_message_end<I>(
        mut self,
        config: &Config<'_>,
        receiver: &mut SameReceiver,
        input: &mut I,
    ) -> State<Waiting>
    where
        I: Iterator<Item = i16>,
    {
        while let Some(msg) = self.state.next.take() {
            if !config.quiet {
                println!("{}", msg);
            }

            let hdr = match msg {
                Message::StartOfMessage(hdr) => hdr,
                Message::EndOfMessage => break, // → Waiting
            };

            if config.child_args.is_empty() {
                debug!("no child process to spawn");
                return self.into(); // → Waiting
            }

            // try to spawn a child
            let mut child = match spawner::spawn(
                config.child_args[0],
                &config.child_args[1..],
                &hdr,
                &config.input_rate_str,
            ) {
                Ok(child) => child,
                Err(err) => {
                    error!("unable to spawn child process: {}", err);
                    return self.into();
                }
            };

            debug!("spawned child process PID {}", child.id());

            // run it to conclusion
            self.state.next = run_child(&mut child, receiver, input);

            // run_child() closes the child's stdin
            // wait for the child to exit
            match child.wait() {
                Ok(exit) => {
                    if exit.success() {
                        debug!("child process exited successfully");
                    } else {
                        warn!(
                            "child process exited abnormally with status {}",
                            exit.code().unwrap_or(1)
                        );
                    }
                }
                Err(err) => {
                    error!("unable to await child process exit: {}", err);
                }
            }
        }

        // → Waiting
        self.into()
    }
}

// Spawn program to handle the alert; run it to completion
//
// If a new start-of-message is received while we are
// processing this child, the new message is reported to
// the caller. Otherwise, returns None on completion.
fn run_child<I>(child: &mut Child, receiver: &mut SameReceiver, input: &mut I) -> Option<Message>
where
    I: Iterator<Item = i16>,
{
    let mut child_pipe = if let Some(pipe) = child.stdin.take() {
        pipe
    } else {
        error!("unable to create pipe to child process");
        return None;
    };

    // read from iterator until we get an end-of-message,
    // writing every byte to the child process (suppressing errors)
    for msg in receiver.iter_messages(
        input
            .inspect(|sa| {
                let _ = child_pipe.write_i16::<NativeEndian>(*sa);
            })
            .map(|sa| sa as f32),
    ) {
        if let Message::StartOfMessage(ref _hdr) = msg {
            warn!("received SAME start-of-message without end-of-message");
        }

        // either a new message or the end of this one
        // will cause this child to terminate
        return Some(msg);
    }

    // input exhausted
    None
}

impl From<Message> for State<Alerting> {
    fn from(message: Message) -> Self {
        debug!("new state: alerting");
        Self {
            state: Alerting {
                next: Some(message),
            },
        }
    }
}

impl From<State<Alerting>> for State<Waiting> {
    fn from(_state: State<Alerting>) -> Self {
        debug!("new state: waiting");
        Self { state: Waiting {} }
    }
}

// Create a demonstration message
fn make_demo_message(at: &DateTime<Utc>) -> Message {
    let msg_string = format!("ZCZC-EAS-DMO-999000+0015-{}-N0 CALL -", at.format("%j%H%M"));
    Message::StartOfMessage(MessageHeader::new(msg_string).expect("unable to create DMO message"))
}

#[cfg(test)]
mod tests {
    use super::*;

    use chrono::{Duration, TimeZone};
    use sameold::EventCode;

    #[test]
    fn test_make_demo_message() {
        let tm = Utc.ymd(2020, 12, 31).and_hms(23, 22, 00);
        let msg = make_demo_message(&tm);
        let msg = match msg {
            Message::StartOfMessage(hdr) => hdr,
            _ => unreachable!(),
        };
        assert_eq!(msg.event().unwrap(), EventCode::PracticeDemoWarning);
        assert_eq!(msg.issue_datetime(&tm).unwrap(), tm);
        assert_eq!(msg.valid_duration(), Duration::minutes(15));
    }
}
