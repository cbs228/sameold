use std::fmt::Display;

use clap::{error::ErrorKind, value_parser, CommandFactory, Parser};

/// Standard input filename
const STDIN_FILE: &str = "-";

const USAGE_SHORT: &str = r#"
This program accepts raw PCM samples in signed 16-bit (i16) format, at the given sampling --rate, and decodes any SAME headers that are present. Decoded headers are printed in their ASCII representation.

See --help for more details.

ALWAYS TEST YOUR DECODING SETUP!
"#;

const USAGE_LONG: &str = r#"
This program accepts raw PCM samples in signed 16-bit (i16) format, at the given sampling --rate, and decodes any SAME headers that are present. Decoded headers are printed in their ASCII representation.

You can pipe in an audio file with sox

    sox input.wav -t raw -r 22.5k -e signed -b 16 -c 1 - \
        | samedec -r 22050

Arguments which follow "--" will be used to spawn a child process. The child process will have the input audio signal piped to its standard input. You can use this to play or store SAME messages.

    parec --channels 1 --format s16ne \
      --rate 22050 --latency-msec 500 \
        | samedec -r 22050 -- pacat \
            --channels 1 --format s16ne \
            --rate 22050 --latency-msec 500

The child process receives the following additional environment variables which describe the message:

  SAMEDEC_RATE="22050" (configured sample --rate)
  SAMEDEC_MSG="ZCZC-EAS-RWT-012057-012081+0030-2780415-WTSP/TV-"
  SAMEDEC_ORG="EAS" (or CIV,NWS,PEP)
  SAMEDEC_ORIGINATOR="EAS Participant"
  SAMEDEC_EVT="RWT"
  SAMEDEC_EVENT="Required Weekly Test"
  SAMEDEC_SIGNIFICANCE="T" (or M,S,E,A,W)
  SAMEDEC_LOCATIONS="012057 012081"
  SAMEDEC_ISSUETIME="1616883240" (UTC UNIX timestamp)
  SAMEDEC_PURGETIME="1616886840" (UTC UNIX timestamp)

Child processes MUST read or close standard input.
Child processes MUST exit when their standard input is closed.

ALWAYS TEST YOUR DECODING SETUP!
"#;

const ADVANCED: &str = "Advanced Modem Options";

/// Top-level program arguments
#[derive(Parser, Clone, Debug)]
#[command(author = "Colin S. <https://github.com/cbs228/sameold>")]
#[command(version)]
#[command(about, long_about = None)]
#[command(after_help = USAGE_SHORT, after_long_help = USAGE_LONG)]
#[command(max_term_width = 100)]
pub struct Args {
    /// Verbosity level (-vvv for more)
    #[arg(short, long, default_value_t = 0, action = clap::ArgAction::Count)]
    pub verbose: u8,

    /// Print NOTHING, not even SAME headers
    #[arg(short, long)]
    pub quiet: bool,

    /// Sampling rate (Hz)
    ///
    /// Set to the sampling rate of your audio source. If sampling from
    /// a sound card, use the card's native rate—usually 44100 or 48000.
    /// Avoid resampling the audio.
    #[arg(short, long, default_value_t = 22050)]
    pub rate: u32,

    /// Input file (or "-" for stdin)
    ///
    /// The input must be one-channel (mono), signed 16-bit
    /// native-endian at --rate.
    #[arg(long, default_value_t = STDIN_FILE.to_string())]
    pub file: String,

    /// Issue demo warning (DMO) and exit
    ///
    /// You must still provide an audio file, but you may use /dev/zero.
    /// samedec will print a demo warning, invoke the CHILD process with
    /// eight seconds of audio, and then exit.
    #[arg(long)]
    pub demo: bool,

    /// DC-blocker filter length (fsym)
    #[arg(long, default_value_t = 0.38)]
    #[arg(hide_short_help = true)]
    #[arg(help_heading = ADVANCED)]
    pub dc_blocker_len: f32,

    /// AGC bandwidth (fsym)
    #[arg(long, default_value_t = 0.01)]
    #[arg(hide_short_help = true)]
    #[arg(help_heading = ADVANCED)]
    pub agc_bw: f32,

    /// Symbol timing loop bandwidth, searching (fsym)
    #[arg(long, default_value_t = 0.125)]
    #[arg(hide_short_help = true)]
    #[arg(help_heading = ADVANCED)]
    pub timing_bw_unlocked: f32,

    /// Symbol timing loop bandwidth, tracking (fsym)
    #[arg(long, default_value_t = 0.05)]
    #[arg(hide_short_help = true)]
    #[arg(help_heading = ADVANCED)]
    pub timing_bw_locked: f32,

    /// Symbol timing maximum deviation (fsym)
    #[arg(long, default_value_t = 0.01)]
    #[arg(hide_short_help = true)]
    #[arg(help_heading = ADVANCED)]
    pub timing_max_dev: f32,

    /// Power req'd to start receiving (0.0 ≤ PWR ≤ 1.0)
    #[arg(long, default_value_t = 0.10)]
    #[arg(hide_short_help = true)]
    #[arg(help_heading = ADVANCED)]
    pub squelch_pwr_open: f32,

    /// Power req'd to keep receiving (0.0 ≤ PWR ≤ 1.0)
    #[arg(long, default_value_t = 0.05)]
    #[arg(hide_short_help = true)]
    #[arg(help_heading = ADVANCED)]
    pub squelch_pwr_close: f32,

    /// Permitted bit errors in sync pattern (<7)
    #[arg(long, default_value_t = 2)]
    #[arg(value_parser = value_parser!(u32).range(0..6))]
    #[arg(hide_short_help = true)]
    #[arg(help_heading = ADVANCED)]
    pub preamble_max_errors: u32,

    /// Spawn child process to handle message audio. Optional.
    ///
    /// Arguments are provided VERBATIM to the child process
    /// without shell interpretation.
    #[arg(last = true)]
    pub child: Vec<String>,
}

impl Args {
    /// Return true if the user requests input from stdin
    pub fn input_is_stdin(&self) -> bool {
        self.file == STDIN_FILE
    }
}

/// A program-level error with exit code
#[derive(Debug)]
pub struct CliError {
    error: anyhow::Error,
    exit_code: i32,
}

impl CliError {
    /// Create new error with a custom exit code
    pub fn new(error: anyhow::Error, code: i32) -> CliError {
        CliError {
            error,
            exit_code: code,
        }
    }

    /// Print this error to the terminal
    ///
    /// Errors from clap are printed verbatim. Other types of errors
    /// are printed indirectly via clap's fancy formatter.
    pub fn print(&self) -> std::io::Result<()> {
        if let Some(e) = self.error.downcast_ref::<clap::Error>() {
            e.print()
        } else {
            Args::command()
                .error(ErrorKind::Format, self.to_string())
                .print()
        }
    }

    /// Print this error to the terminal and exit
    pub fn exit(&self) -> ! {
        drop(self.print());
        std::process::exit(self.exit_code);
    }
}

impl Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.error)
    }
}

impl std::error::Error for CliError {}

impl From<anyhow::Error> for CliError {
    fn from(err: anyhow::Error) -> CliError {
        CliError::new(err, 1)
    }
}

impl From<clap::Error> for CliError {
    fn from(err: clap::Error) -> CliError {
        let code = if err.use_stderr() { 1 } else { 0 };
        CliError::new(err.into(), code)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_clap() {
        use clap::CommandFactory;
        Args::command().debug_assert();
    }
}
