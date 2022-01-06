use std::io;

use byteorder::{NativeEndian, ReadBytesExt};
use clap::{App, Arg, ArgMatches};
use log::{info, LevelFilter};

use sameold::SameReceiverBuilder;

mod app;
mod spawner;

const STDIN_FILE: &str = "-";

const USAGE: &str = r#"
A simple decoder for Specific Area Message Encoding (SAME). This
program accepts raw PCM samples in signed 16-bit (i16) format,
at the given sampling --rate, and decodes any SAME headers that
are present. Decoded headers are printed in their ASCII
representation.

You can pipe in an audio file with sox

    sox input.wav -t raw -r 22.5k -e signed -b 16 -c 1 - \
        | samedec -r 22050

Arguments which follow "--" will be used to spawn a child
process. The child process will have the input audio signal
piped to its standard input. You can use this to play or store
SAME messages.

    parec --channels 1 --format s16ne \
      --rate 22050 --latency-msec 500 \
        | samedec -r 22050 -- pacat \
            --channels 1 --format s16ne \
            --rate 22050 --latency-msec 500

The child process receives the following additional environment
variables which describe the message:

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

fn main() {
    let matches = App::new(env!("CARGO_PKG_NAME"))
        .version(env!("CARGO_PKG_VERSION"))
        .author("Colin S. <https://crates.io/crates/samedec>")
        .about("SAME/EAS decoder")
        .after_help(USAGE)
        .arg(
            Arg::with_name("v")
                .long("verbose")
                .short("v")
                .multiple(true)
                .help("Verbosity level (-vvv for more)")
                .display_order(1),
        )
        .arg(
            Arg::with_name("quiet")
                .long("quiet")
                .short("q")
                .help("Print NOTHING, not even SAME headers")
                .display_order(2),
        )
        .arg(
            Arg::with_name("rate")
                .long("rate")
                .short("r")
                .takes_value(true)
                .default_value("22050")
                .help("Sampling rate (Hz)")
                .display_order(3),
        )
        .arg(
            Arg::with_name("file")
                .long("file")
                .help("Input file (or \"-\" for stdin)")
                .required(false)
                .takes_value(true)
                .default_value(STDIN_FILE),
        )
        .arg(
            Arg::with_name("demo")
                .long("demo")
                .takes_value(false)
                .help("Issue demo warning (DMO) and exit"),
        )
        .arg(
            Arg::with_name("fast_eom")
                .long("fast-eom")
                .takes_value(false)
                .help("Output one \"NNNN\" for each EOM received. Ends child processes sooner."),
        )
        .arg(
            Arg::with_name("dc-blocker-len")
                .long("dc-blocker-len")
                .help("DC Blocker filter length (fsym)")
                .takes_value(true)
                .default_value("0.38")
                .hidden_short_help(true),
        )
        .arg(
            Arg::with_name("agc-bw")
                .long("agc-bw")
                .help("AGC bandwidth (fsym)")
                .takes_value(true)
                .default_value("0.01")
                .hidden_short_help(true),
        )
        .arg(
            Arg::with_name("timing-bw-unlocked")
                .long("timing-bw-unlocked")
                .help("Timing loop bandwidth, searching (fsym)")
                .takes_value(true)
                .default_value("0.125")
                .hidden_short_help(true),
        )
        .arg(
            Arg::with_name("timing-bw-locked")
                .long("timing-bw-locked")
                .help("Timing loop bandwidth, tracking (fsym)")
                .takes_value(true)
                .default_value("0.05")
                .hidden_short_help(true),
        )
        .arg(
            Arg::with_name("timing-max-dev")
                .long("timing-max-dev")
                .help("Timing maximum deviation (fsym)")
                .takes_value(true)
                .default_value("0.01")
                .hidden_short_help(true),
        )
        .arg(
            Arg::with_name("squelch-pwr-open")
                .long("squelch-pwr-open")
                .help("Power req'd to start receiving (0.0 ≤ PWR ≤ 1.0)")
                .takes_value(true)
                .default_value("0.10")
                .hidden_short_help(true),
        )
        .arg(
            Arg::with_name("squelch-pwr-close")
                .long("squelch-pwr-close")
                .help("Power req'd to keep receiving (0.0 ≤ PWR ≤ 1.0)")
                .takes_value(true)
                .default_value("0.05")
                .hidden_short_help(true),
        )
        .arg(
            Arg::with_name("preamble-max-errors")
                .long("preamble-max-errors")
                .help("Permitted bit errors in sync pattern (<7)")
                .takes_value(true)
                .default_value("2")
                .hidden_short_help(true),
        )
        .arg(
            Arg::with_name("CHILD")
                .takes_value(true)
                .multiple(true)
                .last(true)
                .help("Spawn child process to handle message audio."),
        )
        .get_matches();

    log_setup(&matches);

    // create the decoder
    let rate = str::parse::<u32>(matches.value_of("rate").unwrap())
        .expect("invalid sampling --rate: expect integer");
    let mut rx = SameReceiverBuilder::new(rate)
        .with_agc_gain_limits(1.0f32 / (i16::MAX as f32), 1.0 / 200.0)
        .with_agc_bandwidth(
            str::parse(matches.value_of("agc-bw").unwrap()).expect("--agc-bw: expect float"),
        )
        .with_dc_blocker_length(
            str::parse(matches.value_of("dc-blocker-len").unwrap())
                .expect("--dc-blocker-len: expect float"),
        )
        .with_timing_bandwidth(
            str::parse(matches.value_of("timing-bw-unlocked").unwrap())
                .expect("--timing-bw-unlocked: expect float"),
            str::parse(matches.value_of("timing-bw-locked").unwrap())
                .expect("--timing-bw-locked: expect float"),
        )
        .with_timing_max_deviation(
            str::parse(matches.value_of("timing-max-dev").unwrap())
                .expect("--timing-max-dev: expect float"),
        )
        .with_squelch_power(
            str::parse(matches.value_of("squelch-pwr-open").unwrap())
                .expect("--squelch-pwr-open: expect float"),
            str::parse(matches.value_of("squelch-pwr-close").unwrap())
                .expect("--squelch-pwr-close: expect float"),
        )
        .with_preamble_max_errors(
            str::parse(matches.value_of("preamble-max-errors").unwrap())
                .expect("--preamble-max-errors: expect integer"),
        )
        .with_fast_end_of_message(matches.occurrences_of("fast_eom") >= 1)
        .build();

    // file setup: locks stdin in case we need it
    let stdin = io::stdin();
    let stdin_handle = stdin.lock();
    let mut inbuf = file_setup(&matches, stdin_handle);

    // processing: read i16 from the input source
    app::run(
        &matches,
        &mut rx,
        std::iter::from_fn(|| Some(inbuf.read_i16::<NativeEndian>().ok()?)),
    );

    // flush all data samples out of the decoder
    match rx.flush() {
        Some(lastmsg) => {
            if matches.occurrences_of("quiet") == 0 {
                println!("{}", lastmsg)
            }
        }
        None => {}
    }
}

fn log_setup(args: &ArgMatches) {
    if args.occurrences_of("quiet") > 0 {
        // no logging
        return;
    } else if std::env::var_os("RUST_LOG").is_none() {
        // parameter controls
        let log_filter = match args.occurrences_of("v") {
            0 => LevelFilter::Warn,
            1 => LevelFilter::Info,
            2 => LevelFilter::Debug,
            3 | _ => LevelFilter::Trace,
        };

        pretty_env_logger::formatted_builder()
            .filter_module("sameold", log_filter)
            .filter_module("samedec", log_filter)
            .init();
    } else {
        // environment controls
        pretty_env_logger::init();
    }
}

fn file_setup<'stdin>(
    args: &ArgMatches,
    stdin: std::io::StdinLock<'stdin>,
) -> Box<dyn io::BufRead + 'stdin> {
    let filename = args.value_of("file").unwrap();
    if filename == STDIN_FILE {
        info!("SAME decoder reading standard input");
        Box::new(io::BufReader::new(stdin))
    } else {
        info!("SAME decoder reading file");
        Box::new(io::BufReader::new(
            std::fs::File::open(filename).expect("Unable to open requested FILE"),
        ))
    }
}
