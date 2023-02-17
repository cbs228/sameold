use std::io;

use byteorder::{NativeEndian, ReadBytesExt};
use clap::Parser;
use log::{info, LevelFilter};

use sameold::SameReceiverBuilder;

mod app;
mod cli;
mod spawner;

use cli::Args;

fn main() {
    // Parse options and start logging
    let args = Args::parse();
    log_setup(&args);

    // create the decoder
    let mut rx = SameReceiverBuilder::new(args.rate)
        .with_agc_gain_limits(1.0f32 / (i16::MAX as f32), 1.0 / 200.0)
        .with_agc_bandwidth(args.agc_bw)
        .with_dc_blocker_length(args.dc_blocker_len)
        .with_timing_bandwidth(args.timing_bw_unlocked, args.timing_bw_locked)
        .with_timing_max_deviation(args.timing_max_dev)
        .with_squelch_power(args.squelch_pwr_open, args.squelch_pwr_close)
        .with_preamble_max_errors(args.preamble_max_errors)
        .build();

    // file setup: locks stdin in case we need it
    let stdin = io::stdin();
    let stdin_handle = stdin.lock();
    let mut inbuf = file_setup(&args, stdin_handle);

    // processing: read i16 from the input source
    app::run(
        &args,
        &mut rx,
        std::iter::from_fn(|| Some(inbuf.read_i16::<NativeEndian>().ok()?)),
    );

    // flush all data samples out of the decoder
    match rx.flush() {
        Some(lastmsg) => {
            if !args.quiet {
                println!("{}", lastmsg)
            }
        }
        None => {}
    }
}

fn log_setup(args: &Args) {
    if args.quiet {
        // no logging
        return;
    } else if std::env::var_os("RUST_LOG").is_none() {
        // parameter controls
        let log_filter = match args.verbose {
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
    args: &Args,
    stdin: std::io::StdinLock<'stdin>,
) -> Box<dyn io::BufRead + 'stdin> {
    if args.input_is_stdin() {
        info!("SAME decoder reading standard input");
        Box::new(io::BufReader::new(stdin))
    } else {
        info!("SAME decoder reading file");
        Box::new(io::BufReader::new(
            std::fs::File::open(&args.file).expect("Unable to open requested FILE"),
        ))
    }
}
