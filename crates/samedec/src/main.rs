use std::io;

use anyhow::{anyhow, Context};
use byteorder::{NativeEndian, ReadBytesExt};
use clap::Parser;
use log::{info, LevelFilter};

use sameold::SameReceiverBuilder;

mod app;
mod cli;
mod spawner;

use cli::{Args, CliError};

fn main() {
    match samedec() {
        Ok(()) => {}
        Err(cli_error) => cli_error.exit(),
    }
}

fn samedec() -> Result<(), CliError> {
    // Parse options and start logging
    let args = Args::try_parse()?;
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
    let mut inbuf = file_setup(&args, stdin_handle)?;

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

    Ok(())
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
) -> Result<Box<dyn io::BufRead + 'stdin>, anyhow::Error> {
    if args.input_is_stdin() {
        info!("SAME decoder reading standard input");
        if !is_terminal(&std::io::stdin()) {
            Ok(Box::new(io::BufReader::new(stdin)))
        } else {
            Err(anyhow!(
                "cowardly refusing to read audio samples from a terminal.

Pipe a source of raw uncompressed audio from sox, parec, rtl_fm,
or similar into this program."
            ))
        }
    } else {
        info!("SAME decoder reading file: \"{}\"", &args.file);
        Ok(Box::new(io::BufReader::new(
            std::fs::File::open(&args.file)
                .with_context(|| format!("Unable to open --file \"{}\"", args.file))?,
        )))
    }
}

#[cfg(not(target_os = "windows"))]
fn is_terminal<S>(stream: &S) -> bool
where
    S: std::os::fd::AsRawFd,
{
    terminal_size::terminal_size_using_fd(stream.as_raw_fd()).is_some()
}

#[cfg(target_os = "windows")]
fn is_terminal<S>(stream: &S) -> bool
where
    S: std::os::windows::io::AsRawHandle,
{
    terminal_size::terminal_size_using_handle(stream.as_raw_handle()).is_some()
}
