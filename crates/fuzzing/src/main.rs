use anyhow::{Context, Result};
use arbitrary::Arbitrary;
use clap::{App, Arg};
use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process;
use wasmtime_fuzzing::{generators, oracles};

fn generator_arg() -> Arg<'static, 'static> {
    Arg::with_name("generator")
        .short("g")
        .long("generator")
        .help("The test case generator")
        .takes_value(true)
        .possible_values(&["api", "raw", "ttf", "wat", "deserialize-api"])
}

fn input_arg() -> Arg<'static, 'static> {
    Arg::with_name("input")
        .short("i")
        .long("input")
        .takes_value(true)
        .help("The path to the raw input file that should be fed to the test case generator")
}

fn oracle_arg() -> Arg<'static, 'static> {
    Arg::with_name("oracle")
        .long("oracle")
        .help("The oracle used to check the generated test")
        .takes_value(true)
        .possible_values(&["instantiate", "compile", "make_api_calls"])
}

fn strategy_arg() -> Arg<'static, 'static> {
    Arg::with_name("strategy")
        .short("s")
        .long("strategy")
        .takes_value(true)
        .help("The strategy used when compiling Wasm")
        .possible_values(&["auto", "cranelift", "lightbeam"])
        .default_value("auto")
}

fn args() -> clap::ArgMatches<'static> {
    App::new(env!("CARGO_PKG_NAME"))
        .about("Swiss army knife CLI utility for Wasmtime fuzzing.")
        .author(env!("CARGO_PKG_AUTHORS"))
        .subcommand(
            App::new("generate")
                .about("Generate and display a test case from raw input")
                .long_about(
                    "Generate and display a test case from raw input.\n\
                     \n\
                     Will do its best effort to display the test case in a developer-readable\n\
                     form. For generators that create Wasm binaries, we display the WAT\n\
                     disassembly. For others, such as the API call generator, we might use the\n\
                     `Debug` trait.",
                )
                .arg(generator_arg())
                .arg(input_arg()),
        )
        .subcommand(
            App::new("check")
                .about("Run an oracle over a test case and check its results")
                .long_about(
                    "Run an oracle over a test case and check its results.\n\
                     \n\
                     This can be used to reproduce a test failure that a fuzzer found, or as \n\
                     part of an is-interesting predicate during test case reduction.",
                )
                .arg(generator_arg())
                .arg(oracle_arg())
                .arg(input_arg())
                .arg(strategy_arg()),
        )
        .subcommand(
            App::new("reduce")
                .about("Reduce a failing test case")
                .arg(generator_arg())
                .arg(oracle_arg())
                .arg(input_arg())
                .arg(strategy_arg())
                .arg(
                    Arg::with_name("reducer")
                        .short("r")
                        .long("reducer")
                        .help(
                            "The reducer tool that should be used. If not supplied, a default is\n\
                             chosen based on the generator.",
                        )
                        .takes_value(true)
                        .possible_values(&["wasm-reduce", "creduce"]),
                ),
        )
        .get_matches()
}

fn main() -> Result<()> {
    env_logger::init();

    let args = args();
    if let Some(args) = args.subcommand_matches("generate") {
        generate(args).context("failed to generate and display test case")?;
    } else if let Some(args) = args.subcommand_matches("check") {
        check(args).context("failed to check test case with an oracle")?;
    } else if let Some(args) = args.subcommand_matches("reduce") {
        reduce(args).context("failed to reduce failing test case")?;
    } else {
        unreachable!()
    }

    Ok(())
}

fn arg_or_env(args: &clap::ArgMatches, arg_name: &str, env_var: &str) -> Result<String> {
    match args.value_of(arg_name) {
        Some(a) => Ok(a.to_string()),
        None => Ok(env::var(env_var).with_context(|| {
            format!(
                "must supply either `--{}` or set the `{}` environment variable",
                arg_name, env_var,
            )
        })?),
    }
}

fn get_input(args: &clap::ArgMatches) -> Result<Vec<u8>> {
    let input = arg_or_env(args, "input", "WASMTIME_FUZZING_INPUT")?;
    let input = PathBuf::from(input);
    fs::read(&input).with_context(|| format!("failed to read input: {}", input.display()))
}

fn get_generator(args: &clap::ArgMatches) -> Result<String> {
    arg_or_env(args, "generator", "WASMTIME_FUZZING_GENERATOR")
}

fn get_oracle(args: &clap::ArgMatches) -> Result<String> {
    arg_or_env(args, "oracle", "WASMTIME_FUZZING_ORACLE")
}

fn get_reducer(args: &clap::ArgMatches) -> Option<String> {
    arg_or_env(args, "reducer", "WASMTIME_FUZZING_REDUCER").ok()
}

fn generate(args: &clap::ArgMatches) -> Result<()> {
    let input = get_input(args)?;
    let generator = get_generator(args)?;

    match generator.as_str() {
        "api" => {
            let test = api_calls(&input)?;
            println!("{:#?}", test.calls);
            Ok(())
        }
        "deserialize-api" => {
            let test: generators::api::ApiCalls =
                bincode::deserialize(&input).context("failed to deserialize API calls")?;
            println!("{:#?}", test.calls);
            Ok(())
        }
        "raw" => {
            let wat =
                wasmprinter::print_bytes(&input).context("failed to disassemble raw Wasm input")?;
            println!("{}", wat);
            Ok(())
        }
        "ttf" => {
            let test = wasm_opt_ttf(&input)?;
            let wat =
                wasmprinter::print_bytes(&test.wasm).context("failed to disassemble wasm bytes")?;
            println!("{}", wat);
            Ok(())
        }
        "wat" => {
            let wat = std::str::from_utf8(&input).context("UTF-8 validation of input failed")?;
            println!("{}", wat);
            Ok(())
        }
        _ => unreachable!(),
    }
}

fn ring_buffer(input: &[u8]) -> Result<arbitrary::RingBuffer> {
    arbitrary::RingBuffer::new(&input, input.len()).map_err(|e| match e {
        arbitrary::BufferError::EmptyInput => {
            anyhow::anyhow!("cannot create a ring buffer from empty input")
        }
    })
}

fn api_calls(input: &[u8]) -> Result<generators::api::ApiCalls> {
    let mut input = ring_buffer(&input)?;
    Ok(generators::api::ApiCalls::arbitrary(&mut input).expect("ring buffer does not fail"))
}

fn wasm_opt_ttf(input: &[u8]) -> Result<generators::WasmOptTtf> {
    let mut input = ring_buffer(&input)?;
    Ok(generators::WasmOptTtf::arbitrary(&mut input).expect("ring buffer does not fail"))
}

fn check(args: &clap::ArgMatches) -> Result<()> {
    let input = get_input(args)?;
    let generator = get_generator(args)?;
    let oracle = get_oracle(args)?;

    match (generator.as_str(), oracle.as_str()) {
        ("api", "make_api_calls") => {
            let test = api_calls(&input)?;
            oracles::make_api_calls(test);
            Ok(())
        }

        ("deserialize-api", "make_api_calls") => {
            let test: generators::api::ApiCalls =
                bincode::deserialize(&input).context("failed to deserialize API calls")?;
            oracles::make_api_calls(test);
            Ok(())
        }

        (gen, oracle) if is_wasm_generator(gen) && is_wasm_oracle(oracle) => {
            let wasm = match gen {
                "raw" => input,
                "ttf" => wasm_opt_ttf(&input)?.wasm,
                "wat" => {
                    let input = std::str::from_utf8(&input)
                        .context("UTF-8 validation of WAT input failed")?;
                    wat::parse_str(input).context("failed to parse input string as WAT")?
                }
                _ => unreachable!(),
            };
            let strategy = match args.value_of("strategy") {
                Some("auto") | None => wasmtime_jit::CompilationStrategy::Auto,
                Some("cranelift") => wasmtime_jit::CompilationStrategy::Cranelift,
                #[cfg(feature = "lightbeam")]
                Some("lightbeam") => wasmtime_jit::CompilationStrategy::Lightbeam,
                _ => unreachable!(),
            };
            match oracle {
                "instantiate" => oracles::instantiate(&wasm, strategy),
                "compile" => oracles::compile(&wasm, strategy),
                _ => unreachable!(),
            }
            Ok(())
        }

        (gen, oracle) => anyhow::bail!(
            "the test case generator '{}' is incompatible with the oracle '{}'",
            gen,
            oracle
        ),
    }
}

fn is_wasm_generator(g: &str) -> bool {
    match g {
        "raw" | "ttf" | "wat" => true,
        _ => false,
    }
}

fn is_wasm_oracle(o: &str) -> bool {
    match o {
        "instantiate" | "compile" => true,
        _ => false,
    }
}

/// Spawns a `wasmtime-fuzzing check` child process and returns `true` when
/// making the given API calls *fails*.
fn spawn_api_calls_check(test: &generators::api::ApiCalls) -> Result<bool> {
    let mut tmp = tempfile::NamedTempFile::new().context("failed to create temporary file")?;
    let buf = bincode::serialize(test).context("failed to serialize API calls")?;
    tmp.write_all(&buf)
        .context("failed to write serialized API calls to temporary file")?;

    let current_exe = env::current_exe().context("failed to get the current executable's path")?;
    let mut child = process::Command::new(current_exe)
        .args(&[
            "check",
            "--generator",
            "deserialize-api",
            "--oracle",
            "make_api_calls",
        ])
        .env("WASMTIME_FUZZING_INPUT", tmp.path())
        .spawn()
        .context("failed to spawn our `wasmtime-fuzzing check` child process")?;
    let status = child
        .wait()
        .context("failed to wait on our `wasmtime-fuzzing check` child process")?;
    Ok(!status.success())
}

fn reduce(args: &clap::ArgMatches) -> Result<()> {
    let input = get_input(args)?;
    let generator = get_generator(args)?;
    let oracle = get_oracle(args)?;
    let reducer = get_reducer(args);

    match (generator.as_str(), oracle.as_str(), reducer) {
        ("api", "make_api_calls", Some(_)) => anyhow::bail!(
            "API call test case generators and oracles are incompatible with external reducers"
        ),
        ("api", "make_api_calls", None) => {
            let mut test = api_calls(&input)?;
            if !spawn_api_calls_check(&test)? {
                anyhow::bail!("nothing to reduce: initial API calls do not fail");
            }
            'outer: loop {
                // Sigh... the `arbitrary` crate doesn't implement shrinking for
                // anything, just returns an empty iterator, so this shrinking
                // will always fail. I need to send a PR their way.
                for t in test.shrink() {
                    log::trace!("considering candidate test case: {:#?}", test.calls);
                    if spawn_api_calls_check(&t)? {
                        test = t;
                        log::debug!("found new, smaller failing test case: {:#?}", test.calls);
                        continue 'outer;
                    }
                }
                break;
            }
            println!("========== Reduced API Calls ==========");
            println!("{:#?}", test.calls);
            Ok(())
        }

        (gen, oracle, reducer) if is_wasm_generator(gen) && is_wasm_oracle(oracle) => {
            let reducer = reducer.unwrap_or_else(|| "wasm-reduce".to_string());
            unimplemented!();
            process::Command::new(reducer)
        }

        (gen, oracle, _) => anyhow::bail!(
            "the test case generator '{}' is incompatible with the oracle '{}'",
            gen,
            oracle
        ),
    }
}
