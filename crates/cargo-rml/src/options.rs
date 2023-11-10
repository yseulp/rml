use clap::Parser;
use rml::{Options, OutputFile};
use serde::{Deserialize, Serialize};

#[derive(Parser, Serialize, Deserialize)]
#[command(version, author)]
pub struct RmlArgs {
    #[clap(long)]
    /// Only generate proofs for items matching the provided string. The string
    /// is treated as a Rust qualified path.
    focus_on: Option<String>,
    /// Print to stdout.
    #[clap(group = "output", long)]
    stdout: bool,
    /// Print to a file.
    #[clap(group = "output", long, short, env)]
    output_file: Option<String>,
    /// Pretty print the output
    #[clap(long)]
    pretty_print: bool,
    /// Print the expanded AST with inserted specification functions.
    #[clap(long)]
    print_expanded: bool,
    /// Print the collected specs as pretty-printed debug output.
    #[clap(long)]
    print_specs_debug: bool,
}

impl RmlArgs {
    pub fn to_options(self) -> Options {
        let cargo_rml = std::env::var("CARGO_RML").is_ok();
        let should_output = !cargo_rml || std::env::var("CARGO_PRIMARY_PACKAGE").is_ok();

        let output_file = match (self.stdout, self.output_file) {
            (true, _) => Some(OutputFile::Stdout),
            (_, Some(f)) => Some(OutputFile::File(f)),
            _ => None,
        };

        Options {
            should_output,
            output_file,
            in_cargo: cargo_rml,
            print_expanded: self.print_expanded,
            print_specs_debug: self.print_specs_debug,
            pretty_print: self.pretty_print,
        }
    }
}

#[derive(Parser)]
pub struct Args {
    #[clap(flatten)]
    pub rml: RmlArgs,
    #[clap(last = true)]
    pub rust_flags: Vec<String>,
}
