use clap::{Arg, Parser};
use serde::{Deserialize, Serialize};

#[derive(Parser, Serialize, Deserialize)]
pub struct RmlArgs {
    #[clap(long)]
    /// Only generate proofs for items matching the provided string. The string is treated
    /// as a Rust qualified path.
    focus_on: Option<String>,
    /// Print to stdout.
    #[clap(group = "output", long)]
    stdout: bool,
    /// Print to a file.
    #[clap(group = "output", long, env)]
    output_file: Option<String>,
}

#[derive(Parser)]
pub struct Args {
    #[clap(flatten)]
    pub rml: RmlArgs,
    #[clap(last = true)]
    pub rust_flags: Vec<String>,
}
