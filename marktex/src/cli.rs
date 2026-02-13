// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(version)]
pub struct CliArgs {
    #[arg(value_name = "INPUT")]
    pub inputs: Vec<PathBuf>,

    #[arg(short, long, value_name = "FILE")]
    pub output: Option<PathBuf>,

    #[arg(long, value_name = "FILE")]
    pub config: Option<PathBuf>,
}
