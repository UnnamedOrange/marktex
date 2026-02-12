// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

use std::path::Path;

use serde::Deserialize;

#[derive(Default, Clone, Debug, PartialEq, Eq, Deserialize)]
#[serde(default, deny_unknown_fields)]
pub struct Options {
    pub force_figure_strict_here: bool,
    pub enable_heading_xref: bool,
}

pub fn load_options_from_yaml_file(path: &Path) -> crate::Result<Options> {
    let content = std::fs::read_to_string(path)?;
    let options = serde_yaml::from_str::<Options>(&content)?;
    Ok(options)
}
