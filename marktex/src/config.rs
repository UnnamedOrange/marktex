// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

use std::path::Path;

pub fn load_options(path: Option<&Path>) -> marktex_core::Result<marktex_core::Options> {
    match path {
        Some(path) => marktex_core::config::load_options_from_yaml_file(path),
        None => Ok(marktex_core::Options::default()),
    }
}
