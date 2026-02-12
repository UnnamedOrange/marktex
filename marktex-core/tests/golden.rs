// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

use std::fs;
use std::path::Path;

fn read_fixture_options(
    fixture_dir: &Path,
) -> datatest_stable::Result<marktex_core::config::Options> {
    let config_path = fixture_dir.join("config.yaml");
    if config_path.exists() {
        Ok(marktex_core::config::load_options_from_yaml_file(
            config_path.as_path(),
        )?)
    } else {
        Ok(marktex_core::config::Options::default())
    }
}

// 每个 fixture 用例都应读取可选配置并编译为期望的 LaTeX 输出。
fn golden_case(path: &Path) -> datatest_stable::Result<()> {
    let input = fs::read_to_string(path)?;

    let fixture_dir = path
        .parent()
        .ok_or_else(|| "fixture input path should have a parent directory")?;
    let options = read_fixture_options(fixture_dir)?;

    let mut tex_files = fixture_dir
        .read_dir()?
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|p| p.extension().is_some_and(|ext| ext == "tex"))
        .collect::<Vec<_>>();
    tex_files.sort();

    let answer_path = match tex_files.as_slice() {
        [only] => only,
        _ => return Err("fixture directory must contain exactly one .tex answer file".into()),
    };

    let expected = fs::read_to_string(answer_path)?;
    let actual = marktex_core::compile_str_with_options(&input, &options)?;

    assert_eq!(actual, expected);
    Ok(())
}

datatest_stable::harness!({
    test = golden_case,
    root = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures"),
    pattern = r"^[^/]+/input\.md$",
});
