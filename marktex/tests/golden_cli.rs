// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

use std::fs;
use std::path::Path;

use assert_cmd::cargo::cargo_bin_cmd;
use tempfile::NamedTempFile;

fn read_expected_tex(fixture_dir: &Path) -> datatest_stable::Result<String> {
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

    Ok(fs::read_to_string(answer_path)?)
}

// 对每个 fixture：输出文件与 stdout 都应与答案一致的 LaTeX。
fn golden_cli_case(path: &Path) -> datatest_stable::Result<()> {
    let fixture_dir = path
        .parent()
        .ok_or_else(|| "fixture input path should have a parent directory")?;
    let expected = read_expected_tex(fixture_dir)?;

    let output = NamedTempFile::new()?;
    cargo_bin_cmd!("marktex")
        .arg("-o")
        .arg(output.path())
        .arg(path)
        .assert()
        .success();

    let actual = fs::read_to_string(output.path())?;
    assert_eq!(actual, expected);

    cargo_bin_cmd!("marktex")
        .arg(path)
        .assert()
        .success()
        .stdout(expected);
    Ok(())
}

datatest_stable::harness!({
    test = golden_cli_case,
    root = concat!(env!("CARGO_MANIFEST_DIR"), "/../marktex-core/tests/fixtures"),
    pattern = r"^[^/]+/input\.md$",
});
