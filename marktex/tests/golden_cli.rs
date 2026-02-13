// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

use std::fs;
use std::path::Path;

use assert_cmd::Command;
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

fn append_optional_config_arg(cmd: &mut Command, fixture_dir: &Path) {
    let config_path = fixture_dir.join("config.yaml");
    if config_path.exists() {
        cmd.arg("--config").arg(config_path);
    }
}

// 对每个 fixture：输出文件与 stdout 都应在可选配置下与答案一致。
fn golden_cli_case(path: &Path) -> datatest_stable::Result<()> {
    let fixture_dir = path
        .parent()
        .ok_or("fixture input path should have a parent directory")?;
    let expected = read_expected_tex(fixture_dir)?;

    let output = NamedTempFile::new()?;
    let mut compile_to_file_cmd = cargo_bin_cmd!("marktex");
    compile_to_file_cmd.arg("-o").arg(output.path());
    append_optional_config_arg(&mut compile_to_file_cmd, fixture_dir);
    compile_to_file_cmd.arg(path).assert().success();

    let actual = fs::read_to_string(output.path())?;
    assert_eq!(actual, expected);

    let mut compile_to_stdout_cmd = cargo_bin_cmd!("marktex");
    append_optional_config_arg(&mut compile_to_stdout_cmd, fixture_dir);
    compile_to_stdout_cmd
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
