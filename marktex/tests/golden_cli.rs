// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

use std::fs;
use std::path::Path;

use assert_cmd::Command;
use assert_cmd::cargo::cargo_bin_cmd;
use tempfile::NamedTempFile;

fn normalize_line_endings(value: &str) -> String {
    value.replace("\r\n", "\n").replace('\r', "\n")
}

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

fn read_multi_file_input_paths(path: &Path) -> datatest_stable::Result<Vec<std::path::PathBuf>> {
    let fixture_dir = path
        .parent()
        .ok_or("multi-file fixture list path should have a parent directory")?;
    let list = fs::read_to_string(path)?;
    let mut inputs = Vec::new();
    for line in list.lines() {
        let rel = line.trim();
        if rel.is_empty() || rel.starts_with('#') {
            continue;
        }
        inputs.push(fixture_dir.join(rel));
    }
    if inputs.is_empty() {
        return Err("multi-file fixture must provide at least one input markdown".into());
    }
    Ok(inputs)
}

fn append_optional_config_arg(cmd: &mut Command, fixture_dir: &Path) {
    let config_path = fixture_dir.join("config.yaml");
    if config_path.exists() {
        cmd.arg("--config").arg(config_path);
    }
}

// 对每个 fixture：输出文件与 stdout 都应在可选配置下与答案一致。
fn golden_cli_case(path: &Path) -> datatest_stable::Result<()> {
    let file_name = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("");
    let fixture_dir = path
        .parent()
        .ok_or("fixture input path should have a parent directory")?;
    let expected = read_expected_tex(fixture_dir)?;
    let inputs = match file_name {
        "input.md" => vec![path.to_path_buf()],
        "inputs.txt" => read_multi_file_input_paths(path)?,
        _ => {
            return Err(format!(
                "unsupported fixture entry file, expected input.md or inputs.txt: {}",
                path.display()
            )
            .into());
        }
    };

    let output = NamedTempFile::new()?;
    let mut compile_to_file_cmd = cargo_bin_cmd!("marktex");
    compile_to_file_cmd.arg("-o").arg(output.path());
    append_optional_config_arg(&mut compile_to_file_cmd, fixture_dir);
    for input in &inputs {
        compile_to_file_cmd.arg(input);
    }
    compile_to_file_cmd.assert().success();

    let actual = fs::read_to_string(output.path())?;
    assert_eq!(
        normalize_line_endings(&actual),
        normalize_line_endings(&expected)
    );

    let mut compile_to_stdout_cmd = cargo_bin_cmd!("marktex");
    append_optional_config_arg(&mut compile_to_stdout_cmd, fixture_dir);
    for input in &inputs {
        compile_to_stdout_cmd.arg(input);
    }
    let stdout_output = compile_to_stdout_cmd.output()?;
    assert!(stdout_output.status.success());
    let stdout = String::from_utf8(stdout_output.stdout)?;
    assert_eq!(
        normalize_line_endings(&stdout),
        normalize_line_endings(&expected)
    );
    Ok(())
}

datatest_stable::harness!({
    test = golden_cli_case,
    root = concat!(env!("CARGO_MANIFEST_DIR"), "/../marktex-core/tests"),
    pattern = r"^(fixtures/[^/]+/input\.md|fixtures-multi/[^/]+/inputs\.txt)$",
});
