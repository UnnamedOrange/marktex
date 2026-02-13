// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

use std::fs;
use std::path::Path;

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

fn read_multi_file_inputs(path: &Path) -> datatest_stable::Result<Vec<String>> {
    let root = path
        .parent()
        .ok_or("multi-file fixture list path should have a parent directory")?;
    let list = fs::read_to_string(path)?;
    let mut inputs = Vec::new();
    for line in list.lines() {
        let rel = line.trim();
        if rel.is_empty() || rel.starts_with('#') {
            continue;
        }
        inputs.push(fs::read_to_string(root.join(rel))?);
    }
    if inputs.is_empty() {
        return Err("multi-file fixture must provide at least one input markdown".into());
    }
    Ok(inputs)
}

// 每个 fixture 用例都应读取可选配置并编译为期望的 LaTeX 输出。
fn golden_case(path: &Path) -> datatest_stable::Result<()> {
    let file_name = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("");

    let fixture_dir = path
        .parent()
        .ok_or("fixture input path should have a parent directory")?;
    let options = read_fixture_options(fixture_dir)?;
    let expected = read_expected_tex(fixture_dir)?;

    let inputs = match file_name {
        "input.md" => vec![fs::read_to_string(path)?],
        "inputs.txt" => read_multi_file_inputs(path)?,
        _ => {
            return Err(format!(
                "unsupported fixture entry file, expected input.md or inputs.txt: {}",
                path.display()
            )
            .into());
        }
    };

    let actual = marktex_core::compile_many_with_options(&inputs, &options)?;

    assert_eq!(
        normalize_line_endings(&actual),
        normalize_line_endings(&expected)
    );
    Ok(())
}

datatest_stable::harness!({
    test = golden_case,
    root = concat!(env!("CARGO_MANIFEST_DIR"), "/tests"),
    pattern = r"^(fixtures/[^/]+/input\.md|fixtures-multi/[^/]+/inputs\.txt)$",
});
