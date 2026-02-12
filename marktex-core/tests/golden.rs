// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

use std::fs;
use std::path::Path;

// 每个 fixture 用例都应编译为期望的 LaTeX 输出。
fn golden_case(path: &Path) -> datatest_stable::Result<()> {
    let input = fs::read_to_string(path)?;

    let fixture_dir = path
        .parent()
        .ok_or_else(|| "fixture input path should have a parent directory")?;

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
    let actual = marktex_core::compile_str(&input)?;

    assert_eq!(actual, expected);
    Ok(())
}

datatest_stable::harness!({
    test = golden_case,
    root = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures"),
    pattern = r"^[^/]+/input\.md$",
});
