// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

use std::path::PathBuf;

use assert_cmd::cargo::cargo_bin_cmd;
use predicates::prelude::*;
use tempfile::tempdir;

// 行为：无参时退出 0，并输出 help 关键字。
#[test]
fn no_args_prints_help() {
    cargo_bin_cmd!("marktex")
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage"));
}

// 行为：--help 退出 0，并输出 help 关键字。
#[test]
fn help_exits_zero() {
    cargo_bin_cmd!("marktex")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage"));
}

// 行为：--version 退出 0，并输出版本关键字。
#[test]
fn version_exits_zero() {
    cargo_bin_cmd!("marktex")
        .arg("--version")
        .assert()
        .success()
        .stdout(predicate::str::contains("marktex"));
}

// 行为：有输入文件但无 -o 时退出 0，并输出到 stdout。
#[test]
fn input_without_output_writes_stdout() {
    let dir = tempdir().unwrap();
    let input_path = dir.path().join("input.md");
    std::fs::write(&input_path, "# Hello\n").unwrap();

    cargo_bin_cmd!("marktex")
        .arg(&input_path)
        .assert()
        .success()
        .stdout(predicate::str::is_empty().not());
}

// 行为：有输入文件并指定 -o 时退出 0，并写出文件存在。
#[test]
fn input_with_output_writes_file() {
    let dir = tempdir().unwrap();
    let input_path = dir.path().join("input.md");
    std::fs::write(&input_path, "# Hello\n").unwrap();

    let output_path: PathBuf = dir.path().join("out.tex");

    cargo_bin_cmd!("marktex")
        .arg("-o")
        .arg(&output_path)
        .arg(&input_path)
        .assert()
        .success();

    assert!(output_path.exists());
}

// 行为：仅指定 -o 但没有输入文件时退出非 0，并输出固定错误原因。
#[test]
fn output_without_input_errors() {
    let dir = tempdir().unwrap();
    let output_path: PathBuf = dir.path().join("out.tex");

    cargo_bin_cmd!("marktex")
        .arg("-o")
        .arg(&output_path)
        .assert()
        .failure()
        .stderr(predicate::str::contains("没有输入文件"));
}
