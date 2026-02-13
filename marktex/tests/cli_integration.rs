// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

use std::fs;
use std::path::PathBuf;

use assert_cmd::cargo::cargo_bin_cmd;
use predicates::prelude::*;
use tempfile::{NamedTempFile, tempdir};

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

fn write_temp_config(contents: &str) -> NamedTempFile {
    let config = NamedTempFile::new().unwrap();
    fs::write(config.path(), contents).unwrap();
    config
}

// 行为：未指定 --config 时，选项对象应与默认值完全一致。
#[test]
fn options_without_config_use_default() {
    let options = marktex::config::load_options(None::<&std::path::Path>).unwrap();
    assert_eq!(options, marktex_core::Options::default());
}

// 行为：合法 bool 赋值应只覆盖对应字段，其他字段保持默认值。
#[test]
fn options_accept_valid_bool_assignment() {
    let config = write_temp_config("enable_heading_xref: true\n");

    let options = marktex::config::load_options(Some(config.path())).unwrap();
    let expected = marktex_core::Options {
        enable_heading_xref: true,
        ..Default::default()
    };
    assert_eq!(options, expected);
}

// 行为：至少三种非 bool 类型赋值都应触发非法选项错误。
#[test]
fn options_reject_non_bool_values() {
    for invalid in ["123", "\"true\"", "[true]"] {
        let config = write_temp_config(&format!("enable_heading_xref: {invalid}\n"));
        let result = marktex::config::load_options(Some(config.path()));
        assert!(
            result.is_err(),
            "non-bool value should be rejected: {invalid}"
        );
    }
}

// 行为：未知字段应触发非法选项错误。
#[test]
fn options_reject_unknown_field() {
    let config = write_temp_config("this_is_definitely_not_a_valid_option: true\n");
    let result = marktex::config::load_options(Some(config.path()));
    assert!(result.is_err());
}

// 行为：不存在的配置文件应触发配置文件无效错误。
#[test]
fn options_reject_missing_config_file() {
    let dir = tempdir().unwrap();
    let missing = dir.path().join("missing.yaml");
    let result = marktex::config::load_options(Some(missing.as_path()));
    assert!(result.is_err());
}

// 行为：非法 YAML 应触发配置文件无效错误。
#[test]
fn options_reject_invalid_yaml() {
    let config = write_temp_config("enable_heading_xref: [true\n");
    let result = marktex::config::load_options(Some(config.path()));
    assert!(result.is_err());
}
