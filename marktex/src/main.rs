// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

mod cli;

fn main() {}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use clap::error::ErrorKind;
    use clap::Parser;

    // 行为：仅输入文件时能解析 inputs 且 output 为空。
    #[test]
    fn parse_inputs_only() {
        let args = crate::cli::CliArgs::try_parse_from(["marktex", "a.md", "b.md"]).unwrap();
        assert_eq!(args.inputs, vec![PathBuf::from("a.md"), PathBuf::from("b.md")]);
        assert_eq!(args.output, None);
    }

    // 行为：支持短参数 -o/--output，并且允许与 inputs 混排。
    #[test]
    fn parse_output_short_mixed() {
        let args =
            crate::cli::CliArgs::try_parse_from(["marktex", "-o", "out.tex", "a.md"]).unwrap();
        assert_eq!(args.inputs, vec![PathBuf::from("a.md")]);
        assert_eq!(args.output, Some(PathBuf::from("out.tex")));
    }

    // 行为：支持长参数 --output，并且允许与 inputs 混排。
    #[test]
    fn parse_output_long_mixed() {
        let args = crate::cli::CliArgs::try_parse_from([
            "marktex",
            "a.md",
            "--output",
            "out.tex",
            "b.md",
        ])
        .unwrap();
        assert_eq!(args.inputs, vec![PathBuf::from("a.md"), PathBuf::from("b.md")]);
        assert_eq!(args.output, Some(PathBuf::from("out.tex")));
    }

    // 行为：重复指定 -o 会报错。
    #[test]
    fn error_on_duplicate_output() {
        let err = crate::cli::CliArgs::try_parse_from([
            "marktex",
            "-o",
            "a.tex",
            "-o",
            "b.tex",
            "input.md",
        ])
        .unwrap_err();
        assert_eq!(err.kind(), ErrorKind::ArgumentConflict);
    }

    // 行为：-o 缺少值会报错。
    #[test]
    fn error_on_output_missing_value() {
        let err = crate::cli::CliArgs::try_parse_from(["marktex", "-o"]).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::InvalidValue);
    }

    // 行为：未知参数会报错。
    #[test]
    fn error_on_unknown_argument() {
        let err = crate::cli::CliArgs::try_parse_from(["marktex", "--unknown"]).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::UnknownArgument);
    }
}
