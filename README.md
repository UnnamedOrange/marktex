# marktex

A Markdown-to-LaTeX compiler written in Rust.

## Quick Start

```sh
marktex -o output.tex input.md
```

## YAML Config

Use `--config <FILE>` to pass rendering options.

```sh
marktex --config config.yml -o output.tex input.md
```

Example `config.yml`:

```yaml
enable_advanced_xref: true
enable_center_as_figure_caption: true
enable_figure_relative_xref: true
enable_heading_xref: true
force_figure_strict_here: true
```

Options:

| Key                               | Type   | Default | Description                                                  |
| --------------------------------- | ------ | ------- | ------------------------------------------------------------ |
| `enable_advanced_xref`            | `bool` | `false` | Reuse `*...*` as advanced semantics: reference and index entries for other terms. |
| `enable_center_as_figure_caption` | `bool` | `false` | Treat a `<center>...</center>` block adjacent to a standalone image block as figure caption and optional label source. |
| `enable_figure_relative_xref`     | `bool` | `false` | Enable relative figure references (`*上图*`/`*下图*`). This option only takes effect when `enable_advanced_xref` is also `true`. |
| `enable_heading_xref`             | `bool` | `false` | Convert heading anchor links (`[text](#heading)`) to `\hyperref[...]` when resolvable. |
| `force_figure_strict_here`        | `bool` | `false` | Set placement arguments of float images to `[H]`.            |

## License

Copyright (c) UnnamedOrange. Licensed under the MIT License.
See the LICENSE file in the repository root for full license text.

## Credits

### [clap](https://github.com/clap-rs/clap)

Licensed under the MIT License.

### [comrak](https://github.com/kivikakk/comrak)

Licensed under the BSD-2-Clause License.

### [serde](https://github.com/serde-rs/serde)

Licensed under the MIT License.

### [serde_yaml_ng](https://github.com/acatton/serde-yaml-ng)

Licensed under the MIT License.
