// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

pub type Error = Box<dyn std::error::Error>;
pub type Result<T> = std::result::Result<T, Error>;

pub fn compile_str(markdown: &str) -> Result<String> {
    let markdown = preprocess_markdown(markdown);
    let arena = comrak::Arena::new();
    let options = comrak::Options::default();
    let root = comrak::parse_document(&arena, &markdown, &options);

    let mut blocks = Vec::new();
    for node in root.children() {
        if let Some(block) = render_block(node, 0) {
            blocks.push(block);
        }
    }

    if blocks.is_empty() {
        return Ok(String::new());
    }

    Ok(format!("{}\n", blocks.join("\n\n")))
}

fn render_block<'a>(node: &'a comrak::nodes::AstNode<'a>, indent: usize) -> Option<String> {
    match &node.data.borrow().value {
        comrak::nodes::NodeValue::Paragraph => Some(indent_multiline(indent, render_inlines(node))),
        comrak::nodes::NodeValue::Heading(heading) => {
            let content = render_inlines(node);
            let command = match heading.level {
                1 => "chapter",
                2 => "section",
                3 => "subsection",
                4 => "subsubsection",
                _ => return Some(indent_multiline(indent, content)),
            };
            Some(indent_multiline(
                indent,
                format!("\\{command}{{{content}}}"),
            ))
        }
        comrak::nodes::NodeValue::List(list) => Some(render_list(node, indent, list)),
        _ => None,
    }
}

fn render_inlines<'a>(node: &'a comrak::nodes::AstNode<'a>) -> String {
    let mut out = String::new();
    for child in node.children() {
        match &child.data.borrow().value {
            comrak::nodes::NodeValue::Text(text) => push_text(&mut out, text),
            comrak::nodes::NodeValue::Code(code) => {
                out.push_str(&code.literal);
            }
            comrak::nodes::NodeValue::Emph => {
                if let Some(inner) = emph_as_strong_emph(child) {
                    out.push_str(&format!("\\textbf{{\\emph{{{inner}}}}}"));
                } else {
                    let inner = render_inlines(child);
                    out.push_str(&format!("\\emph{{{inner}}}"));
                }
            }
            comrak::nodes::NodeValue::Strong => {
                let inner = render_inlines(child);
                out.push_str(&format!("\\textbf{{{inner}}}"));
            }
            comrak::nodes::NodeValue::SoftBreak => out.push('\n'),
            comrak::nodes::NodeValue::LineBreak => out.push('\n'),
            _ => {}
        }
    }
    out
}

const TRAILING_SPACE_SENTINEL: char = '\u{E000}';

fn preprocess_markdown(markdown: &str) -> String {
    let mut out = String::with_capacity(markdown.len());
    for line in markdown.split_inclusive('\n') {
        let (content, newline) = match line.strip_suffix('\n') {
            Some(content) => (content, "\n"),
            None => (line, ""),
        };

        if let Some(encoded) = encode_space_before_trailing_closing_strong(content) {
            push_text_escape_trailing_space_sentinel(&mut out, &encoded);
            out.push_str(newline);
        } else {
            push_text_escape_trailing_space_sentinel(&mut out, line);
        }
    }
    out
}

fn encode_space_before_trailing_closing_strong(content: &str) -> Option<String> {
    const CLOSING_STRONG_WITH_SPACE: &str = " **";

    let marker_start = content.rfind(CLOSING_STRONG_WITH_SPACE)?;
    let marker_end = marker_start + CLOSING_STRONG_WITH_SPACE.len();
    let suffix = &content[marker_end..];
    if !suffix.chars().all(is_trailing_closing_strong_suffix_char) {
        return None;
    }

    let prefix = &content[..marker_start];
    if prefix.is_empty() || !prefix.contains("**") {
        return None;
    }

    let mut encoded = String::with_capacity(content.len());
    encoded.push_str(prefix);
    encoded.push(TRAILING_SPACE_SENTINEL);
    encoded.push_str("**");
    encoded.push_str(suffix);
    Some(encoded)
}

fn is_trailing_closing_strong_suffix_char(ch: char) -> bool {
    !ch.is_whitespace() && !ch.is_alphanumeric()
}

fn push_text(out: &mut String, text: &str) {
    let mut chars = text.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == TRAILING_SPACE_SENTINEL {
            if chars.peek() == Some(&TRAILING_SPACE_SENTINEL) {
                out.push(TRAILING_SPACE_SENTINEL);
                chars.next();
            } else {
                out.push(' ');
            }
        } else {
            out.push(ch);
        }
    }
}

fn push_text_escape_trailing_space_sentinel(out: &mut String, text: &str) {
    for ch in text.chars() {
        out.push(ch);
        if ch == TRAILING_SPACE_SENTINEL {
            out.push(ch);
        }
    }
}

fn emph_as_strong_emph<'a>(node: &'a comrak::nodes::AstNode<'a>) -> Option<String> {
    let mut children = node.children();
    let child = children.next()?;
    if children.next().is_some() {
        return None;
    }

    matches!(child.data.borrow().value, comrak::nodes::NodeValue::Strong)
        .then(|| render_inlines(child))
}

fn render_list<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    indent: usize,
    list: &comrak::nodes::NodeList,
) -> String {
    let env = match list.list_type {
        comrak::nodes::ListType::Bullet => "itemize",
        comrak::nodes::ListType::Ordered => "enumerate",
    };

    let item_indent = indent + 4;
    let items = node.children().collect::<Vec<_>>();

    let mut lines = Vec::new();
    lines.push(indent_line(indent, format!("\\begin{{{env}}}")));

    for (index, item) in items.iter().enumerate() {
        lines.extend(render_item(item, item_indent, list.tight));
        if !list.tight && index + 1 < items.len() {
            lines.push(String::new());
        }
    }

    lines.push(indent_line(indent, format!("\\end{{{env}}}")));
    lines.join("\n")
}

fn render_item<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    indent: usize,
    parent_tight: bool,
) -> Vec<String> {
    let mut blocks = node.children().collect::<Vec<_>>();
    if blocks.is_empty() {
        return vec![indent_line(indent, "\\item")];
    }

    let first = blocks.remove(0);
    let mut lines = Vec::new();

    match &first.data.borrow().value {
        comrak::nodes::NodeValue::Paragraph => {
            let content = render_inlines(first);
            lines.push(indent_line(indent, format!("\\item {content}")));
        }
        comrak::nodes::NodeValue::List(list) => {
            lines.push(indent_line(indent, "\\item"));
            let nested = render_list(first, indent, list);
            lines.extend(nested.split('\n').map(|l| l.to_string()));
        }
        _ => {
            if let Some(block) = render_block(first, indent + 4) {
                lines.push(indent_line(indent, "\\item"));
                lines.extend(block.split('\n').map(|l| l.to_string()));
            } else {
                lines.push(indent_line(indent, "\\item"));
            }
        }
    }

    for block in blocks {
        match &block.data.borrow().value {
            comrak::nodes::NodeValue::List(list) => {
                let nested = render_list(block, indent, list);
                lines.extend(nested.split('\n').map(|l| l.to_string()));
            }
            _ => {
                if !parent_tight {
                    lines.push(String::new());
                }
                if let Some(rendered) = render_block(block, indent + 4) {
                    lines.extend(rendered.split('\n').map(|l| l.to_string()));
                }
            }
        }
    }

    lines
}

fn indent_line(indent: usize, line: impl AsRef<str>) -> String {
    if indent == 0 {
        line.as_ref().to_string()
    } else {
        format!("{:indent$}{}", "", line.as_ref(), indent = indent)
    }
}

fn indent_multiline(indent: usize, text: String) -> String {
    if indent == 0 {
        return text;
    }

    text.split('\n')
        .map(|line| {
            if line.is_empty() {
                String::new()
            } else {
                indent_line(indent, line)
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}
