// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

pub type Error = Box<dyn std::error::Error>;
pub type Result<T> = std::result::Result<T, Error>;

pub fn compile_str(markdown: &str) -> Result<String> {
    let preprocessed = preprocess_markdown(markdown);
    let arena = comrak::Arena::new();
    let mut options = comrak::Options::default();
    options.extension.math_dollars = true;
    options.parse.ignore_setext = true;
    let root = comrak::parse_document(&arena, &preprocessed.markdown, &options);

    let mut blocks = Vec::new();
    for node in root.children() {
        if let Some(block) = render_block(node, 0, &preprocessed) {
            blocks.push(block);
        }
    }

    if blocks.is_empty() {
        return Ok(String::new());
    }

    Ok(format!("{}\n", blocks.join("\n\n")))
}

fn render_block<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    indent: usize,
    preprocessed: &PreprocessedMarkdown,
) -> Option<String> {
    match &node.data.borrow().value {
        comrak::nodes::NodeValue::Paragraph => {
            Some(indent_multiline(indent, render_inlines(node, preprocessed)))
        }
        comrak::nodes::NodeValue::Heading(heading) => {
            let content = render_inlines(node, preprocessed);
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
        comrak::nodes::NodeValue::BlockQuote => {
            Some(render_block_quote(node, indent, preprocessed))
        }
        comrak::nodes::NodeValue::List(list) => Some(render_list(node, indent, list, preprocessed)),
        _ => None,
    }
}

fn render_inlines<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    preprocessed: &PreprocessedMarkdown,
) -> String {
    let mut out = String::new();
    for child in node.children() {
        match &child.data.borrow().value {
            comrak::nodes::NodeValue::Text(text) => {
                if let Some(literal) = preprocessed.math_block_literal(text) {
                    out.push_str(&render_display_math(literal));
                } else {
                    push_text(&mut out, text, preprocessed.trailing_space_sentinel);
                }
            }
            comrak::nodes::NodeValue::Code(code) => {
                out.push_str(&code.literal);
            }
            comrak::nodes::NodeValue::Emph => {
                if let Some(inner) = emph_as_strong_emph(child, preprocessed) {
                    out.push_str(&format!("\\textbf{{\\emph{{{inner}}}}}"));
                } else {
                    let inner = render_inlines(child, preprocessed);
                    out.push_str(&format!("\\emph{{{inner}}}"));
                }
            }
            comrak::nodes::NodeValue::Strong => {
                let inner = render_inlines(child, preprocessed);
                out.push_str(&format!("\\textbf{{{inner}}}"));
            }
            comrak::nodes::NodeValue::Math(math) => {
                if math.display_math {
                    out.push_str(&render_display_math(&math.literal));
                } else {
                    out.push('$');
                    out.push_str(&math.literal);
                    out.push('$');
                }
            }
            comrak::nodes::NodeValue::SoftBreak => out.push('\n'),
            comrak::nodes::NodeValue::LineBreak => out.push('\n'),
            _ => {}
        }
    }
    out
}

const MATH_BLOCK_PLACEHOLDER_BASE: &str = "@@MARKTEX_MATH_BLOCK_";
const MATH_BLOCK_PLACEHOLDER_SUFFIX: &str = "@@";

struct PreprocessedMarkdown {
    markdown: String,
    math_blocks: Vec<String>,
    math_block_placeholder_prefix: String,
    trailing_space_sentinel: char,
}

impl PreprocessedMarkdown {
    fn math_block_literal(&self, placeholder: &str) -> Option<&str> {
        let number = placeholder
            .strip_prefix(&self.math_block_placeholder_prefix)?
            .strip_suffix(MATH_BLOCK_PLACEHOLDER_SUFFIX)?;
        let index: usize = number.parse().ok()?;
        self.math_blocks.get(index).map(String::as_str)
    }
}

fn choose_math_block_placeholder_prefix(markdown: &str) -> String {
    use std::hash::{Hash, Hasher};

    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    markdown.hash(&mut hasher);
    let mut nonce = hasher.finish();

    loop {
        let prefix = format!("{MATH_BLOCK_PLACEHOLDER_BASE}{nonce:x}_");
        if !markdown.contains(&prefix) {
            return prefix;
        }
        nonce = nonce.wrapping_add(1);
    }
}

fn choose_trailing_space_sentinel(markdown: &str) -> char {
    for codepoint in 0xE000..=0xF8FF {
        let Some(ch) = char::from_u32(codepoint) else {
            continue;
        };
        if !markdown.contains(ch) {
            return ch;
        }
    }

    '\u{E000}'
}

fn preprocess_markdown(markdown: &str) -> PreprocessedMarkdown {
    let trailing_space_sentinel = choose_trailing_space_sentinel(markdown);
    let math_block_placeholder_prefix = choose_math_block_placeholder_prefix(markdown);

    let mut out = String::with_capacity(markdown.len());
    let mut math_blocks = Vec::new();

    let mut lines = markdown.split_inclusive('\n').peekable();
    while let Some(line) = lines.next() {
        let (content, newline) = match line.strip_suffix('\n') {
            Some(content) => (content, "\n"),
            None => (line, ""),
        };

        if content == "$$" {
            let mut literal = String::new();
            while let Some(next) = lines.next() {
                let next_content = next.strip_suffix('\n').unwrap_or(next);
                if next_content == "$$" {
                    break;
                }
                literal.push_str(next);
            }

            let id = math_blocks.len();
            math_blocks.push(literal);
            out.push_str(&math_block_placeholder_prefix);
            out.push_str(&id.to_string());
            out.push_str(MATH_BLOCK_PLACEHOLDER_SUFFIX);
            out.push_str(newline);
            continue;
        }

        if let Some(encoded) =
            encode_space_before_trailing_closing_strong(content, trailing_space_sentinel)
        {
            out.push_str(&encoded);
            out.push_str(newline);
        } else {
            out.push_str(line);
        }
    }

    PreprocessedMarkdown {
        markdown: out,
        math_blocks,
        math_block_placeholder_prefix,
        trailing_space_sentinel,
    }
}

fn encode_space_before_trailing_closing_strong(
    content: &str,
    trailing_space_sentinel: char,
) -> Option<String> {
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
    encoded.push(trailing_space_sentinel);
    encoded.push_str("**");
    encoded.push_str(suffix);
    Some(encoded)
}

fn is_trailing_closing_strong_suffix_char(ch: char) -> bool {
    !ch.is_whitespace() && !ch.is_alphanumeric()
}

fn push_text(out: &mut String, text: &str, trailing_space_sentinel: char) {
    for ch in text.chars() {
        if ch == trailing_space_sentinel {
            out.push(' ');
        } else {
            out.push(ch);
        }
    }
}

fn emph_as_strong_emph<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    preprocessed: &PreprocessedMarkdown,
) -> Option<String> {
    let mut children = node.children();
    let child = children.next()?;
    if children.next().is_some() {
        return None;
    }

    matches!(child.data.borrow().value, comrak::nodes::NodeValue::Strong)
        .then(|| render_inlines(child, preprocessed))
}

fn render_display_math(literal: &str) -> String {
    let literal = literal.strip_prefix('\n').unwrap_or(literal);
    let mut out = String::new();
    out.push_str("$$\n");
    out.push_str(literal);
    if !literal.ends_with('\n') {
        out.push('\n');
    }
    out.push_str("$$");
    out
}

fn render_block_quote<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    indent: usize,
    preprocessed: &PreprocessedMarkdown,
) -> String {
    let content_indent = indent + 4;
    let mut blocks = Vec::new();
    for child in node.children() {
        if let Some(block) = render_block(child, content_indent, preprocessed) {
            blocks.push(block);
        }
    }

    let mut lines = Vec::new();
    lines.push(indent_line(indent, "\\begin{quotation}"));
    if !blocks.is_empty() {
        lines.extend(blocks.join("\n\n").split('\n').map(|l| l.to_string()));
    }
    lines.push(indent_line(indent, "\\end{quotation}"));
    lines.join("\n")
}

fn render_list<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    indent: usize,
    list: &comrak::nodes::NodeList,
    preprocessed: &PreprocessedMarkdown,
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
        lines.extend(render_item(item, item_indent, list.tight, preprocessed));
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
    preprocessed: &PreprocessedMarkdown,
) -> Vec<String> {
    let mut blocks = node.children().collect::<Vec<_>>();
    if blocks.is_empty() {
        return vec![indent_line(indent, "\\item")];
    }

    let first = blocks.remove(0);
    let mut lines = Vec::new();

    match &first.data.borrow().value {
        comrak::nodes::NodeValue::Paragraph => {
            let content = render_inlines(first, preprocessed);
            lines.push(indent_line(indent, format!("\\item {content}")));
        }
        comrak::nodes::NodeValue::List(list) => {
            lines.push(indent_line(indent, "\\item"));
            let nested = render_list(first, indent, list, preprocessed);
            lines.extend(nested.split('\n').map(|l| l.to_string()));
        }
        _ => {
            if let Some(block) = render_block(first, indent + 4, preprocessed) {
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
                let nested = render_list(block, indent, list, preprocessed);
                lines.extend(nested.split('\n').map(|l| l.to_string()));
            }
            _ => {
                if !parent_tight {
                    lines.push(String::new());
                }
                if let Some(rendered) = render_block(block, indent + 4, preprocessed) {
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
