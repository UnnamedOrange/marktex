// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

pub type Error = Box<dyn std::error::Error>;
pub type Result<T> = std::result::Result<T, Error>;

pub fn compile_str(markdown: &str) -> Result<String> {
    let preprocessed = preprocess_markdown(markdown);
    let arena = comrak::Arena::new();
    let mut options = comrak::Options::default();
    options.extension.math_dollars = true;
    options.extension.table = true;
    options.parse.ignore_setext = true;
    let root = comrak::parse_document(&arena, &preprocessed.markdown, &options);

    let mut blocks = Vec::new();
    for node in root.children() {
        if let Some(block) = render_block(node, 0, &preprocessed) {
            blocks.push(RenderedBlock {
                sourcepos: Some(node.data.borrow().sourcepos),
                text: block,
            });
        }
    }

    if blocks.is_empty() {
        return Ok(String::new());
    }

    let mut out = String::new();
    for (index, block) in blocks.iter().enumerate() {
        if index > 0 {
            out.push_str(block_separator(&blocks[index - 1], block));
        }
        out.push_str(&block.text);
    }
    out.push('\n');
    Ok(out)
}

struct RenderedBlock {
    sourcepos: Option<comrak::nodes::Sourcepos>,
    text: String,
}

fn block_separator(prev: &RenderedBlock, next: &RenderedBlock) -> &'static str {
    match (prev.sourcepos, next.sourcepos) {
        (Some(prev), Some(next)) => {
            if next.start.line > prev.end.line + 1 {
                "\n\n"
            } else {
                "\n"
            }
        }
        _ => "\n\n",
    }
}

fn render_block<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    indent: usize,
    preprocessed: &PreprocessedMarkdown,
) -> Option<String> {
    match &node.data.borrow().value {
        comrak::nodes::NodeValue::Paragraph => Some(render_paragraph(node, indent, preprocessed)),
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
        comrak::nodes::NodeValue::CodeBlock(code_block) => Some(render_code_block(code_block)),
        comrak::nodes::NodeValue::HtmlBlock(html) => render_html_block(html, indent),
        comrak::nodes::NodeValue::List(list) => Some(render_list(node, indent, list, preprocessed)),
        comrak::nodes::NodeValue::Table(table) => Some(render_table(node, indent, table, preprocessed)),
        _ => None,
    }
}

fn render_paragraph<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    indent: usize,
    preprocessed: &PreprocessedMarkdown,
) -> String {
    if let Some(figure) = render_paragraph_as_figure(node, indent) {
        return figure;
    }

    indent_multiline(indent, render_inlines(node, preprocessed))
}

fn render_paragraph_as_figure<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    indent: usize,
) -> Option<String> {
    let mut children = node.children();
    let only = children.next()?;
    if children.next().is_some() {
        return None;
    }

    match &only.data.borrow().value {
        comrak::nodes::NodeValue::Image(image) => {
            let include = render_includegraphics(&image.url, None);
            Some(render_figure(indent, &include))
        }
        comrak::nodes::NodeValue::HtmlInline(html) => {
            let image = parse_html_img_tag(html)?;
            let include = render_includegraphics(&image.src, image.option);
            Some(render_figure(indent, &include))
        }
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
                out.push_str(&render_inline_code(&code.literal));
            }
            comrak::nodes::NodeValue::Image(image) => {
                out.push_str(&render_includegraphics(&image.url, None));
            }
            comrak::nodes::NodeValue::HtmlInline(html) => {
                if let Some(image) = parse_html_img_tag(html) {
                    let include = render_includegraphics(&image.src, image.option);
                    out.push_str(&render_figure(0, &include));
                } else if let Some(content) = parse_html_center_tag(html) {
                    out.push_str(&render_center(0, &content));
                }
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

fn render_inlines_escaped<'a>(
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
                    out.push_str(&escape_latex_text_restore_trailing_space_sentinel(
                        text,
                        preprocessed.trailing_space_sentinel,
                    ));
                }
            }
            comrak::nodes::NodeValue::Code(code) => {
                out.push_str(&render_inline_code(&code.literal));
            }
            comrak::nodes::NodeValue::Emph => {
                let inner = render_inlines_escaped(child, preprocessed);
                out.push_str(&format!("\\emph{{{inner}}}"));
            }
            comrak::nodes::NodeValue::Strong => {
                let inner = render_inlines_escaped(child, preprocessed);
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

fn escape_latex_text(text: &str) -> String {
    escape_latex_text_inner(text, None)
}

fn escape_latex_text_restore_trailing_space_sentinel(
    text: &str,
    trailing_space_sentinel: char,
) -> String {
    escape_latex_text_inner(text, Some(trailing_space_sentinel))
}

fn escape_latex_text_inner(text: &str, trailing_space_sentinel: Option<char>) -> String {
    let mut out = String::new();
    for mut ch in text.chars() {
        if trailing_space_sentinel == Some(ch) {
            ch = ' ';
        }

        match ch {
            '&' => out.push_str("\\&"),
            '%' => out.push_str("\\%"),
            '$' => out.push_str("\\$"),
            '#' => out.push_str("\\#"),
            '_' => out.push_str("\\_"),
            '{' => out.push_str("\\{"),
            '}' => out.push_str("\\}"),
            '~' => out.push_str("\\textasciitilde{}"),
            '^' => out.push_str("\\textasciicircum{}"),
            '\\' => out.push_str("\\textbackslash{}"),
            _ => out.push(ch),
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
    if is_top_level_display_math_environment(literal) {
        return literal.strip_suffix('\n').unwrap_or(literal).to_string();
    }

    let mut out = String::new();
    out.push_str("$$\n");
    out.push_str(literal);
    if !literal.ends_with('\n') {
        out.push('\n');
    }
    out.push_str("$$");
    out
}

fn is_top_level_display_math_environment(literal: &str) -> bool {
    let env = literal
        .trim_start()
        .strip_prefix("\\begin{")
        .and_then(|rest| rest.split_once('}'))
        .map(|(env, _)| env);

    let Some(env) = env else {
        return false;
    };

    let supported = matches!(
        env,
        "align"
            | "align*"
            | "equation"
            | "equation*"
            | "alignat"
            | "alignat*"
            | "gather"
            | "gather*"
    );
    if !supported {
        return false;
    }

    literal.trim_end().ends_with(&format!("\\end{{{env}}}"))
}

fn render_code_block(code_block: &comrak::nodes::NodeCodeBlock) -> String {
    let info = code_block.info.trim();
    let language = info.split_whitespace().next().unwrap_or("");

    let mut out = String::new();
    if language.is_empty() {
        out.push_str("\\begin{lstlisting}\n");
    } else {
        out.push_str("\\begin{lstlisting}[language=");
        out.push_str(language);
        out.push_str("]\n");
    }

    out.push_str(code_block.literal.trim_end_matches('\n'));
    out.push('\n');
    out.push_str("\\end{lstlisting}");
    out
}

fn render_table<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    indent: usize,
    table: &comrak::nodes::NodeTable,
    preprocessed: &PreprocessedMarkdown,
) -> String {
    let mut spec = String::new();
    for index in 0..table.num_columns {
        let align = table
            .alignments
            .get(index)
            .copied()
            .unwrap_or(comrak::nodes::TableAlignment::None);
        spec.push(match align {
            comrak::nodes::TableAlignment::Center => 'c',
            comrak::nodes::TableAlignment::Right => 'r',
            comrak::nodes::TableAlignment::Left | comrak::nodes::TableAlignment::None => 'l',
        });
    }

    let mut lines = Vec::new();
    lines.push(indent_line(
        indent,
        format!("\\begin{{tabular}}{{{spec}}}"),
    ));
    lines.push(indent_line(indent + 4, "\\hline"));

    for row in node.children() {
        if !matches!(
            &row.data.borrow().value,
            comrak::nodes::NodeValue::TableRow(_)
        ) {
            continue;
        }

        let mut cells = Vec::new();
        for cell in row.children().take(table.num_columns) {
            if matches!(
                &cell.data.borrow().value,
                comrak::nodes::NodeValue::TableCell
            ) {
                cells.push(render_inlines_escaped(cell, preprocessed));
            }
        }
        while cells.len() < table.num_columns {
            cells.push(String::new());
        }

        lines.push(indent_line(indent + 4, format_table_row(&cells)));
        lines.push(indent_line(indent + 4, "\\\\\\hline"));
    }

    lines.push(indent_line(indent, "\\end{tabular}"));
    lines.join("\n")
}

fn format_table_row(cells: &[String]) -> String {
    let mut out = String::new();
    for (index, cell) in cells.iter().enumerate() {
        if index > 0 {
            out.push_str(" & ");
        }
        out.push_str(cell);
    }

    if out.starts_with(" & ") {
        out.remove(0);
    }
    if out.ends_with(" & ") {
        out.pop();
    }

    out
}

fn render_html_block(html: &comrak::nodes::NodeHtmlBlock, indent: usize) -> Option<String> {
    let images = parse_html_img_tags(&html.literal);
    if !images.is_empty() {
        let figures = images
            .into_iter()
            .map(|image| {
                let include = render_includegraphics(&image.src, image.option);
                render_figure(indent, &include)
            })
            .collect::<Vec<_>>();
        return Some(figures.join("\n"));
    }

    if let Some(content) = parse_html_center_tag(&html.literal) {
        return Some(render_center(indent, &content));
    }

    None
}

enum IncludeGraphicsOption {
    Scale(f64),
    WidthTextWidth(f64),
}

fn render_includegraphics(url: &str, option: Option<IncludeGraphicsOption>) -> String {
    let url = escape_latex_text(url);
    match option {
        Some(IncludeGraphicsOption::Scale(scale)) => {
            format!("\\includegraphics[scale={scale:.2}]{{{url}}}")
        }
        Some(IncludeGraphicsOption::WidthTextWidth(width)) => {
            let width = format_f64_trimmed_2(width);
            format!("\\includegraphics[width={width}\\textwidth]{{{url}}}")
        }
        None => format!("\\includegraphics{{{url}}}"),
    }
}

fn format_f64_trimmed_2(value: f64) -> String {
    let mut out = format!("{value:.2}");
    while out.contains('.') && out.ends_with('0') {
        out.pop();
    }
    if out.ends_with('.') {
        out.pop();
    }
    out
}

fn render_figure(indent: usize, includegraphics: &str) -> String {
    let mut lines = Vec::new();
    lines.push(indent_line(indent, "\\begin{figure}"));
    lines.push(indent_line(indent + 4, "\\centering"));
    lines.push(indent_line(indent + 4, includegraphics));
    lines.push(indent_line(indent, "\\end{figure}"));
    lines.join("\n")
}

fn render_center(indent: usize, content: &str) -> String {
    let mut lines = Vec::new();
    lines.push(indent_line(indent, "\\begin{center}"));
    lines.push(indent_line(indent + 4, content));
    lines.push(indent_line(indent, "\\end{center}"));
    lines.join("\n")
}

struct HtmlImage {
    src: String,
    option: Option<IncludeGraphicsOption>,
}

fn parse_html_img_tag(html: &str) -> Option<HtmlImage> {
    let html = html.trim();
    if !html.starts_with("<img") {
        return None;
    }

    let src = extract_html_attr_value(html, "src")?;
    let option =
        extract_html_attr_value(html, "style").and_then(|style| parse_html_img_option(&style));

    Some(HtmlImage { src, option })
}

fn parse_html_img_tags(html: &str) -> Vec<HtmlImage> {
    let html = html.trim();
    if !html.starts_with("<img") {
        return Vec::new();
    }

    let mut out = Vec::new();
    let mut offset = 0;
    while let Some(start) = html[offset..].find("<img") {
        let start = offset + start;
        let Some(end) = html[start..].find('>') else {
            break;
        };
        let end = start + end + 1;
        if let Some(image) = parse_html_img_tag(&html[start..end]) {
            out.push(image);
        }
        offset = end;
    }

    out
}

fn parse_html_center_tag(html: &str) -> Option<String> {
    let html = html.trim();
    let content = html.strip_prefix("<center>")?.strip_suffix("</center>")?;
    Some(content.trim().to_string())
}

fn extract_html_attr_value(html: &str, attr: &str) -> Option<String> {
    let pattern = format!("{attr}=\"");
    let start = html.find(&pattern)? + pattern.len();
    let rest = &html[start..];
    let end = rest.find('"')?;
    Some(rest[..end].to_string())
}

fn parse_html_img_option(style: &str) -> Option<IncludeGraphicsOption> {
    parse_zoom_scale(style)
        .map(IncludeGraphicsOption::Scale)
        .or_else(|| parse_width_scale(style).map(IncludeGraphicsOption::WidthTextWidth))
}

fn parse_zoom_scale(style: &str) -> Option<f64> {
    let start = style.find("zoom")?;
    let after = &style[start + "zoom".len()..];
    let after = after.strip_prefix(':')?.trim_start();
    let percent = after
        .chars()
        .take_while(|ch| ch.is_ascii_digit() || *ch == '.')
        .collect::<String>();
    let percent: f64 = percent.parse().ok()?;
    Some(percent / 100.0)
}

fn parse_width_scale(style: &str) -> Option<f64> {
    let start = style.find("width")?;
    let after = &style[start + "width".len()..];
    let after = after.strip_prefix(':')?.trim_start();
    let percent = after
        .chars()
        .take_while(|ch| ch.is_ascii_digit() || *ch == '.')
        .collect::<String>();
    let percent: f64 = percent.parse().ok()?;
    Some(percent / 100.0)
}

fn render_inline_code(literal: &str) -> String {
    let delimiter = if !literal.contains('|') {
        '|'
    } else if !literal.contains('`') {
        '`'
    } else if !literal.contains('!') {
        '!'
    } else {
        '|'
    };

    format!("\\lstinline{delimiter}{literal}{delimiter}")
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
            lines.extend(render_item_paragraph(indent, &content));
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

    let mut prev_end_line = first.data.borrow().sourcepos.end.line;
    for block in blocks {
        let start_line = block.data.borrow().sourcepos.start.line;
        let blank_line_before = start_line > prev_end_line + 1;
        prev_end_line = block.data.borrow().sourcepos.end.line;

        match &block.data.borrow().value {
            comrak::nodes::NodeValue::List(list) => {
                if blank_line_before {
                    lines.push(String::new());
                }
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

fn render_item_paragraph(indent: usize, content: &str) -> Vec<String> {
    let mut iter = content.split('\n');
    let first_line = iter.next().unwrap_or("");
    let mut lines = Vec::new();

    if first_line.is_empty() {
        lines.push(indent_line(indent, "\\item"));
    } else {
        lines.push(indent_line(indent, format!("\\item {first_line}")));
    }

    for line in iter {
        if line.is_empty() {
            lines.push(String::new());
        } else {
            lines.push(indent_line(indent + 4, line));
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
