// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

pub mod config;

use std::cell::RefCell;
use std::collections::HashMap;

pub type Error = Box<dyn std::error::Error>;
pub type Result<T> = std::result::Result<T, Error>;

pub use config::Options;

pub fn compile_str(markdown: &str) -> Result<String> {
    compile_str_with_options(markdown, &Options::default())
}

pub fn compile_str_with_options(markdown: &str, options: &Options) -> Result<String> {
    let preprocessed = preprocess_markdown(markdown);
    let arena = comrak::Arena::new();
    let comrak_options = comrak_options();
    let root = comrak::parse_document(&arena, &preprocessed.markdown, &comrak_options);
    let heading_index = options
        .enable_heading_xref
        .then(|| HeadingIndex::new(root, &preprocessed));
    let _runtime_guard = RenderRuntimeGuard::new(RenderRuntime {
        options: options.clone(),
        heading_index,
    });

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

fn comrak_options() -> comrak::Options<'static> {
    let mut options = comrak::Options::default();
    options.extension.alerts = true;
    options.extension.autolink = true;
    options.extension.cjk_friendly_emphasis = true;
    options.extension.highlight = true;
    options.extension.math_dollars = true;
    options.extension.strikethrough = true;
    options.extension.table = true;
    options.parse.ignore_setext = true;
    options
}

struct RenderedBlock {
    sourcepos: Option<comrak::nodes::Sourcepos>,
    text: String,
}

struct HeadingEntry {
    text: String,
    line: usize,
    label: String,
}

struct HeadingIndex {
    entries: Vec<HeadingEntry>,
    labels_by_node: HashMap<usize, String>,
}

impl HeadingIndex {
    fn new<'a>(root: &'a comrak::nodes::AstNode<'a>, preprocessed: &PreprocessedMarkdown) -> Self {
        let mut entries = Vec::new();
        let mut labels_by_node = HashMap::new();
        collect_heading_entries(root, preprocessed, &mut entries, &mut labels_by_node);
        Self {
            entries,
            labels_by_node,
        }
    }

    fn label_for_node<'a>(&self, node: &'a comrak::nodes::AstNode<'a>) -> Option<&str> {
        self.labels_by_node
            .get(&(node as *const _ as usize))
            .map(String::as_str)
    }

    fn resolve_label(&self, target: &str, current_line: usize) -> Option<&str> {
        let mut nearest_above = None;
        let mut first_below = None;
        for entry in self.entries.iter().filter(|entry| entry.text == target) {
            if entry.line <= current_line {
                nearest_above = Some(entry);
            } else if first_below.is_none() {
                first_below = Some(entry);
            }
        }
        nearest_above
            .or(first_below)
            .map(|entry| entry.label.as_str())
    }
}

fn collect_heading_entries<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    preprocessed: &PreprocessedMarkdown,
    entries: &mut Vec<HeadingEntry>,
    labels_by_node: &mut HashMap<usize, String>,
) {
    if matches!(
        node.data.borrow().value,
        comrak::nodes::NodeValue::Heading(_)
    ) {
        let label = format!("sec:{:04}", entries.len() + 1);
        entries.push(HeadingEntry {
            text: collect_heading_text(node, preprocessed),
            line: node_source_line(node),
            label: label.clone(),
        });
        labels_by_node.insert(node as *const _ as usize, label);
    }

    for child in node.children() {
        collect_heading_entries(child, preprocessed, entries, labels_by_node);
    }
}

fn collect_heading_text<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    preprocessed: &PreprocessedMarkdown,
) -> String {
    let mut out = String::new();
    collect_heading_text_to(node, preprocessed, &mut out);
    out
}

fn collect_heading_text_to<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    preprocessed: &PreprocessedMarkdown,
    out: &mut String,
) {
    for child in node.children() {
        match &child.data.borrow().value {
            comrak::nodes::NodeValue::Text(text) => {
                out.push_str(&restore_trailing_space_sentinel(
                    text,
                    preprocessed.trailing_space_sentinel,
                ));
            }
            comrak::nodes::NodeValue::Code(code) => {
                out.push_str(&restore_trailing_space_sentinel(
                    &code.literal,
                    preprocessed.trailing_space_sentinel,
                ));
            }
            comrak::nodes::NodeValue::Math(math) => {
                if math.display_math {
                    out.push_str(&render_display_math(
                        &math.literal,
                        preprocessed.trailing_space_sentinel,
                    ));
                } else {
                    out.push('$');
                    out.push_str(&restore_trailing_space_sentinel(
                        &math.literal,
                        preprocessed.trailing_space_sentinel,
                    ));
                    out.push('$');
                }
            }
            comrak::nodes::NodeValue::SoftBreak | comrak::nodes::NodeValue::LineBreak => {
                out.push('\n')
            }
            _ => collect_heading_text_to(child, preprocessed, out),
        }
    }
}

fn node_source_line<'a>(node: &'a comrak::nodes::AstNode<'a>) -> usize {
    node.data.borrow().sourcepos.start.line
}

struct RenderRuntime {
    options: Options,
    heading_index: Option<HeadingIndex>,
}

thread_local! {
    static RENDER_RUNTIME: RefCell<Option<RenderRuntime>> = const { RefCell::new(None) };
}

struct RenderRuntimeGuard {
    previous: Option<RenderRuntime>,
}

impl RenderRuntimeGuard {
    fn new(runtime: RenderRuntime) -> Self {
        let previous = RENDER_RUNTIME.with(|current| current.replace(Some(runtime)));
        Self { previous }
    }
}

impl Drop for RenderRuntimeGuard {
    fn drop(&mut self) {
        let previous = self.previous.take();
        RENDER_RUNTIME.with(|current| {
            current.replace(previous);
        });
    }
}

fn with_runtime<T>(f: impl FnOnce(Option<&RenderRuntime>) -> T) -> T {
    RENDER_RUNTIME.with(|runtime| f(runtime.borrow().as_ref()))
}

fn force_figure_strict_here() -> bool {
    with_runtime(|runtime| runtime.is_some_and(|runtime| runtime.options.force_figure_strict_here))
}

fn heading_label_for_node<'a>(node: &'a comrak::nodes::AstNode<'a>) -> Option<String> {
    with_runtime(|runtime| {
        runtime
            .filter(|runtime| runtime.options.enable_heading_xref)
            .and_then(|runtime| runtime.heading_index.as_ref())
            .and_then(|index| index.label_for_node(node))
            .map(str::to_string)
    })
}

fn resolve_heading_label(url: &str, source_line: usize) -> Option<String> {
    let target = url.strip_prefix('#')?;
    with_runtime(|runtime| {
        runtime
            .filter(|runtime| runtime.options.enable_heading_xref)
            .and_then(|runtime| runtime.heading_index.as_ref())
            .and_then(|index| index.resolve_label(target, source_line))
            .map(str::to_string)
    })
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
            let label_suffix = heading_label_for_node(node)
                .map(|label| format!("\\label{{{label}}}"))
                .unwrap_or_default();
            let command = match heading.level {
                1 => "chapter",
                2 => "section",
                3 => "subsection",
                4 => "subsubsection",
                _ => return Some(indent_multiline(indent, format!("{content}{label_suffix}"))),
            };
            Some(indent_multiline(
                indent,
                format!("\\{command}{{{content}}}{label_suffix}"),
            ))
        }
        comrak::nodes::NodeValue::BlockQuote => {
            Some(render_block_quote(node, indent, preprocessed))
        }
        comrak::nodes::NodeValue::Alert(alert) => {
            Some(render_alert(node, indent, alert, preprocessed))
        }
        comrak::nodes::NodeValue::CodeBlock(code_block) => Some(render_code_block(
            code_block,
            preprocessed.trailing_space_sentinel,
        )),
        comrak::nodes::NodeValue::HtmlBlock(html) => render_html_block(html, indent, preprocessed),
        comrak::nodes::NodeValue::List(list) => Some(render_list(node, indent, list, preprocessed)),
        comrak::nodes::NodeValue::Table(table) => {
            Some(render_table(node, indent, table, preprocessed))
        }
        comrak::nodes::NodeValue::ThematicBreak => Some(indent_line(indent, "\\bigskip")),
        _ => None,
    }
}

fn render_paragraph<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    indent: usize,
    preprocessed: &PreprocessedMarkdown,
) -> String {
    if let Some(figure) = render_paragraph_as_figure(node, indent, preprocessed) {
        return figure;
    }

    indent_multiline(indent, render_inlines(node, preprocessed))
}

fn render_paragraph_as_figure<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    indent: usize,
    preprocessed: &PreprocessedMarkdown,
) -> Option<String> {
    let mut children = node.children();
    let only = children.next()?;
    if children.next().is_some() {
        return None;
    }

    match &only.data.borrow().value {
        comrak::nodes::NodeValue::Image(image) => {
            let include =
                render_includegraphics(&image.url, None, preprocessed.trailing_space_sentinel);
            Some(render_figure(indent, &include))
        }
        comrak::nodes::NodeValue::HtmlInline(html) => {
            let image = parse_html_img_tag(html)?;
            let include = render_includegraphics(
                &image.src,
                image.option,
                preprocessed.trailing_space_sentinel,
            );
            Some(render_figure(indent, &include))
        }
        _ => None,
    }
}

fn render_inlines<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    preprocessed: &PreprocessedMarkdown,
) -> String {
    render_inlines_with_references(node, preprocessed, true)
}

fn render_inlines_with_references<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    preprocessed: &PreprocessedMarkdown,
    allow_footnotes: bool,
) -> String {
    let mut out = String::new();
    let mut pending_cites = Vec::new();
    let children = node.children().collect::<Vec<_>>();
    let mut index = 0;
    while let Some(child) = children.get(index).copied() {
        match &child.data.borrow().value {
            comrak::nodes::NodeValue::Text(text) => {
                if let Some(literal) = preprocessed.math_block_literal(text) {
                    flush_pending_cites(&mut out, &mut pending_cites);
                    out.push_str(&render_display_math(
                        literal,
                        preprocessed.trailing_space_sentinel,
                    ));
                } else {
                    render_text_with_references(
                        &mut out,
                        text,
                        preprocessed,
                        allow_footnotes,
                        &mut pending_cites,
                    );
                }
            }
            comrak::nodes::NodeValue::Code(code) => {
                flush_pending_cites(&mut out, &mut pending_cites);
                out.push_str(&render_inline_code(
                    &code.literal,
                    preprocessed.trailing_space_sentinel,
                ));
            }
            comrak::nodes::NodeValue::Image(image) => {
                flush_pending_cites(&mut out, &mut pending_cites);
                out.push_str(&render_includegraphics(
                    &image.url,
                    None,
                    preprocessed.trailing_space_sentinel,
                ));
            }
            comrak::nodes::NodeValue::HtmlInline(html) => {
                flush_pending_cites(&mut out, &mut pending_cites);
                if let Some(image) = parse_html_img_tag(html) {
                    let include = render_includegraphics(
                        &image.src,
                        image.option,
                        preprocessed.trailing_space_sentinel,
                    );
                    if inline_node_is_standalone_line(&children, index) {
                        out.push_str(&render_figure(0, &include));
                    } else {
                        out.push_str(&include);
                    }
                } else if let Some(content) = parse_html_center_tag(html) {
                    out.push_str(&render_center(0, &content));
                }
            }
            comrak::nodes::NodeValue::Link(link) => {
                flush_pending_cites(&mut out, &mut pending_cites);
                let url = restore_trailing_space_sentinel(
                    link.url.as_str(),
                    preprocessed.trailing_space_sentinel,
                );
                if url.starts_with('#') {
                    let text = render_inlines_with_references(child, preprocessed, allow_footnotes);
                    if let Some(label) = resolve_heading_label(&url, node_source_line(child)) {
                        out.push_str(&format!("\\hyperref[{label}]{{{text}}}"));
                    } else {
                        out.push_str(&text);
                    }
                } else if link_text_is_exact_url(child, &url, preprocessed.trailing_space_sentinel)
                {
                    let (url, suffix) = split_trailing_url_punctuation(&url);
                    out.push_str(&format!("\\url{{{url}}}{suffix}"));
                } else {
                    let text = render_inlines_with_references(child, preprocessed, allow_footnotes);
                    let url = escape_latex_href_url(&url);
                    out.push_str(&format!("\\href{{{url}}}{{{text}}}"));
                }
            }
            comrak::nodes::NodeValue::Emph => {
                flush_pending_cites(&mut out, &mut pending_cites);
                if let Some(inner) = emph_as_strong_emph(child, preprocessed) {
                    out.push_str(&format!("\\textbf{{\\emph{{{inner}}}}}"));
                } else {
                    let inner =
                        render_inlines_with_references(child, preprocessed, allow_footnotes);
                    out.push_str(&format!("\\emph{{{inner}}}"));
                }
            }
            comrak::nodes::NodeValue::Strong => {
                flush_pending_cites(&mut out, &mut pending_cites);
                let inner = render_inlines_with_references(child, preprocessed, allow_footnotes);
                out.push_str(&format!("\\textbf{{{inner}}}"));
            }
            comrak::nodes::NodeValue::Strikethrough => {
                flush_pending_cites(&mut out, &mut pending_cites);
                let inner = render_inlines_with_references(child, preprocessed, allow_footnotes);
                out.push_str(&format!("\\sout{{{inner}}}"));
            }
            comrak::nodes::NodeValue::Highlight => {
                flush_pending_cites(&mut out, &mut pending_cites);
                let inner = render_inlines_with_references(child, preprocessed, allow_footnotes);
                out.push_str(&format!("\\hl{{{inner}}}"));
            }
            comrak::nodes::NodeValue::Math(math) => {
                flush_pending_cites(&mut out, &mut pending_cites);
                if math.display_math {
                    out.push_str(&render_display_math(
                        &math.literal,
                        preprocessed.trailing_space_sentinel,
                    ));
                } else {
                    // comrak's `$...$` parsing does not understand LaTeX command-group semantics well.
                    // When users nest `$...$` inside a command argument (e.g. `\\text{...}`),
                    // comrak may fragment the intended
                    // output into a `Math(\"...\\\\cmd{\")` node followed by `Text(...)`/`Math(...)`
                    // nodes that still contain `$` delimiters. We stitch these fragments back into
                    // valid LaTeX while being careful not to treat literal dollars (e.g. `\\$`)
                    // as delimiters.
                    if opens_latex_command_group(&math.literal) {
                        if let Some(next) = children.get(index + 1)
                            && let comrak::nodes::NodeValue::Text(text) = &next.data.borrow().value
                            && let Some((inner, outer, suffix)) =
                                split_at_last_two_unescaped_dollars(text)
                        {
                            out.push('$');
                            out.push_str(&restore_trailing_space_sentinel(
                                &math.literal,
                                preprocessed.trailing_space_sentinel,
                            ));
                            out.push('$');
                            push_raw_text(&mut out, inner, preprocessed.trailing_space_sentinel);
                            out.push('$');
                            push_raw_text(&mut out, outer, preprocessed.trailing_space_sentinel);
                            out.push('$');
                            if !suffix.is_empty() {
                                render_text_with_references(
                                    &mut out,
                                    suffix,
                                    preprocessed,
                                    allow_footnotes,
                                    &mut pending_cites,
                                );
                            }
                            index += 2;
                            continue;
                        }

                        if let (Some(next), Some(next_next)) =
                            (children.get(index + 1), children.get(index + 2))
                            && let (
                                comrak::nodes::NodeValue::Math(inner),
                                comrak::nodes::NodeValue::Text(text),
                            ) = (&next.data.borrow().value, &next_next.data.borrow().value)
                            && !inner.display_math
                            && let Some((tail, suffix)) = split_at_last_unescaped_dollar(text)
                        {
                            out.push('$');
                            out.push_str(&restore_trailing_space_sentinel(
                                &math.literal,
                                preprocessed.trailing_space_sentinel,
                            ));
                            out.push('$');
                            out.push_str(&restore_trailing_space_sentinel(
                                &inner.literal,
                                preprocessed.trailing_space_sentinel,
                            ));
                            out.push('$');
                            push_raw_text(&mut out, tail, preprocessed.trailing_space_sentinel);
                            out.push('$');
                            if !suffix.is_empty() {
                                render_text_with_references(
                                    &mut out,
                                    suffix,
                                    preprocessed,
                                    allow_footnotes,
                                    &mut pending_cites,
                                );
                            }
                            index += 3;
                            continue;
                        }
                    }
                    out.push('$');
                    out.push_str(&restore_trailing_space_sentinel(
                        &math.literal,
                        preprocessed.trailing_space_sentinel,
                    ));
                    out.push('$');
                }
            }
            comrak::nodes::NodeValue::SoftBreak => {
                flush_pending_cites(&mut out, &mut pending_cites);
                out.push('\n');
            }
            comrak::nodes::NodeValue::LineBreak => {
                flush_pending_cites(&mut out, &mut pending_cites);
                out.push('\n');
            }
            _ => {}
        }
        index += 1;
    }
    flush_pending_cites(&mut out, &mut pending_cites);
    out
}

fn inline_node_is_standalone_line<'a>(
    siblings: &[&'a comrak::nodes::AstNode<'a>],
    index: usize,
) -> bool {
    let at_line_start = index == 0
        || matches!(
            &siblings[index - 1].data.borrow().value,
            comrak::nodes::NodeValue::SoftBreak | comrak::nodes::NodeValue::LineBreak
        );

    let at_line_end = index + 1 == siblings.len()
        || matches!(
            &siblings[index + 1].data.borrow().value,
            comrak::nodes::NodeValue::SoftBreak | comrak::nodes::NodeValue::LineBreak
        );

    at_line_start && at_line_end
}

fn opens_latex_command_group(literal: &str) -> bool {
    let Some(prefix) = literal.strip_suffix('{') else {
        return false;
    };
    let Some(start) = prefix.rfind('\\') else {
        return false;
    };

    let command = &prefix[start + '\\'.len_utf8()..];
    !command.is_empty()
        && command
            .chars()
            .all(|ch| ch.is_ascii_alphabetic() || ch == '*')
}

fn split_at_last_two_unescaped_dollars(text: &str) -> Option<(&str, &str, &str)> {
    let mut backslashes = 0usize;
    let mut penultimate = None;
    let mut last = None;
    for (index, ch) in text.char_indices() {
        match ch {
            '\\' => backslashes += 1,
            '$' => {
                if backslashes.is_multiple_of(2) {
                    penultimate = last;
                    last = Some(index);
                }
                backslashes = 0;
            }
            _ => backslashes = 0,
        }
    }

    let penultimate = penultimate?;
    let last = last?;
    let before = &text[..penultimate];
    let between = &text[penultimate + '$'.len_utf8()..last];
    let after = &text[last + '$'.len_utf8()..];
    Some((before, between, after))
}

fn split_at_last_unescaped_dollar(text: &str) -> Option<(&str, &str)> {
    let mut backslashes = 0usize;
    let mut last = None;
    for (index, ch) in text.char_indices() {
        match ch {
            '\\' => backslashes += 1,
            '$' => {
                if backslashes.is_multiple_of(2) {
                    last = Some(index);
                }
                backslashes = 0;
            }
            _ => backslashes = 0,
        }
    }

    let pos = last?;
    Some((&text[..pos], &text[pos + '$'.len_utf8()..]))
}

fn render_text_with_references(
    out: &mut String,
    text: &str,
    preprocessed: &PreprocessedMarkdown,
    allow_footnotes: bool,
    pending_cites: &mut Vec<String>,
) {
    let mut rest = text;
    while let Some(start) = rest.find("[^") {
        let (before, after_start) = rest.split_at(start);
        if !before.is_empty() {
            flush_pending_cites(out, pending_cites);
            push_text(out, before, preprocessed.trailing_space_sentinel);
        }

        let Some(end) = after_start.find(']') else {
            break;
        };
        let name = &after_start[2..end];
        if name.is_empty() {
            break;
        }

        let after = &after_start[end + 1..];
        let is_footnote = allow_footnotes && preprocessed.footnote_definition(name).is_some();
        if is_footnote {
            flush_pending_cites(out, pending_cites);
            out.push_str(&render_footnote(name, preprocessed));
        } else {
            pending_cites.push(name.to_string());
        }

        rest = after;
    }

    if !rest.is_empty() {
        flush_pending_cites(out, pending_cites);
        push_text(out, rest, preprocessed.trailing_space_sentinel);
    }
}

fn flush_pending_cites(out: &mut String, pending: &mut Vec<String>) {
    if pending.is_empty() {
        return;
    }

    out.push_str("\\cite{");
    out.push_str(&pending.join(","));
    out.push('}');
    pending.clear();
}

fn render_footnote(name: &str, preprocessed: &PreprocessedMarkdown) -> String {
    let Some(markdown) = preprocessed.footnote_definition(name) else {
        return format!("\\cite{{{name}}}");
    };

    let content = render_inline_fragment(markdown);
    format!("\\footnote{{{content}}}")
}

fn render_inline_fragment(markdown: &str) -> String {
    let preprocessed = preprocess_markdown(markdown);
    let arena = comrak::Arena::new();
    let options = comrak_options();
    let root = comrak::parse_document(&arena, &preprocessed.markdown, &options);

    let mut parts = Vec::new();
    for node in root.children() {
        if matches!(
            node.data.borrow().value,
            comrak::nodes::NodeValue::Paragraph
        ) {
            parts.push(render_inlines_with_references(node, &preprocessed, false));
        } else if let Some(block) = render_block(node, 0, &preprocessed) {
            parts.push(block);
        }
    }
    parts.join("\n\n")
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
                    out.push_str(&render_display_math(
                        literal,
                        preprocessed.trailing_space_sentinel,
                    ));
                } else {
                    out.push_str(&escape_latex_text_restore_trailing_space_sentinel(
                        text,
                        preprocessed.trailing_space_sentinel,
                    ));
                }
            }
            comrak::nodes::NodeValue::Code(code) => {
                out.push_str(&render_inline_code(
                    &code.literal,
                    preprocessed.trailing_space_sentinel,
                ));
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
                    out.push_str(&render_display_math(
                        &math.literal,
                        preprocessed.trailing_space_sentinel,
                    ));
                } else {
                    out.push('$');
                    out.push_str(&restore_trailing_space_sentinel(
                        &math.literal,
                        preprocessed.trailing_space_sentinel,
                    ));
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

fn restore_trailing_space_sentinel(text: &str, trailing_space_sentinel: char) -> String {
    let mut out = String::with_capacity(text.len());
    for ch in text.chars() {
        if ch == trailing_space_sentinel {
            out.push(' ');
        } else {
            out.push(ch);
        }
    }
    out
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
    footnotes: std::collections::HashMap<String, String>,
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

    fn footnote_definition(&self, name: &str) -> Option<&str> {
        self.footnotes.get(name).map(String::as_str)
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
    let mut footnotes = std::collections::HashMap::new();

    let mut lines = markdown.split_inclusive('\n').peekable();
    while let Some(line) = lines.next() {
        let (content, newline) = match line.strip_suffix('\n') {
            Some(content) => (content, "\n"),
            None => (line, ""),
        };

        if let Some((name, definition)) = parse_footnote_definition_line(content) {
            footnotes.insert(name, definition);
            if !newline.is_empty() {
                out.push_str(newline);
            }
            continue;
        }

        if content == "$$" {
            let mut literal = String::new();
            for next in lines.by_ref() {
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

        let content = normalize_heading_anchor_links(content, trailing_space_sentinel);
        if let Some(encoded) =
            encode_space_before_trailing_closing_strong(&content, trailing_space_sentinel)
        {
            out.push_str(&encoded);
            out.push_str(newline);
        } else {
            out.push_str(&content);
            out.push_str(newline);
        }
    }

    PreprocessedMarkdown {
        markdown: out,
        math_blocks,
        footnotes,
        math_block_placeholder_prefix,
        trailing_space_sentinel,
    }
}

fn normalize_heading_anchor_links(text: &str, trailing_space_sentinel: char) -> String {
    let mut out = String::with_capacity(text.len());
    let mut cursor = 0;

    while let Some(rel_open) = text[cursor..].find('[') {
        let open = cursor + rel_open;
        out.push_str(&text[cursor..open]);
        if is_escaped_at(text, open) {
            out.push('[');
            cursor = open + '['.len_utf8();
            continue;
        }

        let Some(close) = find_unescaped_char(text, open + '['.len_utf8(), ']') else {
            out.push_str(&text[open..]);
            return out;
        };
        let after_close = close + ']'.len_utf8();
        if !text[after_close..].starts_with('(') {
            out.push('[');
            cursor = open + '['.len_utf8();
            continue;
        }

        let dest_start = after_close + '('.len_utf8();
        let Some(dest_end) = find_link_destination_end(text, dest_start) else {
            out.push('[');
            cursor = open + '['.len_utf8();
            continue;
        };

        let dest = &text[dest_start..dest_end];
        out.push_str(&text[open..dest_start]);
        if dest.starts_with('#') && dest.chars().any(char::is_whitespace) {
            out.push('#');
            for ch in dest['#'.len_utf8()..].chars() {
                if ch.is_whitespace() {
                    out.push(trailing_space_sentinel);
                } else {
                    out.push(ch);
                }
            }
        } else {
            out.push_str(dest);
        }
        out.push(')');
        cursor = dest_end + ')'.len_utf8();
    }

    out.push_str(&text[cursor..]);
    out
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

fn parse_footnote_definition_line(line: &str) -> Option<(String, String)> {
    let rest = line.strip_prefix("[^")?;
    let (name, definition) = rest.split_once("]: ")?;
    if name.is_empty() {
        return None;
    }
    Some((name.to_string(), definition.to_string()))
}

fn strip_disabled_header_anchor_links(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut cursor = 0;
    while let Some(rel_open) = text[cursor..].find('[') {
        let open = cursor + rel_open;
        out.push_str(&text[cursor..open]);
        if is_escaped_at(text, open) {
            out.push('[');
            cursor = open + '['.len_utf8();
            continue;
        }

        let Some(close) = find_unescaped_char(text, open + '['.len_utf8(), ']') else {
            out.push_str(&text[open..]);
            cursor = text.len();
            break;
        };
        let label = &text[open + '['.len_utf8()..close];
        let after_close = close + ']'.len_utf8();
        if text[after_close..].starts_with('(') {
            let dest_start = after_close + '('.len_utf8();
            if let Some(dest_end) = find_link_destination_end(text, dest_start) {
                let dest = &text[dest_start..dest_end];
                if dest.starts_with('#') && dest.chars().any(|ch| ch.is_whitespace()) {
                    out.push_str(label);
                } else {
                    out.push_str(&text[open..dest_end + ')'.len_utf8()]);
                }
                cursor = dest_end + ')'.len_utf8();
                continue;
            }
        }

        out.push('[');
        cursor = open + '['.len_utf8();
    }
    out.push_str(&text[cursor..]);
    out
}

fn is_escaped_at(text: &str, index: usize) -> bool {
    let mut backslashes = 0usize;
    for ch in text[..index].chars().rev() {
        if ch == '\\' {
            backslashes += 1;
        } else {
            break;
        }
    }
    backslashes % 2 == 1
}

fn find_unescaped_char(text: &str, start: usize, target: char) -> Option<usize> {
    let mut backslashes = 0usize;
    for (offset, ch) in text[start..].char_indices() {
        match ch {
            '\\' => backslashes += 1,
            _ => {
                if ch == target && backslashes.is_multiple_of(2) {
                    return Some(start + offset);
                }
                backslashes = 0;
            }
        }
    }
    None
}

fn find_link_destination_end(text: &str, start: usize) -> Option<usize> {
    let mut depth = 0usize;
    let mut backslashes = 0usize;
    for (offset, ch) in text[start..].char_indices() {
        match ch {
            '\\' => backslashes += 1,
            '(' if backslashes.is_multiple_of(2) => {
                depth += 1;
                backslashes = 0;
            }
            ')' if backslashes.is_multiple_of(2) => {
                if depth == 0 {
                    return Some(start + offset);
                }
                depth -= 1;
                backslashes = 0;
            }
            _ => backslashes = 0,
        }
    }
    None
}
fn push_raw_text(out: &mut String, text: &str, trailing_space_sentinel: char) {
    for ch in text.chars() {
        if ch == trailing_space_sentinel {
            out.push(' ');
        } else if ch == '$' {
            out.push_str("\\$");
        } else {
            out.push(ch);
        }
    }
}

fn push_text(out: &mut String, text: &str, trailing_space_sentinel: char) {
    let text = strip_disabled_header_anchor_links(text);
    out.push_str(&escape_latex_text_restore_trailing_space_sentinel(
        &text,
        trailing_space_sentinel,
    ));
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

fn render_display_math(literal: &str, trailing_space_sentinel: char) -> String {
    let literal = restore_trailing_space_sentinel(literal, trailing_space_sentinel);
    let literal = literal.strip_prefix('\n').unwrap_or(&literal);
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

fn render_code_block(
    code_block: &comrak::nodes::NodeCodeBlock,
    trailing_space_sentinel: char,
) -> String {
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

    let literal = restore_trailing_space_sentinel(
        code_block.literal.trim_end_matches('\n'),
        trailing_space_sentinel,
    );
    out.push_str(&literal);
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
    lines.push(indent_line(indent, format!("\\begin{{tabular}}{{{spec}}}")));
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

fn split_trailing_url_punctuation(url: &str) -> (String, String) {
    let mut end = url.len();
    while end > 0 {
        let last = url[..end].chars().last().unwrap();
        let should_strip = matches!(
            last,
            '.' | ',' | ':' | ';' | '!' | '?' | '。' | '，' | '：' | '；' | '！' | '？'
        );
        if !should_strip {
            break;
        }
        end -= last.len_utf8();
    }

    let (base, suffix) = url.split_at(end);
    (base.to_string(), suffix.to_string())
}

fn link_text_is_exact_url<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    url: &str,
    trailing_space_sentinel: char,
) -> bool {
    let mut children = node.children();
    let Some(only) = children.next() else {
        return false;
    };
    if children.next().is_some() {
        return false;
    }

    matches!(
        &only.data.borrow().value,
        comrak::nodes::NodeValue::Text(text)
            if restore_trailing_space_sentinel(text, trailing_space_sentinel) == url
    )
}

fn escape_latex_href_url(url: &str) -> String {
    // TeX scans macro arguments before hyperref/url packages can sanitize them,
    // so we must escape characters that would otherwise break argument parsing.
    let mut out = String::new();
    for ch in url.chars() {
        match ch {
            '_' => out.push_str("\\_"),
            '&' => out.push_str("\\&"),
            '#' => out.push_str("\\#"),
            '%' => out.push_str("\\%"),
            _ => out.push(ch),
        }
    }
    out
}

fn render_html_block(
    html: &comrak::nodes::NodeHtmlBlock,
    indent: usize,
    preprocessed: &PreprocessedMarkdown,
) -> Option<String> {
    let images = parse_html_img_tags(&html.literal);
    if !images.is_empty() {
        let figures = images
            .into_iter()
            .map(|image| {
                let include = render_includegraphics(
                    &image.src,
                    image.option,
                    preprocessed.trailing_space_sentinel,
                );
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

fn render_includegraphics(
    url: &str,
    option: Option<IncludeGraphicsOption>,
    trailing_space_sentinel: char,
) -> String {
    let url = restore_trailing_space_sentinel(url, trailing_space_sentinel);
    let url = escape_latex_text(&url);
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
    if force_figure_strict_here() {
        lines.push(indent_line(indent, "\\begin{figure}[H]"));
    } else {
        lines.push(indent_line(indent, "\\begin{figure}"));
    }
    lines.push(indent_line(indent + 4, "\\centering"));
    lines.push(indent_line(indent + 4, includegraphics));
    lines.push(indent_line(indent, "\\end{figure}"));
    lines.join("\n")
}

fn render_center(indent: usize, content: &str) -> String {
    let lines = [
        indent_line(indent, "\\begin{center}"),
        indent_line(indent + 4, content),
        indent_line(indent, "\\end{center}"),
    ];
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

fn render_inline_code(literal: &str, trailing_space_sentinel: char) -> String {
    let literal = restore_trailing_space_sentinel(literal, trailing_space_sentinel);
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

fn render_alert<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    indent: usize,
    alert: &comrak::nodes::NodeAlert,
    preprocessed: &PreprocessedMarkdown,
) -> String {
    let kind = match alert.alert_type {
        comrak::nodes::AlertType::Note => "NOTE",
        comrak::nodes::AlertType::Tip => "TIP",
        comrak::nodes::AlertType::Important => "IMPORTANT",
        comrak::nodes::AlertType::Warning => "WARNING",
        comrak::nodes::AlertType::Caution => "CAUTION",
    };

    let content_indent = indent + 4;
    let mut blocks = Vec::new();
    for child in node.children() {
        if let Some(block) = render_block(child, content_indent, preprocessed) {
            blocks.push(block);
        }
    }

    let mut lines = Vec::new();
    lines.push(indent_line(indent, format!("\\begin{{alerts}}{{{kind}}}")));
    if !blocks.is_empty() {
        lines.extend(blocks.join("\n\n").split('\n').map(|l| l.to_string()));
    }
    lines.push(indent_line(indent, "\\end{alerts}"));
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
            let block_indent = if aligns_with_item_indent(first) {
                indent
            } else {
                indent + 4
            };
            if let Some(block) = render_block(first, block_indent, preprocessed) {
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
                let block_indent = if aligns_with_item_indent(block) {
                    indent
                } else {
                    indent + 4
                };
                if let Some(rendered) = render_block(block, block_indent, preprocessed) {
                    lines.extend(rendered.split('\n').map(|l| l.to_string()));
                }
            }
        }
    }

    lines
}

fn aligns_with_item_indent<'a>(node: &'a comrak::nodes::AstNode<'a>) -> bool {
    matches!(
        &node.data.borrow().value,
        comrak::nodes::NodeValue::Alert(_)
            | comrak::nodes::NodeValue::BlockQuote
            | comrak::nodes::NodeValue::List(_)
    )
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
