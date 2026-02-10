// Copyright (c) UnnamedOrange. Licensed under the MIT License.
// See the LICENSE file in the repository root for full license text.

pub type Error = Box<dyn std::error::Error>;
pub type Result<T> = std::result::Result<T, Error>;

pub fn compile_str(markdown: &str) -> Result<String> {
    let arena = comrak::Arena::new();
    let options = comrak::Options::default();
    let root = comrak::parse_document(&arena, markdown, &options);

    let mut blocks = Vec::new();
    for node in root.children() {
        if let Some(block) = render_block(node) {
            blocks.push(block);
        }
    }

    if blocks.is_empty() {
        return Ok(String::new());
    }

    Ok(format!("{}\n", blocks.join("\n\n")))
}

fn render_block<'a>(node: &'a comrak::nodes::AstNode<'a>) -> Option<String> {
    match &node.data.borrow().value {
        comrak::nodes::NodeValue::Paragraph => Some(render_inlines(node)),
        comrak::nodes::NodeValue::Heading(heading) => {
            let content = render_inlines(node);
            let command = match heading.level {
                1 => "chapter",
                2 => "section",
                3 => "subsection",
                4 => "subsubsection",
                _ => return Some(content),
            };
            Some(format!("\\{command}{{{content}}}"))
        }
        _ => None,
    }
}

fn render_inlines<'a>(node: &'a comrak::nodes::AstNode<'a>) -> String {
    let mut out = String::new();
    for child in node.children() {
        match &child.data.borrow().value {
            comrak::nodes::NodeValue::Text(text) => out.push_str(text),
            comrak::nodes::NodeValue::Code(code) => {
                out.push_str(&code.literal);
            }
            comrak::nodes::NodeValue::SoftBreak => out.push('\n'),
            comrak::nodes::NodeValue::LineBreak => out.push('\n'),
            _ => {}
        }
    }
    out
}
