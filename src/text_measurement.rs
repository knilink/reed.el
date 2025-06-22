use taffy::prelude::{AvailableSpace, Size};

// Find the nearest valid split point around a given position
fn find_split_point(text: &[char], target_pos: usize) -> usize {
    let len = text.len();

    if target_pos >= len {
        return len;
    }

    // If target position is at a space, use it
    if text[target_pos] == ' ' {
        return target_pos;
    }

    // Look for nearest space before and after target position
    let mut before = None;
    let mut after = None;

    // Search backwards for space
    for i in (0..target_pos).rev() {
        if text[i] == ' ' {
            before = Some(i);
            break;
        }
    }

    // Search forwards for space
    for i in (target_pos + 1)..len {
        if text[i] == ' ' {
            after = Some(i);
            break;
        }
    }

    // Choose the closest one to target_pos
    match (before, after) {
        (Some(b), Some(a)) => {
            if target_pos - b <= a - target_pos {
                b
            } else {
                a
            }
        }
        (Some(b), None) => b,
        (None, Some(a)) => a,
        (None, None) => target_pos, // No spaces found, split at target
    }
}

fn min_width(text: &[char], lines: usize) -> usize {
    fn trimed_length(text: &[char]) -> usize {
        let start = text
            .iter()
            .position(|c| !c.is_whitespace())
            .unwrap_or(text.len());
        let end = text.iter().rposition(|c| !c.is_whitespace()).unwrap_or(0);
        return if start > end { 0 } else { end + 1 - start };
    }

    let mut current_text = text;
    let mut remaining_lines = lines;
    let mut max_width = 0;

    while remaining_lines > 1 {
        let split_pos = find_split_point(current_text, current_text.len() / remaining_lines);

        let (head, tail) = current_text.split_at(split_pos);

        let head_width = trimed_length(head);
        max_width = max_width.max(head_width);

        current_text = tail;
        remaining_lines -= 1;
    }

    // Handle the last line
    max_width.max(trimed_length(current_text))
}

pub fn min_width_multiline(texts: &[char], max_height: usize) -> usize {
    if texts.is_empty() {
        return 0;
    }

    // Initialize each text with height=1, width=text.len()
    let lines: Vec<&[char]> = texts.split(|&c| c == '\n').map(|text| text).collect();
    let mut splits = vec![1; lines.len()];

    let mut max_i = 0;
    let max_height = max_height.max(lines.len());
    for _ in 0..(max_height - lines.len()) {
        for i in 0..lines.len() {
            if lines[i].len() * splits[max_i] > lines[max_i].len() * splits[i] {
                max_i = i;
            }
        }
        splits[max_i] += 1;
    }
    max_i = 0;
    for i in 0..lines.len() {
        if lines[i].len() * splits[max_i] > lines[max_i].len() * splits[i] {
            max_i = i;
        }
    }

    // Return the maximum width among all lines
    min_width(lines[max_i], splits[max_i])
}

pub fn measure_text_block(
    _known_dimensions: taffy::geometry::Size<Option<f32>>,
    available_space: taffy::geometry::Size<AvailableSpace>,
    content: &str,
) -> taffy::geometry::Size<f32> {
    // Handle width calculation
    if content.is_empty() {
        return Size::ZERO;
    }

    match (available_space.width, available_space.height) {
        (AvailableSpace::MaxContent, AvailableSpace::MaxContent)
        | (AvailableSpace::MinContent, AvailableSpace::MinContent)
        | (AvailableSpace::MaxContent, AvailableSpace::MinContent) => {
            let lines = content.split('\n');
            let mut height = 0usize;
            let width = lines
                .map(|l| {
                    height += 1;
                    l.chars().count()
                })
                .max()
                .unwrap_or(0);
            Size {
                width: width as f32,
                height: height as f32,
            }
        }
        (AvailableSpace::MinContent, AvailableSpace::MaxContent) => Size {
            width: 1.0,
            height: content.chars().filter(|&c| c != '\n').count() as f32,
        },
        (
            AvailableSpace::MinContent | AvailableSpace::MaxContent,
            AvailableSpace::Definite(height),
        ) => {
            let content_vec = content.chars().collect::<Vec<_>>();
            let width = min_width_multiline(&content_vec, height as usize);
            let wrapped = textwrap::wrap(content, width);
            let height = wrapped.len();
            let width = wrapped
                .iter()
                .map(|line| line.chars().count())
                .max()
                .unwrap_or(0);
            Size {
                width: width as f32,
                height: height as f32,
            }
        }
        (
            AvailableSpace::Definite(width),
            AvailableSpace::MinContent | AvailableSpace::MaxContent,
        ) => {
            let wrapped = textwrap::wrap(content, width as usize);
            let height = wrapped.len();
            let width = wrapped
                .iter()
                .map(|line| line.chars().count())
                .max()
                .unwrap_or(0);
            Size {
                width: width as f32,
                height: height as f32,
            }
        }
        (AvailableSpace::Definite(width), AvailableSpace::Definite(height)) => {
            Size { width, height }
        }
    }
}
