use std::{
    env,
    fs::File,
    io::{BufRead, BufReader, BufWriter, Write},
    path::{Path, PathBuf},
};

use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

const METHODS: [(&str, &str, usize, bool); 28] = [
    ("Assert.AreEqual(", "Is.EqualTo", 2, false),
    ("Assert.AreSame(", "Is.SameAs", 2, false),
    ("Assert.IsFalse(", "Is.False", 1, false),
    ("Assert.IsNull(", "Is.Null", 1, false),
    ("Assert.IsNotNull(", "Is.Not.Null", 1, false),
    ("Assert.IsTrue(", "Is.True", 1, false),
    ("Assert.AreNotEqual(", "Is.Not.EqualTo", 2, false),
    ("Assert.AreNotSame(", "Is.Not.SameAs", 2, false),
    ("Assert.Greater(", "Is.GreaterThan", 2, true),
    ("Assert.GreaterOrEqual(", "Is.GreaterThanOrEqualTo", 2, true),
    ("Assert.Less(", "Is.LessThan", 2, true),
    ("Assert.LessOrEqual(", "Is.LessThanOrEqualTo", 2, true),
    ("Assert.Contains(", "Does.Contain", 2, false),
    ("Assert.IsEmpty(", "Is.Empty", 1, false),
    ("Assert.IsNotEmpty(", "Is.Not.Empty", 1, false),
    ("Assert.IsNaN(", "Is.NaN", 1, false),
    ("Assert.IsNegative(", "Is.Negative", 1, false),
    ("Assert.IsPositive(", "Is.Positive", 1, false),
    ("Assert.Zero(", "Is.Zero", 1, false),
    ("Assert.NotZero(", "Is.Not.Zero", 1, false),
    ("Assert.IsInstanceOf(", "Is.InstanceOf", 2, false),
    ("Assert.IsInstanceOf<", "Is.InstanceOf", 1, false),
    ("Assert.IsNotInstanceOf(", "Is.Not.InstanceOf", 2, false),
    ("Assert.IsNotInstanceOf<", "Is.Not.InstanceOf", 1, false),
    ("Assert.IsAssignableFrom(", "Is.AssignableFrom", 2, false),
    ("Assert.IsAssignableFrom<", "Is.AssignableFrom", 1, false),
    (
        "Assert.IsNotAssignableFrom(",
        "Is.Not.AssignableFrom",
        2,
        false,
    ),
    (
        "Assert.IsNotAssignableFrom<",
        "Is.Not.AssignableFrom",
        1,
        false,
    ),
];

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = env::args().collect::<Vec<_>>();
    let path_str = &args[1];
    let cs_paths = get_cs_files_in_folder(path_str);

    cs_paths.par_iter().for_each(|path| {
        file_replacement(path).unwrap();
    });

    Ok(())
}

pub fn file_replacement(path: &PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    let file = File::open(path.clone())?;
    let mut file = BufReader::new(file);
    let mut line = String::new();

    let file_name = path.file_name().unwrap().to_str().unwrap();
    let tmp_name = format!("{}.tmp", file_name);
    let parent = PathBuf::from(path.clone().parent().unwrap());
    let mut tmp_path = parent.into_os_string();
    tmp_path.push(tmp_name);

    let output_file = File::create(tmp_path.clone())?;
    let mut output_file = BufWriter::new(output_file);

    while let Ok(len) = file.read_line(&mut line) {
        if len == 0 {
            break;
        }
        if line == "\n" {
            continue;
        }
        let mut new_line = String::new();
        for (method, new_method, param_len, preserve_order) in &METHODS {
            if let Some(result) =
                replacement(&line, method, new_method, *param_len, *preserve_order)
            {
                new_line = result;
                break;
            }
        }

        let new_line = if new_line.len() == 0 {
            line.as_str()
        } else {
            new_line.as_str()
        };

        output_file.write_all(format!("{}", new_line).as_bytes())?;
        line.clear();
    }
    output_file.flush()?;

    std::fs::remove_file(path.clone())?;
    std::fs::rename(tmp_path, path)?;
    Ok(())
}

pub fn replacement(
    line: &str,
    method: &str,
    new_method: &str,
    param_len: usize,
    preserve_order: bool,
) -> Option<String> {
    let mut parameters = vec![];
    if let Some(start_index) = line.find(method) {
        let start = start_index + method.len() - 1;
        let chars = line.chars().collect::<Vec<_>>();

        let mut i = start;
        let mut parenthesis_depth = 0;
        let mut parameter = String::new();
        let mut generic = String::new();
        let mut in_angle_brackets = false;

        for next_char in &chars[i..] {
            match *next_char {
                '(' => {
                    if parenthesis_depth > 0 {
                        parameter.push(*next_char);
                    }
                    parenthesis_depth += 1;
                }
                ')' => {
                    if parenthesis_depth == 1 {
                        parameters.push(parameter.clone());
                        parameter.clear();
                    } else {
                        parameter.push(*next_char);
                        parenthesis_depth -= 1;
                    }
                }
                ',' => {
                    if parenthesis_depth == 1 {
                        parameters.push(parameter.clone());
                        parameter.clear();
                        if chars[i + 1].is_ascii_whitespace() {
                            i += 1;
                        }
                    } else {
                        parameter.push(*next_char);
                    }
                }
                ';' => {
                    break;
                }
                '<' => {
                    in_angle_brackets = true;
                }
                '>' => {
                    in_angle_brackets = false;
                }
                _ => {
                    if in_angle_brackets {
                        generic.push(*next_char);
                    } else {
                        parameter.push(*next_char);
                    }
                }
            }

            i += 1;
        }

        let new_method = if generic.len() > 0 {
            &format!("{}<{}>()", new_method, generic)
        } else {
            new_method
        };

        parameters
            .iter_mut()
            .for_each(|param| *param = String::from(param.trim()));
        if param_len > 1 {
            if !preserve_order {
                parameters.swap(0, 1);
            }
            parameters[1] = format!("{}({})", new_method, parameters[1]);
        } else {
            parameters.insert(1, new_method.to_string());
        }

        let end = if i == line.len() - 1 {
            line[i..].to_string()
        } else {
            line[i + 1..].to_string()
        };

        let result = format!(
            "{}Assert.That({});{}",
            line[..start_index].to_string(),
            parameters.join(", "),
            end
        );
        Some(result)
    } else {
        None
    }
}

pub fn get_cs_files_in_folder(path: &str) -> Vec<PathBuf> {
    get_cs_files_in_folder_recursive(PathBuf::from(path))
}

pub fn get_cs_files_in_folder_recursive(path: PathBuf) -> Vec<PathBuf> {
    let mut paths = vec![];

    let name_block_list = vec!["packages", ".git", ".vs", "bin", "obj"];

    if let Ok(read_dir) = std::fs::read_dir(path.clone()) {
        for entry_result in read_dir {
            match entry_result {
                Ok(entry) => {
                    let file_type = entry.file_type().unwrap();

                    let mut is_blocked = false;
                    for blocked in &name_block_list {
                        if entry.file_name().eq_ignore_ascii_case(blocked) {
                            is_blocked = true;
                            break;
                        }
                    }
                    if is_blocked {
                        continue;
                    }

                    if file_type.is_file() {
                        if let Some(extension) = Path::new(&entry.file_name()).extension() {
                            if extension == "cs" {
                                paths.push(entry.path());
                            }
                        }
                    } else if file_type.is_dir() {
                        paths.extend(get_cs_files_in_folder_recursive(entry.path()));
                    }
                }
                Err(_) => (),
            }
        }
    }

    paths
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn are_equal() {
        let contents = r#"             Assert.AreEqual(actual, expected);
"#;

        let result = test_replacement(0, contents);
        assert_eq!(
            result,
            r#"             Assert.That(expected, Is.EqualTo(actual));
"#
        );
    }

    #[test]
    fn are_equal_method() {
        let contents = r#"             Assert.AreEqual(actual.Id, GetMethod_Clone(_objname.TypeId.Value, _typeId));
"#;

        let result = test_replacement(0, contents);
        assert_eq!(
            result,
            r#"             Assert.That(GetMethod_Clone(_objname.TypeId.Value, _typeId), Is.EqualTo(actual.Id));
"#
        );
    }

    #[test]
    fn are_equal_method_with_message() {
        let contents = r#"             Assert.AreEqual(actual.Id, GetMethod_Clone(_objname.TypeId.Value, _typeId), "Explanation");
"#;

        let result = test_replacement(0, contents);
        assert_eq!(
            result,
            r#"             Assert.That(GetMethod_Clone(_objname.TypeId.Value, _typeId), Is.EqualTo(actual.Id), "Explanation");
"#
        );
    }

    #[test]
    fn instance_of_type_of() {
        let contents = r#"             Assert.IsInstanceOf(typeof(Exception), new Exception(), "");
"#;

        let result = test_replacement(20, contents);
        assert_eq!(
            result,
            r#"             Assert.That(new Exception(), Is.InstanceOf(typeof(Exception)), "");
"#
        );
    }

    #[test]
    fn instance_of_generic() {
        let contents = r#"             Assert.IsInstanceOf<Exception>(new Exception(), "");
"#;

        let result = test_replacement(21, contents);
        assert_eq!(
            result,
            r#"             Assert.That(new Exception(), Is.InstanceOf<Exception>(), "");
"#
        );
    }

    #[test]
    fn assignable_from_type_of() {
        let contents = r#"             Assert.IsAssignableFrom(typeof(Exception), new Exception(), "");
"#;

        let result = test_replacement(22, contents);
        assert_eq!(
            result,
            r#"             Assert.That(new Exception(), Is.AssignableFrom(typeof(Exception)), "");
"#
        );
    }

    #[test]
    fn assignable_from_generic() {
        let contents = r#"             Assert.IsAssignableFrom<Exception>(new Exception(), "");
"#;

        let result = test_replacement(23, contents);
        assert_eq!(
            result,
            r#"             Assert.That(new Exception(), Is.AssignableFrom<Exception>(), "");
"#
        );
    }

    fn test_replacement(key: usize, contents: &str) -> String {
        let (method, new_method, param_len, preserve_order) = METHODS[key];
        replacement(contents, method, new_method, param_len, preserve_order).unwrap()
    }
}
