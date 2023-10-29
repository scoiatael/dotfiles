extern crate regex;

use regex::Regex;

// TODO: Optimize to write to output buffer instead of returning result
fn capitalize(word: &str) -> String {
    if word.len() == 0 {
        return String::new();
    }
    let mut output = String::with_capacity(word.len());
    let mut chars = word.chars();
    output.push(chars.next().unwrap().to_uppercase().nth(0).unwrap());
    output.push_str(&lowercase_chs(chars));

    output
}

fn lowercase_chs(chars: std::str::Chars) -> String {
    chars.map(|ch| ch.to_lowercase().nth(0).unwrap()).collect()
}

fn lowercase(word: &str) -> String {
    lowercase_chs(word.chars())
}

fn uppercase(word: &str) -> String {
    word.chars()
        .map(|ch| ch.to_uppercase().nth(0).unwrap())
        .collect()
}

// TODO: Optimize to use linear loop instead of regex
pub fn words(sentence: &str) -> Vec<&str> {
    let re = Regex::new(r"([A-Z]*[a-z\d]*)").unwrap();
    re.captures_iter(sentence)
        .map(|capture| capture.get(1))
        .filter(|maybe_word| maybe_word.is_some())
        .map(|some_word| some_word.unwrap())
        .map(|word| word.as_str())
        .collect()
}

#[cfg(test)]
mod words_tests {
    use super::*;

    #[test]
    fn test_words() {
        assert_eq!(words("ENV_VAR"), vec!["ENV", "VAR"]);
        assert_eq!(words("EnvVar"), vec!["Env", "Var"]);
        assert_eq!(words("envVar"), vec!["env", "Var"]);
    }
}

pub fn camelize(sentence: &str) -> String {
    let words = words(sentence);
    let mut output = String::with_capacity(sentence.len());
    words.iter().map(|word| capitalize(word)).for_each(|word| {
        output.push_str(&word)
    });

    output
}

#[cfg(test)]
mod camelize_tests {
    use super::*;

    #[test]
    fn test_camelize() {
        assert_eq!(camelize("ENV_VAR"), "EnvVar");
        assert_eq!(camelize("ENV-VAR"), "EnvVar");
    }

    #[test]
    fn test_camelize_is_idempotent() {
        assert_eq!(camelize("EnvVar"), "EnvVar");
    }
}

pub fn underscore(sentence: &str) -> String {
    let words = words(sentence);
    let mut output = String::with_capacity(sentence.len() + words.len());
    words
        .iter()
        .map(|word| lowercase(word))
        .enumerate()
        .for_each(|(idx, word)| {
            if idx != 0 {
                output.push('_')
            }
            output.push_str(&word)
        });

    output
}

#[cfg(test)]
mod underscore_tests {
    use super::*;

    #[test]
    fn test_underscore() {
        assert_eq!(underscore("ENV_VAR"), "env_var");
        assert_eq!(underscore("ENV-VAR"), "env_var");
        assert_eq!(underscore("EnvVar"), "env_var");
    }

    #[test]
    fn test_underscore_is_idempotent() {
        assert_eq!(underscore("env_var"), "env_var");
    }
}

pub fn dasherize(sentence: &str) -> String {
    let words = words(sentence);
    let mut output = String::with_capacity(sentence.len() + words.len());
    words
        .iter()
        .map(|word| lowercase(word))
        .enumerate()
        .for_each(|(idx, word)| {
            if idx != 0 {
                output.push('-')
            }
            output.push_str(&word)
        });

    output
}

#[cfg(test)]
mod dasherize_tests {
    use super::*;

    #[test]
    fn test_dasherize() {
        assert_eq!(dasherize("ENV_VAR"), "env-var");
        assert_eq!(dasherize("ENV-VAR"), "env-var");
        assert_eq!(dasherize("EnvVar"), "env-var");
    }

    #[test]
    fn test_dasherize_is_idempotent() {
        assert_eq!(dasherize("env-var"), "env-var");
    }
}

pub fn env_varize(sentence: &str) -> String {
    let words = words(sentence);
    let mut output = String::with_capacity(sentence.len() + words.len());
    words
        .iter()
        .map(|word| uppercase(word))
        .enumerate()
        .for_each(|(idx, word)| {
            if idx != 0 {
                output.push('_')
            }
            output.push_str(&word)
        });

    output
}

#[cfg(test)]
mod env_varize_tests {
    use super::*;

    #[test]
    fn test_env_varize() {
        assert_eq!(env_varize("env_var"), "ENV_VAR");
        assert_eq!(env_varize("ENV-VAR"), "ENV_VAR");
        assert_eq!(env_varize("EnvVar"), "ENV_VAR");
    }

    #[test]
    fn test_env_varize_is_idempotent() {
        assert_eq!(env_varize("ENV_VAR"), "ENV_VAR");
    }
}
