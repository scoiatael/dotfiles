use std::env;
use rpassword;

fn index_into(into: String, indices: Vec<usize>) -> Option<Vec<String>> {
    let mut chars = into.chars();
    let mut vec = Vec::new();
    let mut pos = 0;
    for idx in indices.iter() {
        let offset = idx - pos;
        pos += offset + 1;
        vec.push(chars.nth(offset)?.to_string())
    }
    return Some(vec)
}

fn main() {
    let pass = rpassword::read_password_from_tty(Some("index into: ")).unwrap();
    // skip progname, indexed from 1
    let indices = env::args().skip(1).map(|a| a.parse::<usize>().unwrap() - 1).collect();
    println!("{}", index_into(pass, indices).unwrap().join(" "));
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn abc_a() {
        assert_eq!(
            index_into("abc".to_string(), [0].to_vec()),
            Some(["a".to_string()].to_vec())
        )
    }
    #[test]
    fn abc_c() {
        assert_eq!(
            index_into("abc".to_string(), [2].to_vec()),
            Some(["c".to_string()].to_vec())
        )
    }
    #[test]
    fn abc_ac() {
        assert_eq!(
            index_into("abc".to_string(), [0, 2].to_vec()),
            Some(["a".to_string(), "c".to_string()].to_vec())
        )
    }
    #[test]
    fn abc_abc() {
        assert_eq!(
            index_into("abc".to_string(), [0,1,2].to_vec()),
            Some(["a".to_string(), "b".to_string(), "c".to_string()].to_vec())
        )
    }
}
