extern crate clap;
use clap::{App, Arg, ArgGroup};

use std::io::{self, Read};

extern crate inflector;
use inflector::*;

fn main() {
    let matches = App::new("inflector")
        .version("v0.1.0")
        .arg(
            Arg::with_name("INPUT")
                .help("Input for modification")
                .required(false),
        )
        .args_from_usage(
            "-d, --dasherize         'dasherize:  SomeName -> some-name'
             -u, --underscore        'underscore: SomeName -> some_name'
             -e, --env-varize        'env-varize: foo-bar -> FOO_BAR'
             -c, --camelize          'camelize:   ENV_VAR -> EnvVar'",
        )
        .group(
            ArgGroup::with_name("transformation")
                .args(&["dasherize", "underscore", "env-varize", "camelize"])
                .required(true),
        )
        .get_matches();

    let mut buffer = String::new();
    let text = matches.value_of("INPUT").unwrap_or_else(|| {
        io::stdin().read_to_string(&mut buffer).unwrap();

        &buffer.trim()
    });

    if matches.is_present("dasherize") {
        println!("{}", dasherize(text));
    }
    if matches.is_present("camelize") {
        println!("{}", camelize(text));
    }
    if matches.is_present("underscore") {
        println!("{}", underscore(text));
    }
    if matches.is_present("env-varize") {
        println!("{}", env_varize(text));
    }
}
