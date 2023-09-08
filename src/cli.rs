use std::{process, collections::HashMap};

/// Container for CLI configuration.
pub struct Arguments {
    name: &'static str,
    description: &'static str,
    options: Vec<CliOption>,
}

impl Arguments {
    /// Creates a new arguments parser.
    pub fn new(name: &'static str, description: &'static str) -> Self {
        Arguments {
            name,
            description,
            options: vec![],
        }
    }

    /// Adds a positional argument to the parser.
    pub fn option(mut self, name: &'static str, description: &'static str, required: bool) -> Self {
        self.options.push(CliOption {
            name,
            description,
            kind: CliOptionKind::Positional { required },
        });
        self
    }

    /// Adds a flag to the parser. Can optionally have a short version.
    pub fn flag(mut self, name: &'static str, description: &'static str, short: Option<char>, r#type: CliOptionType) -> Self {
        self.options.push(CliOption {
            name,
            description,
            kind: CliOptionKind::Flag { short, r#type },
        });
        self
    }

    /// Parses the arguments. Does not return when an error occurs.
    pub fn parse(self, args: impl Iterator<Item = String>) -> ParsedArguments {
        let mut options = HashMap::new();
        let mut pos_index = 0;
        let positional = self.options.iter().filter_map(|o| match o.kind {
            CliOptionKind::Positional { required } => Some((o.name, required)),
            CliOptionKind::Flag { .. } => None,
        }).collect::<Vec<_>>();

        // parse arguments
        let mut args = args.peekable();
        while let Some(arg) = args.next() {
            if arg == "--help" || arg == "-h" {
                self.display_help();
            }

            if let Some(name) = arg.strip_prefix("--") {
                let option = self.options.iter().find_map(|o| match o.kind {
                    CliOptionKind::Flag { ref r#type, .. } if name == o.name => Some(r#type),
                    _ => None,
                });
                let Some(option) = option else {
                    self.display_unknown_option(name);
                };

                let value = match option {
                    CliOptionType::Boolean => "true".into(),
                    CliOptionType::String { default } => {
                        if let Some(value) = args.peek() && !value.starts_with('-') {
                            args.next().unwrap()
                        }
                        else if let Some(default) = default {
                            (*default).into()
                        }
                        else {
                            self.display_missing_value(name);
                        }
                    }
                };
                options.insert(name.into(), value);
            }
            else {
                let option = positional.get(pos_index);
                let Some(option) = option else {
                    eprintln!("Unexpected positional argument: {arg}");
                    eprintln!("Try --help for more information.");
                    process::exit(1);
                };

                options.insert(option.0.into(), arg);
                pos_index += 1;
            }
        }

        // check for missing required arguments
        for option in self.options {
            match option.kind {
                CliOptionKind::Positional { required } if required => {
                    if !options.contains_key(option.name) {
                        eprintln!("Missing required positional argument: {}", option.name);
                        eprintln!("Try --help for more information.");
                        process::exit(1);
                    }
                },
                CliOptionKind::Flag { .. } => {
                    if !options.contains_key(option.name) {
                        options.insert(option.name.into(), "".into());
                    }
                },
                _ => {},
            }
        }

        ParsedArguments(options)
    }

    fn display_help(self) -> ! {
        eprintln!("{} - {}", self.name, self.description);
        eprintln!("Usage: {} {} [options]", self.name, self.options.iter().filter_map(|o| match o.kind {
            CliOptionKind::Positional { required } => Some(if required { format!("<{}>", o.name) } else { format!("[{}]", o.name) }),
            CliOptionKind::Flag { .. } => None,
        }).collect::<Vec<_>>().join(" "));
        eprintln!();
        eprintln!("Options:");
        let options = self.options.iter()
            .filter_map(|o| match o.kind {
                CliOptionKind::Positional { .. } => None,
                CliOptionKind::Flag { short, .. } => {
                    let mut name_text = format!("--{}", o.name);
                    if let Some(short) = short {
                        name_text.push_str(&format!(", -{short}"));
                    }
                    Some((name_text, o.description))
                },
            })
            .collect::<Vec<_>>();

        let p = options.iter().map(|(name, _)| name.len()).max().unwrap_or(0) + 3;

        eprintln!("    {:p$} Display this help message.", "--help, -h");
        for option in options {
            eprintln!("    {:p$} {}", option.0, option.1);
        }
        process::exit(0);
    }

    fn display_unknown_option(self, name: &str) -> ! {
        eprintln!("Unknown option: --{name}");
        eprintln!("Try --help for more information.");
        process::exit(1);
    }

    fn display_missing_value(self, name: &str) -> ! {
        eprintln!("Missing value for option: --{name}");
        eprintln!("Try --help for more information.");
        process::exit(1);
    }
}

pub struct CliOption {
    name: &'static str,
    description: &'static str,
    kind: CliOptionKind,
}

pub enum CliOptionKind {
    Positional { required: bool },
    Flag { short: Option<char>, r#type: CliOptionType },
}

pub enum CliOptionType {
    String { default: Option<&'static str> },
    Boolean,
}

pub struct ParsedArguments(HashMap<String, String>);

impl ParsedArguments {
    pub fn get(&self, name: &str) -> &str {
        self.0.get(name).map(|s| s.as_str()).unwrap()
    }
}
