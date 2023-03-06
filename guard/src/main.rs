use clap::{ArgMatches, Parser};
use std::collections::HashMap;
use std::fs::File;
mod command;
mod commands;
mod migrate;
mod rules;
mod utils;

use crate::command::{Cli, Runnable};
use crate::commands::parse_tree::ParseTree2;
use crate::commands::test::Test2;
use crate::commands::validate::Validate2;
use crate::commands::{MIGRATE, OUTPUT, PARSE_TREE, RULEGEN};
use crate::utils::reader::{ReadBuffer, Reader};
use crate::utils::writer::WriteBuffer::Stderr;
use crate::utils::writer::{WriteBuffer::File as WBFile, WriteBuffer::Stdout, Writer};
use cfn_guard::command::Commands;
use cfn_guard::utils::reader::ReadBuffer::Stdin;
use command::Command;
use commands::{APP_NAME, APP_VERSION};
use rules::errors::Error;
use std::io::Write;
use std::process::exit;
use std::rc::Rc;

const ABOUT_GUARD: &str = r#"
  Guard is a general-purpose tool that provides a simple declarative syntax to define 
  policy-as-code as rules to validate against any structured hierarchical data (like JSON/YAML).
  Rules are composed of clauses expressed using Conjunctive Normal Form
  (fancy way of saying it is a logical AND of OR clauses). Guard has deep
  integration with CloudFormation templates for evaluation but is a general tool
  that equally works for any JSON- and YAML- data."#;

fn main() -> Result<(), Error> {
    let mut app = clap::Command::new(APP_NAME)
        .version(APP_VERSION)
        .about(ABOUT_GUARD)
        .arg_required_else_help(true);

    let mut commands: Vec<Box<dyn Command>> = Vec::with_capacity(2);
    commands.push(Box::new(commands::parse_tree::ParseTree::new()));
    commands.push(Box::new(commands::test::Test::new()));
    commands.push(Box::new(commands::validate::Validate::new()));
    commands.push(Box::new(commands::rulegen::Rulegen::new()));
    commands.push(Box::new(commands::migrate::Migrate::new()));

    let mappings = commands.iter().map(|s| (s.name(), s)).fold(
        HashMap::with_capacity(commands.len()),
        |mut map, entry| {
            map.insert(entry.0, entry.1.as_ref());
            map
        },
    );

    for each in &commands {
        app = app.subcommand(each.command());
    }

    let help = app.render_usage();
    let app = app.get_matches();

    match app.subcommand() {
        Some((name, value)) => {
            if let Some(command) = mappings.get(name) {
                let mut output_writer: Writer = if [PARSE_TREE, MIGRATE, RULEGEN]
                    .contains(&command.name())
                {
                    let writer: Writer = match value.get_one::<String>(OUTPUT.0) {
                        Some(file) => {
                            Writer::new(WBFile(File::create(file)?), Stderr(std::io::stderr()))
                        }
                        None => Writer::new(Stdout(std::io::stdout()), Stderr(std::io::stderr())),
                    };
                    writer
                } else {
                    Writer::new(Stdout(std::io::stdout()), Stderr(std::io::stderr()))
                };

                match (*command).execute(
                    value,
                    &mut output_writer,
                    &mut Reader::new(ReadBuffer::Stdin(std::io::stdin())),
                ) {
                    Err(e) => {
                        output_writer
                            .write_err(format!("Error occurred {e}"))
                            .expect("failed to write to stderr");

                        exit(-1);
                    }
                    Ok(code) => exit(code),
                }
            } else {
                println!("{}", help);
            }
        }
        None => {
            println!("{}", help);
        }
    }

    Ok(())
}

#[derive(Debug, Parser)]
#[command(name = APP_NAME, about = ABOUT_GUARD, version = APP_VERSION)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Debug, Subcommand)]
pub enum Commands {
    #[command(arg_required_else_help(true), name = "validate")]
    Validate(Validate2),
    #[command(arg_required_else_help(true), name = "validate")]
    Test(Test2),
    #[command(arg_required_else_help(true), name = "validate")]
    ParseTree(ParseTree2),
}

impl Runnable for Commands {
    fn execute(&self, writer: &mut Writer, reader: &mut Reader) -> Result<i32, Error> {
        match &self {
            Commands::Validate(validate) => validate.execute(writer, reader),
            Commands::Test(test) => test.execute(writer, reader),
            Commands::ParseTree(parse_tree) => parse_tree.execute(writer, reader),
        }
    }
}

fn new_main() -> Result<(), Error> {
    let mut reader = Reader::new(Stdin(std::io::stdin()));
    let args = Cli::parse();

    let mut writer = match &args.command {
        Commands::ParseTree(cmd) => match &cmd.output {
            Some(file) => Writer::new(WBFile(File::create(file)?), Stderr(std::io::stderr())),
            None => Writer::new(Stdout(std::io::stdout()), Stderr(std::io::stderr())),
        },
        _ => Writer::new(Stdout(std::io::stdout()), Stderr(std::io::stderr())),
    };

    match args.command.execute(&mut writer, &mut reader) {
        Err(e) => {
            output_writer
                .write_err(format!("Error occurred {e}"))
                .expect("failed to write to stderr");

            exit(-1);
        }
        Ok(code) => exit(code),
    }
}
