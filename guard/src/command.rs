use crate::commands::parse_tree::ParseTree2;
use crate::commands::test::Test2;
use crate::commands::validate::Validate2;
use crate::commands::{ABOUT_GUARD, APP_NAME, APP_VERSION};
use clap::{ArgMatches, Parser, Subcommand};

use crate::rules::errors::Error;
use crate::utils::reader::Reader;
use crate::utils::writer::Writer;

pub trait Command {
    fn name(&self) -> &'static str;
    fn command(&self) -> clap::Command;
    fn execute(
        &self,
        args: &ArgMatches,
        writer: &mut Writer,
        reader: &mut Reader,
    ) -> Result<i32, Error>;
}

//TODO: we can rename this if we'd like, but probably not call it
//command anymore since that conflicts with claps Command anyways
pub trait Runnable {
    fn execute(&self, writer: &mut Writer, reader: &mut Reader) -> Result<i32, Error>;
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
    #[command(arg_required_else_help(true), name = "test")]
    Test(Test2),
    #[command(arg_required_else_help(true), name = "parse-tree")]
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
