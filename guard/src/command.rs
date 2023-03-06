use clap::ArgMatches;

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
