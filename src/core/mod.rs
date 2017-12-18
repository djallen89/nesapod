use std::collections::VecDeque;
use std::fs::{File, OpenOptions};
use std::path::Path;
use std::io::Error as IOError;
use std::io::prelude::*;


/// Debug is meant to output text to a text buffer and a file, printing
/// messages generated by errors and to quantify other metrics for analysis.
pub struct Debug {
    length: u8,
    file: Option<String>,
    messages: VecDeque<String>,
}

impl Debug {
    pub fn new(cap: u8, f: Option<String>) -> Debug {
        Debug {
            length: cap,
            file: f,
            messages: VecDeque::with_capacity(cap as usize)
        }
    }

    pub fn output(&self) -> String {
        let mut out = String::new();
        for msg in self.messages.iter().rev() {
            out.push_str(msg);
            out.push_str("\n");
        }
        out
    }

    pub fn input(&mut self, msg: &str) -> Result<(), ()> {
        while self.messages.len() >= self.length as usize {
            let back = self.messages.pop_back().unwrap();
            match self.write(&back) {
                Ok(_) => {},
                Err(_) => {}
            };
        }
        self.messages.push_front(msg.to_string());
        Ok(())
    }

    pub fn write(&mut self, msg: &str) -> Result<(), IOError> {
        if self.file.is_some() {
            let mut f = self.file();
            f.write_all(msg.as_bytes())?;
            f.write_all(b"\n")?;
            f.sync_data()?;
        }
        Ok(())
    }

    pub fn flush(&mut self) -> Result<(), IOError> {
        if self.file.is_some() {
            let mut file = self.file();

            for msg in self.messages.drain(..).rev() {
                file.write_all(msg.as_bytes())?;
                file.write_all(b"\n")?;
            }

            file.sync_all()?;
        }
        Ok(())
    }

    pub fn file(&self) -> File {
        let pathname = self.file.clone().unwrap();
        let path = Path::new(&pathname);
        println!("{}", path.exists());
        match OpenOptions::new().append(true).create_new(!path.exists()).open(path) {
            Ok(file) => file,
            Err(f) => panic!(f)
        }        
    }
}
