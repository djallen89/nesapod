use time;
use std::collections::VecDeque;
use std::fs::{File, OpenOptions};
use std::io::BufWriter;
use std::io::Error as IOError;
use std::io::prelude::*;
use std::path::PathBuf;

/// Debug is meant to output text to a text buffer and a file, printing
/// messages generated by errors and to quantify other metrics for analysis.
pub struct Debug {
    disp_length: usize,
    max_length: usize,
    logname: Option<PathBuf>,
    messages: VecDeque<String>,
    len_since_read: usize
}

//TODO: Separate Debugging from Logging
pub fn default_log(logging: bool) -> Option<PathBuf> {
    if logging {
        let t = time::now();
        let ts = match time::strftime("%y%m%d%H%M%S", &t) {
            Ok(s) => s,
            Err(f) => panic!(f)
        };
        let p = PathBuf::from(&format!("logs/{}.log", ts));
        match File::create(&p) {
            Ok(_) => {},
            Err(f) => panic!(f)
        }
        Some(p)
    } else {
        None
    }
}

impl Debug {
    pub fn new(cap: usize, logging: bool) -> Debug {
        Debug {
            disp_length: cap,
            max_length: 1024,
            logname: default_log(logging),
            messages: VecDeque::with_capacity(1024),
            len_since_read: 0,
        }
    }

    pub fn output(&mut self) -> String {
        let mut out = String::new();
        let len = self.messages.len();
        let d = self.disp_length;
        let start = if len > d {
            len - d
        } else {
            0
        };
        
        for msg in self.messages.iter().skip(start).take(d) { 
            out.push_str(msg);
        }
        self.len_since_read = 0;
        out
    }

    pub fn print(&mut self) {
        let m = self.messages.len();
        let n = if self.len_since_read > m {
            m
        } else {
            self.len_since_read
        };

        println!("{}", n);
        for i in 0 .. n {
            match self.messages.get(m - n + i) {
                Some(x) => println!("{}", x),
                None => break
            }
        }
        self.len_since_read = 0;
    }

    pub fn input(&mut self, msg: &str) {
        if self.messages.len() >= (self.max_length as usize) {
            let n = (self.max_length as usize) - (self.disp_length as usize);
            match self.flush(n) {
                Ok(_) => {},
                Err(f) => panic!(f)
            }
        }
        self.messages.push_back(format!("{}", msg));
        self.len_since_read += 1;
    }

    fn flush(&mut self, n: usize) -> Result<(), IOError> {
        if let Some(ref path) = self.logname {
            match OpenOptions::new().append(true).open(path) {
                Ok(f) => {
                    let mut buf = BufWriter::new(f);
                    for msg in self.messages.drain(0 .. n) {
                        buf.write(msg.as_bytes())?;
                    }
                    buf.get_mut().sync_all()?;
                    Ok(())
                },
                Err(f) => Err(f)
            }
        } else {
            self.messages.truncate(self.disp_length as usize);
            Ok(())
        }
    }

    pub fn flush_all(&mut self) -> Result<(), IOError> {
        let n = self.messages.len();
        self.flush(n)
    }

    pub fn get(&self, idx: usize) -> String {
        match self.messages.get(idx) {
            Some(x) => x.clone(),
            None => format!("index out of bounds")
        }
    }
}
