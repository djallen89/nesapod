use sdl2;
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::render::{Canvas, TextureCreator, TextureAccess};
use sdl2::video::Window;

use find_folder;
use std;
use std::time::Duration;
use std::thread;
use core;
use core::cpu::{CPU, FRAME_CYCLES};
use core::ines::INES;
use super::debug::Debug;

const WIDTH: u32 = 256;
const HEIGHT: u32 = 240;

pub struct Nesapod {
    debugger: Option<Debug>,
    core: CPU,
}

impl Nesapod {
    pub fn new(rom: Option<String>, debug: bool, logging: bool) -> Nesapod {
        let romname = match rom {
            Some(r) => r,
            None => format!("assets/instr_test-v5/official_only.nes")
        };

        let debugger = if debug {
            Some(Debug::new(32, logging))
        } else {
            None
        };

        let ines = match INES::new(&romname) {
            Ok(r) => r,
            Err(f) => panic!(f)
        };

        let core = match CPU::power_up(ines, debug) {
            Ok(r) => r,
            Err(f) => panic!(f)
        };

        let mut nes = Nesapod {
            debugger: debugger,
            core: core,
        };

        nes.core.reset();
        nes
    }

    pub fn render(&mut self, canvas: &mut Canvas<Window>) {
        //canvas.clear();
        let maybe_texture = canvas.texture_creator();
        let mut texture = match maybe_texture.create_texture(PixelFormatEnum::ARGB8888,
                                                   TextureAccess::Target,
                                                   WIDTH,
                                                   HEIGHT) {
            Ok(r) => r,
            Err(f) => panic!(f)
        };

        let raw_img = self.core.print_screen();
        match texture.update(None, &raw_img, 4) {
            Ok(_) => {},
            Err(f) => panic!(f)
        }

        match canvas.with_texture_canvas(&mut texture, |texture_canvas| {
            //texture_canvas.set_draw_color(Color::RGBA(0, 0, 0, 0));
            texture_canvas.clear();
            texture_canvas.present();
        }) {
            Ok(_) => {},
            Err(f) => panic!(f)
        }
    }
    
    pub fn run(&mut self, s: &mut isize, canvas: &mut Canvas<Window>) {
        if *s != 0 {
            self.core.counter += FRAME_CYCLES;
        }
        while *s < 262 {
            if self.core.counter <= 0 {
                break;
            }

            let new_frame = self.core.run_scanline(*s);
            if new_frame {
                self.core.ppu.clear_frame_signal();
                self.render(canvas);
            }
        }

        *s += 1;
        if *s >= 262 {
            *s = 0;
        }
    }
}

pub fn main(rom: Option<String>, debug: bool, logging: bool) {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    
    let window = video_subsystem.window("Nesapod", WIDTH, HEIGHT)
        .position_centered()
        .opengl()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();

    let mut nesapod = Nesapod::new(rom, debug, logging);

    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();
    canvas.present();

    let mut event_pump = sdl_context.event_pump().unwrap();

    let mut s = 0;
    let mut last_update = std::time::Instant::now();

    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..} | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'running
                },
                _ => {}
            }
        }

        let sixteen_ms = std::time::Duration::from_millis(16);
        let duration_since_last_update = std::time::Instant::now().duration_since(last_update);
        if duration_since_last_update < sixteen_ms {
            std::thread::sleep(sixteen_ms - duration_since_last_update);
        }
        //std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
        nesapod.run(&mut s, &mut canvas);
        last_update = std::time::Instant::now();
    }
}


