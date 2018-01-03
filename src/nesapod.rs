#[cfg(all(feature="winit", feature="glium"))]
use conrod::{self, widget, Colorable, Positionable, Widget};
use conrod::backend::glium::glium::{self, Surface};
use conrod::theme::Theme;
use conrod::color;
use core;
use core::ines::INES;
use core::cpu::CPU;

pub fn main(logname: Option<String>, rom: Option<String>) {
    const WIDTH: u32 = 1200;
    const HEIGHT: u32 = 900;

    let mut debugger = core::Debug::new(25, logname);
    let romname = match rom {
        Some(r) => r,
        None => format!("assets/instr_test-v5/rom_singles/01-basics.nes")
    };
    let ines = match INES::new(&romname) {
        Ok(r) => {
            debugger.input_ln(&format!("Successfully loaded ROM of size {}", r.size()));
            debugger.input_ln(&format!("Mapper Id: {}", r.mapper()));
            r
        },
        Err(f) => {
            panic!(f);
        }
    };
    let mut emulator = CPU::power_up(ines);
    match emulator.init() {
        Ok(r) => debugger.input_ln(&r),
        Err(f) => debugger.input_ln(&f)
    }
    debugger.input(&format!("{}", emulator));


    let mut events_loop = glium::glutin::EventsLoop::new();
    let window = glium::glutin::WindowBuilder::new()
        .with_title("NESAPOD")
        .with_dimensions(WIDTH, HEIGHT);
    let context = glium::glutin::ContextBuilder::new()
        .with_vsync(true)
        .with_multisampling(4);
    let display = glium::Display::new(window, context, &events_loop).unwrap();

    // construct our `Ui`.
    let mut theme = Theme::default();
    theme.background_color = color::LIGHT_ORANGE;
    theme.font_size_large = 16;
    theme.font_size_medium = 12;
    theme.font_size_small = 8;
    theme.shape_color = color::LIGHT_BLUE;
    theme.label_color = color::LIGHT_BLUE;
    let mut ui = conrod::UiBuilder::new([WIDTH as f64, HEIGHT as f64]).theme(theme).build();

    // Generate the widget identifiers.
    widget_ids!(struct Ids { text });
    let ids = Ids::new(ui.widget_id_generator());

    // Add a `Font` to the `Ui`'s `font::Map` from file.
    const FONT_PATH: &'static str =
        concat!(env!("CARGO_MANIFEST_DIR"), "/assets/fonts/terminus/TerminusTTF-4.46.0.ttf");
    ui.fonts.insert_from_file(FONT_PATH).unwrap();

    // A type used for converting `conrod::render::Primitives` into `Command`s that can be used
    // for drawing to the glium `Surface`.
    let mut renderer = conrod::backend::glium::Renderer::new(&display).unwrap();

    // The image map describing each of our widget->image mappings (in our case, none).
    let image_map = conrod::image::Map::<glium::texture::Texture2d>::new();

    let mut events = Vec::new();

    'render: loop {
        events.clear();
        
        // Get all the new events since the last frame.
        events_loop.poll_events(|event| { events.push(event); });

        // If there are no new events, wait for one.
        if events.is_empty() {
            events_loop.run_forever(|event| {
                events.push(event);
                glium::glutin::ControlFlow::Break
            });
        }

        // Process the events.
        for event in events.drain(..) {
            // Break from the loop upon `Escape` or closed window.
            match event {
                glium::glutin::Event::WindowEvent { ref event, .. } => {
                    match *event {
                        glium::glutin::WindowEvent::Closed |
                        glium::glutin::WindowEvent::KeyboardInput {
                            input: glium::glutin::KeyboardInput {
                                virtual_keycode: Some(glium::glutin::VirtualKeyCode::Escape),
                                ..
                            },
                            ..
                        } => break 'render,
                        glium::glutin::WindowEvent::KeyboardInput {
                            input: glium::glutin::KeyboardInput {
                                virtual_keycode: Some(ref code),
                                ..
                            },
                            ..
                        } => {
                            use self::glium::glutin::VirtualKeyCode;
                            let mut steps = match code {
                                &VirtualKeyCode::Key1 => 1,
                                &VirtualKeyCode::Key2 => 2,
                                &VirtualKeyCode::Key3 => 3,
                                &VirtualKeyCode::Key4 => 4,
                                &VirtualKeyCode::Key5 => 5,
                                &VirtualKeyCode::Key6 => 6,
                                &VirtualKeyCode::Key7 => 7,
                                &VirtualKeyCode::Key8 => 8,
                                &VirtualKeyCode::Key9 => 9,
                                &VirtualKeyCode::A => 10,
                                &VirtualKeyCode::B => 11,
                                &VirtualKeyCode::C => 12,
                                &VirtualKeyCode::D => 13,
                                &VirtualKeyCode::E => 14,
                                &VirtualKeyCode::F => 15,
                                _ => 0
                            };
                            while steps > 0 {
                                match emulator.step() {
                                    Ok(x)  => debugger.input(&format!("    {}\n{}", x, emulator)),
                                    Err(f) => debugger.input(&format!("    {}\n{}", f, emulator))
                                }
                                steps -= 1;
                            }
                        }
                        _ => (),
                        
                    }
                }
                _ => (),
            };

            // Use the `winit` backend feature to convert the winit event to a conrod input.
            let input = match conrod::backend::winit::convert_event(event, &display) {
                None => continue,
                Some(input) => input,
            };

            // Handle the input with the `Ui`.
            ui.handle_event(input);

            // Set the widgets.
            let ui = &mut ui.set_widgets();

            // Message displayed in middle of screen
            let msg = debugger.output();
            widget::Text::new(&msg)
                .mid_left_of(ui.window)
                .color(color::GREEN.with_alpha(0.7))
                .font_size(16)
                .set(ids.text, ui);
        }

        // Draw the `Ui` if it has changed.
        if let Some(primitives) = ui.draw_if_changed() {
            renderer.fill(&display, primitives, &image_map);
            let mut target = display.draw();
            target.clear_color(0.0, 0.0, 0.0, 1.0);
            renderer.draw(&display, &mut target, &image_map).unwrap();
            target.finish().unwrap();
        }
    }

    let _ines = emulator.shut_down();

    match debugger.flush() {
        Ok(_) => {}
        Err(f) => panic!(f)
    }
}
