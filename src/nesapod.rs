use conrod;
use conrod::backend::glium::glium;
use conrod::backend::glium::glium::Surface;
use find_folder;
use core;
use core::cpu::CPU;
use core::ines::INES;
use glium::glutin::VirtualKeyCode as VKC;

const WIDTH: u32 = 900;
const HEIGHT: u32 = 700;

struct Fonts {
    regular: conrod::text::font::Id,
    italic: conrod::text::font::Id,
    bold: conrod::text::font::Id,
}

pub fn main(logging: bool, rom: Option<String>) {
    
    let mut debugger = core::Debug::new(32, logging);
    let romname = match rom {
        Some(r) => r,
        None => format!("assets/instr_test-v5/official_only.nes")
    };
    
    let ines = match INES::new(&romname) {
        Ok(r) => r,
        Err(f) => panic!(f)
    };
    
    let mut emulator = match CPU::power_up(ines) {
        Ok(emu) => emu,
        Err(f) => panic!(f)
    };

    match emulator.reset() {
        Ok(_) => {},
        Err(_) => {},
    }
    debugger.input(&format!("{}\n", emulator));

    let mut events_loop = glium::glutin::EventsLoop::new();
    let window = glium::glutin::WindowBuilder::new()
        .with_title("Text Demo")
        .with_dimensions(WIDTH, HEIGHT);
    let context = glium::glutin::ContextBuilder::new()
        .with_vsync(true)
        .with_multisampling(4);
    let display = glium::Display::new(window, context, &events_loop).unwrap();

    let mut ui = conrod::UiBuilder::new([WIDTH as f64, HEIGHT as f64]).build();

    let ids = Ids::new(ui.widget_id_generator());

    let assets = find_folder::Search::KidsThenParents(3, 5).for_folder("assets").unwrap();
    let noto_sans = assets.join("fonts/NotoSans");
    let terminus = assets.join ("fonts/terminus");
    let fonts = Fonts {
        regular: ui.fonts.insert_from_file(terminus.join("TerminusTTF-4.46.0.ttf")).unwrap(),
        italic: ui.fonts.insert_from_file(noto_sans.join("NotoSans-Italic.ttf")).unwrap(),
        bold: ui.fonts.insert_from_file(noto_sans.join("NotoSans-Bold.ttf")).unwrap(),
    };

    ui.theme = support::theme();
    ui.theme.font_id = Some(fonts.regular);

    let mut renderer = conrod::backend::glium::Renderer::new(&display).unwrap();

    let image_map = conrod::image::Map::<glium::texture::Texture2d>::new();

    let mut event_loop = support::EventLoop::new();
    'main: loop {

        for event in event_loop.next(&mut events_loop) {
            if let Some(event) = conrod::backend::winit::convert_event(event.clone(), &display) {
                ui.handle_event(event);
            }

            match event {
                glium::glutin::Event::WindowEvent { event, .. } => match event {
                    glium::glutin::WindowEvent::Closed |
                    glium::glutin::WindowEvent::KeyboardInput {
                        input: glium::glutin::KeyboardInput {
                            virtual_keycode: Some(glium::glutin::VirtualKeyCode::Escape),
                            ..
                        },
                        ..
                    } => {
                        let msg = emulator.dump_ram();
                        debugger.input(&msg);
                        break 'main
                    },
                    glium::glutin::WindowEvent::KeyboardInput {
                        input: glium::glutin::KeyboardInput {
                            state: glium::glutin::ElementState::Released,
                            virtual_keycode: Some(k),
                            ..
                        },
                        ..
                    } => {
                        let mut steps = match k {
                            VKC::Key1 => 1,
                            VKC::F => 1 << 4,
                            VKC::L => 1 << 8,
                            VKC::Z => 1 << 12,
                            VKC::T => 1 << 16,
                            _ => 0
                        };
                        while steps > 0 {
                            match emulator.step() {
                                Ok(_) => debugger.input(&format!("{}\n", emulator)),
                                Err(f) => {
                                    debugger.input(&format!("{}\n", emulator));
                                    debugger.input(&format!("{}\n", f));
                                    break;
                                }
                            }
                            steps -= 1;
                        }
                    },
                    _ => (),
                },
                _ => (),
            }
        }

        let msg = debugger.output();
        set_ui(ui.set_widgets(), &ids, &fonts, &msg);
        // Render the `Ui` and then display it on the screen.
        if let Some(primitives) = ui.draw_if_changed() {
            renderer.fill(&display, primitives, &image_map);
            let mut target = display.draw();
            target.clear_color(0.0, 0.0, 0.0, 1.0);
            renderer.draw(&display, &mut target, &image_map).unwrap();
            target.finish().unwrap();
        }
    }

    match debugger.flush_all() {
        Ok(_) => {},
        Err(f) => panic!(f)
    }
}

widget_ids!{
    struct Ids {
        master,
        middle_col,
        left_text,
        middle_text,
        right_text,
    }
}

fn set_ui(ref mut ui: conrod::UiCell, ids: &Ids, fonts: &Fonts, msg: &str) {
    use conrod::{color, widget, Colorable, Positionable, Scalar, Sizeable, Widget};

    widget::Canvas::new().flow_right(&[
        (ids.middle_col, widget::Canvas::new().color(color::DARK_CHARCOAL)),
    ]).set(ids.master, ui);

    const PAD: Scalar = 20.0;

    widget::Text::new(msg)
        .font_id(fonts.regular)
        .color(color::LIGHT_GREEN)
        .padded_w_of(ids.middle_col, PAD)
        .middle_of(ids.middle_col)
        .center_justify()
        .line_spacing(2.5)
        .set(ids.middle_text, ui);
}


mod support {
    use conrod;
    use std;
    use conrod::backend::glium::glium;

    pub fn theme() -> conrod::Theme {
        use conrod::position::{Align, Direction, Padding, Position, Relative};
        conrod::Theme {
            name: "Demo Theme".to_string(),
            padding: Padding::none(),
            x_position: Position::Relative(Relative::Align(Align::Start), None),
            y_position: Position::Relative(Relative::Direction(Direction::Backwards, 20.0), None),
            background_color: conrod::color::DARK_CHARCOAL,
            shape_color: conrod::color::LIGHT_CHARCOAL,
            border_color: conrod::color::BLACK,
            border_width: 0.0,
            label_color: conrod::color::WHITE,
            font_id: None,
            font_size_large: 26,
            font_size_medium: 18,
            font_size_small: 12,
            widget_styling: conrod::theme::StyleMap::default(),
            mouse_drag_threshold: 0.0,
            double_click_threshold: std::time::Duration::from_millis(500),
        }
    }

    widget_ids! {
        pub struct Ids {
            canvas,
            title,
            introduction,
            image_title,
            rust_logo,
            button,
        }
    }

    /// Instantiate a GUI demonstrating every widget available in conrod.
    pub fn _gui(ui: &mut conrod::UiCell, ids: &Ids) {
        use conrod::{widget, Labelable, Positionable, Sizeable, Widget};

        const MARGIN: conrod::Scalar = 30.0;
        const SHAPE_GAP: conrod::Scalar = 50.0;
        const TITLE_SIZE: conrod::FontSize = 42;
        const SUBTITLE_SIZE: conrod::FontSize = 32;

        const TITLE: &'static str = "All Widgets";
        widget::Canvas::new().pad(MARGIN).scroll_kids_vertically().set(ids.canvas, ui);


        // We'll demonstrate the `Text` primitive widget by using it to draw a title and an
        // introduction to the example.
        let msg = "";
        widget::Text::new(&msg)
            .padded_w_of(ids.canvas, MARGIN)
            .down(60.0)
            .align_middle_x_of(ids.canvas)
            .center_justify()
            .line_spacing(5.0)
            .set(ids.introduction, ui);

        for _press in widget::Button::new()
            .label("PRESS ME")
            .mid_left_with_margin_on(ids.canvas, MARGIN)
            .w_h(60.0, 20.0)
            .set(ids.button, ui)
        {

        }
    }


    /// In most of the examples the `glutin` crate is used for providing the window context and
    /// events while the `glium` crate is used for displaying `conrod::render::Primitives` to the
    /// screen.
    /// 
    /// This `Iterator`-like type simplifies some of the boilerplate involved in setting up a
    /// glutin+glium event loop that works efficiently with conrod.
    pub struct EventLoop {
        ui_needs_update: bool,
        last_update: std::time::Instant,
    }

    impl EventLoop {

        pub fn new() -> Self {
            EventLoop {
                last_update: std::time::Instant::now(),
                ui_needs_update: true,
            }
        }

        /// Produce an iterator yielding all available events.
        pub fn next(&mut self, events_loop: &mut glium::glutin::EventsLoop) -> Vec<glium::glutin::Event> {
            // We don't want to loop any faster than 60 FPS, so wait until it has been at least 16ms
            // since the last yield.
            let last_update = self.last_update;
            let sixteen_ms = std::time::Duration::from_millis(16);
            let duration_since_last_update = std::time::Instant::now().duration_since(last_update);
            if duration_since_last_update < sixteen_ms {
                std::thread::sleep(sixteen_ms - duration_since_last_update);
            }

            // Collect all pending events.
            let mut events = Vec::new();
            events_loop.poll_events(|event| events.push(event));

            // If there are no events and the `Ui` does not need updating, wait for the next event.
            if events.is_empty() && !self.ui_needs_update {
                events_loop.run_forever(|event| {
                    events.push(event);
                    glium::glutin::ControlFlow::Break
                });
            }

            self.ui_needs_update = false;
            self.last_update = std::time::Instant::now();

            events
        }

        /// Notifies the event loop that the `Ui` requires another update whether or not there are any
        /// pending events.
        ///
        /// This is primarily used on the occasion that some part of the `Ui` is still animating and
        /// requires further updates to do so.
        pub fn _needs_update(&mut self) {
            self.ui_needs_update = true;
        }
    }
}
