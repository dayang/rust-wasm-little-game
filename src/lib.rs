mod utils;

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use js_sys::Math;
use std::ptr;
use std::cell::{RefCell};
use std::rc::{Rc};
use std::convert::AsRef;
use web_sys::console;
use std::collections::HashMap;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

const PLAN : &[&str] = &[
    "         v         x!!!x                                                   ",
    "                   xx!!x                                                   ",
    "                   x xvx               o o                                 ",
    "  x              = x                                                       ",
    "  x         o o        |   oo                                              ",
    "  x @      xxxxx           oo      xxxxxxxxx                               ",
    "  xxxxx            =          x        =                                   ",
    "      x!!!!!!!!!!!xx          x            =                               ",
    "      xxxxxxxxxxxxxx      x!!!xx      o o      =                           ",
    "                          x!!!x                    =                       ",
    "=   x                     x!!!x     x!!xx              =                   ",
    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    "                                                                           "
];

/// 向量
#[derive(Copy, Clone, PartialEq)]
pub struct Vector {
    pub x: f64,
    pub y: f64,
}

impl Vector {
    pub fn new(x: f64, y: f64) -> Vector {
        Vector { x, y}
    }

    pub fn plus(&self, other: Vector) -> Vector {
        Vector {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }

    pub fn times(&self, factor: f64) -> Vector {
        Vector {
            x: self.x * factor,
            y: self.y * factor,
        }
    }
}

/// 不可移动物体
#[derive(Clone, Copy)]
pub enum StillObject {
    Wall,
    Lava,
    Empty,
}

/// 可移动物体
pub enum Actor {
    Lava(Lava),
    Player(Player),
    Coin(Coin),
}

impl Actor {
    pub fn get_actor_inner(&self) -> &dyn ActorInfo {
        match self {
            Actor::Lava(ref n) => n as &dyn ActorInfo,
            Actor::Player(ref n) => n as &dyn ActorInfo,
            Actor::Coin(ref n) => n as &dyn ActorInfo,
        }
    }
}

/// 关卡
pub struct Level {
    width: usize,
    height: usize,
    grid: Vec<Vec<StillObject>>,
    actors: RefCell<Vec<Actor>>,
    removed_actors: RefCell<Vec<*const Actor>>,
    status: RefCell<Option<GameStatus>>,
    finish_delay: RefCell<Option<f64>>,
}

pub enum GameStatus {
    Win,
    Lost
}

impl Level {
    pub fn new(plan: &[&str]) -> Level {
        let mut width = 0;
        let mut grid = vec![];
        let mut actors = vec![];
        for (y, line) in plan.iter().enumerate() {
            width = line.chars().count();
            let mut grid_line = vec![];
            for (x, ch) in (*line).chars().enumerate() {
                match ch {
                    '@' => { 
                        actors.push(Actor::Player(Player::new(Vector::new(x as f64, y as f64))));
                        grid_line.push(StillObject::Empty);
                    },
                    'o' => { 
                        actors.push(Actor::Coin(Coin::new(Vector::new(x as f64, y as f64))));
                        grid_line.push(StillObject::Empty);
                    },
                    '=' | '|' | 'v' => {
                         actors.push(Actor::Lava(Lava::new(Vector::new(x as f64, y as f64), ch)));
                         grid_line.push(StillObject::Empty);
                    },
                    'x' => grid_line.push(StillObject::Wall),
                    '!' => grid_line.push(StillObject::Lava),
                    ' ' => grid_line.push(StillObject::Empty),
                    _ => unreachable!()
                }
            }
            grid.push(grid_line);
        }

        Level {
            width,
            height: plan.len(),
            grid,
            actors: RefCell::new(actors),
            removed_actors: RefCell::new(vec![]),
            status: RefCell::new(None),
            finish_delay: RefCell::new(None),
        }
    }

    pub fn get_player(&self) -> (Vector, Vector) {
        for actor in self.actors.borrow().iter() {
            if let Actor::Player(player) = actor {
                return (player.get_pos(), player.get_size());
            }
        }
        (Vector::new(0.0,0.0), Vector::new(0.0,0.0))
    }

    pub fn is_finished(&self) -> bool {
        self.status.borrow().is_some() && self.finish_delay.borrow().is_some() && self.finish_delay.borrow().unwrap() < 0.0
    }

    /// 检测碰到的不可移动物体
    pub fn obstacle_at(&self, pos: Vector, size: Vector) -> Option<StillObject> {
        let x_start = Math::floor(pos.x);
        let x_end = Math::ceil(pos.x + size.x);
        let y_start = Math::floor(pos.y);
        let y_end = Math::ceil(pos.y + size.y);

        if x_start < 0.0 || x_end > (self.width as f64) || y_start < 0.0 {
            return Some(StillObject::Wall);
        }

        if y_end > (self.height as f64) {
            return Some(StillObject::Lava);
        }

        for y in (y_start as usize)..(y_end as usize) {
            for x in (x_start as usize)..(x_end as usize) {
                match self.grid[y][x] {
                    StillObject::Empty => (),
                    other @ _ => return Some(other)
                };
            }
        }

        None
    }

    // 检测碰到的其它actor
    pub fn actor_at(&self, actor: &dyn ActorInfo) -> Option<*const Actor> {
        for other in self.actors.borrow().iter() {
            if actor_collide(actor, other.get_actor_inner()){
                return Some(other as *const Actor);
            }
        }

        None
    }

    pub fn player_touch_still_object(&self, still_object: &StillObject) {
        match still_object {
            StillObject::Lava => {
                if self.status.borrow().is_none() {
                    *self.status.borrow_mut() = Some(GameStatus::Lost);
                    *self.finish_delay.borrow_mut() = Some(1.0);
                }
            },
            _ => ()
        };
    }

    pub fn player_touch_actor(&self, player: &Player)  {
        self.removed_actors.borrow_mut().clear();
        let actor = self.actor_at(player);
        
        if actor.is_none() {
            return;
        }

        let actor = actor.unwrap();
        
        match unsafe{ &*actor } {
            Actor::Coin(_) => {
                self.removed_actors.borrow_mut().push(actor);
            },
            Actor::Lava(_) => {
                if self.status.borrow().is_none() {
                    *self.status.borrow_mut() = Some(GameStatus::Lost);
                    *self.finish_delay.borrow_mut() = Some(1.0);
                }
            },
            _ => ()
        };
    }

    pub fn animate(&self, step: f64, keys: &HashMap<Keys, bool>) {
        if self.status.borrow().is_some() {
            console::log_1(&format!("{}", self.finish_delay.borrow().as_ref().unwrap()).into());
            match self.finish_delay.borrow_mut().as_mut() {
                Some(v) => *v -= step,
                None => ()
            }
        }

        let mut step = step;
        
        while step > 0.0 {
            let this_step = Math::min(step, MAXSTEP);
            for actor in self.actors.borrow().iter() {
                actor.get_actor_inner().act(this_step, self, keys);
            }

            for actor in self.removed_actors.borrow().iter() {
                self.actors.borrow_mut().retain(|a| !ptr::eq(a, *actor));
            }

            if !self.actors.borrow().iter().any(|a| {
                match a {
                    Actor::Coin(_) => true,
                    _ => false
                }
            }){
                if self.status.borrow().is_none() {
                    *self.status.borrow_mut() = Some(GameStatus::Win);
                    *self.finish_delay.borrow_mut() = Some(1.0);
                }
            }

            step -= this_step;
        }
    }
}

const MAXSTEP : f64 = 0.05;

fn actor_collide(actor1: &dyn ActorInfo, actor2: &dyn ActorInfo) -> bool {
    !ptr::eq(actor1, actor2) && 
        actor1.get_pos().x + actor1.get_size().x > actor2.get_pos().x &&
        actor1.get_pos().x < actor2.get_pos().x + actor2.get_size().x &&
        actor1.get_pos().y + actor1.get_size().y > actor2.get_pos().y &&
        actor1.get_pos().y < actor2.get_pos().y + actor2.get_size().y 
}

pub trait ActorInfo {
    fn get_pos(&self) -> Vector;
    fn get_size(&self) -> Vector;
    fn act(&self, step: f64, level: &Level, keys: &HashMap<Keys, bool>);
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Keys {
    Left,
    Up,
    Right,
}

/// 人物
pub struct Player {
    pos: RefCell<Vector>,
    size: RefCell<Vector>,
    speed: RefCell<Vector>,
    x_speed: f64,
    jump_speed: f64,
    gravity: f64,
}

impl Player {
    pub fn new(pos: Vector) -> Player {
        Player {
            pos: RefCell::new(pos.plus(Vector::new(0f64, -0.5))),
            size: RefCell::new(Vector::new(0.8, 1.5)),
            speed: RefCell::new(Vector::new(0f64, 0f64)),
            x_speed: 7.0,
            jump_speed: 17.0,
            gravity: 30.0,
        }
    }

    pub fn move_x(&self, step: f64, level: &Level, keys: &HashMap<Keys, bool>) {
        self.speed.borrow_mut().x = 0.0;
        if let Some(&true) = keys.get(&Keys::Left) {
            self.speed.borrow_mut().x -= self.x_speed
        }

        if let Some(&true) = keys.get(&Keys::Right) {
            self.speed.borrow_mut().x += self.x_speed
        }

        let motion = Vector::new(self.speed.borrow().x * step, 0.0);
        let new_pos = self.pos.borrow().plus(motion);
        let obstacle = level.obstacle_at(new_pos, self.get_size());
        if obstacle.is_some() {
            level.player_touch_still_object(&obstacle.unwrap());
        } else {
            *self.pos.borrow_mut() = new_pos;
        }
    }

    pub fn move_y(&self, step: f64, level: &Level, keys: &HashMap<Keys, bool>) {
        self.speed.borrow_mut().y += step * self.gravity;
        let motion = Vector::new(0.0, self.speed.borrow().y * step);
        let new_pos = self.pos.borrow().plus(motion);
        let obstacle = level.obstacle_at(new_pos, self.get_size());
        if obstacle.is_some() {
            level.player_touch_still_object(&obstacle.unwrap());
            if *keys.get(&Keys::Up).unwrap_or(&false) && self.speed.borrow().y > 0.0 {
                self.speed.borrow_mut().y = -self.jump_speed;
            } else {
                self.speed.borrow_mut().y = 0.0;
            }
        } else {
            *self.pos.borrow_mut() = new_pos;
        }
    }
}

impl ActorInfo for Player {
    fn get_pos(&self) -> Vector {
        self.pos.borrow().clone()
    }

    fn get_size(&self) -> Vector {
        self.size.borrow().clone()
    }

    fn act(&self, step: f64, level: &Level, keys: &HashMap<Keys, bool>){
        self.move_x(step, level, keys);
        self.move_y(step, level, keys);

        level.player_touch_actor(self);
        
        // losing animation
        if let Some(GameStatus::Lost) = *level.status.borrow() {
            self.pos.borrow_mut().y += step;
            self.size.borrow_mut().y -= step;
        }
    }
}

/// 岩浆
pub struct Lava {
    pos: RefCell<Vector>,
    size: RefCell<Vector>,
    speed: RefCell<Vector>,
    repeat_pos: Option<Vector>,
}

impl Lava {
    pub fn new(pos: Vector, ch: char) -> Lava {
        Lava {
            pos: RefCell::new(pos),
            size: RefCell::new(Vector::new(1f64, 1f64)),
            speed: match ch {
                '=' => RefCell::new(Vector::new(2.0, 0.0)),
                '|' => RefCell::new(Vector::new(0.0, 2.0)),
                'v' => RefCell::new(Vector::new(0.0, 3.0)),
                _ => unreachable!()
            },
            repeat_pos: match ch {
                'v' => Some(pos),
                _ => None,
            }
        }
    }
}

impl ActorInfo for Lava {
    fn get_pos(&self) -> Vector {
        self.pos.borrow().clone()
    }

    fn get_size(&self) -> Vector {
        self.size.borrow().clone()
    }

    fn act(&self, step: f64, level: &Level, keys: &HashMap<Keys, bool>) {
        let new_pos = self.pos.borrow().plus(self.speed.borrow().times(step));
        if level.obstacle_at(new_pos, self.get_size()).is_none() {
            *self.pos.borrow_mut() = new_pos;
        } else if self.repeat_pos.is_some() {
            *self.pos.borrow_mut() = self.repeat_pos.unwrap();
        } else {
            let new_speed = self.speed.borrow().times(-1.0);
            *self.speed.borrow_mut() = new_speed;
        }
    }
}

/// 金币
pub struct Coin {
    base_pos: Vector,
    pos: RefCell<Vector>,
    size: Vector,
    wobble: RefCell<f64>,
    wobble_speed: f64,
    wobble_dist: f64
}

impl Coin {
    pub fn new(pos: Vector) -> Coin {
        Coin {
            base_pos: pos.plus(Vector::new(0.2, 0.1)),
            pos: RefCell::new(pos.plus(Vector::new(0.2, 0.1))),
            size: Vector::new(0.6, 0.6),
            wobble: RefCell::new((Math::random() * 3.14 * 2.0) as f64),
            wobble_speed: 8.0,
            wobble_dist: 0.07,
        }
    }
}

impl ActorInfo for Coin {
    fn get_pos(&self) -> Vector {
        self.pos.borrow().clone()
    }

    fn get_size(&self) -> Vector {
        self.size
    }

    fn act(&self, step: f64, _: &Level, _: &HashMap<Keys, bool>) {
        *self.wobble.borrow_mut() += step * self.wobble_speed;
        let wobble_pos = Math::sin(self.wobble.borrow().clone()) * self.wobble_dist;
        *self.pos.borrow_mut() = self.base_pos.plus(Vector::new(0.0, wobble_pos));
    }
}

const SCALE : usize = 20;

pub struct DOMDisplay{
    document: web_sys::Document,
    level: Level,
    wrapper: web_sys::HtmlDivElement,
    actor_layer: RefCell<Option<web_sys::HtmlDivElement>>
}

impl DOMDisplay {
    pub fn new(level: Level) -> DOMDisplay {
        let window = web_sys::window().expect("should have a window in this context");
        let document = window.document().expect("window should have a document");
        let body = document.body().unwrap().dyn_into::<web_sys::HtmlBodyElement>().unwrap();
        let wrapper = document.create_element("div").unwrap().dyn_into::<web_sys::HtmlDivElement>().unwrap();
        body.append_child(&wrapper);
        DOMDisplay {
            document,
            level,
            wrapper,
            actor_layer: RefCell::new(None)
        }
    }

    fn element(&self, name: &str) -> Option<web_sys::Element> {
        self.document.create_element(name).ok()
    }
    
    fn element_with_class(&self, name: &str, class_name: &str) -> Option<web_sys::Element> {
        self.element(name).and_then(|e| match e.set_attribute(&"class", class_name) {
            Ok(_) => Some(e),
            _ => None,
        })
    }

    pub fn draw_background(&self) {
        let table = self.element_with_class("table", "background").unwrap().dyn_into::<web_sys::HtmlTableElement>().unwrap();
        table.style().set_property("width", &format!("{}px", self.level.width * SCALE));
        for row in self.level.grid.iter() {
            let row_elt = self.element("tr").unwrap().dyn_into::<web_sys::HtmlTableRowElement>().unwrap();
            row_elt.style().set_property("height", &format!("{}px", SCALE));
            table.append_child(&row_elt);
            for cell in row.iter() {
                let class_name = match cell {
                    StillObject::Empty => "empty",
                    StillObject::Lava => "lava",
                    StillObject::Wall => "wall",
                };
                let cell = self.element_with_class("td", &class_name).unwrap().dyn_into::<web_sys::HtmlTableCellElement>().unwrap();
                row_elt.append_child(&cell);
            }
        }
        self.wrapper.append_child(&table);
    }

    pub fn draw_actors(&self) -> web_sys::HtmlDivElement {
        let layer =  self.element("div").unwrap().dyn_into::<web_sys::HtmlDivElement>().unwrap();
        for actor in self.level.actors.borrow().iter() {
            let (pos, size, class_name) = match actor {
                Actor::Player(p) => (p.get_pos(), p.get_size(), "player"),
                Actor::Lava(l) => (l.get_pos(), l.get_size(), "lava"),
                Actor::Coin(c) => (c.get_pos(), c.get_size(), "coin"),
            };
            let actor_div = self.element_with_class("div", &format!("actor {}", class_name)).unwrap().dyn_into::<web_sys::HtmlDivElement>().unwrap();
            actor_div.style().set_property("width", &format!("{}px", size.x * SCALE as f64));
            actor_div.style().set_property("height", &format!("{}px", size.y * SCALE as f64));
            actor_div.style().set_property("left", &format!("{}px", pos.x * SCALE as f64));
            actor_div.style().set_property("top", &format!("{}px", pos.y * SCALE as f64));
            layer.append_child(&actor_div);
        }

        layer
    }

    pub fn draw_frame(&self) {
        if self.actor_layer.borrow().is_some() {
            self.wrapper.remove_child(self.actor_layer.borrow().as_ref().unwrap());
        }

        *self.actor_layer.borrow_mut() = Some(self.draw_actors());
        self.wrapper.append_child(self.actor_layer.borrow().as_ref().unwrap());
        self.wrapper.set_attribute("class", &format!("game {}", match self.level.status.borrow().as_ref(){
            Some(GameStatus::Lost) => "lost",
            Some(GameStatus::Win) => "win",
            _ => "",
        })).ok();
        self.scroll_player_into_view();
    }

    pub fn scroll_player_into_view(&self) {
        let width = self.wrapper.client_width();
        let height = self.wrapper.client_height();
        let margin = width / 3;

        // the viewport
        let left = self.wrapper.scroll_left();
        let right = left + width;
        let top = self.wrapper.scroll_top();
        let bottom = top + height;

        let (pos, size) = self.level.get_player();
        let center = pos.plus(size.times(0.5)).times(SCALE as f64);

        let x = center.x as i32;
        let y = center.y as i32;

        if x < left + margin {
            self.wrapper.set_scroll_left(x - margin);
        } else if x > right - margin {
            self.wrapper.set_scroll_left(x + margin - width);
        } else if y < top + margin {
            self.wrapper.set_scroll_top(y - margin);
        } else if y > bottom - margin {
            self.wrapper.set_scroll_top(y + margin - height);
        }
    }

    pub fn clear(&self) {
        self.wrapper.parent_node().unwrap().remove_child(&self.wrapper);
    }
}



fn run_animation(display: DOMDisplay) {
    let mut last_time: Option<f64> = None;
    let window = Rc::new(web_sys::window().expect("should have a window in this context"));
    let moved_window = Rc::clone(&window);

    let arrow_keys = Rc::new(RefCell::new(HashMap::new()));
    let arrow_keys_1 = arrow_keys.clone();

    let event_closure = Closure::wrap(Box::new(move |event: web_sys::KeyboardEvent| {
        let mut pressed = false;
        if AsRef::<web_sys::Event>::as_ref(&event).type_() == "keydown" {
            pressed = true;
        }

        match event.key().as_str() {
            "ArrowLeft" => arrow_keys_1.borrow_mut().insert(Keys::Left, pressed),
            "ArrowUp" => arrow_keys_1.borrow_mut().insert(Keys::Up, pressed),
            "ArrowRight" => arrow_keys_1.borrow_mut().insert(Keys::Right, pressed),
            _ => None,
        };
    }) as Box<dyn FnMut(_)>);

    window.add_event_listener_with_callback("keydown", event_closure.as_ref().unchecked_ref());
    window.add_event_listener_with_callback("keyup", event_closure.as_ref().unchecked_ref());

    event_closure.forget();

    let f : Rc<RefCell<Option<wasm_bindgen::closure::Closure<dyn FnMut(f64)>>>> = Rc::new(RefCell::new(None));
    let g = f.clone();
    
    *g.borrow_mut() = Some(Closure::wrap(Box::new(move |time: f64| {
        let mut stop = false;
        if last_time.is_some() {
            let time_step = Math::min(time - last_time.unwrap(), 100.0) / 1000.0;
            display.level.animate(time_step, &arrow_keys.borrow());
            display.draw_frame();

            if display.level.is_finished() {
                display.clear();
                stop = true;
                match display.level.status.borrow().as_ref() {
                    Some(GameStatus::Win) => alert(&format!("你赢了")),
                    _ => alert(&format!("你失败了"))
                }
                
            }
        }
        last_time = Some(time);
        if !stop {
            moved_window.request_animation_frame(f.borrow().as_ref().unwrap().as_ref().unchecked_ref());
        }
    }) as Box<dyn FnMut(_)>));

    window.request_animation_frame(g.borrow().as_ref().unwrap().as_ref().unchecked_ref()).unwrap();
    //closure.forget();
}

#[wasm_bindgen]
pub fn run() -> Result<(), JsValue> {
    let level = Level::new(PLAN);
    let display = DOMDisplay::new(level);
    display.draw_background();
    run_animation(display);
    Ok(())
}

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}